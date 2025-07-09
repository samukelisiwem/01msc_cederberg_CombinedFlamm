#
setwd("/Users/samukelisiwem/OneDrive - Nelson Mandela University/Documents/3_SCHOLARY/PhD/Data collection/Analysis/Data/")

#packages
library(tidyverse)
library(readxl) 
library(dplyr)
library(GGally)
library(hms)

#reads data
#Loads the Excel file
#Cleans and reshapes the data
#Plots faceted box plots with rotated x-axis labels
#flammability attributes varies across species
read_xlsx("01msc_cederberg_CombinedFlamm.xlsx", sheet = "Combined") %>%
  pivot_longer(cols = c("TimeToFlaming", "PostBurntMassEstimate", "MaximumFlameTemperature","FMC_percentage"),
               names_to = "variable", values_to = "value") %>%
  ggplot() +
  geom_boxplot(aes(y = value, x = species_code)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6)) +
  facet_wrap(vars(variable), scales = "free")

CombinedOG <- read_xlsx("01msc_cederberg_CombinedFlamm.xlsx", sheet = "Combined")


#Datasource shows data from PhD / MSc
CombinedOG <- CombinedOG %>%
  mutate(
    year = substr(as.character(Date), 1, 4),
    DataSource = case_when(
      year == "2024" ~ "PhD",
      year %in% c("2018", "2019") ~ "MSc")
    )
  
#checking PhD/MSc entries
CombinedOG %>%
  count(DataSource)     #alrightee


#rename some headings
Combined02 <- CombinedOG %>%
 rename(
    `FMC(%)` = FMC_percentage,
    `TimeToFlaming(s)` = TimeToFlaming,
    `PostBurntMassEstimate(%)` = PostBurntMassEstimate,
    `MaximumFlameTemperature(°C)` = MaximumFlameTemperature
    )

#save data in long
CombinedLong <- Combined02 %>%
  pivot_longer(cols = c("TimeToFlaming(s)", "PostBurntMassEstimate(%)", "MaximumFlameTemperature(°C)", "FMC(%)", "FlammabilityIndex"),
               names_to = "variable", values_to = "value")

#boxplots on Long data
CombinedLong %>%
  group_by(variable) %>%
  mutate(species_code = fct_reorder(species_code, value, .fun = median, .na_rm = TRUE)) %>%
  ungroup() %>%
  ggplot() +
  geom_boxplot(aes(y = value, x = species_code, fill = DataSource)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6)) +
  facet_wrap(vars(variable), scales = "free")       #MSc shrubs and trees
                                                    #PhD herbs and shrubs

#maybe compare them independent datas
CombinedLong %>%
  group_by(variable) %>%
  summarise(
    p_value = wilcox.test(value ~ DataSource)$p.value
  )


#Flamm Attriutes by family
CombinedLong %>%
  ggplot() +
  geom_boxplot(aes(y = value, x = Accepted_family, fill = DataSource)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8)) +
  facet_wrap(vars(variable), scales = "free")

#Flamm Attriutes by growth form
CombinedLong %>%
  ggplot() +
  geom_boxplot(aes(y = value, x = growth_form, fill = DataSource)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(vars(variable), scales = "free")

# See what sheets are in the Excel workbook
excel_sheets("01msc_cederberg_CombinedFlamm.xlsx") #aaahh!

#checking class of the binary variable Blowtorch 0:no torch, 1 blowtorched
class(CombinedLong$blowtorch)

# structure overview of the entire dataframe
str(CombinedLong)

sapply(CombinedLong, class) #melikeythisone

#cederberg means
Combined02 %>% #cant use long data here because column is just value...
group_by(Site) %>%
  summarize(`FMC(%)` = mean(`FMC(%)`), 
            `TimeToFlaming(s)` = mean(`TimeToFlaming(s)`)
  )

#filtered cederberg, and selected a specific set of variables
#and arranged Temp in desc.order
Combined02 %>% 
  filter(Site == "Cederberg") %>%
  select(`Date`, `MaximumFlameTemperature(°C)`,`FMC(%)`) %>%
  arrange(desc(`MaximumFlameTemperature(°C)`))

#i want to remove some columns from data
#probably should be done before reshaping data to Long format...?
CombinedSelected04 <- Combined02 %>%
  select(-c(Taxonomic_status,TTF,`PreFireMass_(g)`, PBM,MFT,paperbagID,`fresh mass (g)`,`dried mass (g)`,))

#the way time is written here is odd in time burnt
CombinedSelected04 <- CombinedSelected04 %>%
  mutate(time_burnt = as_hms(`time burnt`)) %>%   # create new cleaned column
  select(-`time burnt`)                           # remove the original column

#may try to make long format from the recent df
CombinedLong02 <- CombinedSelected04 %>%
  pivot_longer(cols = c("TimeToFlaming(s)", "PostBurntMassEstimate(%)", "MaximumFlameTemperature(°C)", "FMC(%)", "FlammabilityIndex"),
               names_to = "variable", values_to = "value")

head(CombinedLong02)

# plot everything against everything
CombinedSelected04 %>%
  select(`Site`, `TimeToFlaming(s)`, `PostBurntMassEstimate(%)`, `MaximumFlameTemperature(°C)`, `FMC(%)`) %>%
    GGally::ggpairs(columns = 2:ncol(.))


# Select only the numeric flammability columns 
CombinedSelected04 %>%
  select(
    `TimeToFlaming(s)`,
    `PostBurntMassEstimate(%)`,
    `MaximumFlameTemperature(°C)`,
    `FMC(%)`
  ) %>%
  ggpairs()            # Only numeric columns for pairwise plots
                                
       
#maximum temperature vs postburntmass
ggplot(CombinedSelected04, aes(x = `MaximumFlameTemperature(°C)`, y = `PostBurntMassEstimate(%)`)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(title = "",
       x = "Maximum Temperature (°C)",
       y = "Estimated Unburned Plant Mass") +
  theme_minimal()                              #mmh...


