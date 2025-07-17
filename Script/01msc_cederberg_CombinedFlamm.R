##
###
#packages
library(tidyverse)
library(readxl) 
library(GGally)
library(hms)

#reads data
#Loads the Excel file
#Cleans and reshapes the data
#Plots faceted box plots with rotated x-axis labels
#flammability attributes varies across species
read_xlsx("Data/01msc_cederberg_CombinedFlamm.xlsx", sheet = "Combined") %>%
  pivot_longer(cols = c("TimeToFlaming", "PostBurntMassEstimate", "MaximumFlameTemperature","FMC_percentage"),
               names_to = "variable", values_to = "value") %>%
  ggplot() +
  geom_boxplot(aes(y = value, x = species_code)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6)) +
  facet_wrap(vars(variable), scales = "free")

#create an object of the data I want to use
CombinedOG <- read_xlsx("Data/01msc_cederberg_CombinedFlamm.xlsx", sheet = "Combined")

#See what sheets are in the Excel workbook
excel_sheets("Data/01msc_cederberg_CombinedFlamm.xlsx") #aaahh!

#to see class category of table contents
sapply(CombinedOG, class)            #melikeythisone

#make Date column to Date class and timeburnt to HM
CombinedOG <- CombinedOG %>%
  mutate(
    Date = as.Date(as.character(Date), format = "%Y%m%d"),
    `time burnt` = format(`time burnt`, "%H:%M")
  )

#create Datasource shows data from PhD or MSc
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
CombinedOG <- CombinedOG %>%
 rename(
    `FMC(%)` = FMC_percentage,
    `TimeToFlaming(s)` = TimeToFlaming,
    `PostBurntMassEstimate(%)` = PostBurntMassEstimate,
    `MaximumFlameTemperature(°C)` = MaximumFlameTemperature
    )

#see all columns in df 
colnames(CombinedOG)

#remove columns not needed
CombinedOG <- CombinedOG %>%
  select(-`PreFireMass_(g)`, -PBM, -MFT, -TTF, -year, -paperbagID)

summary(CombinedOG)

#CombinedOG in long format 
CombinedLong <- CombinedOG %>%
  pivot_longer(cols = c("TimeToFlaming(s)", "PostBurntMassEstimate(%)", "MaximumFlameTemperature(°C)", "FMC(%)", "FlammabilityIndex"),
               names_to = "variable", values_to = "value")

#boxplots on Long data
CombinedLong %>%
  filter(variable != "FlammabilityIndex") %>%
  group_by(variable) %>%
  ggplot(aes(y = value, x = species_code, fill = DataSource)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 6)) +
  facet_wrap(vars(variable), scales = "free")       #MSc has more shrubs and trees
                                                    #PhD has more herbs and shrubs
                                                   

#see if MSc vs PhD independent datas if comparable
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

# structure overview of the entire dataframe
str(CombinedLong)

#cederberg means
CombinedOG %>%          #cant use long data here because column is just value
group_by(Site) %>%
  summarize(`FMC(%)` = mean(`FMC(%)`),
            `MaximumFlameTemperature(°C)` = mean(`MaximumFlameTemperature(°C)`),
            `TimeToFlaming(s)` = mean(`TimeToFlaming(s)`), 
            `PostBurntMassEstimate(%)` = mean(`PostBurntMassEstimate(%)`)
  )

#filtered cederberg, and selected a set of variables
#and arranged Temp in desc.order
CombinedOG%>% 
  filter(Site == "Cederberg") %>%
  select(`Date`, `MaximumFlameTemperature(°C)`,`FMC(%)`) %>%
  arrange(desc(`MaximumFlameTemperature(°C)`))

# plot everything against everything
CombinedOG %>%
  select(`Site`, `TimeToFlaming(s)`, `PostBurntMassEstimate(%)`, `MaximumFlameTemperature(°C)`, `FMC(%)`) %>%
    GGally::ggpairs(columns = 2:ncol(.))


# Select only the numeric flammability columns for pairwise plots
CombinedOG %>%
  select(
    `TimeToFlaming(s)`,
    `PostBurntMassEstimate(%)`,
    `MaximumFlameTemperature(°C)`,
    `FMC(%)`
  ) %>%
  ggpairs()         
                                
       
#maximum temperature vs postburntmass
ggplot(CombinedOG, aes(x = `MaximumFlameTemperature(°C)`, y = `PostBurntMassEstimate(%)`)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(title = "",
       x = "Maximum Temperature (°C)",
       y = "Unburned Plant Mass") +
  theme_minimal()                             #mmh...



