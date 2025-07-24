##
###
#packages
library(tidyverse)
library(readxl) 
library(GGally)
library(hms)
library(lme4)
library(ggfortify)
library(factoextra)


#reads data
#Loads the Excel file
#Cleans and reshapes the data
#Plot faceted boxplots with rotated x-axis labels
#species variation across flammability attributes 
read_xlsx("Data/01msc_cederberg_CombinedFlamm.xlsx", sheet = "Combined") %>%
  pivot_longer(cols = c("TimeToFlaming", "PostBurntMassEstimate", "MaximumFlameTemperature","FMC_percentage"),
               names_to = "variable", values_to = "value") %>%
  ggplot() +
  geom_boxplot(aes(y = value, x = species_code)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6)) +
  facet_wrap(vars(variable), scales = "free")


#create an object of the data I want to use
#OG data as recorded in wide format
CombinedOG <- read_xlsx("Data/01msc_cederberg_CombinedFlamm.xlsx", sheet = "Combined")


#See what sheets are in the Excel workbook
excel_sheets("Data/01msc_cederberg_CombinedFlamm.xlsx") #aaahh!


#to see class category of OG data
sapply(CombinedOG, class)                               #melikeythisone


#Date is numeric and time burnt is 1899-12-31 10:10:00
#correct to accepted formats
CombinedOG <- CombinedOG %>%
  mutate(
    Date = as.Date(as.character(Date), format = "%Y%m%d"),
    `time burnt` = format(`time burnt`, "%H:%M")
  )


#create Datasource column to show if data from PhD/ MSc
CombinedOG <- CombinedOG %>%
  mutate(
    year = substr(as.character(Date), 1, 4),
    DataSource = case_when(
      year == "2024" ~ "PhD",
      year %in% c("2018", "2019") ~ "MSc")
    )
  

#checking PhD/MSc entries
CombinedOG %>%
  count(DataSource)                           #alrightee


#rename some headings
CombinedOG <- CombinedOG %>%
 rename(
    `FMC(%)` = FMC_percentage,
    `TimeToFlaming(s)` = TimeToFlaming,
    `PostBurntMassEstimate(%)` = PostBurntMassEstimate,
    `MaximumFlameTemperature(°C)` = MaximumFlameTemperature
    )


#see all columns in OG 
colnames(CombinedOG)

#remove columns not really needed
CombinedOG <- CombinedOG %>%
  select(-`PreFireMass_(g)`, -PBM, -MFT, -TTF, -year, -paperbagID, -`fresh mass (g)`, -`dried mass (g)`)

summary(CombinedOG)


#Pivot CombinedOG to long format 
CombinedLong <- CombinedOG %>%
  pivot_longer(cols = c("TimeToFlaming(s)", "PostBurntMassEstimate(%)", "MaximumFlameTemperature(°C)", "FMC(%)", "FlammabilityIndex"),
               names_to = "flammtrait", values_to = "value")


#boxplots on Long data
#species across variables 
CombinedLong %>%
  filter(flammtrait != "FlammabilityIndex") %>%
  group_by(flammtrait) %>%
  ggplot(aes(y = value, x = species_code, fill = DataSource)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 6)) +
  facet_wrap(vars(flammtrait), scales = "free")                       #MSc has more shrubs and trees
                                                                      #MSc has more higher temps compared to PhD  
                                                                      #PhD has more herbs and shrubs
                                                   

#see if MSc vs PhD are independent / if comparable
#sure differ by site, species, samples, replicates,...etc
summary(lm(value ~ DataSource + species_name, data = CombinedLong))
                                                                      #No significant difference in the values between MSc and PhD
                                                                      #accoubts for species variation, this makes some sense to me than lmer below 
                                                                      #here datas comparable


#maybe treat species as a random factor? 
summary(lmer(value ~ DataSource + (1 | species_name), data = CombinedLong))
                                                                      #not sure about this one.data not comparable and species =no variation?


#Flamm Attributes by family
CombinedLong %>%
  filter (flammtrait != "FlammabilityIndex") %>%
  ggplot() +
  geom_boxplot(aes(y = value, x = Accepted_family, fill = DataSource)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8)) +
  facet_wrap(vars(flammtrait), scales = "free")


#Flamm Attributes by growth form
CombinedLong %>%
  filter (flammtrait != "FlammabilityIndex") %>%
  ggplot() +
  geom_boxplot(aes(y = value, x = growth_form, fill = DataSource)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(vars(flammtrait), scales = "free")


##Does flammability traits differ between families and growth forms?
##Is flammability (can be done per flammatrait) explained by family or  growth form?
summary(lm(value ~ Accepted_family, data = CombinedLong, subset = (flammtrait == "FlammabilityIndex")))

# Model with growth form
summary(lm(value ~ growth_form, data = CombinedLong, subset = (flammtrait == "FlammabilityIndex")))
                                                                              #familiy explain a little more flamm than growth form


#which families are more flammable? PCA maybe?
#i run this on wide data :(
pca_res <- prcomp(CombinedOG[, c("TimeToFlaming(s)", "MaximumFlameTemperature(°C)", "PostBurntMassEstimate(%)")], 
                  scale. = TRUE)

#install.packages('ggfortify')
autoplot(pca_res, data = CombinedOG, colour = "Accepted_family", label = FALSE) +
  theme_minimal()     #way too colourful...perhaps try medians rather? 


fviz_pca_var(pca_res, col.var = "contrib", repel = TRUE)


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

#or

# Select only the numeric flammability columns for pairwise plots
CombinedOG %>%
  select(
    `TimeToFlaming(s)`,
    `PostBurntMassEstimate(%)`,
    `MaximumFlameTemperature(°C)`,
    `FMC(%)`
  ) %>%
  ggpairs()         
                                
       
#trait distribution across sites with violin plots
ggplot(CombinedLong, aes(x = Site, y = value, fill = flammtrait)) +
  geom_violin(trim = FALSE, alpha = 0.6) +
  facet_wrap(~ flammtrait, scales = "free_y") +
  theme_minimal()


#Is the timing of collection (AM vs PM) influencing trait values?
# Does it matter when we collect relative to when we burn?

#whats on these two...
unique(CombinedLong$`time collected`)
unique(CombinedLong$BurnSession)

#
ggplot(filter (CombinedLong, flammtrait != "FlammabilityIndex"), aes(x = `time collected`, y = value, fill = `time collected`)) +
  geom_boxplot() +
  facet_wrap(~ flammtrait, scales = "free_y") +
  theme_minimal() +
  labs(title = "",
       x = "", y = "")
                                                                #interesting minimal spread of values in PM collection


#Does TimeCollected (AM/PM), BurntSession affect trait value?
ggplot(filter (CombinedLong, flammtrait != "FlammabilityIndex"), aes(x = BurnSession, y = value, fill = `time collected`)) +
  geom_boxplot(position = position_dodge()) +
  facet_wrap(~ flammtrait, scales = "free_y") +
  theme_minimal() +
  labs(title = "",
       x = "Burn session", y = "", fill = "Time Collected")
                  #

#test
model01 <- lm(value ~ `time collected` * BurnSession, data = CombinedLong)
summary(model01)








