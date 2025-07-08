#
setwd("/Users/samukelisiwem/OneDrive - Nelson Mandela University/Documents/3_SCHOLARY/PhD/Data collection/Analysis/Data/")

library(tidyverse)
library(readxl) 

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
  ggplot() +
  geom_boxplot(aes(y = value, x = species_code)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6)) +
  facet_wrap(vars(variable), scales = "free")

#Flamm Attriutes by family
CombinedLong %>%
  ggplot() +
  geom_boxplot(aes(y = value, x = Accepted_family)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8)) +
  facet_wrap(vars(variable), scales = "free")

#Flamm Attriutes by growth form
CombinedLong %>%
  ggplot() +
  geom_boxplot(aes(y = value, x = growth_form)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(vars(variable), scales = "free")
