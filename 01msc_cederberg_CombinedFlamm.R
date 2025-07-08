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
CombinedLong <- CombinedOG %>%
  pivot_longer(cols = c("TimeToFlaming", "PostBurntMassEstimate", "MaximumFlameTemperature", "FMC_percentage"),
               names_to = "variable", values_to = "value")




