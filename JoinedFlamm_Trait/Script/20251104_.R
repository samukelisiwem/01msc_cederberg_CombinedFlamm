#
#20251104
#

library(tidyverse)
library(readxl)
library(lme4)
library(MuMIn)
library(car)
library(lmerTest)
library(corrplot)

#read in leaf field trait data 
Fieldtrait <- read.csv("Data/Field_Traits_Final.csv") 

# read in grotbos data 
grootbos <-  read_excel("Data/Grootbos_database_.xlsx", sheet = "flora (2)")

# making lowercase and trimming spaces in species names 
Fieldtrait <- Fieldtrait %>%
  mutate(scientific_name_WFO = tolower(trimws(scientific_name_WFO)))

grootbos <- grootbos %>%
  mutate(`scientific name` = tolower(trimws(`scientific name`)))
  
#explore species 
unique(Fieldtrait$scientific_name_WFO)  #1000+ species 

unique(grootbos$`scientific name`)

#create df of sharedspecies_grot
grootshared <- intersect(unique(Fieldtrait$scientific_name_WFO), 
                            unique(grootbos$`scientific name`))

print(grootshared)


