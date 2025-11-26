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

#see shared species

grootshared <- intersect(unique(Fieldtrait$scientific_name_WFO), 
          unique(grootbos$`scientific name`)) #183spp

length(grootshared)   
print(grootshared)

#filter shared species from each
grootbos_shared <- grootbos %>%
  filter(`scientific name` %in% grootshared)

Fieldtrait_shared <- Fieldtrait %>%
  filter(scientific_name_WFO %in% grootshared)


#join
groot_final <- left_join(
  grootbos_shared,
  Fieldtrait_shared,
  by = c("scientific name" = "scientific_name_WFO")
)

unique(groot_final$`scientific name`)

Grootspecies_list <- data.frame(scientific_name = unique(groot_final$`scientific name`))
head(Grootspecies_list )
nrow(Grootspecies_list )   # should be 183

unique(groot_final$LF_GandP)

groot_final %>%
  count(LF_GandP, sort = TRUE)



#save
#grootspeciesList <- data.frame(scientific_name = groot_final)

#writexl::write_xlsx(Grootspecies_list, "Data/Grootspecies_list.xlsx")


################################################################################
# 20251125 

# read in dunefields data 
dunefields <- read_excel("~/3_SCHOLARY/PhD/Data collection/Analysis/01msc_cederberg_CombinedFlamm/JoinedFlamm_Trait/Data/dunefieldslist.xlsx")
View(dunefields)

#
Fieldtrait <- read_csv("~/3_SCHOLARY/PhD/Data collection/Analysis/01msc_cederberg_CombinedFlamm/JoinedFlamm_Trait/Data/Field_Traits_Final.csv")
View(Fieldtrait)

# making lowercase and trimming spaces in species names 
Fieldtrait <- Fieldtrait %>%
  mutate(scientific_name_WFO = tolower(trimws(scientific_name_WFO)))

#
dunefields <- dunefields %>%
  mutate(`Scientific name` = tolower(trimws(`Scientific name`)))

# explore species 
unique(Fieldtrait$scientific_name_WFO)  #1000+ species 

unique(dunefields$`Scientific name`)
length(dunefields$`Scientific name`)

#see shared species
dunefieldshared <- intersect(unique(Fieldtrait$scientific_name_WFO), 
                         unique(dunefields$`Scientific name`)) #183spp

length(dunefieldshared)   
print(dunefieldshared)

#filter shared species from each
dunefieldshared <- dunefields %>%
  filter(`Scientific name` %in% dunefieldshared)

Fieldtrait_shared <- Fieldtrait %>%
  filter(scientific_name_WFO %in% dunefieldshared)


#join
dunefield_final <- left_join(
  dunefieldshared,
  Fieldtrait_shared,
  by = c("Scientific name" = "scientific_name_WFO")
)

unique(dunefield_final$`Scientific name`)

Dunespecies_list <- data.frame(Scientific_name = unique(dunefield_final$`Scientific name`))

head(Dunespecies_list)
nrow(Dunespecies_list)   # should be 46

unique(dunefield_final$`Scientific name`)

#
# Dunespecies_list <- data.frame(Scientific_name = dunefield_final)
# 
# writexl::write_xlsx(Dunespecies_list, "C:/Users/samukelisiwem/OneDrive - Nelson Mandela University/Documents/3_SCHOLARY/PhD/Data collection/Analysis/01msc_cederberg_CombinedFlamm/JoinedFlamm_Trait/Data/Dunespecies_list.xlsx")


