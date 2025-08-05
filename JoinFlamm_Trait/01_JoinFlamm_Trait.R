
setwd("C:/Users/samukelisiwem/OneDrive - Nelson Mandela University/Documents/3_SCHOLARY/PhD/Data collection/Analysis")

library(readxl)
library(dplyr)


#
##cederberg and george data combined 
Flamm <- read_excel("Data/CombinedOG.xlsx")

#
##henry fyre field trait data
Fieldtrait <- read_excel("Data/Field_Traits_Final.xlsx")


#
##making lowercase and trimming spaces in species names 
Flamm <- Flamm %>%
  mutate(Accepted_name = tolower(trimws(Accepted_name)))

Fieldtrait <- Fieldtrait %>%
  mutate(scientific_name_WFO = tolower(trimws(scientific_name_WFO)))


#
##
str(Flamm)
str(Fieldtrait)


#
##
colnames(Fieldtrait)
colnames(Flamm)


#
##see unique species from each df
##Unique species from df1 (flammability data)
unique(Flamm$Accepted_name)

#
##Unique species from df2 (field leaf traits)
unique(Fieldtrait$scientific_name_WFO)


#
unique(Fieldtrait$subregion)


#
##which species are shared between Flamm and Fieldtrait dfs
shared_species <- intersect(unique(Flamm$Accepted_name), unique(Fieldtrait$scientific_name_WFO))
print(shared_species)   #33 shared, not bad ..19??


#
##create a df of just shared species 
Flamm_shared <- Flamm %>% filter(Accepted_name %in% shared_species)
Fieldtrait_shared <- Fieldtrait %>% filter(scientific_name_WFO %in% shared_species)


#
###join by species
Joined01 <- Flamm %>%
  left_join(Fieldtrait,
            by = c("Accepted_name" = "scientific_name_WFO"))   #not having replicate causes problems..

#
colnames(Joined01)
#
str(Joined01)
#
summary(Joined01)


#
##now i want to join Labtrait data to this Joined01 df and create a new Joined02
##with all flammability traits and leaf traits
##again joined by the common species 

#
##henry fyre Labtrait data
Labtrait <- read_excel("Data/Lab_Traits_Final.xlsx")


#
##make lower case and spacing
Labtrait <- Labtrait %>%
  mutate(scientific_name_WFO = tolower(trimws(scientific_name_WFO)))


#
##see shared species
shared_species02 <- intersect(unique(Joined01$Accepted_name), unique(Labtrait$scientific_name_WFO))
print(shared_species02)  #Same species n=33


#
##
###join by species
Joined02 <- Joined01 %>%
  left_join(Labtrait,
            by = c("Accepted_name" = "scientific_name_WFO"))

#
##
colnames(Joined02)

