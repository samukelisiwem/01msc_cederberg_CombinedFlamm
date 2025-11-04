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

writexl::write_xlsx(Grootspecies_list, "Data/Grootspecies_list.xlsx")

