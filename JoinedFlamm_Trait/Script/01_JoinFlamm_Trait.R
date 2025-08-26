
setwd("C:/Users/samukelisiwem/OneDrive - Nelson Mandela University/Documents/3_SCHOLARY/PhD/Data collection/Trait dimensions Analysis/")

library(tidyverse)
library(readxl)
library(lme4)
library(MuMIn)
library(car)


#
##
###cederberg and george data combined 

Flamm <- read_excel("Data/CombinedOG.xlsx")

#
##
###henry field trait data

Fieldtrait <- read_excel("Data/Field_Traits_Final.xlsx")

#Fieldtrait <- read.csv("Data/Field_Traits_Final.csv") #oki

#
##
###making lowercase and trimming spaces in species names 

Flamm <- Flamm %>%
  mutate(Accepted_name = tolower(trimws(Accepted_name)))

Fieldtrait <- Fieldtrait %>%
  mutate(scientific_name_WFO = tolower(trimws(scientific_name_WFO)))


#
##
str(Flamm)

str(Fieldtrait)

colnames(Fieldtrait)

colnames(Flamm)


#
##
###Unique species from flammability data

unique(Flamm$Accepted_name) #53 species 

#
##
###Unique species from field leaf traits

unique(Fieldtrait$scientific_name_WFO)  #1000+ species 

unique(Fieldtrait$subregion)

#
##
###which species are shared between Flamm and Fieldtrait 

shared_species <- intersect(unique(Flamm$Accepted_name), unique(Fieldtrait$scientific_name_WFO))
print(shared_species)   #33 shared, not bad ..19??

#
##
###create a new df of just shared species 

Flamm_shared <- Flamm %>% filter(Accepted_name %in% shared_species)

Fieldtrait_shared <- Fieldtrait %>% filter(scientific_name_WFO %in% shared_species)

#
##
###join the two dfs by shared species

Shared_spp_01 <- Flamm %>%
  left_join(Fieldtrait,
            by = c("Accepted_name" = "scientific_name_WFO"))   #not having replicate causes problems..

colnames(Shared_spp_01)


###now to join Labtrait data to this shared_spp_01 df and create a new Shared_spp_02
###with all flammability traits and leaf traits

#
##
###henry fyre Labtrait data
Labtrait <- read_excel("Data/Lab_Traits_Final.xlsx")

#
##
###make lower case and spacing

Labtrait <- Labtrait %>%
  mutate(scientific_name_WFO = tolower(trimws(scientific_name_WFO)))

colnames(Labtrait)

#
##
#see shared species with Shared_spp_01 df

shared_species02 <- intersect(unique(Shared_spp_01$Accepted_name), unique(Labtrait$scientific_name_WFO))
print(shared_species02)  #Same species n=33

#
##
###join by species to create a new df

Shared_spp_df <- Shared_spp_01 %>%
  left_join(Labtrait,
            by = c("Accepted_name" = "scientific_name_WFO"))

colnames(Shared_spp_df)

#
##
###make df to long format
###plant trait columns 28 leaf traits 

trait_col <- c(
  # 11 Field traits 
  "height_cm", "canopy_axis_1_cm", "canopy_axis_2_cm", "canopy_area_cm2",
  "branch_order", "pubescence", "percent_N", "percent_C", "C_to_N_ratio",
  "d_15N_14N", "d_13C_12C",
  
  # 17 Lab traits
  "FMC_proportion", "num_leaves", "leaf_area_cm2", "leaf_length_cm",
  "avg_leaf_width_cm", "max_leaf_width_cm", "leaf_thickness_mm",
  "leaf_fresh_wgt_g", "leaf_dry_wgt_g", "twig_fresh_g", "twig_dry_g", 
  "lma", "fwc", "succulence", "ldmc", "lwr", "twig_fwc"
)

#
##
sapply(Shared_spp_df[trait_col], class)

#
##
###Convert all trait columns to relevant class

Shared_spp_df <- Shared_spp_df %>%
  mutate(across(c(
    height_cm, canopy_axis_2_cm, canopy_area_cm2,
    percent_N, percent_C, C_to_N_ratio, d_15N_14N, d_13C_12C,
    num_leaves, leaf_area_cm2, leaf_length_cm, avg_leaf_width_cm, max_leaf_width_cm,
    leaf_thickness_mm, leaf_fresh_wgt_g, leaf_dry_wgt_g,
    twig_fresh_g, twig_dry_g, lma, fwc, succulence, ldmc, lwr, twig_fwc
  ), parse_number)) %>%
  mutate(pubescence = as.factor(pubescence))  #re-run sapply above

#
##
###Flammability traits column names

flamm_col <- c(
  "TimeToFlaming(s)", 
  "PostBurntMassEstimate(%)", 
  "MaximumFlameTemperature(°C)"
)

############################################RQ1
#
##
###linear mixed-effects models with species as random effects 
###adding subregion as second randoms complicates model

m_ignitability <- lmer(`TimeToFlaming(s)` ~
    `FMC(%)` + branch_order + percent_N + percent_C + C_to_N_ratio +
    d_15N_14N + d_13C_12C +
    leaf_area_cm2 + leaf_length_cm + avg_leaf_width_cm +
    leaf_thickness_mm +
    leaf_fresh_wgt_g + leaf_dry_wgt_g +
    twig_fresh_g + twig_dry_g +
    lma + succulence + ldmc +
    (1 | species_name) + (1 | subregion.x), #2 randoms
  data = Shared_spp_df, REML = TRUE)    #Warning message:Some predictor variables are on very different scales: consider rescaling 

#
##
###skewness test to determine is transformation is needed or not 
library(e1071)   # for skewness

# 
##
###Time to Flaming
TT <- Shared_spp_df$`TimeToFlaming(s)`
hist(TT, breaks = 30, main = "Histogram: Time to Flaming", xlab = "Seconds")
skewness(TT, na.rm = TRUE) 
qqnorm(TT); qqline(TT)

#
##
###Max Temperature
MT <- Shared_spp_df$`MaximumFlameTemperature(°C)`
hist(MT, breaks = 30, main = "Histogram: Max Temperature", xlab = "°C")
skewness(MT, na.rm = TRUE)
qqnorm(MT); qqline(MT)

#
##
#### Post Burn Mass
PBM <- Shared_spp_df$`PostBurntMassEstimate(%)`
hist(PBM, breaks = 30, main = "Histogram: Post Burn Mass", xlab = "%")
skewness(PBM, na.rm = TRUE)
qqnorm(PBM); qqline(PBM)

#no transformation needed for any response variable 

#
##
###Traitgroups

structural_traits <- c(
  "height_cm", "canopy_axis_1_cm", "canopy_axis_2_cm", "canopy_area_cm2",
  "branch_order", "leaf_area_cm2", "leaf_length_cm", "avg_leaf_width_cm",
  "max_leaf_width_cm", "leaf_thickness_mm", "lwr", "pubescence"
)

chemical_traits <- c(
  "percent_C", "percent_N", "C_to_N_ratio", "d_13C_12C", "d_15N_14N"
)

water_traits <- c(
   "FMC_proportion",
  "fwc", "twig_fwc", "succulence", "ldmc"
)

mass_traits <- c(
  "leaf_fresh_wgt_g", "leaf_dry_wgt_g", "lma",
  "twig_fresh_g", "twig_dry_g"
)

#
##
###scale() only numeric traits 
Shared_spp_df <- Shared_spp_df %>%
  mutate(across(where(is.numeric) & all_of(trait_col), ~ as.numeric(scale(.))))

#
##
### run TT  

m_ignitability <- lmer(
  `TimeToFlaming(s)` ~ 
    height_cm + canopy_axis_1_cm + canopy_axis_2_cm +
    canopy_area_cm2 + branch_order + pubescence + percent_N +
    percent_C + C_to_N_ratio + d_15N_14N + d_13C_12C +
    FMC_proportion + num_leaves + leaf_area_cm2 + 
    leaf_length_cm + avg_leaf_width_cm + max_leaf_width_cm + 
    leaf_thickness_mm + leaf_fresh_wgt_g + leaf_dry_wgt_g + 
    twig_fresh_g + twig_dry_g + lma + fwc + 
    succulence + ldmc + lwr + twig_fwc +
    (1 | species_name),
  data = Shared_spp_df,
  REML = FALSE
)
summary(m_ignitability)

r.squaredGLMM(m_ignitability)

vif(m_ignitability)
 
#
##
###
m_combustibility <- lmer(
  `MaximumFlameTemperature(°C)` ~ 
    height_cm + canopy_axis_1_cm + canopy_axis_2_cm +
    canopy_area_cm2 + branch_order + pubescence + percent_N +
    percent_C + C_to_N_ratio + d_15N_14N + d_13C_12C +
    FMC_proportion + num_leaves + leaf_area_cm2 + 
    leaf_length_cm + avg_leaf_width_cm + max_leaf_width_cm + 
    leaf_thickness_mm + leaf_fresh_wgt_g + leaf_dry_wgt_g + 
    twig_fresh_g + twig_dry_g + lma + fwc + 
    succulence + ldmc + lwr + twig_fwc +
    (1 | species_name),
  data = Shared_spp_df,
  REML = FALSE
)
summary(m_combustibility)

r.squaredGLMM(m_combustibility)

vif(m_combustibility)

#
##
###
m_consumability <- lmer(
  `PostBurntMassEstimate(%)` ~ 
    height_cm + canopy_axis_1_cm + canopy_axis_2_cm +
    canopy_area_cm2 + branch_order + pubescence + percent_N +
    percent_C + C_to_N_ratio + d_15N_14N + d_13C_12C +
    FMC_proportion + num_leaves + leaf_area_cm2 + 
    leaf_length_cm + avg_leaf_width_cm + max_leaf_width_cm + 
    leaf_thickness_mm + leaf_fresh_wgt_g + leaf_dry_wgt_g + 
    twig_fresh_g + twig_dry_g + lma + fwc + 
    succulence + ldmc + lwr + twig_fwc +
    (1 | species_name),
  data = Shared_spp_df,
  REML = FALSE
)
summary(m_consumability)

r.squaredGLMM(m_consumability)

vif(m_consumability)

########################################RQ2
#
##
###Growth-form models
m_gf_TT  <- lmer(`TimeToFlaming(s)` ~ growth_form + (1|species_name), data = Shared_spp_df, REML = FALSE)
m_gf_MT  <- lmer(`MaximumFlameTemperature(°C)` ~ growth_form + (1|species_name), data = Shared_spp_df, REML = FALSE)
m_gf_PBM <- lmer(`PostBurntMassEstimate(%)` ~ growth_form + (1|species_name), data = Shared_spp_df, REML = FALSE)

# R^2 (marginal = fixed effects; conditional = fixed + random)
r.squaredGLMM(m_gf_TT)
r.squaredGLMM(m_gf_MT)
r.squaredGLMM(m_gf_PBM)

########################
