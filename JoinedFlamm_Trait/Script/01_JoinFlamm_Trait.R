#
#Data chapter 2: Investigating the potential of predicting flammability based of leaf traits
##
###Flamm is flammability experiments data 
###Fieldtrait and Labtrait are Henrys leaf trait data

library(tidyverse)
library(readxl)
library(lme4)
library(MuMIn)
library(car)
library(lmerTest)

#
##
###cederberg and george data combined 
Flamm <- read_excel("Data/CombinedOG.xlsx")

#
####henry field trait data
Fieldtrait <- read.csv("Data/Field_Traits_Final.csv") 

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
###see Unique species from flammability data
unique(Flamm$Accepted_name) #53 species 

#
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
###create a new df of just shared species from each df 
Flamm_shared <- Flamm %>% filter(Accepted_name %in% shared_species)

Fieldtrait_shared <- Fieldtrait %>% filter(scientific_name_WFO %in% shared_species)

#
##
###join the two dfs by shared species
Shared_spp_df01 <- Flamm %>%
  left_join(Fieldtrait,
            by = c("Accepted_name" = "scientific_name_WFO"))   #not having replicate causes problems..??

colnames(Shared_spp_df01)

#
##Join Labtrait data to this shared_spp_df01 and create a new Shared_spp_02
###Shared_spp_df02 will have all flammability traits and leaf traits (field and Lab measured)

#
##Labtrait data
Labtrait <- read.csv("Data/Lab_Traits_Final.csv")

#
##
###make lower case and spacing
Labtrait <- Labtrait %>%
  mutate(scientific_name_WFO = tolower(trimws(scientific_name_WFO)))

colnames(Labtrait)

#
##
###confirm shared species with Shared_spp_df01
shared_species02 <- intersect(unique(Shared_spp_df01$Accepted_name), unique(Labtrait$scientific_name_WFO))
print(shared_species02)  #Same species n=33

#
##
###join by species to create a new df
Shared_spp_df02 <- Shared_spp_df01 %>%
  left_join(Labtrait,
            by = c("Accepted_name" = "scientific_name_WFO")) %>%
  filter(Accepted_name %in% shared_species02)

colnames(Shared_spp_df02)

length(unique(Shared_spp_df02$species_name))

#28 leaf traits 

trait_col <- c(
  "height_cm", "canopy_axis_1_cm", "canopy_axis_2_cm", "canopy_area_cm2",  # 11 Field traits 
  "branch_order", "pubescence", "percent_N", "percent_C", "C_to_N_ratio",
  "d_15N_14N", "d_13C_12C",
  
  "FMC_proportion", "num_leaves", "leaf_area_cm2", "leaf_length_cm",     # 17 Lab traits
  "avg_leaf_width_cm", "max_leaf_width_cm", "leaf_thickness_mm",
  "leaf_fresh_wgt_g", "leaf_dry_wgt_g", "twig_fresh_g", "twig_dry_g", 
  "lma", "fwc", "succulence", "ldmc", "lwr", "twig_fwc"
)

#
##
###
sapply(Shared_spp_df02[trait_col], class)

#
##
###Convert all trait columns to relevant class 
Shared_spp_df02 <- Shared_spp_df02 %>%
  mutate(
    pubescence   = as.factor(pubescence),
    num_leaves   = readr::parse_number(as.character(num_leaves)),
    branch_order = as.numeric(branch_order)   # force numeric
  )          #re-run sapply above

#
##
### 3 Flammability traits names
flamm_col <- c(
  "TimeToFlaming(s)", 
  "PostBurntMassEstimate(%)", 
  "MaximumFlameTemperature(°C)"
)

############################################RQ1

#Do leaf traits explain variation in flammability attributes, and which traits are most influential?
##
###linear mixed-effects models with species as random effects 
###adding subregion as second randoms complicates model

#m_ignitability <- lmer(`TimeToFlaming(s)` ~
#    `FMC(%)` + branch_order + percent_N + percent_C + C_to_N_ratio +
#    d_15N_14N + d_13C_12C +
#    leaf_area_cm2 + leaf_length_cm + avg_leaf_width_cm +
#    leaf_thickness_mm +
#    leaf_fresh_wgt_g + leaf_dry_wgt_g +
#   twig_fresh_g + twig_dry_g +
#    lma + succulence + ldmc +
#    (1 | species_name) + (1 | subregion.x), #2 randoms
#  data = Shared_spp_df02, REML = TRUE)    #Warning message:Some predictor variables are on very different scales: consider rescaling 

#
##
###skewness test to determine is transformation is needed or not for my responses
library(e1071)   # for skewness

#
##Max Temperature
###
MT <- Shared_spp_df02$`MaximumFlameTemperature(°C)`
hist(MT, breaks = 30, main = "Histogram: Max Temperature", xlab = "°C")
skewness(MT, na.rm = TRUE)  #check residuals
qqnorm(MT); qqline(MT)

#
##Post Burn Mass
#### 
PBM <- Shared_spp_df02$`PostBurntMassEstimate(%)`
hist(PBM, breaks = 30, main = "Histogram: Post Burn Mass", xlab = "%")
skewness(PBM, na.rm = TRUE)
qqnorm(PBM); qqline(PBM)       

# 
##Time to Flaming
###
TT <- Shared_spp_df02$`TimeToFlaming(s)`
hist(TT, breaks = 30, main = "Histogram: Time to Flaming", xlab = "Seconds")
skewness(TT, na.rm = TRUE) #no transformation needed for any response variable
qqnorm(TT); qqline(TT)

#
##the above cool for noting but...
###base response transformation on model residuals

#
##Traits grouped
###
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
##scale() traits for the following steps 
###
Shared_spp_df02 <- Shared_spp_df02 %>%
  mutate(across(all_of(trait_col), 
                ~ if (is.numeric(.)) as.numeric(scale(.)) else .))

#
##
###check if trait were scaled 
sapply(Shared_spp_df02[trait_col], function(x) {
  if (is.numeric(x)) c(mean = mean(x, na.rm = TRUE),
                       sd   = sd(x, na.rm = TRUE))
  else NA
})

#
##
###alias columns for my responses (avoiding % and () in names)
Shared_spp_df02 <- Shared_spp_df02 |>
  dplyr::mutate(
    MaxTemp   = `MaximumFlameTemperature(°C)`,
    PostBurnM = `PostBurntMassEstimate(%)`,
    IgnTime   = `TimeToFlaming(s)`
  )         #i see new columns we created not replaced

#COMBUSTIBILITY full model
##
###
m_MaxTemp <- lmer(
    MaxTemp ~ 
    height_cm + canopy_axis_1_cm + canopy_axis_2_cm +
    canopy_area_cm2 + branch_order + pubescence + percent_N +
    percent_C + C_to_N_ratio + d_15N_14N + d_13C_12C +
    FMC_proportion + num_leaves + leaf_area_cm2 + 
    leaf_length_cm + avg_leaf_width_cm + max_leaf_width_cm + 
    leaf_thickness_mm + leaf_fresh_wgt_g + leaf_dry_wgt_g + 
    lma + fwc +    #removed the twiggy traits
    succulence + ldmc + lwr + twig_fwc +
    (1 | species_name),
  data = Shared_spp_df02,
  REML = FALSE
)
summary(m_MaxTemp)

r.squaredGLMM(m_MaxTemp)

###QQ plot of residuals (normality assumption)
qqnorm(resid(m_MaxTemp)); qqline(resid(m_MaxTemp))   #kinda leans with the line 

####Residuals vs fitted (homoscedasticity assumption)
plot(m_MaxTemp)       #maxtemp maybe log transformed ??

##multicollinearity
###check for VIF > 5 High multicollinearity. 
#twigs fresh and dry removed = reduced 
vif(m_MaxTemp)

#leaf weights are a bit problematic

#
##COMBUSTIBILITY selection
### 
dat_MaxTemp <- Shared_spp_df02 %>%
  select(
    MaxTemp, species_name,
    height_cm, canopy_axis_1_cm, canopy_axis_2_cm, canopy_area_cm2,
    branch_order, pubescence,
    percent_N, percent_C, C_to_N_ratio, d_15N_14N, d_13C_12C,
    FMC_proportion, num_leaves, leaf_area_cm2, leaf_length_cm,
    avg_leaf_width_cm, max_leaf_width_cm, leaf_thickness_mm,
    leaf_fresh_wgt_g, leaf_dry_wgt_g, 
    lma, fwc, succulence, ldmc, lwr, twig_fwc
  ) %>%
  tidyr::drop_na() %>%          # <- ensures all submodels use the same rows
  mutate(
    pubescence   = as.factor(pubescence),
    species_name = droplevels(as.factor(species_name))
  )

#
MaxTemp_ml <- lmer(
    MaxTemp ~ 
    height_cm + canopy_axis_1_cm + canopy_axis_2_cm + canopy_area_cm2 +
    branch_order + pubescence +
    percent_N + percent_C + C_to_N_ratio + d_15N_14N + d_13C_12C +
    FMC_proportion + num_leaves + leaf_area_cm2 + leaf_length_cm +
    avg_leaf_width_cm + max_leaf_width_cm + leaf_thickness_mm +
    leaf_fresh_wgt_g + leaf_dry_wgt_g + 
    lma + fwc + succulence + ldmc + lwr + twig_fwc +
    (1 | species_name),
  data = dat_MaxTemp,
  REML = FALSE
)

#
sel_MaxTemp <- step(MaxTemp_ml)
sel_MaxTemp                # step table

###selected MT model refit again
###visualize preferred model. ggplot? 
new_MaxTemp_m <- lmer(MaxTemp ~ 
                   FMC_proportion + (1 | species_name), 
                 data = Shared_spp_df02, 
                 REML = FALSE
)

summary(new_MaxTemp_m)

r.squaredGLMM(new_MaxTemp_m)

#
##
###visualize 
ggplot(Shared_spp_df02, aes(x = FMC_proportion, y = MaxTemp)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE, color = "blue") +
  labs(title = "Combustibility ~ FMC proportion",
       x = "FMC proportion",
       y = "Max Temperature (°C)")


#CONSUMABILITY full model
##
###
m_PostBurnM <- lmer(
  `PostBurnM` ~ 
    height_cm + canopy_axis_1_cm + canopy_axis_2_cm +
    canopy_area_cm2 + branch_order + pubescence + percent_N +
    percent_C + C_to_N_ratio + d_15N_14N + d_13C_12C +
    FMC_proportion + num_leaves + leaf_area_cm2 + 
    leaf_length_cm + avg_leaf_width_cm + max_leaf_width_cm + 
    leaf_thickness_mm + leaf_fresh_wgt_g + leaf_dry_wgt_g + 
    lma + fwc + 
    succulence + ldmc + lwr + twig_fwc +
    (1 | species_name),
  data = Shared_spp_df02,
  REML = FALSE
)
summary(m_PostBurnM)

r.squaredGLMM(m_PostBurnM)

#
##
###QQ plot of residuals (normality assumption)
qqnorm(resid(m_PostBurnM)); qqline(resid(m_PostBurnM))

# Residuals vs fitted (homoscedasticity assumption)
plot(m_PostBurnM)   #maybe no transformation yet

#
vif(m_PostBurnM)    #remove twigs fresh and dry
            
#
##CONSUMABILITY selection
###
dat_PostBurnM <- Shared_spp_df02 %>%
  select(
    PostBurnM, species_name,
    height_cm, canopy_axis_1_cm, canopy_axis_2_cm, canopy_area_cm2,
    branch_order, pubescence,
    percent_N, percent_C, C_to_N_ratio, d_15N_14N, d_13C_12C,
    FMC_proportion, num_leaves, leaf_area_cm2, leaf_length_cm,
    avg_leaf_width_cm, max_leaf_width_cm, leaf_thickness_mm,
    leaf_fresh_wgt_g, leaf_dry_wgt_g,
    lma, fwc, succulence, ldmc, lwr, twig_fwc
  ) %>%
  tidyr::drop_na() %>%          # <- ensures all submodels use the same rows
  mutate(
    pubescence   = as.factor(pubescence),
    species_name = droplevels(as.factor(species_name))
  )

#
PostBurnM_ml <- lmer(
  PostBurnM ~ 
    height_cm + canopy_axis_1_cm + canopy_axis_2_cm + canopy_area_cm2 +
    branch_order + pubescence +
    percent_N + percent_C + C_to_N_ratio + d_15N_14N + d_13C_12C +
    FMC_proportion + num_leaves + leaf_area_cm2 + leaf_length_cm +
    avg_leaf_width_cm + max_leaf_width_cm + leaf_thickness_mm +
    leaf_fresh_wgt_g + leaf_dry_wgt_g + 
    lma + fwc + succulence + ldmc + lwr + twig_fwc +
    (1 | species_name),
  data = dat_PostBurnM,
  REML = FALSE
)

#
sel_PostBurnM <- step(PostBurnM_ml)
sel_PostBurnM                 # step table

#
new_PostBurnM_m <- lmer(PostBurnM  ~ (1 | species_name), 
                           data = Shared_spp_df02, 
                           REML = FALSE
)

summary(new_PostBurnM_m)

r.squaredGLMM(new_PostBurnM_m)

#
##
###visualize 
Shared_spp_df02 %>%
  mutate(species_name = reorder(species_name, PostBurnM, FUN = median, na.rm = TRUE)) %>%
  ggplot(aes(x = species_name, y = PostBurnM, fill = growth_form)) +
  geom_boxplot() +
  labs(title = "Consumability ~ species",
       x = "Species", y = "Post-burn Mass (%)",
       fill = "Growth form") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#IGNITABILITY full model  
##
###
m_IgnTime <- lmer(
    IgnTime ~ 
    height_cm + canopy_axis_1_cm + canopy_axis_2_cm +
    canopy_area_cm2 + branch_order + pubescence + percent_N +
    percent_C + C_to_N_ratio + d_15N_14N + d_13C_12C +
    FMC_proportion + num_leaves + leaf_area_cm2 + 
    leaf_length_cm + avg_leaf_width_cm + max_leaf_width_cm + 
    leaf_thickness_mm + leaf_fresh_wgt_g + leaf_dry_wgt_g + 
    lma + fwc + 
    succulence + ldmc + lwr + twig_fwc +
    (1 | species_name),
  data = Shared_spp_df02,
  REML = FALSE
)
summary(m_IgnTime)

r.squaredGLMM(m_IgnTime)

#
##
###QQ plot of residuals (normality assumption)
qqnorm(resid(m_IgnTime)); qqline(resid(m_IgnTime))

# Residuals vs fitted (homoscedasticity assumption)
plot(m_IgnTime)   

###
vif(m_IgnTime)  #remove twigs fresh and dry

#
##IGNITABILITY selection
###
dat_IgnTime <- Shared_spp_df02 %>%
  select(
    IgnTime, species_name,
    height_cm, canopy_axis_1_cm, canopy_axis_2_cm, canopy_area_cm2,
    branch_order, pubescence,
    percent_N, percent_C, C_to_N_ratio, d_15N_14N, d_13C_12C,
    FMC_proportion, num_leaves, leaf_area_cm2, leaf_length_cm,
    avg_leaf_width_cm, max_leaf_width_cm, leaf_thickness_mm,
    leaf_fresh_wgt_g, leaf_dry_wgt_g, 
    lma, fwc, succulence, ldmc, lwr, twig_fwc
  ) %>%
  tidyr::drop_na() %>%          # <- ensures all submodels use the same rows
  mutate(
    pubescence   = as.factor(pubescence),
    species_name = droplevels(as.factor(species_name))
  )

#
IgnTime_ml <- lmer(
    IgnTime ~ 
    height_cm + canopy_axis_1_cm + canopy_axis_2_cm + canopy_area_cm2 +
    branch_order + pubescence +
    percent_N + percent_C + C_to_N_ratio + d_15N_14N + d_13C_12C +
    FMC_proportion + num_leaves + leaf_area_cm2 + leaf_length_cm +
    avg_leaf_width_cm + max_leaf_width_cm + leaf_thickness_mm +
    leaf_fresh_wgt_g + leaf_dry_wgt_g + 
    lma + fwc + succulence + ldmc + lwr + twig_fwc +
    (1 | species_name),
  data = dat_IgnTime,
  REML = FALSE
)

#
sel_IgnTime <- step(IgnTime_ml)
sel_IgnTime               # step table

#
new_IgnTime_m <- lmer(IgnTime ~ 
                             FMC_proportion + (1 | species_name), 
                           data = Shared_spp_df02, 
                           REML = FALSE
)

summary(new_IgnTime_m)

r.squaredGLMM(new_IgnTime_m)

#
##
###
ggplot(Shared_spp_df02, aes(x = FMC_proportion, y = IgnTime)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE, color = "blue") +
  labs(title = "Ignitability ~ FMC proportion",
       x = "FMC proportion",
       y = "TimeToFlaming") + 
  ylim(0, 120)


########################################RQ2

#Is plant flammability better explained by continuous trait variation or by categorical growth form?
##Growth-form models
###
gf_MaxTemp  <- lmer(`MaxTemp` ~ growth_form + (1|species_name), data = Shared_spp_df02, REML = FALSE)
gf_PostBurnM <- lmer(`PostBurnM` ~ growth_form + (1|species_name), data = Shared_spp_df02, REML = FALSE)
gf_IgnTime  <- lmer(`IgnTime` ~ growth_form + (1|species_name), data = Shared_spp_df02, REML = FALSE)

#
summary(gf_MaxTemp)
summary(gf_PostBurnM)
summary(gf_IgnTime)

# R^2 (marginal = fixed effects; conditional = fixed + random)
r.squaredGLMM(gf_MaxTemp)
r.squaredGLMM(gf_PostBurnM)
r.squaredGLMM(gf_IgnTime)


length(unique(Shared_spp_df02$species_name))   # how many unique species
unique(Shared_spp_df02$species_name)           # list them

#########################################xxxxx#################
<<<<<<< HEAD
########################
=======

>>>>>>> 1993faec244a5d6750d2d6a90e617c13ed8162bb
