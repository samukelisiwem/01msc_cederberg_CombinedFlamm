#  
#################### 20251107 ###########################################
# 
# install.packages(c("tidyverse", "patchwork", "ggplot2",
# "betareg","glmmTMB","effects"))

library(tidyverse)
library(patchwork)
library(readxl)
library(betareg)
library(glmmTMB)
library(effects)
library(lmerTest)
library(corrplot)


#####
Flamm_allNew <- read_excel("Output/Flamm_all NEW.xlsx") #some updates were done on the sheet outside of R
Trait_all_noNA <- read_excel("Output/Trait_all_noNA.xlsx")

colnames(Flamm_allNew)        
unique(Flamm_allNew$SpeciesNames)  #103spp 

colnames(Trait_all_noNA)



#see shared species between flamm and trait all
shared_species_all <- intersect(Flamm_allNew$SpeciesNames, Trait_all_noNA$SpeciesNames)
shared_species_all

length(shared_species_all)  #58

#now join Flamm All and trait all_noNa by shares species 
megadata <- inner_join(Flamm_allNew, Trait_all_noNA, by = "SpeciesNames",
                       relationship = "many-to-many")  #silences the warning

megadata <- megadata %>%
  mutate(SpeciesNames = tolower(SpeciesNames))

colnames(megadata)

unique(megadata$SpeciesNames)#58

sapply(megadata, class)


megadata %>%
  count(SpeciesNames, name = "number of reps") %>%
  arrange(`number of reps`) %>%
  print(n = Inf) #this is number of row entries for each species ...


###############################################################################

#visualize flamm traits
names(megadata)

ggplot(megadata, aes(x = reorder(SpeciesNames, IgnTime),
                     y = IgnTime)) +
  geom_boxplot() +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Species (low → high)",y = "Time to flamming (s)")

#
ggplot(megadata, aes(x = reorder(SpeciesNames, MaxTemp  ),y = MaxTemp)) +
  geom_boxplot() +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = " ",y = "Maximum flame temperature (°C)")


#
ggplot(megadata, aes(x = reorder(SpeciesNames, PostBurnM),y = PostBurnM)) +
  geom_boxplot() +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = " ",y = "Completeness of burn (%)")


###############################################################################

# to relevant class
megadata <- megadata %>%
  mutate(
    pubescence   = as.factor(pubescence),
    num_leaves   = readr::parse_number(as.character(num_leaves)),
    branch_order = as.numeric(branch_order)  
  )

colnames(megadata)

###############################################################################
# histogram 
numeric_col <- sapply(megadata, is.numeric)
par(mfrow = c(3, 3))                  # 9 plots per page
for (col in names(megadata)[numeric_col]) {
  hist(megadata[[col]], main = col, xlab = "", col = "grey80")
}
par(mfrow = c(1, 1)) 


###############################################################################
# scale and clamp response 
# SCALE TO 0–1 

# Ignition time (invert so fast ignition = higher value)
max_time <- 120
megadata$IgnTime01 <- 1 - (megadata$IgnTime / max_time)

# Max temperature (min–max scaling)
temp_range <- range(megadata$MaxTemp, na.rm = TRUE)
megadata$MaxTemp01 <- (megadata$MaxTemp - temp_range[1]) / diff(temp_range)

# Post-burn mass -> proportion consumed
megadata$PostBurnM01 <- (megadata$PostBurnM) / 100

#
## CLAMP TO AVOID 0 OR 1 
eps <- 0.0001  

clampFlamm <- c("IgnTime01", "MaxTemp01", "PostBurnM01")

megadata[clampFlamm] <- lapply(megadata[clampFlamm], function(x)
  pmin(pmax(x, eps), 1 - eps)
)

summary(megadata[clampFlamm])

# if clamped Should all return FALSE
sapply(megadata[clampFlamm], function(x) any(x <= 0 | x >= 1, na.rm = TRUE))


###############################################################################
# correlation matrix 

# 28 leaf traits grouped 
trait_col <- c(
  "height_cm", "canopy_axis_1_cm", "canopy_axis_2_cm", "canopy_area_cm2",  # 11 Field traits 
  "branch_order", "pubescence", "percent_N", "percent_C", "C_to_N_ratio",
  "d_15N_14N", "d_13C_12C",
  
  "FMC_proportion", "num_leaves", "leaf_area_cm2", "leaf_length_cm",     # 17 Lab traits
  "avg_leaf_width_cm", "max_leaf_width_cm", "leaf_thickness_mm",
  "leaf_fresh_wgt_g", "leaf_dry_wgt_g", "twig_fresh_g", "twig_dry_g", 
  "lma", "fwc", "succulence", "ldmc", "lwr", "twig_fwc"
)

# exclude pubescence and num_leaves (factor)
numeric_traits01 <- trait_col[!trait_col %in% c("pubescence", "num_leaves")]

# showing correlation coefficients (-1 inverse relationship red and 1 positive blue)
str(megadata[numeric_traits01])

cor_matrix01 <- cor(megadata[numeric_traits01], use = "pairwise.complete.obs")

corrplot(cor_matrix01, method = "pie", type = "upper", tl.cex = 0.7)

#after cormatrix 16 traits left
trait_col_reduced <- c(
  "height_cm", "canopy_area_cm2",  # 08 Field traits 
  "branch_order", "pubescence", "percent_N", "percent_C",
  "d_15N_14N", "d_13C_12C",
  
  "FMC_proportion", "num_leaves", "leaf_area_cm2",  # 8 Lab traits
  "lma", "fwc", "succulence", "ldmc", "lwr")


###############################################################################
# Check classes first
sapply(megadata[trait_col_reduced], class)

#
unique(megadata$pubescence) #Levels: Both Low None S

# megadata <- megadata %>%
# mutate(across(all_of(trait_col_reduced) & where(is.numeric), ~ as.numeric(scale(.))))

# Identify which of the trait columns are numeric
num_traits <- trait_col_reduced[sapply(megadata[trait_col_reduced], is.numeric)]
num_traits

# Scale ONLY those numeric traits
megadata <- megadata %>%
  mutate(across(all_of(num_traits), ~ as.numeric(scale(.x))))


# exclude fmc prop from scaling
# megadata <- megadata %>%
#   mutate(across(all_of(setdiff(num_traits, "FMC_proportion")),
#                 ~ as.numeric(scale(.))))


# check if trait were scaled 
sapply(megadata[num_traits], function(x) {
  if (is.numeric(x)) c(mean = mean(x, na.rm = TRUE),
                       sd   = sd(x, na.rm = TRUE))
  else NA
})


###############################################################################
library(car)

# ign vs fmc check
ggplot(megadata, aes(x = FMC_proportion, y = IgnTime01)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    x = "Fuel moisture content (proportion)",
    y = "Ignitability") +
  theme_bw()


###############################################################################
# Fit a simple linear model just to get VIFs (betareg doesn’t have vif)
# VIF > 5	strong multicollinearity exists

###########lm1
lm_Ign <- lm(
  IgnTime01 ~ height_cm + canopy_area_cm2 + branch_order +
    pubescence + percent_N + percent_C + d_15N_14N + d_13C_12C + 
    FMC_proportion + num_leaves + leaf_area_cm2 + lma +
    fwc + ldmc + lwr,  #remove succulence
  data = megadata
)

vif(lm_Ign)


########lm2
lm_MT <- lm(MaxTemp01 ~
    height_cm + canopy_area_cm2 + branch_order +
    pubescence + percent_N + percent_C + d_15N_14N + d_13C_12C + 
    FMC_proportion + num_leaves + leaf_area_cm2 + lma +
    fwc + ldmc + lwr, #remove succulence
  data = megadata
)

vif(lm_MT)

######## lm3
lm_PB <- lm(PostBurnM01 ~
    height_cm + canopy_area_cm2 + branch_order +
    pubescence + percent_N + percent_C + d_15N_14N + d_13C_12C + 
    FMC_proportion + num_leaves + leaf_area_cm2 + lma +
    fwc + ldmc + lwr, #remove succulence
  data = megadata
)

vif(lm_PB)


###############################################################################
################### betareg

######## IgnTime - Ignitability
beta_Ign <- betareg(IgnTime01 ~ 
                    height_cm + canopy_area_cm2 + branch_order +
                    pubescence + percent_N + percent_C + d_15N_14N + d_13C_12C + 
                    FMC_proportion + num_leaves + leaf_area_cm2 + lma +
                    fwc + ldmc + lwr,
                    data = megadata, link = "logit")
summary(beta_Ign)

######## MaxT -Combustibility 
beta_MaxT <- betareg(MaxTemp01 ~ 
                       height_cm + canopy_area_cm2 + branch_order +
                       pubescence + percent_N + percent_C + d_15N_14N + d_13C_12C + 
                       FMC_proportion + num_leaves + leaf_area_cm2 + lma +
                       fwc + ldmc + lwr,
                       data   = megadata, link = "logit")
summary(beta_MaxT)

###### Consumability 
beta_PostBM <- betareg(PostBurnM01 ~ 
                         height_cm + canopy_area_cm2 + branch_order +
                         pubescence + percent_N + percent_C + d_15N_14N + d_13C_12C + 
                         FMC_proportion + num_leaves + leaf_area_cm2 + lma +
                         fwc + ldmc + lwr,
                         data   = megadata, link = "logit")
summary(beta_PostBM)


###############################################################################
# effects plots

library(ggeffects)

eff_fmc <- ggpredict(beta_Ign, terms = "FMC_proportion")
plot(eff_fmc)

###############################################################################


################### residuals
par(mfrow=c(2,2))
plot(beta_Ign)

plot(beta_MaxT)

plot(beta_PostBM)

par(mfrow=c(1,1))

###############################################################################

########## betareg with random factors 
#  collection event (Site) as a random effect due to differences in devices
# glmmTMB () for beta distribution + random
Ign_site <- glmmTMB(IgnTime01 ~ 
                      height_cm + canopy_area_cm2 + branch_order + pubescence + 
                      percent_N + percent_C + d_15N_14N + d_13C_12C + 
                      FMC_proportion + num_leaves + leaf_area_cm2 + lma +fwc + 
                      ldmc + lwr +
                      (1 | Site), 
                      data = megadata, # site random effect
                      family = beta_family(link = "logit"))

summary(Ign_site)

# species as random
Ign_spp <- glmmTMB(IgnTime01 ~ 
                      height_cm + canopy_area_cm2 + branch_order + pubescence + 
                      percent_N + percent_C + d_15N_14N + d_13C_12C + 
                      FMC_proportion + num_leaves + leaf_area_cm2 + lma + fwc + 
                      ldmc + lwr +
                      (1 | SpeciesNames), 
                      data = megadata,
                      family = beta_family(link = "logit"))

summary(Ign_spp)


################ MT
MT_site <- glmmTMB(MaxTemp01 ~ 
                      height_cm + canopy_area_cm2 + branch_order + pubescence + 
                      percent_N + percent_C + d_15N_14N + d_13C_12C + 
                      FMC_proportion + num_leaves + leaf_area_cm2 + lma +fwc + 
                      ldmc + lwr +
                      (1 | Site), 
                    data = megadata, # site random effect
                    family = beta_family(link = "logit"))

summary(MT_site)

# species as random
MT_spp <- glmmTMB(MaxTemp01 ~ 
                     height_cm + canopy_area_cm2 + branch_order + pubescence + 
                     percent_N + percent_C + d_15N_14N + d_13C_12C + 
                     FMC_proportion + num_leaves + leaf_area_cm2 + lma + fwc + 
                     ldmc + lwr +
                     (1 | SpeciesNames), 
                   data = megadata,
                   family = beta_family(link = "logit"))

summary(MT_spp)


################ PBurn
PB_site <- glmmTMB(PostBurnM01 ~ 
                     height_cm + canopy_area_cm2 + branch_order + pubescence + 
                     percent_N + percent_C + d_15N_14N + d_13C_12C + 
                     FMC_proportion + num_leaves + leaf_area_cm2 + lma +fwc + 
                     ldmc + lwr +
                     (1 | Site), 
                   data = megadata, # site random effect
                   family = beta_family(link = "logit"))

summary(PB_site) 

# species as random
PB_spp <- glmmTMB(PostBurnM01 ~ 
                    height_cm + canopy_area_cm2 + branch_order + pubescence + 
                    percent_N + percent_C + d_15N_14N + d_13C_12C + 
                    FMC_proportion + num_leaves + leaf_area_cm2 + lma + fwc + 
                    ldmc + lwr +
                    (1 | SpeciesNames), 
                  data = megadata,
                  family = beta_family(link = "logit"))

summary(PB_spp)


####### AIC
AIC(beta_Ign, Ign_site, Ign_spp)
AIC(beta_MaxT, MT_site, MT_spp)
AIC(beta_PostBM, PB_site, PB_spp)

