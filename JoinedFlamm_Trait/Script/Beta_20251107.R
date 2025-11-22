#  
# 20251107
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
Flamm_allNew <- read_excel("Output/Flamm_all NEW.xlsx") #MT values are maxtemp
Trait_all_noNA <- read_excel("Output/Trait_all_noNA.xlsx")

colnames(Flamm_allNew)        #fixed Flamm allNew 
colnames(Trait_all_noNA)


#see shared species between flamm and trait all
shared_species_all <- intersect(Flamm_allNew$SpeciesNames, Trait_all_noNA$SpeciesNames)
shared_species_all

length(shared_species_all)   #41 #39 with noNa #58

#now join Flamm All and trait all_noNa by shares species 
megadata <- inner_join(Flamm_allNew, Trait_all_noNA, by = "SpeciesNames",
                       relationship = "many-to-many")  #silences the warning

megadata <- megadata %>%
  mutate(SpeciesNames = tolower(SpeciesNames))

colnames(megadata)

unique(megadata$SpeciesNames)

sapply(megadata, class)


###################################
#visualise flamm traits
names(megadata)
unique(megadata$SpeciesNames) #57


ggplot(megadata, aes(x = reorder(SpeciesNames, IgnTime),
                     y = IgnTime)) +
  geom_boxplot() +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Species (ordered low → high)",y = "Ignition Time (s)")

#
ggplot(megadata, aes(x = reorder(SpeciesNames, MaxTemp),y = MaxTemp)) +
  geom_boxplot() +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Species (ordered low → high)",y = "Max Flame Temp (°C)")


#
ggplot(megadata, aes(x = reorder(SpeciesNames, PostBurnM),y = PostBurnM)) +
  geom_boxplot() +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Species (ordered low → high)",y = "Post-burn Mass (%)")


#######

# to relevant class
megadata <- megadata %>%
  mutate(
    pubescence   = as.factor(pubescence),
    num_leaves   = readr::parse_number(as.character(num_leaves)),
    branch_order = as.numeric(branch_order)  
  )

colnames(megadata)

######
# histogram 
numeric_col <- sapply(megadata, is.numeric)
par(mfrow = c(3, 3))                  # 9 plots per page
for (col in names(megadata)[numeric_col]) {
  hist(megadata[[col]], main = col, xlab = "", col = "grey80")
}
par(mfrow = c(1, 1))


##################################################
# scale and clamp response 
# SCALE TO 0–1 

# Ignition time (invert so fast ignition = higher value)
max_time <- 120
megadata$IgnTime01 <- 1 - (megadata$IgnTime / max_time)

# Max temperature (min–max scaling)
temp_range <- range(megadata$MaxTemp, na.rm = TRUE)
megadata$MaxTemp01 <- (megadata$MaxTemp - temp_range[1]) / diff(temp_range)

# Post-burn mass -> proportion consumed
megadata$PostBurnM01 <- (100 - megadata$PostBurnM) / 100


#
## CLAMP TO AVOID 0 OR 1 
eps <- 1e-6  

clampFlamm <- c("IgnTime01", "MaxTemp01", "PostBurnM01")

megadata[clampFlamm] <- lapply(megadata[clampFlamm], function(x)
  pmin(pmax(x, eps), 1 - eps)
)

summary(megadata[clampFlamm])

# if clamped Should all return FALSE
sapply(megadata[clampFlamm], function(x) any(x <= 0 | x >= 1, na.rm = TRUE))


######
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
  "lma", "fwc", "succulence", "ldmc", "lwr"
)


#######
# Check classes first
sapply(megadata[trait_col_reduced], class)

###
unique(megadata$pubescence) #Levels: Both Low None S

# megadata <- megadata %>%
# mutate(across(all_of(trait_col_reduced) & where(is.numeric), ~ as.numeric(scale(.))))

# Identify which of the trait columns are numeric
num_traits <- trait_col_reduced[sapply(megadata[trait_col_reduced], is.numeric)]
num_traits

# Scale ONLY those numeric traits
# megadata <- megadata %>%
#   mutate(
#     across(all_of(num_traits), ~ as.numeric(scale(.x)))
#   )


# exclude fmc prop from scaling
megadata <- megadata %>%
  mutate(across(all_of(setdiff(num_traits, "FMC_proportion")),
                ~ as.numeric(scale(.))))


# check if trait were scaled 
sapply(megadata[num_traits], function(x) {
  if (is.numeric(x)) c(mean = mean(x, na.rm = TRUE),
                       sd   = sd(x, na.rm = TRUE))
  else NA
})


##########################################

library(car)

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


################### betareg

beta_Ign <- betareg(IgnTime01 ~ 
                    height_cm + canopy_area_cm2 + branch_order +
                    pubescence + percent_N + percent_C + d_15N_14N + d_13C_12C + 
                    FMC_proportion + num_leaves + leaf_area_cm2 + lma +
                    fwc + ldmc + lwr,
                    data = megadata, link = "logit"
)
summary(beta_Ign)

# dispersion submodel 
beta_Ign2 <- betareg(IgnTime01 ~ 
                    height_cm + canopy_area_cm2 + branch_order +
                    pubescence + percent_N + percent_C + d_15N_14N + d_13C_12C + 
                    FMC_proportion + num_leaves + leaf_area_cm2 + lma +
                    fwc + ldmc + lwr |
                    FMC_proportion + ldmc + canopy_area_cm2,   # dispersion predictors
                    data = megadata,
                    link = "logit"
)

summary(beta_Ign2)




######## MaxT
# Combustibility model
beta_MaxT <- betareg(MaxTemp01 ~ 
                      height_cm + canopy_area_cm2 + branch_order +
                      pubescence + percent_N + percent_C + d_15N_14N + d_13C_12C + 
                      FMC_proportion + num_leaves + leaf_area_cm2 + lma +
                      fwc + ldmc + lwr,
                      data   = megadata, link = "logit"
)
summary(beta_MaxT)

####
beta_MaxT2 <- betareg(MaxTemp01 ~ 
                    height_cm + canopy_area_cm2 + branch_order +
                    pubescence + percent_N + percent_C + d_15N_14N + d_13C_12C +
                    FMC_proportion + num_leaves + leaf_area_cm2 + lma +
                    fwc + ldmc + lwr |
                    FMC_proportion + ldmc + canopy_area_cm2,
                    data = megadata,link = "logit"
)

summary(beta_MaxT2)




# Consumability model
beta_PostBM <- betareg(PostBurnM01 ~ 
                       height_cm + canopy_area_cm2 + branch_order +
                       pubescence + percent_N + percent_C + d_15N_14N + d_13C_12C + 
                       FMC_proportion + num_leaves + leaf_area_cm2 + lma +
                       fwc + ldmc + lwr,
                        data   = megadata, link = "logit"
)
summary(beta_PostBM)

#####
beta_PostBM2 <- betareg(PostBurnM01 ~ 
                        height_cm + canopy_area_cm2 + branch_order +
                        pubescence + percent_N + percent_C + d_15N_14N + d_13C_12C +
                        FMC_proportion + num_leaves + leaf_area_cm2 + lma +
                        fwc + ldmc + lwr |
                        FMC_proportion + ldmc + canopy_area_cm2,
                        data = megadata,link = "logit"
)



########## AIC
AIC(beta_Ign, beta_Ign2)

AIC(beta_MaxT, beta_MaxT2)

AIC(beta_PostBM, beta_PostBM2)



######### residuals
par(mfrow=c(2,2))
plot(beta_Ign2)

plot(beta_MaxT2)

plot(beta_PostBM2)

par(mfrow=c(1,1))

# predictor effects plots


par(mfrow = c(1, 1))  # reset


########################

# collection event (Site) as a random effect due to differences in devices
# glmmTMB () for beta distribution + random
Ign_beta_Si <- glmmTMB(IgnTime01 ~ 
                      height_cm + canopy_area_cm2 + branch_order +
                      pubescence + percent_N + percent_C + d_15N_14N + d_13C_12C + 
                      FMC_proportion + num_leaves + leaf_area_cm2 + lma +fwc + 
                      ldmc + lwr +
                      (1 | Site),data = megadata, # site random effect
                      family = beta_family(link = "logit")
)

summary(Ign_beta_Si)

# species as random
Ign_beta_Spp <- glmmTMB(IgnTime01 ~ height_cm + canopy_area_cm2 + branch_order +
                          pubescence + percent_N + percent_C + d_15N_14N + d_13C_12C + 
                          FMC_proportion + num_leaves + leaf_area_cm2 + lma +
                          fwc + ldmc + lwr +
                          (1 | SpeciesNames), data = megadata,
                          family = beta_family(link = "logit")
)

summary(Ign_beta_Spp)

AIC (Ign_beta_Si, Ign_beta_Spp)



