#
## 
###20251107
 # install.packages(c("tidyverse","ggplot2",
 # "betareg","glmmTMB","ggeffects"))

library(tidyverse)
library(readxl)
library(betareg)
library(glmmTMB)
library(ggeffects)
library(lmerTest)
library(corrplot)

#####
Flamm_allNew <- read_excel("Data/Flamm_all_new.xlsx")
Trait_all_noNA <- read_excel("Data/Trait_all_noNA.xlsx")

# Trait_all <- read_excel("Data/Trait_all.xlsx")

colnames(Flamm_all)
colnames(Trait_all_noNA)


# alias columns for my responses (avoiding % and () in names)
Flamm_all <- Flamm_all |>
  dplyr::mutate(
    IgnTime   = `TimeToFlaming(s)`,
    MaxTemp   = `MaximumFlameTemperature(°C)`,
    PostBurnM = `PostBurntMassEstimate(%)`
    
  )         #i see new columns we created not replaced

#see shared species between flamm and trait all
shared_species_all <- intersect(Flamm_all$SpeciesNames, Trait_all_noNA$SpeciesNames)
shared_species_all

length(shared_species_all)   #41 #39 with noNa

#now join Flamm All and trait all by shares species 
megadata <- inner_join(Flamm_all, Trait_all_noNA, by = "SpeciesNames")

colnames(megadata)
unique(megadata$SpeciesNames)

sapply(megadata, class)

#######
# to relevent class
megadata <- megadata %>%
  mutate(
    pubescence   = as.factor(pubescence),
    num_leaves   = readr::parse_number(as.character(num_leaves)),
    branch_order = as.numeric(branch_order)  
  )        


######
# histogram 
numeric_col <- sapply(megadata, is.numeric)
par(mfrow = c(3, 3))                  # 9 plots per page
for (col in names(megadata)[numeric_col]) {
  hist(megadata[[col]], main = col, xlab = "", col = "grey80")
}
par(mfrow = c(1, 1))

#####
# scale and clap response 
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

# Should all return FALSE:
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

#after cormatrix 16 
trait_col_reduced <- c(
  "height_cm", "canopy_area_cm2",  # 08 Field traits 
  "branch_order", "pubescence", "percent_N", "percent_C",
  "d_15N_14N", "d_13C_12C",
  
  "FMC_proportion", "num_leaves", "leaf_area_cm2",  # 8 Lab traits
  "lma", "fwc", "succulence", "ldmc", "lwr"
)

#######
# scale predictors scale() traits for the following steps 
megadata <- megadata %>%
  mutate(across(all_of(trait_col) & where(is.numeric), ~ as.numeric(scale(.))))

# check if trait were scaled 
sapply(megadata[trait_col], function(x) {
  if (is.numeric(x)) c(mean = mean(x, na.rm = TRUE),
                       sd   = sd(x, na.rm = TRUE))
  else NA
})


################
# betareg
# species as random effect
IgnTMB <- glmmTMB(
  IgnTime01 ~ height_cm + canopy_area_cm2 + branch_order +
    pubescence + percent_N + percent_C + d_15N_14N + d_13C_12C + 
    FMC_proportion + num_leaves + leaf_area_cm2 + lma +
    fwc + succulence + ldmc + lwr +
    (1 | SpeciesNames),
  data   = megadata,
  family = beta_family(link = "logit")
)
summary(IgnTMB)

#
MxTMB <- glmmTMB(
  MaxTemp01 ~ height_cm + canopy_area_cm2 + branch_order +
    pubescence + percent_N + percent_C + d_15N_14N + d_13C_12C + 
    FMC_proportion + num_leaves + leaf_area_cm2 + lma +
    fwc + succulence + ldmc + lwr +
    (1 | SpeciesNames),
  data   = megadata,
  family = beta_family(link = "logit")
)
summary(MxTMB)
 
#
PbTMB <- glmmTMB(
  PostBurnM01 ~ height_cm + canopy_area_cm2 + branch_order +
    pubescence + percent_N + percent_C + d_15N_14N + d_13C_12C + 
    FMC_proportion + num_leaves + leaf_area_cm2 + lma +
    fwc + succulence + ldmc + lwr +
    (1 | SpeciesNames),
  data   = megadata,
  family = beta_family(link = "logit")
)
summary(PbTMB)

#######
#
 