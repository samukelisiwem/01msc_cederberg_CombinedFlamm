#
## 
###20251107
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
Flamm_allNew <- read_excel("Output/Flamm_all_new.xlsx")
Trait_all_noNA <- read_excel("Output/Trait_all_noNA.xlsx")

# Trait_all <- read_excel("Data/Trait_all.xlsx")
colnames(Flamm_allNew)
colnames(Trait_all_noNA)


# alias columns for my responses (avoiding % and () in names)
Flamm_allNew <- Flamm_allNew |>
  dplyr::mutate(
    IgnTime   = `TimeToFlaming(s)`,
    MaxTemp   = `MaximumFlameTemperature(°C)`,
    PostBurnM = `PostBurntMassEstimate(%)`
    
  )         #i see new columns we created not replaced

#see shared species between flamm and trait all
shared_species_all <- intersect(Flamm_allNew$SpeciesNames, Trait_all_noNA$SpeciesNames)
shared_species_all

length(shared_species_all)   #41 #39 with noNa

#now join Flamm All and trait all by shares species 
megadata <- inner_join(Flamm_allNew, Trait_all_noNA, by = "SpeciesNames")

colnames(megadata)
unique(megadata$SpeciesNames)

sapply(megadata, class)

#######
# to relevant class
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

# if clammped Should all return FALSE
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
# scale predictors scale() traits for the following steps 
# megadata <- megadata %>%
#   mutate(across(all_of(trait_col_reduced) & where(is.numeric), ~ as.numeric(scale(.))))

#exclude fmc prop from scaling
megadata <- megadata %>%
  mutate(across(all_of(setdiff(trait_col_reduced, "FMC_proportion")), 
                ~ as.numeric(scale(.))))


# check if trait were scaled 
sapply(megadata[trait_col_reduced], function(x) {
  if (is.numeric(x)) c(mean = mean(x, na.rm = TRUE),
                       sd   = sd(x, na.rm = TRUE))
  else NA
})


##########################################
################### betareg

beta_m1 <- betareg(IgnTime01 ~ height_cm + canopy_area_cm2 + branch_order +
    pubescence + percent_N + percent_C + d_15N_14N + d_13C_12C + 
    FMC_proportion + num_leaves + leaf_area_cm2 + lma +
    fwc + succulence + ldmc + lwr,
  data   = megadata, link = "logit"
)
summary(beta_m1)

# predictor effects plots
par(mfrow = c(3, 3))
plot(predictorEffects (beta_m1))

# par(mfrow = c(1, 1))


#
# I <- ggplot(megadata, aes(x = FMC_proportion, y = IgnTime01)) +
#   geom_point(alpha = 0.5) +
#   geom_smooth(method = "loess", se = TRUE, color = "blue") +
#   labs(x = "Fuel Moisture Content (proportion)",
#        y = "Ignition Time (scaled 0–1)",
#        title = "Relationship between FMC and Ignition Time") +
#   theme_minimal()
# print(I)
#


sm <- summary(beta_m1)
coefs <- as.data.frame(sm$coefficients$mean)
coefs$term <- rownames(coefs)
coefs %>%
  filter(term != "(Intercept)") %>%
  mutate(abs_est = abs(Estimate)) %>%
  arrange(desc(abs_est)) %>%
  ggplot(aes(x = reorder(term, abs_est), y = Estimate)) +
  geom_point(size = 3) + geom_hline(yintercept = 0, lty = 2) +
  coord_flip() + theme_minimal() +
  labs(x = "Trait", y = "Standardized coefficient",
       title = "Relative influence of traits on ignition time")

################ plot

# Boxplot for categorical variable (pubescence)
p_pub <- ggplot(megadata, aes(x = pubescence, y = IgnTime01)) +
  geom_boxplot(alpha = 0.7, fill = "tan") +
  labs(title = "pubescence", x = "Pubescence", y = "Ignition time (0–1)") +
  theme_minimal()

p_canopy <- ggplot(megadata, aes(canopy_area_cm2, IgnTime01)) +
  geom_point(alpha = 0.5) + geom_smooth(method = "loess", color = "blue") +
  labs(title = "canopy_area_cm2", x = "Canopy area (cm²)", y = "Ignition time") +
  theme_minimal()

p_branch <- ggplot(megadata, aes(branch_order, IgnTime01)) +
  geom_point(alpha = 0.5) + geom_smooth(method = "loess", color = "darkgreen") +
  labs(title = "branch_order", x = "Branch order", y = "Ignition time") +
  theme_minimal()

p_N <- ggplot(megadata, aes(percent_N, IgnTime01)) +
  geom_point(alpha = 0.5) + geom_smooth(method = "loess", color = "green4") +
  labs(title = "percent_N", x = "% Nitrogen", y = "Ignition time") +
  theme_minimal()

p_C <- ggplot(megadata, aes(percent_C, IgnTime01)) +
  geom_point(alpha = 0.5) + geom_smooth(method = "loess", color = "forestgreen") +
  labs(title = "percent_C", x = "% Carbon", y = "Ignition time") +
  theme_minimal()

p_d15 <- ggplot(megadata, aes(d_15N_14N, IgnTime01)) +
  geom_point(alpha = 0.5) + geom_smooth(method = "loess", color = "purple") +
  labs(title = "d_15N_14N", x = "δ15N/14N", y = "Ignition time") +
  theme_minimal()

p_d13 <- ggplot(megadata, aes(d_13C_12C, IgnTime01)) +
  geom_point(alpha = 0.5) + geom_smooth(method = "loess", color = "magenta") +
  labs(title = "d_13C_12C", x = "δ13C/12C", y = "Ignition time") +
  theme_minimal()

p_FMC <- ggplot(megadata, aes(FMC_proportion, IgnTime01)) +
  geom_point(alpha = 0.5) + geom_smooth(method = "loess", color = "red") +
  labs(title = "FMC_proportion", x = "Fuel moisture content", y = "Ignition time") +
  theme_minimal()

p_num <- ggplot(megadata, aes(num_leaves, IgnTime01)) +
  geom_point(alpha = 0.5) + geom_smooth(method = "loess", color = "orange") +
  labs(title = "num_leaves", x = "Number of leaves", y = "Ignition time") +
  theme_minimal()

p_area <- ggplot(megadata, aes(leaf_area_cm2, IgnTime01)) +
  geom_point(alpha = 0.5) + geom_smooth(method = "loess", color = "darkorange") +
  labs(title = "leaf_area_cm2", x = "Leaf area (cm²)", y = "Ignition time") +
  theme_minimal()

p_lma <- ggplot(megadata, aes(lma, IgnTime01)) +
  geom_point(alpha = 0.5) + geom_smooth(method = "loess", color = "brown") +
  labs(title = "lma", x = "LMA", y = "Ignition time") +
  theme_minimal()

p_succ <- ggplot(megadata, aes(succulence, IgnTime01)) +
  geom_point(alpha = 0.5) + geom_smooth(method = "loess", color = "deepskyblue4") +
  labs(title = "succulence", x = "Succulence", y = "Ignition time") +
  theme_minimal()

p_ldmc <- ggplot(megadata, aes(ldmc, IgnTime01)) +
  geom_point(alpha = 0.5) + geom_smooth(method = "loess", color = "black") +
  labs(title = "ldmc", x = "LDMC", y = "Ignition time") +
  theme_minimal()

p_lwr <- ggplot(megadata, aes(lwr, IgnTime01)) +
  geom_point(alpha = 0.5) + geom_smooth(method = "loess", color = "grey30") +
  labs(title = "lwr", x = "LWR", y = "Ignition time") +
  theme_minimal()

# --------------------------
# 2️⃣ Combine with patchwork
# --------------------------
(p_pub | p_canopy | p_branch) /
  (p_N | p_C | p_d15) /
  (p_d13 | p_FMC | p_num) /
  (p_area | p_lma | p_succ) /
  (p_ldmc | p_lwr)


# collection event (Site) as a random effect due to differences in devices

# species as random effect
# IgnTMB <- glmmTMB(
#   IgnTime01 ~ height_cm + canopy_area_cm2 + branch_order +
#     pubescence + percent_N + percent_C + d_15N_14N + d_13C_12C + 
#     FMC_proportion + num_leaves + leaf_area_cm2 + lma +
#     fwc + succulence + ldmc + lwr +
#     (1 | SpeciesNames),
#   data   = megadata,
#   family = beta_family(link = "logit")
# )
# summary(IgnTMB)

