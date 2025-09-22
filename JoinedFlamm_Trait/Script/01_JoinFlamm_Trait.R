#
# Data chapter 2: Investigating the potential of predicting flammability based of leaf traits
## Flamm is flammability experiments data 
### Fieldtrait and Labtrait are Henrys leaf trait data

library(tidyverse)
library(readxl)
library(lme4)
library(MuMIn)
library(car)
library(lmerTest)

#
##
### cederberg and george data combined 
Flamm <- read_csv("Data/CombinedOG.csv")

#### henry field trait data
Fieldtrait <- read.csv("Data/Field_Traits_Final.csv") 

### making lowercase and trimming spaces in species names 
Flamm <- Flamm %>%
  mutate(Accepted_name = tolower(trimws(Accepted_name)))

Fieldtrait <- Fieldtrait %>%
  mutate(scientific_name_WFO = tolower(trimws(scientific_name_WFO)))

#
## explore
str(Flamm)

str(Fieldtrait)

colnames(Fieldtrait)

colnames(Flamm)

#
##
### see Unique species from flammability & field leaf data
unique(Flamm$Accepted_name) #52 species 

unique(Fieldtrait$scientific_name_WFO)  #1000+ species 

unique(Fieldtrait$subregion)

#
##
### which species are shared between Flamm and Fieldtrait 
shared_species <- intersect(unique(Flamm$Accepted_name), 
                            unique(Fieldtrait$scientific_name_WFO))
                         #33 shared, not bad ..19??
print(shared_species)

#
##
### create a new df of just shared species from each df 
Flamm_shared <- Flamm %>% filter(Accepted_name %in% shared_species)

Fieldtrait_shared <- Fieldtrait %>% filter(scientific_name_WFO %in% shared_species)

# join the two dfs by shared species
Shared_spp_df01 <- Flamm_shared %>%
  left_join(Fieldtrait_shared,
            by = c("Accepted_name" = "scientific_name_WFO"))  %>%
  filter(Accepted_name %in% shared_species)   #not having replicate causes problems..??

print(sort(unique(Shared_spp_df01$species_name)))

colnames(Shared_spp_df01)

#
## Join Labtrait data to this shared_spp_df01 and create a new Shared_spp_02
### Shared_spp_df02 will have all flammability traits and leaf traits (field and Lab measured)

### read in Labtrait data
Labtrait <- read.csv("Data/Lab_Traits_Final.csv")

### make lower case and spacing
Labtrait <- Labtrait %>%
  mutate(scientific_name_WFO = tolower(trimws(scientific_name_WFO)))

colnames(Labtrait)

### confirm shared species with Shared_spp_df01
shared_species02 <- intersect(unique(Shared_spp_df01$Accepted_name), 
                              unique(Labtrait$scientific_name_WFO))
print(sort(unique(shared_species02)))  #Same species n=33

### join by species to create a new df
Shared_spp_df02 <- Shared_spp_df01 %>%
  left_join(Labtrait,
            by = c("Accepted_name" = "scientific_name_WFO")) %>%
  filter(Accepted_name %in% shared_species02)

colnames(Shared_spp_df02)

length(unique(Shared_spp_df02$species_name))

# 28leaf traits 

trait_col <- c(
  "height_cm", "canopy_axis_1_cm", "canopy_axis_2_cm", "canopy_area_cm2",  # 11 Field traits 
  "branch_order", "pubescence", "percent_N", "percent_C", "C_to_N_ratio",
  "d_15N_14N", "d_13C_12C",
  
  "FMC_proportion", "num_leaves", "leaf_area_cm2", "leaf_length_cm",     # 17 Lab traits
  "avg_leaf_width_cm", "max_leaf_width_cm", "leaf_thickness_mm",
  "leaf_fresh_wgt_g", "leaf_dry_wgt_g", "twig_fresh_g", "twig_dry_g", 
  "lma", "fwc", "succulence", "ldmc", "lwr", "twig_fwc"
)

###
sapply(Shared_spp_df02[trait_col], class)

### Convert all trait columns to relevant class 
Shared_spp_df02 <- Shared_spp_df02 %>%
  mutate(
    pubescence   = as.factor(pubescence),
    num_leaves   = readr::parse_number(as.character(num_leaves)),
    branch_order = as.numeric(branch_order)  
  )          #re-run sapply above

#
##
### 3 Flammability component
flamm_col <- c(
  "TimeToFlaming(s)", 
  "PostBurntMassEstimate(%)", 
  "MaximumFlameTemperature(°C)"
)


############################################RQ1

# Do leaf traits explain variation in flammability attributes, 
# and which traits are most influential?
## linear mixed-effects models with species as random effects 
### adding subregion as second randoms complicates model

# m_ignitability <- lmer(`TimeToFlaming(s)` ~
#    `FMC(%)` + branch_order + percent_N + percent_C + C_to_N_ratio +
#    d_15N_14N + d_13C_12C +
#    leaf_area_cm2 + leaf_length_cm + avg_leaf_width_cm +
#    leaf_thickness_mm +
#    leaf_fresh_wgt_g + leaf_dry_wgt_g +
#   twig_fresh_g + twig_dry_g +
#    lma + succulence + ldmc +
#    (1 | species_name) + (1 | subregion.x), #2 randoms
#  data = Shared_spp_df02, REML = TRUE)    
#Warning message:Some predictor variables are on very different scales: consider rescaling 

#
##
### skewness test to determine is transformation is needed or not for my responses
library(e1071)   # for skewness

#
## 
### Max Temperature
MT <- Shared_spp_df02$`MaximumFlameTemperature(°C)`
hist(MT, breaks = 30, main = "Histogram: Max Temperature", xlab = "°C")
skewness(MT, na.rm = TRUE)  #check residuals
qqnorm(MT); qqline(MT)

#
## 
### Post Burn Mass
PBM <- Shared_spp_df02$`PostBurntMassEstimate(%)`
hist(PBM, breaks = 30, main = "Histogram: Post Burn Mass", xlab = "%")
skewness(PBM, na.rm = TRUE)
qqnorm(PBM); qqline(PBM)       

# 
## Time to Flaming
###
TT <- Shared_spp_df02$`TimeToFlaming(s)`
hist(TT, breaks = 30, main = "Histogram: Time to Flaming", xlab = "Seconds")
skewness(TT, na.rm = TRUE) #no transformation needed for any response variable
qqnorm(TT); qqline(TT)

#
## the above cool for noting but...
### base response transformation on model residuals

#
## Traits grouped
###
structural_traits <- c(
  "height_cm", "canopy_axis_1_cm", "canopy_axis_2_cm", "canopy_area_cm2",
  "branch_order", "leaf_area_cm2", "leaf_length_cm", "avg_leaf_width_cm",
  "max_leaf_width_cm", "leaf_thickness_mm", "lwr", "pubescence", "num_leaves"
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
### scale() traits for the following steps 
Shared_spp_df02 <- Shared_spp_df02 %>%
  mutate(across(all_of(trait_col) & where(is.numeric), ~ as.numeric(scale(.))))

### check if trait were scaled 
sapply(Shared_spp_df02[trait_col], function(x) {
  if (is.numeric(x)) c(mean = mean(x, na.rm = TRUE),
                       sd   = sd(x, na.rm = TRUE))
  else NA
})

#
##
### alias columns for my responses (avoiding % and () in names)
Shared_spp_df02 <- Shared_spp_df02 |>
  dplyr::mutate(
    MaxTemp   = `MaximumFlameTemperature(°C)`,
    PostBurnM = `PostBurntMassEstimate(%)`,
    IgnTime   = `TimeToFlaming(s)`
  )         #i see new columns we created not replaced


#Correlation matrix of traits
## exclude pubescence (factor)
###showing correlation coefficient (-1 to 1)
numeric_traits <- trait_col[trait_col != "pubescence"]
cor_matrix <- cor(Shared_spp_df02[numeric_traits], use = "pairwise.complete.obs")

library(corrplot)
corrplot(cor_matrix, method = "color", type = "upper", tl.cex = 0.7)

corrplot(cor_matrix, method = "color", type = "upper", tl.cex = 0.7)

cor_table <- as.data.frame(as.table(cor_matrix))
print(cor_table)

write.xlsx(cor_table, "Trait_Correlation_Table.xlsx")


#run full model with reduced collinearity 
##
###IGNITABILITY(2) full model  and species not random
##
###
IgnTime2 <- lm(
  IgnTime ~ 
    height_cm +
    canopy_area_cm2 + branch_order + pubescence + percent_N +
    percent_C + d_15N_14N + d_13C_12C +
    FMC_proportion + num_leaves + leaf_area_cm2 + 
    leaf_length_cm + 
    leaf_dry_wgt_g + 
    lma + succulence + ldmc + lwr +
    species_name,
  data = Shared_spp_df02,
  REML = FALSE
)
summary(IgnTime2)

vif(IgnTime2) #suggest leaf length


MaxTemp2 <- lm(
  IgnTime ~ 
    height_cm +
    canopy_area_cm2 + branch_order + pubescence + percent_N +
    percent_C + d_15N_14N + d_13C_12C +
    FMC_proportion + num_leaves + leaf_area_cm2 + 
    leaf_length_cm + 
    leaf_dry_wgt_g + 
    lma + succulence + ldmc + lwr +
    species_name,
  data = Shared_spp_df02,
  REML = FALSE
)
summary(MaxTemp2)

vif(MaxTemp2)


Comb2 <- lm(
  IgnTime ~ 
    height_cm +
    canopy_area_cm2 + branch_order + pubescence + percent_N +
    percent_C + d_15N_14N + d_13C_12C +
    FMC_proportion + num_leaves + leaf_area_cm2 + 
    leaf_length_cm + 
    leaf_dry_wgt_g + 
    lma + succulence + ldmc + lwr +
    species_name,
  data = Shared_spp_df02,
  REML = FALSE
)
summary(Comb2)

vif(Comb2)



# IGNITABILITY full model  
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

qqnorm(resid(m_IgnTime)); qqline(resid(m_IgnTime))

plot(m_IgnTime)   

vif(m_IgnTime)  #remove twigs fresh and dry

#
## IGNITABILITY selection
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
    lma + fwc +    #removed the twiggy traits = multicollinear 
    succulence + ldmc + lwr + twig_fwc +
    (1 | species_name),
  data = Shared_spp_df02,
  REML = FALSE
)
summary(m_MaxTemp)

r.squaredGLMM(m_MaxTemp)

qqnorm(resid(m_MaxTemp)); qqline(resid(m_MaxTemp))  # QQ plot of residuals (normality assumption)
                                                    # kinda leans with the line 
plot(m_MaxTemp)       # Residuals vs fitted (homoscedasticity assumption)
                                  # maxtemp maybe log transformed ??

vif(m_MaxTemp)        #check for VIF > 5 High multicollinearity. 
                      #twigs fresh and dry removed = reduced 
#leaf weights are a bit problematic

#correlation matrix values r values 

#
## COMBUSTIBILITY selection
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
  tidyr::drop_na() %>%          # ensures all submodels use the same rows
  mutate(
    pubescence   = as.factor(pubescence),
    species_name = droplevels(as.factor(species_name))
  )

###
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

sel_MaxTemp <- step(MaxTemp_ml)
sel_MaxTemp                # step table

### selected MT model refit again
new_MaxTemp_m <- lmer(MaxTemp ~ 
                   FMC_proportion + (1 | species_name), 
                 data = Shared_spp_df02, 
                 REML = FALSE
)

summary(new_MaxTemp_m)

r.squaredGLMM(new_MaxTemp_m)

### visualize 
ggplot(Shared_spp_df02, aes(x = FMC_proportion, y = MaxTemp)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE, color = "blue") +
  labs(title = "Combustibility ~ FMC proportion",
       x = "FMC proportion",
       y = "Max Temperature (°C)")


# CONSUMABILITY full model
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
### QQ plot of residuals (normality assumption)
qqnorm(resid(m_PostBurnM)); qqline(resid(m_PostBurnM))

plot(m_PostBurnM)   #maybe no transformation yet

vif(m_PostBurnM)    #remove twigs fresh and dry
            
#
## CONSUMABILITY selection
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

### visualize 
Shared_spp_df02 %>%
  mutate(species_name = reorder(species_name, PostBurnM, FUN = median, na.rm = TRUE)) %>%
  ggplot(aes(x = species_name, y = PostBurnM, fill = growth_form)) +
  geom_boxplot() +
  labs(title = "Consumability ~ species",
       x = "Species", y = "Post-burn Mass (%)",
       fill = "Growth form") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


########################################RQ2

# Is plant flammability better explained by continuous trait variation or by categorical growth form?
## Growth-form models
###
gf_MaxTemp  <- lmer(`MaxTemp` ~ growth_form + (1|species_name), 
                    data = Shared_spp_df02, REML = FALSE)
gf_PostBurnM <- lmer(`PostBurnM` ~ growth_form + (1|species_name), 
                     data = Shared_spp_df02, REML = FALSE)
gf_IgnTime  <- lmer(`IgnTime` ~ growth_form + (1|species_name), 
                    data = Shared_spp_df02, REML = FALSE)

#
summary(gf_IgnTime)
summary(gf_MaxTemp)
summary(gf_PostBurnM)

# R^2 (marginal = fixed effects; conditional = fixed + random)
r.squaredGLMM(gf_IgnTime)
r.squaredGLMM(gf_MaxTemp)
r.squaredGLMM(gf_PostBurnM)



length(unique(Shared_spp_df02$species_name))   # how many unique species

#########################################xxxxx#################
<<<<<<< HEAD
########################
=======

>>>>>>> 1993faec244a5d6750d2d6a90e617c13ed8162bb

##################################################################
# Adapted from Raubenheimer et al 2025
## To investigate the underlying structure of the leaf trait data
### PCA All species pooled

library(FactoMineR)    #For PCA
library(factoextra)    #For visualization
library(missMDA)       #install.packages("missMDA")

#
##
### Extract only trait data for PCA
trait_data <- Shared_spp_df02 %>%
  select(all_of(trait_col)) %>%
  mutate(across(everything(), as.numeric))

sapply(trait_data, class)

colSums(is.na(trait_data))  #must check for missing values

# Estimate optimal number of dimensions for imputation
ncp_est <- estim_ncpPCA(trait_data, scale = TRUE)
ncp_est$ncp             #View recommended number of components

# Use the recommended number to impute missing values
trait_imputed <- imputePCA(trait_data, ncp = 5, scale = TRUE)$completeObs

colSums(is.na(trait_imputed))

trait_complete <- trait_imputed   #Extract the completed dataset

# Run PCA
pca_result <- PCA(trait_complete, scale.unit = TRUE, graph = FALSE)

# Extract PCA individual coordinates
pca_scores <- as.data.frame(pca_result$ind$coord)

# Add back growth form and species name
pca_scores$species_name <- Shared_spp_df02$species_name
pca_scores$growth_form <- Shared_spp_df02$growth_form

#visualise  PCA
fviz_pca_ind(pca_result,
             geom.ind = "point",
             col.ind = pca_scores$growth_form, # color by growth form
             addEllipses = TRUE, # add grouping ellipses
             legend.title = "Growth Form")

##########################################################
#
## Species mean PCA rather
### Keep only numeric traits from your trait_col list
sppgrwth <- Shared_spp_df02 %>%
  dplyr::select(species_name, growth_form)

# keep rows aligned with 'trait_imputed' (if trait_imputed came 
# from trait_data built from Shared_spp_df02)
stopifnot(nrow(sppgrwth) == nrow(trait_imputed))

pca_full <- cbind(sppgrwth, trait_imputed)

#all traits averaged per species
species_means <- pca_full %>%
 group_by(species_name, growth_form) %>%
 summarise(across(where(is.numeric), mean), .groups = "drop")

#building PCA 
pca_matrix <- species_means %>%
  select(where(is.numeric)) %>%
  as.data.frame()

rownames(pca_matrix) <- species_means$species_name

#run PCA
species_pca <- PCA(pca_matrix, scale.unit = TRUE, graph = FALSE)

# 
fviz_pca_ind(species_pca,
           geom.ind    = "point",
           col.ind     = species_means$growth_form, 
           addEllipses = TRUE,
           legend.title = "Growth form",
           title        = "PCA of Species-Mean Traits"
)


#trait loadings included
fviz_pca_biplot(
  species_pca,
  geom.ind  = "point",
  col.ind   = species_means$growth_form,
  label     = "var",      # show variable arrows only
  repel     = TRUE,
  legend.title = "Growth form",
  title     = "PCA Biplot: Species Means & Trait Loadings"
)

#varience explained
pca_result$eig   #dont really get this

species_pca$eig

#
##
###PCA + trait loadings 
fviz_pca_biplot(
  species_pca,
  geom.ind    = "point",
  col.ind     = species_means$growth_form, # color by growth form
  addEllipses = TRUE,                      # draw ellipses
  label       = "var",                      # show variable arrows
  repel       = TRUE,
  legend.title = "Growth form",
  title       = "PCA Biplot: Species Means & Trait Loadings"
)

#############################################################
#
##
###flam traits pca only
flamm_means <- Shared_spp_df02 %>%
  group_by(species_name) %>%
  summarise(across(all_of(flamm_col), mean, na.rm = TRUE))


# Run PCA
flamm_pca <- PCA(flamm_means [, flamm_col], scale.unit = TRUE, graph = FALSE)

fviz_pca_biplot(
  flamm_pca,
  geom.ind    = "point",      # species as points
  col.ind     = species_means$growth_form,
  label       = "var",        # show flammability trait arrows
  repel       = TRUE,
  legend.title = "Growth form",
  title       = "PCA of Flammability Traits"
)


###########################################



