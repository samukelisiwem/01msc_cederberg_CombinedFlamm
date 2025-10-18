
# chapter 2: Investigating the potential of predicting flammability based of leaf traits

############################### Load and prepare ##############################

## Flamm is flammability experiments data 
### Fieldtrait and Labtrait are Henrys leaf trait data


library(tidyverse)
library(readxl)
library(lme4)
library(MuMIn)
library(car)
library(lmerTest)

# cederberg and george data combined 
Flamm <- read_csv("Data/CombinedOG.csv")

# henry field trait data
Fieldtrait <- read.csv("Data/Field_Traits_Final.csv") 

# making lowercase and trimming spaces in species names 
Flamm <- Flamm %>%
  mutate(Accepted_name = tolower(trimws(Accepted_name)))

Fieldtrait <- Fieldtrait %>%
  mutate(scientific_name_WFO = tolower(trimws(scientific_name_WFO)))


########################## Exploratory data analysis ##########################

str(Flamm)

str(Fieldtrait)

colnames(Fieldtrait)

colnames(Flamm)

# find unique (different) species from flammability & field leaf data
unique(Flamm$Accepted_name) #52 species 

unique(Fieldtrait$scientific_name_WFO)  #1000+ species 

unique(Fieldtrait$subregion)


############################## Merge data sets ################################

# which species are shared between Flamm and Fieldtrait df
shared_species <- intersect(unique(Flamm$Accepted_name), 
                            unique(Fieldtrait$scientific_name_WFO))
                         #33 shared, not bad ..19??
print(shared_species)

# create a new df of just shared species from each df 
Flamm_shared <- Flamm %>% filter(Accepted_name %in% shared_species)

Fieldtrait_shared <- Fieldtrait %>% filter(scientific_name_WFO %in% shared_species)

# join the two dfs by shared species
Shared_spp_df01 <- Flamm_shared %>%
  left_join(Fieldtrait_shared,
            by = c("Accepted_name" = "scientific_name_WFO"))  %>%
  filter(Accepted_name %in% shared_species)   #not having replicate causes problems..??

print(sort(unique(Shared_spp_df01$species_name)))

colnames(Shared_spp_df01)

# Join Labtrait data to this shared_spp_df01 and create a new Shared_spp_02
# Shared_spp_df02 will have all flammability traits and leaf traits (field and Lab measured)


############################## read in Labtrait data ##########################

Labtrait <- read.csv("Data/Lab_Traits_Final.csv")

# make lower case and spacing
Labtrait <- Labtrait %>%
  mutate(scientific_name_WFO = tolower(trimws(scientific_name_WFO)))

colnames(Labtrait)

# confirm shared species with Shared_spp_df01
shared_species02 <- intersect(unique(Shared_spp_df01$Accepted_name), 
                              unique(Labtrait$scientific_name_WFO))
print(sort(unique(shared_species02)))  #Same species n=33

# join by species to create a new df
Shared_spp_df02 <- Shared_spp_df01 %>%
  left_join(Labtrait,
            by = c("Accepted_name" = "scientific_name_WFO")) %>%
  filter(Accepted_name %in% shared_species02)

colnames(Shared_spp_df02)

length(unique(Shared_spp_df02$species_name)) #how many different spp

# how many spp from each growth form
sapply(split(Shared_spp_df02$species_name, Shared_spp_df02$growth_form), function(x) length(unique(x)))

# list them spp as per growth form
split(Shared_spp_df02$species_name, Shared_spp_df02$growth_form) %>%
  lapply(unique)

# creating new growth form_2 column_proteiods, ericoids, restios, herbaceous, trees
proteoids <- c("Protea laurifolia","Leucadendron glaberrimum", "Leucadendron pubescens", "Leucadendron eucalyptifolium", "Searsia lucida", "Diospyros dichrophylla", "Gymnosporia buxifolia", "Osyris compressa", "Protea nitida","Dodonaea viscosa" )   

ericoids  <- c("Passerina truncata", "Stoebe plumosa", "Elytropappus gnaphaloides", "Paranomus bracteolaris", "Erica discolor", "Cliffortia ilicifolia", "Agathosma ovata", "Metalasia muricata","Phylica axillaris", "Elytropappus rhinocerotis" )             

restios   <- c("Restio sieberi", "Willdenowia incurvata") 

herbaceous <- c("Pteridium aquilinum", "Schoenus gracillimus", "Pelargonium scabrum") # add herbs, ferns, sedges

trees <- c("Widdringtonia cedarbergensis", "Maytenus oleoides","Brabejum stellatifolium","Pterocelastrus tricuspidatus", "Tarchonanthus littoralis", "Sideroxylon inerme", "Cassine peragua", "Metrosideros angustifolia") # add all trees

# Assign growth form categories
Shared_spp_df02 <- Shared_spp_df02 %>%
  mutate(growth_form_2 = case_when(
    species_name %in% proteoids    ~ "Proteoids",
    species_name %in% ericoids     ~ "Ericoids",
    species_name %in% restios      ~ "Restios",
    species_name %in% herbaceous   ~ "Herbaceous",
    species_name %in% trees        ~ "Tree"
  ))

# how many from each growth_form2 category
sapply(split(Shared_spp_df02$species_name, Shared_spp_df02$growth_form_2), function(x) length(unique(x)))

# list names
split(Shared_spp_df02$species_name, Shared_spp_df02$growth_form_2) %>%
  lapply(unique)

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

# scale() traits for the following steps 
Shared_spp_df02 <- Shared_spp_df02 %>%
  mutate(across(all_of(trait_col) & where(is.numeric), ~ as.numeric(scale(.))))

# check if trait were scaled 
sapply(Shared_spp_df02[trait_col], function(x) {
  if (is.numeric(x)) c(mean = mean(x, na.rm = TRUE),
                       sd   = sd(x, na.rm = TRUE))
  else NA
})

sapply(Shared_spp_df02[trait_col], class)

# Convert all trait columns to relevant class 
Shared_spp_df02 <- Shared_spp_df02 %>%
  mutate(
    pubescence   = as.factor(pubescence),
    num_leaves   = readr::parse_number(as.character(num_leaves)),
    branch_order = as.numeric(branch_order)  
  )                                       #re-run sapply above

# 3 Flammability components grouped
flamm_col <- c(
  "TimeToFlaming(s)", 
  "PostBurntMassEstimate(%)", 
  "MaximumFlameTemperature(°C)"
)


#################### Do leaf traits explain variation in flammability attributes
################### and which traits are most influential? #####################

# skewness test to determine is transformation is needed or not for my responses
library(e1071)   # for skewness
 

# Max Temperature
MT <- Shared_spp_df02$`MaximumFlameTemperature(°C)`
hist(MT, breaks = 30, main = "Histogram: Max Temperature", xlab = "°C")

skewness(MT, na.rm = TRUE)  # +0.5 - 1 = transform
qqnorm(MT); qqline(MT)

# Post Burn Mass
PBM <- Shared_spp_df02$`PostBurntMassEstimate(%)`
hist(PBM, breaks = 30, main = "Histogram: Post Burn Mass", xlab = "%")

skewness(PBM, na.rm = TRUE)
qqnorm(PBM); qqline(PBM)       

# Time to Flaming
TT <- Shared_spp_df02$`TimeToFlaming(s)`
hist(TT, breaks = 30, main = "Histogram: Time to Flaming", xlab = "Seconds")

skewness(TT, na.rm = TRUE) #no transformation needed for any response variable
qqnorm(TT); qqline(TT)

# the above cool for noting for now...

# Traits grouped
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

# alias columns for my responses (avoiding % and () in names)
Shared_spp_df02 <- Shared_spp_df02 |>
  dplyr::mutate(
    MaxTemp   = `MaximumFlameTemperature(°C)`,
    PostBurnM = `PostBurntMassEstimate(%)`,
    IgnTime   = `TimeToFlaming(s)`
  )         #i see new columns we created not replaced


############################# Correlation analysis #############################

# exclude pubescence (factor)
# showing correlation coefficient (-1 inverse relationship RE to 1 positive-blue)
library(corrplot)

numeric_traits <- trait_col[trait_col != "pubescence"]

cor_matrix <- cor(Shared_spp_df02[numeric_traits], use = "pairwise.complete.obs")

corrplot(cor_matrix, method = "pie", type = "upper", tl.cex = 0.7)

cor_table <- as.data.frame(as.table(cor_matrix))
print(cor_table)

# run another corr based on the 18 remaining traits
numeric_traitscorr2 <- c(
  "height_cm",  "canopy_area_cm2","branch_order", "leaf_area_cm2", "leaf_length_cm", 
  "leaf_thickness_mm", "lwr", "num_leaves", "percent_C",  #pubescence not numberic but factor/chacter
  "percent_N", "d_13C_12C", "d_15N_14N", "FMC_proportion", "succulence", 
  "ldmc", "leaf_fresh_wgt_g", "leaf_dry_wgt_g", "lma")
  

cor_matrix2 <- cor(Shared_spp_df02[numeric_traitscorr2], use = "pairwise.complete.obs")

corrplot(cor_matrix2, method = "pie", type = "upper", tl.cex = 0.7)


# write.xlsx(cor_table, "Trait_Correlation_Table.xlsx")


############################### Statistical modelling - lmer ###################

# IGNITABILITY full model
# this was before the cormatrix 
# twig traits (3) removed. 25/28 traits
IgnTime1 <- lmer(
  IgnTime ~ 
    height_cm + canopy_axis_1_cm + canopy_axis_2_cm + canopy_area_cm2 + 
    branch_order + pubescence + percent_N + percent_C + C_to_N_ratio + 
    d_15N_14N + d_13C_12C + FMC_proportion + num_leaves + leaf_area_cm2 + 
    leaf_length_cm + avg_leaf_width_cm + max_leaf_width_cm + 
    leaf_thickness_mm + leaf_fresh_wgt_g + leaf_dry_wgt_g + 
    lma + fwc + succulence + ldmc + lwr +
    (1 | species_name),
  data = Shared_spp_df02,
  REML = FALSE
)

summary(IgnTime1)

r.squaredGLMM(IgnTime1)     # R^2 (marginal = fixed effects; conditional = fixed + random)

#
MaxTemp1 <- lmer(
  MaxTemp ~ 
    height_cm + canopy_axis_1_cm + canopy_axis_2_cm + canopy_area_cm2 + 
    branch_order + pubescence + percent_N + percent_C + C_to_N_ratio + 
    d_15N_14N + d_13C_12C + FMC_proportion + num_leaves + leaf_area_cm2 + 
    leaf_length_cm + avg_leaf_width_cm + max_leaf_width_cm + 
    leaf_thickness_mm + leaf_fresh_wgt_g + leaf_dry_wgt_g + 
    lma + fwc + succulence + ldmc + lwr +
    (1 | species_name),
  data = Shared_spp_df02,
  REML = FALSE
)

summary(MaxTemp1)

r.squaredGLMM(MaxTemp1)

#
PostBurnM1 <- lmer(
  PostBurnM ~ 
    height_cm + canopy_axis_1_cm + canopy_axis_2_cm + canopy_area_cm2 + 
    branch_order + pubescence + percent_N + percent_C + C_to_N_ratio + 
    d_15N_14N + d_13C_12C + FMC_proportion + num_leaves + leaf_area_cm2 + 
    leaf_length_cm + avg_leaf_width_cm + max_leaf_width_cm + 
    leaf_thickness_mm + leaf_fresh_wgt_g + leaf_dry_wgt_g + 
    lma + fwc + succulence + ldmc + lwr +
    (1 | species_name),
  data = Shared_spp_df02,
  REML = FALSE
)

summary(PostBurnM1)

r.squaredGLMM(PostBurnM1)

#
#lmer with 19/28 traits reduced from cormatrix
IgnTime2 <- lmer(
  IgnTime ~ 
    height_cm + canopy_area_cm2 + branch_order + leaf_area_cm2 + leaf_length_cm +
  leaf_thickness_mm + lwr + num_leaves + percent_C + pubescence + 
  percent_N + d_13C_12C + d_15N_14N + FMC_proportion + succulence + ldmc + 
  leaf_fresh_wgt_g + leaf_dry_wgt_g + lma +
    (1 | species_name),
  data = Shared_spp_df02,
  REML = FALSE
)
summary(IgnTime2)

r.squaredGLMM(IgnTime2)

# 
MaxTemp2 <- lmer(
  MaxTemp ~ height_cm +
    canopy_area_cm2 + branch_order + pubescence + percent_N +
    percent_C + d_15N_14N + d_13C_12C +
    FMC_proportion + num_leaves + leaf_area_cm2 + 
    leaf_dry_wgt_g + 
    lma + succulence + ldmc + lwr +
    (1 | species_name),
  data = Shared_spp_df02,
  REML = FALSE
)
summary(MaxTemp2)

r.squaredGLMM(MaxTemp2)    

#
PostBurnM2 <- lmer(
  `PostBurnM` ~ height_cm +
    canopy_area_cm2 + branch_order + pubescence + percent_N +
    percent_C + d_15N_14N + d_13C_12C +
    FMC_proportion + num_leaves + leaf_area_cm2 + 
    leaf_dry_wgt_g + 
    lma + succulence + ldmc + lwr +
    (1 | species_name),
  data = Shared_spp_df02,
  REML = FALSE
)
summary(PostBurnM2)  

r.squaredGLMM(PostBurnM2) 

#
# model selected from significant traits
IgnTime2_best <- lmer(
  IgnTime ~ FMC_proportion + (1 | species_name),
  data = Shared_spp_df02,
  REML = FALSE
)
summary(IgnTime2_best)

r.squaredGLMM(IgnTime2_best)

#
MaxTemp2_best <- lmer(
  MaxTemp ~ FMC_proportion + (1 | species_name),
  data = Shared_spp_df02,
  REML = FALSE
)
summary(MaxTemp2_best)

r.squaredGLMM(MaxTemp2_best)


##################################### visualise ###############################

ggplot(Shared_spp_df02, aes(x = FMC_proportion, y = IgnTime)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE, color = "blue") +
  labs(title = "Ignitability ~ FMC proportion",
       x = "FMC proportion",
       y = "TimeToFlaming") + 
  ylim(0, 120)

# 
ggplot(Shared_spp_df02, aes(x = FMC_proportion, y = MaxTemp)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE, color = "blue") +
  labs(title = "Combustibility ~ FMC proportion",
       x = "FMC proportion",
       y = "Max Temperature (°C)")

#
Shared_spp_df02 %>%
  mutate(species_name = reorder(species_name, PostBurnM, FUN = median, na.rm = TRUE)) %>%
  ggplot(aes(x = species_name, y = PostBurnM, fill = growth_form_2)) +
  geom_boxplot() +
  labs(title = "Consumability ~ species",
       x = "Species", y = "Post-burn Mass (%)",
       fill = "Growth form") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


################################# Statistical modelling - lm ###################

# run full model lm with reduced collinearity 
# IGNITABILITY full model and species not random
IgnTime3 <- lm(
  IgnTime ~ 
    height_cm + canopy_area_cm2 + branch_order + leaf_area_cm2 + leaf_length_cm +
    leaf_thickness_mm + lwr + num_leaves + percent_C + pubescence + 
    percent_N + d_13C_12C + d_15N_14N + FMC_proportion + succulence + ldmc + 
    leaf_fresh_wgt_g + leaf_dry_wgt_g + lma + species_name,
  data = Shared_spp_df02
)

summary(IgnTime3)

# effect plot for only significant traits
library(effects)

plot(allEffects(IgnTime3), select = "FMC_proportion", partial.residuals = TRUE)
plot(allEffects(IgnTime3), select = "species_name")

#
plot(allEffects(MaxTemp3), select = "FMC_proportion", partial.residuals = TRUE)

#
MaxTemp3 <- lm(
  MaxTemp ~ 
    height_cm + canopy_area_cm2 + branch_order + leaf_area_cm2 + leaf_length_cm +
    leaf_thickness_mm + lwr + num_leaves + percent_C + pubescence + 
    percent_N + d_13C_12C + d_15N_14N + FMC_proportion + succulence + ldmc + 
    leaf_fresh_wgt_g + leaf_dry_wgt_g + lma + species_name,
  data = Shared_spp_df02
)
summary(MaxTemp3)

vif(MaxTemp3)


#
PostBurnM3 <- lm(
  PostBurnM ~ 
    height_cm + canopy_area_cm2 + branch_order + leaf_area_cm2 + leaf_length_cm +
    leaf_thickness_mm + lwr + num_leaves + percent_C + pubescence + 
    percent_N + d_13C_12C + d_15N_14N + FMC_proportion + succulence + ldmc + 
    leaf_fresh_wgt_g + leaf_dry_wgt_g + lma + species_name,
  data = Shared_spp_df02
)
summary(PostBurnM3)

vif(PostBurnM3)


########################################RQ2
################################### Growth form model lmer #####################

# Is plant flammability better explained by continuous trait variation or by categorical growth form?
gf_MaxTemp  <- lmer(`MaxTemp` ~ growth_form_2 + (1|species_name), 
                    data = Shared_spp_df02, REML = FALSE)

gf_PostBurnM <- lmer(`PostBurnM` ~ growth_form_2  + (1|species_name), 
                     data = Shared_spp_df02, REML = FALSE)

gf_IgnTime  <- lmer(`IgnTime` ~ growth_form_2  + (1|species_name), 
                    data = Shared_spp_df02, REML = FALSE)

#
summary(gf_IgnTime)
summary(gf_MaxTemp)
summary(gf_PostBurnM)

#
r.squaredGLMM(gf_IgnTime)
r.squaredGLMM(gf_MaxTemp)
r.squaredGLMM(gf_PostBurnM)

#################################### Growth form model lm #####################

gf_IgnTime3  <- lm (`IgnTime` ~ growth_form_2 + species_name, 
                     data = Shared_spp_df02)
summary(gf_IgnTime3)


gf_MaxTemp3  <- lm (`MaxTemp` ~ growth_form_2 + species_name, 
                    data = Shared_spp_df02)
summary(gf_MaxTemp3)


gf_PostBurnM3 <- lm (`PostBurnM` ~ growth_form_2 + species_name, 
                     data = Shared_spp_df02)
summary(gf_PostBurnM3)


#
ggplot(Shared_spp_df02, aes(x = growth_form_2, y = IgnTime, fill = growth_form_2)) +
  geom_boxplot(alpha = 0.6) +
    labs(
    x = "Growth Form",
    y = "Ignition Time (s)",
    title = "Distribution of Ignition Time by Growth Form"
  ) +
  theme_minimal() +
  theme(legend.position = "none")



#########################################xxxxx#################
<<<<<<< HEAD
########################
=======

>>>>>>> 1993faec244a5d6750d2d6a90e617c13ed8162bb


##################################################################
# Adapted from Raubenheimer et al 2025
# To investigate the underlying structure of the leaf trait data
# PCA All species pooled

library(FactoMineR)    #For PCA
library(factoextra)    #For visualization
library(missMDA)       #install.packages("missMDA")

############################# Principal Component analysis #####################

# Extract only trait data for PCA
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
pca_scores$growth_form_2 <- Shared_spp_df02$growth_form_2

#visualize  PCA
fviz_pca_ind(pca_result,
             geom.ind = "point",
             col.ind = pca_scores$growth_form_2, # color by growth form
             addEllipses = TRUE, # add grouping ellipses
             legend.title = "Growth Form")


# PCA for Species means rather
# Keep only numeric traits from your trait_col list
sppgrwth <- Shared_spp_df02 %>%
  dplyr::select(species_name, growth_form_2)

# keep rows aligned with 'trait_imputed' (if trait_imputed came 
# from trait_data built from Shared_spp_df02)
stopifnot(nrow(sppgrwth) == nrow(trait_imputed))

pca_full <- cbind(sppgrwth, trait_imputed)

#all traits averaged per species
species_means <- pca_full %>%
 group_by(species_name, growth_form_2) %>%
 summarise(across(where(is.numeric), mean), .groups = "drop")

#building PCA 
pca_matrix <- species_means %>%
  select(where(is.numeric)) %>%
  as.data.frame()

rownames(pca_matrix) <- species_means$species_name

#run PCA
species_pca <- PCA(pca_matrix, scale.unit = TRUE, graph = FALSE)

fviz_pca_ind(species_pca,
           geom.ind    = "point",
           col.ind     = species_means$growth_form_2, 
           addEllipses = TRUE,
           legend.title = "Growth form",
           title        = "PCA of Species-Mean Traits"
)


#trait loadings included
fviz_pca_biplot(
  species_pca,
  geom.ind  = "point",
  col.ind   = species_means$growth_form_2,
  label     = "var",      # show variable arrows only
  repel     = TRUE,
  legend.title = "Growth form",
  title     = "PCA Biplot: Species Means & Trait Loadings"
)

#varience explained
pca_result$eig   #dont really get this

species_pca$eig

### PCA + trait loadings 
fviz_pca_biplot(
  species_pca,
  geom.ind    = "point",
  col.ind     = species_means$growth_form_2, # color by growth form
  addEllipses = TRUE,                      # draw ellipses
  label       = "var",                      # show variable arrows
  repel       = TRUE,
  legend.title = "Growth form",
  title       = "PCA Biplot: Species Means & Trait Loadings"
)


# flam traits pca only
flamm_means <- Shared_spp_df02 %>%
  group_by(species_name) %>%
  summarise(across(all_of(flamm_col), mean, na.rm = TRUE))


# Run PCA
flamm_pca <- PCA(flamm_means [, flamm_col], scale.unit = TRUE, graph = FALSE)

fviz_pca_biplot(
  flamm_pca,
  geom.ind    = "point",      # species as points
  col.ind     = species_means$growth_form_2,
  label       = "var",        # show flammability trait arrows
  repel       = TRUE,
  legend.title = "Growth form",
  title       = "PCA of Flammability Traits"
)

#
# 





