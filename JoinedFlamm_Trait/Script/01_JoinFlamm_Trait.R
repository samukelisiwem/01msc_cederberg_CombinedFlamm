
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
library(corrplot)

#######################################
#read in leaf field trait data 
Flamm <- read_xlsx("Data/")

Fieldtrait <- read.csv("Data/Field_Traits_Final.csv") 

Labtrait <- read.csv("Data/HenryF/Lab_Traits_Final.csv")

##########################################
# making lowercase and trimming spaces in species names
# Flamm <- Flamm %>%
# mutate(Accepted_name = tolower(trimws(Accepted_name)))
# 
# Fieldtrait <- Fieldtrait %>%
# mutate(scientific_name_WFO = tolower(trimws(scientific_name_WFO)))


################


################################### Exploratory data analysis ##########################

str(Flamm)

str(Fieldtrait)

colnames(Fieldtrait)

colnames(Flamm)

# find unique (different) species from flammability & field leaf data
unique(Flamm$Accepted_name) #52 species 

unique(Fieldtrait$scientific_name_WFO)  #1000+ species 

unique(Fieldtrait$subregion)


################################ Merge datasets ################################

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

#list all species shared in the new df
print(sort(unique(Shared_spp_df01$species_name)))

colnames(Shared_spp_df01)


############################## read in Labtrait data ##########################

# Join Labtrait data to this shared_spp_df01 and create a new Shared_spp_02
# Shared_spp_df02 will have all flammability traits and leaf traits (field and Lab measured)


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


### join FlammAP with shared_sppdf02
# 
FlammAP <- FlammAP %>%
  mutate(Species = tolower(trimws(Species)))

#see shared species

flammshared03 <- intersect(unique(Fieldtrait$scientific_name_WFO), 
                         unique(FlammAP$Species)) 

length(flammshared03)   
print(flammshared03)   #11 species but 3 are already in the original flammdata (MSC)


#filter shared species from each
filtered_flammshared03 <- FlammAP %>%
  filter(Species%in% flammshared03)


filtered_fieldtrait <- Fieldtrait %>%
  filter(scientific_name_WFO %in% flammshared03)


#join
filterdFlamm <- left_join(
  filtered_flammshared03,
  filtered_fieldtrait,
  by = c("Species" = "scientific_name_WFO")
)

unique(filterdFlamm$`Species`)


# now i must join filterdFlamm with Shared_spp_df02 = megaFlamm
#
#megaFlamm <- filterdFlamm %>%
  filter(!Species %in% c("sideroxylon inerme",   # to avoid duplicating or replacing values, remove the 3 species when joining
                         "cassine peragua",
                         "pterocelastrus tricuspidatus")) %>%
  left_join(Shared_spp_df02, by = c("Species" = "Accepted_name"))

#
unique(megaFlamm$`Species`)




#the current flamm with all trait data
print(unique(Shared_spp_df02$species_name))


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

sapply(Shared_spp_df02[trait_col], class)  #pubescence is a character

# Convert all trait columns to relevant class 
Shared_spp_df02 <- Shared_spp_df02 %>%
  mutate(
    pubescence   = as.factor(pubescence),
    num_leaves   = readr::parse_number(as.character(num_leaves)),
    branch_order = as.numeric(branch_order)  
  )                                       #re-run sapply above
                                          #pubescence is a factor 

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

skewness(TT, na.rm = TRUE) 
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
    IgnTime   = `TimeToFlaming(s)`,
    MaxTemp   = `MaximumFlameTemperature(°C)`,
    PostBurnM = `PostBurntMassEstimate(%)`
   
  )         #i see new columns we created not replaced


############################# Correlation analysis #############################

# showing correlation coefficients (-1 inverse relationship red and 1 positive blue)

numeric_traits <- trait_col[trait_col != "pubescence"] # exclude pubescence (factor)

cor_matrix <- cor(Shared_spp_df02[numeric_traits], use = "pairwise.complete.obs")

corrplot(cor_matrix, method = "pie", type = "upper", tl.cex = 0.7)

cor_table <- as.data.frame(as.table(cor_matrix))
print(cor_table)
# write.xlsx(cor_table, "Trait_Correlation_Table.xlsx")

# run another cor.matrix based on the 18 remaining traits
numeric_traitscorr2 <- c(
  "height_cm",  "canopy_area_cm2","branch_order", "leaf_area_cm2", "leaf_length_cm", 
  "leaf_thickness_mm", "lwr", "num_leaves", "percent_C",  #pubescence not numeric but factor
  "percent_N", "d_13C_12C", "d_15N_14N", "FMC_proportion", "succulence", 
  "ldmc", "leaf_fresh_wgt_g", "leaf_dry_wgt_g", "lma")
  

cor_matrix_reduced<- cor(Shared_spp_df02[numeric_traitscorr2], use = "pairwise.complete.obs")

corrplot(cor_matrix_reduced, method = "pie", type = "upper", tl.cex = 0.7)


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


################## lmer with 19/28 traits reduced from cormatrix ##############
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


##################################### visualize ###############################

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

# run model lm with reduced collinearity 
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

#vif(MaxTemp3)

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


# effect plot for only significant traits
library(effects)

plot(allEffects(IgnTime3), select = "FMC_proportion", partial.residuals = TRUE)
plot(allEffects(IgnTime3), select = "species_name")  #must rotate spp names

#
plot(allEffects(MaxTemp3), select = "FMC_proportion", partial.residuals = TRUE)


########################################RQ2
################################### Growth form model lmer #####################

# Is plant flammability better explained by continuous trait variation or by categorical growth form?
gf_IgnTime  <- lmer(`IgnTime` ~ growth_form_2  + (1|species_name), 
                    data = Shared_spp_df02, REML = FALSE)
summary(gf_IgnTime)

#
gf_MaxTemp  <- lmer(`MaxTemp` ~ growth_form_2 + (1|species_name), 
                    data = Shared_spp_df02, REML = FALSE)
summary(gf_MaxTemp)

#
gf_PostBurnM <- lmer(`PostBurnM` ~ growth_form_2  + (1|species_name), 
                     data = Shared_spp_df02, REML = FALSE)
summary(gf_PostBurnM)

#
r.squaredGLMM(gf_IgnTime)
r.squaredGLMM(gf_MaxTemp)
r.squaredGLMM(gf_PostBurnM)

#################################### Growth form model lm #####################

gf_IgnTime3  <- lm (`IgnTime` ~ growth_form_2 + species_name, 
                     data = Shared_spp_df02)
summary(gf_IgnTime3)

#
gf_MaxTemp3  <- lm (`MaxTemp` ~ growth_form_2 + species_name, 
                    data = Shared_spp_df02)
summary(gf_MaxTemp3)

#
gf_PostBurnM3 <- lm (`PostBurnM` ~ growth_form_2 + species_name, 
                     data = Shared_spp_df02)
summary(gf_PostBurnM3)

################################ visualize gf #######################
ggplot(Shared_spp_df02, aes(x = growth_form_2, y = IgnTime, fill = growth_form_2)) +
  geom_boxplot(alpha = 0.6) +
    labs(
    x = "Growth Form",
    y = "Ignition Time (s)",
    title = "Distribution of Ignition Time by Growth Form"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

#
ggplot(Shared_spp_df02, aes(x = growth_form_2, y = MaxTemp, fill = growth_form_2)) +
  geom_boxplot(alpha = 0.6) +
  labs(
    x = "Growth Form",
    y = "Max Temp",
    title = ""
  ) +
  theme_minimal() +
  theme(legend.position = "none")

#
ggplot(Shared_spp_df02, aes(x = growth_form_2, y = PostBurnM, fill = growth_form_2)) +
  geom_boxplot(alpha = 0.6) +
  labs(
    x = "Growth Form",
    y = "Post burn mass",
    title = ""
  ) +
  theme_minimal() +
  theme(legend.position = "none")

###must figure facetwrap.


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



####################################################

### 20251105
## Beta regression. https://cran.r-project.org/web//packages/betareg/vignettes/betareg.html
# 

#View histograms for all numeric columns
numeric_cols <- sapply(Shared_spp_df02, is.numeric)

par(mfrow = c(3, 3))  # adjust to number of vars
for (col in names(Shared_spp_df02)[numeric_cols]) {
  hist(Shared_spp_df02[[col]], main = col, xlab = "", col = "grey80")
}
par(mfrow = c(1, 1))

#
# Post-burn mass → proportion consumed
Shared_spp_df02$Consumed01 <- (100 - Shared_spp_df02$PostBurnM) / 100

# Ignition time → invert and scale
max_time <- 120  # max time in seconds
Shared_spp_df02$Ignite01 <- 1 - (Shared_spp_df02$IgnTime / max_time)

# Max temperature → scale 0–1
Shared_spp_df02$MaxTemp01 <- (Shared_spp_df02$MaxTemp - min(Shared_spp_df02$MaxTemp)) /
  (max(Shared_spp_df02$MaxTemp) - min(Shared_spp_df02$MaxTemp))


###
#install.packages("statmod", "betareg")
library(statmod)
library(betareg)

##without random effect 
#Ignite01, Maxtemp01, Consumed01
IgnTime_beta <- betareg(
  Ignite01 ~ height_cm + canopy_area_cm2 + branch_order + leaf_area_cm2 + leaf_length_cm +
    leaf_thickness_mm + lwr + num_leaves + percent_C + pubescence + 
    percent_N + d_13C_12C + d_15N_14N + FMC_proportion + succulence + ldmc + 
    leaf_fresh_wgt_g + leaf_dry_wgt_g + lma + species_name,
  data = Shared_spp_df02
)

summary(IgnTime_beta)
plot()

#with random effect
m_beta_re <- glmmTMB(
  IgnTime ~ height_cm + canopy_area_cm2 + branch_order + leaf_area_cm2 + leaf_length_cm +
    leaf_thickness_mm + lwr + num_leaves + percent_C + pubescence + 
    percent_N + d_13C_12C + d_15N_14N + FMC_proportion + succulence + ldmc + 
    leaf_fresh_wgt_g + leaf_dry_wgt_g + lma +
    (1 | species_name),
  family = beta_family(link = "logit"),
  data = Shared_spp_df02
)


######################################################################
################# Read all DATAs

# cederberg and george data combined
Flamm <- read_csv("Data/CombinedOG.csv")
colnames(Flamm) #Accepted_name

anyNA(Flamm) #any NA
names(Flamm)[colSums(is.na(Flamm)) > 0] #which var has NA

# Alastair Potts (AP) Cape StF data]
FlammAP <- read_excel("Data/CapeStFrancis_Hons_FlammabilityData_AP.xlsx")
colnames(FlammAP) #"Species"

anyNA(FlammAP) #any NA
names(FlammAP)[colSums(is.na(FlammAP)) > 0] #which var has NA

# henry field trait data
Fieldtrait <- read.csv("Data/Field_Traits_Final.csv")
colnames(Fieldtrait) #"scientific_name_WFO"

anyNA(Fieldtrait) #any NA
names(Fieldtrait)[colSums(is.na(Fieldtrait)) > 0] #which var has NA

nrow(Fieldtrait)   # how many observations
ncol(Fieldtrait)   # how many variables

colSums(is.na(Fieldtrait)) #NA per column

Fieldtrait_noNA <- na.omit(Fieldtrait)
nrow(Fieldtrait_noNA)   # how many observations
ncol(Fieldtrait_noNA) 

# lab trait
Labtrait <- read.csv("Data/Lab_Traits_Final.csv")
colnames(Labtrait)  ##"scientific_name_WFO"

anyNA(Labtrait) #any NA
names(Labtrait)[colSums(is.na(Labtrait)) > 0] #which var has NA

nrow(Labtrait)   # how many observations
ncol(Labtrait)   # how many variables

colSums(is.na(Labtrait)) #NA per column

Labtrait_noNA <- na.omit(Labtrait)
nrow(Labtrait_noNA)   # how many observations
ncol(Labtrait_noNA) 


# 
# make all species columns SpeciesNames in all df
Flamm      <- Flamm      |> rename(SpeciesNames = Accepted_name)
FlammAP    <- FlammAP    |> rename(SpeciesNames = Species)
Fieldtrait <- Fieldtrait |> rename(SpeciesNames = scientific_name_WFO)
Labtrait   <- Labtrait   |> rename(SpeciesNames = scientific_name_WFO)

Fieldtrait_noNA <- Fieldtrait_noNA |> rename(SpeciesNames = scientific_name_WFO)
Labtrait_noNA <- Labtrait_noNA   |> rename(SpeciesNames = scientific_name_WFO)


# make all lower cases
 Flamm$SpeciesNames      <- tolower(Flamm$SpeciesNames)
 FlammAP$SpeciesNames    <- tolower(FlammAP$SpeciesNames)
 Fieldtrait$SpeciesNames <- tolower(Fieldtrait$SpeciesNames)
 Labtrait$SpeciesNames   <- tolower(Labtrait$SpeciesNames)

 Labtrait_noNA$SpeciesNames       <- tolower(Labtrait_noNA$SpeciesNames)
 Fieldtrait_noNA$SpeciesNames       <- tolower(Fieldtrait_noNA$SpeciesNames)

 unique(Flamm$SpeciesNames) #52 species
 length (unique(FlammAP$SpeciesNames)) #26

 length(unique(Fieldtrait$SpeciesNames)) #1327
 unique(Labtrait$SpeciesNames) #1327

 length(unique(Fieldtrait_noNA$SpeciesNames)) #1039
 unique(Labtrait_noNA$SpeciesNames) #986


# see shared species on Flamms
shared_species_flamm <- intersect(
tolower(Flamm$SpeciesNames),
tolower(FlammAP$SpeciesNames)
)

shared_species_flamm

# remove the 4 shared species from AP
FlammAP_filtered <- FlammAP |>
  filter(!tolower(SpeciesNames) %in% shared_species_flamm)

unique(FlammAP_filtered$SpeciesNames) #22

# combine the flamm df
Flamm_all <- bind_rows(Flamm, FlammAP_filtered)
unique(Flamm_all$SpeciesNames) #74


Flamm_all <- Flamm_all %>%
   mutate(`PostBurntMassEstimate(%)` = coalesce(`PostBurntMassEstimate(%)`, `BB_%`))

Flamm_all <- Flamm_all %>%
  mutate(`MaximumFlameTemperature(°C)` = coalesce(`MaximumFlameTemperature(°C)`, `MT`))

Flamm_all <- Flamm_all %>%
  mutate(`FMC(%)` = coalesce(`FMC(%)`, `Moisture_Content`))

#
## combine all trait df
Trait_all <- left_join(Fieldtrait, Labtrait, by = "SpeciesNames")
unique(Trait_all$SpeciesNames) #1000+

Trait_all_noNA <- left_join(Fieldtrait_noNA, Labtrait_noNA, by = "SpeciesNames")
unique(Trait_all_noNA$SpeciesNames) #


# save all both combined
# writexl::write_xlsx(Flamm_all, "Data/Flamm_all.xlsx")
# 
# writexl::write_xlsx(Trait_all, "Data/Trait_all.xlsx")
# 
# writexl::write_xlsx(Trait_all_noNA, "Data/Trait_all_noNA.xlsx")


Flamm_allNew <- read_excel("Data/Flamm_all_new.xlsx")
Trait_all <- read_excel("Data/Trait_all.xlsx")
Trait_all_noNA <- read_excel("Data/Trait_all_noNA.xlsx")

colnames(Flamm_all)

# alias columns for my responses (avoiding % and () in names)
Flamm_all <- Flamm_all |>
  dplyr::mutate(
    IgnTime   = `TimeToFlaming(s)`,
    MaxTemp   = `MaximumFlameTemperature(°C)`,
    PostBurnM = `PostBurntMassEstimate(%)`
    
  )         #i see new columns we created not replaced

#see shared species between flamm and trait all
shared_species_all <- intersect(Flamm_all$SpeciesNames, Trait_all$SpeciesNames)
shared_species_all

length(shared_species_all)   #41

#now join Flamm All and trait all by shares species 
megadata <- inner_join(Flamm_all, Trait_all, by = "SpeciesNames")

colnames(megadata)
unique(megadata$SpeciesNames)

sapply(megadata, class)

# histogram view 
numeric_col <- sapply(megadata, is.numeric)

#histoCol <- c("IgnTime", "MaxTemp", "PostBurnM", "FMC(%)",
#"canopy_axis_1_cm", "canopy_area_cm2", "pubescence", 
#"percent_C", "d_15N_14N", "leaf_area_cm2", 
#"avg_leaf_width_cm", "leaf_thickness_mm", "leaf_dry_wgt_g",
#"twig_dry_g", "fwc", "ldmc", "twig_fwc", "height_cm",
#"canopy_axis_2_cm", "branch_order", "percent_N",
#"C_to_N_ratio", "d_13C_12C", "num_leaves", "leaf_length_cm",
#"max_leaf_width_cm", "leaf_fresh_wgt_g", "twig_fresh_g",
#"lma", "succulence", "lwr")

par(mfrow = c(3, 3))                  # 9 plots per page
for (col in names(megadata)[numeric_col]) {
  hist(megadata[[col]], main = col, xlab = "", col = "grey80")
}
par(mfrow = c(1, 1))

## SCALE TO 0–1 
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

#
# showing correlation coefficients (-1 inverse relationship red and 1 positive blue)
str(megadata[numeric_traits01])

# exclude pubescence and num_leaves (factor)
numeric_traits01 <- trait_col[!trait_col %in% c("pubescence", "num_leaves")]

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

#
#
megadata <- megadata %>%
  mutate(
    pubescence   = as.factor(pubescence),
    num_leaves   = readr::parse_number(as.character(num_leaves)),
    branch_order = as.numeric(branch_order)  
  )        

 #
 ## scale() traits for the following steps 
 megadata <- megadata %>%
   mutate(across(all_of(trait_col) & where(is.numeric), ~ as.numeric(scale(.))))
 
 # check if trait were scaled 
 sapply(megadata[trait_col], function(x) {
   if (is.numeric(x)) c(mean = mean(x, na.rm = TRUE),
                        sd   = sd(x, na.rm = TRUE))
   else NA
 })

 
 ### betareg
 
 # IgnTimeXY_beta <- betareg(
 #   IgnTime01 ~
 #     species_name,
 #   data = megadata
 # )
 # summary(IgnTimeXY_beta)
 # plot(IgnTimeXY_beta)
 
 library(betareg)
 
 IgnTime01_beta <- betareg(
   IgnTime01 ~ height_cm + canopy_area_cm2 + branch_order +
     pubescence + percent_N + percent_C + d_15N_14N + d_13C_12C + 
     FMC_proportion + num_leaves +leaf_area_cm2 + lma +
     fwc + succulence + ldmc + lwr + SpeciesNames,
   data = megadata
 )
 
 summary(IgnTime01_beta)
 plot(IgnTime01_beta)
 
# install.packages("glmmTMB")
library(glmmTMB)
 
 traits_size   <- c("height_cm", "canopy_area_cm2", "branch_order")
 traits_leaf   <- c("leaf_area_cm2", "lma", "ldmc", "lwr", "num_leaves")
 traits_chem   <- c("percent_N", "percent_C", "d_15N_14N", "d_13C_12C", "succulence", "FMC_proportion")
 traits_misc   <- c("fwc", "pubescence")  
 
#
## species as random
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

 library(ggeffects)
 
 # Compute marginal effects for all predictors
 eff <- ggpredict(IgnTMB, terms = c("height_cm", "canopy_area_cm2",
                                    "branch_order", "pubescence",
                                    "percent_N", "percent_C",
                                    "d_15N_14N", "d_13C_12C",
                                    "FMC_proportion", "num_leaves",
                                    "leaf_area_cm2", "lma", "fwc",
                                    "succulence", "ldmc", "lwr"))
 
 # Plot
 plot(eff)
 
 #
 #
 #
 
 
 Igm1 <- glmmTMB(IgnTime01 ~ height_cm + canopy_area_cm2 + branch_order +
                  (1 | SpeciesNames),
                data = megadata,
              family = beta_family(link = "logit")
)          
summary(Igm1)

# table(megadata$IgnTime01)
# range(megadata$IgnTime01, na.rm = TRUE)


#