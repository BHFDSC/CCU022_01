### Author: Pearl Liu
### Compare CRS effect size correlation vs. genetic correlation vs. disease correlation across outcomes ###
### Index: 2015-01-01, follow-up: 10 years (all outcomes)
### Index: 2020-01-01, follow-up: 5 years (COVID hospitalization)

rm(list = ls())
gc()

#### Load libraries ####
library(data.table)
library(tidyverse)
library(forcats)
library(doBy)
library(ggrepel)
library(corrplot)
library(cowplot)

#### Set parameters ####
read_dir <- 'Outputs/'
summary_dir <- 'Summary_Stats/'
figure_dir <- 'Figures_Manuscript/'
file_date <- '2025_05_01'

##### Plot parameters #####
base_font_size <- 24
base_point_size <- 4
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
safe_colorblind_palette <- c("#88CCEE", "#CC6677", "#DDCC77", "#117733", "#332288", "#AA4499",
                             "#44AA99", "#999933", "#882255", "#661100", "#6699CC", "#888888")

# set the commonly used color
blue <- cbPalette[6]
red <- cbPalette[7]
green <- cbPalette[4]
orange <- cbPalette[2]
grey <- cbPalette[1]
yellow <- cbPalette[5]
purple <- cbPalette[8]
skyblue <- cbPalette[3]

#phecode_cat_color_vec <- c("red", "cyan", "tomato", "yellow2",
#  "green2", "deeppink", "orange", "purple",
#  "deepskyblue", "brown", "violet",
#  "seagreen1", "blue", "gold",
#  "grey40")

phecode_cat_color_vec <- c(red, "#88CCEE", "#882255", yellow,
                           green, "#CC6677", orange, purple,
                           blue, "#44AA99", "#AA4499", skyblue,
                           "#661100", "#999933",
                           grey)

predictor_category_vec <- c("Infectious diseases", "Neoplasms", "Endocrine/metabolic",  
                            "Hematopoietic", "Mental disorders", "Neurological",         
                            "Sense organs", "Circulatory system", "Respiratory",          
                            "Digestive", "Genitourinary",  "Dermatologic",         
                            "Musculoskeletal", "Symptoms", "Injuries & poisonings")

phecode_category_color <- data.frame(
  Phecode_Category = predictor_category_vec,
  Color_values = phecode_cat_color_vec)

## Model names
model_name_vec <- c("CRS_all_predictors", 
                    "CRS_top100",
                    "CRS_top50", 
                    "CRS_top20", 
                    "CRS_top10",
                    "Age_Sex")
n_models <- length(model_name_vec)
## Control-case ratio in logistic regression of training data
control_case_ratio <- 4
## Number of covariates 
n_covar <- 2
## Number of Monte Carlo sampling in testing data
n_mc <- 100  ## PL 20260120: update from 200 to 100
## Control-case ratio in testing data for evaluating AUROC, C-index, R2
control_case_ratio_test <- 4
## Number of cases used in testing data
n_cases_test1 <- 3000
n_cases_test2 <- 1000


#### Read processed data ####
##### Observed correlation across outcomes #####
obs_corr_summ <- read.csv(paste0(read_dir, "OutcomeDiseaseCorrelation_WhiteBritish_OddYOB_Index2020.csv"))

##### Liability correlation across outcomes #####
liab_corr_summ <- readRDS(paste0(summary_dir, "Phenotypic_liab_corr/liability_corr.rds"))


##### List and prevalence of predictors and outcomes #####
pred_ds_prev2015_name = "WhiteBritish_OddYOB_Index2015_PredictorDiseasePrevalence.csv"
pred_ds_prev2020_name = "WhiteBritish_OddYOB_Index2020_PredictorDiseasePrevalence.csv"
outcome_ds_prev_name = "TrainData_Outcome_Incidence_OddYOB_Index2015_Followup10yr.csv"

pred_ds_prev2015 = read.csv(paste0(read_dir, pred_ds_prev2015_name)) %>%
  mutate(Predictor_name = paste0('ds', Predictor_phecode))
pred_ds_prev2020 = read.csv(paste0(read_dir, pred_ds_prev2020_name)) %>%
  mutate(Predictor_name = paste0('ds', Predictor_phecode))

## Combine 2015 predictor prevalence (all other outcomes) with 2020 predictor prevlence (covid outcome)
all(pred_ds_prev2015$Predictor_name == pred_ds_prev2020$Predictor_name)
pred_ds_prev <- pred_ds_prev2015 %>%
  mutate(Prevalence2020 = pred_ds_prev2020$Prevalence)

outcome_ds_prev0 = read.csv(paste0(read_dir, outcome_ds_prev_name))
covid_prev <- data.frame("Outcome_phecode" = "COVID_hspt",
                         "Incidence_general" = 0.01027544,
                         "Incidence_sf" = NA)

outcome_ds_prev <- outcome_ds_prev0 %>%
  ## Remove phecode 622, as it corresponds to the same ICD10 codes as 622.1
  filter(Outcome_phecode != "622") %>%
  rbind(covid_prev) %>%
  mutate(Incidence_combined = if_else(is.na(Incidence_sf), Incidence_general, 
                                      Incidence_sf))

n_pred_ds <- nrow(pred_ds_prev)
n_outcome_ds <- nrow(outcome_ds_prev)



##### Phecode information #####
phe_icd10_codelist_name = "phecode_icd10_map_with_parent_info_AllPheCodes.csv"
icd10_phecode_map <- read.csv(paste0("Inputs/For SDE/", 
                                     phe_icd10_codelist_name))
phecode_info <- icd10_phecode_map %>%
  select(PHECODE, phenotype, category, sex) %>%
  distinct() %>%
  mutate(Outcome_phecode = str_replace(PHECODE, '\\.', '_'))

phecode_parent_info <- icd10_phecode_map %>%
  select(PHECODE_parent, phenotype_parent, category) %>%
  distinct() %>%
  mutate(Predictor_name = paste0("ds", PHECODE_parent))

#### UPDATE TO LATEST: Read all summary statistics ####
all_outcome_summ_coef <- readRDS(paste0(summary_dir,
                                        "All_Outcomes+COVID_WeightedLogisticRegression_SummStats_ControlCase4to1_WhiteBritish_OddYOB_Index2015_Followup10yr.rds"))

##### Get predictor disease effect sizes, calculate p-values #####
all_outcome_summ_coef_pred_ds <- all_outcome_summ_coef %>%
  filter(Outcome != "622") %>%
  filter(startsWith(Predictor, "ds")) %>%
  ## Use prevalence by 2020 for COVID outcome
  mutate(Est_scaleX = if_else(Outcome != "COVID_hspt", 
                              Est * sqrt(Predictor_prev * (1 - Predictor_prev)),
                              Est * sqrt(Predictor_prev2020 * (1 - Predictor_prev2020)))) %>%
  mutate(SE_scaleX = if_else(Outcome != "COVID_hspt", 
                              SE * sqrt(Predictor_prev * (1 - Predictor_prev)),
                              SE * sqrt(Predictor_prev2020 * (1 - Predictor_prev2020)))) %>%
  mutate(Pval = 2 * pnorm(abs(Est/SE), lower.tail = F)) %>%
  mutate(Pval_fdr = p.adjust(Pval, method = 'fdr')) %>%
  rename(Outcome_phecode = Outcome) %>%
  ## Convert first letter to capital letter in phecode category
  mutate(Predictor_category = str_to_sentence(Predictor_category))

all_outcome_intercept <- all_outcome_summ_coef %>%
  filter(Outcome != "622") %>%
  filter(Predictor == "(Intercept)")


#### Correlation matrix of CRS effect sizes across outcomes ####
n_outcome_ds <- 88
outcome_ds_vec <- c("COVID_hspt", "death", outcome_ds_prev %>%
                      # remove phecode 622 as it is defined the same as phecode 622.1
                      filter(Outcome_phecode != "622" & Outcome_phecode != "COVID_hspt" &
                               Outcome_phecode != "death") %>%
                      arrange(Outcome_phecode) %>%
                      pull(Outcome_phecode))

phecode_info_sort <- phecode_info %>% 
  filter(Outcome_phecode %in% outcome_ds_vec[-c(1,2)]) %>%
  arrange(Outcome_phecode)
# all.equal(phecode_info_sort$Outcome_phecode, outcome_ds_vec[-c(1,2)])
outcome_pheno_vec <- c("COVID-19 hospitalisation", 
                       "Death",
                       phecode_info_sort$phenotype)

##### Pair phecodes with phenotype for outcomes #####
outcome_phecode_pheno <- cbind(outcome_ds_vec, outcome_pheno_vec) %>%
  as.data.frame()
colnames(outcome_phecode_pheno) <- c("Outcome_phecode", "phenotype")

##### Get correlation matrix with Est and SE - based on raw effect sizes #####
effec_size_cor_mt <- matrix(NA, n_outcome_ds, n_outcome_ds)
colnames(effec_size_cor_mt) <- outcome_pheno_vec
rownames(effec_size_cor_mt) <- outcome_pheno_vec
diag(effec_size_cor_mt) <- 1

effec_size_cor_SE_mt <- effec_size_cor_mt

for (i in 1:(n_outcome_ds-1)) {
  for (j in (i+1):n_outcome_ds) {
    target_ds_i <- outcome_ds_vec[i]
    target_ds_j <- outcome_ds_vec[j]
    effec_size_i <- all_outcome_summ_coef_pred_ds %>%
      filter(Outcome_phecode == target_ds_i) %>%
      select(Predictor, Est) %>%
      rename(Est_i = Est)
    effec_size_j <- all_outcome_summ_coef_pred_ds %>%
      filter(Outcome_phecode == target_ds_j) %>%
      select(Predictor, Est) %>%
      rename(Est_j = Est)
    effec_size_i_j <- effec_size_i %>%
      inner_join(effec_size_j, by = "Predictor")
    effec_size_cor_mt[i,j] <- cor(effec_size_i_j$Est_i,
                                  effec_size_i_j$Est_j)
    effec_size_cor_mt[j,i] <- effec_size_cor_mt[i,j]
    
    ## Get SE for Pearson's correlation (Soper, H. E. (1913))
    n_ij <- nrow(effec_size_i_j)
    r <- effec_size_cor_mt[i,j]
    se_r <- (1 - r^2) / sqrt(n_ij - 1)
    effec_size_cor_SE_mt[i,j] <- se_r
    effec_size_cor_SE_mt[j,i] <- se_r
  }
}

###### Transform Est to long format ######
effec_size_cor_df <- data.frame(effec_size_cor_mt)
colnames(effec_size_cor_df) <- outcome_pheno_vec

effec_size_cor_long <- effec_size_cor_df %>%
  mutate(Phenotype1 = rownames(effec_size_cor_df)) %>%
  pivot_longer(cols = colnames(effec_size_cor_df), 
               names_to = "Phenotype2",
               values_to = "CRS_corr") %>%
  ## Keep only distinct pairs
  filter(Phenotype1 != Phenotype2) %>%
  mutate(Pheno1_Pheno2 = paste(pmin(Phenotype1, Phenotype2), 
                               pmax(Phenotype1, Phenotype2), 
                               sep = "__")) %>%
  ## Map to phecodes
  left_join(outcome_phecode_pheno, by = join_by("Phenotype1" == "phenotype")) %>%
  rename(Outcome_ds1 = Outcome_phecode) %>%
  left_join(outcome_phecode_pheno, by = join_by("Phenotype2" == "phenotype")) %>%
  rename(Outcome_ds2 = Outcome_phecode) 


###### Remove parent/child outcome pairs ######
effec_size_cor_long_parent_child <- effec_size_cor_long %>%
  mutate(parent_child_ind = mapply(grepl, pattern = Outcome_ds1, 
                                   x = Outcome_ds2, fixed = TRUE),
         child_parent_ind = mapply(grepl, pattern = Outcome_ds2, 
                                   x = Outcome_ds1, fixed = TRUE)) 

effec_size_cor_long_final <- effec_size_cor_long_parent_child %>%
  filter(parent_child_ind == F & child_parent_ind == F) %>%
  select(Pheno1_Pheno2, CRS_corr) %>%
  distinct()


saveRDS(effec_size_cor_long_final, paste0(summary_dir, "CRS_effec_size_correlation_across_outcomes_long.rds"))


###### Transform SE to long format ######
effec_size_cor_SE_df <- data.frame(effec_size_cor_SE_mt)
colnames(effec_size_cor_SE_df) <- outcome_pheno_vec

effec_size_cor_SE_long <- effec_size_cor_SE_df %>%
  mutate(Phenotype1 = rownames(effec_size_cor_SE_df)) %>%
  pivot_longer(cols = colnames(effec_size_cor_SE_df), 
               names_to = "Phenotype2",
               values_to = "CRS_corr_SE") %>%
  ## Keep only distinct pairs
  filter(Phenotype1 != Phenotype2) %>%
  mutate(Pheno1_Pheno2 = paste(pmin(Phenotype1, Phenotype2), 
                               pmax(Phenotype1, Phenotype2), 
                               sep = "__")) %>%
  ## Map to phecodes
  left_join(outcome_phecode_pheno, by = join_by("Phenotype1" == "phenotype")) %>%
  rename(Outcome_ds1 = Outcome_phecode) %>%
  left_join(outcome_phecode_pheno, by = join_by("Phenotype2" == "phenotype")) %>%
  rename(Outcome_ds2 = Outcome_phecode) 

###### Remove parent/child outcome pairs ######
effec_size_cor_SE_long_parent_child <- effec_size_cor_SE_long %>%
  mutate(parent_child_ind = mapply(grepl, pattern = Outcome_ds1, 
                                   x = Outcome_ds2, fixed = TRUE),
         child_parent_ind = mapply(grepl, pattern = Outcome_ds2, 
                                   x = Outcome_ds1, fixed = TRUE)) 

effec_size_cor_SE_long_final <- effec_size_cor_SE_long_parent_child %>%
  filter(parent_child_ind == F & child_parent_ind == F) %>%
  select(Pheno1_Pheno2, CRS_corr_SE) %>%
  distinct()

saveRDS(effec_size_cor_SE_long_final, paste0(summary_dir, "CRS_effec_size_correlation_SE_across_outcomes_long.rds"))


###### Read saved file ######
effec_size_cor_long_final <- readRDS(paste0(summary_dir, "CRS_effec_size_correlation_across_outcomes_long.rds"))
effec_size_cor_SE_long_final <- readRDS(paste0(summary_dir, "CRS_effec_size_correlation_SE_across_outcomes_long.rds"))

###### Check specific example outcomes ######
CRS_effec_cor_Est_SE <- effec_size_cor_long_final %>% 
  select("Pheno1_Pheno2", "CRS_corr") %>%
  distinct() %>%
  left_join(effec_size_cor_SE_long_final %>% 
              select("Pheno1_Pheno2", "CRS_corr_SE") %>%
              distinct())

CRS_effec_cor_Est_SE %>%  
  filter(Pheno1_Pheno2 == "Myocardial infarction__Type 2 diabetes")
#  Pheno1_Pheno2                         CRS_corr CRS_corr_SE
#  <chr>                                    <dbl>       <dbl>
#  Myocardial infarction__Type 2 diabetes    0.470      0.0539


##### Get correlation matrix with Est and SE - based on standardized effect sizes #####
effec_size_cor_mt_scaleX <- matrix(NA, n_outcome_ds, n_outcome_ds)
colnames(effec_size_cor_mt_scaleX) <- outcome_pheno_vec
rownames(effec_size_cor_mt_scaleX) <- outcome_pheno_vec
diag(effec_size_cor_mt_scaleX) <- 1

effec_size_cor_SE_mt_scaleX <- effec_size_cor_mt_scaleX

for (i in 1:(n_outcome_ds-1)) {
  for (j in (i+1):n_outcome_ds) {
    target_ds_i <- outcome_ds_vec[i]
    target_ds_j <- outcome_ds_vec[j]
    effec_size_i <- all_outcome_summ_coef_pred_ds %>%
      filter(Outcome_phecode == target_ds_i) %>%
      select(Predictor, Est_scaleX) %>%
      rename(Est_i = Est_scaleX)
    effec_size_j <- all_outcome_summ_coef_pred_ds %>%
      filter(Outcome_phecode == target_ds_j) %>%
      select(Predictor, Est_scaleX) %>%
      rename(Est_j = Est_scaleX)
    effec_size_i_j <- effec_size_i %>%
      inner_join(effec_size_j, by = "Predictor")
    effec_size_cor_mt_scaleX[i,j] <- cor(effec_size_i_j$Est_i,
                                         effec_size_i_j$Est_j)
    effec_size_cor_mt_scaleX[j,i] <- effec_size_cor_mt_scaleX[i,j]
    
    ## Get SE for Pearson's correlation (Soper, H. E. (1913))
    n_ij <- nrow(effec_size_i_j)
    r <- effec_size_cor_mt_scaleX[i,j]
    se_r <- (1 - r^2) / sqrt(n_ij - 1)
    effec_size_cor_SE_mt_scaleX[i,j] <- se_r
    effec_size_cor_SE_mt_scaleX[j,i] <- se_r
  }
}

###### Transform Est to long format ######
effec_size_cor_df_scaleX <- data.frame(effec_size_cor_mt_scaleX)
colnames(effec_size_cor_df_scaleX) <- outcome_pheno_vec

effec_size_cor_scaleX_long <- effec_size_cor_df_scaleX %>%
  mutate(Phenotype1 = rownames(effec_size_cor_df_scaleX)) %>%
  pivot_longer(cols = colnames(effec_size_cor_df_scaleX), 
               names_to = "Phenotype2",
               values_to = "CRS_corr") %>%
  ## Keep only distinct pairs
  filter(Phenotype1 != Phenotype2) %>%
  mutate(Pheno1_Pheno2 = paste(pmin(Phenotype1, Phenotype2), 
                               pmax(Phenotype1, Phenotype2), 
                               sep = "__")) %>%
  ## Map to phecodes
  left_join(outcome_phecode_pheno, by = join_by("Phenotype1" == "phenotype")) %>%
  rename(Outcome_ds1 = Outcome_phecode) %>%
  left_join(outcome_phecode_pheno, by = join_by("Phenotype2" == "phenotype")) %>%
  rename(Outcome_ds2 = Outcome_phecode) 


###### Remove parent/child outcome pairs ######
effec_size_cor_scaleX_long_parent_child <- effec_size_cor_scaleX_long %>%
  mutate(parent_child_ind = mapply(grepl, pattern = Outcome_ds1, 
                                   x = Outcome_ds2, fixed = TRUE),
         child_parent_ind = mapply(grepl, pattern = Outcome_ds2, 
                                   x = Outcome_ds1, fixed = TRUE)) 

effec_size_cor_scaleX_long_final <- effec_size_cor_scaleX_long_parent_child %>%
  filter(parent_child_ind == F & child_parent_ind == F) %>%
  select(Pheno1_Pheno2, CRS_corr) %>%
  distinct()


saveRDS(effec_size_cor_scaleX_long_final, 
        paste0(summary_dir, "CRS_effec_size_scaleX_correlation_across_outcomes_long.rds"))


###### Transform SE to long format ######
effec_size_cor_scaleX_SE_df <- data.frame(effec_size_cor_SE_mt_scaleX)
colnames(effec_size_cor_scaleX_SE_df) <- outcome_pheno_vec

effec_size_cor_scaleX_SE_long <- effec_size_cor_scaleX_SE_df %>%
  mutate(Phenotype1 = rownames(effec_size_cor_scaleX_SE_df)) %>%
  pivot_longer(cols = colnames(effec_size_cor_scaleX_SE_df), 
               names_to = "Phenotype2",
               values_to = "CRS_corr_SE") %>%
  ## Keep only distinct pairs
  filter(Phenotype1 != Phenotype2) %>%
  mutate(Pheno1_Pheno2 = paste(pmin(Phenotype1, Phenotype2), 
                               pmax(Phenotype1, Phenotype2), 
                               sep = "__")) %>%
  ## Map to phecodes
  left_join(outcome_phecode_pheno, by = join_by("Phenotype1" == "phenotype")) %>%
  rename(Outcome_ds1 = Outcome_phecode) %>%
  left_join(outcome_phecode_pheno, by = join_by("Phenotype2" == "phenotype")) %>%
  rename(Outcome_ds2 = Outcome_phecode) 

###### Remove parent/child outcome pairs ######
effec_size_cor_SE_scaleX_long_parent_child <- effec_size_cor_scaleX_SE_long %>%
  mutate(parent_child_ind = mapply(grepl, pattern = Outcome_ds1, 
                                   x = Outcome_ds2, fixed = TRUE),
         child_parent_ind = mapply(grepl, pattern = Outcome_ds2, 
                                   x = Outcome_ds1, fixed = TRUE)) 

effec_size_cor_scaleX_SE_long_final <- effec_size_cor_SE_scaleX_long_parent_child %>%
  filter(parent_child_ind == F & child_parent_ind == F) %>%
  select(Pheno1_Pheno2, CRS_corr_SE) %>%
  distinct()

saveRDS(effec_size_cor_scaleX_SE_long_final, 
        paste0(summary_dir, "CRS_effec_size_scaleX_correlation_SE_across_outcomes_long.rds"))


###### Read saved file ######
effec_size_cor_scaleX_long_final <- readRDS(paste0(summary_dir, "CRS_effec_size_scaleX_correlation_across_outcomes_long.rds"))
effec_size_cor_scaleX_SE_long_final <- readRDS(paste0(summary_dir, "CRS_effec_size_scaleX_correlation_SE_across_outcomes_long.rds"))

###### Check specific example outcomes ######
CRS_effec_cor_scaleX_Est_SE <- effec_size_cor_scaleX_long_final %>% 
  select("Pheno1_Pheno2", "CRS_corr") %>%
  distinct() %>%
  left_join(effec_size_cor_scaleX_SE_long_final %>% 
              select("Pheno1_Pheno2", "CRS_corr_SE") %>%
              distinct())

CRS_effec_cor_scaleX_Est_SE %>%  
  filter(Pheno1_Pheno2 == "Myocardial infarction__Type 2 diabetes")
#    Pheno1_Pheno2                          CRS_corr CRS_corr_SE
#    Myocardial infarction__Type 2 diabetes    0.620      0.0426


CRS_effec_cor_scaleX_Est_SE %>%  
  filter(Pheno1_Pheno2 == "Hyperlipidemia__Myocardial infarction")
#  Pheno1_Pheno2                         CRS_corr CRS_corr_SE
#  Hyperlipidemia__Myocardial infarction    0.761      0.0291


#### Inspect CRS effect correlations across outcomes ####
## PL 20260702: from here we use CRS corr based on standardized effect sizes
effec_size_cor_scaleX_long_final <- readRDS(paste0(summary_dir, 
                                                   "CRS_effec_size_scaleX_correlation_across_outcomes_long.rds"))

effec_size_cor_scaleX_long_final %>%
  filter(CRS_corr > 0.75) %>%
  nrow()
# 62

effec_size_cor_scaleX_long_final %>%
  filter(CRS_corr > 0.7) %>%
  nrow()
# 115

mean(effec_size_cor_scaleX_long_final$CRS_corr)
# 0.2480859

###### Example diseases ######
effec_size_cor_scaleX_long_final %>%
  filter(Pheno1_Pheno2 %in% c("COVID-19 hospitalisation__Sepsis",
                              "COVID-19 hospitalisation__Pneumonia",
                              "Pneumonia__Sepsis")) %>%
  pull(CRS_corr) %>%
  mean()
# 0.7680985

##### Compare with observed correlation across outcomes #####
## Remove parent/child outcome pairs
obs_corr_summ_parent_child <- obs_corr_summ %>%
  mutate(parent_child_ind = mapply(grepl, pattern = Outcome_ds1, 
                                   x = Outcome_ds2, fixed = TRUE),
         child_parent_ind = mapply(grepl, pattern = Outcome_ds2, 
                                   x = Outcome_ds1, fixed = TRUE)) 

obs_corr_summ_final <- obs_corr_summ_parent_child %>%
  filter(parent_child_ind == F & child_parent_ind == F) %>%
  distinct()

obs_corr_summ_final %>%
  filter(Corr > 0.5) %>%
  nrow()
# 2

mean(obs_corr_summ_final$Corr)
# 0.01730655

###### Example diseases ######
obs_corr_summ_final %>%
  filter(Outcome_ds1 %in% c("COVID_hspt", "480", "994.2"),
         Outcome_ds2 %in% c("COVID_hspt", "480", "994.2")) %>%
  pull(Corr) %>%
  mean()
# 0.2088871

## Join with CRS effect corr
obs_corr_summ_final2 <- obs_corr_summ_final %>%
  mutate(Outcome_ds1 = str_replace(Outcome_ds1, "\\.", "_"),
         Outcome_ds2 = str_replace(Outcome_ds2, "\\.", "_")) %>%
  left_join(outcome_phecode_pheno, by = join_by("Outcome_ds1" == "Outcome_phecode")) %>%
  rename(Phenotype1 = phenotype) %>%
  left_join(outcome_phecode_pheno, by = join_by("Outcome_ds2" == "Outcome_phecode")) %>%
  rename(Phenotype2 = phenotype) %>%
  mutate(Pheno1_Pheno2 = paste(pmin(Phenotype1, Phenotype2), 
                               pmax(Phenotype1, Phenotype2), 
                               sep = "__")) %>%
  rename(Obs_corr = Corr)
#  select(Pheno1_Pheno2, Obs_corr)

CRS_vs_obs_corr <- effec_size_cor_scaleX_long_final %>%
  left_join(obs_corr_summ_final2, by = "Pheno1_Pheno2")

plot(CRS_vs_obs_corr$Obs_corr,
     CRS_vs_obs_corr$CRS_corr,
     xlab = "Observed correlation",
     ylab = "CRS effect correlation",
     xlim = c(-1, 1), 
     ylim = c(-1, 1))
abline(0,1)

##### Compare with liability correlation across outcomes #####
###### Transform to long format ######
liab_outcome_phecode_vec <- str_replace(colnames(liab_corr_summ), '\\.', '_')
  
liab_corr_df <- data.frame(liab_corr_summ)
colnames(liab_corr_df) <- liab_outcome_phecode_vec
rownames(liab_corr_df) <- liab_outcome_phecode_vec

liab_corr_long <- liab_corr_df %>%
  mutate(Outcome_ds1 = rownames(liab_corr_df)) %>%
  pivot_longer(cols = colnames(liab_corr_df), 
               names_to = "Outcome_ds2",
               values_to = "Liab_corr") %>%
  ## Remove diagonal values
  filter(Outcome_ds1 != Outcome_ds2) %>%
  ## Map to phenotypes
  left_join(outcome_phecode_pheno, by = join_by("Outcome_ds1" == "Outcome_phecode")) %>%
  rename(Phenotype1 = phenotype) %>%
  left_join(outcome_phecode_pheno, by = join_by("Outcome_ds2" == "Outcome_phecode")) %>%
  rename(Phenotype2 = phenotype) %>%
  mutate(Pheno1_Pheno2 = paste(pmin(Phenotype1, Phenotype2), 
                               pmax(Phenotype1, Phenotype2), 
                               sep = "__"))

###### Remove parent/child outcome pairs ######
liab_corr_long_parent_child <- liab_corr_long %>%
  mutate(parent_child_ind = mapply(grepl, pattern = Outcome_ds1, 
                                   x = Outcome_ds2, fixed = TRUE),
         child_parent_ind = mapply(grepl, pattern = Outcome_ds2, 
                                   x = Outcome_ds1, fixed = TRUE)) 

liab_corr_long_final <- liab_corr_long_parent_child %>%
  filter(parent_child_ind == F & child_parent_ind == F) %>%
  select(Pheno1_Pheno2, Liab_corr) %>%
  distinct()

liab_corr_long_final %>%
  filter(Liab_corr > 0.5) %>%
  nrow()
# 58

mean(liab_corr_long_final$Liab_corr)
# 0.1182467


##### Save CRS effect corr vs. observed corr vs. liability corr #####
## Join with CRS effect corr and obs corr
CRS_vs_obs_vs_liab_corr <- liab_corr_long_final %>%
  inner_join(CRS_vs_obs_corr, by = "Pheno1_Pheno2") %>%
  mutate(Phecode1 = if_else(Outcome_ds1 == "COVID_hspt", Outcome_ds1,
                            str_replace(Outcome_ds1, "_", ".")),
         Phecode2 = if_else(Outcome_ds2 == "COVID_hspt", Outcome_ds2,
                            str_replace(Outcome_ds2, "_", "."))) %>%
  select(Pheno1_Pheno2,
         Phecode1, 
         Phecode2,
         Phenotype1,
         Phenotype2,
         CRS_corr,
         Liab_corr,
         Obs_corr)


saveRDS(CRS_vs_obs_vs_liab_corr, 
        paste0(summary_dir, "CRS_vs_Obs_vs_Liab_correlation.rds"))

###### Make plots to explore ######

plot(CRS_vs_obs_vs_liab_corr$Obs_corr,
     CRS_vs_obs_vs_liab_corr$Liab_corr,
     xlab = "Observed correlation",
     ylab = "Liability-scale correlation",
     xlim = c(-1, 1), 
     ylim = c(-1, 1))
abline(0,1)

plot(
     CRS_vs_obs_vs_liab_corr$Liab_corr,
     CRS_vs_obs_vs_liab_corr$CRS_corr,
     xlab = "Liability-scale correlation",
     ylab = "CRS effect correlation",
     xlim = c(-1, 1), 
     ylim = c(-1, 1))
abline(0,1)

plot(
     CRS_vs_obs_vs_liab_corr$Obs_corr,
     CRS_vs_obs_vs_liab_corr$CRS_corr,
     xlab = "Observed correlation",
     ylab = "CRS effect correlation",
     xlim = c(-1, 1), 
     ylim = c(-1, 1))
abline(0,1)

colMeans(CRS_vs_obs_vs_liab_corr[,c("CRS_corr", "Liab_corr", "Obs_corr")])
# CRS_corr   Liab_corr   Obs_corr 
# 0.24808592 0.11824671 0.01730655 

colMeans(abs(CRS_vs_obs_vs_liab_corr[,c("CRS_corr", "Liab_corr", "Obs_corr")]))
# CRS_corr  Liab_corr   Obs_corr 
# 0.29099931 0.12676249 0.01770194

###### Example diseases ######
CRS_vs_obs_vs_liab_corr %>%
  filter(Phenotype1 == "Hyperlipidemia" &
         Phenotype2 == "Myocardial infarction") 
#   Phecode1 Phecode2 Phenotype1     Phenotype2            CRS_corr Liab_corr Obs_corr
#  1 272.1    411.2    Hyperlipidemia Myocardial infarction    0.761     0.450    0.128

mean(c(liab_corr_summ[colnames(liab_corr_summ) == "COVID_hspt", 
               rownames(liab_corr_summ) == "480"],
     liab_corr_summ[colnames(liab_corr_summ) == "COVID_hspt", 
                    rownames(liab_corr_summ) == "994.2"],
     liab_corr_summ[colnames(liab_corr_summ) == "480", 
                    rownames(liab_corr_summ) == "994.2"]))
# 0.6028333


liab_corr_summ[colnames(liab_corr_summ) == "250.2", 
               rownames(liab_corr_summ) == "411.2"]
# 0.2095

#### Read genetic correlation ####
init_explore_14pairs <- F

if (init_explore_14pairs == T) {
##### 14 pairs #####
genetic_cor_mt <- readRDS(paste0(summary_dir, 
                                 "Genetic_corr/Results_full_rg_15-15.rds"))
all(rownames(genetic_cor_mt) == colnames(genetic_cor_mt))

genetic_cor_SE <- readRDS(paste0(summary_dir, 
                                 "Genetic_corr/Results_full_rgse.rds"))
all(rownames(genetic_cor_SE) == colnames(genetic_cor_SE))
all(rownames(genetic_cor_SE) == rownames(genetic_cor_mt))

outcome_pheno_map0 <- read.csv(paste0(summary_dir, 
                                     "Genetic_corr/SupplementaryTable1_Outcome_List_MatchGeneticCorr.csv")) 
outcome_pheno_map <- outcome_pheno_map0 %>%
  filter(Trait.Name != "") %>%
  select(Phenotype, Trait.Identifier)
}

##### O2 + UKB combined #####
## Genetic corr Est and SE
genetic_cor_mt <- readRDS(paste0(summary_dir, 
                                 "Genetic_corr/combined_rg_matrix.rds"))
all(rownames(genetic_cor_mt) == colnames(genetic_cor_mt))

genetic_cor_SE <- readRDS(paste0(summary_dir, 
                                 "Genetic_corr/combined_rg_se_matrix.rds"))
all(rownames(genetic_cor_SE) == colnames(genetic_cor_SE))
all(rownames(genetic_cor_SE) == rownames(genetic_cor_mt))

## heritability estimates
h2g_O2 <- read.csv(paste0(summary_dir, 
                          "Genetic_corr/h2g_15diseases.csv")) %>%
  mutate(Genetic_trait_name = paste0(traitname, ".sumstats")) %>%
  select(Genetic_trait_name, h2g, h2gse, h2_Z)
h2g_PanUKB <- read.csv(paste0(summary_dir, 
                          "Genetic_corr/h2g_PanUKB.csv")) %>%
  rename(Genetic_trait_name = traits,
         h2g = h2,
         h2gse = h2se,
         h2_Z = h2Z) %>%
  select(Genetic_trait_name, h2g, h2gse, h2_Z)
h2g_combined <- rbind(h2g_O2, h2g_PanUKB)

outcome_pheno_map0 <- read.csv(paste0(summary_dir, 
                                      "Genetic_corr/downloaded_panUKB_sumstat.csv")) 
outcome_pheno_map00 <- outcome_pheno_map0 %>%
  filter(Trait.Identifier.in.O2 != "" | wget_from.Pan.UK.Biobank != "") %>%
  select(Phenotype, Trait.Identifier.in.O2, wget_from.Pan.UK.Biobank) %>%
  mutate(Trait_O2 = if_else(Trait.Identifier.in.O2 == "", "",
              paste0(Trait.Identifier.in.O2, ".sumstats")),
         Trait_PanUKB = if_else(wget_from.Pan.UK.Biobank == "", "",
              paste0(sub(".*/", "", wget_from.Pan.UK.Biobank), ".sumstats.gz"))) %>%
  select(Phenotype, Trait_O2, Trait_PanUKB)

## concatenate O2 and PanUKB phenotypes
outcome_pheno_map <- outcome_pheno_map00 %>%
  pivot_longer(cols = c("Trait_O2", "Trait_PanUKB"),
               names_to = "Genetic_trait_type",
               values_to = "Genetic_trait_name") %>%
  filter(Genetic_trait_name != "") %>%
  ## filter to traits with h2_Z > threshold (4 or 6)
  inner_join(h2g_combined, by = "Genetic_trait_name") %>%
  filter(h2_Z > 6)


#### Convert genetic correlation SE matrix to long format ####
genetic_cor_SE_df <- data.frame(genetic_cor_SE)
colnames(genetic_cor_SE_df) <- rownames(genetic_cor_SE_df)

genetic_cor_SE_long0 <- genetic_cor_SE_df %>%
  mutate(Genetic_Phenotype1 = rownames(genetic_cor_SE_df)) %>%
  pivot_longer(cols = colnames(genetic_cor_SE_df), 
               names_to = "Genetic_Phenotype2",
               values_to = "Genetic_corr_SE")

genetic_cor_SE_long0 %>%
  filter(Genetic_Phenotype1 == "phecode-411.2-both_sexes.tsv.bgz.sumstats.gz" & 
           Genetic_Phenotype2 == "PASS_Type2Diabetes_Xue2018.sumstats")
# 0.0378

genetic_cor_SE_long0 %>%
  filter(Genetic_Phenotype1 == "phecode-411.2-both_sexes.tsv.bgz.sumstats.gz" & 
           Genetic_Phenotype2 == "phecode-250.2-both_sexes.tsv.bgz.sumstats.gz")
# 0.0402

genetic_cor_SE_long00 <- genetic_cor_SE_long0 %>%
  ## Map to phecode phenotypes 
  inner_join(outcome_pheno_map %>% select(Phenotype, Genetic_trait_name), 
            by = join_by("Genetic_Phenotype1" == "Genetic_trait_name")) %>%
  rename(Phenotype1 = Phenotype) %>%
  inner_join(outcome_pheno_map %>% select(Phenotype, Genetic_trait_name), 
            by = join_by("Genetic_Phenotype2" == "Genetic_trait_name")) %>%
  rename(Phenotype2 = Phenotype) 

genetic_cor_SE_long <- genetic_cor_SE_long00 %>%
  filter(Genetic_Phenotype1 != Genetic_Phenotype2) %>%
  ## For the same disease pair with different genetic phenotypes, select the pair with lowest genetic corr SE 
  group_by(Phenotype1, Phenotype2) %>%
  filter(Genetic_corr_SE == min(Genetic_corr_SE)) %>%
  ungroup() %>%
  ## Keep only distinct pairs
  mutate(Pheno1_Pheno2 = paste(pmin(Phenotype1, Phenotype2), 
                               pmax(Phenotype1, Phenotype2), 
                               sep = "__"),
         GeneticPheno1_GeneticPheno2 = paste(pmin(Genetic_Phenotype1, Genetic_Phenotype2), 
                                             pmax(Genetic_Phenotype1, Genetic_Phenotype2), 
                                             sep = "__")) %>%
  select(Pheno1_Pheno2, GeneticPheno1_GeneticPheno2, Genetic_corr_SE) %>%
  distinct()


#### Convert genetic correlation matrix to long format ####
genetic_cor_df <- data.frame(genetic_cor_mt)
colnames(genetic_cor_df) <- rownames(genetic_cor_df)

genetic_cor_long0 <- genetic_cor_df %>%
  mutate(Genetic_Phenotype1 = rownames(genetic_cor_df)) %>%
  pivot_longer(cols = colnames(genetic_cor_df), 
               names_to = "Genetic_Phenotype2",
               values_to = "Genetic_corr")

genetic_cor_long0 %>%
  filter(Genetic_Phenotype1 == "phecode-411.2-both_sexes.tsv.bgz.sumstats.gz" & 
         Genetic_Phenotype2 == "PASS_Type2Diabetes_Xue2018.sumstats")
# 0.424

genetic_cor_long0 %>%
  filter(Genetic_Phenotype1 == "phecode-411.2-both_sexes.tsv.bgz.sumstats.gz" & 
           Genetic_Phenotype2 == "phecode-250.2-both_sexes.tsv.bgz.sumstats.gz")
# 0.482

genetic_cor_long00 <- genetic_cor_long0 %>%
  ## Map to phecode phenotypes 
  inner_join(outcome_pheno_map %>% select(Phenotype, Genetic_trait_name), 
            by = join_by("Genetic_Phenotype1" == "Genetic_trait_name")) %>%
  rename(Phenotype1 = Phenotype) %>%
  inner_join(outcome_pheno_map %>% select(Phenotype, Genetic_trait_name), 
            by = join_by("Genetic_Phenotype2" == "Genetic_trait_name")) %>%
  rename(Phenotype2 = Phenotype) 


genetic_cor_long <- genetic_cor_long00 %>%
  ## Keep only distinct pairs
  filter(Genetic_Phenotype1 != Genetic_Phenotype2) %>%
  mutate(Pheno1_Pheno2 = paste(pmin(Phenotype1, Phenotype2), 
                               pmax(Phenotype1, Phenotype2), 
                               sep = "__"),
         GeneticPheno1_GeneticPheno2 = paste(pmin(Genetic_Phenotype1, Genetic_Phenotype2), 
                                            pmax(Genetic_Phenotype1, Genetic_Phenotype2), 
                                            sep = "__")) %>%
  select(Phenotype1, Phenotype2,
         Pheno1_Pheno2, GeneticPheno1_GeneticPheno2, Genetic_corr) %>%
  distinct()

#### Save CRS effect corr vs. observed corr vs. liability corr vs. genetic corr ####
genetic_cor_Est_SE_long_distinct <- genetic_cor_SE_long %>%
  inner_join(genetic_cor_long, by = c("GeneticPheno1_GeneticPheno2",
                                      "Pheno1_Pheno2")) %>%
  ## remove ischemic stroke due to problematic genetic corr
  filter((Phenotype1 != "Cerebral artery occlusion, with cerebral infarction") & 
           (Phenotype2 != "Cerebral artery occlusion, with cerebral infarction")) %>%
  select(Pheno1_Pheno2,  GeneticPheno1_GeneticPheno2, 
         Genetic_corr, Genetic_corr_SE) %>%
  distinct()

CRS_vs_obs_vs_liab_corr <- readRDS(paste0(summary_dir, "CRS_vs_Obs_vs_Liab_correlation.rds"))
CRS_vs_obs_vs_liab_vs_genetic_corr <- CRS_vs_obs_vs_liab_corr %>%
  inner_join(effec_size_cor_scaleX_SE_long_final %>%
               select(Pheno1_Pheno2, CRS_corr_SE) %>%
               distinct(), by = "Pheno1_Pheno2") %>%
  left_join(genetic_cor_Est_SE_long_distinct, by = "Pheno1_Pheno2") %>%
  mutate(Genetic_corr_available = if_else(is.na(Genetic_corr), 0, 1)) %>%
  select(Pheno1_Pheno2, Phecode1, Phecode2, Phenotype1, Phenotype2,
         CRS_corr, CRS_corr_SE,
         Obs_corr,
         Liab_corr,
         Genetic_corr_available, GeneticPheno1_GeneticPheno2,
         Genetic_corr, Genetic_corr_SE
         )

sum(CRS_vs_obs_vs_liab_vs_genetic_corr$Genetic_corr_available == 1)
# 936

saveRDS(CRS_vs_obs_vs_liab_vs_genetic_corr, 
        paste0(summary_dir, "CRS_vs_Obs_vs_Liab_vs_Genetic_correlation.rds"))


#### Fig. 1e: Compare CRS effect size correlation with genetic correlation ####
CRS_vs_genetic_cor_long <- genetic_cor_SE_long %>%
  inner_join(genetic_cor_long, by = c("GeneticPheno1_GeneticPheno2",
                                     "Pheno1_Pheno2")) %>%
  inner_join(effec_size_cor_scaleX_long_final %>%
              select(Pheno1_Pheno2, CRS_corr) %>%
              distinct(), by = "Pheno1_Pheno2") %>%
  inner_join(effec_size_cor_scaleX_SE_long_final %>%
              select(Pheno1_Pheno2, CRS_corr_SE) %>%
              distinct(), by = "Pheno1_Pheno2")

## Check number of phenotypes involved
c(CRS_vs_genetic_cor_long$Phenotype1,
  CRS_vs_genetic_cor_long$Phenotype2) %>%
  unique() %>%
  length()
# 45

## Check phenotypes with low genetic corr 
CRS_vs_genetic_cor_long_by_pheno <- CRS_vs_genetic_cor_long %>%
  group_by(Phenotype1) %>%
  mutate(Genetic_corr_LB = min(Genetic_corr),
         Genetic_corr_UB = max(Genetic_corr)) %>%
  select(Phenotype1, Genetic_corr_LB, Genetic_corr_UB) %>%
  distinct()
View(CRS_vs_genetic_cor_long_by_pheno)


##### Check example outcome pairs #####
CRS_vs_genetic_cor_long %>%  
    filter(Pheno1_Pheno2 == "Hyperlipidemia__Myocardial infarction") %>%
  select(Pheno1_Pheno2, Genetic_corr, Genetic_corr_SE,
         CRS_corr, CRS_corr_SE) %>%
  distinct()
#   Pheno1_Pheno2                       Genetic_corr Genetic_corr_SE CRS_corr CRS_corr_SE
#  Hyperlipidemia__Myocardial infarct…        0.791          0.0615    0.761      0.0291

##### Generate combined dataset distinct disease pairs #####
CRS_vs_genetic_cor_long_distinct <- CRS_vs_genetic_cor_long %>% 
  ## remove ischemic stroke due to problematic genetic corr
  filter((Phenotype1 != "Cerebral artery occlusion, with cerebral infarction") & 
           (Phenotype2 != "Cerebral artery occlusion, with cerebral infarction")) %>%
  select(Pheno1_Pheno2,  GeneticPheno1_GeneticPheno2, 
         CRS_corr, Genetic_corr, Genetic_corr_SE, CRS_corr_SE) %>% 
  distinct()


plot(CRS_vs_genetic_cor_long_distinct$Genetic_corr,
     CRS_vs_genetic_cor_long_distinct$CRS_corr,
     xlim = c(-0.5,1),
     ylim = c(-0.5,1),
     xlab = "Genetic correlation",
     ylab = "CRS effect correlation")
abline(0,1)

smoothScatter(CRS_vs_genetic_cor_long_distinct$Genetic_corr,
              CRS_vs_genetic_cor_long_distinct$CRS_corr,
              xlim = c(-0.5,1),
              ylim = c(-0.5,1),
              xlab = "Genetic correlation",
              ylab = "CRS effect correlation")
abline(0,1)
abline(lm(CRS_corr ~ 0 + Genetic_corr, data = CRS_vs_genetic_cor_long_distinct))


smoothScatter(CRS_vs_genetic_cor_long_distinct$CRS_corr,
              CRS_vs_genetic_cor_long_distinct$Genetic_corr,
              xlim = c(-0.5,1),
              ylim = c(-0.5,1),
              xlab = "CRS effect correlation",
              ylab = "Genetic correlation")
abline(0,1)
abline(lm(Genetic_corr ~ 0 + CRS_corr, data = CRS_vs_genetic_cor_long))


cor.test(CRS_vs_genetic_cor_long_distinct$Genetic_corr,
         CRS_vs_genetic_cor_long_distinct$CRS_corr)
# 0.4436754
# 0.4856738 (removing ischemic stroke/phecode 433.21)


mean(CRS_vs_genetic_cor_long_distinct$Genetic_corr^2)/mean(CRS_vs_genetic_cor_long_distinct$CRS_corr^2)
# 0.6216376  


summary(lm(CRS_corr ~ 0 + Genetic_corr, data = CRS_vs_genetic_cor_long_distinct))

summary(lm(Genetic_corr ~ 0 + CRS_corr, data = CRS_vs_genetic_cor_long_distinct))

##### Correct final correlation using SE #####
lambda_genetic_corr <- 1 - mean(CRS_vs_genetic_cor_long_distinct$Genetic_corr_SE^2, 
                                na.rm = T)/var(CRS_vs_genetic_cor_long_distinct$Genetic_corr, na.rm = T)

lambda_CRS_corr <- 1 - mean(CRS_vs_genetic_cor_long_distinct$CRS_corr_SE^2, 
                            na.rm = T)/var(CRS_vs_genetic_cor_long_distinct$CRS_corr, na.rm = T)

noise_adjusted_cor <- cor(CRS_vs_genetic_cor_long_distinct$Genetic_corr,
                          CRS_vs_genetic_cor_long_distinct$CRS_corr)/sqrt(lambda_genetic_corr * lambda_CRS_corr)
noise_adjusted_cor
# 0.4856664
# 0.5337346 (removing ischemic stroke/phecode 433.21)

##### Use n quantiles for plotting #####
CRS_vs_genetic_cor_long_q <- CRS_vs_genetic_cor_long_distinct %>%
  mutate(Genetic_corr_quantile = ntile(Genetic_corr, n = 50)) %>%
  group_by(Genetic_corr_quantile) %>%
  mutate(Genetic_corr_mean_within_quantile = mean(Genetic_corr),
         CRS_corr_mean_within_quantile = mean(CRS_corr)) %>%
  ungroup() %>%
  select(Genetic_corr_quantile, 
         Genetic_corr_mean_within_quantile, 
         CRS_corr_mean_within_quantile) %>%
  distinct()

##### Plot function #####
crs_effec_size_compare_plot <- function(CRS_vs_genetic_cor_long_q,
                                        dodge = position_dodge(0.8),
                                        ref_linewidth = 0.6,
                                        metric_lower_lim = -0.5,
                                        metric_upper_lim = 1,
                                        metric_breaks = 0.5,
                                        point_size = 4,
                                        base_size = base_font_size,
                                        error_bar_width = 0.01,
                                        model_fit = F,
                                        color_by_incid = F,
                                        text_label_size = 5,
                                        point_padding = 1,
                                        box_padding = 0.5,
                                        nudge_x = 0,
                                        nudge_y = 0,
                                        seed = 100,
                                        save_format = "pdf",
                                        fig_width = 7,
                                        fig_height = 6) {
  plot_corr_CRS_vs_genetic <- ggplot(CRS_vs_genetic_cor_long_q, 
                                    aes(x = Genetic_corr_mean_within_quantile, 
                                        y = CRS_corr_mean_within_quantile)) + 
    geom_abline(slope = 1, intercept = 0, 
                linewidth = ref_linewidth) +
    geom_hline(yintercept = 0, linetype = "dashed") + 
    geom_vline(xintercept = 0, linetype = "dashed") + 
    geom_point(size = point_size,
               color = grey) +
    # scale_color_manual(values = c("1" = red, "0" = grey)) +
    #      geom_errorbar(aes(ymin = test_R2_LB, ymax = test_R2_UB, color = Incidence_group), 
    #                    width = error_bar_width) +
    #      geom_errorbarh(aes(xmin = train_R2_LB, xmax = train_R2_UB, color = Incidence_group), 
    #                     height = error_bar_width) +
    scale_y_continuous(limits = c(metric_lower_lim, metric_upper_lim), 
                       breaks = seq(metric_lower_lim, metric_upper_lim, metric_breaks)) +
    scale_x_continuous(limits = c(metric_lower_lim, metric_upper_lim), 
                       breaks = seq(metric_lower_lim, metric_upper_lim, metric_breaks)) +
    theme_bw(base_size = base_size)+
    theme( 
      legend.position="right",
      # panel.border = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank()
    ) + 
    labs(x = "Genetic correlation", 
         y = "CRS effect correlation")
  
  ggsave(filename = paste0(figure_dir, "/General_performance/Panel5_CRS_corr_vs_genetic_corr.", save_format), 
         plot_corr_CRS_vs_genetic, width = fig_width, height = fig_height, limitsize = FALSE)
}


##### Make plot #####
crs_effec_size_compare_plot(CRS_vs_genetic_cor_long_q,
                                        dodge = position_dodge(0.8),
                                        ref_linewidth = 0.6,
                                        metric_lower_lim = -0.4,
                                        metric_upper_lim = 1,
                                        metric_breaks = 0.4,
                                        fig_width = 7,
                                        fig_height = 6) 


#### Fig. S6: Other comparisons among different correlations ####
##### Read combined summ file #####
CRS_vs_obs_vs_liab_vs_genetic_corr <- readRDS(paste0(summary_dir, "CRS_vs_Obs_vs_Liab_vs_Genetic_correlation.rds"))

CRS_vs_obs_vs_liab_vs_genetic_corr_subset <- CRS_vs_obs_vs_liab_vs_genetic_corr %>%
  filter(Genetic_corr_available == 1)

##### Correct final correlation using SE #####
lambda_genetic_corr <- 1 - mean(CRS_vs_obs_vs_liab_vs_genetic_corr_subset$Genetic_corr_SE^2, 
                                na.rm = T)/var(CRS_vs_obs_vs_liab_vs_genetic_corr_subset$Genetic_corr, na.rm = T)

lambda_CRS_corr <- 1 - mean(CRS_vs_obs_vs_liab_vs_genetic_corr_subset$CRS_corr_SE^2, 
                            na.rm = T)/var(CRS_vs_obs_vs_liab_vs_genetic_corr_subset$CRS_corr, na.rm = T)

lambda_Obs_corr <- 1 
lambda_Liab_corr <- 1 


noise_adjusted_Obs_vs_genetic_cor <- cor(CRS_vs_obs_vs_liab_vs_genetic_corr_subset$Genetic_corr,
                                         CRS_vs_obs_vs_liab_vs_genetic_corr_subset$Obs_corr)/sqrt(lambda_genetic_corr * lambda_Obs_corr)

noise_adjusted_Liab_vs_genetic_cor <- cor(CRS_vs_obs_vs_liab_vs_genetic_corr_subset$Genetic_corr,
                                         CRS_vs_obs_vs_liab_vs_genetic_corr_subset$Liab_corr)/sqrt(lambda_genetic_corr * lambda_Liab_corr)

noise_adjusted_CRS_vs_genetic_cor <- cor(CRS_vs_obs_vs_liab_vs_genetic_corr_subset$Genetic_corr,
                                         CRS_vs_obs_vs_liab_vs_genetic_corr_subset$CRS_corr)/sqrt(lambda_genetic_corr * lambda_CRS_corr)


noise_adjusted_Obs_vs_genetic_cor
# 0.5478493
noise_adjusted_Liab_vs_genetic_cor
# 0.4888515
noise_adjusted_CRS_vs_genetic_cor
# 0.5337346

##### Get slope from linear regression: Other corr's vs. genetic corr #####
## Regression through the origin
summary(lm(CRS_corr ~ 0 + Genetic_corr, 
           data = CRS_vs_obs_vs_liab_vs_genetic_corr_subset))
# Coefficients:
#               Estimate Std. Error t value Pr(>|t|)    
# Genetic_corr  0.93266    0.02594   35.95   <2e-16 ***

summary(lm(Liab_corr ~ 0 + Genetic_corr, 
           data = CRS_vs_obs_vs_liab_vs_genetic_corr_subset))
# Coefficients:
#               Estimate Std. Error t value Pr(>|t|)    
# Genetic_corr  0.45698    0.01285   35.57   <2e-16 ***

summary(lm(Obs_corr ~ 0 + Genetic_corr, 
           data = CRS_vs_obs_vs_liab_vs_genetic_corr_subset))
# Coefficients:
#              Estimate Std. Error t value Pr(>|t|)    
# Genetic_corr 0.088564   0.002922   30.31   <2e-16 ***


## Usual regression
summary(lm(CRS_corr ~ Genetic_corr, 
           data = CRS_vs_obs_vs_liab_vs_genetic_corr_subset))
# Coefficients:
#              Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.133263   0.009656   13.80   <2e-16 ***
# Genetic_corr 0.585922   0.034507   16.98   <2e-16 ***

summary(lm(Liab_corr ~ Genetic_corr, 
           data = CRS_vs_obs_vs_liab_vs_genetic_corr_subset))
# Coefficients:
#              Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.073861   0.004657   15.86   <2e-16 ***
# Genetic_corr 0.264798   0.016643   15.91   <2e-16 ***

summary(lm(Obs_corr ~ Genetic_corr, 
           data = CRS_vs_obs_vs_liab_vs_genetic_corr_subset))
# Coefficients:
#              Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.003913   0.001187   3.297  0.00101 ** 
# Genetic_corr 0.078384   0.004241  18.484  < 2e-16 ***

##### Get slope from linear regression: Other corr's vs. CRS corr #####
## Regression through the origin
summary(lm(Liab_corr ~ 0 + CRS_corr, 
           data = CRS_vs_obs_vs_liab_vs_genetic_corr))
# Coefficients:
#          Estimate Std. Error t value Pr(>|t|)    
# CRS_corr 0.377072   0.005017   75.16   <2e-16 ***

summary(lm(Obs_corr ~ 0 + CRS_corr, 
           data = CRS_vs_obs_vs_liab_vs_genetic_corr))
# Coefficients:
#           Estimate Std. Error t value Pr(>|t|)    
#  CRS_corr 0.066387   0.001392   47.69   <2e-16 ***


##### Use n quantiles for plotting #####
CRS_vs_obs_vs_liab_vs_genetic_corr_subset_q <- CRS_vs_obs_vs_liab_vs_genetic_corr_subset %>%
  mutate(Genetic_corr_quantile = ntile(Genetic_corr, n = 50)) %>%
  group_by(Genetic_corr_quantile) %>%
  mutate(Genetic_corr_mean_within_quantile = mean(Genetic_corr),
         CRS_corr_mean_within_quantile = mean(CRS_corr),
         Obs_corr_mean_within_quantile = mean(Obs_corr),
         Liab_corr_mean_within_quantile = mean(Liab_corr)) %>%
  ungroup() %>%
  select(Genetic_corr_quantile, 
         Genetic_corr_mean_within_quantile, 
         CRS_corr_mean_within_quantile,
         Obs_corr_mean_within_quantile,
         Liab_corr_mean_within_quantile) %>%
  distinct()

CRS_vs_obs_vs_liab_corr_q <- CRS_vs_obs_vs_liab_vs_genetic_corr %>%
  select(!c(Genetic_corr_available, 
           GeneticPheno1_GeneticPheno2,
           Genetic_corr,
           Genetic_corr_SE)) %>%
  mutate(CRS_corr_quantile = ntile(CRS_corr, n = 50)) %>%
  group_by(CRS_corr_quantile) %>%
  mutate(CRS_corr_mean_within_quantile = mean(CRS_corr),
         Obs_corr_mean_within_quantile = mean(Obs_corr),
         Liab_corr_mean_within_quantile = mean(Liab_corr)) %>%
  ungroup() %>%
  select(CRS_corr_quantile,
         CRS_corr_mean_within_quantile,
         Obs_corr_mean_within_quantile,
         Liab_corr_mean_within_quantile) %>%
  distinct()

##### Plot function #####
general_effec_size_compare_plot <- function(corr_compare_q,
                                            x_quantile_name,
                                            y_quantile_name,
                                        dodge = position_dodge(0.8),
                                        ref_linewidth = 0.6,
                                        metric_lower_lim = -0.5,
                                        metric_upper_lim = 1,
                                        metric_breaks = 0.5,
                                        point_size = 4,
                                        base_size = base_font_size,
                                        error_bar_width = 0.01,
                                        model_fit = F,
                                        color_by_incid = F,
                                        text_label_size = 5,
                                        point_padding = 1,
                                        box_padding = 0.5,
                                        nudge_x = 0,
                                        nudge_y = 0,
                                        seed = 100,
                                        save_format = "pdf",
                                        fig_width = 7,
                                        fig_height = 6) {
  
  plot_corr <- ggplot(corr_compare_q, 
                                     aes(x = x_quantile, 
                                         y = y_quantile)) + 
    geom_abline(slope = 1, intercept = 0, 
                linewidth = ref_linewidth) +
    geom_hline(yintercept = 0, linetype = "dashed") + 
    geom_vline(xintercept = 0, linetype = "dashed") + 
    geom_point(size = point_size,
               color = grey) +
    # scale_color_manual(values = c("1" = red, "0" = grey)) +
    #      geom_errorbar(aes(ymin = test_R2_LB, ymax = test_R2_UB, color = Incidence_group), 
    #                    width = error_bar_width) +
    #      geom_errorbarh(aes(xmin = train_R2_LB, xmax = train_R2_UB, color = Incidence_group), 
    #                     height = error_bar_width) +
    scale_y_continuous(limits = c(metric_lower_lim, metric_upper_lim), 
                       breaks = seq(metric_lower_lim, metric_upper_lim, metric_breaks)) +
    scale_x_continuous(limits = c(metric_lower_lim, metric_upper_lim), 
                       breaks = seq(metric_lower_lim, metric_upper_lim, metric_breaks)) +
    theme_bw(base_size = base_size)+
    theme( 
      legend.position="right",
      # panel.border = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank()
    ) + 
    labs(x = x_quantile_name, 
         y = y_quantile_name)
  
  x_save_name <- gsub("[^A-Za-z0-9_]", "_", x_quantile_name)
  y_save_name <- gsub("[^A-Za-z0-9_]", "_", y_quantile_name)
  
  ggsave(filename = paste0(figure_dir, "/General_performance/", 
                           x_save_name, "_vs_",
                           y_save_name, ".",
                           save_format), 
         plot_corr, width = fig_width, height = fig_height, limitsize = FALSE)
}

##### Observed corr vs. genetic corr #####
corr_compare_q <- CRS_vs_obs_vs_liab_vs_genetic_corr_subset_q %>%
  rename(x_quantile = Genetic_corr_mean_within_quantile,
         y_quantile = Obs_corr_mean_within_quantile)

general_effec_size_compare_plot(corr_compare_q,
                                "Genetic correlation",
                                "Observed correlation")

##### Liability corr vs. genetic corr #####
corr_compare_q <- CRS_vs_obs_vs_liab_vs_genetic_corr_subset_q %>%
  rename(x_quantile = Genetic_corr_mean_within_quantile,
         y_quantile = Liab_corr_mean_within_quantile)

general_effec_size_compare_plot(corr_compare_q,
                                "Genetic correlation",
                                "Liability correlation")


##### Observed corr vs. CRS corr #####
corr_compare_q <- CRS_vs_obs_vs_liab_corr_q %>%
  rename(x_quantile = CRS_corr_mean_within_quantile,
         y_quantile = Obs_corr_mean_within_quantile)

general_effec_size_compare_plot(corr_compare_q,
                                "CRS effect correlation",
                                "Observed correlation")

##### Liability corr vs. CRS corr #####
corr_compare_q <- CRS_vs_obs_vs_liab_corr_q %>%
  rename(x_quantile = CRS_corr_mean_within_quantile,
         y_quantile = Liab_corr_mean_within_quantile)

general_effec_size_compare_plot(corr_compare_q,
                                "CRS effect correlation",
                                "Liability correlation")

