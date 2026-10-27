### Author: Pearl Liu
### Get effect size for specific predictors of interest
### Get correlation in effect sizes from different models
### Odd DOB: training, Even DOB: testing


rm(list = ls())
gc()

#### Load libraries ####
library(data.table)
library(tidyverse)
library(sandwich)
library(doParallel)

source('~/collab/CCU022/CCU022_CRS/0.1_Functions.R')

#### Set parameters ####
temp_data_dir_step1_0 <- '~/collab/CCU022/CCU022_CRS/Data/Step1.0/'
temp_data_dir_step1_1 <- '~/collab/CCU022/CCU022_CRS/Data/Step1.1/'
temp_data_dir_step2_4 <- '~/collab/CCU022/CCU022_CRS/Data/Step2.4/'
temp_data_dir_step3_0 <- '~/collab/CCU022/CCU022_CRS/Data/Step3.0/'
temp_data_dir <- '~/collab/CCU022/CCU022_CRS/Data/Step3.2/'
file_date <- '2025_05_01'

## Control-case ratio in logistic regression of training data
control_case_ratio <- 4
## Number of covariates 
n_covar <- 2
## Model names
model_name_vec <- c("CRS_all_predictors", 
                    "CRS_top100",
                    "CRS_top50", 
                    "CRS_top20", 
                    "CRS_top10",
                    "Age_Sex")
n_models <- length(model_name_vec)
## Number of cores used in parallel computing
n_cores <- 6

#### Read processed data ####
##### List of predictors and outcomes - include COVID #####
pred_ds_list_name = paste0("ccu022_01_Predictor_Diseases_Pheparent_list_by_PL_", file_date)
outcome_ds_list_name = paste0("ccu022_01_Outcome_Diseases_Phecode_list_by_PL_", file_date)
outcome_ds_list_name2 = paste0("ccu022_01_Outcome_Diseases_Phecode_list2_TopCommonPlusPheRS_by_PL_", file_date)

pred_ds_list = readRDS(paste0(temp_data_dir_step1_0, pred_ds_list_name))
outcome_ds_list = readRDS(paste0(temp_data_dir_step1_0, outcome_ds_list_name))
outcome_ds_list2 = readRDS(paste0(temp_data_dir_step1_0, outcome_ds_list_name2))

pred_ds_vec <- pred_ds_list %>% arrange(index) %>% pull(predictor_ds) 
outcome_ds_vec <- c(outcome_ds_list %>% 
                      arrange(index) %>%
                      ## Remove phecode 622
                      filter(target_ds != "622") %>%
                      pull(target_ds),
                    "COVID_hspt",
                    outcome_ds_list2 %>% arrange(index) %>% pull(target_ds),
                    "death")

n_pred_ds <- length(pred_ds_vec)
n_outcome_ds <- length(outcome_ds_vec)

##### Predictor prevalence #####
pred_ds_prev <- readRDS(paste0(temp_data_dir_step1_1, 
                               'WhiteBritish_OddYOB_Index2015_PredictorDiseasePrevalence.rds'))

##### Outcome incidence #####
outcome_incid_vec <- readRDS(paste0(temp_data_dir_step2_4, 
                                    'TrainData_Outcome_Incidence_OddYOB_Index2015_Followup10yr.rds'))

outcome_incid_vec_sf <- readRDS(paste0(temp_data_dir_step2_4, 
                                       'TrainData_Outcome_Incidence_Sex_Specific_OddYOB_Index2015_Followup10yr.rds'))

##### Phecode information #####
phe_icd10_codelist_name = paste0("ccu022_01_PheCode_ICD10AtoN_map_by_PL_", file_date)
icd10_phecode_map <- readRDS(paste0(temp_data_dir_step1_0, 
                                    phe_icd10_codelist_name))
phecode_info <- icd10_phecode_map %>%
  select(PheCode, PheCode_parent, 
         Phenotype, Phenotype_parent,
         Category, Sex) %>%
  distinct()

## Info on sex-specificity of predictor phecodes
pred_phecode_info0 <- phecode_info %>%
  select(PheCode_parent, Phenotype_parent, Sex) %>%
  distinct() %>%
  filter(PheCode_parent %in% names(pred_ds_prev)) %>%
  group_by(PheCode_parent) %>%
  mutate(Sex_parent = case_when(all(Sex == "Female")  ~ "Female",
                                all(Sex == "Male") ~ "Male",
                                TRUE ~ "Both"))

pred_phecode_info <- pred_phecode_info0 %>%
  select(PheCode_parent, Sex_parent) %>%
  distinct() %>%
  arrange(PheCode_parent)

#### Phecode 318 for Outcomes: COVID hspt and sepsis (994.2) ####
##### Predictor prevalence for target predictor #####
pred_ds_prev[which(pred_ds_vec == "318")]
# 318 
# 0.07895079

##### COVID hspt #####
target_ds <- "COVID_hspt"
train_df_fit <- readRDS(paste0(temp_data_dir_step2_4, 'Outcome',
                               target_ds, '_TrainData_Fit_ControlCase', control_case_ratio,
                               'to1_WhiteBritish_OddYOB_Index2020_Followup5yr.rds'))

model_j <- glm(target_ds_status ~ ds318 + age_at_2020 + Female,
               weights = ipcw_wts,
               data = train_df_fit,
               family = binomial())

vcov_model_j <- vcovHC(model_j, type = 'HC')

model_j_est <- summary(model_j)$coefficients[,1]
model_j_se <- sqrt(diag(vcov_model_j))

model_j_summ <- data.frame(Est = model_j_est,
                           SE = model_j_se)
model_j_summ
#              Est           SE
# (Intercept) -3.92682360 0.0319670183
# ds318        0.67511629 0.0109927345
# age_at_2020  0.04422769 0.0005442951
# Female      -0.18507453 0.0086277245

##### Sepsis #####
target_ds <- "994_2"
train_df_fit <- readRDS(paste0(temp_data_dir_step2_4, 'Outcome',
                               target_ds, '_TrainData_Fit_ControlCase', control_case_ratio,
                               'to1_WhiteBritish_OddYOB_Index2015_Followup10yr.rds'))

model_j <- glm(target_ds_status ~ ds318 + age_at_2015 + Female,
               weights = ipcw_wts,
               data = train_df_fit,
               family = binomial())

vcov_model_j <- vcovHC(model_j, type = 'HC')

model_j_est <- summary(model_j)$coefficients[,1]
model_j_se <- sqrt(diag(vcov_model_j))

model_j_summ <- data.frame(Est = model_j_est,
                           SE = model_j_se)
model_j_summ
#             Est           SE
# (Intercept) -5.33446985 0.0198835460
# ds318        1.10602353 0.0080194966
# age_at_2015  0.06795755 0.0003309076
# Female      -0.25758837 0.0052686279

#### Phecode 272 for Outcomes: MI (411.2) and stroke (433.21) ####
##### Predictor prevalence for target predictor #####
pred_ds_prev[which(pred_ds_vec == "272")]
# 272 
# 0.1018413

##### MI #####
target_ds <- "411_2"
train_df_fit <- readRDS(paste0(temp_data_dir_step2_4, 'Outcome',
                               target_ds, '_TrainData_Fit_ControlCase', control_case_ratio,
                               'to1_WhiteBritish_OddYOB_Index2015_Followup10yr.rds'))

model_j <- glm(target_ds_status ~ ds272 + age_at_2015 + Female,
               weights = ipcw_wts,
               data = train_df_fit,
               family = binomial())

vcov_model_j <- vcovHC(model_j, type = 'HC')

model_j_est <- summary(model_j)$coefficients[,1]
model_j_se <- sqrt(diag(vcov_model_j))

model_j_summ <- data.frame(Est = model_j_est,
                           SE = model_j_se)
model_j_summ
#              Est           SE
# (Intercept) -4.0086934 0.0176212065
# ds272        0.8411392 0.0069033947
# age_at_2015  0.0497221 0.0003034718
# Female      -0.8744716 0.0051235754

##### Stroke #####
target_ds <- "433_21"
train_df_fit <- readRDS(paste0(temp_data_dir_step2_4, 'Outcome',
                               target_ds, '_TrainData_Fit_ControlCase', control_case_ratio,
                               'to1_WhiteBritish_OddYOB_Index2015_Followup10yr.rds'))

model_j <- glm(target_ds_status ~ ds272 + age_at_2015 + Female,
               weights = ipcw_wts,
               data = train_df_fit,
               family = binomial())

vcov_model_j <- vcovHC(model_j, type = 'HC')

model_j_est <- summary(model_j)$coefficients[,1]
model_j_se <- sqrt(diag(vcov_model_j))

model_j_summ <- data.frame(Est = model_j_est,
                           SE = model_j_se)
model_j_summ
#              Est           SE
# (Intercept) -4.78756320 0.0246599315
# ds272        0.45853950 0.0095675762
# age_at_2015  0.06116815 0.0004181801
# Female      -0.47709897 0.0067344193


#### Correlation in effect sizes from different models ####

coef_top10_list <- vector(mode = "list", length = n_outcome_ds)
coef_top20_list <- vector(mode = "list", length = n_outcome_ds)
coef_top50_list <- vector(mode = "list", length = n_outcome_ds)
coef_top100_list <- vector(mode = "list", length = n_outcome_ds)
coef_full_list <- vector(mode = "list", length = n_outcome_ds)

for (i in 1:n_outcome_ds) {
  target_ds <- outcome_ds_vec[i]
  if (target_ds %in% c("COVID_hspt", "death")) {
    target_ds_sex <- "Both"
  } else {
    target_ds_sex <- phecode_info %>%
      filter(PheCode == str_replace(target_ds, '_', '\\.')) %>%
      pull(Sex)
  }
  
  ### Read trained effect sizes
  if (target_ds == "COVID_hspt") {
    target_ds_summ_list <- readRDS(paste0(temp_data_dir_step3_0, 'Outcome',
                                          target_ds, '_WeightedLogisticRegression_SummStats_ControlCase', 
                                          control_case_ratio,'to1_WhiteBritish_OddYOB_Index2020_Followup5yr.rds'))
    
  } else {
    if (target_ds_sex %in% c("Male", "Female")) {
      target_ds_summ_list <- readRDS(paste0(temp_data_dir_step3_0, 'Outcome',
                                            target_ds, '_Sex_Specific_WeightedLogisticRegression_SummStats_ControlCase', 
                                            control_case_ratio,'to1_WhiteBritish_OddYOB_Index2015_Followup10yr.rds'))
      
    } else {
      target_ds_summ_list <- readRDS(paste0(temp_data_dir_step3_0, 'Outcome',
                                            target_ds, '_WeightedLogisticRegression_SummStats_ControlCase', 
                                            control_case_ratio,'to1_WhiteBritish_OddYOB_Index2015_Followup10yr.rds'))
    }
  }
  
  coef_top10_list[[i]] <- data.frame("Outcome_phecode" = target_ds,
                                     "Predictor_phecode" = rownames(target_ds_summ_list$CRS_top10),
                                     target_ds_summ_list$CRS_top10,
                                     row.names = NULL)
  coef_top20_list[[i]] <- data.frame("Outcome_phecode" = target_ds,
                                     "Predictor_phecode" = rownames(target_ds_summ_list$CRS_top20),
                                     target_ds_summ_list$CRS_top20,
                                     row.names = NULL)
  coef_top50_list[[i]] <- data.frame("Outcome_phecode" = target_ds,
                                     "Predictor_phecode" = rownames(target_ds_summ_list$CRS_top50),
                                     target_ds_summ_list$CRS_top50,
                                     row.names = NULL)
  coef_top100_list[[i]] <- data.frame("Outcome_phecode" = target_ds,
                                     "Predictor_phecode" = rownames(target_ds_summ_list$CRS_top100),
                                     target_ds_summ_list$CRS_top100,
                                     row.names = NULL)
  coef_full_list[[i]] <- data.frame("Outcome_phecode" = target_ds,
                                     "Predictor_phecode" = rownames(target_ds_summ_list$CRS_all_predictors),
                                     target_ds_summ_list$CRS_all_predictors,
                                     row.names = NULL)
  
}

coef_top10_df <- bind_rows(coef_top10_list)
coef_top20_df <- bind_rows(coef_top20_list)
coef_top50_df <- bind_rows(coef_top50_list)
coef_top100_df <- bind_rows(coef_top100_list)
coef_full_df <- bind_rows(coef_full_list) %>%
  rename(Est_full = Est,
         SE_full = SE)

##### Get correlation for overlapping predictors between each model and the full model #####

coef_top10_vs_full_df <- coef_top10_df %>%
  inner_join(coef_full_df, by = c("Outcome_phecode", "Predictor_phecode")) %>%
  filter(!(Predictor_phecode %in% c("(Intercept)", "age_at_2015", "age_at_2020",
                                    "Female")))

coef_top20_vs_full_df <- coef_top20_df %>%
  inner_join(coef_full_df, by = c("Outcome_phecode", "Predictor_phecode")) %>%
  filter(!(Predictor_phecode %in% c("(Intercept)", "age_at_2015", "age_at_2020",
                                    "Female")))

coef_top50_vs_full_df <- coef_top50_df %>%
  inner_join(coef_full_df, by = c("Outcome_phecode", "Predictor_phecode")) %>%
  filter(!(Predictor_phecode %in% c("(Intercept)", "age_at_2015", "age_at_2020",
                                    "Female")))

coef_top100_vs_full_df <- coef_top100_df %>%
  inner_join(coef_full_df, by = c("Outcome_phecode", "Predictor_phecode")) %>%
  filter(!(Predictor_phecode %in% c("(Intercept)", "age_at_2015", "age_at_2020",
                                    "Female")))

plot(coef_top10_vs_full_df$Est, 
     coef_top10_vs_full_df$Est_full,
     xlab = "Effect sizes - Model with 10 predictors",
     ylab = "Effect sizes - Full CRS model",
     xlim = c(-1, 2),
     ylim = c(-1, 2))
abline(0,1)

plot(coef_top20_vs_full_df$Est, 
     coef_top20_vs_full_df$Est_full,
     xlab = "Effect sizes - Model with 20 predictors",
     ylab = "Effect sizes - Full CRS model",
     xlim = c(-1, 2),
     ylim = c(-1, 2))
abline(0,1)

plot(coef_top50_vs_full_df$Est, 
     coef_top50_vs_full_df$Est_full,
     xlab = "Effect sizes - Model with 50 predictors",
     ylab = "Effect sizes - Full CRS model",
     xlim = c(-1, 2),
     ylim = c(-1, 2))
abline(0,1)

plot(coef_top100_vs_full_df$Est, 
     coef_top100_vs_full_df$Est_full,
     xlab = "Effect sizes - Model with 100 predictors",
     ylab = "Effect sizes - Full CRS model",
     xlim = c(-1, 2),
     ylim = c(-1, 2))
abline(0,1)


cor.test(coef_top10_vs_full_df$Est, 
         coef_top10_vs_full_df$Est_full)

cor.test(coef_top20_vs_full_df$Est, 
         coef_top20_vs_full_df$Est_full)

cor.test(coef_top50_vs_full_df$Est, 
         coef_top50_vs_full_df$Est_full)

cor.test(coef_top100_vs_full_df$Est, 
         coef_top100_vs_full_df$Est_full)

##### Save comparison data #####
coef_compare_all <- coef_top100_vs_full_df %>%
  mutate(Comparison = "CRS_top100") %>%
  rbind(coef_top50_vs_full_df %>%
          mutate(Comparison = "CRS_top50")) %>%
  rbind(coef_top20_vs_full_df %>%
          mutate(Comparison = "CRS_top20")) %>%  
  rbind(coef_top10_vs_full_df %>%
          mutate(Comparison = "CRS_top10"))

write.csv(coef_compare_all,
          paste0(temp_data_dir, "All_outcomes_CRS_effect_comparison_by_models_WhiteBritish_OddYOB.csv"),
          row.names = F)




