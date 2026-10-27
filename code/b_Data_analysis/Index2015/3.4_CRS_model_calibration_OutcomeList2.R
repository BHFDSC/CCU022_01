### Author: Pearl Liu
### For each outcome, calibrate CRS model in training data with original disease prevalence
### Odd DOB: training, Even DOB: testing
### Index: 2015-01-01, follow-up: 10 years
### Outcome List 1

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
temp_data_dir_step2_3 <- '~/collab/CCU022/CCU022_CRS/Data/Step2.3/'
temp_data_dir_step2_4 <- '~/collab/CCU022/CCU022_CRS/Data/Step2.4/'
temp_data_dir_step3_0 <- '~/collab/CCU022/CCU022_CRS/Data/Step3.0/'
temp_data_dir <- '~/collab/CCU022/CCU022_CRS/Data/Step3.4/'
file_date <- '2025_05_01'


## Number of cases used in calibration
n_cases_calib0 <- 3000
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
##### List of predictors and outcomes #####
pred_ds_list_name = paste0("ccu022_01_Predictor_Diseases_Pheparent_list_by_PL_", file_date)
outcome_ds_list_name = paste0("ccu022_01_Outcome_Diseases_Phecode_list2_TopCommonPlusPheRS_by_PL_", file_date)

pred_ds_list = readRDS(paste0(temp_data_dir_step1_0, pred_ds_list_name))
outcome_ds_list = readRDS(paste0(temp_data_dir_step1_0, outcome_ds_list_name))

pred_ds_vec <- pred_ds_list %>% arrange(index) %>% pull(predictor_ds) 
outcome_ds_vec <- c(outcome_ds_list %>% arrange(index) %>% pull(target_ds),
                    "death")

n_pred_ds <- length(pred_ds_vec)
n_outcome_ds <- length(outcome_ds_vec)

##### Predictor prevalence #####
pred_ds_prev <- readRDS(paste0(temp_data_dir_step1_1, 
                               'WhiteBritish_OddYOB_Index2015_PredictorDiseasePrevalence.rds'))

##### Outcome incidence #####
outcome_incid_vec <- readRDS(paste0(temp_data_dir_step2_4, 
                                    'TrainData_Outcome_Incidence_OddYOB_Index2015_Followup10yr_OutcomeList2.rds'))

#outcome_incid_vec_sf <- readRDS(paste0(temp_data_dir_step2_4, 
#                                       'TrainData_Outcome_Incidence_Sex_Specific_OddYOB_Index2015_Followup10yr.rds'))

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

# all.equal(pred_phecode_info$PheCode_parent, pred_ds_vec)

#### Loop through each outcome ####

for (i in 1:n_outcome_ds) {
  target_ds <- outcome_ds_vec[i]
  target_ds_incid <- outcome_incid_vec[i]
  
  if (target_ds == "death") {
    target_ds_sex <- "Both"
  } else {
    target_ds_sex <- phecode_info %>%
      filter(PheCode == str_replace(target_ds, '_', '\\.')) %>%
      pull(Sex)
  }
  
  target_file_name_general <-  paste0(temp_data_dir, 'Outcome',
                                      target_ds, '_CalibratedCRS_SummStats_WhiteBritish_OddYOB_Index2015_Followup10yr.rds')
  target_file_name_sex_specific <-  paste0(temp_data_dir, 'Outcome',
                                           target_ds, '_Sex_Specific_CalibratedCRS_SummStats_WhiteBritish_OddYOB_Index2015_Followup10yr.rds')
  
  if ( (target_ds_sex %in% c("Both", "Empty") & 
        file.exists(target_file_name_general)) | 
       (target_ds_sex %in% c("Female", "Male") & 
        file.exists(target_file_name_general) & 
        file.exists(target_file_name_sex_specific)) ) {
    
    print(paste0('Completed Outcome ', i)) 
    
  } else {
    
    #### Read training dataset - full sample ####
    train_df_full <- readRDS(paste0(temp_data_dir_step2_3, 'Outcome',
                                    target_ds, '_TrainData_AllPts_WhiteBritish_OddYOB_Index2015_Followup10yr.rds'))
    
    
    #### Sex-specific outcomes ####
    
    if ( target_ds_sex %in% c("Male", "Female") & 
         file.exists(target_file_name_sex_specific) == FALSE ) {
      
      target_ds_incid_sf <- outcome_incid_vec_sf[i]
      
      if (target_ds_sex == "Female") {
        train_df_full_sf <- train_df_full[Female == 1]
      } else {
        train_df_full_sf <- train_df_full[Female == 0]
      }
      
      ## Subsetting to 3000 cases with original disease prevalence
      n_cases <- sum(train_df_full_sf$target_ds_status == 1)
      n_controls <- sum(train_df_full_sf$target_ds_status == 0)
      case_pts_idx <- which(train_df_full_sf$target_ds_status == 1)
      control_pts_idx <- which(train_df_full_sf$target_ds_status == 0)
      
      n_cases_calib <- min(n_cases_calib0, n_cases)
      control_case_ratio_orig <- (1 - target_ds_incid_sf)/target_ds_incid_sf
      n_controls_calib <- floor(n_cases_calib * control_case_ratio_orig)
      
      set.seed(2026 + i)
      case_subsample_idx <- sample(case_pts_idx, n_cases_calib)
      control_subsample_idx <- sample(control_pts_idx, n_controls_calib)
      
      subsample_idx <- c(case_subsample_idx, control_subsample_idx)
      train_df_fit0 <- train_df_full_sf[subsample_idx, ]
      
      ##### Read original CRS coefficients #####
      target_ds_summ_list <- readRDS(paste0(temp_data_dir_step3_0, 'Outcome',
                                            target_ds, '_Sex_Specific_WeightedLogisticRegression_SummStats_ControlCase', 
                                            control_case_ratio,'to1_WhiteBritish_OddYOB_Index2015_Followup10yr.rds'))
      target_ds_summ <- target_ds_summ_list$CRS_all_predictors
      
      ## Get original CRS score (link scale)
      train_df_fit <- CRS_calc(train_df_fit0,
                               target_ds_summ) 
      
      ##### Calibrate CRS model (including age + sex) #####
      model_calib <- glm(target_ds_status ~ CRS_link_with_covar,
                     weights = ipcw_wts_sf,  # use sex-specific weights
                     data = train_df_fit,
                     family = binomial())

      model_calib_intercept <- coef(model_calib)[1]
      model_calib_slope <- coef(model_calib)[2]
      
      ## update coef to calibrated version
      target_ds_summ_calib <- target_ds_summ
      target_ds_summ_calib[1, "Est"] <- model_calib_intercept + target_ds_summ[1, "Est"] * model_calib_slope
      target_ds_summ_calib[2:nrow(target_ds_summ), "Est"] <- target_ds_summ[2:nrow(target_ds_summ), "Est"] * model_calib_slope
      
      saveRDS(target_ds_summ_calib, target_file_name_sex_specific)
      
    }
    
    #### General outcomes ####
    if ( target_ds_sex %in% c("Both", "Empty") & 
         file.exists(target_file_name_general) == FALSE ) {
      
      ## Subsetting to 3000 cases with original disease prevalence
      n_cases <- sum(train_df_full$target_ds_status == 1)
      n_controls <- sum(train_df_full$target_ds_status == 0)
      case_pts_idx <- which(train_df_full$target_ds_status == 1)
      control_pts_idx <- which(train_df_full$target_ds_status == 0)
      
      n_cases_calib <- min(n_cases_calib0, n_cases)
      control_case_ratio_orig <- (1 - target_ds_incid)/target_ds_incid
      n_controls_calib <- floor(n_cases_calib * control_case_ratio_orig)
      
      set.seed(2026 + i)
      case_subsample_idx <- sample(case_pts_idx, n_cases_calib)
      control_subsample_idx <- sample(control_pts_idx, n_controls_calib)
      
      subsample_idx <- c(case_subsample_idx, control_subsample_idx)
      train_df_fit0 <- train_df_full[subsample_idx, ]
      
      ##### Read original CRS coefficients #####
      target_ds_summ_list <- readRDS(paste0(temp_data_dir_step3_0, 'Outcome',
                                            target_ds, '_WeightedLogisticRegression_SummStats_ControlCase', 
                                            control_case_ratio,'to1_WhiteBritish_OddYOB_Index2015_Followup10yr.rds'))
      target_ds_summ <- target_ds_summ_list$CRS_all_predictors
      
      ## Get original CRS score (link scale)
      train_df_fit <- CRS_calc(train_df_fit0,
                               target_ds_summ) 
      
      ##### Calibrate CRS model (including age + sex) #####
      model_calib <- glm(target_ds_status ~ CRS_link_with_covar,
                         weights = ipcw_wts,
                         data = train_df_fit,
                         family = binomial())
      
      model_calib_intercept <- coef(model_calib)[1]
      model_calib_slope <- coef(model_calib)[2]
      
      ## update coef to calibrated version
      target_ds_summ_calib <- target_ds_summ
      target_ds_summ_calib[1, "Est"] <- model_calib_intercept + target_ds_summ[1, "Est"] * model_calib_slope
      target_ds_summ_calib[2:nrow(target_ds_summ), "Est"] <- target_ds_summ[2:nrow(target_ds_summ), "Est"] * model_calib_slope
      
      saveRDS(target_ds_summ_calib, target_file_name_general)
      
      
    }
    
    print(paste0('Completed Outcome ', i))  
  }
}



