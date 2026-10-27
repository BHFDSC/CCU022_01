### Author: Pearl Liu
### For each outcome, save training and testing data (full sample) including CCI and IMD data for analysis
### Odd DOB: training, Even DOB: testing
### Index: 2015-01-01, follow-up: 10 years
### Outcome List 2

rm(list = ls())
gc()

#### Load libraries ####
library(data.table)
library(tidyverse)

source('~/collab/CCU022/CCU022_CRS/0.1_Functions.R')

#### Set parameters ####
temp_data_dir_step1_0 <- '~/collab/CCU022/CCU022_CRS/Data/Step1.0/'
temp_data_dir_step1_1 <- '~/collab/CCU022/CCU022_CRS/Data/Step1.1/'
temp_data_dir_step2_1 <- '~/collab/CCU022/CCU022_CRS/Data/Step2.1/'
temp_data_dir_step2_2 <- '~/collab/CCU022/CCU022_CRS/Data/Step2.2/'
temp_data_dir <- '~/collab/CCU022/CCU022_CRS/Data/Step13.0/'
file_date <- '2025_05_01'

run_testing <- 1  # whether to generate testing data
skip_general_outcome <- 0 # whether to skip general (non-sex-specific) outcome

#### Read processed data ####

##### Predictor status pre-index #####
pred_df_pre_index_even_name <- paste0('ccu022_01_WhiteBritish_EvenYOB_Index2015_PredictorDiseaseStatus_by_PL_', file_date)
pred_df_pre_index_odd_name <- paste0('ccu022_01_WhiteBritish_OddYOB_Index2015_PredictorDiseaseStatus_by_PL_', file_date)

pred_df_pre_index_even <- readRDS(paste0(temp_data_dir_step1_1, pred_df_pre_index_even_name))
pred_df_pre_index_odd <- readRDS(paste0(temp_data_dir_step1_1, pred_df_pre_index_odd_name))

##### Outcome status during follow-up #####
outcome_df_fup_even_name <- paste0('ccu022_01_WhiteBritish_EvenYOB_Index2015_Followup10yr_OutcomeDiseaseStatusFollowup_OutcomeList2_by_PL_', file_date)
outcome_df_fup_odd_name <- paste0('ccu022_01_WhiteBritish_OddYOB_Index2015_Followup10yr_OutcomeDiseaseStatusFollowup_OutcomeList2_by_PL_', file_date)

outcome_df_fup_even <- readRDS(paste0(temp_data_dir_step1_1, outcome_df_fup_even_name))
outcome_df_fup_odd <- readRDS(paste0(temp_data_dir_step1_1, outcome_df_fup_odd_name))

##### Time to outcome during follow-up #####
outcome_df_tte_even_name <- paste0('ccu022_01_WhiteBritish_EvenYOB_Index2015_Followup10yr_TimeToOutcomeFollowup_OutcomeList2_by_PL_', file_date)
outcome_df_tte_odd_name <- paste0('ccu022_01_WhiteBritish_OddYOB_Index2015_Followup10yr_TimeToOutcomeFollowup_OutcomeList2_by_PL_', file_date)

outcome_df_tte_even <- readRDS(paste0(temp_data_dir_step1_1, outcome_df_tte_even_name))
outcome_df_tte_odd <- readRDS(paste0(temp_data_dir_step1_1, outcome_df_tte_odd_name))


##### Covariates data - including CCI and IMD #####
covar_df_even_name <- paste0('ccu022_01_WhiteBritish_EvenYOB_Index2015_Covariates_CCI_IMD_by_PL_', file_date)
covar_df_odd_name <- paste0('ccu022_01_WhiteBritish_OddYOB_Index2015_Covariates_CCI_IMD_by_PL_', file_date)

covar_df_even <- readRDS(paste0(temp_data_dir_step1_1, covar_df_even_name))
covar_df_odd <- readRDS(paste0(temp_data_dir_step1_1, covar_df_odd_name))


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




##### Phecode information #####
phe_icd10_codelist_name = paste0("ccu022_01_PheCode_ICD10AtoN_map_by_PL_", file_date)
icd10_phecode_map <- readRDS(paste0(temp_data_dir_step1_0, 
                                    phe_icd10_codelist_name))
phecode_info <- icd10_phecode_map %>%
  select(PheCode, Phenotype, Category, Sex) %>%
  distinct()


#### Loop through each outcome ####

for (i in c(n_outcome_ds)) {
  
  ##### Read study pop index from Step 2.1 #####
  target_ds <- outcome_ds_vec[i]
  
  if (target_ds == "death") {
    target_ds_sex <- "Both"
  } else {
    target_ds_sex <- phecode_info %>%
      filter(PheCode == str_replace(target_ds, '_', '\\.')) %>%
      pull(Sex)
  }
  
  if (skip_general_outcome == T & target_ds_sex %in% c("Empty", "Both")) {
    next
  }
  
  study_pop_train_name <- paste0("Outcome", target_ds, "_study_population_RmPtsWithPriorOutcomeDiag_WhiteBritish_OddYOB_Index2015.rds")
  study_pop_test_name <- paste0("Outcome", target_ds, "_study_population_RmPtsWithPriorOutcomeDiag_WhiteBritish_EvenYOB_Index2015.rds")
  
  study_pop_train <- readRDS(paste0(temp_data_dir_step2_1, study_pop_train_name))
  study_pop_test <- readRDS(paste0(temp_data_dir_step2_1, study_pop_test_name))
  
  study_pop_idx_train <- study_pop_train$study_pop_idx
  study_pop_idx_test <- study_pop_test$study_pop_idx
  
  rm(study_pop_train, study_pop_test)
  
  ##### Subset training and testing data to correct study population #####
  pred_df_train <- pred_df_pre_index_odd[study_pop_idx_train,]
  covar_df_train <- covar_df_odd[study_pop_idx_train,]
  outcome_df_fup_train <- outcome_df_fup_odd[study_pop_idx_train,]
  outcome_df_tte_train <- outcome_df_tte_odd[study_pop_idx_train,]
  
  pred_df_test <- pred_df_pre_index_even[study_pop_idx_test,]
  covar_df_test <- covar_df_even[study_pop_idx_test,]
  outcome_df_fup_test <- outcome_df_fup_even[study_pop_idx_test,]
  outcome_df_tte_test <- outcome_df_tte_even[study_pop_idx_test,]
  
  #### Training data: Get correct list of predictors ####
  
  ##### Remove parent phecode that contain the outcome #####
  if (target_ds == "death") {
    target_ds_parent <- target_ds
  } else {
    target_ds_parent <- gsub("_.*", "", target_ds)
  }
  
  ## Restrict to correct predictor data for training data
  pred_df_train2 <- pred_df_train %>%
    .[, (target_ds_parent) := NULL] %>%
    .[, PID := NULL]
  
  ## Change colnames
  colnames(pred_df_train2) <- paste0('ds', colnames(pred_df_train2))
  
  ## Add covariates
  pred_df_train2 <- pred_df_train2[, c('age_at_2015', 'Female',
                                       "quan_index", "imd_quintile",
                                       "imd_decile") := covar_df_train[,c('age_at_2015', 'Female',
                                                                          "quan_index", "imd_quintile",
                                                                          "imd_decile")]]
  
  #### Training data: Combine predictors and outcomes, removed censored pts ####
  target_ds_fup_vec <- outcome_df_fup_train[, ..target_ds]
  target_ds_tte_vec <- outcome_df_tte_train[, ..target_ds]
  
  ## Get indices for non-censored pts
  non_censor_idx <- which(target_ds_fup_vec != 9)
  
  target_ds_train <- cbind(target_ds_fup_vec, target_ds_tte_vec,
                           covar_df_train[,"age_at_2015"]) 
  colnames(target_ds_train) <- c('target_ds_status', 'days_to_ds', 'age_at_index')
  
  target_ds_train <- target_ds_train %>%
    .[, yrs_to_ds := floor(days_to_ds/365.25)] %>%
    .[, age_at_index_floor := floor(age_at_index)] %>%
    .[, c("days_to_ds", "age_at_index") := NULL] 
  
  ## combine outcome data with predictor data
  train_df_combine <- pred_df_train2[, colnames(target_ds_train) := target_ds_train]
  
  ## Limit to non-censored pts
  train_df_combine2 <- train_df_combine[non_censor_idx, ]
  
  
  #### Training data: Assign IPCW weights (general and sex-specific) to training pts ####
  if (target_ds == "death") {  # For death outcome, set all weights to 1
    train_df_combine3 <- train_df_combine2 %>%
      .[, ipcw_wts := 1]
  } else {
    target_ds_ipcw_wts <- readRDS(paste0(temp_data_dir_step2_2, 'Outcome',
                                         target_ds, '_IPCW_WhiteBritish_OddYOB_Index2015_Followup10yr.rds'))
    
    train_df_combine3 <- train_df_combine2 %>%
      ## match to IPCW weights based on age at index and year to outcome ds
      merge(target_ds_ipcw_wts, 
            by.x = c("yrs_to_ds", "age_at_index_floor"),
            by.y = c("yr_of_fup", "age_at_index"),
            all.x = T) %>%
      .[, c("target_ds") := NULL]
  }
  
  ## Assign sex-specific weights if target disease is sex-specific
  if (target_ds_sex %in% c("Male", "Female")) {
    target_ds_ipcw_wts_sf <- readRDS(paste0(temp_data_dir_step2_2, 'Outcome',
                                            target_ds, '_Sex_Specific_IPCW_WhiteBritish_OddYOB_Index2015_Followup10yr.rds'))
    setnames(target_ds_ipcw_wts_sf, 
             c("ipcw_wts"), c("ipcw_wts_sf"))
    
    train_df_combine_final <- train_df_combine3 %>%
      ## match to IPCW weights based on age at index and year to outcome ds
      merge(target_ds_ipcw_wts_sf, 
            by.x = c("yrs_to_ds", "age_at_index_floor"),
            by.y = c("yr_of_fup", "age_at_index"),
            all.x = T) %>%
      .[, c("target_ds", "yrs_to_ds", "age_at_index_floor") := NULL]
  } else {
    train_df_combine_final <- train_df_combine3 %>%
      .[, c("yrs_to_ds", "age_at_index_floor") := NULL]
  }
  
  #### Training data: Save analytical dataset - full sample ####
  saveRDS(train_df_combine_final, paste0(temp_data_dir, 'Outcome',
                                         target_ds, '_CCI_IMD_TrainData_AllPts_WhiteBritish_OddYOB_Index2015_Followup10yr.rds'))
  
  if (run_testing == 1) {
    #### Testing data: Get correct list of predictors ####
    
    ##### Remove parent phecode that contain the outcome #####
    ## Restrict to correct predictor data for training data
    pred_df_test2 <- pred_df_test %>%
      .[, (target_ds_parent) := NULL] %>%
      .[, PID := NULL]
    
    ## Change colnames
    colnames(pred_df_test2) <- paste0('ds', colnames(pred_df_test2))
    
    ## Add covariates
    pred_df_test2 <- pred_df_test2[, c('age_at_2015', 'Female',
                                       "quan_index", "imd_quintile",
                                       "imd_decile") := covar_df_test[,c('age_at_2015', 'Female',
                                                                         "quan_index", "imd_quintile",
                                                                         "imd_decile")]]
    
    
    #### Testing data: Combine predictors and outcomes, set censored pts to controls ####
    target_ds_test <- outcome_df_fup_test[, ..target_ds]
    colnames(target_ds_test) <- c('target_ds_status')
    
    ## set censored pts to controls
    target_ds_test <- target_ds_test %>%
      .[, target_ds_status := ifelse(target_ds_status == 9, 0, target_ds_status)]
    
    ## combine outcome data with predictor data
    test_df_combine <- pred_df_test2[, colnames(target_ds_test) := target_ds_test]
    
    ## Add time to outcome event
    target_ds_tte <- outcome_df_tte_test[, ..target_ds]
    colnames(target_ds_tte) <- c('target_ds_tte')
    test_df_combine <- test_df_combine[, colnames(target_ds_tte) := target_ds_tte]
    
    #### Testing data: Save analytical dataset - full sample ####
    saveRDS(test_df_combine, paste0(temp_data_dir, 'Outcome',
                                    target_ds, '_CCI_IMD_TestData_AllPts_WhiteBritish_EvenYOB_Index2015_Followup10yr.rds'))
    
  }
  
  print(paste0('Completed Outcome ', i))
  
}



