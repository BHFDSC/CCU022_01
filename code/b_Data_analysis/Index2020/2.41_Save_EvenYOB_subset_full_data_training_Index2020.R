### Author: Pearl Liu
### For each outcome, save data from even YOB (subsetted) for full data training (N = 13 million)
### Here even YOB data is still referred to as "testing data" for consistency with other codes
### Index: 2020-01-01, follow-up: 5 years
### COVID-19 outcome only


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
temp_data_dir_step2_3 <- '~/collab/CCU022/CCU022_CRS/Data/Step2.3/'
temp_data_dir <- '~/collab/CCU022/CCU022_CRS/Data/Step2.4/'
file_date <- '2025_05_01'

## Vector of control-case ratio in logistic regression of testing data
control_case_ratio_vec <- c(4)

#### Read processed data ####

##### Predictor status pre-index #####
pred_df_pre_index_even_name <- paste0('ccu022_01_WhiteBritish_EvenYOB_Index2020_PredictorDiseaseStatus_by_PL_', file_date)
#pred_df_pre_index_odd_name <- paste0('ccu022_01_WhiteBritish_OddYOB_Index2020_PredictorDiseaseStatus_by_PL_', file_date)

pred_df_pre_index_even <- readRDS(paste0(temp_data_dir_step1_1, pred_df_pre_index_even_name))
#pred_df_pre_index_odd <- readRDS(paste0(temp_data_dir_step1_1, pred_df_pre_index_odd_name))

##### Outcome status during follow-up #####
outcome_df_fup_even_name <- paste0('ccu022_01_WhiteBritish_EvenYOB_Index2020_Followup5yr_OutcomeDiseaseStatusFollowup_by_PL_', file_date)
#outcome_df_fup_odd_name <- paste0('ccu022_01_WhiteBritish_OddYOB_Index2020_Followup5yr_OutcomeDiseaseStatusFollowup_by_PL_', file_date)

outcome_df_fup_even <- readRDS(paste0(temp_data_dir_step1_1, outcome_df_fup_even_name))
#outcome_df_fup_odd <- readRDS(paste0(temp_data_dir_step1_1, outcome_df_fup_odd_name))

##### Time to outcome during follow-up #####
outcome_df_tte_even_name <- paste0('ccu022_01_WhiteBritish_EvenYOB_Index2020_Followup5yr_TimeToOutcomeFollowup_by_PL_', file_date)
#outcome_df_tte_odd_name <- paste0('ccu022_01_WhiteBritish_OddYOB_Index2020_Followup5yr_TimeToOutcomeFollowup_by_PL_', file_date)

outcome_df_tte_even <- readRDS(paste0(temp_data_dir_step1_1, outcome_df_tte_even_name))
#outcome_df_tte_odd <- readRDS(paste0(temp_data_dir_step1_1, outcome_df_tte_odd_name))


##### Covariates data #####
covar_df_even_name <- paste0('ccu022_01_WhiteBritish_EvenYOB_Index2020_Covariates_by_PL_', file_date)
#covar_df_odd_name <- paste0('ccu022_01_WhiteBritish_OddYOB_Index2020_Covariates_by_PL_', file_date)

covar_df_even <- readRDS(paste0(temp_data_dir_step1_1, covar_df_even_name))
#covar_df_odd <- readRDS(paste0(temp_data_dir_step1_1, covar_df_odd_name))




##### List of predictors and outcomes #####
pred_ds_list_name = paste0("ccu022_01_Predictor_Diseases_Pheparent_list_by_PL_", file_date)
outcome_ds_list_name = paste0("ccu022_01_Outcome_Diseases_Phecode_list_by_PL_", file_date)

pred_ds_list = readRDS(paste0(temp_data_dir_step1_0, pred_ds_list_name))
outcome_ds_list = readRDS(paste0(temp_data_dir_step1_0, outcome_ds_list_name))

pred_ds_vec <- pred_ds_list %>% arrange(index) %>% pull(predictor_ds) 
outcome_ds_vec <- c(outcome_ds_list %>% arrange(index) %>% pull(target_ds),
                    "COVID_hspt")

n_pred_ds <- length(pred_ds_vec)
n_outcome_ds <- length(outcome_ds_vec)

##### Phecode information #####
phe_icd10_codelist_name = paste0("ccu022_01_PheCode_ICD10AtoN_map_by_PL_", file_date)
icd10_phecode_map <- readRDS(paste0(temp_data_dir_step1_0, 
                                    phe_icd10_codelist_name))
phecode_info <- icd10_phecode_map %>%
  select(PheCode, Phenotype, Category, Sex) %>%
  distinct()


#### Loop through each outcome - focus on COVID-19 outcome here ####

#outcome_incid_vec <- rep(NA, n_outcome_ds)
#outcome_incid_vec_sf <- rep(NA, n_outcome_ds) 

## Only consider COVID-19 outcome
#for (i in 1:n_outcome_ds) {
for (i in c(74)) {
  ##### Read study pop index from Step 2.1 #####
  target_ds <- outcome_ds_vec[i]
  
  if (target_ds == "COVID_hspt") {
    target_ds_sex <- "Both"
  } else {
    target_ds_sex <- phecode_info %>%
      filter(PheCode == str_replace(target_ds, '_', '\\.')) %>%
      pull(Sex)
  }
  
  #study_pop_train_name <- paste0("Outcome", target_ds, "_study_population_RmPtsWithPriorOutcomeDiag_WhiteBritish_OddYOB_Index2020.rds")
  study_pop_test_name <- paste0("Outcome", target_ds, "_study_population_RmPtsWithPriorOutcomeDiag_WhiteBritish_EvenYOB_Index2020.rds")
  
  #study_pop_train <- readRDS(paste0(temp_data_dir_step2_1, study_pop_train_name))
  study_pop_test <- readRDS(paste0(temp_data_dir_step2_1, study_pop_test_name))
  
  #study_pop_idx_train <- study_pop_train$study_pop_idx
  study_pop_idx_test <- study_pop_test$study_pop_idx
  
  rm(study_pop_test)
  
  
  ##### Subset testing data to correct study population #####
#  pred_df_train <- pred_df_pre_index_odd[study_pop_idx_train,]
#  covar_df_train <- covar_df_odd[study_pop_idx_train,]
#  outcome_df_fup_train <- outcome_df_fup_odd[study_pop_idx_train,]
#  outcome_df_tte_train <- outcome_df_tte_odd[study_pop_idx_train,]
  
  pred_df_test <- pred_df_pre_index_even[study_pop_idx_test,]
  covar_df_test <- covar_df_even[study_pop_idx_test,]
  outcome_df_fup_test <- outcome_df_fup_even[study_pop_idx_test,]
  outcome_df_tte_test <- outcome_df_tte_even[study_pop_idx_test,]
  
  #### Get correct list of predictors ####
  
  ##### Remove parent phecode that contain the outcome #####
  if (target_ds == "COVID_hspt") {
    target_ds_parent <- target_ds
  } else {
    target_ds_parent <- gsub("_.*", "", target_ds)
  }
  
  ## Restrict to correct predictor data for testing data
  pred_df_test2 <- pred_df_test %>%
    .[, (target_ds_parent) := NULL] %>%
    .[, PID := NULL]
  
  ## Change colnames
  colnames(pred_df_test2) <- paste0('ds', colnames(pred_df_test2))
  
  ## Add covariates
  pred_df_test2 <- pred_df_test2[, c('age_at_2020', 'Female') := covar_df_test[,c('age_at_2020', 'Female')]]
  
  #### Combine predictors and outcomes, removed censored pts ####
  target_ds_fup_vec <- outcome_df_fup_test[, ..target_ds]
  target_ds_tte_vec <- outcome_df_tte_test[, ..target_ds]
  
  ## Get indices for non-censored pts
  non_censor_idx <- which(target_ds_fup_vec != 9)
  
  target_ds_test <- cbind(target_ds_fup_vec, target_ds_tte_vec,
                           covar_df_test[,"age_at_2020"]) 
  colnames(target_ds_test) <- c('target_ds_status', 'days_to_ds', 'age_at_index')
  
  target_ds_test <- target_ds_test %>%
    .[, yrs_to_ds := floor(days_to_ds/365.25)] %>%
    .[, age_at_index_floor := floor(age_at_index)] %>%
    .[, c("days_to_ds", "age_at_index") := NULL] 
  
  ## combine outcome data with predictor data
  test_df_combine <- pred_df_test2[, colnames(target_ds_test) := target_ds_test]
  
  ## Limit to non-censored pts
  test_df_combine2 <- test_df_combine[non_censor_idx, ]
  
  
  #### Assign IPCW weights (general and sex-specific) to testing pts ####
  target_ds_ipcw_wts <- readRDS(paste0(temp_data_dir_step2_2, 'Outcome',
                                       target_ds, '_IPCW_WhiteBritish_OddYOB_Index2020_Followup5yr.rds'))
  
  test_df_combine3 <- test_df_combine2 %>%
    ## match to IPCW weights based on age at index and year to outcome ds
    merge(target_ds_ipcw_wts, 
          by.x = c("yrs_to_ds", "age_at_index_floor"),
          by.y = c("yr_of_fup", "age_at_index"),
          all.x = T) %>%
    .[, c("target_ds") := NULL]
  
  ## Assign sex-specific weights if target disease is sex-specific
  if (target_ds_sex %in% c("Male", "Female")) {
    target_ds_ipcw_wts_sf <- readRDS(paste0(temp_data_dir_step2_2, 'Outcome',
                                            target_ds, '_Sex_Specific_IPCW_WhiteBritish_OddYOB_Index2020_Followup5yr.rds'))
    setnames(target_ds_ipcw_wts_sf, 
             c("ipcw_wts"), c("ipcw_wts_sf"))
    
    test_df_combine_final <- test_df_combine3 %>%
      ## match to IPCW weights based on age at index and year to outcome ds
      merge(target_ds_ipcw_wts_sf, 
            by.x = c("yrs_to_ds", "age_at_index_floor"),
            by.y = c("yr_of_fup", "age_at_index"),
            all.x = T) %>%
      .[, c("target_ds", "yrs_to_ds", "age_at_index_floor") := NULL]
  } else {
    test_df_combine_final <- test_df_combine3 %>%
      .[, c("yrs_to_ds", "age_at_index_floor") := NULL]
  }
  
  #### Save analytical dataset - full sample ####
  saveRDS(test_df_combine_final, paste0(temp_data_dir_step2_3, 'Outcome',
                                         target_ds, '_TestDataForTraining_AllPts_WhiteBritish_EvenYOB_Index2020_Followup5yr.rds'))
  
  
  #### Subset full testing data for training ####
  target_file_name_general <- paste0(temp_data_dir, 'Outcome',
                             target_ds, '_TestData_Fit_ControlCase', control_case_ratio_vec[1],
                             'to1_WhiteBritish_EvenYOB_Index2020_Followup5yr.rds')
  target_file_name_sex_specific <- paste0(temp_data_dir, 'Outcome',
                                     target_ds, '_TestData_Fit_Sex_Specific_ControlCase', control_case_ratio_vec[1],
                                     'to1_WhiteBritish_EvenYOB_Index2020_Followup5yr.rds')
  
  if ( (target_ds_sex %in% c("Both", "Empty") & 
        file.exists(target_file_name_general)) | 
       (target_ds_sex %in% c("Female", "Male") & 
        file.exists(target_file_name_general) & 
        file.exists(target_file_name_sex_specific)) ) {
    print(paste0('Completed Outcome ', i)) 
  } else {
  
  #### Read full testing data ####
  test_df_combine <- test_df_combine_final
  rm(test_df_combine_final)
    
  #### Sex-specific testing data: For logistic regression: sub-sample controls ####
  if ( target_ds_sex %in% c("Male", "Female") & 
       file.exists(target_file_name_sex_specific) == FALSE) {
    
    if (target_ds_sex == "Female") {
      test_df_combine_sf <- test_df_combine[Female == 1]
    } else {
      test_df_combine_sf <- test_df_combine[Female == 0]
    }
    
    ##### Save sex-specific weighted outcome incidence 
#    ipcw_wts_sf <- test_df_combine_sf$ipcw_wts_sf
#    target_ds_status <- test_df_combine_sf$target_ds_status
#    outcome_incid_vec_sf[i] <- sum(target_ds_status * ipcw_wts_sf)/sum(ipcw_wts_sf)
    
    for (j in 1:length(control_case_ratio_vec)) {
      
      control_case_ratio <- control_case_ratio_vec[j]
      
      n_cases <- sum(test_df_combine_sf$target_ds_status == 1)
      n_controls <- sum(test_df_combine_sf$target_ds_status == 0)
      case_pts_idx <- which(test_df_combine_sf$target_ds_status == 1)
      control_pts_idx <- which(test_df_combine_sf$target_ds_status == 0)
      
      n_controls_fit <- min(control_case_ratio * n_cases, n_controls)
      
      set.seed(2025 + i + j)
      ## Subsample used to fit the CRS model: case-control = 1:4
      control_subsample_idx <- sample(control_pts_idx, n_controls_fit)
      subsample_fit_idx <- c(case_pts_idx, control_subsample_idx)
      
      ##### Limit to subsampled pts for testing data, save subsamples #####
      test_df_fit_subsample <- test_df_combine_sf[subsample_fit_idx, ]
      
      saveRDS(test_df_fit_subsample, paste0(temp_data_dir, 'Outcome',
                                             target_ds, '_TestData_Fit_Sex_Specific_ControlCase', control_case_ratio,
                                             'to1_WhiteBritish_EvenYOB_Index2020_Followup5yr.rds'))
      
    }
    
    print(paste0('Completed Outcome ', i, ' (sex-specific)'))
    
  }
  
  #### General testing data: For logistic regression: sub-sample controls ####
  if ( file.exists(target_file_name_general) == FALSE ) {
    ##### Save weighted outcome incidence 
#    ipcw_wts <- test_df_combine$ipcw_wts
#    target_ds_status <- test_df_combine$target_ds_status
#    outcome_incid_vec[i] <- sum(target_ds_status * ipcw_wts)/sum(ipcw_wts)
    
    for (j in 1:length(control_case_ratio_vec)) {
      
      control_case_ratio <- control_case_ratio_vec[j]
      
      n_cases <- sum(test_df_combine$target_ds_status == 1)
      n_controls <- sum(test_df_combine$target_ds_status == 0)
      case_pts_idx <- which(test_df_combine$target_ds_status == 1)
      control_pts_idx <- which(test_df_combine$target_ds_status == 0)
      
      n_controls_fit <- min(control_case_ratio * n_cases, n_controls)
      
      set.seed(2025 + i + j)
      ## Subsample used to fit the CRS model: case-control = 1:4
      control_subsample_idx <- sample(control_pts_idx, n_controls_fit)
      subsample_fit_idx <- c(case_pts_idx, control_subsample_idx)
      
      ##### Limit to subsampled pts for testing data, save subsamples #####
      test_df_fit_subsample <- test_df_combine[subsample_fit_idx, ]
      
      saveRDS(test_df_fit_subsample, paste0(temp_data_dir, 'Outcome',
                                                  target_ds, '_TestData_Fit_ControlCase', control_case_ratio,
                                                  'to1_WhiteBritish_EvenYOB_Index2020_Followup5yr.rds'))
      
    }
    
    print(paste0('Completed Outcome ', i, ' (both sexes)'))
    }
  
  }

}

