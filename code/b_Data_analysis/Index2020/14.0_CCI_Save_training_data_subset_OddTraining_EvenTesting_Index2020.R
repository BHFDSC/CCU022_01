### Author: Pearl Liu
### For each outcome, save training data (subsetted) including CCI for analysis
### Also save weighted incidence of outcomes (based on IPCW weights)
### Odd DOB: training, Even DOB: testing
### Index: 2020-01-01, follow-up: 5 years
### Outcome List 1 - COVID-19 hospitalisation


rm(list = ls())
gc()

#### Load libraries ####
library(data.table)
library(tidyverse)

source('~/collab/CCU022/CCU022_CRS/0.1_Functions.R')

#### Set parameters ####
temp_data_dir_step1_0 <- '~/collab/CCU022/CCU022_CRS/Data/Step1.0/'
temp_data_dir_step1_1 <- '~/collab/CCU022/CCU022_CRS/Data/Step1.1/'
temp_data_dir_step2_3 <- '~/collab/CCU022/CCU022_CRS/Data/Step2.3/'
temp_data_dir_step13_0 <- '~/collab/CCU022/CCU022_CRS/Data/Step13.0/'
temp_data_dir <- '~/collab/CCU022/CCU022_CRS/Data/Step14.0/'
file_date <- '2025_05_01'

## Vector of control-case ratio in logistic regression of training data
control_case_ratio_vec <- c(4)

#### Read processed data ####
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


#### Loop through each outcome ####
## Only COVID outcome for now

#outcome_incid_vec <- rep(NA, n_outcome_ds)
#outcome_incid_vec_sf <- rep(NA, n_outcome_ds) 

#for (i in 1:n_outcome_ds) {
for (i in c(74)) {
  target_ds <- outcome_ds_vec[i]
  
  if (target_ds == "COVID_hspt") {
    target_ds_sex <- "Both"
  } else {
    target_ds_sex <- phecode_info %>%
      filter(PheCode == str_replace(target_ds, '_', '\\.')) %>%
      pull(Sex)
  }
  
  target_file_name_general <- paste0(temp_data_dir, 'Outcome',
                             target_ds, '_CCI_TrainData_Fit_ControlCase', control_case_ratio_vec[1],
                             'to1_WhiteBritish_OddYOB_Index2020_Followup5yr.rds')
  target_file_name_sex_specific <- paste0(temp_data_dir, 'Outcome',
                                     target_ds, '_CCI_TrainData_Fit_Sex_Specific_ControlCase', control_case_ratio_vec[1],
                                     'to1_WhiteBritish_OddYOB_Index2020_Followup5yr.rds')
  
  if ( (target_ds_sex %in% c("Both", "Empty") & 
        file.exists(target_file_name_general)) | 
       (target_ds_sex %in% c("Female", "Male") & 
        file.exists(target_file_name_general) & 
        file.exists(target_file_name_sex_specific)) ) {
    print(paste0('Completed Outcome ', i)) 
  } else {
  
  #### Read full training data ####
  train_df_combine <- readRDS(paste0(temp_data_dir_step13_0, 'Outcome',
                                    target_ds, '_CCI_IMD_TrainData_AllPts_WhiteBritish_OddYOB_Index2020_Followup5yr.rds'))
  
  #### Sex-specific training data: For logistic regression: sub-sample controls ####
  if ( target_ds_sex %in% c("Male", "Female") & 
       file.exists(target_file_name_sex_specific) == FALSE) {
    
    if (target_ds_sex == "Female") {
      train_df_combine_sf <- train_df_combine[Female == 1]
    } else {
      train_df_combine_sf <- train_df_combine[Female == 0]
    }
    
    ##### Save sex-specific weighted outcome incidence #####
 #   ipcw_wts_sf <- train_df_combine_sf$ipcw_wts_sf
 #   target_ds_status <- train_df_combine_sf$target_ds_status
#    outcome_incid_vec_sf[i] <- sum(target_ds_status * ipcw_wts_sf)/sum(ipcw_wts_sf)
    
    for (j in 1:length(control_case_ratio_vec)) {
      
      control_case_ratio <- control_case_ratio_vec[j]
      
      n_cases <- sum(train_df_combine_sf$target_ds_status == 1)
      n_controls <- sum(train_df_combine_sf$target_ds_status == 0)
      case_pts_idx <- which(train_df_combine_sf$target_ds_status == 1)
      control_pts_idx <- which(train_df_combine_sf$target_ds_status == 0)
      
      n_controls_fit <- min(control_case_ratio * n_cases, n_controls)
      
      set.seed(2025 + i + j)
      ## Subsample used to fit the CRS model: case-control = 1:4
      control_subsample_idx <- sample(control_pts_idx, n_controls_fit)
      subsample_fit_idx <- c(case_pts_idx, control_subsample_idx)
      
      ##### Limit to subsampled pts for training data, save subsamples #####
      train_df_fit_subsample <- train_df_combine_sf[subsample_fit_idx, ] %>%
        .[, c("imd_quintile", "imd_decile") := NULL]
      
      saveRDS(train_df_fit_subsample, paste0(temp_data_dir, 'Outcome',
                                             target_ds, '_CCI_TrainData_Fit_Sex_Specific_ControlCase', control_case_ratio,
                                             'to1_WhiteBritish_OddYOB_Index2020_Followup5yr.rds'))
      
    }
    
    print(paste0('Completed Outcome ', i, ' (sex-specific)'))
    
  }
  
  #### General training data: For logistic regression: sub-sample controls ####
  if ( file.exists(target_file_name_general) == FALSE ) {
    ##### Save weighted outcome incidence #####
#    ipcw_wts <- train_df_combine$ipcw_wts
#    target_ds_status <- train_df_combine$target_ds_status
#    outcome_incid_vec[i] <- sum(target_ds_status * ipcw_wts)/sum(ipcw_wts)
    
    for (j in 1:length(control_case_ratio_vec)) {
      
      control_case_ratio <- control_case_ratio_vec[j]
      
      n_cases <- sum(train_df_combine$target_ds_status == 1)
      n_controls <- sum(train_df_combine$target_ds_status == 0)
      case_pts_idx <- which(train_df_combine$target_ds_status == 1)
      control_pts_idx <- which(train_df_combine$target_ds_status == 0)
      
      n_controls_fit <- min(control_case_ratio * n_cases, n_controls)
      
      set.seed(2025 + i + j)
      ## Subsample used to fit the CRS model: case-control = 1:4
      control_subsample_idx <- sample(control_pts_idx, n_controls_fit)
      subsample_fit_idx <- c(case_pts_idx, control_subsample_idx)
      
      ##### Limit to subsampled pts for training data, save subsamples #####
      train_df_fit_subsample <- train_df_combine[subsample_fit_idx, ] %>%
        .[, c("imd_quintile", "imd_decile") := NULL]
      
      saveRDS(train_df_fit_subsample, paste0(temp_data_dir, 'Outcome',
                                                  target_ds, '_CCI_TrainData_Fit_ControlCase', control_case_ratio,
                                                  'to1_WhiteBritish_OddYOB_Index2020_Followup5yr.rds'))
      
    }
    
    print(paste0('Completed Outcome ', i, ' (both sexes)'))
    }
  
  }

}

#### Save outcome incidence ####
## General
# outcome_incid_file_name <- paste0(temp_data_dir, 'TrainData_Outcome_Incidence_OddYOB_Index2020_Followup5yr.rds')
# if (file.exists(outcome_incid_file_name) == FALSE) {
#   saveRDS(outcome_incid_vec, outcome_incid_file_name)
# }
# 
# ## Sex-specific
# outcome_incid_file_sf_name <- paste0(temp_data_dir, 'TrainData_Outcome_Incidence_Sex_Specific_OddYOB_Index2020_Followup5yr.rds')
# if (file.exists(outcome_incid_file_sf_name) == FALSE) {
#   saveRDS(outcome_incid_vec_sf, outcome_incid_file_sf_name)
# }

