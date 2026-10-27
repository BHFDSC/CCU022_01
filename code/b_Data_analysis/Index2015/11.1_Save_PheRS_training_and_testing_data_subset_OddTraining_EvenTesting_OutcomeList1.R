### Author: Pearl Liu
### For each PheRS outcome, save PheRS training and testing data (subsetted) for analysis
### Also save weighted incidence of outcomes (based on IPCW weights)
### Odd DOB: training, Even DOB: testing
### Index: 2015-01-01, follow-up: 10 years
### 11 outcomes


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
temp_data_dir_step11_0 <- '~/collab/CCU022/CCU022_CRS/Data/Step11.0/'
temp_data_dir <- '~/collab/CCU022/CCU022_CRS/Data/Step11.1/'
file_date <- '2025_05_01'

## Vector of control-case ratio in logistic regression of training data
control_case_ratio_vec <- c(4)

#### Read processed data ####
##### List of predictors and outcomes #####
pred_ds_list_name = paste0("ccu022_01_PheRS_Predictor_Diseases_Pheparent_list_by_PL_", file_date)
pred_ds_list = readRDS(paste0(temp_data_dir_step1_0, pred_ds_list_name))

pred_ds_vec <- pred_ds_list %>% arrange(index) %>% pull(predictor_ds) 

## PheRS outcomes include: 
## colorectal cancer, breast cancer (female only), type ii diabetes, prostate cancer (male only), 
## MI, Coronary atherosclerosis, asthma, Afib and flutter, 
## lung cancer, osteoarthritis (localized), gout
outcome_ds_vec <- c("153", "174_1", "250_2", "185",
                    "411_2", "411_4", "495", "427_2", 
                    "165_1", "740_1", "274_1")

## Whether outcome is in List 1 or List 2
outcome_list_ind <- c(1, 1, 2, 1,
                      1, 2, 2, 2,
                      1, 1, 2)

n_pred_ds <- length(pred_ds_vec)
n_outcome_ds <- length(outcome_ds_vec)


##### Get PheRS outcome index in original outcome lists (see Code 2.3) #####
outcome_ds_list_name1 = paste0("ccu022_01_Outcome_Diseases_Phecode_list_by_PL_", file_date)
outcome_ds_list1 = readRDS(paste0(temp_data_dir_step1_0, outcome_ds_list_name1))
outcome_ds_vec1 <- outcome_ds_list1 %>% arrange(index) %>% pull(target_ds)

outcome_ds_list_name2 = paste0("ccu022_01_Outcome_Diseases_Phecode_list2_TopCommonPlusPheRS_by_PL_", file_date)
outcome_ds_list2 = readRDS(paste0(temp_data_dir_step1_0, outcome_ds_list_name2))
outcome_ds_vec2 <- c(outcome_ds_list2 %>% arrange(index) %>% pull(target_ds),
                    "death")

phers_outcome_ds_idx <- data.frame("PheRS_outcome" = outcome_ds_vec,
                                   "Outcome_list_ind" = outcome_list_ind) %>%
  mutate(Idx = case_when(
    Outcome_list_ind == 1 ~ match(PheRS_outcome, outcome_ds_vec1),
    Outcome_list_ind == 2 ~ match(PheRS_outcome, outcome_ds_vec2))
  ) %>%
  pull(Idx)

##### Phecode information #####
phe_icd10_codelist_name = paste0("ccu022_01_PheCode_ICD10AtoN_map_by_PL_", file_date)
icd10_phecode_map <- readRDS(paste0(temp_data_dir_step1_0, 
                                    phe_icd10_codelist_name))
phecode_info <- icd10_phecode_map %>%
  select(PheCode, Phenotype, Category, Sex) %>%
  distinct()


#### Loop through each outcome ####

outcome_incid_vec <- rep(NA, n_outcome_ds)
outcome_incid_vec_sf <- rep(NA, n_outcome_ds) 

for (i in 1:n_outcome_ds) {
  target_ds <- outcome_ds_vec[i]
  target_ds_info <- phecode_info %>%
    filter(PheCode == str_replace(target_ds, '_', '\\.'))
  
  target_file_name_general <- paste0(temp_data_dir, 'Outcome',
                             target_ds, '_PheRS_TrainData_Fit_ControlCase', control_case_ratio_vec[1],
                             'to1_WhiteBritish_OddYOB_Index2015_Followup10yr.rds')
  target_file_name_sex_specific <- paste0(temp_data_dir, 'Outcome',
                                     target_ds, '_PheRS_TrainData_Fit_Sex_Specific_ControlCase', control_case_ratio_vec[1],
                                     'to1_WhiteBritish_OddYOB_Index2015_Followup10yr.rds')
  
  if ( (target_ds_info$Sex %in% c("Both", "Empty") & 
        file.exists(target_file_name_general)) | 
       (target_ds_info$Sex %in% c("Female", "Male") & 
        file.exists(target_file_name_general) & 
        file.exists(target_file_name_sex_specific)) ) {
    print(paste0('Completed Outcome ', i)) 
  } else {
  
  #### Read full training data ####
  train_df_combine <- readRDS(paste0(temp_data_dir_step11_0, 'Outcome',
                                    target_ds, '_PheRS_TrainData_AllPts_WhiteBritish_OddYOB_Index2015_Followup10yr.rds'))
  
  #### Sex-specific training data: For logistic regression: sub-sample controls ####
  if ( target_ds_info$Sex %in% c("Male", "Female") & 
       file.exists(target_file_name_sex_specific) == FALSE) {
    
    if (target_ds_info$Sex == "Female") {
      train_df_combine_sf <- train_df_combine[Female == 1]
    } else {
      train_df_combine_sf <- train_df_combine[Female == 0]
    }
    
    ##### Save sex-specific weighted outcome incidence #####
    ipcw_wts_sf <- train_df_combine_sf$ipcw_wts_sf
    target_ds_status <- train_df_combine_sf$target_ds_status
    outcome_incid_vec_sf[i] <- sum(target_ds_status * ipcw_wts_sf)/sum(ipcw_wts_sf)
    
    for (j in 1:length(control_case_ratio_vec)) {
      
      control_case_ratio <- control_case_ratio_vec[j]
      
      n_cases <- sum(train_df_combine_sf$target_ds_status == 1)
      n_controls <- sum(train_df_combine_sf$target_ds_status == 0)
      case_pts_idx <- which(train_df_combine_sf$target_ds_status == 1)
      control_pts_idx <- which(train_df_combine_sf$target_ds_status == 0)
      
      n_controls_fit <- min(control_case_ratio * n_cases, n_controls)
      
      set.seed(2025 + phers_outcome_ds_idx[i] + j)
      ## Subsample used to fit the CRS model: case-control = 1:4
      control_subsample_idx <- sample(control_pts_idx, n_controls_fit)
      subsample_fit_idx <- c(case_pts_idx, control_subsample_idx)
      
      ##### Limit to subsampled pts for training data, save subsamples #####
      train_df_fit_subsample <- train_df_combine_sf[subsample_fit_idx, ]
      
      saveRDS(train_df_fit_subsample, paste0(temp_data_dir, 'Outcome',
                                             target_ds, '_PheRS_TrainData_Fit_Sex_Specific_ControlCase', control_case_ratio,
                                             'to1_WhiteBritish_OddYOB_Index2015_Followup10yr.rds'))
      
    }
    
    print(paste0('Completed Outcome ', i, ' (sex-specific)'))
    
  }
  
  #### General training data: For logistic regression: sub-sample controls ####
  if ( file.exists(target_file_name_general) == FALSE ) {
    ##### Save weighted outcome incidence #####
    ipcw_wts <- train_df_combine$ipcw_wts
    target_ds_status <- train_df_combine$target_ds_status
    outcome_incid_vec[i] <- sum(target_ds_status * ipcw_wts)/sum(ipcw_wts)
    
    for (j in 1:length(control_case_ratio_vec)) {
      
      control_case_ratio <- control_case_ratio_vec[j]
      
      n_cases <- sum(train_df_combine$target_ds_status == 1)
      n_controls <- sum(train_df_combine$target_ds_status == 0)
      case_pts_idx <- which(train_df_combine$target_ds_status == 1)
      control_pts_idx <- which(train_df_combine$target_ds_status == 0)
      
      n_controls_fit <- min(control_case_ratio * n_cases, n_controls)
      
      set.seed(2025 + phers_outcome_ds_idx[i] + j)
      ## Subsample used to fit the CRS model: case-control = 1:4
      control_subsample_idx <- sample(control_pts_idx, n_controls_fit)
      subsample_fit_idx <- c(case_pts_idx, control_subsample_idx)
      
      ##### Limit to subsampled pts for training data, save subsamples #####
      train_df_fit_subsample <- train_df_combine[subsample_fit_idx, ]
      
      saveRDS(train_df_fit_subsample, paste0(temp_data_dir, 'Outcome',
                                                  target_ds, '_PheRS_TrainData_Fit_ControlCase', control_case_ratio,
                                                  'to1_WhiteBritish_OddYOB_Index2015_Followup10yr.rds'))
      
    }
    
    print(paste0('Completed Outcome ', i, ' (both sexes)'))
    }
  
  }

}

#### Save outcome incidence ####
## General
outcome_incid_file_name <- paste0(temp_data_dir, 'PheRS_TrainData_Outcome_Incidence_OddYOB_Index2015_Followup10yr.rds')
if (file.exists(outcome_incid_file_name) == FALSE) {
  saveRDS(outcome_incid_vec, outcome_incid_file_name)
}

## Sex-specific
outcome_incid_file_sf_name <- paste0(temp_data_dir, 'PheRS_TrainData_Outcome_Incidence_Sex_Specific_OddYOB_Index2015_Followup10yr.rds')
if (file.exists(outcome_incid_file_sf_name) == FALSE) {
  saveRDS(outcome_incid_vec_sf, outcome_incid_file_sf_name)
}

