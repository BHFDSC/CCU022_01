### Author: Pearl Liu
### For each outcome, save training and testing data (subsetted) for analysis
### Also save weighted incidence of outcomes (based on IPCW weights)
### Odd DOB: training, Even DOB: testing
### Index: 2020-01-01, follow-up: 5 years


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
temp_data_dir <- '~/collab/CCU022/CCU022_CRS/Data/Step2.4/'
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



#### Loop through each outcome - focus on COVID-19 outcome here ####

outcome_incid_vec <- rep(NA, n_outcome_ds)

## Focus on COVID-19 outcome (last outcome)
#for (i in 1:n_outcome_ds) {
for (i in c(74)) {
  target_ds <- outcome_ds_vec[i]
  
  #### Read full training data ####
  train_df_combine <- readRDS(paste0(temp_data_dir_step2_3, 'Outcome',
                                    target_ds, '_TrainData_AllPts_WhiteBritish_OddYOB_Index2020_Followup5yr.rds'))
  
  #### Save weighted outcome incidence ####
  ipcw_wts <- train_df_combine$ipcw_wts
  target_ds_status <- train_df_combine$target_ds_status
  outcome_incid_vec[i] <- sum(target_ds_status * ipcw_wts)/sum(ipcw_wts)
  
  #### Training data: For logistic regression: sub-sample controls ####
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
    train_df_fit_subsample <- train_df_combine[subsample_fit_idx, ]
    
    saveRDS(train_df_fit_subsample, paste0(temp_data_dir, 'Outcome',
                                                target_ds, '_TrainData_Fit_ControlCase', control_case_ratio,
                                                'to1_WhiteBritish_OddYOB_Index2020_Followup5yr.rds'))
    
  }
  
  print(paste0('Completed Outcome ', i))

}

saveRDS(outcome_incid_vec, paste0(temp_data_dir, 'TrainData_Outcome_Incidence_OddYOB_Index2020_Followup5yr.rds'))
