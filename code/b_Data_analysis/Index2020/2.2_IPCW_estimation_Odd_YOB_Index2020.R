### Author: Pearl Liu
### For each outcome, estimate the weights from IPCW using training data
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
temp_data_dir_step2_1 <- '~/collab/CCU022/CCU022_CRS/Data/Step2.1/'
temp_data_dir <- '~/collab/CCU022/CCU022_CRS/Data/Step2.2/'
file_date <- '2025_05_01'

#### Read processed data ####

##### Outcome status during follow-up #####
outcome_df_fup_odd_name <- paste0('ccu022_01_WhiteBritish_OddYOB_Index2020_Followup5yr_OutcomeDiseaseStatusFollowup_by_PL_', file_date)
outcome_df_fup_odd <- readRDS(paste0(temp_data_dir_step1_1, outcome_df_fup_odd_name))

##### Time to outcome during follow-up #####
outcome_df_tte_odd_name <- paste0('ccu022_01_WhiteBritish_OddYOB_Index2020_Followup5yr_TimeToOutcomeFollowup_by_PL_', file_date)
outcome_df_tte_odd <- readRDS(paste0(temp_data_dir_step1_1, outcome_df_tte_odd_name))

##### Covariates data #####
covar_df_odd_name <- paste0('ccu022_01_WhiteBritish_OddYOB_Index2020_Covariates_by_PL_', file_date)
covar_df_odd <- readRDS(paste0(temp_data_dir_step1_1, covar_df_odd_name))


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


#### Training data: Estimate weights for IPCW for each outcome - focus on COVID-19 outcome here (last outcome) ####
for (i in 1:n_outcome_ds) {
  
  ##### Read study pop index from Step 2.1 #####
  target_ds <- outcome_ds_vec[i]
  study_pop_train_name <- paste0("Outcome", target_ds, "_study_population_RmPtsWithPriorOutcomeDiag_WhiteBritish_OddYOB_Index2020.rds")
  study_pop_train <- readRDS(paste0(temp_data_dir_step2_1, study_pop_train_name))
  study_pop_idx_train <- study_pop_train$study_pop_idx
  
  ##### Subset the outcome data to correct study population #####
  covar_df_train <- covar_df_odd[study_pop_idx_train,]
  outcome_df_fup_train <- outcome_df_fup_odd[study_pop_idx_train,]
  outcome_df_tte_train <- outcome_df_tte_odd[study_pop_idx_train, ]
  
  ##### Prepare dataset for IPCW weight estimation #####
  target_ds_fup_vec <- outcome_df_fup_train[, ..target_ds]
  target_ds_tte_vec <- outcome_df_tte_train[, ..target_ds]
  
  target_ds_surv_dt <- cbind(target_ds_fup_vec, target_ds_tte_vec,
                             covar_df_train[,"age_at_2020"])
  colnames(target_ds_surv_dt) <- c('ds_status', 'days_to_ds', 'age_at_index')
  target_ds_surv_dt <- target_ds_surv_dt %>%
    .[, ds_status := ifelse(ds_status == 9, 0, ds_status)]
  
  ##### Estimate IPCW weights for all ages within 40-69 #####
  
  target_ds_ipcw_wts <- ipcw_wts(target_ds_surv_dt, 
                                   start_age = 40, 
                                   end_age = 69, 
                                   fup_yrs = 5,
                                   target_ds)
  
  
  print(paste0("Outcome ", i, ": Range of weights: ",
               range(target_ds_ipcw_wts$ipcw_wts)[1], ", ",
               round(range(target_ds_ipcw_wts$ipcw_wts)[2], 3)))
  
  
  saveRDS(target_ds_ipcw_wts, paste0(temp_data_dir, 'Outcome',
                                     target_ds, '_IPCW_WhiteBritish_OddYOB_Index2020_Followup5yr.rds'))

}

