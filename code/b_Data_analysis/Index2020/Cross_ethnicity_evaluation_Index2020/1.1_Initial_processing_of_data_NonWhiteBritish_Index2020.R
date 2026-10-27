### Author: Pearl Liu
### Initial processing of CVD-COVID-UK data - non-White British ethnicity groups

rm(list = ls())
gc()

#### Load libraries ####
library(tidyverse)
library(data.table)

#### Set parameters ####
temp_data_dir_step1_0 <- '~/collab/CCU022/CCU022_CRS/Data/Step1.0/'
temp_data_dir <- '~/collab/CCU022/CCU022_CRS/Data/Step1.1/'
file_date <- '2025_05_01'

eth_name_vec <- c("OtherWhite", "Asian", "Black", "Mixed")
covar_colnames <- c('PID', 'age_at_2020', 'Female', 'date_of_birth')

#### Read list of predictors and outcomes ####
pred_ds_list_name = paste0("ccu022_01_Predictor_Diseases_Pheparent_list_by_PL_", file_date)
outcome_ds_list_name = paste0("ccu022_01_Outcome_Diseases_Phecode_list_by_PL_", file_date)

pred_ds_list = readRDS(paste0(temp_data_dir_step1_0, pred_ds_list_name))
outcome_ds_list = readRDS(paste0(temp_data_dir_step1_0, outcome_ds_list_name))

pred_ds_vec <- pred_ds_list %>% arrange(index) %>% pull(predictor_ds) 
outcome_ds_vec <- c(outcome_ds_list %>% arrange(index) %>% pull(target_ds),
                    "COVID_hspt")

n_pred_ds <- length(pred_ds_vec)
n_outcome_ds <- length(outcome_ds_vec)

#### Loop through each ethnicity group ####

#for (i in 1:length(eth_name_vec)) {
for (i in c(1)) {
  
  eth_name <- eth_name_vec[i]

  #### Read saved data from Databricks - Index: 2020-01-01, Follow-up: 5 years ####
  ##### Predictor data #####
  pred_df_name = paste0("ccu022_01_", eth_name, "_Age40_69_Index2020_PredictorData_by_PL_", file_date)
  pred_df = readRDS(paste0(temp_data_dir_step1_0, pred_df_name))

  ##### Outcome data #####
  outcome_df_name = paste0("ccu022_01_", eth_name, "_Age40_69_Index2020_AllOutcomeData_by_PL_", file_date)
  outcome_df = readRDS(paste0(temp_data_dir_step1_0, outcome_df_name))
  
  #### Processing of data - Index: 2020-01-01, Follow-up: 5 years ####
  
  ##### Order by PID #####
  pred_df_ordered <- as.data.table(pred_df) %>%
    .[order(PID)]
  outcome_df_ordered <- as.data.table(outcome_df) %>%
    .[order(PID)]
  
  ## sanity check
  all.equal(pred_df_ordered$PID, outcome_df_ordered$PID)
        
  ##### Get matrix of pre-index predictor status #####
  pred_df_pre_index_colnames <- c('PID', pred_ds_vec)
  pred_df_pre_index <- pred_df_ordered[, ..pred_df_pre_index_colnames] 

  pred_df_pre_index_name <- paste0('ccu022_01_', eth_name,'_Index2020_PredictorDiseaseStatus_by_PL_', file_date)
  saveRDS(pred_df_pre_index, paste0(temp_data_dir, pred_df_pre_index_name))

  ##### Get matrix of covariates #####
  covar_df <- pred_df_ordered[, ..covar_colnames] 
  covar_df_name <- paste0('ccu022_01_', eth_name,'_Index2020_Covariates_by_PL_', file_date)
  saveRDS(covar_df, paste0(temp_data_dir, covar_df_name))
  
  ##### Get matrix of pre-index outcome status #####
  outcome_df_pre_index_colnames <- c('PID',
                                     paste0(outcome_ds_vec, '_preidx'))
  outcome_df_pre_index <- outcome_df_ordered[, ..outcome_df_pre_index_colnames] 
  colnames(outcome_df_pre_index) <- c('PID', outcome_ds_vec)

  outcome_df_pre_index_name <- paste0('ccu022_01_', eth_name,'_Index2020_OutcomeDiseaseStatusPreIndex_by_PL_', file_date)
  saveRDS(outcome_df_pre_index, paste0(temp_data_dir, outcome_df_pre_index_name))
  
  ##### Get matrix of outcome status during follow-up #####
  outcome_df_fup_colnames <- c('PID',
                                     paste0(outcome_ds_vec, '_fup'))
  outcome_df_fup <- outcome_df_ordered[, ..outcome_df_fup_colnames] 
  colnames(outcome_df_fup) <- c('PID', outcome_ds_vec)

  outcome_df_fup_name <- paste0('ccu022_01_', eth_name, '_Index2020_Followup5yr_OutcomeDiseaseStatusFollowup_by_PL_', file_date)
  saveRDS(outcome_df_fup, paste0(temp_data_dir, outcome_df_fup_name))

  ##### Get matrix of time to outcome during follow-up #####
  outcome_df_tte_colnames <- c('PID',
                               paste0(outcome_ds_vec, '_tte'))
  outcome_df_tte <- outcome_df_ordered[, ..outcome_df_tte_colnames] 
  colnames(outcome_df_tte) <- c('PID', outcome_ds_vec)
  
  outcome_df_tte_name <- paste0('ccu022_01_', eth_name, '_Index2020_Followup5yr_TimeToOutcomeFollowup_by_PL_', file_date)
  saveRDS(outcome_df_tte, paste0(temp_data_dir, outcome_df_tte_name))

  ##### Sanity check #####
  all(sapply(list(covar_df$PID, 
                  outcome_df_pre_index$PID, 
                  outcome_df_fup$PID, 
                  outcome_df_tte$PID), FUN = identical, 
             pred_df_pre_index$PID)) %>% print()
  
  print(length(pred_df_pre_index$PID))

}

