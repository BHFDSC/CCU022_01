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
covar_colnames <- c('PID', 'age_at_2015', 'Female', 'date_of_birth')

#### Read list of predictors and outcomes (Outcome List 1 and 2) ####
pred_ds_list_name = paste0("ccu022_01_Predictor_Diseases_Pheparent_list_by_PL_", file_date)
outcome_ds_list_name = paste0("ccu022_01_Outcome_Diseases_Phecode_list_by_PL_", file_date)
outcome_ds_list2_name = paste0("ccu022_01_Outcome_Diseases_Phecode_list2_TopCommonPlusPheRS_by_PL_", file_date)

pred_ds_list = readRDS(paste0(temp_data_dir_step1_0, pred_ds_list_name))
outcome_ds_list = readRDS(paste0(temp_data_dir_step1_0, outcome_ds_list_name))
outcome_ds_list2 = readRDS(paste0(temp_data_dir_step1_0, outcome_ds_list2_name))

pred_ds_vec <- pred_ds_list %>% arrange(index) %>% pull(predictor_ds) 
outcome_ds_vec <- outcome_ds_list %>% arrange(index) %>% pull(target_ds)
outcome_ds_vec2 <- c(outcome_ds_list2 %>% arrange(index) %>% pull(target_ds),
                     "death")

n_pred_ds <- length(pred_ds_vec)
n_outcome_ds <- length(outcome_ds_vec)
n_outcome_ds2 <- length(outcome_ds_vec2)

#### Loop through each ethnicity group ####

process_predictor <- F
process_outcomelist1 <- F
process_outcomelist2 <- T


for (i in 1:length(eth_name_vec)) {
  
  eth_name <- eth_name_vec[i]
  
  if (eth_name == "OtherWhite") {
    process_predictor <- T
    process_outcomelist1 <- T
  } else {
    process_predictor <- F
    process_outcomelist1 <- F
  }

  #### Read saved data from Databricks - Index: 2015-01-01, Follow-up: 10 years ####
  ##### Predictor data #####
  if (process_predictor == T) {
    pred_df_name = paste0("ccu022_01_", eth_name, "_Age40_69_Index2015_PredictorData_by_PL_", file_date)
    pred_df = readRDS(paste0(temp_data_dir_step1_0, pred_df_name))
  }

  ##### Outcome data - Outcome List 1 #####
  if (process_outcomelist1 == T) {
    outcome_df_name = paste0("ccu022_01_", eth_name, "_Age40_69_Index2015_AllOutcomeData_by_PL_", file_date)
    outcome_df = readRDS(paste0(temp_data_dir_step1_0, outcome_df_name))
  }
  
  ##### Outcome data: Outcome List 2 #####
  if (process_outcomelist2 == T) {
    outcome_df2_name = paste0("ccu022_01_", eth_name, "_Age40_69_Index2015_AllOutcomeData_OutcomeList2_by_PL_", file_date)
    outcome_df2 = readRDS(paste0(temp_data_dir_step1_0, outcome_df2_name))
  }
  
  #### Processing of data - Index: 2015-01-01, Follow-up: 10 years ####
  
  if (process_predictor == T) {
    ##### Order predictor data by PID #####
    pred_df_ordered <- as.data.table(pred_df) %>%
      .[order(PID)]
          
    ##### Get matrix of pre-index predictor status #####
    pred_df_pre_index_colnames <- c('PID', pred_ds_vec)
    pred_df_pre_index <- pred_df_ordered[, ..pred_df_pre_index_colnames] 
  
    pred_df_pre_index_name <- paste0('ccu022_01_', eth_name,'_Index2015_PredictorDiseaseStatus_by_PL_', file_date)
    saveRDS(pred_df_pre_index, paste0(temp_data_dir, pred_df_pre_index_name))
  
    ##### Get matrix of covariates #####
    covar_df <- pred_df_ordered[, ..covar_colnames] 
    covar_df_name <- paste0('ccu022_01_', eth_name,'_Index2015_Covariates_by_PL_', file_date)
    saveRDS(covar_df, paste0(temp_data_dir, covar_df_name))
  
  }
  
  
  if (process_outcomelist1 == T) {
  ##### Order outcome data (list 1) by PID #####
  outcome_df_ordered <- as.data.table(outcome_df) %>%
    .[order(PID)]
  
  ## sanity check
  # all.equal(pred_df_ordered$PID, outcome_df_ordered$PID)
  
  ##### Get matrix of pre-index outcome status #####
  outcome_df_pre_index_colnames <- c('PID',
                                     paste0(outcome_ds_vec, '_preidx'))
  outcome_df_pre_index <- outcome_df_ordered[, ..outcome_df_pre_index_colnames] 
  colnames(outcome_df_pre_index) <- c('PID', outcome_ds_vec)

  outcome_df_pre_index_name <- paste0('ccu022_01_', eth_name,'_Index2015_OutcomeDiseaseStatusPreIndex_by_PL_', file_date)
  saveRDS(outcome_df_pre_index, paste0(temp_data_dir, outcome_df_pre_index_name))
  
  ##### Get matrix of outcome status during follow-up #####
  outcome_df_fup_colnames <- c('PID',
                                     paste0(outcome_ds_vec, '_fup'))
  outcome_df_fup <- outcome_df_ordered[, ..outcome_df_fup_colnames] 
  colnames(outcome_df_fup) <- c('PID', outcome_ds_vec)

  outcome_df_fup_name <- paste0('ccu022_01_', eth_name, '_Index2015_Followup10yr_OutcomeDiseaseStatusFollowup_by_PL_', file_date)
  saveRDS(outcome_df_fup, paste0(temp_data_dir, outcome_df_fup_name))

  ##### Get matrix of time to outcome during follow-up #####
  outcome_df_tte_colnames <- c('PID',
                               paste0(outcome_ds_vec, '_tte'))
  outcome_df_tte <- outcome_df_ordered[, ..outcome_df_tte_colnames] 
  colnames(outcome_df_tte) <- c('PID', outcome_ds_vec)
  
  outcome_df_tte_name <- paste0('ccu022_01_', eth_name, '_Index2015_Followup10yr_TimeToOutcomeFollowup_by_PL_', file_date)
  saveRDS(outcome_df_tte, paste0(temp_data_dir, outcome_df_tte_name))

  ##### Sanity check #####
  # all(sapply(list(covar_df$PID, 
  #                 outcome_df_pre_index$PID, 
  #                 outcome_df_fup$PID, 
  #                 outcome_df_tte$PID), FUN = identical, 
  #            pred_df_pre_index$PID)) %>% print()
  # 
  # print(length(pred_df_pre_index$PID))
  
  }
  
  if (process_outcomelist2 == T) {
    ##### Order outcome data (list 2) by PID #####
    outcome_df_ordered2 <- as.data.table(outcome_df2) %>%
      .[order(PID)]
    
    ## sanity check
    # all.equal(pred_df_ordered$PID, outcome_df_ordered$PID)
    
    ##### Get matrix of pre-index outcome status #####
    outcome_df_pre_index_colnames2 <- c('PID',
                                       paste0(outcome_ds_vec2, '_preidx'))
    outcome_df_pre_index2 <- outcome_df_ordered2[, ..outcome_df_pre_index_colnames2] 
    colnames(outcome_df_pre_index2) <- c('PID', outcome_ds_vec2)
    
    outcome_df_pre_index_name2 <- paste0('ccu022_01_', eth_name,'_Index2015_OutcomeDiseaseStatusPreIndex_OutcomeList2_by_PL_', file_date)
    saveRDS(outcome_df_pre_index2, paste0(temp_data_dir, outcome_df_pre_index_name2))
    
    ##### Get matrix of outcome status during follow-up #####
    outcome_df_fup_colnames2 <- c('PID',
                                 paste0(outcome_ds_vec2, '_fup'))
    outcome_df_fup2 <- outcome_df_ordered2[, ..outcome_df_fup_colnames2] 
    colnames(outcome_df_fup2) <- c('PID', outcome_ds_vec2)
    
    outcome_df_fup_name2 <- paste0('ccu022_01_', eth_name, '_Index2015_Followup10yr_OutcomeDiseaseStatusFollowup_OutcomeList2_by_PL_', file_date)
    saveRDS(outcome_df_fup2, paste0(temp_data_dir, outcome_df_fup_name2))
    
    ##### Get matrix of time to outcome during follow-up #####
    outcome_df_tte_colnames2 <- c('PID',
                                 paste0(outcome_ds_vec2, '_tte'))
    outcome_df_tte2 <- outcome_df_ordered2[, ..outcome_df_tte_colnames2] 
    colnames(outcome_df_tte2) <- c('PID', outcome_ds_vec2)
    
    outcome_df_tte_name2 <- paste0('ccu022_01_', eth_name, '_Index2015_Followup10yr_TimeToOutcomeFollowup_OutcomeList2_by_PL_', file_date)
    saveRDS(outcome_df_tte2, paste0(temp_data_dir, outcome_df_tte_name2))
    
    ##### Sanity check #####
    # all(sapply(list(covar_df$PID, 
    #                 outcome_df_pre_index$PID, 
    #                 outcome_df_fup$PID, 
    #                 outcome_df_tte$PID), FUN = identical, 
    #            pred_df_pre_index$PID)) %>% print()
    # 
    # print(length(pred_df_pre_index$PID))
    
  }

}

