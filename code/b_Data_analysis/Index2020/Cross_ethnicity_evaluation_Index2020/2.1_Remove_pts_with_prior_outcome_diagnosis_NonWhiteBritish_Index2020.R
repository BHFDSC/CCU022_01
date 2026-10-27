### Author: Pearl Liu
### For each outcome, get correct study population where pts with outcome diagnosis pre-index are removed
### Index: 2020-01-01, follow-up: 5 years
### non-White British ethnicity groups

rm(list = ls())
gc()

#### Load libraries ####
library(data.table)
library(tidyverse)

#### Set parameters ####
temp_data_dir_step1_0 <- '~/collab/CCU022/CCU022_CRS/Data/Step1.0/'
temp_data_dir_step1_1 <- '~/collab/CCU022/CCU022_CRS/Data/Step1.1/'
temp_data_dir <- '~/collab/CCU022/CCU022_CRS/Data/Step2.1/'
file_date <- '2025_05_01'

eth_name_vec <- c("OtherWhite", "Asian", "Black", "Mixed")

#### Read List of predictors and outcomes ####
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

#for (l in 1:length(eth_name_vec)) {
for (l in c(1)) {
  
  eth_name <- eth_name_vec[l]
  
  #### Read pre-index outcome status ####
  outcome_df_pre_index_name <- paste0('ccu022_01_', eth_name,'_Index2020_OutcomeDiseaseStatusPreIndex_by_PL_', file_date)
  outcome_df_pre_index <- readRDS(paste0(temp_data_dir_step1_1, outcome_df_pre_index_name))
  
  #### Index 2020: Get correct study population for each outcome ####
  PID_eth <- outcome_df_pre_index[, .(PID)]
  outcome_mt <- outcome_df_pre_index[, PID := NULL]
  
  ## Exclude COVID outcome (last outcome) here
  phecode_outcome_ds_vec_numeric <- str_replace(outcome_ds_vec[-n_outcome_ds],
                                                '_', '\\.') %>%
    as.numeric()
  phecode_outcome_ds_vec_numeric_parent <- floor(phecode_outcome_ds_vec_numeric)
  
  for (i in 1:n_outcome_ds) {
    #print(nrow(PID_eth))
    
    target_ds <- outcome_ds_vec[i]
    
    if (target_ds != "COVID_hspt") {
      target_ds_numeric_parent <- phecode_outcome_ds_vec_numeric_parent[i]
      ## Find all outcome ds columns starting with the target outcome
      ## If the target outcome is a parent-level phecode, all its child phecodes need to be included
      ## only map phecode 8 to 8.5, etc., but not to 800
      target_ds_cols <- which( startsWith(outcome_ds_vec,  
                                          target_ds) & 
                                 phecode_outcome_ds_vec_numeric < target_ds_numeric_parent + 1)
    } else {
      target_ds_cols <- which(outcome_ds_vec == "COVID_hspt")
    }
    ## find the row-wise max of the outcome columns
    if (length(target_ds_cols) == 1) {
      target_ds_pre_index_vec <- outcome_mt[,..target_ds_cols]
    } else {
      target_ds_pre_index_vec <- do.call(pmax, 
                                     c(outcome_mt[,..target_ds_cols]) )
    }
    
    PID_eth[, target_ds_pre_index := target_ds_pre_index_vec]
    
    ##### Remove pts who had target outcome pre-index #####
    
    ## Select study population for analysis
    study_pop_eth <- PID_eth %>%
      .[, study_pop_idx := 1:nrow(PID_eth)] %>%
      .[target_ds_pre_index == 0] %>%
      .[,.(PID, study_pop_idx)]
    
    ## Save the study pop ID and index
    study_pop_name <- paste0("Outcome", target_ds, 
                             "_study_population_RmPtsWithPriorOutcomeDiag_",
                             eth_name, "_Index2020.rds")
    saveRDS(study_pop_eth, paste0(temp_data_dir, study_pop_name))
    
    print(paste0('Completed outcome ', i, ' for ', eth_name))
  
  }

}


