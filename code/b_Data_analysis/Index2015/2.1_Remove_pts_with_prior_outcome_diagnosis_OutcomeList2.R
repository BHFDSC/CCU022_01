### Author: Pearl Liu
### For each outcome, get correct study population where pts with outcome diagnosis pre-index are removed
### Outcome List 2

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

#### Read processed data ####

##### Pre-index outcome status #####
outcome_df_pre_index_even_name <- paste0('ccu022_01_WhiteBritish_EvenYOB_Index2015_OutcomeDiseaseStatusPreIndex_OutcomeList2_by_PL_', file_date)
outcome_df_pre_index_odd_name <- paste0('ccu022_01_WhiteBritish_OddYOB_Index2015_OutcomeDiseaseStatusPreIndex_OutcomeList2_by_PL_', file_date)

outcome_df_pre_index_even <- readRDS(paste0(temp_data_dir_step1_1, outcome_df_pre_index_even_name))
outcome_df_pre_index_odd <- readRDS(paste0(temp_data_dir_step1_1, outcome_df_pre_index_odd_name))


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


#### Index 2015: Get correct study population for each outcome ####
PID_even <- outcome_df_pre_index_even[, .(PID)]
PID_odd <- outcome_df_pre_index_odd[, .(PID)]

outcome_mt_even <- outcome_df_pre_index_even[, PID := NULL]
outcome_mt_odd <- outcome_df_pre_index_odd[, PID := NULL]

outcome_ds_vec_numeric <- str_replace(outcome_ds_vec, '_', '\\.') %>%
  as.numeric()
outcome_ds_vec_numeric_parent <- floor(outcome_ds_vec_numeric)

for (i in 1:n_outcome_ds) {
  print(nrow(PID_even))
  
  target_ds <- outcome_ds_vec[i]
  
  if (target_ds != "death") {
    target_ds_numeric_parent <- outcome_ds_vec_numeric_parent[i]
    ## Find all outcome ds columns starting with the target outcome
    ## If the target outcome is a parent-level phecode, all its child phecodes need to be included
    ## only map phecode 8 to 8.5, etc., but not to 800
    target_ds_cols <- which( startsWith(outcome_ds_vec,  
                                        target_ds) & 
                               outcome_ds_vec_numeric < target_ds_numeric_parent + 1)
  } else {
    target_ds_cols <- which(outcome_ds_vec == "death")
  }
  
  ## find the row-wise max of the outcome columns
  if (length(target_ds_cols) == 1) {
    target_ds_pre_index_even <- outcome_mt_even[,..target_ds_cols]
    target_ds_pre_index_odd <- outcome_mt_odd[,..target_ds_cols]
  } else {
    target_ds_pre_index_even <- do.call(pmax, 
                                         c(outcome_mt_even[,..target_ds_cols]) )
    target_ds_pre_index_odd <- do.call(pmax, 
                                        c(outcome_mt_odd[,..target_ds_cols]) )
  }
  
  PID_even[, target_ds_pre_index := target_ds_pre_index_even]
  PID_odd[, target_ds_pre_index := target_ds_pre_index_odd]
  
  ##### Remove pts who had target outcome pre-index #####
  
  ## Select study population for analysis
  study_pop_even <- PID_even %>%
    .[, study_pop_idx := 1:nrow(PID_even)] %>%
    .[target_ds_pre_index == 0] %>%
    .[,.(PID, study_pop_idx)]
  
  study_pop_odd <- PID_odd %>%
    .[, study_pop_idx := 1:nrow(PID_odd)] %>%
    .[target_ds_pre_index == 0] %>%
    .[,.(PID, study_pop_idx)]
  
  ## Save the study pop ID and index
  study_pop_even_name <- paste0("Outcome", target_ds, "_study_population_RmPtsWithPriorOutcomeDiag_WhiteBritish_EvenYOB_Index2015.rds")
  saveRDS(study_pop_even, paste0(temp_data_dir, study_pop_even_name))
  
  study_pop_odd_name <- paste0("Outcome", target_ds, "_study_population_RmPtsWithPriorOutcomeDiag_WhiteBritish_OddYOB_Index2015.rds")
  saveRDS(study_pop_odd, paste0(temp_data_dir, study_pop_odd_name))
  
  
  print(paste0('Completed outcome ', i))

}


