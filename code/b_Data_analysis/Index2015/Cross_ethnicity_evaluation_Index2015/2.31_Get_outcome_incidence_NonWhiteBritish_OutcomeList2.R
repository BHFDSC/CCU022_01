### Author: Pearl Liu
### For each outcome, get outcome incidence from testing data (full sample) for analysis
### Index: 2015-01-01, follow-up: 10 years
### non-White British ethnicity groups
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
temp_data_dir <- '~/collab/CCU022/CCU022_CRS/Data/Step2.3/'
file_date <- '2025_05_01'

eth_name_vec <- c("OtherWhite", "Asian", "Black", "Mixed")

#### Read processed data ####

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

#### Loop through each ethnicity group ####

for (l in 2:length(eth_name_vec)) {
  
  eth_name <- eth_name_vec[l]

  #### Loop through each outcome ####
  outcome_incid_vec <- rep(NA, n_outcome_ds)
  outcome_incid_vec_sf <- rep(NA, n_outcome_ds) 
  
  for (i in 1:n_outcome_ds) {
    
    ##### Read study pop index from Step 2.1 #####
    target_ds <- outcome_ds_vec[i]
    
    if (target_ds == "death") {
      target_ds_sex <- "Both"
    } else {
      target_ds_sex <- phecode_info %>%
        filter(PheCode == str_replace(target_ds, '_', '\\.')) %>%
        pull(Sex)
    }
    
    #### Read testing data: full sample ####
    test_df_combine <- readRDS(paste0(temp_data_dir, 'Outcome',
                                    target_ds, '_TestData_AllPts_', eth_name,'_Index2015_Followup10yr.rds'))
    
    
    #### Testing data: Get general and sex-specific outcome incidence ####
    outcome_incid_vec[i] <- mean(test_df_combine$target_ds_status)
    
    if (target_ds_sex == "Female") {
      test_df_combine_sf <- test_df_combine %>%
        filter(Female == 1)
      outcome_incid_vec_sf[i] <- mean(test_df_combine_sf$target_ds_status)
    } else if (target_ds_sex == "Male") {
      test_df_combine_sf <- test_df_combine %>%
        filter(Female == 0)
      outcome_incid_vec_sf[i] <- mean(test_df_combine_sf$target_ds_status)
    }
    
    print(paste0('Completed Outcome ', i, ' for ', eth_name))
    
  }
  
  #### Testing data: Save general and sex-specific outcome incidence ####
  
  ## General
  outcome_incid_file_name <- paste0(temp_data_dir, 'TestData_Outcome_Incidence_', eth_name, '_Index2015_Followup10yr_OutcomeList2.rds')
  if (file.exists(outcome_incid_file_name) == FALSE) {
    saveRDS(outcome_incid_vec, outcome_incid_file_name)
  }
  
  ## Sex-specific
  outcome_incid_file_sf_name <- paste0(temp_data_dir, 'TestData_Outcome_Incidence_Sex_Specific_', eth_name, '_Index2015_Followup10yr_OutcomeList2.rds')
  if (file.exists(outcome_incid_file_sf_name) == FALSE) {
    saveRDS(outcome_incid_vec_sf, outcome_incid_file_sf_name)
  }
  
  print(warnings())

}

