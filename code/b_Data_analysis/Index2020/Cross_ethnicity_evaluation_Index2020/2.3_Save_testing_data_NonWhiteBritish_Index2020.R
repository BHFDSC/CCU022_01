### Author: Pearl Liu
### For each outcome, save all data as testing data (full sample) for analysis
### Index: 2020-01-01, follow-up: 5 years
### non-White British ethnicity groups

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

#eth_name_vec <- c("Asian", "Black", "Mixed", "Other")
eth_name_vec <- c("OtherWhite", "Asian", "Black", "Mixed")

#### Read processed data ####

##### List of predictors and outcomes #####
pred_ds_list_name = paste0("ccu022_01_Predictor_Diseases_Pheparent_list_by_PL_", file_date)
outcome_ds_list_name = paste0("ccu022_01_Outcome_Diseases_Phecode_list_by_PL_", file_date)

pred_ds_list = readRDS(paste0(temp_data_dir_step1_0, pred_ds_list_name))
outcome_ds_list = readRDS(paste0(temp_data_dir_step1_0, outcome_ds_list_name))

pred_ds_vec <- pred_ds_list %>% arrange(index) %>% pull(predictor_ds) 
outcome_ds_vec <- c(outcome_ds_list %>% arrange(index) %>% pull(target_ds),
                    'COVID_hspt')

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

#for (l in 1:length(eth_name_vec)) {
for (l in c(1)) {
  
  eth_name <- eth_name_vec[l]

  ##### Read predictor status pre-index #####
  pred_df_pre_index_name <- paste0('ccu022_01_', eth_name,'_Index2020_PredictorDiseaseStatus_by_PL_', file_date)
  pred_df_pre_index <- readRDS(paste0(temp_data_dir_step1_1, pred_df_pre_index_name))

  ##### Read outcome status during follow-up #####
  outcome_df_fup_name <- paste0('ccu022_01_', eth_name,'_Index2020_Followup5yr_OutcomeDiseaseStatusFollowup_by_PL_', file_date)
  outcome_df_fup <- readRDS(paste0(temp_data_dir_step1_1, outcome_df_fup_name))

  ##### Read time to outcome during follow-up #####
  outcome_df_tte_name <- paste0('ccu022_01_', eth_name,'_Index2020_Followup5yr_TimeToOutcomeFollowup_by_PL_', file_date)
  outcome_df_tte <- readRDS(paste0(temp_data_dir_step1_1, outcome_df_tte_name))
  
  ##### Read covariates data #####
  covar_df_name <- paste0('ccu022_01_', eth_name,'_Index2020_Covariates_by_PL_', file_date)
  covar_df <- readRDS(paste0(temp_data_dir_step1_1, covar_df_name))
  
  #### Loop through each outcome ####
  outcome_incid_vec <- rep(NA, n_outcome_ds)
  outcome_incid_vec_sf <- rep(NA, n_outcome_ds) 
  
  for (i in 1:n_outcome_ds) {
    
    ##### Read study pop index from Step 2.1 #####
    target_ds <- outcome_ds_vec[i]
    
    if (target_ds != "COVID_hspt") {
      target_ds_sex <- phecode_info %>%
        filter(PheCode == str_replace(target_ds, '_', '\\.')) %>%
        pull(Sex)
    } else {
      target_ds_sex <- "NULL"
    }
    
    study_pop_name <- paste0("Outcome", target_ds, "_study_population_RmPtsWithPriorOutcomeDiag_", eth_name,"_Index2020.rds")
    study_pop_eth <- readRDS(paste0(temp_data_dir_step2_1, study_pop_name))

    study_pop_idx <- study_pop_eth$study_pop_idx

    rm(study_pop_eth)
    
    ##### Subset testing data to correct study population #####
    pred_df_test <- pred_df_pre_index[study_pop_idx,]
    covar_df_test <- covar_df[study_pop_idx,]
    outcome_df_fup_test <- outcome_df_fup[study_pop_idx,]
    outcome_df_tte_test <- outcome_df_tte[study_pop_idx,]
    
    #### Testing data: Get correct list of predictors ####
    
    ##### Remove parent phecode that contain the outcome #####
    ## Restrict to correct predictor data for training data
    if (target_ds != "COVID_hspt") {
      target_ds_parent <- gsub("_.*", "", target_ds)
    } else {
      target_ds_parent <- target_ds
    }
    pred_df_test2 <- pred_df_test %>%
      .[, (target_ds_parent) := NULL] %>%
      .[, PID := NULL]
    
    ## Change colnames
    colnames(pred_df_test2) <- paste0('ds', colnames(pred_df_test2))
    
    ## Add covariates
    pred_df_test2 <- pred_df_test2[, c('age_at_2020', 'Female') := covar_df_test[,c('age_at_2020', 'Female')]]
    
    
    #### Testing data: Combine predictors and outcomes, set censored pts to controls ####
    target_ds_test <- outcome_df_fup_test[, ..target_ds]
    colnames(target_ds_test) <- c('target_ds_status')
    
    ## set censored pts to controls
    target_ds_test <- target_ds_test %>%
      .[, target_ds_status := ifelse(target_ds_status == 9, 0, target_ds_status)]
    
    ## combine outcome data with predictor data
    test_df_combine <- pred_df_test2[, colnames(target_ds_test) := target_ds_test]
    
    ## Add time to outcome event
    target_ds_tte <- outcome_df_tte_test[, ..target_ds]
    colnames(target_ds_tte) <- c('target_ds_tte')
    test_df_combine <- test_df_combine[, colnames(target_ds_tte) := target_ds_tte]
    
    #### Testing data: Save analytical dataset - full sample ####
    saveRDS(test_df_combine, paste0(temp_data_dir, 'Outcome',
                                    target_ds, '_TestData_AllPts_', eth_name,'_Index2020_Followup5yr.rds'))
    
    
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
  outcome_incid_file_name <- paste0(temp_data_dir, 'TestData_Outcome_Incidence_', eth_name, '_Index2020_Followup5yr.rds')
  if (file.exists(outcome_incid_file_name) == FALSE) {
    saveRDS(outcome_incid_vec, outcome_incid_file_name)
  }
  
  ## Sex-specific
  outcome_incid_file_sf_name <- paste0(temp_data_dir, 'TestData_Outcome_Incidence_Sex_Specific_', eth_name, '_Index2020_Followup5yr.rds')
  if (file.exists(outcome_incid_file_sf_name) == FALSE) {
    saveRDS(outcome_incid_vec_sf, outcome_incid_file_sf_name)
  }
  
  print(warnings())

}

