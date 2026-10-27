### Author: Pearl Liu
### For each outcome, evaluate model performance in context-specific testing data
### Subset testing data based on year of outcome incidence
### Odd YOB: training, Even YOB: testing
### Index: 2015-01-01, follow-up: 10 years


rm(list = ls())
gc()

#### Load libraries ####
library(data.table)
library(tidyverse)
#library(sandwich)
library(pROC)
library(pec)
library(PRROC)
library(doParallel)
library(doRNG)

source('~/collab/CCU022/CCU022_CRS/0.1_Functions.R')

#### Set parameters ####
temp_data_dir_step1_0 <- '~/collab/CCU022/CCU022_CRS/Data/Step1.0/'
temp_data_dir_step1_1 <- '~/collab/CCU022/CCU022_CRS/Data/Step1.1/'
temp_data_dir_step2_3 <- '~/collab/CCU022/CCU022_CRS/Data/Step2.3/'
temp_data_dir_step2_4 <- '~/collab/CCU022/CCU022_CRS/Data/Step2.4/'
temp_data_dir_step3_0 <- '~/collab/CCU022/CCU022_CRS/Data/Step3.0/'
temp_data_dir_step6_0 <- '~/collab/CCU022/CCU022_CRS/Data/Step6.0/'
temp_data_dir <- '~/collab/CCU022/CCU022_CRS/Data/Step7.3/'
file_date <- '2025_05_01'


## Outcome year settings
outcome_year_settings <- list(c(1,1),
                              c(2,2),
                              c(3,4),
                              c(5,6),
                              c(7,8),
                              c(9,10))
n_outcome_year_settings <- length(outcome_year_settings)

## Model names
model_name_vec <- c("CRS_all_predictors", 
                  #  "CRS_top100",
                  #  "CRS_top50", 
                  #  "CRS_top20", 
                  #  "CRS_top10",
                    "Age_Sex")
model_idx_vec <- c(1, 6)
n_models <- length(model_name_vec)
## Control-case ratio in logistic regression of training data
control_case_ratio <- 4
## Number of covariates 
n_covar <- 2
## Number of Monte Carlo sampling in testing data
n_mc <- 100
## Control-case ratio in testing data for evaluating AUROC, C-index, R2
control_case_ratio_test <- 4
## Max number of cases used in testing data
n_cases_test1 <- 3000
n_cases_test2 <- 1000
## Number of cores used in parallel computing
#n_cores <- 8

#### Read processed data ####
##### List of predictors and outcomes #####
pred_ds_list_name = paste0("ccu022_01_Predictor_Diseases_Pheparent_list_by_PL_", file_date)
outcome_ds_list_name = paste0("ccu022_01_Outcome_Diseases_Phecode_list_by_PL_", file_date)
outcome_ds_list_name2 = paste0("ccu022_01_Outcome_Diseases_Phecode_list2_TopCommonPlusPheRS_by_PL_", file_date)

pred_ds_list = readRDS(paste0(temp_data_dir_step1_0, pred_ds_list_name))
outcome_ds_list = readRDS(paste0(temp_data_dir_step1_0, outcome_ds_list_name))
outcome_ds_list2 = readRDS(paste0(temp_data_dir_step1_0, outcome_ds_list_name2))

pred_ds_vec <- pred_ds_list %>% arrange(index) %>% pull(predictor_ds) 
outcome_ds_vec <- c(outcome_ds_list %>% arrange(index) %>% pull(target_ds),
                    outcome_ds_list2 %>% arrange(index) %>% pull(target_ds),
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


##### Outcome incidence #####
## Note: here we use the original 10-year incidence for each year subset, as year-specific incidence can be very low for some outcomes
outcome_incid_vec1 <- readRDS(paste0(temp_data_dir_step2_4, 
                                     'TrainData_Outcome_Incidence_OddYOB_Index2015_Followup10yr.rds'))
outcome_incid_vec2 <- readRDS(paste0(temp_data_dir_step2_4, 
                                     'TrainData_Outcome_Incidence_OddYOB_Index2015_Followup10yr_OutcomeList2.rds'))

outcome_incid_vec <- c(outcome_incid_vec1, outcome_incid_vec2)

outcome_incid_vec_sf1 <- readRDS(paste0(temp_data_dir_step2_4, 
                                        'TrainData_Outcome_Incidence_Sex_Specific_OddYOB_Index2015_Followup10yr.rds'))

outcome_incid_vec_sf <- c(outcome_incid_vec_sf1, rep(NA, length(outcome_incid_vec2)))


#### Loop through each outcome ####
### Use parallel computing

## Set up clusters
#cl <- makeCluster(n_cores, outfile = "")
#registerDoParallel(cl)

## Set random seed for parallel computing
#registerDoRNG(seed = 2025)

#foreach (i = 1:n_outcome_ds, 
#         .packages = c("data.table", "tidyverse", "sandwich",
#                       "pROC", "pec", "PRROC")) %dopar% {
set.seed(2025)

for (i in 1:44) {
  
    #source('~/collab/CCU022/CCU022_CRS/0.1_Functions.R')

    target_ds <- outcome_ds_vec[i]
    if (target_ds == "death") {
      target_ds_sex <- "Both"
    } else {
      target_ds_sex <- phecode_info %>%
        filter(PheCode == str_replace(target_ds, '_', '\\.')) %>%
        pull(Sex)
    }
    
    target_file_name_general <- paste0(temp_data_dir, 
                                 'Outcome', target_ds, 
                                 '_OutcomeYearSetting', n_outcome_year_settings,
                                 '_TestData_PredictionMetrics_ControlCase',
                                 control_case_ratio, 'to1_WhiteBritish_EvenYOB_Index2015_Followup10yr.rds')
    target_file_name_sex_specific <- paste0(temp_data_dir, 
                                       'Outcome', target_ds, 
                                       '_OutcomeYearSetting', n_outcome_year_settings,
                                       '_Sex_Specific_TestData_PredictionMetrics_ControlCase',
                                       control_case_ratio, 'to1_WhiteBritish_EvenYOB_Index2015_Followup10yr.rds')
    
    if ( (target_ds_sex %in% c("Both", "Empty") & 
          file.exists(target_file_name_general)) | 
         (target_ds_sex %in% c("Female", "Male") & 
          file.exists(target_file_name_sex_specific)) ) {
      print(paste0('Completed Outcome ', i)) 
    } else {
      
      
    #### Loop through each outcome year group ####
    for (j in 1:n_outcome_year_settings) {
      
      gc()
      
      ### Read full testing data
      test_df_combine <- readRDS(paste0(temp_data_dir_step6_0, 'Outcome',
                                        target_ds, '_OutcomeYearSetting', j,
                                        '_TestData_AllPts_WhiteBritish_EvenYOB_Index2015_Followup10yr.rds'))
      
      
      #### Sex-specific evaluation ####
      if ( target_ds_sex %in% c("Male", "Female") & 
           file.exists(target_file_name_sex_specific) == FALSE ) {
        
        if (target_ds_sex == "Female") {
          test_df_combine_sf <- test_df_combine[Female == 1]
        } else {
          test_df_combine_sf <- test_df_combine[Female == 0]
        }
        
        n_cases <- sum(test_df_combine_sf$target_ds_status == 1)
        
        target_ds_incid_sf <- outcome_incid_vec_sf[i]
        
        n_controls <- sum(test_df_combine_sf$target_ds_status == 0)
        
        case_pts_idx <- which(test_df_combine_sf$target_ds_status == 1)
        control_pts_idx <- which(test_df_combine_sf$target_ds_status == 0)
        
        ## PL 20251210 update: let n_controls_select1 be at most n_controls
        n_cases_select1 <- min(n_cases_test1, n_cases)
        n_controls_select1 <- min(n_cases_select1 * control_case_ratio_test,
                                  n_controls)
        
        n_cases_select2 <- min(n_cases_test2, n_cases)
        
    #    if (n_cases_select2 == n_cases_test2) {
        control_case_ratio_orig <- (1 - target_ds_incid_sf)/target_ds_incid_sf
        n_controls_select2 <- min(round(n_cases_select2 * control_case_ratio_orig),
                                  n_controls)
    #    } else if (n_cases_select2 == n_cases) {
    #      n_controls_select2 <- n_controls
    #    }
        
        ##### Read coefficients from training data #####
        target_ds_summ_list <- readRDS(paste0(temp_data_dir_step3_0, 'Outcome',
                                              target_ds, '_Sex_Specific_WeightedLogisticRegression_SummStats_ControlCase', 
                                              control_case_ratio,'to1_WhiteBritish_OddYOB_Index2015_Followup10yr.rds'))
        
        ## Create a list of model-specific performance metrics, record each iteration
        target_ds_metric_list <- vector(mode = 'list', length = n_models)
        names(target_ds_metric_list) <- model_name_vec
        ## Create a dataframe of performance metrics for each model
        for (l in 1:n_models) {
          metrics_dt <- data.frame("R2_obs" = rep(NA, n_mc+1),
                                   "R2_liab" = rep(NA, n_mc+1),
                                   "AUROC" = rep(NA, n_mc+1),
                                   "C_index" = rep(NA, n_mc+1),
                                   "AUPRC" = rep(NA, n_mc+1),
                                   "AUPRC_prev_diff" = rep(NA, n_mc+1))
          target_ds_metric_list[[l]] <- metrics_dt
        }
        
        ##### Initial sample #####
        ## Testing data 1: Subsample to case:control = 1:4 for assessment of AUROC, C-index, R^2 
        set.seed(2025 + i)
        case_subsample_idx1 <- sample(case_pts_idx, n_cases_select1)
        control_subsample_idx1 <- sample(control_pts_idx, n_controls_select1)
        
        subsample_idx1 <- c(case_subsample_idx1, control_subsample_idx1)
        test_dt1 <- test_df_combine_sf[subsample_idx1, ]
        
        ## Testing data 2: Subsample to a subset of all pts for assessment of AURPC, maintaining original case:control ratio 
        set.seed(2025 + i)
        case_subsample_idx2 <- sample(case_pts_idx, n_cases_select2)
        control_subsample_idx2 <- sample(control_pts_idx, n_controls_select2)
        
        subsample_idx2 <- c(case_subsample_idx2, control_subsample_idx2)
        test_dt2 <- test_df_combine_sf[subsample_idx2, ]
        
        for (l in 1:n_models) {
          target_summ <- target_ds_summ_list[[model_idx_vec[l]]]
          target_ds_metric_list[[l]][1,] <- test_pred_models(test_dt1, 
                                                             test_dt2,
                                                             target_ds_incid_sf,
                                                             target_summ)
          
        }
        
        ##### Bootstrapping of testing data #####
        ## Bootstrapping on the selected subsample, preserve case-control ratio in each bootstrap
        for (k in 1:n_mc) {
          ## Testing data 1: Subsample to case:control = 1:4 for assessment of AUROC, C-index, R^2 
          set.seed(2025 + i + k)
          case_subsample_idx1_boot <- sample(case_subsample_idx1, replace = T)
          control_subsample_idx1_boot <- sample(control_subsample_idx1, replace = T)
          
          subsample_idx1_boot <- c(case_subsample_idx1_boot, control_subsample_idx1_boot)
          test_dt1_boot <- test_df_combine_sf[subsample_idx1_boot, ]
          
          ## Testing data 2: Subsample to a subset of all pts for assessment of AURPC, maintaining original case:control ratio 
          set.seed(2025 + i + k)
          case_subsample_idx2_boot <- sample(case_subsample_idx2, replace = T)
          control_subsample_idx2_boot <- sample(control_subsample_idx2, replace = T)
          
          subsample_idx2_boot <- c(case_subsample_idx2_boot, 
                                   control_subsample_idx2_boot)
          test_dt2_boot <- test_df_combine_sf[subsample_idx2_boot, ]
          
          for (l in 1:n_models) {
            target_summ <- target_ds_summ_list[[model_idx_vec[l]]]
            target_ds_metric_list[[l]][k+1,] <- test_pred_models(test_dt1_boot, 
                                                                 test_dt2_boot,
                                                                 target_ds_incid_sf,
                                                                 target_summ)
          }
          
        }
        
        saveRDS(target_ds_metric_list, paste0(temp_data_dir, 'Outcome',
                                              target_ds, '_OutcomeYearSetting', j,
                                              '_Sex_Specific_TestData_PredictionMetrics_ControlCase',
                                              control_case_ratio, 'to1_WhiteBritish_EvenYOB_Index2015_Followup10yr.rds'))
        
        print(paste0('Completed Outcome ', i, ' (sex-specific) for Setting', j))  
        
      }
      
      #### General evaluation ####
      if ( target_ds_sex %in% c("Both", "Empty") & 
           file.exists(target_file_name_general) == FALSE ) {
        
        n_cases <- sum(test_df_combine$target_ds_status == 1)
        
        target_ds_incid <- outcome_incid_vec[i]
        
        n_controls <- sum(test_df_combine$target_ds_status == 0)
        
        case_pts_idx <- which(test_df_combine$target_ds_status == 1)
        control_pts_idx <- which(test_df_combine$target_ds_status == 0)
        
        ## PL 20251210 update: let n_controls_select1 have max of n_controls
        n_cases_select1 <- min(n_cases_test1, n_cases)
        n_controls_select1 <- min(n_cases_select1 * control_case_ratio_test,
                                  n_controls)
        
        n_cases_select2 <- min(n_cases_test2, n_cases)
        
   #     if (n_cases_select2 == n_cases_test2) {
        control_case_ratio_orig <- (1 - target_ds_incid)/target_ds_incid
        n_controls_select2 <- min(round(n_cases_select2 * control_case_ratio_orig),
                                  n_controls)
  #      } else if (n_cases_select2 == n_cases) {
  #        n_controls_select2 <- n_controls
  #      }
        
        ##### Read coefficients from training data #####
        target_ds_summ_list <- readRDS(paste0(temp_data_dir_step3_0, 'Outcome',
                                              target_ds, '_WeightedLogisticRegression_SummStats_ControlCase', 
                                              control_case_ratio,'to1_WhiteBritish_OddYOB_Index2015_Followup10yr.rds'))
        
        ## Create a list of model-specific performance metrics, record each iteration
        target_ds_metric_list <- vector(mode = 'list', length = n_models)
        names(target_ds_metric_list) <- model_name_vec
        
        for (l in 1:n_models) {
          metrics_dt <- data.frame("R2_obs" = rep(NA, n_mc+1),
                                   "R2_liab" = rep(NA, n_mc+1),
                                   "AUROC" = rep(NA, n_mc+1),
                                   "C_index" = rep(NA, n_mc+1),
                                   "AUPRC" = rep(NA, n_mc+1),
                                   "AUPRC_prev_diff" = rep(NA, n_mc+1))
          target_ds_metric_list[[l]] <- metrics_dt
        }
        
        ##### Initial sample #####
        ## Testing data 1: Subsample to case:control = 1:4 for assessment of AUROC, C-index, R^2 
        set.seed(2025 + i)
        case_subsample_idx1 <- sample(case_pts_idx, n_cases_select1)
        control_subsample_idx1 <- sample(control_pts_idx, n_controls_select1)
        
        subsample_idx1 <- c(case_subsample_idx1, control_subsample_idx1)
        test_dt1 <- test_df_combine[subsample_idx1, ]
        
        ## Testing data 2: Subsample to a subset of all pts for assessment of AURPC, maintaining original case:control ratio 
        set.seed(2025 + i)
        case_subsample_idx2 <- sample(case_pts_idx, n_cases_select2)
        control_subsample_idx2 <- sample(control_pts_idx, n_controls_select2)
        
        subsample_idx2 <- c(case_subsample_idx2, control_subsample_idx2)
        test_dt2 <- test_df_combine[subsample_idx2, ]
        
        for (l in 1:n_models) {
          target_summ <- target_ds_summ_list[[model_idx_vec[l]]]
          target_ds_metric_list[[l]][1,] <- test_pred_models(test_dt1, 
                                                             test_dt2,
                                                             target_ds_incid,
                                                             target_summ)
        }
        ##### Bootstrapping of testing data #####
        ## Bootstrapping on the selected subsample, preserve case-control ratio in each bootstrap
        for (k in 1:n_mc) {
          ## Testing data 1: Subsample to case:control = 1:4 for assessment of AUROC, C-index, R^2 
          set.seed(2025 + i + k)
          case_subsample_idx1_boot <- sample(case_subsample_idx1, replace = T)
          control_subsample_idx1_boot <- sample(control_subsample_idx1, replace = T)
          
          subsample_idx1_boot <- c(case_subsample_idx1_boot, control_subsample_idx1_boot)
          test_dt1_boot <- test_df_combine[subsample_idx1_boot, ]
          
          ## Testing data 2: Subsample to a subset of all pts for assessment of AURPC, maintaining original case:control ratio 
          set.seed(2025 + i + k)
          case_subsample_idx2_boot <- sample(case_subsample_idx2, replace = T)
          control_subsample_idx2_boot <- sample(control_subsample_idx2, replace = T)
          
          subsample_idx2_boot <- c(case_subsample_idx2_boot, 
                                   control_subsample_idx2_boot)
          test_dt2_boot <- test_df_combine[subsample_idx2_boot, ]
          
          
          for (l in 1:n_models) {
            target_summ <- target_ds_summ_list[[model_idx_vec[l]]]
            target_ds_metric_list[[l]][k+1,] <- test_pred_models(test_dt1_boot, 
                                                                 test_dt2_boot,
                                                                 target_ds_incid,
                                                                 target_summ)
          }
          
        }
        
        saveRDS(target_ds_metric_list, paste0(temp_data_dir, 'Outcome',
                                              target_ds, '_OutcomeYearSetting', j,
                                              '_TestData_PredictionMetrics_ControlCase',
                                              control_case_ratio, 'to1_WhiteBritish_EvenYOB_Index2015_Followup10yr.rds'))
        
        
        print(paste0('Completed Outcome ', i, ' (both sexes) for Setting ', j))  
      }
      
      
    }
    }
    
    print(warnings())
 }


#stopCluster(cl)
print(warnings())

