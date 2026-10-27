### Author: Pearl Liu
### For COVID hspt outcome, evaluate model performance in testing data
### Odd DOB: training, Even DOB: testing
### Index: 2020-01-01, follow-up: 5 years


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

source('~/collab/CCU022/CCU022_CRS/0.1_Functions.R')

#### Set parameters ####
temp_data_dir_step1_0 <- '~/collab/CCU022/CCU022_CRS/Data/Step1.0/'
temp_data_dir_step1_1 <- '~/collab/CCU022/CCU022_CRS/Data/Step1.1/'
temp_data_dir_step2_3 <- '~/collab/CCU022/CCU022_CRS/Data/Step2.3/'
temp_data_dir_step2_4 <- '~/collab/CCU022/CCU022_CRS/Data/Step2.4/'
temp_data_dir_step3_0 <- '~/collab/CCU022/CCU022_CRS/Data/Step3.0/'
temp_data_dir <- '~/collab/CCU022/CCU022_CRS/Data/Step4.0/'
file_date <- '2025_05_01'

## Model names
model_name_vec <- c("CRS_all_predictors", 
                    "CRS_top100",
                    "CRS_top50", 
                    "CRS_top20", 
                    "CRS_top10",
                    "Age_Sex")
n_models <- length(model_name_vec)
## Control-case ratio in logistic regression of training data
control_case_ratio <- 4
## Number of covariates 
n_covar <- 2
## Number of Monte Carlo sampling in testing data
n_mc <- 100  # PL 20251202: change n_mc from 200 to 100
## Control-case ratio in testing data for evaluating AUROC, C-index, R2
control_case_ratio_test <- 4
## Number of cases used in testing data
n_cases_test1 <- 3000
n_cases_test2 <- 1000
## Number of cores used in parallel computing
n_cores <- 6

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

##### Predictor prevalence #####
pred_ds_prev <- readRDS(paste0(temp_data_dir_step1_1, 
                               'WhiteBritish_OddYOB_Index2020_PredictorDiseasePrevalence.rds'))

##### Outcome incidence #####
outcome_incid_vec <- readRDS(paste0(temp_data_dir_step2_4, 
                                    'TrainData_Outcome_Incidence_OddYOB_Index2020_Followup5yr.rds'))

#### Loop through each outcome - focus on COVID-19 outcome here (last outcome) ####
### Use parallel computing

## Set up clusters
#cl <- makeCluster(n_cores, outfile = "")
#registerDoParallel(cl)

#foreach (i = 1:n_outcome_ds, 
#         .packages = c("data.table", "tidyverse", "sandwich",
#                       "pROC", "pec", "PRROC")) %dopar% {

for (i in c(74)) {
  target_ds <- outcome_ds_vec[i]
  target_ds_incid <- outcome_incid_vec[i]
  
  target_file_name <- paste0(temp_data_dir, 'Outcome',
                               target_ds, '_TestData_PredictionMetrics_ControlCase',
                               control_case_ratio, 'to1_WhiteBritish_EvenYOB_Index2020_Followup5yr.rds')
  
  if (file.exists(target_file_name)) {
    print(paste0('Completed Outcome ', i)) 
  } else {
    
  source('~/collab/CCU022/CCU022_CRS/0.1_Functions.R')
  
  ##### Read full testing data #####
  test_df_combine <- readRDS(paste0(temp_data_dir_step2_3, 'Outcome',
                                    target_ds, '_TestData_AllPts_WhiteBritish_EvenYOB_Index2020_Followup5yr.rds'))
  
  n_cases <- sum(test_df_combine$target_ds_status == 1)
  n_controls <- sum(test_df_combine$target_ds_status == 0)
  case_pts_idx <- which(test_df_combine$target_ds_status == 1)
  control_pts_idx <- which(test_df_combine$target_ds_status == 0)
  
  n_cases_select1 <- min(n_cases_test1, n_cases)
  n_controls_select1 <- n_cases_select1 * control_case_ratio_test
  
  n_cases_select2 <- min(n_cases_test2, n_cases)
  control_case_ratio_orig <- (1 - target_ds_incid)/target_ds_incid
  n_controls_select2 <- round(n_cases_select2 * control_case_ratio_orig)
  
  ##### Read coefficients from training data #####
  target_ds_summ_list <- readRDS(paste0(temp_data_dir_step3_0, 'Outcome',
                                      target_ds, '_WeightedLogisticRegression_SummStats_ControlCase', 
                                      control_case_ratio,'to1_WhiteBritish_OddYOB_Index2020_Followup5yr.rds'))
  
  
  ## Create a list of model-specific performance metrics, record each iteration
  target_ds_metric_list <- vector(mode = 'list', length = n_models)
  names(target_ds_metric_list) <- model_name_vec
  ## Create a dataframe of performance metrics for each model
  for (j in 1:n_models) {
    metrics_dt <- data.frame("R2_obs" = rep(NA, n_mc),
                             "R2_liab" = rep(NA, n_mc),
                             "AUROC" = rep(NA, n_mc),
                             "C_index" = rep(NA, n_mc),
                             "AUPRC" = rep(NA, n_mc),
                             "AUPRC_prev_diff" = rep(NA, n_mc))
    target_ds_metric_list[[j]] <- metrics_dt
  }
  
  
  #### Monte Carlo sampling of testing data ####
  for (k in 1:n_mc) {
    ## Testing data 1: Subsample to case:control = 1:4 for assessment of AUROC, C-index, R^2 
    set.seed(2026 + i + k)
    case_subsample_idx1 <- sample(case_pts_idx, n_cases_select1)
    control_subsample_idx1 <- sample(control_pts_idx, n_controls_select1)
    
    subsample_idx1 <- c(case_subsample_idx1, control_subsample_idx1)
    test_dt1 <- test_df_combine[subsample_idx1, ]
    
    ## Testing data 2: Subsample to a subset of all pts for assessment of AURPC, maintaining original case:control ratio 
    set.seed(2026 + i + k)
    case_subsample_idx2 <- sample(case_pts_idx, n_cases_select2)
    control_subsample_idx2 <- sample(control_pts_idx, n_controls_select2)
    
    subsample_idx2 <- c(case_subsample_idx2, control_subsample_idx2)
    test_dt2 <- test_df_combine[subsample_idx2, ]
    
    ## Loop through each model
    for (j in 1:n_models) {
      target_summ <- target_ds_summ_list[[j]]
      metrics_k <- test_pred_models(test_dt1, 
                                     test_dt2,
                                     target_ds_incid,
                                     target_summ)
      
      target_ds_metric_list[[j]][k,] <- metrics_k
    }
  
  }
  
  saveRDS(target_ds_metric_list, paste0(temp_data_dir, 'Outcome',
                        target_ds, '_TestData_PredictionMetrics_ControlCase',
                        control_case_ratio, 'to1_WhiteBritish_EvenYOB_Index2020_Followup5yr.rds'))
  
  print(paste0('Completed Outcome ', i))  
  
  }
}

#stopCluster(cl)
