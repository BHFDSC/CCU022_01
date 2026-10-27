### Author: Pearl Liu
### For each outcome, Fit CRS and competing models in training data
### Odd DOB: training, Even DOB: testing
### Index: 2020-01-01, follow-up: 5 years


rm(list = ls())
gc()

#### Load libraries ####
library(data.table)
library(tidyverse)
library(sandwich)
library(doParallel)

source('~/collab/CCU022/CCU022_CRS/0.1_Functions.R')

#### Set parameters ####
temp_data_dir_step1_0 <- '~/collab/CCU022/CCU022_CRS/Data/Step1.0/'
temp_data_dir_step1_1 <- '~/collab/CCU022/CCU022_CRS/Data/Step1.1/'
temp_data_dir_step2_4 <- '~/collab/CCU022/CCU022_CRS/Data/Step2.4/'
temp_data_dir <- '~/collab/CCU022/CCU022_CRS/Data/Step3.0/'
file_date <- '2025_05_01'

## Control-case ratio in logistic regression of training data
control_case_ratio <- 4
## Number of covariates 
n_covar <- 2
## Model names
model_name_vec <- c("CRS_all_predictors", 
                    "CRS_top100",
                    "CRS_top50", 
                    "CRS_top20", 
                    "CRS_top10",
                    "Age_Sex")
n_models <- length(model_name_vec)
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


#### Find top common predictor diseases and record predictor names ####
pred_ds_prev_sort <- sort(pred_ds_prev, decreasing = T)
top100_pred_ds <- names(pred_ds_prev_sort)[1:100] 
top50_pred_ds <- names(pred_ds_prev_sort)[1:50]
top20_pred_ds <- names(pred_ds_prev_sort)[1:20]
top10_pred_ds <- names(pred_ds_prev_sort)[1:10] 

all_pred_ds_names <- paste0('ds', pred_ds_vec)
top100_pred_ds_names <- paste0('ds', top100_pred_ds)
top50_pred_ds_names <- paste0('ds', top50_pred_ds)
top20_pred_ds_names <- paste0('ds', top20_pred_ds)
top10_pred_ds_names <- paste0('ds', top10_pred_ds)

covar_names <- c('age_at_2020', 'Female')

#### Loop through each outcome - focus on COVID-19 outcome here ####

#train_R2_list <- vector(mode = "list", length = n_outcome_ds)

## progress bar
#pb <- txtProgressBar(min = 1, max = n_outcome_ds, style = 3)

## Set up clusters
#cl <- makeCluster(n_cores, outfile = "")
#registerDoParallel(cl)


#t_start <- Sys.time()
#foreach (i = 1:n_outcome_ds, 
#         .packages = c("data.table", "tidyverse", "sandwich")) %dopar% {

## Focus on COVID-19 outcome (last outcome)
for (i in c(74)) {  
  target_ds <- outcome_ds_vec[i]
  target_ds_incid <- outcome_incid_vec[i]
  
  target_file_name <-  paste0(temp_data_dir, 'Outcome',
                         target_ds, '_WeightedLogisticRegression_SummStats_ControlCase', 
                         control_case_ratio,'to1_WhiteBritish_OddYOB_Index2020_Followup5yr.rds')
  
  if (file.exists(target_file_name)) {
    print(paste0('Completed Outcome ', i)) 
  } else {
  
  source('~/collab/CCU022/CCU022_CRS/0.1_Functions.R')

  ##### Read training dataset #####
  train_df_fit <- readRDS(paste0(temp_data_dir_step2_4, 'Outcome',
                                         target_ds, '_TrainData_Fit_ControlCase', control_case_ratio,
                                         'to1_WhiteBritish_OddYOB_Index2020_Followup5yr.rds'))
  
  ##### Get regression formula #####
  all_colnames <- colnames(train_df_fit)
  pred_ds_all <- intersect(all_pred_ds_names, all_colnames)
  pred_ds_top100 <- intersect(top100_pred_ds_names, all_colnames)
  pred_ds_top50 <- intersect(top50_pred_ds_names, all_colnames)
  pred_ds_top20 <- intersect(top20_pred_ds_names, all_colnames)
  pred_ds_top10 <- intersect(top10_pred_ds_names, all_colnames)
  
  reg_formula_allds <- paste0('target_ds_status ~', paste(c(pred_ds_all, covar_names), 
                                                    collapse ='+')) %>% 
    as.formula()
  reg_formula_top100 <- paste0('target_ds_status ~', paste(c(pred_ds_top100, covar_names), 
                                                          collapse ='+')) %>% 
    as.formula()
  reg_formula_top50 <- paste0('target_ds_status ~', paste(c(pred_ds_top50, covar_names), 
                                                          collapse ='+')) %>% 
    as.formula()
  reg_formula_top20 <- paste0('target_ds_status ~', paste(c(pred_ds_top20, covar_names), 
                                                          collapse ='+')) %>% 
    as.formula()
  reg_formula_top10 <- paste0('target_ds_status ~', paste(c(pred_ds_top10, covar_names), 
                                                          collapse ='+')) %>% 
    as.formula()
  reg_formula_covar <- paste0('target_ds_status ~', paste(covar_names, 
                                                      collapse ='+')) %>% 
    as.formula()
  
  reg_formula_list <- list(reg_formula_allds,
                           reg_formula_top100,
                           reg_formula_top50,
                           reg_formula_top20,
                           reg_formula_top10,
                           reg_formula_covar)
  
  ##### Loop through each model #####
  ## General info
  Y <- train_df_fit$target_ds_status
  mean_Y <- mean(Y)
  
  ## Create a list of summary statistics
  target_ds_summ_list <- vector(mode = 'list', length = n_models)
  names(target_ds_summ_list) <- model_name_vec
  
  ## Create a dataframe of R2
  train_R2_dt <- data.frame("R2_obs" = rep(NA, n_models),
                      "R2_liab" = rep(NA, n_models))
  rownames(train_R2_dt) <- model_name_vec
  
  for (j in 1:n_models) {
    reg_formula <- reg_formula_list[[j]]
    model_name <- model_name_vec[j]
    
    ##### Weighted logistic regression - Control:case = 4:1 #####
    model_j <- glm(reg_formula,
                        weights = ipcw_wts,
                        data = train_df_fit,
                        family = binomial())
  
    vcov_model_j <- vcovHC(model_j, type = 'HC')
    
    model_j_est <- summary(model_j)$coefficients[,1]
    model_j_se <- sqrt(diag(vcov_model_j))
    
    model_j_summ <- data.frame(Est = model_j_est,
                               SE = model_j_se)
    
    target_ds_summ_list[[j]] <- model_j_summ
    
    ##### Obtain observed and liability R2 from training data #####
    target_ds_pred_response <- predict(model_j, type = "response")
    target_ds_pred_link <- predict(model_j, type = "link")
    
    ## observed R2
    R_obs_response <- cor(Y, target_ds_pred_response)^2
    
    ## liability R2 accounting for case-control sampling
    R_obs_link <- cor(Y, target_ds_pred_link)^2
    R2_liab <-  LiabR2_cc(K = target_ds_incid, P = mean_Y, 
                               R2_obs = R_obs_link)
    
    train_R2_dt[j,] <- c(R_obs_response, R2_liab)
    
  }
  
  ## Save summary stats from regression for each outcome
  saveRDS(target_ds_summ_list, paste0(temp_data_dir, 'Outcome',
                                      target_ds, '_WeightedLogisticRegression_SummStats_ControlCase', 
                                      control_case_ratio,'to1_WhiteBritish_OddYOB_Index2020_Followup5yr.rds'))
  saveRDS(train_R2_dt, 
          paste0(temp_data_dir, 'Outcome',
                 target_ds, '_TrainData_ObservedAndLiabilityR2_OddYOB_Index2020_Followup5yr.rds'))
  
  #setTxtProgressBar(pb, i)
  print(paste0('Completed Outcome ', i))  
  
  }
  
}

#t_end <- Sys.time()
#t_end - t_start

#stopCluster(cl)

#names(train_R2_list) <- outcome_ds_vec[c(13, 20:24)]
#saveRDS(train_R2_list, 
#        paste0(temp_data_dir, 'TrainData_AllOutcomes_ObservedAndLiabilityR2_OddYOB_Index2020_Followup5yr.rds'))


