### Author: Pearl Liu
### Compare CRS against QCovid model for COVID-19 hspt outcome
### Calibrate QCovid within training data, test Qcovid, CRS + age + sex, Qcovid + CRS, Qcovid + CRS + age + sex
### Odd DOB: training, Even DOB: testing
### Index: 2020-01-01, follow-up: 5 years


rm(list = ls())
gc()

#### Load libraries ####
library(data.table)
library(tidyverse)
library(sandwich)
library(pROC)
library(pec)
library(PRROC)

source('~/collab/CCU022/CCU022_CRS/0.1_Functions.R')

#### Set parameters ####
temp_data_dir_step1_0 <- '~/collab/CCU022/CCU022_CRS/Data/Step1.0/'
temp_data_dir_step1_1 <- '~/collab/CCU022/CCU022_CRS/Data/Step1.1/'
temp_data_dir_step2_1 <- '~/collab/CCU022/CCU022_CRS/Data/Step2.1/'
temp_data_dir_step2_3 <- '~/collab/CCU022/CCU022_CRS/Data/Step2.3/'
temp_data_dir_step2_4 <- '~/collab/CCU022/CCU022_CRS/Data/Step2.4/'
temp_data_dir_step3_0 <- '~/collab/CCU022/CCU022_CRS/Data/Step3.0/'
temp_data_dir_step10_0 <- '~/collab/CCU022/CCU022_CRS/Data/Step10.0/'
temp_data_dir <- '~/collab/CCU022/CCU022_CRS/Data/Step10.2/'
file_date <- '2025_05_01'

target_ds <- "COVID_hspt"

## Model names
model_train_name_vec <- c("QCovid",
                          "CRS_QCovid",
                         "CRS_all_predictors_QCovid",
                         "CRS_QCovid_Age_Sex")
n_models_train <- length(model_train_name_vec)


model_test_name_vec <- c("CRS_all_predictors", 
                    "QCovid",
                    "CRS_QCovid",
                    "CRS_all_predictors_QCovid",
                    "CRS_QCovid_Age_Sex",
                    "Age_Sex")
n_models_test <- length(model_test_name_vec)


## Control-case ratio in logistic regression of training data
control_case_ratio <- 4
## Number of Monte Carlo sampling in testing data
n_mc <- 100  # PL 2026 update
## Control-case ratio in testing data for evaluating AUROC, C-index, R2
control_case_ratio_test <- 4
## Number of cases used in testing data
n_cases_test1 <- 3000
n_cases_test2 <- 1000
## Number of cores used in parallel computing
n_cores <- 5

## Whether to perform calibration and training in this run
calib_train <- 0

#### Read processed data ####
##### Outcome incidence #####
outcome_incid_vec <- readRDS(paste0(temp_data_dir_step2_4, 
                                    'TrainData_Outcome_Incidence_OddYOB_Index2020_Followup5yr.rds'))


##### Phecode information #####
phe_icd10_codelist_name = paste0("ccu022_01_PheCode_ICD10AtoN_map_by_PL_", file_date)
icd10_phecode_map <- readRDS(paste0(temp_data_dir_step1_0, 
                                    phe_icd10_codelist_name))
phecode_info <- icd10_phecode_map %>%
  select(PheCode, Phenotype, Category, Sex) %>%
  distinct()

#### Read prepared data for QCOVID vs CRS comparison ####
##### Read train data #####
if (calib_train == 1) {
compare_train_df_crs <- readRDS(paste0(temp_data_dir_step10_0, 'ForComparison_Outcome',
                                      target_ds, '_CRS_TrainData_WhiteBritish_OddYOB_Index2020_Followup5yr.rds'))
compare_train_df_qcovid <- readRDS(paste0(temp_data_dir_step10_0, 'ForComparison_Outcome',
                                         target_ds, '_QCovid_TrainData_WhiteBritish_OddYOB_Index2020_Followup5yr.rds'))

compare_train_df_crs <- compare_train_df_crs[order(PID)]
}

##### Read test data #####
compare_test_df_crs <- readRDS(paste0(temp_data_dir_step10_0, 'ForComparison_Outcome',
                                      target_ds, '_CRS_TestData_WhiteBritish_EvenYOB_Index2020_Followup5yr.rds'))
compare_test_df_qcovid <- readRDS(paste0(temp_data_dir_step10_0, 'ForComparison_Outcome',
                                         target_ds, '_QCovid_TestData_WhiteBritish_EvenYOB_Index2020_Followup5yr.rds'))

compare_test_df_crs <- compare_test_df_crs[order(PID)]

##### Read trained CRS effect sizes #####
crs_summ_list <- readRDS(paste0(temp_data_dir_step3_0, 'Outcome',
                                target_ds, '_WeightedLogisticRegression_SummStats_ControlCase', 
                                control_case_ratio,'to1_WhiteBritish_OddYOB_Index2020_Followup5yr.rds'))
crs_summ <- crs_summ_list$CRS_all_predictors


##### Read QCovid effect sizes #####
qcovid_summ_female <- read.table(paste0(temp_data_dir_step10_0, "QCOVID2_unvaccinated_hspt_female_variable_map.txt"),
                                 sep = "\t", header = T, quote = "", fill = T)
qcovid_summ_male <- read.table(paste0(temp_data_dir_step10_0, "QCOVID2_unvaccinated_hspt_male_variable_map.txt"),
                               sep = "\t", header = T, quote = "", fill = T)

if (calib_train == 1) {  ## Only run this once
#### Create combined dataframe for training ####
##### Get predicted CRS risk in training data #####

train_df_fit_crs <- CRS_calc(compare_train_df_crs,
                           crs_summ)  

##### Get predicted QCovid risk in training data #####
target_ds_pred_qcovid_train <- qcovid_calc(compare_train_df_qcovid, 
                        qcovid_summ_female,
                        qcovid_summ_male,
                        survivor_idx = 6)

##### Create combined dataset #####
train_df_fit <- cbind(compare_train_df_crs[, c("age_at_2020", "Female", 
                                               "target_ds_status", "ipcw_wts")],
                      train_df_fit_crs[, c("CRS_risk_no_covar",
                                               "CRS_risk_with_covar")],
                      target_ds_pred_qcovid_train)

colnames(train_df_fit)[7] <- "QCOVID_score"

#### Calibrate QCovid and train combined model in training data ####
i <- 74
target_ds_incid <- outcome_incid_vec[i]

##### Get regression formula #####
reg_formula_qcovid <- paste0('target_ds_status ~ QCOVID_score') %>% 
  as.formula()
reg_formula_crs_qcovid <- paste0('target_ds_status ~ CRS_risk_no_covar + QCOVID_score') %>% 
  as.formula()
reg_formula_crs_all_pred_qcovid <- paste0('target_ds_status ~ CRS_risk_with_covar + QCOVID_score') %>% 
  as.formula()
reg_formula_crs_qcovid_covar <- paste0('target_ds_status ~ CRS_risk_no_covar + QCOVID_score + age_at_2020 + Female') %>% 
  as.formula()


reg_formula_list <- list(reg_formula_qcovid,
                         reg_formula_crs_qcovid,
                         reg_formula_crs_all_pred_qcovid,
                         reg_formula_crs_qcovid_covar)

##### Loop through each model #####
## General info
Y <- train_df_fit$target_ds_status
mean_Y <- mean(Y)

## Create a list of summary statistics
target_ds_summ_list <- vector(mode = 'list', length = n_models_train)
names(target_ds_summ_list) <- model_train_name_vec

## Create a dataframe of R2
train_R2_dt <- data.frame("R2_obs" = rep(NA, n_models_train),
                          "R2_liab" = rep(NA, n_models_train))
rownames(train_R2_dt) <- model_train_name_vec

for (j in 1:n_models_train) {
  reg_formula <- reg_formula_list[[j]]
  model_name <- model_train_name_vec[j]
  
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

##### Save summary stats from regression #####
saveRDS(target_ds_summ_list, paste0(temp_data_dir, 'ForQCovidComparison_Outcome',
                                    target_ds, '_WeightedLogisticRegression_SummStats_ControlCase', 
                                    control_case_ratio,'to1_WhiteBritish_OddYOB_Index2020_Followup5yr.rds'))
saveRDS(train_R2_dt, 
        paste0(temp_data_dir, 'ForQCovidComparison_Outcome',
               target_ds, '_TrainData_ObservedAndLiabilityR2_OddYOB_Index2020_Followup5yr.rds'))

}

#### Create combined dataframe for testing ####
##### Get predicted CRS risk in testing data #####
## Copy the original dataset first
compare_test_df_crs_orig <- copy(compare_test_df_crs)
test_df_crs <- CRS_calc(compare_test_df_crs_orig,
                        crs_summ)  

##### Get predicted QCovid risk in testing data #####
target_ds_pred_qcovid_test <- qcovid_calc(compare_test_df_qcovid, 
                                           qcovid_summ_female,
                                           qcovid_summ_male,
                                           survivor_idx = 6)

##### Create combined dataset #####
test_df_final <- cbind(compare_test_df_crs[, c("PID", "age_at_2020", "Female", 
                                               "target_ds_status",
                                               "target_ds_tte")],
                       test_df_crs[, c("CRS_risk_no_covar",
                                       "CRS_risk_with_covar")],
                      target_ds_pred_qcovid_test)

colnames(test_df_final)[8] <- "QCOVID_score"


#### Perform comparison in testing data ####

##### Get coef summary stats for target models #####
qcovid_summ_list <- readRDS(paste0(temp_data_dir, 'ForQCovidComparison_Outcome',
                                    target_ds, '_WeightedLogisticRegression_SummStats_ControlCase', 
                                    control_case_ratio,'to1_WhiteBritish_OddYOB_Index2020_Followup5yr.rds'))

final_summ_list <- list(crs_summ_list$CRS_all_predictors,
                        qcovid_summ_list$QCovid,
                        qcovid_summ_list$CRS_QCovid,
                        qcovid_summ_list$CRS_all_predictors_QCovid,
                        qcovid_summ_list$CRS_QCovid_Age_Sex,
                        crs_summ_list$Age_Sex)
names(final_summ_list) <- model_test_name_vec

##### Set up parameters #####
i <- 74
target_ds_incid <- outcome_incid_vec[i]

## Set case-control ratio
n_cases <- sum(test_df_final$target_ds_status == 1)
n_controls <- sum(test_df_final$target_ds_status == 0)
case_pts_idx <- which(test_df_final$target_ds_status == 1)
control_pts_idx <- which(test_df_final$target_ds_status == 0)

n_cases_select1 <- min(n_cases_test1, n_cases)
n_controls_select1 <- n_cases_select1 * control_case_ratio_test

n_cases_select2 <- min(n_cases_test2, n_cases)
control_case_ratio_orig <- (1 - target_ds_incid)/target_ds_incid
n_controls_select2 <- round(n_cases_select2 * control_case_ratio_orig)

## Create a list of model-specific performance metrics
target_ds_metric_list <- vector(mode = 'list', length = n_models_test)
names(target_ds_metric_list) <- model_test_name_vec
for (j in 1:n_models_test) {
  metrics_dt <- data.frame("R2_obs" = rep(NA, n_mc),
                           "R2_liab" = rep(NA, n_mc),
                           "AUROC" = rep(NA, n_mc),
                           "C_index" = rep(NA, n_mc),
                           "AUPRC" = rep(NA, n_mc),
                           "AUPRC_prev_diff" = rep(NA, n_mc))
  target_ds_metric_list[[j]] <- metrics_dt
}

##### Monte Carlo sampling of testing data #####
for (k in 1:n_mc) {
  ## Testing data 1: Subsample to case:control = 1:4 for assessment of AUROC, C-index, R^2 
  set.seed(2026 + i + k)
  case_subsample_idx1 <- sample(case_pts_idx, n_cases_select1)
  control_subsample_idx1 <- sample(control_pts_idx, n_controls_select1)
  
  subsample_idx1 <- c(case_subsample_idx1, control_subsample_idx1)
  
  test_dt1 <- test_df_final[subsample_idx1, ]
  crs_test_dt1 <- compare_test_df_crs[subsample_idx1, ]
  
  ## Testing data 2: Subsample to a subset of all pts for assessment of AURPC, maintaining original case:control ratio 
  set.seed(2026 + i + k)
  case_subsample_idx2 <- sample(case_pts_idx, n_cases_select2)
  control_subsample_idx2 <- sample(control_pts_idx, n_controls_select2)
  
  subsample_idx2 <- c(case_subsample_idx2, control_subsample_idx2)
  
  test_dt2 <- test_df_final[subsample_idx2, ]
  crs_test_dt2 <- compare_test_df_crs[subsample_idx2, ]
  
  for (j in 1:n_models_test) {
      target_summ <- final_summ_list[[j]]
      if (j == 1) {  # need individual coefficients from CRS predictors
        metrics_k <- test_pred_models(crs_test_dt1, 
                                      crs_test_dt2,
                                      target_ds_incid,
                                      target_summ)
      } else {
        metrics_k <- test_pred_models(test_dt1, 
                                      test_dt2,
                                      target_ds_incid,
                                      target_summ)
      }
        
      target_ds_metric_list[[j]][k,] <- metrics_k
  }
  
}


saveRDS(target_ds_metric_list, paste0(temp_data_dir, 'CRS_vs_QCovid_Outcome',
                                      target_ds, '_TestData_PredictionMetrics_ControlCase',
                                      control_case_ratio, 'to1_WhiteBritish_EvenYOB_Index2020_Followup5yr.rds'))


