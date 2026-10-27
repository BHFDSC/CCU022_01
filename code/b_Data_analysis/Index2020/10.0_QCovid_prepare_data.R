### Author: Pearl Liu
### Prepare data for comparison with QCovid
### Odd DOB: training, Even DOB: testing
### Index: 2020-01-01, follow-up: 5 years


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
temp_data_dir_step2_3 <- '~/collab/CCU022/CCU022_CRS/Data/Step2.3/'
temp_data_dir_step2_4 <- '~/collab/CCU022/CCU022_CRS/Data/Step2.4/'
temp_data_dir_step3_0 <- '~/collab/CCU022/CCU022_CRS/Data/Step3.0/'
temp_data_dir <- '~/collab/CCU022/CCU022_CRS/Data/Step10.0/'
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
n_mc <- 200
## Control-case ratio in testing data for evaluating AUROC, C-index, R2
control_case_ratio_test <- 4
## Number of cases used in testing data
n_cases_test1 <- 3000
n_cases_test2 <- 1000
## Number of cores used in parallel computing
n_cores <- 10

#### Read processed data ####
##### List of predictors and outcomes #####
pred_ds_list_name = paste0("ccu022_01_Predictor_Diseases_Pheparent_list_by_PL_", file_date)
outcome_ds_list_name = paste0("ccu022_01_Outcome_Diseases_Phecode_list_by_PL_", file_date)

pred_ds_list = readRDS(paste0(temp_data_dir_step1_0, pred_ds_list_name))
outcome_ds_list = readRDS(paste0(temp_data_dir_step1_0, outcome_ds_list_name))

pred_ds_vec <- pred_ds_list %>% arrange(index) %>% pull(predictor_ds) 
outcome_ds_vec <- outcome_ds_list %>% arrange(index) %>% pull(target_ds)

n_pred_ds <- length(pred_ds_vec)
n_outcome_ds <- length(outcome_ds_vec)

##### Predictor prevalence #####
pred_ds_prev <- readRDS(paste0(temp_data_dir_step1_1, 
                               'WhiteBritish_OddYOB_Index2020_PredictorDiseasePrevalence.rds'))

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

#### Set up training data for calibrating QCovid - only include pts with available BMI ####
target_ds <- "COVID_hspt"

##### Get full training data with PID #####
pred_df_pre_index_odd_name <- paste0('ccu022_01_WhiteBritish_OddYOB_Index2020_PredictorDiseaseStatus_by_PL_', file_date)
pred_df_pre_index_odd <- readRDS(paste0(temp_data_dir_step1_1, pred_df_pre_index_odd_name))
outcome_df_fup_odd_name <- paste0('ccu022_01_WhiteBritish_OddYOB_Index2020_Followup5yr_OutcomeDiseaseStatusFollowup_by_PL_', file_date)
outcome_df_fup_odd <- readRDS(paste0(temp_data_dir_step1_1, outcome_df_fup_odd_name))
outcome_df_tte_odd_name <- paste0('ccu022_01_WhiteBritish_OddYOB_Index2020_Followup5yr_TimeToOutcomeFollowup_by_PL_', file_date)
outcome_df_tte_odd <- readRDS(paste0(temp_data_dir_step1_1, outcome_df_tte_odd_name))
covar_df_odd_name <- paste0('ccu022_01_WhiteBritish_OddYOB_Index2020_Covariates_by_PL_', file_date)
covar_df_odd <- readRDS(paste0(temp_data_dir_step1_1, covar_df_odd_name))

study_pop_train_name <- paste0("Outcome", target_ds, "_study_population_RmPtsWithPriorOutcomeDiag_WhiteBritish_OddYOB_Index2020.rds")
study_pop_train <- readRDS(paste0(temp_data_dir_step2_1, study_pop_train_name))


pred_df_train <- pred_df_pre_index_odd[study_pop_idx_train,]
covar_df_train <- covar_df_odd[study_pop_idx_train,]
outcome_df_fup_train <- outcome_df_fup_odd[study_pop_idx_train,]
outcome_df_tte_train <- outcome_df_tte_odd[study_pop_idx_train,]

target_ds_parent <- target_ds

## Restrict to correct predictor data for training data
pred_df_train2 <- pred_df_train %>%
  .[, (target_ds_parent) := NULL]

## Change colnames
colnames(pred_df_train2)[-1] <- paste0('ds', colnames(pred_df_train2)[-1])

## Add covariates
pred_df_train2 <- pred_df_train2[, c('age_at_2020', 'Female') := covar_df_train[,c('age_at_2020', 'Female')]]

### Training data: Combine predictors and outcomes, removed censored pts 
target_ds_fup_vec <- outcome_df_fup_train[, ..target_ds]
target_ds_tte_vec <- outcome_df_tte_train[, ..target_ds]

## Get indices for non-censored pts
non_censor_idx <- which(target_ds_fup_vec != 9)

target_ds_train <- cbind(target_ds_fup_vec, target_ds_tte_vec,
                         covar_df_train[,"age_at_2020"]) 
colnames(target_ds_train) <- c('target_ds_status', 'days_to_ds', 'age_at_index')

target_ds_train <- target_ds_train %>%
  .[, yrs_to_ds := floor(days_to_ds/365.25)] %>%
  .[, age_at_index_floor := floor(age_at_index)] %>%
  .[, c("days_to_ds", "age_at_index") := NULL] 

## combine outcome data with predictor data
train_df_combine <- pred_df_train2[, colnames(target_ds_train) := target_ds_train]

## Limit to non-censored pts
train_df_combine2 <- train_df_combine[non_censor_idx, ]


### Training data: Assign IPCW weights to training pts 
target_ds_ipcw_wts <- readRDS(paste0(temp_data_dir_step2_2, 'Outcome',
                                     target_ds, '_IPCW_WhiteBritish_OddYOB_Index2020_Followup5yr.rds'))


train_df_combine3 <- train_df_combine2 %>%
  ## match to IPCW weights based on age at index and year to outcome ds
  merge(target_ds_ipcw_wts, 
        by.x = c("yrs_to_ds", "age_at_index_floor"),
        by.y = c("yr_of_fup", "age_at_index"),
        all.x = T) %>%
  .[, c("target_ds", "yrs_to_ds", "age_at_index_floor") := NULL]


saveRDS(train_df_combine3, paste0(temp_data_dir, 'Outcome',
                                  target_ds, '_TrainDataWithPID_AllPts_WhiteBritish_OddYOB_Index2020_Followup5yr.rds'))

##### Read full training data with PID #####
train_df_combine <- readRDS(paste0(temp_data_dir, 'Outcome',
                                   target_ds, '_TrainDataWithPID_AllPts_WhiteBritish_OddYOB_Index2020_Followup5yr.rds'))

colnames(train_df_combine)[1] <- "PID"

##### Read QCovid covariates data - Odd YOB #####
qcovid_cov_BMI_avail_odd_name <- "ccu022_crs_Qcovid_DiagFlags_BMIAvailable_WhiteBritish_OddYOB_Index2020"
qcovid_cov_BMI_avail_odd_diag <- readRDS(paste0(temp_data_dir_step1_1, 
                                                 qcovid_cov_BMI_avail_odd_name))

##### Get overlapping pts #####
calib_train_cohort <- intersect(qcovid_cov_BMI_avail_odd_diag$PID,
                            train_df_combine$PID) %>% sort()

crs_cohort_idx <- which(train_df_combine$PID %in% calib_train_cohort)
qcovid_cohort_idx <- which(qcovid_cov_BMI_avail_odd_diag$PID %in% calib_train_cohort)

calib_train_cohort_crs <- train_df_combine[crs_cohort_idx,] %>%
  .[order(PID)]
# all.equal(calib_train_cohort_crs$PID, calib_train_cohort)
calib_train_cohort_qcovid <- qcovid_cov_BMI_avail_odd_diag[qcovid_cohort_idx,] %>%
  .[order(PID)] %>%
  .[, Female := calib_train_cohort_crs$Female]
# all.equal(calib_train_cohort_crs$PID, calib_train_cohort_qcovid$PID)

##### Use 4:1 control-case sampling #####
n_cases <- sum(calib_train_cohort_crs$target_ds_status == 1)
n_controls <- sum(calib_train_cohort_crs$target_ds_status == 0)
case_pts_idx <- which(calib_train_cohort_crs$target_ds_status == 1)
control_pts_idx <- which(calib_train_cohort_crs$target_ds_status == 0)

n_controls_fit <- min(control_case_ratio * n_cases, n_controls)

set.seed(2025 + 74 + 1)  # See Code 2.4 for seed
control_subsample_idx <- sample(control_pts_idx, n_controls_fit)
subsample_fit_idx <- c(case_pts_idx, control_subsample_idx)

calib_train_cohort_crs_sub <- calib_train_cohort_crs[subsample_fit_idx,]
calib_train_cohort_qcovid_sub <- calib_train_cohort_qcovid[subsample_fit_idx,]

##### Save data #####
saveRDS(calib_train_cohort_crs_sub, paste0(temp_data_dir, 'ForComparison_Outcome',
                                    target_ds, '_CRS_TrainData_WhiteBritish_OddYOB_Index2020_Followup5yr.rds'))
saveRDS(calib_train_cohort_qcovid_sub, paste0(temp_data_dir, 'ForComparison_Outcome',
                                       target_ds, '_QCovid_TrainData_WhiteBritish_OddYOB_Index2020_Followup5yr.rds'))

#all.equal(calib_train_cohort_crs_sub$PID, calib_train_cohort_qcovid_sub$PID)

#### Set up testing data for comparison with QCovid - only include pts with available BMI ####
target_ds <- "COVID_hspt"

##### Read full testing data #####
test_df_combine <- readRDS(paste0(temp_data_dir_step2_3, 'Outcome',
                                  target_ds, '_TestData_AllPts_WhiteBritish_EvenYOB_Index2020_Followup5yr.rds'))
## Add PID (see code 2.3)
pred_df_pre_index_even_name <- paste0('ccu022_01_WhiteBritish_EvenYOB_Index2020_PredictorDiseaseStatus_by_PL_', file_date)
pred_df_pre_index_even <- readRDS(paste0(temp_data_dir_step1_1, pred_df_pre_index_even_name))
study_pop_test_name <- paste0("Outcome", target_ds, "_study_population_RmPtsWithPriorOutcomeDiag_WhiteBritish_EvenYOB_Index2020.rds")
study_pop_test <- readRDS(paste0(temp_data_dir_step2_1, study_pop_test_name))

study_pop_idx_test <- study_pop_test$study_pop_idx
pred_df_test <- pred_df_pre_index_even[study_pop_idx_test,]
rm(study_pop_test)
#all.equal(pred_df_test$`112`, test_df_combine$ds112)
test_df_combine[, PID := pred_df_test$PID]


##### Read QCovid covariates data - Even YOB #####
qcovid_cov_BMI_avail_even_name <- "ccu022_crs_Qcovid_DiagFlags_BMIAvailable_WhiteBritish_EvenYOB_Index2020"
qcovid_cov_BMI_avail_even_diag <- readRDS(paste0(temp_data_dir_step1_1, 
                                                 qcovid_cov_BMI_avail_even_name))


##### Get overlapping pts #####
compare_cohort <- intersect(qcovid_cov_BMI_avail_even_diag$PID,
                           test_df_combine$PID) %>% sort()

crs_cohort_idx <- which(test_df_combine$PID %in% compare_cohort)
qcovid_cohort_idx <- which(qcovid_cov_BMI_avail_even_diag$PID %in% compare_cohort)

compare_test_df_crs <- test_df_combine[crs_cohort_idx,] %>%
  .[order(PID)]
# all.equal(compare_test_df_crs$PID, compare_cohort)
compare_test_df_qcovid <- qcovid_cov_BMI_avail_even_diag[qcovid_cohort_idx,] %>%
  .[order(PID)] %>%
  .[, Female := compare_test_df_crs$Female]
# all.equal(compare_test_df_crs$PID, compare_test_df_qcovid$PID)

##### Save data #####
saveRDS(compare_test_df_crs, paste0(temp_data_dir, 'ForComparison_Outcome',
                                    target_ds, '_CRS_TestData_WhiteBritish_EvenYOB_Index2020_Followup5yr.rds'))
saveRDS(compare_test_df_qcovid, paste0(temp_data_dir, 'ForComparison_Outcome',
                                    target_ds, '_QCovid_TestData_WhiteBritish_EvenYOB_Index2020_Followup5yr.rds'))

#### Check consistency between CRS and QCovid phenotypes ####
### Read test data
compare_test_df_crs <- readRDS(paste0(temp_data_dir, 'ForComparison_Outcome',
                                      target_ds, '_CRS_TestData_WhiteBritish_EvenYOB_Index2020_Followup5yr.rds'))
compare_test_df_qcovid <- readRDS(paste0(temp_data_dir, 'ForComparison_Outcome',
                                         target_ds, '_QCovid_TestData_WhiteBritish_EvenYOB_Index2020_Followup5yr.rds'))

## diabetes
cor(compare_test_df_crs$ds250, compare_test_df_qcovid$qcovid_Diabetes_flag)
# [1] 0.9963348

## ischemic heart disease/CHD
cor(compare_test_df_crs$ds411, compare_test_df_qcovid$qcovid_CoronaryHeartDisease_flag)
# [1] 0.990906

#### Perform initial comparison of CRS and QCovid risk ####
### Read test data
compare_test_df_crs <- readRDS(paste0(temp_data_dir, 'ForComparison_Outcome',
                                    target_ds, '_CRS_TestData_WhiteBritish_EvenYOB_Index2020_Followup5yr.rds'))
compare_test_df_qcovid <- readRDS(paste0(temp_data_dir, 'ForComparison_Outcome',
                                       target_ds, '_QCovid_TestData_WhiteBritish_EvenYOB_Index2020_Followup5yr.rds'))

### Read trained CRS effect sizes
crs_summ_list <- readRDS(paste0(temp_data_dir_step3_0, 'Outcome',
                                      target_ds, '_WeightedLogisticRegression_SummStats_ControlCase', 
                                      control_case_ratio,'to1_WhiteBritish_OddYOB_Index2020_Followup5yr.rds'))
crs_summ <- crs_summ_list$CRS_all_predictors


### Read QCovid effect sizes
qcovid_summ_female <- read.table(paste0(temp_data_dir, "QCOVID2_unvaccinated_hspt_female_variable_map.txt"),
                                 sep = "\t", header = T, quote = "", fill = T)
qcovid_summ_male <- read.table(paste0(temp_data_dir, "QCOVID2_unvaccinated_hspt_male_variable_map.txt"),
                                 sep = "\t", header = T, quote = "", fill = T)


##### Get predicted CRS risk #####
## Get regression coefficient estimate
train_coef_est <- crs_summ$Est

## Get variable names
var_names <- rownames(crs_summ)[-1]

## Get testing data for listed variable names
compare_test_df_crs_analysis <- compare_test_df_crs[, ..var_names]

## Obtain predicted target disease probability based on the training logistic model
link_pred <- as.matrix(compare_test_df_crs_analysis) %*% train_coef_est[-1] + train_coef_est[1]
exp_beta <- exp(-link_pred)
target_ds_pred_crs <- 1/(1+exp_beta)


##### Get predicted QCovid risk #####
#qcovid_calc <- function(compare_test_df_qcovid, 
#                        qcovid_summ_female,
#                        qcovid_summ_male,
#                        survivor_idx = 6) {
  compare_test_df_female <- compare_test_df_qcovid[Female == 1]
  compare_test_df_male <- compare_test_df_qcovid[Female == 0]
  
  ### Get age and bmi spline terms
  age_bmi_adj_f <- qcovid_summ_female %>%
    filter(Label == "Adjust") %>%
    select(Beta) %>%
    pull() # Order: mage_1, mage_2, mbmi_1, mbmi_2
  
  age_bmi_adj_m <- qcovid_summ_male %>%
    filter(Label == "Adjust") %>%
    select(Beta) %>%
    pull() # Order: mage_1, mage_2, mbmi_1, mbmi_2
  
  ## See original QCOVID function for transformation
  compare_test_df_female <- compare_test_df_female %>%
    .[, cage_1 := (age_at_2020/10)^2 - age_bmi_adj_f[1]] %>%
    .[, cage_2 := (age_at_2020/10)^2 * log(age_at_2020/10) - age_bmi_adj_f[2] ] %>%
    .[, cbmi_1 := (BMI_value/10)^(-2) - age_bmi_adj_f[3]] %>%
    .[, cbmi_2 := (BMI_value/10)^(-1) - age_bmi_adj_f[4]] %>%
    .[, cage_1_bmi_1 := cage_1 * cbmi_1] %>%
    .[, cage_1_bmi_2 := cage_1 * cbmi_2] %>%
    .[, cage_2_bmi_1 := cage_2 * cbmi_1] %>%
    .[, cage_2_bmi_2 := cage_2 * cbmi_2]
  
  compare_test_df_male <- compare_test_df_male %>%
    .[, cage_1 := (age_at_2020/10)^(-2) - age_bmi_adj_m[1]] %>%
    .[, cage_2 := (age_at_2020/10)^3 - age_bmi_adj_m[2]] %>%
    .[, cbmi_1 := (BMI_value/10)^(-2) - age_bmi_adj_m[3]] %>%
    .[, cbmi_2 := (BMI_value/10)^(-2) * log(BMI_value/10) - age_bmi_adj_m[4]] %>%
    .[, cage_1_bmi_1 := cage_1 * cbmi_1] %>%
    .[, cage_1_bmi_2 := cage_1 * cbmi_2] %>%
    .[, cage_2_bmi_1 := cage_2 * cbmi_1] %>%
    .[, cage_2_bmi_2 := cage_2 * cbmi_2]
  
  ## Get regression coefficient estimate
  qcovid_coef_female <- qcovid_summ_female %>%
    filter(Label == "Coef")
  qcovid_coef_male <- qcovid_summ_male %>%
    filter(Label == "Coef")
  
  qcovid_coef_female_est <- qcovid_coef_female$Beta
  qcovid_coef_male_est <- qcovid_coef_male$Beta
  
  ## Get variable names
  qcovid_var_names <- qcovid_coef_female$Name
  
  ## Get testing data for listed variable names
  compare_test_df_female_analysis <- compare_test_df_female[, ..qcovid_var_names]
  compare_test_df_male_analysis <- compare_test_df_male[, ..qcovid_var_names]
  
  ## Obtain predicted target disease probability based on QCOVID model
  link_pred_qcovid_female <- as.matrix(compare_test_df_female_analysis) %*% qcovid_coef_female_est
  link_pred_qcovid_male <- as.matrix(compare_test_df_male_analysis) %*% qcovid_coef_male_est
  
  bl_survivor_female <- qcovid_summ_female %>%
    filter(Label == "Survivor_func") %>%
    select(Beta) %>%
    pull()
  bl_survivor_female_t <- bl_survivor_female[survivor_idx]
  
  bl_survivor_male <- qcovid_summ_male %>%
    filter(Label == "Survivor_func") %>%
    select(Beta) %>%
    pull()
  bl_survivor_male_t <- bl_survivor_male[survivor_idx]
  
  target_ds_pred_qcovid_female <- 1 - bl_survivor_female_t^(exp(link_pred_qcovid_female))
  target_ds_pred_qcovid_male <- 1 - bl_survivor_male_t^(exp(link_pred_qcovid_male))
  
  target_ds_pred_qcovid_female_dt <- data.table(PID = compare_test_df_female$PID,
                                                Qcovid_risk = target_ds_pred_qcovid_female)
  target_ds_pred_qcovid_male_dt <- data.table(PID = compare_test_df_male$PID,
                                              Qcovid_risk = target_ds_pred_qcovid_male)
  
  target_ds_pred_qcovid_dt <- rbindlist(list(target_ds_pred_qcovid_female_dt,
                                             target_ds_pred_qcovid_male_dt)) %>%
    .[order(PID)]
  
  target_ds_pred_qcovid <- target_ds_pred_qcovid_dt$Qcovid_risk
  
#  return(target_ds_pred_qcovid)
  
#}


##### Compare #####
cor(target_ds_pred_crs, target_ds_pred_qcovid)
# 0.4638133

#plot(target_ds_pred_crs, target_ds_pred_qcovid)
#abline(0,1)

auroc_crs <- auc(compare_test_df_crs$target_ds_status ~ target_ds_pred_crs)[[1]]
auroc_qcovid <- auc(compare_test_df_crs$target_ds_status ~ target_ds_pred_qcovid)[[1]]

auroc_crs
# [1] 0.7550967

auroc_qcovid
# [1] 0.7371056





