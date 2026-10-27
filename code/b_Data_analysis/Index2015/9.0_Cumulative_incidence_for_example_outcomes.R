### Author: Pearl Liu
### Calculate cumulative incidence by predicted risk groups for example outcomes in testing data
### Odd DOB: training, Even DOB: testing
### Index: 2015-01-01, follow-up: 10 years


rm(list = ls())
gc()

#### Load libraries ####
library(data.table)
library(tidyverse)
library(survival)
library(cmprsk)

source('~/collab/CCU022/CCU022_CRS/0.1_Functions.R')

#### Set parameters ####
temp_data_dir_step1_0 <- '~/collab/CCU022/CCU022_CRS/Data/Step1.0/'
temp_data_dir_step1_1 <- '~/collab/CCU022/CCU022_CRS/Data/Step1.1/'
temp_data_dir_step2_3 <- '~/collab/CCU022/CCU022_CRS/Data/Step2.3/'
temp_data_dir_step2_4 <- '~/collab/CCU022/CCU022_CRS/Data/Step2.4/'
temp_data_dir_step3_0 <- '~/collab/CCU022/CCU022_CRS/Data/Step3.0/'
temp_data_dir <- '~/collab/CCU022/CCU022_CRS/Data/Step9.0/'
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
## Control-case ratio in testing data for evaluating AUROC, C-index, R2
control_case_ratio_test <- 4
## Number of cases used in testing data
n_cases_test1 <- 3000
n_cases_test2 <- 1000

#### Read processed data ####
##### List of predictors and outcomes - include COVID #####
pred_ds_list_name = paste0("ccu022_01_Predictor_Diseases_Pheparent_list_by_PL_", file_date)
outcome_ds_list_name = paste0("ccu022_01_Outcome_Diseases_Phecode_list_by_PL_", file_date)
outcome_ds_list_name2 = paste0("ccu022_01_Outcome_Diseases_Phecode_list2_TopCommonPlusPheRS_by_PL_", file_date)

pred_ds_list = readRDS(paste0(temp_data_dir_step1_0, pred_ds_list_name))
outcome_ds_list = readRDS(paste0(temp_data_dir_step1_0, outcome_ds_list_name))
outcome_ds_list2 = readRDS(paste0(temp_data_dir_step1_0, outcome_ds_list_name2))

pred_ds_vec <- pred_ds_list %>% arrange(index) %>% pull(predictor_ds) 
outcome_ds_vec <- c(outcome_ds_list %>% arrange(index) %>% pull(target_ds),
                    "COVID_hspt",
                    outcome_ds_list2 %>% arrange(index) %>% pull(target_ds),
                    "death")


n_pred_ds <- length(pred_ds_vec)
n_outcome_ds <- length(outcome_ds_vec)

##### Predictor prevalence #####
pred_ds_prev <- readRDS(paste0(temp_data_dir_step1_1, 
                               'WhiteBritish_OddYOB_Index2015_PredictorDiseasePrevalence.rds'))

##### Outcome incidence #####
outcome_incid_vec1 <- readRDS(paste0(temp_data_dir_step2_4, 
                                     'TrainData_Outcome_Incidence_OddYOB_Index2015_Followup10yr.rds'))
outcome_incid_vec2 <- readRDS(paste0(temp_data_dir_step2_4, 
                                     'TrainData_Outcome_Incidence_OddYOB_Index2015_Followup10yr_OutcomeList2.rds'))

outcome_incid_vec <- c(outcome_incid_vec1, outcome_incid_vec2)

outcome_incid_vec_sf1 <- readRDS(paste0(temp_data_dir_step2_4, 
                                        'TrainData_Outcome_Incidence_Sex_Specific_OddYOB_Index2015_Followup10yr.rds'))

outcome_incid_vec_sf <- c(outcome_incid_vec_sf1, rep(NA, length(outcome_incid_vec2)))


##### Phecode information #####
phe_icd10_codelist_name = paste0("ccu022_01_PheCode_ICD10AtoN_map_by_PL_", file_date)
icd10_phecode_map <- readRDS(paste0(temp_data_dir_step1_0, 
                                    phe_icd10_codelist_name))
phecode_info <- icd10_phecode_map %>%
  select(PheCode, Phenotype, Category, Sex) %>%
  distinct()


#### Survival analysis functions ####
surv_summ_get_CI_for_risk_fold2 <- function(target_ds = "411_2",
                               control_case_ratio = 4,
                               risk_q1 = 0.05,
                               risk_q2 = 0.95,
                               strata_col = "target_ds_pred_group",
                               strata_value = "Top 5%",
                               model_type = "CRS_all_predictors"
) {
  
  if (target_ds %in% c("COVID_hspt", "death")) {
    target_ds_sex <- "Both"
  } else {
    target_ds_sex <- phecode_info %>%
      filter(PheCode == str_replace(target_ds, '_', '\\.')) %>%
      pull(Sex)
  }
  
  ### Read trained effect sizes
  if (target_ds == "COVID_hspt") {
    target_ds_summ_list <- readRDS(paste0(temp_data_dir_step3_0, 'Outcome',
                                          target_ds, '_WeightedLogisticRegression_SummStats_ControlCase', 
                                          control_case_ratio,'to1_WhiteBritish_OddYOB_Index2020_Followup5yr.rds'))
    
  } else {
    if (target_ds_sex %in% c("Male", "Female")) {
      target_ds_summ_list <- readRDS(paste0(temp_data_dir_step3_0, 'Outcome',
                                            target_ds, '_Sex_Specific_WeightedLogisticRegression_SummStats_ControlCase', 
                                            control_case_ratio,'to1_WhiteBritish_OddYOB_Index2015_Followup10yr.rds'))
      
    } else {
      target_ds_summ_list <- readRDS(paste0(temp_data_dir_step3_0, 'Outcome',
                                            target_ds, '_WeightedLogisticRegression_SummStats_ControlCase', 
                                            control_case_ratio,'to1_WhiteBritish_OddYOB_Index2015_Followup10yr.rds'))
    }
  }
  
  if (model_type == "CRS_all_predictors") {
    target_summ <- target_ds_summ_list$CRS_all_predictors
  } else if (model_type == "Age_Sex") {
    target_summ <- target_ds_summ_list$Age_Sex
  }
  
  ### Read full testing data
  if (target_ds == "COVID_hspt") {
    test_df_combine <- readRDS(paste0(temp_data_dir_step2_3, 'Outcome',
                                      target_ds, '_TestData_AllPts_WhiteBritish_EvenYOB_Index2020_Followup5yr.rds'))
  } else {
    test_df_combine <- readRDS(paste0(temp_data_dir_step2_3, 'Outcome',
                                      target_ds, '_TestData_AllPts_WhiteBritish_EvenYOB_Index2015_Followup10yr.rds'))
    if (target_ds_sex == "Female") {
      test_df_combine <- test_df_combine[Female == 1]
    } else if (target_ds_sex == "Male") {
      test_df_combine <- test_df_combine[Female == 0]
    }
  }
  
  ### Calculate predicted risks for testing samples
  ## Get regression coefficient estimate
  train_coef_est <- target_summ$Est
  
  ## Get variable names
  var_names <- rownames(target_summ)[-1]
  
  ## Get testing data for listed variable names
  test_dt_analysis <- test_df_combine[, ..var_names]
  
  ## Obtain predicted target disease probability based on the training logistic model
  link_pred <- as.matrix(test_dt_analysis) %*% train_coef_est[-1] + train_coef_est[1]
  
  exp_beta <- exp(-link_pred)
  target_ds_pred <- 1/(1+exp_beta)
  target_ds_pred_q <- quantile(target_ds_pred, c(risk_q1, risk_q2))
  
  test_dt_cuminc_analysis <- test_df_combine[, c("target_ds_status",
                                                 "target_ds_tte")] %>%
    .[, "target_ds_pred_risk" := target_ds_pred] %>%
    .[, "target_ds_pred_group" := case_when(target_ds_pred_risk <= target_ds_pred_q[1] ~ "Bottom 5%",
                                            target_ds_pred_risk > target_ds_pred_q[1] & 
                                              target_ds_pred_risk <= target_ds_pred_q[2] ~ "Middle 90%",
                                            TRUE ~ "Top 5%")] %>%
    as.data.frame()
  
  ### Compute cumulative incidence
  #  target_ds_cuminc <- cuminc(ftime = test_dt_cuminc_analysis$target_ds_tte,
  #                             fstatus = test_dt_cuminc_analysis$target_ds_status,
  #                             group = test_dt_cuminc_analysis$target_ds_pred_group,
  #                             cencode = 0)
  
  #  ggcompetingrisks(target_ds_cuminc, 
  #                   multiple_panels = F,
  #                   conf.int = T)
  
  
  ### Compute cumulative incidence via Kaplan-Meier analysis
  target_ds_km1 <- survfit(Surv(target_ds_tte, target_ds_status) ~ target_ds_pred_group, 
                          data = test_dt_cuminc_analysis)
  target_ds_km2 <- survfit(Surv(target_ds_tte, target_ds_status) ~ 1, 
                           data = test_dt_cuminc_analysis)
  
  t1 <- max(test_dt_cuminc_analysis$target_ds_tte)
  t2 <- test_dt_cuminc_analysis %>%
    filter(target_ds_pred_group == strata_value) %>%
    pull(target_ds_tte) %>%
    max()
  
  target_ds_km_summ1 <- summary(target_ds_km1,
                                    times = t1,
                                    extend = TRUE)
  target_ds_km_summ2 <- summary(target_ds_km2,
                                     times = t2,
                                     extend = TRUE)
  
  # identify target stratum
  target_label <- paste0(strata_col, "=", strata_value)
  
  idx <- which(target_ds_km_summ1$strata == target_label)
  
  S1 <- target_ds_km_summ1$surv[idx]
  S2 <- target_ds_km_summ2$surv
  
  # survival::survfit gives standard error of S(t)
  se_S1 <- target_ds_km_summ1$std.err[idx]
  se_S2 <- target_ds_km_summ2$std.err
  
  V1 <- se_S1^2
  V2 <- se_S2^2
  
  # cumulative risks
  F1 <- 1 - S1
  F2 <- 1 - S2
  
  risk_ratio <- F1 / F2
  log_rr <- log(risk_ratio)
  
  # assuming cov(S1, S2) = 0
  se_log_rr <- sqrt(
    V1 / F1^2 +
      V2 / F2^2
  )
  
  ci <- exp(log_rr + c(-1, 1) * qnorm(0.975) * se_log_rr)
  
  final_summ <- data.frame(
    t1 = t1,
    t2 = t2,
    S1 = S1,
    S2 = S2,
    risk1 = F1,
    risk2 = F2,
    risk_ratio = risk_ratio,
    se_log_risk_ratio = se_log_rr,
    CI_LB = ci[1],
    CI_UB = ci[2]
  )
  
  if (target_ds == "COVID_hspt") {
    if (model_type == "CRS_all_predictors") {
      saveRDS(final_summ,
              paste0(temp_data_dir, "Outcome", target_ds, 
                     "_RiskFoldSummaryV2_WithCI_WhiteBritish_EvenYOB_Index2020_Followup5yr.rds"))
      
    } else {
      saveRDS(final_summ,
              paste0(temp_data_dir, "Outcome", target_ds, "_", model_type,
                     "_RiskFoldSummaryV2_WithCI_WhiteBritish_EvenYOB_Index2020_Followup5yr.rds"))
    }
    
  } else {
  
    if (model_type == "CRS_all_predictors") {
      saveRDS(final_summ,
              paste0(temp_data_dir, "Outcome", target_ds, 
                     "_RiskFoldSummaryV2_WithCI_WhiteBritish_EvenYOB_Index2015_Followup10yr.rds"))
      
    } else {
      saveRDS(final_summ,
                paste0(temp_data_dir, "Outcome", target_ds, "_", model_type,
                       "_RiskFoldSummaryV2_WithCI_WhiteBritish_EvenYOB_Index2015_Followup10yr.rds"))
    }
    
  }
  
}

surv_summ_get_CI_for_risk_fold2_bootstrap <- function(target_ds = "411_2",
                                            control_case_ratio = 4,
                                            risk_q1 = 0.05,
                                            risk_q2 = 0.95,
                                            n_boot = 100,
                                            seed = 2026,
                                            strata_col = "target_ds_pred_group",
                                            strata_value = "Top 5%",
                                            model_type = "CRS_all_predictors"
) {
  
  if (target_ds %in% c("COVID_hspt", "death")) {
    target_ds_sex <- "Both"
  } else {
    target_ds_sex <- phecode_info %>%
      filter(PheCode == str_replace(target_ds, '_', '\\.')) %>%
      pull(Sex)
  }
  
  ### Read trained effect sizes
  if (target_ds == "COVID_hspt") {
    target_ds_summ_list <- readRDS(paste0(temp_data_dir_step3_0, 'Outcome',
                                          target_ds, '_WeightedLogisticRegression_SummStats_ControlCase', 
                                          control_case_ratio,'to1_WhiteBritish_OddYOB_Index2020_Followup5yr.rds'))
    
  } else {
    if (target_ds_sex %in% c("Male", "Female")) {
      target_ds_summ_list <- readRDS(paste0(temp_data_dir_step3_0, 'Outcome',
                                            target_ds, '_Sex_Specific_WeightedLogisticRegression_SummStats_ControlCase', 
                                            control_case_ratio,'to1_WhiteBritish_OddYOB_Index2015_Followup10yr.rds'))
      
    } else {
      target_ds_summ_list <- readRDS(paste0(temp_data_dir_step3_0, 'Outcome',
                                            target_ds, '_WeightedLogisticRegression_SummStats_ControlCase', 
                                            control_case_ratio,'to1_WhiteBritish_OddYOB_Index2015_Followup10yr.rds'))
    }
  }
  
  if (model_type == "CRS_all_predictors") {
    target_summ <- target_ds_summ_list$CRS_all_predictors
  } else if (model_type == "Age_Sex") {
    target_summ <- target_ds_summ_list$Age_Sex
  }
  
  ### Read full testing data
  if (target_ds == "COVID_hspt") {
    test_df_combine <- readRDS(paste0(temp_data_dir_step2_3, 'Outcome',
                                      target_ds, '_TestData_AllPts_WhiteBritish_EvenYOB_Index2020_Followup5yr.rds'))
  } else {
    test_df_combine <- readRDS(paste0(temp_data_dir_step2_3, 'Outcome',
                                      target_ds, '_TestData_AllPts_WhiteBritish_EvenYOB_Index2015_Followup10yr.rds'))
    if (target_ds_sex == "Female") {
      test_df_combine <- test_df_combine[Female == 1]
    } else if (target_ds_sex == "Male") {
      test_df_combine <- test_df_combine[Female == 0]
    }
  }
  
  ### Calculate predicted risks for testing samples
  ## Get regression coefficient estimate
  train_coef_est <- target_summ$Est
  
  ## Get variable names
  var_names <- rownames(target_summ)[-1]
  
  
  boot_summ <- data.frame(
    t1 = rep(NA, n_boot),
    t2 = rep(NA, n_boot),
    S1 = rep(NA, n_boot),
    S2 = rep(NA, n_boot),
    risk1 = rep(NA, n_boot),
    risk2 = rep(NA, n_boot),
    risk_ratio = rep(NA, n_boot),
    se_log_risk_ratio = rep(NA, n_boot),
    CI_LB = rep(NA, n_boot),
    CI_UB = rep(NA, n_boot)
  )
  
  set.seed(seed)
  for (j in 1:n_boot) {
    boot_idx <- sample(1:nrow(test_df_combine), replace = T)
    test_df_combine_j <- test_df_combine[boot_idx,]
    ## Get testing data for listed variable names
    test_dt_analysis <- test_df_combine_j[, ..var_names]
    
    ## Obtain predicted target disease probability based on the training logistic model
    link_pred <- as.matrix(test_dt_analysis) %*% train_coef_est[-1] + train_coef_est[1]
    
    exp_beta <- exp(-link_pred)
    target_ds_pred <- 1/(1+exp_beta)
    target_ds_pred_q <- quantile(target_ds_pred, c(risk_q1, risk_q2))
    
    test_dt_cuminc_analysis <- test_df_combine_j[, c("target_ds_status",
                                                   "target_ds_tte")] %>%
      .[, "target_ds_pred_risk" := target_ds_pred] %>%
      .[, "target_ds_pred_group" := case_when(target_ds_pred_risk <= target_ds_pred_q[1] ~ "Bottom 5%",
                                              target_ds_pred_risk > target_ds_pred_q[1] & 
                                                target_ds_pred_risk <= target_ds_pred_q[2] ~ "Middle 90%",
                                              TRUE ~ "Top 5%")] %>%
      as.data.frame()
    
    ### Compute cumulative incidence
    #  target_ds_cuminc <- cuminc(ftime = test_dt_cuminc_analysis$target_ds_tte,
    #                             fstatus = test_dt_cuminc_analysis$target_ds_status,
    #                             group = test_dt_cuminc_analysis$target_ds_pred_group,
    #                             cencode = 0)
    
    #  ggcompetingrisks(target_ds_cuminc, 
    #                   multiple_panels = F,
    #                   conf.int = T)
    
    
    ### Compute cumulative incidence via Kaplan-Meier analysis
    target_ds_km1 <- survfit(Surv(target_ds_tte, target_ds_status) ~ target_ds_pred_group, 
                             data = test_dt_cuminc_analysis)
    target_ds_km2 <- survfit(Surv(target_ds_tte, target_ds_status) ~ 1, 
                             data = test_dt_cuminc_analysis)
    
    t1 <- max(test_dt_cuminc_analysis$target_ds_tte)
    t2 <- test_dt_cuminc_analysis %>%
      filter(target_ds_pred_group == strata_value) %>%
      pull(target_ds_tte) %>%
      max()
    
    target_ds_km_summ1 <- summary(target_ds_km1,
                                  times = t1,
                                  extend = TRUE)
    target_ds_km_summ2 <- summary(target_ds_km2,
                                  times = t2,
                                  extend = TRUE)
    
    # identify target stratum
    target_label <- paste0(strata_col, "=", strata_value)
    
    idx <- which(target_ds_km_summ1$strata == target_label)
    
    S1 <- target_ds_km_summ1$surv[idx]
    S2 <- target_ds_km_summ2$surv
    
    # survival::survfit gives standard error of S(t)
    se_S1 <- target_ds_km_summ1$std.err[idx]
    se_S2 <- target_ds_km_summ2$std.err
    
    V1 <- se_S1^2
    V2 <- se_S2^2
    
    # cumulative risks
    F1 <- 1 - S1
    F2 <- 1 - S2
    
    risk_ratio <- F1 / F2
    log_rr <- log(risk_ratio)
    
    # assuming cov(S1, S2) = 0
    se_log_rr <- sqrt(
      V1 / F1^2 +
        V2 / F2^2
    )
    
    ci <- exp(log_rr + c(-1, 1) * qnorm(0.975) * se_log_rr)
    
    boot_summ[j,] <- c(
      t1,
      t2,
      S1,
      S2,
      F1,
      F2,
      risk_ratio,
      se_log_rr,
      ci[1],
      ci[2]
    )
    
  }
  
  if (target_ds == "COVID_hspt") {
    if (model_type == "CRS_all_predictors") {
      saveRDS(boot_summ,
              paste0(temp_data_dir, "Outcome", target_ds, 
                     "_RiskFoldSummaryV2_WithCI_Bootstrap_WhiteBritish_EvenYOB_Index2020_Followup5yr.rds"))
      
    } else {
      saveRDS(boot_summ,
              paste0(temp_data_dir, "Outcome", target_ds, "_", model_type,
                     "_RiskFoldSummaryV2_WithCI_Bootstrap_WhiteBritish_EvenYOB_Index2020_Followup5yr.rds"))
    }
    
  } else {
    
    if (model_type == "CRS_all_predictors") {
      saveRDS(final_summ,
              paste0(temp_data_dir, "Outcome", target_ds, 
                     "_RiskFoldSummaryV2_WithCI_Bootstrap_WhiteBritish_EvenYOB_Index2015_Followup10yr.rds"))
      
    } else {
      saveRDS(final_summ,
              paste0(temp_data_dir, "Outcome", target_ds, "_", model_type,
                     "_RiskFoldSummaryV2_WithCI_Bootstrap_WhiteBritish_EvenYOB_Index2015_Followup10yr.rds"))
    }
    
  }
  
}

## cumrisk for full population
surv_summ_full_pop <- function(target_ds = "411_2",
                              control_case_ratio = 4,
                              model_type = "CRS_all_predictors") {
  
  if (target_ds %in% c("COVID_hspt", "death")) {
    target_ds_sex <- "Both"
  } else {
    target_ds_sex <- phecode_info %>%
      filter(PheCode == str_replace(target_ds, '_', '\\.')) %>%
      pull(Sex)
  }
  
  ### Read trained effect sizes
  if (target_ds == "COVID_hspt") {
    target_ds_summ_list <- readRDS(paste0(temp_data_dir_step3_0, 'Outcome',
                                          target_ds, '_WeightedLogisticRegression_SummStats_ControlCase', 
                                          control_case_ratio,'to1_WhiteBritish_OddYOB_Index2020_Followup5yr.rds'))
    
  } else {
    if (target_ds_sex %in% c("Male", "Female")) {
      target_ds_summ_list <- readRDS(paste0(temp_data_dir_step3_0, 'Outcome',
                                            target_ds, '_Sex_Specific_WeightedLogisticRegression_SummStats_ControlCase', 
                                            control_case_ratio,'to1_WhiteBritish_OddYOB_Index2015_Followup10yr.rds'))
      
    } else {
      target_ds_summ_list <- readRDS(paste0(temp_data_dir_step3_0, 'Outcome',
                                            target_ds, '_WeightedLogisticRegression_SummStats_ControlCase', 
                                            control_case_ratio,'to1_WhiteBritish_OddYOB_Index2015_Followup10yr.rds'))
    }
  }
  
  if (model_type == "CRS_all_predictors") {
    target_summ <- target_ds_summ_list$CRS_all_predictors
  } else if (model_type == "Age_Sex") {
    target_summ <- target_ds_summ_list$Age_Sex
  }
  
  ### Read full testing data
  if (target_ds == "COVID_hspt") {
    test_df_combine <- readRDS(paste0(temp_data_dir_step2_3, 'Outcome',
                                      target_ds, '_TestData_AllPts_WhiteBritish_EvenYOB_Index2020_Followup5yr.rds'))
  } else {
    test_df_combine <- readRDS(paste0(temp_data_dir_step2_3, 'Outcome',
                                      target_ds, '_TestData_AllPts_WhiteBritish_EvenYOB_Index2015_Followup10yr.rds'))
    if (target_ds_sex == "Female") {
      test_df_combine <- test_df_combine[Female == 1]
    } else if (target_ds_sex == "Male") {
      test_df_combine <- test_df_combine[Female == 0]
    }
  }
  
  ### Calculate predicted risks for testing samples
  ## Get regression coefficient estimate
  train_coef_est <- target_summ$Est
  
  ## Get variable names
  var_names <- rownames(target_summ)[-1]
  
  ## Get testing data for listed variable names
  test_dt_analysis <- test_df_combine[, ..var_names]
  
  ## Obtain predicted target disease probability based on the training logistic model
  link_pred <- as.matrix(test_dt_analysis) %*% train_coef_est[-1] + train_coef_est[1]
  
  exp_beta <- exp(-link_pred)
  target_ds_pred <- 1/(1+exp_beta)
#  target_ds_pred_q <- quantile(target_ds_pred, c(risk_q1, risk_q2))
  
  test_dt_cuminc_analysis <- test_df_combine[, c("target_ds_status",
                                                 "target_ds_tte")] %>%
    .[, "target_ds_pred_risk" := target_ds_pred] %>%
 #   .[, "target_ds_pred_group" := case_when(target_ds_pred_risk <= target_ds_pred_q[1] ~ "Bottom 10%",
 #                                           target_ds_pred_risk > target_ds_pred_q[1] & 
 #                                           target_ds_pred_risk <= target_ds_pred_q[2] ~ "Middle 80%",
 #                                           TRUE ~ "Top 10%")] %>%
    as.data.frame()
  
  ### Compute cumulative incidence
  #  target_ds_cuminc <- cuminc(ftime = test_dt_cuminc_analysis$target_ds_tte,
  #                             fstatus = test_dt_cuminc_analysis$target_ds_status,
  #                             group = test_dt_cuminc_analysis$target_ds_pred_group,
  #                             cencode = 0)
  
  #  ggcompetingrisks(target_ds_cuminc, 
  #                   multiple_panels = F,
  #                   conf.int = T)
  
  
  ### Compute cumulative incidence via Kaplan-Meier analysis
  target_ds_km <- survfit(Surv(target_ds_tte, target_ds_status) ~ 1, 
                          data = test_dt_cuminc_analysis)
  target_ds_km_summ <- surv_summary(target_ds_km,
                                    data = test_dt_cuminc_analysis)
  
  if (target_ds == "COVID_hspt") {
    write.csv(target_ds_km_summ,
              paste0(temp_data_dir, "Outcome", target_ds, 
                     "_SurvivalAnalysisSummaryFullPop_WhiteBritish_EvenYOB_Index2020_Followup5yr.csv"),
              row.names = F)
  } else {
    write.csv(target_ds_km_summ,
              paste0(temp_data_dir, "Outcome", target_ds, 
                     "_SurvivalAnalysisSummaryFullPop_WhiteBritish_EvenYOB_Index2015_Followup10yr.csv"),
              row.names = F)
  }
}

## Cumrisk analysis by CRS risk groups
surv_summ_by_risk <- function(target_ds = "411_2",
                              control_case_ratio = 4,
                              risk_q1 = 0.1,
                              risk_q2 = 0.9,
                              model_type = "CRS_all_predictors"
                            ) {
  
  if (target_ds %in% c("COVID_hspt", "death")) {
    target_ds_sex <- "Both"
  } else {
    target_ds_sex <- phecode_info %>%
      filter(PheCode == str_replace(target_ds, '_', '\\.')) %>%
      pull(Sex)
  }
  
  ### Read trained effect sizes
  if (target_ds == "COVID_hspt") {
    target_ds_summ_list <- readRDS(paste0(temp_data_dir_step3_0, 'Outcome',
                                          target_ds, '_WeightedLogisticRegression_SummStats_ControlCase', 
                                          control_case_ratio,'to1_WhiteBritish_OddYOB_Index2020_Followup5yr.rds'))
    
  } else {
    if (target_ds_sex %in% c("Male", "Female")) {
      target_ds_summ_list <- readRDS(paste0(temp_data_dir_step3_0, 'Outcome',
                                            target_ds, '_Sex_Specific_WeightedLogisticRegression_SummStats_ControlCase', 
                                            control_case_ratio,'to1_WhiteBritish_OddYOB_Index2015_Followup10yr.rds'))
      
    } else {
      target_ds_summ_list <- readRDS(paste0(temp_data_dir_step3_0, 'Outcome',
                                            target_ds, '_WeightedLogisticRegression_SummStats_ControlCase', 
                                            control_case_ratio,'to1_WhiteBritish_OddYOB_Index2015_Followup10yr.rds'))
    }
  }
  
  if (model_type == "CRS_all_predictors") {
    target_summ <- target_ds_summ_list$CRS_all_predictors
  } else if (model_type == "Age_Sex") {
    target_summ <- target_ds_summ_list$Age_Sex
  }
  
  ### Read full testing data
  if (target_ds == "COVID_hspt") {
    test_df_combine <- readRDS(paste0(temp_data_dir_step2_3, 'Outcome',
                                      target_ds, '_TestData_AllPts_WhiteBritish_EvenYOB_Index2020_Followup5yr.rds'))
  } else {
    test_df_combine <- readRDS(paste0(temp_data_dir_step2_3, 'Outcome',
                                      target_ds, '_TestData_AllPts_WhiteBritish_EvenYOB_Index2015_Followup10yr.rds'))
    if (target_ds_sex == "Female") {
      test_df_combine <- test_df_combine[Female == 1]
    } else if (target_ds_sex == "Male") {
      test_df_combine <- test_df_combine[Female == 0]
    }
  }
  
  ### Calculate predicted risks for testing samples
  ## Get regression coefficient estimate
  train_coef_est <- target_summ$Est
  
  ## Get variable names
  var_names <- rownames(target_summ)[-1]
  
  ## Get testing data for listed variable names
  test_dt_analysis <- test_df_combine[, ..var_names]
  
  ## Obtain predicted target disease probability based on the training logistic model
  link_pred <- as.matrix(test_dt_analysis) %*% train_coef_est[-1] + train_coef_est[1]
  
  exp_beta <- exp(-link_pred)
  target_ds_pred <- 1/(1+exp_beta)
  target_ds_pred_q <- quantile(target_ds_pred, c(risk_q1, risk_q2))

  test_dt_cuminc_analysis <- test_df_combine[, c("target_ds_status",
                                                 "target_ds_tte")] %>%
    .[, "target_ds_pred_risk" := target_ds_pred] %>%
    .[, "target_ds_pred_group" := case_when(target_ds_pred_risk <= target_ds_pred_q[1] ~ "Bottom 10%",
                                            target_ds_pred_risk > target_ds_pred_q[1] & 
                                              target_ds_pred_risk <= target_ds_pred_q[2] ~ "Middle 80%",
                                            TRUE ~ "Top 10%")] %>%
    as.data.frame()
  
  ### Compute cumulative incidence
#  target_ds_cuminc <- cuminc(ftime = test_dt_cuminc_analysis$target_ds_tte,
#                             fstatus = test_dt_cuminc_analysis$target_ds_status,
#                             group = test_dt_cuminc_analysis$target_ds_pred_group,
#                             cencode = 0)
  
#  ggcompetingrisks(target_ds_cuminc, 
#                   multiple_panels = F,
#                   conf.int = T)
  
  
  ### Compute cumulative incidence via Kaplan-Meier analysis
  target_ds_km <- survfit(Surv(target_ds_tte, target_ds_status) ~ target_ds_pred_group, 
                          data = test_dt_cuminc_analysis)
  target_ds_km_summ <- surv_summary(target_ds_km,
                                    data = test_dt_cuminc_analysis)
  
  if (target_ds == "COVID_hspt") {
    write.csv(target_ds_km_summ,
              paste0(temp_data_dir, "Outcome", target_ds, 
                     "_SurvivalAnalysisSummary_WhiteBritish_EvenYOB_Index2020_Followup5yr.csv"),
              row.names = F)
  } else {
    write.csv(target_ds_km_summ,
              paste0(temp_data_dir, "Outcome", target_ds, 
                     "_SurvivalAnalysisSummary_WhiteBritish_EvenYOB_Index2015_Followup10yr.csv"),
              row.names = F)
  }
}

surv_summ_by_risk2 <- function(target_ds = "411_2",
                              control_case_ratio = 4,
                              risk_q1 = 0.05,
                              risk_q2 = 0.95,
                              model_type = "CRS_all_predictors"
) {
  
  if (target_ds %in% c("COVID_hspt", "death")) {
    target_ds_sex <- "Both"
  } else {
    target_ds_sex <- phecode_info %>%
      filter(PheCode == str_replace(target_ds, '_', '\\.')) %>%
      pull(Sex)
  }
  
  ### Read trained effect sizes
  if (target_ds == "COVID_hspt") {
    target_ds_summ_list <- readRDS(paste0(temp_data_dir_step3_0, 'Outcome',
                                          target_ds, '_WeightedLogisticRegression_SummStats_ControlCase', 
                                          control_case_ratio,'to1_WhiteBritish_OddYOB_Index2020_Followup5yr.rds'))
    
  } else {
    if (target_ds_sex %in% c("Male", "Female")) {
      target_ds_summ_list <- readRDS(paste0(temp_data_dir_step3_0, 'Outcome',
                                            target_ds, '_Sex_Specific_WeightedLogisticRegression_SummStats_ControlCase', 
                                            control_case_ratio,'to1_WhiteBritish_OddYOB_Index2015_Followup10yr.rds'))
      
    } else {
      target_ds_summ_list <- readRDS(paste0(temp_data_dir_step3_0, 'Outcome',
                                            target_ds, '_WeightedLogisticRegression_SummStats_ControlCase', 
                                            control_case_ratio,'to1_WhiteBritish_OddYOB_Index2015_Followup10yr.rds'))
    }
  }
  
  if (model_type == "CRS_all_predictors") {
    target_summ <- target_ds_summ_list$CRS_all_predictors
  } else if (model_type == "Age_Sex") {
    target_summ <- target_ds_summ_list$Age_Sex
  }
  
  ### Read full testing data
  if (target_ds == "COVID_hspt") {
    test_df_combine <- readRDS(paste0(temp_data_dir_step2_3, 'Outcome',
                                      target_ds, '_TestData_AllPts_WhiteBritish_EvenYOB_Index2020_Followup5yr.rds'))
  } else {
    test_df_combine <- readRDS(paste0(temp_data_dir_step2_3, 'Outcome',
                                      target_ds, '_TestData_AllPts_WhiteBritish_EvenYOB_Index2015_Followup10yr.rds'))
    if (target_ds_sex == "Female") {
      test_df_combine <- test_df_combine[Female == 1]
    } else if (target_ds_sex == "Male") {
      test_df_combine <- test_df_combine[Female == 0]
    }
  }  
  
  ### Calculate predicted risks for testing samples
  ## Get regression coefficient estimate
  train_coef_est <- target_summ$Est
  
  ## Get variable names
  var_names <- rownames(target_summ)[-1]
  
  ## Get testing data for listed variable names
  test_dt_analysis <- test_df_combine[, ..var_names]
  
  ## Obtain predicted target disease probability based on the training logistic model
  link_pred <- as.matrix(test_dt_analysis) %*% train_coef_est[-1] + train_coef_est[1]
  
  exp_beta <- exp(-link_pred)
  target_ds_pred <- 1/(1+exp_beta)
  target_ds_pred_q <- quantile(target_ds_pred, c(risk_q1, risk_q2))
  
  test_dt_cuminc_analysis <- test_df_combine[, c("target_ds_status",
                                                 "target_ds_tte")] %>%
    .[, "target_ds_pred_risk" := target_ds_pred] %>%
    .[, "target_ds_pred_group" := case_when(target_ds_pred_risk <= target_ds_pred_q[1] ~ "Bottom 5%",
                                            target_ds_pred_risk > target_ds_pred_q[1] & 
                                              target_ds_pred_risk <= target_ds_pred_q[2] ~ "Middle 90%",
                                            TRUE ~ "Top 5%")] %>%
    as.data.frame()
  
  ### Compute cumulative incidence
  #  target_ds_cuminc <- cuminc(ftime = test_dt_cuminc_analysis$target_ds_tte,
  #                             fstatus = test_dt_cuminc_analysis$target_ds_status,
  #                             group = test_dt_cuminc_analysis$target_ds_pred_group,
  #                             cencode = 0)
  
  #  ggcompetingrisks(target_ds_cuminc, 
  #                   multiple_panels = F,
  #                   conf.int = T)
  
  
  ### Compute cumulative incidence via Kaplan-Meier analysis
  target_ds_km <- survfit(Surv(target_ds_tte, target_ds_status) ~ target_ds_pred_group, 
                          data = test_dt_cuminc_analysis)
  target_ds_km_summ <- surv_summary(target_ds_km,
                                    data = test_dt_cuminc_analysis)
  
  if (target_ds == "COVID_hspt") {
    write.csv(target_ds_km_summ,
              paste0(temp_data_dir, "Outcome", target_ds, 
                     "_SurvivalAnalysisSummaryV2_WhiteBritish_EvenYOB_Index2020_Followup5yr.csv"),
              row.names = F)
  } else {
    write.csv(target_ds_km_summ,
              paste0(temp_data_dir, "Outcome", target_ds, 
                     "_SurvivalAnalysisSummaryV2_WhiteBritish_EvenYOB_Index2015_Followup10yr.csv"),
              row.names = F)
  }
}

surv_summ_by_risk3 <- function(target_ds = "411_2",
                               control_case_ratio = 4,
                               risk_q1 = 0.01,
                               risk_q2 = 0.99,
                               model_type = "CRS_all_predictors"
) {
  
  if (target_ds %in% c("COVID_hspt", "death")) {
    target_ds_sex <- "Both"
  } else {
    target_ds_sex <- phecode_info %>%
      filter(PheCode == str_replace(target_ds, '_', '\\.')) %>%
      pull(Sex)
  }
  
  ### Read trained effect sizes
  if (target_ds == "COVID_hspt") {
    target_ds_summ_list <- readRDS(paste0(temp_data_dir_step3_0, 'Outcome',
                                          target_ds, '_WeightedLogisticRegression_SummStats_ControlCase', 
                                          control_case_ratio,'to1_WhiteBritish_OddYOB_Index2020_Followup5yr.rds'))
    
  } else {
    if (target_ds_sex %in% c("Male", "Female")) {
      target_ds_summ_list <- readRDS(paste0(temp_data_dir_step3_0, 'Outcome',
                                            target_ds, '_Sex_Specific_WeightedLogisticRegression_SummStats_ControlCase', 
                                            control_case_ratio,'to1_WhiteBritish_OddYOB_Index2015_Followup10yr.rds'))
      
    } else {
      target_ds_summ_list <- readRDS(paste0(temp_data_dir_step3_0, 'Outcome',
                                            target_ds, '_WeightedLogisticRegression_SummStats_ControlCase', 
                                            control_case_ratio,'to1_WhiteBritish_OddYOB_Index2015_Followup10yr.rds'))
    }
  }
  
  if (model_type == "CRS_all_predictors") {
    target_summ <- target_ds_summ_list$CRS_all_predictors
  } else if (model_type == "Age_Sex") {
    target_summ <- target_ds_summ_list$Age_Sex
  }
  
  ### Read full testing data
  if (target_ds == "COVID_hspt") {
    test_df_combine <- readRDS(paste0(temp_data_dir_step2_3, 'Outcome',
                                      target_ds, '_TestData_AllPts_WhiteBritish_EvenYOB_Index2020_Followup5yr.rds'))
  } else {
    test_df_combine <- readRDS(paste0(temp_data_dir_step2_3, 'Outcome',
                                      target_ds, '_TestData_AllPts_WhiteBritish_EvenYOB_Index2015_Followup10yr.rds'))
    if (target_ds_sex == "Female") {
      test_df_combine <- test_df_combine[Female == 1]
    } else if (target_ds_sex == "Male") {
      test_df_combine <- test_df_combine[Female == 0]
    }
  }
  
  ### Calculate predicted risks for testing samples
  ## Get regression coefficient estimate
  train_coef_est <- target_summ$Est
  
  ## Get variable names
  var_names <- rownames(target_summ)[-1]
  
  ## Get testing data for listed variable names
  test_dt_analysis <- test_df_combine[, ..var_names]
  
  ## Obtain predicted target disease probability based on the training logistic model
  link_pred <- as.matrix(test_dt_analysis) %*% train_coef_est[-1] + train_coef_est[1]
  
  exp_beta <- exp(-link_pred)
  target_ds_pred <- 1/(1+exp_beta)
  target_ds_pred_q <- quantile(target_ds_pred, c(risk_q1, risk_q2))
  
  test_dt_cuminc_analysis <- test_df_combine[, c("target_ds_status",
                                                 "target_ds_tte")] %>%
    .[, "target_ds_pred_risk" := target_ds_pred] %>%
    .[, "target_ds_pred_group" := case_when(target_ds_pred_risk <= target_ds_pred_q[1] ~ "Bottom 1%",
                                            target_ds_pred_risk > target_ds_pred_q[1] & 
                                              target_ds_pred_risk <= target_ds_pred_q[2] ~ "Middle 98%",
                                            TRUE ~ "Top 1%")] %>%
    as.data.frame()
  
  ### Compute cumulative incidence
  #  target_ds_cuminc <- cuminc(ftime = test_dt_cuminc_analysis$target_ds_tte,
  #                             fstatus = test_dt_cuminc_analysis$target_ds_status,
  #                             group = test_dt_cuminc_analysis$target_ds_pred_group,
  #                             cencode = 0)
  
  #  ggcompetingrisks(target_ds_cuminc, 
  #                   multiple_panels = F,
  #                   conf.int = T)
  
  
  ### Compute cumulative incidence via Kaplan-Meier analysis
  target_ds_km <- survfit(Surv(target_ds_tte, target_ds_status) ~ target_ds_pred_group, 
                          data = test_dt_cuminc_analysis)
  target_ds_km_summ <- surv_summary(target_ds_km,
                                    data = test_dt_cuminc_analysis)
  
  if (target_ds == "COVID_hspt") {
    write.csv(target_ds_km_summ,
              paste0(temp_data_dir, "Outcome", target_ds, 
                     "_SurvivalAnalysisSummaryV3_WhiteBritish_EvenYOB_Index2020_Followup5yr.csv"),
              row.names = F)
  } else {
    write.csv(target_ds_km_summ,
              paste0(temp_data_dir, "Outcome", target_ds, 
                     "_SurvivalAnalysisSummaryV3_WhiteBritish_EvenYOB_Index2015_Followup10yr.csv"),
              row.names = F)
  }
}


## The following functions are copied from survminer package

surv_summary <- function (x, data = NULL){
  res <- as.data.frame(.compact(unclass(x)[c("time", "n.risk",
                                             "n.event", "n.censor")]))
  if (inherits(x, "survfitms")) {
    surv <- 1 - x$prev
    upper <- 1 - x$upper
    lower <- 1 - x$lower
    res <- cbind(res, surv = c(surv), std.err = c(x$std.err),
                 upper = c(upper), lower = c(lower))
    res$state <- rep(x$states, each = nrow(surv))
  }
  else {
    # Case of survfit(res.cox, newdata)
    if(is.matrix(x$surv)){
      ncurve <- ncol(x$surv)
      res <- data.frame(time = rep(x$time, ncurve), n.risk = rep(x$n.risk, ncurve),
                        n.event = rep(x$n.event, ncurve), n.censor = rep(x$n.censor, ncurve))
      res <- cbind(res, surv = .flat(x$surv),
                   std.err = .flat(x$std.err),
                   upper = .flat(x$upper),
                   lower = .flat(x$lower))
      res$strata <- as.factor(rep(colnames(x$surv), each = nrow(x$surv)))
    }
    # case of standard survfit() or survfit(res.cox)
    else res <- cbind(res, surv = x$surv, std.err = x$std.err,
                      upper = x$upper, lower = x$lower)
  }
  if (!is.null(x$strata)) {
    data <- .get_data(x, data = data) # data used to compute survfit
    res$strata <- rep(names(x$strata), x$strata)
    res$strata <- .clean_strata(res$strata, x)
    # Add column for each variable in survival fit
    variables <- .get_variables(res$strata, x, data)
    for(variable in variables) res[[variable]] <- .get_variable_value(variable, res$strata, x, data)
  }
  structure(res, class = c("data.frame", "surv_summary"))
  attr(res, "table") <-  as.data.frame(summary(x)$table)
  res
}

.compact <- function(x){Filter(Negate(is.null), x)}

.get_data <- function(fit, data = NULL, complain = TRUE) {
  if(is.null(data)){
    if (complain)
      warning ("The `data` argument is not provided. Data will be extracted from model fit.")
    data <- eval(fit$call$data)
    if (is.null(data))
      stop("The `data` argument should be provided either to ggsurvfit or survfit.")
  }
  data
}

.clean_strata <- function(strata, fit){
  is_dollar_sign <- grepl("$", as.character(strata)[1], fixed=TRUE)
  if(is_dollar_sign) {
    strata <- as.character(strata)
    data_name <- unlist(strsplit(strata[1], "$", fixed =TRUE))[1]
    strata <- gsub(paste0(data_name, "$"), "", strata, fixed=TRUE)
    strata <- as.factor(strata)
  }
  else if(!missing(fit)) strata <- factor(strata, levels = names(fit$strata))
  return(strata)
}

.get_variables <- function(strata, fit, data = NULL){
  variables <- sapply(as.vector(strata),
                      function(x){
                        x <- unlist(strsplit(x, "=|,\\s+", perl=TRUE))
                        x[seq(1, length(x), 2)]
                      })
  variables <- unique(as.vector(variables))
  variables <- intersect(variables, colnames(.get_data(fit, data) ))
  variables
}

.get_variable_value <- function(variable, strata, fit, data = NULL){
  res <- sapply(as.vector(strata), function(x){
    x <- unlist(strsplit(x, "=|(\\s+)?,\\s+", perl=TRUE))
    # When a factor name is the same as one of its level, index is of length 2
    index <- grep(paste0("^", variable, "$"), x)[1]
    .trim(x[index+1])
  })
  res <- as.vector(res)
  var_levels <- levels(.get_data(fit, data)[, variable])
  if(!is.null(var_levels)) res <- factor(res, levels = var_levels)
  else res <- as.factor(res)
  res
}

.trim <- function(x){gsub("^\\s+|\\s+$", "", x)}


#### Output KM analysis summaries for example outcomes - top 10%, middle 80%, bottom 10% ####

example_outcome_anlaysis <- F

if (example_outcome_anlaysis == T) {
  
## lung cancer
surv_summ_by_risk(target_ds = "165_1")

## MI
surv_summ_by_risk(target_ds = "411_2")

## Sepsis
surv_summ_by_risk(target_ds = "994_2")

## Death
surv_summ_by_risk(target_ds = "death")

}

#### Output KM analysis summaries for all outcomes - top 10%, middle 80%, bottom 10% ####
KM_by_risk_group <- T

if (KM_by_risk_group == T) {
  for (i in 1:n_outcome_ds) {
    target_ds_i <- outcome_ds_vec[i]
    surv_summ_by_risk(target_ds = target_ds_i)
    print(paste0("Completed Outcome ", i))
  }
}


#### Output KM analysis summaries for all outcomes - top 5%, middle 90%, bottom 5% ####
KM_by_risk_group2 <- T

if (KM_by_risk_group2 == T) {
  for (i in 1:n_outcome_ds) {
    target_ds_i <- outcome_ds_vec[i]
    surv_summ_by_risk2(target_ds = target_ds_i)
    print(paste0("Completed Outcome ", i))
  }
}

#### Output risk ratio summaries for all outcomes - top 5%, middle 90%, bottom 5% - CRS model ####
KM_get_risk_fold2 <- T

if (KM_get_risk_fold2 == T) {
  for (i in 1:n_outcome_ds) {
    target_ds_i <- outcome_ds_vec[i]
    surv_summ_get_CI_for_risk_fold2(target_ds = target_ds_i)
    print(paste0("Completed Outcome ", i))
  }
}


#### Explore: Output risk ratio summaries for all outcomes - top 5%, middle 90%, bottom 5% - CRS model, Bootstrap ####
start.time <- Sys.time()
surv_summ_get_CI_for_risk_fold2_bootstrap(target_ds = "411_2")
end.time <- Sys.time()
end.time - start.time

#### Output risk ratio summaries for all outcomes - top 5%, middle 90%, bottom 5% - Age + Sex model ####
KM_get_risk_fold2_age_sex <- T

if (KM_get_risk_fold2_age_sex == T) {
  for (i in 1:n_outcome_ds) {
    target_ds_i <- outcome_ds_vec[i]
    surv_summ_get_CI_for_risk_fold2(target_ds = target_ds_i,
                                    model_type = "Age_Sex")
    print(paste0("Completed Outcome ", i))
  }
}


#### Output KM analysis summaries for all outcomes - top 1%, middle 98%, bottom 1% ####
KM_by_risk_group3 <- T

if (KM_by_risk_group3 == T) {
  for (i in 1:n_outcome_ds) {
    target_ds_i <- outcome_ds_vec[i]
    surv_summ_by_risk3(target_ds = target_ds_i)
    print(paste0("Completed Outcome ", i))
  }
}


#### Output KM analysis summaries for all outcomes - full testing data ####
KM_full_pop <- T

if (KM_full_pop == T) {
  for (i in 1:n_outcome_ds) {
    target_ds_i <- outcome_ds_vec[i]
    surv_summ_full_pop(target_ds = target_ds_i)
    print(paste0("Completed Outcome ", i))
  }
}



