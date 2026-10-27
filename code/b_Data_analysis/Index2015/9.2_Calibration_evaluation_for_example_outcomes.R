### Author: Pearl Liu
### Calibration of risk for example outcomes in testing data
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
temp_data_dir_step3_4 <- '~/collab/CCU022/CCU022_CRS/Data/Step3.4/'
temp_data_dir <- '~/collab/CCU022/CCU022_CRS/Data/Step9.2/'
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
## cumrisk for each decile
surv_summ_by_decile <- function(target_ds = "411_2",
                                n_tiles = 10
) {
  
  if (target_ds %in% c("COVID_hspt", "death")) {
    target_ds_sex <- "Both"
  } else {
    target_ds_sex <- phecode_info %>%
      filter(PheCode == str_replace(target_ds, '_', '\\.')) %>%
      pull(Sex)
  }
  
  
  if (target_ds == "COVID_hspt") {
    ### Read trained effect sizes
    target_summ <- readRDS(paste0(temp_data_dir_step3_4, 'Outcome',
                                  target_ds, '_CalibratedCRS_SummStats_WhiteBritish_OddYOB_Index2020_Followup5yr.rds'))
    
    ### Read full testing data
    test_df_combine <- readRDS(paste0(temp_data_dir_step2_3, 'Outcome',
                                      target_ds, '_TestData_AllPts_WhiteBritish_EvenYOB_Index2020_Followup5yr.rds'))
    
  } else {
    ### Read full testing data
    test_df_combine <- readRDS(paste0(temp_data_dir_step2_3, 'Outcome',
                                      target_ds, '_TestData_AllPts_WhiteBritish_EvenYOB_Index2015_Followup10yr.rds'))
    
    
    ### Read trained effect sizes and get sex-specific data
    if (target_ds_sex %in% c("Male", "Female")) {
      target_summ <- readRDS(paste0(temp_data_dir_step3_4, 'Outcome',
                                    target_ds, '_Sex_Specific_CalibratedCRS_SummStats_WhiteBritish_OddYOB_Index2015_Followup10yr.rds'))
      
      if (target_ds_sex == "Female") {
        test_df_combine <- test_df_combine[Female == 1]
      } else {
        test_df_combine <- test_df_combine[Female == 0]
      }
      
      
    } else {
      target_summ <- readRDS(paste0(temp_data_dir_step3_4, 'Outcome',
                                            target_ds, '_CalibratedCRS_SummStats_WhiteBritish_OddYOB_Index2015_Followup10yr.rds'))
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
  
  test_dt_cuminc_analysis <- test_df_combine[, c("target_ds_status",
                                                 "target_ds_tte")] %>%
    .[, "target_ds_pred_risk" := target_ds_pred] %>%
    .[, "target_ds_pred_group" := ntile(target_ds_pred_risk, n_tiles)] %>%
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
  
  ### Compute mean and SD for predicted risk for each decile
  target_ds_pred_summ <- test_dt_cuminc_analysis %>%
    group_by(target_ds_pred_group) %>%
    mutate(mean_pred_risk = mean(target_ds_pred_risk),
           SD_pred_risk = sd(target_ds_pred_risk)) %>%
    select(target_ds_pred_group,
           mean_pred_risk,
           SD_pred_risk) %>%
    distinct() %>%
    arrange(target_ds_pred_group) %>%
    ungroup()
  
  if (target_ds == "COVID_hspt") {
    
    if (n_tiles == 10) {
      saveRDS(list(target_ds_km_summ = target_ds_km_summ,
                   target_ds_pred_summ = target_ds_pred_summ),
              paste0(temp_data_dir, "Outcome", target_ds, 
                     "_SurvivalAnalysisSummaryByDecile_WhiteBritish_EvenYOB_Index2020_Followup5yr.rds"))
    } else {
      saveRDS(list(target_ds_km_summ = target_ds_km_summ,
                   target_ds_pred_summ = target_ds_pred_summ),
              paste0(temp_data_dir, "Outcome", target_ds, 
                     "_SurvivalAnalysisSummaryBy", n_tiles, "Tiles_WhiteBritish_EvenYOB_Index2020_Followup5yr.rds"))
      
    }
    
  } else {
    
    if (n_tiles == 10) {
      saveRDS(list(target_ds_km_summ = target_ds_km_summ,
                   target_ds_pred_summ = target_ds_pred_summ),
                paste0(temp_data_dir, "Outcome", target_ds, 
                       "_SurvivalAnalysisSummaryByDecile_WhiteBritish_EvenYOB_Index2015_Followup10yr.rds"))
    } else {
      saveRDS(list(target_ds_km_summ = target_ds_km_summ,
                   target_ds_pred_summ = target_ds_pred_summ),
              paste0(temp_data_dir, "Outcome", target_ds, 
                     "_SurvivalAnalysisSummaryBy", n_tiles, "Tiles_WhiteBritish_EvenYOB_Index2015_Followup10yr.rds"))
      
    }
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


#### Output KM analysis summaries for example outcomes by decile ####

example_outcome_anlaysis <- F

if (example_outcome_anlaysis == T) {
  
## lung cancer
surv_summ_by_decile(target_ds = "165_1")

## MI
surv_summ_by_decile(target_ds = "411_2")

## T2D
surv_summ_by_decile(target_ds = "250_2")

## Sepsis
surv_summ_by_decile(target_ds = "994_2")

## Death
surv_summ_by_decile(target_ds = "death")

}

#### Output KM analysis summaries for all outcomes by decile ####
KM_by_decile <- F

if (KM_by_decile == T) {
  for (i in 1:n_outcome_ds) {
    target_ds_i <- outcome_ds_vec[i]
    surv_summ_by_decile(target_ds = target_ds_i)
    print(paste0("Completed Outcome ", i))
  }
}

#### Output KM analysis summaries for all outcomes by 20 quantiles ####
KM_by_20tiles <- T

if (KM_by_20tiles == T) {
  for (i in 1:n_outcome_ds) {
    target_ds_i <- outcome_ds_vec[i]
    surv_summ_by_decile(target_ds = target_ds_i,
                        n_tiles = 20)
    print(paste0("Completed Outcome ", i))
  }
}

#### Inspect calibration ####

example_plot <- F

if (example_plot == T) {

target_ds <- "COVID_hspt"
n_tiles <- 20

if (target_ds == "COVID_hspt") {
  if (n_tiles == 20) {
    target_ds_calib_summ <- readRDS(paste0(temp_data_dir, "Outcome", target_ds, 
                                           "_SurvivalAnalysisSummaryBy20Tiles_WhiteBritish_EvenYOB_Index2020_Followup5yr.rds"))
    
  } else if (n_tiles == 10) {
    target_ds_calib_summ <- readRDS(paste0(temp_data_dir, "Outcome", target_ds, 
                                           "_SurvivalAnalysisSummaryByDecile_WhiteBritish_EvenYOB_Index2020_Followup5yr.rds"))
    
  }
  
} else {
  if (n_tiles == 20) {
    target_ds_calib_summ <- readRDS(paste0(temp_data_dir, "Outcome", target_ds, 
                                           "_SurvivalAnalysisSummaryBy20Tiles_WhiteBritish_EvenYOB_Index2015_Followup10yr.rds"))
    
  } else if (n_tiles == 10) {
    target_ds_calib_summ <- readRDS(paste0(temp_data_dir, "Outcome", target_ds, 
                                           "_SurvivalAnalysisSummaryByDecile_WhiteBritish_EvenYOB_Index2015_Followup10yr.rds"))
    
  }
}
target_ds_km_summ <- target_ds_calib_summ$target_ds_km_summ
target_ds_pred_summ <- target_ds_calib_summ$target_ds_pred_summ


target_ds_km_summ_by_decile <- target_ds_km_summ %>%
  group_by(target_ds_pred_group) %>%
  filter(time == max(time)) %>%
  mutate(cuminc = 1 - surv,
         cuminc_LB = 1 - upper,
         cuminc_UB = 1 - lower) %>%
  select(target_ds_pred_group, cuminc, std.err, cuminc_LB, cuminc_UB) %>%
  ungroup() 

target_ds_n <- target_ds_km_summ %>%
  group_by(target_ds_pred_group) %>%
  filter(time == min(time)) %>%
  select(target_ds_pred_group, n.risk) %>%
  ungroup() %>%
  mutate(n.risk.initial = n.risk)

target_ds_final_calib_summ <- target_ds_pred_summ %>%
  mutate(target_ds_pred_group = as.factor(target_ds_pred_group)) %>%
  left_join(target_ds_km_summ_by_decile, by = "target_ds_pred_group") %>%
  left_join(target_ds_n, by = "target_ds_pred_group")

plot(target_ds_final_calib_summ$mean_pred_risk,
     target_ds_final_calib_summ$cuminc,
     xlab = "Predicted Risk",
     ylab = "Observed risk",
     xlim = c(0, 0.1),
     ylim = c(0, 0.1))
lines(target_ds_final_calib_summ$mean_pred_risk,
      target_ds_final_calib_summ$cuminc)
abline(0,1)

}

#### Save calibration summary - results by decile ####
save_results_by_decile <- F

if (save_results_by_decile == T) {

temp_data_dir <- '~/collab/CCU022/CCU022_CRS/Data/Step9.2/'
##### Get outcomes list #####
outcome_ds_list_name = paste0("ccu022_01_Outcome_Diseases_Phecode_list_by_PL_", file_date)
outcome_ds_list_name2 = paste0("ccu022_01_Outcome_Diseases_Phecode_list2_TopCommonPlusPheRS_by_PL_", file_date)

outcome_ds_list = readRDS(paste0(temp_data_dir_step1_0, outcome_ds_list_name))
outcome_ds_list2 = readRDS(paste0(temp_data_dir_step1_0, outcome_ds_list_name2))

outcome_ds_vec <- c(outcome_ds_list %>% 
                      arrange(index) %>% 
                      ## Remove phecode 622
                      filter(target_ds != "622") %>% 
                      pull(target_ds),
                    "COVID_hspt",
                    outcome_ds_list2 %>% arrange(index) %>% pull(target_ds),
                    "death")

n_outcome_ds <- length(outcome_ds_vec)

calib_by_decile <- vector(mode = "list", length = n_outcome_ds)

for (i in 1:n_outcome_ds) {
  target_ds <- outcome_ds_vec[i]
  
  if (target_ds == "COVID_hspt") {
    target_ds_calib_summ <- readRDS(paste0(temp_data_dir, "Outcome", target_ds, 
                                           "_SurvivalAnalysisSummaryByDecile_WhiteBritish_EvenYOB_Index2020_Followup5yr.rds"))
    
  } else {
    target_ds_calib_summ <- readRDS(paste0(temp_data_dir, "Outcome", target_ds, 
                                           "_SurvivalAnalysisSummaryByDecile_WhiteBritish_EvenYOB_Index2015_Followup10yr.rds"))
  }
  target_ds_km_summ <- target_ds_calib_summ$target_ds_km_summ
  target_ds_pred_summ <- target_ds_calib_summ$target_ds_pred_summ
  
  target_ds_km_summ_by_decile <- target_ds_km_summ %>%
    group_by(target_ds_pred_group) %>%
    filter(time == max(time)) %>%
    mutate(cuminc = 1 - surv,
           cuminc_LB = 1 - upper,
           cuminc_UB = 1 - lower) %>%
    select(target_ds_pred_group, cuminc, std.err, cuminc_LB, cuminc_UB) %>%
    ungroup() 
  
  target_ds_n <- target_ds_km_summ %>%
    group_by(target_ds_pred_group) %>%
    filter(time == min(time)) %>%
    select(target_ds_pred_group, n.risk) %>%
    ungroup()
  
  target_ds_final_calib_summ <- target_ds_pred_summ %>%
    mutate(target_ds_pred_group = as.factor(target_ds_pred_group)) %>%
    left_join(target_ds_km_summ_by_decile, by = "target_ds_pred_group") %>%
    left_join(target_ds_n, by = "target_ds_pred_group") %>%
    mutate(Outcome_phecode = target_ds) %>%
    select(Outcome_phecode, target_ds_pred_group, mean_pred_risk, SD_pred_risk, 
           cuminc, std.err, cuminc_LB, cuminc_UB, n.risk)
  
  calib_by_decile[[i]] <- target_ds_final_calib_summ
}

calib_by_decile_all_outcomes <- bind_rows(calib_by_decile)

write.csv(calib_by_decile_all_outcomes, 
          paste0(temp_data_dir, "All_outcomes_calibration_results_by_decile.csv"),
          row.names = F)

}



#### Save calibration summary - results by 20 tiles ####
save_results_by_20tiles <- F

if (save_results_by_20tiles == T) {
  
  temp_data_dir <- '~/collab/CCU022/CCU022_CRS/Data/Step9.2/'
  ##### Get outcomes list #####
  outcome_ds_list_name = paste0("ccu022_01_Outcome_Diseases_Phecode_list_by_PL_", file_date)
  outcome_ds_list_name2 = paste0("ccu022_01_Outcome_Diseases_Phecode_list2_TopCommonPlusPheRS_by_PL_", file_date)
  
  outcome_ds_list = readRDS(paste0(temp_data_dir_step1_0, outcome_ds_list_name))
  outcome_ds_list2 = readRDS(paste0(temp_data_dir_step1_0, outcome_ds_list_name2))
  
  outcome_ds_vec <- c(outcome_ds_list %>% 
                        arrange(index) %>% 
                        ## Remove phecode 622
                        filter(target_ds != "622") %>% 
                        pull(target_ds),
                      "COVID_hspt",
                      outcome_ds_list2 %>% arrange(index) %>% pull(target_ds),
                      "death")
  
  n_outcome_ds <- length(outcome_ds_vec)
  
  calib_by_decile <- vector(mode = "list", length = n_outcome_ds)
  
  for (i in 1:n_outcome_ds) {
    target_ds <- outcome_ds_vec[i]
    
    if (target_ds == "COVID_hspt") {
      target_ds_calib_summ <- readRDS(paste0(temp_data_dir, "Outcome", target_ds, 
                                             "_SurvivalAnalysisSummaryBy20Tiles_WhiteBritish_EvenYOB_Index2020_Followup5yr.rds"))
      
    } else {
      target_ds_calib_summ <- readRDS(paste0(temp_data_dir, "Outcome", target_ds, 
                                             "_SurvivalAnalysisSummaryBy20Tiles_WhiteBritish_EvenYOB_Index2015_Followup10yr.rds"))
    }
    target_ds_km_summ <- target_ds_calib_summ$target_ds_km_summ
    target_ds_pred_summ <- target_ds_calib_summ$target_ds_pred_summ
    
    target_ds_km_summ_by_decile <- target_ds_km_summ %>%
      group_by(target_ds_pred_group) %>%
      filter(time == max(time)) %>%
      mutate(cuminc = 1 - surv,
             cuminc_LB = 1 - upper,
             cuminc_UB = 1 - lower) %>%
      select(target_ds_pred_group, cuminc, std.err, cuminc_LB, cuminc_UB) %>%
      ungroup() 
    
    target_ds_n <- target_ds_km_summ %>%
      group_by(target_ds_pred_group) %>%
      filter(time == min(time)) %>%
      select(target_ds_pred_group, n.risk) %>%
      ungroup()
    
    target_ds_final_calib_summ <- target_ds_pred_summ %>%
      mutate(target_ds_pred_group = as.factor(target_ds_pred_group)) %>%
      left_join(target_ds_km_summ_by_decile, by = "target_ds_pred_group") %>%
      left_join(target_ds_n, by = "target_ds_pred_group") %>%
      mutate(Outcome_phecode = target_ds) %>%
      select(Outcome_phecode, target_ds_pred_group, mean_pred_risk, SD_pred_risk, 
             cuminc, std.err, cuminc_LB, cuminc_UB, n.risk)
    
    calib_by_decile[[i]] <- target_ds_final_calib_summ
  }
  
  calib_by_decile_all_outcomes <- bind_rows(calib_by_decile)
  
  write.csv(calib_by_decile_all_outcomes, 
            paste0(temp_data_dir, "All_outcomes_calibration_results_by_20tiles.csv"),
            row.names = F)
  
}


