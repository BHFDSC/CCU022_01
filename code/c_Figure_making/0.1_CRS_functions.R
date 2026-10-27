library(dplyr)
library(tidyr)
library(ggplot2)
library(doBy)
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
safe_colorblind_palette <- c("#88CCEE", "#CC6677", "#DDCC77", "#117733", "#332288", "#AA4499",
                             "#44AA99", "#999933", "#882255", "#661100", "#6699CC", "#888888")
# set the commonly used color
blue <- cbPalette[6]
red <- cbPalette[7]
green <- cbPalette[4]
orange <- cbPalette[2]
grey <- cbPalette[1]
yellow <- cbPalette[5]
purple <- cbPalette[8]
skyblue <- cbPalette[3]

# this is a function to create age-window based case-control data
comorbid_case_control <- function(ehr_data, target_disease, followup, start_age, end_age, censor_flg = F){
  target_disease <- ehr_data %>% 
    filter(diag_icd10 %in% target_disease) %>%
    group_by(eid) %>%
    filter(n() == 1 | age_diag == min(age_diag) ) %>% # this row is highly optimized, a lot faster the slice_min ### don't change
    slice(1) 
  # get the individuals who already have the disease
  exclude_ids <- target_disease %>% 
    filter(age_diag < start_age)
  cases_ids <- target_disease %>%
    filter(age_diag >= start_age, age_diag < end_age) %>%
    mutate(disease = 1) %>%
    select(eid, disease)
  # get the control: if censor_flg, use all individuals as controls; otherwise only use those who have passed age window end
  if(censor_flg){
    controls_ids <- followup %>%
      group_by(eid) %>% # note the followup file has some replications. 
      slice(1) %>%
      ungroup() %>%
      anti_join(exclude_ids, by = "eid") %>%
      anti_join(cases_ids, by = "eid") %>%
      filter(age_followup >= start_age) %>%
      mutate(disease = 0) %>%
      select(eid, disease)
  }else{
    controls_ids <- followup %>%
      group_by(eid) %>% # note the followup file has some replications. 
      slice(1) %>%
      ungroup() %>%
      anti_join(exclude_ids, by = "eid") %>%
      anti_join(cases_ids, by = "eid") %>%
      filter(age_followup >= end_age) %>%
      mutate(disease = 0) %>%
      select(eid, disease)
  }

  
  # only keep individuals with at least one case before start_age  
  # idv_with_one_prior_rec <- ehr_data %>% 
  #   filter(age_diag <= start_age) %>%
  #   group_by(eid) %>%
  #   slice(1) %>%
  #   pull(eid)
  
  case_control_data <- bind_rows(cases_ids, controls_ids) # %>%
    # filter(eid %in% idv_with_one_prior_rec)
  
  ehr_data_past <- ehr_data %>%
    filter(eid %in% case_control_data$eid, age_diag < start_age)
  return(list(case_control_data, ehr_data_past))
}


# this is a function to create age-window based case-control data
comorbid_imputed_censoring <- function(ehr_data, target_disease, followup, start_age, end_age, ht){
  target_disease <- ehr_data %>% 
    filter(diag_icd10 == target_disease) %>%
    group_by(eid) %>%
    filter(n() == 1 | age_diag == min(age_diag) ) %>% # this row is highly optimized, a lot faster the slice_min ### don't change
    slice(1) 
  # get the individuals who already have the disease
  exclude_ids <- target_disease %>% 
    filter(age_diag < start_age)
  cases_ids <- target_disease %>%
    filter(age_diag >= start_age, age_diag < end_age) %>%
    mutate(disease = 1) %>%
    select(eid, disease)
  # get the control
  controls_ids <- followup %>%
    group_by(eid) %>% # note the followup file has some replications. 
    slice(1) %>%
    ungroup() %>%
    anti_join(exclude_ids, by = "eid") %>%
    anti_join(cases_ids, by = "eid") %>%
    filter(age_followup >= end_age) %>%
    mutate(disease = 0) %>%
    select(eid, disease)
  
  # impute censored 
  ht_window <- ht %>%
    filter(first_diagnosis_age_hes >= start_age, first_diagnosis_age_hes < end_age) %>%
    arrange(first_diagnosis_age_hes)
  
  ## PL Q: what is this calculation?
  ht_window$counterfactual_prob <- 1 - rev( cumprod(rev(1 - ht_window$hazard_ratio)) )/sqrt(1 - ht_window$hazard_ratio )
  censored_ids <- followup %>%
    group_by(eid) %>% # note the followup file has some replications. 
    slice(1) %>%
    ungroup() %>%
    anti_join(exclude_ids, by = "eid") %>%
    anti_join(cases_ids, by = "eid") %>%
    filter(age_followup >= start_age, age_followup < end_age) %>%
    mutate(rounded_age = floor(age_followup)) %>%
    left_join(ht_window, by = c("rounded_age" = "first_diagnosis_age_hes")) %>%
    mutate(disease = counterfactual_prob) %>%
    select(eid, disease)
  
  # # only keep individuals with at least one case before start_age  
  # idv_with_one_prior_rec <- ehr_data %>% 
  #   filter(age_diag <= start_age) %>%
  #   group_by(eid) %>%
  #   slice(1) %>%
  #   pull(eid)
  
  case_control_data <- bind_rows(cases_ids, controls_ids, censored_ids) 
  
  ehr_data_past <- ehr_data %>%
    filter(eid %in% case_control_data$eid, age_diag < start_age)
  return(list(case_control_data, ehr_data_past))
}


# this is a function to create age-window based case-control data
## PL: modified calculation to impute disease risk for censored individuals - using survival package
comorbid_imputed_censoring2 <- function(ehr_data, target_ds, followup, start_age, end_age, all_ds_surv_dt, all_multiple_regression){
  target_disease <- ehr_data %>% 
    filter(diag_icd10 == target_ds) %>%
    group_by(eid) %>%
    filter(n() == 1 | age_diag == min(age_diag) ) %>% # this row is highly optimized, a lot faster the slice_min ### don't change
    slice(1) 
  # get the individuals who already have the disease
  exclude_ids <- target_disease %>% 
    filter(age_diag < start_age)
  cases_ids <- target_disease %>%
    filter(age_diag >= start_age, age_diag < end_age) %>%
    mutate(disease = 1) %>%
    select(eid, disease)
  # get the control
  controls_ids <- followup %>%
    # group_by(eid) %>% # no need to de-duplicate here, as the followup file is already de-duplicated
    # slice(1) %>%
    # ungroup() %>%
    anti_join(exclude_ids, by = "eid") %>%
    anti_join(cases_ids, by = "eid") %>%
    filter(age_followup >= end_age) %>%
    mutate(disease = 0) %>%
    select(eid, disease)
  
  # impute censored pts
  risk_window <- all_ds_surv_dt %>%
    subset(select = c('Age', target_ds)) %>%
    filter(Age >= start_age, Age <= end_age) %>%
    arrange(Age)
  
  ds_surv <- risk_window[,2]
  risk_window$counterfactual_prob <- 1 - ds_surv[which(risk_window$Age == end_age)]/ds_surv
  
  censored_ids <- followup %>%
    # group_by(eid) %>% # no need to de-duplicate here, as the followup file is already de-duplicated
    # slice(1) %>%
    # ungroup() %>%
    anti_join(exclude_ids, by = "eid") %>%
    anti_join(cases_ids, by = "eid") %>%
    filter(age_followup >= start_age, age_followup < end_age) %>%
    mutate(rounded_age = floor(age_followup)) %>%
    left_join(risk_window, by = c("rounded_age" = "Age")) %>%
    mutate(disease = counterfactual_prob) %>%
    select(eid, disease)
  
  # # only keep individuals with at least one case before start_age  
  # idv_with_one_prior_rec <- ehr_data %>% 
  #   filter(age_diag <= start_age) %>%
  #   group_by(eid) %>%
  #   slice(1) %>%
  #   pull(eid)
  
  case_control_data <- bind_rows(cases_ids, controls_ids, censored_ids) %>%
    rename(target_ds_status = disease)
  
  ehr_data_past <- ehr_data %>%
    filter(eid %in% case_control_data$eid, age_diag < start_age)
  
  ## get all predictor diseases need for the target disease
  pred_ds <- all_multiple_regression %>% 
    filter(target_disease == target_ds, age_window_start == start_age) %>%
    select(predictor_ds)
  pred_ds_vec <- pred_ds$predictor_ds %>% as.character()
  
  ## Create a wide matrix for past diagnoses
  ehr_data_past_w <- ehr_data_past %>%
    filter(diag_icd10 %in% pred_ds_vec) %>%
    mutate(diag_status = 1) %>%
    select(eid, diag_icd10, diag_status) %>%
    pivot_wider(names_from = diag_icd10, values_from = diag_status) %>%
    replace(is.na(.), 0)
  
  ## set all diagnosis status to zero for predictor diseases not included
  if (ncol(ehr_data_past_w)-1 < length(pred_ds_vec)) {
    extra_pred_ds <- setdiff(pred_ds_vec, colnames(ehr_data_past_w)[-1])
    ehr_data_past_w[, extra_pred_ds] <- 0
  }
  
  ## Order predictor diseases according to prediction model
  ehr_data_past_w_ordered <- cbind(ehr_data_past_w[,1], ehr_data_past_w[,pred_ds_vec])
  
  ehr_data_all <- case_control_data %>% 
    left_join(ehr_data_past_w_ordered, by = 'eid') %>% 
    replace(is.na(.), 0) %>%
    left_join(select(ehr_data, eid, sex, bmi)) %>%
    drop_na()
  
  return(ehr_data_all)
}


# this is a function to create age-window based case-control data
## PL: modified calculation to impute disease risk for censored individuals - using survival package
## Also used data.table to speed up computing
comorbid_imputed_censoring3 <- function(ehr_data, target_ds, followup, start_age, end_age, all_ds_surv_dt,
                                        all_multiple_regression){
  ehr_data <- setDT(ehr_data, key = 'eid')
  followup <- setDT(followup, key = 'eid')
  
  target_disease <- ehr_data[diag_icd10 == target_ds] %>%
    group_by(eid) %>%
    filter(n() == 1 | age_diag == min(age_diag) ) %>% # this row is highly optimized, a lot faster the slice_min ### don't change
    slice(1) %>%
    setDT()
  
  # target_disease <- ehr_data[diag_icd10 == target_ds] %>%
  #   .[, .SD[.N == 1 | age_diag == min(age_diag)], by = eid] %>% # this row is highly optimized, a lot faster the slice_min ### don't change
  #   .[, .SD[1], by = eid]
  
  # get the individuals who already have the disease
  exclude_ids <- target_disease[age_diag < start_age]
  cases_ids <- target_disease[age_diag >= start_age & age_diag < end_age] %>%
    .[,disease := 1] %>%
    .[,.(eid, disease)]
  # get the control
  controls_ids <- followup %>%
    # group_by(eid) %>% # no need to de-duplicate here, as the followup file is already de-duplicated
    # slice(1) %>%
    # ungroup() %>%
    .[!( eid %in% union(exclude_ids$eid, cases_ids$eid) )] %>%
    .[age_followup >= end_age] %>%
    .[, disease := 0] %>%
    .[,.(eid, disease)]
  
  # impute censored 
  risk_window <- all_ds_surv_dt %>%
    subset(select = c('Age', target_ds)) %>%
    filter(Age >= start_age, Age <= end_age) %>%
    arrange(Age)
  
  ds_surv <- risk_window[,target_ds]
  risk_window$counterfactual_prob <- 1 - ds_surv[which(risk_window$Age == end_age)]/ds_surv
  
  censored_ids <- followup %>%
    # group_by(eid) %>% # no need to de-duplicate here, as the followup file is already de-duplicated
    # slice(1) %>%
    # ungroup() %>%
    .[!( eid %in% union(exclude_ids$eid, cases_ids$eid) )] %>%
    .[age_followup >= start_age & age_followup < end_age] %>%
    .[, rounded_age := floor(age_followup)] %>%
    left_join(data.table(risk_window), by = join_by(rounded_age == Age)) %>%
    .[, disease := counterfactual_prob] %>%
    .[, .(eid, disease)]
  
  
  # # only keep individuals with at least one case before start_age  
  # idv_with_one_prior_rec <- ehr_data %>% 
  #   filter(age_diag <= start_age) %>%
  #   group_by(eid) %>%
  #   slice(1) %>%
  #   pull(eid)
  
  case_control_data <- bind_rows(cases_ids, controls_ids, censored_ids) %>%
    rename(target_ds_status = disease)
  
  ehr_data_past <- ehr_data[eid %in% case_control_data$eid & age_diag < start_age]
  
  ## get all predictor diseases needed for the target disease
  pred_ds <- all_multiple_regression %>% 
    filter(target_disease == target_ds, age_window_start == start_age) %>%
    select(predictor_ds)
  pred_ds_vec <- pred_ds$predictor_ds %>% as.character()
  
  ehr_data_past_w <- ehr_data_past[diag_icd10 %in% pred_ds_vec] %>%
    .[, diag_status := 1] %>%
    .[, .(eid, diag_icd10, diag_status)] %>%
    dcast(eid ~ diag_icd10, value.var = 'diag_status') %>%
    setnafill(fill = 0)
  
  #rm(ehr_data_past)
  
  ## set all diagnosis status to zero for predictor diseases not included
  if (ncol(ehr_data_past_w)-1 < length(pred_ds_vec)) {
    extra_pred_ds <- setdiff(pred_ds_vec, colnames(ehr_data_past_w)[-1])
    ehr_data_past_w[, extra_pred_ds] <- 0
  }
  
  ## Order predictor diseases according to prediction model
  ehr_data_past_w_ordered <- cbind(ehr_data_past_w[,1], 
                                   ehr_data_past_w[,..pred_ds_vec])
  rm(ehr_data_past_w)
  
  ehr_data_all <- case_control_data %>%   
    left_join(ehr_data_past_w_ordered, by = 'eid') %>%
    setnafill(fill = 0) 
  
  ## Join with sex and BMI data
  demo_info <- ehr_data[, .(eid, sex, bmi)] %>%
    distinct()
  ehr_data_all_final <- ehr_data_all %>%
    left_join(demo_info, by = 'eid') %>%
    drop_na()
  
  ## Get empirical target disease prevalence from training data
  target_ds_prev <- sum(ehr_data_all_final$target_ds_status == 1)/nrow(ehr_data_all_final)
  
  return(list(ehr_data_all_final = ehr_data_all_final,
              target_ds_prev = target_ds_prev))
}


# this is a function to create age-window based case-control data
## PL: modified calculation to impute disease risk for censored individuals - using survival package
## Also used data.table to speed up computing
## Implement fixed control-case ratio in training data
comorbid_imputed_censoring4 <- function(ehr_data, target_ds, followup, start_age, end_age, all_ds_surv_dt,
                                        all_multiple_regression, control_case_ratio = 1,
                                        seed_control){
  ehr_data <- setDT(ehr_data, key = 'eid')
  followup <- setDT(followup, key = 'eid')
  
  target_disease <- ehr_data[diag_icd10 == target_ds] %>%
    group_by(eid) %>%
    filter(n() == 1 | age_diag == min(age_diag) ) %>% # this row is highly optimized, a lot faster the slice_min ### don't change
    slice(1) %>%
    setDT()
  
  # target_disease <- ehr_data[diag_icd10 == target_ds] %>%
  #   .[, .SD[.N == 1 | age_diag == min(age_diag)], by = eid] %>% # this row is highly optimized, a lot faster the slice_min ### don't change
  #   .[, .SD[1], by = eid]
  
  # get the individuals who already have the disease
  exclude_ids <- target_disease[age_diag < start_age]
  cases_ids <- target_disease[age_diag >= start_age & age_diag < end_age] %>%
    .[,disease := 1] %>%
    .[,.(eid, disease)]
  # get the control
  controls_ids <- followup %>%
    # group_by(eid) %>% # no need to de-duplicate here, as the followup file is already de-duplicated
    # slice(1) %>%
    # ungroup() %>%
    .[!( eid %in% union(exclude_ids$eid, cases_ids$eid) )] %>%
    .[age_followup >= end_age] %>%
    .[, disease := 0] %>%
    .[,.(eid, disease)]
  
  # impute censored 
  risk_window <- all_ds_surv_dt %>%
    subset(select = c('Age', target_ds)) %>%
    filter(Age >= start_age, Age <= end_age) %>%
    arrange(Age)
  
  ds_surv <- risk_window[,target_ds]
  risk_window$counterfactual_prob <- 1 - ds_surv[which(risk_window$Age == end_age)]/ds_surv
  
  censored_ids <- followup %>%
    # group_by(eid) %>% # no need to de-duplicate here, as the followup file is already de-duplicated
    # slice(1) %>%
    # ungroup() %>%
    .[!( eid %in% union(exclude_ids$eid, cases_ids$eid) )] %>%
    .[age_followup >= start_age & age_followup < end_age] %>%
    .[, rounded_age := floor(age_followup)] %>%
    left_join(data.table(risk_window), by = join_by(rounded_age == Age)) %>%
    .[, disease := counterfactual_prob] %>%
    .[, .(eid, disease)]
  
  
  # # only keep individuals with at least one case before start_age  
  # idv_with_one_prior_rec <- ehr_data %>% 
  #   filter(age_diag <= start_age) %>%
  #   group_by(eid) %>%
  #   slice(1) %>%
  #   pull(eid)
  
  case_control_data <- bind_rows(cases_ids, controls_ids, censored_ids) %>%
    rename(target_ds_status = disease)
  
  ehr_data_past <- ehr_data[eid %in% case_control_data$eid & age_diag < start_age]
  
  ## get all predictor diseases needed for the target disease
  pred_ds <- all_multiple_regression %>% 
    filter(target_disease == target_ds, age_window_start == start_age) %>%
    select(predictor_ds)
  pred_ds_vec <- pred_ds$predictor_ds %>% as.character()
  
  ehr_data_past_w <- ehr_data_past[diag_icd10 %in% pred_ds_vec] %>%
    .[, diag_status := 1] %>%
    .[, .(eid, diag_icd10, diag_status)] %>%
    dcast(eid ~ diag_icd10, value.var = 'diag_status') %>%
    setnafill(fill = 0)
  
  #rm(ehr_data_past)
  
  ## set all diagnosis status to zero for predictor diseases not included
  if (ncol(ehr_data_past_w)-1 < length(pred_ds_vec)) {
    extra_pred_ds <- setdiff(pred_ds_vec, colnames(ehr_data_past_w)[-1])
    ehr_data_past_w[, extra_pred_ds] <- 0
  }
  
  ## Order predictor diseases according to prediction model
  ehr_data_past_w_ordered <- cbind(ehr_data_past_w[,1], 
                                   ehr_data_past_w[,..pred_ds_vec])
  rm(ehr_data_past_w)
  
  ehr_data_all <- case_control_data %>%   
    left_join(ehr_data_past_w_ordered, by = 'eid') %>%
    setnafill(fill = 0) 
  
  ## Join with sex and BMI data
  demo_info <- ehr_data[, .(eid, sex, bmi)] %>%
    distinct()
  ehr_data_all_demo <- ehr_data_all %>%
    left_join(demo_info, by = 'eid') %>%
    drop_na()
  
  ## Get empirical target disease prevalence from training data
  target_ds_prev <- sum(ehr_data_all_demo$target_ds_status == 1)/nrow(ehr_data_all_demo)
  
  ## Report empirical prevalence from testing data
  n_cases <- sum(ehr_data_all_demo$target_ds_status == 1)
  n_controls <- sum(ehr_data_all_demo$target_ds_status != 1)
  
  ## Sample controls using a fixed control_case ratio
  if (control_case_ratio > 0) {
    
    if (n_cases >= n_controls) {
      stop("There are more cases than controls.")
    } else {
      # randomly select controls that match the number of cases
      set.seed(seed_control)
      controls_select_idx <- sample(1:n_controls, n_cases * control_case_ratio)
      
      controls_dt <- ehr_data_all_demo %>% 
        filter(target_ds_status != 1)
      controls_dt_select <- controls_dt[controls_select_idx,]
      
      cases_dt <- ehr_data_all_demo %>% 
        filter(target_ds_status == 1)
      ehr_data_all_final <- rbind(cases_dt, controls_dt_select)
    }
    
  } else {
    ehr_data_all_final <- ehr_data_all_demo
  }
  
  return(list(ehr_data_all_final = ehr_data_all_final,
              target_ds_prev = target_ds_prev))
}



## Function to train prediction model using (1) CRS + sex + bmi, (2) sex + bmi
## Return regression coefficients for models
## Input: 
## train_dt: training dataset including outcome and predictor data (output of comorbid_imputed_censoring3 function)
train_pred_models <- function(train_dt, target_ds, all_multiple_regression, 
                              start_age, end_age, center_XY = F) {
  ## get all predictor diseases needed for the target disease
  pred_info <- all_multiple_regression %>% 
    filter(target_disease == target_ds, age_window_start == start_age)
  pred_ds_vec <- pred_info$predictor_ds %>% as.character()
  pred_num <- length(pred_ds_vec)
  pred_coef <- pred_info$b_multiple ## note: need to use joint effect sizes as weights
  
  ## Center the predictor diseases and other covariates in training dataset
  pred_idx <- 3:(pred_num + 2)
  #pred_ds_dt <- train_dt[,..pred_idx]
  pred_ds_dt_c <- scale(train_dt[,..pred_idx], center = T, scale = F)
  covar_idx <- (pred_num + 3):ncol(train_dt)
  
  ## Construct a variable for CRS
  crs_dt <- data.table(pred_ds_dt_c) %>%
    .[, Map("*", .SD, pred_coef)] %>%
    .[, crs := rowSums(.SD)]
  
  if (center_XY == F) {
      train_dt_crs <- cbind(train_dt$target_ds_status, crs_dt$crs, train_dt[,..covar_idx]) %>%
        .[, sex := factor(sex, levels = c('Male', 'Female'))]
  } else {
    ## Center the outcome in training dataset
    target_ds_dt_c <- scale(train_dt$target_ds_status, center = T, scale = F)
    ## Center the CRS score
    crs_c <- scale(crs_dt$crs, center = T, scale = F)
    ## Center the covariates
    covar_dt <- train_dt[,..covar_idx] %>% 
      .[, sex := ifelse(sex == 'Female', 1, 0)]
    covar_dt_c <- scale(covar_dt, center = T, scale = F)
    
    train_dt_crs <- cbind(target_ds_dt_c, crs_c, covar_dt_c) %>% as.data.frame()
  }
  colnames(train_dt_crs)[1:2] <- c("target_ds_status", "crs")
  
  ## Train CRS + sex + bmi model
  m_crs <- lm(target_ds_status ~ crs + sex + bmi, data = train_dt_crs)
  
  ## Train sex + bmi model
  m1 <- lm(target_ds_status ~ sex + bmi, data = train_dt_crs)
  
  ## Return coefficent estimates of the models
  m_crs_coef <- lm_summ(m_crs)
  m1_coef <- lm_summ(m1)
  
  return(list(crs_coef_est = m_crs_coef$coef_est, 
              crs_coef_all = m_crs_coef$coef_all, 
              crs_model = m_crs,
              crs_model_summ = m_crs_coef$model_summ,
              model_ref1_coef_est = m1_coef$coef_est,
              model_ref1_coef_all = m1_coef$coef_all,
              model_ref1_model = m1,
              model_ref1_model_summ = m1_coef$model_summ))

}


## Function to return coefficient estimates of lm models
lm_summ <- function(lm_model) {
  lm_summary <- summary(lm_model)
  coef_vec <- lm_summary$coefficients[,1]
  return(list(coef_est = coef_vec, 
              coef_all = lm_summary$coefficients,
              model_summ = lm_summary))
}

## Function to prepare testing data for prediction
## See comorbid_imputed_censoring3 function for reference
## Create a time variable to indicate age of diagnosis for cases and time of censoring for controls
## case_control_ratio = -1 means: use the entire population
## control_def = 1: Controls are defined as patients who were followed up past age window start (start_age) without outcome diagnosis
## control_def = 2: Controls are defined as patients who were followed up past age window end (end_age) without outcome diagnosis

test_dt_prepare <- function(ehr_data, target_ds, followup, start_age, end_age, 
                            all_multiple_regression,
                            seed_control = 123456,
                            control_case_ratio = 1,
                            control_def = 1) {
  ehr_data <- setDT(ehr_data, key = 'eid')
  followup <- setDT(followup, key = 'eid')
  
  target_disease <- ehr_data[diag_icd10 == target_ds] %>%
    group_by(eid) %>%
    filter(n() == 1 | age_diag == min(age_diag) ) %>% # this row is highly optimized, a lot faster the slice_min ### don't change
    slice(1) %>%
    setDT()
  
  # target_disease <- ehr_data[diag_icd10 == target_ds] %>%
  #   .[, .SD[.N == 1 | age_diag == min(age_diag)], by = eid] %>% # this row is highly optimized, a lot faster the slice_min ### don't change
  #   .[, .SD[1], by = eid]
  
  # get the individuals who already have the disease
  exclude_ids <- target_disease[age_diag < start_age]
  cases_ids <- target_disease[age_diag >= start_age & age_diag < end_age] %>%
    .[, disease := 1] %>%
    .[,.(eid, disease, age_diag)]
  setnames(cases_ids, "age_diag", "time")
  
  # get the control
  controls_ids_init <- followup %>%
    # group_by(eid) %>% # no need to de-duplicate here, as the followup file is already de-duplicated
    # slice(1) %>%
    # ungroup() %>%
    .[!( eid %in% union(exclude_ids$eid, cases_ids$eid) )] 
  
  if (control_def == 1) {
    controls_ids <- controls_ids_init %>%
      .[age_followup >= start_age] %>%
      .[, disease := 0] %>%
      .[,.(eid, disease, age_followup)]
  } else if (control_def == 2) {
    controls_ids <- controls_ids_init %>%
      .[age_followup >= end_age] %>%
      .[, disease := 0] %>%
      .[,.(eid, disease, age_followup)]
  }
  ## Censor follow-up time at end of age window for controls
  controls_ids <- controls_ids %>%
    .[, age_followup := ifelse(age_followup >= end_age, end_age, age_followup)]
  setnames(controls_ids, "age_followup", "time")
  
  case_control_data <- bind_rows(cases_ids, controls_ids) %>%
    rename(target_ds_status = disease)
  
  ehr_data_past <- ehr_data[eid %in% case_control_data$eid & age_diag < start_age]
  
  ## get all predictor diseases needed for the target disease
  pred_ds <- all_multiple_regression %>% 
    filter(target_disease == target_ds, age_window_start == start_age) %>%
    select(predictor_ds)
  pred_ds_vec <- pred_ds$predictor_ds %>% as.character()
  
  ehr_data_past_w <- ehr_data_past[diag_icd10 %in% pred_ds_vec] %>%
    .[, diag_status := 1] %>%
    .[, .(eid, diag_icd10, diag_status)] %>%
    dcast(eid ~ diag_icd10, value.var = 'diag_status') %>%
    setnafill(fill = 0)
  
  #rm(ehr_data_past)
  
  ## set all diagnosis status to zero for predictor diseases not included
  if (ncol(ehr_data_past_w)-1 < length(pred_ds_vec)) {
    extra_pred_ds <- setdiff(pred_ds_vec, colnames(ehr_data_past_w)[-1])
    ehr_data_past_w[, extra_pred_ds] <- 0
  }
  
  ## Order predictor diseases according to prediction model
  ehr_data_past_w_ordered <- cbind(ehr_data_past_w[,1], 
                                   ehr_data_past_w[,..pred_ds_vec])
  rm(ehr_data_past_w)
  
  ehr_data_all <- case_control_data %>%   
    left_join(ehr_data_past_w_ordered, by = 'eid') %>%
    setnafill(fill = 0) 
  
  ## Join with sex and BMI data
  demo_info <- ehr_data[, .(eid, sex, bmi)] %>%
    distinct()
  ehr_data_all_demo <- ehr_data_all %>%
    left_join(demo_info, by = 'eid') %>%
    drop_na()
  
  ## Report empirical prevalence from testing data
  n_cases <- sum(ehr_data_all_demo$target_ds_status == 1)
  n_controls <- sum(ehr_data_all_demo$target_ds_status == 0)
  
  target_ds_prev <- n_cases/(n_cases + n_controls)
  
  ## Sample controls using a fixed control_case ratio
  if (control_case_ratio > 0) {
    
    if (n_cases >= n_controls) {
      stop("There are more cases than controls.")
    } else {
      # randomly select controls that match the number of cases
      set.seed(seed_control)
      controls_select_idx <- sample(1:n_controls, n_cases * control_case_ratio)
      
      controls_dt <- ehr_data_all_demo %>% 
        filter(target_ds_status == 0)
      controls_dt_select <- controls_dt[controls_select_idx,]
      
      cases_dt <- ehr_data_all_demo %>% 
        filter(target_ds_status == 1)
      ehr_data_all_final <- rbind(cases_dt, controls_dt_select)
    }
  
  } else {
    ehr_data_all_final <- ehr_data_all_demo
  }
  
  return(list(ehr_data_all_final = ehr_data_all_final,
              target_ds_prev = target_ds_prev))
}


## Function to get prediction results in testing data using training models: (1) CRS + sex + bmi, (2) sex + bmi
## Return predicted disease probability and other prediction metrics, including:
  ## AUROC, C-index, prediction R^2 -- applies to fixed case-control ratio
  ## AUPRC -- applies to entire testing data
## Input: 
## test_dt: testing dataset including outcome and predictor data (output of test_dt_prepare function)
## control_case_ratio = -1 means: use the entire population

test_pred_models <- function(test_dt, target_ds, 
                             target_ds_prev,
                             all_multiple_regression, 
                             train_results,
                             start_age, end_age, 
                             control_case_ratio = 1,
                             center_XY = F) {
  
  ## Get all predictor diseases needed for the target disease
  pred_info <- all_multiple_regression %>% 
    filter(target_disease == target_ds, age_window_start == start_age)
  pred_ds_vec <- pred_info$predictor_ds %>% as.character()
  pred_num <- length(pred_ds_vec)
  pred_coef <- pred_info$b_multiple ## note: need to use joint effect sizes
  
  ## Center the predictor diseases in training dataset
  pred_idx <- 4:(pred_num + 3)  ## double-check the indices to account for "time" variable
  pred_ds_dt_c <- scale(test_dt[,..pred_idx], center = T, scale = F)
  covar_idx <- (pred_num + 4):ncol(test_dt)

  ## Construct a variable for CRS: linear combination of disease status and regression coefficients
  crs_dt <- data.table(pred_ds_dt_c) %>%
    .[, Map("*", .SD, pred_coef)] %>%
    .[, crs := rowSums(.SD)]
  
  
  if (center_XY == F) {
    test_dt_crs <- cbind(test_dt$target_ds_status, crs_dt$crs, test_dt[,..covar_idx]) %>%
      .[, sex := ifelse(sex == 'Female', 1, 0)]
    # test_dt_crs0 <- cbind(test_dt$target_ds_status, crs_dt$crs, test_dt[,..covar_idx]) %>%
    #  .[, sex := factor(sex, levels = c('Male', 'Female'))]
  } else {
    ## Center the outcome in testing dataset
    target_ds_dt_c <- scale(test_dt$target_ds_status, center = T, scale = F)
    ## Center the CRS score
    crs_c <- scale(crs_dt$crs, center = T, scale = F)
    ## Center the covariates
    covar_dt <- test_dt[,..covar_idx] %>% 
      .[, sex := ifelse(sex == 'Female', 1, 0)]
    covar_dt_c <- scale(covar_dt, center = T, scale = F)
    
    test_dt_crs <- cbind(target_ds_dt_c, crs_c, covar_dt_c) %>% as.data.frame()
  }
  
  colnames(test_dt_crs)[1:2] <- c("target_ds_status", "crs")
  #colnames(test_dt_crs0)[1:2] <- c("target_ds_status", "crs")
  
  test_dt_crs_predictors <- test_dt_crs[,-1]
  #test_dt_crs_predictors0 <- test_dt_crs0[,-1]
  
  ## Read trained model coefficients
  crs_coef_est <- train_results$crs_coef_est
  #crs_model <- train_results$crs_model
  
  ## Obtain predicted target disease probability for CRS + sex + bmi model
  crs_target_ds_pred <- as.matrix(test_dt_crs_predictors) %*% crs_coef_est[-1] + crs_coef_est[1]
  # crs_target_ds_pred0 <- predict(crs_model, 
  #                                newdata = test_dt_crs_predictors0,
  #                                type = 'response')
  # Alternative code for prediction - see above
  
  
  ## Obtain predicted target disease probability for sex + bmi model
  test_dt_other_predictors <- test_dt_crs[,-c(1,2)]
  model_ref1_coef_est <- train_results$model_ref1_coef_est
  model_ref1_target_ds_pred <- as.matrix(test_dt_other_predictors) %*% model_ref1_coef_est[-1] + model_ref1_coef_est[1]

  ## Get true outcome status
  Y <- test_dt$target_ds_status
  mean_Y <- mean(Y)
  
  if (control_case_ratio > 0) {
    ## Report prediction R^2 on the observed scale
    
    ## Calculate R^2 assuming that both X and Y are centered and scaled - not using this formula, use correlation^2 between Y_hat and Y instead
    if (center_XY == T) {
        Y_c <- test_dt_crs$target_ds_status
        mean_Y_c <- mean(Y_c)
      
        #crs_pred_R2_obs <- 1 - sum((Y_c - crs_target_ds_pred)^2)/sum((Y_c - mean_Y_c)^2)
        #model_ref1_pred_R2_obs <- 1 - sum((Y_c - model_ref1_target_ds_pred)^2)/sum((Y_c - mean_Y_c)^2)
        
        crs_pred_R2_obs <- cor(crs_target_ds_pred, Y)^2
        model_ref1_pred_R2_obs <- cor(model_ref1_target_ds_pred, Y)^2
    } else { 
        #crs_pred_R2_obs <- 1 - sum((scale(Y, scale = F) - scale(crs_target_ds_pred,scale = F))^2)/sum(scale(Y,scale = F)^2)
        #model_ref1_pred_R2_obs <- 1 - sum((scale(Y, scale = F) - scale(model_ref1_target_ds_pred, scale = F))^2)/sum(scale(Y,scale = F)^2)
       
      crs_pred_R2_obs <- cor(crs_target_ds_pred, Y)^2
      model_ref1_pred_R2_obs <- cor(model_ref1_target_ds_pred, Y)^2
    }
    ## Report prediction R^2 on the liability scale
    crs_pred_R2_liab <- LiabR2_cc(K = target_ds_prev, P = mean_Y, 
                                  R2_obs = crs_pred_R2_obs)
    model_ref1_pred_R2_liab <- LiabR2_cc(K = target_ds_prev, P = mean_Y, 
                                         R2_obs = model_ref1_pred_R2_obs)
    
    ## Report AUROC
    crs_auc <- auc(Y ~ crs_target_ds_pred)[[1]]
    model_ref1_auc <- auc(Y ~ model_ref1_target_ds_pred)[[1]]
    
    ## Report C-index
    ## Reference: https://www.sciencedirect.com/science/article/pii/S1532046420301246#b0075

    ## Use cindex function from pec package to speed up computation
    ## Note: the first argument should be survival probability
    crs_cindex <- cindex(matrix(1 - crs_target_ds_pred),
                          formula = Surv(time,target_ds_status) ~ 1,
                          data = test_dt)$AppCindex$matrix
    
    model_ref1_cindex <- cindex(matrix(1 - model_ref1_target_ds_pred),
                                formula = Surv(time,target_ds_status) ~ 1,
                                data = test_dt)$AppCindex$matrix
    
    ## Using Cindex function from nftbart package is slower
    # crs_cindex <- Cindex(risk = crs_target_ds_pred,
    #                      times = test_dt$time,
    #                      delta = test_dt$target_ds_status)
    
    ## Store the metrics
    crs_metrics <- c(crs_pred_R2_obs, crs_pred_R2_liab, crs_auc, crs_cindex)
    model_ref1_metrics <- c(model_ref1_pred_R2_obs, model_ref1_pred_R2_liab,
                           model_ref1_auc, model_ref1_cindex)
    names(crs_metrics) <- c('R2_obs', 'R2_liab', 'AUROC', 'C_index')
    names(model_ref1_metrics) <- names(crs_metrics)
  } else if (control_case_ratio == -1) {
    ## Report AUPRC
    crs_prec_recall <- precision.at.all.recall.levels(scores = crs_target_ds_pred, 
                                                      labels = Y)
    model_ref1_prec_recall <- precision.at.all.recall.levels(scores = model_ref1_target_ds_pred, 
                                                      labels = Y)
    crs_auprc <- AUPRC(list(crs_prec_recall))[[1]]
    model_ref1_auprc <- AUPRC(list(model_ref1_prec_recall))[[1]]
    
    ## Report the difference between AUPRC and target disease prevalence
    crs_auprc_prev_diff <- crs_auprc - target_ds_prev
    model_ref1_auprc_prev_diff <- model_ref1_auprc - target_ds_prev
    
    crs_metrics <- c(crs_auprc, crs_auprc_prev_diff)
    model_ref1_metrics <- c(model_ref1_auprc, model_ref1_auprc_prev_diff)
    names(crs_metrics) <- c('AUPRC', 'AUPRC_prev_diff')
    names(model_ref1_metrics) <- names(crs_metrics)
  }
  
  ## Return predicted target disease probabilities of the models and performance metrics
  return(list(crs_pred = crs_target_ds_pred, 
              model_ref1_pred = model_ref1_target_ds_pred, 
              crs_metrics = crs_metrics,
              model_ref1_metrics = model_ref1_metrics))
}


## Variation of the test_pred_models() function for diagnostic purpose
test_pred_models_ReportAll <- function(test_dt, target_ds, 
                             target_ds_prev,
                             all_multiple_regression, 
                             train_results,
                             start_age, end_age, 
                             control_case_ratio = 1,
                             center_XY = F) {
  
  ## Get all predictor diseases needed for the target disease
  pred_info <- all_multiple_regression %>% 
    filter(target_disease == target_ds, age_window_start == start_age)
  pred_ds_vec <- pred_info$predictor_ds %>% as.character()
  pred_num <- length(pred_ds_vec)
  pred_coef <- pred_info$b_multiple ## note: need to use joint effect sizes
  
  ## Center the predictor diseases in training dataset
  pred_idx <- 4:(pred_num + 3)  ## double-check the indices to account for "time" variable
  pred_ds_dt_c <- scale(test_dt[,..pred_idx], center = T, scale = F)
  covar_idx <- (pred_num + 4):ncol(test_dt)
  
  ## Construct a variable for CRS: linear combination of disease status and regression coefficients
  crs_dt <- data.table(pred_ds_dt_c) %>%
    .[, Map("*", .SD, pred_coef)] %>%
    .[, crs := rowSums(.SD)]
  
  
  if (center_XY == F) {
    test_dt_crs <- cbind(test_dt$target_ds_status, crs_dt$crs, test_dt[,..covar_idx]) %>%
      .[, sex := ifelse(sex == 'Female', 1, 0)]
    # test_dt_crs0 <- cbind(test_dt$target_ds_status, crs_dt$crs, test_dt[,..covar_idx]) %>%
    #  .[, sex := factor(sex, levels = c('Male', 'Female'))]
  } else {
    ## Center the outcome in testing dataset
    target_ds_dt_c <- scale(test_dt$target_ds_status, center = T, scale = F)
    ## Center the CRS score
    crs_c <- scale(crs_dt$crs, center = T, scale = F)
    ## Center the covariates
    covar_dt <- test_dt[,..covar_idx] %>% 
      .[, sex := ifelse(sex == 'Female', 1, 0)]
    covar_dt_c <- scale(covar_dt, center = T, scale = F)
    
    test_dt_crs <- cbind(target_ds_dt_c, crs_c, covar_dt_c) %>% as.data.frame()
  }
  
  colnames(test_dt_crs)[1:2] <- c("target_ds_status", "crs")
  #colnames(test_dt_crs0)[1:2] <- c("target_ds_status", "crs")
  
  test_dt_crs_predictors <- test_dt_crs[,-1]
  #test_dt_crs_predictors0 <- test_dt_crs0[,-1]
  
  ## Read trained model coefficients
  crs_coef_est <- train_results$crs_coef_est
  #crs_model <- train_results$crs_model
  
  ## Obtain predicted target disease probability for CRS + sex + bmi model
  crs_target_ds_pred <- as.matrix(test_dt_crs_predictors) %*% crs_coef_est[-1] + crs_coef_est[1]
  # crs_target_ds_pred0 <- predict(crs_model, 
  #                                newdata = test_dt_crs_predictors0,
  #                                type = 'response')
  # Alternative code for prediction - see above
  
  
  ## Obtain predicted target disease probability for sex + bmi model
  test_dt_other_predictors <- test_dt_crs[,-c(1,2)]
  model_ref1_coef_est <- train_results$model_ref1_coef_est
  model_ref1_target_ds_pred <- as.matrix(test_dt_other_predictors) %*% model_ref1_coef_est[-1] + model_ref1_coef_est[1]
  
  ## Get true outcome status
  Y <- test_dt$target_ds_status
  mean_Y <- mean(Y)

    ## Report prediction R^2 on the observed scale
    
    ## Calculate R^2 assuming that both X and Y are centered and scaled - not using this formula, use correlation^2 between Y_hat and Y instead
    if (center_XY == T) {
      Y_c <- test_dt_crs$target_ds_status
      mean_Y_c <- mean(Y_c)
      
      #crs_pred_R2_obs <- 1 - sum((Y_c - crs_target_ds_pred)^2)/sum((Y_c - mean_Y_c)^2)
      #model_ref1_pred_R2_obs <- 1 - sum((Y_c - model_ref1_target_ds_pred)^2)/sum((Y_c - mean_Y_c)^2)
      
      crs_pred_R2_obs <- cor(crs_target_ds_pred, Y)^2
      model_ref1_pred_R2_obs <- cor(model_ref1_target_ds_pred, Y)^2
    } else { 
      #crs_pred_R2_obs <- 1 - sum((scale(Y, scale = F) - scale(crs_target_ds_pred,scale = F))^2)/sum(scale(Y,scale = F)^2)
      #model_ref1_pred_R2_obs <- 1 - sum((scale(Y, scale = F) - scale(model_ref1_target_ds_pred, scale = F))^2)/sum(scale(Y,scale = F)^2)
      
      crs_pred_R2_obs <- cor(crs_target_ds_pred, Y)^2
      model_ref1_pred_R2_obs <- cor(model_ref1_target_ds_pred, Y)^2
    }
    ## Report prediction R^2 on the liability scale
    crs_pred_R2_liab <- LiabR2_cc(K = target_ds_prev, P = mean_Y, 
                                  R2_obs = crs_pred_R2_obs)
    model_ref1_pred_R2_liab <- LiabR2_cc(K = target_ds_prev, P = mean_Y, 
                                         R2_obs = model_ref1_pred_R2_obs)
    
    ## Report AUROC
    crs_auc <- auc(Y ~ crs_target_ds_pred)[[1]]
    model_ref1_auc <- auc(Y ~ model_ref1_target_ds_pred)[[1]]
    
    ## Report C-index
    ## Reference: https://www.sciencedirect.com/science/article/pii/S1532046420301246#b0075
    
    ## Use cindex function from pec package to speed up computation
    ## Note: the first argument should be survival probability
    crs_cindex <- cindex(matrix(1 - crs_target_ds_pred),
                         formula = Surv(time,target_ds_status) ~ 1,
                         data = test_dt)$AppCindex$matrix
    
    model_ref1_cindex <- cindex(matrix(1 - model_ref1_target_ds_pred),
                                formula = Surv(time,target_ds_status) ~ 1,
                                data = test_dt)$AppCindex$matrix
    
    ## Using Cindex function from nftbart package is slower
    # crs_cindex <- Cindex(risk = crs_target_ds_pred,
    #                      times = test_dt$time,
    #                      delta = test_dt$target_ds_status)
    
    ## Report AUPRC
    crs_prec_recall <- precision.at.all.recall.levels(scores = crs_target_ds_pred, 
                                                      labels = Y)
    model_ref1_prec_recall <- precision.at.all.recall.levels(scores = model_ref1_target_ds_pred, 
                                                             labels = Y)
    crs_auprc <- AUPRC(list(crs_prec_recall))[[1]]
    model_ref1_auprc <- AUPRC(list(model_ref1_prec_recall))[[1]]
    
    ## Report the difference between AUPRC and target disease prevalence
    crs_auprc_prev_diff <- crs_auprc - target_ds_prev
    model_ref1_auprc_prev_diff <- model_ref1_auprc - target_ds_prev
    
    crs_metrics <- c(crs_auprc, crs_auprc_prev_diff)
    model_ref1_metrics <- c(model_ref1_auprc, model_ref1_auprc_prev_diff)
    names(crs_metrics) <- c('AUPRC', 'AUPRC_prev_diff')
    names(model_ref1_metrics) <- names(crs_metrics)
    
    ## Store the metrics
    crs_metrics <- c(crs_pred_R2_obs, crs_pred_R2_liab, crs_auc, crs_cindex,
                     crs_auprc, crs_auprc_prev_diff)
    model_ref1_metrics <- c(model_ref1_pred_R2_obs, model_ref1_pred_R2_liab,
                            model_ref1_auc, model_ref1_cindex,
                            model_ref1_auprc, model_ref1_auprc_prev_diff)
    names(crs_metrics) <- c('R2_obs', 'R2_liab', 'AUROC', 'C_index',
                            'AUPRC', 'AUPRC_prev_diff')
    names(model_ref1_metrics) <- names(crs_metrics)
  
  ## Return predicted target disease probabilities of the models and performance metrics
  return(list(crs_pred = crs_target_ds_pred, 
              model_ref1_pred = model_ref1_target_ds_pred, 
              crs_metrics = crs_metrics,
              model_ref1_metrics = model_ref1_metrics))
}

## Variation of the test_pred_models() function for diagnostic purpose
test_pred_models_ReportROC <- function(test_dt, target_ds, 
                                       target_ds_prev,
                                       all_multiple_regression, 
                                       train_results,
                                       start_age, end_age, 
                                       control_case_ratio = 1,
                                       center_XY = F) {
  
  ## Get all predictor diseases needed for the target disease
  pred_info <- all_multiple_regression %>% 
    filter(target_disease == target_ds, age_window_start == start_age)
  pred_ds_vec <- pred_info$predictor_ds %>% as.character()
  pred_num <- length(pred_ds_vec)
  pred_coef <- pred_info$b_multiple ## note: need to use joint effect sizes
  
  ## Center the predictor diseases in training dataset
  pred_idx <- 4:(pred_num + 3)  ## double-check the indices to account for "time" variable
  pred_ds_dt_c <- scale(test_dt[,..pred_idx], center = T, scale = F)
  covar_idx <- (pred_num + 4):ncol(test_dt)
  
  ## Construct a variable for CRS: linear combination of disease status and regression coefficients
  crs_dt <- data.table(pred_ds_dt_c) %>%
    .[, Map("*", .SD, pred_coef)] %>%
    .[, crs := rowSums(.SD)]
  
  
  if (center_XY == F) {
    test_dt_crs <- cbind(test_dt$target_ds_status, crs_dt$crs, test_dt[,..covar_idx]) %>%
      .[, sex := ifelse(sex == 'Female', 1, 0)]
    # test_dt_crs0 <- cbind(test_dt$target_ds_status, crs_dt$crs, test_dt[,..covar_idx]) %>%
    #  .[, sex := factor(sex, levels = c('Male', 'Female'))]
  } else {
    ## Center the outcome in testing dataset
    target_ds_dt_c <- scale(test_dt$target_ds_status, center = T, scale = F)
    ## Center the CRS score
    crs_c <- scale(crs_dt$crs, center = T, scale = F)
    ## Center the covariates
    covar_dt <- test_dt[,..covar_idx] %>% 
      .[, sex := ifelse(sex == 'Female', 1, 0)]
    covar_dt_c <- scale(covar_dt, center = T, scale = F)
    
    test_dt_crs <- cbind(target_ds_dt_c, crs_c, covar_dt_c) %>% as.data.frame()
  }
  
  colnames(test_dt_crs)[1:2] <- c("target_ds_status", "crs")
  #colnames(test_dt_crs0)[1:2] <- c("target_ds_status", "crs")
  
  test_dt_crs_predictors <- test_dt_crs[,-1]
  #test_dt_crs_predictors0 <- test_dt_crs0[,-1]
  
  ## Read trained model coefficients
  crs_coef_est <- train_results$crs_coef_est
  #crs_model <- train_results$crs_model
  
  ## Obtain predicted target disease probability for CRS + sex + bmi model
  crs_target_ds_pred <- as.matrix(test_dt_crs_predictors) %*% crs_coef_est[-1] + crs_coef_est[1]
  # crs_target_ds_pred0 <- predict(crs_model, 
  #                                newdata = test_dt_crs_predictors0,
  #                                type = 'response')
  # Alternative code for prediction - see above
  
  
  ## Obtain predicted target disease probability for sex + bmi model
  test_dt_other_predictors <- test_dt_crs[,-c(1,2)]
  model_ref1_coef_est <- train_results$model_ref1_coef_est
  model_ref1_target_ds_pred <- as.matrix(test_dt_other_predictors) %*% model_ref1_coef_est[-1] + model_ref1_coef_est[1]
  
  ## Get true outcome status
  Y <- test_dt$target_ds_status
  mean_Y <- mean(Y)
  
  ## Report ROC metrics
  crs_roc <- roc(Y ~ crs_target_ds_pred)
  model_ref1_roc <- roc(Y ~ model_ref1_target_ds_pred)
  
  # like.coordinates <- coords(crs_roc, 
  #                            c(-Inf, sort(my.cutoff), Inf), 
  #                            input="threshold", 
  #                            ret=c("specificity", "sensitivity"))
  
  ## Return predicted target disease probabilities of the models and performance metrics
  return(list(crs_roc = crs_roc, 
              model_ref1_roc = model_ref1_roc))
}



## Variation of the test_pred_models() function for diagnostic purpose
test_pred_models_ReportPred <- function(test_dt, target_ds, 
                                       target_ds_prev,
                                       all_multiple_regression, 
                                       train_results,
                                       start_age, end_age, 
                                       control_case_ratio = 1,
                                       center_XY = F) {
  
  ## Get all predictor diseases needed for the target disease
  pred_info <- all_multiple_regression %>% 
    filter(target_disease == target_ds, age_window_start == start_age)
  pred_ds_vec <- pred_info$predictor_ds %>% as.character()
  pred_num <- length(pred_ds_vec)
  pred_coef <- pred_info$b_multiple ## note: need to use joint effect sizes
  
  ## Center the predictor diseases in training dataset
  pred_idx <- 4:(pred_num + 3)  ## double-check the indices to account for "time" variable
  pred_ds_dt_c <- scale(test_dt[,..pred_idx], center = T, scale = F)
  covar_idx <- (pred_num + 4):ncol(test_dt)
  
  ## Construct a variable for CRS: linear combination of disease status and regression coefficients
  crs_dt <- data.table(pred_ds_dt_c) %>%
    .[, Map("*", .SD, pred_coef)] %>%
    .[, crs := rowSums(.SD)]
  
  
  if (center_XY == F) {
    test_dt_crs <- cbind(test_dt$target_ds_status, crs_dt$crs, test_dt[,..covar_idx]) %>%
      .[, sex := ifelse(sex == 'Female', 1, 0)]
    # test_dt_crs0 <- cbind(test_dt$target_ds_status, crs_dt$crs, test_dt[,..covar_idx]) %>%
    #  .[, sex := factor(sex, levels = c('Male', 'Female'))]
  } else {
    ## Center the outcome in testing dataset
    target_ds_dt_c <- scale(test_dt$target_ds_status, center = T, scale = F)
    ## Center the CRS score
    crs_c <- scale(crs_dt$crs, center = T, scale = F)
    ## Center the covariates
    covar_dt <- test_dt[,..covar_idx] %>% 
      .[, sex := ifelse(sex == 'Female', 1, 0)]
    covar_dt_c <- scale(covar_dt, center = T, scale = F)
    
    test_dt_crs <- cbind(target_ds_dt_c, crs_c, covar_dt_c) %>% as.data.frame()
  }
  
  colnames(test_dt_crs)[1:2] <- c("target_ds_status", "crs")
  #colnames(test_dt_crs0)[1:2] <- c("target_ds_status", "crs")
  
  test_dt_crs_predictors <- test_dt_crs[,-1]
  #test_dt_crs_predictors0 <- test_dt_crs0[,-1]
  
  ## Read trained model coefficients
  crs_coef_est <- train_results$crs_coef_est
  #crs_model <- train_results$crs_model
  
  ## Obtain predicted target disease probability for CRS + sex + bmi model
  crs_target_ds_pred <- as.matrix(test_dt_crs_predictors) %*% crs_coef_est[-1] + crs_coef_est[1]
  # crs_target_ds_pred0 <- predict(crs_model, 
  #                                newdata = test_dt_crs_predictors0,
  #                                type = 'response')
  # Alternative code for prediction - see above
  
  
  ## Obtain predicted target disease probability for sex + bmi model
  test_dt_other_predictors <- test_dt_crs[,-c(1,2)]
  model_ref1_coef_est <- train_results$model_ref1_coef_est
  model_ref1_target_ds_pred <- as.matrix(test_dt_other_predictors) %*% model_ref1_coef_est[-1] + model_ref1_coef_est[1]
  
  ## Get true outcome status
  Y <- test_dt$target_ds_status
  mean_Y <- mean(Y)

  pred_result = list(crs_target_ds_pred = crs_target_ds_pred, 
                 model_ref1_target_ds_pred = model_ref1_target_ds_pred, 
                 Y = Y)
  
  ## Return predicted target disease probabilities of the models and performance metrics
  return(pred_result)
}


## Obtain R^2 on liability scale based on observed-scale R^2
## Reference: A Better Coefficient of Determination for Genetic Profile Analysis by Lee et. al. - Equation (8) and Appendix B
LiabR2 <- function(K, R2_obs){
  
  # Assume follow Normal distribution
  # The threshold on the normal distribution which truncates the proportion of disease prevalence
  thd <- -qnorm(K, 0, 1)
  zv <- dnorm(thd)
  
  # Convert using equation (8) of Lee
  R2_liab <- R2_obs * K * (1-K) / zv^2
  
  return(R2_liab)
}


## Obtain R^2 on liability scale based on observed-scale R^2, with case-control correction
## Reference: A Better Coefficient of Determination for Genetic Profile Analysis by Lee et. al. - Equation (15) and Appendix B
## K: population prevalence
## P: proportion of cases in the case-control samples
LiabR2_cc <- function(K, P, R2_obs){
  # Assume follow Normal distribution
  # The threshold on the normal distribution which truncates the proportion of disease prevalence
  thd <- -qnorm(K, 0, 1)
  zv <- dnorm(thd) #z (normal density)
  mv = zv/K  #mean liability for case
  
  # Convert using equation (15) of Lee
  theta = mv*(P-K)/(1-K)*(mv*(P-K)/(1-K)-thd) #theta in equation (15)
  cv = K*(1-K)/zv^2*K*(1-K)/(P*(1-P)) #C in equation (15)
  R2_liab = R2_obs*cv/(1+R2_obs*theta*cv)
  
  return(R2_liab)
}

# from marginal effect and correlation matrix to reconstruct multiple Rsquare
marginal2multiple_regression <- function(target_summstaz, cor_matrix){
  # keep only one of any pair of predictors that has correlation above 0.9 
  remove_highly_correlated_ds <- cor_matrix %>% filter(PopulationCorrelation > 0.9, disease1 != disease2) %>%
    left_join(select(target_summstaz, predictor_ds, predictor_prevalence), by = c("disease1" = "predictor_ds"))%>%
    left_join(select(target_summstaz, predictor_ds, predictor_prevalence), by = c("disease2" = "predictor_ds")) %>%
    mutate(remove_ds = if_else(predictor_prevalence.x < predictor_prevalence.y, disease1, disease2)) %>%
    pull(remove_ds)
  remove_highly_correlated_ds <- unique(remove_highly_correlated_ds)
  if(length(remove_highly_correlated_ds) > 0){
    print(paste("removing predictors", remove_highly_correlated_ds, "as they are highly correlated"))
    target_summstaz <- target_summstaz %>%
      filter(!(predictor_ds %in% remove_highly_correlated_ds))
    cor_matrix <- cor_matrix %>%
      filter(!(disease1 %in% remove_highly_correlated_ds), !(disease2 %in% remove_highly_correlated_ds))
  }
  predictor_order <- target_summstaz %>%
    pull(predictor_ds)
  # compute covariance matrix (X^T)X
  XTX_entries <- expand.grid(x_axis = predictor_order, y_axis = predictor_order) %>%
    left_join(select(cor_matrix, disease1, disease2, PopulationCorrelation), by = c("x_axis" = "disease1", "y_axis" = "disease2")) %>%
    left_join(select(target_summstaz, -target_disease, - age_window_start, - target_prevalence), by = c("x_axis" = "predictor_ds")) %>%
    left_join(select(target_summstaz, -target_disease, - age_window_start, - target_prevalence), by = c("y_axis" = "predictor_ds")) %>%
    mutate(xtx = PopulationCorrelation * sample_size.x * sqrt((predictor_prevalence.x - predictor_prevalence.x^2) * (predictor_prevalence.y - predictor_prevalence.y^2)))
  # covariance matrix  
  xtx_matrix <- XTX_entries %>% 
    select(xtx) %>%
    as.matrix() %>% 
    matrix(ncol = length(predictor_order)) 
  # diagnal matrix of predictor variance
  D_matrix <- diag(diag(xtx_matrix))
  # variance of target
  # PL: the formula below applies when Y is binary
  # yty <- target_summstaz$sample_size[1] *  (target_summstaz$target_prevalence[1] - target_summstaz$target_prevalence[1]^2)
  # PL: the formula below only applies when a fixed residual variance is assumed across individuals
  yty <- mean(diag(D_matrix) * ( (target_summstaz$sample_size[1] - 1) * target_summstaz$standard_error^2 +  target_summstaz$effect_size^2))
  # marginal effect size  
  beta_hat <- target_summstaz %>%
    mutate(predictor_ds =  factor(predictor_ds, levels = predictor_order)) %>%
    arrange(predictor_ds) %>%
    pull(effect_size)
  # compute the R_square
  R_sqaure <- 1/yty * (t(beta_hat) %*% D_matrix %*% solve(xtx_matrix) %*% D_matrix %*% beta_hat)
  # compute the multiple effects
  b_multiple <- solve(xtx_matrix) %*% D_matrix %*% beta_hat
  # PL Q: To check definition of per predictor variance - just calculate R^2 assuming the only predictor is that disease?
  per_disease_variance = 1/yty * b_multiple * diag(D_matrix) * b_multiple
  # compute standard error of the estimate
  # PL: SEs need update - use sandwich estimator
  var_e <- (1-R_sqaure) * yty/(target_summstaz$sample_size[1] - length(b_multiple))
  se_b_multiple <- sqrt(diag(solve(xtx_matrix) * var_e[1,1]))
  
  b_multiple <- tibble(predictor_ds = predictor_order, b_multiple = b_multiple[,1], se_b_multiple = se_b_multiple, per_disease_variance = per_disease_variance[,1])
  output_data <- target_summstaz %>%
    left_join(b_multiple, by="predictor_ds") %>%
    mutate(multiple_b_P = (1-pnorm(abs(b_multiple/se_b_multiple)))*2)
  return(list(R_sqaure[1,1], output_data))
}

## Function to compute sample M_e from a vector of effect estimates, without centering in calculating kurtosis
Me_compute <- function(effec_size_vec) {
  # numerator of kappa (normalized kurtosis)
  k_num <- mean(effec_size_vec^4)
  # denominator of kappa (normalized kurtosis)
  k_denom <- (mean(effec_size_vec^2))^2
  k <- k_num/k_denom
  M <- length(effec_size_vec)
  Me <- 3*M/k
  return(Me)
}

## Function to compute sample M_e from a vector of effect estimates, with centering in calculating kurtosis
Me_compute_center <- function(effec_size_vec) {
  # numerator of kappa (normalized kurtosis)
  k_num <- mean((effec_size_vec - mean(effec_size_vec))^4)
  # denominator of kappa (normalized kurtosis)
  k_denom <- (mean((effec_size_vec - mean(effec_size_vec))^2))^2
  k <- k_num/k_denom
  M <- length(effec_size_vec)
  ## unbiased estimator of kurtosis
  # References: https://uk.mathworks.com/help/stats/kurtosis.html#description, https://www.jstor.org/stable/2988433?seq=2 
  k1 <- (M-1) / ( (M-2) * (M-3) ) * ( (M+1) * k - 3 * (M-1) ) + 3
  Me <- 3*M/k1
  return(list(Me = Me, k1 = k1))
}

## Test
M <- 346
n_sim <- 1000
Me_vec <- rep(NA, n_sim)
k_vec <- rep(NA, n_sim)

for (i in 1:n_sim) {
test_vec <- rnorm(n = M, mean = 0, sd = 1)
Me_res <- Me_compute_center(test_vec)
Me_vec[i] <- Me_res$Me
k_vec[i] <- Me_res$k1
}

mean(Me_vec)
mean(k_vec)

## Function to plot joint effect sizes (unscaled and scaled) for each disease/age window
## Plot the phenotype names based on certain thresholds on effect sizes/p-values
plot_effect_size <- function(age_starts,   # age window start
                             target_ds,    # target disease (phecode)
                             effec_size_thres,  # threshold to display predictor name - original effect size
                             effec_size_scaledX_thres, # threshold to display predictor name - X-scaled effect size
                             effec_size_scaledXY_thres, # threshold to display predictor name - XY-scaled effect size
                             fig_width = 12,
                             fig_height = 5
) {
  target_pheno <- phe_phecode$phenotype[phe_phecode$phecode == target_ds]
  ds_idx <- which(all_age_ds_Me$target_ds == target_ds)
  reg_results_age_ds <- all_multiple_regression %>% 
    filter(age_window_start == age_starts & target_disease == target_ds) %>%
    mutate(predictor_ds = factor(predictor_ds, 
                                 levels = ds_list$diag_icd10[1:348], 
                                 ordered = T))

  fig_title <- paste0(phe_phecode$phenotype[which(phe_phecode$phecode == target_ds)], ", age ", age_starts, "-", age_starts + 10)
  plt_orig <- ggplot(reg_results_age_ds) +
    geom_point(aes(x = predictor_ds, y = b_multiple)) + 
    geom_label_repel(data = filter(reg_results_age_ds, 
                                   b_multiple > effec_size_thres, 
                                   P_fdr < 0.05), 
                     aes(label=phenotype, x = predictor_ds, y = b_multiple)) +
    geom_abline(intercept = effec_size_thres, slope = 0, linetype = "dotted") +
    theme(
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank()) +
    labs(x="Predictor diseases", y="Effect sizes - unscaled", 
         title=fig_title)
  
  plt_scaledX <- ggplot(reg_results_age_ds) +
    geom_point(aes(x = predictor_ds, y = b_multiple_scaledX)) + 
    geom_label_repel(data = filter(reg_results_age_ds, 
                                   b_multiple_scaledX > effec_size_scaledX_thres,
                                   P_fdr < 0.05), 
                     aes(label=phenotype, x = predictor_ds, y = b_multiple_scaledX)) +
    geom_abline(intercept = effec_size_scaledX_thres, slope = 0, linetype = "dotted") +
    theme(
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank()) +
    labs(x="Predictor diseases", y="Effect sizes - scaled in X", 
         title=fig_title)
  
  
  plt_scaledXY <- ggplot(reg_results_age_ds) +
    geom_point(aes(x = predictor_ds, y = b_multiple_scaledXY)) + 
    geom_label_repel(data = filter(reg_results_age_ds, 
                                   b_multiple_scaledXY > effec_size_scaledXY_thres,
                                   P_fdr < 0.05), 
                     aes(label=phenotype, x = predictor_ds, y = b_multiple_scaledXY)) +
    geom_abline(intercept = effec_size_scaledXY_thres, slope = 0, linetype = "dotted") +
    theme(
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank()) +
    labs(x="Predictor diseases", y="Effect sizes - scaled in X and Y", 
         title=fig_title)
  
  
  ggsave(filename = paste0("./Figures/CVD_COVID_UK_joint_effect_sizes_orig_", age_starts, "_", target_pheno,".jpg"), 
         plt_orig, width = fig_width, height = fig_height, limitsize = FALSE)
  
  ggsave(filename = paste0("./Figures/CVD_COVID_UK_joint_effect_sizes_scaledX_", age_starts, "_", target_pheno,".jpg"), 
         plt_scaledX, width = fig_width, height = fig_height, limitsize = FALSE)
  
  ggsave(filename = paste0("./Figures/CVD_COVID_UK_joint_effect_sizes_scaledXY_", age_starts, "_", target_pheno,".jpg"), 
         plt_scaledXY, width = fig_width, height = fig_height, limitsize = FALSE)

}



## Function to plot standardized effect sizes (in both X and Y) for each disease/age window
## Only plot the phenotype name of top contributing predictors (based on effect sizes)
## Color predictors based on significance
plot_effect_size2 <- function(age_starts,   # age window start
                             target_ds,    # target disease (phecode)
                             effec_size_thres,  # threshold to display predictor name - original effect size
                             effec_size_scaledX_thres, # threshold to display predictor name - X-scaled effect size
                             effec_size_scaledXY_thres, # threshold to display predictor name - XY-scaled effect size
                             top_ds_num = 20,   # number of top predictors to label
                             y_lim,   # y axis limit (vector)
                             labeled = T,   # whether to label top predictors
                             fig_width = 12,
                             fig_height = 5
) {
  target_pheno <- phe_phecode$phenotype[phe_phecode$phecode == target_ds]
  ds_idx <- which(all_age_ds_Me$target_ds == target_ds)
  reg_results_age_ds <- all_multiple_regression %>% 
    filter(age_window_start == age_starts & target_disease == target_ds) %>%
    mutate(predictor_ds = factor(predictor_ds, 
                                 levels = ds_list$diag_icd10[1:348], 
                                 ordered = T),
           color_sig = ifelse(multiple_b_P_fdr < 0.05, "Significant", "Non-signficant")) %>%
    mutate(color_sig = factor(color_sig, levels = c('Significant', 'Non-significant')))
                                   
  
  fig_title <- paste0(phe_phecode$phenotype[which(phe_phecode$phecode == target_ds)], ", age ", age_starts, "-", age_starts + 10)
  pts_size <- 3
  # plt_orig <- ggplot(reg_results_age_ds) +
  #   geom_point(aes(x = predictor_ds, y = b_multiple, color = color_sig),
  #              size = pts_size) + 
  #   #geom_abline(intercept = effec_size_thres, slope = 0, linetype = "dotted") +
  #   theme_bw() +
  #   theme(
  #     legend.position="none",
  #     panel.border = element_blank(),
  #     panel.grid.major.x = element_blank(),
  #     panel.grid.minor.x = element_blank(),
  #     axis.text.x=element_blank(),
  #     axis.ticks.x=element_blank()) +
  #   scale_color_manual(values = c("red","black")) +
  #   ylim(y_lim[1], y_lim[2]) + 
  #   labs(x="Predictor diseases", y="Effect sizes - unscaled", 
  #        title=fig_title)
  # 
  # plt_scaledX <- ggplot(reg_results_age_ds) +
  #   geom_point(aes(x = predictor_ds, y = b_multiple_scaledX, color = color_sig),
  #              size = pts_size) + 
  #   #geom_abline(intercept = effec_size_scaledX_thres, slope = 0, linetype = "dotted") +
  #   theme_bw() +
  #   theme(
  #     legend.position="none",
  #     panel.border = element_blank(),
  #     panel.grid.major.x = element_blank(),
  #     panel.grid.minor.x = element_blank(),
  #     axis.text.x=element_blank(),
  #     axis.ticks.x=element_blank()) +
  #   scale_color_manual(values = c("red","black")) +
  #   ylim(y_lim[1], y_lim[2]) + 
  #   labs(x="Predictor diseases", y="Effect sizes - scaled in X", 
  #        title=fig_title)
  # 
  
  plt_scaledXY <- ggplot(reg_results_age_ds) +
    geom_point(aes(x = predictor_ds, y = b_multiple_scaledXY, color = color_sig),
               size = pts_size) + 
    #geom_abline(intercept = effec_size_scaledXY_thres, slope = 0, linetype = "dotted") +
    theme_bw() +
    theme(
      legend.position="none",
      panel.border = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank()) +
    scale_color_manual(values = c("red","black")) +
    ylim(y_lim[1], y_lim[2]) + 
    labs(x="Predictor diseases", y="Standardized effect sizes",
         title = fig_title)
  
  if (labeled == T) {
    # plt_orig <- plt_orig + geom_label_repel(data = reg_results_age_ds[which.maxn(reg_results_age_ds$b_multiple, top_ds_num),], 
    #                                         aes(label=phenotype, x = predictor_ds, y = b_multiple),
    #                                         max.overlaps = 20)
    # plt_scaledX <- plt_scaledX + geom_label_repel(data = reg_results_age_ds[which.maxn(reg_results_age_ds$b_multiple_scaledX, top_ds_num),], 
    #                  aes(label=phenotype, x = predictor_ds, y = b_multiple_scaledX),
    #                  max.overlaps = 20)
    top_ds_idx <- which.maxn(abs(reg_results_age_ds$b_multiple_scaledXY), top_ds_num)
    plt_scaledXY <- plt_scaledXY + geom_label_repel(data = reg_results_age_ds[top_ds_idx,], 
                     aes(label=phenotype, x = predictor_ds, y = b_multiple_scaledXY),
                     max.overlaps = 20)
  }
  
  
  # ggsave(filename = paste0("./Figures/CVD_COVID_UK_joint_effect_sizes_orig_", age_starts, "_", target_pheno, "_labeled",
  #                          as.numeric(labeled), ".jpg"), 
  #        plt_orig, width = fig_width, height = fig_height, limitsize = FALSE)
  # 
  # ggsave(filename = paste0("./Figures/CVD_COVID_UK_joint_effect_sizes_scaledX_", age_starts, "_", target_pheno, "_labeled",
  #                          as.numeric(labeled), ".jpg"), 
  #        plt_scaledX, width = fig_width, height = fig_height, limitsize = FALSE)
  
  ggsave(filename = paste0("./Figures/CVD_COVID_UK_joint_effect_sizes_scaledXY_", age_starts, "_", target_pheno, "_labeled",
                           as.numeric(labeled), ".jpg"), 
         plt_scaledXY, width = fig_width, height = fig_height, limitsize = FALSE)
  
}

