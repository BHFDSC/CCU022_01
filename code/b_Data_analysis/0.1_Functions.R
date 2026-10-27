library(tidyverse)
library(survival)

## Function to split a column of numeric string into a matrix of single-digit numbers
str_to_num <- function(str_vec, colnames_vec) {
  a1 <- str_vec %>% as.matrix()
  a2 <- strsplit(a1, "")
  a3 <- unlist(a2) %>% as.numeric()
  a4 <- matrix(a3, nrow=dim(a1)[1], byrow=T) %>% as.data.table()
  colnames(a4) <- colnames_vec
  return(a4)
}

## Function to calculate IPCW weights for an age group
## The weight distribution will be estimated separately for each age at index (e.g., 40, 41, ...)
## Output (wide version): columns: age at index; rows:year of follow-up; cells: IPCW weights
## Adapted from: https://github.com/docvock/JBI_IPCW_for_ML/blob/master/IPCW_analysis.R
ipcw_wts <- function(target_ds_surv_dt, start_age, end_age, fup_yrs, 
                     target_ds_colname){
  num_age <- end_age - start_age + 1
  ipcw_dt <- data.frame('yr_of_fup' = 0:fup_yrs,
                        matrix(NA, fup_yrs+1, num_age))
  colnames(ipcw_dt)[2:ncol(ipcw_dt)] <- start_age:end_age
  for (i in 1:num_age) {
    age_i <- start_age + i - 1
    ## Filter to each age in the age group
    surv_dt_i <- target_ds_surv_dt %>% 
      .[age_at_index >= age_i & age_at_index < age_i + 1]
    ## Kaplan-Meier analysis on time to censoring
    km_model <- survfit(Surv(days_to_ds, 1-ds_status) ~ 1, data = surv_dt_i)
    fup_yrs_vec <- 0:fup_yrs
    fup_days_vec <- floor((fup_yrs_vec) * 365.25)
    ## Get probability of being censored after each year of follow-up of interest
    G_t <- sapply(1:length(fup_yrs_vec), function(pos) {
      if ( fup_yrs_vec[pos] < fup_yrs) {
        km_model$surv[min(which(km_model$time > fup_days_vec[pos]))]
      } else {
        ## all pts are censored at end of study period, use the second last probability
        km_model$surv[min(which(km_model$time >= tail(km_model$time, 2)[1]))]
      }
    })
    ## Get IPCW weights
    ipcw_dt[,i+1] <- 1/G_t
  }
  ## Transform from wide to long
  ipcw_dt_long <- ipcw_dt %>%
    pivot_longer(!yr_of_fup, 
                 names_to = 'age_at_index',
                 values_to = 'ipcw_wts') %>%
    mutate(target_ds = target_ds_colname) %>%
    mutate(age_at_index = as.numeric(age_at_index))
  
  return(ipcw_dt_long)
}



## Example function to get prediction results in testing data using training models: (1) CRS + covariates, (2) covariates only
## Return prediction performance metrics, including:
## AUROC, C-index, prediction R^2 -- applies to fixed case-control ratio
## AUPRC -- applies to entire testing data
## Input: 
## test_dt_final1: testing dataset with 4:1 control-case ratio
## test_dt_final2: testing dataset with original prevalence preserved

test_pred_models_example <- function(test_dt_final1, 
                             test_dt_final2,
                             target_ds_prev,
                             train_coef_dt,
                             model_type,
                             liabR_wtd = F) {
  
  ## Get regression coefficient estimate
  if (!(model_type %in% c('logistic_calib', 'linear_calib'))) {
      train_coef_est <- train_coef_dt$Est
      
      ## Get variable names
      var_names <- rownames(train_coef_dt)[-1]
      
      ## Get testing data for listed variable names
      test_dt1_analysis <- test_dt_final1[, ..var_names]
      test_dt2_analysis <- test_dt_final2[, ..var_names]
  } else {
    train_coef_est_crs <- train_coef_dt$crs_summ$Est
    train_coef_est <- train_coef_dt$calib_summ$Est
    
    ## Get predictor ds names
    ds_names <- rownames(train_coef_dt$crs_summ)
    
    ## Get testing data for listed ds names
    test_dt1_ds <- test_dt_final1[, ..ds_names]
    test_dt2_ds <- test_dt_final2[, ..ds_names]
    
    ## Get CRS score
    crs1 <- as.matrix(test_dt1_ds) %*% train_coef_est_crs %>% as.data.table()
    
    crs2 <- as.matrix(test_dt2_ds) %*% train_coef_est_crs %>% as.data.table()
    
    if (model_type == 'logistic_calib') {
        colnames(crs1) <- 'CRS'
        colnames(crs2) <- 'CRS'
    } else if (model_type == 'linear_calib') {
        colnames(crs1) <- 'CRS_linear'
        colnames(crs2) <- 'CRS_linear'
    }
    
    test_dt_final1 <- test_dt_final1 %>% 
      .[, colnames(crs1) := crs1]
    test_dt_final2 <- test_dt_final2 %>% 
      .[, colnames(crs2) := crs2]
    
    ## Get variable names in calibration model
    var_names <- rownames(train_coef_dt$calib_summ)[-1]
    
    ## Get testing data for listed variable names in calibration model
    test_dt1_analysis <- test_dt_final1[, ..var_names]
    test_dt2_analysis <- test_dt_final2[, ..var_names]
  }
  
  ## Obtain predicted target disease probability based on the training model
  if (model_type == 'linear' | model_type == 'linear_calib') {
      target_ds_pred1 <- as.matrix(test_dt1_analysis) %*% train_coef_est[-1] + train_coef_est[1]
      target_ds_pred2 <- as.matrix(test_dt2_analysis) %*% train_coef_est[-1] + train_coef_est[1]
  } else if (model_type == 'logistic' | model_type == 'logistic_calib' ) {
    
      link_pred1 <- as.matrix(test_dt1_analysis) %*% train_coef_est[-1] + train_coef_est[1]
      link_pred2 <- as.matrix(test_dt2_analysis) %*% train_coef_est[-1] + train_coef_est[1]
    
      exp_beta1 <- exp(-link_pred1)
      target_ds_pred1 <- 1/(1+exp_beta1)
      
      exp_beta2 <- exp(-link_pred2)
      target_ds_pred2 <- 1/(1+exp_beta2)
  } 
  
  ## Get true outcome status
  Y1 <- test_dt_final1$target_ds_status
  mean_Y1 <- mean(Y1)
  
  Y2 <- test_dt_final2$target_ds_status
  mean_Y2 <- mean(Y2)
  
  ## Report prediction R^2 on the observed scale
  ## For logistic regression, this equals Efron's R^2
  pred_R2_obs <- cor(target_ds_pred1, Y1)^2
    
  ## Report prediction R^2 on the liability scale
  if (model_type == 'linear' | model_type == 'linear_calib') {
      pred_R2_liab <- LiabR2_cc(K = target_ds_prev, P = mean_Y1, 
                            R2_obs = pred_R2_obs)
  } else if (model_type == 'logistic' | model_type == 'logistic_calib') {
      pred_R2_liab <- LiabR2_logistic(K = target_ds_prev, 
                                      P = mean_Y1, 
                                      link_pred = link_pred1, 
                                      Y = Y1, 
                                      wtd  = liabR_wtd)
  }
    
  ## Report AUROC
  auroc <- auc(Y1 ~ target_ds_pred1)[[1]]
    
  ## Report C-index
  ## Reference: https://www.sciencedirect.com/science/article/pii/S1532046420301246#b0075
  cindex <- Cindex(risk = target_ds_pred1,
                   times = test_dt_final1$target_ds_tte,
                   delta = Y1)
  
  ## Report AUPRC
  auprc <- pr.curve(scores.class0 = target_ds_pred2, 
                    weights.class0 = Y2)$auc.integral
    
  ## Report the difference between AUPRC and target disease prevalence
  auprc_prev_diff <- auprc - target_ds_prev
    
  ## Store the metrics
  pred_metrics <- c(pred_R2_obs, pred_R2_liab, auroc, cindex, auprc,
                    auprc_prev_diff)
  names(pred_metrics) <- c('R2_obs', 'R2_liab', 'AUROC', 'C_index',
                           'AUPRC', 'AUPRC_prev_diff')
  
  ## Return predicted target disease probabilities of the models and performance metrics
  return(pred_metrics)
}


## Function to get prediction results in testing data using training models: (1) CRS + covariates, (2) covariates only
## Return prediction performance metrics, including:
## AUROC, C-index, prediction R^2 -- applies to fixed case-control ratio
## AUPRC -- applies to testing data with original case-control ratio
## Input: 
## test_dt1: testing dataset with 4:1 control-case ratio
## test_dt2: testing dataset with original prevalence preserved
test_pred_models <- function(test_dt1, 
                             test_dt2,
                             target_ds_incid,
                             target_summ) {
  
  ## Get regression coefficient estimate
  train_coef_est <- target_summ$Est
  
  ## Get variable names
  var_names <- rownames(target_summ)[-1]
  
  ## Get testing data for listed variable names
  test_dt1_analysis <- test_dt1[, ..var_names]
  test_dt2_analysis <- test_dt2[, ..var_names]
  
  ## Obtain predicted target disease probability based on the training logistic model
  link_pred1 <- as.matrix(test_dt1_analysis) %*% train_coef_est[-1] + train_coef_est[1]
  link_pred2 <- as.matrix(test_dt2_analysis) %*% train_coef_est[-1] + train_coef_est[1]
  
  exp_beta1 <- exp(-link_pred1)
  target_ds_pred1 <- 1/(1+exp_beta1)
  
  exp_beta2 <- exp(-link_pred2)
  target_ds_pred2 <- 1/(1+exp_beta2)
  
  ## Get true outcome status
  Y1 <- test_dt1$target_ds_status
  mean_Y1 <- mean(Y1)
  
  Y2 <- test_dt2$target_ds_status
  mean_Y2 <- mean(Y2)
  
  ## Report prediction R^2 on the observed scale
  ## For logistic regression, this equals Efron's R^2
  pred_R2_obs <- cor(target_ds_pred1, Y1)^2
  
  ## Report prediction R^2 on the liability scale, account for case-control sampling
  R2_obs_link <- cor(link_pred1, Y1)^2
  pred_R2_liab <- LiabR2_cc(K = target_ds_incid, 
                            P = mean_Y1, 
                            R2_obs = R2_obs_link)
  
  ## Report AUROC
  auroc <- auc(Y1 ~ target_ds_pred1)[[1]]
  
  ## Report C-index
  ## Reference: https://www.sciencedirect.com/science/article/pii/S1532046420301246#b0075
  ## Note: the first argument should be survival probability
  c_index <- cindex(matrix(1 - target_ds_pred1),
                    formula = Surv(target_ds_tte, target_ds_status) ~ 1,
                    data = test_dt1)$AppCindex$matrix
  
  ## Report AUPRC
  auprc <- pr.curve(scores.class0 = target_ds_pred2, 
                    weights.class0 = Y2)$auc.integral
  
  ## Report the difference between AUPRC and target disease prevalence
  auprc_prev_diff <- auprc - target_ds_incid
  
  ## Store the metrics
  pred_metrics <- c(pred_R2_obs, pred_R2_liab, 
                    auroc, c_index, 
                    auprc, auprc_prev_diff)
  names(pred_metrics) <- c("R2_obs", "R2_liab",
                           "AUROC", "C_index",
                           "AUPRC", "AUPRC_prev_diff")
  
  ## Return predicted target disease probabilities of the models and performance metrics
  return(pred_metrics)
}


## Function to get prediction results in testing data using QCovid2 model
## Return prediction performance metrics, including:
## AUROC, C-index, observed prediction R^2 -- applies to fixed case-control ratio
## AUPRC -- applies to testing data with original case-control ratio
## Input: 
## test_dt1: testing dataset with 4:1 control-case ratio
## test_dt2: testing dataset with original prevalence preserved
## survivor_idx: index of time points for baseline survivor function (default is 241 days)
test_pred_models_qcovid <- function(qcovid_test_dt1, 
                                    qcovid_test_dt2,
                                    target_ds_incid,
                                    qcovid_summ_female,
                                    qcovid_summ_male,
                                    survivor_idx = 6) {
  
  ## Order data by PID
  qcovid_test_dt1 <- qcovid_test_dt1[order(PID)]
  qcovid_test_dt2 <- qcovid_test_dt2[order(PID)]
  
  ## Get predicted outcome risk
  target_ds_pred1 <- qcovid_calc(qcovid_test_dt1, 
                                 qcovid_summ_female,
                                 qcovid_summ_male,
                                 survivor_idx)
  
  target_ds_pred2 <- qcovid_calc(qcovid_test_dt2, 
                                 qcovid_summ_female,
                                 qcovid_summ_male,
                                 survivor_idx)
  
  ## Get true outcome status
  Y1 <- qcovid_test_dt1$target_ds_status
  mean_Y1 <- mean(Y1)
  
  Y2 <- qcovid_test_dt2$target_ds_status
  mean_Y2 <- mean(Y2)
  
  ## Report prediction R^2 on the observed scale
  pred_R2_obs <- cor(target_ds_pred1, Y1)^2
  
  ## Report AUROC
  auroc <- auc(Y1 ~ target_ds_pred1)[[1]]
  
  ## Report C-index
  ## Reference: https://www.sciencedirect.com/science/article/pii/S1532046420301246#b0075
  ## Note: the first argument should be survival probability
  c_index <- cindex(matrix(1 - target_ds_pred1),
                    formula = Surv(target_ds_tte, target_ds_status) ~ 1,
                    data = qcovid_test_dt1)$AppCindex$matrix
  
  ## Report AUPRC
  auprc <- pr.curve(scores.class0 = target_ds_pred2, 
                    weights.class0 = Y2)$auc.integral
  
  ## Report the difference between AUPRC and target disease prevalence
  auprc_prev_diff <- auprc - target_ds_incid
  
  ## Store the metrics
  pred_metrics <- c(pred_R2_obs, 
                    # pred_R2_liab, 
                    auroc, c_index, 
                    auprc, auprc_prev_diff)
  names(pred_metrics) <- c("R2_obs", 
                           # "R2_liab",
                           "AUROC", "C_index",
                           "AUPRC", "AUPRC_prev_diff")
  
  ## Return predicted target disease probabilities of the models and performance metrics
  return(pred_metrics)
}


### Function to calculate QCOVID2 score given covariates data
## Ref: https://www.qcovid.org/Home/Algorithm
## survivor_idx: index of time points for baseline survivor function
qcovid_calc <- function(compare_test_df_qcovid, 
                        qcovid_summ_female,
                        qcovid_summ_male,
                        survivor_idx = 6) {
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
  
  return(target_ds_pred_qcovid)
  
}

### Function to calculate CRS given covariates data
CRS_calc <- function(diag_dt,
                     crs_summ) {
  
  ### Not including age + sex when calculating CRS
  ## Get variable names and coef
  var_idx1 <- grep("^ds", rownames(crs_summ))
  var_names1 <- grep("^ds", rownames(crs_summ), value = T)
  var_est1 <- crs_summ$Est[var_idx1]
  
  ## Get data for listed variable names
  diag_dt_analysis1 <- diag_dt[, ..var_names1]
  
  ## Obtain predicted target disease probabilities based on the CRS model
  link_pred1 <- as.matrix(diag_dt_analysis1) %*% var_est1
  
  exp_beta1 <- exp(-link_pred1)
  target_ds_pred1 <- 1/(1+exp_beta1)
    
  ### Calculate risk based on CRS + Age + Sex
  ## Get variable names and coef
  var_names2 <- rownames(crs_summ)[-1]
  var_est2 <- crs_summ$Est
  
  ## Get data for listed variable names
  diag_dt_analysis2 <- diag_dt[, ..var_names2]
  
  ## Obtain predicted target disease probabilities based on the CRS model
  link_pred2 <- as.matrix(diag_dt_analysis2) %*% var_est2[-1] + var_est2[1]
  
  exp_beta2 <- exp(-link_pred2)
  target_ds_pred2 <- 1/(1+exp_beta2)
  
  ## Output updated dataset
  diag_dt_new <- diag_dt %>% 
    .[, grep("^ds", colnames(diag_dt)) := NULL] %>%
    .[, CRS_risk_no_covar := target_ds_pred1] %>%
    .[, CRS_risk_with_covar := target_ds_pred2] %>%
    .[, CRS_link_no_covar := link_pred1] %>%
    .[, CRS_link_with_covar := link_pred2]
  
  return(diag_dt_new)
  
}

### Function to calculate PheRS score given predictor data
### Output original PheRS risk and de-standardized PheRS risk
PheRS_calc <- function(diag_dt,
                       phers_coef_summ,
                       target_ds_name,
                       target_ds_sex,
                       data_type = "train") {
  
  target_ds_coef <- phers_coef_summ %>%
    filter(Disease == target_ds_name)
  
  ## Get variable names and coef
  var_names <- target_ds_coef$Predictor
  var_est <- target_ds_coef$UKB_coef
  var_est_destd <- target_ds_coef$UKB_coef_destd
  
  ## Get data for listed variable names
  diag_dt_analysis <- diag_dt[, ..var_names]
  
  ## Obtain predicted target disease probabilities based on the PheRS model
  link_pred <- as.matrix(diag_dt_analysis) %*% var_est
  exp_beta <- exp(-link_pred)
  target_ds_pred <- 1/(1+exp_beta)
  
  link_pred2 <- as.matrix(diag_dt_analysis) %*% var_est_destd
  exp_beta2 <- exp(-link_pred2)
  target_ds_pred2 <- 1/(1+exp_beta2)
  
  ## Output updated dataset
  
  if (data_type == "train") {
    if (target_ds_sex %in% c("Female", "Male")) {
      diag_dt_new <- diag_dt %>% 
        .[, c("age_at_2015", "Female", "target_ds_status", 
              "ipcw_wts", "ipcw_wts_sf")] %>%
        .[, PheRS_risk := target_ds_pred] %>%
        .[, PheRS_destd_risk := target_ds_pred2]
    } else {
      diag_dt_new <- diag_dt %>%
        .[, c("age_at_2015", "Female", "target_ds_status", 
              "ipcw_wts")] %>%
        .[, PheRS_risk := target_ds_pred] %>%
        .[, PheRS_destd_risk := target_ds_pred2]
    }
  } else if (data_type == "test") {
      diag_dt_new <- diag_dt %>% 
        .[, c("age_at_2015", "Female", "target_ds_status", 
              "target_ds_tte")] %>%
        .[, PheRS_risk := target_ds_pred] %>%
        .[, PheRS_destd_risk := target_ds_pred2]
  }
  
  return(diag_dt_new)
  
}


## Obtain R^2 on liability scale based on observed-scale R^2
## Reference: A Better Coefficient of Determination for Genetic Profile Analysis by Lee et. al. - Equation (8) and Appendix B
## K_hat: estimated population prevalence
LiabR2 <- function(K_hat, R2_obs){
  # Assume follow Normal distribution
  # The threshold on the normal distribution which truncates the proportion of disease prevalence
  thd <- -qnorm(K_hat, 0, 1)
  zv <- dnorm(thd) #z (normal density)
  
  # Convert using equation (8) of Lee
  R2_liab = R2_obs * K_hat * (1 - K_hat)/zv^2
  
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


## Obtain McKelvey and Zavoina's R^2
## Reference: https://github.com/easystats/performance/blob/main/R/r2_mckelvey.R
LiabR2_logistic <- function(K, P, link_pred, Y, wtd  = F){
  if (wtd == F) {
      var_pred <- var(link_pred)
  } else {
      wv <- (1-P)*K/(P*(1-K))
      set.seed(2026)
      vr <- runif(length(link_pred),0,1) #uniform random values
      vsel <- link_pred[Y == 0 | vr < wv] #select controls and a proportion (wv) of cases
      var_pred <- var(vsel)
  }
  var_epsilon <- pi^2 / 3
  R2_liab <- var_pred/(var_pred + var_epsilon)
  return(R2_liab)
}


## Calculate C-index:
## Reference: https://rdrr.io/cran/nftbart/man/Cindex.html
Cindex=function(risk, times, delta=NULL)
{   
  N=length(risk)
  if(N!=length(times))
    stop('risk and times must be the same length')
  if(length(delta)==0) delta=rep(1, N)
  else if(N!=length(delta))
    stop('risk and delta must be the same length')
  
  l=0
  k=0
  for(i in 1:N) {
    h=which((times[i]==times & delta[i]>delta) |
              (times[i]<times & delta[i]>0))
    if(length(h)>0) {
      l=l+sum(risk[i]>risk[h])
      k=k+length(h)
    }
  }
  return(l/k)
}

## Adapted from: https://github.com/hongooi73/glmnetUtils/blob/master/R/cvaGlmnetFormula.r
minlossplot_ReturnValue <- function(x, ..., cv.type=c("1se", "min"))
{
  alpha <- x$alpha
  cv.type <- match.arg(cv.type)
  cv.type <- paste0("lambda.", cv.type)
  cvm <- sapply(x$modlist, function(mod) {
    mod$cvm[mod$lambda == mod[[cv.type]]]
  })
  return(cvm)
}


Me_compute_noncenter_simple <- function(effec_size_vec) {
  # numerator of kappa (normalized kurtosis)
  k_num <- mean(effec_size_vec^4)
  # denominator of kappa (normalized kurtosis)
  k_denom <- (mean(effec_size_vec^2))^2
  k <- k_num/k_denom
  M <- length(effec_size_vec)
  Me <- 3*M/k
  return(list(Me = Me, k = k))
}

Me_compute_center_simple <- function(effec_size_vec) {
  # numerator of kappa (normalized kurtosis)
  k_num <- mean((effec_size_vec - mean(effec_size_vec))^4)
  # denominator of kappa (normalized kurtosis)
  k_denom <- (mean((effec_size_vec - mean(effec_size_vec))^2))^2
  k <- k_num/k_denom
  M <- length(effec_size_vec)
  Me <- 3*M/k
  return(list(Me = Me, k = k))
}

Me_compute_center_debias1 <- function(effec_size_vec) {
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
  return(list(Me = Me, k = k1))
}


Me_compute_center_debias2 <- function(effec_size_vec) {
  # numerator of kappa (normalized kurtosis)
  k_num <- mean((effec_size_vec - mean(effec_size_vec))^4)
  # denominator of kappa (normalized kurtosis)
  k_denom <- (mean((effec_size_vec - mean(effec_size_vec))^2))^2
  k <- k_num/k_denom
  M <- length(effec_size_vec)
  ## unbiased estimator of kurtosis
  # References: https://uk.mathworks.com/help/stats/kurtosis.html#description, https://www.jstor.org/stable/2988433?seq=2 
  k1 <- (M-1) / ( (M-2) * (M-3) ) * ( (M+1) * k - 3 * (M-1) ) + 3
  
  ## Further debias 1/k1
  # Reference: https://www.sciencedirect.com/science/article/pii/016771528790040X
  mu1 <- sample_central_moment(effec_size_vec, 1) 
  mu2 <- sample_central_moment(effec_size_vec, 2) 
  mu3 <- sample_central_moment(effec_size_vec, 3) 
  mu4 <- sample_central_moment(effec_size_vec, 4) 
  mu5 <- sample_central_moment(effec_size_vec, 5)
  mu6 <- sample_central_moment(effec_size_vec, 6)
  mu8 <- sample_central_moment(effec_size_vec, 8)
  
  M_var_k1 <- 1/mu2^6 * (mu8*mu2^2 - mu4^2*mu2^2 - 8*mu5*mu3*mu2^2 + 
                           16*mu2^3*mu3^2 - 4*mu4*mu6*mu2 - 32*mu2^2*mu1*mu3*mu4 +
                           8*mu5*mu1*mu2*mu4 + 16*mu3^2*mu2*mu4 + 4*mu4^3 +
                           16*mu2*mu1^2*mu4^2 - 16*mu1*mu3*mu4^2)
  
  Me <- 3*M/k1 - 3*M_var_k1/k1^3
  return(list(Me = Me, k = k1))
}

sample_central_moment <- function(x_vec, p) {
  return(mean((x_vec - mean(x_vec))^p))
}

## Function to plot standardized effect sizes (in both X and Y) for each disease/age window
## Only plot the phenotype name of top contributing predictors (based on effect sizes)
## Color predictors based on significance
plot_effect_size <- function(target_ds_coef_dt,
                             model_type,
                              age_starts,   # age window start
                              target_ds,    # target disease (phecode)
                              effec_size_thres = NULL,  # threshold to display predictor name - original effect size
                              effec_size_scaledX_thres = NULL, # threshold to display predictor name - X-scaled effect size
                              effec_size_scaledXY_thres = NULL, # threshold to display predictor name - XY-scaled effect size
                              top_ds_num = 10,   # number of top predictors to label
                              y_lim,   # y axis limit (vector)
                              labeled = T,   # whether to label top predictors
                              fig_width = 12,
                              fig_height = 5
) {
  target_pheno <- phecode_info$Phenotype[phecode_info$PheCode == target_ds]
  category_sort <- target_ds_coef_dt$Category %>% unique()
  reg_results_age_ds <- target_ds_coef_dt %>% 
    mutate(PheCode_parent = factor(PheCode_parent, 
                                 levels = sort(as.numeric(PheCode_parent)), 
                                 ordered = T)) %>%
    mutate(Significance = ifelse(pval_fdr < 0.05, "Significant", "Non-signficant")) %>%
    mutate(Significance = factor(Significance, levels = c("Significant", "Non-signficant"))) %>%
    mutate(Category = factor(Category, levels = category_sort))
  
  
  fig_title <- paste0(target_pheno, ", age group ", age_starts, "-", age_starts + 9)
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
  
  plt_scaledX <- ggplot(reg_results_age_ds, aes(shape = Significance)) +
    geom_point(aes(x = PheCode_parent, y = Est_scaleX, color = Category),
               size = pts_size) + 
    #geom_abline(intercept = effec_size_scaledXY_thres, slope = 0, linetype = "dotted") +
    theme_bw() +
    theme(
   #   legend.position="none",
      panel.border = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank()) +
    scale_color_manual(values = c("red","cyan", "coral", "yellow2", "green", "blue",
                                  "orange", "purple", "deepskyblue",  "gold", "violet",
                                  "tan", "deeppink", "seagreen1", "brown",  "grey40")) +
    scale_shape_manual(values = c('circle', 'circle open')) + 
    ylim(y_lim[1], y_lim[2]) + 
    expand_limits(x= c(-1, nrow(reg_results_age_ds) + 2)) +
    labs(x="Predictor diseases", y="Standardized effect sizes",
         title = fig_title)
  
  if (labeled == T) {
    # plt_orig <- plt_orig + geom_label_repel(data = reg_results_age_ds[which.maxn(reg_results_age_ds$b_multiple, top_ds_num),], 
    #                                         aes(label=phenotype, x = predictor_ds, y = b_multiple),
    #                                         max.overlaps = 20)
    # plt_scaledX <- plt_scaledX + geom_label_repel(data = reg_results_age_ds[which.maxn(reg_results_age_ds$b_multiple_scaledX, top_ds_num),], 
    #                  aes(label=phenotype, x = predictor_ds, y = b_multiple_scaledX),
    #                  max.overlaps = 20)
    top_ds_idx <- sort(abs(reg_results_age_ds$Est_scaleX), 
                       decreasing = T, index.return=T)$ix[1:top_ds_num]
    plt_scaledX <- plt_scaledX + geom_label_repel(data = reg_results_age_ds[top_ds_idx,], 
                                                    aes(label=Phenotype_parent, x = PheCode_parent, 
                                                        y = Est_scaleX),
                                                    max.overlaps = 20)
  }
  
  
  # ggsave(filename = paste0("./Figures/CVD_COVID_UK_joint_effect_sizes_orig_", age_starts, "_", target_pheno, "_labeled",
  #                          as.numeric(labeled), ".jpg"), 
  #        plt_orig, width = fig_width, height = fig_height, limitsize = FALSE)
  # 
  # ggsave(filename = paste0("./Figures/CVD_COVID_UK_joint_effect_sizes_scaledX_", age_starts, "_", target_pheno, "_labeled",
  #                          as.numeric(labeled), ".jpg"), 
  #        plt_scaledX, width = fig_width, height = fig_height, limitsize = FALSE)
  
  if (model_type == 'linear') {
    ggsave(filename = paste0(processed_data_dir, 
                             "/Figures/CVD_COVID_UK_linear_joint_effect_sizes_scaledX_Age", 
                             age_starts, "_", target_pheno, "_labeled",
                             as.numeric(labeled), ".jpg"), 
           plt_scaledX, width = fig_width, height = fig_height, limitsize = FALSE)
  } else {
      ggsave(filename = paste0(processed_data_dir, 
                               "/Figures/CVD_COVID_UK_logistic_joint_effect_sizes_scaledX_Age", 
                               age_starts, "_", target_pheno, "_labeled",
                               as.numeric(labeled), ".jpg"), 
             plt_scaledX, width = fig_width, height = fig_height, limitsize = FALSE)
  }
  
}
