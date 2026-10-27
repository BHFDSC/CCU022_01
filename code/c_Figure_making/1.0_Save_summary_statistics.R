### Author: Pearl Liu
### For each outcome, read and save summary statistics
### Index: 2015-01-01, follow-up: 10 years (all outcomes)
### Index: 2020-01-01, follow-up: 5 years (COVID hospitalization)


rm(list = ls())
gc()

#### Load libraries ####
library(data.table)
library(tidyverse)
#library(sandwich)
#library(pROC)
#library(pec)
#library(PRROC)

#source('~/collab/CCU022/CCU022_CRS/0.1_Functions.R')

#### Set parameters ####
read_dir <- 'Outputs/'
summary_dir <- 'Summary_Stats/'
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
n_mc <- 100  ## PL 20260120: update from 200 to 100
## Control-case ratio in testing data for evaluating AUROC, C-index, R2
control_case_ratio_test <- 4
## Number of cases used in testing data
n_cases_test1 <- 3000
n_cases_test2 <- 1000

#### Read processed data ####
##### UPDATE TO LATEST: List and prevalence of predictors and outcomes #####
pred_ds_prev2015_name = "WhiteBritish_OddYOB_Index2015_PredictorDiseasePrevalence.csv"
pred_ds_prev2020_name = "WhiteBritish_OddYOB_Index2020_PredictorDiseasePrevalence.csv"
outcome_ds_prev_name = "TrainData_Outcome_Incidence_OddYOB_Index2015_Followup10yr.csv"

pred_ds_prev2015 = read.csv(paste0(read_dir, pred_ds_prev2015_name)) %>%
  mutate(Predictor_name = paste0('ds', Predictor_phecode))
pred_ds_prev2020 = read.csv(paste0(read_dir, pred_ds_prev2020_name)) %>%
  mutate(Predictor_name = paste0('ds', Predictor_phecode))

## Combine 2015 predictor prevalence (all other outcomes) with 2020 predictor prevlence (covid outcome)
all(pred_ds_prev2015$Predictor_name == pred_ds_prev2020$Predictor_name)
pred_ds_prev <- pred_ds_prev2015 %>%
  mutate(Prevalence2020 = pred_ds_prev2020$Prevalence)

outcome_ds_prev0 = read.csv(paste0(read_dir, outcome_ds_prev_name))
covid_prev <- data.frame("Outcome_phecode" = "COVID_hspt",
                         "Incidence_general" = 0.01027544,
                         "Incidence_sf" = NA)

outcome_ds_prev <- outcome_ds_prev0 %>%
  ## Remove phecode 622, as it corresponds to the same ICD10 codes as 622.1
  filter(Outcome_phecode != "622") %>%
  rbind(covid_prev) %>%
  mutate(Incidence_combined = if_else(is.na(Incidence_sf), Incidence_general, 
                                      Incidence_sf))

n_pred_ds <- nrow(pred_ds_prev)
n_outcome_ds <- nrow(outcome_ds_prev)



##### Phecode information #####
phe_icd10_codelist_name = "phecode_icd10_map_with_parent_info_AllPheCodes.csv"
icd10_phecode_map <- read.csv(paste0("Inputs/For SDE/", 
                                    phe_icd10_codelist_name))
phecode_info <- icd10_phecode_map %>%
  select(PHECODE, phenotype, category, sex) %>%
  distinct() %>%
  mutate(Outcome_phecode = str_replace(PHECODE, '\\.', '_'))

phecode_parent_info <- icd10_phecode_map %>%
  select(PHECODE_parent, phenotype_parent, category) %>%
  distinct() %>%
  mutate(Predictor_name = paste0("ds", PHECODE_parent))


#### Read and save general prediction performance metrics ####
##### UPDATE TO LATEST: Read downloaded data from SDE #####
## NEED TO UPDATE RESULTS FROM ETHNICITIES - UPDATED 20260304
## Note that phecode 622 should be already removed from the following datasets in SDE
all_outcome_metrics_dt_final_wb <- read.csv(paste0(read_dir,
                                                "All_Outcomes+COVID_PredictionMetrics_ControlCase4to1_WhiteBritish_Index2015_Followup10yr.csv"))
all_outcome_metrics_dt_final_ow <- read.csv(paste0(read_dir,
                                                   "All_Outcomes+COVID_PredictionMetrics_ControlCase4to1_OtherWhite_Index2015_Followup10yr.csv"))
all_outcome_metrics_dt_final_asian <- read.csv(paste0(read_dir,
                                                   "All_Outcomes+COVID_PredictionMetrics_ControlCase4to1_Asian_Index2015_Followup10yr.csv"))
all_outcome_metrics_dt_final_black <- read.csv(paste0(read_dir,
                                                   "All_Outcomes+COVID_PredictionMetrics_ControlCase4to1_Black_Index2015_Followup10yr.csv"))
all_outcome_metrics_dt_final_mixed <- read.csv(paste0(read_dir,
                                                   "All_Outcomes+COVID_PredictionMetrics_ControlCase4to1_Mixed_Index2015_Followup10yr.csv"))

all_outcome_metrics_dt_final_list <- list(all_outcome_metrics_dt_final_wb,
                                          all_outcome_metrics_dt_final_ow,
                                          all_outcome_metrics_dt_final_asian,
                                          all_outcome_metrics_dt_final_black,
                                          all_outcome_metrics_dt_final_mixed)

all_outcome_train_R2_dt <- read.csv(paste0(read_dir,
                                           "All_Outcomes+COVID_TrainingR2_ControlCase4to1_WhiteBritish_Index2015_Followup10yr.csv"))

##### Loop through each ethnicity #####
eth_name_vec <- c("WhiteBritish", "OtherWhite", "Asian", "Black", "Mixed")
metrics_names <-  c("R2_obs", "R2_liab","AUROC", "C_index", "AUPRC", "AUPRC_prev_diff")

for (l in 2:length(eth_name_vec)) {
  
  eth_name <- eth_name_vec[l]
  all_outcome_metrics_dt_final <- all_outcome_metrics_dt_final_list[[l]]
  
  ##### Add 95% CI upper and lower bounds for absolute metrics #####
  n_metrics <- length(metrics_names)
  for (k in 1:n_metrics) {
    metric_k <- metrics_names[k]
    metric_k_mean <- all_outcome_metrics_dt_final[, paste0(metric_k, "_mean")]
    if (eth_name == "WhiteBritish") {
      metric_k_se <- all_outcome_metrics_dt_final[, paste0(metric_k, "_SD")]/sqrt(n_mc)
    } else {
      metric_k_se <- all_outcome_metrics_dt_final[, paste0(metric_k, "_SD")]
    }
    all_outcome_metrics_dt_final[, paste0(metric_k, "_SE")] <- metric_k_se
    all_outcome_metrics_dt_final[, paste0(metric_k, "_LB")] <- metric_k_mean - qnorm(0.975) * metric_k_se
    all_outcome_metrics_dt_final[, paste0(metric_k, "_UB")] <- metric_k_mean + qnorm(0.975) * metric_k_se
    all_outcome_metrics_dt_final[, paste0(metric_k, "_Pval")] <- 2*pnorm(abs(metric_k_mean/metric_k_se),
                                                                                   lower.tail = F)
  }
  
  ##### Calculate 95% CI and p-value for difference between model of interest and age+sex model #####
  all_outcome_metrics_delta_dt <- all_outcome_metrics_dt_final[, c("Outcome_phecode",
                                                             "Model", 
                                                             paste0("Delta_", metrics_names, "_mean"))]
  all_outcome_metrics_delta_dt[, paste0("Delta_", metrics_names, "_SE")] <- NA
  all_outcome_metrics_delta_dt[, paste0("Delta_", metrics_names, "_LB")] <- NA
  all_outcome_metrics_delta_dt[, paste0("Delta_", metrics_names, "_UB")] <- NA
  all_outcome_metrics_delta_dt[, paste0("Delta_", metrics_names, "_Pval")] <- NA
  
  ## Loop through each metric
  n_metrics <- length(metrics_names)
  for (k in 1:n_metrics) {
    metric_k <- metrics_names[k]
    delta_k_mean <- all_outcome_metrics_dt_final[, paste0("Delta_", metric_k, "_mean")]
    if (eth_name == "WhiteBritish") {
      delta_k_se <- all_outcome_metrics_dt_final[, paste0("Delta_", metric_k, "_SD")]/sqrt(n_mc)
    } else {
      delta_k_se <- all_outcome_metrics_dt_final[, paste0("Delta_", metric_k, "_SD")]
    }
    all_outcome_metrics_delta_dt[, paste0("Delta_", metric_k, "_SE")] <- delta_k_se
    all_outcome_metrics_delta_dt[, paste0("Delta_", metric_k, "_LB")] <- delta_k_mean - qnorm(0.975) * delta_k_se
    all_outcome_metrics_delta_dt[, paste0("Delta_", metric_k, "_UB")] <- delta_k_mean + qnorm(0.975) * delta_k_se
    all_outcome_metrics_delta_dt[, paste0("Delta_", metric_k, "_Pval")] <- 2*pnorm(abs(delta_k_mean/delta_k_se),
                                                                                   lower.tail = F)
  }
  
  ##### Check number of significant differences and average difference for each metric #####
  ## Loop through each model and metric
  n_pos_diff_dt <- data.frame("Model" = model_name_vec)
  n_pos_diff_dt[, metrics_names] <- NA
  
  n_neg_diff_dt <- n_pos_diff_dt
  
  avg_diff_dt <- n_pos_diff_dt
  avg_metric_dt <- n_pos_diff_dt
  avg_perc_diff_dt <- n_pos_diff_dt
  
  age_sex_avg_metrics <- all_outcome_metrics_dt_final %>%
    filter(Outcome_phecode != "Avg") %>%
    filter(Model == "Age_Sex") %>%
    select(paste0(metrics_names, "_mean")) %>%
    colMeans()
  
  for (j in 1:n_models) {
    model_j <- model_name_vec[j]
    model_j_metrics_delta <- all_outcome_metrics_delta_dt %>%
      filter(Outcome_phecode != "Avg") %>%
      filter(Model == model_j)
    model_j_metrics <- all_outcome_metrics_dt_final %>%
      filter(Outcome_phecode != "Avg") %>%
      filter(Model == model_j)
    for (k in 1:n_metrics) {
      metric_k <- metrics_names[k]
      delta_k_mean <- model_j_metrics_delta[, paste0("Delta_", metric_k, "_mean")]
      delta_k_pval <- model_j_metrics_delta[, paste0("Delta_", metric_k, "_Pval")]
      
      metric_k_mean <- model_j_metrics[, paste0(metric_k, "_mean")]
      
      n_pos_diff_dt[j, metric_k] <- sum(delta_k_mean > 0 & delta_k_pval < 0.05)
      n_neg_diff_dt[j, metric_k] <- sum(delta_k_mean < 0 & delta_k_pval < 0.05)
      
      avg_metric_dt[j, metric_k] <- mean(metric_k_mean)
      avg_diff_dt[j, metric_k] <- mean(delta_k_mean)
      avg_perc_diff_dt[j, metric_k] <- mean(delta_k_mean)/age_sex_avg_metrics[k] * 100
    }
    
  }
  
  ## Calculate percentage of positive/negative differences among outcomes
  perc_pos_diff_dt <- n_pos_diff_dt %>%
    mutate_at(metrics_names, function(x){x/n_outcome_ds*100})
  perc_neg_diff_dt <- n_neg_diff_dt %>%
    mutate_at(metrics_names, function(x){x/n_outcome_ds*100})
  
  
  
  #View(avg_metric_dt)
  
  #View(avg_diff_dt)
  
  #View(avg_perc_diff_dt)
  
  #View(n_pos_diff_dt[,-7])
  #View(n_neg_diff_dt[,-7])
  
  ##### Save all summary statistics #####
  final_summ_stats <- list(all_outcome_metrics_dt = all_outcome_metrics_dt_final,
                           all_outcome_metrics_delta_dt = all_outcome_metrics_delta_dt,
                           all_outcome_train_R2_dt = all_outcome_train_R2_dt,
                           avg_metric_dt = avg_metric_dt,
                           avg_diff_dt = avg_diff_dt,
                           avg_perc_diff_dt = avg_perc_diff_dt,
                           n_pos_diff_dt = n_pos_diff_dt,
                           n_neg_diff_dt = n_neg_diff_dt)
  
  saveRDS(final_summ_stats, paste0(summary_dir, 
                                   "Sex_Specific_All_Outcomes+COVID_PredictionMetrics_ControlCase",
                                   control_case_ratio, "to1_", eth_name, "_Index2015_Followup10yr.rds"))

}

#### Modify risk ratio summ stats ####
rr_summ <- read.csv(paste0(read_dir, 
                           "All_outcomes_Risk_ratio_with_CI_NeedUpdate.csv"))

rr_outcome_ds_vec <- rep(sort(outcome_ds_prev$Outcome_phecode),
                         each = 2)

rr_summ$Outcome_phecode <- rr_outcome_ds_vec

write.csv(rr_summ, paste0(read_dir, "All_outcomes_Risk_ratio_with_CI.csv"))

#### UPDATE TO LATEST: Read and save CRS effect sizes (training data) ####
all_outcome_summ <- read.csv(paste0(read_dir,
                                        "All_Outcomes_WeightedLogisticRegression_SummStats_ControlCase4to1_WhiteBritish_OddYOB_Index2015_Followup10yr.csv"))
covid_summ <- read.csv(paste0(read_dir,
                              "COVID_hspt_WeightedLogisticRegression_SummStats_ControlCase4to1_WhiteBritish_OddYOB_Index2020_Followup5yr.csv")) %>%
  mutate(Outcome = "COVID_hspt") %>%
  select(Outcome, Predictor, Est, SE)


##### Format dataframe #####
# ncol_pt1 <- ncol(all_outcome_summ_pt1)/3
# ncol_pt2 <- ncol(all_outcome_summ_pt2)/3
# 
# all_outcome_summ_pt1_list <- vector(mode = 'list', length = ncol_pt1)
# all_outcome_summ_pt2_list <- vector(mode = 'list', length = ncol_pt2)
# 
# for (i in 1:ncol_pt1) {
#   all_outcome_summ_pt1_list[[i]] <- all_outcome_summ_pt1[,(3*i-2):(3*i)] %>%
#     na.omit()
#   colnames(all_outcome_summ_pt1_list[[i]]) <- c("Outcome_Predictor", "Est", "SE")
# }
# 
# for (i in 1:ncol_pt2) {
#   all_outcome_summ_pt2_list[[i]] <- all_outcome_summ_pt2[,(3*i-2):(3*i)] %>%
#     na.omit()
#   colnames(all_outcome_summ_pt2_list[[i]]) <- c("Outcome_Predictor", "Est", "SE")
# }
# 
# 
# all_outcome_summ_pt1_long <- do.call(rbind, all_outcome_summ_pt1_list)
# all_outcome_summ_pt2_long <- do.call(rbind, all_outcome_summ_pt2_list)
# 
all_outcome_summ <- all_outcome_summ %>%
  separate_wider_delim(cols = "Outcome_Predictor", 
                       names = c("Outcome", "Predictor"),
           delim = "+") %>%
  rbind(covid_summ)

## Add predictor phecode info
all_outcome_summ_final <- all_outcome_summ %>% 
  left_join(phecode_parent_info, by = join_by(Predictor == Predictor_name)) %>%
  left_join(pred_ds_prev, by = join_by(Predictor == Predictor_name)) %>%
  select(!c(PHECODE_parent, Predictor_phecode)) %>%
  rename(Predictor_prev = Prevalence,
         Predictor_prev2020 = Prevalence2020,
         Predictor_phenotype = phenotype_parent,
         Predictor_category = category)

##### Save all outcomes' effect sizes #####
saveRDS(all_outcome_summ_final, paste0(summary_dir,
                                       "All_Outcomes+COVID_WeightedLogisticRegression_SummStats_ControlCase4to1_WhiteBritish_OddYOB_Index2015_Followup10yr.rds"))


#### UPDATE TO LATEST: Read and save CRS effect sizes (full cohort) ####
all_outcome_summ_full <- read.csv(paste0(read_dir,
                                    "All_Outcomes_WeightedLogisticRegression_SummStats_ControlCase4to1_WhiteBritish_FullData_Index2015_Followup10yr.csv"))
covid_summ_full <- read.csv(paste0(read_dir,
                              "COVID_hspt_WeightedLogisticRegression_SummStats_ControlCase4to1_WhiteBritish_FullData_Index2020_Followup5yr.csv")) %>%
  mutate(Outcome = "COVID_hspt") %>%
  select(Outcome, Predictor, Est, SE)


##### Format dataframe #####
all_outcome_summ_full <- all_outcome_summ_full %>%
  separate_wider_delim(cols = "Outcome_Predictor", 
                       names = c("Outcome", "Predictor"),
                       delim = "+") %>%
  rbind(covid_summ_full)

## Add predictor phecode info
all_outcome_summ_full_final <- all_outcome_summ_full %>% 
  left_join(phecode_parent_info, by = join_by(Predictor == Predictor_name)) %>%
  left_join(pred_ds_prev, by = join_by(Predictor == Predictor_name)) %>%
  select(!c(PHECODE_parent, Predictor_phecode)) %>%
  rename(Predictor_prev = Prevalence,
         Predictor_prev2020 = Prevalence2020,
         Predictor_phenotype = phenotype_parent,
         Predictor_category = category)

##### Save all outcomes' effect sizes #####
saveRDS(all_outcome_summ_full_final, paste0(summary_dir,
                                       "All_Outcomes+COVID_WeightedLogisticRegression_SummStats_ControlCase4to1_WhiteBritish_FullData_Index2015_Followup10yr.rds"))



#### UPDATE TO LATEST: Read and save CRS effect sizes (calibrated) ####
all_outcome_summ_calib <- read.csv(paste0(read_dir,
                                         "All_Outcomes_CalibratedCRS_SummStats_WhiteBritish_OddYOB_Index2015_Followup10yr.csv"))
covid_summ_calib <- read.csv(paste0(read_dir,
                                   "COVID_hspt_CalibratedCRS_SummStats_WhiteBritish_OddYOB_Index2020_Followup5yr.csv")) %>%
  mutate(Outcome = "COVID_hspt") %>%
  select(Outcome, Predictor, Est, SE)


##### Format dataframe #####
all_outcome_summ_calib <- all_outcome_summ_calib %>%
  separate_wider_delim(cols = "Outcome_Predictor", 
                       names = c("Outcome", "Predictor"),
                       delim = "+") %>%
  rbind(covid_summ_calib)

## Add predictor phecode info
all_outcome_summ_calib_final <- all_outcome_summ_calib %>% 
  left_join(phecode_parent_info, by = join_by(Predictor == Predictor_name)) %>%
  left_join(pred_ds_prev, by = join_by(Predictor == Predictor_name)) %>%
  select(!c(PHECODE_parent, Predictor_phecode)) %>%
  rename(Predictor_prev = Prevalence,
         Predictor_prev2020 = Prevalence2020,
         Predictor_phenotype = phenotype_parent,
         Predictor_category = category)

##### Save all outcomes' effect sizes #####
saveRDS(all_outcome_summ_calib_final, paste0(summary_dir,
                                            "All_Outcomes+COVID_CalibratedCRS_SummStats_WhiteBritish_OddYOB_Index2015_Followup10yr.rds"))



#### Read and save predictor correlation matrix ####
pred_cor_long_name <- "PredictorDiseaseCorrelation_WhiteBritish_OddYOB_Index2015.csv"
pred_cor_long <- read.csv(paste0(read_dir, pred_cor_long_name))
pred_ds_vec <- c(unique(pred_cor_long$Pred_ds1), 994)
## Transform from long to matrix format
pred_cor_mt <- matrix(NA, M, M)
colnames(pred_cor_mt) <- as.character(pred_ds_vec)
rownames(pred_cor_mt) <- as.character(pred_ds_vec)
for (p1 in 1:M) {
  for (p2 in 1:M) {
    pred_ds1 <- pred_ds_vec[p1]
    pred_ds2 <- pred_ds_vec[p2]
    if (pred_ds1 != pred_ds2) {
      corr_v1 <- pred_cor_long %>%
        filter(Pred_ds1 == pred_ds1 & Pred_ds2 == pred_ds2) %>%
        pull(Corr)
      corr_v2 <- pred_cor_long %>%
        filter(Pred_ds1 == pred_ds2 & Pred_ds2 == pred_ds1) %>%
        pull(Corr)
      corr_final <- max(corr_v1, corr_v2)
      pred_cor_mt[p1, p2] <- corr_final
    } else if (pred_ds1 == pred_ds2) {
      pred_cor_mt[p1, p2] <- 1
    }
  }
}

saveRDS(pred_cor_mt, 
        paste0(summary_dir, "PredictorDiseaseCorrelationMatrix_WhiteBritish_OddYOB_Index2015.rds"))


#### Read and save CRS performance for Delphi comparison ####
##### UPDATE TO LATEST: Read downloaded data from SDE #####
all_outcome_metrics_dt_delphi <- read.csv(paste0(read_dir,
                                                   "ForDelphiComparison_Sex_Specific_All_Outcomes_PredictionMetrics_ControlCase4to1_WhiteBritish_IndexMid2020_Followup1yr.csv"))

##### Updated parameters #####
#eth_name_vec <- c("WhiteBritish", "Asian", "Black", "Mixed")
eval_type <- "Bootstrap"
metrics_names <-  c("R2_obs", "R2_liab","AUROC", "C_index", "AUPRC", "AUPRC_prev_diff")
model_name_vec <- c("CRS_all_predictors", 
                    "Age_Sex")
n_models <- length(model_name_vec)

#for (l in 1:length(eth_name_vec)) {

  all_outcome_metrics_dt_final <- all_outcome_metrics_dt_delphi
  
  ##### Add 95% CI upper and lower bounds for absolute metrics #####
  n_metrics <- length(metrics_names)
  for (k in 1:n_metrics) {
    metric_k <- metrics_names[k]
    metric_k_mean <- all_outcome_metrics_dt_final[, paste0(metric_k, "_mean")]
    if (eval_type == "MonteCarlo") {
      metric_k_se <- all_outcome_metrics_dt_final[, paste0(metric_k, "_SD")]/sqrt(n_mc)
    } else {
      metric_k_se <- all_outcome_metrics_dt_final[, paste0(metric_k, "_SD")]
    }
    all_outcome_metrics_dt_final[, paste0(metric_k, "_SE")] <- metric_k_se
    all_outcome_metrics_dt_final[, paste0(metric_k, "_LB")] <- metric_k_mean - qnorm(0.975) * metric_k_se
    all_outcome_metrics_dt_final[, paste0(metric_k, "_UB")] <- metric_k_mean + qnorm(0.975) * metric_k_se
    all_outcome_metrics_dt_final[, paste0(metric_k, "_Pval")] <- 2*pnorm(abs(metric_k_mean/metric_k_se),
                                                                         lower.tail = F)
  }
  
  ##### Calculate 95% CI and p-value for difference between model of interest and age+sex model #####
  all_outcome_metrics_delta_dt <- all_outcome_metrics_dt_final[, c("Outcome_phecode",
                                                                   "Model", 
                                                                   paste0("Delta_", metrics_names, "_mean"))]
  all_outcome_metrics_delta_dt[, paste0("Delta_", metrics_names, "_SE")] <- NA
  all_outcome_metrics_delta_dt[, paste0("Delta_", metrics_names, "_LB")] <- NA
  all_outcome_metrics_delta_dt[, paste0("Delta_", metrics_names, "_UB")] <- NA
  all_outcome_metrics_delta_dt[, paste0("Delta_", metrics_names, "_Pval")] <- NA
  
  ## Loop through each metric
  n_metrics <- length(metrics_names)
  for (k in 1:n_metrics) {
    metric_k <- metrics_names[k]
    delta_k_mean <- all_outcome_metrics_dt_final[, paste0("Delta_", metric_k, "_mean")]
    if (eval_type == "MonteCarlo") {
      delta_k_se <- all_outcome_metrics_dt_final[, paste0("Delta_", metric_k, "_SD")]/sqrt(n_mc)
    } else {
      delta_k_se <- all_outcome_metrics_dt_final[, paste0("Delta_", metric_k, "_SD")]
    }
    all_outcome_metrics_delta_dt[, paste0("Delta_", metric_k, "_SE")] <- delta_k_se
    all_outcome_metrics_delta_dt[, paste0("Delta_", metric_k, "_LB")] <- delta_k_mean - qnorm(0.975) * delta_k_se
    all_outcome_metrics_delta_dt[, paste0("Delta_", metric_k, "_UB")] <- delta_k_mean + qnorm(0.975) * delta_k_se
    all_outcome_metrics_delta_dt[, paste0("Delta_", metric_k, "_Pval")] <- 2*pnorm(abs(delta_k_mean/delta_k_se),
                                                                                   lower.tail = F)
  }
  
  ##### Check number of significant differences and average difference for each metric #####
  ## Loop through each model and metric
  n_pos_diff_dt <- data.frame("Model" = model_name_vec)
  n_pos_diff_dt[, metrics_names] <- NA
  
  n_neg_diff_dt <- n_pos_diff_dt
  
  avg_diff_dt <- n_pos_diff_dt
  avg_metric_dt <- n_pos_diff_dt
  avg_perc_diff_dt <- n_pos_diff_dt
  
  age_sex_avg_metrics <- all_outcome_metrics_dt_final %>%
    filter(Outcome_phecode != "Avg") %>%
    filter(Model == "Age_Sex") %>%
    select(paste0(metrics_names, "_mean")) %>%
    colMeans()
  
  for (j in 1:n_models) {
    model_j <- model_name_vec[j]
    model_j_metrics_delta <- all_outcome_metrics_delta_dt %>%
      filter(Outcome_phecode != "Avg") %>%
      filter(Model == model_j)
    model_j_metrics <- all_outcome_metrics_dt_final %>%
      filter(Outcome_phecode != "Avg") %>%
      filter(Model == model_j)
    for (k in 1:n_metrics) {
      metric_k <- metrics_names[k]
      delta_k_mean <- model_j_metrics_delta[, paste0("Delta_", metric_k, "_mean")]
      delta_k_pval <- model_j_metrics_delta[, paste0("Delta_", metric_k, "_Pval")]
      
      metric_k_mean <- model_j_metrics[, paste0(metric_k, "_mean")]
      
      n_pos_diff_dt[j, metric_k] <- sum(delta_k_mean > 0 & delta_k_pval < 0.05)
      n_neg_diff_dt[j, metric_k] <- sum(delta_k_mean < 0 & delta_k_pval < 0.05)
      
      avg_metric_dt[j, metric_k] <- mean(metric_k_mean)
      avg_diff_dt[j, metric_k] <- mean(delta_k_mean)
      avg_perc_diff_dt[j, metric_k] <- mean(delta_k_mean)/age_sex_avg_metrics[k] * 100
    }
    
  }
  
  ## Calculate percentage of positive/negative differences among outcomes
  perc_pos_diff_dt <- n_pos_diff_dt %>%
    mutate_at(metrics_names, function(x){x/n_outcome_ds*100})
  perc_neg_diff_dt <- n_neg_diff_dt %>%
    mutate_at(metrics_names, function(x){x/n_outcome_ds*100})
  
  
  
  #View(avg_metric_dt)
  
  #View(avg_diff_dt)
  
  #View(avg_perc_diff_dt)
  
  #View(n_pos_diff_dt[,-7])
  #View(n_neg_diff_dt[,-7])
  
  ##### Save all summary statistics #####
  final_summ_stats <- list(all_outcome_metrics_dt = all_outcome_metrics_dt_final,
                           all_outcome_metrics_delta_dt = all_outcome_metrics_delta_dt,
                          # all_outcome_train_R2_dt = all_outcome_train_R2_dt,
                           avg_metric_dt = avg_metric_dt,
                           avg_diff_dt = avg_diff_dt,
                           avg_perc_diff_dt = avg_perc_diff_dt,
                           n_pos_diff_dt = n_pos_diff_dt,
                           n_neg_diff_dt = n_neg_diff_dt)
  
  saveRDS(final_summ_stats, paste0(summary_dir, 
                                   "ForDelphiComparison_Sex_Specific_All_Outcomes+COVID_PredictionMetrics_ControlCase",
                                   control_case_ratio, "to1_WhiteBritish_IndexMid2020_Followup1yr.rds"))
  
#}


#### Read and save CRS vs PheRS comparison ####
##### UPDATE TO LATEST: Read downloaded data from SDE #####
all_outcome_metrics_dt_phers <- read.csv(paste0(read_dir,
                                                 "Sex_Specific_All_Outcomes_CRS_vs_PheRS_PredictionMetrics_ControlCase4to1_WhiteBritish_Index2015_Followup10yr.csv"))

##### Updated parameters #####
n_outcome_ds_phers <- 12
  
#eth_name_vec <- c("WhiteBritish", "Asian", "Black", "Mixed")
eval_type <- "MonteCarlo"
metrics_names <-  c("R2_obs", "R2_liab","AUROC", "C_index", "AUPRC", "AUPRC_prev_diff")
model_name_vec <- c("CRS_all_predictors",
                    "PheRS_age_sex",
                    "Age_Sex")
n_models <- length(model_name_vec)

#for (l in 1:length(eth_name_vec)) {

all_outcome_metrics_dt_final <- all_outcome_metrics_dt_phers

##### Add 95% CI upper and lower bounds for absolute metrics #####
n_metrics <- length(metrics_names)
for (k in 1:n_metrics) {
  metric_k <- metrics_names[k]
  metric_k_mean <- all_outcome_metrics_dt_final[, paste0(metric_k, "_mean")]
  if (eval_type == "MonteCarlo") {
    metric_k_se <- all_outcome_metrics_dt_final[, paste0(metric_k, "_SD")]/sqrt(n_mc)
  } else {
    metric_k_se <- all_outcome_metrics_dt_final[, paste0(metric_k, "_SD")]
  }
  all_outcome_metrics_dt_final[, paste0(metric_k, "_SE")] <- metric_k_se
  all_outcome_metrics_dt_final[, paste0(metric_k, "_LB")] <- metric_k_mean - qnorm(0.975) * metric_k_se
  all_outcome_metrics_dt_final[, paste0(metric_k, "_UB")] <- metric_k_mean + qnorm(0.975) * metric_k_se
  all_outcome_metrics_dt_final[, paste0(metric_k, "_Pval")] <- 2*pnorm(abs(metric_k_mean/metric_k_se),
                                                                       lower.tail = F)
}

##### Calculate 95% CI and p-value for difference between model of interest and age+sex model #####
all_outcome_metrics_delta_dt <- all_outcome_metrics_dt_final[, c("Outcome_phecode",
                                                                 "PheRS_outcome_name",
                                                                 "Model", 
                                                                 paste0("Delta_", metrics_names, "_mean"))]
all_outcome_metrics_delta_dt[, paste0("Delta_", metrics_names, "_SE")] <- NA
all_outcome_metrics_delta_dt[, paste0("Delta_", metrics_names, "_LB")] <- NA
all_outcome_metrics_delta_dt[, paste0("Delta_", metrics_names, "_UB")] <- NA
all_outcome_metrics_delta_dt[, paste0("Delta_", metrics_names, "_Pval")] <- NA

## Loop through each metric
n_metrics <- length(metrics_names)
for (k in 1:n_metrics) {
  metric_k <- metrics_names[k]
  delta_k_mean <- all_outcome_metrics_dt_final[, paste0("Delta_", metric_k, "_mean")]
  if (eval_type == "MonteCarlo") {
    delta_k_se <- all_outcome_metrics_dt_final[, paste0("Delta_", metric_k, "_SD")]/sqrt(n_mc)
  } else {
    delta_k_se <- all_outcome_metrics_dt_final[, paste0("Delta_", metric_k, "_SD")]
  }
  all_outcome_metrics_delta_dt[, paste0("Delta_", metric_k, "_SE")] <- delta_k_se
  all_outcome_metrics_delta_dt[, paste0("Delta_", metric_k, "_LB")] <- delta_k_mean - qnorm(0.975) * delta_k_se
  all_outcome_metrics_delta_dt[, paste0("Delta_", metric_k, "_UB")] <- delta_k_mean + qnorm(0.975) * delta_k_se
  all_outcome_metrics_delta_dt[, paste0("Delta_", metric_k, "_Pval")] <- 2*pnorm(abs(delta_k_mean/delta_k_se),
                                                                                 lower.tail = F)
}

##### Check number of significant differences and average difference for each metric #####
## Loop through each model and metric
n_pos_diff_dt <- data.frame("Model" = model_name_vec)
n_pos_diff_dt[, metrics_names] <- NA

n_neg_diff_dt <- n_pos_diff_dt

avg_diff_dt <- n_pos_diff_dt
avg_metric_dt <- n_pos_diff_dt
avg_perc_diff_dt <- n_pos_diff_dt

age_sex_avg_metrics <- all_outcome_metrics_dt_final %>%
  filter(Outcome_phecode != "Avg") %>%
  filter(Model == "Age_Sex") %>%
  select(paste0(metrics_names, "_mean")) %>%
  colMeans()

for (j in 1:n_models) {
  model_j <- model_name_vec[j]
  model_j_metrics_delta <- all_outcome_metrics_delta_dt %>%
    filter(Outcome_phecode != "Avg") %>%
    filter(Model == model_j)
  model_j_metrics <- all_outcome_metrics_dt_final %>%
    filter(Outcome_phecode != "Avg") %>%
    filter(Model == model_j)
  for (k in 1:n_metrics) {
    metric_k <- metrics_names[k]
    delta_k_mean <- model_j_metrics_delta[, paste0("Delta_", metric_k, "_mean")]
    delta_k_pval <- model_j_metrics_delta[, paste0("Delta_", metric_k, "_Pval")]
    
    metric_k_mean <- model_j_metrics[, paste0(metric_k, "_mean")]
    
    n_pos_diff_dt[j, metric_k] <- sum(delta_k_mean > 0 & delta_k_pval < 0.05)
    n_neg_diff_dt[j, metric_k] <- sum(delta_k_mean < 0 & delta_k_pval < 0.05)
    
    avg_metric_dt[j, metric_k] <- mean(metric_k_mean)
    avg_diff_dt[j, metric_k] <- mean(delta_k_mean)
    avg_perc_diff_dt[j, metric_k] <- mean(delta_k_mean)/age_sex_avg_metrics[k] * 100
  }
  
}

## Calculate percentage of positive/negative differences among outcomes
perc_pos_diff_dt <- n_pos_diff_dt %>%
  mutate_at(metrics_names, function(x){x/n_outcome_ds_phers*100})
perc_neg_diff_dt <- n_neg_diff_dt %>%
  mutate_at(metrics_names, function(x){x/n_outcome_ds_phers*100})



#View(avg_metric_dt)

#View(avg_diff_dt)

#View(avg_perc_diff_dt)

#View(n_pos_diff_dt[,-7])
#View(n_neg_diff_dt[,-7])

##### Save all summary statistics #####
final_summ_stats <- list(all_outcome_metrics_dt = all_outcome_metrics_dt_final,
                         all_outcome_metrics_delta_dt = all_outcome_metrics_delta_dt,
                         # all_outcome_train_R2_dt = all_outcome_train_R2_dt,
                         avg_metric_dt = avg_metric_dt,
                         avg_diff_dt = avg_diff_dt,
                         avg_perc_diff_dt = avg_perc_diff_dt,
                         n_pos_diff_dt = n_pos_diff_dt,
                         n_neg_diff_dt = n_neg_diff_dt)

saveRDS(final_summ_stats, paste0(summary_dir, 
                                 "Sex_Specific_All_Outcomes_CRS_vs_PheRS_PredictionMetrics_ControlCase4to1_WhiteBritish_Index2015_Followup10yr.rds"))

#### Read and save CRS vs IMD comparison ####
##### UPDATE TO LATEST: Read downloaded data from SDE #####
all_outcome_metrics_dt_imd <- read.csv(paste0(read_dir,
                                                 "Sex_Specific_CRS_vs_IMDq_PredictionMetrics_ControlCase4to1_WhiteBritish_Index2015_Followup10yr.csv"))

##### Updated parameters #####
#eth_name_vec <- c("WhiteBritish", "Asian", "Black", "Mixed")
eval_type <- "MonteCarlo"
metrics_names <-  c("R2_obs", "R2_liab","AUROC", "C_index", "AUPRC", "AUPRC_prev_diff")
model_name_vec <- unique(all_outcome_metrics_dt_imd$Model)
n_models <- length(model_name_vec)

#for (l in 1:length(eth_name_vec)) {

all_outcome_metrics_dt_final <- all_outcome_metrics_dt_imd

##### Add 95% CI upper and lower bounds for absolute metrics #####
n_metrics <- length(metrics_names)
for (k in 1:n_metrics) {
  metric_k <- metrics_names[k]
  metric_k_mean <- all_outcome_metrics_dt_final[, paste0(metric_k, "_mean")]
  if (eval_type == "MonteCarlo") {
    metric_k_se <- all_outcome_metrics_dt_final[, paste0(metric_k, "_SD")]/sqrt(n_mc)
  } else {
    metric_k_se <- all_outcome_metrics_dt_final[, paste0(metric_k, "_SD")]
  }
  all_outcome_metrics_dt_final[, paste0(metric_k, "_SE")] <- metric_k_se
  all_outcome_metrics_dt_final[, paste0(metric_k, "_LB")] <- metric_k_mean - qnorm(0.975) * metric_k_se
  all_outcome_metrics_dt_final[, paste0(metric_k, "_UB")] <- metric_k_mean + qnorm(0.975) * metric_k_se
  all_outcome_metrics_dt_final[, paste0(metric_k, "_Pval")] <- 2*pnorm(abs(metric_k_mean/metric_k_se),
                                                                       lower.tail = F)
}

##### Calculate 95% CI and p-value for difference between model of interest and age+sex model #####
all_outcome_metrics_delta_dt <- all_outcome_metrics_dt_final[, c("Outcome_phecode",
                                                                 "Model", 
                                                                 paste0("Delta_", metrics_names, "_mean"))]
all_outcome_metrics_delta_dt[, paste0("Delta_", metrics_names, "_SE")] <- NA
all_outcome_metrics_delta_dt[, paste0("Delta_", metrics_names, "_LB")] <- NA
all_outcome_metrics_delta_dt[, paste0("Delta_", metrics_names, "_UB")] <- NA
all_outcome_metrics_delta_dt[, paste0("Delta_", metrics_names, "_Pval")] <- NA

## Loop through each metric
n_metrics <- length(metrics_names)
for (k in 1:n_metrics) {
  metric_k <- metrics_names[k]
  delta_k_mean <- all_outcome_metrics_dt_final[, paste0("Delta_", metric_k, "_mean")]
  if (eval_type == "MonteCarlo") {
    delta_k_se <- all_outcome_metrics_dt_final[, paste0("Delta_", metric_k, "_SD")]/sqrt(n_mc)
  } else {
    delta_k_se <- all_outcome_metrics_dt_final[, paste0("Delta_", metric_k, "_SD")]
  }
  all_outcome_metrics_delta_dt[, paste0("Delta_", metric_k, "_SE")] <- delta_k_se
  all_outcome_metrics_delta_dt[, paste0("Delta_", metric_k, "_LB")] <- delta_k_mean - qnorm(0.975) * delta_k_se
  all_outcome_metrics_delta_dt[, paste0("Delta_", metric_k, "_UB")] <- delta_k_mean + qnorm(0.975) * delta_k_se
  all_outcome_metrics_delta_dt[, paste0("Delta_", metric_k, "_Pval")] <- 2*pnorm(abs(delta_k_mean/delta_k_se),
                                                                                 lower.tail = F)
}

##### Check number of significant differences and average difference for each metric #####
## Loop through each model and metric
n_pos_diff_dt <- data.frame("Model" = model_name_vec)
n_pos_diff_dt[, metrics_names] <- NA

n_neg_diff_dt <- n_pos_diff_dt

avg_diff_dt <- n_pos_diff_dt
avg_metric_dt <- n_pos_diff_dt
avg_perc_diff_dt <- n_pos_diff_dt

age_sex_avg_metrics <- all_outcome_metrics_dt_final %>%
  filter(Outcome_phecode != "Avg") %>%
  filter(Model == "Age_Sex") %>%
  select(paste0(metrics_names, "_mean")) %>%
  colMeans()

for (j in 1:n_models) {
  model_j <- model_name_vec[j]
  model_j_metrics_delta <- all_outcome_metrics_delta_dt %>%
    filter(Outcome_phecode != "Avg") %>%
    filter(Model == model_j)
  model_j_metrics <- all_outcome_metrics_dt_final %>%
    filter(Outcome_phecode != "Avg") %>%
    filter(Model == model_j)
  for (k in 1:n_metrics) {
    metric_k <- metrics_names[k]
    delta_k_mean <- model_j_metrics_delta[, paste0("Delta_", metric_k, "_mean")]
    delta_k_pval <- model_j_metrics_delta[, paste0("Delta_", metric_k, "_Pval")]
    
    metric_k_mean <- model_j_metrics[, paste0(metric_k, "_mean")]
    
    n_pos_diff_dt[j, metric_k] <- sum(delta_k_mean > 0 & delta_k_pval < 0.05)
    n_neg_diff_dt[j, metric_k] <- sum(delta_k_mean < 0 & delta_k_pval < 0.05)
    
    avg_metric_dt[j, metric_k] <- mean(metric_k_mean)
    avg_diff_dt[j, metric_k] <- mean(delta_k_mean)
    avg_perc_diff_dt[j, metric_k] <- mean(delta_k_mean)/age_sex_avg_metrics[k] * 100
  }
  
}

## Calculate percentage of positive/negative differences among outcomes
perc_pos_diff_dt <- n_pos_diff_dt %>%
  mutate_at(metrics_names, function(x){x/n_outcome_ds*100})
perc_neg_diff_dt <- n_neg_diff_dt %>%
  mutate_at(metrics_names, function(x){x/n_outcome_ds*100})



#View(avg_metric_dt)

#View(avg_diff_dt)

#View(avg_perc_diff_dt)

#View(n_pos_diff_dt[,-7])
#View(n_neg_diff_dt[,-7])

##### Save all summary statistics #####
final_summ_stats <- list(all_outcome_metrics_dt = all_outcome_metrics_dt_final,
                         all_outcome_metrics_delta_dt = all_outcome_metrics_delta_dt,
                         # all_outcome_train_R2_dt = all_outcome_train_R2_dt,
                         avg_metric_dt = avg_metric_dt,
                         avg_diff_dt = avg_diff_dt,
                         avg_perc_diff_dt = avg_perc_diff_dt,
                         n_pos_diff_dt = n_pos_diff_dt,
                         n_neg_diff_dt = n_neg_diff_dt)

saveRDS(final_summ_stats, paste0(summary_dir, 
                                 "Sex_Specific_CRS_vs_IMDq_PredictionMetrics_ControlCase4to1_WhiteBritish_Index2015_Followup10yr.rds"))


#### Read and save CRS vs CCI comparison ####
##### UPDATE TO LATEST: Read downloaded data from SDE #####
all_outcome_metrics_dt_cci <- read.csv(paste0(read_dir,
                                              "Sex_Specific_CRS_vs_CCI_PredictionMetrics_ControlCase4to1_WhiteBritish_Index2015_Followup10yr.csv"))

##### Updated parameters #####
#eth_name_vec <- c("WhiteBritish", "Asian", "Black", "Mixed")
eval_type <- "MonteCarlo"
metrics_names <-  c("R2_obs", "R2_liab","AUROC", "C_index", "AUPRC", "AUPRC_prev_diff")
model_name_vec <- unique(all_outcome_metrics_dt_cci$Model)
n_models <- length(model_name_vec)

#for (l in 1:length(eth_name_vec)) {

all_outcome_metrics_dt_final <- all_outcome_metrics_dt_cci

##### Add 95% CI upper and lower bounds for absolute metrics #####
n_metrics <- length(metrics_names)
for (k in 1:n_metrics) {
  metric_k <- metrics_names[k]
  metric_k_mean <- all_outcome_metrics_dt_final[, paste0(metric_k, "_mean")]
  if (eval_type == "MonteCarlo") {
    metric_k_se <- all_outcome_metrics_dt_final[, paste0(metric_k, "_SD")]/sqrt(n_mc)
  } else {
    metric_k_se <- all_outcome_metrics_dt_final[, paste0(metric_k, "_SD")]
  }
  all_outcome_metrics_dt_final[, paste0(metric_k, "_SE")] <- metric_k_se
  all_outcome_metrics_dt_final[, paste0(metric_k, "_LB")] <- metric_k_mean - qnorm(0.975) * metric_k_se
  all_outcome_metrics_dt_final[, paste0(metric_k, "_UB")] <- metric_k_mean + qnorm(0.975) * metric_k_se
  all_outcome_metrics_dt_final[, paste0(metric_k, "_Pval")] <- 2*pnorm(abs(metric_k_mean/metric_k_se),
                                                                       lower.tail = F)
}

##### Calculate 95% CI and p-value for difference between model of interest and age+sex model #####
all_outcome_metrics_delta_dt <- all_outcome_metrics_dt_final[, c("Outcome_phecode",
                                                                 "Model", 
                                                                 paste0("Delta_", metrics_names, "_mean"))]
all_outcome_metrics_delta_dt[, paste0("Delta_", metrics_names, "_SE")] <- NA
all_outcome_metrics_delta_dt[, paste0("Delta_", metrics_names, "_LB")] <- NA
all_outcome_metrics_delta_dt[, paste0("Delta_", metrics_names, "_UB")] <- NA
all_outcome_metrics_delta_dt[, paste0("Delta_", metrics_names, "_Pval")] <- NA

## Loop through each metric
n_metrics <- length(metrics_names)
for (k in 1:n_metrics) {
  metric_k <- metrics_names[k]
  delta_k_mean <- all_outcome_metrics_dt_final[, paste0("Delta_", metric_k, "_mean")]
  if (eval_type == "MonteCarlo") {
    delta_k_se <- all_outcome_metrics_dt_final[, paste0("Delta_", metric_k, "_SD")]/sqrt(n_mc)
  } else {
    delta_k_se <- all_outcome_metrics_dt_final[, paste0("Delta_", metric_k, "_SD")]
  }
  all_outcome_metrics_delta_dt[, paste0("Delta_", metric_k, "_SE")] <- delta_k_se
  all_outcome_metrics_delta_dt[, paste0("Delta_", metric_k, "_LB")] <- delta_k_mean - qnorm(0.975) * delta_k_se
  all_outcome_metrics_delta_dt[, paste0("Delta_", metric_k, "_UB")] <- delta_k_mean + qnorm(0.975) * delta_k_se
  all_outcome_metrics_delta_dt[, paste0("Delta_", metric_k, "_Pval")] <- 2*pnorm(abs(delta_k_mean/delta_k_se),
                                                                                 lower.tail = F)
}

##### Check number of significant differences and average difference for each metric #####
## Loop through each model and metric
n_pos_diff_dt <- data.frame("Model" = model_name_vec)
n_pos_diff_dt[, metrics_names] <- NA

n_neg_diff_dt <- n_pos_diff_dt

avg_diff_dt <- n_pos_diff_dt
avg_metric_dt <- n_pos_diff_dt
avg_perc_diff_dt <- n_pos_diff_dt

age_sex_avg_metrics <- all_outcome_metrics_dt_final %>%
  filter(Outcome_phecode != "Avg") %>%
  filter(Model == "Age_Sex") %>%
  select(paste0(metrics_names, "_mean")) %>%
  colMeans()

for (j in 1:n_models) {
  model_j <- model_name_vec[j]
  model_j_metrics_delta <- all_outcome_metrics_delta_dt %>%
    filter(Outcome_phecode != "Avg") %>%
    filter(Model == model_j)
  model_j_metrics <- all_outcome_metrics_dt_final %>%
    filter(Outcome_phecode != "Avg") %>%
    filter(Model == model_j)
  for (k in 1:n_metrics) {
    metric_k <- metrics_names[k]
    delta_k_mean <- model_j_metrics_delta[, paste0("Delta_", metric_k, "_mean")]
    delta_k_pval <- model_j_metrics_delta[, paste0("Delta_", metric_k, "_Pval")]
    
    metric_k_mean <- model_j_metrics[, paste0(metric_k, "_mean")]
    
    n_pos_diff_dt[j, metric_k] <- sum(delta_k_mean > 0 & delta_k_pval < 0.05)
    n_neg_diff_dt[j, metric_k] <- sum(delta_k_mean < 0 & delta_k_pval < 0.05)
    
    avg_metric_dt[j, metric_k] <- mean(metric_k_mean)
    avg_diff_dt[j, metric_k] <- mean(delta_k_mean)
    avg_perc_diff_dt[j, metric_k] <- mean(delta_k_mean)/age_sex_avg_metrics[k] * 100
  }
  
}

## Calculate percentage of positive/negative differences among outcomes
perc_pos_diff_dt <- n_pos_diff_dt %>%
  mutate_at(metrics_names, function(x){x/n_outcome_ds*100})
perc_neg_diff_dt <- n_neg_diff_dt %>%
  mutate_at(metrics_names, function(x){x/n_outcome_ds*100})



#View(avg_metric_dt)

#View(avg_diff_dt)

#View(avg_perc_diff_dt)

#View(n_pos_diff_dt[,-7])
#View(n_neg_diff_dt[,-7])

##### Save all summary statistics #####
final_summ_stats <- list(all_outcome_metrics_dt = all_outcome_metrics_dt_final,
                         all_outcome_metrics_delta_dt = all_outcome_metrics_delta_dt,
                         # all_outcome_train_R2_dt = all_outcome_train_R2_dt,
                         avg_metric_dt = avg_metric_dt,
                         avg_diff_dt = avg_diff_dt,
                         avg_perc_diff_dt = avg_perc_diff_dt,
                         n_pos_diff_dt = n_pos_diff_dt,
                         n_neg_diff_dt = n_neg_diff_dt)

saveRDS(final_summ_stats, paste0(summary_dir, 
                                 "Sex_Specific_CRS_vs_CCI_PredictionMetrics_ControlCase4to1_WhiteBritish_Index2015_Followup10yr.rds"))


#### Read and save CRS vs QCovid comparison ####
##### UPDATE TO LATEST: Read downloaded data from SDE #####
all_outcome_metrics_dt_qcovid <- read.csv(paste0(read_dir,
                                              "Summary_COVID_hspt_CRS_vs_QCovid_PredictionMetrics_ControlCase4to1_WhiteBritish_Index2015_Followup10yr.csv"))

##### Updated parameters #####
#eth_name_vec <- c("WhiteBritish", "Asian", "Black", "Mixed")
eval_type <- "MonteCarlo"
metrics_names <-  c("R2_obs", "R2_liab","AUROC", "C_index", "AUPRC", "AUPRC_prev_diff")
model_name_vec <- unique(all_outcome_metrics_dt_qcovid$Model)
n_models <- length(model_name_vec)

#for (l in 1:length(eth_name_vec)) {

all_outcome_metrics_dt_final <- all_outcome_metrics_dt_qcovid

##### Add 95% CI upper and lower bounds for absolute metrics #####
n_metrics <- length(metrics_names)
for (k in 1:n_metrics) {
  metric_k <- metrics_names[k]
  metric_k_mean <- all_outcome_metrics_dt_final[, paste0(metric_k, "_mean")]
  if (eval_type == "MonteCarlo") {
    metric_k_se <- all_outcome_metrics_dt_final[, paste0(metric_k, "_SD")]/sqrt(n_mc)
  } else {
    metric_k_se <- all_outcome_metrics_dt_final[, paste0(metric_k, "_SD")]
  }
  all_outcome_metrics_dt_final[, paste0(metric_k, "_SE")] <- metric_k_se
  all_outcome_metrics_dt_final[, paste0(metric_k, "_LB")] <- metric_k_mean - qnorm(0.975) * metric_k_se
  all_outcome_metrics_dt_final[, paste0(metric_k, "_UB")] <- metric_k_mean + qnorm(0.975) * metric_k_se
  all_outcome_metrics_dt_final[, paste0(metric_k, "_Pval")] <- 2*pnorm(abs(metric_k_mean/metric_k_se),
                                                                       lower.tail = F)
}

##### Calculate 95% CI and p-value for difference between model of interest and age+sex model #####
all_outcome_metrics_delta_dt <- all_outcome_metrics_dt_final[, c("Outcome_phecode",
                                                                 "Model", 
                                                                 paste0("Delta_", metrics_names, "_mean"))]
all_outcome_metrics_delta_dt[, paste0("Delta_", metrics_names, "_SE")] <- NA
all_outcome_metrics_delta_dt[, paste0("Delta_", metrics_names, "_LB")] <- NA
all_outcome_metrics_delta_dt[, paste0("Delta_", metrics_names, "_UB")] <- NA
all_outcome_metrics_delta_dt[, paste0("Delta_", metrics_names, "_Pval")] <- NA

## Loop through each metric
n_metrics <- length(metrics_names)
for (k in 1:n_metrics) {
  metric_k <- metrics_names[k]
  delta_k_mean <- all_outcome_metrics_dt_final[, paste0("Delta_", metric_k, "_mean")]
  if (eval_type == "MonteCarlo") {
    delta_k_se <- all_outcome_metrics_dt_final[, paste0("Delta_", metric_k, "_SD")]/sqrt(n_mc)
  } else {
    delta_k_se <- all_outcome_metrics_dt_final[, paste0("Delta_", metric_k, "_SD")]
  }
  all_outcome_metrics_delta_dt[, paste0("Delta_", metric_k, "_SE")] <- delta_k_se
  all_outcome_metrics_delta_dt[, paste0("Delta_", metric_k, "_LB")] <- delta_k_mean - qnorm(0.975) * delta_k_se
  all_outcome_metrics_delta_dt[, paste0("Delta_", metric_k, "_UB")] <- delta_k_mean + qnorm(0.975) * delta_k_se
  all_outcome_metrics_delta_dt[, paste0("Delta_", metric_k, "_Pval")] <- 2*pnorm(abs(delta_k_mean/delta_k_se),
                                                                                 lower.tail = F)
}

##### Save all summary statistics #####
final_summ_stats <- list(all_outcome_metrics_dt = all_outcome_metrics_dt_final,
                         all_outcome_metrics_delta_dt = all_outcome_metrics_delta_dt
                         )

saveRDS(final_summ_stats, paste0(summary_dir, 
                                 "Summary_COVID_hspt_CRS_vs_QCovid_PredictionMetrics_ControlCase4to1_WhiteBritish_Index2015_Followup10yr.rds"))


#### Context-specific performance by age: Read and save performance ####
##### UPDATE TO LATEST: Read downloaded data from SDE #####
all_outcome_metrics_dt_by_age <- read.csv(paste0(read_dir,
                                              "ByAge_Sex_Specific_All_Outcomes+COVID_PredictionMetrics_ControlCase4to1_WhiteBritish_EvenYOB_Index2015_Followup10yr.csv"))

##### Updated parameters #####
#eth_name_vec <- c("WhiteBritish", "Asian", "Black", "Mixed")
eval_type <- "Bootstrap"
metrics_names <-  c("R2_obs", "R2_liab","AUROC", "C_index", "AUPRC", "AUPRC_prev_diff")
model_name_vec <- unique(all_outcome_metrics_dt_by_age$Model)
n_models <- length(model_name_vec)

#for (l in 1:length(eth_name_vec)) {

all_outcome_metrics_dt_final <- all_outcome_metrics_dt_by_age

##### Add 95% CI upper and lower bounds for absolute metrics #####
n_metrics <- length(metrics_names)
for (k in 1:n_metrics) {
  metric_k <- metrics_names[k]
  metric_k_mean <- all_outcome_metrics_dt_final[, paste0(metric_k, "_mean")]
  if (eval_type == "MonteCarlo") {
    metric_k_se <- all_outcome_metrics_dt_final[, paste0(metric_k, "_SD")]/sqrt(n_mc)
  } else {
    metric_k_se <- all_outcome_metrics_dt_final[, paste0(metric_k, "_SD")]
  }
  all_outcome_metrics_dt_final[, paste0(metric_k, "_SE")] <- metric_k_se
  all_outcome_metrics_dt_final[, paste0(metric_k, "_LB")] <- metric_k_mean - qnorm(0.975) * metric_k_se
  all_outcome_metrics_dt_final[, paste0(metric_k, "_UB")] <- metric_k_mean + qnorm(0.975) * metric_k_se
  all_outcome_metrics_dt_final[, paste0(metric_k, "_Pval")] <- 2*pnorm(abs(metric_k_mean/metric_k_se),
                                                                       lower.tail = F)
}

##### Calculate 95% CI and p-value for difference between model of interest and age+sex model #####
all_outcome_metrics_delta_dt <- all_outcome_metrics_dt_final[, c("Outcome_phecode",
                                                                 "Model", 
                                                                 "Age",
                                                                 paste0("Delta_", metrics_names, "_mean"))]
all_outcome_metrics_delta_dt[, paste0("Delta_", metrics_names, "_SE")] <- NA
all_outcome_metrics_delta_dt[, paste0("Delta_", metrics_names, "_LB")] <- NA
all_outcome_metrics_delta_dt[, paste0("Delta_", metrics_names, "_UB")] <- NA
all_outcome_metrics_delta_dt[, paste0("Delta_", metrics_names, "_Pval")] <- NA

## Loop through each metric
n_metrics <- length(metrics_names)
for (k in 1:n_metrics) {
  metric_k <- metrics_names[k]
  delta_k_mean <- all_outcome_metrics_dt_final[, paste0("Delta_", metric_k, "_mean")]
  if (eval_type == "MonteCarlo") {
    delta_k_se <- all_outcome_metrics_dt_final[, paste0("Delta_", metric_k, "_SD")]/sqrt(n_mc)
  } else {
    delta_k_se <- all_outcome_metrics_dt_final[, paste0("Delta_", metric_k, "_SD")]
  }
  all_outcome_metrics_delta_dt[, paste0("Delta_", metric_k, "_SE")] <- delta_k_se
  all_outcome_metrics_delta_dt[, paste0("Delta_", metric_k, "_LB")] <- delta_k_mean - qnorm(0.975) * delta_k_se
  all_outcome_metrics_delta_dt[, paste0("Delta_", metric_k, "_UB")] <- delta_k_mean + qnorm(0.975) * delta_k_se
  all_outcome_metrics_delta_dt[, paste0("Delta_", metric_k, "_Pval")] <- 2*pnorm(abs(delta_k_mean/delta_k_se),
                                                                                 lower.tail = F)
}

##### Check number of significant differences and average difference for each metric #####
## Loop through each model and metric
n_pos_diff_dt <- data.frame("Model" = model_name_vec)
n_pos_diff_dt[, metrics_names] <- NA

n_neg_diff_dt <- n_pos_diff_dt

avg_diff_dt <- n_pos_diff_dt
avg_metric_dt <- n_pos_diff_dt
avg_perc_diff_dt <- n_pos_diff_dt

age_sex_avg_metrics <- all_outcome_metrics_dt_final %>%
  filter(Outcome_phecode != "Avg") %>%
  filter(Model == "Age_Sex") %>%
  select(paste0(metrics_names, "_mean")) %>%
  colMeans()

for (j in 1:n_models) {
  model_j <- model_name_vec[j]
  model_j_metrics_delta <- all_outcome_metrics_delta_dt %>%
    filter(Outcome_phecode != "Avg") %>%
    filter(Model == model_j)
  model_j_metrics <- all_outcome_metrics_dt_final %>%
    filter(Outcome_phecode != "Avg") %>%
    filter(Model == model_j)
  for (k in 1:n_metrics) {
    metric_k <- metrics_names[k]
    delta_k_mean <- model_j_metrics_delta[, paste0("Delta_", metric_k, "_mean")]
    delta_k_pval <- model_j_metrics_delta[, paste0("Delta_", metric_k, "_Pval")]
    
    metric_k_mean <- model_j_metrics[, paste0(metric_k, "_mean")]
    
    n_pos_diff_dt[j, metric_k] <- sum(delta_k_mean > 0 & delta_k_pval < 0.05)
    n_neg_diff_dt[j, metric_k] <- sum(delta_k_mean < 0 & delta_k_pval < 0.05)
    
    avg_metric_dt[j, metric_k] <- mean(metric_k_mean)
    avg_diff_dt[j, metric_k] <- mean(delta_k_mean)
    avg_perc_diff_dt[j, metric_k] <- mean(delta_k_mean)/age_sex_avg_metrics[k] * 100
  }
  
}

## Calculate percentage of positive/negative differences among outcomes
perc_pos_diff_dt <- n_pos_diff_dt %>%
  mutate_at(metrics_names, function(x){x/n_outcome_ds*100})
perc_neg_diff_dt <- n_neg_diff_dt %>%
  mutate_at(metrics_names, function(x){x/n_outcome_ds*100})



#View(avg_metric_dt)

#View(avg_diff_dt)

#View(avg_perc_diff_dt)

#View(n_pos_diff_dt[,-7])
#View(n_neg_diff_dt[,-7])

##### Save all summary statistics #####
final_summ_stats <- list(all_outcome_metrics_dt = all_outcome_metrics_dt_final,
                         all_outcome_metrics_delta_dt = all_outcome_metrics_delta_dt,
                         # all_outcome_train_R2_dt = all_outcome_train_R2_dt,
                         avg_metric_dt = avg_metric_dt,
                         avg_diff_dt = avg_diff_dt,
                         avg_perc_diff_dt = avg_perc_diff_dt,
                         n_pos_diff_dt = n_pos_diff_dt,
                         n_neg_diff_dt = n_neg_diff_dt)

saveRDS(final_summ_stats, paste0(summary_dir, 
                                 "ByAge_Sex_Specific_All_Outcomes+COVID_PredictionMetrics_ControlCase4to1_WhiteBritish_EvenYOB_Index2015_Followup10yr.rds"))


#### Context-specific performance by sex: Read and save performance ####
##### UPDATE TO LATEST: Read downloaded data from SDE #####
all_outcome_metrics_dt_by_sex <- read.csv(paste0(read_dir,
                                                 "BySex_All_Outcomes+COVID_PredictionMetrics_ControlCase4to1_WhiteBritish_EvenYOB_Index2015_Followup10yr.csv"))

##### Updated parameters #####
#eth_name_vec <- c("WhiteBritish", "Asian", "Black", "Mixed")
eval_type <- "Bootstrap"
metrics_names <-  c("R2_obs", "R2_liab","AUROC", "C_index", "AUPRC", "AUPRC_prev_diff")
model_name_vec <- unique(all_outcome_metrics_dt_by_sex$Model)
n_models <- length(model_name_vec)

#for (l in 1:length(eth_name_vec)) {

all_outcome_metrics_dt_final <- all_outcome_metrics_dt_by_sex

##### Add 95% CI upper and lower bounds for absolute metrics #####
n_metrics <- length(metrics_names)
for (k in 1:n_metrics) {
  metric_k <- metrics_names[k]
  metric_k_mean <- all_outcome_metrics_dt_final[, paste0(metric_k, "_mean")]
  if (eval_type == "MonteCarlo") {
    metric_k_se <- all_outcome_metrics_dt_final[, paste0(metric_k, "_SD")]/sqrt(n_mc)
  } else {
    metric_k_se <- all_outcome_metrics_dt_final[, paste0(metric_k, "_SD")]
  }
  all_outcome_metrics_dt_final[, paste0(metric_k, "_SE")] <- metric_k_se
  all_outcome_metrics_dt_final[, paste0(metric_k, "_LB")] <- metric_k_mean - qnorm(0.975) * metric_k_se
  all_outcome_metrics_dt_final[, paste0(metric_k, "_UB")] <- metric_k_mean + qnorm(0.975) * metric_k_se
  all_outcome_metrics_dt_final[, paste0(metric_k, "_Pval")] <- 2*pnorm(abs(metric_k_mean/metric_k_se),
                                                                       lower.tail = F)
}

##### Calculate 95% CI and p-value for difference between model of interest and age+sex model #####
all_outcome_metrics_delta_dt <- all_outcome_metrics_dt_final[, c("Outcome_phecode",
                                                                 "Model", 
                                                                 "Sex",
                                                                 paste0("Delta_", metrics_names, "_mean"))]
all_outcome_metrics_delta_dt[, paste0("Delta_", metrics_names, "_SE")] <- NA
all_outcome_metrics_delta_dt[, paste0("Delta_", metrics_names, "_LB")] <- NA
all_outcome_metrics_delta_dt[, paste0("Delta_", metrics_names, "_UB")] <- NA
all_outcome_metrics_delta_dt[, paste0("Delta_", metrics_names, "_Pval")] <- NA

## Loop through each metric
n_metrics <- length(metrics_names)
for (k in 1:n_metrics) {
  metric_k <- metrics_names[k]
  delta_k_mean <- all_outcome_metrics_dt_final[, paste0("Delta_", metric_k, "_mean")]
  if (eval_type == "MonteCarlo") {
    delta_k_se <- all_outcome_metrics_dt_final[, paste0("Delta_", metric_k, "_SD")]/sqrt(n_mc)
  } else {
    delta_k_se <- all_outcome_metrics_dt_final[, paste0("Delta_", metric_k, "_SD")]
  }
  all_outcome_metrics_delta_dt[, paste0("Delta_", metric_k, "_SE")] <- delta_k_se
  all_outcome_metrics_delta_dt[, paste0("Delta_", metric_k, "_LB")] <- delta_k_mean - qnorm(0.975) * delta_k_se
  all_outcome_metrics_delta_dt[, paste0("Delta_", metric_k, "_UB")] <- delta_k_mean + qnorm(0.975) * delta_k_se
  all_outcome_metrics_delta_dt[, paste0("Delta_", metric_k, "_Pval")] <- 2*pnorm(abs(delta_k_mean/delta_k_se),
                                                                                 lower.tail = F)
}

##### Check number of significant differences and average difference for each metric #####
## Loop through each model and metric
n_pos_diff_dt <- data.frame("Model" = model_name_vec)
n_pos_diff_dt[, metrics_names] <- NA

n_neg_diff_dt <- n_pos_diff_dt

avg_diff_dt <- n_pos_diff_dt
avg_metric_dt <- n_pos_diff_dt
avg_perc_diff_dt <- n_pos_diff_dt

age_sex_avg_metrics <- all_outcome_metrics_dt_final %>%
  filter(Outcome_phecode != "Avg") %>%
  filter(Model == "Age_Sex") %>%
  select(paste0(metrics_names, "_mean")) %>%
  colMeans()

for (j in 1:n_models) {
  model_j <- model_name_vec[j]
  model_j_metrics_delta <- all_outcome_metrics_delta_dt %>%
    filter(Outcome_phecode != "Avg") %>%
    filter(Model == model_j)
  model_j_metrics <- all_outcome_metrics_dt_final %>%
    filter(Outcome_phecode != "Avg") %>%
    filter(Model == model_j)
  for (k in 1:n_metrics) {
    metric_k <- metrics_names[k]
    delta_k_mean <- model_j_metrics_delta[, paste0("Delta_", metric_k, "_mean")]
    delta_k_pval <- model_j_metrics_delta[, paste0("Delta_", metric_k, "_Pval")]
    
    metric_k_mean <- model_j_metrics[, paste0(metric_k, "_mean")]
    
    n_pos_diff_dt[j, metric_k] <- sum(delta_k_mean > 0 & delta_k_pval < 0.05)
    n_neg_diff_dt[j, metric_k] <- sum(delta_k_mean < 0 & delta_k_pval < 0.05)
    
    avg_metric_dt[j, metric_k] <- mean(metric_k_mean)
    avg_diff_dt[j, metric_k] <- mean(delta_k_mean)
    avg_perc_diff_dt[j, metric_k] <- mean(delta_k_mean)/age_sex_avg_metrics[k] * 100
  }
  
}

## Calculate percentage of positive/negative differences among outcomes
perc_pos_diff_dt <- n_pos_diff_dt %>%
  mutate_at(metrics_names, function(x){x/n_outcome_ds*100})
perc_neg_diff_dt <- n_neg_diff_dt %>%
  mutate_at(metrics_names, function(x){x/n_outcome_ds*100})



#View(avg_metric_dt)

#View(avg_diff_dt)

#View(avg_perc_diff_dt)

#View(n_pos_diff_dt[,-7])
#View(n_neg_diff_dt[,-7])

##### Save all summary statistics #####
final_summ_stats <- list(all_outcome_metrics_dt = all_outcome_metrics_dt_final,
                         all_outcome_metrics_delta_dt = all_outcome_metrics_delta_dt,
                         # all_outcome_train_R2_dt = all_outcome_train_R2_dt,
                         avg_metric_dt = avg_metric_dt,
                         avg_diff_dt = avg_diff_dt,
                         avg_perc_diff_dt = avg_perc_diff_dt,
                         n_pos_diff_dt = n_pos_diff_dt,
                         n_neg_diff_dt = n_neg_diff_dt)

saveRDS(final_summ_stats, paste0(summary_dir, 
                                 "BySex_All_Outcomes+COVID_PredictionMetrics_ControlCase4to1_WhiteBritish_EvenYOB_Index2015_Followup10yr.rds"))


#### Context-specific performance by number of comorbidities: Read and save performance ####
##### UPDATE TO LATEST: Read downloaded data from SDE #####
all_outcome_metrics_dt_by_comorb <- read.csv(paste0(read_dir,
                                                 "ByComorbidity_Sex_Specific_All_Outcomes+COVID_PredictionMetrics_ControlCase4to1_WhiteBritish_EvenYOB_Index2015_Followup10yr.csv"))

##### Updated parameters #####
#eth_name_vec <- c("WhiteBritish", "Asian", "Black", "Mixed")
eval_type <- "Bootstrap"
metrics_names <-  c("R2_obs", "R2_liab","AUROC", "C_index", "AUPRC", "AUPRC_prev_diff")
model_name_vec <- unique(all_outcome_metrics_dt_by_comorb$Model)
n_models <- length(model_name_vec)

#for (l in 1:length(eth_name_vec)) {

all_outcome_metrics_dt_final <- all_outcome_metrics_dt_by_comorb

##### Add 95% CI upper and lower bounds for absolute metrics #####
n_metrics <- length(metrics_names)
for (k in 1:n_metrics) {
  metric_k <- metrics_names[k]
  metric_k_mean <- all_outcome_metrics_dt_final[, paste0(metric_k, "_mean")]
  if (eval_type == "MonteCarlo") {
    metric_k_se <- all_outcome_metrics_dt_final[, paste0(metric_k, "_SD")]/sqrt(n_mc)
  } else {
    metric_k_se <- all_outcome_metrics_dt_final[, paste0(metric_k, "_SD")]
  }
  all_outcome_metrics_dt_final[, paste0(metric_k, "_SE")] <- metric_k_se
  all_outcome_metrics_dt_final[, paste0(metric_k, "_LB")] <- metric_k_mean - qnorm(0.975) * metric_k_se
  all_outcome_metrics_dt_final[, paste0(metric_k, "_UB")] <- metric_k_mean + qnorm(0.975) * metric_k_se
  all_outcome_metrics_dt_final[, paste0(metric_k, "_Pval")] <- 2*pnorm(abs(metric_k_mean/metric_k_se),
                                                                       lower.tail = F)
}

##### Calculate 95% CI and p-value for difference between model of interest and age+sex model #####
all_outcome_metrics_delta_dt <- all_outcome_metrics_dt_final[, c("Outcome_phecode",
                                                                 "Model", 
                                                                 "Comorbidity",
                                                                 paste0("Delta_", metrics_names, "_mean"))]
all_outcome_metrics_delta_dt[, paste0("Delta_", metrics_names, "_SE")] <- NA
all_outcome_metrics_delta_dt[, paste0("Delta_", metrics_names, "_LB")] <- NA
all_outcome_metrics_delta_dt[, paste0("Delta_", metrics_names, "_UB")] <- NA
all_outcome_metrics_delta_dt[, paste0("Delta_", metrics_names, "_Pval")] <- NA

## Loop through each metric
n_metrics <- length(metrics_names)
for (k in 1:n_metrics) {
  metric_k <- metrics_names[k]
  delta_k_mean <- all_outcome_metrics_dt_final[, paste0("Delta_", metric_k, "_mean")]
  if (eval_type == "MonteCarlo") {
    delta_k_se <- all_outcome_metrics_dt_final[, paste0("Delta_", metric_k, "_SD")]/sqrt(n_mc)
  } else {
    delta_k_se <- all_outcome_metrics_dt_final[, paste0("Delta_", metric_k, "_SD")]
  }
  all_outcome_metrics_delta_dt[, paste0("Delta_", metric_k, "_SE")] <- delta_k_se
  all_outcome_metrics_delta_dt[, paste0("Delta_", metric_k, "_LB")] <- delta_k_mean - qnorm(0.975) * delta_k_se
  all_outcome_metrics_delta_dt[, paste0("Delta_", metric_k, "_UB")] <- delta_k_mean + qnorm(0.975) * delta_k_se
  all_outcome_metrics_delta_dt[, paste0("Delta_", metric_k, "_Pval")] <- 2*pnorm(abs(delta_k_mean/delta_k_se),
                                                                                 lower.tail = F)
}

##### Check number of significant differences and average difference for each metric #####
## Loop through each model and metric
n_pos_diff_dt <- data.frame("Model" = model_name_vec)
n_pos_diff_dt[, metrics_names] <- NA

n_neg_diff_dt <- n_pos_diff_dt

avg_diff_dt <- n_pos_diff_dt
avg_metric_dt <- n_pos_diff_dt
avg_perc_diff_dt <- n_pos_diff_dt

age_sex_avg_metrics <- all_outcome_metrics_dt_final %>%
  filter(Outcome_phecode != "Avg") %>%
  filter(Model == "Age_Sex") %>%
  select(paste0(metrics_names, "_mean")) %>%
  colMeans()

for (j in 1:n_models) {
  model_j <- model_name_vec[j]
  model_j_metrics_delta <- all_outcome_metrics_delta_dt %>%
    filter(Outcome_phecode != "Avg") %>%
    filter(Model == model_j)
  model_j_metrics <- all_outcome_metrics_dt_final %>%
    filter(Outcome_phecode != "Avg") %>%
    filter(Model == model_j)
  for (k in 1:n_metrics) {
    metric_k <- metrics_names[k]
    delta_k_mean <- model_j_metrics_delta[, paste0("Delta_", metric_k, "_mean")]
    delta_k_pval <- model_j_metrics_delta[, paste0("Delta_", metric_k, "_Pval")]
    
    metric_k_mean <- model_j_metrics[, paste0(metric_k, "_mean")]
    
    n_pos_diff_dt[j, metric_k] <- sum(delta_k_mean > 0 & delta_k_pval < 0.05)
    n_neg_diff_dt[j, metric_k] <- sum(delta_k_mean < 0 & delta_k_pval < 0.05)
    
    avg_metric_dt[j, metric_k] <- mean(metric_k_mean)
    avg_diff_dt[j, metric_k] <- mean(delta_k_mean)
    avg_perc_diff_dt[j, metric_k] <- mean(delta_k_mean)/age_sex_avg_metrics[k] * 100
  }
  
}

## Calculate percentage of positive/negative differences among outcomes
perc_pos_diff_dt <- n_pos_diff_dt %>%
  mutate_at(metrics_names, function(x){x/n_outcome_ds*100})
perc_neg_diff_dt <- n_neg_diff_dt %>%
  mutate_at(metrics_names, function(x){x/n_outcome_ds*100})



#View(avg_metric_dt)

#View(avg_diff_dt)

#View(avg_perc_diff_dt)

#View(n_pos_diff_dt[,-7])
#View(n_neg_diff_dt[,-7])

##### Save all summary statistics #####
final_summ_stats <- list(all_outcome_metrics_dt = all_outcome_metrics_dt_final,
                         all_outcome_metrics_delta_dt = all_outcome_metrics_delta_dt,
                         # all_outcome_train_R2_dt = all_outcome_train_R2_dt,
                         avg_metric_dt = avg_metric_dt,
                         avg_diff_dt = avg_diff_dt,
                         avg_perc_diff_dt = avg_perc_diff_dt,
                         n_pos_diff_dt = n_pos_diff_dt,
                         n_neg_diff_dt = n_neg_diff_dt)

saveRDS(final_summ_stats, paste0(summary_dir, 
                                 "ByComorbidity_Sex_Specific_All_Outcomes+COVID_PredictionMetrics_ControlCase4to1_WhiteBritish_EvenYOB_Index2015_Followup10yr.rds"))


#### Context-specific performance by year of outcome incidence: Read and save performance ####
##### UPDATE TO LATEST: Read downloaded data from SDE #####
all_outcome_metrics_dt_by_year <- read.csv(paste0(read_dir,
                                                    "ByOutcomeYear_Sex_Specific_All_Outcomes+COVID_PredictionMetrics_ControlCase4to1_WhiteBritish_EvenYOB_Index2015_Followup10yr.csv"))

##### Updated parameters #####
#eth_name_vec <- c("WhiteBritish", "Asian", "Black", "Mixed")
eval_type <- "Bootstrap"
metrics_names <-  c("R2_obs", "R2_liab","AUROC", "C_index", "AUPRC", "AUPRC_prev_diff")
model_name_vec <- unique(all_outcome_metrics_dt_by_year$Model)
n_models <- length(model_name_vec)

#for (l in 1:length(eth_name_vec)) {

all_outcome_metrics_dt_final <- all_outcome_metrics_dt_by_year

##### Add 95% CI upper and lower bounds for absolute metrics #####
n_metrics <- length(metrics_names)
for (k in 1:n_metrics) {
  metric_k <- metrics_names[k]
  metric_k_mean <- all_outcome_metrics_dt_final[, paste0(metric_k, "_mean")]
  if (eval_type == "MonteCarlo") {
    metric_k_se <- all_outcome_metrics_dt_final[, paste0(metric_k, "_SD")]/sqrt(n_mc)
  } else {
    metric_k_se <- all_outcome_metrics_dt_final[, paste0(metric_k, "_SD")]
  }
  all_outcome_metrics_dt_final[, paste0(metric_k, "_SE")] <- metric_k_se
  all_outcome_metrics_dt_final[, paste0(metric_k, "_LB")] <- metric_k_mean - qnorm(0.975) * metric_k_se
  all_outcome_metrics_dt_final[, paste0(metric_k, "_UB")] <- metric_k_mean + qnorm(0.975) * metric_k_se
  all_outcome_metrics_dt_final[, paste0(metric_k, "_Pval")] <- 2*pnorm(abs(metric_k_mean/metric_k_se),
                                                                       lower.tail = F)
}

##### Calculate 95% CI and p-value for difference between model of interest and age+sex model #####
all_outcome_metrics_delta_dt <- all_outcome_metrics_dt_final[, c("Outcome_phecode",
                                                                 "Model", 
                                                                 "Year",
                                                                 paste0("Delta_", metrics_names, "_mean"))]
all_outcome_metrics_delta_dt[, paste0("Delta_", metrics_names, "_SE")] <- NA
all_outcome_metrics_delta_dt[, paste0("Delta_", metrics_names, "_LB")] <- NA
all_outcome_metrics_delta_dt[, paste0("Delta_", metrics_names, "_UB")] <- NA
all_outcome_metrics_delta_dt[, paste0("Delta_", metrics_names, "_Pval")] <- NA

## Loop through each metric
n_metrics <- length(metrics_names)
for (k in 1:n_metrics) {
  metric_k <- metrics_names[k]
  delta_k_mean <- all_outcome_metrics_dt_final[, paste0("Delta_", metric_k, "_mean")]
  if (eval_type == "MonteCarlo") {
    delta_k_se <- all_outcome_metrics_dt_final[, paste0("Delta_", metric_k, "_SD")]/sqrt(n_mc)
  } else {
    delta_k_se <- all_outcome_metrics_dt_final[, paste0("Delta_", metric_k, "_SD")]
  }
  all_outcome_metrics_delta_dt[, paste0("Delta_", metric_k, "_SE")] <- delta_k_se
  all_outcome_metrics_delta_dt[, paste0("Delta_", metric_k, "_LB")] <- delta_k_mean - qnorm(0.975) * delta_k_se
  all_outcome_metrics_delta_dt[, paste0("Delta_", metric_k, "_UB")] <- delta_k_mean + qnorm(0.975) * delta_k_se
  all_outcome_metrics_delta_dt[, paste0("Delta_", metric_k, "_Pval")] <- 2*pnorm(abs(delta_k_mean/delta_k_se),
                                                                                 lower.tail = F)
}

##### Check number of significant differences and average difference for each metric #####
## Loop through each model and metric
n_pos_diff_dt <- data.frame("Model" = model_name_vec)
n_pos_diff_dt[, metrics_names] <- NA

n_neg_diff_dt <- n_pos_diff_dt

avg_diff_dt <- n_pos_diff_dt
avg_metric_dt <- n_pos_diff_dt
avg_perc_diff_dt <- n_pos_diff_dt

age_sex_avg_metrics <- all_outcome_metrics_dt_final %>%
  filter(Outcome_phecode != "Avg") %>%
  filter(Model == "Age_Sex") %>%
  select(paste0(metrics_names, "_mean")) %>%
  colMeans()

for (j in 1:n_models) {
  model_j <- model_name_vec[j]
  model_j_metrics_delta <- all_outcome_metrics_delta_dt %>%
    filter(Outcome_phecode != "Avg") %>%
    filter(Model == model_j)
  model_j_metrics <- all_outcome_metrics_dt_final %>%
    filter(Outcome_phecode != "Avg") %>%
    filter(Model == model_j)
  for (k in 1:n_metrics) {
    metric_k <- metrics_names[k]
    delta_k_mean <- model_j_metrics_delta[, paste0("Delta_", metric_k, "_mean")]
    delta_k_pval <- model_j_metrics_delta[, paste0("Delta_", metric_k, "_Pval")]
    
    metric_k_mean <- model_j_metrics[, paste0(metric_k, "_mean")]
    
    n_pos_diff_dt[j, metric_k] <- sum(delta_k_mean > 0 & delta_k_pval < 0.05)
    n_neg_diff_dt[j, metric_k] <- sum(delta_k_mean < 0 & delta_k_pval < 0.05)
    
    avg_metric_dt[j, metric_k] <- mean(metric_k_mean)
    avg_diff_dt[j, metric_k] <- mean(delta_k_mean)
    avg_perc_diff_dt[j, metric_k] <- mean(delta_k_mean)/age_sex_avg_metrics[k] * 100
  }
  
}

## Calculate percentage of positive/negative differences among outcomes
perc_pos_diff_dt <- n_pos_diff_dt %>%
  mutate_at(metrics_names, function(x){x/n_outcome_ds*100})
perc_neg_diff_dt <- n_neg_diff_dt %>%
  mutate_at(metrics_names, function(x){x/n_outcome_ds*100})



#View(avg_metric_dt)

#View(avg_diff_dt)

#View(avg_perc_diff_dt)

#View(n_pos_diff_dt[,-7])
#View(n_neg_diff_dt[,-7])

##### Save all summary statistics #####
final_summ_stats <- list(all_outcome_metrics_dt = all_outcome_metrics_dt_final,
                         all_outcome_metrics_delta_dt = all_outcome_metrics_delta_dt,
                         # all_outcome_train_R2_dt = all_outcome_train_R2_dt,
                         avg_metric_dt = avg_metric_dt,
                         avg_diff_dt = avg_diff_dt,
                         avg_perc_diff_dt = avg_perc_diff_dt,
                         n_pos_diff_dt = n_pos_diff_dt,
                         n_neg_diff_dt = n_neg_diff_dt)

saveRDS(final_summ_stats, paste0(summary_dir, 
                                 "ByOutcomeYear_Sex_Specific_All_Outcomes+COVID_PredictionMetrics_ControlCase4to1_WhiteBritish_EvenYOB_Index2015_Followup10yr.rds"))

  