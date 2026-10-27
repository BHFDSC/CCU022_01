### Author: Pearl Liu
### For each PheRS outcome, derive summary statistics from performance metrics in testing data
### Combine with CRS summ stats
### Odd DOB: training, Even DOB: testing
### Index: 2015-01-01, follow-up: 10 years
### 11 outcomes, use unscaled PheRS coefficients

rm(list = ls())
gc()

#### Load libraries ####
library(data.table)
library(tidyverse)
#library(sandwich)
#library(pROC)
#library(pec)
#library(PRROC)

source('~/collab/CCU022/CCU022_CRS/0.1_Functions.R')

#### Set parameters ####
temp_data_dir_step1_0 <- '~/collab/CCU022/CCU022_CRS/Data/Step1.0/'
temp_data_dir_step1_1 <- '~/collab/CCU022/CCU022_CRS/Data/Step1.1/'
temp_data_dir_step2_3 <- '~/collab/CCU022/CCU022_CRS/Data/Step2.3/'
temp_data_dir_step2_4 <- '~/collab/CCU022/CCU022_CRS/Data/Step2.4/'
temp_data_dir_step3_0 <- '~/collab/CCU022/CCU022_CRS/Data/Step3.0/'
temp_data_dir_step4_0 <- '~/collab/CCU022/CCU022_CRS/Data/Step4.0/'
temp_data_dir_step11_0 <- '~/collab/CCU022/CCU022_CRS/Data/Step11.0/'
temp_data_dir_step11_1 <- '~/collab/CCU022/CCU022_CRS/Data/Step11.1/'
temp_data_dir_step11_2 <- '~/collab/CCU022/CCU022_CRS/Data/Step11.2/'
temp_data_dir_step11_3 <- '~/collab/CCU022/CCU022_CRS/Data/Step11.3/'
temp_data_dir <- '~/collab/CCU022/CCU022_CRS/Data/Step11.4/'
file_date <- '2025_05_01'

## Model names
model_name_vec <- c("CRS_all_predictors", 
                    "PheRS_age_sex",
                    "Age_Sex")
n_models <- length(model_name_vec)

## Performance metrics
metrics_names <-  c("R2_obs", "R2_liab","AUROC", "C_index", "AUPRC", "AUPRC_prev_diff")
n_metrics <- length(metrics_names)

## Control-case ratio in logistic regression of training data
control_case_ratio <- 4
## Number of covariates 
n_covar <- 2
## Number of Monte Carlo sampling in testing data
n_mc <- 100   # PL 20251202: change n_mc from 200 to 100
## Control-case ratio in testing data for evaluating AUROC, C-index, R2
control_case_ratio_test <- 4
## Number of cases used in testing data
n_cases_test1 <- 3000
n_cases_test2 <- 1000

#### Read processed data ####
##### List of outcomes #####
## PheRS outcomes include: 
## colorectal cancer, breast cancer (female only), type ii diabetes, prostate cancer (male only), 
## MI, Coronary atherosclerosis, asthma, Afib and flutter, 
## lung cancer, osteoarthritis (localized), gout
outcome_ds_vec <- c("153", "174_1", "250_2", "185",
                    "411_2", "411_4", "495", "427_2", 
                    "165_1", "740_1", "740_1", "274_1")

## Outcome code for output
outcome_ds_vec_output <- c("153", "174_1", "250_2", "185",
                           "411_2", "411_4", "495", "427_2", 
                           "165_1", "740_1hipOA", "740_1kneeOA", "274_1")

## Repeat osteoarthritis (localized) twice to cover Hip OA and Knee OA

## Outcome names in PheRS model
outcome_ds_names <- c("Colorectal cancer",
                      "Breast cancer",
                      "Type 2 diabetes (T2D)",
                      "Prostate cancer",
                      "Coronary heart disease (CHD)",
                      "Coronary heart disease (CHD)",
                      "Asthma",
                      "Atrial fibrillation (AF)",
                      "Lung cancer",
                      "Hip osteoarthritis (hip OA)",  
                      "Knee osteoarthritis (knee OA)",
                      "Gout")

n_outcome_ds <- length(outcome_ds_vec)


##### Phecode information #####
phe_icd10_codelist_name = paste0("ccu022_01_PheCode_ICD10AtoN_map_by_PL_", file_date)
icd10_phecode_map <- readRDS(paste0(temp_data_dir_step1_0, 
                                    phe_icd10_codelist_name))
phecode_info <- icd10_phecode_map %>%
  select(PheCode, Phenotype, Category, Sex) %>%
  distinct()

##### Outcome incidence #####
outcome_incid_vec0 <- readRDS(paste0(temp_data_dir_step11_1, 
                                     'PheRS_TrainData_Outcome_Incidence_OddYOB_Index2015_Followup10yr.rds'))

outcome_incid_vec_sf0 <- readRDS(paste0(temp_data_dir_step11_1, 
                                        'PheRS_TrainData_Outcome_Incidence_Sex_Specific_OddYOB_Index2015_Followup10yr.rds'))

## Accounting for repeated OA
oa_idx <- which(outcome_ds_vec == "740_1")[1]
outcome_incid_vec <- c(outcome_incid_vec0[1:oa_idx], outcome_incid_vec0[oa_idx],
                       outcome_incid_vec0[(oa_idx+1):11])
outcome_incid_vec_sf <- c(outcome_incid_vec_sf0[1:oa_idx], outcome_incid_vec_sf0[oa_idx],
                          outcome_incid_vec_sf0[(oa_idx+1):11])


#### Confirm the number of output files in previous steps ####
files_step11_3 <- list.files(temp_data_dir_step11_3)
length(files_step11_3) 
# [1] 12

#### Prioritize: Sex-specific evaluation (sex-specific performance for sex-specific outcomes) ####

## Store summary statistics of performance metrics of testing data
metric_colnames <-  c(paste0(metrics_names, "_mean"),
                      paste0(metrics_names, "_SD"),
                      ## Delta is the difference between Model of interest and Age + sex model
                      paste0("Delta_", metrics_names, "_mean"),
                      paste0("Delta_", metrics_names, "_SD") )
all_outcome_metrics_dt <- data.frame(
  "Outcome_phecode" = rep(outcome_ds_vec, each = n_models),
  "PheRS_outcome_name" = rep(outcome_ds_names, each = n_models),
  "Model" = rep(model_name_vec, n_outcome_ds)
)

all_outcome_metrics_dt[, metric_colnames] <- NA


## Store sum of metrics over all outcomes for all models for all Monte Carlo samples
metrics_names <-  c("R2_obs", "R2_liab","AUROC", "C_index", "AUPRC", "AUPRC_prev_diff")
sum_all_outcomes_metrics <- vector(mode = "list", length = n_models)
names(sum_all_outcomes_metrics) <- model_name_vec
for (j in 1:n_models) {
  sum_all_outcomes_metrics[[j]] <- data.frame(matrix(0,
                                                     n_mc, 
                                                     n_metrics))
  colnames(sum_all_outcomes_metrics[[j]]) <- metrics_names
  
}


## Store sum of delta metrics over all outcomes for all models for all Monte Carlo samples
sum_all_outcomes_metrics_delta <- sum_all_outcomes_metrics
  

##### Loop through each PheRS outcome #####
for (i in 1:n_outcome_ds) {
  target_ds <- outcome_ds_vec[i]
  
  if (target_ds == "COVID_hspt") {
    target_ds_sex <- "NULL"
  } else if (target_ds == "death") {
    target_ds_sex <- "Both"
  } else {
    target_ds_sex <- phecode_info %>%
      filter(PheCode == str_replace(target_ds, '_', '\\.')) %>%
      pull(Sex)
  }
  
  ##### Read performance metrics #####
  if (target_ds_sex %in% c("Female", "Male")) {
    target_ds_metric_list_crs <- readRDS(paste0(temp_data_dir_step4_0, 'Outcome',
                                                target_ds, '_Sex_Specific_TestData_PredictionMetrics_ControlCase',
                                                control_case_ratio, 'to1_WhiteBritish_EvenYOB_Index2015_Followup10yr.rds'))
    
    target_ds_metric_list_phers <- readRDS(paste0(temp_data_dir_step11_3, 'Outcome',
                                           outcome_ds_vec_output[i], '_PheRS_Sex_Specific_TestData_PredictionMetrics_ControlCase',
                                           control_case_ratio, 'to1_WhiteBritish_EvenYOB_Index2015_Followup10yr.rds'))
    
  } else if (target_ds_sex %in% c("Both", "Empty")) {
    target_ds_metric_list_crs <- readRDS(paste0(temp_data_dir_step4_0, 'Outcome',
                                                target_ds, '_TestData_PredictionMetrics_ControlCase',
                                                control_case_ratio, 'to1_WhiteBritish_EvenYOB_Index2015_Followup10yr.rds'))
    
    target_ds_metric_list_phers <- readRDS(paste0(temp_data_dir_step11_3, 'Outcome',
                                           outcome_ds_vec_output[i], '_PheRS_TestData_PredictionMetrics_ControlCase',
                                           control_case_ratio, 'to1_WhiteBritish_EvenYOB_Index2015_Followup10yr.rds'))
  } 
  
  ##### Combine CRS and PheRS metric list #####
  target_ds_metric_list <- c(target_ds_metric_list_crs,
                             target_ds_metric_list_phers)[c("CRS_all_predictors",
                                                            "PheRS_age_sex",
                                                            "Age_Sex")]
  target_ds_metric_baseline <- target_ds_metric_list$Age_Sex
  
  ##### Loop through each model and store testing metrics #####
  for (j in 1:n_models) {
    target_ds_metric_j <- target_ds_metric_list[[j]]
    update_idx <- (i-1)*n_models + j
    all_outcome_metrics_dt[update_idx, 
                           paste0(metrics_names, "_mean")] <- colMeans(target_ds_metric_j)
    all_outcome_metrics_dt[update_idx, 
                           paste0(metrics_names, "_SD")] <- apply(target_ds_metric_j, 2, sd)
    
    delta_target_ds_metric_j <- target_ds_metric_j - target_ds_metric_baseline
    
    all_outcome_metrics_dt[update_idx, 
                           paste0("Delta_", metrics_names, "_mean")] <- colMeans(delta_target_ds_metric_j)
    all_outcome_metrics_dt[update_idx, 
                           paste0("Delta_", metrics_names, "_SD")] <- apply(delta_target_ds_metric_j, 2, sd)
    
    ## Sum of metrics or delta metrics over all outcomes
    sum_all_outcomes_metrics[[j]] <- sum_all_outcomes_metrics[[j]] + target_ds_metric_j
    sum_all_outcomes_metrics_delta[[j]] <- sum_all_outcomes_metrics_delta[[j]] + delta_target_ds_metric_j
 
  }
  
}

avg_all_outcomes_metrics <- lapply(sum_all_outcomes_metrics,
                                   function(x){x/n_outcome_ds})
avg_all_outcomes_metrics_delta <- lapply(sum_all_outcomes_metrics_delta,
                                        function(x){x/n_outcome_ds})


##### Calculate average metrics summary over outcomes #####
metrics_names <-  c("R2_obs", "R2_liab","AUROC", "C_index", "AUPRC", "AUPRC_prev_diff")
metric_colnames <-  c(paste0(metrics_names, "_mean"),
                      paste0(metrics_names, "_SD"),
                      ## Delta is the difference between Model of interest and Age + sex model
                      paste0("Delta_", metrics_names, "_mean"),
                      paste0("Delta_", metrics_names, "_SD") )
avg_outcome_metrics_dt <- data.frame(
  "Outcome_phecode" = rep("Avg", each = n_models),
  "PheRS_outcome_name" = rep("Avg", each = n_models),
  "Model" = rep(model_name_vec, 1)
)

avg_outcome_metrics_dt[, metric_colnames] <- NA

for (j in 1:n_models) {
  avg_outcome_metrics_dt[j, 
                         paste0(metrics_names, "_mean")] <- colMeans(avg_all_outcomes_metrics[[j]])
  avg_outcome_metrics_dt[j, 
                         paste0(metrics_names, "_SD")] <- apply(avg_all_outcomes_metrics[[j]], 2, sd)
  
  
  avg_outcome_metrics_dt[j, 
                         paste0("Delta_", metrics_names, "_mean")] <- colMeans(avg_all_outcomes_metrics_delta[[j]])
  avg_outcome_metrics_dt[j, 
                         paste0("Delta_", metrics_names, "_SD")] <- apply(avg_all_outcomes_metrics_delta[[j]], 2, sd)

}

### Combine metrics summary for individual outcomes and average metrics summary 
all_outcome_metrics_dt_final <- rbind(all_outcome_metrics_dt, avg_outcome_metrics_dt)


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
  delta_k_se <- all_outcome_metrics_dt_final[, paste0("Delta_", metric_k, "_SD")]/sqrt(n_mc)
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

###### Specific baseline model (PheRS) ######
baseline_avg_metrics <- all_outcome_metrics_dt_final %>%
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
    avg_perc_diff_dt[j, metric_k] <- mean(delta_k_mean)/baseline_avg_metrics[k] * 100
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
                       #  all_outcome_train_R2_dt = all_outcome_train_R2_dt,
                         avg_metric_dt = avg_metric_dt,
                         avg_diff_dt = avg_diff_dt,
                         avg_perc_diff_dt = avg_perc_diff_dt,
                         n_pos_diff_dt = n_pos_diff_dt,
                         n_neg_diff_dt = n_neg_diff_dt)

saveRDS(final_summ_stats, paste0(temp_data_dir, 
                                 "Sex_Specific_All_Outcomes_CRS_vs_PheRS_PredictionMetrics_ControlCase",
                                 control_case_ratio, "to1_WhiteBritish_Index2015_Followup10yr.rds"))


#### Read all summary statistics ####
##### Sex-specific - prioritize #####
temp_data_dir_step11_4 <- '~/collab/CCU022/CCU022_CRS/Data/Step11.4/'
final_summ_stats_phers <- readRDS(paste0(temp_data_dir_step11_4, 
                                   "Sex_Specific_All_Outcomes_CRS_vs_PheRS_PredictionMetrics_ControlCase",
                                   control_case_ratio, "to1_WhiteBritish_Index2015_Followup10yr.rds"))

#all_outcome_train_R2_dt <- final_summ_stats$all_outcome_train_R2_dt
all_outcome_metrics_dt <- final_summ_stats_phers$all_outcome_metrics_dt
all_outcome_metrics_delta_dt <- final_summ_stats_phers$all_outcome_metrics_delta_dt
avg_metric_dt <- final_summ_stats_phers$avg_metric_dt

View(final_summ_stats_phers$all_outcome_metrics_delta_dt %>% filter(Outcome_phecode == "Avg" &
                                                                       Model == "CRS_all_predictors"))

View(final_summ_stats_phers$all_outcome_metrics_dt %>% filter(Outcome_phecode == "Avg" &
                                                                 Model == "CRS_all_predictors"))


#### Explore metric performances ####
all_outcome_metrics_dt %>%
  filter(Model == "CRS_all_predictors" & Outcome_phecode == 'Avg') %>%
  View()

all_outcome_metrics_delta_dt %>%
  filter(Model == "CRS_all_predictors" & Outcome_phecode == 'Avg') %>%
  View()

all_outcome_metrics_dt %>%
  filter(Model == "CRS_all_predictors") %>%
  View()

all_outcome_metrics_dt %>%
  filter(Outcome_phecode == "COVID_hspt") %>%
  View()

all_outcome_metrics_dt %>%
  filter(Outcome_phecode == "death") %>%
  View()

#### Plot CRS vs. PheRS performance ####
crs_vs_phers_compare <- final_summ_stats_phers$all_outcome_metrics_dt %>%
  filter(Outcome_phecode != "Avg") %>%
  select(Outcome_phecode, PheRS_outcome_name, 
         Model, AUROC_mean, C_index_mean, 
         R2_liab_mean, R2_obs_mean) %>%
  pivot_wider(names_from = "Model", values_from = c("AUROC_mean",
                                                    "C_index_mean",
                                                    "R2_liab_mean",
                                                    "R2_obs_mean"))
plot(crs_vs_phers_compare$AUROC_mean_PheRS_age_sex,
     crs_vs_phers_compare$AUROC_mean_CRS_all_predictors,
     xlab = "AUROC (PheRS)", ylab = "AUROC (CRS)")
abline(0,1)

plot(crs_vs_phers_compare$AUROC_mean_PheRS_age_sex - crs_vs_phers_compare$AUROC_mean_Age_Sex,
     crs_vs_phers_compare$AUROC_mean_CRS_all_predictors - crs_vs_phers_compare$AUROC_mean_Age_Sex,
     xlab = "Delta AUROC (PheRS)", ylab = "Delta AUROC (CRS)",
     xlim = c(-0.01, 0.1), ylim = c(-0.01, 0.1))
abline(0,1)

plot(crs_vs_phers_compare$C_index_mean_PheRS_age_sex,
     crs_vs_phers_compare$C_index_mean_CRS_all_predictors,
     xlab = "C-index (PheRS)", ylab = "C-index (CRS)")
abline(0,1)

plot(crs_vs_phers_compare$R2_liab_mean_PheRS_age_sex,
     crs_vs_phers_compare$R2_liab_mean_CRS_all_predictors,
     xlab = "R2 liab (PheRS)", ylab = "R2 liab (CRS)")
abline(0,1)

plot(crs_vs_phers_compare$R2_obs_mean_PheRS_age_sex,
     crs_vs_phers_compare$R2_obs_mean_CRS_all_predictors,
     xlab = "R2 obs (PheRS)", ylab = "R2 obs (CRS)")
abline(0,1)





