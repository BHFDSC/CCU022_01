### Author: Pearl Liu
### Compare CRS against QCovid model for COVID-19 hspt outcome: result summary
### Calibrate QCovid within training data, test Qcovid, CRS + age + sex, Qcovid + CRS, Qcovid + CRS + age + sex
### Odd DOB: training, Even DOB: testing
### Index: 2020-01-01, follow-up: 5 years


rm(list = ls())
gc()

#### Load libraries ####
library(data.table)
library(tidyverse)
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
i <- 1

## Model names
model_name_vec <- c("CRS_all_predictors", 
                    "QCovid",
                    "CRS_QCovid",
                    "CRS_all_predictors_QCovid",
                    "CRS_QCovid_Age_Sex",
                    "Age_Sex")
n_models <- length(model_name_vec)
## Control-case ratio in logistic regression of training data
control_case_ratio <- 4
## Number of Monte Carlo sampling in testing data
n_mc <- 100  # PL 2026 update
## Control-case ratio in testing data for evaluating AUROC, C-index, R2
control_case_ratio_test <- 4
## Number of cases used in testing data
n_cases_test1 <- 3000
n_cases_test2 <- 1000

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

#### Get summary stats ####

## Store summary statistics of performance metrics of testing data
metrics_names <-  c("R2_obs", 
                    "R2_liab",
                    "AUROC", "C_index", "AUPRC", "AUPRC_prev_diff")
metric_colnames <-  c(paste0(metrics_names, "_mean"),
                      paste0(metrics_names, "_SD"),
                      ## Delta is the difference between Model of interest and Age + sex model
                      paste0("Delta_", metrics_names, "_mean"),
                      paste0("Delta_", metrics_names, "_SD") )
all_outcome_metrics_dt <- data.frame(
  "Outcome_phecode" = rep("COVID_hspt", n_models),
  "Model" = model_name_vec
)

all_outcome_metrics_dt[, metric_colnames] <- NA



##### Read performance metrics #####
target_ds_metric_list <- readRDS(paste0(temp_data_dir, 'CRS_vs_QCovid_Outcome',
                                        target_ds, '_TestData_PredictionMetrics_ControlCase',
                                        control_case_ratio, 'to1_WhiteBritish_EvenYOB_Index2020_Followup5yr.rds'))

#target_ds_metric_baseline <- target_ds_metric_list$QCovid
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
  
}


#### Calculate 95% CI and p-value for difference between target models and age + sex model ####
all_outcome_metrics_delta_dt <- all_outcome_metrics_dt[, c("Outcome_phecode",
                                                           "Model", 
                                                           paste0("Delta_", metrics_names, "_mean"))]
all_outcome_metrics_delta_dt[, paste0("Delta_", metrics_names, "_LB")] <- NA
all_outcome_metrics_delta_dt[, paste0("Delta_", metrics_names, "_UB")] <- NA
all_outcome_metrics_delta_dt[, paste0("Delta_", metrics_names, "_Pval")] <- NA

## Loop through each metric
n_metrics <- length(metrics_names)
for (k in 1:n_metrics) {
  metric_k <- metrics_names[k]
  delta_k_mean <- all_outcome_metrics_dt[, paste0("Delta_", metric_k, "_mean")]
  delta_k_se <- all_outcome_metrics_dt[, paste0("Delta_", metric_k, "_SD")]/sqrt(n_mc)
  all_outcome_metrics_delta_dt[, paste0("Delta_", metric_k, "_LB")] <- delta_k_mean - qnorm(0.975) * delta_k_se
  all_outcome_metrics_delta_dt[, paste0("Delta_", metric_k, "_UB")] <- delta_k_mean + qnorm(0.975) * delta_k_se
  all_outcome_metrics_delta_dt[, paste0("Delta_", metric_k, "_Pval")] <- 2*pnorm(abs(delta_k_mean/delta_k_se),
                                                                                 lower.tail = F)
}

#### Save all summary statistics ####
final_summ_stats <- list(all_outcome_metrics_dt = all_outcome_metrics_dt,
                         all_outcome_metrics_delta_dt = all_outcome_metrics_delta_dt)

saveRDS(final_summ_stats, paste0(temp_data_dir, 
                                 "Summary_COVID_hspt_CRS_vs_QCovid_PredictionMetrics_ControlCase",
                                 control_case_ratio, "to1_WhiteBritish_Index2020_Followup5yr.rds"))


#### Read and plot performance metrics ####
temp_data_dir_step10_2 <- '~/collab/CCU022/CCU022_CRS/Data/Step10.2/'
final_summ_stats_qcovid <- readRDS(paste0(temp_data_dir_step10_2, 
                                 "Summary_COVID_hspt_CRS_vs_QCovid_PredictionMetrics_ControlCase",
                                 control_case_ratio, "to1_WhiteBritish_Index2020_Followup5yr.rds"))

all_outcome_metrics_dt_qcovid <- final_summ_stats_qcovid$all_outcome_metrics_dt %>%
  mutate(Model_name = c("CRS + Age + Sex", "QCovid", "CRS + QCovid",
                        "CRS_age_sex + QCovid",
                        "CRS + QCovid + Age + Sex", 
                        "Age + Sex"),
         AUROC_LB = AUROC_mean - qnorm(0.975)*AUROC_SD/sqrt(n_mc),
         AUROC_UB = AUROC_mean + qnorm(0.975)*AUROC_SD/sqrt(n_mc)) %>%
  filter(!(Model_name %in% c("CRS_age_sex + QCovid", "CRS + QCovid"))) %>%
  arrange(AUROC_mean)

all_outcome_metrics_dt_qcovid$Model_name <- factor(all_outcome_metrics_dt_qcovid$Model_name,
                                            levels = all_outcome_metrics_dt_qcovid$Model_name)

auroc_by_model <- ggplot(all_outcome_metrics_dt_qcovid, 
                          aes(x = Model_name, y = AUROC_mean - 0.5)) +
  geom_col(width = 0.8) + 
  geom_errorbar(aes(x = Model_name, 
                    ymin = AUROC_LB - 0.5, 
                    ymax = AUROC_UB - 0.5),
                width=.2) + 
  labs(x = "Model", y = "AUROC - 0.5")



