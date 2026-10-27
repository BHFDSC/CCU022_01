### Author: Pearl Liu
### Cohort for comparison with Delphi-2M
### For each Delphi-CRS overlapping outcome, derive CRS summary statistics from performance metrics
### Testing data satisfy: 
###  (1) 50-80 yrs old at 2020-06-30
###  (2) 1-year follow-up for outcome incidence from 2021-07-01 to 2022-07-01
### Odd DOB: training, Even DOB: testing


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
temp_data_dir_step16_0 <- '~/collab/CCU022/CCU022_CRS/Data/Step16.0/'
temp_data_dir_step16_1 <- '~/collab/CCU022/CCU022_CRS/Data/Step16.1/'
temp_data_dir_step16_2 <- '~/collab/CCU022/CCU022_CRS/Data/Step16.2/'
temp_data_dir <- '~/collab/CCU022/CCU022_CRS/Data/Step16.3/'
Figure_dir <- '~/collab/CCU022/CCU022_CRS/Data/Figures/'
file_date <- '2025_05_01'


## Model names
model_name_vec <- c("CRS_all_predictors", 
                 #   "CRS_top100",
                 #   "CRS_top50", 
                 #   "CRS_top20", 
                 #   "CRS_top10",
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
n_mc <- 100
## Control-case ratio in testing data for evaluating AUROC, C-index, R2
control_case_ratio_test <- 4
## Number of cases used in testing data
n_cases_test1 <- 3000
n_cases_test2 <- 1000

#### Read processed data ####
##### Phecode information #####
phe_icd10_codelist_name = paste0("ccu022_01_PheCode_ICD10AtoN_map_by_PL_", file_date)
icd10_phecode_map <- readRDS(paste0(temp_data_dir_step1_0, 
                                    phe_icd10_codelist_name))
phecode_info <- icd10_phecode_map %>%
  select(PheCode, Phenotype, Category, Sex) %>%
  distinct() %>%
  mutate(Outcome_phecode = str_replace(PheCode, '\\.', '_'))

##### CRS phecode vs. Delphi ICD10 code map #####
crs_delphi_map <- read.table(paste0(temp_data_dir_step16_1, "CRS_phecode_vs_Delphi_ICD10_map.txt"),
                             sep = "\t", header = T, quote = "", fill = T) 

##### Get list of CRS-Delphi overlapping outcomes #####
outcome_ds_vec <- crs_delphi_map %>%
  select(Outcome_phecode) %>%
  mutate(Outcome_phecode = 
           if_else(Outcome_phecode == "Dea", "death", Outcome_phecode)) %>%
  pull(Outcome_phecode) %>%
  unique() %>%
  sort()

n_outcome_ds <- length(outcome_ds_vec)


#### Confirm the number of output files in previous steps ####
files_step16_2 <- list.files(temp_data_dir_step16_2)
length(files_step16_2)
# 49


#### Sex-specific evaluation (sex-specific performance for sex-specific outcomes) ####
##### Loop through each outcome #####

## Store summary statistics of performance metrics of testing data (include COVID hspt)
metric_colnames <-  c(paste0(metrics_names, "_mean"),
                      paste0(metrics_names, "_SD"),
                      ## Delta is the difference between Model of interest and Age + sex model
                      paste0("Delta_", metrics_names, "_mean"),
                      paste0("Delta_", metrics_names, "_SD") )
all_outcome_metrics_dt <- data.frame(
  "Outcome_phecode" = rep(outcome_ds_vec, each = n_models),
  "Model" = rep(model_name_vec, n_outcome_ds)
)

all_outcome_metrics_dt[, metric_colnames] <- NA


## Store sum of metrics over all outcomes for all models for all Monte Carlo samples
metrics_names <-  c("R2_obs", "R2_liab","AUROC", "C_index", "AUPRC", "AUPRC_prev_diff")
sum_all_outcomes_metrics <- vector(mode = "list", length = n_models)
names(sum_all_outcomes_metrics) <- model_name_vec
for (j in 1:n_models) {
  sum_all_outcomes_metrics[[j]] <- data.frame(matrix(0,
                                                     n_mc+1, 
                                                     n_metrics))
  colnames(sum_all_outcomes_metrics[[j]]) <- metrics_names
  
}


## Store sum of delta metrics over all outcomes for all models for all Monte Carlo samples
sum_all_outcomes_metrics_delta <- sum_all_outcomes_metrics


for (i in 1:n_outcome_ds) {
  target_ds <- outcome_ds_vec[i]
  
  if (target_ds == "death") {
    target_ds_sex <- "Both"
  } else {
    target_ds_sex <- phecode_info %>%
      filter(PheCode == str_replace(target_ds, '_', '\\.')) %>%
      pull(Sex)
  }
  
  
  ##### Read performance metrics #####
  if (target_ds_sex %in% c("Female", "Male")) {
    target_ds_metric_list <- readRDS(paste0(temp_data_dir_step16_2, 'Outcome',
                                            target_ds, 
                                            '_Delphi_Sex_Specific_TestData_PredictionMetrics_ControlCase',
                                            control_case_ratio, 'to1_WhiteBritish_EvenDOB_IndexMid2020_Followup1yr.rds'))
  } else if (target_ds_sex %in% c("Both", "Empty")) {
    target_ds_metric_list <- readRDS(paste0(temp_data_dir_step16_2, 'Outcome',
                                            target_ds, 
                                            '_Delphi_TestData_PredictionMetrics_ControlCase',
                                            control_case_ratio, 'to1_WhiteBritish_EvenDOB_IndexMid2020_Followup1yr.rds'))
  } 
  
  target_ds_metric_age_sex <- target_ds_metric_list$Age_Sex
  
  ##### Loop through each model and store testing metrics #####
  for (l in 1:n_models) {
    target_ds_metric_l <- target_ds_metric_list[[l]]
    update_idx <- (i-1)*n_models + l 
    all_outcome_metrics_dt[update_idx, 
                           paste0(metrics_names, "_mean")] <- target_ds_metric_l[1,]
    all_outcome_metrics_dt[update_idx, 
                           paste0(metrics_names, "_SD")] <- apply(target_ds_metric_l[-1,], 2, sd)
    
    delta_target_ds_metric_l <- target_ds_metric_l - target_ds_metric_age_sex
    
    all_outcome_metrics_dt[update_idx, 
                           paste0("Delta_", metrics_names, "_mean")] <- delta_target_ds_metric_l[1,]
    all_outcome_metrics_dt[update_idx, 
                           paste0("Delta_", metrics_names, "_SD")] <- apply(delta_target_ds_metric_l[-1,], 2, sd)
    
    ## Sum of metrics or delta metrics over all outcomes
    sum_all_outcomes_metrics[[l]] <- sum_all_outcomes_metrics[[l]] + target_ds_metric_l
    sum_all_outcomes_metrics_delta[[l]] <- sum_all_outcomes_metrics_delta[[l]] + delta_target_ds_metric_l
    
  }
 
}

avg_all_outcomes_metrics <- lapply(sum_all_outcomes_metrics,
                                   function(x){x/n_outcome_ds})
avg_all_outcomes_metrics_delta <- lapply(sum_all_outcomes_metrics_delta,
                                         function(x){x/n_outcome_ds})


##### Calculate average metrics summary over outcomes #####
metric_colnames <-  c(paste0(metrics_names, "_mean"),
                      paste0(metrics_names, "_SD"),
                      ## Delta is the difference between Model of interest and Age + sex model
                      paste0("Delta_", metrics_names, "_mean"),
                      paste0("Delta_", metrics_names, "_SD") )
avg_outcome_metrics_dt <- data.frame(
  "Outcome_phecode" = rep("Avg", each = n_models),
  "Model" = model_name_vec
)

avg_outcome_metrics_dt[, metric_colnames] <- NA

#for (j in 1:n_settings) {
  for (l in 1:n_models) {
    avg_outcome_metrics_dt[l, 
                           paste0(metrics_names, "_mean")] <- avg_all_outcomes_metrics[[l]][1,]
    avg_outcome_metrics_dt[l, 
                           paste0(metrics_names, "_SD")] <- apply(avg_all_outcomes_metrics[[l]][-1,], 2, sd)
    
    
    avg_outcome_metrics_dt[l, 
                           paste0("Delta_", metrics_names, "_mean")] <- avg_all_outcomes_metrics_delta[[l]][1,]
    avg_outcome_metrics_dt[l, 
                           paste0("Delta_", metrics_names, "_SD")] <- apply(avg_all_outcomes_metrics_delta[[l]][-1,], 2, sd)
    
  }

  #}

### Combine metrics summary for individual outcomes and average metrics summary 
all_outcome_metrics_dt_final <- rbind(all_outcome_metrics_dt, avg_outcome_metrics_dt)


##### Calculate 95% CI and p-value for difference between model of interest and age+sex model #####
all_outcome_metrics_delta_dt <- all_outcome_metrics_dt_final[, c("Outcome_phecode",
                                                                 "Model", 
                                                                 paste0("Delta_", metrics_names, "_mean"))]
all_outcome_metrics_delta_dt[, paste0("Delta_", metrics_names, "_SE")] <- NA
all_outcome_metrics_delta_dt[, paste0("Delta_", metrics_names, "_LB")] <- NA
all_outcome_metrics_delta_dt[, paste0("Delta_", metrics_names, "_UB")] <- NA
all_outcome_metrics_delta_dt[, paste0("Delta_", metrics_names, "_Pval")] <- NA

## Loop through each metric
for (k in 1:n_metrics) {
  metric_k <- metrics_names[k]
  delta_k_mean <- all_outcome_metrics_dt_final[, paste0("Delta_", metric_k, "_mean")]
  delta_k_se <- all_outcome_metrics_dt_final[, paste0("Delta_", metric_k, "_SD")]
  all_outcome_metrics_delta_dt[, paste0("Delta_", metric_k, "_SE")] <- delta_k_se
  all_outcome_metrics_delta_dt[, paste0("Delta_", metric_k, "_LB")] <- delta_k_mean - qnorm(0.975) * delta_k_se
  all_outcome_metrics_delta_dt[, paste0("Delta_", metric_k, "_UB")] <- delta_k_mean + qnorm(0.975) * delta_k_se
  all_outcome_metrics_delta_dt[, paste0("Delta_", metric_k, "_Pval")] <- 2*pnorm(abs(delta_k_mean/delta_k_se),
                                                                                 lower.tail = F)
}


##### Check number of significant differences and average difference for each metric #####

## SKIP

##### Save all summary statistics #####
final_summ_stats <- list(all_outcome_metrics_dt = all_outcome_metrics_dt_final,
                         all_outcome_metrics_delta_dt = all_outcome_metrics_delta_dt)

saveRDS(final_summ_stats, paste0(temp_data_dir, 
                                 "ForDelphiComparison_Sex_Specific_All_Outcomes_PredictionMetrics_ControlCase",
                                 control_case_ratio, "to1_WhiteBritish_EvenYOB_IndexMid2020_Followup1yr.rds"))


#### Read all summary statistics ####
##### Sex-specific - prioritize #####
temp_data_dir_step16_3 <- '~/collab/CCU022/CCU022_CRS/Data/Step16.3/'
final_summ_stats_delphi <- readRDS(paste0(temp_data_dir_step16_3, 
                                          "ForDelphiComparison_Sex_Specific_All_Outcomes_PredictionMetrics_ControlCase",
                                          control_case_ratio, "to1_WhiteBritish_EvenYOB_IndexMid2020_Followup1yr.rds"))


#### Compare CRS vs. Delphi-2M ####
crs_summ_stats <- final_summ_stats_delphi$all_outcome_metrics_dt %>%
  filter(Model %in% c("CRS_all_predictors", "Age_Sex")) %>%
  select(Outcome_phecode, Model, AUROC_mean, C_index_mean) %>%
  pivot_wider(names_from = "Model", 
              values_from = c("AUROC_mean", "C_index_mean")) %>%
  mutate(Outcome_phecode = if_else(Outcome_phecode == "death", "Dea",
                                    Outcome_phecode))

## Get final comparison data set
compare_summ_stats <- crs_summ_stats %>%
  inner_join(crs_delphi_map, by = "Outcome_phecode") %>%
  mutate(Delta_AUC_Delphi = AUC_Delphi_avg - AUC_Base_Shmatko_avg,
         Delta_AUC_CRS = AUROC_mean_CRS_all_predictors - AUROC_mean_Age_Sex) %>%
  select(PHECODE, PHECODE_phenotype, sex, AUC_Delphi_avg, Delta_AUC_Delphi,
         AUROC_mean_CRS_all_predictors, Delta_AUC_CRS) %>%
  distinct()

## Check outcomes with better performance for Delphi-2M vs. CRS
compare_summ_stats %>% 
  arrange(desc(AUC_Delphi_avg - AUROC_mean_CRS_all_predictors)) %>%
  select(PHECODE_phenotype, AUROC_mean_CRS_all_predictors, AUC_Delphi_avg) %>%
  View()

compare_summ_stats %>% 
  arrange(desc(Delta_AUC_Delphi - Delta_AUC_CRS)) %>%
  select(PHECODE_phenotype, Delta_AUC_CRS, Delta_AUC_Delphi) %>%
  View()

##### Summary #####
compare_results <- 
  compare_summ_stats %>%
  summarise(CRS_better = sum(AUROC_mean_CRS_all_predictors - AUC_Delphi_avg > 0),
            Delphi_better = sum(AUROC_mean_CRS_all_predictors - AUC_Delphi_avg < 0),
            Delta_CRS_better = sum(Delta_AUC_CRS - Delta_AUC_Delphi > 0),
            Delta_Delphi_better = sum(Delta_AUC_CRS - Delta_AUC_Delphi < 0),
            Avg_CRS_Delphi_diff = mean(AUROC_mean_CRS_all_predictors - AUC_Delphi_avg),
            Avg_CRS_Delphi_delta_diff = mean(Delta_AUC_CRS - Delta_AUC_Delphi))

View(compare_results)

##### Plot #####

## Absolute AUC
plot(compare_summ_stats$AUC_Delphi_avg, 
     compare_summ_stats$AUROC_mean_CRS_all_predictors,
     xlim = c(0.4,0.95), ylim = c(0.4,0.95),
     xlab = "AUC (Delphi-2M)", ylab = "AUC (CRS)")
abline(0,1)


## Absolute AUC limited to non-sex-specific outcomes
plot(compare_summ_stats$AUC_Delphi_avg[compare_summ_stats$sex %in% c("Both", "Empty")], 
     compare_summ_stats$AUROC_mean_CRS_all_predictors[compare_summ_stats$sex %in% c("Both", "Empty")],
     xlim = c(0.4,0.95), ylim = c(0.4,0.95),
     xlab = "AUC (Delphi-2M)", ylab = "AUC (CRS)")
abline(0,1)

## Delta AUC
plot(compare_summ_stats$Delta_AUC_Delphi, 
     compare_summ_stats$Delta_AUC_CRS,
     xlim = c(-0.12,0.25), ylim = c(-0.12,0.25),
     xlab = "Delta AUC (Delphi-2M)", ylab = "Delta AUC (CRS)")
abline(0,1)



## Delta AUC limited to non-sex-specific outcomes
plot(compare_summ_stats$Delta_AUC_Delphi[compare_summ_stats$sex %in% c("Both", "Empty")], 
     compare_summ_stats$Delta_AUC_CRS[compare_summ_stats$sex %in% c("Both", "Empty")],
     xlim = c(-0.12,0.2), ylim = c(-0.12,0.2),
     xlab = "Delta AUC (Delphi-2M)", ylab = "Delta AUC (CRS)")
abline(0,1)




