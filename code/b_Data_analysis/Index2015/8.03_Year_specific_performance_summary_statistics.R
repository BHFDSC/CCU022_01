### Author: Pearl Liu
### For each outcome, derive summary statistics from performance metrics in context-specific testing data
### Odd DOB: training, Even DOB: testing
### Index: 2015-01-01, follow-up: 10 years


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
temp_data_dir_step7_3 <- '~/collab/CCU022/CCU022_CRS/Data/Step7.3/'
temp_data_dir_step8_0 <- '~/collab/CCU022/CCU022_CRS/Data/Step8.0/'
temp_data_dir <- '~/collab/CCU022/CCU022_CRS/Data/Step8.0/'
Figure_dir <- '~/collab/CCU022/CCU022_CRS/Data/Figures/'
file_date <- '2025_05_01'


## Outcome year settings
context_specific_settings <-  list(c(1,1),
                                   c(2,2),
                                   c(3,4),
                                   c(5,6),
                                   c(7,8),
                                   c(9,10))
context_specific_settings_text <- c("1",
                                    "2", 
                                    "3-4", 
                                    "5-6", 
                                    "7-8",
                                    "9-10")
n_settings <- length(context_specific_settings)


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
##### List of predictors and outcomes #####
pred_ds_list_name = paste0("ccu022_01_Predictor_Diseases_Pheparent_list_by_PL_", file_date)
outcome_ds_list_name = paste0("ccu022_01_Outcome_Diseases_Phecode_list_by_PL_", file_date)
outcome_ds_list_name2 = paste0("ccu022_01_Outcome_Diseases_Phecode_list2_TopCommonPlusPheRS_by_PL_", file_date)

pred_ds_list = readRDS(paste0(temp_data_dir_step1_0, pred_ds_list_name))
outcome_ds_list = readRDS(paste0(temp_data_dir_step1_0, outcome_ds_list_name))
outcome_ds_list2 = readRDS(paste0(temp_data_dir_step1_0, outcome_ds_list_name2))

pred_ds_vec <- pred_ds_list %>% arrange(index) %>% pull(predictor_ds) 
outcome_ds_vec <- c(outcome_ds_list %>% 
                      arrange(index) %>% 
                      ## Remove phecode 622
                      filter(target_ds != "622") %>% 
                      pull(target_ds), "COVID_hspt",
                    outcome_ds_list2 %>% arrange(index) %>% pull(target_ds),
                    "death")


n_pred_ds <- length(pred_ds_vec)
n_outcome_ds <- length(outcome_ds_vec)




##### Phecode information #####
phe_icd10_codelist_name = paste0("ccu022_01_PheCode_ICD10AtoN_map_by_PL_", file_date)
icd10_phecode_map <- readRDS(paste0(temp_data_dir_step1_0, 
                                    phe_icd10_codelist_name))
phecode_info <- icd10_phecode_map %>%
  select(PheCode, Phenotype, Category, Sex) %>%
  distinct() %>%
  mutate(Outcome_phecode = str_replace(PheCode, '\\.', '_'))


#### Confirm the number of output files in previous steps ####
files_step7_3 <- list.files(temp_data_dir_step7_3)
length(files_step7_3) 

# 442


#### Sex-specific evaluation (sex-specific performance for sex-specific outcomes) ####
##### Loop through each outcome #####

## Store summary statistics of performance metrics of testing data (include COVID hspt)
metric_colnames <-  c(paste0(metrics_names, "_mean"),
                      paste0(metrics_names, "_SD"),
                      ## Delta is the difference between Model of interest and Age + sex model
                      paste0("Delta_", metrics_names, "_mean"),
                      paste0("Delta_", metrics_names, "_SD") )
all_outcome_metrics_dt <- data.frame(
  "Outcome_phecode" = rep(outcome_ds_vec, each = n_models) %>% rep(n_settings),
  "Model" = rep(model_name_vec, n_outcome_ds) %>% rep(n_settings),
  "Year" = rep(context_specific_settings_text, each = n_models * n_outcome_ds)
) 

all_outcome_metrics_dt[, metric_colnames] <- NA


## Store sum of metrics over all outcomes for all models for all bootstrap samples
sum_all_outcomes_metrics <- vector(mode = "list", length = n_settings)
names(sum_all_outcomes_metrics) <- paste0("Year_", context_specific_settings_text)
for (j in 1:n_settings) {
  sum_all_outcomes_metrics[[j]] <- vector(mode = "list", length = n_models)
  names(sum_all_outcomes_metrics[[j]]) <- model_name_vec
  for (l in 1:n_models) {
    sum_all_outcomes_metrics[[j]][[l]] <- data.frame(matrix(0, n_mc + 1, n_metrics))
    colnames(sum_all_outcomes_metrics[[j]][[l]]) <- metrics_names
  }
}

## Store sum of delta metrics over all outcomes for all models for all Monte Carlo samples
sum_all_outcomes_metrics_delta <- sum_all_outcomes_metrics


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
  
  
  for (j in 1:n_settings) {
  
  ##### Read performance metrics #####
  if (target_ds_sex %in% c("Female", "Male")) {
    target_ds_metric_list <- readRDS(paste0(temp_data_dir_step7_3, 'Outcome',
                                            target_ds, '_OutcomeYearSetting', j,
                                            '_Sex_Specific_TestData_PredictionMetrics_ControlCase',
                                            control_case_ratio, 'to1_WhiteBritish_EvenYOB_Index2015_Followup10yr.rds'))
  } else if (target_ds_sex %in% c("Both", "Empty")) {
    target_ds_metric_list <- readRDS(paste0(temp_data_dir_step7_3, 'Outcome',
                                            target_ds, '_OutcomeYearSetting', j, 
                                            '_TestData_PredictionMetrics_ControlCase',
                                            control_case_ratio, 'to1_WhiteBritish_EvenYOB_Index2015_Followup10yr.rds'))
  } else if (target_ds_sex == "NULL" & j <= 4) {
    target_ds_metric_list <- readRDS(paste0(temp_data_dir_step7_3, 'Outcome',
                                            target_ds, '_OutcomeYearSetting', j,
                                            '_TestData_PredictionMetrics_ControlCase',
                                           control_case_ratio, 'to1_WhiteBritish_EvenYOB_Index2020_Followup5yr.rds'))
    
  } else if (target_ds_sex == "NULL" & j >= 5) {
    next
  }
  target_ds_metric_age_sex <- target_ds_metric_list$Age_Sex
  
  ##### Loop through each model and store testing metrics #####
  for (l in 1:n_models) {
    target_ds_metric_l <- target_ds_metric_list[[l]]
    update_idx <- (i-1)*n_models + l + (j-1) * n_models * n_outcome_ds
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
    sum_all_outcomes_metrics[[j]][[l]] <- sum_all_outcomes_metrics[[j]][[l]] + target_ds_metric_l
    sum_all_outcomes_metrics_delta[[j]][[l]] <- sum_all_outcomes_metrics_delta[[j]][[l]] + delta_target_ds_metric_l
    
  }
 }
}

avg_all_outcomes_metrics <- vector(mode = 'list', length = n_settings)
names(avg_all_outcomes_metrics) <- paste0('Year_', context_specific_settings_text)
avg_all_outcomes_metrics_delta <- avg_all_outcomes_metrics

for (j in 1:n_settings) {
  if (j <= 4) {
    avg_all_outcomes_metrics[[j]] <- lapply(sum_all_outcomes_metrics[[j]],
                                       function(x){x/n_outcome_ds})
    avg_all_outcomes_metrics_delta[[j]] <- lapply(sum_all_outcomes_metrics_delta[[j]],
                                             function(x){x/n_outcome_ds})
  } else if (j >= 5) {
    avg_all_outcomes_metrics[[j]] <- lapply(sum_all_outcomes_metrics[[j]],
                                            function(x){x/(n_outcome_ds - 1)})
    avg_all_outcomes_metrics_delta[[j]] <- lapply(sum_all_outcomes_metrics_delta[[j]],
                                                  function(x){x/(n_outcome_ds - 1)})
  }
}

##### Calculate average metrics summary over outcomes #####
metric_colnames <-  c(paste0(metrics_names, "_mean"),
                      paste0(metrics_names, "_SD"),
                      ## Delta is the difference between Model of interest and Age + sex model
                      paste0("Delta_", metrics_names, "_mean"),
                      paste0("Delta_", metrics_names, "_SD") )
avg_outcome_metrics_dt <- data.frame(
  "Outcome_phecode" = rep("Avg", each = n_models) %>% rep(n_settings),
  "Model" = rep(model_name_vec, n_settings),
  "Year" = rep(context_specific_settings_text, each = n_models)
)

avg_outcome_metrics_dt[, metric_colnames] <- NA

for (j in 1:n_settings) {
  for (l in 1:n_models) {
    avg_outcome_metrics_dt[l + (j-1) * n_models, 
                           paste0(metrics_names, "_mean")] <- avg_all_outcomes_metrics[[j]][[l]][1,]
    avg_outcome_metrics_dt[l + (j-1) * n_models, 
                           paste0(metrics_names, "_SD")] <- apply(avg_all_outcomes_metrics[[j]][[l]][-1,], 2, sd)
    
    
    avg_outcome_metrics_dt[l + (j-1) * n_models, 
                           paste0("Delta_", metrics_names, "_mean")] <- avg_all_outcomes_metrics_delta[[j]][[l]][1,]
    avg_outcome_metrics_dt[l + (j-1) * n_models, 
                           paste0("Delta_", metrics_names, "_SD")] <- apply(avg_all_outcomes_metrics_delta[[j]][[l]][-1,], 2, sd)
    
  }
}

### Combine metrics summary for individual outcomes and average metrics summary 
all_outcome_metrics_dt_final <- rbind(all_outcome_metrics_dt, avg_outcome_metrics_dt) %>%
  filter(!(Outcome_phecode == "COVID_hspt" & Year %in% c("7-8", "9-10")))


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
                                 "ByOutcomeYear_Sex_Specific_All_Outcomes+COVID_PredictionMetrics_ControlCase",
                                 control_case_ratio, "to1_WhiteBritish_EvenYOB_Index2015_Followup10yr.rds"))


#### Read all summary statistics ####
##### Sex-specific - prioritize #####
final_summ_stats_year <- readRDS(paste0(temp_data_dir_step8_0, 
                                          "ByOutcomeYear_Sex_Specific_All_Outcomes+COVID_PredictionMetrics_ControlCase",
                                          control_case_ratio, "to1_WhiteBritish_EvenYOB_Index2015_Followup10yr.rds"))


#### Explore metric performances ####
## On average: decreased performance with increasing year group
## Mixed trend vs. year group among individual outcomes and metrics
View(final_summ_stats_year$all_outcome_metrics_delta_dt %>% filter(Outcome_phecode == "Avg" &
                                                                       Model == "CRS_all_predictors"))

View(final_summ_stats_year$all_outcome_metrics_dt %>% filter(Outcome_phecode == "Avg" &
                                                                 Model == "CRS_all_predictors"))


View(final_summ_stats_year$all_outcome_metrics_delta_dt %>% filter(Outcome_phecode == "COVID_hspt" &
                                                                       Model == "CRS_all_predictors"))

View(final_summ_stats_year$all_outcome_metrics_dt %>% filter(Outcome_phecode == "COVID_hspt" &
                                                                 Model == "CRS_all_predictors"))


View(final_summ_stats_year$all_outcome_metrics_delta_dt %>% filter(Outcome_phecode == "165_1" &
                                                                       Model == "CRS_all_predictors"))

View(final_summ_stats_year$all_outcome_metrics_dt %>% filter(Outcome_phecode == "165_1" &
                                                                 Model == "CRS_all_predictors"))


View(final_summ_stats_year$all_outcome_metrics_delta_dt %>% filter(Outcome_phecode == "411_2" &
                                                                       Model == "CRS_all_predictors"))

View(final_summ_stats_year$all_outcome_metrics_dt %>% filter(Outcome_phecode == "411_2" &
                                                                 Model == "CRS_all_predictors"))


View(final_summ_stats_year$all_outcome_metrics_delta_dt %>% filter(Outcome_phecode == "994_2" &
                                                                       Model == "CRS_all_predictors"))

View(final_summ_stats_year$all_outcome_metrics_dt %>% filter(Outcome_phecode == "994_2" &
                                                                 Model == "CRS_all_predictors"))


#### Plot performance metrics vs. setting ####
source('~/collab/CCU022/CCU022_CRS/0.11_Plot_functions.R')

##### Filter to selected outcomes #####
metrics_vs_setting_dt <- final_summ_stats_year$all_outcome_metrics_delta_dt %>%
  filter(Outcome_phecode %in% selected_outcome_ds & Model == "CRS_all_predictors") %>%
  select(Outcome_phecode,
         Year,
         Delta_R2_liab_mean, 
         Delta_R2_liab_LB, 
         Delta_R2_liab_UB,
         Delta_AUROC_mean,
         Delta_AUROC_LB,
         Delta_AUROC_UB) %>%
  left_join(phecode_info[, c("Outcome_phecode", "Phenotype")], 
            by = "Outcome_phecode") %>%
  mutate(Phenotype = case_when(Outcome_phecode == "COVID_hspt" ~ "COVID-19 hospitalisation",
                               Outcome_phecode == "Avg" ~ "Average across 88 outcomes",
                               Outcome_phecode == "death" ~ "Death",
                               TRUE ~ Phenotype)) %>%
  mutate(Phenotype = factor(Phenotype, levels = selected_outcome_phenotype)) %>%
  mutate(yearidity = factor(Year, levels = c("1", "2", "3-4", "5-6", "7-8", "9-10"))) %>%
  mutate(alpha_category = factor(if_else(Outcome_phecode == "Avg", "Full", "Transparent"),
                                 levels = c("Full", "Transparent")))


##### Make plot for AUROC #####
metrics_vs_setting_dt_select <- metrics_vs_setting_dt %>%
  rename(context_setting = Year,
         metric_mean = Delta_AUROC_mean,
         metric_LB = Delta_AUROC_LB,
         metric_UB = Delta_AUROC_UB)

year_plot <- Fig5_plot(metrics_vs_setting_dt_select,
          setting_label = "Year of outcome incidence",
          metric_label = "AUROC",
          ref_linewidth = 0.6,
          metric_lower_lim = 0,
          metric_upper_lim = 0.25,
          metric_breaks = 0.05,
          point_size = 4,
          add_error_bar = T,
          error_bar_width = 0.1)


Fig5_plot_dodge(metrics_vs_setting_dt_select,
                setting_label = "Year of outcome incidence",
                metric_label = "AUROC",
                ref_linewidth = 0.6,
                metric_lower_lim = 0,
                metric_upper_lim = 0.25,
                metric_breaks = 0.05,
                point_size = 4,
                add_error_bar = T,
                error_bar_width = 0.1,
                dodge_width = 0.3)

##### Make plot for liability R2 #####
metrics_vs_setting_dt_select <- metrics_vs_setting_dt %>%
  rename(context_setting = yearidity,
         metric_mean = Delta_R2_liab_mean,
         metric_LB = Delta_R2_liab_LB,
         metric_UB = Delta_R2_liab_UB)

Fig5_plot(metrics_vs_setting_dt_select,
          setting_label = "Number of yearidities",
          metric_label = "Liability R2",
          ref_linewidth = 0.6,
          metric_lower_lim = -0.05,
          metric_upper_lim = 0.2,
          metric_breaks = 0.05,
          point_size = 4,
          add_error_bar = T,
          error_bar_width = 0.1)

Fig5_plot_dodge(metrics_vs_setting_dt_select,
                setting_label = "Number of yearidities",
                metric_label = "Liability R2",
                ref_linewidth = 0.6,
                metric_lower_lim = -0.05,
                metric_upper_lim = 0.2,
                metric_breaks = 0.05,
                point_size = 4,
                add_error_bar = T,
                error_bar_width = 0.1,
                dodge_width = 0.3)



