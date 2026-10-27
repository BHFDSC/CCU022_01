### Author: Pearl Liu
### For each outcome, derive summary statistics from performance metrics in testing data
### NOTE: this code is only for inspection of COVID-19 results, see Code 4.1 in Index2015 folder for combined derivation of summary stats for all outcomes + COVID-19 outcome
### Odd DOB: training, Even DOB: testing
### Index: 2020-01-01, follow-up: 5 years


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
temp_data_dir <- '~/collab/CCU022/CCU022_CRS/Data/Step4.1/'
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

#### Read processed data ####
##### Predictor prevalence #####
pred_ds_prev <- readRDS(paste0(temp_data_dir_step1_1, 
                               'WhiteBritish_OddYOB_Index2020_PredictorDiseasePrevalence.rds'))

##### Outcome incidence #####
outcome_incid_vec <- readRDS(paste0(temp_data_dir_step2_4, 
                                    'TrainData_Outcome_Incidence_OddYOB_Index2020_Followup5yr.rds'))


##### Phecode map #####
phe_icd10_codelist_name = paste0("ccu022_01_PheCode_ICD10AtoN_map_by_PL_", file_date)
icd10_phecode_map <- readRDS(paste0(temp_data_dir_step1_0, phe_icd10_codelist_name))

phecode_predictor_info <- icd10_phecode_map %>%
  mutate(PheCode_analysis = paste0("ds", PheCode_parent)) %>%
  select(PheCode_analysis, Phenotype_parent, Category) %>%
  distinct() 
  

##### List of predictors and outcomes #####
pred_ds_list_name = paste0("ccu022_01_Predictor_Diseases_Pheparent_list_by_PL_", file_date)
outcome_ds_list_name = paste0("ccu022_01_Outcome_Diseases_Phecode_list_by_PL_", file_date)

pred_ds_list = readRDS(paste0(temp_data_dir_step1_0, pred_ds_list_name))
outcome_ds_list = readRDS(paste0(temp_data_dir_step1_0, outcome_ds_list_name))

pred_ds_vec <- pred_ds_list %>% arrange(index) %>% pull(predictor_ds) 
outcome_ds_vec <- c(outcome_ds_list %>% arrange(index) %>% pull(target_ds),
                    "COVID_hspt")

n_pred_ds <- length(pred_ds_vec)
n_outcome_ds <- length(outcome_ds_vec)


#### Confirm the number of output files in previous steps ####
files_step4_0 <- list.files(temp_data_dir_step4_0)
length(files_step4_0) # 74


#### Check COVID hspt results ####
##### Read training dataset and explore #####
target_ds <- "COVID_hspt"
train_df_fit <- readRDS(paste0(temp_data_dir_step2_4, 'Outcome',
                               target_ds, '_TrainData_Fit_ControlCase', control_case_ratio,
                               'to1_WhiteBritish_OddYOB_Index2020_Followup5yr.rds'))
train_df_fit <- train_df_fit %>% 
  mutate(age_cat = case_when(age_at_2020 < 50 ~ "40-49",
                             (50 <= age_at_2020 & age_at_2020 < 65) ~ "50-64",
                             age_at_2020 >= 65 ~ "65-69"))

## Check pts with multiple sclerosis (phecode 335), which has the largest effect size for COVID hspt
pts_ds335 <- train_df_fit %>% filter(ds335 == 1)
mean(pts_ds335$age_at_2020)
mean(pts_ds335$Female == 1)

## average number of comorbidities
mean(rowSums(pts_ds335[,1:(n_outcome_ds-1)]))  # 3.897997
mean(rowSums(train_df_fit[,1:(n_outcome_ds-1)]))   # 1.893999

model_test <- glm(target_ds_status ~ ds335 + age_at_2020 + Female,
                  data = train_df_fit, 
                  family = binomial(link = "logit"), 
                  weights = ipcw_wts)

model_test2 <- glm(target_ds_status ~ age_cat + Female,
                  data = train_df_fit, 
                  family = binomial(link = "logit"), 
                  weights = ipcw_wts)

model_test3 <- glm(target_ds_status ~ age_at_2020 + Female,
                   data = train_df_fit, 
                   family = binomial(link = "logit"), 
                   weights = ipcw_wts)

## smoking and COPD
model_test4 <- glm(target_ds_status ~ ds318 + age_at_2020 + Female,
                   data = train_df_fit, 
                   family = binomial(link = "logit"), 
                   weights = ipcw_wts)

model_test5 <- glm(target_ds_status ~ ds318 + ds496 + age_at_2020 + Female,
                   data = train_df_fit, 
                   family = binomial(link = "logit"), 
                   weights = ipcw_wts)

model_test6 <- glm(target_ds_status ~ ds318 + ds496 + ds411 + ds480 + ds509 + ds519 + 
                     age_at_2020 + Female,
                   data = train_df_fit, 
                   family = binomial(link = "logit"), 
                   weights = ipcw_wts)

##### Check effect sizes #####
target_ds <- "COVID_hspt"
covid_summ_list <- readRDS(paste0(temp_data_dir_step3_0, 'Outcome',
                                      target_ds, '_WeightedLogisticRegression_SummStats_ControlCase', 
                                      control_case_ratio,'to1_WhiteBritish_OddYOB_Index2020_Followup5yr.rds'))

covid_summ_list_full_model <- covid_summ_list$CRS_all_predictors %>%
  mutate(PheCode_analysis = rownames(covid_summ_list$CRS_all_predictors)) %>%
  merge(phecode_predictor_info, by = "PheCode_analysis", all.x = T) %>%
  select(Est, SE, PheCode_analysis, Phenotype_parent) %>%
  arrange(desc(abs(Est)))
View(covid_summ_list_full_model)

covid_summ_list_top100 <- covid_summ_list$CRS_top100 %>%
  mutate(PheCode_analysis = rownames(covid_summ_list$CRS_top100)) %>%
  merge(phecode_predictor_info, by = "PheCode_analysis", all.x = T) %>%
  select(Est, SE, PheCode_analysis, Phenotype_parent) %>%
  arrange(desc(abs(Est)))
View(covid_summ_list_top100)

covid_summ_list_age_sex <- covid_summ_list$Age_Sex
View(covid_summ_list_age_sex)

##### Process performance metrics #####
outcome_ds <- c("COVID_hspt")
n_outcome_ds_single <- 1

## Store summary statistics of performance metrics of testing data
metrics_names <-  c("R2_obs", "R2_liab","AUROC", "C_index", "AUPRC", "AUPRC_prev_diff")
metric_colnames <-  c(paste0(metrics_names, "_mean"),
                      paste0(metrics_names, "_SD"),
                      ## Delta is the difference between Model of interest and Age + sex model
                      paste0("Delta_", metrics_names, "_mean"),
                      paste0("Delta_", metrics_names, "_SD") )
covid_metrics_dt <- data.frame(
  "Outcome_phecode" = rep(outcome_ds, each = n_models),
  "Model" = rep(model_name_vec, n_outcome_ds_single)
)

covid_metrics_dt[, metric_colnames] <- NA


## Store R2 of training data
covid_train_R2_dt <- data.frame(
  "Outcome_phecode" = rep(outcome_ds, each = n_models),
  "Model" = rep(model_name_vec, n_outcome_ds_single),
  "R2_obs" = NA,
  "R2_liab" = NA
)

for (i in 1:n_outcome_ds_single) {
  target_ds <- outcome_ds[i]
  
  ### Read performance metrics 
  target_ds_metric_list <- readRDS(paste0(temp_data_dir_step4_0, 'Outcome',
                                          target_ds, '_TestData_PredictionMetrics_ControlCase',
                                          control_case_ratio, 'to1_WhiteBritish_EvenYOB_Index2020_Followup5yr.rds'))
  
  target_ds_metric_age_sex <- target_ds_metric_list$Age_Sex
  
  ### Loop through each model and store testing metrics 
  for (j in 1:n_models) {
    target_ds_metric_j <- target_ds_metric_list[[j]]
    update_idx <- (i-1)*n_models + j
    covid_metrics_dt[update_idx, 
                           paste0(metrics_names, "_mean")] <- colMeans(target_ds_metric_j)
    covid_metrics_dt[update_idx, 
                           paste0(metrics_names, "_SD")] <- apply(target_ds_metric_j, 2, sd)
    
    delta_target_ds_metric_j <- target_ds_metric_j - target_ds_metric_age_sex
    
    covid_metrics_dt[update_idx, 
                           paste0("Delta_", metrics_names, "_mean")] <- colMeans(delta_target_ds_metric_j)
    covid_metrics_dt[update_idx, 
                           paste0("Delta_", metrics_names, "_SD")] <- apply(delta_target_ds_metric_j, 2, sd)
    
  }
  
  ### Store training R2 
  train_R2_dt <- readRDS(paste0(temp_data_dir_step3_0, 'Outcome',
                                target_ds, '_TrainData_ObservedAndLiabilityR2_OddYOB_Index2020_Followup5yr.rds'))
  target_ds_idx <- which(covid_train_R2_dt$Outcome_phecode == target_ds)
  covid_train_R2_dt[target_ds_idx, c("R2_obs", "R2_liab")] <- train_R2_dt
  
}

### Calculate 95% CI and p-value for difference between model of interest and age+sex model 
covid_metrics_delta_dt <- covid_metrics_dt[, c("Outcome_phecode",
                                                           "Model", 
                                                           paste0("Delta_", metrics_names, "_mean"))]
covid_metrics_delta_dt[, paste0("Delta_", metrics_names, "_LB")] <- NA
covid_metrics_delta_dt[, paste0("Delta_", metrics_names, "_UB")] <- NA
covid_metrics_delta_dt[, paste0("Delta_", metrics_names, "_Pval")] <- NA

## Loop through each metric
n_metrics <- length(metrics_names)
for (k in 1:n_metrics) {
  metric_k <- metrics_names[k]
  delta_k_mean <- covid_metrics_dt[, paste0("Delta_", metric_k, "_mean")]
  delta_k_se <- covid_metrics_dt[, paste0("Delta_", metric_k, "_SD")]/sqrt(n_mc)
  covid_metrics_delta_dt[, paste0("Delta_", metric_k, "_LB")] <- delta_k_mean - qnorm(0.975) * delta_k_se
  covid_metrics_delta_dt[, paste0("Delta_", metric_k, "_UB")] <- delta_k_mean + qnorm(0.975) * delta_k_se
  covid_metrics_delta_dt[, paste0("Delta_", metric_k, "_Pval")] <- 2*pnorm(abs(delta_k_mean/delta_k_se),
                                                                                 lower.tail = F)
}

View(covid_metrics_dt)
View(covid_metrics_delta_dt)
View(covid_train_R2_dt)


##### Save COVID hspt outcome #####
covid_summ_stats <- list(all_outcome_metrics_dt = covid_metrics_dt,
                         all_outcome_metrics_delta_dt = covid_metrics_delta_dt,
                         all_outcome_train_R2_dt = covid_train_R2_dt)

saveRDS(covid_summ_stats, paste0(temp_data_dir, 
                                 "COVID_hspt_PredictionMetrics_ControlCase",
                                 control_case_ratio, "to1_WhiteBritish_Index2020_Followup5yr.rds"))



#### Loop through each outcome ####

## Store summary statistics of performance metrics of testing data
metrics_names <-  c("R2_obs", "R2_liab","AUROC", "C_index", "AUPRC", "AUPRC_prev_diff")
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


## Store R2 of training data
all_outcome_train_R2_dt <- data.frame(
  "Outcome_phecode" = rep(outcome_ds_vec, each = n_models),
  "Model" = rep(model_name_vec, n_outcome_ds),
  "R2_obs" = NA,
  "R2_liab" = NA
)

for (i in 1:n_outcome_ds) {
  target_ds <- outcome_ds_vec[i]
  
  ##### Read performance metrics #####
  target_ds_metric_list <- readRDS(paste0(temp_data_dir_step4_0, 'Outcome',
                                        target_ds, '_TestData_PredictionMetrics_ControlCase',
                                        control_case_ratio, 'to1_WhiteBritish_EvenYOB_Index2020_Followup5yr.rds'))
  
  target_ds_metric_age_sex <- target_ds_metric_list$Age_Sex
  
  ##### Loop through each model and store testing metrics #####
  for (j in 1:n_models) {
    target_ds_metric_j <- target_ds_metric_list[[j]]
    update_idx <- (i-1)*n_models + j
    all_outcome_metrics_dt[update_idx, 
                           paste0(metrics_names, "_mean")] <- colMeans(target_ds_metric_j)
    all_outcome_metrics_dt[update_idx, 
                           paste0(metrics_names, "_SD")] <- apply(target_ds_metric_j, 2, sd)
    
    delta_target_ds_metric_j <- target_ds_metric_j - target_ds_metric_age_sex
    
    all_outcome_metrics_dt[update_idx, 
                           paste0("Delta_", metrics_names, "_mean")] <- colMeans(delta_target_ds_metric_j)
    all_outcome_metrics_dt[update_idx, 
                           paste0("Delta_", metrics_names, "_SD")] <- apply(delta_target_ds_metric_j, 2, sd)
    
  }
  
  ##### Store training R2 #####
  train_R2_dt <- readRDS(paste0(temp_data_dir_step3_0, 'Outcome',
                                target_ds, '_TrainData_ObservedAndLiabilityR2_OddYOB_Index2020_Followup5yr.rds'))
  target_ds_idx <- which(all_outcome_train_R2_dt$Outcome_phecode == target_ds)
  all_outcome_train_R2_dt[target_ds_idx, c("R2_obs", "R2_liab")] <- train_R2_dt
  
}

#### Calculate 95% CI and p-value for difference between model of interest and age+sex model ####
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

##### Check number of significant differences and average difference for each metric #####
## Loop through each model and metric
n_pos_diff_dt <- data.frame("Model" = model_name_vec)
n_pos_diff_dt[, metrics_names] <- NA

n_neg_diff_dt <- n_pos_diff_dt

avg_diff_dt <- n_pos_diff_dt
avg_metric_dt <- n_pos_diff_dt
avg_perc_diff_dt <- n_pos_diff_dt

age_sex_avg_metrics <- all_outcome_metrics_dt %>%
  filter(Model == "Age_Sex") %>%
  select(paste0(metrics_names, "_mean")) %>%
  colMeans()

for (j in 1:n_models) {
  model_j <- model_name_vec[j]
  model_j_metrics_delta <- all_outcome_metrics_delta_dt %>%
    filter(Model == model_j)
  model_j_metrics <- all_outcome_metrics_dt %>%
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
  

View(n_pos_diff_dt[,-7])
View(n_neg_diff_dt[,-7])

View(avg_metric_dt)

View(avg_diff_dt)

View(avg_perc_diff_dt)


#### Save all summary statistics ####
final_summ_stats <- list(all_outcome_metrics_dt = all_outcome_metrics_dt,
                         all_outcome_metrics_delta_dt = all_outcome_metrics_delta_dt,
                         all_outcome_train_R2_dt = all_outcome_train_R2_dt,
                         avg_metric_dt = avg_metric_dt,
                         avg_diff_dt = avg_diff_dt,
                         avg_perc_diff_dt = avg_perc_diff_dt,
                         n_pos_diff_dt = n_pos_diff_dt,
                         n_neg_diff_dt = n_neg_diff_dt)

saveRDS(final_summ_stats, paste0(temp_data_dir, 
              "All_Outcomes_PredictionMetrics_ControlCase",
              control_case_ratio, "to1_WhiteBritish_Index2020_Followup5yr.rds"))

#### Read all summary statistics ####
final_summ_stats <- readRDS(paste0(temp_data_dir, 
                                    "All_Outcomes_PredictionMetrics_ControlCase",
                                    control_case_ratio, "to1_WhiteBritish_Index2020_Followup5yr.rds"))
all_outcome_metrics_dt <- final_summ_stats$all_outcome_metrics_dt

#### Plot testing R2_liab vs number of predictors ####
model_label1 <- c(n_pred_ds, 100, 50, 20, 10, "Age + Sex")
plot(1:nrow(avg_metric_dt), avg_metric_dt$R2_liab, xaxt = "n",
     ylim = c(0.05, 0.11), 
     xlab = "Number of predictor diseases", 
     ylab = "Average testing R2_liab across outcomes")
axis(1, at=1:nrow(avg_metric_dt), labels=model_label1)


model_label2 <- c(n_pred_ds, 100, 50, 20, 10)
avg_metric_dt2 <- avg_metric_dt %>% filter(Model != "Age_Sex")
plot(model_label2, avg_metric_dt2$R2_liab, 
     xaxt = "n",
     ylim = c(0.05, 0.11), 
     xlab = "Number of predictor diseases", 
     ylab = "Average testing R2_liab across outcomes")
axis(1, at = seq(10, 220, by = 10), las=2)

#### Plot training R2_liab vs. testing R2_liab ####
##### CRS with all predictors #####
all_outcome_train_R2_dt_crs_full <- all_outcome_train_R2_dt %>%
  filter(Model == "CRS_all_predictors")
all_outcome_test_R2_dt_crs_full <- all_outcome_metrics_dt %>%
  filter(Model == "CRS_all_predictors")

training_vs_testing_R2_crs_full <- data.frame("Outcome_phecode" = all_outcome_test_R2_dt_crs_full[, c('Outcome_phecode')],
                                     'Testing_R2_liab' = all_outcome_test_R2_dt_crs_full$R2_liab_mean,
                                     'Training_R2_liab' = all_outcome_train_R2_dt_crs_full$R2_liab)

## color by outcome incidence
plot(training_vs_testing_R2_crs_full$Testing_R2_liab,
     training_vs_testing_R2_crs_full$Training_R2_liab,
     xlab = "Testing R2_liab", ylab = "Training R2_liab", 
     main = "CRS with all predictors",
     xlim = c(0, 0.3), ylim = c(0, 0.3))
abline(0, 1)

training_vs_testing_R2_crs_full %>%
  filter(Testing_R2_liab >= Training_R2_liab) %>%
  View()

##### CRS with top 100 predictors #####
all_outcome_train_R2_dt_crs_top100 <- all_outcome_train_R2_dt %>%
  filter(Model == "CRS_top100")
all_outcome_test_R2_dt_crs_top100 <- all_outcome_metrics_dt %>%
  filter(Model == "CRS_top100")

training_vs_testing_R2_crs_top100 <- data.frame("Outcome_phecode" = all_outcome_test_R2_dt_crs_top100[, c('Outcome_phecode')],
                                              'Testing_R2_liab' = all_outcome_test_R2_dt_crs_top100$R2_liab_mean,
                                              'Training_R2_liab' = all_outcome_train_R2_dt_crs_top100$R2_liab)

plot(training_vs_testing_R2_crs_top100$Testing_R2_liab,
     training_vs_testing_R2_crs_top100$Training_R2_liab,
     xlab = "Testing R2_liab", ylab = "Training R2_liab",
     main = "CRS with top 100 predictors",
     xlim = c(0, 0.3), ylim = c(0, 0.3))
abline(0, 1)

training_vs_testing_R2_crs_top100 %>%
  filter(Testing_R2_liab >= Training_R2_liab) %>%
  View()




