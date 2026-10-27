### Author: Pearl Liu
### Compare cumulative incidence by predicted risk groups in testing data
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
temp_data_dir_step9_0 <- '~/collab/CCU022/CCU022_CRS/Data/Step9.0/'
temp_data_dir_step16_1 <- '~/collab/CCU022/CCU022_CRS/Data/Step16.1/'
temp_data_dir <- '~/collab/CCU022/CCU022_CRS/Data/Step9.1/'
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
outcome_ds_vec <- c(outcome_ds_list %>% 
                      arrange(index) %>%
                      ## Remove phecode 622
                      filter(target_ds != "622") %>%
                      pull(target_ds),
                    "COVID_hspt",
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
  distinct()

#### Get list of CRS-Delphi overlapping outcomes ####
crs_delphi_map <- read.table(paste0(temp_data_dir_step16_1, "CRS_phecode_vs_Delphi_ICD10_map.txt"),
                             sep = "\t", header = T, quote = "", fill = T) 

outcome_ds_delphi_vec <- crs_delphi_map %>%
  select(Outcome_phecode) %>%
  mutate(Outcome_phecode = 
           if_else(Outcome_phecode == "Dea", "death", Outcome_phecode)) %>%
  filter(Outcome_phecode != "278_1") %>%  # remove obesity
  pull(Outcome_phecode) %>%
  unique() %>%
  sort()

n_outcome_ds_delphi <- length(outcome_ds_delphi_vec)

#### Calculate the fold increase in observed risk of top X% risk group vs. overall risk ####
outcome_risk_increase_vec <- data.frame("Outcome_phecode" = outcome_ds_vec,
                                       "Full_pop_risk" = rep(NA, n_outcome_ds),
                                       "Top10q_risk" = rep(NA, n_outcome_ds),
                                       "Top10q_risk_fold" = rep(NA, n_outcome_ds),
                                       "Mid80q_risk" = rep(NA, n_outcome_ds),
                                       "Bottom10q_risk" = rep(NA, n_outcome_ds),
                                       "Top5q_risk" = rep(NA, n_outcome_ds),
                                       "Top5q_risk_fold" = rep(NA, n_outcome_ds),
                                       "Top1q_risk" =  rep(NA, n_outcome_ds),
                                       "Top1q_risk_fold" =  rep(NA, n_outcome_ds))
for (i in 1:n_outcome_ds) {
  target_ds_i <- outcome_ds_vec[i]
  
  if (target_ds_i == "COVID_hspt") {
    surv_summ_full_pop_i <- read.csv(paste0(temp_data_dir_step9_0, 
                                            "Outcome", target_ds_i, "_SurvivalAnalysisSummaryFullPop_WhiteBritish_EvenYOB_Index2020_Followup5yr.csv"))
    surv_summ_by_risk_i <- read.csv(paste0(temp_data_dir_step9_0, 
                                           "Outcome", target_ds_i, "_SurvivalAnalysisSummary_WhiteBritish_EvenYOB_Index2020_Followup5yr.csv"))
    surv_summ_by_risk_i_v2 <- read.csv(paste0(temp_data_dir_step9_0, 
                                           "Outcome", target_ds_i, "_SurvivalAnalysisSummaryV2_WhiteBritish_EvenYOB_Index2020_Followup5yr.csv"))
    surv_summ_by_risk_i_v3 <- read.csv(paste0(temp_data_dir_step9_0, 
                                           "Outcome", target_ds_i, "_SurvivalAnalysisSummaryV3_WhiteBritish_EvenYOB_Index2020_Followup5yr.csv"))
    
    
  } else {
    surv_summ_full_pop_i <- read.csv(paste0(temp_data_dir_step9_0, 
                                            "Outcome", target_ds_i, "_SurvivalAnalysisSummaryFullPop_WhiteBritish_EvenYOB_Index2015_Followup10yr.csv"))
    surv_summ_by_risk_i <- read.csv(paste0(temp_data_dir_step9_0, 
                                            "Outcome", target_ds_i, "_SurvivalAnalysisSummary_WhiteBritish_EvenYOB_Index2015_Followup10yr.csv"))
    surv_summ_by_risk_i_v2 <- read.csv(paste0(temp_data_dir_step9_0, 
                                           "Outcome", target_ds_i, "_SurvivalAnalysisSummaryV2_WhiteBritish_EvenYOB_Index2015_Followup10yr.csv"))
    surv_summ_by_risk_i_v3 <- read.csv(paste0(temp_data_dir_step9_0, 
                                           "Outcome", target_ds_i, "_SurvivalAnalysisSummaryV3_WhiteBritish_EvenYOB_Index2015_Followup10yr.csv"))
    
  }
  
  full_pop_risk <- surv_summ_full_pop_i %>% 
     filter(time == max(time)) %>%
     mutate(cuminc = 1 - surv) %>%
     pull(cuminc)
  
  top10q_risk <- surv_summ_by_risk_i %>%
    filter(target_ds_pred_group == "Top 10%") %>%
    filter(time == max(time)) %>%
    mutate(cuminc = 1 - surv) %>%
    pull(cuminc)
  
  mid80q_risk <- surv_summ_by_risk_i %>%
    filter(target_ds_pred_group == "Middle 80%") %>%
    filter(time == max(time)) %>%
    mutate(cuminc = 1 - surv) %>%
    pull(cuminc)
  
  bottom10q_risk <- surv_summ_by_risk_i %>%
    filter(target_ds_pred_group == "Bottom 10%") %>%
    filter(time == max(time)) %>%
    mutate(cuminc = 1 - surv) %>%
    pull(cuminc)
  
  top5q_risk <- surv_summ_by_risk_i_v2 %>%
    filter(target_ds_pred_group == "Top 5%") %>%
    filter(time == max(time)) %>%
    mutate(cuminc = 1 - surv) %>%
    pull(cuminc)
  
  top1q_risk <- surv_summ_by_risk_i_v3 %>%
    filter(target_ds_pred_group == "Top 1%") %>%
    filter(time == max(time)) %>%
    mutate(cuminc = 1 - surv) %>%
    pull(cuminc)
  
  outcome_risk_increase_vec[i, c("Full_pop_risk",
                                 "Top10q_risk",
                                 "Mid80q_risk",
                                 "Bottom10q_risk",
                                 "Top5q_risk",
                                 "Top1q_risk")] <- c(full_pop_risk,
                                                        top10q_risk,
                                                        mid80q_risk,
                                                        bottom10q_risk,
                                                     top5q_risk,
                                                     top1q_risk)
  outcome_risk_increase_vec[i, "Top10q_risk_fold"] <- top10q_risk/full_pop_risk
  outcome_risk_increase_vec[i, "Top5q_risk_fold"] <- top5q_risk/full_pop_risk
  outcome_risk_increase_vec[i, "Top1q_risk_fold"] <- top1q_risk/full_pop_risk
}


saveRDS(outcome_risk_increase_vec,
        paste0(temp_data_dir, "Cumulative_incidence_risk_comparison.rds"))

##### Check average #####
outcome_risk_increase_vec <- readRDS(paste0(temp_data_dir, "Cumulative_incidence_risk_comparison.rds"))
outcome_risk_increase_vec %>% 
  select(-c(Outcome_phecode)) %>%
  colMeans()

# Full_pop_risk   Top10q_risk   Top10q_risk_fold      Mid80q_risk   Bottom10q_risk 
# 0.03596358       0.09336093       2.78604270       0.03219279       0.01291722 
# Top5q_risk   Top5q_risk_fold       Top1q_risk  Top1q_risk_fold 
# 0.11250986       3.43878143       0.15817526       5.12906068 


## Average for Delphi-overlapping outcomes
outcome_risk_increase_vec %>% 
  filter(Outcome_phecode %in% outcome_ds_delphi_vec) %>%
  select(-c(Outcome_phecode)) %>%
  colMeans()
# Full_pop_risk   Top10q_risk   Top10q_risk_fold    Mid80q_risk   Bottom10q_risk 
# 0.04108314       0.10578249       2.71732466       0.03689074       0.01457407 
# Top5q_risk  Top5q_risk_fold       Top1q_risk   Top1q_risk_fold 
# 0.12638189       3.31234243       0.17468444       4.84852520 


outcome_risk_increase_vec %>%
  filter(Outcome_phecode %in% c("165_1", "250_2", "411_2", "death", "COVID_hspt")) %>%
  select(Outcome_phecode, Full_pop_risk, 
         Top10q_risk, Top10q_risk_fold,
         Top5q_risk, Top5q_risk_fold, 
         Top1q_risk, Top1q_risk_fold) %>%
  View()

#### Output risk ratio and CI for all outcomes ####
outcome_RR_summ_list_CRS <- vector(mode = "list", length = n_outcome_ds)
outcome_RR_summ_list_age_sex <- vector(mode = "list", length = n_outcome_ds)

for (i in 1:n_outcome_ds) {
  target_ds <- outcome_ds_vec[i]
  if (target_ds == "COVID_hspt") {
    outcome_RR_summ_CRS <- readRDS(paste0(temp_data_dir_step9_0, "Outcome", target_ds, 
                                          "_RiskFoldSummaryV2_WithCI_WhiteBritish_EvenYOB_Index2020_Followup5yr.rds"))
    outcome_RR_summ_age_sex <- readRDS(paste0(temp_data_dir_step9_0, "Outcome", target_ds, 
                                          "_Age_Sex_RiskFoldSummaryV2_WithCI_WhiteBritish_EvenYOB_Index2020_Followup5yr.rds"))
    
  } else {
    outcome_RR_summ_CRS <- readRDS(paste0(temp_data_dir_step9_0, "Outcome", target_ds, 
                                          "_RiskFoldSummaryV2_WithCI_WhiteBritish_EvenYOB_Index2015_Followup10yr.rds"))
    outcome_RR_summ_age_sex <- readRDS(paste0(temp_data_dir_step9_0, "Outcome", target_ds, 
                                              "_Age_Sex_RiskFoldSummaryV2_WithCI_WhiteBritish_EvenYOB_Index2015_Followup10yr.rds"))
    
  }
  
  outcome_RR_summ_list_CRS[[i]] <- outcome_RR_summ_CRS
  outcome_RR_summ_list_age_sex[[i]] <- outcome_RR_summ_age_sex
}

outcome_RR_summ_df_CRS <- bind_rows(outcome_RR_summ_list_CRS) %>%
  mutate(Model_type = "CRS_all_predictors",
         Outcome_phecode = outcome_ds_vec)
outcome_RR_summ_df_age_sex <- bind_rows(outcome_RR_summ_list_age_sex) %>%
  mutate(Model_type = "Age_Sex",
         Outcome_phecode = outcome_ds_vec)
outcome_RR_summ_df_final <- rbind(outcome_RR_summ_df_CRS,
                                  outcome_RR_summ_df_age_sex) %>%
  arrange(Outcome_phecode, Model_type)

write.csv(outcome_RR_summ_df_final,
          paste0(temp_data_dir, "All_outcomes_Risk_ratio_with_CI.csv"),
          row.names = F)


