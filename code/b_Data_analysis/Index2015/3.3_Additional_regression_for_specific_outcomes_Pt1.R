### Author: Pearl Liu
### Find predictor covariates that contribute to the largest drop in effect sizes for tobacco use disorder/disorder of lipid metabolism
### Odd DOB: training, Even DOB: testing
### Index: 2015-01-01, follow-up: 10 years


rm(list = ls())
gc()

#### Load libraries ####
library(data.table)
library(tidyverse)
library(sandwich)
library(doParallel)

source('~/collab/CCU022/CCU022_CRS/0.1_Functions.R')

#### Set parameters ####
temp_data_dir_step1_0 <- '~/collab/CCU022/CCU022_CRS/Data/Step1.0/'
temp_data_dir_step1_1 <- '~/collab/CCU022/CCU022_CRS/Data/Step1.1/'
temp_data_dir_step2_4 <- '~/collab/CCU022/CCU022_CRS/Data/Step2.4/'
temp_data_dir <- '~/collab/CCU022/CCU022_CRS/Data/Step3.3/'
file_date <- '2025_05_01'

## Control-case ratio in logistic regression of training data
control_case_ratio <- 4
## Number of covariates 
n_covar <- 2
## Model names
model_name_vec <- c("CRS_all_predictors", 
                    "CRS_top100",
                    "CRS_top50", 
                    "CRS_top20", 
                    "CRS_top10",
                    "Age_Sex")
n_models <- length(model_name_vec)


covar_names_sex_specific <- c('age_at_2015')

#### Read processed data ####
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

all_pred_ds_names <- paste0('ds', pred_ds_vec)

##### Predictor prevalence #####
pred_ds_prev <- readRDS(paste0(temp_data_dir_step1_1, 
                               'WhiteBritish_OddYOB_Index2015_PredictorDiseasePrevalence.rds'))

##### Outcome incidence #####
outcome_incid_vec2015 <- readRDS(paste0(temp_data_dir_step2_4, 
                                    'TrainData_Outcome_Incidence_OddYOB_Index2015_Followup10yr.rds'))

outcome_incid_vec_sf <- readRDS(paste0(temp_data_dir_step2_4, 
                                       'TrainData_Outcome_Incidence_Sex_Specific_OddYOB_Index2015_Followup10yr.rds'))

outcome_incid_vec2020 <- readRDS(paste0(temp_data_dir_step2_4, 
                                    'TrainData_Outcome_Incidence_OddYOB_Index2020_Followup5yr.rds'))

outcome_incid_vec <- c(outcome_incid_vec2015, outcome_incid_vec2020[74])

##### Phecode information #####
phe_icd10_codelist_name = paste0("ccu022_01_PheCode_ICD10AtoN_map_by_PL_", file_date)
icd10_phecode_map <- readRDS(paste0(temp_data_dir_step1_0, 
                                    phe_icd10_codelist_name))
phecode_info <- icd10_phecode_map %>%
  select(PheCode, PheCode_parent, 
         Phenotype, Phenotype_parent,
         Category, Sex) %>%
  distinct()


#### Loop through target outcomes ####
target_outcome_ds_vec <- c("433_21", "COVID_hspt")
target_outcome_idx <- which(outcome_ds_vec %in% target_outcome_ds_vec)
target_pred_ds_names <- c("ds272", "ds318")

target_reg_coef_list <- vector(mode = "list", length = 2)
names(target_reg_coef_list) <- target_outcome_ds_vec

for (i in target_outcome_idx)  {
             
   target_ds <- outcome_ds_vec[i]
   if (target_ds == "433_21") {
      target_pred_name <- "ds272"
      covar_names <- c('age_at_2015', 'Female')
   } else if (target_ds == "COVID_hspt") {
     target_pred_name <- "ds318"
     covar_names <- c('age_at_2020', 'Female')
   }
   target_ds_incid <- outcome_incid_vec[i]
   
   if (target_ds == "COVID_hspt") {
     target_ds_sex <- "Both"
   } else {
     target_ds_sex <- phecode_info %>%
       filter(PheCode == str_replace(target_ds, '_', '\\.')) %>%
       pull(Sex)
   }
   
     
   #### General regression ####
   if ( target_ds_sex %in% c("Both", "Empty") ) {
     
     ##### Read training dataset #####
     if (target_ds == "COVID_hspt") {
       train_df_fit <- readRDS(paste0(temp_data_dir_step2_4, 'Outcome',
                                      target_ds, '_TrainData_Fit_ControlCase', control_case_ratio,
                                      'to1_WhiteBritish_OddYOB_Index2020_Followup5yr.rds'))
       
     } else {
       train_df_fit <- readRDS(paste0(temp_data_dir_step2_4, 'Outcome',
                                      target_ds, '_TrainData_Fit_ControlCase', control_case_ratio,
                                      'to1_WhiteBritish_OddYOB_Index2015_Followup10yr.rds'))
     }
     
     all_colnames <- colnames(train_df_fit)
     pred_ds_all <- intersect(all_pred_ds_names, all_colnames)
     pred_ds_all_i <- setdiff(pred_ds_all, target_pred_name)
     
     target_reg_coef_list[[target_ds]] <- data.frame("covar_pred_name" = rep(NA, length(pred_ds_all_i)),
                                                     "Est" = rep(NA, length(pred_ds_all_i)),
                                                     "SE" = rep(NA, length(pred_ds_all_i)))
     
     for (j in 1:length(pred_ds_all_i)) {
       covar_j <- pred_ds_all_i[j]
       
       ## Get regression formula
       reg_formula_j <- paste0('target_ds_status ~', paste(c(target_pred_name, covar_j, covar_names), 
                                                               collapse ='+')) %>% 
         as.formula()
       
       ##### Weighted logistic regression - Control:case = 4:1 #####
       model_j <- glm(reg_formula_j,
                      weights = ipcw_wts,
                      data = train_df_fit,
                      family = binomial())
       
       vcov_model_j <- vcovHC(model_j, type = 'HC')
       
       model_j_est <- summary(model_j)$coefficients[2,1]
       model_j_se <- sqrt(diag(vcov_model_j))[2]
       
       target_reg_coef_list[[target_ds]][j, "covar_pred_name"] <- covar_j
       target_reg_coef_list[[target_ds]][j, "Est"] <- model_j_est
       target_reg_coef_list[[target_ds]][j, "SE"] <- model_j_se
       
       print(paste("Completed Outcome ", i, " for covariate ", j))
     }
           
   }
}

saveRDS(target_reg_coef_list, paste0(temp_data_dir, 'Target_Outcomes_WeightedLogisticRegression_SummStats_ControlCase', 
                                      control_case_ratio,'to1_WhiteBritish_OddYOB.rds'))
   
#### Read results ####
target_reg_coef_list <- readRDS(paste0(temp_data_dir, 'Target_Outcomes_WeightedLogisticRegression_SummStats_ControlCase', 
                                       control_case_ratio,'to1_WhiteBritish_OddYOB.rds'))


