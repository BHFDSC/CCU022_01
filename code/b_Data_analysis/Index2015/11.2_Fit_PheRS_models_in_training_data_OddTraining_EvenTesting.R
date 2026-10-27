### Author: Pearl Liu
### For each PheRS outcome, Fit PheRS + age + sex model in training data
### Odd DOB: training, Even DOB: testing
### Index: 2015-01-01, follow-up: 10 years
### 11 outcomes


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
temp_data_dir_step11_0 <- '~/collab/CCU022/CCU022_CRS/Data/Step11.0/'
temp_data_dir_step11_1 <- '~/collab/CCU022/CCU022_CRS/Data/Step11.1/'
temp_data_dir <- '~/collab/CCU022/CCU022_CRS/Data/Step11.2/'
file_date <- '2025_05_01'

## Control-case ratio in logistic regression of training data
control_case_ratio <- 4
## Number of covariates 
n_covar <- 2
## Model names
model_name_vec <- c("PheRS_age_sex",
                    "PheRS_destd_age_sex")
n_models <- length(model_name_vec)

## Covariate names
covar_names <- c('age_at_2015', 'Female')
covar_names_sex_specific <- c('age_at_2015')

#### Read processed data ####
##### List of predictors and outcomes #####
pred_ds_list_name = paste0("ccu022_01_PheRS_Predictor_Diseases_Pheparent_list_by_PL_", file_date)
pred_ds_list = readRDS(paste0(temp_data_dir_step1_0, pred_ds_list_name))

pred_ds_vec <- pred_ds_list %>% arrange(index) %>% pull(predictor_ds) 

## PheRS outcomes include: 
## colorectal cancer, breast cancer (female only), type ii diabetes, prostate cancer (male only), 
## MI, Coronary atherosclerosis, asthma, Afib and flutter, 
## lung cancer, osteoarthritis (localized), gout
outcome_ds_vec <- c("153", "174_1", "250_2", "185",
                    "411_2", "411_4", "495", "427_2", 
                    "165_1", "740_1", "740_1", "274_1")

## Repeat osteoarthritis (localized) twice to cover Hip OA and Knee OA

## Outcome code for output
outcome_ds_vec_output <- c("153", "174_1", "250_2", "185",
                           "411_2", "411_4", "495", "427_2", 
                           "165_1", "740_1hipOA", "740_1kneeOA", "274_1")

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

n_pred_ds <- length(pred_ds_vec)
n_outcome_ds <- length(outcome_ds_vec)

##### Predictor prevalence #####
pred_ds_prev <- readRDS(paste0(temp_data_dir_step1_1, 
                               'WhiteBritish_OddYOB_Index2015_PheRSPredictorDiseasePrevalence.rds'))
pred_ds_prev_df <- data.frame("Predictor" = paste0("ds", names(pred_ds_prev)),
                              "Prevalence" = pred_ds_prev)

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

##### Phecode information #####
phe_icd10_codelist_name = paste0("ccu022_01_PheCode_ICD10AtoN_map_by_PL_", file_date)
icd10_phecode_map <- readRDS(paste0(temp_data_dir_step1_0, 
                                    phe_icd10_codelist_name))
phecode_info <- icd10_phecode_map %>%
  select(PheCode, PheCode_parent, 
         Phenotype, Phenotype_parent,
         Category, Sex) %>%
  distinct()

## Info on sex-specificity of predictor phecodes
pred_phecode_info0 <- phecode_info %>%
  select(PheCode_parent, Phenotype_parent, Sex) %>%
  distinct() %>%
  filter(PheCode_parent %in% names(pred_ds_prev)) %>%
  group_by(PheCode_parent) %>%
  mutate(Sex_parent = case_when(all(Sex == "Female")  ~ "Female",
                                all(Sex == "Male") ~ "Male",
                                TRUE ~ "Both"))

pred_phecode_info <- pred_phecode_info0 %>%
  select(PheCode_parent, Sex_parent) %>%
  distinct() %>%
  arrange(PheCode_parent)

# all.equal(pred_phecode_info$PheCode_parent, pred_ds_vec)


##### Read PheRS effect sizes #####
phers_coef_summ0 <- read.table(paste0(temp_data_dir, "PheRS_Coef_final.txt"),
                               sep = "\t", header = T, quote = "", fill = T) 
phers_coef_summ <- phers_coef_summ0 %>%
  filter(Category != "baseline") %>%
  mutate(Predictor = paste0("ds", Code)) %>%
  inner_join(pred_ds_prev_df, by = "Predictor") %>%
  ## Add de-standardized effect sizes
  mutate(UKB_coef_destd = UKB_coef / sqrt(Prevalence * (1 - Prevalence)))

#### Loop through each outcome ####

## progress bar
#pb <- txtProgressBar(min = 1, max = n_outcome_ds, style = 3)

## Set up clusters
#cl <- makeCluster(n_cores, outfile = "")
#registerDoParallel(cl)


#t_start <- Sys.time()
#foreach (i = 1:n_outcome_ds, 
#         .packages = c("data.table", "tidyverse", "sandwich")) %dopar% {

for (i in 1:n_outcome_ds) { 
  target_ds <- outcome_ds_vec[i]
  target_ds_incid <- outcome_incid_vec[i]
  
  target_ds_sex <- phecode_info %>%
    filter(PheCode == str_replace(target_ds, '_', '\\.')) %>%
    pull(Sex)
  
  target_file_name_general <-  paste0(temp_data_dir, 'Outcome',
                         outcome_ds_vec_output[i], '_PheRS_WeightedLogisticRegression_SummStats_ControlCase', 
                         control_case_ratio,'to1_WhiteBritish_OddYOB_Index2015_Followup10yr.rds')
  target_file_name_sex_specific <-  paste0(temp_data_dir, 'Outcome',
                                           outcome_ds_vec_output[i], '_PheRS_Sex_Specific_WeightedLogisticRegression_SummStats_ControlCase', 
                                      control_case_ratio,'to1_WhiteBritish_OddYOB_Index2015_Followup10yr.rds')
  
  if ( (target_ds_sex %in% c("Both", "Empty") & 
        file.exists(target_file_name_general)) | 
       (target_ds_sex %in% c("Female", "Male") & 
        file.exists(target_file_name_general) & 
        file.exists(target_file_name_sex_specific)) ) {
    
    print(paste0('Completed Outcome ', i)) 
    
  } else {
  
  #### Sex-specific regression ####
  if ( target_ds_sex %in% c("Male", "Female") & 
       file.exists(target_file_name_sex_specific) == FALSE ) {
    
    target_ds_incid_sf <- outcome_incid_vec_sf[i]
    
    ##### Read sex-specific training dataset #####
    train_df_fit <- readRDS(paste0(temp_data_dir_step11_1, 'Outcome',
                                   target_ds, '_PheRS_TrainData_Fit_Sex_Specific_ControlCase', control_case_ratio,
                                   'to1_WhiteBritish_OddYOB_Index2015_Followup10yr.rds'))
    
    ##### Get predicted PheRS risks #####
    train_df_fit_update <- PheRS_calc(train_df_fit,
                                      phers_coef_summ,
                                      outcome_ds_names[i],
                                      target_ds_sex)
    
    ##### Get regression formula #####
    reg_formula_PheRS <- paste0('target_ds_status ~', paste(c("PheRS_risk", covar_names_sex_specific), 
                                                            collapse ='+')) %>% 
      as.formula()
    
    reg_formula_PheRS_destd <- paste0('target_ds_status ~', paste(c("PheRS_destd_risk", covar_names_sex_specific), 
                                                            collapse ='+')) %>% 
      as.formula()
    
    reg_formula_list <- list(reg_formula_PheRS,
                             reg_formula_PheRS_destd)
    
    ##### Loop through each model #####
    ## General info
    Y <- train_df_fit_update$target_ds_status
    mean_Y <- mean(Y)
    
    ## Create a list of summary statistics
    target_ds_summ_list <- vector(mode = 'list', length = n_models)
    names(target_ds_summ_list) <- model_name_vec
    
    ## Create a dataframe of R2
    train_R2_dt <- data.frame("R2_obs" = rep(NA, n_models),
                              "R2_liab" = rep(NA, n_models))
    rownames(train_R2_dt) <- model_name_vec
    
    for (j in 1:n_models) {
      reg_formula <- reg_formula_list[[j]]
      model_name <- model_name_vec[j]
      
      ##### Weighted logistic regression - Control:case = 4:1 #####
      model_j <- glm(reg_formula,
                     weights = ipcw_wts_sf,  # use sex-specific weights
                     data = train_df_fit_update,
                     family = binomial())
      
      vcov_model_j <- vcovHC(model_j, type = 'HC')
      
      model_j_est <- summary(model_j)$coefficients[,1]
      model_j_se <- sqrt(diag(vcov_model_j))
      
      model_j_summ <- data.frame(Est = model_j_est,
                                 SE = model_j_se)
      
      target_ds_summ_list[[j]] <- model_j_summ
      
      ##### Obtain observed and liability R2 from training data #####
      target_ds_pred_response <- predict(model_j, type = "response")
      target_ds_pred_link <- predict(model_j, type = "link")
      
      ## observed R2
      R_obs_response <- cor(Y, target_ds_pred_response)^2
      
      ## liability R2 accounting for case-control sampling
      R_obs_link <- cor(Y, target_ds_pred_link)^2
      R2_liab <-  LiabR2_cc(K = target_ds_incid_sf, P = mean_Y, 
                            R2_obs = R_obs_link)
      
      train_R2_dt[j,] <- c(R_obs_response, R2_liab)
      
    }
    
    ## Save summary stats from regression for each outcome
    saveRDS(target_ds_summ_list, paste0(temp_data_dir, 'Outcome',
                                        target_ds, '_PheRS_Sex_Specific_WeightedLogisticRegression_SummStats_ControlCase', 
                                        control_case_ratio,'to1_WhiteBritish_OddYOB_Index2015_Followup10yr.rds'))
    saveRDS(train_R2_dt, 
            paste0(temp_data_dir, 'Outcome',
                   target_ds, '_PheRS_Sex_Specific_TrainData_ObservedAndLiabilityR2_OddYOB_Index2015_Followup10yr.rds'))
    
    #setTxtProgressBar(pb, i)
    print(paste0('Completed Outcome ', i, ' (sex-specific)'))  
    
  }
  
  #### General regression ####
  if ( target_ds_sex %in% c("Both", "Empty") & 
       file.exists(target_file_name_general) == FALSE ) {
  
    ##### Read training dataset #####
    train_df_fit <- readRDS(paste0(temp_data_dir_step11_1, 'Outcome',
                                   target_ds, '_PheRS_TrainData_Fit_ControlCase', control_case_ratio,
                                   'to1_WhiteBritish_OddYOB_Index2015_Followup10yr.rds'))
    
    ##### Get predicted PheRS risks #####
    train_df_fit_update <- PheRS_calc(train_df_fit,
                                      phers_coef_summ,
                                      outcome_ds_names[i],
                                      target_ds_sex)
    
    ##### Get regression formula #####
    reg_formula_PheRS <- paste0('target_ds_status ~', paste(c("PheRS_risk", covar_names), 
                                                            collapse ='+')) %>% 
      as.formula()
    
    reg_formula_PheRS_destd <- paste0('target_ds_status ~', paste(c("PheRS_destd_risk", covar_names), 
                                                                  collapse ='+')) %>% 
      as.formula()
    
    reg_formula_list <- list(reg_formula_PheRS,
                             reg_formula_PheRS_destd)
    
    ##### Loop through each model #####
    ## General info
    Y <- train_df_fit_update$target_ds_status
    mean_Y <- mean(Y)
    
    ## Create a list of summary statistics
    target_ds_summ_list <- vector(mode = 'list', length = n_models)
    names(target_ds_summ_list) <- model_name_vec
    
    ## Create a dataframe of R2
    train_R2_dt <- data.frame("R2_obs" = rep(NA, n_models),
                              "R2_liab" = rep(NA, n_models))
    rownames(train_R2_dt) <- model_name_vec
    
    for (j in 1:n_models) {
      reg_formula <- reg_formula_list[[j]]
      model_name <- model_name_vec[j]
      
      ##### Weighted logistic regression - Control:case = 4:1 #####
      model_j <- glm(reg_formula,
                          weights = ipcw_wts,
                          data = train_df_fit_update,
                          family = binomial())
    
      vcov_model_j <- vcovHC(model_j, type = 'HC')
      
      model_j_est <- summary(model_j)$coefficients[,1]
      model_j_se <- sqrt(diag(vcov_model_j))
      
      model_j_summ <- data.frame(Est = model_j_est,
                                 SE = model_j_se)
      
      target_ds_summ_list[[j]] <- model_j_summ
      
      ##### Obtain observed and liability R2 from training data #####
      target_ds_pred_response <- predict(model_j, type = "response")
      target_ds_pred_link <- predict(model_j, type = "link")
      
      ## observed R2
      R_obs_response <- cor(Y, target_ds_pred_response)^2
      
      ## liability R2 accounting for case-control sampling
      R_obs_link <- cor(Y, target_ds_pred_link)^2
      R2_liab <-  LiabR2_cc(K = target_ds_incid, P = mean_Y, 
                                 R2_obs = R_obs_link)
      
      train_R2_dt[j,] <- c(R_obs_response, R2_liab)
      
    }
    
    ## Save summary stats from regression for each outcome
    
    saveRDS(target_ds_summ_list, paste0(temp_data_dir, 'Outcome',
                                        outcome_ds_vec_output[i], '_PheRS_WeightedLogisticRegression_SummStats_ControlCase', 
                                        control_case_ratio,'to1_WhiteBritish_OddYOB_Index2015_Followup10yr.rds'))
    saveRDS(train_R2_dt, 
            paste0(temp_data_dir, 'Outcome',
                   outcome_ds_vec_output[i], '_PheRS_TrainData_ObservedAndLiabilityR2_OddYOB_Index2015_Followup10yr.rds'))
    
    
    #setTxtProgressBar(pb, i)
    print(paste0('Completed Outcome ', i, ' (both sexes)'))  
    print(warnings())
  }
  
  }
  
}

#t_end <- Sys.time()
#t_end - t_start

#stopCluster(cl)

#names(train_R2_list) <- outcome_ds_vec[c(13, 20:24)]
#saveRDS(train_R2_list, 
#        paste0(temp_data_dir, 'TrainData_AllOutcomes_ObservedAndLiabilityR2_OddYOB_Index2015_Followup10yr.rds'))

print(warnings())

