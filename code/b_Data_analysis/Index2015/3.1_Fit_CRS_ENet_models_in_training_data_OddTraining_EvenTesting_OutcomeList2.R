### Author: Pearl Liu
### For each outcome, Fit CRS elastic net models in training data
### Odd DOB: training, Even DOB: testing
### Index: 2015-01-01, follow-up: 10 years
### Outcome List 2


rm(list = ls())
gc()

#### Load libraries ####
library(data.table)
library(tidyverse)
library(glmnet)
library(glmnetUtils)
library(sandwich)
library(doParallel)

source('~/collab/CCU022/CCU022_CRS/0.1_Functions.R')

#### Set parameters ####
temp_data_dir_step1_0 <- '~/collab/CCU022/CCU022_CRS/Data/Step1.0/'
temp_data_dir_step1_1 <- '~/collab/CCU022/CCU022_CRS/Data/Step1.1/'
temp_data_dir_step2_4 <- '~/collab/CCU022/CCU022_CRS/Data/Step2.4/'
temp_data_dir <- '~/collab/CCU022/CCU022_CRS/Data/Step3.1/'
file_date <- '2025_05_01'

## Control-case ratio in logistic regression of training data
control_case_ratio <- 4
## Number of covariates 
n_covar <- 2
## Model names
model_name_vec <- c("CRS_ENet")
                #    "CRS_top100",
                #    "CRS_top50", 
                #    "CRS_top20", 
                #    "CRS_top10",
                #    "Age_Sex")
n_models <- length(model_name_vec)
## Number of cores used in parallel computing
n_cores <- 6

##### glmnet setting #####
## vector of alpha values (L1/L2 ratio in Elastic Net)
alpha_vec <- seq(0, 1, 0.2)
n_alpha <- length(alpha_vec)
## number of cross validation folds
nfolds <- 10
## lambda selection criterion
lambda_select <- "lambda.1se"

#### Read processed data ####
##### List of predictors and outcomes #####
pred_ds_list_name = paste0("ccu022_01_Predictor_Diseases_Pheparent_list_by_PL_", file_date)
outcome_ds_list_name = paste0("ccu022_01_Outcome_Diseases_Phecode_list2_TopCommonPlusPheRS_by_PL_", file_date)

pred_ds_list = readRDS(paste0(temp_data_dir_step1_0, pred_ds_list_name))
outcome_ds_list = readRDS(paste0(temp_data_dir_step1_0, outcome_ds_list_name))

pred_ds_vec <- pred_ds_list %>% arrange(index) %>% pull(predictor_ds) 
outcome_ds_vec <- c(outcome_ds_list %>% arrange(index) %>% pull(target_ds),
                    "death")

n_pred_ds <- length(pred_ds_vec)
n_outcome_ds <- length(outcome_ds_vec)

##### Predictor prevalence #####
pred_ds_prev <- readRDS(paste0(temp_data_dir_step1_1, 
                               'WhiteBritish_OddYOB_Index2015_PredictorDiseasePrevalence.rds'))

##### Outcome incidence #####
outcome_incid_vec <- readRDS(paste0(temp_data_dir_step2_4, 
                                    'TrainData_Outcome_Incidence_OddYOB_Index2015_Followup10yr_OutcomeList2.rds'))

#outcome_incid_vec_sf <- readRDS(paste0(temp_data_dir_step2_4, 
#                                    'TrainData_Outcome_Incidence_Sex_Specific_OddYOB_Index2015_Followup10yr.rds'))

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

#### Find top common predictor diseases and record predictor names ####
pred_ds_prev_sort <- sort(pred_ds_prev, decreasing = T)
top100_pred_ds <- names(pred_ds_prev_sort)[1:100] 
top50_pred_ds <- names(pred_ds_prev_sort)[1:50]
top20_pred_ds <- names(pred_ds_prev_sort)[1:20]
top10_pred_ds <- names(pred_ds_prev_sort)[1:10] 

all_pred_ds_names <- paste0('ds', pred_ds_vec)
top100_pred_ds_names <- paste0('ds', top100_pred_ds)
top50_pred_ds_names <- paste0('ds', top50_pred_ds)
top20_pred_ds_names <- paste0('ds', top20_pred_ds)
top10_pred_ds_names <- paste0('ds', top10_pred_ds)

covar_names <- c('age_at_2015', 'Female')
covar_names_sex_specific <- c('age_at_2015')

##### Get sex-specific predictors #####
## Female (remove male-only predictors)
pred_phecode_info_f <- pred_phecode_info %>%
  filter(Sex_parent != "Male")

pred_ds_vec_f <- pred_phecode_info_f %>%
  filter(PheCode_parent %in% pred_ds_vec) %>%
  pull(PheCode_parent)
top100_pred_ds_f <- pred_phecode_info_f %>%
  filter(PheCode_parent %in% top100_pred_ds) %>%
  pull(PheCode_parent)
top50_pred_ds_f <- pred_phecode_info_f %>%
  filter(PheCode_parent %in% top50_pred_ds) %>%
  pull(PheCode_parent)
top20_pred_ds_f <- pred_phecode_info_f %>%
  filter(PheCode_parent %in% top20_pred_ds) %>%
  pull(PheCode_parent)
top10_pred_ds_f <- pred_phecode_info_f %>%
  filter(PheCode_parent %in% top10_pred_ds) %>%
  pull(PheCode_parent)

all_pred_ds_names_f <- paste0('ds', pred_ds_vec_f)
top100_pred_ds_names_f <- paste0('ds', top100_pred_ds_f)
top50_pred_ds_names_f <- paste0('ds', top50_pred_ds_f)
top20_pred_ds_names_f <- paste0('ds', top20_pred_ds_f)
top10_pred_ds_names_f <- paste0('ds', top10_pred_ds_f)

## Male (remove female-only predictors)
pred_phecode_info_m <- pred_phecode_info %>%
  filter(Sex_parent != "Female")

pred_ds_vec_m <- pred_phecode_info_m %>%
  filter(PheCode_parent %in% pred_ds_vec) %>%
  pull(PheCode_parent)
top100_pred_ds_m <- pred_phecode_info_m %>%
  filter(PheCode_parent %in% top100_pred_ds) %>%
  pull(PheCode_parent)
top50_pred_ds_m <- pred_phecode_info_m %>%
  filter(PheCode_parent %in% top50_pred_ds) %>%
  pull(PheCode_parent)
top20_pred_ds_m <- pred_phecode_info_m %>%
  filter(PheCode_parent %in% top20_pred_ds) %>%
  pull(PheCode_parent)
top10_pred_ds_m <- pred_phecode_info_m %>%
  filter(PheCode_parent %in% top10_pred_ds) %>%
  pull(PheCode_parent)

all_pred_ds_names_m <- paste0('ds', pred_ds_vec_m)
top100_pred_ds_names_m <- paste0('ds', top100_pred_ds_m)
top50_pred_ds_names_m <- paste0('ds', top50_pred_ds_m)
top20_pred_ds_names_m <- paste0('ds', top20_pred_ds_m)
top10_pred_ds_names_m <- paste0('ds', top10_pred_ds_m)

#### Loop through each outcome ####

## Set up clusters
#cl <- makeCluster(n_cores, outfile = "")
#registerDoParallel(cl)


#foreach (i = 1:n_outcome_ds, 
#         .packages = c("data.table", "tidyverse", "sandwich")) %dopar% {

for (i in c(3,9)) {
  target_ds <- outcome_ds_vec[i]
  target_ds_incid <- outcome_incid_vec[i]
  
  target_ds_sex <- phecode_info %>%
    filter(PheCode == str_replace(target_ds, '_', '\\.')) %>%
    pull(Sex)
  
  target_file_name_general <-  paste0(temp_data_dir, 'Outcome',
                         target_ds, '_ElasticNet_AllModels_ControlCase', 
                         control_case_ratio,'to1_WhiteBritish_OddYOB_Index2015_Followup10yr.rds')
  target_file_name_sex_specific <-  paste0(temp_data_dir, 'Outcome',
                                      target_ds, '_Sex_Specific_ElasticNet_AllModels_ControlCase', 
                                      control_case_ratio,'to1_WhiteBritish_OddYOB_Index2015_Followup10yr.rds')
  
  if ( (target_ds_sex %in% c("Both", "Empty") & 
        file.exists(target_file_name_general)) | 
       (target_ds_sex %in% c("Female", "Male") & 
        file.exists(target_file_name_general) & 
        file.exists(target_file_name_sex_specific)) ) {
    
    print(paste0('Completed Outcome ', i)) 
    
  } else {
  
  source('~/collab/CCU022/CCU022_CRS/0.1_Functions.R')
    
  #### Sex-specific regression ####
  if ( target_ds_sex %in% c("Male", "Female") & 
       file.exists(target_file_name_sex_specific) == FALSE ) {
    
    target_ds_incid_sf <- outcome_incid_vec_sf[i]
    
    ##### Read sex-specific training dataset #####
    train_df_fit <- readRDS(paste0(temp_data_dir_step2_4, 'Outcome',
                                   target_ds, '_TrainData_Fit_Sex_Specific_ControlCase', control_case_ratio,
                                   'to1_WhiteBritish_OddYOB_Index2015_Followup10yr.rds'))
    
    all_colnames <- colnames(train_df_fit)
    
    if (target_ds_sex == "Female") {
      pred_ds_all <- intersect(all_pred_ds_names_f, all_colnames)
      pred_ds_top100 <- intersect(top100_pred_ds_names_f, all_colnames)
      pred_ds_top50 <- intersect(top50_pred_ds_names_f, all_colnames)
      pred_ds_top20 <- intersect(top20_pred_ds_names_f, all_colnames)
      pred_ds_top10 <- intersect(top10_pred_ds_names_f, all_colnames)
    
    } else if (target_ds_sex == "Male") {
      pred_ds_all <- intersect(all_pred_ds_names_m, all_colnames)
      pred_ds_top100 <- intersect(top100_pred_ds_names_m, all_colnames)
      pred_ds_top50 <- intersect(top50_pred_ds_names_m, all_colnames)
      pred_ds_top20 <- intersect(top20_pred_ds_names_m, all_colnames)
      pred_ds_top10 <- intersect(top10_pred_ds_names_m, all_colnames)
    }
    
    ##### Get regression formula #####
    reg_formula_allds <- paste0('target_ds_status ~', paste(c(pred_ds_all, covar_names_sex_specific), 
                                                            collapse ='+')) %>% 
      as.formula()
    reg_formula_top100 <- paste0('target_ds_status ~', paste(c(pred_ds_top100, covar_names_sex_specific), 
                                                             collapse ='+')) %>% 
      as.formula()
    reg_formula_top50 <- paste0('target_ds_status ~', paste(c(pred_ds_top50, covar_names_sex_specific), 
                                                            collapse ='+')) %>% 
      as.formula()
    reg_formula_top20 <- paste0('target_ds_status ~', paste(c(pred_ds_top20, covar_names_sex_specific), 
                                                            collapse ='+')) %>% 
      as.formula()
    reg_formula_top10 <- paste0('target_ds_status ~', paste(c(pred_ds_top10, covar_names_sex_specific), 
                                                            collapse ='+')) %>% 
      as.formula()
    reg_formula_covar <- paste0('target_ds_status ~', paste(covar_names_sex_specific, 
                                                            collapse ='+')) %>% 
      as.formula()
    
    reg_formula_list <- list(reg_formula_allds,
                             reg_formula_top100,
                             reg_formula_top50,
                             reg_formula_top20,
                             reg_formula_top10,
                             reg_formula_covar)
    
    ##### Loop through each model #####
    ## General info
    Y <- train_df_fit$target_ds_status
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
                     data = train_df_fit,
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
                                        target_ds, '_Sex_Specific_WeightedLogisticRegression_SummStats_ControlCase', 
                                        control_case_ratio,'to1_WhiteBritish_OddYOB_Index2015_Followup10yr.rds'))
    saveRDS(train_R2_dt, 
            paste0(temp_data_dir, 'Outcome',
                   target_ds, '_Sex_Specific_TrainData_ObservedAndLiabilityR2_OddYOB_Index2015_Followup10yr.rds'))
    
    #setTxtProgressBar(pb, i)
    print(paste0('Completed Outcome ', i, ' (sex-specific)'))  
    
  }
  
  #### General regression ####
  if ( target_ds_sex %in% c("Both", "Empty") & 
       file.exists(target_file_name_general) == FALSE ) {
  
    ##### Read training dataset #####
    train_df_fit <- readRDS(paste0(temp_data_dir_step2_4, 'Outcome',
                                   target_ds, '_TrainData_Fit_ControlCase', control_case_ratio,
                                   'to1_WhiteBritish_OddYOB_Index2015_Followup10yr.rds'))
    
    all_colnames <- colnames(train_df_fit)
    pred_ds_all <- intersect(all_pred_ds_names, all_colnames)
  #  pred_ds_top100 <- intersect(top100_pred_ds_names, all_colnames)
  #  pred_ds_top50 <- intersect(top50_pred_ds_names, all_colnames)
  #  pred_ds_top20 <- intersect(top20_pred_ds_names, all_colnames)
  #  pred_ds_top10 <- intersect(top10_pred_ds_names, all_colnames)
    
    ##### Get regression data matrix #####
    reg_pred_colnames <- c(pred_ds_all, covar_names)
    pred_dt_allds <- train_df_fit[, ..reg_pred_colnames] %>% as.matrix()
    
    ##### Get outcome #####
    Y <- train_df_fit$target_ds_status
    mean_Y <- mean(Y)
    
    ## Create a list of summary statistics
    target_ds_summ_list <- vector(mode = 'list', length = n_models)
    names(target_ds_summ_list) <- model_name_vec
    
    ## Create a dataframe of R2
    train_R2_dt <- data.frame("R2_obs" = rep(NA, n_models),
                        "R2_liab" = rep(NA, n_models))
    rownames(train_R2_dt) <- model_name_vec
    
    ##### Weighted elastic net logistic regression - Control:case = 4:1 #####
    ## fold id in cross validation
    set.seed(2025)
    foldid <- sample(1:nfolds, size = length(Y), replace = TRUE)
    
    start.time <- Sys.time()
    model_enet <- cva.glmnet(x = pred_dt_allds,
                           y = Y,
                            weights = train_df_fit$ipcw_wts,
                            family = "binomial",
                            alpha = alpha_vec,
                            nfolds = nfolds,
                            foldid = foldid,
                            type.measure = "auc",
                            keep = TRUE)
    end.time <- Sys.time()
    print(end.time - start.time)
    
    ###### Save all ENet models ######
    saveRDS(model_enet, target_file_name_general)
    
    ###### Choose optimal alpha based on min CV loss ######
    min_loss_by_alpha <- minlossplot_ReturnValue(model_enet, 
                                                 cv.type = "1se")
    
    optim_alpha_idx <- which.min(min_loss_by_alpha)
    alpha_final <- alpha_vec[optim_alpha_idx]
    model_enet_final <- model_enet$modlist[[optim_alpha_idx]]

    model_enet_coef <- coef(model_enet_final, 
                         s = lambda_select) %>% 
                      as.matrix() %>% 
                      as.data.frame()
    colnames(model_enet_coef) <- "Est"
    target_ds_summ_list[[1]] <- model_enet_coef
    
    ##### Obtain observed and liability R2 from training data #####
    target_ds_pred_response <- predict(model_enet_final, 
                                       newx = pred_dt_allds,
                                       type = "response",
                                       s = lambda_select)
    target_ds_pred_link <- predict(model_enet_final, 
                                   newx = pred_dt_allds,
                                   type = "link",
                                   s = lambda_select)
    
    ## observed R2
    R_obs_response <- cor(Y, target_ds_pred_response)^2
    
    ## liability R2 accounting for case-control sampling
    R_obs_link <- cor(Y, target_ds_pred_link)^2
    R2_liab <-  LiabR2_cc(K = target_ds_incid, P = mean_Y, 
                               R2_obs = R_obs_link)
    
    train_R2_dt[1,] <- c(R_obs_response, R2_liab)
      
    
    ##### Save summary stats from regression for each outcome #####
    saveRDS(target_ds_summ_list, paste0(temp_data_dir, 'Outcome',
                                        target_ds, '_ElasticNet_FinalSummStats_ControlCase', 
                                        control_case_ratio,'to1_WhiteBritish_OddYOB_Index2015_Followup10yr.rds'))
    saveRDS(list(train_R2_dt = train_R2_dt,
                 alpha_final = alpha_final), 
            paste0(temp_data_dir, 'Outcome',
                   target_ds, '_ElasticNet_FinalAlpha_TrainData_ObservedAndLiabilityR2_OddYOB_Index2015_Followup10yr.rds'))
    
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
