### Author: Pearl Liu
### Initial processing of CVD-COVID-UK data

rm(list = ls())
gc()

#### Load libraries ####
library(tidyverse)
library(data.table)

#### Set parameters ####
temp_data_dir_step1_0 <- '~/collab/CCU022/CCU022_CRS/Data/Step1.0/'
temp_data_dir <- '~/collab/CCU022/CCU022_CRS/Data/Step1.1/'
file_date <- '2025_05_01'

#### Read saved data from Databricks - Index: 2015-01-01, Follow-up: 10 years ####
##### Predictor data #####
pred_df_age40_even_name = paste0("ccu022_01_WhiteBritish_Age40_49_EvenYOB_Index2015_PredictorData_by_PL_", file_date)
pred_df_age40_odd_name = paste0('ccu022_01_WhiteBritish_Age40_49_OddYOB_Index2015_PredictorData_by_PL_', file_date)
pred_df_age50_even_name = paste0("ccu022_01_WhiteBritish_Age50_59_EvenYOB_Index2015_PredictorData_by_PL_", file_date)
pred_df_age50_odd_name = paste0('ccu022_01_WhiteBritish_Age50_59_OddYOB_Index2015_PredictorData_by_PL_', file_date)
pred_df_age60_even_name = paste0("ccu022_01_WhiteBritish_Age60_69_EvenYOB_Index2015_PredictorData_by_PL_", file_date)
pred_df_age60_odd_name = paste0('ccu022_01_WhiteBritish_Age60_69_OddYOB_Index2015_PredictorData_by_PL_', file_date)

pred_df_age40_even = readRDS(paste0(temp_data_dir_step1_0, pred_df_age40_even_name))
pred_df_age40_odd = readRDS(paste0(temp_data_dir_step1_0, pred_df_age40_odd_name))
pred_df_age50_even = readRDS(paste0(temp_data_dir_step1_0, pred_df_age50_even_name))
pred_df_age50_odd = readRDS(paste0(temp_data_dir_step1_0, pred_df_age50_odd_name))
pred_df_age60_even = readRDS(paste0(temp_data_dir_step1_0, pred_df_age60_even_name))
pred_df_age60_odd = readRDS(paste0(temp_data_dir_step1_0, pred_df_age60_odd_name))

##### CCI and IMD data #####
cci_imd_df_name = paste0("ccu022_01_WhiteBritish_Age40_69_Index2015_CCI_IMD_final_by_PL_", file_date)
cci_imd_df <- readRDS(paste0(temp_data_dir_step1_0, cci_imd_df_name))
cci_imd_df <- cci_imd_df %>%
  as.data.table() %>%
  .[order(PID)]
  

##### Outcome data: Outcome List 1 #####
outcome_df_age40_even_name = paste0("ccu022_01_WhiteBritish_Age40_49_EvenYOB_Index2015_AllOutcomeData_by_PL_", file_date)
outcome_df_age40_odd_name = paste0('ccu022_01_WhiteBritish_Age40_49_OddYOB_Index2015_AllOutcomeData_by_PL_', file_date)
outcome_df_age50_even_name = paste0("ccu022_01_WhiteBritish_Age50_59_EvenYOB_Index2015_AllOutcomeData_by_PL_", file_date)
outcome_df_age50_odd_name = paste0('ccu022_01_WhiteBritish_Age50_59_OddYOB_Index2015_AllOutcomeData_by_PL_', file_date)
outcome_df_age60_even_name = paste0("ccu022_01_WhiteBritish_Age60_69_EvenYOB_Index2015_AllOutcomeData_by_PL_", file_date)
outcome_df_age60_odd_name = paste0('ccu022_01_WhiteBritish_Age60_69_OddYOB_Index2015_AllOutcomeData_by_PL_', file_date)

outcome_df_age40_even = readRDS(paste0(temp_data_dir_step1_0, outcome_df_age40_even_name))
outcome_df_age40_odd = readRDS(paste0(temp_data_dir_step1_0, outcome_df_age40_odd_name))
outcome_df_age50_even = readRDS(paste0(temp_data_dir_step1_0, outcome_df_age50_even_name))
outcome_df_age50_odd = readRDS(paste0(temp_data_dir_step1_0, outcome_df_age50_odd_name))
outcome_df_age60_even = readRDS(paste0(temp_data_dir_step1_0, outcome_df_age60_even_name))
outcome_df_age60_odd = readRDS(paste0(temp_data_dir_step1_0, outcome_df_age60_odd_name))

##### Outcome data: Outcome List 2 #####
outcome_df2_even_name = paste0("ccu022_01_WhiteBritish_Age40_69_EvenYOB_Index2015_AllOutcomeData_OutcomeList2_by_PL_", file_date)
outcome_df2_odd_name = paste0('ccu022_01_WhiteBritish_Age40_69_OddYOB_Index2015_AllOutcomeData_OutcomeList2_by_PL_', file_date)

outcome_df2_even = readRDS(paste0(temp_data_dir_step1_0, outcome_df2_even_name))
outcome_df2_odd = readRDS(paste0(temp_data_dir_step1_0, outcome_df2_odd_name))


##### List of predictors and outcomes (List 1 and List 2) #####
pred_ds_list_name = paste0("ccu022_01_Predictor_Diseases_Pheparent_list_by_PL_", file_date)
outcome_ds_list_name = paste0("ccu022_01_Outcome_Diseases_Phecode_list_by_PL_", file_date)
outcome_ds_list2_name = paste0("ccu022_01_Outcome_Diseases_Phecode_list2_TopCommonPlusPheRS_by_PL_", file_date)

pred_ds_list = readRDS(paste0(temp_data_dir_step1_0, pred_ds_list_name))
outcome_ds_list = readRDS(paste0(temp_data_dir_step1_0, outcome_ds_list_name))
outcome_ds_list2 = readRDS(paste0(temp_data_dir_step1_0, outcome_ds_list2_name))

pred_ds_vec <- pred_ds_list %>% arrange(index) %>% pull(predictor_ds) 
outcome_ds_vec <- outcome_ds_list %>% arrange(index) %>% pull(target_ds)
outcome_ds_vec2 <- c(outcome_ds_list2 %>% arrange(index) %>% pull(target_ds),
                     "death")

n_pred_ds <- length(pred_ds_vec)
n_outcome_ds <- length(outcome_ds_vec)
n_outcome_ds2 <- length(outcome_ds_vec2)

#### Processing of data - Index: 2015-01-01, Follow-up: 10 years ####

##### Combine data from all age groups #####
pred_df_even <- rbindlist(list(pred_df_age40_even,
                             pred_df_age50_even,
                             pred_df_age60_even)) %>%
  .[order(PID)]
outcome_df_even <- rbindlist(list(outcome_df_age40_even,
                              outcome_df_age50_even,
                              outcome_df_age60_even)) %>%
  .[order(PID)]
pred_df_odd <- rbindlist(list(pred_df_age40_odd,
                          pred_df_age50_odd,
                          pred_df_age60_odd)) %>%
  .[order(PID)]
outcome_df_odd <- rbindlist(list(outcome_df_age40_odd,
                              outcome_df_age50_odd,
                              outcome_df_age60_odd)) %>%
  .[order(PID)]

######  PL 20251202: Add outcome List 2 ###### 
outcome_df2_even <- as.data.table(outcome_df2_even) %>% 
  .[order(PID)]
outcome_df2_odd <- as.data.table(outcome_df2_odd) %>%
  .[order(PID)]


## sanity check
all(pred_df_even$PID == outcome_df_even$PID)
all(pred_df_odd$PID == outcome_df_odd$PID)

all(outcome_df_even$PID == outcome_df2_even$PID)
all(outcome_df_odd$PID == outcome_df2_odd$PID)

##### Get matrix of pre-index predictor status #####
pred_df_pre_index_colnames <- c('PID', pred_ds_vec)

pred_df_pre_index_even <- pred_df_even[, ..pred_df_pre_index_colnames] 
pred_df_pre_index_odd <- pred_df_odd[, ..pred_df_pre_index_colnames]

pred_df_pre_index_even_name <- paste0('ccu022_01_WhiteBritish_EvenYOB_Index2015_PredictorDiseaseStatus_by_PL_', file_date)
pred_df_pre_index_odd_name <- paste0('ccu022_01_WhiteBritish_OddYOB_Index2015_PredictorDiseaseStatus_by_PL_', file_date)

saveRDS(pred_df_pre_index_even, paste0(temp_data_dir, pred_df_pre_index_even_name))
saveRDS(pred_df_pre_index_odd, paste0(temp_data_dir, pred_df_pre_index_odd_name))

##### Get matrix of covariates #####
covar_colnames <- c('PID', 'age_at_2015', 'Female', 'date_of_birth')

covar_df_even <- pred_df_even[, ..covar_colnames] 
covar_df_odd <- pred_df_odd[, ..covar_colnames]

covar_df_even_name <- paste0('ccu022_01_WhiteBritish_EvenYOB_Index2015_Covariates_by_PL_', file_date)
covar_df_odd_name <- paste0('ccu022_01_WhiteBritish_OddYOB_Index2015_Covariates_by_PL_', file_date)

saveRDS(covar_df_even, paste0(temp_data_dir, covar_df_even_name))
saveRDS(covar_df_odd, paste0(temp_data_dir, covar_df_odd_name))


##### Get matrix of covariates - Version 2: include CCI and IMD #####
even_yr_idx <- match(covar_df_even$PID, cci_imd_df$PID)
odd_yr_idx <- match(covar_df_odd$PID, cci_imd_df$PID)

cci_imd_df_even <- cci_imd_df[even_yr_idx,]
cci_imd_df_odd <- cci_imd_df[odd_yr_idx,]

## sanity check
all.equal(cci_imd_df_even$PID, covar_df_even$PID)
all.equal(cci_imd_df_odd$PID, covar_df_odd$PID)

covar_df_even2 <- covar_df_even[, colnames(cci_imd_df_even) := cci_imd_df_even]
covar_df_odd2 <- covar_df_odd[, colnames(cci_imd_df_odd) := cci_imd_df_odd]

covar_df_even_name2 <- paste0('ccu022_01_WhiteBritish_EvenYOB_Index2015_Covariates_CCI_IMD_by_PL_', file_date)
covar_df_odd_name2 <- paste0('ccu022_01_WhiteBritish_OddYOB_Index2015_Covariates_CCI_IMD_by_PL_', file_date)

saveRDS(covar_df_even2, paste0(temp_data_dir, covar_df_even_name2))
saveRDS(covar_df_odd2, paste0(temp_data_dir, covar_df_odd_name2))


##### Get matrix of pre-index outcome status (Outcome List 1) #####
outcome_df_pre_index_colnames <- c('PID',
                                   paste0(outcome_ds_vec, '_preidx'))

outcome_df_pre_index_even <- outcome_df_even[, ..outcome_df_pre_index_colnames] 
outcome_df_pre_index_odd <- outcome_df_odd[, ..outcome_df_pre_index_colnames]

colnames(outcome_df_pre_index_even) <- c('PID', outcome_ds_vec)
colnames(outcome_df_pre_index_odd) <- c('PID', outcome_ds_vec)

outcome_df_pre_index_even_name <- paste0('ccu022_01_WhiteBritish_EvenYOB_Index2015_OutcomeDiseaseStatusPreIndex_by_PL_', file_date)
outcome_df_pre_index_odd_name <- paste0('ccu022_01_WhiteBritish_OddYOB_Index2015_OutcomeDiseaseStatusPreIndex_by_PL_', file_date)

saveRDS(outcome_df_pre_index_even, paste0(temp_data_dir, outcome_df_pre_index_even_name))
saveRDS(outcome_df_pre_index_odd, paste0(temp_data_dir, outcome_df_pre_index_odd_name))

##### Get matrix of pre-index outcome status (Outcome List 2) #####
outcome_df_pre_index_colnames2 <- c('PID',
                                   paste0(outcome_ds_vec2, '_preidx'))

outcome_df_pre_index_even2 <- outcome_df2_even[, ..outcome_df_pre_index_colnames2] 
outcome_df_pre_index_odd2 <- outcome_df2_odd[, ..outcome_df_pre_index_colnames2]

colnames(outcome_df_pre_index_even2) <- c('PID', outcome_ds_vec2)
colnames(outcome_df_pre_index_odd2) <- c('PID', outcome_ds_vec2)

outcome_df_pre_index_even_name2 <- paste0('ccu022_01_WhiteBritish_EvenYOB_Index2015_OutcomeDiseaseStatusPreIndex_OutcomeList2_by_PL_', file_date)
outcome_df_pre_index_odd_name2 <- paste0('ccu022_01_WhiteBritish_OddYOB_Index2015_OutcomeDiseaseStatusPreIndex_OutcomeList2_by_PL_', file_date)

saveRDS(outcome_df_pre_index_even2, paste0(temp_data_dir, outcome_df_pre_index_even_name2))
saveRDS(outcome_df_pre_index_odd2, paste0(temp_data_dir, outcome_df_pre_index_odd_name2))



##### Get matrix of outcome status during follow-up (Outcome List 1) #####
outcome_df_fup_colnames <- c('PID',
                                   paste0(outcome_ds_vec, '_fup'))

outcome_df_fup_even <- outcome_df_even[, ..outcome_df_fup_colnames] 
outcome_df_fup_odd <- outcome_df_odd[, ..outcome_df_fup_colnames]

colnames(outcome_df_fup_even) <- c('PID', outcome_ds_vec)
colnames(outcome_df_fup_odd) <- c('PID', outcome_ds_vec)

outcome_df_fup_even_name <- paste0('ccu022_01_WhiteBritish_EvenYOB_Index2015_Followup10yr_OutcomeDiseaseStatusFollowup_by_PL_', file_date)
outcome_df_fup_odd_name <- paste0('ccu022_01_WhiteBritish_OddYOB_Index2015_Followup10yr_OutcomeDiseaseStatusFollowup_by_PL_', file_date)

saveRDS(outcome_df_fup_even, paste0(temp_data_dir, outcome_df_fup_even_name))
saveRDS(outcome_df_fup_odd, paste0(temp_data_dir, outcome_df_fup_odd_name))

##### Get matrix of outcome status during follow-up (Outcome List 2) #####
outcome_df_fup_colnames2 <- c('PID',
                             paste0(outcome_ds_vec2, '_fup'))

outcome_df_fup_even2 <- outcome_df2_even[, ..outcome_df_fup_colnames2] 
outcome_df_fup_odd2 <- outcome_df2_odd[, ..outcome_df_fup_colnames2]

colnames(outcome_df_fup_even2) <- c('PID', outcome_ds_vec2)
colnames(outcome_df_fup_odd2) <- c('PID', outcome_ds_vec2)

outcome_df_fup_even_name2 <- paste0('ccu022_01_WhiteBritish_EvenYOB_Index2015_Followup10yr_OutcomeDiseaseStatusFollowup_OutcomeList2_by_PL_', file_date)
outcome_df_fup_odd_name2 <- paste0('ccu022_01_WhiteBritish_OddYOB_Index2015_Followup10yr_OutcomeDiseaseStatusFollowup_OutcomeList2_by_PL_', file_date)

saveRDS(outcome_df_fup_even2, paste0(temp_data_dir, outcome_df_fup_even_name2))
saveRDS(outcome_df_fup_odd2, paste0(temp_data_dir, outcome_df_fup_odd_name2))

##### Get matrix of time to outcome during follow-up (Outcome List 1) #####
outcome_df_tte_colnames <- c('PID',
                             paste0(outcome_ds_vec, '_tte'))

outcome_df_tte_even <- outcome_df_even[, ..outcome_df_tte_colnames] 
outcome_df_tte_odd <- outcome_df_odd[, ..outcome_df_tte_colnames]

colnames(outcome_df_tte_even) <- c('PID', outcome_ds_vec)
colnames(outcome_df_tte_odd) <- c('PID', outcome_ds_vec)

outcome_df_tte_even_name <- paste0('ccu022_01_WhiteBritish_EvenYOB_Index2015_Followup10yr_TimeToOutcomeFollowup_by_PL_', file_date)
outcome_df_tte_odd_name <- paste0('ccu022_01_WhiteBritish_OddYOB_Index2015_Followup10yr_TimeToOutcomeFollowup_by_PL_', file_date)

saveRDS(outcome_df_tte_even, paste0(temp_data_dir, outcome_df_tte_even_name))
saveRDS(outcome_df_tte_odd, paste0(temp_data_dir, outcome_df_tte_odd_name))

##### Get matrix of time to outcome during follow-up (Outcome List 2) #####
outcome_df_tte_colnames2 <- c('PID',
                             paste0(outcome_ds_vec2, '_tte'))

outcome_df_tte_even2 <- outcome_df2_even[, ..outcome_df_tte_colnames2] 
outcome_df_tte_odd2 <- outcome_df2_odd[, ..outcome_df_tte_colnames2]

colnames(outcome_df_tte_even2) <- c('PID', outcome_ds_vec2)
colnames(outcome_df_tte_odd2) <- c('PID', outcome_ds_vec2)

outcome_df_tte_even_name2 <- paste0('ccu022_01_WhiteBritish_EvenYOB_Index2015_Followup10yr_TimeToOutcomeFollowup_OutcomeList2_by_PL_', file_date)
outcome_df_tte_odd_name2 <- paste0('ccu022_01_WhiteBritish_OddYOB_Index2015_Followup10yr_TimeToOutcomeFollowup_OutcomeList2_by_PL_', file_date)

saveRDS(outcome_df_tte_even2, paste0(temp_data_dir, outcome_df_tte_even_name2))
saveRDS(outcome_df_tte_odd2, paste0(temp_data_dir, outcome_df_tte_odd_name2))

##### Sanity check #####
all(sapply(list(covar_df_even$PID, 
                outcome_df_pre_index_even$PID, 
                outcome_df_fup_even$PID, 
                outcome_df_tte_even$PID), FUN = identical, 
           pred_df_pre_index_even$PID)) %>% print()


all(sapply(list(covar_df_odd$PID, 
                outcome_df_pre_index_odd$PID, 
                outcome_df_fup_odd$PID, 
                outcome_df_tte_odd$PID), FUN = identical, 
           pred_df_pre_index_odd$PID)) %>% print()

print(length(pred_df_pre_index_even$PID) + length(pred_df_pre_index_odd$PID))


#### Obtain prevalence for predictor diseases among pts with even YOB ####
pred_ds_prev <- apply(pred_df_pre_index_even[,-1], 2, function(x){mean(x == 1)})

saveRDS(pred_ds_prev, 
        paste0(temp_data_dir, 'WhiteBritish_EvenYOB_Index2015_PredictorDiseasePrevalence.rds'))

summary(pred_ds_prev) %>% print()
sort(pred_ds_prev, decreasing = F) %>% print()

#### Obtain prevalence for predictor diseases among pts with odd YOB ####
pred_ds_prev <- apply(pred_df_pre_index_odd[,-1], 2, function(x){mean(x == 1)})

saveRDS(pred_ds_prev, 
        paste0(temp_data_dir, 'WhiteBritish_OddYOB_Index2015_PredictorDiseasePrevalence.rds'))

summary(pred_ds_prev) %>% print()
sort(pred_ds_prev, decreasing = F) %>% print()

##### Number of cases for each predictor #####
n_cases_by_ds <- apply(pred_df_pre_index_odd[,-1], 2, function(x){sum(x == 1)})
summary(n_cases_by_ds) %>% print()
n_cases_by_ds[which.min(n_cases_by_ds)] %>% print()

sort(n_cases_by_ds, decreasing = F) %>% print()


#### Processing of PheRS predictor data - Index: 2015-01-01, Follow-up: 10 years ####
##### Read saved PheRS predictor data #####
phers_pred_df_age40_even_name = paste0("ccu022_01_WhiteBritish_Age40_49_EvenYOB_Index2015_PheRSPredictorData_by_PL_", file_date)
phers_pred_df_age40_odd_name = paste0('ccu022_01_WhiteBritish_Age40_49_OddYOB_Index2015_PheRSPredictorData_by_PL_', file_date)
phers_pred_df_age50_even_name = paste0("ccu022_01_WhiteBritish_Age50_59_EvenYOB_Index2015_PheRSPredictorData_by_PL_", file_date)
phers_pred_df_age50_odd_name = paste0('ccu022_01_WhiteBritish_Age50_59_OddYOB_Index2015_PheRSPredictorData_by_PL_', file_date)
phers_pred_df_age60_even_name = paste0("ccu022_01_WhiteBritish_Age60_69_EvenYOB_Index2015_PheRSPredictorData_by_PL_", file_date)
phers_pred_df_age60_odd_name = paste0('ccu022_01_WhiteBritish_Age60_69_OddYOB_Index2015_PheRSPredictorData_by_PL_', file_date)

phers_pred_df_age40_even = readRDS(paste0(temp_data_dir_step1_0, phers_pred_df_age40_even_name))
phers_pred_df_age40_odd = readRDS(paste0(temp_data_dir_step1_0, phers_pred_df_age40_odd_name))
phers_pred_df_age50_even = readRDS(paste0(temp_data_dir_step1_0, phers_pred_df_age50_even_name))
phers_pred_df_age50_odd = readRDS(paste0(temp_data_dir_step1_0, phers_pred_df_age50_odd_name))
phers_pred_df_age60_even = readRDS(paste0(temp_data_dir_step1_0, phers_pred_df_age60_even_name))
phers_pred_df_age60_odd = readRDS(paste0(temp_data_dir_step1_0, phers_pred_df_age60_odd_name))


##### Read list of PheRS predictors #####
phers_pred_ds_list_name = paste0("ccu022_01_PheRS_Predictor_Diseases_Pheparent_list_by_PL_", file_date)

phers_pred_ds_list = readRDS(paste0(temp_data_dir_step1_0, phers_pred_ds_list_name))

phers_pred_ds_vec <- phers_pred_ds_list %>% arrange(index) %>% pull(predictor_ds) 

n_phers_pred_ds <- length(phers_pred_ds_vec)

##### Combine data from all age groups #####
phers_pred_df_even <- rbindlist(list(phers_pred_df_age40_even,
                                     phers_pred_df_age50_even,
                                     phers_pred_df_age60_even)) %>%
  .[order(PID)]
phers_pred_df_odd <- rbindlist(list(phers_pred_df_age40_odd,
                                    phers_pred_df_age50_odd,
                                    phers_pred_df_age60_odd)) %>%
  .[order(PID)]


##### Get matrix of pre-index PheRS predictor status #####
phers_pred_df_pre_index_colnames <- c('PID', phers_pred_ds_vec)

phers_pred_df_pre_index_even <- phers_pred_df_even[, ..phers_pred_df_pre_index_colnames] 
phers_pred_df_pre_index_odd <- phers_pred_df_odd[, ..phers_pred_df_pre_index_colnames]

phers_pred_df_pre_index_even_name <- paste0('ccu022_01_WhiteBritish_EvenYOB_Index2015_PheRSPredictorDiseaseStatus_by_PL_', file_date)
phers_pred_df_pre_index_odd_name <- paste0('ccu022_01_WhiteBritish_OddYOB_Index2015_PheRSPredictorDiseaseStatus_by_PL_', file_date)

saveRDS(phers_pred_df_pre_index_even, paste0(temp_data_dir, phers_pred_df_pre_index_even_name))
saveRDS(phers_pred_df_pre_index_odd, paste0(temp_data_dir, phers_pred_df_pre_index_odd_name))

##### Obtain prevalence for PheRS predictor diseases among pts with even YOB #####
phers_pred_ds_prev_even <- apply(phers_pred_df_pre_index_even[,-1], 2, 
                            function(x){mean(x == 1)})

saveRDS(phers_pred_ds_prev_even, 
        paste0(temp_data_dir, 'WhiteBritish_EvenYOB_Index2015_PheRSPredictorDiseasePrevalence.rds'))

#summary(phers_pred_ds_prev_even) %>% print()
#sort(phers_pred_ds_prev_even, decreasing = F) %>% print()

##### Obtain prevalence for PheRS predictor diseases among pts with odd YOB #####
phers_pred_ds_prev_odd <- apply(phers_pred_df_pre_index_odd[,-1], 2, 
                            function(x){mean(x == 1)})

saveRDS(phers_pred_ds_prev_odd, 
        paste0(temp_data_dir, 'WhiteBritish_OddYOB_Index2015_PheRSPredictorDiseasePrevalence.rds'))

#summary(phers_pred_ds_prev_odd) %>% print()
#sort(phers_pred_ds_prev_odd, decreasing = F) %>% print()


#### Processing of Delphi comparison data - Index: 2021-07-01, Follow-up: 1 year ####

##### Read saved data #####
###### Predictor data ######
pred_df_age50_even_name = paste0("ccu022_01_WhiteBritish_Age50_59_EvenYOB_IndexMid2020_PredictorData_by_PL_", file_date)
pred_df_age50_odd_name = paste0('ccu022_01_WhiteBritish_Age50_59_OddYOB_IndexMid2020_PredictorData_by_PL_', file_date)
pred_df_age60_even_name = paste0("ccu022_01_WhiteBritish_Age60_69_EvenYOB_IndexMid2020_PredictorData_by_PL_", file_date)
pred_df_age60_odd_name = paste0('ccu022_01_WhiteBritish_Age60_69_OddYOB_IndexMid2020_PredictorData_by_PL_', file_date)
pred_df_age70_even_name = paste0("ccu022_01_WhiteBritish_Age70_80_EvenYOB_IndexMid2020_PredictorData_by_PL_", file_date)
pred_df_age70_odd_name = paste0('ccu022_01_WhiteBritish_Age70_80_OddYOB_IndexMid2020_PredictorData_by_PL_', file_date)


pred_df_age50_even = readRDS(paste0(temp_data_dir_step1_0, pred_df_age50_even_name))
pred_df_age50_odd = readRDS(paste0(temp_data_dir_step1_0, pred_df_age50_odd_name))
pred_df_age60_even = readRDS(paste0(temp_data_dir_step1_0, pred_df_age60_even_name))
pred_df_age60_odd = readRDS(paste0(temp_data_dir_step1_0, pred_df_age60_odd_name))
pred_df_age70_even = readRDS(paste0(temp_data_dir_step1_0, pred_df_age70_even_name))
pred_df_age70_odd = readRDS(paste0(temp_data_dir_step1_0, pred_df_age70_odd_name))


###### Outcome data: Outcome List 1 ######
outcome_df_age50_even_name = paste0("ccu022_01_WhiteBritish_Age50_59_EvenYOB_IndexMid2020_AllOutcomeData_by_PL_", file_date)
outcome_df_age50_odd_name = paste0('ccu022_01_WhiteBritish_Age50_59_OddYOB_IndexMid2020_AllOutcomeData_by_PL_', file_date)
outcome_df_age60_even_name = paste0("ccu022_01_WhiteBritish_Age60_69_EvenYOB_IndexMid2020_AllOutcomeData_by_PL_", file_date)
outcome_df_age60_odd_name = paste0('ccu022_01_WhiteBritish_Age60_69_OddYOB_IndexMid2020_AllOutcomeData_by_PL_', file_date)
outcome_df_age70_even_name = paste0("ccu022_01_WhiteBritish_Age70_80_EvenYOB_IndexMid2020_AllOutcomeData_by_PL_", file_date)
outcome_df_age70_odd_name = paste0('ccu022_01_WhiteBritish_Age70_80_OddYOB_IndexMid2020_AllOutcomeData_by_PL_', file_date)

outcome_df_age50_even = readRDS(paste0(temp_data_dir_step1_0, outcome_df_age50_even_name))
outcome_df_age50_odd = readRDS(paste0(temp_data_dir_step1_0, outcome_df_age50_odd_name))
outcome_df_age60_even = readRDS(paste0(temp_data_dir_step1_0, outcome_df_age60_even_name))
outcome_df_age60_odd = readRDS(paste0(temp_data_dir_step1_0, outcome_df_age60_odd_name))
outcome_df_age70_even = readRDS(paste0(temp_data_dir_step1_0, outcome_df_age70_even_name))
outcome_df_age70_odd = readRDS(paste0(temp_data_dir_step1_0, outcome_df_age70_odd_name))


###### Outcome data: Outcome List 2 ######
outcome_df2_even_name = paste0("ccu022_01_WhiteBritish_Age50_80_EvenYOB_IndexMid2020_AllOutcomeData_OutcomeList2_by_PL_", file_date)
outcome_df2_odd_name = paste0('ccu022_01_WhiteBritish_Age50_80_OddYOB_IndexMid2020_AllOutcomeData_OutcomeList2_by_PL_', file_date)

outcome_df2_even = readRDS(paste0(temp_data_dir_step1_0, outcome_df2_even_name))
outcome_df2_odd = readRDS(paste0(temp_data_dir_step1_0, outcome_df2_odd_name))


##### Combine data from all age groups #####
pred_df_even <- rbindlist(list(pred_df_age50_even,
                               pred_df_age60_even,
                               pred_df_age70_even)) %>%
  .[order(PID)]
outcome_df_even <- rbindlist(list(outcome_df_age50_even,
                                  outcome_df_age60_even,
                                  outcome_df_age70_even)) %>%
  .[order(PID)]
pred_df_odd <- rbindlist(list(pred_df_age50_odd,
                              pred_df_age60_odd,
                              pred_df_age70_odd)) %>%
  .[order(PID)]
outcome_df_odd <- rbindlist(list(outcome_df_age50_odd,
                                 outcome_df_age60_odd,
                                 outcome_df_age70_odd)) %>%
  .[order(PID)]

## PL 20251202: Add outcome List 2
outcome_df2_even <- as.data.table(outcome_df2_even) %>% 
  .[order(PID)]
outcome_df2_odd <- as.data.table(outcome_df2_odd) %>%
  .[order(PID)]


## sanity check
all(pred_df_even$PID == outcome_df_even$PID)
all(pred_df_odd$PID == outcome_df_odd$PID)

all(outcome_df_even$PID == outcome_df2_even$PID)
all(outcome_df_odd$PID == outcome_df2_odd$PID)

##### Get matrix of pre-index predictor status #####
pred_df_pre_index_colnames <- c('PID', pred_ds_vec)

pred_df_pre_index_even <- pred_df_even[, ..pred_df_pre_index_colnames] 
pred_df_pre_index_odd <- pred_df_odd[, ..pred_df_pre_index_colnames]

pred_df_pre_index_even_name <- paste0('ccu022_01_WhiteBritish_EvenYOB_IndexMid2020_PredictorDiseaseStatus_by_PL_', file_date)
pred_df_pre_index_odd_name <- paste0('ccu022_01_WhiteBritish_OddYOB_IndexMid2020_PredictorDiseaseStatus_by_PL_', file_date)

saveRDS(pred_df_pre_index_even, paste0(temp_data_dir, pred_df_pre_index_even_name))
saveRDS(pred_df_pre_index_odd, paste0(temp_data_dir, pred_df_pre_index_odd_name))

##### Get matrix of covariates #####
covar_colnames <- c('PID', 'age_at_mid2020', 'Female', 'date_of_birth')

covar_df_even <- pred_df_even[, ..covar_colnames] 
covar_df_odd <- pred_df_odd[, ..covar_colnames]

covar_df_even_name <- paste0('ccu022_01_WhiteBritish_EvenYOB_IndexMid2020_Covariates_by_PL_', file_date)
covar_df_odd_name <- paste0('ccu022_01_WhiteBritish_OddYOB_IndexMid2020_Covariates_by_PL_', file_date)

saveRDS(covar_df_even, paste0(temp_data_dir, covar_df_even_name))
saveRDS(covar_df_odd, paste0(temp_data_dir, covar_df_odd_name))


##### Get matrix of pre-index outcome status (Outcome List 1) #####
outcome_df_pre_index_colnames <- c('PID',
                                   paste0(outcome_ds_vec, '_preidx'))

outcome_df_pre_index_even <- outcome_df_even[, ..outcome_df_pre_index_colnames] 
outcome_df_pre_index_odd <- outcome_df_odd[, ..outcome_df_pre_index_colnames]

colnames(outcome_df_pre_index_even) <- c('PID', outcome_ds_vec)
colnames(outcome_df_pre_index_odd) <- c('PID', outcome_ds_vec)

outcome_df_pre_index_even_name <- paste0('ccu022_01_WhiteBritish_EvenYOB_IndexMid2020_OutcomeDiseaseStatusPreIndex_by_PL_', file_date)
outcome_df_pre_index_odd_name <- paste0('ccu022_01_WhiteBritish_OddYOB_IndexMid2020_OutcomeDiseaseStatusPreIndex_by_PL_', file_date)

saveRDS(outcome_df_pre_index_even, paste0(temp_data_dir, outcome_df_pre_index_even_name))
saveRDS(outcome_df_pre_index_odd, paste0(temp_data_dir, outcome_df_pre_index_odd_name))

##### Get matrix of pre-index outcome status (Outcome List 2) #####
outcome_df_pre_index_colnames2 <- c('PID',
                                    paste0(outcome_ds_vec2, '_preidx'))

outcome_df_pre_index_even2 <- outcome_df2_even[, ..outcome_df_pre_index_colnames2] 
outcome_df_pre_index_odd2 <- outcome_df2_odd[, ..outcome_df_pre_index_colnames2]

colnames(outcome_df_pre_index_even2) <- c('PID', outcome_ds_vec2)
colnames(outcome_df_pre_index_odd2) <- c('PID', outcome_ds_vec2)

outcome_df_pre_index_even_name2 <- paste0('ccu022_01_WhiteBritish_EvenYOB_IndexMid2020_OutcomeDiseaseStatusPreIndex_OutcomeList2_by_PL_', file_date)
outcome_df_pre_index_odd_name2 <- paste0('ccu022_01_WhiteBritish_OddYOB_IndexMid2020_OutcomeDiseaseStatusPreIndex_OutcomeList2_by_PL_', file_date)

saveRDS(outcome_df_pre_index_even2, paste0(temp_data_dir, outcome_df_pre_index_even_name2))
saveRDS(outcome_df_pre_index_odd2, paste0(temp_data_dir, outcome_df_pre_index_odd_name2))



##### Get matrix of outcome status during follow-up (Outcome List 1) #####
outcome_df_fup_colnames <- c('PID',
                             paste0(outcome_ds_vec, '_fup'))

outcome_df_fup_even <- outcome_df_even[, ..outcome_df_fup_colnames] 
outcome_df_fup_odd <- outcome_df_odd[, ..outcome_df_fup_colnames]

colnames(outcome_df_fup_even) <- c('PID', outcome_ds_vec)
colnames(outcome_df_fup_odd) <- c('PID', outcome_ds_vec)

outcome_df_fup_even_name <- paste0('ccu022_01_WhiteBritish_EvenYOB_IndexMid2020_Followup1yr_OutcomeDiseaseStatusFollowup_by_PL_', file_date)
outcome_df_fup_odd_name <- paste0('ccu022_01_WhiteBritish_OddYOB_IndexMid2020_Followup1yr_OutcomeDiseaseStatusFollowup_by_PL_', file_date)

saveRDS(outcome_df_fup_even, paste0(temp_data_dir, outcome_df_fup_even_name))
saveRDS(outcome_df_fup_odd, paste0(temp_data_dir, outcome_df_fup_odd_name))

##### Get matrix of outcome status during follow-up (Outcome List 2) #####
outcome_df_fup_colnames2 <- c('PID',
                              paste0(outcome_ds_vec2, '_fup'))

outcome_df_fup_even2 <- outcome_df2_even[, ..outcome_df_fup_colnames2] 
outcome_df_fup_odd2 <- outcome_df2_odd[, ..outcome_df_fup_colnames2]

colnames(outcome_df_fup_even2) <- c('PID', outcome_ds_vec2)
colnames(outcome_df_fup_odd2) <- c('PID', outcome_ds_vec2)

outcome_df_fup_even_name2 <- paste0('ccu022_01_WhiteBritish_EvenYOB_IndexMid2020_Followup1yr_OutcomeDiseaseStatusFollowup_OutcomeList2_by_PL_', file_date)
outcome_df_fup_odd_name2 <- paste0('ccu022_01_WhiteBritish_OddYOB_IndexMid2020_Followup1yr_OutcomeDiseaseStatusFollowup_OutcomeList2_by_PL_', file_date)

saveRDS(outcome_df_fup_even2, paste0(temp_data_dir, outcome_df_fup_even_name2))
saveRDS(outcome_df_fup_odd2, paste0(temp_data_dir, outcome_df_fup_odd_name2))

##### Get matrix of time to outcome during follow-up (Outcome List 1) #####
outcome_df_tte_colnames <- c('PID',
                             paste0(outcome_ds_vec, '_tte'))

outcome_df_tte_even <- outcome_df_even[, ..outcome_df_tte_colnames] 
outcome_df_tte_odd <- outcome_df_odd[, ..outcome_df_tte_colnames]

colnames(outcome_df_tte_even) <- c('PID', outcome_ds_vec)
colnames(outcome_df_tte_odd) <- c('PID', outcome_ds_vec)

outcome_df_tte_even_name <- paste0('ccu022_01_WhiteBritish_EvenYOB_IndexMid2020_Followup1yr_TimeToOutcomeFollowup_by_PL_', file_date)
outcome_df_tte_odd_name <- paste0('ccu022_01_WhiteBritish_OddYOB_IndexMid2020_Followup1yr_TimeToOutcomeFollowup_by_PL_', file_date)

saveRDS(outcome_df_tte_even, paste0(temp_data_dir, outcome_df_tte_even_name))
saveRDS(outcome_df_tte_odd, paste0(temp_data_dir, outcome_df_tte_odd_name))

##### Get matrix of time to outcome during follow-up (Outcome List 2) #####
outcome_df_tte_colnames2 <- c('PID',
                              paste0(outcome_ds_vec2, '_tte'))

outcome_df_tte_even2 <- outcome_df2_even[, ..outcome_df_tte_colnames2] 
outcome_df_tte_odd2 <- outcome_df2_odd[, ..outcome_df_tte_colnames2]

colnames(outcome_df_tte_even2) <- c('PID', outcome_ds_vec2)
colnames(outcome_df_tte_odd2) <- c('PID', outcome_ds_vec2)

outcome_df_tte_even_name2 <- paste0('ccu022_01_WhiteBritish_EvenYOB_IndexMid2020_Followup1yr_TimeToOutcomeFollowup_OutcomeList2_by_PL_', file_date)
outcome_df_tte_odd_name2 <- paste0('ccu022_01_WhiteBritish_OddYOB_IndexMid2020_Followup1yr_TimeToOutcomeFollowup_OutcomeList2_by_PL_', file_date)

saveRDS(outcome_df_tte_even2, paste0(temp_data_dir, outcome_df_tte_even_name2))
saveRDS(outcome_df_tte_odd2, paste0(temp_data_dir, outcome_df_tte_odd_name2))

##### Sanity check #####
all(sapply(list(covar_df_even$PID, 
                outcome_df_pre_index_even$PID, 
                outcome_df_pre_index_even2$PID,
                outcome_df_fup_even$PID, 
                outcome_df_fup_even2$PID, 
                outcome_df_tte_even$PID,
                outcome_df_tte_even2$PID), FUN = identical, 
           pred_df_pre_index_even$PID)) %>% print()


all(sapply(list(covar_df_odd$PID, 
                outcome_df_pre_index_odd$PID, 
                outcome_df_pre_index_odd2$PID, 
                outcome_df_fup_odd$PID, 
                outcome_df_fup_odd2$PID, 
                outcome_df_tte_odd$PID,
                outcome_df_tte_odd2$PID), FUN = identical, 
           pred_df_pre_index_odd$PID)) %>% print()

print(length(pred_df_pre_index_even$PID) + length(pred_df_pre_index_odd$PID))



