### For each outcome, save context-specific testing data (full sample) for analysis
### Divide testing data into 3 age groups: 40-49, 50-59, 60-69 at index
### Divide testing data into 2 sex groups: male, female
### Subset testing data based on number of comorbidities pre-index
### Subset testing data based on year of outcome incidence
### Odd DOB: training, Even DOB: testing
### Index: 2020-01-01, follow-up: 5 years

rm(list = ls())
gc()

#### Load libraries ####
library(data.table)
library(tidyverse)
library(doParallel)
library(doRNG)

source('~/collab/CCU022/CCU022_CRS/0.1_Functions.R')

#### Set parameters ####
temp_data_dir_step1_0 <- '~/collab/CCU022/CCU022_CRS/Data/Step1.0/'
temp_data_dir_step1_1 <- '~/collab/CCU022/CCU022_CRS/Data/Step1.1/'
temp_data_dir_step2_1 <- '~/collab/CCU022/CCU022_CRS/Data/Step2.1/'
temp_data_dir_step2_2 <- '~/collab/CCU022/CCU022_CRS/Data/Step2.2/'
temp_data_dir_step2_3 <- '~/collab/CCU022/CCU022_CRS/Data/Step2.3/'
temp_data_dir <- '~/collab/CCU022/CCU022_CRS/Data/Step6.0/'
file_date <- '2025_05_01'

## Number of cores used in parallel computing
#n_cores <- 8

## Comorbidity settings
comorb_settings <- list(c(1,1),
                        c(2,2),
                        c(3,4),
                        c(5,6),
                        c(7,Inf))
n_comorb_settings <- length(comorb_settings)

## Outcome year settings
outcome_year_settings <- list(c(1,1),
                              c(2,2),
                              c(3,4),
                              c(5,6))
n_outcome_year_settings <- length(outcome_year_settings)

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


##### Phecode information #####
phe_icd10_codelist_name = paste0("ccu022_01_PheCode_ICD10AtoN_map_by_PL_", file_date)
icd10_phecode_map <- readRDS(paste0(temp_data_dir_step1_0, 
                                    phe_icd10_codelist_name))
phecode_info <- icd10_phecode_map %>%
  select(PheCode, Phenotype, Category, Sex) %>%
  distinct()


#### Loop through each outcome - focus on COVID-19 outcome here (last outcome) ####

### Use parallel computing
## Set up clusters
#cl <- makeCluster(n_cores, outfile = "")
#registerDoParallel(cl)

## Set random seed for parallel computing
#registerDoRNG(seed = 2025)

#foreach (i = 1:n_outcome_ds, 
#         .packages = c("data.table", "tidyverse")) %dopar% {

for (i in n_outcome_ds) {
           
  target_ds <- outcome_ds_vec[i]
           
  last_file_name <- paste0(temp_data_dir, 'Outcome', target_ds,
                           '_OutcomeYearSetting', n_outcome_year_settings,
                           '_TestData_AllPts_WhiteBritish_EvenYOB_Index2020_Followup5yr.rds')
                
  if (file.exists(last_file_name)) {
    print(paste0('Completed Outcome ', i))
  } else {
  
    ##### Read full testing data #####
    if (target_ds == "COVID_hspt") {
      target_ds_sex <- "Both"
    } else {
      target_ds_sex <- phecode_info %>%
        filter(PheCode == str_replace(target_ds, '_', '\\.')) %>%
        pull(Sex)
    }
    test_df_combine <- readRDS(paste0(temp_data_dir_step2_3, 'Outcome',
                                    target_ds, '_TestData_AllPts_WhiteBritish_EvenYOB_Index2020_Followup5yr.rds'))
    
    ##### Subset based on age group #####
    test_df_combine_age40 <- test_df_combine[age_at_2020 >= 40 & age_at_2020 < 50]
    test_df_combine_age50 <- test_df_combine[age_at_2020 >= 50 & age_at_2020 < 60]
    test_df_combine_age60 <- test_df_combine[age_at_2020 >= 60 & age_at_2020 < 70]
    

    ###### Save subgroups ######
    saveRDS(test_df_combine_age40, paste0(temp_data_dir, 'Outcome', target_ds,
                                    '_Age40_49_TestData_AllPts_WhiteBritish_EvenYOB_Index2020_Followup5yr.rds'))
    saveRDS(test_df_combine_age50, paste0(temp_data_dir, 'Outcome', target_ds,
                                          '_Age50_59_TestData_AllPts_WhiteBritish_EvenYOB_Index2020_Followup5yr.rds'))
    saveRDS(test_df_combine_age60, paste0(temp_data_dir, 'Outcome', target_ds,
                                          '_Age60_69_TestData_AllPts_WhiteBritish_EvenYOB_Index2020_Followup5yr.rds'))
    
    ##### Subset based on sex (non-sex-specific outcomes only) #####
    if (target_ds_sex %in% c("Both", "Empty")) {
    
      test_df_combine_male <- test_df_combine[Female == 0]
      test_df_combine_female <- test_df_combine[Female == 1]
    
      ###### Save subgroups ######
      saveRDS(test_df_combine_male, paste0(temp_data_dir, 'Outcome', target_ds,
                                           '_SexMale_TestData_AllPts_WhiteBritish_EvenYOB_Index2020_Followup5yr.rds'))
      saveRDS(test_df_combine_female, paste0(temp_data_dir, 'Outcome', target_ds,
                                             '_SexFemale_TestData_AllPts_WhiteBritish_EvenYOB_Index2020_Followup5yr.rds'))
      
    }
    
    
    ##### Subset based on number of comorbidities pre-index #####
    pred_ds_colnames <- colnames(test_df_combine)[startsWith(colnames(test_df_combine), "ds")]
    test_df_combine[, Sum_comorb := rowSums(.SD), .SDcols = pred_ds_colnames]
    
    ## select cutoff based on quintiles (outcome i = 1)
    # n_comorb_vec <- test_df_combine$Sum_comorb
    # quantile(n_comorb_vec, seq(0, 1, 0.2))
    #  0%  20%  40%  60%  80% 100% 
    #  1    1    2    4    6   75 
    
    for (k in 1:n_comorb_settings) {
      setting_k <- comorb_settings[[k]]
      test_df_combine_comorb <- test_df_combine[Sum_comorb >= setting_k[1] & 
                                                 Sum_comorb <= setting_k[2]] %>%
        .[, Sum_comorb := NULL]
      
      ###### Save subgroups ######
      saveRDS(test_df_combine_comorb, paste0(temp_data_dir, 'Outcome', target_ds,
                                              '_ComorbiditySetting', k,
                                              '_TestData_AllPts_WhiteBritish_EvenYOB_Index2020_Followup5yr.rds'))
      
    }
 
    ##### Subset based on year of outcome incidence #####
    test_df_combine[, target_ds_year := if_else(target_ds_tte == 1827, 
                                                5,
                                                ceiling(target_ds_tte/365.25))]
    test_df_combine_cases <- test_df_combine[target_ds_status == 1]
    test_df_combine_controls <- test_df_combine[target_ds_status == 0]
    n_controls <- nrow(test_df_combine_controls)
    
    ## permutate controls and divide into subsets of n_outcome_year_settings
    #set.seed(2025 + i)
    #control_permuate_idx <- sample(1:n_controls)
    #test_df_combine_controls_permute <- test_df_combine_controls[control_permuate_idx,]
    #n_controls_per_setting <- round(n_controls/n_outcome_year_settings)
    
    for (k in 1:n_outcome_year_settings) {
      setting_k <- outcome_year_settings[[k]]
      
      test_df_combine_cases_k <- test_df_combine_cases[target_ds_year >= setting_k[1] & 
                                                       target_ds_year <= setting_k[2]]
      
      ## PL 20251210 update: require controls in each setting to have follow-up >= setting_k[1]
      if (k == n_outcome_year_settings) {
        test_df_combine_controls_k <- test_df_combine_controls[target_ds_year >= setting_k[1]]
      } else {
        test_df_combine_controls_k <- rbindlist(list(test_df_combine_cases[target_ds_year > setting_k[2]],
                                                     test_df_combine_controls[target_ds_year >= setting_k[1]])) %>%
          .[, target_ds_status := 0]
      }
      
      test_df_combine_final_k <- rbindlist(list(test_df_combine_cases_k,
                                                test_df_combine_controls_k)) %>%
        .[, c("Sum_comorb", "target_ds_year") := NULL]
      
      ###### Save subgroups ######
      saveRDS(test_df_combine_final_k, paste0(temp_data_dir, 'Outcome', target_ds,
                                             '_OutcomeYearSetting', k,
                                             '_TestData_AllPts_WhiteBritish_EvenYOB_Index2020_Followup5yr.rds'))
      
    }
    
    print(paste0('Completed Outcome ', i))
  }
}

#stopCluster(cl)



