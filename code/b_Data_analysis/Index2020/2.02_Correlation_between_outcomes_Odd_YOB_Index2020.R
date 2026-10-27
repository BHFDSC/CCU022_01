### Author: Pearl Liu
### Calculate the pairwise correlation of followup outcome disease status

rm(list = ls())
gc()

#### Load libraries ####
library(data.table)
library(tidyverse)

#### Set parameters ####
temp_data_dir_step1_0 <- '~/collab/CCU022/CCU022_CRS/Data/Step1.0/'
temp_data_dir_step1_1 <- '~/collab/CCU022/CCU022_CRS/Data/Step1.1/'
temp_data_dir <- '~/collab/CCU022/CCU022_CRS/Data/Step2.0/'
file_date <- '2025_05_01'

#### Read processed data ####
Combine_data <- F

if (Combine_data == T) {
  
##### Pre-index outcome disease status - Outcome List 1 and 2 #####  
#outcome_df_pre_index_even_name <- paste0('ccu022_01_WhiteBritish_EvenYOB_Index2020_OutcomeDiseaseStatusPreIndex_by_PL_', file_date)
outcome_df_pre_index_odd_name <- paste0('ccu022_01_WhiteBritish_OddYOB_Index2020_OutcomeDiseaseStatusPreIndex_by_PL_', file_date)

#saveRDS(outcome_df_pre_index_even, paste0(temp_data_dir_step1_1, outcome_df_pre_index_even_name))
outcome_df_pre_index_odd <- readRDS(paste0(temp_data_dir_step1_1, outcome_df_pre_index_odd_name))


#outcome_df_pre_index_even_name2 <- paste0('ccu022_01_WhiteBritish_EvenYOB_Index2020_OutcomeDiseaseStatusPreIndex_OutcomeList2_by_PL_', file_date)
outcome_df_pre_index_odd_name2 <- paste0('ccu022_01_WhiteBritish_OddYOB_Index2020_OutcomeDiseaseStatusPreIndex_OutcomeList2_by_PL_', file_date)

#saveRDS(outcome_df_pre_index_even2, paste0(temp_data_dir_step1_1, outcome_df_pre_index_even_name2))
outcome_df_pre_index_odd2 <- readRDS(paste0(temp_data_dir_step1_1, outcome_df_pre_index_odd_name2))

all(outcome_df_pre_index_odd$PID == outcome_df_pre_index_odd2$PID)

## Join two data tables
outcome_df_preidx_odd_full <- outcome_df_pre_index_odd[, colnames(outcome_df_pre_index_odd2[,-1]) := outcome_df_pre_index_odd2[,-1]]

saveRDS(outcome_df_preidx_odd_full,
        paste0(temp_data_dir_step1_1,
               'ccu022_01_WhiteBritish_OddYOB_Index2020_Followup5yr_OutcomeDiseaseStatusPreIndex_FullOutcomeList_by_PL_', file_date))

} else {
  
##### Followup outcome disease status - Outcome List 1 and 2 #####
#outcome_df_fup_even_name <- paste0('ccu022_01_WhiteBritish_EvenYOB_Index2020_Followup5yr_OutcomeDiseaseStatusFollowup_by_PL_', file_date)
outcome_df_fup_odd_name <- paste0('ccu022_01_WhiteBritish_OddYOB_Index2020_Followup5yr_OutcomeDiseaseStatusFollowup_by_PL_', file_date)

#outcome_df_fup_even <- readRDS(paste0(temp_data_dir_step1_1, outcome_df_fup_even_name))
outcome_df_fup_odd <- readRDS(paste0(temp_data_dir_step1_1, outcome_df_fup_odd_name))


#outcome_df_fup_even_name2 <- paste0('ccu022_01_WhiteBritish_EvenYOB_Index2020_Followup5yr_OutcomeDiseaseStatusFollowup_OutcomeList2_by_PL_', file_date)
outcome_df_fup_odd_name2 <- paste0('ccu022_01_WhiteBritish_OddYOB_Index2020_Followup5yr_OutcomeDiseaseStatusFollowup_OutcomeList2_by_PL_', file_date)

#outcome_df_fup_even2 <- readRDS(paste0(temp_data_dir_step1_1, outcome_df_fup_even_name2))
outcome_df_fup_odd2 <- readRDS(paste0(temp_data_dir_step1_1, outcome_df_fup_odd_name2))

all(outcome_df_fup_odd$PID == outcome_df_fup_odd2$PID)

## Join two data tables
outcome_df_fup_odd_full <- outcome_df_fup_odd[, colnames(outcome_df_fup_odd2[,-1]) := outcome_df_fup_odd2[,-1]]

saveRDS(outcome_df_fup_odd_full,
        paste0(temp_data_dir_step1_1,
         'ccu022_01_WhiteBritish_OddYOB_Index2020_Followup5yr_OutcomeDiseaseStatusFollowup_FullOutcomeList_by_PL_', file_date))

} else {
##### Read full outcome status data #####
outcome_df_fup_odd_full <- readRDS(paste0(temp_data_dir_step1_1,
                                          'ccu022_01_WhiteBritish_OddYOB_Index2020_Followup5yr_OutcomeDiseaseStatusFollowup_FullOutcomeList_by_PL_', file_date))

outcome_df_preidx_odd_full <- readRDS(paste0(temp_data_dir_step1_1,
                                          'ccu022_01_WhiteBritish_OddYOB_Index2020_Followup5yr_OutcomeDiseaseStatusPreIndex_FullOutcomeList_by_PL_', file_date))


}

##### List of outcomes #####
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


#### Index 2020, Training data: Calculate correlation matrix across 88 outcomes (without removing pts with prior diseases) ####
## Restrict outcomes to those of interest
colnames_final <- c("PID", outcome_ds_vec)
outcome_df_fup_odd_analysis <- outcome_df_fup_odd_full[, ..colnames_final]
## Replace 9 (censored) to 0 for all entries
outcome_df_fup_odd_analysis[] <- lapply(outcome_df_fup_odd_analysis, function(v) {
  if (is.numeric(v)) replace(v, v == 9, 0) else v
})

outcome_df_mt <- outcome_df_fup_odd_analysis[, PID := NULL]

outcome_cor_mt <- matrix(NA, n_outcome_ds, n_outcome_ds)
diag(outcome_cor_mt) <- 1
colnames(outcome_cor_mt) <- outcome_ds_vec
rownames(outcome_cor_mt) <- outcome_ds_vec

for (i in 1:(n_outcome_ds-1)) {
  for (j in (i+1):n_outcome_ds) {
    outcome_i <- outcome_ds_vec[i]
    outcome_j <- outcome_ds_vec[j]
    outcome_cor_mt[i,j] <- cor(outcome_df_mt[,..outcome_i], outcome_df_mt[,..outcome_j])
    outcome_cor_mt[j,i] <-  outcome_cor_mt[i,j]
    
    print(paste0('Completed correlation between ds ', i, ' and ds ', j))
  }

}

##### Get outcome incidence #####
outcome_incid_vec <- data.frame("Outcome_phecode" = outcome_ds_vec,
                                "Incidence_5yr" = rep(NA, n_outcome_ds))

for (i in 1:n_outcome_ds) {
  target_ds <- outcome_ds_vec[i]
  outcome_incid_vec[i, "Incidence_5yr"] <- mean(outcome_df_mt[[target_ds]])
}


##### Transform from correlation matrix to long format, retain distinct rows #####
outcome_cor_mt_long <- outcome_cor_mt %>% as.data.frame() %>%
  mutate(Outcome_ds1 = rownames(outcome_cor_mt)) %>%
  pivot_longer(!Outcome_ds1, names_to = "Outcome_ds2", values_to = "Corr") %>%
  filter(Outcome_ds1 < Outcome_ds2) %>%
  ## join with outcome incidence
  left_join(outcome_incid_vec, by = join_by("Outcome_ds1" == "Outcome_phecode")) %>%
  rename(Outcome_ds1_5yr_incid = Incidence_5yr) %>%
  left_join(outcome_incid_vec, by = join_by("Outcome_ds2" == "Outcome_phecode")) %>%
  rename(Outcome_ds2_5yr_incid = Incidence_5yr) %>%
  mutate(Outcome_ds1 = if_else(Outcome_ds1 == "COVID_hspt", 
                               Outcome_ds1, 
                               str_replace(Outcome_ds1, "_", ".")),
         Outcome_ds2 =  if_else(Outcome_ds2 == "COVID_hspt", 
                                Outcome_ds2,
                                str_replace(Outcome_ds2, "_", ".")))

saveRDS(outcome_cor_mt_long, 
        paste0(temp_data_dir, "OutcomeDiseaseCorrelation_WhiteBritish_OddYOB_Index2020.rds"))

write.csv(outcome_cor_mt_long,
          paste0(temp_data_dir, "OutcomeDiseaseCorrelation_WhiteBritish_OddYOB_Index2020.csv"),
          row.names = F)

outcome_cor_mt_long %>% 
  filter(Outcome_ds1 %in% c("480", "994_2", "COVID_hspt") &
         Outcome_ds2 %in% c("480", "994_2", "COVID_hspt")) %>%
  View()
  
outcome_cor_mt_long %>% 
  filter(Outcome_ds1 %in% c("411_2", "411_4", "433_21") &
         Outcome_ds2 %in% c("411_2", "411_4", "433_21") ) %>%
  View()


#### Index 2020, Training data: Calculate correlation matrix across 88 outcomes (removing pts with any prior diseases) ####
## Restrict outcomes to those of interest
colnames_final <- c("PID", outcome_ds_vec)
outcome_df_fup_odd_analysis <- outcome_df_fup_odd_full[, ..colnames_final]
outcome_df_preidx_odd_analysis <- outcome_df_preidx_odd_full[, ..colnames_final] %>%
  .[, row_sum := rowSums(.SD, na.rm = TRUE), .SDcols = outcome_ds_vec]

all(outcome_df_fup_odd_analysis$PID == outcome_df_preidx_odd_analysis$PID)

pts_no_prior_ds <- outcome_df_preidx_odd_analysis[row_sum == 0] %>%
  pull()

mean(outcome_df_preidx_odd_analysis$row_sum == 0) * 100
# 12.98

## Only 13% of pts did not have any prior outcome disease, making them an over-healthy non-representative sample

