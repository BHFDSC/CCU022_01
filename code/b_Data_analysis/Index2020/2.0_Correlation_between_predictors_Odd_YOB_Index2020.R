### Author: Pearl Liu
### Calculate the pairwise correlation of pre-index predictor disease status

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
##### Pre-index predictor disease status #####
#pred_df_pre_index_even_name <- paste0('ccu022_01_WhiteBritish_EvenYOB_Index2020_PredictorDiseaseStatus_by_PL_', file_date)
pred_df_pre_index_odd_name <- paste0('ccu022_01_WhiteBritish_OddYOB_Index2020_PredictorDiseaseStatus_by_PL_', file_date)

#pred_df_pre_index_even <- readRDS(paste0(temp_data_dir_step1_1, pred_df_pre_index_even_name))
pred_df_pre_index_odd <- readRDS(paste0(temp_data_dir_step1_1, pred_df_pre_index_odd_name))

##### List of predictors #####
pred_ds_list_name = paste0("ccu022_01_Predictor_Diseases_Pheparent_list_by_PL_", file_date)
pred_ds_list = readRDS(paste0(temp_data_dir_step1_0, pred_ds_list_name))

pred_ds_vec <- pred_ds_list %>% arrange(index) %>% pull(predictor_ds) 
n_pred_ds <- length(pred_ds_vec)

#### Index 2020, Training data: Calculate correlation matrix across predictors in the overall population ####
pred_df_mt <- pred_df_pre_index_odd[, PID := NULL]

pred_cor_mt <- matrix(NA, n_pred_ds, n_pred_ds)
diag(pred_cor_mt) <- 1
colnames(pred_cor_mt) <- pred_ds_vec
rownames(pred_cor_mt) <- pred_ds_vec

for (i in 1:(n_pred_ds-1)) {
  for (j in (i+1):n_pred_ds) {
    pred_cor_mt[i,j] <- cor(pred_df_mt[,..i], pred_df_mt[,..j])
    pred_cor_mt[j,i] <-  pred_cor_mt[i,j]
    
    print(paste0('Completed correlation between ds ', i, ' and ds ', j))
  }

}

## Transform from correlation matrix to long format, retain distinct rows
pred_cor_mt_long <- pred_cor_mt %>% as.data.frame() %>%
  mutate(Pred_ds1 = rownames(pred_cor_mt)) %>%
  pivot_longer(!Pred_ds1, names_to = "Pred_ds2", values_to = "Corr") %>%
  mutate(Pred_ds1 = as.numeric(Pred_ds1), Pred_ds2 = as.numeric(Pred_ds2)) %>%
  filter(Pred_ds1 < Pred_ds2)

saveRDS(pred_cor_mt_long, paste0(temp_data_dir, "PredictorDiseaseCorrelation_WhiteBritish_OddYOB_Index2020.rds"))

