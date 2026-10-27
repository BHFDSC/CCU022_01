### Author: Pearl Liu
### Save CVD-COVID-UK data from Databricks - non-White British ethnicity groups

rm(list = ls())
gc()

#### Load libraries and connect to DBI ####
library(DBI)

## Connect to DBI
con <- dbConnect(
  odbc::odbc(),
  dsn = 'databricks',
  HTTPPath = 'XXX',
  PWD = 'XXX')

#### Set parameters ####
db_collab <- "."   # Data directory from Databricks
temp_data_dir <- '~/collab/CCU022/CCU022_CRS/Data/Step1.0/'  # Target directory to save data
file_date <- "2025_05_01"
eth_name_vec <- c("OtherWhite", "Asian", "Black", "Mixed")

#### Loop through each ethnicity ####

save_predictor <- F
save_outcomelist1 <- F
save_outcomelist2 <- F

for (i in 1:length(eth_name_vec)) {
  
  eth_name <- eth_name_vec[i]
  
  if (eth_name == "OtherWhite") {
    save_predictor <- T
    save_outcomelist1 <- T
  } else {
    save_predictor <- F
    save_outcomelist1 <- F
  }
  
  if (save_predictor == T) {

    #### Read and save predictor data for each ethnicity group - Index date: 2020-01-01 ####
    pred_df_name = paste0("ccu022_01_", eth_name, "_Age40_69_Index2020_PredictorData_by_PL_", file_date)
  
    pred_df <- dbGetQuery(con, paste0('select * from ', db_collab, pred_df_name))
   
    saveRDS(pred_df, paste0(temp_data_dir, pred_df_name))
  
    rm(pred_df)
    gc()
  
  }
  
  if (save_outcomelist1 == T) {
    #### Read and save outcome data for each age group - White British, Index date: 2020-01-01, Follow-up: 5 years ####
    outcome_df_name = paste0("ccu022_01_", eth_name, "_Age40_69_Index2020_AllOutcomeData_by_PL_", file_date)
    
    outcome_df <- dbGetQuery(con, paste0('select * from ', db_collab, outcome_df_name))
    
    saveRDS(outcome_df, paste0(temp_data_dir, outcome_df_name))
    
    rm(outcome_df)
    gc()
  }
  
  if (save_outcomelist2 == T) {
    #### Read and save outcome data for each age group - Outcome List 2 - White British, Index date: 2015-01-01, Follow-up: 10 years ####
    outcome_df_name = paste0("ccu022_01_", eth_name, "_Age40_69_Index2020_AllOutcomeData_OutcomeList2_by_PL_", file_date)
    
    outcome_df <- dbGetQuery(con, paste0('select * from ', db_collab, outcome_df_name))
    
    saveRDS(outcome_df, paste0(temp_data_dir, outcome_df_name))
    
    rm(outcome_df)
    gc()
  }

}

