### Author: Pearl Liu
### Save CVD-COVID-UK data from Databricks

rm(list = ls())
gc()

#### Load libraries and connect to DBI ####
library(DBI)

## Connect to DBI 
# updated 20251112 after SDE update
# updated 20260527
con <- dbConnect(
  odbc::odbc(),
  dsn = 'databricks',
  HTTPPath = 'XXX',
  PWD = 'XXX'
)


#### Set parameters ####
db_collab <- "."   # Data directory from Databricks
temp_data_dir <- '~/collab/CCU022/CCU022_CRS/Data/Step1.0/'  # Target directory to save data
file_date <- "2025_05_01"
index_year <- 2020

#### Read and save predictor data for each age group - White British, Index date: 2020-01-01 ####
pred_df_age40_even_name = paste0("ccu022_01_WhiteBritish_Age40_49_EvenYOB_Index2020_PredictorData_by_PL_", file_date)
pred_df_age40_odd_name = paste0('ccu022_01_WhiteBritish_Age40_49_OddYOB_Index2020_PredictorData_by_PL_', file_date)
pred_df_age50_even_name = paste0("ccu022_01_WhiteBritish_Age50_59_EvenYOB_Index2020_PredictorData_by_PL_", file_date)
pred_df_age50_odd_name = paste0('ccu022_01_WhiteBritish_Age50_59_OddYOB_Index2020_PredictorData_by_PL_', file_date)
pred_df_age60_even_name = paste0("ccu022_01_WhiteBritish_Age60_69_EvenYOB_Index2020_PredictorData_by_PL_", file_date)
pred_df_age60_odd_name = paste0('ccu022_01_WhiteBritish_Age60_69_OddYOB_Index2020_PredictorData_by_PL_', file_date)

pred_df_age40_even <- dbGetQuery(con,
                                 paste0('select * from ', db_collab, pred_df_age40_even_name))
pred_df_age40_odd <-dbGetQuery(con,
                               paste0('select * from ', db_collab, pred_df_age40_odd_name))
pred_df_age50_even <- dbGetQuery(con,
                                 paste0('select * from ', db_collab, pred_df_age50_even_name))
pred_df_age50_odd <- dbGetQuery(con,
                                paste0('select * from ', db_collab, pred_df_age50_odd_name))
pred_df_age60_even <- dbGetQuery(con,
                                 paste0('select * from ', db_collab, pred_df_age60_even_name))
pred_df_age60_odd <- dbGetQuery(con,
                                paste0('select * from ', db_collab, pred_df_age60_odd_name))

saveRDS(pred_df_age40_even, paste0(temp_data_dir, pred_df_age40_even_name))
saveRDS(pred_df_age40_odd, paste0(temp_data_dir, pred_df_age40_odd_name))
saveRDS(pred_df_age50_even, paste0(temp_data_dir, pred_df_age50_even_name))
saveRDS(pred_df_age50_odd, paste0(temp_data_dir, pred_df_age50_odd_name))
saveRDS(pred_df_age60_even, paste0(temp_data_dir, pred_df_age60_even_name))
saveRDS(pred_df_age60_odd, paste0(temp_data_dir, pred_df_age60_odd_name))

rm(pred_df_age40_even, pred_df_age40_odd, 
   pred_df_age50_even, pred_df_age50_odd, 
   pred_df_age60_even, pred_df_age60_odd)
gc()

#### Read and save CCI and IMD info - White British, Index date: 2020-01-01 ####
cci_imd_df_name = paste0("ccu022_01_WhiteBritish_Age40_69_Index2020_CCI_IMD_final_by_PL_", file_date)

cci_imd_df <- dbGetQuery(con,
                         paste0('select * from ', db_collab, cci_imd_df_name))

saveRDS(cci_imd_df, paste0(temp_data_dir, cci_imd_df_name))



#### Read and save outcome data for each age group from Outcome List 1 - White British, Index date: 2020-01-01, Follow-up: 5 years ####
outcome_df_age40_even_name = paste0("ccu022_01_WhiteBritish_Age40_49_EvenYOB_Index2020_AllOutcomeData_by_PL_", file_date)
outcome_df_age40_odd_name = paste0('ccu022_01_WhiteBritish_Age40_49_OddYOB_Index2020_AllOutcomeData_by_PL_', file_date)
outcome_df_age50_even_name = paste0("ccu022_01_WhiteBritish_Age50_59_EvenYOB_Index2020_AllOutcomeData_by_PL_", file_date)
outcome_df_age50_odd_name = paste0('ccu022_01_WhiteBritish_Age50_59_OddYOB_Index2020_AllOutcomeData_by_PL_', file_date)
outcome_df_age60_even_name = paste0("ccu022_01_WhiteBritish_Age60_69_EvenYOB_Index2020_AllOutcomeData_by_PL_", file_date)
outcome_df_age60_odd_name = paste0('ccu022_01_WhiteBritish_Age60_69_OddYOB_Index2020_AllOutcomeData_by_PL_', file_date)


outcome_df_age40_even <- dbGetQuery(con,
                                 paste0('select * from ', db_collab, outcome_df_age40_even_name))
outcome_df_age40_odd <-dbGetQuery(con,
                               paste0('select * from ', db_collab, outcome_df_age40_odd_name))
outcome_df_age50_even <- dbGetQuery(con,
                                 paste0('select * from ', db_collab, outcome_df_age50_even_name))
outcome_df_age50_odd <- dbGetQuery(con,
                                paste0('select * from ', db_collab, outcome_df_age50_odd_name))
outcome_df_age60_even <- dbGetQuery(con,
                                 paste0('select * from ', db_collab, outcome_df_age60_even_name))
outcome_df_age60_odd <- dbGetQuery(con,
                                paste0('select * from ', db_collab, outcome_df_age60_odd_name))


saveRDS(outcome_df_age40_even, paste0(temp_data_dir, outcome_df_age40_even_name))
saveRDS(outcome_df_age40_odd, paste0(temp_data_dir, outcome_df_age40_odd_name))
saveRDS(outcome_df_age50_even, paste0(temp_data_dir, outcome_df_age50_even_name))
saveRDS(outcome_df_age50_odd, paste0(temp_data_dir, outcome_df_age50_odd_name))
saveRDS(outcome_df_age60_even, paste0(temp_data_dir, outcome_df_age60_even_name))
saveRDS(outcome_df_age60_odd, paste0(temp_data_dir, outcome_df_age60_odd_name))


rm(outcome_df_age40_even, outcome_df_age40_odd, 
   outcome_df_age50_even, outcome_df_age50_odd, 
   outcome_df_age60_even, outcome_df_age60_odd)
gc()


#### Read and save outcome data from Outcome List 2 - White British, Index date: 2020-01-01, Follow-up: 10 years ####
outcome_df2_even_name = paste0("ccu022_01_WhiteBritish_Age40_69_EvenYOB_Index2020_AllOutcomeData_OutcomeList2_by_PL_", file_date)
outcome_df2_odd_name = paste0('ccu022_01_WhiteBritish_Age40_69_OddYOB_Index2020_AllOutcomeData_OutcomeList2_by_PL_', file_date)

outcome_df2_even <- dbGetQuery(con,
                               paste0('select * from ', db_collab, outcome_df2_even_name))
outcome_df2_odd <-dbGetQuery(con,
                             paste0('select * from ', db_collab, outcome_df2_odd_name))

saveRDS(outcome_df2_even, paste0(temp_data_dir, outcome_df2_even_name))
saveRDS(outcome_df2_odd, paste0(temp_data_dir, outcome_df2_odd_name))

rm(outcome_df2_even, outcome_df2_odd)
gc()


if (index_year == 2015) {

#### Read and save list of predictors and outcomes ####
pred_ds_list_name = paste0("ccu022_01_Predictor_Diseases_Pheparent_list_by_PL_", file_date)
outcome_ds_list_name = paste0("ccu022_01_Outcome_Diseases_Phecode_list_by_PL_", file_date)


pred_ds_list <- dbGetQuery(con,
                          paste0('select * from ', db_collab, pred_ds_list_name))
outcome_ds_list <- dbGetQuery(con,
                          paste0('select * from ', db_collab, outcome_ds_list_name))


saveRDS(pred_ds_list, paste0(temp_data_dir, pred_ds_list_name))
saveRDS(outcome_ds_list, paste0(temp_data_dir, outcome_ds_list_name))


#### Read and save map between ICD10 codes and phecodes ####
phe_icd10_codelist_name = paste0("ccu022_01_PheCode_ICD10AtoN_map_by_PL_", file_date)

icd10_phecode_map <- dbGetQuery(con,
                                paste0('select * from ', db_collab, phe_icd10_codelist_name))

saveRDS(icd10_phecode_map, paste0(temp_data_dir, phe_icd10_codelist_name))


}


#### Read and save Qcovid covariates data - White British, Index date: 2020-01-01 ####
qcovid_cov_BMI_avail_age40_even_name <- "ccu022_crs_Qcovid_final_covariates_BMIAvailable_WhiteBritish_Age40_49_EvenYOB_Index2020"
qcovid_cov_BMI_avail_age50_even_name <- "ccu022_crs_Qcovid_final_covariates_BMIAvailable_WhiteBritish_Age50_59_EvenYOB_Index2020"
qcovid_cov_BMI_avail_age60_even_name <- "ccu022_crs_Qcovid_final_covariates_BMIAvailable_WhiteBritish_Age60_69_EvenYOB_Index2020"
qcovid_cov_BMI_avail_age40_odd_name <- "ccu022_crs_Qcovid_final_covariates_BMIAvailable_WhiteBritish_Age40_49_OddYOB_Index2020"
qcovid_cov_BMI_avail_age50_odd_name <- "ccu022_crs_Qcovid_final_covariates_BMIAvailable_WhiteBritish_Age50_59_OddYOB_Index2020"
qcovid_cov_BMI_avail_age60_odd_name <- "ccu022_crs_Qcovid_final_covariates_BMIAvailable_WhiteBritish_Age60_69_OddYOB_Index2020"


qcovid_cov_BMI_avail_age40_even <- dbGetQuery(con,
                                              paste0('select * from ', db_collab, qcovid_cov_BMI_avail_age40_even_name))
qcovid_cov_BMI_avail_age50_even <- dbGetQuery(con,
                                              paste0('select * from ', db_collab, qcovid_cov_BMI_avail_age50_even_name))
qcovid_cov_BMI_avail_age60_even <- dbGetQuery(con,
                                              paste0('select * from ', db_collab, qcovid_cov_BMI_avail_age60_even_name))

qcovid_cov_BMI_avail_age40_odd <- dbGetQuery(con,
                                              paste0('select * from ', db_collab, qcovid_cov_BMI_avail_age40_odd_name))
qcovid_cov_BMI_avail_age50_odd <- dbGetQuery(con,
                                              paste0('select * from ', db_collab, qcovid_cov_BMI_avail_age50_odd_name))
qcovid_cov_BMI_avail_age60_odd <- dbGetQuery(con,
                                              paste0('select * from ', db_collab, qcovid_cov_BMI_avail_age60_odd_name))


saveRDS(qcovid_cov_BMI_avail_age40_even, 
        paste0(temp_data_dir, qcovid_cov_BMI_avail_age40_even_name))
saveRDS(qcovid_cov_BMI_avail_age50_even, 
        paste0(temp_data_dir, qcovid_cov_BMI_avail_age50_even_name))
saveRDS(qcovid_cov_BMI_avail_age60_even, 
        paste0(temp_data_dir, qcovid_cov_BMI_avail_age60_even_name))
saveRDS(qcovid_cov_BMI_avail_age40_odd, 
        paste0(temp_data_dir, qcovid_cov_BMI_avail_age40_odd_name))
saveRDS(qcovid_cov_BMI_avail_age50_odd, 
        paste0(temp_data_dir, qcovid_cov_BMI_avail_age50_odd_name))
saveRDS(qcovid_cov_BMI_avail_age60_odd, 
        paste0(temp_data_dir, qcovid_cov_BMI_avail_age60_odd_name))

