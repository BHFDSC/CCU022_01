### Author: Pearl Liu
### Save CVD-COVID-UK data from Databricks

rm(list = ls())
gc()

#### Load libraries and connect to DBI ####
library(DBI)

## Connect to DBI
# updated 20260527
con <- dbConnect(
  odbc::odbc(),
  dsn = 'databricks',
  HTTPPath = 'XXX',  # plug in path
  PWD = 'XXX'   # plug in password
)


#### Set parameters ####
db_collab <- "."   # Data directory from Databricks
temp_data_dir <- '~/collab/CCU022/CCU022_CRS/Data/Step1.0/'  # Target directory to save data
file_date <- "2025_05_01"

#### Read and save predictor data for each age group - White British, Index date: 2015-01-01 ####
pred_df_age40_even_name = paste0("ccu022_01_WhiteBritish_Age40_49_EvenYOB_Index2015_PredictorData_by_PL_", file_date)
pred_df_age40_odd_name = paste0('ccu022_01_WhiteBritish_Age40_49_OddYOB_Index2015_PredictorData_by_PL_', file_date)
pred_df_age50_even_name = paste0("ccu022_01_WhiteBritish_Age50_59_EvenYOB_Index2015_PredictorData_by_PL_", file_date)
pred_df_age50_odd_name = paste0('ccu022_01_WhiteBritish_Age50_59_OddYOB_Index2015_PredictorData_by_PL_', file_date)
pred_df_age60_even_name = paste0("ccu022_01_WhiteBritish_Age60_69_EvenYOB_Index2015_PredictorData_by_PL_", file_date)
pred_df_age60_odd_name = paste0('ccu022_01_WhiteBritish_Age60_69_OddYOB_Index2015_PredictorData_by_PL_', file_date)

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


#### Read and save CCI and IMD info - White British, Index date: 2015-01-01 ####
cci_imd_df_name = paste0("ccu022_01_WhiteBritish_Age40_69_Index2015_CCI_IMD_final_by_PL_", file_date)

cci_imd_df <- dbGetQuery(con,
                                 paste0('select * from ', db_collab, cci_imd_df_name))

saveRDS(cci_imd_df, paste0(temp_data_dir, cci_imd_df_name))

#### Read and save outcome data from Outcome List 1 for each age group - White British, Index date: 2015-01-01, Follow-up: 10 years ####
outcome_df_age40_even_name = paste0("ccu022_01_WhiteBritish_Age40_49_EvenYOB_Index2015_AllOutcomeData_by_PL_", file_date)
outcome_df_age40_odd_name = paste0('ccu022_01_WhiteBritish_Age40_49_OddYOB_Index2015_AllOutcomeData_by_PL_', file_date)
outcome_df_age50_even_name = paste0("ccu022_01_WhiteBritish_Age50_59_EvenYOB_Index2015_AllOutcomeData_by_PL_", file_date)
outcome_df_age50_odd_name = paste0('ccu022_01_WhiteBritish_Age50_59_OddYOB_Index2015_AllOutcomeData_by_PL_', file_date)
outcome_df_age60_even_name = paste0("ccu022_01_WhiteBritish_Age60_69_EvenYOB_Index2015_AllOutcomeData_by_PL_", file_date)
outcome_df_age60_odd_name = paste0('ccu022_01_WhiteBritish_Age60_69_OddYOB_Index2015_AllOutcomeData_by_PL_', file_date)


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

#### Read and save outcome data from Outcome List 2 - White British, Index date: 2015-01-01, Follow-up: 10 years ####
outcome_df2_even_name = paste0("ccu022_01_WhiteBritish_Age40_69_EvenYOB_Index2015_AllOutcomeData_OutcomeList2_by_PL_", file_date)
outcome_df2_odd_name = paste0('ccu022_01_WhiteBritish_Age40_69_OddYOB_Index2015_AllOutcomeData_OutcomeList2_by_PL_', file_date)

outcome_df2_even <- dbGetQuery(con,
                                    paste0('select * from ', db_collab, outcome_df2_even_name))
outcome_df2_odd <-dbGetQuery(con,
                                  paste0('select * from ', db_collab, outcome_df2_odd_name))

saveRDS(outcome_df2_even, paste0(temp_data_dir, outcome_df2_even_name))
saveRDS(outcome_df2_odd, paste0(temp_data_dir, outcome_df2_odd_name))

rm(outcome_df2_even, outcome_df2_odd)
gc()

#### Read and save list of predictors and outcomes (Outcome List 1 and List 2) ####
pred_ds_list_name = paste0("ccu022_01_Predictor_Diseases_Pheparent_list_by_PL_", file_date)
outcome_ds_list_name = paste0("ccu022_01_Outcome_Diseases_Phecode_list_by_PL_", file_date)
outcome_ds_list2_name = paste0("ccu022_01_Outcome_Diseases_Phecode_list2_TopCommonPlusPheRS_by_PL_", file_date)


pred_ds_list <- dbGetQuery(con,
                          paste0('select * from ', db_collab, pred_ds_list_name))
outcome_ds_list <- dbGetQuery(con,
                          paste0('select * from ', db_collab, outcome_ds_list_name))
outcome_ds_list2 <- dbGetQuery(con,
                              paste0('select * from ', db_collab, outcome_ds_list2_name))


saveRDS(pred_ds_list, paste0(temp_data_dir, pred_ds_list_name))
saveRDS(outcome_ds_list, paste0(temp_data_dir, outcome_ds_list_name))
saveRDS(outcome_ds_list2, paste0(temp_data_dir, outcome_ds_list2_name))



#### Read and save list of all parent phecodes with prevalence ####
pheparent_list_name = paste0("ccu022_01_All_PheParent_codelist_with_prevalence_by_PL_", file_date)


pheparent_list <- dbGetQuery(con,
                           paste0('select * from ', db_collab, pheparent_list_name))

saveRDS(pheparent_list, paste0(temp_data_dir, pheparent_list_name))


#### Read and save map between ICD10 codes and phecodes ####
phe_icd10_codelist_name = paste0("ccu022_01_PheCode_ICD10AtoN_map_by_PL_", file_date)

icd10_phecode_map <- dbGetQuery(con,
                                paste0('select * from ', db_collab, phe_icd10_codelist_name))

saveRDS(icd10_phecode_map, paste0(temp_data_dir, phe_icd10_codelist_name))


#### PheRS-related ####
##### Read and save PheRS predictor data for each age group - White British, Index date: 2015-01-01 #####
phers_pred_df_age40_even_name = paste0("ccu022_01_WhiteBritish_Age40_49_EvenYOB_Index2015_PheRSPredictorData_by_PL_", file_date)
phers_pred_df_age40_odd_name = paste0('ccu022_01_WhiteBritish_Age40_49_OddYOB_Index2015_PheRSPredictorData_by_PL_', file_date)
phers_pred_df_age50_even_name = paste0("ccu022_01_WhiteBritish_Age50_59_EvenYOB_Index2015_PheRSPredictorData_by_PL_", file_date)
phers_pred_df_age50_odd_name = paste0('ccu022_01_WhiteBritish_Age50_59_OddYOB_Index2015_PheRSPredictorData_by_PL_', file_date)
phers_pred_df_age60_even_name = paste0("ccu022_01_WhiteBritish_Age60_69_EvenYOB_Index2015_PheRSPredictorData_by_PL_", file_date)
phers_pred_df_age60_odd_name = paste0('ccu022_01_WhiteBritish_Age60_69_OddYOB_Index2015_PheRSPredictorData_by_PL_', file_date)

phers_pred_df_age40_even <- dbGetQuery(con,
                                 paste0('select * from ', db_collab, phers_pred_df_age40_even_name))
phers_pred_df_age40_odd <-dbGetQuery(con,
                               paste0('select * from ', db_collab, phers_pred_df_age40_odd_name))
phers_pred_df_age50_even <- dbGetQuery(con,
                                 paste0('select * from ', db_collab, phers_pred_df_age50_even_name))
phers_pred_df_age50_odd <- dbGetQuery(con,
                                paste0('select * from ', db_collab, phers_pred_df_age50_odd_name))
phers_pred_df_age60_even <- dbGetQuery(con,
                                 paste0('select * from ', db_collab, phers_pred_df_age60_even_name))
phers_pred_df_age60_odd <- dbGetQuery(con,
                                paste0('select * from ', db_collab, phers_pred_df_age60_odd_name))

saveRDS(phers_pred_df_age40_even, paste0(temp_data_dir, phers_pred_df_age40_even_name))
saveRDS(phers_pred_df_age40_odd, paste0(temp_data_dir, phers_pred_df_age40_odd_name))
saveRDS(phers_pred_df_age50_even, paste0(temp_data_dir, phers_pred_df_age50_even_name))
saveRDS(phers_pred_df_age50_odd, paste0(temp_data_dir, phers_pred_df_age50_odd_name))
saveRDS(phers_pred_df_age60_even, paste0(temp_data_dir, phers_pred_df_age60_even_name))
saveRDS(phers_pred_df_age60_odd, paste0(temp_data_dir, phers_pred_df_age60_odd_name))

rm(phers_pred_df_age40_even, phers_pred_df_age40_odd, 
   phers_pred_df_age50_even, phers_pred_df_age50_odd, 
   phers_pred_df_age60_even, phers_pred_df_age60_odd)
gc()


##### Read and save list of PheRS predictors #####
phers_pred_ds_list_name = paste0("ccu022_01_PheRS_Predictor_Diseases_Pheparent_list_by_PL_", file_date)

phers_pred_ds_list <- dbGetQuery(con,
                           paste0('select * from ', db_collab, phers_pred_ds_list_name))

saveRDS(phers_pred_ds_list, paste0(temp_data_dir, phers_pred_ds_list_name))


#### Delphi comparison ####
##### Read and save predictor data for each age group - White British, Index date: 2021-07-01 #####
pred_df_age50_even_name = paste0("ccu022_01_WhiteBritish_Age50_59_EvenYOB_IndexMid2020_PredictorData_by_PL_", file_date)
pred_df_age50_odd_name = paste0('ccu022_01_WhiteBritish_Age50_59_OddYOB_IndexMid2020_PredictorData_by_PL_', file_date)
pred_df_age60_even_name = paste0("ccu022_01_WhiteBritish_Age60_69_EvenYOB_IndexMid2020_PredictorData_by_PL_", file_date)
pred_df_age60_odd_name = paste0('ccu022_01_WhiteBritish_Age60_69_OddYOB_IndexMid2020_PredictorData_by_PL_', file_date)
pred_df_age70_even_name = paste0("ccu022_01_WhiteBritish_Age70_80_EvenYOB_IndexMid2020_PredictorData_by_PL_", file_date)
pred_df_age70_odd_name = paste0('ccu022_01_WhiteBritish_Age70_80_OddYOB_IndexMid2020_PredictorData_by_PL_', file_date)


pred_df_age50_even <- dbGetQuery(con,
                                 paste0('select * from ', db_collab, pred_df_age50_even_name))
pred_df_age50_odd <- dbGetQuery(con,
                                paste0('select * from ', db_collab, pred_df_age50_odd_name))
pred_df_age60_even <- dbGetQuery(con,
                                 paste0('select * from ', db_collab, pred_df_age60_even_name))
pred_df_age60_odd <- dbGetQuery(con,
                                paste0('select * from ', db_collab, pred_df_age60_odd_name))
pred_df_age70_even <- dbGetQuery(con,
                                 paste0('select * from ', db_collab, pred_df_age70_even_name))
pred_df_age70_odd <-dbGetQuery(con,
                               paste0('select * from ', db_collab, pred_df_age70_odd_name))



saveRDS(pred_df_age50_even, paste0(temp_data_dir, pred_df_age50_even_name))
saveRDS(pred_df_age50_odd, paste0(temp_data_dir, pred_df_age50_odd_name))
saveRDS(pred_df_age60_even, paste0(temp_data_dir, pred_df_age60_even_name))
saveRDS(pred_df_age60_odd, paste0(temp_data_dir, pred_df_age60_odd_name))
saveRDS(pred_df_age70_even, paste0(temp_data_dir, pred_df_age70_even_name))
saveRDS(pred_df_age70_odd, paste0(temp_data_dir, pred_df_age70_odd_name))

rm( 
   pred_df_age50_even, pred_df_age50_odd, 
   pred_df_age60_even, pred_df_age60_odd,
   pred_df_age70_even, pred_df_age70_odd)
gc()



##### Read and save outcome data from Outcome List 1 for each age group - White British, Index date: 2021-07-01, Follow-up: 1 year #####
outcome_df_age50_even_name = paste0("ccu022_01_WhiteBritish_Age50_59_EvenYOB_IndexMid2020_AllOutcomeData_by_PL_", file_date)
outcome_df_age50_odd_name = paste0('ccu022_01_WhiteBritish_Age50_59_OddYOB_IndexMid2020_AllOutcomeData_by_PL_', file_date)
outcome_df_age60_even_name = paste0("ccu022_01_WhiteBritish_Age60_69_EvenYOB_IndexMid2020_AllOutcomeData_by_PL_", file_date)
outcome_df_age60_odd_name = paste0('ccu022_01_WhiteBritish_Age60_69_OddYOB_IndexMid2020_AllOutcomeData_by_PL_', file_date)
outcome_df_age70_even_name = paste0("ccu022_01_WhiteBritish_Age70_80_EvenYOB_IndexMid2020_AllOutcomeData_by_PL_", file_date)
outcome_df_age70_odd_name = paste0('ccu022_01_WhiteBritish_Age70_80_OddYOB_IndexMid2020_AllOutcomeData_by_PL_', file_date)


outcome_df_age50_even <- dbGetQuery(con,
                                    paste0('select * from ', db_collab, outcome_df_age50_even_name))
outcome_df_age50_odd <- dbGetQuery(con,
                                   paste0('select * from ', db_collab, outcome_df_age50_odd_name))
outcome_df_age60_even <- dbGetQuery(con,
                                    paste0('select * from ', db_collab, outcome_df_age60_even_name))
outcome_df_age60_odd <- dbGetQuery(con,
                                   paste0('select * from ', db_collab, outcome_df_age60_odd_name))
outcome_df_age70_even <- dbGetQuery(con,
                                    paste0('select * from ', db_collab, outcome_df_age70_even_name))
outcome_df_age70_odd <-dbGetQuery(con,
                                  paste0('select * from ', db_collab, outcome_df_age70_odd_name))


saveRDS(outcome_df_age50_even, paste0(temp_data_dir, outcome_df_age50_even_name))
saveRDS(outcome_df_age50_odd, paste0(temp_data_dir, outcome_df_age50_odd_name))
saveRDS(outcome_df_age60_even, paste0(temp_data_dir, outcome_df_age60_even_name))
saveRDS(outcome_df_age60_odd, paste0(temp_data_dir, outcome_df_age60_odd_name))
saveRDS(outcome_df_age70_even, paste0(temp_data_dir, outcome_df_age70_even_name))
saveRDS(outcome_df_age70_odd, paste0(temp_data_dir, outcome_df_age70_odd_name))


rm( 
   outcome_df_age50_even, outcome_df_age50_odd, 
   outcome_df_age60_even, outcome_df_age60_odd,
   outcome_df_age70_even, outcome_df_age70_odd)
gc()

##### Read and save outcome data from Outcome List 2 - White British, Index date: 2021-07-01, Follow-up: 1 year #####
outcome_df2_even_name = paste0("ccu022_01_WhiteBritish_Age50_80_EvenYOB_IndexMid2020_AllOutcomeData_OutcomeList2_by_PL_", file_date)
outcome_df2_odd_name = paste0('ccu022_01_WhiteBritish_Age50_80_OddYOB_IndexMid2020_AllOutcomeData_OutcomeList2_by_PL_', file_date)

outcome_df2_even <- dbGetQuery(con,
                               paste0('select * from ', db_collab, outcome_df2_even_name))
outcome_df2_odd <-dbGetQuery(con,
                             paste0('select * from ', db_collab, outcome_df2_odd_name))

saveRDS(outcome_df2_even, paste0(temp_data_dir, outcome_df2_even_name))
saveRDS(outcome_df2_odd, paste0(temp_data_dir, outcome_df2_odd_name))

rm(outcome_df2_even, outcome_df2_odd)
gc()


