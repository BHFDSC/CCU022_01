# Databricks notebook source
# MAGIC %md
# MAGIC ### Preparation of data for analysis - Index date: 2020-01-01, 5-year follow-up - preparation of predictors and outcomes
# MAGIC ##### Pearl Liu
# MAGIC
# MAGIC ##### v1.1
# MAGIC Limit to Black, Black British, Caribbean or African population.

# COMMAND ----------

# MAGIC %md
# MAGIC #### Load modules and functions

# COMMAND ----------

import pyspark
from pyspark.sql import SparkSession, Window, Column, DataFrame
from pyspark.sql.functions import year, lit, least, to_date
import pandas as pd
import pyspark.sql.functions as F

file_date = "_by_PL_2025_05_01"  # latest file date

# COMMAND ----------

# MAGIC %run "./parameters"

# COMMAND ----------

# MAGIC %md
# MAGIC #### Load demographic data for main analytical sample (Black pts aged 40-69 yrs on 2020-01-01)

# COMMAND ----------

## Read info on main analytical sample (Black pts aged 40-69 yrs on 2020-01-01)
db_collab = ""
analysis_df_name = "ccu022_01_Black_Age40_69_Index2020_StudyPopInfo"
analysis_df = spark.read.table(db_collab + '.' + analysis_df_name + file_date)

# COMMAND ----------

n_total = analysis_df.select('PID').distinct().count()  
n_total 

# COMMAND ----------

# MAGIC %md
# MAGIC #### Load final study population Pt ID

# COMMAND ----------

db_collab = ""
study_pop_2020_name = "ccu022_01_Black_Age40_69_Index2020_StudyPopPID"
study_pop_2020 = spark.read.table(db_collab + '.' + study_pop_2020_name + file_date)

study_pop_2020.count()

# COMMAND ----------

# MAGIC %md
# MAGIC ### Prepare data for predictors

# COMMAND ----------

# MAGIC %md
# MAGIC ##### Subset to final study population

# COMMAND ----------

## Read diagnosis data for predictor diseases (see Cell 19 from "Preparation of analysis data - Index 2020 - study population")
db_collab = ""
diag_target_pop_pred_ds1_name = "ccu022_01_predictor_diseases_diagnosis_age40to69in2020"
pred_ds_df0 = spark.read.table(db_collab + '.' + diag_target_pop_pred_ds1_name + file_date)

# COMMAND ----------

## Limit to study population of interest
pred_ds_df1 = pred_ds_df0.join(analysis_df.select('PID'), on = 'PID', how='inner')

pred_ds_df1.select('PID').distinct().count() 

# COMMAND ----------

# MAGIC %md
# MAGIC ###### Sanity check for diagnosis dates

# COMMAND ----------

## Check range of DATE: should be before (2020-01-01 - 3 months)
pred_ds_df0.select(F.min('DATE'), F.max('DATE')).display()

# COMMAND ----------

# Check any records with diagnosis dates < birth date
pred_ds_df1.filter(F.col('DATE') < F.col('date_of_birth')).count()

# COMMAND ----------

# MAGIC %md
# MAGIC ###### Save filtered predictor diagnosis data

# COMMAND ----------

## Save filtered predictor diagnosis data
db_collab = ""
pred_ds_df1_name = "ccu022_01_Black_Age40_69_Index2020_PredictorDiseases_long"
pred_ds_df1.write.option("overwriteSchema", "true").saveAsTable(name=(db_collab + '.' + pred_ds_df1_name + file_date), mode="overwrite")

# COMMAND ----------

# MAGIC %md
# MAGIC ##### Transform diagnosis data from long to wide format

# COMMAND ----------

## Read saved data
pred_ds_df1_name = "ccu022_01_Black_Age40_69_Index2020_PredictorDiseases_long"
pred_ds_df_long = spark.read.table(db_collab + '.' + pred_ds_df1_name + file_date)

# COMMAND ----------

## Transform from long to wide format
pred_ds_df_wide = pred_ds_df_long.groupBy('PID').pivot('PheCode_parent').count().fillna(0)
display(pred_ds_df_wide)

# COMMAND ----------

## Sanity check: number of columns = total number of predictor ds + 1
len(pred_ds_df_wide.columns)  

# COMMAND ----------

## all cells should have count <= 1
pred_ds_only = pred_ds_df_wide.drop('PID')
pred_ds_df_wide.withColumn('max_count', F.greatest(*pred_ds_only.columns)).select(F.max('max_count')).display()

# COMMAND ----------

## all pts should have >= 1 diagnosis
pred_ds_only = pred_ds_df_wide.drop('PID')
pred_ds_df_wide.withColumn('rowsum', sum(pred_ds_only[col] for col in pred_ds_only.columns)).select(F.min('rowsum')).display()  # 1

# COMMAND ----------

# MAGIC %md
# MAGIC ##### Join with demo data

# COMMAND ----------

## Join with demo data
pred_df_final = pred_ds_df_wide.join(analysis_df.select('PID', 'sex_code', 'age_at_2020', 'even_birth_year', 'date_of_birth'), on='PID', how='left').withColumn('Female', F.col('sex_code') - 1).drop('sex_code').orderBy('PID')

display(pred_df_final)

# COMMAND ----------

## Check distribution of sex
pred_df_final.groupBy('Female').count().orderBy('count', ascending=False).display()

# COMMAND ----------

## Check that there is no NA values
pred_df_final.select([F.count(F.when(F.col(c).isNull(), c)).alias(c) for c in pred_df_final.columns]).display()

# COMMAND ----------

# MAGIC %md
# MAGIC ##### Add COVID status

# COMMAND ----------

## Read PID with confirmed COVID cases <= 2024-12-31
db_collab = ""
covid_curated_all_PID_name = "ccu022_01_PID_with_COVID_anytime_by_2024_12_31_age40to69in2020"
covid_curated_all_PID = spark.read.table(db_collab + '.' + covid_curated_all_PID_name + file_date) 

# COMMAND ----------

## Join with predictor data
covid_curated_all = covid_curated_all_PID.withColumn("Any_COVID", F.lit(1)).withColumnRenamed("person_id", "PID")
pred_df_final_covid = pred_df_final.join(covid_curated_all, on = "PID", how = "left").fillna(0)
pred_df_final_covid.display()

# COMMAND ----------

## Check the number of pts with COVID anytime <= 2024-12-31
pred_df_final_covid.filter(F.col("Any_COVID") == 1).count()

# COMMAND ----------

# MAGIC %md
# MAGIC ##### Save combined predictor disease and covariates data

# COMMAND ----------

## list of predictors
n_pred = 212
pred_ds_list = pred_df_final_covid.columns[1:(n_pred+1)]

## Convert ds columns to byte type to save space
pred_df_final_covid2 = pred_df_final_covid.select('PID', *(F.col(c).cast("byte").alias(c) for c in pred_ds_list), 'age_at_2020', 'even_birth_year', 'date_of_birth', 'Female', 'Any_COVID')

# COMMAND ----------

## Save combined predictor ds and covariates data
db_collab = ""
pred_df_final_name = "ccu022_01_Black_Age40_69_Index2020_PredictorData"
pred_df_final_covid2.write.option("overwriteSchema", "true").saveAsTable(name=(db_collab + '.' + pred_df_final_name + file_date), mode="overwrite")

# COMMAND ----------

# MAGIC %md
# MAGIC ### Prepare data for outcomes

# COMMAND ----------

# MAGIC %md
# MAGIC ##### Read COVID hospitalization data 

# COMMAND ----------

## Read the COVID hospitalization data - first onset per pt
db_collab = ""
first_onset_covid_curated_hspt_name = "ccu022_01_first_onset_covid_hospitalization_by_2024_12_31_age40to69in2020"
first_onset_covid_curated_hspt = spark.read.table(db_collab + '.' + first_onset_covid_curated_hspt_name + file_date).withColumnRenamed("person_id", "PID")

# COMMAND ----------

# MAGIC %md
# MAGIC ##### Subset to final study population

# COMMAND ----------

## Read study population PID and outcome diagnosis data
db_collab = ""

study_pop_2020_name = "ccu022_01_Black_Age40_69_Index2020_StudyPopPID"
study_pop_2020 = spark.read.table(db_collab + '.' + study_pop_2020_name + file_date)

## See Cell 60 of "Preparation of analysis data - Index 2020 - 5-year followup - predictors and outcomes" 
diag_combine_target_name = "ccu022_01_first_onset_combined_diagnosis_age40to69in2020_PheCode_Cleaned_NHSID_2024_12_31"
target_ds_df0 = spark.read.table(db_collab + '.' + diag_combine_target_name + file_date)

# COMMAND ----------

## Inner join with the final study population
target_ds_df1 = target_ds_df0.join(study_pop_2020, on = 'PID', how='inner')

target_ds_df1.select('PID').distinct().count()

# COMMAND ----------

# MAGIC %md
# MAGIC ##### Subset to outcome diseases

# COMMAND ----------

## Read list of outcome phecodes
db_collab = ""
outcome_ds_final_name = "ccu022_01_outcome_disease_PheCode_Primary50_FollowupCount150000"
outcome_ds_final = spark.read.table(db_collab + '.' + outcome_ds_final_name + file_date)

outcome_ds_final.count()

# COMMAND ----------

## Filter diag data to outcome diseases
target_ds_df2 = target_ds_df1.join(outcome_ds_final.select('PheCode'), on = 'PheCode', how='inner')

# COMMAND ----------

# MAGIC %md
# MAGIC ##### Record the outcome diagnosis status pre-index

# COMMAND ----------

## Limit to diagnosis dates before 2020-01-01
target_ds_pre_idx_long = target_ds_df2.filter(F.col('DATE') < '2020-01-01')

## Transform from long to wide format
target_ds_pre_idx_wide = target_ds_pre_idx_long.groupBy('PID').pivot('PheCode').count().fillna(0)
display(target_ds_pre_idx_wide)

# COMMAND ----------

# MAGIC %md
# MAGIC ###### Get the list of outcomes

# COMMAND ----------

## Replace '.' with '_' in outcome ds column names
target_ds_list = target_ds_pre_idx_wide.columns[1:]
target_ds_list = [str.replace('.', '_') for str in target_ds_list] 
target_ds_pre_idx_wide2 = target_ds_pre_idx_wide.toDF(*(['PID'] + target_ds_list))

target_ds_list

# COMMAND ----------

## Check that number of columns = total number of outcome ds + 1
len(target_ds_pre_idx_wide2.columns)  

# COMMAND ----------

## Sanity check: all cells should have count <= 1
target_ds_pre_idx_wide2.withColumn('max_count', F.greatest(*target_ds_list)).select(F.max('max_count')).display()

# COMMAND ----------

# MAGIC %md
# MAGIC ###### Join with full final study population

# COMMAND ----------

## Join with final study population, fill NA values with 0 for pts without prior outcome diagnosis
target_ds_pre_idx_wide3 = target_ds_pre_idx_wide2.join(study_pop_2020, on = 'PID', how = 'right').fillna(0)

# COMMAND ----------

# MAGIC %md
# MAGIC ###### Add pre-index COVID hospitalization status

# COMMAND ----------

## Limit COVID status to before 2020-01-01
covid_hspt_pre_idx = first_onset_covid_curated_hspt.filter(F.col('date') < '2020-01-01').withColumn("COVID_hspt", F.lit(1))

## Join with other outcomes, fill NA values with 0
target_ds_pre_idx_wide3_covid = target_ds_pre_idx_wide3.join(covid_hspt_pre_idx.select('PID', 'COVID_hspt'), on = "PID", how = "left").fillna(0)

# COMMAND ----------

## Sanity check
target_ds_pre_idx_wide3_covid.filter(F.col('COVID_hspt') == 1).count()

# COMMAND ----------

# MAGIC %md
# MAGIC ###### Save pre-index outcome status - used for deciding analysis population for each outcome (pts with pre-index outcome diagnosis will be removed)

# COMMAND ----------

target_ds_pre_idx_wide3_covid.count()

# COMMAND ----------

## Save the temp data
db_collab = ""
target_ds_pre_idx_wide3_name = "ccu022_01_Black_Age40_69_Index2020_PreIndexOutcomeData"
target_ds_pre_idx_wide3_covid.write.option("overwriteSchema", "true").saveAsTable(name=(db_collab + '.' + target_ds_pre_idx_wide3_name + file_date), mode="overwrite")

# COMMAND ----------

# MAGIC %md
# MAGIC ##### Record the outcome diagnosis status during follow-up

# COMMAND ----------

## Read the list of outcome phecodes
db_collab = ""
target_ds_phecode_df_name = "ccu022_01_Outcome_Diseases_Phecode_list"
target_ds_phecode_df = spark.read.table(db_collab + '.' + target_ds_phecode_df_name + file_date)
target_ds_phecode_ref = target_ds_phecode_df.orderBy('index').select('target_ds').toPandas().target_ds.to_list()
target_ds_phecode_ref


# COMMAND ----------

## Limit to diagnosis dates >= 2020-01-01 (and by default <= 2024-12-31)
target_ds_fup_long = target_ds_df2.filter(F.col('DATE') >= '2020-01-01').filter(F.col('DATE') <= '2024-12-31')

## Transform from long to wide format
target_ds_fup_wide = target_ds_fup_long.groupBy('PID').pivot('PheCode').count()
display(target_ds_fup_wide)

# COMMAND ----------

## Replace '.' with '_' in outcome ds column names
target_ds_list = target_ds_fup_wide.columns[1:]
target_ds_list = [str.replace('.', '_') for str in target_ds_list] 
target_ds_fup_wide2 = target_ds_fup_wide.toDF(*(['PID'] + target_ds_list))

## Check that the list matches with the reference list
target_ds_list == target_ds_phecode_ref

# COMMAND ----------

## Check that number of columns = total number of outcome ds + 1
len(target_ds_fup_wide2.columns)  

# COMMAND ----------

## Sanity check: all cells should have count <= 1
target_ds_fup_wide2.withColumn('max_count', F.greatest(*target_ds_list)).select(F.max('max_count')).display()

# COMMAND ----------

## Check min and max number of cases
target_ds_n_cases = target_ds_fup_wide2.select([F.sum(c).alias(c) for c in target_ds_list])
target_ds_n_cases.withColumn('min_count', F.least(*target_ds_list)).withColumn('max_count', F.greatest(*target_ds_list)).select('min_count', 'max_count').display()

# COMMAND ----------

## Join with final study population
target_ds_fup_wide3 = target_ds_fup_wide2.join(study_pop_2020, on = 'PID', how = 'right')

## Get follow-up time and demo info
target_ds_fup_wide4 = target_ds_fup_wide3.join(analysis_df.select('PID', 'fup_days_5yr', 'fup_end_5yr', 'sex_code', 'age_at_2020', 'even_birth_year', 'date_of_birth'), on = 'PID', how = 'left').withColumn('Female', F.col('sex_code') - 1).drop('sex_code')

display(target_ds_fup_wide4)

# COMMAND ----------

# MAGIC %md
# MAGIC ###### Add follow-up COVID hospitalization status

# COMMAND ----------

## Limit COVID status to >= 2020-01-01 (by default <= 2024-12-31)
covid_hspt_fup = first_onset_covid_curated_hspt.filter(F.col('date') >= '2020-01-01').filter(F.col('date') <= '2024-12-31').withColumn("COVID_hspt", F.lit(1))

## Join with other outcomes
target_ds_fup_wide4_covid = target_ds_fup_wide4.join(covid_hspt_fup.select('PID', 'COVID_hspt'), on = "PID", how = "left")

# COMMAND ----------

## Number of COVID hspt during follow-up
target_ds_fup_wide4_covid.filter(F.col('COVID_hspt') == 1).count()

# COMMAND ----------

display(target_ds_fup_wide4_covid)

# COMMAND ----------

# MAGIC %md
# MAGIC ###### Set censored and control outcome status

# COMMAND ----------

## For pts with NA values for outcome diagnosis status, set status as 9 (censored) if the follow-up end is < 2024-12-31, otherwise set status as 0 (controls)
target_ds_fup_wide4_censored = target_ds_fup_wide4_covid.filter(F.col('fup_end_5yr') < '2024-12-31').fillna(9, subset = target_ds_list + ['COVID_hspt'])
target_ds_fup_wide4_uncensored = target_ds_fup_wide4_covid.filter(F.col('fup_end_5yr') == '2024-12-31').fillna(0, subset = target_ds_list + ['COVID_hspt'])

target_ds_fup_wide5 = target_ds_fup_wide4_censored.union(target_ds_fup_wide4_uncensored)
target_ds_fup_wide5.count()

# COMMAND ----------

## Check number of censored pts
target_ds_fup_wide4_censored.count() 

# COMMAND ----------

target_ds_fup_wide5.display()

# COMMAND ----------

# MAGIC %md
# MAGIC ###### Save outcome status during follow-up

# COMMAND ----------

## Save the follow-up outcome status data
db_collab = ""
target_ds_fup_wide5_name = "ccu022_01_Black_Age40_69_Index2020_FollowupOutcomeStatus"
target_ds_fup_wide5.write.option("overwriteSchema", "true").saveAsTable(name=(db_collab + '.' + target_ds_fup_wide5_name + file_date), mode="overwrite")

# COMMAND ----------

# MAGIC %md
# MAGIC ##### Record the time to outcome diagnosis 

# COMMAND ----------

## Read the list of outcome phecodes
db_collab = ""
target_ds_phecode_df_name = "ccu022_01_Outcome_Diseases_Phecode_list"
target_ds_phecode_df = spark.read.table(db_collab + '.' + target_ds_phecode_df_name + file_date)
target_ds_phecode_ref = target_ds_phecode_df.orderBy('index').select('target_ds').toPandas().target_ds.to_list()
target_ds_phecode_ref

# COMMAND ----------

## Limit to diagnosis dates >= 2020-01-01 (and by default <= 2024-12-31)
target_ds_fup_long = target_ds_df2.filter(F.col('DATE') >= '2020-01-01').filter(F.col('DATE') <= '2024-12-31')

## Create a new column for time to event
target_ds_fup_long2 = target_ds_fup_long.withColumn('time_to_diag', F.datediff(F.col('DATE'), F.lit('2020-01-01')) + 1)

target_ds_fup_long2.select(F.min('time_to_diag'), F.max('time_to_diag')).display()

# COMMAND ----------

## Transform time-to-event from long to wide format
target_ds_fup_tte_wide = target_ds_fup_long2.groupBy('PID').pivot('PheCode').agg(F.first('time_to_diag'))
display(target_ds_fup_tte_wide)

# COMMAND ----------

## Replace '.' with '_' in outcome ds column names
target_ds_list = target_ds_fup_tte_wide.columns[1:]
target_ds_list = [str.replace('.', '_') for str in target_ds_list] 
target_ds_fup_tte_wide2 = target_ds_fup_tte_wide.toDF(*(['PID'] + target_ds_list))
target_ds_list == target_ds_phecode_ref

# COMMAND ----------

## Check that number of columns = total number of outcome ds + 1
len(target_ds_fup_tte_wide2.columns)  

# COMMAND ----------

## Sanity check: all cells should have tte <= 1827
target_ds_fup_tte_wide2.withColumn('max_count', F.greatest(*target_ds_list)).select(F.max('max_count')).display()

# COMMAND ----------

## Join with final study population
target_ds_fup_tte_wide3 = target_ds_fup_tte_wide2.join(study_pop_2020, on = 'PID', how = 'right')

## Get follow-up time and demo info
target_ds_fup_tte_wide4 = target_ds_fup_tte_wide3.join(analysis_df.select('PID', 'fup_days_5yr', 'fup_end_5yr', 'sex_code', 'age_at_2020', 'even_birth_year', 'date_of_birth'), on = 'PID', how = 'left').withColumn('Female', F.col('sex_code') - 1).drop('sex_code')

display(target_ds_fup_tte_wide4)

# COMMAND ----------

# MAGIC %md
# MAGIC ###### Add time to COVID hospitalization

# COMMAND ----------

## Limit COVID status to >= 2020-01-01 (by default <= 2024-12-31), add a new column for time to event
covid_hspt_tte = first_onset_covid_curated_hspt.filter(F.col('date') >= '2020-01-01').filter(F.col('date') <= '2024-12-31').withColumn("COVID_hspt", F.datediff(F.col('date'), F.lit('2020-01-01')) + 1)

## Join with other outcomes
target_ds_fup_tte_wide4_covid = target_ds_fup_tte_wide4.join(covid_hspt_tte.select('PID','COVID_hspt'), on = "PID", how = "left")

# COMMAND ----------

## Sanity check
target_ds_fup_tte_wide4_covid.select(F.min('COVID_hspt'), F.max('COVID_hspt')).display()

# COMMAND ----------

# MAGIC %md
# MAGIC ###### Set time-to-event for censored and control pts

# COMMAND ----------

## For pts with NA values for tte, set tte as follow-up time
target_ds_fup_tte_wide5 = target_ds_fup_tte_wide4_covid
for target_ds in (target_ds_list + ["COVID_hspt"]):
    target_ds_fup_tte_wide5 = target_ds_fup_tte_wide5.withColumn(target_ds, F.when(F.col(target_ds).isNull(), F.col('fup_days_5yr')).otherwise(F.col(target_ds)))

display(target_ds_fup_tte_wide5)

# COMMAND ----------

# MAGIC %md
# MAGIC ###### Save time to outcome diagnosis data during follow-up

# COMMAND ----------

target_ds_fup_tte_final = target_ds_fup_tte_wide5.drop('fup_days_5yr', 'fup_end_5yr').orderBy('PID')

target_ds_fup_tte_final.count()

# COMMAND ----------

## Save the temp data
db_collab = ""
target_ds_fup_tte_final_name = "ccu022_01_Black_Age40_69_Index2020_FollowupTimeToOutcome"
target_ds_fup_tte_final.write.option("overwriteSchema", "true").saveAsTable(name=(db_collab + '.' + target_ds_fup_tte_final_name + file_date), mode="overwrite")

# COMMAND ----------

# MAGIC %md
# MAGIC ##### Save combined outcome data

# COMMAND ----------

# MAGIC %md
# MAGIC ###### Read all outcome-related data

# COMMAND ----------

## total number of non-COVID outcomes
n_outcomes = 73

# COMMAND ----------

## Read pre-index outcome status
db_collab = ""
target_ds_pre_idx_name = "ccu022_01_Black_Age40_69_Index2020_PreIndexOutcomeData"
target_ds_pre_idx0 = spark.read.table(db_collab + '.' + target_ds_pre_idx_name + file_date)

## Rename outcome columns
target_ds_list = target_ds_pre_idx0.columns[1:(n_outcomes+1)] + ["COVID_hspt"]
target_ds_list_new = [str + '_preidx' for str in target_ds_list]
target_ds_pre_idx = target_ds_pre_idx0.toDF(*(['PID'] + target_ds_list_new))

## Convert ds columns to byte type to save space
target_ds_pre_idx_final = target_ds_pre_idx.select('PID', *(F.col(c).cast("byte").alias(c) for c in target_ds_list_new))

# COMMAND ----------

## Read outcome status during follow-up
target_ds_fup_name = "ccu022_01_Black_Age40_69_Index2020_FollowupOutcomeStatus"
target_ds_fup0 = spark.read.table(db_collab + '.' + target_ds_fup_name + file_date)

## Rename outcome columns
target_ds_list = target_ds_fup0.columns[1:(n_outcomes+1)] + ["COVID_hspt"]
target_ds_fup0_dsonly = target_ds_fup0.select('PID', *target_ds_list)

target_ds_list_new = [str + '_fup' for str in target_ds_list]
target_ds_fup = target_ds_fup0_dsonly.toDF(*(['PID'] + target_ds_list_new))

## Convert ds columns to byte type to save space
target_ds_fup_final = target_ds_fup.select('PID', *(F.col(c).cast("byte").alias(c) for c in target_ds_list_new))

# COMMAND ----------

## Read time to outcome during follow-up
target_ds_fup_tte_name = "ccu022_01_Black_Age40_69_Index2020_FollowupTimeToOutcome"
target_ds_fup_tte0 = spark.read.table(db_collab + '.' + target_ds_fup_tte_name + file_date)

## Rename time to event columns
target_ds_list = target_ds_fup_tte0.columns[1:(n_outcomes+1)] + ["COVID_hspt"]
target_ds_list_new = [str + '_tte' for str in target_ds_list]
target_ds_fup_tte = target_ds_fup_tte0.select('PID', *target_ds_list, 'age_at_2020', 'even_birth_year', 'date_of_birth', 'Female').toDF(*(['PID'] + target_ds_list_new + ['age_at_2020', 'even_birth_year', 'date_of_birth', 'Female']))

## Convert ds columns to short type to save space
target_ds_fup_tte_final = target_ds_fup_tte.select('PID', *(F.col(c).cast("short").alias(c) for c in target_ds_list_new), 'age_at_2020', 'even_birth_year', 'date_of_birth', 'Female')

# COMMAND ----------

## Joint all datasets
outcome_df_final = target_ds_pre_idx_final.join(target_ds_fup_final, on = 'PID', how = 'left').join(target_ds_fup_tte_final, on = 'PID', how = 'left')

outcome_df_final.count()

# COMMAND ----------

outcome_df_final.display()

# COMMAND ----------

# MAGIC %md
# MAGIC ###### Save the combined outcome data

# COMMAND ----------

## Save the combined dataset
db_collab = ""
outcome_df_final_name = "ccu022_01_Black_Age40_69_Index2020_AllOutcomeData"
outcome_df_final.write.option("overwriteSchema", "true").saveAsTable(name=(db_collab + '.' + outcome_df_final_name + file_date), mode="overwrite")