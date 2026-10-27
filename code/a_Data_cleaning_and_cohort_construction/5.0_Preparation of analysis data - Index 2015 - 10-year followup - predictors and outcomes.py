# Databricks notebook source
# MAGIC %md
# MAGIC ### Preparation of data for analysis - Index date: 2015-01-01, 10-year follow-up - preparation of predictors and outcomes
# MAGIC ##### Pearl Liu
# MAGIC
# MAGIC ##### v1.1

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

# MAGIC %md
# MAGIC #### Load demographic data for main analytical sample (White British aged 40-69 yrs on 2015-01-01)

# COMMAND ----------

## Read info on main analytical sample (White British aged 40-69 yrs on 2015-01-01)
db_collab = ""
analysis_df_name = "ccu022_01_WhiteBritish_Age40_69_Index2015_StudyPopInfo"
analysis_df = spark.read.table(db_collab + '.' + analysis_df_name + file_date)

# COMMAND ----------

n_total = analysis_df.select('PID').distinct().count()  
n_total 

# COMMAND ----------

# MAGIC %md
# MAGIC #### Load final study population Pt ID

# COMMAND ----------

db_collab = ""
study_pop_2015_name = "ccu022_01_WhiteBritish_Age40_69_Index2015_StudyPopPID"
study_pop_2015 = spark.read.table(db_collab + '.' + study_pop_2015_name + file_date)

study_pop_2015.count()

# COMMAND ----------

# MAGIC %md
# MAGIC ### Prepare data for predictors

# COMMAND ----------

# MAGIC %md
# MAGIC ##### Subset to final study population

# COMMAND ----------

## Read diagnosis data for predictor diseases (see Cell 19 from "Preparation of analysis data - Index 2015 - study population")
db_collab = ""
diag_target_pop_pred_ds1_name = "ccu022_01_predictor_diseases_diagnosis_age40to69in2015"
pred_ds_df0 = spark.read.table(db_collab + '.' + diag_target_pop_pred_ds1_name + file_date)

# COMMAND ----------

## Limit to study population of interest
pred_ds_df1 = pred_ds_df0.join(analysis_df.select('PID'), on = 'PID', how='inner')

pred_ds_df1.select('PID').distinct().count() 

# COMMAND ----------

# MAGIC %md
# MAGIC ###### Sanity check for diagnosis dates

# COMMAND ----------

## Check range of DATE: should be before (2015-01-01 - 3 months)
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
pred_ds_df1_name = "ccu022_01_WhiteBritish_Age40_69_Index2015_PredictorDiseases_long"
pred_ds_df1.write.option("overwriteSchema", "true").saveAsTable(name=(db_collab + '.' + pred_ds_df1_name + file_date), mode="overwrite")

# COMMAND ----------

# MAGIC %md
# MAGIC ##### Transform diagnosis data from long to wide format

# COMMAND ----------

## Read saved data
db_collab = ""
pred_ds_df1_name = "ccu022_01_WhiteBritish_Age40_69_Index2015_PredictorDiseases_long"
pred_ds_df_long = spark.read.table(db_collab + '.' + pred_ds_df1_name + file_date)

# COMMAND ----------

# DBTITLE 1,Calculate the average year of followup from first diagnosis to death or followup end
w = Window.partitionBy("PID").orderBy("DATE")
pred_ds_df_firstdiag = pred_ds_df_long.withColumn("rk", F.row_number().over(w)).filter(F.col("rk") == 1).drop("rk")
pred_ds_df_firstdiag = pred_ds_df_firstdiag.withColumn("fup_end", F.least(F.col("date_of_death"), F.to_date(F.lit("2024-12-31"))))

pred_ds_df_firstdiag.select(F.min('DATE'), F.max('DATE'), F.min("fup_end"), F.max("fup_end")).display()


# COMMAND ----------

pred_ds_df_firstdiag.select(
    F.avg(
        F.months_between(F.col("fup_end"), F.col("DATE")) / 12
    ).alias("avg_fup_years")
).show()

# COMMAND ----------

pred_ds_df_long.count()

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
pred_df_final = pred_ds_df_wide.join(analysis_df.select('PID', 'sex_code', 'age_at_2015', 'even_birth_year', 'date_of_birth'), on='PID', how='left').withColumn('Female', F.col('sex_code') - 1).drop('sex_code').orderBy('PID')

display(pred_df_final)

# COMMAND ----------

## Check distribution of sex
pred_df_final.groupBy('Female').count().orderBy('count', ascending=False).display()

# COMMAND ----------

## Check that there is no NA values
pred_df_final.select([F.count(F.when(F.col(c).isNull(), c)).alias(c) for c in pred_df_final.columns]).display()

# COMMAND ----------

# MAGIC %md
# MAGIC ##### Save combined predictor disease and covariates data

# COMMAND ----------

## Save combined predictor ds and covariates data
db_collab = ""
pred_df_final_name = "ccu022_01_WhiteBritish_Age40_69_Index2015_PredictorData"
pred_df_final.write.option("overwriteSchema", "true").saveAsTable(name=(db_collab + '.' + pred_df_final_name + file_date), mode="overwrite")

# COMMAND ----------

# MAGIC %md
# MAGIC ###### Save list of phecodes for predictor diseases

# COMMAND ----------

## Get list of predictor phecodes
pred_ds_only = pred_ds_df_wide.drop('PID')
pred_ds_phecode = pred_ds_only.columns
pred_ds_phecode

# COMMAND ----------

## Create a dataframe and save the phecode list 
pred_ds_phecode_df = spark.createDataFrame(data=[(cnt+1,a) for cnt,a in enumerate(pred_ds_phecode)], schema = ["index","predictor_ds"])

pred_ds_phecode_df.display()

db_collab = ""
pred_ds_phecode_df_name = "ccu022_01_Predictor_Diseases_Pheparent_list"
pred_ds_phecode_df.write.saveAsTable(name=(db_collab + '.' + pred_ds_phecode_df_name + file_date), mode="overwrite")


# COMMAND ----------

# MAGIC %md
# MAGIC ##### Subset to different age groups and even/odd birth years

# COMMAND ----------

## Read combined predictor ds and covariates data
db_collab = ""
pred_df_final_name = "ccu022_01_WhiteBritish_Age40_69_Index2015_PredictorData"
pred_df_final0 = spark.read.table(db_collab + '.' + pred_df_final_name + file_date)

# COMMAND ----------

## list of predictors
n_pred = 212
pred_ds_list = pred_df_final0.columns[1:(n_pred+1)]

## Convert ds columns to byte type to save space
pred_df_final = pred_df_final0.select('PID', *(F.col(c).cast("byte").alias(c) for c in pred_ds_list), 'age_at_2015', 'even_birth_year', 'date_of_birth', 'Female')

# COMMAND ----------

# MAGIC %md
# MAGIC ###### Age 40-49

# COMMAND ----------

pred_df_age40 = pred_df_final.filter(F.col('age_at_2015') >= 40).filter(F.col('age_at_2015') < 50)
pred_df_age40_evenyob = pred_df_age40.filter(F.col('even_birth_year') == 1).drop('even_birth_year')
pred_df_age40_oddyob = pred_df_age40.filter(F.col('even_birth_year') == 0).drop('even_birth_year')

# COMMAND ----------

pred_df_age40_evenyob.count()

# COMMAND ----------

pred_df_age40_oddyob.count()

# COMMAND ----------

db_collab = ""
pred_df_age40_evenyob_name = "ccu022_01_WhiteBritish_Age40_49_EvenYOB_Index2015_PredictorData"
pred_df_age40_evenyob.write.option("overwriteSchema", "true").saveAsTable(name=(db_collab + '.' + pred_df_age40_evenyob_name + file_date), mode="overwrite")

pred_df_age40_oddyob_name = "ccu022_01_WhiteBritish_Age40_49_OddYOB_Index2015_PredictorData"
pred_df_age40_oddyob.write.option("overwriteSchema", "true").saveAsTable(name=(db_collab + '.' + pred_df_age40_oddyob_name + file_date), mode="overwrite")

# COMMAND ----------

# MAGIC %md
# MAGIC ###### Age 50-59

# COMMAND ----------

pred_df_age50 = pred_df_final.filter(F.col('age_at_2015') >= 50).filter(F.col('age_at_2015') < 60)
pred_df_age50_evenyob = pred_df_age50.filter(F.col('even_birth_year') == 1).drop('even_birth_year')
pred_df_age50_oddyob = pred_df_age50.filter(F.col('even_birth_year') == 0).drop('even_birth_year')

# COMMAND ----------

pred_df_age50_evenyob.count()

# COMMAND ----------

pred_df_age50_oddyob.count()

# COMMAND ----------

db_collab = ""
pred_df_age50_evenyob_name = "ccu022_01_WhiteBritish_Age50_59_EvenYOB_Index2015_PredictorData"
pred_df_age50_evenyob.write.option("overwriteSchema", "true").saveAsTable(name=(db_collab + '.' + pred_df_age50_evenyob_name + file_date), mode="overwrite")

pred_df_age50_oddyob_name = "ccu022_01_WhiteBritish_Age50_59_OddYOB_Index2015_PredictorData"
pred_df_age50_oddyob.write.option("overwriteSchema", "true").saveAsTable(name=(db_collab + '.' + pred_df_age50_oddyob_name + file_date), mode="overwrite")

# COMMAND ----------

# MAGIC %md
# MAGIC ###### Age 60-69

# COMMAND ----------

pred_df_age60 = pred_df_final.filter(F.col('age_at_2015') >= 60).filter(F.col('age_at_2015') < 70)
pred_df_age60_evenyob = pred_df_age60.filter(F.col('even_birth_year') == 1).drop('even_birth_year')
pred_df_age60_oddyob = pred_df_age60.filter(F.col('even_birth_year') == 0).drop('even_birth_year')

# COMMAND ----------

pred_df_age60_evenyob.count()

# COMMAND ----------

pred_df_age60_oddyob.count()

# COMMAND ----------

db_collab = ""
pred_df_age60_evenyob_name = "ccu022_01_WhiteBritish_Age60_69_EvenYOB_Index2015_PredictorData"
pred_df_age60_evenyob.write.option("overwriteSchema", "true").saveAsTable(name=(db_collab + '.' + pred_df_age60_evenyob_name + file_date), mode="overwrite")

pred_df_age60_oddyob_name = "ccu022_01_WhiteBritish_Age60_69_OddYOB_Index2015_PredictorData"
pred_df_age60_oddyob.write.option("overwriteSchema", "true").saveAsTable(name=(db_collab + '.' + pred_df_age60_oddyob_name + file_date), mode="overwrite")

# COMMAND ----------

# MAGIC %md
# MAGIC ### Prepare data for outcomes - Outcome list 1 (72 diseases)
# MAGIC Phecodes with >= 150,000 cases and >=50% primary diagnosis in HES APC

# COMMAND ----------

## Read study population PID and outcome diagnosis data (see Cell 12 from "Selection of predictor and outcome diseases")
db_collab = ""

study_pop_2015_name = "ccu022_01_WhiteBritish_Age40_69_Index2015_StudyPopPID"
study_pop_2015 = spark.read.table(db_collab + '.' + study_pop_2015_name + file_date)

diag_combine_target_name = "ccu022_01_first_onset_combined_diagnosis_age40to69in2015_PheCode_Cleaned_NHSID_2024_12_31"
target_ds_df0 = spark.read.table(db_collab + '.' + diag_combine_target_name + file_date)

# COMMAND ----------

# MAGIC %md
# MAGIC ##### Subset to final study population

# COMMAND ----------

## Inner join with the final study population
target_ds_df1 = target_ds_df0.join(study_pop_2015, on = 'PID', how='inner')

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

## Limit to diagnosis dates before 2015-01-01
target_ds_pre_idx_long = target_ds_df2.filter(F.col('DATE') < '2015-01-01')

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
target_ds_pre_idx_wide3 = target_ds_pre_idx_wide2.join(study_pop_2015, on = 'PID', how = 'right').fillna(0)

# COMMAND ----------

# MAGIC %md
# MAGIC ###### Save pre-index outcome status - used for deciding analysis population for each outcome (pts with pre-index outcome diagnosis will be removed)

# COMMAND ----------

target_ds_pre_idx_wide3.count()

# COMMAND ----------

## Save the temp data
db_collab = ""
target_ds_pre_idx_wide3_name = "ccu022_01_WhiteBritish_Age40_69_Index2015_PreIndexOutcomeData"
target_ds_pre_idx_wide3.write.option("overwriteSchema", "true").saveAsTable(name=(db_collab + '.' + target_ds_pre_idx_wide3_name + file_date), mode="overwrite")

# COMMAND ----------

# MAGIC %md
# MAGIC ###### Save list of phecodes for outcome diseases
# MAGIC

# COMMAND ----------

## Create a dataframe and save the phecode list 
target_ds_phecode_df = spark.createDataFrame(data=[(cnt+1,a) for cnt,a in enumerate(target_ds_list)], schema = ["index","target_ds"])

target_ds_phecode_df.display()

db_collab = ""
target_ds_phecode_df_name = "ccu022_01_Outcome_Diseases_Phecode_list"
target_ds_phecode_df.write.saveAsTable(name=(db_collab + '.' + target_ds_phecode_df_name + file_date), mode="overwrite")

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

## Limit to diagnosis dates >= 2015-01-01 (and by default <= 2024-12-31)
target_ds_fup_long = target_ds_df2.filter(F.col('DATE') >= '2015-01-01').filter(F.col('DATE') <= '2024-12-31')
target_ds_fup_long.count()

# COMMAND ----------

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
target_ds_fup_wide3 = target_ds_fup_wide2.join(study_pop_2015, on = 'PID', how = 'right')

## Get follow-up time and demo info
target_ds_fup_wide4 = target_ds_fup_wide3.join(analysis_df.select('PID', 'fup_days_10yr', 'fup_end_10yr', 'sex_code', 'age_at_2015', 'even_birth_year', 'date_of_birth'), on = 'PID', how = 'left').withColumn('Female', F.col('sex_code') - 1).drop('sex_code')

display(target_ds_fup_wide4)

# COMMAND ----------

# MAGIC %md
# MAGIC ###### Set censored and control outcome status

# COMMAND ----------

## For pts with NA values for outcome diagnosis status, set status as 9 (censored) if the follow-up end is < 2024-12-31, otherwise set status as 0 (controls)
target_ds_fup_wide4_censored = target_ds_fup_wide4.filter(F.col('fup_end_10yr') < '2024-12-31').fillna(9, subset = target_ds_list)
target_ds_fup_wide4_uncensored = target_ds_fup_wide4.filter(F.col('fup_end_10yr') == '2024-12-31').fillna(0, subset = target_ds_list)

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
target_ds_fup_wide5_name = "ccu022_01_WhiteBritish_Age40_69_Index2015_FollowupOutcomeStatus"
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

## Limit to diagnosis dates >= 2015-01-01 (and by default <= 2024-12-31)
target_ds_fup_long = target_ds_df2.filter(F.col('DATE') >= '2015-01-01').filter(F.col('DATE') <= '2024-12-31')

## Create a new column for time to event
target_ds_fup_long2 = target_ds_fup_long.withColumn('time_to_diag', F.datediff(F.col('DATE'), F.lit('2015-01-01')) + 1)

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

## Sanity check: all cells should have tte <= 3653
target_ds_fup_tte_wide2.withColumn('max_count', F.greatest(*target_ds_list)).select(F.max('max_count')).display()

# COMMAND ----------

## Join with final study population
target_ds_fup_tte_wide3 = target_ds_fup_tte_wide2.join(study_pop_2015, on = 'PID', how = 'right')

## Get follow-up time and demo info
target_ds_fup_tte_wide4 = target_ds_fup_tte_wide3.join(analysis_df.select('PID', 'fup_days_10yr', 'fup_end_10yr', 'sex_code', 'age_at_2015', 'even_birth_year', 'date_of_birth'), on = 'PID', how = 'left').withColumn('Female', F.col('sex_code') - 1).drop('sex_code')

display(target_ds_fup_tte_wide4)

# COMMAND ----------

# MAGIC %md
# MAGIC ###### Set time-to-event for censored and control pts

# COMMAND ----------

## For pts with NA values for tte, set tte as follow-up time
target_ds_fup_tte_wide5 = target_ds_fup_tte_wide4
for target_ds in target_ds_list:
    target_ds_fup_tte_wide5 = target_ds_fup_tte_wide5.withColumn(target_ds, F.when(F.col(target_ds).isNull(), F.col('fup_days_10yr')).otherwise(F.col(target_ds)))

display(target_ds_fup_tte_wide5)

# COMMAND ----------

# MAGIC %md
# MAGIC ###### Save time to outcome diagnosis data during follow-up

# COMMAND ----------

target_ds_fup_tte_final = target_ds_fup_tte_wide5.drop('fup_days_10yr', 'fup_end_10yr').orderBy('PID')

target_ds_fup_tte_final.count()

# COMMAND ----------

## Save the temp data
db_collab = ""
target_ds_fup_tte_final_name = "ccu022_01_WhiteBritish_Age40_69_Index2015_FollowupTimeToOutcome"
target_ds_fup_tte_final.write.option("overwriteSchema", "true").saveAsTable(name=(db_collab + '.' + target_ds_fup_tte_final_name + file_date), mode="overwrite")

# COMMAND ----------

# MAGIC %md
# MAGIC ##### Save combined outcome data

# COMMAND ----------

# MAGIC %md
# MAGIC ###### Read all outcome-related data

# COMMAND ----------

## total number of outcomes
n_outcomes = 73

# COMMAND ----------

## Read pre-index outcome status
db_collab = ""
target_ds_pre_idx_name = "ccu022_01_WhiteBritish_Age40_69_Index2015_PreIndexOutcomeData"
target_ds_pre_idx0 = spark.read.table(db_collab + '.' + target_ds_pre_idx_name + file_date)

## Rename outcome columns
target_ds_list = target_ds_pre_idx0.columns[1:(n_outcomes+1)]
target_ds_list_new = [str + '_preidx' for str in target_ds_list]
target_ds_pre_idx = target_ds_pre_idx0.toDF(*(['PID'] + target_ds_list_new))

## Convert ds columns to byte type to save space
target_ds_pre_idx_final = target_ds_pre_idx.select('PID', *(F.col(c).cast("byte").alias(c) for c in target_ds_list_new))

# COMMAND ----------

## Read outcome status during follow-up
target_ds_fup_name = "ccu022_01_WhiteBritish_Age40_69_Index2015_FollowupOutcomeStatus"
target_ds_fup0 = spark.read.table(db_collab + '.' + target_ds_fup_name + file_date)

## Rename outcome columns
target_ds_list = target_ds_fup0.columns[1:(n_outcomes+1)]
target_ds_fup0_dsonly = target_ds_fup0.select('PID', *target_ds_list)

target_ds_list_new = [str + '_fup' for str in target_ds_list]
target_ds_fup = target_ds_fup0_dsonly.toDF(*(['PID'] + target_ds_list_new))

## Convert ds columns to byte type to save space
target_ds_fup_final = target_ds_fup.select('PID', *(F.col(c).cast("byte").alias(c) for c in target_ds_list_new))

# COMMAND ----------

## Read time to outcome during follow-up
target_ds_fup_tte_name = "ccu022_01_WhiteBritish_Age40_69_Index2015_FollowupTimeToOutcome"
target_ds_fup_tte0 = spark.read.table(db_collab + '.' + target_ds_fup_tte_name + file_date)

## Rename time to event columns
target_ds_list = target_ds_fup_tte0.columns[1:(n_outcomes+1)]
target_ds_list_new = [str + '_tte' for str in target_ds_list]
target_ds_fup_tte = target_ds_fup_tte0.toDF(*(['PID'] + target_ds_list_new + ['age_at_2015', 'even_birth_year', 'date_of_birth', 'Female']))

## Convert ds columns to short type to save space
target_ds_fup_tte_final = target_ds_fup_tte.select('PID', *(F.col(c).cast("short").alias(c) for c in target_ds_list_new), 'age_at_2015', 'even_birth_year', 'date_of_birth', 'Female')

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
outcome_df_final_name = "ccu022_01_WhiteBritish_Age40_69_Index2015_AllOutcomeData"
outcome_df_final.write.option("overwriteSchema", "true").saveAsTable(name=(db_collab + '.' + outcome_df_final_name + file_date), mode="overwrite")

# COMMAND ----------

# MAGIC %md
# MAGIC ##### Subset to different age groups and even/odd birth years

# COMMAND ----------

# MAGIC %md
# MAGIC ###### Read the combined outcome data

# COMMAND ----------

db_collab = ""
outcome_df_final_name = "ccu022_01_WhiteBritish_Age40_69_Index2015_AllOutcomeData"
outcome_df_final = spark.read.table(db_collab + '.' + outcome_df_final_name + file_date)

# COMMAND ----------

# MAGIC %md
# MAGIC ###### Age 40-49

# COMMAND ----------

outcome_df_age40 = outcome_df_final.filter(F.col('age_at_2015') >= 40).filter(F.col('age_at_2015') < 50)
outcome_df_age40_evenyob = outcome_df_age40.filter(F.col('even_birth_year') == 1).drop('even_birth_year')
outcome_df_age40_oddyob = outcome_df_age40.filter(F.col('even_birth_year') == 0).drop('even_birth_year')

# COMMAND ----------

outcome_df_age40_evenyob.count() 

# COMMAND ----------

outcome_df_age40_oddyob.count() 

# COMMAND ----------

db_collab = ""
outcome_df_age40_evenyob_name = "ccu022_01_WhiteBritish_Age40_49_EvenYOB_Index2015_AllOutcomeData"
outcome_df_age40_evenyob.write.option("overwriteSchema", "true").saveAsTable(name=(db_collab + '.' + outcome_df_age40_evenyob_name + file_date), mode="overwrite")

outcome_df_age40_oddyob_name = "ccu022_01_WhiteBritish_Age40_49_OddYOB_Index2015_AllOutcomeData"
outcome_df_age40_oddyob.write.option("overwriteSchema", "true").saveAsTable(name=(db_collab + '.' + outcome_df_age40_oddyob_name + file_date), mode="overwrite")

# COMMAND ----------

# MAGIC %md
# MAGIC ###### Age 50-59

# COMMAND ----------

outcome_df_age50 = outcome_df_final.filter(F.col('age_at_2015') >= 50).filter(F.col('age_at_2015') < 60)
outcome_df_age50_evenyob = outcome_df_age50.filter(F.col('even_birth_year') == 1).drop('even_birth_year')
outcome_df_age50_oddyob = outcome_df_age50.filter(F.col('even_birth_year') == 0).drop('even_birth_year')

# COMMAND ----------

outcome_df_age50_evenyob.count()  

# COMMAND ----------

outcome_df_age50_oddyob.count() 

# COMMAND ----------

db_collab = ""
outcome_df_age50_evenyob_name = "ccu022_01_WhiteBritish_Age50_59_EvenYOB_Index2015_AllOutcomeData"
outcome_df_age50_evenyob.write.option("overwriteSchema", "true").saveAsTable(name=(db_collab + '.' + outcome_df_age50_evenyob_name + file_date), mode="overwrite")

outcome_df_age50_oddyob_name = "ccu022_01_WhiteBritish_Age50_59_OddYOB_Index2015_AllOutcomeData"
outcome_df_age50_oddyob.write.option("overwriteSchema", "true").saveAsTable(name=(db_collab + '.' + outcome_df_age50_oddyob_name + file_date), mode="overwrite")

# COMMAND ----------

# MAGIC %md
# MAGIC ###### Age 60-69

# COMMAND ----------

outcome_df_age60 = outcome_df_final.filter(F.col('age_at_2015') >= 60).filter(F.col('age_at_2015') < 70)
outcome_df_age60_evenyob = outcome_df_age60.filter(F.col('even_birth_year') == 1).drop('even_birth_year')
outcome_df_age60_oddyob = outcome_df_age60.filter(F.col('even_birth_year') == 0).drop('even_birth_year')

# COMMAND ----------

outcome_df_age60_evenyob.count()  

# COMMAND ----------

outcome_df_age60_oddyob.count()  

# COMMAND ----------

db_collab = ""
outcome_df_age60_evenyob_name = "ccu022_01_WhiteBritish_Age60_69_EvenYOB_Index2015_AllOutcomeData"
outcome_df_age60_evenyob.write.option("overwriteSchema", "true").saveAsTable(name=(db_collab + '.' + outcome_df_age60_evenyob_name + file_date), mode="overwrite")

outcome_df_age60_oddyob_name = "ccu022_01_WhiteBritish_Age60_69_OddYOB_Index2015_AllOutcomeData"
outcome_df_age60_oddyob.write.option("overwriteSchema", "true").saveAsTable(name=(db_collab + '.' + outcome_df_age60_oddyob_name + file_date), mode="overwrite")

# COMMAND ----------

# MAGIC %md
# MAGIC ### Prepare data for outcomes - Outcome list 2 (14 diseases)
# MAGIC Top 10 common phecodes + PheRS outcomes

# COMMAND ----------

## Read study population PID and outcome diagnosis data (see Cell 12 from "Selection of predictor and outcome diseases")
db_collab = ""

study_pop_2015_name = "ccu022_01_WhiteBritish_Age40_69_Index2015_StudyPopPID"
study_pop_2015 = spark.read.table(db_collab + '.' + study_pop_2015_name + file_date)

diag_combine_target_name = "ccu022_01_first_onset_combined_diagnosis_age40to69in2015_PheCode_Cleaned_NHSID_2024_12_31"
target_ds_df0 = spark.read.table(db_collab + '.' + diag_combine_target_name + file_date)

# COMMAND ----------

# DBTITLE 1,Subset to final study population
## Inner join with the final study population
target_ds_df1 = target_ds_df0.join(study_pop_2015, on = 'PID', how='inner')
target_ds_df1.select('PID').distinct().count()

# COMMAND ----------

# DBTITLE 1,Subset to outcome diseases
## Read list of outcome phecodes
db_collab = ""
outcome_ds_final2_name = "ccu022_01_outcome_disease_PheCode_FollowupCount150000_TopCommonPlusPheRS"
outcome_ds_final2 = spark.read.table(db_collab + '.' + outcome_ds_final2_name + file_date)
outcome_ds_final2.count()

## Filter diag data to outcome diseases
target_ds_df2 = target_ds_df1.join(outcome_ds_final2.select('PheCode'), on = 'PheCode', how='inner')

# COMMAND ----------

# MAGIC %md
# MAGIC ##### Record the outcome diagnosis status pre-index

# COMMAND ----------

## Limit to diagnosis dates before 2015-01-01
target_ds_pre_idx_long = target_ds_df2.filter(F.col('DATE') < '2015-01-01')

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
target_ds_pre_idx_wide3 = target_ds_pre_idx_wide2.join(study_pop_2015, on = 'PID', how = 'right').fillna(0)

# COMMAND ----------

# MAGIC %md
# MAGIC ###### Save pre-index outcome status - used for deciding analysis population for each outcome (pts with pre-index outcome diagnosis will be removed)

# COMMAND ----------

target_ds_pre_idx_wide3.count()

# COMMAND ----------

## Save the temp data
db_collab = ""
target_ds_pre_idx_wide3_name = "ccu022_01_WhiteBritish_Age40_69_Index2015_PreIndexOutcomeData_OutcomeList2"
target_ds_pre_idx_wide3.write.option("overwriteSchema", "true").saveAsTable(name=(db_collab + '.' + target_ds_pre_idx_wide3_name + file_date), mode="overwrite")

# COMMAND ----------

# MAGIC %md
# MAGIC ###### Save list of phecodes for outcome diseases
# MAGIC

# COMMAND ----------

## Create a dataframe and save the phecode list 
target_ds_phecode_df = spark.createDataFrame(data=[(cnt+1,a) for cnt,a in enumerate(target_ds_list)], schema = ["index","target_ds"])

target_ds_phecode_df.display()

db_collab = ""
target_ds_phecode_df_name = "ccu022_01_Outcome_Diseases_Phecode_list2_TopCommonPlusPheRS"
target_ds_phecode_df.write.saveAsTable(name=(db_collab + '.' + target_ds_phecode_df_name + file_date), mode="overwrite")

# COMMAND ----------

# MAGIC %md
# MAGIC ##### Record the outcome diagnosis status during follow-up

# COMMAND ----------

## Read the list of outcome phecodes
db_collab = ""
target_ds_phecode_df_name = "ccu022_01_Outcome_Diseases_Phecode_list2_TopCommonPlusPheRS"
target_ds_phecode_df = spark.read.table(db_collab + '.' + target_ds_phecode_df_name + file_date)
target_ds_phecode_ref = target_ds_phecode_df.orderBy('index').select('target_ds').toPandas().target_ds.to_list()
target_ds_phecode_ref


# COMMAND ----------

## Limit to diagnosis dates >= 2015-01-01 (and by default <= 2024-12-31)
target_ds_fup_long = target_ds_df2.filter(F.col('DATE') >= '2015-01-01').filter(F.col('DATE') <= '2024-12-31')
target_ds_fup_long.count()

# COMMAND ----------

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
target_ds_fup_wide3 = target_ds_fup_wide2.join(study_pop_2015, on = 'PID', how = 'right')

## Get follow-up time and demo info
target_ds_fup_wide4 = target_ds_fup_wide3.join(analysis_df.select('PID', 'fup_days_10yr', 'fup_end_10yr', 'sex_code', 'age_at_2015', 'even_birth_year', 'date_of_birth'), on = 'PID', how = 'left').withColumn('Female', F.col('sex_code') - 1).drop('sex_code')

display(target_ds_fup_wide4)

# COMMAND ----------

# MAGIC %md
# MAGIC ###### Set censored and control outcome status

# COMMAND ----------

## For pts with NA values for outcome diagnosis status, set status as 9 (censored) if the follow-up end is < 2024-12-31, otherwise set status as 0 (controls)
target_ds_fup_wide4_censored = target_ds_fup_wide4.filter(F.col('fup_end_10yr') < '2024-12-31').fillna(9, subset = target_ds_list)
target_ds_fup_wide4_uncensored = target_ds_fup_wide4.filter(F.col('fup_end_10yr') == '2024-12-31').fillna(0, subset = target_ds_list)

target_ds_fup_wide5 = target_ds_fup_wide4_censored.union(target_ds_fup_wide4_uncensored)
target_ds_fup_wide5.count()

# COMMAND ----------

## Check number of censored pts
target_ds_fup_wide4_censored.count() # ??% of all pts

# COMMAND ----------

target_ds_fup_wide5.display()

# COMMAND ----------

# MAGIC %md
# MAGIC ###### Save outcome status during follow-up

# COMMAND ----------

## Save the follow-up outcome status data
db_collab = ""
target_ds_fup_wide5_name = "ccu022_01_WhiteBritish_Age40_69_Index2015_FollowupOutcomeStatus_OutcomeList2"
target_ds_fup_wide5.write.option("overwriteSchema", "true").saveAsTable(name=(db_collab + '.' + target_ds_fup_wide5_name + file_date), mode="overwrite")

# COMMAND ----------

# MAGIC %md
# MAGIC ##### Record the time to outcome diagnosis 

# COMMAND ----------

## Read the list of outcome phecodes
db_collab = ""
target_ds_phecode_df_name = "ccu022_01_Outcome_Diseases_Phecode_list2_TopCommonPlusPheRS"
target_ds_phecode_df = spark.read.table(db_collab + '.' + target_ds_phecode_df_name + file_date)
target_ds_phecode_ref = target_ds_phecode_df.orderBy('index').select('target_ds').toPandas().target_ds.to_list()
target_ds_phecode_ref

# COMMAND ----------

## Limit to diagnosis dates >= 2015-01-01 (and by default <= 2024-12-31)
target_ds_fup_long = target_ds_df2.filter(F.col('DATE') >= '2015-01-01').filter(F.col('DATE') <= '2024-12-31')

## Create a new column for time to event
target_ds_fup_long2 = target_ds_fup_long.withColumn('time_to_diag', F.datediff(F.col('DATE'), F.lit('2015-01-01')) + 1)

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

## Sanity check: all cells should have tte <= 3653
target_ds_fup_tte_wide2.withColumn('max_count', F.greatest(*target_ds_list)).select(F.max('max_count')).display()

# COMMAND ----------

## Join with final study population
target_ds_fup_tte_wide3 = target_ds_fup_tte_wide2.join(study_pop_2015, on = 'PID', how = 'right')

## Get follow-up time and demo info
target_ds_fup_tte_wide4 = target_ds_fup_tte_wide3.join(analysis_df.select('PID', 'fup_days_10yr', 'fup_end_10yr', 'sex_code', 'age_at_2015', 'even_birth_year', 'date_of_birth'), on = 'PID', how = 'left').withColumn('Female', F.col('sex_code') - 1).drop('sex_code')

display(target_ds_fup_tte_wide4)

# COMMAND ----------

# MAGIC %md
# MAGIC ###### Set time-to-event for censored and control pts

# COMMAND ----------

## For pts with NA values for tte, set tte as follow-up time
target_ds_fup_tte_wide5 = target_ds_fup_tte_wide4
for target_ds in target_ds_list:
    target_ds_fup_tte_wide5 = target_ds_fup_tte_wide5.withColumn(target_ds, F.when(F.col(target_ds).isNull(), F.col('fup_days_10yr')).otherwise(F.col(target_ds)))

display(target_ds_fup_tte_wide5)

# COMMAND ----------

# MAGIC %md
# MAGIC ###### Save time to outcome diagnosis data during follow-up

# COMMAND ----------

target_ds_fup_tte_final = target_ds_fup_tte_wide5.drop('fup_days_10yr', 'fup_end_10yr').orderBy('PID')

target_ds_fup_tte_final.count()

# COMMAND ----------

## Save the temp data
db_collab = ""
target_ds_fup_tte_final_name = "ccu022_01_WhiteBritish_Age40_69_Index2015_FollowupTimeToOutcome_OutcomeList2"
target_ds_fup_tte_final.write.option("overwriteSchema", "true").saveAsTable(name=(db_collab + '.' + target_ds_fup_tte_final_name + file_date), mode="overwrite")

# COMMAND ----------

# MAGIC %md
# MAGIC ##### Save combined outcome data

# COMMAND ----------

# MAGIC %md
# MAGIC ###### Read all outcome-related data

# COMMAND ----------

## total number of outcomes
n_outcomes = 14

# COMMAND ----------

## Read pre-index outcome status
db_collab = ""
target_ds_pre_idx_name = "ccu022_01_WhiteBritish_Age40_69_Index2015_PreIndexOutcomeData_OutcomeList2"
target_ds_pre_idx0 = spark.read.table(db_collab + '.' + target_ds_pre_idx_name + file_date)

## Rename outcome columns
target_ds_list = target_ds_pre_idx0.columns[1:(n_outcomes+1)]
target_ds_list_new = [str + '_preidx' for str in target_ds_list]
target_ds_pre_idx = target_ds_pre_idx0.toDF(*(['PID'] + target_ds_list_new))

## Convert ds columns to byte type to save space
target_ds_pre_idx_final = target_ds_pre_idx.select('PID', *(F.col(c).cast("byte").alias(c) for c in target_ds_list_new))

# COMMAND ----------

## Read outcome status during follow-up
target_ds_fup_name = "ccu022_01_WhiteBritish_Age40_69_Index2015_FollowupOutcomeStatus_OutcomeList2"
target_ds_fup0 = spark.read.table(db_collab + '.' + target_ds_fup_name + file_date)

## Rename outcome columns
target_ds_list = target_ds_fup0.columns[1:(n_outcomes+1)]
target_ds_fup0_dsonly = target_ds_fup0.select('PID', *target_ds_list)

target_ds_list_new = [str + '_fup' for str in target_ds_list]
target_ds_fup = target_ds_fup0_dsonly.toDF(*(['PID'] + target_ds_list_new))

## Convert ds columns to byte type to save space
target_ds_fup_final = target_ds_fup.select('PID', *(F.col(c).cast("byte").alias(c) for c in target_ds_list_new))

# COMMAND ----------

## Read time to outcome during follow-up
target_ds_fup_tte_name = "ccu022_01_WhiteBritish_Age40_69_Index2015_FollowupTimeToOutcome_OutcomeList2"
target_ds_fup_tte0 = spark.read.table(db_collab + '.' + target_ds_fup_tte_name + file_date)

## Rename time to event columns
target_ds_list = target_ds_fup_tte0.columns[1:(n_outcomes+1)]
target_ds_list_new = [str + '_tte' for str in target_ds_list]
target_ds_fup_tte = target_ds_fup_tte0.toDF(*(['PID'] + target_ds_list_new + ['age_at_2015', 'even_birth_year', 'date_of_birth', 'Female']))

## Convert ds columns to short type to save space
target_ds_fup_tte_final = target_ds_fup_tte.select('PID', *(F.col(c).cast("short").alias(c) for c in target_ds_list_new), 'age_at_2015', 'even_birth_year', 'date_of_birth', 'Female')

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
outcome_df_final_name = "ccu022_01_WhiteBritish_Age40_69_Index2015_AllOutcomeData_OutcomeList2"
outcome_df_final.write.option("overwriteSchema", "true").saveAsTable(name=(db_collab + '.' + outcome_df_final_name + file_date), mode="overwrite")

# COMMAND ----------

# MAGIC %md
# MAGIC ##### Add death and subset to even/odd birth years

# COMMAND ----------

# MAGIC %md
# MAGIC ###### Read the combined outcome data

# COMMAND ----------

db_collab = ""
outcome_df_final_name = "ccu022_01_WhiteBritish_Age40_69_Index2015_AllOutcomeData_OutcomeList2"
outcome_df_final0 = spark.read.table(db_collab + '.' + outcome_df_final_name + file_date)

# COMMAND ----------

# MAGIC %md
# MAGIC ###### Add death as an outcome

# COMMAND ----------

analysis_df.select(F.min("date_of_death"), F.max("date_of_death")).display()

# COMMAND ----------

analysis_df.select(F.min("fup_days_10yr"), F.max("fup_days_10yr")).display()

# COMMAND ----------

analysis_df.select(F.min("fup_end_10yr"), F.max("fup_end_10yr")).display()

# COMMAND ----------

# DBTITLE 1,Add death info
## For death outcome, no one is censored (either confirmed death or not)
## See Cell 51 of "Preparation of data for analysis - Index date: 2015-01-01 - selection of study population" for follow-up end definition
death_info = analysis_df.withColumn("death_fup", F.when((F.col("death_flag") == 1) & (F.col("date_of_death") <= "2024-12-31"), 1).otherwise(0)).withColumn("death_preidx", F.lit(0)).withColumn("death_tte", F.col("fup_days_10yr")).select("PID", "death_preidx", "death_fup", "death_tte")
outcome_df_final = outcome_df_final0.join(death_info, on = "PID", how = "inner")

# COMMAND ----------

outcome_df_final.count()

# COMMAND ----------

# MAGIC %md
# MAGIC ###### Subset to even/odd birth years

# COMMAND ----------

outcome_df_evenyob = outcome_df_final.filter(F.col('even_birth_year') == 1).drop('even_birth_year')
outcome_df_oddyob = outcome_df_final.filter(F.col('even_birth_year') == 0).drop('even_birth_year')

# COMMAND ----------

outcome_df_evenyob.count() 

# COMMAND ----------

outcome_df_oddyob.count() 

# COMMAND ----------

# DBTITLE 1,Save datasets
db_collab = ""
outcome_df_evenyob_name = "ccu022_01_WhiteBritish_Age40_69_EvenYOB_Index2015_AllOutcomeData_OutcomeList2"
outcome_df_evenyob.write.option("overwriteSchema", "true").saveAsTable(name=(db_collab + '.' + outcome_df_evenyob_name + file_date), mode="overwrite")

outcome_df_oddyob_name = "ccu022_01_WhiteBritish_Age40_69_OddYOB_Index2015_AllOutcomeData_OutcomeList2"
outcome_df_oddyob.write.option("overwriteSchema", "true").saveAsTable(name=(db_collab + '.' + outcome_df_oddyob_name + file_date), mode="overwrite")