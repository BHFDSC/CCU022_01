# Databricks notebook source
# MAGIC %md
# MAGIC ### Preparation of data for analysis - Index date: 2020-01-01 - selection of study population
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

# MAGIC %run "./parameters"

# COMMAND ----------

# MAGIC %md
# MAGIC #### Load combined diagnosis data from HES APC and GDPPR

# COMMAND ----------

db_collab = ""

## Combined data at parent phecode level
diag_combine_agg_pheparent_nhs_name = "ccu022_01_first_onset_combined_diagnosis_age40to69in2010to2020_Parent_PheCode_Cleaned_NHSID_2024_12_31"
diag_combine_agg_pheparent_nhs = spark.read.table(db_collab + '.' + diag_combine_agg_pheparent_nhs_name + file_date)

# COMMAND ----------

# MAGIC %md
# MAGIC #### Load list of predictor diseases

# COMMAND ----------

db_collab = ""
phecode_parent_pred_name = "ccu022_01_predictor_disease_PheParent_codelist_Prev0_002"
phecode_parent_pred = spark.read.table(db_collab + '.' + phecode_parent_pred_name + file_date)

phecode_parent_pred.count()

# COMMAND ----------

# MAGIC %md
# MAGIC #### Load pt characteristics table

# COMMAND ----------

## Read pt characteristics table
df_demo = spark.table(params.path_demographics)
display(df_demo.limit(100))

# COMMAND ----------

# MAGIC %md
# MAGIC ### Sample selection

# COMMAND ----------

# MAGIC %md
# MAGIC ####  Patients aged 40-69 and alive on 2020-01-01 (1950-01-01 < yob <= 1980-01-01)

# COMMAND ----------

## Get date of death 
df_death = df_demo.select('person_id', 'date_of_death')

## Subset to pts alive on or after 2020-01-01
df_death_filter = df_death.filter("(date_of_death is null) OR (date_of_death >= '2020-01-01')")

# COMMAND ----------

## Restrict to pts aged 40-69 on 2020-01-01 (1950-01-01 < yob <= 1980-01-01) and alive on or after 2020-01-01
diag_target_pop = diag_combine_agg_pheparent_nhs.join(df_death_filter, diag_combine_agg_pheparent_nhs.PID == df_death_filter.person_id, how='inner').filter((F.col('date_of_birth') <= '1980-01-01') & (F.col('date_of_birth') > '1950-01-01')).drop('person_id')

# COMMAND ----------

## Check sample size
n0 = diag_target_pop.select('PID').distinct().count()
n0

# COMMAND ----------

# MAGIC %md
# MAGIC #### Pts had >=1 predictor disease 3 months before index

# COMMAND ----------

## Limit to predictor phecodes
diag_target_pop_pred_ds0 = diag_target_pop.join(phecode_parent_pred.select('PheCode_parent'), on = 'PheCode_parent', how='inner').select('PID', 'DATE', 'PheCode_parent', 'Phenotype_parent', 'Category', 'date_of_birth', 'date_of_death')

## Limit to diagnoses 3 months before index
diag_target_pop_pred_ds1 = diag_target_pop_pred_ds0.filter(F.col('DATE') < F.date_sub(F.lit('2020-01-01'), round(3*365.25/12)))
diag_target_pop_pred_ds1.select(F.min('DATE'), F.max('DATE')).display()

# COMMAND ----------

# MAGIC %md
# MAGIC ##### Save filtered predictor diag data

# COMMAND ----------

## Save filtered predictor diag data
db_collab = ""
diag_target_pop_pred_ds1_name = "ccu022_01_predictor_diseases_diagnosis_age40to69in2020"
diag_target_pop_pred_ds1.write.option("overwriteSchema", "true").saveAsTable(name=(db_collab + '.' + diag_target_pop_pred_ds1_name + file_date), mode="overwrite")

# COMMAND ----------

# MAGIC %md
# MAGIC ##### Check sample size

# COMMAND ----------

db_collab = ""
diag_target_pop_pred_ds1_name = "ccu022_01_predictor_diseases_diagnosis_age40to69in2020"
diag_target_pop_pred_ds1 = spark.read.table(db_collab + '.' + diag_target_pop_pred_ds1_name + file_date)

# COMMAND ----------

## Get study population:
study_pop1 = diag_target_pop_pred_ds1.select('PID').distinct()
n1 = study_pop1.count() 
n1


# COMMAND ----------

# MAGIC %md
# MAGIC #### Match study population to pt characteristics

# COMMAND ----------

## Read predictor diag data
db_collab = ""
diag_target_pop_pred_ds1_name = "ccu022_01_predictor_diseases_diagnosis_age40to69in2020"
diag_target_pop_pred_ds1 = spark.read.table(db_collab + '.' + diag_target_pop_pred_ds1_name + file_date)

## get study population
study_pop1 = diag_target_pop_pred_ds1.select('PID').distinct()

# COMMAND ----------

# MAGIC %md
# MAGIC ##### Remove missingness in sex and ethnicity

# COMMAND ----------

## Join study pop pt ID with pt char table
study_pop1_demo = study_pop1.join(df_demo, study_pop1.PID == df_demo.person_id, how='inner')

## Remove missing data in sex, limit sex to males and females
study_pop2_demo = study_pop1_demo.filter('sex is not null').filter(F.col('sex').isin(['M', 'F']))
n2 = study_pop2_demo.count()
n2 

# COMMAND ----------

n2/n1*100 

# COMMAND ----------

## Remove missing data in ethnicity (note: missingness is the same for ethnicity_5_group and ethnicity_18_group)
study_pop3_demo = study_pop2_demo.filter('ethnicity_5_group is not null')
n3 = study_pop3_demo.count()
n3  

# COMMAND ----------

n3/n1*100

# COMMAND ----------

## Sanity check
study_pop3_demo.filter('date_of_birth is null').count()
study_pop3_demo.filter('sex is null').count()
study_pop3_demo.filter('ethnicity_5_group is null').count()
study_pop3_demo.filter('ethnicity_18_group is null').count()

# COMMAND ----------

study_pop3_demo.select(F.min('date_of_birth'), F.max('date_of_birth')).display()

# COMMAND ----------

# MAGIC %md
# MAGIC ##### Save filtered demo data

# COMMAND ----------

## Save the filtered pt demo data
db_collab = ""
study_pop3_demo_name = "ccu022_01_demographics_age40to69in2020"
study_pop3_demo.write.option("overwriteSchema", "true").saveAsTable(name=(db_collab + '.' + study_pop3_demo_name + file_date), mode="overwrite")

# COMMAND ----------

# MAGIC %md
# MAGIC #### Inspect distribution of pt char

# COMMAND ----------

## Read the filtered pt demo data
db_collab = ""
study_pop3_demo_name = "ccu022_01_demographics_age40to69in2020"
study_pop3_demo = spark.read.table(db_collab + '.' + study_pop3_demo_name + file_date)

# COMMAND ----------

# MAGIC %md
# MAGIC ##### Sex

# COMMAND ----------

## Check distribution of sex
study_pop3_demo.groupBy('sex_code').count().orderBy('count', ascending=False).display()

# COMMAND ----------

## Check percentage of tie in sex assignment
study_pop3_demo.agg((F.sum(F.col('sex_tie_flag'))/n3 * 100).alias('perc_tie_sex')).display()

# COMMAND ----------

## Check percentage of tie in birth date assignment
study_pop3_demo.agg((F.sum(F.col('date_of_birth_tie_flag'))/n3 * 100).alias('perc_tie_dob')).display()

# COMMAND ----------

# MAGIC %md
# MAGIC ##### Ethnicity

# COMMAND ----------

## Check distribution of different ethnicity variables
study_pop3_demo.groupBy('ethnicity_5_group').count().orderBy('count', ascending=False).display()

# COMMAND ----------

study_pop3_demo.groupBy('ethnicity_18_group').count().orderBy('count', ascending=False).display()

# COMMAND ----------

study_pop3_demo.groupBy('ethnicity_5_group','ethnicity_18_group').count().orderBy('count', ascending=False).display()

# COMMAND ----------

## Check percentage of tie in ethnicity assignment
study_pop3_demo.agg((F.sum(F.col('ethnicity_5_tie_flag'))/n3 * 100).alias('perc_tie_eth5')).display()

# COMMAND ----------

## Check percentage of tie in ethnicity assignment
study_pop3_demo.agg((F.sum(F.col('ethnicity_18_tie_flag'))/n3 * 100).alias('perc_tie_eth18')).display()

# COMMAND ----------

# MAGIC %md
# MAGIC #### Add birth year and age at index

# COMMAND ----------

## Read filtered demo data
db_collab = ""
study_pop3_demo_name = "ccu022_01_demographics_age40to69in2020"
study_pop3_demo = spark.read.table(db_collab + '.' + study_pop3_demo_name + file_date)

# COMMAND ----------

## Add birth year, add age on 2020-01-01
study_pop_demo_final = study_pop3_demo.withColumn('year_of_birth', year(F.col('date_of_birth'))).withColumn('age_at_2020', F.months_between(F.lit('2020-01-01'), F.col('date_of_birth'))/12)

## Add variable indicating even vs odd birth year
study_pop_demo_final = study_pop_demo_final.withColumn('even_birth_year', F.when(F.col('year_of_birth') % 2 == 0, 1).otherwise(0))

display(study_pop_demo_final)


# COMMAND ----------

## Sanity check
study_pop_demo_final.select(F.min('age_at_2020'), F.max('age_at_2020')).display()

# COMMAND ----------

# MAGIC %md
# MAGIC #### Add follow-up info

# COMMAND ----------

## Add column for end of follow-up (up to 2024-12-31)
study_pop_demo_final2 = study_pop_demo_final.withColumn('fup_end_5yr', F.when((F.col('death_flag') == 1) & (F.col('date_of_death') <= '2024-12-31'), F.col('date_of_death')).otherwise('2024-12-31'))

## Add column for follow-up time (max 5 years)
study_pop_demo_final3 = study_pop_demo_final2.withColumn('fup_days_5yr', F.datediff(F.col('fup_end_5yr'), F.lit('2020-01-01')) + 1)

study_pop_demo_final3.display()

# COMMAND ----------

## Sanity check
study_pop_demo_final3.select(F.min('fup_end_5yr'), F.max('fup_end_5yr')).display()

# COMMAND ----------

study_pop_demo_final3.select(F.min('fup_days_5yr'), F.max('fup_days_5yr')).display()

# COMMAND ----------

# MAGIC %md
# MAGIC #### Subset to White British pts for training

# COMMAND ----------

## Subset to White British ethnicity
analysis_df_wb = study_pop_demo_final3.filter(F.col('ethnicity_5_group') == 'White').filter(F.col('ethnicity_18_group') == 'British')

n_analysis = analysis_df_wb.count() 
n_analysis

# COMMAND ----------

# MAGIC %md
# MAGIC ##### Save demographics data for main analytical sample

# COMMAND ----------

## Save demo info for main analytical sample
analysis_df_final = analysis_df_wb

db_collab = ""
analysis_df_final_name = "ccu022_01_WhiteBritish_Age40_69_Index2020_StudyPopInfo"
analysis_df_final.write.option("overwriteSchema", "true").saveAsTable(name=(db_collab + '.' + analysis_df_final_name + file_date), mode="overwrite")

# COMMAND ----------

# MAGIC %md
# MAGIC ##### Save Patient ID of final study population
# MAGIC

# COMMAND ----------

study_pop_PID_final = analysis_df_final.select('PID').distinct()

db_collab = ""
study_pop_PID_final_name = "ccu022_01_WhiteBritish_Age40_69_Index2020_StudyPopPID"
study_pop_PID_final.write.option("overwriteSchema", "true").saveAsTable(name=(db_collab + '.' + study_pop_PID_final_name + file_date), mode="overwrite")

# COMMAND ----------

# MAGIC %md
# MAGIC #### Subset to Other White/Irish pts for testing

# COMMAND ----------

## Subset to Other White/Irish ethnicity
analysis_df_OtherWhite = study_pop_demo_final3.filter((F.col('ethnicity_18_group') == 'Any other White background') | (F.col('ethnicity_18_group') == 'Irish'))

n_analysis = analysis_df_OtherWhite.count() 
n_analysis

# COMMAND ----------

## Save demo info for main analytical sample
analysis_df_final = analysis_df_OtherWhite

db_collab = ""
analysis_df_final_name = "ccu022_01_OtherWhite_Age40_69_Index2020_StudyPopInfo"
analysis_df_final.write.option("overwriteSchema", "true").saveAsTable(name=(db_collab + '.' + analysis_df_final_name + file_date), mode="overwrite")

# COMMAND ----------

study_pop_PID_final = analysis_df_final.select('PID').distinct()

db_collab = ""
study_pop_PID_final_name = "ccu022_01_OtherWhite_Age40_69_Index2020_StudyPopPID"
study_pop_PID_final.write.option("overwriteSchema", "true").saveAsTable(name=(db_collab + '.' + study_pop_PID_final_name + file_date), mode="overwrite")

# COMMAND ----------

# MAGIC %md
# MAGIC #### Subset to Asian pts for testing

# COMMAND ----------

## Subset to Asian/Asian British ethnicity
analysis_df_asian = study_pop_demo_final3.filter(F.col('ethnicity_5_group') == 'Asian or Asian British')

n_analysis = analysis_df_asian.count() 
n_analysis

# COMMAND ----------

## Save demo info for main analytical sample
analysis_df_final = analysis_df_asian

db_collab = ""
analysis_df_final_name = "ccu022_01_Asian_Age40_69_Index2020_StudyPopInfo"
analysis_df_final.write.option("overwriteSchema", "true").saveAsTable(name=(db_collab + '.' + analysis_df_final_name + file_date), mode="overwrite")

# COMMAND ----------

study_pop_PID_final = analysis_df_final.select('PID').distinct()

db_collab = ""
study_pop_PID_final_name = "ccu022_01_Asian_Age40_69_Index2020_StudyPopPID"
study_pop_PID_final.write.option("overwriteSchema", "true").saveAsTable(name=(db_collab + '.' + study_pop_PID_final_name + file_date), mode="overwrite")

# COMMAND ----------

# MAGIC %md
# MAGIC #### Subset to Black pts for testing

# COMMAND ----------

## Subset to Black ethnicity
analysis_df_black = study_pop_demo_final3.filter(F.col('ethnicity_5_group') == 'Black, Black British, Caribbean or African')

n_analysis = analysis_df_black.count() 
n_analysis

# COMMAND ----------

## Save demo info for main analytical sample
analysis_df_final = analysis_df_black

db_collab = ""
analysis_df_final_name = "ccu022_01_Black_Age40_69_Index2020_StudyPopInfo"
analysis_df_final.write.option("overwriteSchema", "true").saveAsTable(name=(db_collab + '.' + analysis_df_final_name + file_date), mode="overwrite")

# COMMAND ----------

study_pop_PID_final = analysis_df_final.select('PID').distinct()

db_collab = ""
study_pop_PID_final_name = "ccu022_01_Black_Age40_69_Index2020_StudyPopPID"
study_pop_PID_final.write.option("overwriteSchema", "true").saveAsTable(name=(db_collab + '.' + study_pop_PID_final_name + file_date), mode="overwrite")

# COMMAND ----------

# MAGIC %md
# MAGIC #### Subset to Mixed ethnicity pts for testing

# COMMAND ----------

## Subset to Mixed ethnicity
analysis_df_mix = study_pop_demo_final3.filter(F.col('ethnicity_5_group') == 'Mixed or multiple ethnic groups')

n_analysis = analysis_df_mix.count() 
n_analysis

# COMMAND ----------

## Save demo info for main analytical sample
analysis_df_final = analysis_df_mix

db_collab = ""
analysis_df_final_name = "ccu022_01_Mixed_Age40_69_Index2020_StudyPopInfo"
analysis_df_final.write.option("overwriteSchema", "true").saveAsTable(name=(db_collab + '.' + analysis_df_final_name + file_date), mode="overwrite")

# COMMAND ----------

study_pop_PID_final = analysis_df_final.select('PID').distinct()

db_collab = ""
study_pop_PID_final_name = "ccu022_01_Mixed_Age40_69_Index2020_StudyPopPID"
study_pop_PID_final.write.option("overwriteSchema", "true").saveAsTable(name=(db_collab + '.' + study_pop_PID_final_name + file_date), mode="overwrite")

# COMMAND ----------

# MAGIC %md
# MAGIC #### Subset to Other ethnicity pts for testing (NOTE: Other ethnicity is not used in subsequent ethnicity-speicific analyses due to heterogeneity within group)

# COMMAND ----------

## Subset to Other ethnicity
analysis_df_other = study_pop_demo_final3.filter(F.col('ethnicity_5_group') == 'Other ethnic group')

n_analysis = analysis_df_other.count() 
n_analysis

# COMMAND ----------

## Save demo info for main analytical sample
analysis_df_final = analysis_df_other

db_collab = ""
analysis_df_final_name = "ccu022_01_Other_Age40_69_Index2020_StudyPopInfo"
analysis_df_final.write.option("overwriteSchema", "true").saveAsTable(name=(db_collab + '.' + analysis_df_final_name + file_date), mode="overwrite")

# COMMAND ----------

study_pop_PID_final = analysis_df_final.select('PID').distinct()

db_collab = ""
study_pop_PID_final_name = "ccu022_01_Other_Age40_69_Index2020_StudyPopPID"
study_pop_PID_final.write.option("overwriteSchema", "true").saveAsTable(name=(db_collab + '.' + study_pop_PID_final_name + file_date), mode="overwrite")

# COMMAND ----------

# MAGIC %md
# MAGIC ### Check sample sizes for training data based on pts born in even years

# COMMAND ----------

## Reading main analytical dataset
db_collab = ""
analysis_df_final_name = "ccu022_01_WhiteBritish_Age40_69_Index2020_StudyPopInfo"
analysis_df_final = spark.read.table(db_collab + '.' + analysis_df_final_name + file_date)

# COMMAND ----------

## total number of pts
analysis_df_final.count()

# COMMAND ----------

# MAGIC %md
# MAGIC #### 40-49 age group

# COMMAND ----------

## Subset to target age group
analysis_df_age40 = analysis_df_final.filter(F.col('age_at_2020') >= 40).filter(F.col('age_at_2020') < 50)
analysis_df_age40.count() 

# COMMAND ----------

## pts who died (censored) within the 5-year follow-up period
analysis_df_age40.filter(F.col('date_of_death') <= '2024-12-31').count()

# COMMAND ----------

analysis_df_age40.select(F.min('date_of_death'), F.max('date_of_death')).display()

# COMMAND ----------

# MAGIC %md
# MAGIC #### 50-59 age group

# COMMAND ----------

## Subset to target age group
analysis_df_age50 = analysis_df_final.filter(F.col('age_at_2020') >= 50).filter(F.col('age_at_2020') < 60)
analysis_df_age50.count() 

# COMMAND ----------

analysis_df_age50.filter(F.col('date_of_death') <= '2024-12-31').count()

# COMMAND ----------

analysis_df_age50.select(F.min('date_of_death'), F.max('date_of_death')).display()

# COMMAND ----------

# MAGIC %md
# MAGIC #### 60-69 age group

# COMMAND ----------

## Subset to target age group
analysis_df_age60 = analysis_df_final.filter(F.col('age_at_2020') >= 60).filter(F.col('age_at_2020') < 70)
analysis_df_age60.count() 

# COMMAND ----------

analysis_df_age60.filter(F.col('date_of_death') <= '2024-12-31').count()

# COMMAND ----------

analysis_df_age60.select(F.min('date_of_death'), F.max('date_of_death')).display()
