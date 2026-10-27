# Databricks notebook source
# MAGIC %md
# MAGIC ### Extract COVID cases - Index 2020
# MAGIC ##### Pearl Liu
# MAGIC
# MAGIC ##### v1.0

# COMMAND ----------

# MAGIC %md
# MAGIC #### Load functions and modules

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
# MAGIC #### Explore: Extract pts with COVID hospitalization in HES APC

# COMMAND ----------

## Read the curated HES APC diagnosis table with first onset diagnosis per pt and ICD-10 (A-Z) diagnoses
db_collab = ""
first_onset_hes_apc_icd10_name = "ccu022_01_first_onset_hes_apc_diagnosis_icd10AtoZ_2025_02_04_by_PL_2025_03_31"
first_onset_hes_apc_icd10 = spark.read.table(db_collab + "." + first_onset_hes_apc_icd10_name)

# COMMAND ----------

## Read pt characteristics table
df_demo = spark.table(params.path_demographics)

## Get date of birth
df_dob = df_demo.select('person_id', 'date_of_birth', 'date_of_death')

## Restrict to pts aged 40-69 and alive on 2020-01-01 (1950-01-01 < yob <= 1980-01-01)
df_dob_filter = df_dob.filter((F.col('date_of_birth') <= '1980-01-01') & (F.col('date_of_birth') > '1950-01-01')).filter( (F.col('date_of_death').isNull() ) | (F.col('date_of_death') >= '2020-01-01'))

## join with HES APC
first_onset_hes_apc_target = first_onset_hes_apc_icd10.join(df_dob_filter, on = 'person_id', how = 'inner')

# COMMAND ----------

## Read token pseudo ID lookup table
id_lookup = spark.read.table(".token_pseudo_id_lookup")

## Get NHS IDs
nhs_id = id_lookup.filter(F.col('valid_nhs_number') == 'true').select('pseudo_id').distinct()

## limit diag data to NHS IDs
first_onset_hes_apc_target_nhs = first_onset_hes_apc_target.join(nhs_id, first_onset_hes_apc_target.person_id == nhs_id.pseudo_id, how='inner').drop('pseudo_id')

# COMMAND ----------

## Clean epistart dates
## For epistart <= 1900-01-01, replace with epiend date
first_onset_hes_apc_target_nhs = first_onset_hes_apc_target_nhs.withColumn('epistart', F.when(F.col('epistart') <= '1900-01-01', F.col('epiend')).otherwise(F.col('epistart')))

## For epistart < birth date, replace with epiend date
first_onset_hes_apc_target_nhs = first_onset_hes_apc_target_nhs.withColumn('epistart', F.when(F.col('epistart') < F.col('date_of_birth'), F.col('epiend')).otherwise(F.col('epistart')))

## Limit to records with diagnosis dates >= dob
first_onset_hes_apc_target_nhs = first_onset_hes_apc_target_nhs.filter(F.col('epistart') >= F.col('date_of_birth'))

# COMMAND ----------

## Get pts with confirmed COVID hospitalization >= 2020-01-01 and <= 2024-12-31
covid_apc = first_onset_hes_apc_target_nhs.filter(F.col('code') == 'U071').filter(F.col('epistart') >= "2020-01-01").filter(F.col('epistart') <= "2024-12-31").filter(F.col('diag_position') == 1)

covid_apc_PID = covid_apc.select('person_id').distinct()

# COMMAND ----------

covid_apc.select(F.min('date_of_birth'), F.max('date_of_birth'), F.min('epistart'), F.max('epistart')).display()

# COMMAND ----------

# MAGIC %md
# MAGIC #### Explore: ICNARC

# COMMAND ----------

icnarc_dt = spark.read.table("_collab.icnarc__archive").filter(F.col('archived_on') == '2025-02-04'); icnarc_dt.display()

# COMMAND ----------

icnarc_PID = icnarc_dt.select("NHS_NUMBER_DEID").distinct().withColumnRenamed("NHS_NUMBER_DEID", "person_id")
icnarc_PID.count()

# COMMAND ----------

## Read curated COVID dataset, limit to HES APC or CHESS
db_collab = ""
covid_curated_dt_name = "hds_curated_assets__covid_positive_2025_02_04"
covid_curated_dt = spark.read.table(db_collab + "." + covid_curated_dt_name)
covid_curated_dt_hes = covid_curated_dt.filter(F.col('data_source').isin( ['hes_apc_primary_diagnosis', 'hes_apc_any_diagnosis', 'chess']))

covid_curated_dt_PID = covid_curated_dt.select('person_id').distinct()
covid_curated_dt_hes_PID = covid_curated_dt_hes.select('person_id').distinct()

# COMMAND ----------

## Check overlap
covid_curated_icnarc_overlap_PID1 = icnarc_PID.join(covid_curated_dt_PID, on = 'person_id', how='inner').distinct()
covid_curated_icnarc_overlap_PID1.count()

# COMMAND ----------

## Check overlap
covid_curated_icnarc_overlap_PID2 = icnarc_PID.join(covid_curated_dt_hes_PID, on = 'person_id', how='inner').distinct()
covid_curated_icnarc_overlap_PID2.count()

# COMMAND ----------

# MAGIC %md
# MAGIC #### Extract pts with COVID hospitalization in curated COVID dataset

# COMMAND ----------

## Read curated COVID dataset - 2025-02-04 version
db_collab = ""
covid_curated_dt_name = "hds_curated_assets__covid_positive_2025_02_04"
covid_curated_dt = spark.read.table(db_collab + "." + covid_curated_dt_name)

# COMMAND ----------

# MAGIC %md
# MAGIC ##### Inspect data

# COMMAND ----------

covid_curated_dt.display()

# COMMAND ----------

covid_curated_dt.select("data_source").distinct().display()

# COMMAND ----------

covid_curated_dt.filter(F.col('data_source') == 'hes_apc_primary_diagnosis').display()

# COMMAND ----------

covid_curated_dt.filter(F.col('data_source') == 'hes_apc_primary_diagnosis').select('code').distinct().display()

# COMMAND ----------

covid_curated_dt.filter(F.col('data_source') == 'chess').display()

# COMMAND ----------

covid_curated_dt.select(F.min('date'), F.max('date')).display()

# COMMAND ----------

covid_curated_dt.count()

# COMMAND ----------

# MAGIC %md
# MAGIC ##### Aggregate to earliest record per pt/data source/COVID status

# COMMAND ----------

## For each pt, keep only the first COVID diagnosis per data source and COVID status
w = Window.partitionBy('person_id', 'data_source', 'covid_status').orderBy(F.col('date').asc_nulls_last())
first_onset_covid_curated_dt = covid_curated_dt.withColumn('rank', F.row_number().over(w)).filter("rank = 1").drop('rank')
first_onset_covid_curated_dt.count()

# COMMAND ----------

## Save the first onset curated COVID data
db_collab = ""
first_onset_covid_curated_dt_name = "ccu022_01_first_onset_curated_covid_positive_2025_02_04"
first_onset_covid_curated_dt.write.option("overwriteSchema", "true").saveAsTable(name=(db_collab + '.' + first_onset_covid_curated_dt_name + file_date), mode="overwrite")  

# COMMAND ----------

# MAGIC %md
# MAGIC ##### Filter to target study population

# COMMAND ----------

## Read the first onset curated COVID data
db_collab = ""
first_onset_covid_curated_dt_name = "ccu022_01_first_onset_curated_covid_positive_2025_02_04"
first_onset_covid_curated_dt = spark.read.table(db_collab + '.' + first_onset_covid_curated_dt_name + file_date)  

# COMMAND ----------

## Read pt characteristics table
df_demo = spark.table(params.path_demographics)

## Get date of birth
df_dob = df_demo.select('person_id', 'date_of_birth', 'date_of_death')

## Restrict to pts aged 40-69 and alive on 2020-01-01 (1950-01-01 < yob <= 1980-01-01)
df_dob_filter = df_dob.filter((F.col('date_of_birth') <= '1980-01-01') & (F.col('date_of_birth') > '1950-01-01')).filter( (F.col('date_of_death').isNull() ) | (F.col('date_of_death') >= '2020-01-01'))

## join with curated COVID data, restrict to diagnosis dates >= dob
covid_curated_dt_target = first_onset_covid_curated_dt.join(df_dob_filter, on = 'person_id', how = 'inner').filter(F.col('date') >= F.col('date_of_birth'))

# COMMAND ----------

## Read token pseudo ID lookup table
id_lookup = spark.read.table(".token_pseudo_id_lookup")

## Get NHS IDs
nhs_id = id_lookup.filter(F.col('valid_nhs_number') == 'true').select('pseudo_id').distinct()

## limit data to NHS IDs
covid_curated_dt_target_nhs = covid_curated_dt_target.join(nhs_id, covid_curated_dt_target.person_id == nhs_id.pseudo_id, how='inner').drop('pseudo_id')

# COMMAND ----------

# MAGIC %md
# MAGIC ##### Explore: Extract HES APC primary diagnosis and compare with manually extracted data

# COMMAND ----------

covid_curated_hes_apc_primary = covid_curated_dt_target_nhs.filter(F.col('data_source') == 'hes_apc_primary_diagnosis').filter(F.col('covid_status') == 'confirmed').filter(F.col('date') >= "2020-01-01").filter(F.col('date') <= "2024-12-31")
covid_curated_hes_apc_primary_PID = covid_curated_hes_apc_primary.select('person_id').distinct()
covid_curated_hes_apc_primary_PID.count()

# COMMAND ----------

covid_curated_hes_apc_primary.select(F.min('date'), F.max('date')).display()

# COMMAND ----------

covid_apc_PID.count()

# COMMAND ----------

covid_curated_manual_overlap_PID = covid_apc_PID.join(covid_curated_hes_apc_primary_PID, on = 'person_id', how='inner').distinct()
covid_curated_manual_overlap_PID.count()

# COMMAND ----------

## Pts identified in HES APC but not in curated data
covid_apc_missed_PID = covid_apc_PID.join(covid_curated_hes_apc_primary_PID, on = 'person_id', how='leftanti').distinct()
covid_apc_missed_PID.display()

# COMMAND ----------

covid_apc.filter(F.col('person_id') == '').display()

# COMMAND ----------

covid_curated_dt_target_nhs.filter(F.col('person_id') == '').display()

# COMMAND ----------

## Pts identified in curated data but not in HES APC
covid_curated_missed_PID = covid_curated_hes_apc_primary_PID.join(covid_apc_PID, on = 'person_id', how='leftanti').distinct()
first_onset_hes_apc_target_nhs.join(covid_curated_missed_PID, on = 'person_id', how='inner').filter(F.col('code').isin(['U071', 'U072'])).filter(F.col('epistart') >= "2020-01-01").filter(F.col('epistart') <= "2024-12-31").display()

# COMMAND ----------

# MAGIC %md
# MAGIC ##### Extract all confirmed COVID cases

# COMMAND ----------

## All pts with confirmed COVID diagnosis anytime <= 2024-12-31
covid_curated_all = covid_curated_dt_target_nhs.filter(F.col('covid_status') == 'confirmed').filter(F.col('date') <= "2024-12-31")
covid_curated_all.count()

# COMMAND ----------

## Check pts with confirmed COVID diagnosis before 2020-01-01
covid_curated_dt_target_nhs.filter(F.col('covid_status') == 'confirmed').filter(F.col('date') < "2020-01-01").display()

# COMMAND ----------

## distinct pts
covid_curated_all_PID = covid_curated_all.select('person_id').distinct()
covid_curated_all_PID.count()

# COMMAND ----------

## Save PID with COVID status anytime
db_collab = ""
covid_curated_all_PID_name = "ccu022_01_PID_with_COVID_anytime_by_2024_12_31_age40to69in2020"
covid_curated_all_PID.write.option("overwriteSchema", "true").saveAsTable(name=(db_collab + '.' + covid_curated_all_PID_name + file_date), mode="overwrite") 

# COMMAND ----------

# MAGIC %md
# MAGIC ##### Extract all confirmed COVID hospitalization cases

# COMMAND ----------

## pts with confirmed COVID diagnosis <=2024-12-31 in HES APC (primary diagnosis) or CHESS
covid_curated_hspt = covid_curated_dt_target_nhs.filter(F.col('data_source').isin( ['hes_apc_primary_diagnosis', 'chess'])).filter(F.col('covid_status') == 'confirmed').filter(F.col('date') <= "2024-12-31")
covid_curated_hspt.count()

# COMMAND ----------

## distinct pts
covid_curated_hspt_PID = covid_curated_hspt.select('person_id').distinct()
covid_curated_hspt_PID.count()

# COMMAND ----------

# MAGIC %md
# MAGIC ###### Aggregate to earliest record per pt

# COMMAND ----------

## For each pt, keep only the first COVID diagnosis
w = Window.partitionBy('person_id').orderBy(F.col('date').asc_nulls_last())
first_onset_covid_curated_hspt = covid_curated_hspt.withColumn('rank', F.row_number().over(w)).filter("rank = 1").drop('rank')

# COMMAND ----------

## Sanity check
first_onset_covid_curated_hspt.count()

# COMMAND ----------

## Save the COVID hospitalization data - first onset per pt
db_collab = ""
first_onset_covid_curated_hspt_name = "ccu022_01_first_onset_covid_hospitalization_by_2024_12_31_age40to69in2020"
first_onset_covid_curated_hspt.write.option("overwriteSchema", "true").saveAsTable(name=(db_collab + '.' + first_onset_covid_curated_hspt_name + file_date), mode="overwrite") 

# COMMAND ----------

# MAGIC %md
# MAGIC ###### Explore: Check overlap with manual selection in HES APC

# COMMAND ----------

covid_curated_manual_overlap_PID2 = covid_apc_PID.join(covid_curated_hspt_PID, on = 'person_id', how='inner').distinct()
covid_curated_manual_overlap_PID2.count()