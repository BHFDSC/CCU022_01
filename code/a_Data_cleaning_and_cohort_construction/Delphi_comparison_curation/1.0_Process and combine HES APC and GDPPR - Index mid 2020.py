# Databricks notebook source
# MAGIC %md
# MAGIC ### Process and combine HES APC and GDPPR diagnosis tables - Index mid-2020
# MAGIC ##### Pearl Liu
# MAGIC
# MAGIC ##### v1.1
# MAGIC
# MAGIC Sections that are modified for comparison with Delphi model are marked with "MODIFY".

# COMMAND ----------

# MAGIC %md
# MAGIC #### Load modules and parameters

# COMMAND ----------

import pyspark
from pyspark.sql import SparkSession, Window, Column
from pyspark.sql.functions import year
import pandas as pd
import pyspark.sql.functions as F

file_date = "_by_PL_2025_05_01"  # latest file date

# COMMAND ----------

# MAGIC %run "./parameters"

# COMMAND ----------

# MAGIC %md
# MAGIC #### Read ICD10-PheCode map - limit to ICD10 A-N

# COMMAND ----------

## Read ICD10-phecode map (saved to SDE via "Read ICD-10 PheCode Map")
db_collab = ""
phe_icd10_codelist_name = "ccu022_01_PheCode_ICD10_AllPheCodes_map"
phe_icd10_codelist0 = spark.read.table(db_collab + '.' + phe_icd10_codelist_name + file_date)
phe_icd10_codelist0.count()

# COMMAND ----------

## Sanity check
phe_icd10_codelist0.select('PheCode').distinct().count() 

# COMMAND ----------

phe_icd10_codelist0.select('PheCode_parent').distinct().count() 

# COMMAND ----------

phe_icd10_codelist0.select('ICD10').distinct().count() 

# COMMAND ----------

# MAGIC %md
# MAGIC ##### Limit to ICD10 codes starting with A-N

# COMMAND ----------

## limit to ICD10 codes starting with A-N
phe_icd10_codelist = phe_icd10_codelist0.filter(F.col('ICD10').rlike('^[A-N]'))
phe_icd10_codelist.count()

# COMMAND ----------

## Save the filtered code list
db_collab = ""
phe_icd10_codelist_name = "ccu022_01_PheCode_ICD10AtoN_map"
phe_icd10_codelist.write.option("overwriteSchema", "true").saveAsTable(name=(db_collab + '.' + phe_icd10_codelist_name + file_date), mode="overwrite") 

# COMMAND ----------

# MAGIC %md
# MAGIC ##### Read again

# COMMAND ----------

db_collab = ""
phe_icd10_codelist_name = "ccu022_01_PheCode_ICD10AtoN_map"
phe_icd10_codelist = spark.read.table(db_collab + '.' + phe_icd10_codelist_name + file_date) 

# COMMAND ----------

# MAGIC %md
# MAGIC #### Get SNOMED-PheCode map - limit to ICD10 A-N

# COMMAND ----------

# MAGIC %md
# MAGIC ##### Read SNOMED-ICD10 (A-N) map (saved to SDE via "Read ICD-10 SNOMED Map")

# COMMAND ----------

db_collab = ""
snomed_icd10_codelist_name = "ccu022_01_SNOMED_ICD10AtoN_map_Intl"
snomed_icd10_codelist = spark.read.table(db_collab + '.' + snomed_icd10_codelist_name + file_date)

# COMMAND ----------

## Sanity check
snomed_icd10_codelist.count()

# COMMAND ----------

# MAGIC %md
# MAGIC ##### Join SNOMED-ICD10 (A-N) map and ICD10 (A-N)-phecode map

# COMMAND ----------

## Join SNOMED-ICD10 map and ICD10-phecode map
snomed_icd10_phecode_map = snomed_icd10_codelist.join(phe_icd10_codelist, on = "ICD10", how = 'left')
snomed_icd10_phecode_map.count()

# COMMAND ----------

## Drop ICD10 column and make SNOMED-phecode map
snomed_phecode_map = snomed_icd10_phecode_map.drop('ICD10').distinct()
snomed_phecode_map.count()

# COMMAND ----------

## Save SNOMED-phecode map
db_collab = ""
snomed_phecode_map_name = "ccu022_01_PheCode_SNOMED_map_ICD10AtoN_Intl"
snomed_phecode_map.write.option("overwriteSchema", "true").saveAsTable(name=(db_collab + '.' + snomed_phecode_map_name + file_date), mode="overwrite")  

# COMMAND ----------

# MAGIC %md
# MAGIC ##### Read again

# COMMAND ----------

db_collab = ""
phe_snomed_codelist_name = "ccu022_01_PheCode_SNOMED_map_ICD10AtoN_Intl"
phe_snomed_codelist = spark.read.table(db_collab + '.' + phe_snomed_codelist_name + file_date)  
phe_snomed_codelist.count()

# COMMAND ----------

# MAGIC %md
# MAGIC ### Processing of HES APC diagnosis table

# COMMAND ----------

## Read in the curated HES APC diagnosis table
db_collab = ""
hes_apc_name = "hds_curated_assets__hes_apc_diagnosis_2025_02_04"  ## latest dataset
hes_apc_df = spark.read.table(db_collab + "." + hes_apc_name)
display(hes_apc_df)

# COMMAND ----------

## Example of multiple records per pt and code
hes_apc_df.filter((F.col('person_id') == '') & (F.col('code') == 'T84')).display()

# COMMAND ----------

# MAGIC %md
# MAGIC #### Aggregate HES APC at pt-ICD level

# COMMAND ----------

## For each pt, keep only the first diagnosis of each ICD code, keep NULL epistart at last while ranking
w = Window.partitionBy('person_id', 'code').orderBy(F.col('epistart').asc_nulls_last(), F.col('epiend').asc_nulls_last())
first_onset_hes_apc = hes_apc_df.withColumn('rank', F.row_number().over(w)).filter("rank = 1").drop('rank')

## Save the first onset HES APC data
first_onset_hes_apc_name = "ccu022_01_first_onset_hes_apc_diagnosis_2025_02_04"
first_onset_hes_apc.write.saveAsTable(name=(db_collab + '.' + first_onset_hes_apc_name + file_date), mode="overwrite")  


# COMMAND ----------

## Read the curated HES APC diagnosis table with first onset diagnosis per pt
db_collab = ""
first_onset_hes_apc_name = "ccu022_01_first_onset_hes_apc_diagnosis_2025_02_04"
first_onset_hes_apc = spark.read.table(db_collab + "." + first_onset_hes_apc_name + file_date)
#display(first_onset_hes_apc)

# COMMAND ----------

## Sanity check to make sure each pt has the earliest epistart per code
first_onset_hes_apc.filter((F.col('person_id') == '') & (F.col('code') == 'T84')).display()

# COMMAND ----------

# MAGIC %md
# MAGIC #### Check missing dates of diagnoses

# COMMAND ----------

## total number of pt-code pairs
first_onset_hes_apc.count() 

## Check the proportion of records with epistart being NULL per pt and code
first_onset_hes_apc.where(F.col('epistart').isNull()).count() 

## For records with missing epistart, check if other date colomns are populated
first_onset_hes_apc_missing_dates = first_onset_hes_apc.where(F.col('epistart').isNull())
first_onset_hes_apc_missing_dates.where(F.col('epiend').isNotNull() | F.col('admidate').isNotNull() | F.col('disdate').isNotNull()).count()  

## Check the proportion of records with epistart <= 1900
first_onset_hes_apc.filter(F.col('epistart') <= F.lit('1900-01-01')).count()  
first_onset_hes_apc.filter(F.col('epistart') <= F.lit('1900-01-01')).display()  # can replace these epistart dates with epiend dates

# COMMAND ----------

## Check range of epistart dates
first_onset_hes_apc.select(F.min('epistart'), F.max('epistart')).display()

# COMMAND ----------

# MAGIC %md
# MAGIC #### Get distinct ICD-10 codes and filter HES APC data accordingly

# COMMAND ----------

## Read the curated HES APC diagnosis table with first onset diagnosis per pt
db_collab = ""
first_onset_hes_apc_name = "ccu022_01_first_onset_hes_apc_diagnosis_2025_02_04"
first_onset_hes_apc = spark.read.table(db_collab + "." + first_onset_hes_apc_name + file_date)
display(first_onset_hes_apc)

# COMMAND ----------

## Get distinct ICD codes
icd_codes = first_onset_hes_apc.select('code').distinct().sort('code')
display(icd_codes)

# COMMAND ----------

## Limit to ICD-10 codes starting with A-Z
icd10_codes = icd_codes.filter(F.col('code').rlike('^[A-Z]'))
display(icd10_codes)

icd10_codes_name = "ccu022_01_ICD10_codelist_AtoZ_hes_apc_diagnosis_2025_02_04"
icd10_codes.write.saveAsTable(name=(db_collab + '.' + icd10_codes_name + file_date), mode="overwrite")

# COMMAND ----------

## Check records with ICD-9 codes
first_onset_hes_apc.filter((F.col('code').startswith('1'))).display()  # Neoplasms
first_onset_hes_apc.filter((F.col('code').startswith('4'))).display()  # Diseases of the Circulatory/Respiratory System
first_onset_hes_apc.filter((F.col('code').startswith('5'))).display()  # Diseases of the Respiratory/Digestive System

# COMMAND ----------

## Read the ICD-10 code list
icd10_codes_name = "ccu022_01_ICD10_codelist_AtoZ_hes_apc_diagnosis_2025_02_04"
icd10_codes = spark.read.table(db_collab + '.' + icd10_codes_name + file_date)
icd10_list = icd10_codes.toPandas().code.to_list()

# COMMAND ----------

## Sanity check
icd10_list[0:10]

# COMMAND ----------

icd10_list[-10:]

# COMMAND ----------

## Filter HES APC data to records with specified ICD-10 codes (starting with A-Z)
first_onset_hes_apc_icd10 = first_onset_hes_apc.filter(F.col('code').isin(icd10_list))

first_onset_hes_apc_icd10_name = "ccu022_01_first_onset_hes_apc_diagnosis_icd10AtoZ_2025_02_04"
first_onset_hes_apc_icd10.write.saveAsTable(name=(db_collab + '.' + first_onset_hes_apc_icd10_name + file_date), mode="overwrite")


# COMMAND ----------

# MAGIC %md
# MAGIC ##### Check the number of records/pts for ICD-10 vs. ICD-9

# COMMAND ----------

## Check the number of records for different datasets

# ICD-10 codes starting with A-Z
first_onset_hes_apc_icd10.count() 
# ICD-9 codes
first_onset_hes_apc.filter((F.col('code').rlike('^[0-9]'))).count()  
# All records
first_onset_hes_apc.count() 

## The proportion of ICD-9 codes is extremely small - can ignore in the analyses.

# COMMAND ----------

## Check the number of unique pts

# Pts with ICD-10 codes starting with A-Z
total_pts_icd = first_onset_hes_apc_icd10.select('person_id').distinct().count() 
total_pts_icd 

# Total number of pts in HES APC
total_pts = first_onset_hes_apc.select('person_id').distinct().count()  
total_pts  

total_pts_icd/total_pts   

# COMMAND ----------

# MAGIC %md
# MAGIC #### Map ICD-10 codes to PheCodes

# COMMAND ----------

## Read the curated HES APC diagnosis table with first onset diagnosis per pt and ICD-10 (A-Z) diagnoses
db_collab = ""
first_onset_hes_apc_icd10_name = "ccu022_01_first_onset_hes_apc_diagnosis_icd10AtoZ_2025_02_04"
first_onset_hes_apc_icd10 = spark.read.table(db_collab + "." + first_onset_hes_apc_icd10_name + file_date)
first_onset_hes_apc_icd10.display()

# COMMAND ----------

## Map ICD10 codes in HES APC table to phecodes
first_onset_hes_apc_icd10_phecode = first_onset_hes_apc_icd10.join(phe_icd10_codelist, first_onset_hes_apc_icd10.code == phe_icd10_codelist.ICD10, how='inner').drop('code')
display(first_onset_hes_apc_icd10_phecode)

# COMMAND ----------

# MAGIC %md
# MAGIC ##### Aggregate the data at pt-phecode level

# COMMAND ----------

## For each pt, keep only the first diagnosis of each phecode, keep NULL epistart at last while ranking
w = Window.partitionBy('person_id', 'PheCode').orderBy(F.col('epistart').asc_nulls_last(), F.col('epiend').asc_nulls_last())
first_onset_hes_apc_phecode = first_onset_hes_apc_icd10_phecode.drop('ICD10').withColumn('rank', F.row_number().over(w)).filter("rank = 1").drop('rank')

## Save the aggregated table
db_collab = ""
first_onset_hes_apc_phecode_name = "ccu022_01_first_onset_hes_apc_diagnosis_PheCode_MappedtoICD10AtoN_2025_02_04"
first_onset_hes_apc_phecode.write.saveAsTable(name=(db_collab + '.' + first_onset_hes_apc_phecode_name + file_date), mode="overwrite")

# COMMAND ----------

# MAGIC %md
# MAGIC #### Check sample attrition

# COMMAND ----------

## Read first onset HES APC phecode diagnosis table 
db_collab = ""
first_onset_hes_apc_phecode_name = "ccu022_01_first_onset_hes_apc_diagnosis_PheCode_MappedtoICD10AtoN_2025_02_04"
first_onset_hes_apc_phecode = spark.read.table(db_collab + '.' + first_onset_hes_apc_phecode_name + file_date)

# COMMAND ----------

## pts with mapped phecodes from ICD10 A-N
pts_phecode = first_onset_hes_apc_phecode.select('person_id').distinct().count()
pts_phecode 


# COMMAND ----------

## Percentage of pts with mapped phecodes from ICD10 A-N (out of all pts with ICD-10 codes starting with A-Z)
total_pts_icd = 76273261
pts_phecode/total_pts_icd*100

# COMMAND ----------

## Percentage of pts with mapped phecodes from ICD10 A-N (out of all pts in HES APC table)
total_pts = 76274017
pts_phecode/total_pts*100 

# COMMAND ----------

# MAGIC %md
# MAGIC #### MODIFY - Restrict HES APC to target study population (broad)

# COMMAND ----------

## Read first onset HES APC phecode diagnosis table 
db_collab = ""
first_onset_hes_apc_phecode_name = "ccu022_01_first_onset_hes_apc_diagnosis_PheCode_MappedtoICD10AtoN_2025_02_04"
first_onset_hes_apc_phecode = spark.read.table(db_collab + '.' + first_onset_hes_apc_phecode_name + file_date)
display(first_onset_hes_apc_phecode)

# COMMAND ----------

first_onset_hes_apc_phecode.count() 

# COMMAND ----------

## Read pt characteristics table
df_demo = spark.table(params.path_demographics)

# COMMAND ----------

# DBTITLE 1,Age 50-80 by 2020-06-30
## Get date of birth
df_dob = df_demo.select('person_id', 'date_of_birth')

## Restrict to pts aged 50-80 at 2020-06-30 (1939-06-30 < DOB <= 1970-06-30)
df_dob_filter = df_dob.filter((F.col('date_of_birth') <= '1970-06-30') & (F.col('date_of_birth') > '1939-06-30'))

# COMMAND ----------

## join with HES APC
first_onset_hes_apc_phecode2 = first_onset_hes_apc_phecode.join(df_dob_filter, on = 'person_id', how = 'inner')

first_onset_hes_apc_phecode2.count()  

# COMMAND ----------

## Save the filtered data
db_collab = ""
first_onset_hes_apc_phecode2_name = "ccu022_01_first_onset_hes_apc_diagnosis_age50to80inMid2020_PheCode_MappedtoICD10AtoN_2025_02_04"
first_onset_hes_apc_phecode2.write.saveAsTable(name=(db_collab + '.' + first_onset_hes_apc_phecode2_name + file_date), mode="overwrite")

# COMMAND ----------

# MAGIC %md
# MAGIC #### Clean HES APC data

# COMMAND ----------

## Read first onset HES APC phecode diagnosis table 
db_collab = ""
first_onset_hes_apc_phecode2_name = "ccu022_01_first_onset_hes_apc_diagnosis_age50to80inMid2020_PheCode_MappedtoICD10AtoN_2025_02_04"
first_onset_hes_apc_phecode2 = spark.read.table(db_collab + '.' + first_onset_hes_apc_phecode2_name + file_date)
display(first_onset_hes_apc_phecode2)

# COMMAND ----------

## For epistart <= 1900-01-01, replace with epiend date
first_onset_hes_apc_phecode3 = first_onset_hes_apc_phecode2.withColumn('epistart', F.when(F.col('epistart') <= '1900-01-01', F.col('epiend')).otherwise(F.col('epistart')))

# COMMAND ----------

## For epistart < birth date, replace with epiend date
first_onset_hes_apc_phecode4 = first_onset_hes_apc_phecode3.withColumn('epistart', F.when(F.col('epistart') < F.col('date_of_birth'), F.col('epiend')).otherwise(F.col('epistart')))

# COMMAND ----------

## Check range of epistart dates
first_onset_hes_apc_phecode4.select(F.min('epistart'), F.max('epistart')).display()

# before end of 2024, no filtering needed.

# COMMAND ----------

# MAGIC %md
# MAGIC ##### Save final HES APC table

# COMMAND ----------

## Modify format
hes_apc_final = first_onset_hes_apc_phecode4.select('person_id', 'date_of_birth', 'epistart', 'PheCode', 'PheCode_parent', 'Phenotype', 'Phenotype_parent', 'Category').withColumnRenamed('person_id', 'PID').withColumnRenamed('epistart', 'DATE').withColumn('Source', F.lit('HES APC'))
display(hes_apc_final)

## Save cleaned HES APC data at phecode level
db_collab = ""
hes_apc_final_name = "ccu022_01_first_onset_hes_apc_diagnosis_age50to80inMid2020_PheCode_Cleaned_2024_12_31"
hes_apc_final.write.option("overwriteSchema", "true").saveAsTable(name=(db_collab + '.' + hes_apc_final_name + file_date), mode="overwrite")

# COMMAND ----------

# MAGIC %md
# MAGIC ### Processing of GDPPR table

# COMMAND ----------

## Inspect the GDPPR dataset
display(gdppr.limit(20))

# COMMAND ----------

print(gdppr.columns)

# COMMAND ----------

# MAGIC %md
# MAGIC #### Aggregate GDPPR at pt-SNOMED level

# COMMAND ----------

## For each pt, keep only the first diagnosis of each SNOMED code, keep NULL DATE at last while ranking
## Note: the "DATE" variable is the date when the diagnosis occurred
w = Window.partitionBy('NHS_NUMBER_DEID', 'CODE').orderBy(F.col('DATE').asc_nulls_last(), F.col('RECORD_DATE').asc_nulls_last())
first_onset_gdppr = gdppr.select('NHS_NUMBER_DEID', 'CODE', 'DATE', 'RECORD_DATE').withColumn('rank', F.row_number().over(w)).filter("rank = 1").drop('rank')

## Save the first onset GDPPR data
db_collab = ""
first_onset_gdppr_name = "ccu022_01_first_onset_gdppr_diagnosis_2025_02_04"
first_onset_gdppr.write.option("overwriteSchema", "true").saveAsTable(name=(db_collab + '.' + first_onset_gdppr_name + file_date), mode="overwrite")

# COMMAND ----------

# MAGIC %md
# MAGIC #### Data inspection

# COMMAND ----------

# MAGIC %md
# MAGIC ##### Check the missing dates of diagnosis

# COMMAND ----------

## Read the first onset GDPPR dataset
db_collab = ""
first_onset_gdppr_name = "ccu022_01_first_onset_gdppr_diagnosis_2025_02_04"
first_onset_gdppr = spark.read.table(db_collab + '.' + first_onset_gdppr_name + file_date)

# COMMAND ----------

## total number of pt-code pairs in the GDPPR table
first_onset_gdppr.count() 

## Check the proportion of records with DATE being NULL per pt and code
first_onset_gdppr.where(F.col('DATE').isNull()).count() 

## For records with missing DATE, check if other dates colomns are populated
first_onset_gdppr_missing_dates = first_onset_gdppr.where(F.col('DATE').isNull())
first_onset_gdppr_missing_dates.where(F.col('RECORD_DATE').isNotNull()).count()  
first_onset_gdppr_missing_dates.where(F.col('RECORD_DATE').isNotNull()).display()

## Check the proportion of records with DATE <= 1900
first_onset_gdppr.filter(F.col('DATE') <= F.lit('1900-01-01')).count()  
first_onset_gdppr.filter(F.col('DATE') <= F.lit('1900-01-01')).display()  # can replace these DATE dates with RECORD_DATE

# COMMAND ----------

# MAGIC %md
# MAGIC ##### Check prevalence of specific SNOMED codes

# COMMAND ----------

## Check prevalence of specific SNOMED codes
## These SNOMED codes capture smoking history and belong to ICD10 code Z864 and phecode 306, which has the highest prevalence in HES APC + GDPPR
smoking_list = ['266923002', '266928006', '266925009', '266924008', '266921000', '266922007', '735128000', '160617001', '8517006', '281018007', '702975009']
gdppr.filter(F.col('YEAR_OF_BIRTH') >= 1940).filter(F.col('YEAR_OF_BIRTH') < 1980).filter(F.col('CODE').isin(smoking_list)).select('NHS_NUMBER_DEID').distinct().count()

# COMMAND ----------

## Check infectious mononucleosis 
gdppr.filter(F.col('YEAR_OF_BIRTH') >= 1940).filter(F.col('YEAR_OF_BIRTH') < 1980).filter(F.col('CODE') == '271558008').select('NHS_NUMBER_DEID').distinct().count()

# COMMAND ----------

## Check encephalitis
gdppr.filter(F.col('YEAR_OF_BIRTH') >= 1940).filter(F.col('YEAR_OF_BIRTH') < 1980).filter(F.col('CODE') == '161479005').select('NHS_NUMBER_DEID').distinct().count()

# COMMAND ----------

## Check pulmonary fibrosis
gdppr.filter(F.col('YEAR_OF_BIRTH') >= 1940).filter(F.col('YEAR_OF_BIRTH') < 1980).filter(F.col('CODE').isin(['51615001', '233623000'])).select('NHS_NUMBER_DEID').distinct().count()

# COMMAND ----------

# MAGIC %md
# MAGIC ##### Check % of SNOMED codes in GDPPR that are covered by the SNOMED map

# COMMAND ----------

## list of unique SNOMED codes in GDPPR
snomed_gdppr = first_onset_gdppr.select('CODE').distinct().withColumnRenamed('CODE', 'SNOMED')
snomed_gdppr.count()

# COMMAND ----------

## list of unique SNOMED codes in SNOMED-phecode map
snomed_ref = phe_snomed_codelist.select('SNOMED').distinct()
snomed_ref.count()

# COMMAND ----------

## list of unique SNOMED codes in original international codelist (version 2025-02-01)
db_collab = ""
snomed_intl_name = "ccu022_01_SNOMED_list_InternationalVersion20250201_by_PL_2025_03_31"
snomed_intl = spark.read.table(db_collab + '.' + snomed_intl_name)
snomed_intl.count()

# COMMAND ----------

## list of unique SNOMED codes in original UK codelist, mapped to ICD10 A-N (version 2025-02-12)
db_collab = ""
snomed_uk_AtoN_name = "ccu022_01_SNOMED_list_MappedToICD10AtoN_UKVersion20250212_by_PL_2025_03_31"
snomed_uk_AtoN = spark.read.table(db_collab + '.' + snomed_uk_AtoN_name)
snomed_uk_AtoN.count()

# COMMAND ----------

## list of unique SNOMED codes in original international codelist, mapped to ICD10 A-N (version 2024-08-01)
db_collab = ""
snomed_intl_AtoN_name = "ccu022_01_SNOMED_list_MappedToICD10AtoN_InternationalVersion20240801_by_PL_2025_03_31"
snomed_intl_AtoN = spark.read.table(db_collab + '.' + snomed_intl_AtoN_name)
snomed_intl_AtoN.count()

# COMMAND ----------

## Check overlap
snomed_gdppr_overlap1 = snomed_gdppr.join(snomed_ref, on = 'SNOMED', how = 'inner')
snomed_gdppr_overlap1.count()

# COMMAND ----------

## Check overlap
snomed_gdppr_overlap2 = snomed_gdppr.join(snomed_intl, on = 'SNOMED', how = 'inner')
snomed_gdppr_overlap2.count()

# COMMAND ----------

## Check overlap
snomed_gdppr_overlap3 = snomed_gdppr.join(snomed_uk_AtoN, on = 'SNOMED', how = 'inner')
snomed_gdppr_overlap3.count()

# COMMAND ----------

## Check overlap
snomed_gdppr_overlap4 = snomed_gdppr.join(snomed_intl_AtoN, on = 'SNOMED', how = 'inner')
snomed_gdppr_overlap4.count()

# COMMAND ----------

snomed_gdppr_overlap_intl_uk_AtoN = snomed_gdppr_overlap3.join(snomed_gdppr_overlap4, on = 'SNOMED', how = 'inner')
snomed_gdppr_overlap_intl_uk_AtoN.count()

# COMMAND ----------

snomed_gdppr_agg_intl_uk_AtoN = snomed_gdppr_overlap3.join(snomed_gdppr_overlap4, on = 'SNOMED', how = 'outer')
snomed_gdppr_agg_intl_uk_AtoN.count()

# COMMAND ----------

# MAGIC %md
# MAGIC #### Map SNOMED codes to PheCodes

# COMMAND ----------

## Read the first onset GDPPR dataset
db_collab = ""
first_onset_gdppr_name = "ccu022_01_first_onset_gdppr_diagnosis_2025_02_04"
first_onset_gdppr = spark.read.table(db_collab + '.' + first_onset_gdppr_name + file_date)

# COMMAND ----------

## Join GDPPR table with SNOMED-phecode map
first_onset_gdppr_snomed_phecode = first_onset_gdppr.join(phe_snomed_codelist, first_onset_gdppr.CODE == phe_snomed_codelist.SNOMED, how='inner').drop('CODE')
display(first_onset_gdppr_snomed_phecode)

# COMMAND ----------

# MAGIC %md
# MAGIC ##### Aggregate GDPPR at pt-phecode level

# COMMAND ----------

## For each pt, keep only the first diagnosis of each phecode
w = Window.partitionBy('NHS_NUMBER_DEID', 'PheCode').orderBy(F.col('DATE').asc_nulls_last(), F.col('RECORD_DATE').asc_nulls_last())
first_onset_gdppr_phecode = first_onset_gdppr_snomed_phecode.drop('SNOMED').withColumn('rank', F.row_number().over(w)).filter("rank = 1").drop('rank')
#display(first_onset_gdppr_phecode)

## Save the data at phecode level
db_collab = ""
first_onset_gdppr_phecode_name = "ccu022_01_first_onset_gdppr_diagnosis_phecode_MappedtoICD10AtoN_2025_02_04"
first_onset_gdppr_phecode.write.option("overwriteSchema", "true").saveAsTable(name=(db_collab + '.' + first_onset_gdppr_phecode_name + file_date), mode="overwrite")

# COMMAND ----------

# MAGIC %md
# MAGIC #### Check sample attrition

# COMMAND ----------

## Read first onset GDPPR data at phecode level
db_collab = ""
first_onset_gdppr_phecode_name = "ccu022_01_first_onset_gdppr_diagnosis_phecode_MappedtoICD10AtoN_2025_02_04"
first_onset_gdppr_phecode = spark.read.table(db_collab + '.' + first_onset_gdppr_phecode_name + file_date)

# COMMAND ----------

## Total number of first onset phecode records (mapped to ICD10 A-N) in GDPPR table
total_pts_phecode_records = first_onset_gdppr_phecode.count()
total_pts_phecode_records

# COMMAND ----------

## Number of pts with SNOMED codes mapped to phecodes (limited to ICD10 A-N)
pts_phecode = first_onset_gdppr_phecode.select('NHS_NUMBER_DEID').distinct().count()
pts_phecode 

# COMMAND ----------

## Total number of pts in the GDPPR table
total_pts = first_onset_gdppr.select('NHS_NUMBER_DEID').distinct().count()
total_pts 

# COMMAND ----------

pts_phecode/total_pts * 100   

# COMMAND ----------

# MAGIC %md
# MAGIC #### MODIFY - Restrict GDPPR to target study population (broad)

# COMMAND ----------

## Read first onset GDPPR data at phecode level
db_collab = ""
first_onset_gdppr_phecode_name = "ccu022_01_first_onset_gdppr_diagnosis_phecode_MappedtoICD10AtoN_2025_02_04"
first_onset_gdppr_phecode = spark.read.table(db_collab + '.' + first_onset_gdppr_phecode_name + file_date)

# COMMAND ----------

first_onset_gdppr_phecode.count() 

# COMMAND ----------

## Read pt characteristics table
df_demo = spark.table(params.path_demographics)

# COMMAND ----------

# DBTITLE 1,Age 50-80 by 2020-06-30
## Get date of birth
df_dob = df_demo.select('person_id', 'date_of_birth')

## Restrict to pts aged 50-80 at 2020-06-30 (1939-06-30 < DOB <= 1970-06-30)
df_dob_filter = df_dob.filter((F.col('date_of_birth') <= '1970-06-30') & (F.col('date_of_birth') > '1939-06-30'))

# COMMAND ----------

## join with GDPPR
first_onset_gdppr_phecode2 = first_onset_gdppr_phecode.join(df_dob_filter, first_onset_gdppr_phecode.NHS_NUMBER_DEID == df_dob_filter.person_id, how = 'inner')

first_onset_gdppr_phecode2.count()  

# COMMAND ----------

## Save the filtered data
db_collab = ""
first_onset_gdppr_phecode2_name = "ccu022_01_first_onset_gdppr_diagnosis_age50to80inMid2020_PheCode_MappedtoICD10AtoN_2025_02_04"
first_onset_gdppr_phecode2.write.option("overwriteSchema", "true").saveAsTable(name=(db_collab + '.' + first_onset_gdppr_phecode2_name + file_date), mode="overwrite")

# COMMAND ----------

# MAGIC %md
# MAGIC #### Clean GDPPR data

# COMMAND ----------

db_collab = ""
first_onset_gdppr_phecode2_name = "ccu022_01_first_onset_gdppr_diagnosis_age50to80inMid2020_PheCode_MappedtoICD10AtoN_2025_02_04"
first_onset_gdppr_phecode2 = spark.read.table(db_collab + '.' + first_onset_gdppr_phecode2_name + file_date)

# COMMAND ----------

## For DATE with NULL value, replace with RECORD_DATE
first_onset_gdppr_phecode3 = first_onset_gdppr_phecode2.withColumn('DATE', F.when(F.col('DATE').isNull(), F.col('RECORD_DATE')).otherwise(F.col('DATE')))

## For DATE <= 1900-01-01, replace with RECORD_DATE
first_onset_gdppr_phecode4 = first_onset_gdppr_phecode3.withColumn('DATE', F.when(F.col('DATE') <= '1900-01-01', F.col('RECORD_DATE')).otherwise(F.col('DATE')))

## For DATE < birth date, replace with RECORD_DATE
first_onset_gdppr_phecode5 = first_onset_gdppr_phecode4.withColumn('DATE', F.when(F.col('DATE') < F.col('date_of_birth'), F.col('RECORD_DATE')).otherwise(F.col('DATE')))


# COMMAND ----------

## Check range of DATE dates
first_onset_gdppr_phecode5.select(F.min('DATE'), F.max('DATE')).display()

# COMMAND ----------

## filter dates to be before end of 2024
first_onset_gdppr_phecode6 = first_onset_gdppr_phecode5.filter(F.col('DATE') <= '2024-12-31')
first_onset_gdppr_phecode6.select(F.min('DATE'), F.max('DATE')).display()

## To remove out-of-range dates after combination of HES APC and GDPPR

# COMMAND ----------

# MAGIC %md
# MAGIC ##### Save final GDPPR table

# COMMAND ----------

## Modify format
gdppr_final = first_onset_gdppr_phecode6.select('NHS_NUMBER_DEID', 'date_of_birth', 'DATE', 'PheCode', 'PheCode_parent', 'Phenotype', 'Phenotype_parent', 'Category').withColumnRenamed('NHS_NUMBER_DEID', 'PID').withColumn('Source', F.lit('GDPPR'))
display(gdppr_final)

## Save cleaned GDPPR data at phecode level
db_collab = ""
gdppr_final_name = "ccu022_01_first_onset_gdppr_diagnosis_age50to80inMid2020_PheCode_Cleaned_2024_12_31"
gdppr_final.write.option("overwriteSchema", "true").saveAsTable(name=(db_collab + '.' + gdppr_final_name + file_date), mode="overwrite")

# COMMAND ----------

# MAGIC %md
# MAGIC ### Combine HES APC and GDPPR

# COMMAND ----------

# MAGIC %md
# MAGIC #### Read processed data

# COMMAND ----------

db_collab = ""

## HES APC
hes_apc_final_name = "ccu022_01_first_onset_hes_apc_diagnosis_age50to80inMid2020_PheCode_Cleaned_2024_12_31"
hes_apc_final = spark.read.table(db_collab + '.' + hes_apc_final_name + file_date)

## GDPPR
gdppr_final_name = "ccu022_01_first_onset_gdppr_diagnosis_age50to80inMid2020_PheCode_Cleaned_2024_12_31"
gdppr_final = spark.read.table(db_collab + '.' + gdppr_final_name + file_date)

# COMMAND ----------

# MAGIC %md
# MAGIC ##### Check patient count from each data source

# COMMAND ----------

## Check pt count
hes_apc_final.select('PID').distinct().count()  

# COMMAND ----------

gdppr_final.select('PID').distinct().count()  

# COMMAND ----------

# MAGIC %md
# MAGIC #### Combine and clean combined data

# COMMAND ----------

## Combine two datasets
diag_combine = hes_apc_final.union(gdppr_final)

# COMMAND ----------

## Aggregate at pt-phecode level
## For each pt, keep only the first diagnosis of each phecode
w = Window.partitionBy('PID', 'PheCode').orderBy(F.col('DATE').asc_nulls_last())
diag_combine_agg = diag_combine.drop('Source').withColumn('rank', F.row_number().over(w)).filter("rank = 1").drop('rank')

# COMMAND ----------

## Check pt count
diag_combine_agg.select('PID').distinct().count() 

# COMMAND ----------

## Keep records with (1) DATE > 1900-01-01, (2) DATE >= birth date, (3) DATE is not NULL
diag_combine_agg_clean = diag_combine_agg.filter("DATE > '1900-01-01'").filter(F.col('DATE') >= F.col('date_of_birth')).filter("DATE is not null")

# COMMAND ----------

diag_combine_agg_clean.select('PID').distinct().count() 

# COMMAND ----------

## Check range of dates
diag_combine_agg_clean.select(F.min('DATE'), F.max('DATE')).display()

# COMMAND ----------

# MAGIC %md
# MAGIC ##### Save cleaned combined data at phecode level

# COMMAND ----------

## Save cleaned combined data at phecode level
db_collab = ""
diag_combine_agg_clean_name = "ccu022_01_first_onset_combined_diagnosis_age50to80inMid2020_PheCode_Cleaned_2024_12_31"
diag_combine_agg_clean.write.option("overwriteSchema", "true").saveAsTable(name=(db_collab + '.' + diag_combine_agg_clean_name + file_date), mode="overwrite")

# COMMAND ----------

# MAGIC %md
# MAGIC ##### Save cleaned combined data at parent phecode level

# COMMAND ----------

## Aggregate at pt-parent phecode level
## For each pt, keep only the first diagnosis of each parent phecode
w = Window.partitionBy('PID', 'PheCode_parent').orderBy(F.col('DATE').asc_nulls_last())
diag_combine_agg_pheparent = diag_combine_agg_clean.drop('PheCode', 'Phenotype').withColumn('rank', F.row_number().over(w)).filter("rank = 1").drop('rank')
display(diag_combine_agg_pheparent)

# COMMAND ----------

## Save cleaned combined data at parent phecode level
db_collab = ""
diag_combine_agg_pheparent_name = "ccu022_01_first_onset_combined_diagnosis_age50to80inMid2020_Parent_PheCode_Cleaned_2024_12_31"
diag_combine_agg_pheparent.write.option("overwriteSchema", "true").saveAsTable(name=(db_collab + '.' + diag_combine_agg_pheparent_name + file_date), mode="overwrite")

# COMMAND ----------

# MAGIC %md
# MAGIC ### Remove single-use ID 

# COMMAND ----------

# MAGIC %md
# MAGIC #### Read processed data

# COMMAND ----------

db_collab = ""

## Read cleaned combined data at phecode level
diag_combine_agg_phecode_name = "ccu022_01_first_onset_combined_diagnosis_age50to80inMid2020_PheCode_Cleaned_2024_12_31"
diag_combine_agg_phecode = spark.read.table(db_collab + '.' + diag_combine_agg_phecode_name + file_date)

## Read cleaned combined data at parent phecode level
diag_combine_agg_pheparent_name = "ccu022_01_first_onset_combined_diagnosis_age50to80inMid2020_Parent_PheCode_Cleaned_2024_12_31"
diag_combine_agg_pheparent = spark.read.table(db_collab + '.' + diag_combine_agg_pheparent_name + file_date)

# COMMAND ----------

n0 = diag_combine_agg_phecode.select('PID').distinct().count() 
n0 

# COMMAND ----------

# MAGIC %md
# MAGIC #### Read token pseudo ID lookup table

# COMMAND ----------

## Read token pseudo ID lookup table
id_lookup = spark.read.table(".token_pseudo_id_lookup")
display(id_lookup.limit(100))

# COMMAND ----------

# MAGIC %md
# MAGIC #### Check pts with MPS_ID vs. NHS number

# COMMAND ----------

## Get mps IDs
mps_id = id_lookup.filter(F.col('mps_id') == 'true').select('pseudo_id').distinct()

## limit diag data to mps IDs
diag_combine_agg_phecode_mps = diag_combine_agg_phecode.join(mps_id, diag_combine_agg_phecode.PID == mps_id.pseudo_id, how='inner')
n_mps = diag_combine_agg_phecode_mps.select('PID').distinct().count()  
n_mps

# COMMAND ----------

n_mps/n0*100

# COMMAND ----------

## Compare to pts with valide NHS ID
## Get NHS IDs
nhs_id = id_lookup.filter(F.col('valid_nhs_number') == 'true').select('pseudo_id').distinct()

## limit diag data to NHS IDs
diag_combine_agg_phecode_nhs = diag_combine_agg_phecode.join(nhs_id, diag_combine_agg_phecode.PID == nhs_id.pseudo_id, how='inner')
n_nhs = diag_combine_agg_phecode_nhs.select('PID').distinct().count()  
n_nhs

# COMMAND ----------

n_nhs/n0*100

# COMMAND ----------

## Check the number of records per pt with mps_id
n_rec_per_pt = diag_combine_agg_phecode_mps.groupBy('PID').count()
n_rec_per_pt.select('count').summary().show()

# COMMAND ----------

## Check the number of records per pt with nhs id
n_rec_per_pt = diag_combine_agg_phecode_nhs.groupBy('PID').count()
n_rec_per_pt.select('count').summary().show()

# COMMAND ----------

## Check the gap in dates between first and last record per pt with mps_id
w1 = Window.partitionBy('PID').orderBy(F.col('DATE').asc_nulls_last())
w2 = Window.partitionBy('PID').orderBy(F.col('DATE').desc_nulls_last())

first_rec_per_pt = diag_combine_agg_phecode_mps.withColumn('rank', F.row_number().over(w1)).filter("rank = 1").drop('rank').withColumnRenamed('DATE', 'first_DATE').select('PID', 'first_DATE')
last_rec_per_pt = diag_combine_agg_phecode_mps.withColumn('rank', F.row_number().over(w2)).filter("rank = 1").drop('rank').withColumnRenamed('DATE', 'last_DATE').select('PID', 'last_DATE')
rec_gap_pre_pt = first_rec_per_pt.join(last_rec_per_pt, on = 'PID', how = 'inner').withColumn('rec_gap', F.datediff(F.col('last_DATE'), F.col('first_DATE')))
rec_gap_pre_pt.select('rec_gap').summary("count", "mean", "stddev", "min", "25%", "50%", "75%", "80%", "90%", "max").show()

# COMMAND ----------

## Check the gap in dates between first and last record per pt with nhs id
w1 = Window.partitionBy('PID').orderBy(F.col('DATE').asc_nulls_last())
w2 = Window.partitionBy('PID').orderBy(F.col('DATE').desc_nulls_last())

first_rec_per_pt = diag_combine_agg_phecode_nhs.withColumn('rank', F.row_number().over(w1)).filter("rank = 1").drop('rank').withColumnRenamed('DATE', 'first_DATE').select('PID', 'first_DATE')
last_rec_per_pt = diag_combine_agg_phecode_nhs.withColumn('rank', F.row_number().over(w2)).filter("rank = 1").drop('rank').withColumnRenamed('DATE', 'last_DATE').select('PID', 'last_DATE')
rec_gap_pre_pt = first_rec_per_pt.join(last_rec_per_pt, on = 'PID', how = 'inner').withColumn('rec_gap', F.datediff(F.col('last_DATE'), F.col('first_DATE')))
rec_gap_pre_pt.select('rec_gap').summary("count", "mean", "stddev", "min", "25%", "50%", "75%", "80%", "90%", "max").show()

# COMMAND ----------

# MAGIC %md
# MAGIC #### Limit diagnosis data to pts with valid NHS number

# COMMAND ----------

## Get NHS IDs
nhs_id = id_lookup.filter(F.col('valid_nhs_number') == 'true').select('pseudo_id').distinct()

## limit diag data to NHS IDs
diag_combine_agg_phecode_nhs = diag_combine_agg_phecode.join(nhs_id, diag_combine_agg_phecode.PID == nhs_id.pseudo_id, how='inner').drop('pseudo_id')
diag_combine_agg_pheparent_nhs = diag_combine_agg_pheparent.join(nhs_id, diag_combine_agg_pheparent.PID == nhs_id.pseudo_id, how='inner').drop('pseudo_id')

n_nhs = diag_combine_agg_phecode_nhs.select('PID').distinct().count()  
n_nhs

# COMMAND ----------

n_nhs/n0*100

# COMMAND ----------

# MAGIC %md
# MAGIC ##### Save diagnosis data for pts with valid NHS number

# COMMAND ----------

## Save filtered data
db_collab = ""

diag_combine_agg_phecode_nhs_name = "ccu022_01_first_onset_combined_diagnosis_age50to80inMid2020_PheCode_Cleaned_NHSID_2024_12_31"
diag_combine_agg_phecode_nhs.write.option("overwriteSchema", "true").saveAsTable(name=(db_collab + '.' + diag_combine_agg_phecode_nhs_name + file_date), mode="overwrite")

diag_combine_agg_pheparent_nhs_name = "ccu022_01_first_onset_combined_diagnosis_age50to80inMid2020_Parent_PheCode_Cleaned_NHSID_2024_12_31"
diag_combine_agg_pheparent_nhs.write.option("overwriteSchema", "true").saveAsTable(name=(db_collab + '.' + diag_combine_agg_pheparent_nhs_name + file_date), mode="overwrite")



# COMMAND ----------

# MAGIC %md
# MAGIC #### Check overlap between pts with NHS number/MPS ID and pts in GDPPR

# COMMAND ----------

db_collab = ""
diag_combine_agg_phecode_nhs_name = "ccu022_01_first_onset_combined_diagnosis_age40to69in2010to2020_PheCode_Cleaned_NHSID_2024_12_31"
diag_combine_agg_phecode_nhs = spark.read.table(db_collab + '.' + diag_combine_agg_phecode_nhs_name + file_date)

# COMMAND ----------

diag_combine_agg_phecode_nhs.select('PID').distinct().count()

# COMMAND ----------

## limit to pts within GDPPR
gdppr_pts = gdppr.select('NHS_NUMBER_DEID').distinct().withColumnRenamed('NHS_NUMBER_DEID', 'PID')

pts_overlap = diag_combine_agg_phecode_nhs.join(gdppr_pts, on = 'PID', how = 'inner')
pts_overlap.select('PID').distinct().count()

# COMMAND ----------

## pts with NHS ID but not in GDPPR
pts_nhs_notgdppr = diag_combine_agg_phecode_nhs.join(gdppr_pts, on = 'PID', how = 'left_anti')

## Check the number of records per pt with NHS ID but not in GDPPR
n_rec_per_pt = pts_nhs_notgdppr.groupBy('PID').count()
n_rec_per_pt.select('count').summary().show()

# COMMAND ----------

## Check the gap in dates between first and last record per pt with mps_id
w1 = Window.partitionBy('PID').orderBy(F.col('DATE').asc_nulls_last())
w2 = Window.partitionBy('PID').orderBy(F.col('DATE').desc_nulls_last())

first_rec_per_pt = pts_nhs_notgdppr.withColumn('rank', F.row_number().over(w1)).filter("rank = 1").drop('rank').withColumnRenamed('DATE', 'first_DATE').select('PID', 'first_DATE')
last_rec_per_pt = pts_nhs_notgdppr.withColumn('rank', F.row_number().over(w2)).filter("rank = 1").drop('rank').withColumnRenamed('DATE', 'last_DATE').select('PID', 'last_DATE')
rec_gap_pre_pt = first_rec_per_pt.join(last_rec_per_pt, on = 'PID', how = 'inner').withColumn('rec_gap', F.datediff(F.col('last_DATE'), F.col('first_DATE')))
rec_gap_pre_pt.select('rec_gap').summary("count", "mean", "stddev", "min", "25%", "50%", "75%", "80%", "90%", "max").show()

# COMMAND ----------

## Get mps IDs
mps_id = id_lookup.filter(F.col('mps_id') == 'true').select('pseudo_id').distinct()

## limit diag data to mps IDs
diag_combine_agg_phecode_mps = diag_combine_agg_phecode.join(mps_id, diag_combine_agg_phecode.PID == mps_id.pseudo_id, how='inner')

diag_combine_agg_phecode_mps.select('PID').distinct().count()

# COMMAND ----------

mps_pts_overlap = diag_combine_agg_phecode_mps.join(gdppr_pts, on = 'PID', how = 'inner')
mps_pts_overlap.select('PID').distinct().count()