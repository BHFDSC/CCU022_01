# Databricks notebook source
# MAGIC %md
# MAGIC ### Selection of predictor and outcome diseases
# MAGIC ##### Pearl Liu
# MAGIC
# MAGIC ##### v1.2

# COMMAND ----------

# MAGIC %md
# MAGIC Selection is based on the combined diagnosis data from HES APC and GDPPR.

# COMMAND ----------

# MAGIC %md
# MAGIC #### Load modules

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
# MAGIC #### Read combined diagnosis data from HES APC and GDPPR

# COMMAND ----------

db_collab = ""

## Combined data at phecode level
diag_combine_agg_phecode_nhs_name = "ccu022_01_first_onset_combined_diagnosis_age40to69in2010to2020_PheCode_Cleaned_NHSID_2024_12_31"
diag_combine_agg_phecode_nhs = spark.read.table(db_collab + '.' + diag_combine_agg_phecode_nhs_name + file_date)

## Combined data at parent phecode level
diag_combine_agg_pheparent_nhs_name = "ccu022_01_first_onset_combined_diagnosis_age40to69in2010to2020_Parent_PheCode_Cleaned_NHSID_2024_12_31"
diag_combine_agg_pheparent_nhs = spark.read.table(db_collab + '.' + diag_combine_agg_pheparent_nhs_name + file_date)

# COMMAND ----------

# MAGIC %md
# MAGIC #### Limit to main study population - patients aged 40-69 on 2015-01-01 (1945-01-01 < yob <= 1975-01-01)

# COMMAND ----------

## Read pt characteristics table
df_demo = spark.table(params.path_demographics)

## Get date of death 
df_death = df_demo.select('person_id', 'date_of_death')

# COMMAND ----------

## Subset to pts alive on or after 2015-01-01
df_death_filter = df_death.filter("(date_of_death is null) OR (date_of_death >= '2015-01-01')")

## Restrict to pts aged 40-69 on 2015-01-01 (1945-01-01 < yob <= 1975-01-01) and alive on or after 2015-01-01
diag_combine_target = diag_combine_agg_phecode_nhs.join(df_death_filter, diag_combine_agg_phecode_nhs.PID == df_death_filter.person_id, how='inner').filter((F.col('date_of_birth') <= '1975-01-01') & (F.col('date_of_birth') > '1945-01-01')).drop('person_id')

# COMMAND ----------

diag_combine_target.select('PID').distinct().count()

# COMMAND ----------

## Save diag data for this population (phecode level)
db_collab = ""
diag_combine_target_name = "ccu022_01_first_onset_combined_diagnosis_age40to69in2015_PheCode_Cleaned_NHSID_2024_12_31"
diag_combine_target.write.option("overwriteSchema", "true").saveAsTable(name=(db_collab + '.' + diag_combine_target_name + file_date), mode="overwrite")

# COMMAND ----------

# MAGIC %md
# MAGIC #### Calculate the prevalence of all phecodes

# COMMAND ----------

## Read diag data for target study population of main analysis (phecode level)
db_collab = ""
diag_combine_target_name = "ccu022_01_first_onset_combined_diagnosis_age40to69in2015_PheCode_Cleaned_NHSID_2024_12_31"
diag_combine_target = spark.read.table(db_collab + '.' + diag_combine_target_name + file_date)

# COMMAND ----------

# MAGIC %md
# MAGIC ##### Get denominator for the prevalence

# COMMAND ----------

## Get pts with >=1 record from GDPPR or HES APC
db_collab = ""
first_onset_hes_apc_name = "ccu022_01_first_onset_hes_apc_diagnosis_2025_02_04"
first_onset_hes_apc = spark.read.table(db_collab + "." + first_onset_hes_apc_name + file_date)
hes_apc_pts = first_onset_hes_apc.select('person_id').distinct().withColumnRenamed('person_id', 'PID')

gdppr_pts = gdppr.select('NHS_NUMBER_DEID').distinct().withColumnRenamed('NHS_NUMBER_DEID', 'PID')

total_pts = gdppr_pts.union(hes_apc_pts).distinct()

# COMMAND ----------

## Limit pts to those with valid NHS number
id_lookup = spark.read.table(".token_pseudo_id_lookup")

## Get NHS IDs
nhs_id = id_lookup.filter(F.col('valid_nhs_number') == 'true').select('pseudo_id').distinct()

## limit diag data to NHS IDs
total_pts_nhs = total_pts.join(nhs_id, total_pts.PID == nhs_id.pseudo_id, how='inner').drop('pseudo_id')

# COMMAND ----------

## Limit pts to main study population
df_demo = spark.table(params.path_demographics)
df_demo_join = df_demo.select('person_id', 'date_of_birth', 'date_of_death')

## Restrict to pts aged 40-69 on 2015-01-01 (1945-01-01 < yob <= 1975-01-01) and alive on or after 2015-01-01
target_pts_nhs = total_pts_nhs.join(df_demo_join, total_pts_nhs.PID == df_demo_join.person_id, how='inner').filter((F.col('date_of_birth') <= '1975-01-01') & (F.col('date_of_birth') > '1945-01-01')).filter("(date_of_death is null) OR (date_of_death >= '2015-01-01')").drop('person_id')

n_denom = target_pts_nhs.count()
n_denom

# COMMAND ----------

# MAGIC %md
# MAGIC ##### Calculate prevalence

# COMMAND ----------

## Limit to diagnoses <= 2015-01-01
diag_combine_target_lifetime = diag_combine_target.filter(F.col('DATE') <= '2015-01-01')

diag_combine_target_lifetime.select(F.min('DATE'), F.max('DATE')).display()

# COMMAND ----------

## Get the number of cases for each PheCode
phecode_count = diag_combine_target_lifetime.groupBy('PheCode', 'Phenotype').count().select('PheCode', 'Phenotype','count').sort('count', ascending=False)

## Get prevalence for each PheCode
phecode_prev = phecode_count.withColumn('prevalence', F.col('count')/n_denom)
display(phecode_prev)

# COMMAND ----------

## Check number of PheCodes for different prevalence thresholds
phecode_prev.filter(F.col('prevalence') >= 0.1/100).count()  

# COMMAND ----------

phecode_prev.filter(F.col('prevalence') >= 0.2/100).count()  

# COMMAND ----------

phecode_prev.filter(F.col('prevalence') >= 0.5/100).count()   

# COMMAND ----------

phecode_prev.filter(F.col('prevalence') >= 1/100).count()  

# COMMAND ----------

# MAGIC %md
# MAGIC #### Save all phecodes

# COMMAND ----------

## Save list of all phecodes
phecode_prev_filter = phecode_prev

db_collab = ""
phecode_prev_filter_name = "ccu022_01_PheCode_codelist_All"
phecode_prev_filter.write.option("overwriteSchema", "true").saveAsTable(name=(db_collab + '.' + phecode_prev_filter_name + file_date), mode="overwrite")

# COMMAND ----------

# MAGIC %md
# MAGIC #### Save the list of phecodes with prevalence >= 0.1%

# COMMAND ----------

## Save list of phecodes with prevalence >= 0.1%
phecode_prev_filter = phecode_prev.filter(F.col('prevalence') >= 0.1/100)

db_collab = ""
phecode_prev_filter_name = "ccu022_01_PheCode_codelist_Prev0_001"
phecode_prev_filter.write.option("overwriteSchema", "true").saveAsTable(name=(db_collab + '.' + phecode_prev_filter_name + file_date), mode="overwrite")

# COMMAND ----------

# MAGIC %md
# MAGIC #### Save the list of phecodes with prevalence >= 0.2%

# COMMAND ----------

## Save list of phecodes with prevalence >= 0.2%
phecode_prev_filter = phecode_prev.filter(F.col('prevalence') >= 0.2/100)

db_collab = ""
phecode_prev_filter_name = "ccu022_01_PheCode_codelist_Prev0_002"
phecode_prev_filter.write.option("overwriteSchema", "true").saveAsTable(name=(db_collab + '.' + phecode_prev_filter_name + file_date), mode="overwrite")

# COMMAND ----------

# MAGIC %md
# MAGIC ### Selection of predictor diseases

# COMMAND ----------

# MAGIC %md
# MAGIC #### Read diag data and list of phecodes with prevalence >= 0.2%

# COMMAND ----------

## Read diag data for target study population of main analysis (phecode level)
db_collab = ""
diag_combine_target_name = "ccu022_01_first_onset_combined_diagnosis_age40to69in2015_PheCode_Cleaned_NHSID_2024_12_31"
diag_combine_target = spark.read.table(db_collab + '.' + diag_combine_target_name + file_date)

## Read list of phecodes with prevalence >= 0.2%
phecode_prev_filter_name = "ccu022_01_PheCode_codelist_Prev0_002"
phecode_prev_filter = spark.read.table(db_collab + '.' + phecode_prev_filter_name + file_date)
phecode_prev_filter.count()

# COMMAND ----------

display(phecode_prev_filter)

# COMMAND ----------

# DBTITLE 1,Get % of all diagnoses covered by selected predictor phecodes
n_all_diag = diag_combine_target.count()
n_all_diag

# COMMAND ----------

diag_combine_target_PredOnly = phecode_prev_filter.join(diag_combine_target, on = 'PheCode', how = 'inner')
n_pred_diag = diag_combine_target_PredOnly.count()
n_pred_diag

# COMMAND ----------

n_pred_diag/n_all_diag * 100

# COMMAND ----------

# MAGIC %md
# MAGIC #### Aggregate selected phecodes at parent level

# COMMAND ----------

## Aggregate selected phecodes at parent level
diag_combine_phecode_info = diag_combine_target.select('PheCode', 'PheCode_parent', 'Phenotype_parent').distinct()

phecode_parent_filter = phecode_prev_filter.join(diag_combine_phecode_info, on = 'PheCode', how = 'inner').select('PheCode_parent', 'Phenotype_parent').distinct()
phecode_parent_filter.display()

# COMMAND ----------

# MAGIC %md
# MAGIC #### Save the list of predictor diseases

# COMMAND ----------

## Remove phecode 1010
phecode_parent_filter_final  = phecode_parent_filter.filter(F.col('PheCode_parent') != '1010')

db_collab = ""
phecode_parent_filter_name = "ccu022_01_predictor_disease_PheParent_codelist_Prev0_002"
phecode_parent_filter_final.write.option("overwriteSchema", "true").saveAsTable(name=(db_collab + '.' + phecode_parent_filter_name + file_date), mode="overwrite")

# COMMAND ----------

phecode_parent_filter_final.count()

# COMMAND ----------

# MAGIC %md
# MAGIC ### Save all parent phecode-level prevalence

# COMMAND ----------

# MAGIC %md
# MAGIC #### Read diag data and list of all phecodes

# COMMAND ----------

## Read diag data for target study population of main analysis (phecode level)
db_collab = ""
diag_combine_target_name = "ccu022_01_first_onset_combined_diagnosis_age40to69in2015_PheCode_Cleaned_NHSID_2024_12_31"
diag_combine_target = spark.read.table(db_collab + '.' + diag_combine_target_name + file_date)

# COMMAND ----------

## Limit to target study population for parent phecode data
target_pop = diag_combine_target.select('PID').distinct()
diag_combine_pheparent_target = diag_combine_agg_pheparent_nhs.join(target_pop, on = "PID", how = "inner")
diag_combine_pheparent_target.select('PID').distinct().count()

# COMMAND ----------

## Limit to diagnoses <= 2015-01-01
diag_combine_pheparent_target_lifetime = diag_combine_pheparent_target.filter(F.col('DATE') <= '2015-01-01')

diag_combine_pheparent_target_lifetime.select(F.min('DATE'), F.max('DATE')).display()

# COMMAND ----------

## Get the number of cases for each PheCode
pheparent_count = diag_combine_pheparent_target_lifetime.groupBy('PheCode_parent', 'Phenotype_parent').count().select('PheCode_parent', 'Phenotype_parent','count').sort('count', ascending=False)

## Get prevalence for each parent PheCode
pheparent_prev = pheparent_count.withColumn('prevalence', F.col('count')/n_denom)
display(pheparent_prev)

# COMMAND ----------

# MAGIC %md
# MAGIC #### Save the list of parent phecodes with prevalence

# COMMAND ----------

db_collab = ""
pheparent_prev_name = "ccu022_01_All_PheParent_codelist_with_prevalence"
pheparent_prev.write.option("overwriteSchema", "true").saveAsTable(name=(db_collab + '.' + pheparent_prev_name + file_date), mode="overwrite")

# COMMAND ----------

# MAGIC %md
# MAGIC ### Selection of outcome diseases

# COMMAND ----------

# MAGIC %md
# MAGIC #### Read list of phecodes with prevalence >= 0.2%

# COMMAND ----------

## Read list of phecodes with prevalence >= 0.2%
db_collab = ""
phecode_prev_filter_name = "ccu022_01_PheCode_codelist_Prev0_002"
phecode_prev_filter = spark.read.table(db_collab + '.' + phecode_prev_filter_name + file_date)

# COMMAND ----------

# MAGIC %md
# MAGIC #### Process HES APC data

# COMMAND ----------

## Read HES APC diag data (Cell 63 from "Process and combine HES APC and GDPPR")
db_collab = ""
hes_apc_diag_name = "ccu022_01_first_onset_hes_apc_diagnosis_age40to69in2010to2020_PheCode_MappedtoICD10AtoN_2025_02_04"
hes_apc_diag = spark.read.table(db_collab + '.' + hes_apc_diag_name + file_date)

# COMMAND ----------

## Limit pts to those with valid NHS number
id_lookup = spark.read.table(".token_pseudo_id_lookup")

## Get NHS IDs
nhs_id = id_lookup.filter(F.col('valid_nhs_number') == 'true').select('pseudo_id').distinct()

## limit diag data to NHS IDs
hes_apc_diag_nhs = hes_apc_diag.join(nhs_id, hes_apc_diag.person_id == nhs_id.pseudo_id, how='inner').drop('pseudo_id')

# COMMAND ----------

## Get date of death info
df_demo = spark.table(params.path_demographics)
df_death = df_demo.select('person_id', 'date_of_death')

## Subset to pts alive on or after 2015-01-01
df_death_filter = df_death.filter("(date_of_death is null) OR (date_of_death >= '2015-01-01')")

## Restrict to pts aged 40-69 on 2015-01-01 (1945-01-01 < yob <= 1975-01-01) and alive on or after 2015-01-01
hes_apc_diag_nhs_target = hes_apc_diag_nhs.join(df_death_filter, on='person_id', how='inner').filter((F.col('date_of_birth') <= '1975-01-01') & (F.col('date_of_birth') > '1945-01-01'))

# COMMAND ----------

hes_apc_diag_nhs_target.select('person_id').distinct().count()

# COMMAND ----------

## Save processed HES APC data
db_collab = ""
hes_apc_diag_nhs_target_name = "ccu022_01_first_onset_hes_apc_diagnosis_age40to69in2015_PheCode_NHSID_2024_12_31"
hes_apc_diag_nhs_target.write.option("overwriteSchema", "true").saveAsTable(name=(db_collab + '.' + hes_apc_diag_nhs_target_name + file_date), mode="overwrite")

# COMMAND ----------

# MAGIC %md
# MAGIC #### 1. Filter based on ratio of primary diagnoses per disease

# COMMAND ----------

## Read HES APC diag data for target population
db_collab = ""
hes_apc_diag_nhs_target_name = "ccu022_01_first_onset_hes_apc_diagnosis_age40to69in2015_PheCode_NHSID_2024_12_31"
hes_apc_diag_nhs_target = spark.read.table(db_collab + '.' + hes_apc_diag_nhs_target_name + file_date)

# COMMAND ----------

## Calculate the proportion of primary diagnosis for each phecode
## Primary diagnoses are indicated by diag_position = 1
primary_cts = hes_apc_diag_nhs_target.filter(F.col("diag_position") == 1).groupBy('PheCode', 'Phenotype').count().withColumnRenamed('count', 'primary_diag_count').select('PheCode', 'Phenotype', 'primary_diag_count')
all_cts = hes_apc_diag_nhs_target.groupBy('PheCode').count().withColumnRenamed('count', 'all_diag_count').select('PheCode', 'all_diag_count')
primary_prop = primary_cts.join(all_cts, on="PheCode", how="left").withColumn("prop_primary", F.col('primary_diag_count')/F.col('all_diag_count')).withColumn("primary_diag_count_million", F.col("primary_diag_count")/10**6).withColumn("all_diag_count_million", F.col("all_diag_count")/10**6).drop("primary_diag_count", "all_diag_count").orderBy(F.col("prop_primary").desc()) 

display(primary_prop)

# COMMAND ----------

## Filter to diseases with proportion of primary diagnosis >= 50%
target_ds_primary50 = primary_prop.filter(F.col('prop_primary') >= 0.5).orderBy(F.col('prop_primary').desc())
display(target_ds_primary50)

# COMMAND ----------

## Save the candidate disease outcomes
db_collab = ""
target_ds_primary50_name = "ccu022_01_PheCode_codelist_primary50"
target_ds_primary50.write.saveAsTable(name=(db_collab + '.' + target_ds_primary50_name + file_date), mode="overwrite")

# COMMAND ----------

# MAGIC %md
# MAGIC ##### Filter based on number of cases during follow-up

# COMMAND ----------

## Read candidate outcome diseases
db_collab = ""
target_ds_primary50_name = "ccu022_01_PheCode_codelist_primary50"
target_ds_primary50 = spark.read.table(db_collab + '.' + target_ds_primary50_name + file_date)

target_ds_primary50.count()

# COMMAND ----------

## Read combined diag data for target population
db_collab = ""
diag_combine_target_name = "ccu022_01_first_onset_combined_diagnosis_age40to69in2015_PheCode_Cleaned_NHSID_2024_12_31"
diag_combine_target = spark.read.table(db_collab + '.' + diag_combine_target_name + file_date)

# COMMAND ----------

## Limit to phecodes for candidate outcomes
diag_combine_target_ds = diag_combine_target.join(target_ds_primary50.select('PheCode'), on="PheCode", how="inner")

## Limit to dates on or after 2015-01-01
diag_combine_target_ds_fup = diag_combine_target_ds.filter(F.col("DATE") >= F.lit("2015-01-01"))
diag_combine_target_ds_fup.select(F.min('DATE'), F.max('DATE')).display()

# COMMAND ----------

## Get the number of cases for each PheCode
fup_phecode_count = diag_combine_target_ds_fup.groupBy('PheCode', 'Phenotype').count().select('PheCode', 'Phenotype','count').sort('count', ascending=False)

display(fup_phecode_count)

# COMMAND ----------

target_ds_primary50.select('PheCode').join(fup_phecode_count.select('PheCode'), on = "PheCode", how = "leftanti").display()

# COMMAND ----------

fup_phecode_count.select('count').summary().show()

# COMMAND ----------

n_outcome_ds = fup_phecode_count.filter(F.col('count') >= 150000).count()
n_outcome_ds

# COMMAND ----------

## Get list of outcomes, filter out symptoms, filter out non-specific outcomes, remove 681.5 and 681.6
outcome_fup_count = fup_phecode_count.filter(F.col('count') >= 150000).drop('Phenotype').withColumnRenamed('count','fup_count')

phecode_info = diag_combine_target_ds_fup.select('PheCode', 'PheCode_parent', 'Phenotype_parent', 'Category').distinct()
outcome_ds_final = target_ds_primary50.join(outcome_fup_count, on='PheCode', how='inner').join(phecode_info, on = 'PheCode', how = 'left').filter(F.col('Category') != "symptoms").filter(~F.col('Phenotype').startswith('Other')).filter(~F.col('PheCode').isin(['681.5', '681.6']))

outcome_ds_final.display()


# COMMAND ----------

## distinct number of parent phecodes
outcome_ds_final.select('PheCode_parent').distinct().count()

# COMMAND ----------

## Get most common outcome (based on case count during follow-up) per category
w = Window.partitionBy('category').orderBy(F.desc('fup_count'))
outcome_ds_final.withColumn('rank', F.row_number().over(w)).filter(F.col('rank') <= 1).drop('rank').display()

# COMMAND ----------

# MAGIC %md
# MAGIC ##### Save the list of outcome diseases
# MAGIC

# COMMAND ----------

outcome_ds_final.count()

db_collab = ""
outcome_ds_final_name = "ccu022_01_outcome_disease_PheCode_Primary50_FollowupCount150000"
outcome_ds_final.write.saveAsTable(name=(db_collab + '.' + outcome_ds_final_name + file_date), mode="overwrite")

# COMMAND ----------

# MAGIC %md
# MAGIC #### 2. Directly filter based on number of cases during follow-up

# COMMAND ----------

## Read combined diag data for target population
db_collab = ""
diag_combine_target_name = "ccu022_01_first_onset_combined_diagnosis_age40to69in2015_PheCode_Cleaned_NHSID_2024_12_31"
diag_combine_target = spark.read.table(db_collab + '.' + diag_combine_target_name + file_date)

# COMMAND ----------

## Limit to dates on or after 2015-01-01
diag_combine_target_ds_fup = diag_combine_target.filter(F.col("DATE") >= F.lit("2015-01-01"))
diag_combine_target_ds_fup.select(F.min('DATE'), F.max('DATE')).display()

# COMMAND ----------

## Get the number of cases for each PheCode
fup_phecode_count = diag_combine_target_ds_fup.groupBy('PheCode', 'Phenotype').count().select('PheCode', 'Phenotype','count').sort('count', ascending=False)

display(fup_phecode_count)

# COMMAND ----------

n_outcome_ds = fup_phecode_count.filter(F.col('count') >= 150000).count()
n_outcome_ds

# COMMAND ----------

## Get list of outcomes, filter out symptoms, filter out non-specific outcomes, remove 681.5 and 681.6
outcome_fup_count = fup_phecode_count.filter(F.col('count') >= 150000).withColumnRenamed('count','fup_count')

phecode_info = diag_combine_target_ds_fup.select('PheCode', 'PheCode_parent', 'Phenotype_parent', 'Category').distinct()

outcome_ds_select = outcome_fup_count.join(phecode_info, on = 'PheCode', how = 'left').filter(F.col('Category') != "symptoms").filter(~F.col('Phenotype').startswith('Other')).filter(~F.col('PheCode').isin(['681.5', '681.6'])).withColumn('Incidence', F.col('fup_count')/n_denom).orderBy(F.desc('Incidence'))

outcome_ds_select.display()

# COMMAND ----------

## Select top 10 common outcomes (removing parent codes if also present) and PheRS outcomes
outcome_ds_final2 = outcome_ds_select.filter(F.col('PheCode').isin(['401.1', '278.1', '465', '716.9', '250.2', '272.1', '318', '300.1', '530.1', '550', '427.2', '411.4', '495', '274.1'])).withColumn("HES_Primary_50perc", F.lit(0))
outcome_ds_final2.display()

# COMMAND ----------

# DBTITLE 1,Save the list of outcomes
outcome_ds_final2.count()

db_collab = ""
outcome_ds_final2_name = "ccu022_01_outcome_disease_PheCode_FollowupCount150000_TopCommonPlusPheRS"
outcome_ds_final2.write.saveAsTable(name=(db_collab + '.' + outcome_ds_final2_name + file_date), mode="overwrite")