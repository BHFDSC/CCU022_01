# Databricks notebook source
# MAGIC %md
# MAGIC ### Preparation of QCovid predictor data
# MAGIC ##### Pearl Liu adapted from Project CCU051

# COMMAND ----------

# MAGIC %md # CCU051_D09a_covariates
# MAGIC
# MAGIC **Description** This notebook creates the covariates for CCU051.
# MAGIC  * BMI
# MAGIC  * Shielding groups
# MAGIC  * QCovid items
# MAGIC  * Any other hospitalisations (that is not Covid)
# MAGIC   
# MAGIC **Author(s)** Tom Bolton, Genevieve Cezard
# MAGIC
# MAGIC **Project** CCU051
# MAGIC
# MAGIC **First copied over** 2022.11.29 (from CCU002_07)
# MAGIC
# MAGIC **Date last updated** 2023.03.12
# MAGIC
# MAGIC **Date last run** 2023.03.12
# MAGIC
# MAGIC **Data input** functions - libraries - parameters\
# MAGIC codelist_qcovid - codelist_covariates\
# MAGIC 'ccu051_tmp_cohort'\
# MAGIC gdppr - hes_apc_long - hes_apc_oper_long
# MAGIC
# MAGIC **Data output** - 'CCU051_out_covariates'
# MAGIC
# MAGIC **Acknowledgements** Adapted from previous work by Tom Bolton (John Nolan, Elena Raffetti, Alexia Sampri) from CCU002_07, CCU018_01 and earlier CCU002 sub-projects.

# COMMAND ----------

spark.sql('CLEAR CACHE')

# COMMAND ----------

# DBTITLE 1,Libraries
import pyspark.sql.functions as f
import pyspark.sql.types as t
from pyspark.sql import Window

from functools import reduce

#import databricks.koalas as ks
import pyspark.pandas as pd
import numpy as np

import re
import io
import datetime

import matplotlib
import matplotlib.pyplot as plt
from matplotlib import dates as mdates
import seaborn as sns

print("Matplotlib version: ", matplotlib.__version__)
print("Seaborn version: ", sns.__version__)
_datetimenow = datetime.datetime.now() # .strftime("%Y%m%d")
print(f"_datetimenow:  {_datetimenow}")

# COMMAND ----------

# DBTITLE 1,Functions
# MAGIC %run "./General_functions"

# COMMAND ----------

# MAGIC %md # 0. Parameters

# COMMAND ----------

# MAGIC %run "./CCU051_D01_parameters"

# COMMAND ----------

# MAGIC %md # 1. Data

# COMMAND ----------

# DBTITLE 1,Codelists and data sources
# codelists
spark.sql(f'REFRESH TABLE {path_out_codelist_qcovid}')
codelist_qcovid = spark.table(path_out_codelist_qcovid)

spark.sql(f'REFRESH TABLE {dbc}.{proj}_out_codelist_covariates')
codelist_covariates = spark.table(f'{dbc}.{proj}_out_codelist_covariates')

# cohort - PL updated below
#spark.sql(f'REFRESH TABLE {dbc}.{proj}_out_cohort')
#cohort = spark.table(f'{dbc}.{proj}_out_cohort')

# data sources
gdppr = extract_batch_from_archive(parameters_df_datasets, 'gdppr')
spark.sql(f'REFRESH TABLE {path_cur_hes_apc_long}')
hes_apc_long = spark.table(path_cur_hes_apc_long)
spark.sql(f'REFRESH TABLE {path_cur_hes_apc_oper_long}')
hes_apc_oper_long = spark.table(path_cur_hes_apc_oper_long)
# pmeds = extract_batch_from_archive(parameters_df_datasets, 'pmeds')
# note: pmeds not required since no additional DMD codes captured in pmeds that are not being captured in gdppr (see codelist_qcovid notebook)

# COMMAND ----------

# DBTITLE 1,Include patient cohort with index 2020
analysis_df_final_name = "ccu022_01_WhiteBritish_Age40_69_Index2020_StudyPopInfo"
file_date = "_by_PL_2025_05_01"
cohort = spark.read.table(dbc + '.' + analysis_df_final_name + file_date)

# COMMAND ----------

# MAGIC %md # 2. Check

# COMMAND ----------

# MAGIC %md ## 2.1. Codelist

# COMMAND ----------

# DBTITLE 1,BMI
# check
display(codelist_covariates.orderBy('name', 'terminology', 'code'))

# COMMAND ----------

# check
tmpt = tab(codelist_covariates, 'name', 'terminology'); print()

# COMMAND ----------

# DBTITLE 1,Qcovid items
# check
display(codelist_qcovid.orderBy('name', 'terminology', 'code'))

# COMMAND ----------

# check
tmpt = tab(codelist_qcovid, 'name'); print()
tmpt = tab(codelist_qcovid, 'name', 'terminology'); print()

# COMMAND ----------

# MAGIC %md ## 2.2. Cohort

# COMMAND ----------

# DBTITLE 1,Add baseline date
## Baseline date is 2020-01-01 - 3 months
cohort = cohort.withColumn("baseline_date", f.to_date(f.lit('2019-10-01')))

# COMMAND ----------

# check
display(cohort)

# COMMAND ----------

# check
count_var(cohort, 'PERSON_ID'); print()
tmpt = tabstat(cohort, 'baseline_date', date=1); print()
tmpt = tabstat(cohort, 'DOB', date=1); print()
tmpt = tabstat(cohort, 'AGE'); print()
tmpt = tab(cohort, 'SEX'); print()
tmpt = tab(cohort, 'ETHNIC_CAT'); print()
tmpt = tab(cohort, 'region'); print()
tmpt = tab(cohort, 'IMD_2019_deciles', 'IMD_2019_quintiles'); print()
tmpt = tab(cohort, 'RUC11_bin'); print()
tmpt = tabstat(cohort, 'DOD', date=1); print()

# COMMAND ----------

tmpp = (
  cohort
  .withColumn('DOB_ym', f.date_format(f.col('DOB'), 'yyyy-MM'))
  .groupBy('DOB_ym')
  .agg(f.count(f.lit(1)).alias('n'))
  .toPandas()
)
tmpp['date_formatted'] = pd.to_datetime(tmpp['DOB_ym'], errors='coerce')
# display(tmpp)

# plot
plt.rcParams.update({'font.size': 8})
fig, axes = plt.subplots(1, 1, figsize=(15,5), sharex=False) # was 4.75 for 2

axes.bar(tmpp['date_formatted'], tmpp['n'], width = 365, edgecolor = 'black')
# axes.set(xticks=np.arange(0, 19, step=1))
# axes.set_xlim(datetime.datetime(1904, 11, 1), datetime.datetime(2019, 11, 1))
axes.set(xlabel="DOB_ym")
axes.set(ylabel="Number of individuals")
axes.spines['right'].set_visible(False)
axes.spines['top'].set_visible(False)
display(fig)

# COMMAND ----------

tmpp = (
  cohort
  .groupBy('DOD')
  .agg(f.count(f.lit(1)).alias('n'))
  .toPandas()
)
tmpp['date_formatted'] = pd.to_datetime(tmpp['DOD'], errors='coerce')

# plot
plt.rcParams.update({'font.size': 8})
fig, axes = plt.subplots(1, 1, figsize=(15,5), sharex=False) # was 4.75 for 2

axes.bar(tmpp['date_formatted'], tmpp['n'], width = 1, edgecolor = 'Black')
# axes.set(xticks=np.arange(0, 19, step=1))
# axes.set_xlim(datetime.datetime(1904, 11, 1), datetime.datetime(2019, 11, 1))
axes.set(xlabel="DOD")
axes.set(ylabel="Number of individuals")
axes.spines['right'].set_visible(False)
axes.spines['top'].set_visible(False)
display(fig)

# COMMAND ----------

# MAGIC %md ## 2.3. Source data

# COMMAND ----------

# print('---------------------------------------------------------------------------------')
# print(f'gdppr') 
# print('---------------------------------------------------------------------------------')
# count_var(gdppr, 'NHS_NUMBER_DEID'); print()
# tmpt = tab(gdppr, 'archived_on'); print()


# print('---------------------------------------------------------------------------------')
# print(f'hes_apc_long') 
# print('---------------------------------------------------------------------------------')
# count_var(hes_apc_long, 'PERSON_ID_DEID'); print()
# tmpt = tab(hes_apc, 'ProductionDate'); print()
# tmpt = tab(hes_apc, 'ProjectCopyDate'); print()

# print('---------------------------------------------------------------------------------')
# print(f'hes_apc_oper_long') 
# print('---------------------------------------------------------------------------------')
# count_var(hes_apc_long, 'PERSON_ID_DEID'); print()
# tmpt = tab(hes_apc, 'ProductionDate'); print()
# tmpt = tab(hes_apc, 'ProjectCopyDate'); print()


# print('---------------------------------------------------------------------------------')
# print(f'pmeds') 
# print('---------------------------------------------------------------------------------')
# count_var(pmeds, 'Person_ID_DEID'); print()
# tmpt = tab(pmeds, 'archived_on'); print()

"""
---------------------------------------------------------------------------------
gdppr
---------------------------------------------------------------------------------
counts (gdppr)
  N(rows)                                     =  
  N(non-missing NHS_NUMBER_DEID)              =  
  N(non-missing and distinct NHS_NUMBER_DEID) =     

      archived_on              N    PCT
0      2022-12-31    100.0
Total                100.0
"""

# COMMAND ----------

# display(gdppr.where((f.col('NHS_NUMBER_DEID') == '') & (f.col('CODE') == '320884002')))

# COMMAND ----------

# DBTITLE 1,HES APC
display(hes_apc_long)

# COMMAND ----------

# DBTITLE 1,HES APC OPER
display(hes_apc_oper_long)

# COMMAND ----------

# DBTITLE 1,GDPPR
display(gdppr)

# COMMAND ----------

# MAGIC %md # 3. Prepare

# COMMAND ----------

# MAGIC %md ## 3.1. Codelist

# COMMAND ----------

# check
count_varlist(codelist_qcovid, ['name'])
count_varlist(codelist_covariates, ['name'])

# append
tmp1 = (
  codelist_qcovid
  .withColumn('value', f.lit(None))
)
tmp2 = (
  codelist_covariates
  .withColumn('name_orig', f.lit(None))
)
codelist_prepared = (
  tmp1
  .unionByName(tmp2)
)
 
# check
count_varlist(codelist_prepared, ['name'])
count_varlist(codelist_prepared, ['name', 'terminology', 'code'])
tmpt = tab(codelist_prepared, 'name', 'terminology'); print()
print(codelist_prepared.orderBy('name', 'terminology', 'code').limit(10).toPandas().to_string()); print()

# COMMAND ----------

# MAGIC %md ## 3.2. Cohort

# COMMAND ----------

print('--------------------------------------------------------------------------------------')
print('individual_censor_dates - lookback to identify covariates')
print('--------------------------------------------------------------------------------------')
# print(proj_start_date)
# , f.to_date(f.lit(proj_start_date)))

# PL 20251108 update
# CENSOR_DATE_END is the end of the lookback, so Baseline date i.e. 2019-10-01
# CENSOR_DATE_START is the start of the lookback, set up as DOB
individual_censor_dates = (
  cohort
  .select('person_id', 'baseline_date', 'date_of_birth') 
  .withColumnRenamed('baseline_date', 'CENSOR_DATE_END') 
  .withColumn('CENSOR_DATE_START', f.col('date_of_birth'))
  .drop('date_of_birth')
)

# check
count_var(individual_censor_dates, 'person_id'); print()
tmpt = tabstat(individual_censor_dates, 'CENSOR_DATE_START', date=1); print()
tmpt = tabstat(individual_censor_dates, 'CENSOR_DATE_END', date=1); print()
print(individual_censor_dates.limit(10).toPandas().to_string()); print()

# COMMAND ----------

# MAGIC %md ## 3.3. Source data

# COMMAND ----------

# DBTITLE 1,GDPPR
# copied from CCU003_05

# # # # NOTE # # # #
# The covariates in this notebook are defined according to the last 1 or 3 years of data
# therefore we only use DATE and do not utilise RECORD_DATE as done previously
# because RECORD_DATE could relate to a DATE that is outside of the last 1 or 3 years
# e.g., DATE is null, RECORD_DATE is as at last 1 year date - but the true DATE could be before RECORD_DATE...

print('---------------------------------------------------------------------------------')
print(f'gdppr') 
print('---------------------------------------------------------------------------------')
# reduce and rename columns
# remove nulls
# 20230306 RECORD_DATE added for use with medication counts
#          for now, we ignore the fact that we filter by DATE below
gdppr_1 = (
  gdppr
  .select(f.col('NHS_NUMBER_DEID').alias('person_id'), 'DATE', 'CODE', 'VALUE1_CONDITION', 'RECORD_DATE')
  .where(f.col('person_id').isNotNull())  
  .where(f.col('DATE').isNotNull())
  .withColumn('mono_id', f.monotonically_increasing_id())
)

# add individual censor dates
# gdppr_2 = merge(gdppr_1, individual_censor_dates, ['person_id'], validate='m:1', keep_results=['both'], indicator=0); print()
gdppr_2 = (
  gdppr_1
  .join(individual_censor_dates, on=['person_id'], how='inner')
)
  
# check
# count_var(gdppr_2, 'person_id'); print()  
  
# filter
gdppr_3 = (
  gdppr_2
  .where(f.col('DATE') >= f.col('CENSOR_DATE_START')) 
  .where(f.col('DATE') <= f.col('CENSOR_DATE_END'))
  .withColumn('_diff', f.datediff(f.col('CENSOR_DATE_END'), f.col('DATE')))
)

# temp save
gdppr_prepared = temp_save(df=gdppr_3, out_name=f'{proj}_tmp_covariates_gdppr'); print() 

# check
count_var(gdppr_prepared, 'person_id'); print()
tmpt = tabstat(gdppr_prepared, 'DATE', date=1); print()  
tmpt = tabstat(gdppr_prepared, '_diff'); print()  
  
# tidy
gdppr_prepared = (
  gdppr_prepared
  .drop('_diff')
)

# check
print(gdppr_prepared.limit(10).toPandas().to_string()); print()

# 18 mins

# COMMAND ----------

# DBTITLE 1,HES APC
print('---------------------------------------------------------------------------------')
print(f'hes_apc_long') 
print('---------------------------------------------------------------------------------')
# # reduce and rename columns
# # remove nulls
hes_apc_long_1 = (
   hes_apc_long
   .select(['person_id', f.col('epistart').alias('DATE'), 'code'])
   .where(f.col('person_id').isNotNull())  
   .where(f.col('DATE').isNotNull())
)

# # add individual censor dates
# # hes_apc_long_2 = merge(hes_apc_long_1, individual_censor_dates, ['person_id'], validate='m:1', keep_results=['both'], indicator=0); print()
hes_apc_long_2 = (
   hes_apc_long_1
   .join(individual_censor_dates, on=['person_id'], how='inner')
)
  
# # # check
# # count_var(hes_apc_long_2, 'person_id'); print()  
  
# # filter
hes_apc_long_3 = (
   hes_apc_long_2
   .where(f.col('DATE') >= f.col('CENSOR_DATE_START')) 
   .where(f.col('DATE') <= f.col('CENSOR_DATE_END'))
   .withColumn('_diff', f.datediff(f.col('CENSOR_DATE_END'), f.col('DATE')))
)

# # temp save
hes_apc_long_prepared = temp_save(df=hes_apc_long_3, out_name=f'{proj}_tmp_covariates_hes_apc'); print() 

# # check
count_var(hes_apc_long_prepared, 'person_id'); print()
tmpt = tabstat(hes_apc_long_prepared, 'DATE', date=1); print()  
tmpt = tabstat(hes_apc_long_prepared, '_diff'); print()  
  
# # tidy
hes_apc_long_prepared = (
   hes_apc_long_prepared
   .drop('_diff')
)

# # check
print(hes_apc_long_prepared.limit(10).toPandas().to_string()); print()

# 3 mins

# COMMAND ----------

# DBTITLE 1,HES APC OPER
print('---------------------------------------------------------------------------------')
print(f'hes_apc_oper_long') 
print('---------------------------------------------------------------------------------')
# # reduce and rename columns
# # remove nulls
hes_apc_oper_long_1 = (
   hes_apc_oper_long
   .select(['person_id', f.col('epistart').alias('DATE'), 'code'])
   .where(f.col('person_id').isNotNull())  
   .where(f.col('DATE').isNotNull())
)

# # add individual censor dates
# # hes_apc_oper_long_2 = merge(hes_apc_oper_long_1, individual_censor_dates, ['person_id'], validate='m:1', keep_results=['both'], indicator=0); print()
hes_apc_oper_long_2 = (
   hes_apc_oper_long_1
   .join(individual_censor_dates, on=['person_id'], how='inner')
)
  
# # # check
# # count_var(hes_apc_oper_long_2, 'PERSON_ID'); print()  
  
# # filter
hes_apc_oper_long_3 = (
   hes_apc_oper_long_2
   .where(f.col('DATE') >= f.col('CENSOR_DATE_START')) 
   .where(f.col('DATE') <= f.col('CENSOR_DATE_END'))
   .withColumn('_diff', f.datediff(f.col('CENSOR_DATE_END'), f.col('DATE')))
)

# # temp save
hes_apc_oper_long_prepared = temp_save(df=hes_apc_oper_long_3, out_name=f'{proj}_tmp_covariates_hes_apc_oper'); print() 

# # check
count_var(hes_apc_oper_long_prepared, 'person_id'); print()
tmpt = tabstat(hes_apc_oper_long_prepared, 'DATE', date=1); print()  
tmpt = tabstat(hes_apc_oper_long_prepared, '_diff'); print()  
  
# # tidy
hes_apc_oper_long_prepared = (
   hes_apc_oper_long_prepared
   .drop('_diff')
)

# # check
print(hes_apc_oper_long_prepared.limit(10).toPandas().to_string()); print()

# 1 mins

# COMMAND ----------

# DBTITLE 1,No need for pmeds
# # print('--------------------------------------------------------------------------------------')
# # print('pmeds')
# # print('--------------------------------------------------------------------------------------')
# # reduce and rename columns
# _pmeds = pmeds\
#   .select(['Person_ID_DEID', 'ProcessingPeriodDate', 'PrescribedBNFCode'])\
#   .withColumnRenamed('Person_ID_DEID', 'PERSON_ID')\
#   .withColumnRenamed('ProcessingPeriodDate', 'DATE')\
#   .withColumnRenamed('PrescribedBNFCode', 'CODE')

# # check
# count_var(_pmeds, 'PERSON_ID'); print()

# # add censor dates
# _pmeds = _pmeds\
#   .join(individual_censor_dates, on='PERSON_ID', how='inner')

# # check
# count_var(_pmeds, 'PERSON_ID'); print()

# # check before CENSOR_DATE_END, accounting for nulls
# # note: checked in curated_data for potential columns to use in the case of null DATE - none
# # ...
# _pmeds = _pmeds\
#   .withColumn('_tmp1',\
#     f.when((f.col('DATE').isNull()), 1)\
#      .when((f.col('DATE').isNotNull()) & (f.col('DATE') <= f.col('CENSOR_DATE_END')), 2)\
#      .when((f.col('DATE').isNotNull()) & (f.col('DATE') >  f.col('CENSOR_DATE_END')), 3)\
#   )
# tmpt = tab(_pmeds, '_tmp1'); print()

# # filter to before CENSOR_DATE_END
# # ...
# _pmeds = _pmeds\
#   .where(f.col('_tmp1').isin([2]))\
#   .drop('_tmp1')

# # check
# count_var(_pmeds, 'PERSON_ID'); print()

# # check on or after CENSOR_DATE_END_less_180d
# # ...
# _pmeds = _pmeds\
#   .withColumn('CENSOR_DATE_END_less_90d', f.date_add(f.col('CENSOR_DATE_END'), -90))\
#   .withColumn('_tmp2',\
#     f.when((f.col('DATE') >= f.col('CENSOR_DATE_END_less_90d')), 1)\
#      .when((f.col('DATE') <  f.col('CENSOR_DATE_END_less_90d')), 2)\
#   )
# tmpt = tab(_pmeds, '_tmp2'); print()

# # filter to on or after CENSOR_DATE_END_less_12m
# # ...
# _pmeds = _pmeds\
#   .where(f.col('_tmp2').isin([1]))\
#   .drop('_tmp2')

# # check
# count_var(_pmeds, 'PERSON_ID'); print()
# print(_pmeds.limit(10).toPandas().to_string()); print()

# COMMAND ----------

# DBTITLE 1,Read saved data
gdppr_prepared = (
  spark.table(f'{dbc}.{proj}_tmp_covariates_gdppr')
  .drop('_diff')
)
hes_apc_long_prepared = (
  spark.table(f'{dbc}.{proj}_tmp_covariates_hes_apc')
  .drop('_diff')
)
hes_apc_oper_long_prepared = (
  spark.table(f'{dbc}.{proj}_tmp_covariates_hes_apc_oper')
  .drop('_diff')
)

# COMMAND ----------

# MAGIC %md # 4. Codelist match

# COMMAND ----------

# MAGIC %md ## 4.1. Codelist

# COMMAND ----------

# check terminologies
tmpt = tab(codelist_prepared, 'name', 'terminology'); print()
list_terminology = (
  list(codelist_covariates
    .select('terminology')
    .distinct()
    .toPandas()['terminology']
  )
)
assert set(list_terminology) <= set(['SNOMED', 'DMD', 'ICD10', 'OPCS4', 'BNF'])

# partition codelist 
codelist_snomed_dmd = (
  codelist_prepared
  .where(f.col('terminology').isin(['SNOMED', 'DMD']))
)
codelist_icd10 = (
  codelist_prepared
  .where(f.col('terminology').isin(['ICD10']))
)
codelist_opcs4 = (
  codelist_prepared
  .where(f.col('terminology').isin(['OPCS4']))
)
# codelist_bnf = (
#   codelist_prepared
#   .where(f.col('terminology').isin(['BNF']))
# )

# check
tmpt = tab(codelist_snomed_dmd, 'name', 'terminology'); print()
tmpt = tab(codelist_icd10, 'name', 'terminology'); print()
tmpt = tab(codelist_opcs4, 'name', 'terminology'); print()
# tmpt = tab(codelist_bnf, 'name', 'terminology'); print()

# COMMAND ----------

# MAGIC %md ## 4.2. Create

# COMMAND ----------

# dictionary - key: dataset, codelist, and ordering in the event of tied records
dict_input = {
    'gdppr':        ['gdppr_prepared', 'codelist_snomed_dmd', 3]
  , 'hes_apc':      ['hes_apc_long_prepared', 'codelist_icd10', 1]
  , 'hes_apc_oper': ['hes_apc_oper_long_prepared', 'codelist_opcs4', 2]
}

# run codelist match and codelist match summary functions
## PL 20251110: modify to output wide dataframe (set stages_to_run = 3)
dict_all, df_1st, df_1st_wide = codelist_match_v2_test(dict_input, _name_prefix=f'cov_', codelist_cols=['value'], stages_to_run=3); print()
df_all_summ_name, df_all_summ_name_code = codelist_match_summ(dict_input, dict_all); print()

# COMMAND ----------

# DBTITLE 1,Save output
# temp save
df_all = dict_all['all']
df_all = temp_save(df=df_all, out_name=f'{proj}_QCovid_covariates_df_all'); print()
df_1st = temp_save(df=df_1st, out_name=f'{proj}_QCovid_covariates_df_1st'); print()
df_1st_wide = temp_save(df=df_1st_wide, out_name=f'{proj}_QCovid_covariates_df_1st_wide'); print()
df_all_summ_name = temp_save(df=df_all_summ_name, out_name=f'{proj}_QCovid_covariates_df_all_summ_name'); print()
df_all_summ_name_code = temp_save(df=df_all_summ_name_code, out_name=f'{proj}_QCovid_covariates_df_all_summ_name_code'); print()

# COMMAND ----------

# MAGIC %md ## 4.3. Check

# COMMAND ----------

# DBTITLE 1,Read saved data
df_all = spark.table(f'{dbc}.{proj}_QCovid_covariates_df_all')
df_1st = spark.table(f'{dbc}.{proj}_QCovid_covariates_df_1st')
df_1st_wide = spark.table(f'{dbc}.{proj}_QCovid_covariates_df_1st_wide')

df_all_summ_name = spark.table(f'{dbc}.{proj}_QCovid_covariates_df_all_summ_name')
df_all_summ_name_code = spark.table(f'{dbc}.{proj}_QCovid_covariates_df_all_summ_name_code')


# COMMAND ----------

display(df_1st_wide)

# COMMAND ----------

display(df_all.orderBy('PERSON_ID', 'DATE', 'name', 'source', 'code'))

# COMMAND ----------

# DBTITLE 1,Number of disease cases
display(df_all_summ_name.orderBy('name'))

# COMMAND ----------

display(df_all_summ_name_code.orderBy('name', 'terminology', 'code'))

# COMMAND ----------

# display(df_all_summ_name_code.where(f.lower(f.col('name')).rlike('asthma')))
display(df_all_summ_name_code.where(f.lower(f.col('name')).rlike('qcovid_hiv')))

# COMMAND ----------

display(df_all_summ_name_code.where(f.lower(f.col('name')).rlike('qcovid_diab')))

# COMMAND ----------

display(df_all_summ_name_code.where(f.lower(f.col('name')).rlike('qcovid_learn')))

# COMMAND ----------

# MAGIC %md # 5. Further processing

# COMMAND ----------

# DBTITLE 1,Read saved data
df_all = spark.table(f'{dbc}.{proj}_QCovid_covariates_df_all')
df_1st = spark.table(f'{dbc}.{proj}_QCovid_covariates_df_1st')
df_1st_wide = spark.table(f'{dbc}.{proj}_QCovid_covariates_df_1st_wide')

df_all_summ_name = spark.table(f'{dbc}.{proj}_QCovid_covariates_df_all_summ_name')
df_all_summ_name_code = spark.table(f'{dbc}.{proj}_QCovid_covariates_df_all_summ_name_code')

# COMMAND ----------

# MAGIC %md ## 5.1. Filter nulls/OOR

# COMMAND ----------

# BMI - filter to avoid selecting a null or out of range (OOR) unusable value below

# round
df_all_rounded = (
  df_all
  .withColumn('VALUE1_CONDITION', f.round(f.col('VALUE1_CONDITION'), 1))
)


print('--------------------------------------------------------------------------------------')
print('check')
print('--------------------------------------------------------------------------------------')
# check
tmpt = tab(df_all_rounded, 'name', 'source'); print()

# check
tmpf = (
  df_all_rounded
  .where(f.col('name') == 'BMI')
)
# tmpt = tab(tmpf, 'VALUE1_CONDITION'); print()
tmpt = tabstat(tmpf, 'VALUE1_CONDITION'); print()


print('--------------------------------------------------------------------------------------')
print('filter')
print('--------------------------------------------------------------------------------------')
# filter nulls and out of range
df_all_filtered_1 = (
  df_all_rounded
  .where(
    (f.col('name') != 'BMI')
    | ((f.col('name') == 'BMI') & (f.col('VALUE1_CONDITION').isNotNull()) & (f.col('VALUE1_CONDITION') >= 10) & (f.col('VALUE1_CONDITION') <= 100))
  )
)
print()


print('--------------------------------------------------------------------------------------')
print('check')
print('--------------------------------------------------------------------------------------')
# check
tmpt = tab(df_all_filtered_1, 'name', 'source'); print()

# check
tmpf = (
  df_all_filtered_1
  .where(f.col('name') == 'BMI')
)
# tmpt = tab(tmpf, 'VALUE1_CONDITION'); print()
tmpt = tabstat(tmpf, 'VALUE1_CONDITION'); print()

# number of names check
# assert df_all_filtered.select('name').distinct().count() == count_name # check no names have been missed

# COMMAND ----------

# MAGIC %md ## 5.2. Filter lookback

# COMMAND ----------

# filter lookback according to name
# Look within past 6 months: (2019-10-01 - 6 months) to 2019-10-01

# define 
## PL 20251111: remove solid organ transplant, based on Hippisley QCovid 2-3 paper
list_6m = [  
  'qcovid_BoneMarrowTransplantInLas'
  , 'qcovid_RadioTherapyInLast6Months'
  #, 'qcovid_SolidOrganTransplant'
  , 'qcovid_PrescribedImmunoSuppressa'
  , 'qcovid_PrescribedOralSteroids'
  , 'qcovid_TakingAntiLeukotrieneOrLa'
]

# prepare
df_all_filtered_1b = (
  df_all_filtered_1
  .withColumn('lookback_filter', f.when(f.col('name').isin(list_6m), f.lit('6m')).otherwise(f.lit('none')))
  .withColumn('fu', f.datediff(f.col('DATE'), f.col('CENSOR_DATE_END'))/365.25)
  .withColumn('CENSOR_DATE_END_minus_6m', f.add_months(f.col('CENSOR_DATE_END'), -6))
)

# check
tmpt = tab(df_all_filtered_1b, 'name', 'lookback_filter'); print()
tmpt = tabstat(df_all_filtered_1b, 'fu', byvar=['lookback_filter', 'name']); print()
tmpt = tabstat(df_all_filtered_1b, 'CENSOR_DATE_END_minus_6m', byvar='name', date=1); print()

# filter
df_all_filtered_2 = (
  df_all_filtered_1b  
  .where(
    (f.col('lookback_filter') == 'none')  # no lookback filtering
    # 6-month lookback filter
    | ((f.col('lookback_filter') == '6m') & (f.col('DATE') >= f.col('CENSOR_DATE_END_minus_6m')))
  )
)

# check
assert df_all_filtered_1b.select('name').distinct().count() == df_all_filtered_2.select('name').distinct().count() # check no names have been dropped
tmpt = tabstat(df_all_filtered_2, 'fu', byvar=['lookback_filter', 'name']); print()

# COMMAND ----------

# MAGIC %md ## 5.3. Check

# COMMAND ----------

# check
display(df_all_filtered_2.orderBy('PERSON_ID', 'DATE', 'name', 'source', 'code'))

# COMMAND ----------

# MAGIC %md # 6. Selection

# COMMAND ----------

# list of names
list_names = list(  
  df_all_filtered_2
  .select('name')
  .distinct()
  .where(f.col('name') != "CEV")  # PL note: no need to include CEV
  .toPandas()['name']
)
list_names.sort()

# check
for i, name in enumerate(list_names): 
  print(i, name)

# define
list_count = [
  'qcovid_PrescribedImmunoSuppressa'
  , 'qcovid_PrescribedOralSteroids'
  , 'qcovid_TakingAntiLeukotrieneOrLa'
]

list_first = [name for name in list_names if((re.match('^qcovid_.*$', name)) and (name not in list_count))]
#list_first = list_first + ['CEV']
list_last = ['BMI']

# check
print(list_count); print()
print(list_first); print()
print(list_last); print()

list_all = list_count + list_first + list_last
list_all.sort()
assert list_all == list_names, 'list_all != list_names'

# COMMAND ----------

# MAGIC %md ## 6.1. First (QCovid items)

# COMMAND ----------

print('---------------------------------------------------------------------------------')
print(f'first (earliest) event') 
print('---------------------------------------------------------------------------------')  
# filter names
# reduce
# drop duplicates 
first_1 = (
  df_all_filtered_2
  .where(f.col('name').isin(list_first))
  .select('PERSON_ID', 'CENSOR_DATE_START', 'CENSOR_DATE_END', 'name', 'DATE', 'source', 'sourcen', 'mono_id')
  .dropDuplicates(['PERSON_ID', 'CENSOR_DATE_START', 'CENSOR_DATE_END', 'name', 'DATE', 'source', 'sourcen'])
)

# check
tmpt = tab(first_1, 'name'); print()
count_varlist(first_1, ['PERSON_ID', 'name'])

# select
# first event
win_first_id_name_ord = (
  Window
  .partitionBy('PERSON_ID', 'name')
  .orderBy('DATE', 'sourcen', 'mono_id')  
)
first_2 = (
  first_1
  .withColumn('rownum_id_name', f.row_number().over(win_first_id_name_ord))
  .where(f.col('rownum_id_name') == 1)
  .drop('rownum_id_name', 'sourcen', 'mono_id')
)

# temp save
first_2 = temp_save(df=first_2, out_name=f'{proj}_tmp_covariates_first_2'); print() 

# check
count_varlist(first_2, ['PERSON_ID', 'name'])
tmpt = tab(first_2, 'name'); print()

# COMMAND ----------

# check
display(first_2)

# COMMAND ----------

print('---------------------------------------------------------------------------------')
print(f'reshape') 
print('---------------------------------------------------------------------------------')  
# check
tmpt = tab(first_2, 'name'); print()

# reshape long to wide  
first_3 = (
  first_2
  #.withColumn('name', f.concat(f.lit(f'cov_hx_'), f.lower(f.col('name'))))\
  .groupBy('PERSON_ID', 'CENSOR_DATE_START', 'CENSOR_DATE_END')
  .pivot('name')
  .agg(f.min('DATE'))
  .orderBy('PERSON_ID')  
)

# add flag and date columns
# reorder columns
vlist = []
for i, v in enumerate([col for col in list(first_3.columns) if re.match(f'^qcovid_', col)]):
  print(' ' , i, v)
  first_3 = (
    first_3
    .withColumnRenamed(v, v + '_date')
    .withColumn(v + '_flag', f.when(f.col(v + '_date').isNotNull(), 1))
  )
  vlist = vlist + [v + '_flag', v + '_date']
first_3 = (
  first_3
  .select(['PERSON_ID', 'CENSOR_DATE_START', 'CENSOR_DATE_END'] + vlist)    
)

# save
first_3 = temp_save(df=first_3, out_name=f'{proj}_tmp_covariates_first_3'); print() 

# check
print()
count_varlist(first_3, ['PERSON_ID'])
print(len(first_3.columns)); print()
print(pd.DataFrame({f'_cols': first_3.columns}).to_string()); print()

# COMMAND ----------

# check
display(first_3)

# COMMAND ----------

# MAGIC %md ## 6.2. Last (BMI)

# COMMAND ----------

print('---------------------------------------------------------------------------------')
print(f'last (most recent) event') 
print('---------------------------------------------------------------------------------')  
# filter names
# reduce
# drop duplicates (before the check for ties later)
last_1 = (
  df_all_filtered_2
  .where(f.col('name').isin(list_last))
  .select('PERSON_ID', 'CENSOR_DATE_START', 'CENSOR_DATE_END', 'name', 'DATE', 'VALUE1_CONDITION', 'value', 'source', 'sourcen', 'mono_id')
  .dropDuplicates(['PERSON_ID', 'CENSOR_DATE_START', 'CENSOR_DATE_END', 'name', 'DATE', 'VALUE1_CONDITION', 'value', 'source', 'sourcen'])
)

# check
tmpt = tab(last_1, 'name'); print()
count_varlist(last_1, ['PERSON_ID', 'name']); print()

# select
# flag and collect values where there are mulitple distinct values on the same date
win_id_name_ord = (
  Window
  .partitionBy('PERSON_ID', 'name')
  .orderBy(f.desc('DATE'), 'sourcen', 'mono_id')  
)
win_id_name_date = (
  Window
  .partitionBy('PERSON_ID', 'name', 'DATE')
)
last_2 = (
  last_1
  .withColumn('rownum_id_name', f.row_number().over(win_id_name_ord))
  .withColumn('rownummax_id_name_date', f.count(f.lit(1)).over(win_id_name_date))
  .withColumn('value_list', f.sort_array(f.collect_list(f.col('VALUE1_CONDITION')).over(win_id_name_date)))
  .where(f.col('rownum_id_name') == 1)
  .drop('rownum_id_name')
  .withColumnRenamed('rownummax_id_name_date', 'n_distinct_values_on_date')
  .withColumn('flag_multi_distinct_values_on_date', f.when(f.col('n_distinct_values_on_date') > 1, 1).otherwise(0))
)

# temp save
last_2 = temp_save(df=last_2, out_name=f'{proj}_tmp_covariates_last_2'); print() 

# check
count_varlist(last_2, ['PERSON_ID', 'name'])
tmpt = tab(last_2, 'n_distinct_values_on_date', 'flag_multi_distinct_values_on_date'); print()
tmpt = tab(last_2, 'name', 'flag_multi_distinct_values_on_date'); print()

# min, max, and look at diff of ties, diff between 0 dp rounded values

# COMMAND ----------

display(last_2)

# COMMAND ----------

display(last_2.where(f.col('flag_multi_distinct_values_on_date') == 1))

# COMMAND ----------

print('---------------------------------------------------------------------------------')
print(f'reshape') 
print('---------------------------------------------------------------------------------') 
# check
tmpt = tab(last_2, 'name'); print()

# separate reshape - because here we reshape both date and value
last_3 = (
  last_2
  # .where(f.col('name').isin(['height', 'mrc']))
  .withColumn('value', f.when(f.col('name') == 'BMI', f.col('VALUE1_CONDITION')).otherwise(f.col('value')))
  .select('PERSON_ID', 'CENSOR_DATE_START', 'CENSOR_DATE_END', 'name', 'DATE', 'value')  
  # .withColumn('name', f.concat(f.lit(f'cov_'), f.lower(f.col('name'))))
  .groupBy('PERSON_ID', 'CENSOR_DATE_START', 'CENSOR_DATE_END')
  .pivot('name')
  .agg(
    f.min('DATE').alias('date')
    , f.first('value').alias('value')
  )
  .where(f.col('PERSON_ID').isNotNull())
  .orderBy('PERSON_ID')  
)

# temp save
last_3 = temp_save(df=last_3, out_name=f'{proj}_tmp_covariates_last_3'); print() 

# check
count_varlist(last_3, ['PERSON_ID'])
print(len(last_3.columns)); print()
print(pd.DataFrame({f'_cols': last_3.columns}).to_string()); print()

# COMMAND ----------

display(last_3)

# COMMAND ----------

# MAGIC %md ## 6.3. Count (Meds)

# COMMAND ----------

print('---------------------------------------------------------------------------------')
print(f'count') 
print('---------------------------------------------------------------------------------')  

# 20230307 discussed possibility of using DATE or RECORD_DATE
#          comparisons performed below
#          decision made to use DATE for now
# TB. Q: Apply washout? otherwise will count events that are 1 day apart etc... GC: Not at the moment


# filter names
count_1 = (
  df_all_filtered_2
  .where(f.col('name').isin(list_count))
  .select('PERSON_ID', 'CENSOR_DATE_START', 'CENSOR_DATE_END', 'name', 'DATE', 'source', 'sourcen', 'mono_id', 'RECORD_DATE')
  .dropDuplicates(['PERSON_ID', 'CENSOR_DATE_START', 'CENSOR_DATE_END', 'name', 'DATE', 'source', 'sourcen', 'RECORD_DATE'])
)  


# check
tmpt = tab(count_1, 'name'); print()
count_varlist(count_1, ['PERSON_ID', 'name']); print()
count_varlist(count_1, ['PERSON_ID', 'name', 'DATE']); print()
count_varlist(count_1, ['PERSON_ID', 'name', 'RECORD_DATE']); print()


# check max rows per person and name, and date difference
print('---------------------------------------------------------------------------------')
print(f'DATE') 
print('---------------------------------------------------------------------------------')  
win_count_id_name_ord = Window\
  .partitionBy('PERSON_ID', 'name')\
  .orderBy('DATE', 'sourcen', 'mono_id')  
win_count_id_name = Window\
  .partitionBy('PERSON_ID', 'name')
tmpf = (
  count_1
  .withColumn('rownum_id_name', f.row_number().over(win_count_id_name_ord))
  .withColumn('rownummax_id_name', f.count(f.lit(1)).over(win_count_id_name))
  .withColumn('datediff_id_name', f.datediff(f.col('DATE'), f.lag(f.col('DATE'), 1).over(win_count_id_name_ord)))
)
tmpt = tab(tmpf.where(f.col('rownum_id_name') == 1), 'rownummax_id_name', 'name'); print()
tmpt = tabstat(tmpf, 'datediff_id_name', byvar=['name']); print()

print('---------------------------------------------------------------------------------')
print(f'RECORD_DATE') 
print('---------------------------------------------------------------------------------')  
win_count_id_name_ord = Window\
  .partitionBy('PERSON_ID', 'name')\
  .orderBy('RECORD_DATE', 'sourcen', 'mono_id')  
win_count_id_name = Window\
  .partitionBy('PERSON_ID', 'name')
tmpf = (
  count_1
  .withColumn('rownum_id_name', f.row_number().over(win_count_id_name_ord))
  .withColumn('rownummax_id_name', f.count(f.lit(1)).over(win_count_id_name))
  .withColumn('datediff_id_name', f.datediff(f.col('RECORD_DATE'), f.lag(f.col('RECORD_DATE'), 1).over(win_count_id_name_ord)))
)
tmpt = tab(tmpf.where(f.col('rownum_id_name') == 1), 'rownummax_id_name', 'name'); print()
tmpt = tabstat(tmpf, 'datediff_id_name', byvar=['name']); print()


# select
# dropduplicates on DATE not actually needed below because only one source 
count_2 = (
  count_1
  .select('PERSON_ID', 'CENSOR_DATE_START', 'CENSOR_DATE_END', 'name', 'DATE', 'RECORD_DATE')  
  .groupBy('PERSON_ID', 'CENSOR_DATE_START', 'CENSOR_DATE_END', 'name')
  .agg(
    f.countDistinct(f.col('DATE')).alias('n_DATE')
    , f.countDistinct(f.col('RECORD_DATE')).alias('n_RECORD_DATE')
    , f.min(f.col('DATE')).alias('DATE')
    , f.min(f.col('RECORD_DATE')).alias('RECORD_DATE')
  )
  .withColumn('n_DATE_ge_4', f.when(f.col('n_DATE') >= 4, f.lit(1)).otherwise(f.lit(0)))
  .withColumn('n_RECORD_DATE_ge_4', f.when(f.col('n_RECORD_DATE') >= 4, f.lit(1)).otherwise(f.lit(0)))
)

# temp save
count_2 = temp_save(df=count_2, out_name=f'{proj}_tmp_covariates_count_2'); print() 

# check
tmpt = tab(count_2, 'name'); print()
count_varlist(count_2, ['PERSON_ID', 'name']); print()
tmpt = tab(count_2, 'n_DATE', 'n_DATE_ge_4'); print()
tmpt = tab(count_2, 'n_RECORD_DATE', 'n_RECORD_DATE_ge_4'); print()
tmpt = tab(count_2, 'n_DATE_ge_4', 'n_RECORD_DATE_ge_4'); print()
tmpt = tab(count_2, 'name', 'n_DATE_ge_4'); print()
tmpt = tab(count_2, 'name', 'n_RECORD_DATE_ge_4'); print()
tmpt = tab(count_2.where(f.col('name') == 'qcovid_PrescribedImmunoSuppressa'), 'n_DATE_ge_4', 'n_RECORD_DATE_ge_4'); print()
tmpt = tab(count_2.where(f.col('name') == 'qcovid_PrescribedOralSteroids'), 'n_DATE_ge_4', 'n_RECORD_DATE_ge_4'); print()
tmpt = tab(count_2.where(f.col('name') == 'qcovid_TakingAntiLeukotrieneOrLa'), 'n_DATE_ge_4', 'n_RECORD_DATE_ge_4'); print()

# COMMAND ----------

# check
display(count_1.orderBy('PERSON_ID', 'name'))

# COMMAND ----------

# check
display(count_2.orderBy('PERSON_ID', 'name'))

# COMMAND ----------

# check
display(count_2.orderBy('PERSON_ID', 'name').where(f.col('n_DATE_ge_4') != f.col('n_RECORD_DATE_ge_4')))

# COMMAND ----------

print('---------------------------------------------------------------------------------')
print(f'reshape') 
print('---------------------------------------------------------------------------------')  
# check
count_varlist(count_2, ['PERSON_ID'])
tmpt = tab(count_2, 'name'); print()

# separate reshape - because here we reshape both date and value
count_3 = (
  count_2
  .select('PERSON_ID', 'CENSOR_DATE_START', 'CENSOR_DATE_END', 'name', 'n_DATE', 'n_DATE_ge_4')  
  # .withColumn('name', f.concat(f.lit(f'cov_hx_'), f.lower(f.col('name'))))
  .groupBy('PERSON_ID', 'CENSOR_DATE_START', 'CENSOR_DATE_END')
  .pivot('name')
  .agg(
    f.min('n_DATE').alias('n')
    , f.min('n_DATE_ge_4').alias('flag')
  )
  .orderBy('PERSON_ID')  
)

# temp save
count_3 = temp_save(df=count_3, out_name=f'{proj}_tmp_covariates_count_3'); print() 

# check
count_varlist(count_3, ['PERSON_ID'])
print(len(count_3.columns)); print()
print(pd.DataFrame({f'_cols': count_3.columns}).to_string()); print()

# COMMAND ----------

# check
display(count_3)

# COMMAND ----------

# MAGIC %md ## 6.4. Combine

# COMMAND ----------

# DBTITLE 1,Read saved data
first_3 = spark.table(f'{dbc}.{proj}_tmp_covariates_first_3')
last_3 = spark.table(f'{dbc}.{proj}_tmp_covariates_last_3')
count_3 = spark.table(f'{dbc}.{proj}_tmp_covariates_count_3')

# COMMAND ----------

# merge
cov_1 = merge(first_3, last_3, ['PERSON_ID', 'CENSOR_DATE_START', 'CENSOR_DATE_END'], validate='1:1', indicator=0); print()
cov_2 = merge(cov_1, count_3, ['PERSON_ID', 'CENSOR_DATE_START', 'CENSOR_DATE_END'], validate='1:1', indicator=0); print()

# check
count_var(cov_2, 'PERSON_ID'); print()

# COMMAND ----------

# MAGIC %md ## 6.5. Check

# COMMAND ----------

display(cov_2)

# COMMAND ----------

# MAGIC %md # 7. Additional

# COMMAND ----------

# MAGIC %md
# MAGIC ## 7.01. Modify CKD stage

# COMMAND ----------

## Set CKD stage to the most severe stage 
cov_2a = (
    cov_2.withColumn("qcovid_ChronicKidneyDisease3_final_flag", 
                          f.when((f.col("qcovid_ChronicKidneyDisease3_flag") == 1) & 
                                 (f.col("qcovid_ChronicKidneyDisease4_flag").isNull()) & 
                                 (f.col("qcovid_ChronicKidneyDisease5_flag").isNull()) & 
                                 (f.col("qcovid_ChronicKidneyDisease5Tran_flag").isNull()) & 
                                 (f.col("qcovid_ChronicKidneyDisease5Dial_flag").isNull()), 1).otherwise(0))
          .withColumn("qcovid_ChronicKidneyDisease4_final_flag", 
                          f.when((f.col("qcovid_ChronicKidneyDisease4_flag") == 1) & 
                                 (f.col("qcovid_ChronicKidneyDisease5_flag").isNull()) & 
                                 (f.col("qcovid_ChronicKidneyDisease5Tran_flag").isNull()) & 
                                 (f.col("qcovid_ChronicKidneyDisease5Dial_flag").isNull()), 1).otherwise(0))
          .withColumn("qcovid_ChronicKidneyDisease5_final_flag", 
                          f.when((f.col("qcovid_ChronicKidneyDisease5_flag") == 1) & 
                                 (f.col("qcovid_ChronicKidneyDisease5Tran_flag").isNull()) & 
                                 (f.col("qcovid_ChronicKidneyDisease5Dial_flag").isNull()), 1).otherwise(0))
          .withColumn("qcovid_ChronicKidneyDisease5Dial_final_flag", 
                          f.when((f.col("qcovid_ChronicKidneyDisease5Dial_flag") == 1) &  
                                 (f.col("qcovid_ChronicKidneyDisease5Tran_flag").isNull()), 1).otherwise(0))
)

# COMMAND ----------

tab(cov_2a, 'qcovid_ChronicKidneyDisease3_final_flag', 'qcovid_ChronicKidneyDisease3_flag')

# COMMAND ----------

# MAGIC %md
# MAGIC ## 7.02. Add Down's Syndrome flag and Type 1/2 Diabetes flag

# COMMAND ----------

display(codelist_qcovid.where(f.col('name') == "qcovid_LearningDisabilityOrDowns"))

# COMMAND ----------

display(codelist_qcovid.where(f.col('name') == "qcovid_Diabetes"))

# COMMAND ----------

## Get codelist for Down's Syndrome
codelist_qcovid_down = codelist_qcovid.filter(f.col('name') == "qcovid_LearningDisabilityOrDowns").where( (f.lower(f.col('term')).rlike('trisomy 21')) |  (f.lower(f.col('term')).rlike('down')) ).withColumn("name", f.lit("qcovid_Downs"))
display(codelist_qcovid_down)

# COMMAND ----------

## Get codelist for Type 1 diabetes
codelist_qcovid_diab1 = codelist_qcovid.filter(f.col('name') == "qcovid_Diabetes").where( (f.lower(f.col('term')).rlike('type 1')) |  (f.lower(f.col('term')).rlike('type i ')) ).withColumn("name", f.lit("qcovid_Type1Diabetes"))
display(codelist_qcovid_diab1)

# COMMAND ----------

## Get codelist for Type 2 diabetes
codelist_qcovid_diab2 = codelist_qcovid.filter(f.col('name') == "qcovid_Diabetes").where( (f.lower(f.col('term')).rlike('type 2')) |  (f.lower(f.col('term')).rlike('type ii')) ).withColumn("name", f.lit("qcovid_Type2Diabetes"))
display(codelist_qcovid_diab2)

# COMMAND ----------

# DBTITLE 1,Prepare codelist
# Combine codelists
codelist_qcovid_down_diab = codelist_qcovid_down.unionByName(codelist_qcovid_diab1).unionByName(codelist_qcovid_diab2)

# Separate by code type
codelist_snomed_down_diab = (
  codelist_qcovid_down_diab
  .where(f.col('terminology').isin(['SNOMED', 'DMD']))
)
codelist_icd10_down_diab = (
  codelist_qcovid_down_diab
  .where(f.col('terminology').isin(['ICD10']))
)

codelist_snomed_down_diab.count() + codelist_icd10_down_diab.count()

# COMMAND ----------

# DBTITLE 1,Codelist match
# dictionary - key: dataset, codelist, and ordering in the event of tied records
dict_input_down_diab = {
    'gdppr':        ['gdppr_prepared', 'codelist_snomed_down_diab', 1]
  , 'hes_apc':      ['hes_apc_long_prepared', 'codelist_icd10_down_diab', 2]
}

# run codelist match and codelist match summary functions
dict_all_down_diab, df_1st_down_diab, df_1st_wide_down_diab = codelist_match_v2_test(dict_input_down_diab, _name_prefix=f'cov_', stages_to_run=3); print()
df_all_summ_name_down_diab, df_all_summ_name_code_down_diab = codelist_match_summ(dict_input_down_diab, dict_all_down_diab); print()

# COMMAND ----------

display(df_1st_wide_down_diab)
df_1st_wide_down_diab.count()

# COMMAND ----------

display(df_all_summ_name_down_diab)

# COMMAND ----------

display(df_all_summ_name_code_down_diab)

# COMMAND ----------

# DBTITLE 1,Join with main dataframe
df_1st_wide_down_diab_v2 = df_1st_wide_down_diab.withColumnsRenamed({"cov_qcovid_downs_flag": "qcovid_downs_flag", "cov_qcovid_downs_date": "qcovid_downs_date", "cov_qcovid_Type1Diabetes_flag": "qcovid_Type1Diabetes_flag","cov_qcovid_Type1Diabetes_date": "qcovid_Type1Diabetes_date", "cov_qcovid_Type2Diabetes_flag": "qcovid_Type2Diabetes_flag","cov_qcovid_Type2Diabetes_date": "qcovid_Type2Diabetes_date"}).select("person_id", "qcovid_downs_flag", "qcovid_downs_date", "qcovid_Type1Diabetes_flag", "qcovid_Type1Diabetes_date", "qcovid_Type2Diabetes_flag", "qcovid_Type2Diabetes_date")
cov_2b = cov_2a.join(df_1st_wide_down_diab_v2, on = "person_id", how = "left")
# display(cov_2b)

# COMMAND ----------

# DBTITLE 1,Modify learning disability category
cov_2c = cov_2b.withColumn("qcovid_LearningDisability_flag", 
                          f.when((f.col("qcovid_LearningDisabilityOrDowns_flag") == 1) &  
                                 (f.col("qcovid_downs_flag").isNull()), 1).otherwise(0))

# COMMAND ----------

# DBTITLE 1,Check
tab(cov_2c, "qcovid_LearningDisability_flag", "qcovid_LearningDisabilityOrDowns_flag")

# COMMAND ----------

tab(cov_2c, "qcovid_Type2Diabetes_flag", "qcovid_Diabetes_flag")

# COMMAND ----------

tab(cov_2c, "qcovid_Type1Diabetes_flag", "qcovid_Diabetes_flag")

# COMMAND ----------

cov_2c.where((f.col("qcovid_Diabetes_flag") == 1) & (f.col("qcovid_Type1Diabetes_flag").isNull()) & (f.col("qcovid_Type2Diabetes_flag").isNull()) ).count()

# 1.6% of pts with diabetes cannot be classified into Type 1 vs. Type 2

# COMMAND ----------

# MAGIC %md ## 7.1. Add cohort 

# COMMAND ----------

# merge cov_1 and cohort ID
cov_3 = merge(cov_2c, individual_censor_dates, ['PERSON_ID', 'CENSOR_DATE_START', 'CENSOR_DATE_END'], validate='1:1', assert_results=['both', 'right_only'], indicator=0); print()

# check
count_var(cov_3, 'PERSON_ID'); print()

# COMMAND ----------

# DBTITLE 1,Filter to individuals with BMI
cov_3_bmi = cov_3.where(f.col("BMI_value").isNotNull())

count_var(cov_3_bmi, 'PERSON_ID'); print()

# COMMAND ----------

# MAGIC %md ## 7.2. QCovid score - SKIP

# COMMAND ----------

# list of qcovid flag names
list_qcovid_flag = [name for name in cov_3.columns if re.match('^cov_hx_qcovid_.*_flag$', name)]
print(len(list_qcovid_flag)); print()
print(list_qcovid_flag); print()

# fill zero
cov_4 = (
  cov_3
  .na.fill(value=0, subset=list_qcovid_flag)
)

# row sum to calculate score
cov_4 = (
  cov_4
  .withColumn('qcovid_SCORE', sum([f.col(qcovid_flag) for qcovid_flag in list_qcovid_flag]))
)

# check
tmpt = tab(cov_4, 'qcovid_SCORE'); print()

# COMMAND ----------

# check 
display(cov_4)

# COMMAND ----------

# MAGIC %md ## 7.3. Shielded - SKIP

# COMMAND ----------

# check
# tmpt = tab(cov_4, 'cov_cev_value'); print()
tmpt = tab(cov_4, 'cov_hx_cev_flag'); print()

# shielded
cov_4 = (
  cov_4
  # .withColumn('cov_shielded', f.when(f.col('cov_cev_value') == '1 High', 1).otherwise(0))
  .withColumn('cov_shielded', f.col('cov_hx_cev_flag'))
)

# check
# tmpt = tab(cov_4, 'cov_cev_value', 'cov_shielded'); print()
tmpt = tab(cov_4, 'cov_hx_cev_flag', 'cov_shielded'); print()

# COMMAND ----------

# MAGIC %md # 8. Check - SKIP

# COMMAND ----------

# see data_checks\...covariates_check

# COMMAND ----------

# check
count_var(cov_4, 'PERSON_ID'); print()
print(len(cov_4.columns)); print()
print(pd.DataFrame({f'_cols': cov_4.columns}).to_string()); print()

# COMMAND ----------

# check
display(cov_4)

# COMMAND ----------

# check
tmpt = tab(cov_4, 'qcovid_SCORE', 'cov_shielded'); print()

# COMMAND ----------

win = Window\
  .partitionBy('cov_shielded')
tmp1 = (
  cov_4
  .groupBy('qcovid_SCORE', 'cov_shielded')
  .agg(f.count(f.lit(1)).alias('n'))
  .withColumn('total', f.sum(f.col('n')).over(win))
  .withColumn('pct', f.col('n')/f.col('total'))
  .orderBy('qcovid_SCORE', 'cov_shielded')
)
display(tmp1)

# COMMAND ----------

# MAGIC %md # 9. Save

# COMMAND ----------

# save name
outName = f'{proj}_out_Qcovid_final_covariates'.lower()
outName_bmi = f'{proj}_out_Qcovid_final_covariates_BMIAvailable'.lower()

# save previous version for comparison purposes
#tmpt = spark.sql(f"""SHOW TABLES FROM {dbc}""")\
#  .select('tableName')\
#  .where(f.col('tableName') == outName)\
#  .collect()
#if(len(tmpt)>0):
#  _datetimenow = datetime.datetime.now().strftime("%Y%m%d_%H%M%S")
#  outName_pre = f'{outName}_pre{_datetimenow}'.lower()
#  print(outName_pre)
#  spark.table(f'{dbc}.{outName}').write.mode('overwrite').saveAsTable(f'{dbc}.{outName_pre}')
#  spark.sql(f'ALTER TABLE {dbc}.{outName_pre} OWNER TO {dbc}')

# save
cov_3.write.mode('overwrite').option("overwriteSchema", "true").saveAsTable(f'{dbc}.{outName}')
cov_3_bmi.write.mode('overwrite').option("overwriteSchema", "true").saveAsTable(f'{dbc}.{outName_bmi}')
#spark.sql(f'ALTER TABLE {dbc}.{outName} OWNER TO {dbc}')
cov_3 = spark.table(f'{dbc}.{outName}')
cov_3_bmi = spark.table(f'{dbc}.{outName_bmi}')

# COMMAND ----------

# DBTITLE 1,Split data based on age and YOB
cov_3_bmi_v2 = cov_3_bmi.join(cohort.select("person_id", "age_at_2020", "even_birth_year"), on = "person_id", how = "inner")

cov_3_bmi_age40_even = cov_3_bmi_v2.filter((f.col("age_at_2020") < 50) & (f.col("even_birth_year") == 1))
cov_3_bmi_age50_even = cov_3_bmi_v2.filter((f.col("age_at_2020") >= 50) & (f.col("age_at_2020") < 60 ) & (f.col("even_birth_year") == 1))
cov_3_bmi_age60_even = cov_3_bmi_v2.filter((f.col("age_at_2020") >= 60) & (f.col("age_at_2020") < 70 ) & (f.col("even_birth_year") == 1))
                                           
cov_3_bmi_age40_odd = cov_3_bmi_v2.filter((f.col("age_at_2020") < 50) & (f.col("even_birth_year") == 0))
cov_3_bmi_age50_odd = cov_3_bmi_v2.filter((f.col("age_at_2020") >= 50) & (f.col("age_at_2020") < 60 ) & (f.col("even_birth_year") == 0))
cov_3_bmi_age60_odd = cov_3_bmi_v2.filter((f.col("age_at_2020") >= 60) & (f.col("age_at_2020") < 70 ) & (f.col("even_birth_year") == 0))                                        

# COMMAND ----------

# DBTITLE 1,Save data
cov_3_bmi_age40_even.write.mode('overwrite').option("overwriteSchema", "true").saveAsTable(f'{dbc}.{proj}_Qcovid_final_covariates_BMIAvailable_WhiteBritish_Age40_49_EvenYOB_Index2020')
cov_3_bmi_age50_even.write.mode('overwrite').option("overwriteSchema", "true").saveAsTable(f'{dbc}.{proj}_Qcovid_final_covariates_BMIAvailable_WhiteBritish_Age50_59_EvenYOB_Index2020')
cov_3_bmi_age60_even.write.mode('overwrite').option("overwriteSchema", "true").saveAsTable(f'{dbc}.{proj}_Qcovid_final_covariates_BMIAvailable_WhiteBritish_Age60_69_EvenYOB_Index2020')

cov_3_bmi_age40_odd.write.mode('overwrite').option("overwriteSchema", "true").saveAsTable(f'{dbc}.{proj}_Qcovid_final_covariates_BMIAvailable_WhiteBritish_Age40_49_OddYOB_Index2020')
cov_3_bmi_age50_odd.write.mode('overwrite').option("overwriteSchema", "true").saveAsTable(f'{dbc}.{proj}_Qcovid_final_covariates_BMIAvailable_WhiteBritish_Age50_59_OddYOB_Index2020')
cov_3_bmi_age60_odd.write.mode('overwrite').option("overwriteSchema", "true").saveAsTable(f'{dbc}.{proj}_Qcovid_final_covariates_BMIAvailable_WhiteBritish_Age60_69_OddYOB_Index2020')
