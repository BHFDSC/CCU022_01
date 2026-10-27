# Databricks notebook source
# MAGIC %md
# MAGIC ### Preparation of BMI codelist (needed for QCovid score)
# MAGIC ##### Pearl Liu adapted from Project CCU051

# COMMAND ----------

# MAGIC %md # CCU051_D02d_codelist_covariates
# MAGIC
# MAGIC **Description** This notebook creates the covariates codelist needed for CCU051.
# MAGIC
# MAGIC **Author(s)** Tom Bolton
# MAGIC
# MAGIC **Project** CCU051
# MAGIC
# MAGIC **First created** 2023.02.15
# MAGIC
# MAGIC **Date last updated** 2023.03.02 / 2023.06.23 (to export codelist)
# MAGIC
# MAGIC **Date last run** 2023.03.02 / 2023.06.23 (to export codelist)
# MAGIC
# MAGIC **Data input** functions - libraries - parameters - manual codelist input\
# MAGIC gdppr_refset
# MAGIC
# MAGIC **Data output** - 'ccu051_out_codelist_covariates'

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

## PL 20251029: Add catalog name: reference_data
gdppr_refset = spark.table('reference_data..gdppr_cluster_refset')

# COMMAND ----------

# MAGIC %md # 2. BMI

# COMMAND ----------

# QCovid items
# Source - TO SAVE IN:
# CVD-COVID-UK Consortium Shared Folder\Projects\CCU051\Name_file.txt

## PL note: see COVID-19+PRA+code+list+v1 file - BMI tab for comparison

tmp_codelist_bmi_values = """
name,code
BMI,722595002
BMI,914741000000103
BMI,914731000000107
BMI,914721000000105
BMI,35425004
BMI,48499001
BMI,301331008
BMI,6497000
BMI,310252000
BMI,427090001
BMI,408512008
BMI,162864005
BMI,162863004
BMI,412768003
BMI,60621009
BMI,846931000000101
"""
tmp_codelist_bmi_values = (
  spark.createDataFrame(pd.DataFrame(pd.read_csv(io.StringIO(tmp_codelist_bmi_values))))
  .withColumn('terminology', f.lit('SNOMED'))
)

# check
count_var(tmp_codelist_bmi_values, 'code'); print()
tmpt = tab(tmp_codelist_bmi_values, 'name'); print()

# merge with refset to get description
gdppr_refset_1 = (
  gdppr_refset
  .select(f.col('ConceptId').alias('code'), f.col('ConceptId_Description').alias('term'))
  .dropDuplicates(['code'])
)
codelist_bmi_values = merge(tmp_codelist_bmi_values, gdppr_refset_1, ['code'], validate='1:1', assert_results=['both', 'right_only'], keep_results=['both'], indicator=0); print()

# check
count_var(codelist_bmi_values, 'code'); print()
print(codelist_bmi_values.toPandas().to_string()); print()

# COMMAND ----------

# MAGIC %md # 3. CEV

# COMMAND ----------

# CEV=Clinically extremely vulnerable
# High=Shielding

# 20230302. Removed Moderate and Low. To be consistent with Scotland we will now look for any history of shielding, not the latest CEV status. 
# CEV,SNOMED,1300571000000100,Moderate risk category for developing complication from coronavirus disease 19 caused by severe acute respiratory syndrome coronavirus 2 infection (finding),2 Moderate
# CEV,SNOMED,1300591000000101,Low risk category for developing complication from coronavirus disease 19 caused by severe acute respiratory syndrome coronavirus 2 infection (finding),3 Low

codelist_cev = """
name,terminology,code,term,value
CEV,SNOMED,1300561000000107,High risk category for developing complication from coronavirus disease 19 caused by severe acute respiratory syndrome coronavirus 2 infection (finding),1 High
"""
codelist_cev = (
  spark.createDataFrame(pd.DataFrame(pd.read_csv(io.StringIO(codelist_cev))))
)

# check
count_var(codelist_cev, 'code'); print()
tmpt = tab(codelist_cev, 'name'); print()
print(codelist_cev.toPandas().to_string()); print()

# COMMAND ----------

# MAGIC %md # 4. Combine

# COMMAND ----------

codelist = (
  codelist_bmi_values
  .withColumn('value', f.lit(None))
  .unionByName(codelist_cev)
)

# check
count_varlist(codelist, ['name', 'terminology', 'code']); print()
tmpt = tab(codelist, 'name', 'terminology'); print()
tmpt = tab(codelist, 'name', 'value'); print()
print(codelist.toPandas().to_string()); print()

# COMMAND ----------

# MAGIC %md # 5. Checks

# COMMAND ----------

display(codelist)

# COMMAND ----------

# MAGIC %md # 6. Save

# COMMAND ----------

# save name
outName = f'{proj}_out_codelist_covariates'

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
codelist.write.mode('overwrite').saveAsTable(f'{dbc}.{outName}')
#spark.sql(f'ALTER TABLE {dbc}.{outName} OWNER TO {dbc}')

# COMMAND ----------

# compare
# old = spark.table(f'{dbc}.{proj}_out_codelist_covariates_pre20230302_093344')
# new = codelist
# key = ['name', 'terminology', 'code']
# file1, file2, file3, file3_differences = compare_files(old, new, key, warningError=0)

# COMMAND ----------

# display(file1)

# COMMAND ----------

# MAGIC %md # 7. Export codelist

# COMMAND ----------

tmp = spark.table(f'{dbc}.{proj}_out_codelist_covariates')

# COMMAND ----------

display(tmp)
