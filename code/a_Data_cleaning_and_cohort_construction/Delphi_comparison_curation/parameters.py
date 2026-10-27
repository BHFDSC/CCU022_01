# Databricks notebook source
# MAGIC %md
# MAGIC # Parameters

# COMMAND ----------

import pyspark.sql.functions as f
import pyspark.sql.types as t
from pyspark.sql import Window

#from functools import reduce

import pandas as pd
import pyspark.pandas as ps
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

class params_class:
    # -----------------------------------------------------------------------------
    # Project
    # -----------------------------------------------------------------------------
    proj = "ccu022_crs"
    # -----------------------------------------------------------------------------
    # Project specific
    # -----------------------------------------------------------------------------
    tmp_archived_on = "2025-02-04"
    tmp_archived_on_str = "2025_02_04"
    stduy_start_date = ""
    study_end_date = ""
    # -----------------------------------------------------------------------------
    # Databases
    # -----------------------------------------------------------------------------
    db = ""
    dbc_old = f"_collab"
    dbc = ""
    dss = ""
    list_datasets = [
        [
            "deaths",
            dbc_old,
            f"deaths__archive",
            tmp_archived_on,
            "DEC_CONF_NHS_NUMBER_CLEAN_DEID",
            "REG_DATE_OF_DEATH",
        ],
        [
            "gdppr",
            dbc_old,
            f"gdppr__archive",
            tmp_archived_on,
            "NHS_NUMBER_DEID",
            "DATE",
        ],
        [
            "hes_apc",
            dbc_old,
            f"hes_apc_all_years_archive",
            tmp_archived_on,
            "PERSON_ID_DEID",
            "EPISTART",
        ],
        [
            "pmeds",
            dbc_old,
            f"primary_care_meds_{db}_archive",
            tmp_archived_on,
            "PERSON_ID_DEID",
            "ProcessingPeriodDate",
        ],
    ]
    pd_datasets = pd.DataFrame(
        list_datasets, columns=["dataset", "database", "table", "archived_on", "idVar", "dateVar"])
    
    path_gdppr_arch = f"{dbc_old}.gdppr__archive"
    path_hes_apc_arch = f"{dbc_old}.hes_apc_all_years_archive"
    path_pmeds_arch = f"{dbc_old}.primary_care_meds__archive"
    path_deaths_arch = f"{dbc_old}.deaths__archive"
    # curated tables
    path_demographics = f"{dbc}.hds_curated_assets__demographics_{tmp_archived_on_str}"
    path_patient_id_type_lookup = f"{db}.token_pseudo_id_lookup"
    path_hes_apc_cips = f"{dbc}.hds_curated_assets__hes_apc_cips_cips_{tmp_archived_on_str}"
    path_hes_apc_epis = (
        f"{dbc}.hds_curated_assets__hes_apc_cips_episodes_{tmp_archived_on_str}")
    path_hes_apc_pspells = (
        f"{dbc}.hds_curated_assets__hes_apc_cips_provider_spells_{tmp_archived_on_str}")
    path_hes_apc_long = f"{dbc}.hds_curated_assets__hes_apc_diagnosis_{tmp_archived_on_str}"
    path_cur_deaths_sing = f"{dbc}.hds_curated_assets__deaths_single_{tmp_archived_on_str}"
    path_cur_deaths_long = (
        f"{dbc}.hds_curated_assets__deaths_cause_of_death_{tmp_archived_on_str}")
    path_lsoa_multi = f"{dbc}.hds_curated_assets__lsoa_multisource_{tmp_archived_on_str}"
    path_dob_ind = (
        f"{dbc}.hds_curated_assets__date_of_birth_individual_{tmp_archived_on_str}")
    path_dob_multi = (
        f"{dbc}.hds_curated_assets__date_of_birth_multisource_{tmp_archived_on_str}")
    # reference tables
    path_mainspef = f"{dss}.hesf_mainspef"
    path_tretspef = f"{dss}.hesf_tretspef"
    path_icd10 = f"{dss}.icd10_codes"
    path_lsoa_2011_imd_lookup = f"{dbc}.hds_cur_lsoa_2011_imd_lookup"
    path_lsoa_2011_reg_lookup = f"{dbc}.hds_cur_lsoa_region_lookup"
    path_gdppr_refset = f"{dss}.gdppr_cluster_refset"

# codelist tables
# path_out_codelist_quality_assurance = f'{dbc}.{proj}_out_codelist_quality_assurance'

params = params_class()

# COMMAND ----------

# function to extract the batch corresponding to the pre-defined archived_on date from the archive for the specified dataset
from pyspark.sql import DataFrame
def extract_batch_from_archive(_df_datasets: DataFrame, _dataset: str, verbose_counts = False):
  
  # get row from df_archive_tables corresponding to the specified dataset
  _row = _df_datasets[_df_datasets['dataset'] == _dataset]
  
  # check one row only
  assert _row.shape[0] != 0, f"dataset = {_dataset} not found in _df_datasets (datasets = {_df_datasets['dataset'].tolist()})"
  assert _row.shape[0] == 1, f"dataset = {_dataset} has >1 row in _df_datasets"
  
  # create path and extract archived on
  _row = _row.iloc[0]
  _path = _row['database'] + '.' + _row['table']  
  _archived_on = _row['archived_on']  
  print(_path + ' (archived_on = ' + _archived_on + ')')
  
  # check path exists # commented out for runtime
#   _tmp_exists = spark.sql(f"SHOW TABLES FROM {_row['database']}")\
#     .where(f.col('tableName') == _row['table'])\
#     .count()
#   assert _tmp_exists == 1, f"path = {_path} not found"

  # extract batch
  _tmp = spark.table(_path)\
    .where(f.col('archived_on') == _archived_on)  
  
  # check number of records returned
  if verbose_counts:
      _tmp_records = _tmp.count()
      print(f'  {_tmp_records:,} records')
      assert _tmp_records > 0, f"number of records == 0"

  # return dataframe
  return _tmp

# COMMAND ----------

display(params.pd_datasets)

# COMMAND ----------

gdppr = extract_batch_from_archive(params.pd_datasets, 'gdppr')
#hes_apc = extract_batch_from_archive(params.pd_datasets, 'hes_apc')
#pmeds = extract_batch_from_archive(params.pd_datasets, 'pmeds')
#deaths = extract_batch_from_archive(params.pd_datasets, 'deaths')

# COMMAND ----------

