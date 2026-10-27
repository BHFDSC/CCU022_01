# Databricks notebook source
# MAGIC %md
# MAGIC ### Preparation of data for analysis - Index date: 2015-01-01, 10-year follow-up - preparation of PheRS predictors
# MAGIC ##### Pearl Liu
# MAGIC
# MAGIC ##### v1.0

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
version_flag = "_ICDAtoZ"

# COMMAND ----------

# MAGIC %run "/ccu022/CRS_analysis_2025/parameters"

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
# MAGIC #### Read PheRS phecode predictor list

# COMMAND ----------

## See local file "PheRS_Predictor_PheCodes.csv"
phers_predictor_text = f'''41	Bacterial infection NOS	infectious diseases
172	Skin cancer	neoplasms
174	Breast cancer	neoplasms
195	Cancer, suspected or other	neoplasms
197	Chemotherapy	neoplasms
208	Benign neoplasm of colon	neoplasms
214	Lipoma	neoplasms
216	Benign neoplasm of skin	neoplasms
218	Benign neoplasm of uterus	neoplasms
244	Hypothyroidism	endocrine/metabolic
250	Diabetes mellitus	endocrine/metabolic
272	Disorders of lipoid metabolism	endocrine/metabolic
276	Disorders of fluid, electrolyte, and acid-base balance	endocrine/metabolic
278	Overweight, obesity and other hyperalimentation	endocrine/metabolic
285	Other anemias	hematopoietic
296	Mood disorders	mental disorders
306	Other mental disorder	mental disorders
317	Alcohol-related disorders	mental disorders
318	Tobacco use disorder	mental disorders
339	Other headache syndromes	neurological
351	Other peripheral nerve disorders	neurological
366	Cataract	sense organs
401	Hypertension	circulatory system
411	Ischemic Heart Disease	circulatory system
418	Nonspecific chest pain	circulatory system
426	Cardiac conduction disorders	circulatory system
427	Cardiac dysrhythmias	circulatory system
433	Cerebrovascular disease	circulatory system
454	Varicose veins	circulatory system
455	Hemorrhoids	circulatory system
459	Other disorders of circulatory system	circulatory system
480	Pneumonia	respiratory
495	Asthma	respiratory
496	Chronic airway obstruction	respiratory
512	Other symptoms of respiratory system	respiratory
519	Other diseases of respiratory system, not elsewhere classified	respiratory
530	Diseases of esophagus	digestive
531	Peptic ulcer (excl. esophageal)	digestive
535	Gastritis and duodenitis	digestive
550	Abdominal hernia	digestive
558	Noninfectious gastroenteritis	digestive
561	Symptoms involving digestive system	digestive
562	Diverticulosis and diverticulitis	digestive
563	Constipation	digestive
564	Functional digestive disorders	digestive
565	Anal and rectal conditions	digestive
573	Other disorders of liver	digestive
574	Cholelithiasis and cholecystitis	digestive
578	Gastrointestinal hemorrhage	digestive
585	Renal failure	genitourinary
591	Urinary tract infection	genitourinary
593	Hematuria	genitourinary
594	Urinary calculus	genitourinary
596	Other disorders of bladder	genitourinary
599	Other symptoms/disorders or the urinary system	genitourinary
600	Hyperplasia of prostate	genitourinary
614	Inflammatory diseases of female pelvic organs	genitourinary
618	Genital prolapse	genitourinary
619	Noninflammatory female genital disorders	genitourinary
622	Polyp of female genital organs	genitourinary
624	Symptoms involving female genital tract	genitourinary
626	Disorders of menstruation and other abnormal bleeding from female genital tract	genitourinary
627	Menopausal and postmenopausal disorders	genitourinary
634	Miscarriage; stillbirth	pregnancy complications
655	Known or suspected fetal abnormality affecting management of mother	pregnancy complications
665	Obstetrical/birth trauma	pregnancy complications
681	Superficial cellulitis and abscess	dermatologic
706	Diseases of sebaceous glands	dermatologic
716	Other arthropathies	musculoskeletal
721	Spondylosis and allied disorders	musculoskeletal
722	Intervertebral disc disorders	musculoskeletal
726	Peripheral enthesopathies and allied syndromes	musculoskeletal
727	Other disorders of synovium, tendon, and bursa	musculoskeletal
735	Acquired foot deformities	musculoskeletal
740	Osteoarthrosis	musculoskeletal
743	Osteoporosis, osteopenia and pathological fracture	musculoskeletal
745	Pain in joint	musculoskeletal
760	Back pain	symptoms
773	Pain in limb	symptoms
781	Symptoms involving nervous and musculoskeletal systems	symptoms
785	Abdominal pain	symptoms
788	Syncope and collapse	symptoms
789	Nausea and vomiting	symptoms
800	Fracture of lower limb	injuries & poisonings
803	Fracture of upper limb	injuries & poisonings
835	Internal derangement of knee	injuries & poisonings
851	Complications of transplants and reattached limbs	injuries & poisonings
960	Poisoning by antibiotics	injuries & poisonings
965	Poisoning by analgesics, antipyretics, and antirheumatics	injuries & poisonings
1009	Injury, NOS	other
1010	Other tests	other
1019	Other ill-defined and unknown causes of morbidity and mortality	other
'''

# COMMAND ----------

## Create a DataFrame from the pasted code list
list_in = phers_predictor_text.splitlines()
codelist = []
for line in list_in:
    # list of tuples
    codelist.append(tuple(line.split("\t")))
header = ["PheCode_parent", "Phenotype_parent", "Category"]
phers_predictor_codelist = spark.createDataFrame(codelist, header)
display(phers_predictor_codelist)

## Save the code list
db_collab = ""
phers_predictor_codelist_name = "ccu022_01_PheRS_Predictor_PheCode_list"
phers_predictor_codelist.write.option("overwriteSchema", "true").saveAsTable(name=(db_collab + '.' + phers_predictor_codelist_name + file_date), mode="overwrite")

# COMMAND ----------

# MAGIC %md
# MAGIC ### Filter parent phecode diagnosis data to target population

# COMMAND ----------

# DBTITLE 1,Read combined diagnosis data from HES APC and GDPPR
db_collab = ""

## Combined data at parent phecode level
diag_combine_agg_pheparent_nhs_name = "ccu022_01_first_onset_combined_diagnosis_age40to69in2010to2020_Parent_PheCode_Cleaned_NHSID_2024_12_31"
diag_combine_agg_pheparent_nhs = spark.read.table(db_collab + '.' + diag_combine_agg_pheparent_nhs_name + version_flag + file_date)

# COMMAND ----------

# DBTITLE 1,Limit to main study population
## Read pt characteristics table
df_demo = spark.table(params.path_demographics)

## Get date of death 
df_death = df_demo.select('person_id', 'date_of_death')

## Subset to pts alive on or after 2015-01-01
df_death_filter = df_death.filter("(date_of_death is null) OR (date_of_death >= '2015-01-01')")

## Restrict to pts aged 40-69 on 2015-01-01 (1945-01-01 < yob <= 1975-01-01) and alive on or after 2015-01-01
diag_combine_target = diag_combine_agg_pheparent_nhs.join(df_death_filter, diag_combine_agg_pheparent_nhs.PID == df_death_filter.person_id, how='inner').filter((F.col('date_of_birth') <= '1975-01-01') & (F.col('date_of_birth') > '1945-01-01')).drop('person_id')

# COMMAND ----------

# DBTITLE 1,Save temp data
## Save diag data for this population (parent phecode level)
db_collab = ""
diag_combine_target_name = "ccu022_01_first_onset_combined_diagnosis_age40to69in2015_Parent_PheCode_Cleaned_NHSID_2024_12_31"
diag_combine_target.write.option("overwriteSchema", "true").saveAsTable(name=(db_collab + '.' + diag_combine_target_name + version_flag + file_date), mode="overwrite")

# COMMAND ----------

# MAGIC %md
# MAGIC ### Filter to PheRS phecode predictor list

# COMMAND ----------

## Read parent phecode diag data
db_collab = ""
diag_combine_target_name = "ccu022_01_first_onset_combined_diagnosis_age40to69in2015_Parent_PheCode_Cleaned_NHSID_2024_12_31"
diag_combine_target = spark.read.table(db_collab + '.' + diag_combine_target_name + version_flag + file_date)

## Read PheRS predictor phecode list
phers_predictor_codelist_name = "ccu022_01_PheRS_Predictor_PheCode_list"
phers_predictor_codelist = spark.read.table(db_collab + '.' + phers_predictor_codelist_name + file_date)

# COMMAND ----------

## Join with PheRS predictor list
pred_ds_df0 = diag_combine_target.join(phers_predictor_codelist.select('PheCode_parent'), on = "PheCode_parent", how = 'inner')

# COMMAND ----------

## Sanity check
pred_ds_df0.select("PheCode_parent").distinct().count()

# COMMAND ----------

## Find PheRS phecodes not captured
phers_predictor_codelist.select("PheCode_parent").join(pred_ds_df0.select('PheCode_parent').distinct(), on = "PheCode_parent", how = 'left_anti').display()

# COMMAND ----------

# DBTITLE 1,Limit to pre-index period
## Limit to diagnoses 3 months before index
pred_ds_df01 = pred_ds_df0.filter(F.col('DATE') < F.date_sub(F.lit('2015-01-01'), round(3*365.25/12)))
pred_ds_df01.select(F.min('DATE'), F.max('DATE')).display()

# COMMAND ----------

## Find PheRS phecodes not captured
phers_predictor_codelist.select("PheCode_parent").join(pred_ds_df01.select('PheCode_parent').distinct(), on = "PheCode_parent", how = 'left_anti').display()

# COMMAND ----------

# MAGIC %md
# MAGIC ### Subset to final study population

# COMMAND ----------

## Limit to study population of interest
pred_ds_df1 = pred_ds_df01.join(analysis_df.select('PID'), on = 'PID', how='inner')

pred_ds_df1.select('PID').distinct().count() 

# COMMAND ----------

# MAGIC %md
# MAGIC ###### Sanity check for diagnosis dates

# COMMAND ----------

## Check range of DATE: should be before (2015-01-01 - 3 months)
pred_ds_df1.select(F.min('DATE'), F.max('DATE')).display()

# COMMAND ----------

# Check any records with diagnosis dates < birth date
pred_ds_df1.filter(F.col('DATE') < F.col('date_of_birth')).count()

# COMMAND ----------

# MAGIC %md
# MAGIC ###### Save filtered predictor diagnosis data

# COMMAND ----------

## Save filtered predictor diagnosis data
db_collab = ""
pred_ds_df1_name = "ccu022_01_WhiteBritish_Age40_69_Index2015_PheRSPredictorDiseases_long"
pred_ds_df1.write.option("overwriteSchema", "true").saveAsTable(name=(db_collab + '.' + pred_ds_df1_name + file_date), mode="overwrite")

# COMMAND ----------

# MAGIC %md
# MAGIC ##### Transform diagnosis data from long to wide format

# COMMAND ----------

## Read saved data
db_collab = ""
pred_ds_df1_name = "ccu022_01_WhiteBritish_Age40_69_Index2015_PheRSPredictorDiseases_long"
pred_ds_df_long = spark.read.table(db_collab + '.' + pred_ds_df1_name + file_date)

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

# MAGIC %md
# MAGIC ##### Join with demo data

# COMMAND ----------

## Join with demo data (right join, set diag status to 0 for pts without any predictor diagnosis)
pred_df_final = pred_ds_df_wide.join(analysis_df.select('PID', 'sex_code', 'age_at_2015', 'even_birth_year', 'date_of_birth'), on='PID', how='right').fillna(0).withColumn('Female', F.col('sex_code') - 1).drop('sex_code').orderBy('PID')

display(pred_df_final)

# COMMAND ----------

pred_df_final.select("PID").distinct().count()

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
pred_df_final_name = "ccu022_01_WhiteBritish_Age40_69_Index2015_PheRSPredictorData"
pred_df_final.write.option("overwriteSchema", "true").saveAsTable(name=(db_collab + '.' + pred_df_final_name + file_date), mode="overwrite")

# COMMAND ----------

# MAGIC %md
# MAGIC ###### Save list of phecodes for PheRS predictor diseases

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
pred_ds_phecode_df_name = "ccu022_01_PheRS_Predictor_Diseases_Pheparent_list"
pred_ds_phecode_df.write.saveAsTable(name=(db_collab + '.' + pred_ds_phecode_df_name + file_date), mode="overwrite")


# COMMAND ----------

# MAGIC %md
# MAGIC ##### Subset to different age groups and even/odd birth years

# COMMAND ----------

## Read combined predictor ds and covariates data
db_collab = ""
pred_df_final_name = "ccu022_01_WhiteBritish_Age40_69_Index2015_PheRSPredictorData"
pred_df_final0 = spark.read.table(db_collab + '.' + pred_df_final_name + file_date)

# COMMAND ----------

## list of predictors
n_pred = 92
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
pred_df_age40_evenyob_name = "ccu022_01_WhiteBritish_Age40_49_EvenYOB_Index2015_PheRSPredictorData"
pred_df_age40_evenyob.write.option("overwriteSchema", "true").saveAsTable(name=(db_collab + '.' + pred_df_age40_evenyob_name + file_date), mode="overwrite")

pred_df_age40_oddyob_name = "ccu022_01_WhiteBritish_Age40_49_OddYOB_Index2015_PheRSPredictorData"
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
pred_df_age50_evenyob_name = "ccu022_01_WhiteBritish_Age50_59_EvenYOB_Index2015_PheRSPredictorData"
pred_df_age50_evenyob.write.option("overwriteSchema", "true").saveAsTable(name=(db_collab + '.' + pred_df_age50_evenyob_name + file_date), mode="overwrite")

pred_df_age50_oddyob_name = "ccu022_01_WhiteBritish_Age50_59_OddYOB_Index2015_PheRSPredictorData"
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
pred_df_age60_evenyob_name = "ccu022_01_WhiteBritish_Age60_69_EvenYOB_Index2015_PheRSPredictorData"
pred_df_age60_evenyob.write.option("overwriteSchema", "true").saveAsTable(name=(db_collab + '.' + pred_df_age60_evenyob_name + file_date), mode="overwrite")

pred_df_age60_oddyob_name = "ccu022_01_WhiteBritish_Age60_69_OddYOB_Index2015_PheRSPredictorData"
pred_df_age60_oddyob.write.option("overwriteSchema", "true").saveAsTable(name=(db_collab + '.' + pred_df_age60_oddyob_name + file_date), mode="overwrite")