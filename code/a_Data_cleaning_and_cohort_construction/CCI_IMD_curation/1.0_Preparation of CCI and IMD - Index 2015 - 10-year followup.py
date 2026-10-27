# Databricks notebook source
# MAGIC %md
# MAGIC ### Preparation of Charlson Comorbidity Index and IMD for study population with index 2015
# MAGIC ##### Pearl Liu adapted from BHF Data Science team

# COMMAND ----------

# MAGIC %md
# MAGIC # ICD-10 Quan Charlson Comorbidity Index 
# MAGIC  
# MAGIC **Description** This notebook calculates the Charlson Comorbidity Index (CCI) Quan variation based on ICD-10 codelists in HES APC data.  
# MAGIC
# MAGIC **Development** BHF Data Science Centre Health Data Science team
# MAGIC

# MAGIC
# MAGIC **Verson 02** Takes the *hx_out_1st_wide* for the first event date only from the Pipeline and calculates the Quan CCI. 
# MAGIC **When to use Version 02** When only the first event date is needed **after date censoring**. For all time or the first event date **before date censoring**, use Version 01. 
# MAGIC
# MAGIC **Contexts which this notebook can be applied on**
# MAGIC This notebook is exclusively designed to reflect the Quan CCI as published in the following article:
# MAGIC
# MAGIC ```
# MAGIC Quan H, Li B, Couris CM, Fushimi K, Graham P, Hider P, Januel JM, Sundararajan V. 
# MAGIC Updating and validating the Charlson comorbidity index and score for risk adjustment in hospital discharge abstracts using data from 6 countries. 
# MAGIC Am J Epidemiol. 2011 Mar 15;173(6):676-82. doi: 10.1093/aje/kwq433. Epub 2011 Feb 17. PMID: 21330339.
# MAGIC ```
# MAGIC The context is described in the following items. **Assess the applicability and generalisability to your specific study design**
# MAGIC - Setting: Admitted patient care data (which is not contextually mappable to primary care)
# MAGIC - Study population: Admitted patients over 18 years (in the Calgary health region, Alberta, Canada)
# MAGIC - ICD-10 version: Canasian version of the ICD-10
# MAGIC - Observational study design: Retrospective cohort study 
# MAGIC - Exposure: 17 comorbidities as primary exposures
# MAGIC - Outcome/dependent varialbe: One year all-cause mortality expressed as a numeric score 
# MAGIC - Methodology for model validation: 
# MAGIC   - Cox proportional hazard model
# MAGIC   - Stepwise method for assessing colinearity between exposures
# MAGIC   - Logistic regression (with 17 comorbidities, age, and sex as dependent variables) to evaluate the Chalson score
# MAGIC   - C statistics to assess the discrimination of models 
# MAGIC - Time window for exposure: one year prior to the final admission date within the study period
# MAGIC - Time window for measuring the outcome: one year post admission
# MAGIC - Final list of comorbidities and associatied weights:
# MAGIC |    | Label    | Weight | Comorbidity                                     | Indicator of mutual exclusivity                     |
# MAGIC |----|----------|--------|-------------------------------------------------|-----------------------------------------------------|
# MAGIC | 1  | mi       | 0      | Myocardial infarction                           | -                                                   |
# MAGIC | 2  | chf      | 2      | Congestive heart failure                        | -                                                   |
# MAGIC | 3  | pvd      | 0      | Peripheral vascular disease                     | -                                                   |
# MAGIC | 4  | cevd     | 0      | Cerebrovascular disease                         | -                                                   |
# MAGIC | 5  | dementia | 2      | Dementia                                        | -                                                   |
# MAGIC | 6  | cpd      | 1      | Chronic pulmonary disease                       | -                                                   |
# MAGIC | 7  | rheumd   | 1      | Rheumatilogic disease                           | -                                                   |
# MAGIC | 8  | pud      | 0      | Peptic ulcer disease                            | -                                                   |
# MAGIC | 9  | *diabwc*   | 1      | Diabetes with chronic complications             | True, with diabetes w/out complications diab        |
# MAGIC | 10 | hp       | 2      | Hemiplegia of paraplegia                        | -                                                   |
# MAGIC | 11 | rend     | 1      | Renal disease                                   | -                                                   |
# MAGIC | 12 | *msld*     | 4      | Moderate of severe liver disease                | True, with mild liver disease mld                   |
# MAGIC | 13 | *metacanc* | 6      | Metastatic solid tumor                          | True, with any malignancy canc                      |
# MAGIC | 14 | aids     | 4      | AIDS/HIV                                        | -                                                   |
# MAGIC | 15 | *diab*     | 0      | Diabetes without chronic complications          | True, with diabetis with complications diabwc       |
# MAGIC | 16 | *canc*     | 2      | Any malignancy, including leukemia and lymphoma | True, with metastatic cancer metacanc               |
# MAGIC | 17 | *mld*      | 2      | Mild liver disease                              | True, with moderate to severe condition mlsd        |
# MAGIC
# MAGIC
# MAGIC - **Important**
# MAGIC   - The following comorbidities are mutual-exclusive. If both mild and severe comorbidities are present in a patient's record, only the severe comorbidity will contribute to the final Chalson score.
# MAGIC     - mld and mlsd
# MAGIC     - diab and diebwc
# MAGIC     - canc and metacanc
# MAGIC     
# MAGIC **Notes on Quan Charlson Comorbidity Index (CCI)**
# MAGIC - Notes on Quan Charlson Comorbidity Index CCI:
# MAGIC   - The original Quan CCI is based on 12 conditions (vs 17 in original Chalrson index). This is reflected in the study by assigning a weight of zero to 5 excluded comorbidities.
# MAGIC   - The baseline date is the final admission date to the hospital in the study period. 
# MAGIC   - The look-back time window is one year. 
# MAGIC   - The presence/absence of a comorbidity (what is refered to as phenotyping in the context of this notebook) is determined by:
# MAGIC     - a) For the final admission (i.e., baseline admission) the ICD-10 codes of all 17 conditions in any diagnosis position **except for the primary position**
# MAGIC     - b) for any admission up to 1 year prior to the final admission, the ICD-10 codes of all 17 conditions in any diagnosis position
# MAGIC   - Outcome or interest: one year all-cause mortality (using proportional hazard model) from final admission date
# MAGIC     
# MAGIC **Notes on the implementation of Quan CCI in this notebook**
# MAGIC
# MAGIC - This notebook is a brach that takes the *hx_out* or *hx_out_1st* output of the *comorbidity* notebook in the pipeline as shown in the following code snippet:
# MAGIC
# MAGIC ```
# MAGIC comorbidity_all = spark.sql("SELECT * FROM _collab.shds_mm2_tmp_covariates_hx_out_wide")
# MAGIC
# MAGIC ```
# MAGIC - Phenotype extraction in the pipeline:
# MAGIC   - Use the code in the following notebooks under CCU002_07 pipeline:
# MAGIC     - parameters
# MAGIC     - codelist_outcome
# MAGIC     - curated_data
# MAGIC     - covariates
# MAGIC   - A collection of codes from pipeline to use is shown in: /Workspaces/_collab/SHDS/project_support/CCI/Temp_
# MAGIC   
# MAGIC
# MAGIC    - Extract the phenotype for 17 comorbidiites of Quan index (done in the following code snippet)
# MAGIC    
# MAGIC ```
# MAGIC   # dictionary - dataset, codelist, and ordering in the event of tied records
# MAGIC _hx_out_in = {
# MAGIC   'hes_apc': ['_hes_apc', 'codelist_hx_out_hes_apc',  1]
# MAGIC }
# MAGIC
# MAGIC # run codelist match and codelist match summary functions
# MAGIC _hx_out, _hx_out_1st, _hx_out_1st_wide = codelist_match(_hx_out_in, _name_prefix=f'cov_hx_out_'); print()
# MAGIC _hx_out_summ_name, _hx_out_summ_name_code = codelist_match_summ(_hx_out_in, _hx_out); print()
# MAGIC
# MAGIC # temp save
# MAGIC _hx_out_1st_wide = temp_save(df=_hx_out_1st_wide, out_name=f'{proj}_tmp_covariates_hx_out_1st_wide'); print()
# MAGIC ```
# MAGIC
# MAGIC    - Save the resutls for event dates suitable for your project (e.g., first event date)
# MAGIC       - Use _hx_out_1st_wide for the first eventdate
# MAGIC       - **Note that if your censor data before phenotyping, the actual first event date might have been filtered.**
# MAGIC - Extract the Quan index 
# MAGIC - Check the Quan index

# COMMAND ----------

# MAGIC %md
# MAGIC # Load modules and functions

# COMMAND ----------

import pandas as pd
import pyspark.sql.functions as F
import pyspark.sql.types as T
from pyspark.sql.window import Window

import re
import io
import datetime

file_date = "_by_PL_2025_05_01"  # latest file date

# COMMAND ----------

# MAGIC %run "./General_functions"

# COMMAND ----------

# MAGIC %md
# MAGIC # Preparation of diagnosis data

# COMMAND ----------

# MAGIC %md
# MAGIC ### Read Quan CCI codelist
# MAGIC
# MAGIC Code descriptions are extracted from the following sources:
# MAGIC
# MAGIC https://www.ncbi.nlm.nih.gov/books/NBK575402/
# MAGIC
# MAGIC https://www.datadictionary.nhs.uk/Covid19PRA/Diabetes.html
# MAGIC
# MAGIC https://classbrowser.nhs.uk/#/book/ICD-10-5TH-Edtion"

# COMMAND ----------

# MAGIC %md 
# MAGIC #### Note on code_type column
# MAGIC
# MAGIC The code_type column indicates that if the code is an incident diagnostic code or a historical/prevalence code. Here are notes on how we have decided about this flag. you might need to change this flag based on your study design (retrospective vs prospective, incident only vs prevalence only vs mixed cohort, quality assurance level).
# MAGIC - Any code with a description that indicates the code is "historical" or "pre-existing" is considered as a "prevalence code" with a flag = 0
# MAGIC - "Chronic" codes are considered as incident codes in this table. If in your study you determine to use acute diagnostic codes only as incident codes, change the associated flag to 0
# MAGIC - "Neonatal" and "Hereditary" conditions are considered as incident codes in this table. If you consider these categories as "historical codes", change the associated flag to 0
# MAGIC
# MAGIC The following phenotype extraction is perfomed without taking the code_type filed into account. If you need only one type of code, filter the codelist first. 

# COMMAND ----------

# DBTITLE 1,Quan CCI codelist
quan_codelist_header = ["name", "terminology", "code", "term", "code_type", "RecordDate"]
quan_icd10_codelist = [("mi", "ICD10", "I21", "I21 Acute myocardial infarction", "1", "20221212"),
 ("mi","ICD10","I22","I22 Subsequent myocardial infarction","1","20221212"),
 ("mi", "ICD10", "I252", "I25.2 Old myocardial infarction", "0", "20221212"),
 ("chf","ICD10","I099","I09.9 Rheumatic heart disease, unspecified","1","20221212"),
 ("chf","ICD10","I110","I11.0 Hypertensive heart disease with (congestive) heart failure","1","20221212"),
 ("chf","ICD10","I130","I13.0 Hypertensive heart and renal disease with (congestive) heart failure","1","20221212"),
 ("chf","ICD10","I132","I13.2 Hypertensive heart and renal disease with both (congestive) heart failure and renal failure","1","20221212"),
 ("chf", "ICD10", "I255", "I25.5 Ischaemic cardiomyopathy", "1", "20221212"),
 ("chf", "ICD10", "I420", "I42.0 Dilated cardiomyopathy", "1", "20221212"),
 ("chf","ICD10","I425","I42.5 Other restrictive cardiomyopathy","1","20221212"),
 ("chf", "ICD10", "I426", "I42.6 Alcoholic cardiomyopathy", "1", "20221212"),
 ("chf","ICD10","I427","I42.7 Cardiomyopathy due to drugs and other external agents","1","20221212"),
 ("chf", "ICD10", "I428", "I42.8 Other cardiomyopathies", "1", "20221212"),
 ("chf","ICD10","I429","I42.9 Cardiomyopathy, unspecified","1","20221212"),
 ("chf","ICD10","I43","I43 Cardiomyopathy in diseases classified elsewhere","1","20221212"),
 ("chf", "ICD10", "I50", "I50 Heart failure", "1", "20221212"),
 ("chf", "ICD10", "P290", "P29.0 Neonatal cardiac failure", "1", "20221212"),
 ("pvd", "ICD10", "I70", "I70 Atherosclerosis", "1", "20221212"),
 ("pvd","ICD10","I71","I71 Aortic aneurysm and dissection","1","20221212"),
 ("pvd","ICD10","I731","I73.1 Thromboangiitis obliterans [Buerger]","1","20221212"),
 ("pvd","ICD10","I738","I73.8 Other specified peripheral vascular diseases","1","20221212"),
 ("pvd","ICD10","I739","I73.9 Peripheral vascular disease, unspecified","1","20221212"),
 ("pvd", "ICD10", "I771", "I77.1 Stricture of artery", "1", "20221212"),
 ("pvd","ICD10","I790","I79.0 Aneurysm of aorta in diseases classified elsewhere","1","20221212"),
 ("pvd","ICD10","I792","I79.2 Peripheral angiopathy in diseases classified elsewhere","1","20221212"),
 ("pvd","ICD10","K551","K55.1 Chronic vascular disorders of intestine","1","20221212"),
 ("pvd","ICD10","K558","K55.8 Other vascular disorders of intestine","1","20221212"),
 ("pvd","ICD10","K559","K55.9 Vascular disorder of intestine, unspecified","1","20221212"),
 ("pvd","ICD10","Z958","Z95.8 Presence of other cardiac and vascular implants and grafts","1","20221212"),
 ("pvd","ICD10","Z959","Z95.9 Presence of cardiac and vascular implant and graft, unspecified","1","20221212"),
 ("cevd","ICD10","G45","G45 Transient cerebral ischaemic attacks and related syndromes","1","20221212"),
 ("cevd","ICD10","G46","G46 Vascular syndromes of brain in cerebrovascular diseases (I60-I67)","1","20221212"),
 ("cevd","ICD10","H340","H34.0 Transient retinal artery occlusion","1","20221212"),
 ("cevd", "ICD10", "I60", "I60 Subarachnoid haemorrhage", "1", "20221212"),
 ("cevd", "ICD10", "I61", "I61 Intracerebral haemorrhage", "1", "20221212"),
 ("cevd","ICD10","I62","I62 Other nontraumatic intracranial haemorrhage","1","20221212"),
 ("cevd", "ICD10", "I63", "I63 Cerebral infarction", "1", "20221212"),
 ("cevd","ICD10","I64","I64 Stroke, not specified as haemorrhage or infarction","1","20221212"),
 ("cevd","ICD10","I65","I65 Occlusion and stenosis of precerebral arteries, not resulting in cerebral infarction","1","20221212"),
 ("cevd","ICD10","I66","I66 Occlusion and stenosis of cerebral arteries, not resulting in cerebral infarction","1","20221212"),
 ("cevd","ICD10","I67","I67 Other cerebrovascular diseases","1","20221212"),
 ("cevd","ICD10","I68","I68 Cerebrovascular disorders in diseases classified elsewhere","1","20221212"),
 ("cevd","ICD10","I69","I69 Sequelae of cerebrovascular disease","1","20221212"),
 ("dementia","ICD10","F00","F00 Dementia in Alzheimer disease (G30.-)","1","20221212"),
 ("dementia", "ICD10", "F01", "F01 Vascular dementia", "1", "20221212"),
 ("dementia","ICD10","F02","F02 Dementia in other diseases classified elsewhere","1","20221212"),
 ("dementia", "ICD10", "F03", "F03 Unspecified dementia", "1", "20221212"),
 ("dementia","ICD10","F051","F05.1 Delirium superimposed on dementia","1","20221212"),
 ("dementia", "ICD10", "G30", "G30 Alzheimer disease", "1", "20221212"),
 ("dementia","ICD10","G311","G31.1 Senile degeneration of brain, not elsewhere classified","1","20221212"),
 ("cpd","ICD10","I278","I27.8 Other specified pulmonary heart diseases","1","20221212"),
 ("cpd","ICD10","I279","I27.9 Pulmonary heart disease, unspecified","1","20221212"),
 ("cpd","ICD10","J40","J40 Bronchitis, not specified as acute or chronic","1","20221212"),
 ("cpd","ICD10","J41","J41 Simple and mucopurulent chronic bronchitis","1","20221212"),
 ("cpd","ICD10","J42","J42 Unspecified chronic bronchitis","1","20221212"),
 ("cpd", "ICD10", "J43", "J43 Emphysema", "1", "20221212"),
 ("cpd","ICD10","J44","J44 Other chronic obstructive pulmonary disease","1","20221212"),
 ("cpd", "ICD10", "J45", "J45 Asthma", "1", "20221212"),
 ("cpd", "ICD10", "J46", "J46 Status asthmaticus", "1", "20221212"),
 ("cpd", "ICD10", "J47", "J47 Bronchiectasis", "1", "20221212"),
 ("cpd", "ICD10", "J60", "J60 Coalworker pneumoconiosis", "1", "20221212"),
 ("cpd","ICD10","J61","J61 Pneumoconiosis due to asbestos and other mineral fibres","1","20221212"),
 ("cpd","ICD10","J62","J62 Pneumoconiosis due to dust containing silica","1","20221212"),
 ("cpd","ICD10","J63","J63 Pneumoconiosis due to other inorganic dusts","1","20221212"),
 ("cpd", "ICD10", "J64", "J64 Unspecified pneumoconiosis", "1", "20221212"),
 ("cpd","ICD10","J65","J65 Pneumoconiosis associated with tuberculosis","1","20221212"),
 ("cpd","ICD10","J66","J66 Airway disease due to specific organic dust","1","20221212"),
 ("cpd","ICD10","J67","J67 Hypersensitivity pneumonitis due to organic dust","1","20221212"),
 ("cpd","ICD10","J684","J68.4 Chronic respiratory conditions due to chemicals, gases, fumes and vapours","1","20221212"),
 ("cpd","ICD10","J701","J70.1 Chronic and other pulmonary manifestations due to radiation","1","20221212"),
 ("cpd","ICD10","J703","J70.3 Chronic drug-induced interstitial lung disorders","1","20221212"),
 ("rheumd","ICD10","M05","M05 Seropositive rheumatoid arthritis","1","20221212"),
 ("rheumd", "ICD10", "M06", "M06 Other rheumatoid arthritis", "1", "20221212"),
 ("rheumd","ICD10","M315","M31.5 Giant cell arteritis with polymyalgia rheumatica","1","20221212"),
 ("rheumd","ICD10","M32","M32 Systemic lupus erythematosus","1","20221212"),
 ("rheumd", "ICD10", "M33", "M33 Dermatopolymyositis", "1", "20221212"),
 ("rheumd", "ICD10", "M34", "M34 Systemic sclerosis", "1", "20221212"),
 ("rheumd", "ICD10", "M351", "M35.1 Other overlap syndromes", "1", "20221212"),
 ("rheumd", "ICD10", "M353", "M35.3 Polymyalgia rheumatica", "1", "20221212"),
 ("rheumd","ICD10","M360","M36.0 Dermato(poly)myositis in neoplastic disease (C00-D48)","1","20221212"),
 ("pud", "ICD10", "K25", "K25 Gastric ulcer", "1", "20221212"),
 ("pud", "ICD10", "K26", "K26 Duodenal ulcer", "1", "20221212"),
 ("pud","ICD10","K27","K27 Peptic ulcer, site unspecified","1","20221212"),
 ("pud", "ICD10", "K28", "K28 Gastrojejunal ulcer", "1", "20221212"),
 ("mld", "ICD10", "B18", "B18 Chronic viral hepatitis", "1", "20221212"),
 ("mld", "ICD10", "K700", "K70.0 Alcoholic fatty liver", "1", "20221212"),
 ("mld", "ICD10", "K701", "K70.1 Alcoholic hepatitis", "1", "20221212"),
 ("mld","ICD10","K702","K70.2 Alcoholic fibrosis and sclerosis of liver","1","20221212"),
 ("mld","ICD10","K703","K70.3 Alcoholic cirrhosis of liver","1","20221212"),
 ("mld","ICD10","K709","K70.9 Alcoholic liver disease, unspecified","1","20221212"),
 ("mld","ICD10","K713","K71.3 Toxic liver disease with chronic persistent hepatitis","1","20221212"),
 ("mld","ICD10","K714","K71.4 Toxic liver disease with chronic lobular hepatitis","1","20221212"),
 ("mld","ICD10","K715","K71.5 Toxic liver disease with chronic active hepatitis","1","20221212"),
 ("mld","ICD10","K717","K71.7 Toxic liver disease with fibrosis and cirrhosis of liver","1","20221212"),
 ("mld","ICD10","K73","K73 Chronic hepatitis, not elsewhere classified","1","20221212"),
 ("mld","ICD10","K74","K74 Fibrosis and cirrhosis of liver","1","20221212"),
 ("mld","ICD10","K760","K76.0 Fatty (change of) liver, not elsewhere classified","1","20221212"),
 ("mld","ICD10","K762","K76.2 Central haemorrhagic necrosis of liver","1","20221212"),
 ("mld", "ICD10", "K763", "K76.3 Infarction of liver", "1", "20221212"),
 ("mld", "ICD10", "K764", "K76.4 Peliosis hepatis", "1", "20221212"),
 ("mld","ICD10","K768","K76.8 Other specified diseases of liver","1","20221212"),
 ("mld", "ICD10", "K769", "K76.9 Liver disease, unspecified", "1", "20221212"),
 ("mld", "ICD10", "Z944", "Z94.4 Liver transplant status", "1", "20221212"),
 ("diab","ICD10","E100","Type 1 diabetes mellitus - With coma","1","20221212"),
 ("diab","ICD10","E101","Type 1 diabetes mellitus - With ketoacidosis\nType 1 diabetes mellitus - With ketoacidosis","1","20221212"),
 ("diab","ICD10","E106","Type 1 diabetes mellitus - With other specified complications","1","20221212"),
 ("diab","ICD10","E108","Type 1 diabetes mellitus - With unspecified complications","1","20221212"),
 ("diab","ICD10","E109","Type 1 diabetes mellitus - Without complications","1","20221212"),
 ("diab","ICD10","E110","Type 2 diabetes mellitus - With coma","1","20221212"),
 ("diab","ICD10","E111","Type 2 diabetes mellitus - With ketoacidosis","1","20221212"),
 ("diab","ICD10","E116","Type 2 diabetes mellitus - With other specified complications","1","20221212"),
 ("diab","ICD10","E118","Type 2 diabetes mellitus - With unspecified complications","1","20221212"),
 ("diab","ICD10","E119","Type 2 diabetes mellitus - Without complications","1","20221212"),
 ("diab","ICD10","E120","Malnutrition-related diabetes mellitus - With coma","1","20221212"),
 ("diab","ICD10","E121","Malnutrition-related diabetes mellitus - With ketoacidosis","1","20221212"),
 ("diab","ICD10","E126","Malnutrition-related diabetes mellitus with ketoacidosis","1","20221212"),
 ("diab","ICD10","E128","Malnutrition-related diabetes mellitus with unspecified complications","1","20221212"),
 ("diab","ICD10","E129","Malnutrition-related diabetes mellitus without complications","1","20221212"),
 ("diab","ICD10","E130","Other specified diabetes mellitus with coma","1","20221212"),
 ("diab","ICD10","E131","Other specified diabetes mellitus with ketoacidosis","1","20221212"),
 ("diab","ICD10","E136","Other specified diabetes mellitus with other specified complications","1","20221212"),
 ("diab","ICD10","E138","Other specified diabetes mellitus with unspecified complications","1","20221212"),
 ("diab","ICD10","E139","Other specified diabetes mellitus without complications","1","20221212"),
 ("diab","ICD10","E140","Unspecified diabetes mellitus with coma","1","20221212"),
 ("diab","ICD10","E141","Unspecified diabetes mellitus with ketoacidosis","1","20221212"),
 ("diab","ICD10","E146","Unspecified diabetes mellitus with other specified complications","1","20221212"),
 ("diab","ICD10","E148","Unspecified diabetes mellitus with unspecified complications","1","20221212"),
 ("diab","ICD10","E149","Unspecified diabetes mellitus without complications","1","20221212"),
 ("diabwc","ICD10","E102","Type 1 diabetes mellitus with kidney complications","1","20221212"),
 ("diabwc","ICD10","E103","Type 1 diabetes mellitus with ophthalmic complications","1","20221212"),
 ("diabwc","ICD10","E104","Type 1 diabetes mellitus with neurological complications","1","20221212"),
 ("diabwc","ICD10","E105","Type 1 diabetes mellitus with circulatory complications","1","20221212"),
 ("diabwc","ICD10","E107","Type 1 diabetes mellitus with multiple complications","1","20221212"),
 ("diabwc","ICD10","E112","Type 2 diabetes mellitus with kidney complications","1","20221212"),
 ("diabwc","ICD10","E113","Type 2 diabetes mellitus with ophthalmic complications","1","20221212"),
 ("diabwc","ICD10","E114","Type 2 diabetes mellitus with neurological complications","1","20221212"),
 ("diabwc","ICD10","E115","Type 2 diabetes mellitus with circulatory complications","1","20221212"),
 ("diabwc","ICD10","E117","Malnutrition-related diabetes mellitus with multiple complications","1","20221212"),
 ("diabwc","ICD10","E122","Malnutrition-related diabetes mellitus with kidney complications","1","20221212"),
 ("diabwc","ICD10","E123","Malnutrition-related diabetes mellitus with ophthalmic complications","1","20221212"),
 ("diabwc","ICD10","E124","Malnutrition-related diabetes mellitus with neurological complications","1","20221212"),
 ("diabwc","ICD10","E125","Malnutrition-related diabetes mellitus with circulatory complications","1","20221212"),
 ("diabwc","ICD10","E127","Malnutrition-related diabetes mellitus with multiple complications","1","20221212"),
 ("diabwc","ICD10","E132","Other specified diabetes mellitus with kidney complications","1","20221212"),
 ("diabwc","ICD10","E133","Other specified diabetes mellitus with ophthalmic complications","1","20221212"),
 ("diabwc","ICD10","E134","Other specified diabetes mellitus with neurological complications","1","20221212"),
 ("diabwc","ICD10","E135","Diabetes mellitus with circulatory complications","1","20221212"),
 ("diabwc","ICD10","E137","Other specified diabetes mellitus with multiple complications","1","20221212"),
 ("diabwc","ICD10","E142","Unspecified diabetes mellitus with kidney complications","1","20221212"),
 ("diabwc","ICD10","E143","Unspecified diabetes mellitus with ophthalmic complications","1","20221212"),
 ("diabwc","ICD10","E144","Unspecified diabetes mellitus with neurological complications","1","20221212"),
 ("diabwc","ICD10","E145","Unspecified diabetes mellitus with circulatory complications","1","20221212"),
 ("diabwc","ICD10","E147","Unspecified diabetes mellitus with multiple complications","1","20221212"),
 ("hp", "ICD10", "G041", "Tropical spastic paraplegia", "1", "20221212"),
 ("hp", "ICD10", "G114", "Hereditary spastic paraplegia", "1", "20221212"),
 ("hp", "ICD10", "G801", "Spastic diplegic cerebral palsy", "1", "20221212"),
 ("hp", "ICD10", "G802", "Spastic hemiplegic cerebral palsy", "1", "20221212"),
 ("hp", "ICD10", "G81", "Hemiplegia and hemiparesis", "1", "20221212"),
 ("hp","ICD10","G82","Paraplegia (paraparesis) and quadriplegia (quadriparesis)","1","20221212"),
 ("hp", "ICD10", "G830", "Diplegia of upper limbs", "1", "20221212"),
 ("hp", "ICD10", "G831", "Monoplegia of lower limb", "1", "20221212"),
 ("hp", "ICD10", "G832", "Monoplegia of upper limb", "1", "20221212"),
 ("hp", "ICD10", "G833", "Monoplegia, unspecified", "1", "20221212"),
 ("hp", "ICD10", "G834", "Cauda equina syndrome", "1", "20221212"),
 ("hp", "ICD10", "G839", "Paralytic syndrome, unspecified", "1", "20221212"),
 ("rend","ICD10","I120","Hypertensive chronic kidney disease with stage 5 chronic kidney disease or end-stage renal disease","1","20221212"),
 ("rend","ICD10","I131","Hypertensive heart and chronic kidney disease without heart failure","1","20221212"),
 ("rend","ICD10","N032","Chronic nephritic syndrome with diffuse membranous glomerulonephritis","1","20221212"),
 ("rend","ICD10","N033","Chronic nephritic syndrome with diffuse mesangial proliferative glomerulonephritis","1","20221212"),
 ("rend","ICD10","N034","Chronic nephritic syndrome with diffuse endocapillary proliferative glomerulonephritis","1","20221212"),
 ("rend","ICD10","N035","Chronic nephritic syndrome with diffuse mesangiocapillary glomerulonephritis","1","20221212"),
 ("rend","ICD10","N036","Chronic nephritic syndrome with dense deposit disease","1","20221212"),
 ("rend","ICD10","N037","Chronic nephritic syndrome with diffuse crescentic glomerulonephritis","1","20221212"),
 ("rend","ICD10","N052","Unspecified nephritic syndrome with diffuse membranous glomerulonephritis","1","20221212"),
 ("rend","ICD10","N053","Unspecified nephritic syndrome with diffuse mesangial proliferative glomerulonephritis","1","20221212"),
 ("rend","ICD10","N054","Unspecified nephritic syndrome with diffuse endocapillary proliferative glomerulonephritis","1","20221212"),
 ("rend","ICD10","N055","Unspecified nephritic syndrome with diffuse mesangiocapillary glomerulonephritis","1","20221212"),
 ("rend","ICD10","N056","Unspecified nephritic syndrome with dense deposit disease","1","20221212"),
 ("rend","ICD10","N057","Unspecified nephritic syndrome with diffuse crescentic glomerulonephritis","1","20221212"),
 ("rend", "ICD10", "N18", "Chronic kidney disease", "1", "20221212"),
 ("rend", "ICD10", "N19", "Unspecified kidney failure", "1", "20221212"),
 ("rend", "ICD10", "N250", "Renal osteodystrophy", "1", "20221212"),
 ("rend","ICD10","Z490","Preparatory care for renal dialysis","1","20221212"),
 ("rend", "ICD10", "Z491", "Extracorporeal dialysis", "1", "20221212"),
 ("rend", "ICD10", "Z492", "Other dialysis", "1", "20221212"),
 ("rend", "ICD10", "Z940", "Kidney transplant status", "1", "20221212"),
 ("rend", "ICD10", "Z992", "Dependence on renal dialysis", "1", "20221212"),
 ("canc", "ICD10", "C00", "Malignant neoplasm of lip", "1", "20221212"),
 ("canc","ICD10","C01","Malignant neoplasm of base of tongue","1","20221212"),
 ("canc","ICD10","C02","Malignant neoplasm of other and unspecified parts of tongue","1","20221212"),
 ("canc", "ICD10", "C03", "Malignant neoplasm of gum", "1", "20221212"),
 ("canc","ICD10","C04","Malignant neoplasm of floor of mouth","1","20221212"),
 ("canc", "ICD10", "C05", "Malignant neoplasm of palate", "1", "20221212"),
 ("canc","ICD10","C06","Malignant neoplasm of other and unspecified parts of mouth","1","20221212"),
 ("canc","ICD10","C07","Malignant neoplasm of parotid gland","1","20221212"),
 ("canc","ICD10","C08","Malignant neoplasm of other and unspecified major salivary glands","1","20221212"),
 ("canc", "ICD10", "C09", "Malignant neoplasm of tonsil", "1", "20221212"),
 ("canc", "ICD10", "C10", "Malignant neoplasm of oropharynx", "1", "20221212"),
 ("canc","ICD10","C11","Malignant neoplasm of nasopharynx","1","20221212"),
 ("canc","ICD10","C12","Malignant neoplasm of pyriform sinus","1","20221212"),
 ("canc","ICD10","C13","Malignant neoplasm of hypopharynx","1","20221212"),
 ("canc","ICD10","C14","Malignant neoplasm of other and ill-defined sites in the lip, oral cavity and pharynx","1","20221212"),
 ("canc", "ICD10", "C15", "Malignant neoplasm of oesophagus", "1", "20221212"),
 ("canc", "ICD10", "C16", "Malignant neoplasm of stomach", "1", "20221212"),
 ("canc","ICD10","C17","Malignant neoplasm of small intestine","1","20221212"),
 ("canc", "ICD10", "C18", "Malignant neoplasm of colon", "1", "20221212"),
 ("canc","ICD10","C19","Malignant neoplasm of rectosigmoid junction","1","20221212"),
 ("canc", "ICD10", "C20", "Malignant neoplasm of rectum", "1", "20221212"),
 ("canc","ICD10","C21","Malignant neoplasm of anus and anal canal","1","20221212"),
 ("canc","ICD10","C22","Malignant neoplasm of liver and intrahepatic bile ducts","1","20221212"),
 ("canc","ICD10","C23","Malignant neoplasm of gallbladder","1","20221212"),
 ("canc","ICD10","C24","Malignant neoplasm of other and unspecified parts of biliary tract","1","20221212"),
 ("canc", "ICD10", "C25", "Malignant neoplasm of pancreas", "1", "20221212"),
 ("canc","ICD10","C26","Malignant neoplasm of other and ill-defined digestive organs","1","20221212"),
 ("canc","ICD10","C30","Malignant neoplasm of nasal cavity and middle ear","1","20221212"),
 ("canc","ICD10","C31","Malignant neoplasm of accessory sinuses","1","20221212"),
 ("canc", "ICD10", "C32", "Malignant neoplasm of larynx", "1", "20221212"),
 ("canc", "ICD10", "C33", "Malignant neoplasm of trachea", "1", "20221212"),
 ("canc","ICD10","C34","Malignant neoplasm of bronchus and lung","1","20221212"),
 ("canc", "ICD10", "C37", "Malignant neoplasm of thymus", "1", "20221212"),
 ("canc","ICD10","C38","Malignant neoplasm of heart, mediastinum and pleura","1","20221212"),
 ("canc","ICD10","C39","Malignant neoplasm of other and ill-defined sites in the respiratory system and intrathoracic organs","1","20221212"),
 ("canc","ICD10","C40","Malignant neoplasm of bone and articular cartilage of limbs","1","20221212"),
 ("canc","ICD10","C41","Malignant neoplasm of bone and articular cartilage of other and unspecified sites","1","20221212"),
 ("canc", "ICD10", "C43", "Malignant melanoma of skin", "1", "20221212"),
 ("canc", "ICD10", "C45", "Mesothelioma", "1", "20221212"),
 ("canc", "ICD10", "C46", "Kaposis sarcoma", "1", "20221212"),
 ("canc","ICD10","C47","Malignant neoplasm of peripheral nerves and autonomic nervous system","1","20221212"),
 ("canc","ICD10","C48","Malignant neoplasm of retroperitoneum and peritoneum","1","20221212"),
 ("canc","ICD10","C49","Malignant neoplasm of other connective and soft tissue","1","20221212"),
 ("canc", "ICD10", "C50", "Malignant neoplasm of breast", "1", "20221212"),
 ("canc", "ICD10", "C51", "Malignant neoplasm of vulva", "1", "20221212"),
 ("canc", "ICD10", "C52", "Malignant neoplasm of vagina", "1", "20221212"),
 ("canc","ICD10","C53","Malignant neoplasm of cervix uteri","1","20221212"),
 ("canc","ICD10","C54","Malignant neoplasm of corpus uteri","1","20221212"),
 ("canc","ICD10","C55","Malignant neoplasm of uterus, part unspecified","1","20221212"),
 ("canc", "ICD10", "C56", "Malignant neoplasm of ovary", "1", "20221212"),
 ("canc","ICD10","C57","Malignant neoplasm of other and unspecified female genital organs","1","20221212"),
 ("canc", "ICD10", "C58", "Malignant neoplasm of placenta", "1", "20221212"),
 ("canc", "ICD10", "C60", "Malignant neoplasm of penis", "1", "20221212"),
 ("canc", "ICD10", "C61", "Malignant neoplasm of prostate", "1", "20221212"),
 ("canc", "ICD10", "C62", "Malignant neoplasm of testis", "1", "20221212"),
 ("canc","ICD10","C63","Malignant neoplasm of other and unspecified male genital organs","1","20221212"),
 ("canc","ICD10","C64","Malignant neoplasm of kidney, except renal pelvis","1","20221212"),
 ("canc","ICD10","C65","Malignant neoplasm of renal pelvis","1","20221212"),
 ("canc", "ICD10", "C66", "Malignant neoplasm of ureter", "1", "20221212"),
 ("canc", "ICD10", "C67", "Malignant neoplasm of bladder", "1", "20221212"),
 ("canc","ICD10","C68","Malignant neoplasm of other and unspecified urinary organs","1","20221212"),
 ("canc","ICD10","C69","Malignant neoplasm of eye and adnexa","1","20221212"),
 ("canc", "ICD10", "C70", "Malignant neoplasm of meninges", "1", "20221212"),
 ("canc", "ICD10", "C71", "Malignant neoplasm of brain", "1", "20221212"),
 ("canc","ICD10","C72","Malignant neoplasm of spinal cord, cranial nerves and other parts of central nervous system","1","20221212"),
 ("canc","ICD10","C73","Malignant neoplasm of thyroid gland","1","20221212"),
 ("canc","ICD10","C74","Malignant neoplasm of adrenal gland","1","20221212"),
 ("canc","ICD10","C75","Malignant neoplasm of other endocrine glands and related structures","1","20221212"),
 ("canc","ICD10","C76","Malignant neoplasm of other and ill-defined sites","1","20221212"),
 ("canc", "ICD10", "C81", "Hodgkin?s lymphoma", "1", "20221212"),
 ("canc", "ICD10", "C82", "Follicular lymphoma", "1", "20221212"),
 ("canc", "ICD10", "C83", "Non-follicular lymphoma", "1", "20221212"),
 ("canc", "ICD10", "C84", "Mature T NK-cell lymphomas", "1", "20221212"),
 ("canc","ICD10","C85","Other specified and unspecified types of non-Hodgkin?s lymphoma","1","20221212"),
 ("canc","ICD10","C88","Malignant immunoproliferative diseases and certain other B-cell lymphomas","1","20221212"),
 ("canc","ICD10","C90","Multiple myeloma and malignant plasma cell neoplasms","1","20221212"),
 ("canc", "ICD10", "C91", "Lymphoid leukaemia", "1", "20221212"),
 ("canc", "ICD10", "C92", "Myeloid leukaemia", "1", "20221212"),
 ("canc", "ICD10", "C93", "Monocytic leukaemia", "1", "20221212"),
 ("canc","ICD10","C94","C94 Other leukaemias of specified cell type","1","20221212"),
 ("canc","ICD10","C95","Leukaemia of unspecified cell type","1","20221212"),
 ("canc","ICD10","C96","Other and unspecified malignant neoplasms of lymphoid, haematopoietic and related tissue","1","20221212"),
 ("canc","ICD10","C97","Malignant neoplasms of independent (primary) multiple sites","1","20221212"),
 ("msld","ICD10","I850","I85.0 Oesophageal varices with bleeding","1","20221212"),
 ("msld","ICD10","I859","I85.9 Oesophageal varices without bleeding","1","20221212"),
 ("msld", "ICD10", "I864", "I86.4 Gastric varices", "1", "20221212"),
 ("msld","ICD10","I982","I98.2 Oesophageal varices without bleeding in diseases classified elsewhere","1","20221212"),
 ("msld", "ICD10", "K704", "K70.4 Alcoholic hepatic failure", "1", "20221212"),
 ("msld","ICD10","K711","K71.1 Toxic liver disease with hepatic necrosis","1","20221212"),
 ("msld", "ICD10", "K721", "K72.1 Chronic hepatic failure", "1", "20221212"),
 ("msld","ICD10","K729","K72.9 Hepatic failure, unspecified","1","20221212"),
 ("msld","ICD10","K765","K76.5 Hepatic veno-occlusive disease","1","20221212"),
 ("msld", "ICD10", "K766", "K76.6 Portal hypertension", "1", "20221212"),
 ("msld", "ICD10", "K767", "K76.7 Hepatorenal syndrome", "1", "20221212"),
 ("metacanc","ICD10","C77","C77 Secondary and unspecified malignant neoplasm of lymph nodes","1","20221212"),
 ("metacanc","ICD10","C78","C78 Secondary malignant neoplasm of respiratory and digestive organs","1","20221212"),
 ("metacanc","ICD10","C79","C79 Secondary malignant neoplasm of other and unspecified sites","1","20221212"),
 ("metacanc","ICD10","C80","C80 Malignant neoplasm, without specification of site","1","20221212"),
 ("aids","ICD10","B20","B20 Human immunodeficiency virus [HIV] disease resulting in infectious and parasitic diseases","1","20221212"),
 ("aids","ICD10","B21","B21 Human immunodeficiency virus [HIV] disease resulting in malignant neoplasms","1","20221212"),
 ("aids","ICD10","B22","B22 Human immunodeficiency virus [HIV] disease resulting in other specified diseases","1","20221212"),
 ("aids","ICD10","B24","B24 Unspecified human immunodeficiency virus [HIV] disease","1","20221212")
  ]
 

# COMMAND ----------

# DBTITLE 1,Save codelist
codelist_hx_out_hes_apc = spark.createDataFrame(quan_icd10_codelist, quan_codelist_header)

db_collab = ""
cci_codelist_name = "ccu022_01_CCI_ICD10_codelist"
codelist_hx_out_hes_apc.write.option("overwriteSchema", "true").saveAsTable(name=(db_collab + '.' + cci_codelist_name), mode="overwrite")

# COMMAND ----------

# DBTITLE 1,Read again
db_collab = ""
cci_codelist_name = "ccu022_01_CCI_ICD10_codelist"
codelist_hx_out_hes_apc = spark.table(db_collab + '.' + cci_codelist_name)

display(codelist_hx_out_hes_apc)

# COMMAND ----------

## Check number of ICD10 codes per disease
codelist_hx_out_hes_apc.groupBy("name").count().orderBy("count").display()

# COMMAND ----------

# MAGIC %md
# MAGIC ### Prepare HES APC data

# COMMAND ----------

# DBTITLE 1,Original long data
## Read in the curated HES APC diagnosis table
db_collab = ""
hes_apc_name = "hds_curated_assets__hes_apc_diagnosis_2025_02_04"  ## latest dataset to use for analysis
hes_apc_df = spark.read.table(db_collab + "." + hes_apc_name)
display(hes_apc_df)

# COMMAND ----------

# DBTITLE 1,Processing
hes_apc_df_1 = (
   hes_apc_df
   .select(['person_id', F.col('epistart').alias('DATE'), 'code'])
   .where(F.col('person_id').isNotNull())  
   .where(F.col('DATE').isNotNull())
)

# COMMAND ----------

# DBTITLE 1,Limit to Index 2015 study population
db_collab = ""
analysis_df_2015_name = "ccu022_01_WhiteBritish_Age40_69_Index2015_StudyPopInfo"
file_date = "_by_PL_2025_05_01"
cohort_2015 = spark.read.table(db_collab + '.' + analysis_df_2015_name + file_date)

# COMMAND ----------

# CENSOR_DATE_END is the end of the lookback, i.e. 2015-01-01 - 3 months (2014-10-01)
# CENSOR_DATE_START is the start of the lookback, set up as DOB
hes_apc_2015 = hes_apc_df_1.join(cohort_2015.select("person_id", "date_of_birth"), on = "person_id", how = "inner").withColumn('CENSOR_DATE_START', F.col("date_of_birth")).withColumn('CENSOR_DATE_END', f.to_date(f.lit('2014-10-01'))).drop("date_of_birth")

hes_apc_2015_final = hes_apc_2015.where(f.col('DATE') >= F.col('CENSOR_DATE_START')).where(f.col('DATE') <= F.col('CENSOR_DATE_END'))

## Save data
db_collab = ""
hes_apc_2015_final_name = "ccu022_01_ForCCI_WhiteBritish_Age40_69_Index2015_HES_APC_diag"
hes_apc_2015_final.write.option("overwriteSchema", "true").saveAsTable(name=(db_collab + '.' + hes_apc_2015_final_name + file_date), mode="overwrite")

# COMMAND ----------

# DBTITLE 1,Read again
db_collab = ""
hes_apc_2015_final_name = "ccu022_01_ForCCI_WhiteBritish_Age40_69_Index2015_HES_APC_diag"
hes_apc_2015_final = spark.table(db_collab + '.' + hes_apc_2015_final_name + file_date)

display(hes_apc_2015_final)

# COMMAND ----------

# MAGIC %md
# MAGIC # Phenotype extraction
# MAGIC
# MAGIC
# MAGIC The approach is this notebook:
# MAGIC
# MAGIC - Extract phenotypes for any "code_type"
# MAGIC - Only for ICD10 codes in HES_APC (Diagnosis columns)
# MAGIC - Check for the presence of the codes in any diagnosis position 
# MAGIC - Save a table with phenotype results for the conditons in the Quan codelist for **the first event date only**. 
# MAGIC - **Note** You might need to adjust when to filter your cohort based on your phenotyping requirements and censoring end-points. If you are interested in all-time phenotyping (e.g., first ever, across all historic records, no left censoring applied), filtering the cohort before phenotyping would lead to the exclusion of many old diagnosis. In this case, filter your cohort after the phenotyping. If you are interested in the first/any diagnosis within you specific study time-window, you can apply filtering before phenotyping.  

# COMMAND ----------

# DBTITLE 1,Codelist match
_hx_out_in = {
  'hes_apc': ['hes_apc_2015_final', 'codelist_hx_out_hes_apc',  1]
}

# run codelist match and codelist match summary functions
_hx_out, _hx_out_1st, _hx_out_1st_wide = codelist_match_v2_test(_hx_out_in, _name_prefix=f'cov_hx_out_', stages_to_run=3); print()
_hx_out_summ_name, _hx_out_summ_name_code = codelist_match_summ(_hx_out_in, _hx_out); print()

# COMMAND ----------

# DBTITLE 1,Save 1st event wide data
db_collab = ""
_hx_out_1st_wide_name = "ccu022_01_ForCCI_WhiteBritish_Age40_69_Index2015_CCI_components_wide"
_hx_out_1st_wide.write.option("overwriteSchema", "true").saveAsTable(name=(db_collab + '.' + _hx_out_1st_wide_name + file_date), mode="overwrite")

# COMMAND ----------

# DBTITLE 1,Read again
db_collab = ""
_hx_out_1st_wide_name = "ccu022_01_ForCCI_WhiteBritish_Age40_69_Index2015_CCI_components_wide"
comorbidity_1st = spark.table(db_collab + '.' + _hx_out_1st_wide_name + file_date)

display(comorbidity_1st)

# COMMAND ----------

# DBTITLE 1,Sanity check
## MI
comorbidity_1st.where(F.col("cov_hx_out_mi_flag") == 1).count()

# COMMAND ----------

hes_apc_2015_final.where(F.col("code").isin(["I21", "I22", "I252"])).select("person_id").distinct().count()

# COMMAND ----------

# MAGIC %md
# MAGIC # Make CCI ready table (applied to phenotype data with first event date only)
# MAGIC
# MAGIC We are interested in the presence of any code at **any** event date from censoring start to end. If you are interested in the first or last eventdate, take the _hx_out_1st (_hx_out_last) as input. 

# COMMAND ----------

# Make a wide table with:
# - One row per patient
# - One column per comorbidity with a 0 or 1 flag indicateing the absence or presence of the comorbidity per patient
cci_df = comorbidity_1st.\
drop("CENSOR_DATE_START").drop("CENSOR_DATE_END")

columns = ["mi", "chf", "pvd", "cevd", "dementia", "cpd", "rheumd", "pud", "diab", "diabwc", "hp", "rend",
           "mld", "msld", "canc", "metacanc", "aids"]

for index, item in enumerate(columns):
  cci_df = cci_df.withColumnRenamed(f'''cov_hx_out_{item}_flag''', item).\
  drop(f'''cov_hx_out_{item}_date''').\
  withColumn(item, F.when(F.col(item).isNull(), F.lit(0)).otherwise(F.col(item)))


# COMMAND ----------

display(cci_df)

# COMMAND ----------

print(f' distinct count=  {cci_df.select("PERSON_ID").distinct().count()}')
print(f' total count=  {cci_df.count()}')

# COMMAND ----------

# MAGIC %md
# MAGIC # CCI calculator

# COMMAND ----------

# DBTITLE 1,Quan CCI weights
quan_weight_dic = {
    "mi": 0,
    "chf": 2,
    "pvd": 0,
    "cevd": 0,
    "dementia": 2,
    "cpd": 1,
    "rheumd": 1,
    "pud": 0,
    "diabwc": 1,
    "hp": 2,
    "rend": 1,
    "msld": 4,
    "metacanc": 6,
    "aids": 4,
    "diab": 0,
    "canc": 2,
    "mld": 2
}

# COMMAND ----------

# DBTITLE 1,Calculate CCI
# Quan CCI calculator
df = cci_df
# Mapping of comorbidities to Quan weights
map_all = quan_weight_dic

# Add columns to store weights
col_list = map_all.keys()
prefix = "quan"
new_col_list = [f'''{prefix}_{x}'''  for x in col_list]
for col, new_col in zip(col_list, new_col_list):
    df = df.withColumn(new_col, F.when(
        F.col(col) == 1, F.lit(map_all.get(col))).otherwise(F.lit(0)))
#Add another set of columns for mutual-exclusive Quan (original Quan)
prefix_mutual_exclusive = "mex"
no_mex_col_list = [x for x in new_col_list if x not in ["quan_diab", "quan_canc", "quan_mld"]]
mex_only_list = ["mex_quan_diab", "mex_quan_canc", "mex_quan_mld"]
# Add columns for mild conditions of mutually-exclusive comorbidity pairs
df = df.withColumn("mex_quan_diab", F.when(F.col("quan_diabwc") != 0, F.lit(
        0)).otherwise(F.col("quan_diab")))
df= df.withColumn("mex_quan_canc", F.when(F.col("quan_metacanc") != 0, F.lit(
        0)).otherwise(F.col("quan_canc")))
df= df.withColumn("mex_quan_mld", F.when(F.col("quan_msld") != 0, F.lit(
        0)).otherwise(F.col("quan_mld")))
mex_col_list= ["mex_quan_diab", "mex_quan_canc", "mex_quan_mld"]
new_col_list_mex = no_mex_col_list+mex_col_list

# Add a list of comorbidities as a column
df = df.withColumn("comorbidity_set", F.array([F.lit("temp")]))
for index, item in enumerate(col_list):
  df = df.withColumn(f'''temp_{item}''', F.when(F.col(item)==1, F.array(F.lit(item))).otherwise(F.array([F.lit("temp")])))
  df = df.withColumn("comorbidity_set", F.array_union(F.col("comorbidity_set"), F.col(f'''temp_{item}''')))
  df = df.drop(f'''temp_{item}''')
  df = df.withColumn("comorbidity_set", F.array_remove(F.col("comorbidity_set"), "temp"))

# Calculate Quan CCI 

# Comment out the next line to have Quan without mutual-exclusivity condition applied
df = df.withColumn("Experimental_quan_all", F.expr('+'.join(new_col_list))) 
df = df.withColumn("quan_index", F.expr('+'.join(new_col_list_mex)))
df_final = df
df_final= df_final.drop(*new_col_list)
df_final = df_final.drop(*new_col_list_mex)
df_final = df_final.drop(*col_list)
df_final = df_final.drop("Experimental_quan_all")
display(df_final)
 
# col_list = original flags indicating the presence of absence of a comorbidity (e.g. mi = 1 measns the patient has myocardial infarction)
# new_col_list = weights of each comorbidity. This set contains weights without taking mutual-exclusivity into account
# mex_col_list = The list of mild conditions of mutualy-exclusive conditions. for example if a patient has both mld and msld, and if we are interested in mutual-exclusive Quan index, 
# then quan_msld=4, mex_quan_mld = 0, and quan_mld=2 (which will be ignored for mutual-exclusivity) 
# new_col_list_mex = The list of columns to be used if mutual-exclusibity is needed.


# COMMAND ----------

# Print weights map
print(dict(sorted(quan_weight_dic.items())))

# COMMAND ----------

## Check min and max CCI
df_final.select(F.min(F.col("quan_index")), F.max(F.col("quan_index"))).show()

# COMMAND ----------

# MAGIC %md
# MAGIC # Join with study population and IMD variables

# COMMAND ----------

# MAGIC %md
# MAGIC ## Check IMD variables

# COMMAND ----------

cohort_2015.select(F.min("imd_quintile"), F.max("imd_quintile")).display()

# COMMAND ----------

cohort_2015.where(F.col("imd_quintile").isNull()).count()

# COMMAND ----------

cohort_2015.select(F.min("imd_decile"), F.max("imd_decile")).display()

# COMMAND ----------

cohort_2015.where(F.col("imd_decile").isNull()).count()

# COMMAND ----------

# MAGIC %md
# MAGIC ## Join with target study pop and IMD variables

# COMMAND ----------

## Note that pts with null IMD values are filled with 0
cci_imd_final  = df_final.drop("comorbidity_set").join(cohort_2015.select("person_id", "imd_quintile", "imd_decile"), on = "person_id", how = "right").fillna(0).withColumnRenamed("person_id", "PID")

display(cci_imd_final)

# COMMAND ----------

## Sanity check
print(cci_imd_final.count())
print(cohort_2015.count())

# COMMAND ----------

# DBTITLE 1,Save
db_collab = ""
cci_imd_final_name = "ccu022_01_WhiteBritish_Age40_69_Index2015_CCI_IMD_final"
cci_imd_final.write.option("overwriteSchema", "true").saveAsTable(name=(db_collab + '.' + cci_imd_final_name + file_date), mode="overwrite")

# COMMAND ----------

# MAGIC %md
# MAGIC # Extra:

# COMMAND ----------

# MAGIC %md
# MAGIC ## Alternative outputs for checking 

# COMMAND ----------

# output 1: dataframe with comorbidity flags available
df_flags = df.drop(*new_col_list).drop(*new_col_list_mex)

display(df_flags)

# COMMAND ----------

# output 2: dataframe with weights available. The mutual exclusive condition is applied

df_mutual_exclusive = df.drop(*col_list).drop("Experimental_quan_all")

display(df_mutual_exclusive)

# COMMAND ----------

# output 3: dataframe with weights available. The mutual exclusive condition is not applied
# Experimental_quan_all shows the Quan index without mutual exclusivity adjustment
# quan_index shows the index with mutual exclusivity adjustment
# Note: the value 0 in this table refers to the wight, not the presence/absence of comorbidity
df_experimental = df.drop(*col_list).drop(*mex_only_list)

display(df_experimental)

# COMMAND ----------

# output 4: Full column output
# All intermediate and output columns available 
df_full = df
display(df_full)

# COMMAND ----------

# MAGIC %md
# MAGIC ## Check Quan index

# COMMAND ----------

print(f' distinct count=  {df_final.select("PERSON_ID").distinct().count()}')
print(f' total count=  {df_final.count()}')

# COMMAND ----------

#Check the maximum
df_final.select(F.max(F.col("quan_index"))).show()

# COMMAND ----------

# Records with maximum Quan index
display(df_final.filter(F.col("quan_index")==24))

# COMMAND ----------

# Records with both mutual exclusive conditions present
a_col = "comorbidity_set"
df_mex = df_final.filter(((F.array_contains(F.col(a_col), "diab"))&(F.array_contains(F.col(a_col), "diabwc"))) | \
                  ((F.array_contains(F.col(a_col), "mld"))&(F.array_contains(F.col(a_col), "msld"))) | \
                  ((F.array_contains(F.col(a_col), "canc"))&(F.array_contains(F.col(a_col), "metacanc"))))
display(df_mex)

# COMMAND ----------

#Check  maximum
df_mex.select(F.max(F.col("quan_index"))).show()

# COMMAND ----------

#Check minimum
df_mex.select(F.min(F.col("quan_index"))).show()

# COMMAND ----------

# Check maximum records
display(df_mex.filter(F.col("quan_index")==24))

# COMMAND ----------

# Check minimum records
display(df_mex.filter(F.col("quan_index")==1))

# COMMAND ----------

# Records without combination of mutual exclusive conditions
df_nomex = df.join(df_mex.select("PERSON_ID"), on = ["PERSON_ID"], how='left_anti')
display(df_nomex)

# COMMAND ----------

#Check maximum
df_nomex.select(F.max(F.col("quan_index"))).show()

# COMMAND ----------

#Check minimum
df_nomex.select(F.min(F.col("quan_index"))).show()

# COMMAND ----------

# Check maximum records
display(df_nomex.filter(F.col("quan_index")==17))

# COMMAND ----------

# Check maximum records
display(df_nomex.filter(F.col("quan_index")==0))

# COMMAND ----------

# MAGIC %md
# MAGIC ## How to create a long version of Quan index table?

# COMMAND ----------

df_long = df.select("PERSON_ID", "quan_index", F.explode(F.col("comorbidity_set")).alias("comorbidity"))
display(df_long)
