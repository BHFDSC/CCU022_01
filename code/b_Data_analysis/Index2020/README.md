## Data analysis for Index 2020 cohort

Scripts in this folder were for analyses of the study cohort with index date 1 January 2020, with a focus on COVID-19 hospitalisation outcome. 

A description of each script is as follows:

| Script number/folder | Description |
|-------|---------|
| 1.0-1.1  | Save and pre-process curated predictor and outcome data from Databricks | 
| 2.0, 2.02  | Calculate pairwise correlation among predictor diseases, and correlation among outcome diseases | 
| 2.1  | For each outcome, remove individuals with prior outcome diagnosis before the index date |
| 2.2  | For each outcome, obtain weights from inverse probability of censoring weighting (IPCW) |
| 2.3-2.4 | Save training data (White British individuals born in odd years; case-control ratio of 1:4) and testing data (White British individuals born in even years) |
| 3.0 | Fit CRS model for COVID-19 hospitalisation in training data using IPCW-weighted logistic regression |
| 2.41, 3.01 | Fit CRS model for COVID-19 hospitalisation in the full White British cohort |
| 3.4 | Calibration of CRS model for COVID-19 hospitalisation | 
| 4.01 | Main evaluation of CRS predictive performance for COVID-19 hospitalisation in testing data | 
| 4.1 | Generate summary statistics of performance metrics for CRS | 
| 6.0, 7.0-7.3 | Evaluate context-specific performance of CRS for COVID-19 hospitalisation, stratified by age, sex, number of comorbidities, and year of outcome incidence | 
| 9.0 | Calculate observed risk ratio (top 5% of the predicted risks vs. population average) of CRS in testing data for COVID-19 hospitalisation  | 
| 10.0-10.2 | Evaluate the performance of QCovid and QCovid + CRS | 
| 13.0, 14.0-14.2, 15.0-15.2 | Evaluate the performance improvement of CRS over CCI and IMD for COVID-19 hospitalisation | 
| Cross_ethnicity_evaluation_Index2020 | Evaluate the performance of CRS (trained in White British individuals) in different ethnicity groups for COVID-19 hospitalisation | 

