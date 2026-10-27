## Data analysis for Index 2015 cohort

Scripts in this folder were for analyses of the study cohort with index date 1 January 2015. 

A description of each script is as follows:

| Script number/folder | Description |
|-------|---------|
| 1.0-1.1  | Save and pre-process curated predictor and outcome data from Databricks | 
| 2.0  | Calculate pairwise correlation among predictor diseases | 
| 2.1  | For each outcome, remove individuals with prior outcome diagnosis before the index date |
| 2.2  | For each outcome, obtain weights from inverse probability of censoring weighting (IPCW) |
| 2.3-2.4 | Save training data (White British individuals born in odd years; case-control ratio of 1:4) and testing data (White British individuals born in even years) |
| 3.0 | Fit CRS model for all outcomes (except for COVID-19 hospitalisation) in training data using IPCW-weighted logistic regression |
| 2.41, 3.01 | Fit CRS model for all outcomes in the full White British cohort |
| 3.2-3.3 | Investigation of specific predictor effect sizes across different outcomes  |
| 3.4 | Calibration of CRS model | 
| 4.0 | Main evaluation of CRS predictive performance in testing data | 
| 4.1 | Generate summary statistics of performance metrics for CRS | 
| 3.1, 4.01, 4.11 | Evaluate the performance of elastic net-based CRS model for selected outcomes | 
| 6.0, 7.0-7.3, 8.00-8.03 | Evaluate context-specific performance of CRS, stratified by age, sex, number of comorbidities, and year of outcome incidence | 
| 9.0-9.2 | Calculate observed risk ratio (top 5% of the predicted risks vs. population average) and evaluate calibration (observed vs. predicted risks across quantiles) of CRS in testing data  | 
| 11.0-11.4 | Evaluate the performance of PheRS | 
| 13.0, 14.0-14.3, 15.0-15.3 | Evaluate the performance improvement of CRS over CCI and IMD | 
| 16.0-16.3 | Evaluate CRS performance in the cohort with matched characteristics to Delphi-2M population | 
| Cross_ethnicity_evaluation_Index2015 | Evaluate the performance of CRS (trained in White British individuals) in different ethnicity groups | 

