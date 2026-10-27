## Data cleaning and cohort construction

Scripts in this folder were developed and implemented in the [NHS England SDE](https://digital.nhs.uk/services/secure-data-environment-service). Briefly, we cleaned and combined EHR data from HES APC and GDPPR (primary care), selected and curated predictor and outcome disease variables, and constructed the target study cohorts.

A description of each script is as follows:

| Script number/folder | Description |
|-------|---------|
| 0.1-0.3  | Read code lists for predictors and outcomes | 
| 1.0  | Clean and combine EHR data from HES APC and GDPPR<br>EHR data were aggregated to earliest diagnosis per individual per Phecode | 
| 2.0  | Selection of predictor and outcome diseases based on disease prevalence and other rules |
| 3.0-3.1  | Selection of two study cohorts with index dates of (1) 1 January 2015 (for non-COVID outcomes), and (2) 1 January 2020 (for COVID-19 hospitalisation outcome) |
| 4.0 | Extract COVID-19 hospitalisation cases from `covid_positive` table |
| 5.0-5.1 | Curate prediction and outcome data in White British individuals for the two study cohorts (Index 2015 and Index 2020) |
| 6.0-6.7 | Curate prediction and outcome data in individuals from (1) Asian, (2) Black, (3) Mixed, and (4) Other White or Irish ethnicity groups, for the two study cohorts (Index 2015 and Index 2020) |
| QCovid_curation | Curate predictor data needed for the [QCovid](https://www.qcovid.org/Home/Algorithm#unvaccinated) model |
| Delphi_comparison_curation | Curate study cohort for indirect comparison with the [Delphi-2M](https://www.nature.com/articles/s41586-025-09529-3) model, with matching age range (50-80) and follow-up period (1 July 2021 - 1 July 2022) |
| PheRS_curation | Curate predictor data needed for the [PheRS](https://www.nature.com/articles/s41588-025-02298-9) model | 
| CCI_IMD_curation | Curate data for Charlson Comorbidity Index (CCI) and Index of Multiple Deprivation (IMD) | 

