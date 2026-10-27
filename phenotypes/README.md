## Phenotype description

We developed Comorbidity Risk Scores (CRSs) for 88 common disease outcomes based on 212 predictor diseases.

### Outcomes

Outcomes include:

-   COVID-19 hospitalisation
    -   Defined as:
        -   ICD-10: U07.1 as primary diagnosis in HES APC, or
        -   Confirmed COVID-19 diagnosis in SARI Watch system
    -   Available from [`covid_positive`](https://bhfdsc.github.io/documentation/docs/curated_assets/covid_positive) table
-   Death
    -   All-cause mortality
-   86 other common diseases
    -   Defined based on [Phecode 1.2](https://phewascatalog.org/phewas/#phe12)
    -   Captured in the combined data of HES APC and GDPPR (primary care)
    -   See `CCU022_01_Outcome_Description.csv`

### Predictors

Predictors include 212 common diseases defined based on parent-level Phecodes, captured in the combined data of HES APC and GDPPR. See `CCU022_01_Predictor_Description.csv`.

### Code lists

ICD-10 and SNOMED CT code lists for outcome and predictor Phecodes are provided in `CCU022_01_Predictor_and_Outcome_Codelists.csv`, with the following key columns:

-   **Phecode**: Target outcome or predictor Phecode
-   **Code**: ICD-10 or SNOMED CT codes corresponding to **Phecode**
-   **Description**: Definition of **Code**
-   **Terminology**: Coding system

