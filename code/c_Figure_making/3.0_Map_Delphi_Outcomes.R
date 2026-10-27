### Author: Pearl Liu
### Map outcomes between CRS and Delphi-2M

#### Load packages and parameters ####
library(tidyverse)

summary_dir <- 'Summary_Stats/'

#### Read input files ####
phecode_icd_map <- 
  read.csv('Inputs/For SDE/phecode_icd10_map_with_parent_info_AllPheCodes.csv')

crs_outcomes <-
  read.csv('Protocol/SupplementaryTable1_Outcome_List.csv')

delphi_results <- 
  read.csv('Delphi_compare/prediction_ukb.csv') %>% na.omit()
colnames(delphi_results)[1] <- "Index"

#### Map between CRS outcomes (phecodes) and Delphi-2M outcomes (ICD10 codes) ####
## Focus on CRS outcome and 3-digit ICD10 codes
CRS_phecode_icd3_map <- phecode_icd_map %>%
  select(ICD10_code, PHECODE, phenotype, sex) %>%
  filter(PHECODE %in% crs_outcomes$Phecode) %>%
  filter(str_length(ICD10_code) == 3) %>%
  ## Add death
  rbind(data.frame("ICD10_code" = "Dea",
                   "PHECODE" = "Dea",
                   "phenotype" = "Death",
                   "sex" = "Both"))

CRS_phecode_delphi_map <- CRS_phecode_icd3_map %>%
  inner_join(delphi_results, by = join_by(ICD10_code == Target)) %>%
  select(PHECODE, ICD10_code, phenotype, sex, AUC_Delphi, AUC_Base) %>%
  rename(PHECODE_phenotype = phenotype,
         AUC_Base_Shmatko = AUC_Base) %>%
  arrange(PHECODE, ICD10_code)

## If one phecode maps to multiple ICD10 codes, 
## take the average of AUC values across the ICD10 codes
CRS_phecode_delphi_map2 <- CRS_phecode_delphi_map %>%
  group_by(PHECODE) %>%
  mutate(AUC_Delphi_avg = mean(AUC_Delphi),
         AUC_Base_Shmatko_avg = mean(AUC_Base_Shmatko))

## If one ICD code maps to multiple phecodes, 
## select the most specific child phecode
CRS_phecode_delphi_map2 %>%
  filter(ICD10_code != "Dea") %>%
  mutate(PHECODE = as.numeric(PHECODE)) %>%
  group_by(ICD10_code) %>%
  mutate(n_parent = length(unique(round(PHECODE)))) %>%
  pull(n_parent) %>%
  unique() # any ICD code that maps to multiple phecodes would map to the same parent code

CRS_phecode_delphi_map3 <- CRS_phecode_delphi_map2 %>%
  filter(ICD10_code != "Dea") %>%
  mutate(PHECODE = as.numeric(PHECODE)) %>%
  group_by(ICD10_code) %>%
  filter(PHECODE == max(PHECODE)) %>%
  mutate(PHECODE = as.character(PHECODE)) %>%
  rbind(CRS_phecode_delphi_map2 %>%
          filter(ICD10_code == "Dea")) %>%
  mutate(Outcome_phecode = str_replace(as.character(PHECODE), "\\.", "_"))

## Save CRS phecode vs. delphi ICD-10 map
write.csv(CRS_phecode_delphi_map3,
          "Inputs/For SDE/CRS_phecode_vs_Delphi_ICD10_map.csv",
          row.names = F)

## Read again
CRS_phecode_delphi_map3 <- read.csv("Inputs/For SDE/CRS_phecode_vs_Delphi_ICD10_map.csv")

#### Explore: Compare CRS vs. Delphi-2M performance ####
##### UPDATE TO LATEST: Read CRS performance #####
final_summ_stats_wb <- readRDS(paste0(summary_dir, 
                                      "ForDelphiComparison_Sex_Specific_All_Outcomes+COVID_PredictionMetrics_ControlCase4to1_WhiteBritish_IndexMid2020_Followup1yr.rds"))

crs_summ_stats <- final_summ_stats_wb$all_outcome_metrics_dt %>%
  filter(Model %in% c("CRS_all_predictors", "Age_Sex")) %>%
  select(Outcome_phecode, Model, AUROC_mean, AUROC_LB, AUROC_UB) %>%
  inner_join(select(final_summ_stats_wb$all_outcome_metrics_delta_dt, 
                    "Outcome_phecode", "Model", 
                    "Delta_AUROC_mean", "Delta_AUROC_LB", "Delta_AUROC_UB"),
             by = c("Outcome_phecode", "Model")) %>%
  pivot_wider(names_from = "Model", 
              values_from = c("AUROC_mean", "AUROC_LB", "AUROC_UB",
                              "Delta_AUROC_mean", "Delta_AUROC_LB", "Delta_AUROC_UB")) %>%
  mutate(Outcome_phecode = if_else(Outcome_phecode == "death", "Dea", Outcome_phecode))
  
##### Get final comparison data set #####
compare_summ_stats <- crs_summ_stats %>%
  inner_join(CRS_phecode_delphi_map3, by = "Outcome_phecode") %>%
  mutate(AUC_CRS = AUROC_mean_CRS_all_predictors,
         AUC_LB_CRS = AUROC_LB_CRS_all_predictors,
         AUC_UB_CRS = AUROC_UB_CRS_all_predictors,
         Delta_AUC_CRS = Delta_AUROC_mean_CRS_all_predictors,
         Delta_AUC_LB_CRS = Delta_AUROC_LB_CRS_all_predictors,
         Delta_AUC_UB_CRS = Delta_AUROC_UB_CRS_all_predictors,
         AUC_Delphi = AUC_Delphi_avg,
         Delta_AUC_Delphi = AUC_Delphi_avg - AUC_Base_Shmatko_avg) %>%
  select(PHECODE, PHECODE_phenotype, sex, 
         AUC_CRS, AUC_LB_CRS, AUC_UB_CRS,
         Delta_AUC_CRS, Delta_AUC_LB_CRS, Delta_AUC_UB_CRS,
         AUC_Delphi, Delta_AUC_Delphi) %>%
  distinct() %>%
  mutate(sex = if_else(sex == "Empty", "Both", sex))

## Save
write.csv(compare_summ_stats, paste0(summary_dir, 
                                     "CRS_vs_Delphi_compare_AUROC.csv"),
          row.names = F)

## Check outcomes with better performance for Delphi-2M vs. CRS
compare_summ_stats %>% 
  arrange(desc(AUC_Delphi - AUC_CRS)) %>%
  select(PHECODE_phenotype, AUC_CRS, AUC_Delphi) %>%
  View()

compare_summ_stats %>% 
  arrange(desc(Delta_AUC_Delphi - Delta_AUC_CRS)) %>%
  select(PHECODE_phenotype, Delta_AUC_CRS, Delta_AUC_Delphi) %>%
  View()

##### Summary #####
compare_results <- 
compare_summ_stats %>%
  summarise(CRS_better = sum(AUC_CRS - AUC_Delphi > 0),
            Delphi_better = sum(AUC_CRS - AUC_Delphi < 0),
            Delta_CRS_better = sum(Delta_AUC_CRS - Delta_AUC_Delphi > 0),
            Delta_Delphi_better = sum(Delta_AUC_CRS - Delta_AUC_Delphi < 0),
            Avg_CRS_Delphi_diff = mean(AUC_CRS - AUC_Delphi),
            Avg_CRS_Delphi_delta_diff = mean(Delta_AUC_CRS - Delta_AUC_Delphi))

View(compare_results)

##### Plot #####
compare_summ_stats <- read.csv(paste0(summary_dir, 
                                     "CRS_vs_Delphi_compare_AUROC.csv"))


## Absolute AUC
plot(compare_summ_stats$AUC_Delphi, 
     compare_summ_stats$AUC_CRS,
     xlim = c(0.4,0.9), ylim = c(0.4,0.9),
     xlab = "AUC (Delphi-2M)", ylab = "AUC (CRS)")
abline(0,1)

## Absolute AUC limited to non-sex-specific outcomes
plot(compare_summ_stats$AUC_Delphi[compare_summ_stats$sex %in% c("Both", "Empty")], 
     compare_summ_stats$AUC_CRS[compare_summ_stats$sex %in% c("Both", "Empty")],
     xlim = c(0.4,0.9), ylim = c(0.4,0.9),
     xlab = "AUC (Delphi-2M)", ylab = "AUC (CRS)")
abline(0,1)

## Delta AUC
plot(compare_summ_stats$Delta_AUC_Delphi, 
     compare_summ_stats$Delta_AUC_CRS,
     xlim = c(-0.12,0.24), ylim = c(-0.12,0.24),
     xlab = "Delta AUC (Delphi-2M)", ylab = "Delta AUC (CRS)")
abline(0,1)

## Delta AUC limited to non-sex-specific outcomes
plot(compare_summ_stats$Delta_AUC_Delphi[compare_summ_stats$sex %in% c("Both", "Empty")], 
     compare_summ_stats$Delta_AUC_CRS[compare_summ_stats$sex %in% c("Both", "Empty")],
     xlim = c(-0.12,0.2), ylim = c(-0.12,0.2),
     xlab = "Delta AUC (Delphi-2M)", ylab = "Delta AUC (CRS)")
abline(0,1)


