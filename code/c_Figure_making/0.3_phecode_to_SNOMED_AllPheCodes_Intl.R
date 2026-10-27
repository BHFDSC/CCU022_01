### Author: Pearl Liu
### Map between all PheCodes and SNOMED code - to use in GDPPR data
rm(list = ls())

#### Load packages ####
library(tidyverse)
#library(ATM)

#### Read input files ####
## SNOMED International: SNOMED CT to ICD-10 (WHO) map, 2025-02-01 version
snomed_icd10_map_source0 <- read.table('Inputs/der2_iisssccRefset_ExtendedMapFull_INT_20250201.txt',
                                      sep = '\t', fill = T, na.strings = '',
                                      header = T)

## Map between phecode and icd10 codes (contains all PheCodes)
phecode_icd10_map_source <- read.csv('Inputs/For SDE/phecode_icd10_map_with_parent_info_AllPheCodes.csv')

## Check number of distinct parent phecodes
phecode_icd10_map_source %>%
  select(PHECODE_parent) %>%
  distinct() %>%
  count() # 556

## Check phecodes above 1000
phecode_icd10_map_source %>%
  filter(PHECODE >= 1000) %>%
  View()

#### Save list of all SNOMED codes ####
snomed_intl <- snomed_icd10_map_source0$referencedComponentId %>% unique()
snomed_intl_dt <- data.frame(SNOMED = snomed_intl)
write.csv(snomed_intl_dt, 'Inputs/SNOMED_list_international_version_20250201.csv',
          row.names = F)

#### Processing of SNOMED-ICD10 map ####
snomed_icd10_map_source <- snomed_icd10_map_source0 %>%
  ## Limit to strict map rules, 
  ## where the map source concept is properly classified (see notes on mapCategoryId in technical guide).
  ## mapPriority is almost always 1 for these maps.
  ## Same as the approach in https://cran.r-project.org/web/packages/Rdiagnosislist/vignettes/SNOMEDcodelists.html
  filter(mapCategoryId == 447637006) %>%
  group_by(referencedComponentId) %>%
  ## Find the top priority target for each SNOMED
  filter(mapPriority == min(mapPriority)) %>%
  ungroup() %>%
  select(referencedComponentId, mapTarget) %>%
  rename(SNOMED = referencedComponentId,
         ICD10 = mapTarget) %>%
  mutate(ICD10_code = str_trim(sub("[.]","",ICD10))) %>%
  distinct() 

#### Get ICD-10 codes mapped from all phecodes ####
icd10_df <- phecode_icd10_map_source %>% 
  select(ICD10_code) %>%
  distinct()

#### Map from selected ICD-10 codes to SNOMED ####
icd10_vec <- icd10_df$ICD10_code %>% unique()
n_icd <- length(icd10_vec)

snomed_list <- vector(mode = "list", length = n_icd)

## identify SNOMED codes mapped to each unique ICD-10 code
for (i in 1:n_icd) {
  icd_i <- icd10_vec[i]
  snomed_i <- snomed_icd10_map_source %>% 
    ## Map to all ICD10 codes that start with the target ICD10 codes
    filter(startsWith(ICD10_code, icd_i)) %>%
    select(ICD10_code, SNOMED) %>%
    rename(ICD10_mapped = ICD10_code)
  n_row <- nrow(snomed_i)
  if (n_row > 0) {
    add_i <- data.frame(ICD10_code = rep(icd_i, n_row),
                        snomed_i)
  } else {
    add_i <- data.frame(ICD10_code = icd_i,
                        ICD10_mapped = NA,
                        SNOMED = NA)
  }
  snomed_list[[i]] <- add_i
}


snomed_map_df <- do.call(rbind, snomed_list) 

##### Map from SNOMED to Phecode - Intl version #####
snomed_map_all <- snomed_map_df %>% left_join(phecode_icd10_map_source,
                                         by = 'ICD10_code')

anyNA(snomed_map_all)
View(snomed_map_all)

snomed_map_all %>% select(PHECODE) %>% unique() %>% count()  
# total phecodes: 1570
nrow(snomed_map_all) # 284412

snomed_map_final <- snomed_map_all %>% filter(!is.na(SNOMED))
snomed_map_final %>% select(PHECODE) %>% unique() %>% count()  
# 1569 phecodes can be mapped to SNOMED

setdiff(snomed_map_all$PHECODE, snomed_map_final$PHECODE)
# 202.23 (Lymphosarcoma) cannot be mapped to SNOMED codes

nrow(snomed_map_final) # 284108

## Sanity check
snomed_map_final_phecodes <- snomed_map_final %>% select(PHECODE) %>% unique() %>% arrange(PHECODE)
phecode_orig <- phecode_icd10_map_source %>% select(PHECODE) %>% arrange(PHECODE)
all(snomed_map_final_phecodes$PHECODE %in% phecode_orig$PHECODE)


#### Save the SNOMED-ICD10-phecode mapping table ####
write.csv(snomed_map_final, 
          'Inputs/phecode_snomed_map_with_parent_info_AllPheCodes_Intl_StrictRules.csv',
          row.names = F)

snomed_map_final <- read.csv('Inputs/phecode_snomed_map_with_parent_info_AllPheCodes_Intl_StrictRules.csv')


snomed_map_final_simple0 <- snomed_map_final %>% 
  select(!(ICD10_mapped)) %>%
  distinct()
nrow(snomed_map_final_simple0)  # 281987

## Remove ICD10 columns to reduce redundant SNOMED codes
snomed_map_final_simple <- snomed_map_final %>% 
  select(!(ICD10_code:ICD10_mapped)) %>%
  distinct()
View(snomed_map_final_simple)

nrow(snomed_map_final_simple) # 190836

write.csv(snomed_map_final_simple, 
          'Inputs/phecode_snomed_map_with_parent_info_AllPheCodes_simple_Intl_StrictRules.csv',
          row.names = F)

#### Limit the mapping table to ICD10 A-N ####
snomed_map_final_AtoN <- snomed_map_final %>% 
  filter(str_detect(ICD10_code, "^[A-N]")) %>% 
  select(!ICD10_mapped) %>%
  distinct()

nrow(snomed_map_final_AtoN) # 173940

snomed_map_simple_AtoN <- snomed_map_final_AtoN %>% 
  select(!(ICD10_code)) %>%
  distinct()

nrow(snomed_map_simple_AtoN) # 117361

write.csv(snomed_map_simple_AtoN,
          'Inputs/phecode_snomed_map_with_parent_info_AllPheCodes_simple_Intl_StrictRules_ICD10AtoN.csv',
          row.names = F)

#### Save the map between ICD10 and SNOMED ####
snomed_map_final <- read.csv('Inputs/phecode_snomed_map_with_parent_info_AllPheCodes_Intl_StrictRules.csv')

snomed_icd_map <- snomed_map_final %>% 
  select(SNOMED, ICD10_code) %>%
  distinct()

nrow(snomed_icd_map)  # 198894
write.csv(snomed_icd_map, 
          'Inputs/snomed_ICD10_map_Intl_StrictRules.csv',
          row.names = F)


snomed_icd_map_AtoN <- snomed_icd_map %>%
  filter(str_detect(ICD10_code, "^[A-N]"))

nrow(snomed_icd_map_AtoN)  # 119492
write.csv(snomed_icd_map_AtoN, 
          'Inputs/snomed_ICD10_AtoN_map_Intl_StrictRules.csv',
          row.names = F)


snomed_icd_map_OtoZ <- snomed_icd_map %>%
  filter(str_detect(ICD10_code, "^[O-Z]"))

nrow(snomed_icd_map_OtoZ)  # 79402
write.csv(snomed_icd_map_OtoZ, 
          'Inputs/snomed_ICD10_OtoZ_map_Intl_StrictRules.csv',
          row.names = F)

#### Check the % of SNOMED codes with strictly mapped ICD-10 ####
snomed_icd10_map_source_orig <- snomed_icd10_map_source0 %>%
  select(referencedComponentId, mapTarget) %>%
  rename(SNOMED = referencedComponentId,
         ICD10 = mapTarget) %>%
  mutate(ICD10_code = sub("[.]","",ICD10)) %>%
  mutate(ICD10_code = sub("[?]","",ICD10_code)) %>%
  distinct()

snomed_all <- snomed_icd10_map_source_orig$SNOMED %>% unique()
snomed_filtered <- snomed_icd10_map_source$SNOMED %>% unique()

length(snomed_filtered)/length(snomed_all) * 100
# 80.65668

##### limit to ICD-10 codes starting with A-N #####
snomed_icd10_map_source_orig_AtoN <- snomed_icd10_map_source_orig %>%
  filter(str_detect(ICD10_code, "^[A-N]"))

snomed_all2 <- snomed_icd10_map_source_orig_AtoN$SNOMED %>% unique()
snomed_filtered2 <- snomed_map_final_AtoN$SNOMED %>% unique()

length(snomed_filtered2)/length(snomed_all2) * 100
# 97.76799

setdiff(snomed_all2, snomed_filtered2)[1:20]
