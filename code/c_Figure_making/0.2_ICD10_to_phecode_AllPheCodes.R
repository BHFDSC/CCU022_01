### Author: Pearl Liu
### Map between ICD-10 codes and all PheCodes

## Version 1.2 Map between ICD-10 (WHO) and phecode downloaded from: https://phewascatalog.org/phewas/#phe12

#### Load packages ####
library(tidyverse)

#### Read input files ####
phecode_map_source0 <- read.csv('Inputs/Phecode_map_v1_2_icd10_WHO_beta.csv')
phecode_def <- read.csv('Inputs/phecode_definitions1.2.csv')

#### Initial processing of source map data ####
phecode_map_source1 <- phecode_map_source0 %>%
  mutate(ICD10_code = str_replace(ICD10, '\\.', '')) # remove dot in ICD10 codes 

#### Update phecode-ICD10 map to include any leaf phecodes ####
## e.g., ICD10 codes for 401.2 should also include ICD10 codes for 401.21
phecode_vec <- phecode_map_source1$PHECODE %>% unique() %>% na.omit()
n_phecode <- length(phecode_vec)

icd10_map_list <- vector(mode = "list", length = n_phecode)

for (i in 1:n_phecode) {
  phecode_i <- phecode_vec[i]
  icd10_map_i <- phecode_map_source1 %>% 
    ## Map to all phecodes that start with the target phecode
    filter(startsWith(as.character(PHECODE), as.character(phecode_i)) & 
             PHECODE < phecode_i + 1) %>%  ## only map 8 to 8.5, etc. but not to 800
    select(PHECODE, ICD10_code) %>%
    rename(PHECODE_mapped = PHECODE)
  n_row <- nrow(icd10_map_i)
  if (n_row > 0) {
    add_i <- data.frame(PHECODE = rep(phecode_i, n_row),
                        icd10_map_i)
  } else {
    add_i <- data.frame(PHECODE = phecode_i,
                        PHECODE_mapped = NA,
                        ICD10_code = NA)
  }
  icd10_map_list[[i]] <- add_i
}

icd10_map <- do.call(rbind, icd10_map_list) %>% 
  arrange(PHECODE, PHECODE_mapped)

View(icd10_map)

phecode_map_source1_1 <- icd10_map %>%
  filter(!is.na(PHECODE)) %>%
  select(PHECODE, ICD10_code) %>%
  distinct()

anyNA(phecode_map_source1_1)
View(phecode_map_source1_1)

#### Obtain parent phecode for each phecode (e.g., 401 is the parent code of 401.2) and add phecode descriptions ####
## PL update 2025.05.14: add sex column
phecode_map_source2 <- phecode_map_source1_1 %>%
  mutate(PHECODE_parent = str_replace(PHECODE, '\\..*', '')) %>%
  select(ICD10_code, PHECODE, PHECODE_parent) %>%
  arrange(ICD10_code) %>%
  left_join(select(phecode_def, phecode, phenotype, sex), 
            by = join_by('PHECODE' == 'phecode'))

View(phecode_map_source2)

#### Get the distinct phecodes at the parent level and descriptions ####
phecode_map_source_parent0 <- phecode_map_source2 %>%
  select(PHECODE_parent) %>%
  distinct() %>%
  mutate(PHECODE_parent = as.numeric(PHECODE_parent)) %>%
  arrange(PHECODE_parent) %>%
  drop_na(PHECODE_parent) %>%
  left_join(select(phecode_def, phecode, phenotype, category), 
                   by = join_by('PHECODE_parent' == 'phecode')) %>%
  mutate(phenotype_parent = phenotype) %>%
  mutate(phenotype_parent_avail = ifelse(is.na(phenotype), 0, 1)) %>%
  select(PHECODE_parent, phenotype_parent, category, phenotype_parent_avail)

View(phecode_map_source_parent0)

phecode_map_source_parent1 <- phecode_map_source_parent0 %>%
  mutate(phenotype_parent = ifelse(PHECODE_parent == 305, 
                                 'Eating disorder/Anorexia nervosa',
                                 phenotype_parent)) %>%
  mutate(category = ifelse(PHECODE_parent == 305, 
                                   'mental disorders',
                                    category)) %>%
  select(PHECODE_parent, phenotype_parent, category)

nrow(phecode_map_source_parent1)  # 556
View(phecode_map_source_parent1)


#### Save parent-level phecode info along with ICD10-PheCode map tables ####
phecode_map_source3 <- phecode_map_source2  %>%
  mutate(PHECODE_parent = as.numeric(PHECODE_parent)) %>%
  drop_na(PHECODE) %>%
  left_join(phecode_map_source_parent1, by = 'PHECODE_parent') %>%
  select(ICD10_code, 
         PHECODE, PHECODE_parent, 
         phenotype, phenotype_parent, 
         category, sex) %>%
  mutate(sex = if_else(sex == "", "Empty", sex))

View(phecode_map_source3)
write.csv(phecode_map_source3, 
          'Inputs/For SDE/phecode_icd10_map_with_parent_info_AllPheCodes.csv',
          row.names = F)

phecode_map_source3 <- 
          read.csv('Inputs/For SDE/phecode_icd10_map_with_parent_info_AllPheCodes.csv')

nrow(phecode_map_source3) # 13335
phecode_map_source3$PHECODE %>% unique() %>% length() # 1570
phecode_map_source3$phenotype_parent %>% unique() %>% length() # 556
phecode_map_source3$ICD10_code %>% unique() %>% length() # 9060

phecode_map_source3 %>%
  filter(str_detect(ICD10_code, "^[A-N]")) %>%
  nrow() # 8858


#### Appendix ####
## function to count the number of digits after decimal point
count_digits <- function(phecode) {
  phecode <- as.character(phecode)
  if (grepl('\\.', phecode)) {
    n_digits <- nchar(strsplit(phecode, "\\.")[[1]][2])
  } else {
    n_digits <- 0
  }
  return(n_digits)
}

#### Compare our map with the map from: https://wei-lab.app.vumc.org/phecode-data/phecode_international_version ####
phecode_map_wei <- read.csv('Inputs/phecode_international_version_rolled.csv')
phecode_map_wei1 <- phecode_map_wei %>% 
  filter(!is.na(phecode)) %>%
  mutate(ICD10_code = str_replace(ICD10, '\\.', '')) %>%
  select(ICD10_code, phecode) %>%
  distinct() %>%
  arrange(ICD10_code, phecode)

phecode_map_pl <- 
  read.csv('Inputs/For SDE/phecode_icd10_map_with_parent_info_AllPheCodes.csv')
phecode_map_pl1 <- phecode_map_pl %>%
  select(ICD10_code, PHECODE) %>%
  rename(phecode = PHECODE) %>%
  arrange(ICD10_code, phecode)

all.equal(phecode_map_wei1, phecode_map_pl1)
