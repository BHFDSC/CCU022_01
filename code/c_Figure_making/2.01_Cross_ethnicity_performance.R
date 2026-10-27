### Author: Pearl Liu
### Plot cross-ethnicity performance ###
### Index: 2015-01-01, follow-up: 10 years (all outcomes)
### Index: 2020-01-01, follow-up: 5 years (COVID hospitalization)

rm(list = ls())
gc()

#### Load libraries ####
library(data.table)
library(tidyverse)
library(forcats)
library(doBy)
library(ggrepel)

#### Set parameters ####
read_dir <- 'Outputs/'
summary_dir <- 'Summary_Stats/'
figure_dir <- 'Figures_Manuscript/'
file_date <- '2025_05_01'

## Model names
model_name_vec <- c("CRS_all_predictors", 
                    "CRS_top100",
                    "CRS_top50", 
                    "CRS_top20", 
                    "CRS_top10",
                    "Age_Sex")
n_models <- length(model_name_vec)
## Control-case ratio in logistic regression of training data
control_case_ratio <- 4
## Number of covariates 
n_covar <- 2
## Number of Monte Carlo sampling in testing data
n_mc <- 100 ## PL 20260120: update from 200 to 100
## Control-case ratio in testing data for evaluating AUROC, C-index, R2
control_case_ratio_test <- 4
## Number of cases used in testing data
n_cases_test1 <- 3000
n_cases_test2 <- 1000

##### Plot parameters #####
base_font_size <- 24
base_point_size <- 4
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
safe_colorblind_palette <- c("#88CCEE", "#CC6677", "#DDCC77", "#117733", "#332288", "#AA4499",
                             "#44AA99", "#999933", "#882255", "#661100", "#6699CC", "#888888")
# set the commonly used color
blue <- cbPalette[6]
red <- cbPalette[7]
green <- cbPalette[4]
orange <- cbPalette[2]
grey <- cbPalette[1]
yellow <- cbPalette[5]
purple <- cbPalette[8]
skyblue <- cbPalette[3]

#### Read processed data ####
##### List and prevalence of predictors and outcomes #####
pred_ds_prev_name = "WhiteBritish_OddYOB_Index2015_PredictorDiseasePrevalence.csv"
outcome_ds_prev_name = "TrainData_Outcome_Incidence_OddYOB_Index2015_Followup10yr.csv"

pred_ds_prev = read.csv(paste0(read_dir, pred_ds_prev_name)) %>%
  mutate(Predictor_name = paste0('ds', Predictor_phecode))

outcome_ds_prev0 = read.csv(paste0(read_dir, outcome_ds_prev_name))
covid_prev <- data.frame("Outcome_phecode" = "COVID_hspt",
                         "Incidence_general" = 0.01027544,
                         "Incidence_sf" = NA)

outcome_ds_prev <- outcome_ds_prev0 %>%
  ## Remove phecode 622, as it corresponds to the same ICD10 codes as 622.1
  filter(Outcome_phecode != "622") %>%
  rbind(covid_prev) %>%
  mutate(Incidence_combined = if_else(is.na(Incidence_sf), Incidence_general, 
                                      Incidence_sf))

n_pred_ds <- nrow(pred_ds_prev)
n_outcome_ds <- nrow(outcome_ds_prev)



##### Phecode information #####
phe_icd10_codelist_name = "phecode_icd10_map_with_parent_info_AllPheCodes.csv"
icd10_phecode_map <- read.csv(paste0("Inputs/For SDE/", 
                                     phe_icd10_codelist_name))
phecode_info <- icd10_phecode_map %>%
  select(PHECODE, phenotype, category, sex) %>%
  distinct() %>%
  mutate(Outcome_phecode = str_replace(PHECODE, '\\.', '_'))

phecode_parent_info <- icd10_phecode_map %>%
  select(PHECODE_parent, phenotype_parent, category) %>%
  distinct() %>%
  mutate(Predictor_name = paste0("ds", PHECODE_parent))

#### UPDATE TO LATEST: Read all summary statistics ####
final_summ_stats_wb <- readRDS(paste0(summary_dir, 
                                      "Sex_Specific_All_Outcomes+COVID_PredictionMetrics_ControlCase",
                                      control_case_ratio, "to1_WhiteBritish_Index2015_Followup10yr.rds"))
final_summ_stats_ow <- readRDS(paste0(summary_dir, 
                                      "Sex_Specific_All_Outcomes+COVID_PredictionMetrics_ControlCase",
                                      control_case_ratio, "to1_OtherWhite_Index2015_Followup10yr.rds"))
final_summ_stats_asian <- readRDS(paste0(summary_dir, 
                                         "Sex_Specific_All_Outcomes+COVID_PredictionMetrics_ControlCase",
                                         control_case_ratio, "to1_Asian_Index2015_Followup10yr.rds"))
final_summ_stats_black <- readRDS(paste0(summary_dir, 
                                         "Sex_Specific_All_Outcomes+COVID_PredictionMetrics_ControlCase",
                                         control_case_ratio, "to1_Black_Index2015_Followup10yr.rds"))
final_summ_stats_mixed <- readRDS(paste0(summary_dir, 
                                         "Sex_Specific_All_Outcomes+COVID_PredictionMetrics_ControlCase",
                                         control_case_ratio, "to1_Mixed_Index2015_Followup10yr.rds"))


#### Fig. 3a: AUROC in other ethnicities vs. AUROC in White British ####
##### Combine performance metrics across ethnicities #####
auc_summ_wb <- final_summ_stats_wb$all_outcome_metrics_dt %>%
  filter(Model %in% c("CRS_all_predictors")) %>%
  filter(Outcome_phecode != "Avg") %>%
  select(Outcome_phecode, AUROC_mean, AUROC_LB, AUROC_UB) %>%
  rename(AUROC_mean_wb = AUROC_mean,
         AUROC_LB_wb = AUROC_LB,
         AUROC_UB_wb = AUROC_UB)

auc_summ_ow <- final_summ_stats_ow$all_outcome_metrics_dt %>%
  filter(Model %in% c("CRS_all_predictors")) %>%
  filter(Outcome_phecode != "Avg") %>%
  select(Outcome_phecode, AUROC_mean, AUROC_LB, AUROC_UB) %>%
  rename(AUROC_mean_ow = AUROC_mean,
         AUROC_LB_ow = AUROC_LB,
         AUROC_UB_ow = AUROC_UB)

auc_summ_asian <- final_summ_stats_asian$all_outcome_metrics_dt %>%
  filter(Model %in% c("CRS_all_predictors")) %>%
  filter(Outcome_phecode != "Avg") %>%
  select(Outcome_phecode, AUROC_mean, AUROC_LB, AUROC_UB) %>%
  rename(AUROC_mean_asian = AUROC_mean,
         AUROC_LB_asian = AUROC_LB,
         AUROC_UB_asian = AUROC_UB)

auc_summ_black <- final_summ_stats_black$all_outcome_metrics_dt %>%
  filter(Model %in% c("CRS_all_predictors")) %>%
  filter(Outcome_phecode != "Avg") %>%
  select(Outcome_phecode, AUROC_mean, AUROC_LB, AUROC_UB) %>%
  rename(AUROC_mean_black = AUROC_mean,
         AUROC_LB_black = AUROC_LB,
         AUROC_UB_black = AUROC_UB)

auc_summ_mixed <- final_summ_stats_mixed$all_outcome_metrics_dt %>%
  filter(Model %in% c("CRS_all_predictors")) %>%
  filter(Outcome_phecode != "Avg") %>%
  select(Outcome_phecode, AUROC_mean, AUROC_LB, AUROC_UB) %>%
  rename(AUROC_mean_mixed = AUROC_mean,
         AUROC_LB_mixed = AUROC_LB,
         AUROC_UB_mixed = AUROC_UB)

auc_summ_all <- auc_summ_wb %>%
  left_join(auc_summ_ow, by = "Outcome_phecode") %>%
  left_join(auc_summ_asian, by = "Outcome_phecode") %>%
  left_join(auc_summ_black, by = "Outcome_phecode") %>%
  left_join(auc_summ_mixed, by = "Outcome_phecode") %>%
  left_join(phecode_info[, c("Outcome_phecode", "phenotype")], 
            by = "Outcome_phecode") %>%
  mutate(phenotype = case_when(Outcome_phecode == "COVID_hspt" ~ "COVID-19 hospitalisation",
                               Outcome_phecode == "death" ~ "Death",
                               .default = phenotype))

## save auc_summ_all
saveRDS(auc_summ_all, paste0(summary_dir, "AUROC_all_outcomes_all_ethnic_groups.rds"))

##### Plot functions #####
selected_outcome_ds <- c(
  "COVID_hspt", 
  "death",
  "165_1",
  "250_2",
  #  "208",
  # "366", 
  "411_2")
#  "550_5",
#  "681_3",
# "994_2")
selected_outcome_phenotype <- c(
  "COVID-19 hospitalisation",
  "Death",
  "Cancer of bronchus; lung",
  "Type 2 diabetes",
  #    "Benign neoplasm of colon",
  #  "Cataract",
  "Myocardial infarction")
#    "Ventral hernia",
#      "Cellulitis and abscess of arm/hand",
#  "Sepsis")
auroc_cross_eth_plot <- function(auc_summ_select,
                                 eth_x_name = "White British",
                                 eth_y_name,
                                 dodge = position_dodge(0.8),
                                 ref_linewidth = 0.6,
                                 metric_lower_lim = 0.5,
                                 metric_upper_lim = 0.9,
                                 metric_breaks = 0.1,
                                 point_size = base_point_size,
                                 base_size = base_font_size,
                                 text_label_size = 6,
                                 error_bar_width = 0.01,
                                 label_top_N = T,
                                 point_padding = 1,
                                 box_padding = 0.5,
                                 nudge_x = 0,
                                 nudge_y = 0,
                                 seed = 100,
                                 save_format = "pdf",
                                 fig_width = 7,
                                 fig_height = 6) {
  eth_vs_wb_plot <- ggplot(auc_summ_select, 
                           aes(x = AUROC_mean_x, 
                               y = AUROC_mean_y)) +
    geom_abline(slope = 1, intercept = 0, 
                linewidth = ref_linewidth) +
    geom_point(size = point_size, color = grey) + 
    scale_x_continuous(limits = c(metric_lower_lim, metric_upper_lim), 
                       breaks = seq(metric_lower_lim, metric_upper_lim, 
                                    metric_breaks)) +
    scale_y_continuous(limits = c(metric_lower_lim, metric_upper_lim), 
                       breaks = seq(metric_lower_lim, metric_upper_lim, 
                                    metric_breaks)) +
    theme_bw(base_size = base_size) +
    theme( 
      # legend.position="right",
      # legend.key.spacing.y = unit(legend_spacing, 'cm'),
      #  panel.border = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank()
    ) + 
    labs(x = paste0("AUROC (", eth_x_name, ")"), 
         y = paste0("AUROC (", eth_y_name, ")"))
  
  if (label_top_N == T) {
    
    if (eth_y_name %in% c("Irish/Other White")) {
      selected_outcome_ds_final <- c(selected_outcome_ds, "401_1", "318")
      top_N_idx <- which(auc_summ_select$Outcome_phecode %in% selected_outcome_ds_final)
      all_predictor_dt_label <- auc_summ_select[top_N_idx,]
      nudge_x <- case_when(all_predictor_dt_label$phenotype == "Sepsis" ~ nudge_x - 0.03,
                           all_predictor_dt_label$phenotype == "Cataract" ~ nudge_x + 0.07,
                           all_predictor_dt_label$phenotype == "Essential hypertension" ~ nudge_x + 0.03,
                           all_predictor_dt_label$phenotype == "Tobacco use disorder" ~ nudge_x + 0.05,
                           all_predictor_dt_label$phenotype == "Cancer of bronchus; lung" ~ nudge_x - 0.05,
                           all_predictor_dt_label$phenotype == "COVID-19 hospitalisation" ~ nudge_x + 0.03,
                           all_predictor_dt_label$phenotype == "Myocardial infarction" ~ nudge_x - 0.06,
                           .default = nudge_x)
      nudge_y = case_when(all_predictor_dt_label$phenotype == "Sepsis" ~ nudge_y + 0.01,
                          all_predictor_dt_label$phenotype == "Cataract" ~ nudge_y - 0.02,
                          all_predictor_dt_label$phenotype == "Death" ~ nudge_y + 0.03,
                          all_predictor_dt_label$phenotype == "Cancer of bronchus; lung" ~ nudge_y + 0.05,
                          all_predictor_dt_label$phenotype == "Myocardial infarction" ~ nudge_y,
                          .default = nudge_y)
    } else if (eth_y_name %in% c("Asian")) {
      selected_outcome_ds_final <- c(selected_outcome_ds, "250_2", "401_1")
      top_N_idx <- which(auc_summ_select$Outcome_phecode %in% selected_outcome_ds_final)
      all_predictor_dt_label <- auc_summ_select[top_N_idx,]
      nudge_x <- case_when(all_predictor_dt_label$phenotype == "Sepsis" ~ nudge_x + 0.08,
                           all_predictor_dt_label$phenotype == "Essential hypertension" ~ nudge_x + 0,
                           all_predictor_dt_label$phenotype == "Death" ~ nudge_x + 0.02,
                           all_predictor_dt_label$phenotype == "Cancer of bronchus; lung" ~ nudge_x - 0.12,
                           all_predictor_dt_label$phenotype == "COVID-19 hospitalisation" ~ nudge_x + 0.05,
                           all_predictor_dt_label$phenotype == "Myocardial infarction" ~ nudge_x - 0.05,
                           .default = nudge_x)
      nudge_y = case_when(all_predictor_dt_label$phenotype == "Sepsis" ~ nudge_y - 0.01,
                          all_predictor_dt_label$phenotype == "Death" ~ nudge_y + 0,
                          all_predictor_dt_label$phenotype == "Cancer of bronchus; lung" ~ nudge_y + 0.06,
                          .default = nudge_y)
      
    } else if (eth_y_name %in% c("Black")) {
      selected_outcome_ds_final <- c(selected_outcome_ds, "401_1", "250_2")
      top_N_idx <- which(auc_summ_select$Outcome_phecode %in% selected_outcome_ds_final)
      all_predictor_dt_label <- auc_summ_select[top_N_idx,]
      nudge_x <- case_when(all_predictor_dt_label$phenotype == "Sepsis" ~ nudge_x + 0.08,
                           all_predictor_dt_label$phenotype == "Essential hypertension" ~ nudge_x + 0.05,
                           all_predictor_dt_label$phenotype == "Death" ~ nudge_x + 0.02,
                           all_predictor_dt_label$phenotype == "Cancer of bronchus; lung" ~ nudge_x - 0.12,
                           all_predictor_dt_label$phenotype == "COVID-19 hospitalisation" ~ nudge_x + 0.06,
                           all_predictor_dt_label$phenotype == "Myocardial infarction" ~ nudge_x - 0.05,
                           all_predictor_dt_label$phenotype == "Type 2 diabetes" ~ nudge_x + 0.04,
                           .default = nudge_x)
      nudge_y = case_when(all_predictor_dt_label$phenotype == "Sepsis" ~ nudge_y - 0.01,
                          all_predictor_dt_label$phenotype == "Death" ~ nudge_y + 0,
                          all_predictor_dt_label$phenotype == "Cancer of bronchus; lung" ~ nudge_y + 0.06,
                          .default = nudge_y)
    } else if (eth_y_name %in% c("Mixed")) {
      selected_outcome_ds_final <- c(selected_outcome_ds, "401_1", "250_2")
      top_N_idx <- which(auc_summ_select$Outcome_phecode %in% selected_outcome_ds_final)
      all_predictor_dt_label <- auc_summ_select[top_N_idx,]
      nudge_x <- case_when(all_predictor_dt_label$phenotype == "Sepsis" ~ nudge_x + 0.08,
                           all_predictor_dt_label$phenotype == "Essential hypertension" ~ nudge_x + 0.05,
                           all_predictor_dt_label$phenotype == "Death" ~ nudge_x + 0.02,
                           all_predictor_dt_label$phenotype == "Cancer of bronchus; lung" ~ nudge_x - 0.12,
                           all_predictor_dt_label$phenotype == "COVID-19 hospitalisation" ~ nudge_x + 0.06,
                           all_predictor_dt_label$phenotype == "Myocardial infarction" ~ nudge_x - 0.05,
                           all_predictor_dt_label$phenotype == "Type 2 diabetes" ~ nudge_x + 0.04,
                           .default = nudge_x)
      nudge_y = case_when(all_predictor_dt_label$phenotype == "Sepsis" ~ nudge_y - 0.01,
                          all_predictor_dt_label$phenotype == "Death" ~ nudge_y + 0,
                          all_predictor_dt_label$phenotype == "Cancer of bronchus; lung" ~ nudge_y + 0.06,
                          .default = nudge_y)
    }
    
    eth_vs_wb_plot <- eth_vs_wb_plot + 
      geom_point(data = auc_summ_select[top_N_idx,], 
                 color = red,
                 size = point_size) + 
      geom_label_repel(data = auc_summ_select[top_N_idx,], 
                       aes(label = phenotype, 
                           x = AUROC_mean_x, 
                           y = AUROC_mean_y),
                       size = text_label_size,
                       point.padding = point_padding,
                       box.padding = box_padding,
                       nudge_x = nudge_x,
                       nudge_y = nudge_y,
                       seed = seed)
    
  }
  
  eth_x_name_new <- "White_British"
  
  if (eth_y_name == "Irish/Other White") {
    eth_y_name_new <- "Irish_Other_White" 
  } else {
    eth_y_name_new <- eth_y_name
  }
  ggsave(filename = paste0(figure_dir, "Cross_ethnicity/AUROC_", 
                           eth_x_name_new, "_vs_",
                           eth_y_name_new,".", save_format), 
         eth_vs_wb_plot, 
         width = fig_width, 
         height = fig_height, 
         limitsize = FALSE)
}

##### Make plots #####
## Irish/Other White
auc_summ_ow_vs_wb <- auc_summ_all %>%
  mutate(AUROC_mean_x = AUROC_mean_wb,
         AUROC_mean_y = AUROC_mean_ow)

auroc_cross_eth_plot(auc_summ_ow_vs_wb,
                     eth_x_name = "White British",
                     eth_y_name = "Irish/Other White",
                     fig_width = 7,
                     fig_height = 6)

## Asian
auc_summ_asian_vs_wb <- auc_summ_all %>%
  mutate(AUROC_mean_x = AUROC_mean_wb,
         AUROC_mean_y = AUROC_mean_asian)

auroc_cross_eth_plot(auc_summ_asian_vs_wb,
                     eth_x_name = "White British",
                     eth_y_name = "Asian",
                     fig_width = 7,
                     fig_height = 6)

## Black
auc_summ_black_vs_wb <- auc_summ_all %>%
  mutate(AUROC_mean_x = AUROC_mean_wb,
         AUROC_mean_y = AUROC_mean_black)

auroc_cross_eth_plot(auc_summ_black_vs_wb,
                     eth_x_name = "White British",
                     eth_y_name = "Black",
                     fig_width = 7,
                     fig_height = 6)

## Mixed
auc_summ_mixed_vs_wb <- auc_summ_all %>%
  mutate(AUROC_mean_x = AUROC_mean_wb,
         AUROC_mean_y = AUROC_mean_mixed)

auroc_cross_eth_plot(auc_summ_mixed_vs_wb,
                     eth_x_name = "White British",
                     eth_y_name = "Mixed",
                     fig_width = 7,
                     fig_height = 6)


##### Slope from regression - UPDATE TO LATEST #####
summary(lm(AUROC_mean_ow ~ 0 + AUROC_mean_wb, data = auc_summ_all))
# Coefficients:
#                Estimate Std. Error t value Pr(>|t|)    
# AUROC_mean_wb  0.99928    0.00267   374.3   <2e-16 ***


summary(lm(AUROC_mean_asian ~ 0 + AUROC_mean_wb, data = auc_summ_all))
# Coefficients:
#               Estimate Std. Error t value Pr(>|t|)    
# AUROC_mean_wb 0.981110   0.004676   209.8   <2e-16 ***


summary(lm(AUROC_mean_black ~ 0 + AUROC_mean_wb, data = auc_summ_all))
# Coefficients:
#               Estimate Std. Error t value Pr(>|t|)    
# AUROC_mean_wb 0.972594   0.004641   209.6   <2e-16 ***


summary(lm(AUROC_mean_mixed ~ 0 + AUROC_mean_wb, data = auc_summ_all))
# Coefficients:
#               Estimate Std. Error t value Pr(>|t|)    
# AUROC_mean_wb 0.986468   0.003737     264   <2e-16 ***






#### Fig. S9: Cross-ethnicity delta AUROC: delta AUROC in other ethnicities vs. delta AUROC in White British ####
##### Combine performance metrics across ethnicities #####
delta_auc_summ_wb <- final_summ_stats_wb$all_outcome_metrics_delta_dt %>%
  filter(Model %in% c("CRS_all_predictors")) %>%
  select(Outcome_phecode, Delta_AUROC_mean, Delta_AUROC_LB, Delta_AUROC_UB) %>%
  rename(Delta_AUROC_mean_wb = Delta_AUROC_mean,
         Delta_AUROC_LB_wb = Delta_AUROC_LB,
         Delta_AUROC_UB_wb = Delta_AUROC_UB)

delta_auc_summ_ow <- final_summ_stats_ow$all_outcome_metrics_delta_dt %>%
  filter(Model %in% c("CRS_all_predictors")) %>%
  select(Outcome_phecode, Delta_AUROC_mean, Delta_AUROC_LB, Delta_AUROC_UB) %>%
  rename(Delta_AUROC_mean_ow = Delta_AUROC_mean,
         Delta_AUROC_LB_ow = Delta_AUROC_LB,
         Delta_AUROC_UB_ow = Delta_AUROC_UB)

delta_auc_summ_asian <- final_summ_stats_asian$all_outcome_metrics_delta_dt %>%
  filter(Model %in% c("CRS_all_predictors")) %>%
  select(Outcome_phecode, Delta_AUROC_mean, Delta_AUROC_LB, Delta_AUROC_UB) %>%
  rename(Delta_AUROC_mean_asian = Delta_AUROC_mean,
         Delta_AUROC_LB_asian = Delta_AUROC_LB,
         Delta_AUROC_UB_asian = Delta_AUROC_UB)

delta_auc_summ_black <- final_summ_stats_black$all_outcome_metrics_delta_dt %>%
  filter(Model %in% c("CRS_all_predictors")) %>%
  select(Outcome_phecode, Delta_AUROC_mean, Delta_AUROC_LB, Delta_AUROC_UB) %>%
  rename(Delta_AUROC_mean_black = Delta_AUROC_mean,
         Delta_AUROC_LB_black = Delta_AUROC_LB,
         Delta_AUROC_UB_black = Delta_AUROC_UB)

delta_auc_summ_mixed <- final_summ_stats_mixed$all_outcome_metrics_delta_dt %>%
  filter(Model %in% c("CRS_all_predictors")) %>%
  select(Outcome_phecode, Delta_AUROC_mean, Delta_AUROC_LB, Delta_AUROC_UB) %>%
  rename(Delta_AUROC_mean_mixed = Delta_AUROC_mean,
         Delta_AUROC_LB_mixed = Delta_AUROC_LB,
         Delta_AUROC_UB_mixed = Delta_AUROC_UB)

delta_auc_summ_all <- delta_auc_summ_wb %>%
  left_join(delta_auc_summ_ow, by = "Outcome_phecode") %>%
  left_join(delta_auc_summ_asian, by = "Outcome_phecode") %>%
  left_join(delta_auc_summ_black, by = "Outcome_phecode") %>%
  left_join(delta_auc_summ_mixed, by = "Outcome_phecode") %>%
  left_join(phecode_info[, c("Outcome_phecode", "phenotype")], 
            by = "Outcome_phecode") %>%
  mutate(phenotype = case_when(Outcome_phecode == "COVID_hspt" ~ "COVID-19 hospitalisation",
                               Outcome_phecode == "death" ~ "Death",
                               .default = phenotype))

##### Plot function #####
selected_outcome_ds <- c(
  "COVID_hspt", 
  "death",
  "165_1",
  "250_2",
  #  "208",
  # "366", 
  "411_2")
#  "550_5",
#  "681_3",
# "994_2")
selected_outcome_phenotype <- c(
  "COVID-19 hospitalisation",
  "Death",
  "Cancer of bronchus; lung",
  "Type 2 diabetes",
  #    "Benign neoplasm of colon",
  #  "Cataract",
  "Myocardial infarction")
#    "Ventral hernia",
#      "Cellulitis and abscess of arm/hand",
#  "Sepsis")

delta_auroc_cross_eth_plot <- function(delta_auc_summ_select,
                                       eth_x_name = "White British",
                                       eth_y_name,
                                       dodge = position_dodge(0.8),
                                       ref_linewidth = 0.6,
                                       metric_lower_lim = 0.5,
                                       metric_upper_lim = 0.9,
                                       metric_breaks = 0.1,
                                       point_size = base_point_size,
                                       base_size = base_font_size,
                                       text_label_size = 6,
                                       error_bar_width = 0.01,
                                       label_top_N = T,
                                       point_padding = 1,
                                       box_padding = 0.5,
                                       nudge_x = 0,
                                       nudge_y = 0,
                                       seed = 100,
                                       save_format = "pdf",
                                       fig_width = 7,
                                       fig_height = 6) {
  eth_x_name_p <- paste0("(", eth_x_name, ")")
  eth_y_name_p <- paste0("(", eth_y_name, ")")
  
  eth_vs_wb_plot <- ggplot(delta_auc_summ_select, 
                           aes(x = Delta_AUROC_mean_x, 
                               y = Delta_AUROC_mean_y)) +
    geom_abline(slope = 1, intercept = 0, 
                linewidth = ref_linewidth) +
    geom_hline(yintercept = 0, linetype = "dashed") + 
    geom_vline(xintercept = 0, linetype = "dashed") + 
    geom_point(size = point_size, color = grey) + 
    scale_x_continuous(limits = c(metric_lower_lim, metric_upper_lim), 
                       breaks = seq(metric_lower_lim, metric_upper_lim, 
                                    metric_breaks)) +
    scale_y_continuous(limits = c(metric_lower_lim, metric_upper_lim), 
                       breaks = seq(metric_lower_lim, metric_upper_lim, 
                                    metric_breaks)) +
    theme_bw(base_size = base_size) +
    theme( 
      # legend.position="right",
      # legend.key.spacing.y = unit(legend_spacing, 'cm'),
      #  panel.border = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank()
    ) + 
    labs(x = bquote(Delta * "AUROC" ~ .(eth_x_name_p)), 
         y = bquote(Delta * "AUROC" ~ .(eth_y_name_p)))
  
  if (label_top_N == T) {
    
    if (eth_y_name %in% c("Irish/Other White")) {
      selected_outcome_ds_final <- selected_outcome_ds
      top_N_idx <- which(delta_auc_summ_select$Outcome_phecode %in% selected_outcome_ds_final)
      all_predictor_dt_label <- delta_auc_summ_select[top_N_idx,]
      nudge_x <- case_when(all_predictor_dt_label$phenotype == "Death" ~ nudge_x + 0.04,
                           all_predictor_dt_label$phenotype == "Cancer of bronchus; lung" ~ nudge_x + 0.06,
                           all_predictor_dt_label$phenotype == "COVID-19 hospitalisation" ~ nudge_x,
                           all_predictor_dt_label$phenotype == "Myocardial infarction" ~ nudge_x - 0.05,
                           all_predictor_dt_label$phenotype == "Type 2 diabetes" ~ nudge_x + 0.05,
                           .default = nudge_x)
      nudge_y = case_when(
        all_predictor_dt_label$phenotype == "Death" ~ nudge_y,
        all_predictor_dt_label$phenotype == "Cancer of bronchus; lung" ~ nudge_y - 0.02,
        all_predictor_dt_label$phenotype == "COVID-19 hospitalisation" ~ nudge_y + 0.05,
        all_predictor_dt_label$phenotype == "Myocardial infarction" ~ nudge_y + 0.03,
        .default = nudge_y)
    } else if (eth_y_name %in% c("Asian")) {
      selected_outcome_ds_final <- c(selected_outcome_ds, "250_2", "318")
      top_N_idx <- which(delta_auc_summ_select$Outcome_phecode %in% selected_outcome_ds_final)
      all_predictor_dt_label <- delta_auc_summ_select[top_N_idx,]
      nudge_x <- case_when(
        all_predictor_dt_label$phenotype == "Death" ~ nudge_x + 0.03,
        all_predictor_dt_label$phenotype == "Cancer of bronchus; lung" ~ nudge_x + 0.1,
        all_predictor_dt_label$phenotype == "COVID-19 hospitalisation" ~ nudge_x,
        all_predictor_dt_label$phenotype == "Myocardial infarction" ~ nudge_x - 0.04,
        .default = nudge_x)
      nudge_y = case_when(all_predictor_dt_label$phenotype == "Sepsis" ~ nudge_y ,
                          all_predictor_dt_label$phenotype == "Death" ~ nudge_y - 0.02,
                          all_predictor_dt_label$phenotype == "Cancer of bronchus; lung" ~ nudge_y + 0.03 ,
                          all_predictor_dt_label$phenotype == "COVID-19 hospitalisation" ~ nudge_y + 0.05,
                          .default = nudge_y)
      
    } else if (eth_y_name %in% c("Black")) {
      selected_outcome_ds_final <- c(selected_outcome_ds, "401_1", "250_2")
      top_N_idx <- which(delta_auc_summ_select$Outcome_phecode %in% selected_outcome_ds_final)
      all_predictor_dt_label <- delta_auc_summ_select[top_N_idx,]
      nudge_x <- case_when(
        all_predictor_dt_label$phenotype == "Essential hypertension" ~ nudge_x + 0.05 ,
        all_predictor_dt_label$phenotype == "Death" ~ nudge_x,
        all_predictor_dt_label$phenotype == "Cancer of bronchus; lung" ~ nudge_x ,
        all_predictor_dt_label$phenotype == "COVID-19 hospitalisation" ~ nudge_x ,
        all_predictor_dt_label$phenotype == "Myocardial infarction" ~ nudge_x - 0.03,
        all_predictor_dt_label$phenotype == "Type 2 diabetes" ~ nudge_x ,
        .default = nudge_x)
      nudge_y = case_when(all_predictor_dt_label$phenotype == "Sepsis" ~ nudge_y ,
                          all_predictor_dt_label$phenotype == "Death" ~ nudge_y ,
                          all_predictor_dt_label$phenotype == "Cancer of bronchus; lung" ~ nudge_y ,
                          all_predictor_dt_label$phenotype == "Myocardial infarction" ~ nudge_y + 0.05,
                          all_predictor_dt_label$phenotype == "Type 2 diabetes" ~ nudge_y - 0.03 ,
                          all_predictor_dt_label$phenotype == "COVID-19 hospitalisation" ~ nudge_y + 0.03 ,
                          .default = nudge_y)
    } else if (eth_y_name %in% c("Mixed")) {
      selected_outcome_ds_final <- c(selected_outcome_ds, "250_2")
      top_N_idx <- which(delta_auc_summ_select$Outcome_phecode %in% selected_outcome_ds_final)
      all_predictor_dt_label <- delta_auc_summ_select[top_N_idx,]
      nudge_x <- case_when(
        all_predictor_dt_label$phenotype == "Death" ~ nudge_x + 0.03,
        all_predictor_dt_label$phenotype == "Cancer of bronchus; lung" ~ nudge_x + 0.06,
        all_predictor_dt_label$phenotype == "COVID-19 hospitalisation" ~ nudge_x ,
        all_predictor_dt_label$phenotype == "Myocardial infarction" ~ nudge_x - 0.05 ,
        all_predictor_dt_label$phenotype == "Type 2 diabetes" ~ nudge_x + 0.04,
        .default = nudge_x)
      nudge_y = case_when(
        all_predictor_dt_label$phenotype == "Death" ~ nudge_y ,
        all_predictor_dt_label$phenotype == "Cancer of bronchus; lung" ~ nudge_y  - 0.03,
        all_predictor_dt_label$phenotype == "COVID-19 hospitalisation" ~ nudge_y + 0.05 ,
        all_predictor_dt_label$phenotype == "Myocardial infarction" ~ nudge_y + 0.04 ,
        .default = nudge_y)
    }
    
    eth_vs_wb_plot <- eth_vs_wb_plot + 
      geom_point(data = delta_auc_summ_select[top_N_idx,], 
                 color = red,
                 size = point_size) + 
      geom_label_repel(data = delta_auc_summ_select[top_N_idx,], 
                       aes(label = phenotype, 
                           x = Delta_AUROC_mean_x, 
                           y = Delta_AUROC_mean_y),
                       size = text_label_size,
                       point.padding = point_padding,
                       box.padding = box_padding,
                       nudge_x = nudge_x,
                       nudge_y = nudge_y,
                       seed = seed)
    
  }
  
  eth_x_name_new <- "White_British"
  
  if (eth_y_name == "Irish/Other White") {
    eth_y_name_new <- "Irish_Other_White" 
  } else {
    eth_y_name_new <- eth_y_name
  }
  ggsave(filename = paste0(figure_dir, "Cross_ethnicity/Delta_AUROC_", 
                           eth_x_name_new, "_vs_",
                           eth_y_name_new,".", save_format), 
         eth_vs_wb_plot, 
         width = fig_width, 
         height = fig_height, 
         limitsize = FALSE)
}

##### Make plots #####
## Irish/Other White
delta_auc_summ_ow_vs_wb <- delta_auc_summ_all %>%
  mutate(Delta_AUROC_mean_x = Delta_AUROC_mean_wb,
         Delta_AUROC_mean_y = Delta_AUROC_mean_ow)

delta_auroc_cross_eth_plot(delta_auc_summ_ow_vs_wb,
                           eth_x_name = "White British",
                           eth_y_name = "Irish/Other White",
                           metric_lower_lim = -0.05,
                           metric_upper_lim = 0.2,
                           metric_breaks = 0.05,
                           fig_width = 7,
                           fig_height = 6)

## Asian
delta_auc_summ_asian_vs_wb <- delta_auc_summ_all %>%
  mutate(Delta_AUROC_mean_x = Delta_AUROC_mean_wb,
         Delta_AUROC_mean_y = Delta_AUROC_mean_asian)

delta_auroc_cross_eth_plot(delta_auc_summ_asian_vs_wb,
                           eth_x_name = "White British",
                           eth_y_name = "Asian",
                           metric_lower_lim = -0.05,
                           metric_upper_lim = 0.2,
                           metric_breaks = 0.05,
                           fig_width = 7,
                           fig_height = 6)

## Black
delta_auc_summ_black_vs_wb <- delta_auc_summ_all %>%
  mutate(Delta_AUROC_mean_x = Delta_AUROC_mean_wb,
         Delta_AUROC_mean_y = Delta_AUROC_mean_black)

delta_auroc_cross_eth_plot(delta_auc_summ_black_vs_wb,
                           eth_x_name = "White British",
                           eth_y_name = "Black",
                           metric_lower_lim = -0.05,
                           metric_upper_lim = 0.2,
                           metric_breaks = 0.05,
                           fig_width = 7,
                           fig_height = 6)

## Mixed
delta_auc_summ_mixed_vs_wb <- delta_auc_summ_all %>%
  mutate(Delta_AUROC_mean_x = Delta_AUROC_mean_wb,
         Delta_AUROC_mean_y = Delta_AUROC_mean_mixed)

delta_auroc_cross_eth_plot(delta_auc_summ_mixed_vs_wb,
                           eth_x_name = "White British",
                           eth_y_name = "Mixed",
                           metric_lower_lim = -0.05,
                           metric_upper_lim = 0.2,
                           metric_breaks = 0.05,
                           fig_width = 7,
                           fig_height = 6)


##### Slope from regression - UPDATE TO LATEST #####
summary(lm(Delta_AUROC_mean_asian ~ 0 + Delta_AUROC_mean_wb, 
           data = delta_auc_summ_all))
# Coefficients:
#                      Estimate   Std. Error t value Pr(>|t|)    
# Delta_AUROC_mean_wb  0.85547    0.03206   26.69   <2e-16 ***


summary(lm(Delta_AUROC_mean_black ~ 0 + Delta_AUROC_mean_wb, 
           data = delta_auc_summ_all))
# Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
# Delta_AUROC_mean_wb   0.9055     0.0282   32.11   <2e-16 ***


summary(lm(Delta_AUROC_mean_mixed ~ 0 + Delta_AUROC_mean_wb, 
           data = delta_auc_summ_all))
# Coefficients:
#                      Estimate Std. Error t value Pr(>|t|)    
# Delta_AUROC_mean_wb  0.97037    0.02352   41.26   <2e-16 ***

summary(lm(Delta_AUROC_mean_ow ~ 0 + Delta_AUROC_mean_wb, 
           data = delta_auc_summ_all))
# Coefficients:
#                      Estimate Std. Error t value Pr(>|t|)    
# Delta_AUROC_mean_wb  0.91957    0.01697   54.18   <2e-16 ***



#### Fig. S10: Cross-ethnicity C-index ####
##### Combine performance metrics across ethnicities #####
cindex_summ_wb <- final_summ_stats_wb$all_outcome_metrics_dt %>%
  filter(Model %in% c("CRS_all_predictors")) %>%
  filter(Outcome_phecode != "Avg") %>%
  select(Outcome_phecode, C_index_mean, C_index_LB, C_index_UB) %>%
  rename(C_index_mean_wb = C_index_mean,
         C_index_LB_wb = C_index_LB,
         C_index_UB_wb = C_index_UB)

cindex_summ_ow <- final_summ_stats_ow$all_outcome_metrics_dt %>%
  filter(Model %in% c("CRS_all_predictors")) %>%
  filter(Outcome_phecode != "Avg") %>%
  select(Outcome_phecode, C_index_mean, C_index_LB, C_index_UB) %>%
  rename(C_index_mean_ow = C_index_mean,
         C_index_LB_ow = C_index_LB,
         C_index_UB_ow = C_index_UB)

cindex_summ_asian <- final_summ_stats_asian$all_outcome_metrics_dt %>%
  filter(Model %in% c("CRS_all_predictors")) %>%
  filter(Outcome_phecode != "Avg") %>%
  select(Outcome_phecode, C_index_mean, C_index_LB, C_index_UB) %>%
  rename(C_index_mean_asian = C_index_mean,
         C_index_LB_asian = C_index_LB,
         C_index_UB_asian = C_index_UB)

cindex_summ_black <- final_summ_stats_black$all_outcome_metrics_dt %>%
  filter(Model %in% c("CRS_all_predictors")) %>%
  filter(Outcome_phecode != "Avg") %>%
  select(Outcome_phecode, C_index_mean, C_index_LB, C_index_UB) %>%
  rename(C_index_mean_black = C_index_mean,
         C_index_LB_black = C_index_LB,
         C_index_UB_black = C_index_UB)

cindex_summ_mixed <- final_summ_stats_mixed$all_outcome_metrics_dt %>%
  filter(Model %in% c("CRS_all_predictors")) %>%
  filter(Outcome_phecode != "Avg") %>%
  select(Outcome_phecode, C_index_mean, C_index_LB, C_index_UB) %>%
  rename(C_index_mean_mixed = C_index_mean,
         C_index_LB_mixed = C_index_LB,
         C_index_UB_mixed = C_index_UB)

cindex_summ_all <- cindex_summ_wb %>%
  left_join(cindex_summ_ow, by = "Outcome_phecode") %>%
  left_join(cindex_summ_asian, by = "Outcome_phecode") %>%
  left_join(cindex_summ_black, by = "Outcome_phecode") %>%
  left_join(cindex_summ_mixed, by = "Outcome_phecode") %>%
  left_join(phecode_info[, c("Outcome_phecode", "phenotype")], 
            by = "Outcome_phecode") %>%
  mutate(phenotype = case_when(Outcome_phecode == "COVID_hspt" ~ "COVID-19 hospitalisation",
                               Outcome_phecode == "death" ~ "Death",
                               .default = phenotype))

##### Plot function #####
cindex_cross_eth_plot <- function(cindex_summ_select,
                                   eth_x_name = "White British",
                                   eth_y_name,
                                   dodge = position_dodge(0.8),
                                   ref_linewidth = 0.6,
                                   metric_lower_lim = 0.5,
                                   metric_upper_lim = 0.9,
                                   metric_breaks = 0.1,
                                   point_size = base_point_size,
                                   base_size = base_font_size,
                                   text_label_size = 6,
                                   error_bar_width = 0.01,
                                   label_top_N = F,
                                   point_padding = 1,
                                   box_padding = 0.5,
                                   nudge_x = 0,
                                   nudge_y = 0,
                                   seed = 100,
                                   save_format = "pdf",
                                   fig_width = 7,
                                   fig_height = 6) {
  eth_x_name_p <- paste0("(", eth_x_name, ")")
  eth_y_name_p <- paste0("(", eth_y_name, ")")
  
  eth_vs_wb_plot <- ggplot(cindex_summ_select, 
                           aes(x = C_index_mean_x, 
                               y = C_index_mean_y)) +
    geom_abline(slope = 1, intercept = 0, 
                linewidth = ref_linewidth) +
   # geom_hline(yintercept = 0, linetype = "dashed") + 
   # geom_vline(xintercept = 0, linetype = "dashed") + 
    geom_point(size = point_size, color = grey) + 
    scale_x_continuous(limits = c(metric_lower_lim, metric_upper_lim), 
                       breaks = seq(metric_lower_lim, metric_upper_lim, 
                                    metric_breaks)) +
    scale_y_continuous(limits = c(metric_lower_lim, metric_upper_lim), 
                       breaks = seq(metric_lower_lim, metric_upper_lim, 
                                    metric_breaks)) +
    theme_bw(base_size = base_size) +
    theme( 
      # legend.position="right",
      # legend.key.spacing.y = unit(legend_spacing, 'cm'),
      #  panel.border = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank()
    ) + 
    labs(x = paste0("C-index ", eth_x_name_p), 
         y = paste0("C-index ", eth_y_name_p))
  
  eth_x_name_new <- "White_British"
  
  if (eth_y_name == "Irish/Other White") {
    eth_y_name_new <- "Irish_Other_White" 
  } else {
    eth_y_name_new <- eth_y_name
  }
  ggsave(filename = paste0(figure_dir, "Cross_ethnicity/C_index_", 
                           eth_x_name_new, "_vs_",
                           eth_y_name_new,".", save_format), 
         eth_vs_wb_plot, 
         width = fig_width, 
         height = fig_height, 
         limitsize = FALSE)
}

##### Make plots #####
## Asian
cindex_summ_asian_vs_wb <- cindex_summ_all %>%
  mutate(C_index_mean_x = C_index_mean_wb,
         C_index_mean_y = C_index_mean_asian)

cindex_cross_eth_plot(cindex_summ_asian_vs_wb,
                       eth_x_name = "White British",
                       eth_y_name = "Asian",
                       fig_width = 7,
                       fig_height = 6)

## Black
cindex_summ_black_vs_wb <- cindex_summ_all %>%
  mutate(C_index_mean_x = C_index_mean_wb,
         C_index_mean_y = C_index_mean_black)

cindex_cross_eth_plot(cindex_summ_black_vs_wb,
                       eth_x_name = "White British",
                       eth_y_name = "Black",
                       fig_width = 7,
                       fig_height = 6)

## Mixed
cindex_summ_mixed_vs_wb <- cindex_summ_all %>%
  mutate(C_index_mean_x = C_index_mean_wb,
         C_index_mean_y = C_index_mean_mixed)

cindex_cross_eth_plot(cindex_summ_mixed_vs_wb,
                       eth_x_name = "White British",
                       eth_y_name = "Mixed",
                       fig_width = 7,
                       fig_height = 6)

## Irish/Other White
cindex_summ_ow_vs_wb <- cindex_summ_all %>%
  mutate(C_index_mean_x = C_index_mean_wb,
         C_index_mean_y = C_index_mean_ow)

cindex_cross_eth_plot(cindex_summ_ow_vs_wb,
                       eth_x_name = "White British",
                       eth_y_name = "Irish/Other White",
                       fig_width = 7,
                       fig_height = 6)



##### Slope from regression - UPDATE TO LATEST #####
summary(lm(C_index_mean_asian ~ 0 + C_index_mean_wb, 
           data = cindex_summ_all))
# Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
# C_index_mean_wb 0.979927   0.004224     232   <2e-16 ***

summary(lm(C_index_mean_black ~ 0 + C_index_mean_wb, 
           data = cindex_summ_all))
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# C_index_mean_wb  0.97125    0.00424   229.1   <2e-16 ***

summary(lm(C_index_mean_mixed ~ 0 + C_index_mean_wb, 
           data = cindex_summ_all))
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# C_index_mean_wb 0.983633   0.003332   295.2   <2e-16 ***

summary(lm(C_index_mean_ow ~ 0 + C_index_mean_wb, 
           data = cindex_summ_all))
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# C_index_mean_wb 0.996111   0.002353   423.4   <2e-16 ***


#### Fig. S10: Cross-ethnicity liability R2 ####
##### Combine performance metrics across ethnicities #####
R2_liab_summ_wb <- final_summ_stats_wb$all_outcome_metrics_dt %>%
  filter(Model %in% c("CRS_all_predictors")) %>%
  filter(Outcome_phecode != "Avg") %>%
  select(Outcome_phecode, R2_liab_mean, R2_liab_LB, R2_liab_UB) %>%
  rename(R2_liab_mean_wb = R2_liab_mean,
         R2_liab_LB_wb = R2_liab_LB,
         R2_liab_UB_wb = R2_liab_UB)

R2_liab_summ_ow <- final_summ_stats_ow$all_outcome_metrics_dt %>%
  filter(Model %in% c("CRS_all_predictors")) %>%
  filter(Outcome_phecode != "Avg") %>%
  select(Outcome_phecode, R2_liab_mean, R2_liab_LB, R2_liab_UB) %>%
  rename(R2_liab_mean_ow = R2_liab_mean,
         R2_liab_LB_ow = R2_liab_LB,
         R2_liab_UB_ow = R2_liab_UB)

R2_liab_summ_asian <- final_summ_stats_asian$all_outcome_metrics_dt %>%
  filter(Model %in% c("CRS_all_predictors")) %>%
  filter(Outcome_phecode != "Avg") %>%
  select(Outcome_phecode, R2_liab_mean, R2_liab_LB, R2_liab_UB) %>%
  rename(R2_liab_mean_asian = R2_liab_mean,
         R2_liab_LB_asian = R2_liab_LB,
         R2_liab_UB_asian = R2_liab_UB)

R2_liab_summ_black <- final_summ_stats_black$all_outcome_metrics_dt %>%
  filter(Model %in% c("CRS_all_predictors")) %>%
  filter(Outcome_phecode != "Avg") %>%
  select(Outcome_phecode, R2_liab_mean, R2_liab_LB, R2_liab_UB) %>%
  rename(R2_liab_mean_black = R2_liab_mean,
         R2_liab_LB_black = R2_liab_LB,
         R2_liab_UB_black = R2_liab_UB)

R2_liab_summ_mixed <- final_summ_stats_mixed$all_outcome_metrics_dt %>%
  filter(Model %in% c("CRS_all_predictors")) %>%
  filter(Outcome_phecode != "Avg") %>%
  select(Outcome_phecode, R2_liab_mean, R2_liab_LB, R2_liab_UB) %>%
  rename(R2_liab_mean_mixed = R2_liab_mean,
         R2_liab_LB_mixed = R2_liab_LB,
         R2_liab_UB_mixed = R2_liab_UB)

R2_liab_summ_all <- R2_liab_summ_wb %>%
  left_join(R2_liab_summ_ow, by = "Outcome_phecode") %>%
  left_join(R2_liab_summ_asian, by = "Outcome_phecode") %>%
  left_join(R2_liab_summ_black, by = "Outcome_phecode") %>%
  left_join(R2_liab_summ_mixed, by = "Outcome_phecode") %>%
  left_join(phecode_info[, c("Outcome_phecode", "phenotype")], 
            by = "Outcome_phecode") %>%
  mutate(phenotype = case_when(Outcome_phecode == "COVID_hspt" ~ "COVID-19 hospitalisation",
                               Outcome_phecode == "death" ~ "Death",
                               .default = phenotype))

##### Plot function #####
R2_liab_cross_eth_plot <- function(R2_liab_summ_select,
                                 eth_x_name = "White British",
                                 eth_y_name,
                                 dodge = position_dodge(0.8),
                                 ref_linewidth = 0.6,
                                 metric_lower_lim = -0.1,
                                 metric_upper_lim = 0.4,
                                 metric_breaks = 0.1,
                                 point_size = base_point_size,
                                 base_size = base_font_size,
                                 text_label_size = 6,
                                 error_bar_width = 0.01,
                                 label_top_N = F,
                                 point_padding = 1,
                                 box_padding = 0.5,
                                 nudge_x = 0,
                                 nudge_y = 0,
                                 seed = 100,
                                 save_format = "pdf",
                                 fig_width = 7,
                                 fig_height = 6) {
  eth_x_name_p <- paste0("(", eth_x_name, ")")
  eth_y_name_p <- paste0("(", eth_y_name, ")")
  
  eth_vs_wb_plot <- ggplot(R2_liab_summ_select, 
                           aes(x = R2_liab_mean_x, 
                               y = R2_liab_mean_y)) +
    geom_abline(slope = 1, intercept = 0, 
                linewidth = ref_linewidth) +
    geom_hline(yintercept = 0, linetype = "dashed") + 
    geom_vline(xintercept = 0, linetype = "dashed") + 
    geom_point(size = point_size, color = grey) + 
    scale_x_continuous(limits = c(metric_lower_lim, metric_upper_lim), 
                       breaks = seq(metric_lower_lim, metric_upper_lim, 
                                    metric_breaks)) +
    scale_y_continuous(limits = c(metric_lower_lim, metric_upper_lim), 
                       breaks = seq(metric_lower_lim, metric_upper_lim, 
                                    metric_breaks)) +
    theme_bw(base_size = base_size) +
    theme( 
      # legend.position="right",
      # legend.key.spacing.y = unit(legend_spacing, 'cm'),
      #  panel.border = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank()
    ) + 
    labs(x = bquote(Liability ~ R^2 ~ .(eth_x_name_p)), 
         y = bquote(Liability ~ R^2 ~ .(eth_y_name_p)))
  
  eth_x_name_new <- "White_British"
  
  if (eth_y_name == "Irish/Other White") {
    eth_y_name_new <- "Irish_Other_White" 
  } else {
    eth_y_name_new <- eth_y_name
  }
  ggsave(filename = paste0(figure_dir, "Cross_ethnicity/R2_liab_", 
                           eth_x_name_new, "_vs_",
                           eth_y_name_new,".", save_format), 
         eth_vs_wb_plot, 
         width = fig_width, 
         height = fig_height, 
         limitsize = FALSE)
}

##### Make plots #####
## Asian
R2_liab_summ_asian_vs_wb <- R2_liab_summ_all %>%
  mutate(R2_liab_mean_x = R2_liab_mean_wb,
         R2_liab_mean_y = R2_liab_mean_asian)

R2_liab_cross_eth_plot(R2_liab_summ_asian_vs_wb,
                     eth_x_name = "White British",
                     eth_y_name = "Asian",
                     fig_width = 7,
                     fig_height = 6)

## Black
R2_liab_summ_black_vs_wb <- R2_liab_summ_all %>%
  mutate(R2_liab_mean_x = R2_liab_mean_wb,
         R2_liab_mean_y = R2_liab_mean_black)

R2_liab_cross_eth_plot(R2_liab_summ_black_vs_wb,
                     eth_x_name = "White British",
                     eth_y_name = "Black",
                     fig_width = 7,
                     fig_height = 6)

## Mixed
R2_liab_summ_mixed_vs_wb <- R2_liab_summ_all %>%
  mutate(R2_liab_mean_x = R2_liab_mean_wb,
         R2_liab_mean_y = R2_liab_mean_mixed)

R2_liab_cross_eth_plot(R2_liab_summ_mixed_vs_wb,
                     eth_x_name = "White British",
                     eth_y_name = "Mixed",
                     fig_width = 7,
                     fig_height = 6)

## Irish/Other White
R2_liab_summ_ow_vs_wb <- R2_liab_summ_all %>%
  mutate(R2_liab_mean_x = R2_liab_mean_wb,
         R2_liab_mean_y = R2_liab_mean_ow)

R2_liab_cross_eth_plot(R2_liab_summ_ow_vs_wb,
                       eth_x_name = "White British",
                       eth_y_name = "Irish/Other White",
                       fig_width = 7,
                       fig_height = 6)


##### Slope from regression - UPDATE TO LATEST #####
summary(lm(R2_liab_mean_asian ~ 0 + R2_liab_mean_wb, 
           data = R2_liab_summ_all))
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# R2_liab_mean_wb  0.87845    0.02741   32.05   <2e-16 ***

summary(lm(R2_liab_mean_black ~ 0 + R2_liab_mean_wb, 
           data = R2_liab_summ_all))
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# R2_liab_mean_wb  0.77115    0.02082   37.05   <2e-16 ***

summary(lm(R2_liab_mean_mixed ~ 0 + R2_liab_mean_wb, 
           data = R2_liab_summ_all))
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# R2_liab_mean_wb  0.87868    0.02143      41   <2e-16 ***

summary(lm(R2_liab_mean_ow ~ 0 + R2_liab_mean_wb, 
           data = R2_liab_summ_all))
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# R2_liab_mean_wb  0.94752    0.01866   50.79   <2e-16 ***



#### Fig. S10: Cross-ethnicity observed R2 ####
##### Combine performance metrics across ethnicities #####
R2_obs_summ_wb <- final_summ_stats_wb$all_outcome_metrics_dt %>%
  filter(Model %in% c("CRS_all_predictors")) %>%
  filter(Outcome_phecode != "Avg") %>%
  select(Outcome_phecode, R2_obs_mean, R2_obs_LB, R2_obs_UB) %>%
  rename(R2_obs_mean_wb = R2_obs_mean,
         R2_obs_LB_wb = R2_obs_LB,
         R2_obs_UB_wb = R2_obs_UB)

R2_obs_summ_ow <- final_summ_stats_ow$all_outcome_metrics_dt %>%
  filter(Model %in% c("CRS_all_predictors")) %>%
  filter(Outcome_phecode != "Avg") %>%
  select(Outcome_phecode, R2_obs_mean, R2_obs_LB, R2_obs_UB) %>%
  rename(R2_obs_mean_ow = R2_obs_mean,
         R2_obs_LB_ow = R2_obs_LB,
         R2_obs_UB_ow = R2_obs_UB)

R2_obs_summ_asian <- final_summ_stats_asian$all_outcome_metrics_dt %>%
  filter(Model %in% c("CRS_all_predictors")) %>%
  filter(Outcome_phecode != "Avg") %>%
  select(Outcome_phecode, R2_obs_mean, R2_obs_LB, R2_obs_UB) %>%
  rename(R2_obs_mean_asian = R2_obs_mean,
         R2_obs_LB_asian = R2_obs_LB,
         R2_obs_UB_asian = R2_obs_UB)

R2_obs_summ_black <- final_summ_stats_black$all_outcome_metrics_dt %>%
  filter(Model %in% c("CRS_all_predictors")) %>%
  filter(Outcome_phecode != "Avg") %>%
  select(Outcome_phecode, R2_obs_mean, R2_obs_LB, R2_obs_UB) %>%
  rename(R2_obs_mean_black = R2_obs_mean,
         R2_obs_LB_black = R2_obs_LB,
         R2_obs_UB_black = R2_obs_UB)

R2_obs_summ_mixed <- final_summ_stats_mixed$all_outcome_metrics_dt %>%
  filter(Model %in% c("CRS_all_predictors")) %>%
  filter(Outcome_phecode != "Avg") %>%
  select(Outcome_phecode, R2_obs_mean, R2_obs_LB, R2_obs_UB) %>%
  rename(R2_obs_mean_mixed = R2_obs_mean,
         R2_obs_LB_mixed = R2_obs_LB,
         R2_obs_UB_mixed = R2_obs_UB)

R2_obs_summ_all <- R2_obs_summ_wb %>%
  left_join(R2_obs_summ_ow, by = "Outcome_phecode") %>%
  left_join(R2_obs_summ_asian, by = "Outcome_phecode") %>%
  left_join(R2_obs_summ_black, by = "Outcome_phecode") %>%
  left_join(R2_obs_summ_mixed, by = "Outcome_phecode") %>%
  left_join(phecode_info[, c("Outcome_phecode", "phenotype")], 
            by = "Outcome_phecode") %>%
  mutate(phenotype = case_when(Outcome_phecode == "COVID_hspt" ~ "COVID-19 hospitalisation",
                               Outcome_phecode == "death" ~ "Death",
                               .default = phenotype))

##### Plot function #####
R2_obs_cross_eth_plot <- function(R2_obs_summ_select,
                                   eth_x_name = "White British",
                                   eth_y_name,
                                   dodge = position_dodge(0.8),
                                   ref_linewidth = 0.6,
                                   metric_lower_lim = -0.1,
                                   metric_upper_lim = 0.3,
                                   metric_breaks = 0.1,
                                   point_size = base_point_size,
                                   base_size = base_font_size,
                                   text_label_size = 6,
                                   error_bar_width = 0.01,
                                   label_top_N = F,
                                   point_padding = 1,
                                   box_padding = 0.5,
                                   nudge_x = 0,
                                   nudge_y = 0,
                                   seed = 100,
                                   save_format = "pdf",
                                   fig_width = 7,
                                   fig_height = 6) {
  eth_x_name_p <- paste0("(", eth_x_name, ")")
  eth_y_name_p <- paste0("(", eth_y_name, ")")
  
  eth_vs_wb_plot <- ggplot(R2_obs_summ_select, 
                           aes(x = R2_obs_mean_x, 
                               y = R2_obs_mean_y)) +
    geom_abline(slope = 1, intercept = 0, 
                linewidth = ref_linewidth) +
    geom_hline(yintercept = 0, linetype = "dashed") + 
    geom_vline(xintercept = 0, linetype = "dashed") + 
    geom_point(size = point_size, color = grey) + 
    scale_x_continuous(limits = c(metric_lower_lim, metric_upper_lim), 
                       breaks = seq(metric_lower_lim, metric_upper_lim, 
                                    metric_breaks)) +
    scale_y_continuous(limits = c(metric_lower_lim, metric_upper_lim), 
                       breaks = seq(metric_lower_lim, metric_upper_lim, 
                                    metric_breaks)) +
    theme_bw(base_size = base_size) +
    theme( 
      # legend.position="right",
      # legend.key.spacing.y = unit(legend_spacing, 'cm'),
      #  panel.border = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank()
    ) + 
    labs(x = bquote(Observed ~ R^2 ~ .(eth_x_name_p)), 
         y = bquote(Observed ~ R^2 ~ .(eth_y_name_p)))
  
  eth_x_name_new <- "White_British"
  
  if (eth_y_name == "Irish/Other White") {
    eth_y_name_new <- "Irish_Other_White" 
  } else {
    eth_y_name_new <- eth_y_name
  }
  ggsave(filename = paste0(figure_dir, "Cross_ethnicity/R2_obs_", 
                           eth_x_name_new, "_vs_",
                           eth_y_name_new,".", save_format), 
         eth_vs_wb_plot, 
         width = fig_width, 
         height = fig_height, 
         limitsize = FALSE)
}

##### Make plots #####
## Asian
R2_obs_summ_asian_vs_wb <- R2_obs_summ_all %>%
  mutate(R2_obs_mean_x = R2_obs_mean_wb,
         R2_obs_mean_y = R2_obs_mean_asian)

R2_obs_cross_eth_plot(R2_obs_summ_asian_vs_wb,
                       eth_x_name = "White British",
                       eth_y_name = "Asian",
                       fig_width = 7,
                       fig_height = 6)

## Black
R2_obs_summ_black_vs_wb <- R2_obs_summ_all %>%
  mutate(R2_obs_mean_x = R2_obs_mean_wb,
         R2_obs_mean_y = R2_obs_mean_black)

R2_obs_cross_eth_plot(R2_obs_summ_black_vs_wb,
                       eth_x_name = "White British",
                       eth_y_name = "Black",
                       fig_width = 7,
                       fig_height = 6)

## Mixed
R2_obs_summ_mixed_vs_wb <- R2_obs_summ_all %>%
  mutate(R2_obs_mean_x = R2_obs_mean_wb,
         R2_obs_mean_y = R2_obs_mean_mixed)

R2_obs_cross_eth_plot(R2_obs_summ_mixed_vs_wb,
                       eth_x_name = "White British",
                       eth_y_name = "Mixed",
                       fig_width = 7,
                       fig_height = 6)

## Irish/Other White
R2_obs_summ_ow_vs_wb <- R2_obs_summ_all %>%
  mutate(R2_obs_mean_x = R2_obs_mean_wb,
         R2_obs_mean_y = R2_obs_mean_ow)

R2_obs_cross_eth_plot(R2_obs_summ_ow_vs_wb,
                       eth_x_name = "White British",
                       eth_y_name = "Irish/Other White",
                       fig_width = 7,
                       fig_height = 6)


##### Slope from regression - UPDATE TO LATEST #####
summary(lm(R2_obs_mean_asian ~ 0 + R2_obs_mean_wb, 
           data = R2_obs_summ_all))
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# R2_obs_mean_wb   0.9172     0.0249   36.83   <2e-16 ***

summary(lm(R2_obs_mean_black ~ 0 + R2_obs_mean_wb, 
           data = R2_obs_summ_all))
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# R2_obs_mean_wb  0.83655    0.02224   37.62   <2e-16 ***

summary(lm(R2_obs_mean_mixed ~ 0 + R2_obs_mean_wb, 
           data = R2_obs_summ_all))
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# R2_obs_mean_wb  0.93106    0.02247   41.44   <2e-16 ***

summary(lm(R2_obs_mean_ow ~ 0 + R2_obs_mean_wb, 
           data = R2_obs_summ_all))
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# R2_obs_mean_wb   1.0021     0.0178   56.31   <2e-16 ***




#### Fig. S10: Cross-ethnicity AUPRC - incidence ####
##### Combine performance metrics across ethnicities #####
auprc_summ_wb <- final_summ_stats_wb$all_outcome_metrics_dt %>%
  filter(Model %in% c("CRS_all_predictors")) %>%
  filter(Outcome_phecode != "Avg") %>%
  select(Outcome_phecode, AUPRC_prev_diff_mean, 
         AUPRC_prev_diff_LB, AUPRC_prev_diff_UB) %>%
  rename(AUPRC_prev_diff_mean_wb = AUPRC_prev_diff_mean,
         AUPRC_prev_diff_LB_wb = AUPRC_prev_diff_LB,
         AUPRC_prev_diff_UB_wb = AUPRC_prev_diff_UB)

auprc_summ_ow <- final_summ_stats_ow$all_outcome_metrics_dt %>%
  filter(Model %in% c("CRS_all_predictors")) %>%
  filter(Outcome_phecode != "Avg") %>%
  select(Outcome_phecode, AUPRC_prev_diff_mean, AUPRC_prev_diff_LB, AUPRC_prev_diff_UB) %>%
  rename(AUPRC_prev_diff_mean_ow = AUPRC_prev_diff_mean,
         AUPRC_prev_diff_LB_ow = AUPRC_prev_diff_LB,
         AUPRC_prev_diff_UB_ow = AUPRC_prev_diff_UB)

auprc_summ_asian <- final_summ_stats_asian$all_outcome_metrics_dt %>%
  filter(Model %in% c("CRS_all_predictors")) %>%
  filter(Outcome_phecode != "Avg") %>%
  select(Outcome_phecode, AUPRC_prev_diff_mean, AUPRC_prev_diff_LB, AUPRC_prev_diff_UB) %>%
  rename(AUPRC_prev_diff_mean_asian = AUPRC_prev_diff_mean,
         AUPRC_prev_diff_LB_asian = AUPRC_prev_diff_LB,
         AUPRC_prev_diff_UB_asian = AUPRC_prev_diff_UB)

auprc_summ_black <- final_summ_stats_black$all_outcome_metrics_dt %>%
  filter(Model %in% c("CRS_all_predictors")) %>%
  filter(Outcome_phecode != "Avg") %>%
  select(Outcome_phecode, AUPRC_prev_diff_mean, AUPRC_prev_diff_LB, AUPRC_prev_diff_UB) %>%
  rename(AUPRC_prev_diff_mean_black = AUPRC_prev_diff_mean,
         AUPRC_prev_diff_LB_black = AUPRC_prev_diff_LB,
         AUPRC_prev_diff_UB_black = AUPRC_prev_diff_UB)

auprc_summ_mixed <- final_summ_stats_mixed$all_outcome_metrics_dt %>%
  filter(Model %in% c("CRS_all_predictors")) %>%
  filter(Outcome_phecode != "Avg") %>%
  select(Outcome_phecode, AUPRC_prev_diff_mean, AUPRC_prev_diff_LB, AUPRC_prev_diff_UB) %>%
  rename(AUPRC_prev_diff_mean_mixed = AUPRC_prev_diff_mean,
         AUPRC_prev_diff_LB_mixed = AUPRC_prev_diff_LB,
         AUPRC_prev_diff_UB_mixed = AUPRC_prev_diff_UB)

auprc_summ_all <- auprc_summ_wb %>%
  left_join(auprc_summ_ow, by = "Outcome_phecode") %>%
  left_join(auprc_summ_asian, by = "Outcome_phecode") %>%
  left_join(auprc_summ_black, by = "Outcome_phecode") %>%
  left_join(auprc_summ_mixed, by = "Outcome_phecode") %>%
  left_join(phecode_info[, c("Outcome_phecode", "phenotype")], 
            by = "Outcome_phecode") %>%
  mutate(phenotype = case_when(Outcome_phecode == "COVID_hspt" ~ "COVID-19 hospitalisation",
                               Outcome_phecode == "death" ~ "Death",
                               .default = phenotype))

##### Get % (AUPRC - incidence) #####
auprc_summ_all <- auprc_summ_all %>%
  left_join(outcome_ds_prev[, c("Outcome_phecode", "Incidence_combined")]) %>%
  mutate(AUPRC_prev_diff_perc_mean_wb = AUPRC_prev_diff_mean_wb/Incidence_combined * 100,
         AUPRC_prev_diff_perc_mean_ow = AUPRC_prev_diff_mean_ow/Incidence_combined * 100,
         AUPRC_prev_diff_perc_mean_asian = AUPRC_prev_diff_mean_asian/Incidence_combined * 100,
         AUPRC_prev_diff_perc_mean_black = AUPRC_prev_diff_mean_black/Incidence_combined * 100,
         AUPRC_prev_diff_perc_mean_mixed = AUPRC_prev_diff_mean_mixed/Incidence_combined * 100)

##### Plot function #####
auprc_cross_eth_plot <- function(auprc_summ_select,
                                  eth_x_name = "White British",
                                  eth_y_name,
                                  dodge = position_dodge(0.8),
                                  ref_linewidth = 0.6,
                                  metric_lower_lim = -100,
                                  metric_upper_lim = 550,
                                  metric_breaks = 150,
                                  point_size = base_point_size,
                                  base_size = base_font_size,
                                  text_label_size = 6,
                                  error_bar_width = 0.01,
                                  label_top_N = F,
                                  point_padding = 1,
                                  box_padding = 0.5,
                                  nudge_x = 0,
                                  nudge_y = 0,
                                  seed = 100,
                                  save_format = "pdf",
                                  fig_width = 7,
                                  fig_height = 6) {
  eth_x_name_p <- paste0("(", eth_x_name, ")")
  eth_y_name_p <- paste0("(", eth_y_name, ")")
  
  eth_vs_wb_plot <- ggplot(auprc_summ_select, 
                           aes(x = AUPRC_prev_diff_perc_mean_x, 
                               y = AUPRC_prev_diff_perc_mean_y)) +
    geom_abline(slope = 1, intercept = 0, 
                linewidth = ref_linewidth) +
    geom_hline(yintercept = 0, linetype = "dashed") + 
    geom_vline(xintercept = 0, linetype = "dashed") + 
    geom_point(size = point_size, color = grey) + 
    scale_x_continuous(limits = c(metric_lower_lim, metric_upper_lim), 
                       breaks = seq(metric_lower_lim, metric_upper_lim, 
                                    metric_breaks)) +
    scale_y_continuous(limits = c(metric_lower_lim, metric_upper_lim), 
                       breaks = seq(metric_lower_lim, metric_upper_lim, 
                                    metric_breaks)) +
    theme_bw(base_size = base_size) +
    theme( 
      # legend.position="right",
      # legend.key.spacing.y = unit(legend_spacing, 'cm'),
      #  panel.border = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank()
    ) + 
    labs(x = paste0("%(AUPRC - incidence) ", eth_x_name_p), 
         y = paste0("%(AUPRC - incidence) ", eth_y_name_p))
  
  if (eth_y_name == "Irish/Other White") {
    eth_vs_wb_plot <- eth_vs_wb_plot + 
      labs(x = paste0("%(AUPRC - incidence) ", eth_x_name_p), 
           y = paste0("%(AUPRC - incidence)\n", eth_y_name_p))
  }
  
  eth_x_name_new <- "White_British"
  
  if (eth_y_name == "Irish/Other White") {
    eth_y_name_new <- "Irish_Other_White" 
  } else {
    eth_y_name_new <- eth_y_name
  }
  ggsave(filename = paste0(figure_dir, "Cross_ethnicity/AUPRC_prev_diff_perc_", 
                           eth_x_name_new, "_vs_",
                           eth_y_name_new,".", save_format), 
         eth_vs_wb_plot, 
         width = fig_width, 
         height = fig_height, 
         limitsize = FALSE)
}

##### Make plots #####
## Asian
auprc_summ_asian_vs_wb <- auprc_summ_all %>%
  mutate(AUPRC_prev_diff_perc_mean_x = AUPRC_prev_diff_perc_mean_wb,
         AUPRC_prev_diff_perc_mean_y = AUPRC_prev_diff_perc_mean_asian)

auprc_cross_eth_plot(auprc_summ_asian_vs_wb,
                      eth_x_name = "White British",
                      eth_y_name = "Asian",
                      fig_width = 7,
                      fig_height = 6)

## Black
auprc_summ_black_vs_wb <- auprc_summ_all %>%
  mutate(AUPRC_prev_diff_perc_mean_x = AUPRC_prev_diff_perc_mean_wb,
         AUPRC_prev_diff_perc_mean_y = AUPRC_prev_diff_perc_mean_black)

auprc_cross_eth_plot(auprc_summ_black_vs_wb,
                      eth_x_name = "White British",
                      eth_y_name = "Black",
                      fig_width = 7,
                      fig_height = 6)

## Mixed
auprc_summ_mixed_vs_wb <- auprc_summ_all %>%
  mutate(AUPRC_prev_diff_perc_mean_x = AUPRC_prev_diff_perc_mean_wb,
         AUPRC_prev_diff_perc_mean_y = AUPRC_prev_diff_perc_mean_mixed)

auprc_cross_eth_plot(auprc_summ_mixed_vs_wb,
                      eth_x_name = "White British",
                      eth_y_name = "Mixed",
                      fig_width = 7,
                      fig_height = 6)

## Irish/Other White
auprc_summ_ow_vs_wb <- auprc_summ_all %>%
  mutate(AUPRC_prev_diff_perc_mean_x = AUPRC_prev_diff_perc_mean_wb,
         AUPRC_prev_diff_perc_mean_y = AUPRC_prev_diff_perc_mean_ow)

auprc_cross_eth_plot(auprc_summ_ow_vs_wb,
                      eth_x_name = "White British",
                      eth_y_name = "Irish/Other White",
                      fig_width = 7,
                      fig_height = 6)




##### Slope from regression - UPDATE TO LATEST #####
summary(lm(AUPRC_prev_diff_mean_asian ~ 0 + AUPRC_prev_diff_mean_wb, 
           data = auprc_summ_all))
#  Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# AUPRC_prev_diff_mean_wb  0.75717    0.03674   20.61   <2e-16 ***

summary(lm(AUPRC_prev_diff_mean_black ~ 0 + AUPRC_prev_diff_mean_wb, 
           data = auprc_summ_all))
# Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
# AUPRC_prev_diff_mean_wb  0.64050    0.02871   22.31   <2e-16 ***

summary(lm(AUPRC_prev_diff_mean_mixed ~ 0 + AUPRC_prev_diff_mean_wb, 
           data = auprc_summ_all))
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# AUPRC_prev_diff_mean_wb  0.68027    0.02492    27.3   <2e-16 ***

summary(lm(AUPRC_prev_diff_mean_ow ~ 0 + AUPRC_prev_diff_mean_wb, 
           data = auprc_summ_all))
# Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
# AUPRC_prev_diff_mean_wb   0.7448     0.0222   33.55   <2e-16 ***


##### Slope from regression (use % (AUPRC - incidence)) - UPDATE TO LATEST #####
summary(lm(AUPRC_prev_diff_perc_mean_asian ~ 0 + AUPRC_prev_diff_perc_mean_wb, 
           data = auprc_summ_all))
#  Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# AUPRC_prev_diff_perc_mean_wb  0.81229    0.03558   22.83   <2e-16 ***

summary(lm(AUPRC_prev_diff_perc_mean_black ~ 0 + AUPRC_prev_diff_perc_mean_wb, 
           data = auprc_summ_all))
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# AUPRC_prev_diff_perc_mean_wb  0.74221    0.03343    22.2   <2e-16 ***

summary(lm(AUPRC_prev_diff_perc_mean_mixed ~ 0 + AUPRC_prev_diff_perc_mean_wb, 
           data = auprc_summ_all))
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# AUPRC_prev_diff_perc_mean_wb  0.81124    0.02213   36.65   <2e-16 ***

summary(lm(AUPRC_prev_diff_perc_mean_ow ~ 0 + AUPRC_prev_diff_perc_mean_wb, 
           data = auprc_summ_all))
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# AUPRC_prev_diff_perc_mean_wb  0.86635    0.01309   66.17   <2e-16 ***







