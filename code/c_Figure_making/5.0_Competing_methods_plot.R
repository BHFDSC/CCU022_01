### Author: Pearl Liu
### Plot performance for CRS vs. competing methods/additional covariates ###
### Index: 2015-01-01, follow-up: 10 years (all outcomes)
### Index: 2020-01-01, follow-up: 5 years (COVID hospitalization)
### Delphi comparison: predictor <= 2020-06-30, follow-up: 2021-07-01 to 2022-07-01

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

#### Delphi-CRS phenotype map ####
CRS_phecode_delphi_map3 <- read.csv("Inputs/For SDE/CRS_phecode_vs_Delphi_ICD10_map.csv")

#### UPDATE TO LATEST: Read all summary statistics ####
final_summ_stats_delphi <- readRDS(paste0(summary_dir, 
                                      "ForDelphiComparison_Sex_Specific_All_Outcomes+COVID_PredictionMetrics_ControlCase4to1_WhiteBritish_IndexMid2020_Followup1yr.rds"))

final_summ_stats_phers <- readRDS(paste0(summary_dir, 
                                    "Sex_Specific_All_Outcomes_CRS_vs_PheRS_PredictionMetrics_ControlCase4to1_WhiteBritish_Index2015_Followup10yr.rds"))

final_summ_stats_imd <- readRDS(paste0(summary_dir, 
                                 "Sex_Specific_CRS_vs_IMDq_PredictionMetrics_ControlCase4to1_WhiteBritish_Index2015_Followup10yr.rds"))

final_summ_stats_cci <- readRDS(paste0(summary_dir, 
                                       "Sex_Specific_CRS_vs_CCI_PredictionMetrics_ControlCase4to1_WhiteBritish_Index2015_Followup10yr.rds"))

final_summ_stats_qcovid <- readRDS(paste0(summary_dir, 
                                       "Summary_COVID_hspt_CRS_vs_QCovid_PredictionMetrics_ControlCase4to1_WhiteBritish_Index2015_Followup10yr.rds"))

#### Fig. 2c: COVID hospitalisation plot ####
## Show QCovid, IMD, CCI comparison

##### Combine different datasets #####
## Use consistent Age + Sex performance 
covid_summ_combined <- rbind(final_summ_stats_qcovid$all_outcome_metrics_dt %>% filter(Model != "Age_Sex"),
                             final_summ_stats_imd$all_outcome_metrics_dt %>% filter(Outcome_phecode == "COVID_hspt" & Model == "Age_Sex")) %>%
  mutate(Plot_group = "CRS vs. QCovid") %>%
  rbind(final_summ_stats_imd$all_outcome_metrics_dt %>%
          filter(Outcome_phecode == "COVID_hspt") %>%
          mutate(Plot_group = "CRS vs. IMD")) %>%
  rbind(final_summ_stats_cci$all_outcome_metrics_dt %>%
          filter(Outcome_phecode == "COVID_hspt") %>%
          mutate(Plot_group = "CRS vs. CCI")) %>%
  select(!c(Outcome_phecode)) %>%
  mutate(Plot_group = factor(Plot_group, 
                             levels = c("CRS vs. QCovid",
                                        "CRS vs. CCI",
                                        "CRS vs. IMD")))

##### Plot function #####
covid_comparison_plot_vertical <- function(covid_summ_select, 
                                  metric_name = "AUROC",
                                  covid_model_color = covid_model_color,
                                  metric_lower_lim = 0,
                                  metric_upper_lim = 0.3,
                                  metric_breaks = 0.1,
                                  point_size = base_point_size,
                                  base_size = base_font_size,
                                  dodge = position_dodge(0.8),
                                  legend_rm = T,
                                  save_format = "pdf",
                                  fig_width = 15,
                                  fig_height = 6) {
  
  if (metric_name %in% c("AUROC", "C-index")) {
    y_axis_label <- seq(metric_lower_lim, metric_upper_lim, metric_breaks) + 0.5
  } else {
    y_axis_label <- seq(metric_lower_lim, metric_upper_lim, metric_breaks)
  }
  
  barplot_summ <- ggplot(covid_summ_select, 
                         aes(x = Plot_group, 
                             y = metric_mean, 
                             group = Model)) + 
    geom_col(aes(fill = Model),
             position = dodge, width = 0.7, alpha = 0.8) +
    geom_errorbar(aes(x = Plot_group, 
                      ymin = metric_lb, ymax = metric_ub),
                  position = dodge, width=.2) +
    scale_y_continuous(limits = c(metric_lower_lim, metric_upper_lim), 
                       breaks = seq(metric_lower_lim, metric_upper_lim, metric_breaks),
                       labels = format(y_axis_label, nsmall = 2)) +
    scale_fill_manual(limits = levels(covid_summ_select$Model),
                      values = covid_model_color) +
    theme_bw(base_size = base_size)+
    theme( 
      legend.position="bottom",
      panel.border = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank()
    ) 
  
  if (legend_rm == T) {
    barplot_summ <- barplot_summ + 
      guides(fill="none")
  }
  
  if (metric_name %in% c("AUROC", "C-index")) {
    barplot_summ <- barplot_summ + 
      labs(x = "", 
           y = paste0(metric_name, "")) 
  }
  
  ggsave(filename = paste0(figure_dir, "/Competing_methods/COVID_hspt_", metric_name, ".",
                           save_format), 
         barplot_summ, width = fig_width, height = fig_height, limitsize = FALSE)
  
}


##### Make plot: AUROC #####
covid_model_vec <- c("Age_Sex",
                     "QCovid",
                     "CCI_Age_Sex",
                     "IMDq_Age_Sex",
                     "CRS_all_predictors",
                     "CRS_QCovid_Age_Sex",
                     "CRS_CCI_Age_Sex",
                     "CRS_IMDq_Age_Sex")

covid_model_color <- c(grey,
                       skyblue,
                       green,
                       purple,
                       red,
                       blue,
                       "#117733",
                       "#882255")

covid_summ_select <- covid_summ_combined %>%
  filter(Model %in% covid_model_vec) %>%
  mutate(Model = factor(Model,
                        levels = covid_model_vec)) %>%
  select(Model, Plot_group, AUROC_mean, AUROC_LB, AUROC_UB) %>%
  mutate(AUROC_mean_above_05 = AUROC_mean - 0.5,
         AUROC_LB_above_05 = AUROC_LB - 0.5,
         AUROC_UB_above_05 = AUROC_UB - 0.5) %>%
  rename(metric_mean = AUROC_mean_above_05,
         metric_lb = AUROC_LB_above_05,
         metric_ub = AUROC_UB_above_05)

covid_comparison_plot_vertical(covid_summ_select, 
                                  metric_name = "AUROC",
                                  covid_model_color = covid_model_color,
                                  metric_lower_lim = 0,
                                  metric_upper_lim = 0.3,
                                  metric_breaks = 0.1,
                                  point_size = base_point_size,
                                  base_size = base_font_size + 2,
                                  legend_rm = T,
                                  save_format = "pdf",
                                  fig_width = 16,
                                  fig_height = 5.5)

#### Fig. 2a: CRS vs. Delphi plot ####
crs_summ_stats_delphi <- final_summ_stats_delphi$all_outcome_metrics_dt %>%
  filter(Model %in% c("CRS_all_predictors", "Age_Sex")) %>%
  select(Outcome_phecode, Model, AUROC_mean, AUROC_SE, AUROC_LB, AUROC_UB) %>%
  inner_join(select(final_summ_stats_delphi$all_outcome_metrics_delta_dt, 
                    "Outcome_phecode", "Model", 
                    "Delta_AUROC_mean", "Delta_AUROC_SE", 
                    "Delta_AUROC_LB", "Delta_AUROC_UB"),
             by = c("Outcome_phecode", "Model")) %>%
  pivot_wider(names_from = "Model", 
              values_from = c("AUROC_mean", "AUROC_SE", "AUROC_LB", "AUROC_UB",
                              "Delta_AUROC_mean", "Delta_AUROC_SE",
                              "Delta_AUROC_LB", "Delta_AUROC_UB")) %>%
  mutate(Outcome_phecode = if_else(Outcome_phecode == "death", "Dea", Outcome_phecode))

##### Get final comparison data set #####
compare_summ_stats_delphi <- crs_summ_stats_delphi %>%
  inner_join(CRS_phecode_delphi_map3, by = "Outcome_phecode") %>%
  mutate(AUC_CRS = AUROC_mean_CRS_all_predictors,
         AUC_SE = AUROC_SE_CRS_all_predictors,
         AUC_LB_CRS = AUROC_LB_CRS_all_predictors,
         AUC_UB_CRS = AUROC_UB_CRS_all_predictors,
         Delta_AUC_CRS = Delta_AUROC_mean_CRS_all_predictors,
         Delta_AUC_SE_CRS = Delta_AUROC_SE_CRS_all_predictors,
         Delta_AUC_LB_CRS = Delta_AUROC_LB_CRS_all_predictors,
         Delta_AUC_UB_CRS = Delta_AUROC_UB_CRS_all_predictors,
         AUC_Delphi = AUC_Delphi_avg,
         Delta_AUC_Delphi = AUC_Delphi_avg - AUC_Base_Shmatko_avg) %>%
  select(PHECODE, PHECODE_phenotype, sex, 
         AUC_CRS, AUC_SE, AUC_LB_CRS, AUC_UB_CRS,
         Delta_AUC_CRS, Delta_AUC_SE_CRS, 
         Delta_AUC_LB_CRS, Delta_AUC_UB_CRS,
         AUC_Delphi, Delta_AUC_Delphi) %>%
  distinct() %>%
  mutate(sex = if_else(sex == "Empty", "Both", sex))

## Save
write.csv(compare_summ_stats_delphi, paste0(summary_dir,
                                    "CRS_vs_Delphi_compare_AUROC.csv"),
         row.names = F)

##### Read saved comparison data #####
compare_summ_stats_delphi <- read.csv(paste0(summary_dir,
                                    "CRS_vs_Delphi_compare_AUROC.csv"))

## Check outcomes with better performance for Delphi-2M vs. CRS
compare_summ_stats_delphi %>% 
  arrange(desc(AUC_Delphi - AUC_CRS)) %>%
  select(PHECODE_phenotype, AUC_CRS, AUC_Delphi) %>%
  View()

compare_summ_stats_delphi %>% 
  arrange(desc(Delta_AUC_Delphi - Delta_AUC_CRS)) %>%
  select(PHECODE_phenotype, Delta_AUC_CRS, Delta_AUC_Delphi) %>%
  View()

##### Summary #####
compare_results_delphi <- 
  compare_summ_stats_delphi %>%
  summarise(CRS_better = sum(AUC_CRS - AUC_Delphi > 0),
            Delphi_better = sum(AUC_CRS - AUC_Delphi < 0),
            Delta_CRS_better = sum(Delta_AUC_CRS - Delta_AUC_Delphi > 0),
            Delta_Delphi_better = sum(Delta_AUC_CRS - Delta_AUC_Delphi < 0),
            Avg_CRS_Delphi_diff = mean(AUC_CRS - AUC_Delphi),
            Avg_CRS_Delphi_delta_diff = mean(Delta_AUC_CRS - Delta_AUC_Delphi))

View(compare_results_delphi)

##### Label key outcomes #####
selected_outcome_ds_delphi <- c(#"COVID_hspt", 
  "death",
  # "165_1",
  #  "208",
  "250_2",
  "278_1",
  "300",
  # "366",
  "401_1",
  "411_2",
  "480"
  #  "550_5",
  # "681"
  #"994_2"
)
selected_outcome_phenotype_delphi <- c(#"COVID-19 hospitalisation",
  "Death",
  #  "Cancer of bronchus; lung",
  #    "Benign neoplasm of colon",
  "Type 2 diabetes",
  "Obesity",
  "Anxiety disorder",
  # "Cataract",
  "Essential hypertension",
  "Myocardial infarction",
  "Pneumonia"
  #    "Ventral hernia",
 #  "Superficial cellulitis and abscess"
  #"Sepsis"
)

compare_summ_stats_delphi_final <- read.csv(paste0(summary_dir, 
                                                 "CRS_vs_Delphi_compare_AUROC.csv")) %>%
  mutate(outcome_label = if_else(PHECODE_phenotype %in% selected_outcome_phenotype_delphi, 
                                 PHECODE_phenotype, 
                                 "")) %>%
  mutate(outcome_label_status = if_else(outcome_label == "", 
                                        "0", 
                                        "1"))


##### Plot function #####
CRS_Delphi_compare_plot <- function(compare_summ_stats_delphi_final,
                               dodge = position_dodge(0.8),
                               ref_linewidth = 0.6,
                               metric_lower_lim = -0.15,
                               metric_upper_lim = 0.3,
                               metric_breaks = 0.1,
                               point_size = base_point_size,
                               base_size = base_font_size,
                               error_bar_width = 0.01,
                               text_label_size = 6,
                               point_padding = 1,
                               box_padding = 0.5,
                               nudge_x = 0,
                               nudge_y = 0,
                               seed = 100,
                               save_format = "pdf",
                               fig_width = 7,
                               fig_height = 6) {
  
    crs_delphi_plot <- ggplot(compare_summ_stats_delphi_final, 
                         aes(x = Delta_AUC_Delphi, 
                             y = Delta_AUC_CRS)) + 
      geom_abline(slope = 1, intercept = 0, 
                  linewidth = ref_linewidth) +
      geom_hline(yintercept = 0, linetype = "dashed") + 
      geom_vline(xintercept = 0, linetype = "dashed") + 
      geom_point(size = point_size,
                 aes(color = outcome_label_status)) +
      scale_color_manual(values = c("1" = red, "0" = grey)) +
      scale_y_continuous(limits = c(metric_lower_lim, metric_upper_lim), 
                       breaks = seq(metric_lower_lim, metric_upper_lim, metric_breaks)) +
      scale_x_continuous(limits = c(metric_lower_lim, metric_upper_lim), 
                         breaks = seq(metric_lower_lim, metric_upper_lim, metric_breaks)) +
      theme_bw(base_size = base_size)+
      theme( 
        legend.position="right",
        # panel.border = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank()
      ) + 
      labs(x = expression(Delta * AUROC ~ "(Delphi-2M vs. Age + Sex)"),
           y = expression(Delta * AUROC ~ "(CRS vs. Age + Sex)")) 
    
    
      all_outcome_dt_label <- filter(compare_summ_stats_delphi_final, 
                                        outcome_label != "")
      nudge_x <- case_when(all_outcome_dt_label$outcome_label == "Myocardial infarction" ~ nudge_x - 0.1,
                           all_outcome_dt_label$outcome_label == "Death" ~ nudge_x + 0.07,
                           all_outcome_dt_label$outcome_label == "Pneumonia" ~ nudge_x + 0.1,
                           all_outcome_dt_label$outcome_label == "Essential hypertension" ~ nudge_x + 0.1,
                           all_outcome_dt_label$outcome_label == "Type 2 diabetes" ~ nudge_x,
                               .default = nudge_x)
      
      nudge_y = case_when(all_outcome_dt_label$outcome_label == "Myocardial infarction" ~ nudge_y + 0.1,
                          all_outcome_dt_label$outcome_label == "Death" ~ nudge_y,
                          all_outcome_dt_label$outcome_label == "Pneumonia" ~ nudge_y + 0.11,
                          all_outcome_dt_label$outcome_label == "Essential hypertension" ~ nudge_y - 0.07,
                          all_outcome_dt_label$outcome_label == "Type 2 diabetes" ~ nudge_y,
                          .default = nudge_y)
      
      crs_delphi_plot <- crs_delphi_plot +
        geom_point(data = filter(compare_summ_stats_delphi_final, 
                                 outcome_label != ""),
                   aes(x = Delta_AUC_Delphi,
                       y = Delta_AUC_CRS),
                   size = point_size,
                   color = red) +
        ggrepel::geom_label_repel(data = filter(compare_summ_stats_delphi_final, 
                                                outcome_label != ""),
                                  aes(x = Delta_AUC_Delphi,
                                      y = Delta_AUC_CRS, 
                                      label = outcome_label),
                                  size = text_label_size,
                                  point.padding = point_padding,
                                  box.padding = box_padding,
                                  nudge_x = nudge_x,
                                  nudge_y = nudge_y,
                                  seed = seed)+
        guides(color = "none")
  
  ggsave(filename = paste0(figure_dir, "/Competing_methods/CRS_vs_Delphi.", save_format), 
         crs_delphi_plot, width = fig_width, height = fig_height, limitsize = FALSE)

}

##### Make plot #####
CRS_Delphi_compare_plot(compare_summ_stats_delphi_final,
                                    dodge = position_dodge(0.8),
                                    ref_linewidth = 0.6,
                                    metric_lower_lim = -0.15,
                                    metric_upper_lim = 0.3,
                                    metric_breaks = 0.1,
                                    point_size = base_point_size,
                                    base_size = base_font_size,
                                    error_bar_width = 0.01,
                                    text_label_size = 6,
                                    point_padding = 1,
                                    box_padding = 0.5,
                                    nudge_x = 0,
                                    nudge_y = 0,
                                    seed = 100,
                                    save_format = "pdf",
                                    fig_width = 7,
                                    fig_height = 6)

#### Fig. 2b: CRS vs. PheRS plot ####
crs_summ_stats_phers <- final_summ_stats_phers$all_outcome_metrics_dt %>%
  filter(Model %in% c("CRS_all_predictors", "PheRS_age_sex", "Age_Sex")) %>%
  select(Outcome_phecode, PheRS_outcome_name, Model, 
         AUROC_mean, AUROC_LB, AUROC_UB) %>%
  inner_join(select(final_summ_stats_phers$all_outcome_metrics_delta_dt, 
                    "Outcome_phecode", "PheRS_outcome_name", "Model", 
                    "Delta_AUROC_mean", "Delta_AUROC_LB", "Delta_AUROC_UB"),
             by = c("Outcome_phecode", "PheRS_outcome_name", "Model")) %>%
  pivot_wider(names_from = "Model", 
              values_from = c("AUROC_mean", "AUROC_LB", "AUROC_UB",
                              "Delta_AUROC_mean", "Delta_AUROC_LB", "Delta_AUROC_UB")) %>%
  mutate(Outcome_phecode = if_else(Outcome_phecode == "death", "Dea", Outcome_phecode))

## Average hip OA and knee OA performance for PheRS
crs_summ_stats_phers2 <- crs_summ_stats_phers %>%
  filter(Outcome_phecode != "Avg") %>%
  select(Outcome_phecode, 
         Delta_AUROC_mean_CRS_all_predictors, 
         Delta_AUROC_LB_CRS_all_predictors, 
         Delta_AUROC_UB_CRS_all_predictors,
         Delta_AUROC_mean_PheRS_age_sex, 
         Delta_AUROC_LB_PheRS_age_sex, 
         Delta_AUROC_UB_PheRS_age_sex) %>%
  group_by(Outcome_phecode) %>%
  summarise(across(everything(), mean)) %>%
  ungroup() %>%
  left_join(select(phecode_info, Outcome_phecode, phenotype), 
            by = "Outcome_phecode")

##### Label key outcomes #####
#selected_outcome_phenotype_phers <- crs_summ_stats_phers2$phenotype
selected_outcome_phenotype_phers <- c("Asthma",
                                     "Type 2 diabetes",
                                     "Myocardial infarction",
                                     "Cancer of bronchus; lung",
                                     "Osteoarthritis; localized")

compare_summ_stats_phers_final <- crs_summ_stats_phers2 %>%
  mutate(outcome_label = if_else(phenotype %in% selected_outcome_phenotype_phers, 
                                 phenotype, 
                                 "")) %>%
  mutate(outcome_label_status = if_else(outcome_label == "", 
                                        "0", 
                                        "1"))

##### Plot function #####
CRS_PheRS_compare_plot <- function(compare_summ_stats_phers_final,
                                    dodge = position_dodge(0.8),
                                    ref_linewidth = 0.6,
                                    metric_lower_lim = -0.02,
                                    metric_upper_lim = 0.12,
                                    metric_breaks = 0.04,
                                    point_size = base_point_size,
                                    base_size = base_font_size,
                                    error_bar_width = 0.01,
                                    text_label_size = 6,
                                    point_padding = 1,
                                    box_padding = 0.5,
                                    nudge_x = 0,
                                    nudge_y = 0,
                                    seed = 100,
                                    save_format = "pdf",
                                    fig_width = 7,
                                    fig_height = 6) {
  
  crs_phers_plot <- ggplot(compare_summ_stats_phers_final, 
                            aes(x = Delta_AUROC_mean_PheRS_age_sex, 
                                y = Delta_AUROC_mean_CRS_all_predictors)) + 
    geom_abline(slope = 1, intercept = 0, 
                linewidth = ref_linewidth) +
    geom_hline(yintercept = 0, linetype = "dashed") + 
    geom_vline(xintercept = 0, linetype = "dashed") + 
    geom_point(size = point_size,
               aes(color = outcome_label_status)) +
    scale_color_manual(values = c("1" = red, "0" = grey)) +
    scale_y_continuous(limits = c(metric_lower_lim, metric_upper_lim), 
                       breaks = seq(metric_lower_lim, metric_upper_lim, metric_breaks)) +
    scale_x_continuous(limits = c(metric_lower_lim, metric_upper_lim), 
                       breaks = seq(metric_lower_lim, metric_upper_lim, metric_breaks)) +
    theme_bw(base_size = base_size)+
    theme( 
      legend.position="right",
      # panel.border = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank()
    ) + 
    labs(x = expression(Delta * AUROC ~ "(PheRS vs. Age + Sex)"),
         y = expression(Delta * AUROC ~ "(CRS vs. Age + Sex)")) 
  
  
  all_outcome_dt_label <- filter(compare_summ_stats_phers_final, 
                                    outcome_label != "")
  nudge_x <- case_when(all_outcome_dt_label$outcome_label == "Myocardial infarction" ~ nudge_x + 0.05,
                       all_outcome_dt_label$outcome_label == "Osteoarthritis; localized" ~ nudge_x + 0.05,
                     #  all_outcome_dt_label$outcome_label == "Coronary atherosclerosis" ~ nudge_x,
                       all_outcome_dt_label$outcome_label == "Cancer of bronchus; lung" ~ nudge_x - 0.05,
                       all_outcome_dt_label$outcome_label == "Asthma" ~ nudge_x - 0.02,
                       .default = nudge_x)

  nudge_y = case_when(all_outcome_dt_label$outcome_label == "Myocardial infarction" ~ nudge_y,
                      all_outcome_dt_label$outcome_label == "Osteoarthritis; localized" ~ nudge_y - 0.005,
                    #  all_outcome_dt_label$outcome_label == "Coronary atherosclerosis" ~ nudge_y - 0.01,
                      all_outcome_dt_label$outcome_label == "Cancer of bronchus; lung" ~ nudge_y + 0.015,
                      all_outcome_dt_label$outcome_label == "Asthma" ~ nudge_y,
                      .default = nudge_y)
  
  crs_phers_plot <- crs_phers_plot +
    geom_point(data = filter(compare_summ_stats_phers_final, 
                             outcome_label != ""),
               aes(x = Delta_AUROC_mean_PheRS_age_sex,
                   y = Delta_AUROC_mean_CRS_all_predictors),
               size = point_size,
               color = red) +
    ggrepel::geom_label_repel(data = filter(compare_summ_stats_phers_final, 
                                            outcome_label != ""),
                              aes(x = Delta_AUROC_mean_PheRS_age_sex,
                                  y = Delta_AUROC_mean_CRS_all_predictors, 
                                  label = outcome_label),
                              size = text_label_size,
                              point.padding = point_padding,
                              box.padding = box_padding,
                              nudge_x = nudge_x,
                              nudge_y = nudge_y,
                              seed = seed)+
    guides(color = "none")
  
  ggsave(filename = paste0(figure_dir, "/Competing_methods/CRS_vs_PheRS.", save_format), 
         crs_phers_plot, width = fig_width, height = fig_height, limitsize = FALSE)
  
}

##### Make plot #####
CRS_PheRS_compare_plot(compare_summ_stats_phers_final,
                                   dodge = position_dodge(0.8),
                                   ref_linewidth = 0.6,
                       metric_lower_lim = -0.02,
                       metric_upper_lim = 0.12,
                       metric_breaks = 0.04,
                                   point_size = base_point_size,
                                   base_size = base_font_size,
                                   error_bar_width = 0.01,
                                   text_label_size = 6,
                                   point_padding = 1,
                                   box_padding = 0.5,
                                   nudge_x = 0,
                                   nudge_y = 0,
                                   seed = 100,
                                   save_format = "pdf",
                                   fig_width = 7,
                                   fig_height = 6) 

#### Fig. 2d: CRS vs. CCI plot ####
crs_summ_stats_cci <- final_summ_stats_cci$all_outcome_metrics_dt %>%
  filter(Outcome_phecode != "Avg") %>%
  filter(Model %in% c("CRS_CCI_Age_Sex",
                      "CCI_Age_Sex")) %>%
  select(Outcome_phecode, Model, AUROC_mean, AUROC_LB, AUROC_UB) %>%
  inner_join(select(final_summ_stats_cci$all_outcome_metrics_delta_dt, 
                    "Outcome_phecode", "Model", 
                    "Delta_AUROC_mean", "Delta_AUROC_LB", "Delta_AUROC_UB"),
             by = c("Outcome_phecode", "Model")) %>%
  pivot_wider(names_from = "Model", 
              values_from = c("AUROC_mean", "AUROC_LB", "AUROC_UB",
                              "Delta_AUROC_mean", "Delta_AUROC_LB", "Delta_AUROC_UB")) %>%
  left_join(select(phecode_info, Outcome_phecode, phenotype), 
            by = "Outcome_phecode") %>%
  mutate(phenotype = case_when(Outcome_phecode == "death" ~ "Death", 
                               Outcome_phecode == "COVID_hspt" ~ "COVID-19 hospitalisation", 
                               .default = phenotype))

crs_summ_stats_cci %>% 
  arrange(desc(AUROC_mean_CRS_CCI_Age_Sex - AUROC_mean_CCI_Age_Sex)) %>%
  select(phenotype, 
         Delta_AUROC_mean_CRS_CCI_Age_Sex, 
         Delta_AUROC_mean_CCI_Age_Sex) %>%
  View()

##### Label key outcomes #####
selected_outcome_phenotype_cci <- c("COVID-19 hospitalisation",
                                    "Cancer of bronchus; lung",
                                    "Type 2 diabetes",
                                    "Myocardial infarction",
                                    # "Synovitis and tenosynovitis",
                                    # "Sepsis", 
                                    "Death")

compare_summ_stats_cci_final <- crs_summ_stats_cci %>%
  mutate(outcome_label = if_else(phenotype %in% selected_outcome_phenotype_cci, 
                                 phenotype, 
                                 "")) %>%
  mutate(outcome_label_status = if_else(outcome_label == "", 
                                        "0", 
                                        "1"))

##### Plot function #####
CRS_CCI_compare_plot <- function(compare_summ_stats_cci_final,
                                 dodge = position_dodge(0.8),
                                 ref_linewidth = 0.6,
                                 metric_lower_lim = 0.4,
                                 metric_upper_lim = 0.9,
                                 metric_breaks = 0.1,
                                 point_size = base_point_size,
                                 base_size = base_font_size,
                                 error_bar_width = 0.01,
                                 text_label_size = 6,
                                 point_padding = 1,
                                 box_padding = 0.5,
                                 nudge_x = 0,
                                 nudge_y = 0,
                                 seed = 100,
                                 save_format = "pdf",
                                 fig_width = 7,
                                 fig_height = 6) {
  
  crs_cci_plot <- ggplot(compare_summ_stats_cci_final, 
                         aes(x = AUROC_mean_CCI_Age_Sex, 
                             y = AUROC_mean_CRS_CCI_Age_Sex)) + 
    geom_abline(slope = 1, intercept = 0, 
                linewidth = ref_linewidth) +
    # geom_hline(yintercept = 0.5, linetype = "dashed") + 
    # geom_vline(xintercept = 0.5, linetype = "dashed") + 
    geom_point(size = point_size,
               aes(color = outcome_label_status)) +
    scale_color_manual(values = c("1" = red, "0" = grey)) +
    scale_y_continuous(limits = c(metric_lower_lim, metric_upper_lim), 
                       breaks = seq(metric_lower_lim, metric_upper_lim, metric_breaks)) +
    scale_x_continuous(limits = c(metric_lower_lim, metric_upper_lim), 
                       breaks = seq(metric_lower_lim, metric_upper_lim, metric_breaks)) +
    theme_bw(base_size = base_size)+
    theme( 
      legend.position="right",
      # panel.border = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank()
    ) + 
    labs(x = "AUROC (CCI + Age + Sex)",
         y = "AUROC (CRS + CCI + Age + Sex)") 
  
  
  all_outcome_dt_label <- filter(compare_summ_stats_cci_final, 
                                 outcome_label != "")
  nudge_x <- case_when(all_outcome_dt_label$outcome_label == "Myocardial infarction" ~ nudge_x + 0.12,
                       all_outcome_dt_label$outcome_label == "COVID-19 hospitalisation" ~ nudge_x - 0.15,
                       all_outcome_dt_label$outcome_label == "Sepsis" ~ nudge_x - 0.05,
                       #  all_outcome_dt_label$outcome_label == "Coronary atherosclerosis" ~ nudge_x,
                       all_outcome_dt_label$outcome_label == "Cancer of bronchus; lung" ~ nudge_x,
                       all_outcome_dt_label$outcome_label == "Type 2 diabetes" ~ nudge_x - 0.06,
                       .default = nudge_x)
  
  nudge_y = case_when(all_outcome_dt_label$outcome_label == "Myocardial infarction" ~ nudge_y - 0.02,
                      all_outcome_dt_label$outcome_label == "COVID-19 hospitalisation" ~ nudge_y + 0.04,
                      all_outcome_dt_label$outcome_label == "Sepsis" ~ nudge_y + 0.02,
                      #  all_outcome_dt_label$outcome_label == "Coronary atherosclerosis" ~ nudge_y - 0.01,
                      all_outcome_dt_label$outcome_label == "Cancer of bronchus; lung" ~ nudge_y + 0.08,
                      all_outcome_dt_label$outcome_label == "Type 2 diabetes" ~ nudge_y + 0.04,
                      .default = nudge_y)
  
  crs_cci_plot <- crs_cci_plot +
    geom_point(data = filter(compare_summ_stats_cci_final, 
                             outcome_label != ""),
               aes(x = AUROC_mean_CCI_Age_Sex,
                   y = AUROC_mean_CRS_CCI_Age_Sex),
               size = point_size,
               color = red) +
    ggrepel::geom_label_repel(data = filter(compare_summ_stats_cci_final, 
                                            outcome_label != ""),
                              aes(x = AUROC_mean_CCI_Age_Sex,
                                  y = AUROC_mean_CRS_CCI_Age_Sex, 
                                  label = outcome_label),
                              size = text_label_size,
                              point.padding = point_padding,
                              box.padding = box_padding,
                              nudge_x = nudge_x,
                              nudge_y = nudge_y,
                              seed = seed)+
    guides(color = "none")
  
  ggsave(filename = paste0(figure_dir, "/Competing_methods/CRS_vs_CCI.", save_format), 
         crs_cci_plot, width = fig_width, height = fig_height, limitsize = FALSE)
  
}

##### Make plot #####
CRS_CCI_compare_plot(compare_summ_stats_cci_final,
                     dodge = position_dodge(0.8),
                     ref_linewidth = 0.6,
                     metric_lower_lim = 0.5,
                     metric_upper_lim = 0.9,
                     metric_breaks = 0.1,
                     point_size = base_point_size,
                     base_size = base_font_size,
                     error_bar_width = 0.01,
                     text_label_size = 6,
                     point_padding = 1,
                     box_padding = 0.5,
                     nudge_x = 0,
                     nudge_y = 0,
                     seed = 100,
                     save_format = "pdf",
                     fig_width = 7,
                     fig_height = 6) 



#### Fig. 2e: CRS vs. IMD plot ####
crs_summ_stats_imd <- final_summ_stats_imd$all_outcome_metrics_dt %>%
  filter(Outcome_phecode != "Avg") %>%
  filter(Model %in% c("CRS_IMDq_Age_Sex",
                      "IMDq_Age_Sex")) %>%
  select(Outcome_phecode, Model, AUROC_mean, AUROC_LB, AUROC_UB) %>%
  inner_join(select(final_summ_stats_imd$all_outcome_metrics_delta_dt, 
                    "Outcome_phecode", "Model", 
                    "Delta_AUROC_mean", "Delta_AUROC_LB", "Delta_AUROC_UB"),
             by = c("Outcome_phecode", "Model")) %>%
  pivot_wider(names_from = "Model", 
              values_from = c("AUROC_mean", "AUROC_LB", "AUROC_UB",
                              "Delta_AUROC_mean", "Delta_AUROC_LB", "Delta_AUROC_UB")) %>%
  left_join(select(phecode_info, Outcome_phecode, phenotype), 
            by = "Outcome_phecode") %>%
  mutate(phenotype = case_when(Outcome_phecode == "death" ~ "Death", 
                               Outcome_phecode == "COVID_hspt" ~ "COVID-19 hospitalisation", 
                               .default = phenotype))

crs_summ_stats_imd %>% 
  arrange(desc(AUROC_mean_CRS_IMDq_Age_Sex - AUROC_mean_IMDq_Age_Sex)) %>%
  select(phenotype, 
         Delta_AUROC_mean_CRS_IMDq_Age_Sex, 
         Delta_AUROC_mean_IMDq_Age_Sex) %>%
  View()

##### Label key outcomes #####
selected_outcome_phenotype_imd <- c("COVID-19 hospitalisation",
                                    "Cancer of bronchus; lung",
                                    "Type 2 diabetes",
                                      "Myocardial infarction",
                                     # "Synovitis and tenosynovitis",
                                     # "Sepsis", 
                                      "Death")

compare_summ_stats_imd_final <- crs_summ_stats_imd %>%
  mutate(outcome_label = if_else(phenotype %in% selected_outcome_phenotype_imd, 
                                 phenotype, 
                                 "")) %>%
  mutate(outcome_label_status = if_else(outcome_label == "", 
                                        "0", 
                                        "1"))

##### Plot function #####
CRS_IMD_compare_plot <- function(compare_summ_stats_imd_final,
                                   dodge = position_dodge(0.8),
                                   ref_linewidth = 0.6,
                                   metric_lower_lim = 0.5,
                                   metric_upper_lim = 0.9,
                                   metric_breaks = 0.1,
                                   point_size = base_point_size,
                                   base_size = base_font_size,
                                   error_bar_width = 0.01,
                                   text_label_size = 6,
                                   point_padding = 1,
                                   box_padding = 0.5,
                                   nudge_x = 0,
                                   nudge_y = 0,
                                   seed = 100,
                                   save_format = "pdf",
                                   fig_width = 7,
                                   fig_height = 6) {
  
  crs_imd_plot <- ggplot(compare_summ_stats_imd_final, 
                           aes(x = AUROC_mean_IMDq_Age_Sex, 
                               y = AUROC_mean_CRS_IMDq_Age_Sex)) + 
    geom_abline(slope = 1, intercept = 0, 
                linewidth = ref_linewidth) +
   # geom_hline(yintercept = 0.5, linetype = "dashed") + 
   # geom_vline(xintercept = 0.5, linetype = "dashed") + 
    geom_point(size = point_size,
               aes(color = outcome_label_status)) +
    scale_color_manual(values = c("1" = red, "0" = grey)) +
    scale_y_continuous(limits = c(metric_lower_lim, metric_upper_lim), 
                       breaks = seq(metric_lower_lim, metric_upper_lim, metric_breaks)) +
    scale_x_continuous(limits = c(metric_lower_lim, metric_upper_lim), 
                       breaks = seq(metric_lower_lim, metric_upper_lim, metric_breaks)) +
    theme_bw(base_size = base_size)+
    theme( 
      legend.position="right",
      # panel.border = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank()
    ) + 
    labs(x = "AUROC (IMD + Age + Sex)",
         y = "AUROC (CRS + IMD + Age + Sex)") 
  
  
  all_outcome_dt_label <- filter(compare_summ_stats_imd_final, 
                                 outcome_label != "")
   nudge_x <- case_when(all_outcome_dt_label$outcome_label == "Myocardial infarction" ~ nudge_x + 0.12,
                        all_outcome_dt_label$outcome_label == "COVID-19 hospitalisation" ~ nudge_x,
                        all_outcome_dt_label$outcome_label == "Sepsis" ~ nudge_x - 0.05,
                        #  all_outcome_dt_label$outcome_label == "Coronary atherosclerosis" ~ nudge_x,
                        all_outcome_dt_label$outcome_label == "Cancer of bronchus; lung" ~ nudge_x + 0.05,
                        all_outcome_dt_label$outcome_label == "Type 2 diabetes" ~ nudge_x - 0.1,
                        .default = nudge_x)
   
   nudge_y = case_when(all_outcome_dt_label$outcome_label == "Myocardial infarction" ~ nudge_y,
                       all_outcome_dt_label$outcome_label == "COVID-19 hospitalisation" ~ nudge_y + 0.08,
                       all_outcome_dt_label$outcome_label == "Sepsis" ~ nudge_y + 0.02,
                       #  all_outcome_dt_label$outcome_label == "Coronary atherosclerosis" ~ nudge_y - 0.01,
                       all_outcome_dt_label$outcome_label == "Cancer of bronchus; lung" ~ nudge_y + 0.08,
                       all_outcome_dt_label$outcome_label == "Type 2 diabetes" ~ nudge_y + 0.05,
                       .default = nudge_y)
  
  crs_imd_plot <- crs_imd_plot +
    geom_point(data = filter(compare_summ_stats_imd_final, 
                             outcome_label != ""),
               aes(x = AUROC_mean_IMDq_Age_Sex,
                   y = AUROC_mean_CRS_IMDq_Age_Sex),
               size = point_size,
               color = red) +
    ggrepel::geom_label_repel(data = filter(compare_summ_stats_imd_final, 
                                            outcome_label != ""),
                              aes(x = AUROC_mean_IMDq_Age_Sex,
                                  y = AUROC_mean_CRS_IMDq_Age_Sex, 
                                  label = outcome_label),
                              size = text_label_size,
                              point.padding = point_padding,
                              box.padding = box_padding,
                              nudge_x = nudge_x,
                              nudge_y = nudge_y,
                              seed = seed)+
    guides(color = "none")
  
  ggsave(filename = paste0(figure_dir, "/Competing_methods/CRS_vs_IMD.", save_format), 
         crs_imd_plot, width = fig_width, height = fig_height, limitsize = FALSE)
  
}

##### Make plot #####
CRS_IMD_compare_plot(compare_summ_stats_imd_final,
                                 dodge = position_dodge(0.8),
                                 ref_linewidth = 0.6,
                     metric_lower_lim = 0.5,
                     metric_upper_lim = 0.9,
                     metric_breaks = 0.1,
                                 point_size = base_point_size,
                                 base_size = base_font_size,
                                 error_bar_width = 0.01,
                                 text_label_size = 6,
                                 point_padding = 1,
                                 box_padding = 0.5,
                                 nudge_x = 0,
                                 nudge_y = 0,
                                 seed = 100,
                                 save_format = "pdf",
                                 fig_width = 7,
                                 fig_height = 6) 


#### Fig. S8a: CRS + CCI vs. CRS plot ####
crs_summ_stats_cci_supp <- final_summ_stats_cci$all_outcome_metrics_dt %>%
  filter(Outcome_phecode != "Avg") %>%
  filter(Model %in% c("CRS_CCI_Age_Sex",
                      "CRS_all_predictors")) %>%
  select(Outcome_phecode, Model, AUROC_mean, AUROC_LB, AUROC_UB) %>%
  inner_join(select(final_summ_stats_cci$all_outcome_metrics_delta_dt, 
                    "Outcome_phecode", "Model", 
                    "Delta_AUROC_mean", "Delta_AUROC_LB", "Delta_AUROC_UB"),
             by = c("Outcome_phecode", "Model")) %>%
  pivot_wider(names_from = "Model", 
              values_from = c("AUROC_mean", "AUROC_LB", "AUROC_UB",
                              "Delta_AUROC_mean", "Delta_AUROC_LB", "Delta_AUROC_UB")) %>%
  left_join(select(phecode_info, Outcome_phecode, phenotype), 
            by = "Outcome_phecode") %>%
  mutate(phenotype = case_when(Outcome_phecode == "death" ~ "Death", 
                               Outcome_phecode == "COVID_hspt" ~ "COVID-19 hospitalisation", 
                               .default = phenotype))

crs_summ_stats_cci_supp %>% 
  arrange(desc(AUROC_mean_CRS_CCI_Age_Sex - AUROC_mean_CRS_all_predictors)) %>%
  select(phenotype, 
         Delta_AUROC_mean_CRS_CCI_Age_Sex, 
         Delta_AUROC_mean_CRS_all_predictors) %>%
  View()

##### Label key outcomes #####
selected_outcome_phenotype_cci <- c("COVID-19 hospitalisation",
                                    "Cancer of bronchus; lung",
                                    "Type 2 diabetes",
                                    "Myocardial infarction",
                                    # "Synovitis and tenosynovitis",
                                    # "Sepsis", 
                                    "Death")

compare_summ_stats_cci_supp_final <- crs_summ_stats_cci_supp %>%
  mutate(outcome_label = if_else(phenotype %in% selected_outcome_phenotype_cci, 
                                 phenotype, 
                                 "")) %>%
  mutate(outcome_label_status = if_else(outcome_label == "", 
                                        "0", 
                                        "1"))

##### Plot function #####
CRS_CCI_compare_supp_plot <- function(compare_summ_stats_cci_supp_final,
                                 dodge = position_dodge(0.8),
                                 ref_linewidth = 0.6,
                                 metric_lower_lim = 0.4,
                                 metric_upper_lim = 0.9,
                                 metric_breaks = 0.1,
                                 point_size = base_point_size,
                                 base_size = base_font_size,
                                 error_bar_width = 0.01,
                                 text_label_size = 6,
                                 point_padding = 1,
                                 box_padding = 0.5,
                                 nudge_x = 0,
                                 nudge_y = 0,
                                 seed = 100,
                                 save_format = "pdf",
                                 fig_width = 7,
                                 fig_height = 6) {
  
  crs_cci_plot <- ggplot(compare_summ_stats_cci_supp_final, 
                         aes(x = AUROC_mean_CRS_all_predictors, 
                             y = AUROC_mean_CRS_CCI_Age_Sex)) + 
    geom_abline(slope = 1, intercept = 0, 
                linewidth = ref_linewidth) +
    # geom_hline(yintercept = 0.5, linetype = "dashed") + 
    # geom_vline(xintercept = 0.5, linetype = "dashed") + 
    geom_point(size = point_size,
               aes(color = outcome_label_status)) +
    scale_color_manual(values = c("1" = red, "0" = grey)) +
    scale_y_continuous(limits = c(metric_lower_lim, metric_upper_lim), 
                       breaks = seq(metric_lower_lim, metric_upper_lim, metric_breaks)) +
    scale_x_continuous(limits = c(metric_lower_lim, metric_upper_lim), 
                       breaks = seq(metric_lower_lim, metric_upper_lim, metric_breaks)) +
    theme_bw(base_size = base_size)+
    theme( 
      legend.position="right",
      # panel.border = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank()
    ) + 
    labs(x = "AUROC (CRS + Age + Sex)",
         y = "AUROC (CRS + CCI + Age + Sex)") 
  
  
  all_outcome_dt_label <- filter(compare_summ_stats_cci_supp_final, 
                                 outcome_label != "")
  nudge_x <- case_when(all_outcome_dt_label$outcome_label == "Myocardial infarction" ~ nudge_x - 0.13,
                       all_outcome_dt_label$outcome_label == "COVID-19 hospitalisation" ~ nudge_x - 0.16,
                       #  all_outcome_dt_label$outcome_label == "Coronary atherosclerosis" ~ nudge_x,
                       all_outcome_dt_label$outcome_label == "Cancer of bronchus; lung" ~ nudge_x - 0.13,
                       all_outcome_dt_label$outcome_label == "Type 2 diabetes" ~ nudge_x + 0.1,
                       all_outcome_dt_label$outcome_label == "Death" ~ nudge_x - 0.02,
                       all_outcome_dt_label$outcome_label == "Tobacco use disorder" ~ nudge_x - 0.04,
                       .default = nudge_x)
  
  nudge_y = case_when(all_outcome_dt_label$outcome_label == "Myocardial infarction" ~ nudge_y,
                      all_outcome_dt_label$outcome_label == "COVID-19 hospitalisation" ~ nudge_y,
                      #  all_outcome_dt_label$outcome_label == "Coronary atherosclerosis" ~ nudge_y - 0.01,
                      all_outcome_dt_label$outcome_label == "Cancer of bronchus; lung" ~ nudge_y + 0.05,
                      all_outcome_dt_label$outcome_label == "Type 2 diabetes" ~ nudge_y - 0.03,
                      all_outcome_dt_label$outcome_label == "Death" ~ nudge_y + 0.04,
                      all_outcome_dt_label$outcome_label == "Tobacco use disorder" ~ nudge_y + 0.03,
                      .default = nudge_y)
  
  crs_cci_plot <- crs_cci_plot +
    geom_point(data = filter(compare_summ_stats_cci_supp_final, 
                             outcome_label != ""),
               aes(x = AUROC_mean_CRS_all_predictors,
                   y = AUROC_mean_CRS_CCI_Age_Sex),
               size = point_size,
               color = red) +
    ggrepel::geom_label_repel(data = filter(compare_summ_stats_cci_supp_final, 
                                            outcome_label != ""),
                              aes(x = AUROC_mean_CRS_all_predictors,
                                  y = AUROC_mean_CRS_CCI_Age_Sex, 
                                  label = outcome_label),
                              size = text_label_size,
                              point.padding = point_padding,
                              box.padding = box_padding,
                              nudge_x = nudge_x,
                              nudge_y = nudge_y,
                              seed = seed)+
    guides(color = "none")
  
  ggsave(filename = paste0(figure_dir, "/Competing_methods/CRS_CCI_vs_CRS.", save_format), 
         crs_cci_plot, width = fig_width, height = fig_height, limitsize = FALSE)
  
}

##### Make plot #####
CRS_CCI_compare_supp_plot(compare_summ_stats_cci_supp_final,
                     dodge = position_dodge(0.8),
                     ref_linewidth = 0.6,
                     metric_lower_lim = 0.5,
                     metric_upper_lim = 0.9,
                     metric_breaks = 0.1,
                     point_size = base_point_size,
                     base_size = base_font_size,
                     error_bar_width = 0.01,
                     text_label_size = 6,
                     point_padding = 1,
                     box_padding = 0.5,
                     nudge_x = 0,
                     nudge_y = 0,
                     seed = 100,
                     save_format = "pdf",
                     fig_width = 7,
                     fig_height = 6) 





#### Fig. S8b: CRS + IMD vs. CRS plot ####
crs_summ_stats_imd_supp <- final_summ_stats_imd$all_outcome_metrics_dt %>%
  filter(Outcome_phecode != "Avg") %>%
  filter(Model %in% c("CRS_IMDq_Age_Sex",
                      "CRS_all_predictors")) %>%
  select(Outcome_phecode, Model, AUROC_mean, AUROC_LB, AUROC_UB) %>%
  inner_join(select(final_summ_stats_imd$all_outcome_metrics_delta_dt, 
                    "Outcome_phecode", "Model", 
                    "Delta_AUROC_mean", "Delta_AUROC_LB", "Delta_AUROC_UB"),
             by = c("Outcome_phecode", "Model")) %>%
  pivot_wider(names_from = "Model", 
              values_from = c("AUROC_mean", "AUROC_LB", "AUROC_UB",
                              "Delta_AUROC_mean", "Delta_AUROC_LB", "Delta_AUROC_UB")) %>%
  left_join(select(phecode_info, Outcome_phecode, phenotype), 
            by = "Outcome_phecode") %>%
  mutate(phenotype = case_when(Outcome_phecode == "death" ~ "Death", 
                               Outcome_phecode == "COVID_hspt" ~ "COVID-19 hospitalisation", 
                               .default = phenotype))

crs_summ_stats_imd_supp %>% 
  arrange(desc(AUROC_mean_CRS_IMDq_Age_Sex - AUROC_mean_CRS_all_predictors)) %>%
  select(phenotype, 
         Delta_AUROC_mean_CRS_IMDq_Age_Sex, 
         Delta_AUROC_mean_CRS_all_predictors) %>%
  View()

##### Label key outcomes #####
selected_outcome_phenotype_imd <- c("COVID-19 hospitalisation",
                                    "Cancer of bronchus; lung",
                                    "Type 2 diabetes",
                                    "Myocardial infarction",
                                    # "Synovitis and tenosynovitis",
                                    # "Sepsis", 
                                    "Death",
                                    "Tobacco use disorder")

crs_summ_stats_imd_supp_final <- crs_summ_stats_imd_supp %>%
  mutate(outcome_label = if_else(phenotype %in% selected_outcome_phenotype_imd, 
                                 phenotype, 
                                 "")) %>%
  mutate(outcome_label_status = if_else(outcome_label == "", 
                                        "0", 
                                        "1"))

##### Plot function #####
CRS_IMD_compare_supp_plot <- function(crs_summ_stats_imd_supp_final,
                                 dodge = position_dodge(0.8),
                                 ref_linewidth = 0.6,
                                 metric_lower_lim = 0.5,
                                 metric_upper_lim = 0.9,
                                 metric_breaks = 0.1,
                                 point_size = base_point_size,
                                 base_size = base_font_size,
                                 error_bar_width = 0.01,
                                 text_label_size = 6,
                                 point_padding = 1,
                                 box_padding = 0.5,
                                 nudge_x = 0,
                                 nudge_y = 0,
                                 seed = 100,
                                 save_format = "pdf",
                                 fig_width = 7,
                                 fig_height = 6) {
  
  crs_imd_plot <- ggplot(crs_summ_stats_imd_supp_final, 
                         aes(x = AUROC_mean_CRS_all_predictors, 
                             y = AUROC_mean_CRS_IMDq_Age_Sex)) + 
    geom_abline(slope = 1, intercept = 0, 
                linewidth = ref_linewidth) +
    # geom_hline(yintercept = 0.5, linetype = "dashed") + 
    # geom_vline(xintercept = 0.5, linetype = "dashed") + 
    geom_point(size = point_size,
               aes(color = outcome_label_status)) +
    scale_color_manual(values = c("1" = red, "0" = grey)) +
    scale_y_continuous(limits = c(metric_lower_lim, metric_upper_lim), 
                       breaks = seq(metric_lower_lim, metric_upper_lim, metric_breaks)) +
    scale_x_continuous(limits = c(metric_lower_lim, metric_upper_lim), 
                       breaks = seq(metric_lower_lim, metric_upper_lim, metric_breaks)) +
    theme_bw(base_size = base_size)+
    theme( 
      legend.position="right",
      # panel.border = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank()
    ) + 
    labs(x = "AUROC (CRS + Age + Sex)",
         y = "AUROC (CRS + IMD + Age + Sex)") 
  
  
  all_outcome_dt_label <- filter(crs_summ_stats_imd_supp_final, 
                                 outcome_label != "")
  nudge_x <- case_when(all_outcome_dt_label$outcome_label == "Myocardial infarction" ~ nudge_x - 0.13,
                       all_outcome_dt_label$outcome_label == "COVID-19 hospitalisation" ~ nudge_x - 0.16,
                       #  all_outcome_dt_label$outcome_label == "Coronary atherosclerosis" ~ nudge_x,
                       all_outcome_dt_label$outcome_label == "Cancer of bronchus; lung" ~ nudge_x - 0.13,
                       all_outcome_dt_label$outcome_label == "Type 2 diabetes" ~ nudge_x + 0.1,
                       all_outcome_dt_label$outcome_label == "Death" ~ nudge_x - 0.02,
                       all_outcome_dt_label$outcome_label == "Tobacco use disorder" ~ nudge_x - 0.04,
                       .default = nudge_x)
  
  nudge_y = case_when(all_outcome_dt_label$outcome_label == "Myocardial infarction" ~ nudge_y,
                      all_outcome_dt_label$outcome_label == "COVID-19 hospitalisation" ~ nudge_y,
                      #  all_outcome_dt_label$outcome_label == "Coronary atherosclerosis" ~ nudge_y - 0.01,
                      all_outcome_dt_label$outcome_label == "Cancer of bronchus; lung" ~ nudge_y + 0.05,
                      all_outcome_dt_label$outcome_label == "Type 2 diabetes" ~ nudge_y - 0.03,
                      all_outcome_dt_label$outcome_label == "Death" ~ nudge_y + 0.04,
                      all_outcome_dt_label$outcome_label == "Tobacco use disorder" ~ nudge_y + 0.03,
                      .default = nudge_y)
  
  crs_imd_plot <- crs_imd_plot +
    geom_point(data = filter(crs_summ_stats_imd_supp_final, 
                             outcome_label != ""),
               aes(x = AUROC_mean_CRS_all_predictors,
                   y = AUROC_mean_CRS_IMDq_Age_Sex),
               size = point_size,
               color = red) +
    ggrepel::geom_label_repel(data = filter(crs_summ_stats_imd_supp_final, 
                                            outcome_label != ""),
                              aes(x = AUROC_mean_CRS_all_predictors,
                                  y = AUROC_mean_CRS_IMDq_Age_Sex, 
                                  label = outcome_label),
                              size = text_label_size,
                              point.padding = point_padding,
                              box.padding = box_padding,
                              nudge_x = nudge_x,
                              nudge_y = nudge_y,
                              seed = seed)+
    guides(color = "none")
  
  ggsave(filename = paste0(figure_dir, "/Competing_methods/CRS_IMD_vs_CRS.", save_format), 
         crs_imd_plot, width = fig_width, height = fig_height, limitsize = FALSE)
  
}

##### Make plot #####
CRS_IMD_compare_supp_plot(crs_summ_stats_imd_supp_final,
                     dodge = position_dodge(0.8),
                     ref_linewidth = 0.6,
                     metric_lower_lim = 0.5,
                     metric_upper_lim = 0.9,
                     metric_breaks = 0.1,
                     point_size = base_point_size,
                     base_size = base_font_size,
                     error_bar_width = 0.01,
                     text_label_size = 6,
                     point_padding = 1,
                     box_padding = 0.5,
                     nudge_x = 0,
                     nudge_y = 0,
                     seed = 100,
                     save_format = "pdf",
                     fig_width = 7,
                     fig_height = 6) 


