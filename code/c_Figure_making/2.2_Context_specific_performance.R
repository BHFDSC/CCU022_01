### Author: Pearl Liu
### Plot context-specific performance ###
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
library(sysfonts)
library(showtext)

#### Set parameters ####
read_dir <- 'Outputs/'
summary_dir <- 'Summary_Stats/'
figure_dir <- 'Figures_Manuscript/'
file_date <- '2025_05_01'

## Model names
model_name_vec <- c("CRS_all_predictors", 
                  #  "CRS_top100",
                  #  "CRS_top50", 
                  #  "CRS_top20", 
                  #  "CRS_top10",
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

##### UPDATE TO LATEST: Read all summary statistics #####
final_summ_stats_by_age <- readRDS(paste0(summary_dir, 
                                 "ByAge_Sex_Specific_All_Outcomes+COVID_PredictionMetrics_ControlCase4to1_WhiteBritish_EvenYOB_Index2015_Followup10yr.rds"))
final_summ_stats_by_sex <- readRDS(paste0(summary_dir, 
                                 "BySex_All_Outcomes+COVID_PredictionMetrics_ControlCase4to1_WhiteBritish_EvenYOB_Index2015_Followup10yr.rds"))
final_summ_stats_by_comorb <- readRDS(paste0(summary_dir, 
                                 "ByComorbidity_Sex_Specific_All_Outcomes+COVID_PredictionMetrics_ControlCase4to1_WhiteBritish_EvenYOB_Index2015_Followup10yr.rds"))
final_summ_stats_by_year <- readRDS(paste0(summary_dir, 
                                 "ByOutcomeYear_Sex_Specific_All_Outcomes+COVID_PredictionMetrics_ControlCase4to1_WhiteBritish_EvenYOB_Index2015_Followup10yr.rds"))

##### Selected phenotypes to plot #####
selected_outcome_ds <- c("Avg", 
                          "COVID_hspt", 
                          "death",
                         "250_2",
                          "165_1",
                          #  "208",
                         # "366",
                          "411_2"
                        #  "994_2"
                          #  "550_5",
                          #  "681_3",
                         )

#### Fig. S3: Performance stratified by number of comorbidities ####
##### Prepare data for plot #####
selected_outcome_ds_pt1 <- selected_outcome_ds[1:3]
selected_summ_by_comorb_pt1 <- final_summ_stats_by_comorb$all_outcome_metrics_delta_dt %>%
  filter(Model == "CRS_all_predictors") %>%
  filter(Outcome_phecode %in% selected_outcome_ds_pt1) %>%
  left_join(phecode_info[, c("Outcome_phecode", "phenotype")], 
            by = "Outcome_phecode") %>%
  mutate(phenotype = case_when(Outcome_phecode == "COVID_hspt" ~ "COVID-19 hospitalisation",
                               Outcome_phecode == "death" ~ "Death",
                               Outcome_phecode == "Avg" ~ "Average",
                               .default = phenotype)) %>%
  mutate(Outcome_phecode = factor(Outcome_phecode, 
                                  levels = c("Avg", "COVID_hspt", "death",
                                             "250_2", "165_1", "411_2"))) %>%
  mutate(Comorbidity = factor(Comorbidity,
                              levels = c("1", "2", "3-4", "5-6", "≥7")))

selected_outcome_ds_pt2 <- selected_outcome_ds[4:6]
selected_summ_by_comorb_pt2 <- final_summ_stats_by_comorb$all_outcome_metrics_delta_dt %>%
  filter(Model == "CRS_all_predictors") %>%
  filter(Outcome_phecode %in% selected_outcome_ds_pt2) %>%
  left_join(phecode_info[, c("Outcome_phecode", "phenotype")], 
            by = "Outcome_phecode") %>%
  mutate(phenotype = case_when(Outcome_phecode == "COVID_hspt" ~ "COVID-19 hospitalisation",
                               Outcome_phecode == "death" ~ "Death",
                               Outcome_phecode == "Avg" ~ "Average",
                               .default = phenotype)) %>%
  mutate(Outcome_phecode = factor(Outcome_phecode, 
                                  levels = c("Avg", "COVID_hspt", "death",
                                             "250_2", "165_1", "411_2"))) %>%
  mutate(Comorbidity = factor(Comorbidity,
                              levels = c("1", "2", "3-4", "5-6", "≥7")))

##### Plot functions #####
stratify_by_comorb_plot <- function(selected_summ_by_comorb,
                                    ref_linewidth = 0.6,
                                    dodge = position_dodge(0.8),
                                    metric_lower_lim = 0,
                                    metric_upper_lim = 0.2,
                                    metric_breaks = 0.05,
                                    point_size = base_point_size,
                                    base_size = base_font_size,
                                    error_bar_width = 0.01,
                                    save_format = "pdf",
                                    fig_width = 25,
                                    fig_height = 6,
                                    fig_part = 0
                                    ) {
  delta_auc_by_comorb <- ggplot(selected_summ_by_comorb, 
                                 aes(x = Comorbidity, 
                                     y = Delta_AUROC_mean, 
                                     group = Outcome_phecode)) +
    geom_point(aes(group = Outcome_phecode), 
              size = point_size) +
    geom_line(aes(group = Outcome_phecode), 
              linewidth = ref_linewidth) +
    geom_errorbar(aes(x = Comorbidity, 
                      ymin = Delta_AUROC_LB, 
                      ymax = Delta_AUROC_UB),
                  position = dodge, width=.2) +
    theme_bw(base_size = base_font_size) +
    theme(
      text = element_text(family = "arial"),
      legend.position="none",
      panel.border = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
    #  axis.text.x = element_blank(),
    #  axis.ticks.x = element_blank(),
      strip.background = element_blank(),
      strip.text.x = element_blank()
    ) + labs(x = "Number of comorbidities", 
             y = expression(Delta ~ AUROC)) +
    scale_y_continuous(limits = c(metric_lower_lim, metric_upper_lim), 
                       breaks = seq(metric_lower_lim, metric_upper_lim, 
                                    metric_breaks)) +
    facet_grid( ~ Outcome_phecode)
  
  font_add("arial", "Arial.ttf") 
  showtext_auto()
  ggsave(paste0(figure_dir, "Context_specific/Delta_AUROC_by_comorb_pt", 
                fig_part, ".", save_format),
          delta_auc_by_comorb, width = fig_width, height = fig_height)
  
#  cairo_pdf(paste0(figure_dir, "Context_specific/Delta_AUROC_by_comorb.", save_format),
#            width = fig_width, height = fig_height)
#  print(delta_auc_by_comorb)
#  dev.off()
  
}

##### Make plot #####
stratify_by_comorb_plot(selected_summ_by_comorb,
                                    ref_linewidth = 0.6,
                                    dodge = position_dodge(0.8),
                                    metric_lower_lim = 0,
                                    metric_upper_lim = 0.2,
                                    metric_breaks = 0.05,
                                    point_size = base_point_size,
                                    base_size = base_font_size,
                                    error_bar_width = 0.01,
                                    save_format = "pdf",
                                    fig_width = 25,
                                    fig_height = 6) 

## Split into two figures
stratify_by_comorb_plot(selected_summ_by_comorb_pt1,
                        ref_linewidth = 0.6,
                        dodge = position_dodge(0.8),
                        metric_lower_lim = 0,
                        metric_upper_lim = 0.2,
                        metric_breaks = 0.05,
                        point_size = base_point_size,
                        base_size = base_font_size,
                        error_bar_width = 0.01,
                        save_format = "pdf",
                        fig_width = 13,
                        fig_height = 6,
                        fig_part = 1) 

stratify_by_comorb_plot(selected_summ_by_comorb_pt2,
                        ref_linewidth = 0.6,
                        dodge = position_dodge(0.8),
                        metric_lower_lim = 0,
                        metric_upper_lim = 0.2,
                        metric_breaks = 0.05,
                        point_size = base_point_size,
                        base_size = base_font_size,
                        error_bar_width = 0.01,
                        save_format = "pdf",
                        fig_width = 13,
                        fig_height = 6,
                        fig_part = 2) 


#### Fig. 3b: Performance stratified by predictor-outcome time gap ####
##### Prepare data for plot #####
selected_summ_by_year <- final_summ_stats_by_year$all_outcome_metrics_delta_dt %>%
  filter(Model == "CRS_all_predictors") %>%
  filter(Outcome_phecode %in% selected_outcome_ds) %>%
  left_join(phecode_info[, c("Outcome_phecode", "phenotype")], 
            by = "Outcome_phecode") %>%
  mutate(phenotype = case_when(Outcome_phecode == "COVID_hspt" ~ "COVID-19 hospitalisation",
                               Outcome_phecode == "death" ~ "Death",
                               Outcome_phecode == "Avg" ~ "Average",
                               .default = phenotype)) %>%
  mutate(Outcome_phecode = factor(Outcome_phecode, 
                                  levels = c("Avg", "COVID_hspt", "death",
                                             "250_2", "165_1", "411_2"))) %>%
  ## plot year gap instead of year of outcome incidence 
  mutate(Year = case_when(Year == "1" ~ "0",
                          Year == "2" ~ "1",
                          Year == "3-4" ~ "2",
                          Year == "5-6" ~ "4",
                          Year == "7-8" ~ "6",
                          Year == "9-10" ~ "8")) %>%
  mutate(Year = factor(Year,
                              levels = c("0", "1", "2", "4", "6",
                                         "8")))

saveRDS(selected_summ_by_year, paste0(summary_dir, "Example_outcomes_performance_by_predictor_outcome_gap.rds"))

##### Plot functions #####
stratify_by_year_plot <- function(selected_summ_by_year,
                                    ref_linewidth = 0.6,
                                    dodge = position_dodge(0.8),
                                    metric_lower_lim = 0,
                                    metric_upper_lim = 0.2,
                                    metric_breaks = 0.05,
                                    point_size = base_point_size,
                                    base_size = base_font_size,
                                    error_bar_width = 0.01,
                                    save_format = "pdf",
                                    fig_width = 25,
                                    fig_height = 6
) {
  delta_auc_by_year <- ggplot(selected_summ_by_year, 
                                aes(x = Year, 
                                    y = Delta_AUROC_mean, 
                                    group = Outcome_phecode)) +
    geom_point(aes(group = Outcome_phecode), 
               size = point_size) +
    geom_line(aes(group = Outcome_phecode), 
              linewidth = ref_linewidth) +
    geom_errorbar(aes(x = Year, 
                      ymin = Delta_AUROC_LB, 
                      ymax = Delta_AUROC_UB),
                  position = dodge, width=.2) +
    theme_bw(base_size = base_font_size) +
    theme(
      panel.spacing.x = unit(0, "pt"),
      text = element_text(family = "arial"),
      legend.position="none",
      panel.border = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      #  axis.text.x = element_blank(),
      #  axis.ticks.x = element_blank(),
      strip.background = element_blank(),
      strip.text.x = element_blank()
    ) + labs(x = "Gap between predictor assessment and outcome incidence (years)", 
             y = expression(Delta * AUROC ~ "(CRS vs. Age + Sex)")) +
    scale_y_continuous(limits = c(metric_lower_lim, metric_upper_lim), 
                       breaks = seq(metric_lower_lim, metric_upper_lim, 
                                    metric_breaks)) +
    facet_grid( ~ Outcome_phecode)
  
  font_add("arial", "Arial.ttf") 
  showtext_auto()
  ggsave(paste0(figure_dir, "Context_specific/Delta_AUROC_by_year.", save_format),
         delta_auc_by_year, width = fig_width, height = fig_height)
  
}

##### Make plot #####
stratify_by_year_plot(selected_summ_by_year,
                        ref_linewidth = 0.6,
                        dodge = position_dodge(0.8),
                        metric_lower_lim = 0,
                        metric_upper_lim = 0.2,
                        metric_breaks = 0.05,
                        point_size = base_point_size,
                        base_size = base_font_size,
                        error_bar_width = 0.01,
                        save_format = "pdf",
                        fig_width = 23,
                        fig_height = 6) 

#### Fig. 3c: Performance stratified by age ####
##### Prepare data for plot #####
selected_summ_by_age <- final_summ_stats_by_age$all_outcome_metrics_delta_dt %>%
  filter(Model == "CRS_all_predictors") %>%
  filter(Outcome_phecode %in% selected_outcome_ds) %>%
  left_join(phecode_info[, c("Outcome_phecode", "phenotype")], 
            by = "Outcome_phecode") %>%
  mutate(phenotype = case_when(Outcome_phecode == "COVID_hspt" ~ "COVID-19 hospitalisation",
                               Outcome_phecode == "death" ~ "Death",
                               Outcome_phecode == "Avg" ~ "Average",
                               .default = phenotype)) %>%
  mutate(Outcome_phecode = factor(Outcome_phecode, 
                                  levels = c("Avg", "COVID_hspt", "death",
                                            "250_2", "165_1", "411_2"))) %>%
  mutate(Age = factor(Age,
                       levels = c("40-49", "50-59", "60-69")))

saveRDS(selected_summ_by_age, paste0(summary_dir, "Example_outcomes_performance_by_age.rds"))


##### Plot functions #####
stratify_by_age_barplot <- function(selected_summ_by_age, 
                                           metric_lower_lim = 0,
                                           metric_upper_lim = 0.2,
                                           metric_breaks = 0.05,
                                           point_size = base_point_size,
                                           base_size = base_font_size,
                                           dodge = position_dodge(0.8),
                                           save_format = "pdf",
                                           fig_width = 15,
                                           fig_height = 6) {
  
  barplot_summ <- ggplot(selected_summ_by_age, 
                         aes(x = Outcome_phecode, 
                             y = Delta_AUROC_mean, 
                             group = Age)) + 
    geom_col(aes(fill = Age),
             position = dodge,
             width = 0.7, alpha = 0.8) +
    geom_errorbar(aes(x = Outcome_phecode, 
                      ymin = Delta_AUROC_LB, 
                      ymax = Delta_AUROC_UB),
                  position = dodge,
                      width=.2) +
    scale_y_continuous(limits = c(metric_lower_lim, metric_upper_lim), 
                       breaks = seq(metric_lower_lim, metric_upper_lim, 
                                    metric_breaks)) +
    scale_fill_manual(limits = levels(selected_summ_by_age$Age),
                      values = c(blue, orange, red)) +
    theme_bw(base_size = base_size) +
    theme( 
      legend.position="none",
      panel.border = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank()
    ) + 
    labs(x = "", 
         y = expression(Delta * AUROC ~ "(CRS vs. Age + Sex)"))
  
  ggsave(filename = paste0(figure_dir, "/Context_specific/Delta_AUROC_by_age.",
                           save_format), 
         barplot_summ, width = fig_width, height = fig_height, limitsize = FALSE)
  
}

##### Make plot #####
stratify_by_age_barplot(selected_summ_by_age, 
                                    metric_lower_lim = 0,
                                    metric_upper_lim = 0.25,
                                    metric_breaks = 0.05,
                                    point_size = base_point_size,
                                    base_size = base_font_size,
                                    save_format = "pdf",
                                    fig_width = 13,
                                    fig_height = 6) 

#### Fig. 3d: Performance stratified by sex ####
##### Prepare data for plot #####
selected_summ_by_sex <- final_summ_stats_by_sex$all_outcome_metrics_delta_dt %>%
  filter(Model == "CRS_all_predictors") %>%
  filter(Outcome_phecode %in% selected_outcome_ds) %>%
  left_join(phecode_info[, c("Outcome_phecode", "phenotype")], 
            by = "Outcome_phecode") %>%
  mutate(phenotype = case_when(Outcome_phecode == "COVID_hspt" ~ "COVID-19 hospitalisation",
                               Outcome_phecode == "death" ~ "Death",
                               Outcome_phecode == "Avg" ~ "Average",
                               .default = phenotype)) %>%
  mutate(Outcome_phecode = factor(Outcome_phecode, 
                                  levels = c("Avg", "COVID_hspt", "death",
                                             "250_2", "165_1", "411_2"))) %>%
  mutate(Sex = factor(Sex,
                      levels = c("Female", "Male")))

saveRDS(selected_summ_by_sex, paste0(summary_dir, "Example_outcomes_performance_by_sex.rds"))


##### Plot functions #####
stratify_by_sex_barplot <- function(selected_summ_by_sex, 
                                    metric_lower_lim = 0,
                                    metric_upper_lim = 0.2,
                                    metric_breaks = 0.05,
                                    point_size = base_point_size,
                                    base_size = base_font_size,
                                    dodge = position_dodge(0.8),
                                    save_format = "pdf",
                                    fig_width = 15,
                                    fig_height = 6) {
  
  barplot_summ <- ggplot(selected_summ_by_sex, 
                         aes(x = Outcome_phecode, 
                             y = Delta_AUROC_mean, 
                             group = Sex)) + 
    geom_col(aes(fill = Sex),
             position = dodge,
             width = 0.7, alpha = 0.8) +
    geom_errorbar(aes(x = Outcome_phecode, 
                      ymin = Delta_AUROC_LB, 
                      ymax = Delta_AUROC_UB),
                  position = dodge,
                  width=.2) +
    scale_y_continuous(limits = c(metric_lower_lim, metric_upper_lim), 
                       breaks = seq(metric_lower_lim, metric_upper_lim, 
                                    metric_breaks)) +
    scale_fill_manual(limits = levels(selected_summ_by_sex$Sex),
                      values = c(green, skyblue)) +
    theme_bw(base_size = base_size) +
    theme( 
      legend.position="none",
      panel.border = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank()
    ) + 
    labs(x = "", 
         y = expression(Delta * AUROC ~ "(CRS vs. Age + Sex)"))
  
  ggsave(filename = paste0(figure_dir, "/Context_specific/Delta_AUROC_by_sex.",
                           save_format), 
         barplot_summ, width = fig_width, height = fig_height, limitsize = FALSE)
  
}

##### Make plot #####
stratify_by_sex_barplot(selected_summ_by_sex, 
                        metric_lower_lim = 0,
                        metric_upper_lim = 0.25,
                        metric_breaks = 0.05,
                        point_size = base_point_size,
                        base_size = base_font_size,
                        save_format = "pdf",
                        fig_width = 13,
                        fig_height = 6) 



