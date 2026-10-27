### Author: Pearl Liu
### Plot general performance metrics ###
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

##### Risk fold for top 5% CRS distribution #####
rr_summ <- read.csv(paste0(read_dir, 
                           "All_outcomes_Risk_ratio_with_CI.csv"))

##### CRS effect size comparison between models #####
crs_effec_size_by_model <- read.csv(paste0(read_dir, 
                                           "All_outcomes_CRS_effect_comparison_by_models_WhiteBritish_OddYOB.csv"))

##### CRS vs. elastic net #####
crs_enet_summ <- read.csv(paste0(read_dir,
                                 "Sex_Specific_CRS_LogisticRegression_vs_CRS_ENet_PredictionMetrics_ControlCase4to1_WhiteBritish_Index2015_Followup10yr.csv"))

#### Fig 1a: General performance: White British pts, Bar chart of AUC, risk ratio, liab R2 for example outcomes ####
##### Select example outcomes #####
selected_outcome_ds <- c("Avg", "COVID_hspt", "death",
                        "165_1", "411_2", "250_2")

selected_outcome_phenotype <- c(paste0("Average across ", n_outcome_ds, " outcomes"), 
                                "COVID-19 hospitalisation",
                                "Death",
                                "Cancer of bronchus; lung",
                                "Myocardial infarction",
                                "Type 2 diabetes"
                                
)

outcome_ds_prev_info <- outcome_ds_prev %>%
  left_join(phecode_info, by = "Outcome_phecode") %>%
  select(Outcome_phecode, Incidence_general, phenotype, category) 

outcome_CRS_metrics <- final_summ_stats_wb$all_outcome_metrics_dt %>%
  filter(Model %in% c("CRS_all_predictors")) %>%
  select(Outcome_phecode, 
         R2_liab_mean,
         AUROC_mean,
         C_index_mean,
         Delta_R2_liab_mean,
         Delta_AUROC_mean, 
         Delta_C_index_mean)

selected_outcome_ds_info <- outcome_ds_prev_info %>%
  left_join(outcome_CRS_metrics, by = "Outcome_phecode") %>%
  filter(Outcome_phecode %in% selected_outcome_ds) %>%
  na.omit() %>%
  arrange(desc(AUROC_mean))

##### Filter metrics to CRS full model and Age+sex model and selected outcomes #####
selected_outcome_summ <- final_summ_stats_wb$all_outcome_metrics_dt %>%
  filter(Model %in% c("CRS_all_predictors", "Age_Sex")) %>%
  filter(Outcome_phecode %in% selected_outcome_ds) %>%
  left_join(selected_outcome_ds_info %>% select(Outcome_phecode,
                                                Incidence_general,
                                                phenotype,
                                                category), 
            by = "Outcome_phecode") %>%
  mutate(phenotype = case_when(Outcome_phecode == "Avg" ~ paste0("Average across ", n_outcome_ds, " outcomes"),
                               Outcome_phecode == "COVID_hspt" ~ "COVID-19 hospitalisation",
                               Outcome_phecode == "death" ~ "Death",
                               .default = phenotype)) %>%
  mutate(Model_label = factor(if_else(Model == "CRS_all_predictors", "CRS + Age + Sex",
                               "Age + Sex"), 
         levels = c("CRS + Age + Sex", "Age + Sex"))) %>%
  mutate(phenotype = factor(phenotype, levels = selected_outcome_phenotype))


##### Filter metrics to CRS full model and Age+sex model and all outcomes (for Supp Figures and Tables) #####
all_outcome_summ0 <- final_summ_stats_wb$all_outcome_metrics_dt %>%
  filter(Model %in% c("CRS_all_predictors", "Age_Sex")) %>%
  left_join(outcome_ds_prev_info, by = "Outcome_phecode") %>%
  mutate(phenotype = case_when(Outcome_phecode == "Avg" ~ paste0("Average across ", n_outcome_ds, " outcomes"),
                               Outcome_phecode == "COVID_hspt" ~ "COVID-19 hospitalisation",
                               Outcome_phecode == "death" ~ "Death",
                               Outcome_phecode == "626" ~ "Disorders of menstruation",
                               Outcome_phecode == "189" ~ "Cancer of urinary organs",
                               Outcome_phecode == "433_21" ~ "Ischemic stroke",
                               Outcome_phecode == "465" ~ "Acute upper respiratory infections",
                               Outcome_phecode == "702" ~ "Degenerative skin conditions",
                               .default = phenotype)) %>%
  mutate(Model_label = factor(if_else(Model == "CRS_all_predictors", "CRS + Age + Sex",
                                      "Age + Sex"), 
                              levels = c("CRS + Age + Sex", "Age + Sex"))) 

## Order all phenotypes according to AUROC (desc.)
all_phenotype_ordered0 <- all_outcome_summ0 %>%
  filter(!(Outcome_phecode %in% c("Avg", "COVID_hspt"))) %>%
  filter(Model == "CRS_all_predictors") %>%
  arrange(desc(AUROC_mean)) %>%
  pull(phenotype)
all_phenotype_ordered <- c(paste0("Average across ", n_outcome_ds, " outcomes"),
                           "COVID-19 hospitalisation",
                           all_phenotype_ordered0)

all_outcome_summ <- all_outcome_summ0 %>%
  mutate(phenotype = factor(phenotype, levels = all_phenotype_ordered))


##### Plot functions #####
General_performance_plot <- function(selected_outcome_summ_metric,
                             metric_name = "AUROC",
                             dodge = position_dodge(0.8),
                             ref_linewidth = 0.6,
                             metric_lower_lim = 0,
                             metric_upper_lim = 0.3,
                             metric_breaks = 0.1,
                             point_size = 4,
                             base_size = base_font_size,
                             legend_rm = T,
                             save_format = "pdf",
                             fig_width = 8.5,
                             fig_height = 4.8,
                             fig_type = "main") {
  if (metric_name %in% c("AUROC", "C-index")) {
    x_axis_label <- seq(metric_lower_lim, metric_upper_lim, metric_breaks) + 0.5
  } else {
    x_axis_label <- seq(metric_lower_lim, metric_upper_lim, metric_breaks)
  }
  
  barplot_summ <- ggplot(selected_outcome_summ_metric, 
                       aes(x = fct_rev(phenotype), y = metric_mean, group = fct_rev(Model_label))) + 
  geom_col(aes(fill = fct_rev(Model_label)),
           position = dodge, width=.7, alpha = 0.8) +
#  geom_point(aes(color = fct_rev(Model_label)), size = point_size,
#             position = dodge) +
  geom_errorbar(aes(x = fct_rev(phenotype), ymin = metric_lb, ymax = metric_ub),
                position = dodge, width=.2) +
  scale_y_continuous(limits = c(metric_lower_lim, metric_upper_lim), 
                     breaks = seq(metric_lower_lim, metric_upper_lim, metric_breaks),
                     labels = format(x_axis_label, nsmall = 2)) +
  coord_flip() + 
  scale_fill_manual(limits = levels(selected_outcome_summ_metric$Model_label),
                    values = c(red, grey)) +
  theme_bw(base_size = base_size)+
  theme( 
    legend.position="top",
    panel.border = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank()
  ) 
  
  if (legend_rm == T) {
    barplot_summ <- barplot_summ + 
      guides(fill="none")
  }
  
  if (metric_name %in% c("AUROC", "C-index")) {
    barplot_summ <- barplot_summ + 
      labs(x = "", y = paste0(metric_name, ""), fill = "Model") 
#      geom_hline(yintercept=0.5, linetype="dashed", linewidth = ref_linewidth)
  }
  
  if (metric_name == "R2_liab") {
    barplot_summ <- barplot_summ + 
      labs(x = "", y = expression(Liability ~ R^2), fill = "Model") 
  }
  
  if (metric_name == "R2_obs") {
    barplot_summ <- barplot_summ + 
      labs(x = "", y = expression(Observed ~ R^2), fill = "Model") 
  }
  
  if (metric_name == "Risk_ratio") {
    barplot_summ <- barplot_summ + 
      labs(x = "", y = "Risk ratio", fill = "Model") 
  }
  
  if (metric_name == "AUPRC_prev_diff") {
    barplot_summ <- barplot_summ + 
      labs(x = "", y = "AUPRC - incidence", fill = "Model") 
  }

  if (fig_type == "supp") {
    ggsave(filename = paste0(figure_dir, "/General_performance/Supp_Panel1_", metric_name, ".",
                             save_format), 
           barplot_summ, width = fig_width, height = fig_height, limitsize = FALSE)
    
  } else if (fig_type == "main") {
    ggsave(filename = paste0(figure_dir, "/General_performance/Panel1_", metric_name, ".",
                             save_format), 
         barplot_summ, width = fig_width, height = fig_height, limitsize = FALSE)
  }

}

General_performance_plot_poster <- function(selected_outcome_summ_metric,
                             metric_name = "AUROC",
                             dodge = position_dodge(0.8),
                             ref_linewidth = 0.6,
                             metric_lower_lim = 0,
                             metric_upper_lim = 0.3,
                             metric_breaks = 0.05,
                             point_size = 4) {
  barplot_summ <- ggplot(selected_outcome_summ_metric, 
                         aes(x = fct_rev(phenotype), y = metric_mean, group = fct_rev(Model_label))) + 
    geom_col(aes(fill = fct_rev(Model_label)),
             position = dodge, width=.7, alpha = 0.8) +
    #  geom_point(aes(color = fct_rev(Model_label)), size = point_size,
    #             position = dodge) +
    geom_errorbar(aes(x = fct_rev(phenotype), ymin = metric_lb, ymax = metric_ub),
                  position = dodge, width=.2) +
    scale_y_continuous(limits = c(metric_lower_lim, metric_upper_lim), 
                       breaks = seq(metric_lower_lim, metric_upper_lim, metric_breaks)) +
    coord_flip() + 
    scale_fill_manual(limits = levels(selected_outcome_summ_metric$Model_label),
                      values = c("red", "gray")) +
    theme_bw(base_size = 24)+
    theme( 
      legend.position="top",
      panel.border = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank()
    ) 
  
  if (metric_name %in% c("AUROC", "C-index")) {
    barplot_summ <- barplot_summ + 
      labs(x = "", y = paste0(metric_name, " - 0.5"), fill = "Model") 
    #      geom_hline(yintercept=0.5, linetype="dashed", linewidth = ref_linewidth)
  }
  
  if (metric_name == "R2_liab") {
    barplot_summ <- barplot_summ + 
      labs(x = "", y = expression(Liability ~ R^2), fill = "Model") 
  }
  
  ggsave(filename = paste0(figure_dir, "/Figure1/Panel1_Poster_", metric_name, ".jpg"), 
         barplot_summ, width = 11, height = 7, limitsize = FALSE)
  
}

##### Plot for AUROC (example outcomes) #####
selected_outcome_summ_auc <- selected_outcome_summ %>%
  select(phenotype, Model_label, AUROC_mean, AUROC_LB, AUROC_UB) %>%
  mutate(AUROC_mean_above_05 = AUROC_mean - 0.5,
         AUROC_LB_above_05 = AUROC_LB - 0.5,
         AUROC_UB_above_05 = AUROC_UB - 0.5) %>%
  rename(metric_mean = AUROC_mean_above_05,
         metric_lb = AUROC_LB_above_05,
         metric_ub = AUROC_UB_above_05)


General_performance_plot(selected_outcome_summ_auc,
                         metric_name = "AUROC",
                         dodge = position_dodge(0.8),
                         ref_linewidth = 0.6,
                         metric_lower_lim = 0,
                         metric_upper_lim = 0.3,
                         metric_breaks = 0.1,
                         point_size = 4)

General_performance_plot_poster(selected_outcome_summ_auc,
                 metric_name = "AUROC",
                 dodge = position_dodge(0.8),
                 ref_linewidth = 0.6,
                 metric_lower_lim = 0,
                 metric_upper_lim = 0.3,
                 metric_breaks = 0.05,
                 point_size = 4)

##### Plot for AUROC (all outcomes) #####
all_outcome_summ_auc <- all_outcome_summ %>%
  select(phenotype, Model_label, AUROC_mean, AUROC_LB, AUROC_UB) %>%
  mutate(AUROC_mean_above_05 = AUROC_mean - 0.5,
         AUROC_LB_above_05 = AUROC_LB - 0.5,
         AUROC_UB_above_05 = AUROC_UB - 0.5) %>%
  rename(metric_mean = AUROC_mean_above_05,
         metric_lb = AUROC_LB_above_05,
         metric_ub = AUROC_UB_above_05)


General_performance_plot(all_outcome_summ_auc,
                         metric_name = "AUROC",
                         dodge = position_dodge(0.8),
                         ref_linewidth = 0.6,
                         metric_lower_lim = 0,
                         metric_upper_lim = 0.35,
                         metric_breaks = 0.1,
                         point_size = 4,
                         fig_width = 10,
                         fig_height = 24,
                         fig_type = "supp")

##### Risk ratio summary stats #####
## Add avg risk ratio and approximated 95% CI for each model
# Assume common correlation rho between log(RR)s
rho <- 0.5

## CRS model
rr_summ_crs <- rr_summ %>% filter(Model == "CRS_all_predictors")
se_log_rr_crs <- rr_summ_crs$se_log_risk_ratio
Sigma_log_rr_crs <- outer(se_log_rr_crs, se_log_rr_crs) * rho
diag(Sigma_log_rr_crs) <- se_log_rr_crs^2
mean_rr_crs <- mean(rr_summ_crs$risk_ratio)

# Delta-method gradient
g_crs <- rr_summ_crs$risk_ratio / n_outcome_ds
var_mean_rr_crs <- as.numeric(t(g_crs) %*% Sigma_log_rr_crs %*% g_crs)
se_mean_rr_crs <- sqrt(var_mean_rr_crs)
mean_rr_ci_crs <- mean_rr_crs + c(-1, 1) * qnorm(0.975) * se_mean_rr_crs


## Age + Sex model
rr_summ_baseline <- rr_summ %>% filter(Model == "Age_Sex")
se_log_rr_baseline <- rr_summ_baseline$se_log_risk_ratio
Sigma_log_rr_baseline <- outer(se_log_rr_baseline, se_log_rr_baseline) * rho
diag(Sigma_log_rr_baseline) <- se_log_rr_baseline^2
mean_rr_baseline <- mean(rr_summ_baseline$risk_ratio)

# Delta-method gradient
g_baseline <- rr_summ_baseline$risk_ratio / n_outcome_ds
var_mean_rr_baseline <- as.numeric(t(g_baseline) %*% Sigma_log_rr_baseline %*% g_baseline)
se_mean_rr_baseline <- sqrt(var_mean_rr_baseline)
mean_rr_ci_baseline <- mean_rr_baseline + c(-1, 1) * qnorm(0.975) * se_mean_rr_baseline


##### Plot for Risk ratio (example outcomes) #####
selected_outcome_summ_rr0 <- rr_summ %>%
  filter(Outcome_phecode %in% selected_outcome_ds) %>%
  left_join(selected_outcome_ds_info %>% select(Outcome_phecode,
                                                Incidence_general,
                                                phenotype,
                                                category), 
            by = "Outcome_phecode") %>%
  mutate(phenotype = case_when(Outcome_phecode == "Avg" ~ paste0("Average across ", n_outcome_ds, " outcomes"),
                               Outcome_phecode == "COVID_hspt" ~ "COVID-19 hospitalisation",
                               Outcome_phecode == "death" ~ "Death",
                               .default = phenotype)) %>%
  mutate(Model_label = factor(if_else(Model == "CRS_all_predictors", "CRS + Age + Sex",
                                      "Age + Sex"), 
                              levels = c("CRS + Age + Sex", "Age + Sex"))) %>%
  mutate(phenotype = factor(phenotype, levels = selected_outcome_phenotype))

selected_outcome_summ_rr <- selected_outcome_summ_rr0 %>%
  select(phenotype, Model_label, risk_ratio, CI_LB, CI_UB) %>%
  rbind(data.frame(phenotype = paste0("Average across ", n_outcome_ds, " outcomes"),
                   Model_label = "CRS + Age + Sex",
                   risk_ratio = mean_rr_crs,
                   CI_LB = mean_rr_ci_crs[1],
                   CI_UB = mean_rr_ci_crs[2])) %>%
  rbind(data.frame(phenotype = paste0("Average across ", n_outcome_ds, " outcomes"),
                   Model_label = "Age + Sex",
                   risk_ratio = mean_rr_baseline,
                   CI_LB = mean_rr_ci_baseline[1],
                   CI_UB = mean_rr_ci_baseline[2])) %>%
  mutate(phenotype = factor(phenotype, levels = selected_outcome_phenotype)) %>%
  rename(metric_mean = risk_ratio,
         metric_lb = CI_LB,
         metric_ub = CI_UB) 

General_performance_plot(selected_outcome_summ_rr,
                         metric_name = "Risk_ratio",
                         dodge = position_dodge(0.8),
                         ref_linewidth = 0.6,
                         metric_lower_lim = 0,
                         metric_upper_lim = 7,
                         metric_breaks = 2,
                         point_size = 4)


##### Plot for Risk ratio (all outcomes) #####
all_outcome_summ_rr0 <- rr_summ %>%
  left_join(outcome_ds_prev_info, 
            by = "Outcome_phecode") %>%
  mutate(phenotype = case_when(Outcome_phecode == "Avg" ~ paste0("Average across ", n_outcome_ds, " outcomes"),
                               Outcome_phecode == "COVID_hspt" ~ "COVID-19 hospitalisation",
                               Outcome_phecode == "death" ~ "Death",
                               Outcome_phecode == "626" ~ "Disorders of menstruation",
                               Outcome_phecode == "189" ~ "Cancer of urinary organs",
                               Outcome_phecode == "433_21" ~ "Ischemic stroke",
                               Outcome_phecode == "465" ~ "Acute upper respiratory infections",
                               Outcome_phecode == "702" ~ "Degenerative skin conditions",
                               .default = phenotype)) %>%
  mutate(Model_label = factor(if_else(Model == "CRS_all_predictors", "CRS + Age + Sex",
                                      "Age + Sex"), 
                              levels = c("CRS + Age + Sex", "Age + Sex"))) %>%
  mutate(phenotype = factor(phenotype, levels = all_phenotype_ordered))

all_outcome_summ_rr <- all_outcome_summ_rr0 %>%
  select(phenotype, Model_label, risk_ratio, CI_LB, CI_UB) %>%
  rbind(data.frame(phenotype = paste0("Average across ", n_outcome_ds, " outcomes"),
                   Model_label = "CRS + Age + Sex",
                   risk_ratio = mean_rr_crs,
                   CI_LB = mean_rr_ci_crs[1],
                   CI_UB = mean_rr_ci_crs[2])) %>%
  rbind(data.frame(phenotype = paste0("Average across ", n_outcome_ds, " outcomes"),
                   Model_label = "Age + Sex",
                   risk_ratio = mean_rr_baseline,
                   CI_LB = mean_rr_ci_baseline[1],
                   CI_UB = mean_rr_ci_baseline[2])) %>%
  mutate(phenotype = factor(phenotype, levels = all_phenotype_ordered)) %>%
  rename(metric_mean = risk_ratio,
         metric_lb = CI_LB,
         metric_ub = CI_UB) 

General_performance_plot(all_outcome_summ_rr,
                         metric_name = "Risk_ratio",
                         dodge = position_dodge(0.8),
                         ref_linewidth = 0.6,
                         metric_lower_lim = 0,
                         metric_upper_lim = 7,
                         metric_breaks = 2,
                         point_size = 4,
                         fig_width = 10,
                         fig_height = 24,
                         fig_type = "supp")


##### Plot for C-index (example outcomes) #####
selected_outcome_summ_cindex <- selected_outcome_summ %>%
  select(phenotype, Model_label, C_index_mean, C_index_LB, C_index_UB) %>%
  mutate(C_index_mean_above_05 = C_index_mean - 0.5,
         C_index_LB_above_05 = C_index_LB - 0.5,
         C_index_UB_above_05 = C_index_UB - 0.5) %>%
  rename(metric_mean = C_index_mean_above_05,
         metric_lb = C_index_LB_above_05,
         metric_ub = C_index_UB_above_05)

General_performance_plot(selected_outcome_summ_cindex,
                         metric_name = "C-index",
                         dodge = position_dodge(0.8),
                         ref_linewidth = 0.6,
                         metric_lower_lim = 0,
                         metric_upper_lim = 0.3,
                         metric_breaks = 0.1,
                         point_size = 4)

General_performance_plot_poster(selected_outcome_summ_cindex,
                 metric_name = "C-index",
                 dodge = position_dodge(0.8),
                 ref_linewidth = 0.6,
                 metric_lower_lim = 0,
                 metric_upper_lim = 0.3,
                 metric_breaks = 0.05,
                 point_size = 4)


##### Plot for C-index (all outcomes) #####
all_outcome_summ_cindex <- all_outcome_summ %>%
  select(phenotype, Model_label, C_index_mean, C_index_LB, C_index_UB) %>%
  mutate(C_index_mean_above_05 = C_index_mean - 0.5,
         C_index_LB_above_05 = C_index_LB - 0.5,
         C_index_UB_above_05 = C_index_UB - 0.5) %>%
  rename(metric_mean = C_index_mean_above_05,
         metric_lb = C_index_LB_above_05,
         metric_ub = C_index_UB_above_05)

General_performance_plot(all_outcome_summ_cindex,
                         metric_name = "C-index",
                         dodge = position_dodge(0.8),
                         ref_linewidth = 0.6,
                         metric_lower_lim = 0,
                         metric_upper_lim = 0.3,
                         metric_breaks = 0.1,
                         point_size = 4,
                         fig_width = 10,
                         fig_height = 24,
                         fig_type = "supp")


##### Plot for liability R2 (example outcomes) #####
selected_outcome_summ_R2 <- selected_outcome_summ %>%
  select(phenotype, Model_label, R2_liab_mean, R2_liab_LB, R2_liab_UB) %>%
  rename(metric_mean = R2_liab_mean,
         metric_lb = R2_liab_LB,
         metric_ub = R2_liab_UB)

General_performance_plot(selected_outcome_summ_R2,
                 metric_name = "R2_liab",
                 dodge = position_dodge(0.8),
                 ref_linewidth = 0.6,
                 metric_lower_lim = 0,
                 metric_upper_lim = 0.38,
                 metric_breaks = 0.12,
                 point_size = 4)


##### Plot for liability R2 (all outcomes) #####
all_outcome_summ_R2 <- all_outcome_summ %>%
  select(phenotype, Model_label, R2_liab_mean, R2_liab_LB, R2_liab_UB) %>%
  rename(metric_mean = R2_liab_mean,
         metric_lb = R2_liab_LB,
         metric_ub = R2_liab_UB)

General_performance_plot(all_outcome_summ_R2,
                         metric_name = "R2_liab",
                         dodge = position_dodge(0.8),
                         ref_linewidth = 0.6,
                         metric_lower_lim = 0,
                         metric_upper_lim = 0.38,
                         metric_breaks = 0.12,
                         point_size = 4,
                         fig_width = 10,
                         fig_height = 24,
                         fig_type = "supp")

##### Plot for observed R2 (all outcomes) #####
all_outcome_summ_R2_obs <- all_outcome_summ %>%
  select(phenotype, Model_label, R2_obs_mean, R2_obs_LB, R2_obs_UB) %>%
  rename(metric_mean = R2_obs_mean,
         metric_lb = R2_obs_LB,
         metric_ub = R2_obs_UB)

General_performance_plot(all_outcome_summ_R2_obs,
                         metric_name = "R2_obs",
                         dodge = position_dodge(0.8),
                         ref_linewidth = 0.6,
                         metric_lower_lim = 0,
                         metric_upper_lim = 0.24,
                         metric_breaks = 0.08,
                         point_size = 4,
                         fig_width = 10,
                         fig_height = 24,
                         fig_type = "supp")


##### Plot for AUPRC - incidence (all outcomes) #####
all_outcome_summ_AUPRC_prev_diff <- all_outcome_summ %>%
  select(phenotype, Model_label, 
         AUPRC_prev_diff_mean, AUPRC_prev_diff_LB, AUPRC_prev_diff_UB) %>%
  rename(metric_mean = AUPRC_prev_diff_mean,
         metric_lb = AUPRC_prev_diff_LB,
         metric_ub = AUPRC_prev_diff_UB)

General_performance_plot(all_outcome_summ_AUPRC_prev_diff,
                         metric_name = "AUPRC_prev_diff",
                         dodge = position_dodge(0.8),
                         ref_linewidth = 0.6,
                         metric_lower_lim = 0,
                         metric_upper_lim = 0.28,
                         metric_breaks = 0.07,
                         point_size = 4,
                         fig_width = 10,
                         fig_height = 24,
                         fig_type = "supp")


#### Fig 1b: Sample size saturation: White British pts, Testing R^2 vs. Training R^2 (liability scale) over all outcomes ####
##### Approximate the SE for training R2 #####

## Obtain R^2 on observed scale based on liability-scale R^2, with case-control correction
## Reference: A Better Coefficient of Determination for Genetic Profile Analysis by Lee et. al. - Equation (15) and Appendix B
## K: population prevalence
## P: proportion of cases in the case-control samples
LiabR2_cc_inverse <- function(K, P, R2_liab){
  # Assume follow Normal distribution
  # The threshold on the normal distribution which truncates the proportion of disease prevalence
  thd <- -qnorm(K, 0, 1)
  zv <- dnorm(thd) #z (normal density)
  mv = zv/K  #mean liability for case
  
  # Convert using equation (15) of Lee
  theta = mv*(P-K)/(1-K)*(mv*(P-K)/(1-K)-thd) #theta in equation (15)
  cv = K*(1-K)/zv^2*K*(1-K)/(P*(1-P)) #C in equation (15)
  
  R2_obs = R2_liab/(cv*(1-R2_liab*theta))
  
  return(R2_obs)
}


all_outcome_train_R2_dt <- final_summ_stats_wb$all_outcome_train_R2_dt %>%
  filter(Outcome_phecode != "Avg") %>%
  left_join(outcome_ds_prev[, c("Outcome_phecode", "Incidence_combined",
                                "Incidence_general")], 
            by = "Outcome_phecode")

## Obtain observed R2 on link scale
all_outcome_train_R2_dt$R2_obs_link <- mapply(function(x, y) {
  LiabR2_cc_inverse(x, 0.2, y)
}, all_outcome_train_R2_dt$Incidence_combined, 
   all_outcome_train_R2_dt$R2_liab)

## Approximate SE of R2_liab based on SE of R2_obs_link
## Reference: Bonett, D. G. (2008). Meta-analytic interval estimation for bivariate correlations.
## Use delta method to approximate SE(R^2) from SE(R)
all_outcome_train_R2_dt <- all_outcome_train_R2_dt %>%
   rename(train_R2_liab = R2_liab)

##### Combine Testing R^2 and Training R^2 #####
all_outcome_test_R2_dt <- final_summ_stats_wb$all_outcome_metrics_dt %>%
  filter(Model == "CRS_all_predictors") %>%
  filter(Outcome_phecode != "Avg") %>%
  select(Outcome_phecode, R2_liab_mean, R2_liab_LB, R2_liab_UB) %>%
  rename(test_R2_liab = R2_liab_mean,
         test_R2_LB = R2_liab_LB,
         test_R2_UB = R2_liab_UB) %>%
  left_join(phecode_info[, c("Outcome_phecode", "phenotype")], 
            by = "Outcome_phecode") %>%
  mutate(phenotype = case_when(Outcome_phecode == "COVID_hspt" ~ "COVID-19 hospitalisation",
                               Outcome_phecode == "death" ~ "Death",
                             .default = phenotype))

all_outcome_train_R2_dt_select <- all_outcome_train_R2_dt %>%
  filter(Model == "CRS_all_predictors") %>%
  select(Outcome_phecode, 
         Incidence_general, 
         train_R2_liab)




all_outcome_R2_dt <- all_outcome_test_R2_dt %>%
  left_join(all_outcome_train_R2_dt_select, by = "Outcome_phecode") 


###### Label key outcomes ######
selected_outcome_ds0 <- c("Avg", 
                          "COVID_hspt", 
                          "death",
                          "165_1",
                          "250_2",
                          #  "208",
                         # "366", 
                          "411_2")
                          #  "550_5",
                          #  "681_3",
                        #  "994_2")
selected_outcome_phenotype0 <- c(paste0("Average across ", n_outcome_ds, " outcomes"), 
                                 "COVID-19 hospitalisation",
                                 "Death",
                                 "Cancer of bronchus; lung",
                                 "Type 2 diabetes",
                                 #    "Benign neoplasm of colon",
                                # "Cataract",
                                 "Myocardial infarction")
                                 #    "Ventral hernia",
                                 #      "Cellulitis and abscess of arm/hand",
                               #  "Sepsis")
all_outcome_R2_dt <- all_outcome_R2_dt %>% 
  mutate(outcome_label = if_else(phenotype %in% selected_outcome_phenotype0, 
                                  phenotype, 
                                  "")) %>%
  mutate(outcome_label_status = if_else(outcome_label == "", 
                               "0", 
                               "1"))

##### Plot function #####
R2_saturation_plot <- function(all_outcome_R2_dt,
                             dodge = position_dodge(0.8),
                             ref_linewidth = 0.6,
                             metric_lower_lim = 0,
                             metric_upper_lim = 0.35,
                             metric_breaks = 0.05,
                             point_size = 4,
                             base_size = base_font_size,
                             error_bar_width = 0.01,
                             model_fit = F,
                             color_by_incid = F,
                             text_label_size = 5,
                             point_padding = 1,
                             box_padding = 0.5,
                             nudge_x = 0,
                             nudge_y = 0,
                             seed = 100,
                             save_format = "pdf",
                             fig_width = 7,
                             fig_height = 6) {
  if (color_by_incid == F) {
    plot_NoFit <- ggplot(all_outcome_R2_dt, 
                                aes(x = train_R2_liab, 
                                    y = test_R2_liab)) + 
      geom_abline(slope = 1, intercept = 0, 
                  linewidth = ref_linewidth) +
      geom_point(size = point_size,
                 aes(color = outcome_label_status)) +
      scale_color_manual(values = c("1" = red, "0" = grey)) +
#      geom_errorbar(aes(ymin = test_R2_LB, ymax = test_R2_UB, color = Incidence_group), 
#                    width = error_bar_width) +
#      geom_errorbarh(aes(xmin = train_R2_LB, xmax = train_R2_UB, color = Incidence_group), 
#                     height = error_bar_width) +
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
      labs(x = expression(Training ~ liability ~ R^2), 
           y = expression(Testing ~ liability ~ R^2)) 
    
    if (model_fit == T) {
      all_outcome_R2_dt_label <- filter(all_outcome_R2_dt, 
                                        outcome_label != "")
      nudge_x <- case_when(all_outcome_R2_dt_label$outcome_label == "Myocardial infarction" ~ nudge_x + 0.06,
                           all_outcome_R2_dt_label$outcome_label == "COVID-19 hospitalisation" ~ nudge_x - 0.1,
                           all_outcome_R2_dt_label$outcome_label == "Cancer of bronchus; lung" ~ nudge_x + 0.05,
                           all_outcome_R2_dt_label$outcome_label == "Type 2 diabetes" ~ nudge_x + 0.05,
                            .default = nudge_x)
      nudge_y <- case_when(all_outcome_R2_dt_label$outcome_label == "Myocardial infarction" ~ nudge_y - 0.02,
                           all_outcome_R2_dt_label$outcome_label == "COVID-19 hospitalisation" ~ nudge_y + 0.05,
                           all_outcome_R2_dt_label$outcome_label == "Cancer of bronchus; lung" ~ nudge_y,
                           all_outcome_R2_dt_label$outcome_label == "Type 2 diabetes" ~ nudge_y - 0.02,
                           .default = nudge_y)
      
      plot_Fit <- plot_NoFit +
        geom_smooth(method = "lm", formula = y~0+x,
                    alpha = 0.5) + 
        geom_point(data = filter(all_outcome_R2_dt, 
                                 outcome_label != ""),
                   aes(x = train_R2_liab,
                       y = test_R2_liab),
                   size = point_size,
                   color = red) +
        ggrepel::geom_label_repel(data = filter(all_outcome_R2_dt, 
                                                outcome_label != ""),
                                  aes(x = train_R2_liab,
                                      y = test_R2_liab, 
                                      label = outcome_label),
                                  size = text_label_size,
                                  point.padding = point_padding,
                                  box.padding = box_padding,
                                  nudge_x = nudge_x,
                                  nudge_y = nudge_y,
                                  seed = seed)+
        guides(color = "none")
    }
    
    
  } else if (color_by_incid == T) {
    plot_NoFit <- ggplot(all_outcome_R2_dt, 
                                aes(x = train_R2_liab, 
                                    y = test_R2_liab)) + 
      geom_abline(slope = 1, intercept = 0, 
                  linewidth = ref_linewidth) +
      geom_point(aes(color = Incidence_group), size = point_size) +
#      geom_errorbar(aes(ymin = test_R2_LB, ymax = test_R2_UB, color = Incidence_group), 
#                    width = error_bar_width) +
#      geom_errorbarh(aes(xmin = train_R2_LB, xmax = train_R2_UB, color = Incidence_group), 
#                     height = error_bar_width) +
      scale_y_continuous(limits = c(metric_lower_lim, metric_upper_lim), 
                         breaks = seq(metric_lower_lim, metric_upper_lim, metric_breaks)) +
      scale_x_continuous(limits = c(metric_lower_lim, metric_upper_lim), 
                         breaks = seq(metric_lower_lim, metric_upper_lim, metric_breaks)) +
      #  geom_smooth(method = "lm", formula = y~0+x,
      #              alpha = 0.5) +
      scale_color_manual(limits = levels(all_outcome_R2_dt$Incidence_group),
                         values = c("gold", "orange", "red", "brown")) +
      theme_bw(base_size = 20)+
      theme( 
        legend.position="right",
        # panel.border = element_blank(),
        # panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank()
      ) + 
      labs(x = expression(Training ~ R^2), 
           y = expression(Testing ~ R^2), 
           color = "Outcome incidence")
    
    if (model_fit == T) {
      plot_Fit <- plot_NoFit +
        geom_smooth(method = "lm", formula = y~0+x,
                    alpha = 0.5)
    }
    
  } 
  
  ggsave(filename = paste0(figure_dir, "/General_performance/Panel2_NoFit.", save_format), 
         plot_NoFit, width = fig_width, height = fig_height, limitsize = FALSE)
#  ggsave(filename = paste0(figure_dir, "/Figure1/Panel2_NoFit_Label.jpg"), 
#         Fig1_panel2_NoFit_Label, width = 11, height = 7, limitsize = FALSE)
  if (model_fit == T) {
    ggsave(filename = paste0(figure_dir, "/General_performance/Panel2_Fit.", save_format), 
           plot_Fit, width = fig_width, height = fig_height, limitsize = FALSE)
  }
}



##### Slope from regression #####
summary(lm(test_R2_liab ~ 0 + train_R2_liab, data = all_outcome_R2_dt))
# Coefficients:
#               Estimate Std. Error t value Pr(>|t|)    
# train_R2_liab 0.835480   0.009468   88.24   <2e-16 ***


##### Plot and save plots #####
R2_saturation_plot(all_outcome_R2_dt,
                            dodge = position_dodge(0.8),
                            ref_linewidth = 0.6,
                            metric_lower_lim = 0,
                            metric_upper_lim = 0.4,
                            metric_breaks = 0.1,
                            point_size = 4,
                            error_bar_width = 0.01,
                            model_fit = T,
                            color_by_incid = F,
                   text_label_size = 6,
                   point_padding = 0.5,
                   box_padding = 0.45,
                   nudge_x = 0.02,
                   nudge_y = 0,
                   seed = 100,
                   fig_width = 7.6,
                   fig_height = 6.5)

#### Fig 1c: Predictor saturation: White British pts, Delta liability R^2 vs. number of predictors ####
##### Filter to selected outcomes #####
selected_outcome_ds0 <- c("Avg", 
                          "COVID_hspt", 
                          "death",
                          "165_1",
                          "250_2",
                          #  "208",
                          # "366", 
                          "411_2")
#  "550_5",
#  "681_3",
#  "994_2")
selected_outcome_phenotype0 <- c(paste0("Average across ", n_outcome_ds, " outcomes"), 
                                 "COVID-19 hospitalisation",
                                 "Death",
                                 "Cancer of bronchus; lung",
                                 "Type 2 diabetes",
                                 #    "Benign neoplasm of colon",
                                 # "Cataract",
                                 "Myocardial infarction")
#    "Ventral hernia",
#      "Cellulitis and abscess of arm/hand",
#  "Sepsis")


## Order by delta R2
outcome_order <- c(3, 2, 4, 5, 6, 1) 
selected_outcome_ds <- selected_outcome_ds0[outcome_order]
selected_outcome_phenotype <- selected_outcome_phenotype0[outcome_order]

test_R2_vs_model_dt <- final_summ_stats_wb$all_outcome_metrics_delta_dt %>%
  filter(Outcome_phecode %in% selected_outcome_ds) %>%
  select(Outcome_phecode, Model, Delta_R2_liab_mean, 
         Delta_R2_liab_LB, Delta_R2_liab_UB) %>%
  left_join(phecode_info[, c("Outcome_phecode", "phenotype")], 
            by = "Outcome_phecode") %>%
  mutate(phenotype = case_when(Outcome_phecode == "COVID_hspt" ~ "COVID-19 hospitalisation",
                               Outcome_phecode == "death" ~ "Death",
                               Outcome_phecode == "Avg" ~ paste0("Average across ", n_outcome_ds, " outcomes"),
                               .default = phenotype)) %>%
  mutate(phenotype = factor(phenotype, levels = selected_outcome_phenotype)) %>%
  mutate(N_predictors = as.numeric(case_when(Model == "CRS_all_predictors" ~ 212,
                                  Model == "CRS_top100" ~ 100,
                                  Model == "CRS_top50" ~ 50,
                                  Model == "CRS_top20" ~ 20,
                                  Model == "CRS_top10" ~ 10, 
                                  Model == "Age_Sex" ~ 0))) %>%
  mutate(alpha_category = factor(if_else(Outcome_phecode %in% c("Avg", "COVID_hspt"), 
                                         "Full", "Transparent"),
                                 levels = c("Full", "Transparent")))

##### Make plot #####
dodge <- position_dodge(0.8)
ref_linewidth <- 0.6
metric_lower_lim <- 0
metric_upper_lim <- 0.25
metric_breaks <- 0.05
point_size = 4
base_size = base_font_size
color_values <- c("black", "red", "blue","orange","magenta","green", "cyan")
cb_color_values <- c(blue, red, purple, orange, green, "black", skyblue)
error_bar_width <- 0.01
save_format <- "pdf"

Predictor_saturation_plot <- ggplot(test_R2_vs_model_dt, 
                      aes(x = N_predictors, 
                          y = Delta_R2_liab_mean, 
                          group = phenotype
                        #  alpha = alpha_category
                        )) +
  geom_point(aes(color = phenotype 
                 #alpha = alpha_category
                 ), size = point_size) + 
 # geom_errorbar(aes(ymin = Delta_R2_liab_LB, ymax = Delta_R2_liab_UB, color = phenotype)) + 
  geom_line(aes(color = phenotype 
                #alpha = alpha_category
                )) + 
  scale_y_continuous(limits = c(metric_lower_lim, metric_upper_lim), 
                     breaks = seq(metric_lower_lim, metric_upper_lim, 
                                  metric_breaks)) +
  scale_color_manual(limits = levels(test_R2_vs_model_dt$phenotype),
                     values = cb_color_values) +
 # scale_alpha_manual(values = c(1, 0.7)) + 
  theme_bw(base_size = base_font_size) +
  theme( 
    legend.position="right",
  #  panel.border = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    legend.key.spacing.y = unit(0.3, 'cm')
  ) + 
 # guides(alpha = "none") + 
  labs(x = "Number of predictor diseases", 
       y = expression(Delta ~ Liability ~ R^2 ~ ("CRS vs. Age + Sex")), 
       color = 'Outcome')

##### Save plot #####
ggsave(filename = paste0(figure_dir, "/General_performance/Panel3.", save_format), 
       Predictor_saturation_plot, width = 12.46, height = 6.5, limitsize = FALSE)


#### Fig 1d: CRS effect sizes of full model vs. top 100 predictor model ####
crs_effec_size_compare <- crs_effec_size_by_model %>%
  filter(Comparison == "CRS_top100")

##### Plot function #####
crs_effec_size_compare_plot <- function(crs_effec_size_compare,
                                        dodge = position_dodge(0.8),
                                        ref_linewidth = 0.6,
                                        metric_lower_lim = -1.5,
                                        metric_upper_lim = 2,
                                        metric_breaks = 1,
                                        point_size = 4,
                                        base_size = base_font_size,
                                        error_bar_width = 0.01,
                                        model_fit = F,
                                        color_by_incid = F,
                                        text_label_size = 5,
                                        point_padding = 1,
                                        box_padding = 0.5,
                                        nudge_x = 0,
                                        nudge_y = 0,
                                        seed = 100,
                                        save_format = "pdf",
                                        fig_width = 7,
                                        fig_height = 6) {
  plot_effec_size_compare <- ggplot(crs_effec_size_compare, 
                       aes(x = Est, 
                           y = Est_full)) + 
    geom_abline(slope = 1, intercept = 0, 
                linewidth = ref_linewidth) +
    geom_hline(yintercept = 0, linetype = "dashed") + 
    geom_vline(xintercept = 0, linetype = "dashed") + 
    geom_point(size = point_size,
               color = grey) +
   # scale_color_manual(values = c("1" = red, "0" = grey)) +
    #      geom_errorbar(aes(ymin = test_R2_LB, ymax = test_R2_UB, color = Incidence_group), 
    #                    width = error_bar_width) +
    #      geom_errorbarh(aes(xmin = train_R2_LB, xmax = train_R2_UB, color = Incidence_group), 
    #                     height = error_bar_width) +
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
    labs(x = "CRS effect size (top 100 predictors)", 
         y = "CRS effect size (full model)")
  
  ggsave(filename = paste0(figure_dir, "/General_performance/Panel4_CRS_effect_size_compare.", save_format), 
         plot_effec_size_compare, width = fig_width, height = fig_height, limitsize = FALSE)
}

##### Make plot #####
crs_effec_size_compare_plot(crs_effec_size_compare,
                                        dodge = position_dodge(0.8),
                                        ref_linewidth = 0.6,
                                        metric_lower_lim = -1.5,
                                        metric_upper_lim = 2.5,
                                        metric_breaks = 1,
                                        save_format = "pdf",
                                        fig_width = 7,
                                        fig_height = 6)

cor.test(crs_effec_size_compare$Est,
         crs_effec_size_compare$Est_full)
# 0.9775853


#### Fig. S4: Performance for CRS vs. elastic net ####
## Combine performance metrics for CRS and elastic net
metrics_crs <- crs_enet_summ %>%
  filter(Model == "CRS_all_predictors") %>%
  filter(Outcome_phecode != "Avg") %>%
  select(Outcome_phecode, 
         AUROC_mean,
         C_index_mean,
         R2_liab_mean,
         R2_obs_mean,
         AUPRC_prev_diff_mean) %>%
  rename(AUROC_mean_crs = AUROC_mean,
         C_index_mean_crs = C_index_mean,
         R2_liab_mean_crs = R2_liab_mean,
         R2_obs_mean_crs = R2_obs_mean,
         AUPRC_prev_diff_mean_crs = AUPRC_prev_diff_mean)

metrics_enet <- crs_enet_summ %>%
  filter(Model == "CRS_ENet") %>%
  filter(Outcome_phecode != "Avg") %>%
  select(Outcome_phecode, 
         AUROC_mean,
         C_index_mean,
         R2_liab_mean,
         R2_obs_mean,
         AUPRC_prev_diff_mean) %>%
  rename(AUROC_mean_enet = AUROC_mean,
         C_index_mean_enet = C_index_mean,
         R2_liab_mean_enet = R2_liab_mean,
         R2_obs_mean_enet = R2_obs_mean,
         AUPRC_prev_diff_mean_enet = AUPRC_prev_diff_mean)

metrics_summ_crs_enet <- metrics_crs %>%
  left_join(metrics_enet, by = "Outcome_phecode") %>%
  left_join(phecode_info[, c("Outcome_phecode", "phenotype")], 
            by = "Outcome_phecode") 

###### Plot function ######
selected_outcome_ds <- c(
  "165_1",
  "274_1",
  "411_2",
  "994_2")
selected_outcome_phenotype <- c(
  "Cancer of bronchus; lung",
  "Gout",
  "Myocardial infarction",
  "Sepsis")

crs_vs_enet_plot <- function(target_metric_summ_crs_enet,
                             target_metric = "AUROC",
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
  crs_vs_enet_plot <- ggplot(target_metric_summ_crs_enet, 
                           aes(x = target_metric_crs, 
                               y = target_metric_enet)) +
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
    labs(x = paste0(target_metric, " (logistic regression)"), 
         y = paste0(target_metric, " (elastic net)"))
  
  if (target_metric == "R2_liab") {
    crs_vs_enet_plot <- crs_vs_enet_plot + 
      labs(x = expression(Liability ~ R^2 ~ "(logistic regression)"), 
           y = expression(Liability ~ R^2 ~ "(elastic net)"))
  } else if (target_metric == "R2_obs") {
    crs_vs_enet_plot <- crs_vs_enet_plot + 
      labs(x = expression(Observed ~ R^2 ~ "(logistic regression)"), 
           y = expression(Observed ~ R^2 ~ "(elastic net)"))
  } else if (target_metric == "AUPRC_prev_diff") {
    crs_vs_enet_plot <- crs_vs_enet_plot + 
      labs(x = "AUPRC - incidence (logistic regression)", 
           y = "AUPRC - incidence (elastic net)")
  }
  
  if (label_top_N == T) {
    
    selected_outcome_ds_final <- selected_outcome_ds
    top_N_idx <- which(target_metric_summ_crs_enet$Outcome_phecode %in% selected_outcome_ds_final)
    all_predictor_dt_label <- target_metric_summ_crs_enet[top_N_idx,]
    
    
    if (target_metric %in% c("AUROC", "C-index")) {
      nudge_x <- case_when(all_predictor_dt_label$phenotype == "Sepsis" ~ nudge_x + 0.05 ,
                           all_predictor_dt_label$phenotype == "Cancer of bronchus; lung" ~ nudge_x - 0.15,
                           all_predictor_dt_label$phenotype == "Gout" ~ nudge_x - 0.03,
                           all_predictor_dt_label$phenotype == "Myocardial infarction" ~ nudge_x + 0.08,
                           .default = nudge_x)
      nudge_y = case_when(all_predictor_dt_label$phenotype == "Sepsis" ~ nudge_y ,
                          all_predictor_dt_label$phenotype == "Cancer of bronchus; lung" ~ nudge_y,
                          all_predictor_dt_label$phenotype == "Gout" ~ nudge_y,
                          all_predictor_dt_label$phenotype == "Myocardial infarction" ~ nudge_y - 0.03,
                          .default = nudge_y)
    } else if (target_metric %in% c("R2_liab")) {
      nudge_x <- case_when(all_predictor_dt_label$phenotype == "Sepsis" ~ nudge_x - 0.03,
                           all_predictor_dt_label$phenotype == "Cancer of bronchus; lung" ~ nudge_x - 0.08,
                           all_predictor_dt_label$phenotype == "Gout" ~ nudge_x - 0.03,
                           all_predictor_dt_label$phenotype == "Myocardial infarction" ~ nudge_x + 0.05,
                           .default = nudge_x)
      nudge_y = case_when(all_predictor_dt_label$phenotype == "Sepsis" ~ nudge_y ,
                          all_predictor_dt_label$phenotype == "Cancer of bronchus; lung" ~ nudge_y - 0.02,
                          all_predictor_dt_label$phenotype == "Gout" ~ nudge_y,
                          all_predictor_dt_label$phenotype == "Myocardial infarction" ~ nudge_y - 0.03,
                          .default = nudge_y)
    } else if (target_metric %in% c("R2_obs")) {
      nudge_x <- case_when(all_predictor_dt_label$phenotype == "Sepsis" ~ nudge_x - 0.03,
                           all_predictor_dt_label$phenotype == "Cancer of bronchus; lung" ~ nudge_x - 0.08,
                           all_predictor_dt_label$phenotype == "Gout" ~ nudge_x + 0.02,
                           all_predictor_dt_label$phenotype == "Myocardial infarction" ~ nudge_x + 0.05,
                           .default = nudge_x)
      nudge_y = case_when(all_predictor_dt_label$phenotype == "Sepsis" ~ nudge_y ,
                          all_predictor_dt_label$phenotype == "Cancer of bronchus; lung" ~ nudge_y,
                          all_predictor_dt_label$phenotype == "Gout" ~ nudge_y - 0.03,
                          all_predictor_dt_label$phenotype == "Myocardial infarction" ~ nudge_y,
                          .default = nudge_y)
    } else if (target_metric %in% c("AUPRC_prev_diff")) {
      nudge_x <- case_when(all_predictor_dt_label$phenotype == "Sepsis" ~ nudge_x - 0.03,
                           all_predictor_dt_label$phenotype == "Cancer of bronchus; lung" ~ nudge_x,
                           all_predictor_dt_label$phenotype == "Gout" ~ nudge_x + 0.01,
                           all_predictor_dt_label$phenotype == "Myocardial infarction" ~ nudge_x + 0.05,
                           .default = nudge_x)
      nudge_y = case_when(all_predictor_dt_label$phenotype == "Sepsis" ~ nudge_y ,
                          all_predictor_dt_label$phenotype == "Cancer of bronchus; lung" ~ nudge_y + 0.02,
                          all_predictor_dt_label$phenotype == "Gout" ~ nudge_y - 0.01,
                          all_predictor_dt_label$phenotype == "Myocardial infarction" ~ nudge_y,
                          .default = nudge_y)
    }
    
    crs_vs_enet_plot <- crs_vs_enet_plot + 
      geom_point(data = target_metric_summ_crs_enet[top_N_idx,], 
                 color = red,
                 size = point_size) + 
      geom_label_repel(data = target_metric_summ_crs_enet[top_N_idx,], 
                       aes(label = phenotype, 
                           x = target_metric_crs, 
                           y = target_metric_enet),
                       size = text_label_size,
                       point.padding = point_padding,
                       box.padding = box_padding,
                       nudge_x = nudge_x,
                       nudge_y = nudge_y,
                       seed = seed)
    
  }
  
  ggsave(filename = paste0(figure_dir, "General_performance/CRS_vs_Enet_", 
                           target_metric,".", save_format), 
         crs_vs_enet_plot, 
         width = fig_width, 
         height = fig_height, 
         limitsize = FALSE)
}

##### Make plot #####
## AUROC 
target_metric_summ_crs_enet <- metrics_summ_crs_enet %>%
  rename(target_metric_crs = AUROC_mean_crs,
         target_metric_enet = AUROC_mean_enet)

crs_vs_enet_plot(target_metric_summ_crs_enet,
                             target_metric = "AUROC",
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
                             fig_height = 6)

## C-index
target_metric_summ_crs_enet <- metrics_summ_crs_enet %>%
  rename(target_metric_crs = C_index_mean_crs,
         target_metric_enet = C_index_mean_enet)

crs_vs_enet_plot(target_metric_summ_crs_enet,
                 target_metric = "C-index",
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
                 fig_height = 6)

## Liab R2
target_metric_summ_crs_enet <- metrics_summ_crs_enet %>%
  rename(target_metric_crs = R2_liab_mean_crs,
         target_metric_enet = R2_liab_mean_enet)

crs_vs_enet_plot(target_metric_summ_crs_enet,
                 target_metric = "R2_liab",
                 dodge = position_dodge(0.8),
                 ref_linewidth = 0.6,
                 metric_lower_lim = 0,
                 metric_upper_lim = 0.25,
                 metric_breaks = 0.05,
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
                 fig_height = 6)

## Observed R2
target_metric_summ_crs_enet <- metrics_summ_crs_enet %>%
  rename(target_metric_crs = R2_obs_mean_crs,
         target_metric_enet = R2_obs_mean_enet)

crs_vs_enet_plot(target_metric_summ_crs_enet,
                 target_metric = "R2_obs",
                 dodge = position_dodge(0.8),
                 ref_linewidth = 0.6,
                 metric_lower_lim = 0,
                 metric_upper_lim = 0.2,
                 metric_breaks = 0.05,
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
                 fig_height = 6)

## AUPRC - prev
target_metric_summ_crs_enet <- metrics_summ_crs_enet %>%
  rename(target_metric_crs = AUPRC_prev_diff_mean_crs,
         target_metric_enet = AUPRC_prev_diff_mean_enet)

crs_vs_enet_plot(target_metric_summ_crs_enet,
                 target_metric = "AUPRC_prev_diff",
                 dodge = position_dodge(0.8),
                 ref_linewidth = 0.6,
                 metric_lower_lim = 0,
                 metric_upper_lim = 0.12,
                 metric_breaks = 0.03,
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
                 fig_height = 6)

#### Fig. S5: prevalence of predictors ####
## Read prevalence for all parent level phecodes
pheparent_prev <- read.csv(paste0(read_dir, "All_pts_Index2015_PheParent_Prevalence.csv"))
pheparent_prev_400 <- pheparent_prev %>%
  arrange(desc(prevalence)) %>%
  mutate(rank = row_number()) %>%
  filter(rank <= 400) 

pheparent_prev_400_by_group <- pheparent_prev_400 %>%
  mutate(rank_group = case_when(rank <= 10 ~ "1-10",
                                rank > 10 & rank <= 20 ~ "11-20",
                                rank > 20 & rank <= 50 ~ "21-50",
                                rank > 50 & rank <= 100 ~ "51-100",
                                rank > 100 & rank <= 212 ~ "101-212",
                                rank > 212 ~ "213-400")) %>%
  group_by(rank_group) %>%
  mutate(group_mean_prev = mean(prevalence),
         group_mean_prev_SE = sd(prevalence)/sqrt(n())) %>%
  select(rank_group, group_mean_prev, group_mean_prev_SE) %>%
  distinct() %>%
  mutate(rank_group = factor(rank_group, levels = c("1-10", "11-20", 
                                                    "21-50", "51-100",
                                                    "101-212", "213-400"))) %>%
  mutate(group_mean_prev_perc = group_mean_prev * 100,
         group_mean_prev_SE_perc = group_mean_prev_SE * 100)

pheparent_prev_400_by_group %>%
  filter(rank_group == "213-400")
#   rank_group group_mean_prev group_mean_prev_SE group_mean_prev_perc
#   213-400           0.000847          0.0000420               0.0847

##### Plot function #####
predictor_prev_plot <- function(pheparent_prev_400_by_group,
                                              metric_lower_lim = 0,
                                              metric_upper_lim = 10,
                                              metric_breaks = 2,
                                              point_size = base_point_size,
                                              base_size = base_font_size,
                                              add_error_bar = T,
                                              error_bar_width = 0.3,
                                              figure_width = 10,
                                              figure_height = 6,
                                              save_format = "pdf") {
  
  predictor_prev_barplot <- ggplot(pheparent_prev_400_by_group, 
                                         aes(x = fct_rev(rank_group), 
                                             y = group_mean_prev_perc)) + 
    geom_col(fill = grey) +
    scale_y_continuous(limits = c(metric_lower_lim, metric_upper_lim), 
                       breaks = seq(metric_lower_lim, metric_upper_lim, metric_breaks)) +
    coord_flip() + 
    theme_bw(base_size = base_size) +
    theme( 
      # legend.position="top",
      panel.border = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_blank(),
      legend.key.spacing.y = unit(0.3, 'cm')
    ) + 
    labs(x = "Prevalence ranking", 
         y = "Parent-level phecode prevalence (%)")
  
  if (add_error_bar == T) {
    predictor_prev_barplot <- predictor_prev_barplot + geom_errorbar(aes(x = fct_rev(rank_group), 
                                                                                     ymin = group_mean_prev_perc - group_mean_prev_SE_perc, 
                                                                                     ymax = group_mean_prev_perc + group_mean_prev_SE_perc),
                                                                                 # position = dodge, 
                                                                                 width = error_bar_width)
  }
  
  ggsave(filename = paste0(figure_dir, "General_performance/SuppFig5_predictor_prevalence.pdf"), 
         predictor_prev_barplot, 
         width = figure_width, height = figure_height, limitsize = FALSE)

  
}

##### Make plot #####
predictor_prev_plot(pheparent_prev_400_by_group,
                                metric_lower_lim = 0,
                                metric_upper_lim = 10,
                                metric_breaks = 2,
                                point_size = base_point_size,
                                base_size = base_font_size,
                                add_error_bar = T,
                                error_bar_width = 0.3,
                                figure_width = 10,
                                figure_height = 6,
                                save_format = "pdf")

