### Author: Pearl Liu
### Plot effect sizes and comorbidity architecture ###
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
library(corrplot)
library(cowplot)

#### Set parameters ####
read_dir <- 'Outputs/'
summary_dir <- 'Summary_Stats/'
figure_dir <- 'Figures_Manuscript/'
file_date <- '2025_05_01'

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

#phecode_cat_color_vec <- c("red", "cyan", "tomato", "yellow2",
#  "green2", "deeppink", "orange", "purple",
#  "deepskyblue", "brown", "violet",
#  "seagreen1", "blue", "gold",
#  "grey40")

phecode_cat_color_vec <- c(red, "#88CCEE", "#882255", yellow,
                           green, "#CC6677", orange, purple,
                           blue, "#44AA99", "#AA4499", skyblue,
                           "#661100", "#999933",
                           grey)

predictor_category_vec <- c("Infectious diseases", "Neoplasms", "Endocrine/metabolic",  
                            "Hematopoietic", "Mental disorders", "Neurological",         
                            "Sense organs", "Circulatory system", "Respiratory",          
                            "Digestive", "Genitourinary",  "Dermatologic",         
                            "Musculoskeletal", "Symptoms", "Injuries & poisonings")

phecode_category_color <- data.frame(
  Phecode_Category = predictor_category_vec,
  Color_values = phecode_cat_color_vec)

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
n_mc <- 100  ## PL 20260120: update from 200 to 100
## Control-case ratio in testing data for evaluating AUROC, C-index, R2
control_case_ratio_test <- 4
## Number of cases used in testing data
n_cases_test1 <- 3000
n_cases_test2 <- 1000

## Number of bootstrap/jackknife samples to get 95% CI for Me - NOT USED
n_boot <- 1000
n_jack <- 212

#### Read processed data ####
##### List and prevalence of predictors and outcomes #####
pred_ds_prev2015_name = "WhiteBritish_OddYOB_Index2015_PredictorDiseasePrevalence.csv"
pred_ds_prev2020_name = "WhiteBritish_OddYOB_Index2020_PredictorDiseasePrevalence.csv"
outcome_ds_prev_name = "TrainData_Outcome_Incidence_OddYOB_Index2015_Followup10yr.csv"

pred_ds_prev2015 = read.csv(paste0(read_dir, pred_ds_prev2015_name)) %>%
  mutate(Predictor_name = paste0('ds', Predictor_phecode))
pred_ds_prev2020 = read.csv(paste0(read_dir, pred_ds_prev2020_name)) %>%
  mutate(Predictor_name = paste0('ds', Predictor_phecode))

## Combine 2015 predictor prevalence (all other outcomes) with 2020 predictor prevlence (covid outcome)
all(pred_ds_prev2015$Predictor_name == pred_ds_prev2020$Predictor_name)
pred_ds_prev <- pred_ds_prev2015 %>%
  mutate(Prevalence2020 = pred_ds_prev2020$Prevalence)

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
all_outcome_summ_coef <- readRDS(paste0(summary_dir,
                                       "All_Outcomes+COVID_WeightedLogisticRegression_SummStats_ControlCase4to1_WhiteBritish_OddYOB_Index2015_Followup10yr.rds"))

final_summ_stats_wb <- readRDS(paste0(summary_dir, 
                                      "Sex_Specific_All_Outcomes+COVID_PredictionMetrics_ControlCase",
                                      control_case_ratio, "to1_WhiteBritish_Index2015_Followup10yr.rds"))
final_summ_stats_asian <- readRDS(paste0(summary_dir, 
                                         "Sex_Specific_All_Outcomes+COVID_PredictionMetrics_ControlCase",
                                         control_case_ratio, "to1_Asian_Index2015_Followup10yr.rds"))
final_summ_stats_black <- readRDS(paste0(summary_dir, 
                                         "Sex_Specific_All_Outcomes+COVID_PredictionMetrics_ControlCase",
                                         control_case_ratio, "to1_Black_Index2015_Followup10yr.rds"))
final_summ_stats_mixed <- readRDS(paste0(summary_dir, 
                                         "Sex_Specific_All_Outcomes+COVID_PredictionMetrics_ControlCase",
                                         control_case_ratio, "to1_Mixed_Index2015_Followup10yr.rds"))

##### Get predictor disease effect sizes, calculate p-values #####
all_outcome_summ_coef_pred_ds <- all_outcome_summ_coef %>%
  filter(Outcome != "622") %>%
  filter(startsWith(Predictor, "ds")) %>%
  ## Use prevalence by 2020 for COVID outcome
  mutate(Est_scaleX = if_else(Outcome != "COVID_hspt", 
                              Est * sqrt(Predictor_prev * (1 - Predictor_prev)),
                              Est * sqrt(Predictor_prev2020 * (1 - Predictor_prev2020)))) %>%
  mutate(Pval = 2 * pnorm(abs(Est/SE), lower.tail = F)) %>%
  mutate(Pval_fdr = p.adjust(Pval, method = 'fdr')) %>%
  rename(Outcome_phecode = Outcome) %>%
  ## Convert first letter to capital letter in phecode category
  mutate(Predictor_category = str_to_sentence(Predictor_category))

all_outcome_intercept <- all_outcome_summ_coef %>%
  filter(Outcome != "622") %>%
  filter(Predictor == "(Intercept)")

#### Functions ####
Me_compute_center_debias2 <- function(effec_size_vec) {
  # numerator of kappa (normalized kurtosis)
  k_num <- mean((effec_size_vec - mean(effec_size_vec))^4)
  # denominator of kappa (normalized kurtosis)
  k_denom <- (mean((effec_size_vec - mean(effec_size_vec))^2))^2
  k <- k_num/k_denom
  M <- length(effec_size_vec)
  ## unbiased estimator of kurtosis
  # References: https://uk.mathworks.com/help/stats/kurtosis.html#description, https://www.jstor.org/stable/2988433?seq=2 
  k1 <- (M-1) / ( (M-2) * (M-3) ) * ( (M+1) * k - 3 * (M-1) ) + 3
  
  ## Further debias 1/k1
  # Reference: Eq 3.8 from https://www.sciencedirect.com/science/article/pii/016771528790040X
  mu1 <- sample_central_moment(effec_size_vec, 1) 
  mu2 <- sample_central_moment(effec_size_vec, 2) 
  mu3 <- sample_central_moment(effec_size_vec, 3) 
  mu4 <- sample_central_moment(effec_size_vec, 4) 
  mu5 <- sample_central_moment(effec_size_vec, 5)
  mu6 <- sample_central_moment(effec_size_vec, 6)
  mu8 <- sample_central_moment(effec_size_vec, 8)
  
  # Get variance of k
  M_var_k <- 1/mu2^6 * (mu8*mu2^2 - mu4^2*mu2^2 - 8*mu5*mu3*mu2^2 + 
                           16*mu2^3*mu3^2 - 4*mu4*mu6*mu2 - 32*mu2^2*mu1*mu3*mu4 +
                           8*mu5*mu1*mu2*mu4 + 16*mu3^2*mu2*mu4 + 4*mu4^3 +
                           16*mu2*mu1^2*mu4^2 - 16*mu1*mu3*mu4^2)
  
  Me <- 3*M/k1 - 3 * ((M-1)^2 * (M+1)^2) / ((M-2)^2 * (M-3)^2) * M_var_k/k1^3
  
  Me_M_ratio <- Me/M
  
  ## Get asymptotic sd of Me estimate using Delta method
  ## PL 20251213 update: update 1/k^4 to 1/k1^4
  k1_inv_var <- ((M-1)^2 * (M+1)^2) / ((M-2)^2 * (M-3)^2) * M_var_k/M * 1/k1^4
  Me_sd <- 3 * M * sqrt(k1_inv_var)
  
  return(list(Me = Me, Me_M_ratio = Me_M_ratio, k = k1,
              Me_sd = Me_sd))
}

sample_central_moment <- function(x_vec, p) {
  return(mean((x_vec - mean(x_vec))^p))
}

## Using jackknife to estimate SD of an estimator
## Ref: https://projecteuclid.org/journals/annals-of-statistics/volume-9/issue-3/The-Jackknife-Estimate-of-Variance/10.1214/aos/1176345462.full
jackknife_sd <- function(jack_est_vec) {
  n <- length(jack_est_vec)
  jack_mean <- mean(jack_est_vec)
  jack_var <- (n-1)/n * sum( (jack_est_vec - jack_mean)^2 )
  jack_sd <- sqrt(jack_var)
  return( jack_sd )
}

#### Calculate Me statistics, get 95% CI via Delta Method ####
Me_df <- data.frame(
  "Outcome_phecode" = outcome_ds_prev$Outcome_phecode,
  "Me" = rep(NA, n_outcome_ds),
  "Me_sd" = rep(NA, n_outcome_ds),
  "Me_perc" = rep(NA, n_outcome_ds),
  "N_pred_ds" = rep(NA, n_outcome_ds)
)

for (i in 1:n_outcome_ds) {
  target_ds <- outcome_ds_prev$Outcome_phecode[i]
  effec_size_vec_i <- all_outcome_summ_coef_pred_ds %>%
    filter(Outcome_phecode == target_ds) %>%
    pull(Est_scaleX)
  n_pred_i <- length(effec_size_vec_i)
  Me_df$Me[i] <- Me_compute_center_debias2(effec_size_vec_i)$Me
  Me_df$Me_perc[i] <- Me_df$Me[i]/n_pred_i * 100
  Me_df$N_pred_ds[i] <- n_pred_i
  ## Get analytical SD via Delta Method
  Me_df$Me_sd[i] <- Me_compute_center_debias2(effec_size_vec_i)$Me_sd
}

Avg_Me_df <- data.frame("Outcome_phecode" = "Avg",
                        "Me" = mean(Me_df$Me),
                        "Me_sd" = sd(Me_df$Me)/sqrt(n_outcome_ds),
                        "Me_perc" = mean(Me_df$Me_perc),
                        "N_pred_ds" = mean(Me_df$N_pred_ds),
                        "phenotype" = paste0("Average across ", n_outcome_ds, " outcomes"))

Me_df_phenotype <- Me_df %>%
  left_join(phecode_info[, c("Outcome_phecode", "phenotype")], 
            by = "Outcome_phecode") %>%
  rbind(Avg_Me_df) %>%
  mutate(phenotype = case_when(Outcome_phecode == "COVID_hspt" ~ "COVID-19 hospitalisation",
                               Outcome_phecode == "death" ~ "Death",
                             .default = phenotype)) %>%
  mutate(Me_LB = pmax(0, Me - qnorm(0.975) * Me_sd),
         Me_UB = pmin(N_pred_ds, Me + qnorm(0.975) * Me_sd) ) %>%
  select(Outcome_phecode, 
         phenotype,
         Me, Me_LB, Me_UB, 
         Me_perc,
         Me_sd,
         N_pred_ds)

## save file
saveRDS(Me_df_phenotype, paste0(summary_dir, "All_outcomes_Me_estimates.rds"))

##### Read saved Me data #####
Me_df_phenotype <- readRDS(paste0(summary_dir, "All_outcomes_Me_estimates.rds"))

##### Join with performance metrics, filter outcomes with good performance metrics #####
Delta_metrics_dt <- final_summ_stats_wb$all_outcome_metrics_dt %>%
  filter(Model == "CRS_all_predictors") %>%
  select(Outcome_phecode, Delta_R2_liab_mean, Delta_AUROC_mean, Delta_C_index_mean)

Me_df_phenotype_metrics <- Me_df_phenotype %>%
  left_join(Delta_metrics_dt, by = "Outcome_phecode")

delta_R2_q75 <- Me_df_phenotype_metrics %>% 
  filter(Outcome_phecode != "Avg") %>%
  pull(Delta_R2_liab_mean) %>%
  quantile(0.75)
delta_AUROC_q75 <- Me_df_phenotype_metrics %>% 
  filter(Outcome_phecode != "Avg") %>%
  pull(Delta_AUROC_mean) %>%
  quantile(0.75)
delta_C_index_q75 <- Me_df_phenotype_metrics %>% 
  filter(Outcome_phecode != "Avg") %>%
  pull(Delta_C_index_mean) %>%
  quantile(0.75)

## Filter to outcomes with R2_liab, AUROC, or C-index >= 75% quantile
Me_df_phenotype_good_metrics <- Me_df_phenotype_metrics %>%
  filter(Outcome_phecode != "Avg") %>%
  filter(Delta_R2_liab_mean >= delta_R2_q75 | 
           Delta_AUROC_mean >= delta_AUROC_q75 |
           Delta_C_index_mean >= delta_C_index_q75)

## Check for top and bottom 5 Me
View(Me_df_phenotype_good_metrics)

#### Fig. 4a: Comorbidity architecture: Barplot of Me for selected outcomes ####
##### Filter to selected outcomes #####
## Avg, COVID hspt, bottom 5 Me, top 5 Me of outcomes with good performance metrics
## Include key example outcomes
selected_small_Me_ds <- c("274_1", "250_2", "165_1", "480_11", "411_2")
selected_large_Me_ds <- c("death", "994_2", "550_5", "835", "745")
selected_outcome_ds <- c("Avg", "COVID_hspt",
                         selected_small_Me_ds,
                         selected_large_Me_ds)

selected_outcome_phenotype <- Me_df_phenotype %>%
  filter(Outcome_phecode %in% selected_outcome_ds) %>%
  arrange(factor(Outcome_phecode, levels = selected_outcome_ds)) %>%
  pull(phenotype)

selected_Me_df_phenotype <- Me_df_phenotype %>%
  filter(Outcome_phecode %in% selected_outcome_ds) %>%
  mutate(phenotype = factor(phenotype, levels = selected_outcome_phenotype)) %>%
  ## color outcomes differently
  mutate(color_group = case_when(Outcome_phecode == "Avg" ~ 1,
                                 Outcome_phecode == "COVID_hspt" ~ 2,
                                 Outcome_phecode %in% selected_small_Me_ds ~ 3,
                                 .default = 4)) %>%
  mutate(color_group = as.factor(color_group))

##### Plot function #####
Me_barplot <- function(selected_Me_df_phenotype,
                             ref_linewidth = 0.6,
                             metric_lower_lim = 0,
                             metric_upper_lim = 105,
                             metric_breaks = 30,
                       y_label = "",
                             point_size = base_point_size,
                             base_size = base_font_size,
                             bar_color = grey,
                             color_by_category = T,
                             add_error_bar = T,
                             error_bar_width = 0.3,
                             fig_width = 12.5,
                             fig_height = 6,
                       save_format = "pdf") {
  Me_plot <- ggplot(selected_Me_df_phenotype, 
                        aes(x = fct_rev(phenotype), 
                            y = Me)) + 
    geom_col(fill = bar_color) + 
    scale_y_continuous(limits = c(metric_lower_lim, metric_upper_lim), 
                       breaks = seq(metric_lower_lim, metric_upper_lim, metric_breaks)) +
    coord_flip() + 
    theme_bw(base_size = base_size) +
    theme( 
      legend.position="top",
      panel.border = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_blank()
    ) + 
    labs(x = "", y = y_label)
  
  if (color_by_category == T) {
    Me_plot <- Me_plot + 
      geom_col(aes(fill = color_group)) +
      scale_fill_manual(
        limits = levels(selected_Me_df_phenotype$color_group),
        values = c(grey, red, orange, blue)) +
      guides(fill = "none") 
  }
  
  if (add_error_bar == T) {
    Me_plot <- Me_plot + geom_errorbar(aes(x = fct_rev(phenotype), 
                                                   ymin = Me - Me_sd, ymax = Me + Me_sd),
                                               # position = dodge, 
                                               width = error_bar_width)
  }
  
  if (add_error_bar == T & color_by_category == T) {
    if (y_label == "") {
        figure_name <- paste0("/Comorbidity_architecture/Panel1_WithSE_Colored_plot_only.", save_format)
    } else {
      figure_name <- paste0("/Comorbidity_architecture/Panel1_WithSE_Colored_y_label.", save_format)
    }
  } else if (add_error_bar == T & color_by_category == F) {
    figure_name <- paste0("/Comorbidity_architecture/Panel1_WithSE_NoColor.", save_format)
  } else if (add_error_bar == F & color_by_category == F) {
    figure_name <- paste0("/Comorbidity_architecture/Panel1_NoSE_NoColor.", save_format)
  }
  
  ggsave(filename = paste0(figure_dir, figure_name), 
         Me_plot, width = fig_width, height = fig_height, limitsize = FALSE)
  
}

##### Make plot #####
## Colored 
# Plot only
Me_barplot(selected_Me_df_phenotype,
           ref_linewidth = 0.6,
           metric_lower_lim = 0,
           metric_upper_lim = 105,
           metric_breaks = 25,
           y_label = "",
           point_size = base_point_size,
           base_size = base_font_size,
           bar_color = grey,
           color_by_category = T,
           add_error_bar = T,
           error_bar_width = 0.3,
           fig_width = 10.83,
           fig_height = 5.4,
           save_format = "pdf")

## For extracting y label
Me_barplot(selected_Me_df_phenotype,
                 ref_linewidth = 0.6,
                 metric_lower_lim = 0,
                 metric_upper_lim = 105,
                 metric_breaks = 25,
           y_label = expression(widehat(M)[e] ~ ": effective number of independently predictive diseases"),
                 point_size = base_point_size,
                 base_size = base_font_size,
                 bar_color = grey,
                 color_by_category = T,
                 add_error_bar = T,
                 error_bar_width = 0.3,
                 fig_width = 13,
                 fig_height = 6,
           save_format = "pdf")

## No color
Me_barplot(selected_Me_df_phenotype,
                 ref_linewidth = 0.6,
                 metric_lower_lim = 0,
                 metric_upper_lim = 105,
                 metric_breaks = 30,
                 point_size = 4,
                 bar_color = grey,
                 color_by_category = F,
                 add_error_bar = T,
                 error_bar_width = 0.3)


#### Fig. 4b,d: Comorbidity architecture: sum(beta^2) vs. predictor disease category ####
##### Calculate sum(beta^2) per disease category per outcome #####
all_outcome_summ_coef_pred_ds_SumBeta <- all_outcome_summ_coef_pred_ds %>%
  group_by(Outcome_phecode, Predictor_category) %>%
  mutate(SumBetaSquared_perCategory_scaleX = sum(Est_scaleX^2),
         SumBetaSquared_perCategory = sum(Est^2)) %>%
  ungroup()

## Standardise by total sum(beta^2) per outcome
all_outcome_summ_coef_pred_ds_SumBeta <- all_outcome_summ_coef_pred_ds_SumBeta %>%
  group_by(Outcome_phecode) %>%
  mutate(SumBetaSquared_perOutcome_scaleX = sum(Est_scaleX^2),
         SumBetaSquared_perOutcome = sum(Est^2)) %>%
  ungroup() %>%
  mutate(SumBetaSquared_perCategory_scaleX_percOutcome = SumBetaSquared_perCategory_scaleX/SumBetaSquared_perOutcome_scaleX * 100,
         SumBetaSquared_perCategory_percOutcome = SumBetaSquared_perCategory/SumBetaSquared_perOutcome * 100)

## Get % sum(beta^2) per category averaged over all outcomes
avg_SumBeta_percOutcome <- all_outcome_summ_coef_pred_ds_SumBeta %>%
  select(Outcome_phecode, Predictor_category,
         SumBetaSquared_perCategory_scaleX_percOutcome,
         SumBetaSquared_perCategory_percOutcome) %>%
  distinct() %>%
  group_by(Predictor_category) %>%
  mutate(Avg_SumBetaSquared_perCategory_scaleX_percOutcome = 
           mean(SumBetaSquared_perCategory_scaleX_percOutcome),
         Avg_SumBetaSquared_perCategory_percOutcome = 
           mean(SumBetaSquared_perCategory_percOutcome),
         SE_SumBetaSquared_perCategory_scaleX_percOutcome = 
           sd(SumBetaSquared_perCategory_scaleX_percOutcome)/sqrt(n_outcome_ds),
         SE_SumBetaSquared_perCategory_percOutcome = 
           sd(SumBetaSquared_perCategory_percOutcome)/sqrt(n_outcome_ds)) %>%
  select(Predictor_category, 
         Avg_SumBetaSquared_perCategory_scaleX_percOutcome,
         Avg_SumBetaSquared_perCategory_percOutcome,
         SE_SumBetaSquared_perCategory_scaleX_percOutcome,
         SE_SumBetaSquared_perCategory_percOutcome) %>%
  distinct() %>%
  mutate(Outcome_phecode = "Avg") %>%
  select(Outcome_phecode, 
         Predictor_category,
         Avg_SumBetaSquared_perCategory_scaleX_percOutcome,
         Avg_SumBetaSquared_perCategory_percOutcome,
         SE_SumBetaSquared_perCategory_scaleX_percOutcome,
         SE_SumBetaSquared_perCategory_percOutcome) %>%
  rename(SumBetaSquared_perCategory_scaleX_percOutcome = 
           Avg_SumBetaSquared_perCategory_scaleX_percOutcome,
         SumBetaSquared_perCategory_percOutcome = 
           Avg_SumBetaSquared_perCategory_percOutcome)

## Get combined dataset
all_outcome_SumBeta_dt <- all_outcome_summ_coef_pred_ds_SumBeta %>%
  select(Outcome_phecode, Predictor_category,
         SumBetaSquared_perCategory_scaleX_percOutcome,
         SumBetaSquared_perCategory_percOutcome) %>%
  distinct() %>%
  rbind(select(avg_SumBeta_percOutcome,
               Outcome_phecode, 
               Predictor_category,
               SumBetaSquared_perCategory_scaleX_percOutcome,
               SumBetaSquared_perCategory_percOutcome)) %>%
  left_join(phecode_info[, c("Outcome_phecode", "phenotype")],
            by = "Outcome_phecode") %>%
  mutate(phenotype = case_when(Outcome_phecode == "COVID_hspt" ~ "COVID-19 hospitalisation",
                               Outcome_phecode == "death" ~ "Death",
                             .default = phenotype)) %>%
  rename(Outcome_phenotype = phenotype)

## Save file
saveRDS(all_outcome_SumBeta_dt, paste0(summary_dir, 
                                       "All_outcomes_R2_contribution_by_predictor_category.rds"))

###### Read saved file ######
all_outcome_SumBeta_dt <- readRDS(paste0(summary_dir, 
                                         "All_outcomes_R2_contribution_by_predictor_category.rds"))

##### Plot function #####
R2_contribution_avg_plot <- function(avg_SumBeta_percOutcome_plot,
                             target_ds_phecode,
                             metric_lower_lim = 0,
                             metric_upper_lim = 25,
                             metric_breaks = 5,
                             point_size = base_point_size,
                             base_size = base_font_size,
                             scaleX = T,
                             bar_color = grey,
                             color_by_category = T,
                             add_error_bar = T,
                             error_bar_width = 0.3,
                             save_format = "pdf",
                             fig_width = 12,
                             fig_height = 7.5) {
  
  if (scaleX == T) {
    pred_cat_order <- avg_SumBeta_percOutcome_plot %>%
      arrange(desc(SumBetaSquared_perCategory_scaleX_percOutcome)) %>%
      pull(Predictor_category)
    
    avg_SumBeta_percOutcome_plot <- avg_SumBeta_percOutcome_plot %>%
      mutate(Predictor_category = factor(Predictor_category, 
                                         levels = pred_cat_order)) %>%
      rename(SumBetaSquared = SumBetaSquared_perCategory_scaleX_percOutcome,
             SE_SumBetaSquared = SE_SumBetaSquared_perCategory_scaleX_percOutcome)
  } else {
    pred_cat_order <- avg_SumBeta_percOutcome_plot %>%
      arrange(desc(SumBetaSquared_perCategory_percOutcome)) %>%
      pull(Predictor_category)
    
    avg_SumBeta_percOutcome_plot <- avg_SumBeta_percOutcome_plot %>%
      mutate(Predictor_category = factor(Predictor_category, 
                                         levels = pred_cat_order)) %>%
      rename(SumBetaSquared = SumBetaSquared_perCategory_percOutcome,
             SE_SumBetaSquared = SE_SumBetaSquared_perCategory_percOutcome)
    
  }
  
  R2_contribution <- ggplot(avg_SumBeta_percOutcome_plot, 
                         aes(x = fct_rev(Predictor_category), 
                             y = SumBetaSquared)) + 
    geom_col(fill = bar_color) +
    scale_y_continuous(limits = c(metric_lower_lim, metric_upper_lim), 
                       breaks = seq(metric_lower_lim, metric_upper_lim, metric_breaks)) +
    coord_flip() + 
    theme_bw(base_size = base_size) +
    theme( 
     # legend.position="top",
      panel.border = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_blank()
    )  + 
    labs(x = "", y = expression(paste("Average percentage of ", R^2, " contribution")))

  
  if (color_by_category == T) {
    R2_contribution <- R2_contribution + geom_col(aes(fill = Predictor_category)) +
      scale_fill_manual(
        limits = avg_SumBeta_percOutcome_plot$Predictor_category,
        values = avg_SumBeta_percOutcome_plot$Color_values) +
      guides(fill = "none")
  }
  
  if (add_error_bar == T) {
    R2_contribution <- R2_contribution + geom_errorbar(aes(x = fct_rev(Predictor_category), 
                                                   ymin = SumBetaSquared - SE_SumBetaSquared, 
                                                   ymax = SumBetaSquared + SE_SumBetaSquared),
                                               # position = dodge, 
                                               width = error_bar_width)
  }
  
  if (scaleX == T) {
    if (add_error_bar == T & color_by_category == T) {
      figure_name <- paste0("/Comorbidity_architecture/Panel2_ScaleX_WithSE_ColorByCategory_", target_ds_phecode, ".", save_format)
    } else if (add_error_bar == T & color_by_category == F) {
      figure_name <- paste0("/Comorbidity_architecture/Panel2_ScaleX_WithSE_NoColor_", target_ds_phecode, ".", save_format)
    } else if (add_error_bar == F & color_by_category == F) {
      figure_name <- paste0("/Comorbidity_architecture/Panel2_ScaleX_NoSE_NoColor_", target_ds_phecode, ".", save_format)
    }
  } else {
    if (add_error_bar == T & color_by_category == T) {
      figure_name <- paste0("/Comorbidity_architecture/Panel2_RawEffectSizes_WithSE_ColorByCategory_", target_ds_phecode, ".", save_format)
    } else if (add_error_bar == T & color_by_category == F) {
      figure_name <- paste0("/Comorbidity_architecture/Panel2_RawEffectSizes_WithSE_NoColor_", target_ds_phecode, ".", save_format)
    } else if (add_error_bar == F & color_by_category == F) {
      figure_name <- paste0("/Comorbidity_architecture/Panel2_RawEffectSizes_NoSE_NoColor_", target_ds_phecode, ".", save_format)
    }
  }
  
  ggsave(filename = paste0(figure_dir, figure_name), 
         R2_contribution, width = fig_width, height = fig_height, 
         limitsize = FALSE)
  
}

R2_contribution_target_ds_plot <- function(all_outcome_SumBeta_dt,
                                       target_ds_phecode,
                                       pred_cat_order = NULL,
                                       metric_lower_lim = 0,
                                       metric_upper_lim = 25,
                                       metric_breaks = 10,
                                       point_size = base_point_size,
                                       base_size = base_font_size,
                                       scaleX = T,
                                       bar_color = grey,
                                       save_format = "pdf",
                                       figure_width = 7,
                                       figure_height = 7.5) {
  
  if (scaleX == T) {
    if (is.null(pred_cat_order)) {
      pred_cat_order <- all_outcome_SumBeta_dt %>%
        filter(Outcome_phecode == target_ds_phecode) %>% 
        arrange(desc(SumBetaSquared_perCategory_scaleX_percOutcome)) %>%
        pull(Predictor_category)
    }
    target_ds_SumBeta_dt <- all_outcome_SumBeta_dt %>%
      filter(Outcome_phecode == target_ds_phecode) %>%
      mutate(Predictor_category = factor(Predictor_category, 
                                         levels = pred_cat_order)) %>%
      rename(SumBetaSquared = SumBetaSquared_perCategory_scaleX_percOutcome)
  } else {
    if (is.null(pred_cat_order)) {
      pred_cat_order <- all_outcome_SumBeta_dt %>%
        filter(Outcome_phecode == target_ds_phecode) %>% 
        arrange(desc(SumBetaSquared_perCategory_percOutcome)) %>%
        pull(Predictor_category)
    }
    target_ds_SumBeta_dt <- all_outcome_SumBeta_dt %>%
      filter(Outcome_phecode == target_ds_phecode) %>%
      mutate(Predictor_category = factor(Predictor_category, 
                                         levels = pred_cat_order)) %>%
      rename(SumBetaSquared = SumBetaSquared_perCategory_percOutcome)
    
  }
  
  target_ds_pheno <- target_ds_SumBeta_dt$Outcome_phenotype %>% unique()
  
  R2_contribution <- ggplot(target_ds_SumBeta_dt, 
                        aes(x = fct_rev(Predictor_category), 
                            y = SumBetaSquared)) + 
    geom_col(fill = bar_color) +
    scale_y_continuous(limits = c(metric_lower_lim, metric_upper_lim), 
                       breaks = seq(metric_lower_lim, metric_upper_lim, metric_breaks)) +
    coord_flip() + 
    theme_bw(base_size = base_size) +
    theme( 
      # legend.position="top",
      panel.border = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_blank()
    ) + 
    labs(x = "", y = "", title = "")
  
  if (scaleX == T) {
    figure_name <- paste0("/Comorbidity_architecture/Panel2_ScaleX_",
                          target_ds_phecode, ".", save_format)
  } else {
    figure_name <- paste0("/Comorbidity_architecture/Panel2_RawEffectSizes_",
                          target_ds_phecode, ".", save_format)
  }
  
  ggsave(filename = paste0(figure_dir, figure_name), 
         R2_contribution, width = figure_width, height = figure_height, 
         limitsize = FALSE)
  
}


##### Fig. 4b: Plot for average cross outcomes #####
avg_SumBeta_percOutcome_plot <- avg_SumBeta_percOutcome %>%
  left_join(phecode_category_color, 
            by = join_by(Predictor_category == Phecode_Category)) %>%
  mutate(Predictor_category = if_else(Predictor_category == "Mental disorders",
                                      "Mental disorders and SU",
                                      Predictor_category))

R2_contribution_avg_plot(avg_SumBeta_percOutcome_plot,
                     target_ds_phecode = "Avg",
                     metric_lower_lim = 0,
                     metric_upper_lim = 20,
                     metric_breaks = 5,
                     scaleX = T,
                     bar_color = grey,
                     color_by_category = F,
                     add_error_bar = T,
                     error_bar_width = 0.3,
                     fig_width = 9.6,
                     fig_height = 6)

# R2_contribution_avg_plot(avg_SumBeta_percOutcome_plot,
#                      target_ds_phecode = "Avg",
#                      metric_lower_lim = 0,
#                      metric_upper_lim = 20,
#                      metric_breaks = 5,
#                      point_size = 4,
#                      scaleX = T,
#                      bar_color = "grey40",
#                      color_by_category = T,
#                      add_error_bar = T,
#                      error_bar_width = 0.3)


##### Fig. 4d: Plot for COVID hspt, death, lung cancer, MI, and T2D - individual barplot #####
pred_cat_order <- avg_SumBeta_percOutcome_plot %>%
     arrange(desc(SumBetaSquared_perCategory_scaleX_percOutcome)) %>%
     pull(Predictor_category)
metric_upper_lim <- 50
metric_breaks <- 20
figure_width <- 5.3
figure_height <- 6

all_outcome_SumBeta_dt <- all_outcome_SumBeta_dt %>%
  mutate(Predictor_category = if_else(Predictor_category == "Mental disorders",
                                      "Mental disorders and SU",
                                      Predictor_category))

R2_contribution_target_ds_plot(all_outcome_SumBeta_dt,
                                       target_ds_phecode = "COVID_hspt",
                                       pred_cat_order = pred_cat_order,
                                       metric_lower_lim = 0,
                                       metric_upper_lim = metric_upper_lim,
                                       metric_breaks = metric_breaks,
                                       scaleX = T,
                                       bar_color = red,
                                       figure_width = figure_width,
                                       figure_height = figure_height) 


R2_contribution_target_ds_plot(all_outcome_SumBeta_dt,
                           target_ds_phecode = "165_1",
                           pred_cat_order = pred_cat_order,
                           metric_lower_lim = 0,
                           metric_upper_lim = metric_upper_lim,
                           metric_breaks = metric_breaks,
                           scaleX = T,
                           bar_color = orange,
                           figure_width = figure_width,
                           figure_height = figure_height) 


R2_contribution_target_ds_plot(all_outcome_SumBeta_dt,
                               target_ds_phecode = "250_2",
                               pred_cat_order = pred_cat_order,
                               metric_lower_lim = 0,
                               metric_upper_lim = metric_upper_lim,
                               metric_breaks = metric_breaks,
                               scaleX = T,
                               bar_color = orange,
                               figure_width = figure_width,
                               figure_height = figure_height) 



R2_contribution_target_ds_plot(all_outcome_SumBeta_dt,
                           target_ds_phecode = "411_2",
                           pred_cat_order = pred_cat_order,
                           metric_lower_lim = 0,
                           metric_upper_lim = metric_upper_lim,
                           metric_breaks = metric_breaks,
                           scaleX = T,
                           bar_color = orange,
                           figure_width = figure_width,
                           figure_height = figure_height) 

R2_contribution_target_ds_plot(all_outcome_SumBeta_dt,
                               target_ds_phecode = "death",
                               pred_cat_order = pred_cat_order,
                               metric_lower_lim = 0,
                               metric_upper_lim = metric_upper_lim,
                               metric_breaks = metric_breaks,
                               scaleX = T,
                               bar_color = blue,
                               figure_width = figure_width,
                               figure_height = figure_height) 

R2_contribution_target_ds_plot(all_outcome_SumBeta_dt,
                           target_ds_phecode = "994_2",
                           pred_cat_order = pred_cat_order,
                           metric_lower_lim = 0,
                           metric_upper_lim = metric_upper_lim,
                           metric_breaks = metric_breaks,
                           scaleX = T,
                           bar_color = blue,
                           figure_width = figure_width,
                           figure_height = figure_height) 


#### Fig. 4c: Comorbidity architecture: Example predictors with high percentage of beta^2 ####
##### Get average percentage of beta^2 for each predictor #####
all_outcome_summ_coef_pred_ds_Beta2 <- all_outcome_summ_coef_pred_ds %>%
  ## Calculate beta^2 per predictor per outcome
  mutate(BetaSquared_scaleX = Est_scaleX^2,
         BetaSquared = Est^2) %>%
  ## Standardise by total sum(beta^2) per outcome
  group_by(Outcome_phecode) %>%
  mutate(SumBetaSquared_perOutcome_scaleX = sum(Est_scaleX^2),
         SumBetaSquared_perOutcome = sum(Est^2)) %>%
  ungroup() %>%
  mutate(BetaSquared_scaleX_percOutcome = BetaSquared_scaleX/SumBetaSquared_perOutcome_scaleX * 100,
         BetaSquared_percOutcome = BetaSquared/SumBetaSquared_perOutcome * 100)

## Get % beta^2 averaged over all outcomes
avg_Beta2_percOutcome <- all_outcome_summ_coef_pred_ds_Beta2 %>%
  select(Outcome_phecode, 
         Predictor, 
         Predictor_phenotype,
         Predictor_category,
         BetaSquared_scaleX_percOutcome,
         BetaSquared_percOutcome) %>%
  distinct() %>%
  group_by(Predictor) %>%
  mutate(Avg_BetaSquared_scaleX_percOutcome = 
           mean(BetaSquared_scaleX_percOutcome),
         Avg_BetaSquared_percOutcome = 
           mean(BetaSquared_percOutcome),
         SE_BetaSquared_scaleX_percOutcome = 
           sd(BetaSquared_scaleX_percOutcome)/sqrt(n_outcome_ds),
         SE_BetaSquared_percOutcome = 
           sd(BetaSquared_percOutcome)/sqrt(n_outcome_ds)) %>%
  select(Predictor,
         Predictor_phenotype,
         Predictor_category, 
         Avg_BetaSquared_scaleX_percOutcome,
         Avg_BetaSquared_percOutcome,
         SE_BetaSquared_scaleX_percOutcome,
         SE_BetaSquared_percOutcome) %>%
  distinct() %>%
  mutate(Outcome_phecode = "Avg") %>%
  select(Outcome_phecode, 
         Predictor,
         Predictor_phenotype,
         Predictor_category,
         Avg_BetaSquared_scaleX_percOutcome,
         Avg_BetaSquared_percOutcome,
         SE_BetaSquared_scaleX_percOutcome,
         SE_BetaSquared_percOutcome) %>%
  rename(BetaSquared_scaleX_percOutcome = 
           Avg_BetaSquared_scaleX_percOutcome,
         BetaSquared_percOutcome = 
           Avg_BetaSquared_percOutcome) %>%
  ungroup()

## Get combined dataset
all_outcome_Beta2_dt <- all_outcome_summ_coef_pred_ds_Beta2 %>%
  select(Outcome_phecode, 
         Predictor,
         Predictor_phenotype,
         Predictor_category,
         BetaSquared_scaleX_percOutcome,
         BetaSquared_percOutcome) %>%
  distinct() %>%
  rbind(select(avg_Beta2_percOutcome,
               Outcome_phecode, 
               Predictor,
               Predictor_phenotype,
               Predictor_category,
               BetaSquared_scaleX_percOutcome,
               BetaSquared_percOutcome))

## Save file
saveRDS(all_outcome_Beta2_dt, paste(summary_dir, "All_outcomes_R2_contribution_by_predictor.rds"))

###### Read saved file ######
all_outcome_Beta2_dt <- readRDS(paste(summary_dir, 
                                      "All_outcomes_R2_contribution_by_predictor.rds"))


##### Get final dataset for plotting - select top predictors with highest % of beta^2 averaged across all outcomes #####
phecode_category_color <- data.frame(
  Phecode_Category = predictor_category_vec,
  Color_values = phecode_cat_color_vec)

avg_Beta2_percOutcome <- avg_Beta2_percOutcome %>%
  mutate(Predictor_phenotype = case_when(Predictor_phenotype == "Other diseases of respiratory system, not elsewhere classified" ~
                                           "Other diseases of respiratory system", 
                                         Predictor_phenotype == "Overweight, obesity and other hyperalimentation" ~
                                           "Overweight and obesity",
                                         Predictor_phenotype == "Disorders of lipoid metabolism" ~ 
                                           "Disorders of lipid metabolism", 
                                         .default = Predictor_phenotype))

###### Top 10 predictors ######
avg_Beta2_percOutcome_plot <- avg_Beta2_percOutcome %>%
  slice_max(BetaSquared_scaleX_percOutcome, n = 10) %>%
  left_join(phecode_category_color, by = join_by(Predictor_category == Phecode_Category))

avg_Beta2_percOutcome_plot_sort <- avg_Beta2_percOutcome_plot %>%
  arrange(desc(BetaSquared_scaleX_percOutcome)) 

pred_ds_sort <- avg_Beta2_percOutcome_plot_sort %>%
  pull(Predictor_phenotype)

pred_cat_sort <- avg_Beta2_percOutcome_plot_sort %>%
  pull(Predictor_category) %>%
  unique()

pred_cat_color_sort <- avg_Beta2_percOutcome_plot_sort %>%
  pull(Color_values) %>%
  unique()

avg_Beta2_percOutcome_plot <- avg_Beta2_percOutcome_plot %>%
  mutate(Predictor_phenotype = factor(Predictor_phenotype,
                                      levels = pred_ds_sort)) %>%
  mutate(Predictor_category = factor(Predictor_category,
                                     levels = pred_cat_sort)) %>%
  mutate(Color_values = factor(Color_values,
                               levels = pred_cat_color_sort))


###### Top 15 predictors ######
avg_Beta2_percOutcome_plot2 <- avg_Beta2_percOutcome %>%
  slice_max(BetaSquared_scaleX_percOutcome, n = 15) %>%
  left_join(phecode_category_color, by = join_by(Predictor_category == Phecode_Category)) %>%
  mutate(Predictor_phenotype = if_else(Predictor == "ds626", 
                                       "Disorders of menstruation",
                                       Predictor_phenotype))

avg_Beta2_percOutcome_plot2_sort <- avg_Beta2_percOutcome_plot2 %>%
  arrange(desc(BetaSquared_scaleX_percOutcome)) 

pred_ds_sort <- avg_Beta2_percOutcome_plot2_sort %>%
  pull(Predictor_phenotype)

pred_cat_sort <- avg_Beta2_percOutcome_plot2_sort %>%
  pull(Predictor_category) %>%
  unique()

pred_cat_color_sort <- avg_Beta2_percOutcome_plot2_sort %>%
  pull(Color_values) %>%
  unique()

avg_Beta2_percOutcome_plot2 <- avg_Beta2_percOutcome_plot2 %>%
  mutate(Predictor_phenotype = factor(Predictor_phenotype,
                                      levels = pred_ds_sort)) %>%
  mutate(Predictor_category = factor(Predictor_category,
                                     levels = pred_cat_sort)) %>%
  mutate(Color_values = factor(Color_values,
                               levels = pred_cat_color_sort))


##### Plot function #####
R2_contribution_by_predictor_plot <- function(avg_Beta2_percOutcome_plot,
                             metric_lower_lim = 0,
                             metric_upper_lim = 8,
                             metric_breaks = 2,
                             point_size = base_point_size,
                             base_size = base_font_size,
                             color_by_category = T,
                             add_error_bar = T,
                             error_bar_width = 0.3,
                             figure_width = 12,
                             figure_height = 6,
                             n_top_pred = 10,
                             save_format = "pdf",
                             plot_type = "plot only") {
  
  R2_contribution_by_predictor <- ggplot(avg_Beta2_percOutcome_plot, 
                        aes(x = fct_rev(Predictor_phenotype), 
                            y = BetaSquared_scaleX_percOutcome)) + 
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
    labs(x = "", y = expression(paste("Average percentage of ", R^2, " contribution")))
  
  if (color_by_category == T) {
    R2_contribution_by_predictor <- R2_contribution_by_predictor + 
      geom_col(aes(fill = Predictor_category)) +
      scale_fill_manual(
        limits = levels(avg_Beta2_percOutcome_plot$Predictor_category),
        values = levels(avg_Beta2_percOutcome_plot$Color_values)) +
      labs(fill = "Predictor category")
  }
  
  if (add_error_bar == T) {
    R2_contribution_by_predictor <- R2_contribution_by_predictor + geom_errorbar(aes(x = fct_rev(Predictor_phenotype), 
                                       ymin = BetaSquared_scaleX_percOutcome - SE_BetaSquared_scaleX_percOutcome, 
                                       ymax = BetaSquared_scaleX_percOutcome + SE_BetaSquared_scaleX_percOutcome),
                                   # position = dodge, 
                                   width = error_bar_width)
  }
  
  if (add_error_bar == T & color_by_category == T) {
    figure_name <- paste0("/Comorbidity_architecture/Panel5_Top", n_top_pred,
                          "_WithSE_ColorByCategory.", save_format)
  } else if (add_error_bar == T & color_by_category == F) {
    figure_name <- paste0("/Comorbidity_architecture/Panel5_Top", n_top_pred, 
                          "_WithSE_NoColor.", save_format)
  } else if (add_error_bar == F & color_by_category == F) {
    figure_name <- paste0("/Comorbidity_architecture/Panel5_Top", n_top_pred,
                          "_NoSE_NoColor.", save_format) 
  }
  
  if (plot_type == "plot only") {
    R2_contribution_by_predictor <- R2_contribution_by_predictor + theme(legend.position="none")
    ggsave(filename = paste0(figure_dir, figure_name), 
           R2_contribution_by_predictor, 
           width = figure_width, height = figure_height, limitsize = FALSE)
  } else if (plot_type == "legend only") {
    legend_color <- get_legend(R2_contribution_by_predictor)
    ggsave(filename = paste0(figure_dir, "/Comorbidity_architecture/Panel5_legend.", save_format),
           ggdraw(legend_color), width = 6, height = 4)
    
  }
  
}

##### Make plot #####
###### Top 10: ######
## plot only
R2_contribution_by_predictor_plot(avg_Beta2_percOutcome_plot,
                 metric_lower_lim = 0,
                 metric_upper_lim = 8,
                 metric_breaks = 2,
                 color_by_category = T,
                 add_error_bar = T,
                 error_bar_width = 0.3,
                 figure_width = 10,
                 figure_height = 5,
                 n_top_pred = 10,
                 plot_type = "plot only") 

## legend only
R2_contribution_by_predictor_plot(avg_Beta2_percOutcome_plot,
                                  metric_lower_lim = 0,
                                  metric_upper_lim = 8,
                                  metric_breaks = 2,
                                  color_by_category = T,
                                  add_error_bar = T,
                                  error_bar_width = 0.3,
                                  figure_width = 10,
                                  figure_height = 5,
                                  n_top_pred = 10,
                                  plot_type = "legend only") 

###### Top 15: ######
R2_contribution_by_predictor_plot(avg_Beta2_percOutcome_plot2,
                                  metric_lower_lim = 0,
                                  metric_upper_lim = 8,
                                  metric_breaks = 2,
                                  color_by_category = F,
                                  add_error_bar = T,
                                  error_bar_width = 0.3,
                                  figure_width = 9.6,
                                  figure_height = 6,
                                  n_top_pred = 15,
                                  plot_type = "plot only") 


#### Correlation matrix of effect sizes across outcomes ####
n_outcome_ds <- 88
outcome_ds_vec <- c("COVID_hspt", "death", outcome_ds_prev %>%
                      # remove phecode 622 as it is defined the same as phecode 622.1
                      filter(Outcome_phecode != "622" & Outcome_phecode != "COVID_hspt" &
                               Outcome_phecode != "death") %>%
                      arrange(Outcome_phecode) %>%
                      pull(Outcome_phecode))

phecode_info_sort <- phecode_info %>% 
  filter(Outcome_phecode %in% outcome_ds_vec[-c(1,2)]) %>%
  arrange(Outcome_phecode)
# all.equal(phecode_info_sort$Outcome_phecode, outcome_ds_vec[-c(1,2)])
outcome_pheno_vec <- c("COVID-19 hospitalisation", 
                       "Death",
                       phecode_info_sort$phenotype)


##### Get correlation matrix - based on raw effect sizes #####
effec_size_cor_mt <- matrix(NA, n_outcome_ds, n_outcome_ds)
colnames(effec_size_cor_mt) <- rep(" ", n_outcome_ds)
rownames(effec_size_cor_mt) <- outcome_pheno_vec
diag(effec_size_cor_mt) <- 1

for (i in 1:(n_outcome_ds-1)) {
  for (j in (i+1):n_outcome_ds) {
    target_ds_i <- outcome_ds_vec[i]
    target_ds_j <- outcome_ds_vec[j]
    effec_size_i <- all_outcome_summ_coef_pred_ds %>%
      filter(Outcome_phecode == target_ds_i) %>%
      select(Predictor, Est) %>%
      rename(Est_i = Est)
    effec_size_j <- all_outcome_summ_coef_pred_ds %>%
      filter(Outcome_phecode == target_ds_j) %>%
      select(Predictor, Est) %>%
      rename(Est_j = Est)
    effec_size_i_j <- effec_size_i %>%
      inner_join(effec_size_j, by = "Predictor")
    effec_size_cor_mt[i,j] <- cor(effec_size_i_j$Est_i,
                                  effec_size_i_j$Est_j)
    effec_size_cor_mt[j,i] <- effec_size_cor_mt[i,j]
  }
}

##### Get correlation matrix - based on standardized effect sizes #####
effec_size_cor_mt_scaleX <- matrix(NA, n_outcome_ds, n_outcome_ds)
colnames(effec_size_cor_mt_scaleX) <- rep(" ", n_outcome_ds)
rownames(effec_size_cor_mt_scaleX) <- outcome_pheno_vec
diag(effec_size_cor_mt_scaleX) <- 1

for (i in 1:(n_outcome_ds-1)) {
  for (j in (i+1):n_outcome_ds) {
    target_ds_i <- outcome_ds_vec[i]
    target_ds_j <- outcome_ds_vec[j]
    effec_size_i <- all_outcome_summ_coef_pred_ds %>%
      filter(Outcome_phecode == target_ds_i) %>%
      select(Predictor, Est_scaleX) %>%
      rename(Est_i = Est_scaleX)
    effec_size_j <- all_outcome_summ_coef_pred_ds %>%
      filter(Outcome_phecode == target_ds_j) %>%
      select(Predictor, Est_scaleX) %>%
      rename(Est_j = Est_scaleX)
    effec_size_i_j <- effec_size_i %>%
      inner_join(effec_size_j, by = "Predictor")
    effec_size_cor_mt_scaleX[i,j] <- cor(effec_size_i_j$Est_i,
                                  effec_size_i_j$Est_j)
    effec_size_cor_mt_scaleX[j,i] <- effec_size_cor_mt_scaleX[i,j]
  }
}


##### Functions #####
dist2order = function(corr, method, ...) {
  d_corr = as.dist(1 - corr)
  s = seriate(d_corr, method = method, ...)
  i = get_order(s)
  return(i)
}

##### Make correlation plot - use all outcomes #####
jpeg(paste0(figure_dir, "/Comorbidity_similarity/Panel1_all_outcomes.jpeg"),
     width = 23, height = 23, units = "in", res = 300)
corrplot(effec_size_cor_mt, 
         method = "circle",
         type = 'upper', 
         order = 'hclust', 
         tl.col = 'black',
         tl.cex = 1,
         tl.srt = 45,
         cl.ratio = 0.08, 
         addrect = 15,
         col = rev(COL2('RdYlBu', 200))
         )
dev.off()

jpeg(paste0(figure_dir, "/Comorbidity_similarity/Panel1_all_outcomes_scaleX.jpeg"),
     width = 23, height = 23, units = "in", res = 300)
corrplot(effec_size_cor_mt_scaleX, 
         method = "circle",
         type = 'upper', 
         order = 'hclust', 
         tl.col = 'black',
         tl.cex = 1,
         tl.srt = 45,
         cl.ratio = 0.08, 
         addrect = 15,
         col = rev(COL2('RdYlBu', 200))
)
dev.off()

## average correlation
mean(effec_size_cor_mt_scaleX[row(effec_size_cor_mt_scaleX) != col(effec_size_cor_mt_scaleX)])

## Specific clusters
cluster1_idx <- which(rownames(effec_size_cor_mt_scaleX) %in%
                        c("Myocardial infarction",
                          "Coronary atherosclerosis",
                          "Transient cerebral ischemia",
                         # "Occlusion of cerebral infarction",
                          "Cerebral artery occlusion, with cerebral infarction"))
effec_size_cor_mt_scaleX_cluster1 <- effec_size_cor_mt_scaleX[cluster1_idx, cluster1_idx]
mean(effec_size_cor_mt_scaleX_cluster1[row(effec_size_cor_mt_scaleX_cluster1) != col(effec_size_cor_mt_scaleX_cluster1)])
# 0.825598

cluster2_idx <- which(rownames(effec_size_cor_mt_scaleX) %in%
                        c("Pneumonia",
                          "COVID-19 hospitalisation",
                          "Sepsis",
                          "Death"))
effec_size_cor_mt_scaleX_cluster2 <- effec_size_cor_mt_scaleX[cluster2_idx, cluster2_idx]
mean(effec_size_cor_mt_scaleX_cluster2[row(effec_size_cor_mt_scaleX_cluster2) != col(effec_size_cor_mt_scaleX_cluster2)])
# 0.7524215

##### Make correlation plot - use leaf outcomes only #####
leaf_outcome_ds_vec <- c()
for (i in 1:n_outcome_ds) {
  target_ds_i <- outcome_ds_vec[i]
  if ( sum(startsWith(outcome_ds_vec, target_ds_i)) == 1 ) {
    leaf_outcome_ds_vec <- c(leaf_outcome_ds_vec, target_ds_i)
  }
}

leaf_outcome_idx <- which(outcome_ds_vec %in% leaf_outcome_ds_vec)

jpeg(paste0(figure_dir, "/Comorbidity_similarity/Panel1_leaf_outcomes.jpeg"),
     width = 16, height = 16, units = "in", res = 300)
corrplot(effec_size_cor_mt[leaf_outcome_idx, leaf_outcome_idx], 
         method = "circle",
         type = 'upper', 
         order = 'hclust', 
         tl.col = 'black',
         tl.cex = 1,
         cl.ratio = 0.08, 
         tl.srt = 45,
         col = rev(COL2('RdYlBu', 200))
)
dev.off()

jpeg(paste0(figure_dir, "/Comorbidity_similarity/Panel1_leaf_outcomes_scaleX.jpeg"),
     width = 16, height = 16, units = "in", res = 300)
corrplot(effec_size_cor_mt_scaleX[leaf_outcome_idx, leaf_outcome_idx], 
         method = "circle",
         type = 'upper', 
         order = 'hclust', 
         tl.col = 'black',
         tl.cex = 1,
         cl.ratio = 0.08, 
         tl.srt = 45,
         col = rev(COL2('RdYlBu', 200))
)
dev.off()


##### Make correlation plot - use outcomes with good performance metrics #####
good_metrics_idx <- which(outcome_ds_vec %in% Me_df_phenotype_good_metrics$Outcome_phecode)

jpeg(paste0(figure_dir, "/Comorbidity_similarity/Panel1_good_metrics.jpeg"),
     width = 12, height = 12, units = "in", res = 300)
corrplot(effec_size_cor_mt[good_metrics_idx, good_metrics_idx], 
         method = "circle",
         type = 'upper', 
         order = 'hclust', 
         tl.col = 'black',
         tl.cex = 1,
         cl.ratio = 0.08, 
         tl.srt = 45,
         col = rev(COL2('RdYlBu', 200))
)
dev.off()

jpeg(paste0(figure_dir, "/Comorbidity_similarity/Panel1_good_metrics_scaleX.jpeg"),
     width = 12, height = 12, units = "in", res = 300)
corrplot(effec_size_cor_mt_scaleX[good_metrics_idx, good_metrics_idx], 
         method = "circle",
         type = 'upper', 
         order = 'hclust', 
         tl.col = 'black',
         tl.cex = 1,
         cl.ratio = 0.08, 
         tl.srt = 45,
         col = rev(COL2('RdYlBu', 200))
)
dev.off()


#### Fig. 6, left: Similarity in comorbidity profiles - CRS effect sizes across outcomes ####
##### Get outcome pairs for effect size comparison - standardized effect sizes #####
effec_size_cor_mt_scaleX2 <- effec_size_cor_mt_scaleX %>%
  as.data.frame()
colnames(effec_size_cor_mt_scaleX2) <- outcome_pheno_vec

covid_corr <- data.frame(
  "Outcome_phecode" = outcome_ds_vec,
  "Corr" = effec_size_cor_mt_scaleX2$`COVID-19 hospitalisation`
)
covid_corr_outcome <- covid_corr %>%
  filter(Corr != 1) %>%
  filter(Corr == max(Corr)) %>%
  pull(Outcome_phecode)


effec_size_cor_mt2 <- effec_size_cor_mt %>%
  as.data.frame()
colnames(effec_size_cor_mt2) <- outcome_pheno_vec

##### Combine raw and standardized effect sizes #####
covid_corr_combined <- data.frame(
  "Outcome_phecode" = outcome_ds_vec,
  "Outcome_phenotype" = outcome_pheno_vec,
  "Corr" = effec_size_cor_mt2$`COVID-19 hospitalisation`,
  "Corr_scaleX" = effec_size_cor_mt_scaleX2$`COVID-19 hospitalisation`
) %>% filter(Outcome_phecode != "COVID_hspt")

mi_corr_combined <- data.frame(
  "Outcome_phecode" = outcome_ds_vec,
  "Outcome_phenotype" = outcome_pheno_vec,
  "Corr" = effec_size_cor_mt2$`Myocardial infarction`,
  "Corr_scaleX" = effec_size_cor_mt_scaleX2$`Myocardial infarction`
) %>% filter(Outcome_phecode != "411_2")

lc_corr_combined <- data.frame(
  "Outcome_phecode" = outcome_ds_vec,
  "Outcome_phenotype" = outcome_pheno_vec,
  "Corr" = effec_size_cor_mt2$`Cancer of bronchus; lung`,
  "Corr_scaleX" = effec_size_cor_mt_scaleX2$`Cancer of bronchus; lung`
) %>% filter(Outcome_phecode != "165_1")


View(covid_corr_combined)
View(mi_corr_combined)

##### Plot function #####
get_effec_size_comparison <- function(all_outcome_summ_coef_pred_ds,
                                      phecode_info,
                               Outcome_phecode1,
                               Outcome_phecode2) {
  effec_size_dt1 <- all_outcome_summ_coef_pred_ds %>%
    filter(Outcome_phecode == Outcome_phecode1) %>%
    select(Outcome_phecode, Predictor, Predictor_phenotype, 
           Est, Est_scaleX) %>%
    left_join(phecode_info[, c("Outcome_phecode", "phenotype")], 
              by = "Outcome_phecode") %>%
    select(!Outcome_phecode) %>%
    rename(Outcome_phenotype1 = phenotype,
           Est1 = Est, 
           Est_scaleX1 = Est_scaleX)
  
  if (Outcome_phecode1 == "COVID_hspt") {
    effec_size_dt1$Outcome_phenotype1 <- "COVID-19 hospitalisation"
  }
  
  effec_size_dt2 <- all_outcome_summ_coef_pred_ds %>%
    filter(Outcome_phecode == Outcome_phecode2) %>%
    select(Outcome_phecode, Predictor, Est, Est_scaleX) %>%
    left_join(phecode_info[, c("Outcome_phecode", "phenotype")], 
              by = "Outcome_phecode") %>%
    select(!Outcome_phecode) %>%
    rename(Outcome_phenotype2 = phenotype,
           Est2 = Est, 
           Est_scaleX2 = Est_scaleX)
  
  if (Outcome_phecode2 == "COVID_hspt") {
    effec_size_dt2$Outcome_phenotype2 <- "COVID-19 hospitalisation"
  }
  
  effec_size_dt_combined <- effec_size_dt1 %>%
    inner_join(effec_size_dt2, by = "Predictor")
  
  return(effec_size_dt_combined)
}


CRS_effec_size_across_outcomes_plot <- function(effec_size_dt_combined,
                             phecode_info,
                             ref_linewidth = 0.6,
                             y_lower_lim = -0.05,
                             y_upper_lim = 0.20,
                             y_breaks = 0.05,
                             x_lower_lim = -0.05,
                             x_upper_lim = 0.20,
                             x_breaks = 0.05,
                             point_size = base_point_size,
                             base_size = base_font_size,
                             text_label_size = 6,
                             point_padding = 1,
                             box_padding = 0.5,
                             nudge_x = 0,
                             nudge_y = 0,
                             seed = 100,
                             model_fit = T,
                             label_top_N = T,
                             save_format = "pdf",
                             fig_width = 7,
                             fig_height = 6) {
  
  Outcome_phenotype1 <- effec_size_dt_combined$Outcome_phenotype1 %>% unique()
  Outcome_phenotype2 <- effec_size_dt_combined$Outcome_phenotype2 %>% unique()
  
  effec_size_across_outcomes_plot <- ggplot(effec_size_dt_combined, 
                       aes(x = Est_scaleX1, 
                           y = Est_scaleX2)) + 
    geom_abline(slope = 1, intercept = 0, 
                linewidth = ref_linewidth) +
    geom_hline(yintercept = 0, linetype = "dashed") + 
    geom_vline(xintercept = 0, linetype = "dashed") + 
    geom_point(size = point_size) +
    #      geom_errorbar(aes(ymin = test_R2_LB, ymax = test_R2_UB, color = Incidence_group), 
    #                    width = error_bar_width) +
    #      geom_errorbarh(aes(xmin = train_R2_LB, xmax = train_R2_UB, color = Incidence_group), 
    #                     height = error_bar_width) +
    scale_y_continuous(limits = c(y_lower_lim, y_upper_lim), 
                       breaks = seq(y_lower_lim, y_upper_lim, y_breaks)) +
    scale_x_continuous(limits = c(x_lower_lim, x_upper_lim), 
                       breaks = seq(x_lower_lim, x_upper_lim, x_breaks)) +
    theme_bw(base_size = base_size)+
    theme( 
      # legend.position="right",
      # panel.border = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank()
    ) + 
    labs(x = paste0("CRS effect size (", Outcome_phenotype1, ")"), 
         y = paste0("CRS effect size (", Outcome_phenotype2, ")"))
  
  if (model_fit == T) {
    effec_size_across_outcomes_plot <- effec_size_across_outcomes_plot +
      geom_smooth(method = "lm", formula = y ~ 0 + x,
                  alpha = 0.5)
  }
  
  if (label_top_N == T) {
    top_N_idx <- which.maxn(abs(effec_size_dt_combined$Est_scaleX1 - effec_size_dt_combined$Est_scaleX2), 5)
    all_predictor_dt_label <- effec_size_dt_combined[top_N_idx,]
    
    if (Outcome_phenotype1 == "COVID-19 hospitalisation") {
      nudge_x <- case_when(all_predictor_dt_label$Predictor_phenotype == "Substance addiction and disorders" ~ nudge_x,
                           .default = nudge_x)
      nudge_y = case_when(all_predictor_dt_label$Predictor_phenotype == "Substance addiction and disorders" ~ nudge_y + 0.02,
                          .default = nudge_y)
      target_predictor <- "Tobacco use disorder"
    } else if (Outcome_phenotype1 == "Ischaemic stroke") {
      nudge_x <- case_when(all_predictor_dt_label$Predictor_phenotype == "Epilepsy, recurrent seizures, convulsions" ~ nudge_x + 0.03,
                           all_predictor_dt_label$Predictor_phenotype == "Congestive heart failure" ~ nudge_x,
                           all_predictor_dt_label$Predictor_phenotype == "Intracranial hemorrhage" ~ nudge_x + 0.05,
                           .default = nudge_x)
      nudge_y = case_when(all_predictor_dt_label$Predictor_phenotype == "Epilepsy, recurrent seizures, convulsions" ~ nudge_y + 0.02,
                          all_predictor_dt_label$Predictor_phenotype == "Chronic airway obstruction" ~ nudge_y,
                          all_predictor_dt_label$Predictor_phenotype == "Congestive heart failure" ~ nudge_y,
                          all_predictor_dt_label$Predictor_phenotype == "Intracranial hemorrhage" ~ nudge_y,
                          .default = nudge_y)
      target_predictor <- "Disorders of lipid metabolism"
    }
    
    effec_size_dt_combined$label_idx <- if_else(row_number(effec_size_dt_combined) %in% top_N_idx, T, F)
    effec_size_across_outcomes_plot <- effec_size_across_outcomes_plot + 
      geom_point(color = ifelse(effec_size_dt_combined$label_idx == T, red, grey),
                 size = point_size) + 
      geom_label_repel(data = effec_size_dt_combined[top_N_idx,], 
                        aes(label = Predictor_phenotype, 
                            x = Est_scaleX1, 
                            y = Est_scaleX2,
                            fill = if_else(all_predictor_dt_label$Predictor_phenotype == target_predictor,
                                           1, 0) %>% as.factor()),
                        size = text_label_size,
                       point.padding = point_padding,
                       box.padding = box_padding,
                       nudge_x = nudge_x,
                       nudge_y = nudge_y,
                       seed = seed) +
      scale_fill_discrete(aesthetics = c("fill"),
                          type = c("white", yellow)) + 
      guides(fill = "none") 
      
  }
  
  if (model_fit == F & label_top_N == F) {
    figure_name <- paste0(figure_dir, "/Comorbidity_similarity/Panel2_NoFit_NoLabel_",
                          Outcome_phenotype1, "_", 
                          Outcome_phenotype2, ".", save_format)
  } else if (model_fit == T & label_top_N == F) {
    figure_name <- paste0(figure_dir, "/Comorbidity_similarity/Panel2_Fit_NoLabel_",
                             Outcome_phenotype1, "_", 
                             Outcome_phenotype2, ".", save_format)
  } else if (model_fit == F & label_top_N == T) {
    figure_name <- paste0(figure_dir, "/Comorbidity_similarity/Panel2_NoFit_Labeled_",
                          Outcome_phenotype1, "_", 
                          Outcome_phenotype2, ".", save_format)
  } else if (model_fit == T & label_top_N == T) {
    figure_name <- paste0(figure_dir, "/Comorbidity_similarity/Panel2_Fit_Labeled_",
                          Outcome_phenotype1, "_", 
                          Outcome_phenotype2, ".", save_format)
  }
  
  figure_name <- str_replace(figure_name, "Ischaemic", "Ischemic")
    
  ggsave(filename = figure_name, 
         effec_size_across_outcomes_plot, 
         width = fig_width, 
         height = fig_height, 
         limitsize = FALSE)
}


##### Make plot - COVID hspt vs. sepsis #####
effec_size_dt_combined <- get_effec_size_comparison(all_outcome_summ_coef_pred_ds,
                                                    phecode_info,
                                                    Outcome_phecode1 = "COVID_hspt",
                                                    Outcome_phecode2 = "994_2") %>%
  mutate(Predictor_phenotype = case_when(Predictor_phenotype == "Other diseases of respiratory system, not elsewhere classified" ~
                                           "Other diseases of respiratory system", 
                                         Predictor_phenotype == "Overweight, obesity and other hyperalimentation" ~
                                           "Overweight and obesity",
                                         Predictor_phenotype == "Disorders of lipoid metabolism" ~ 
                                           "Disorders of lipid metabolism", 
                                         .default = Predictor_phenotype))

summary(lm(Est_scaleX2 ~ 0 + Est_scaleX1, data = effec_size_dt_combined))
#  Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#  Est_scaleX1  0.83729    0.04789   17.48   <2e-16 ***

cor.test(effec_size_dt_combined$Est_scaleX1,
         effec_size_dt_combined$Est_scaleX2)
#  95 percent confidence interval:
#   0.6260679 0.7644024
#  sample estimates:
#   cor 
#  0.7017915 

CRS_effec_size_across_outcomes_plot(effec_size_dt_combined,
                                    phecode_info,
                                    ref_linewidth = 0.6,
                                    y_lower_lim = -0.05,
                                    y_upper_lim = 0.20,
                                    y_breaks = 0.05,
                                    x_lower_lim = -0.05,
                                    x_upper_lim = 0.20,
                                    x_breaks = 0.05,
                                    point_size = base_point_size,
                                    base_size = base_font_size,
                                    text_label_size = 6.5,
                                    model_fit = F,
                                    label_top_N = T,
                                    save_format = "pdf",
                                    fig_width = 7.8,
                                    fig_height = 6.8) 


##### Make plot - MI vs. Ischemic stroke #####
effec_size_dt_combined <- get_effec_size_comparison(all_outcome_summ_coef_pred_ds,
                                                    phecode_info,
                                                    Outcome_phecode1 = "433_21",
                                                    Outcome_phecode2 = "411_2") %>%
  mutate(Predictor_phenotype = case_when(Predictor_phenotype == "Other diseases of respiratory system, not elsewhere classified" ~
                                           "Other diseases of respiratory system", 
                                         Predictor_phenotype == "Overweight, obesity and other hyperalimentation" ~
                                           "Overweight and obesity",
                                         Predictor_phenotype == "Disorders of lipoid metabolism" ~ 
                                           "Disorders of lipid metabolism", 
                                         Predictor_phenotype == "Congestive heart failure; nonhypertensive" ~ 
                                           "Congestive heart failure",
                                         .default = Predictor_phenotype)) %>%
  mutate(Outcome_phenotype1 = "Ischaemic stroke")

summary(lm(Est_scaleX2 ~ 0 + Est_scaleX1, data = effec_size_dt_combined))
#  Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#  Est_scaleX1  0.94252    0.04078   23.11   <2e-16 ***

cor.test(effec_size_dt_combined$Est_scaleX1,
         effec_size_dt_combined$Est_scaleX2)
# 95 percent confidence interval:
#   0.7764197 0.8639578
# sample estimates:
#   cor 
# 0.8250781 

CRS_effec_size_across_outcomes_plot(effec_size_dt_combined,
                                    phecode_info,
                                    ref_linewidth = 0.6,
                                    y_lower_lim = -0.05,
                                    y_upper_lim = 0.20,
                                    y_breaks = 0.05,
                                    x_lower_lim = -0.05,
                                    x_upper_lim = 0.20,
                                    x_breaks = 0.05,
                                    point_size = base_point_size,
                                    base_size = base_font_size,
                                    text_label_size = 6.5,
                                    model_fit = F,
                                    label_top_N = T,
                                    save_format = "pdf",
                                    fig_width = 7.8,
                                    fig_height = 6.8) 

#### Fig. 6, right: Effect sizes of target predictor in different models by outcome ####

##### Read effect sizes for lipid disorder from different models for Ischemic stroke vs. MI #####
n_models <- 6
lipid_disorder_effec_size_stroke <- data.frame("Outcome_phenotype" = rep("Ischemic stroke", n_models),
                                               "N_predictors" = c(1, 10, 20, 50, 100, 212) - 1,
                                               "Est" = c(0.4585395, 0.1421576, 0.05543171, 0.03164941, 0.01187475, 4.260443e-5),
                                               "SE" = c(0.0095675762, 0.01008326, 0.01071491, 0.01082935, 0.01091818, 0.01097403),
                                               "Predictor_prev" = rep(0.1018413, n_models))

lipid_disorder_effec_size_mi <- data.frame("Outcome_phenotype" = rep("Myocardial infarction", n_models),
                                               "N_predictors" = c(1, 10, 20, 50, 100, 212) - 1,
                                               "Est" = c(0.8411392, 0.5334476, 0.511536, 0.4593205, 0.4240438, 0.4062696),
                                               "SE" = c(0.0069033947, 0.007275128, 0.007352095, 0.007484792, 0.007589137, 0.00764289),
                                               "Predictor_prev" = rep(0.1018413, n_models))

lipid_disorder_effec_size_df <- rbind(lipid_disorder_effec_size_stroke,
                                      lipid_disorder_effec_size_mi) %>%
  mutate(Est_scaleX = Est * sqrt(Predictor_prev * (1 - Predictor_prev))) %>%
  mutate(SE_scaleX = SE * sqrt(Predictor_prev * (1 - Predictor_prev))) %>%
  mutate(Est_scaleX_LB = Est_scaleX - qnorm(0.975) * SE_scaleX,
         Est_scaleX_UB = Est_scaleX + qnorm(0.975) * SE_scaleX) %>%
  mutate(Outcome_phenotype = if_else(Outcome_phenotype == "Ischemic stroke", 
                                     "Ischaemic stroke",
                                     Outcome_phenotype)) %>%
  mutate(Outcome_phenotype = factor(Outcome_phenotype,
                                    levels = c("Myocardial infarction",
                                               "Ischaemic stroke")))

##### Read effect sizes for tobacco use disorder from different models for COVID hspt vs. sepsis #####
n_models <- 6
smoking_effec_size_covid <- data.frame("Outcome_phenotype" = rep("COVID-19 hospitalisation", n_models),
                                               "N_predictors" = c(1, 10, 20, 50, 100, 212) - 1,
                                               "Est" = c(0.67511629, 0.42885449, 0.27537762, 0.0121208969, -0.037910951, -0.063790165),
                                               "SE" = c(0.0109927345, 0.0119038543, 0.0124940121, 0.013866394, 0.01421021, 0.01454000),
                                               "Predictor_prev" = rep(0.13270430, n_models))

smoking_effec_size_sepsis <- data.frame("Outcome_phenotype" = rep("Sepsis", n_models),
                                           "N_predictors" = c(1, 10, 20, 50, 100, 212) - 1,
                                           "Est" = c(1.10602353, 0.98765958, 0.815746898, 0.597049513, 0.514202181, 0.480608822),
                                           "SE" = c(0.0080194966, 0.008319383, 0.0087467723, 0.0093374807, 0.009625624, 0.009840437),
                                           "Predictor_prev" = rep(0.07895079, n_models))

smoking_effec_size_df <- rbind(smoking_effec_size_covid,
                               smoking_effec_size_sepsis) %>%
  mutate(Est_scaleX = Est * sqrt(Predictor_prev * (1 - Predictor_prev))) %>%
  mutate(SE_scaleX = SE * sqrt(Predictor_prev * (1 - Predictor_prev))) %>%
  mutate(Est_scaleX_LB = Est_scaleX - qnorm(0.975) * SE_scaleX,
         Est_scaleX_UB = Est_scaleX + qnorm(0.975) * SE_scaleX) %>%
  mutate(Outcome_phenotype = factor(Outcome_phenotype,
                                    levels = c("Sepsis",
                                               "COVID-19 hospitalisation")))


##### Plot function #####
CRS_effec_size_by_model <- function(effec_size_compare_df,
                        predictor_name,
                        ref_linewidth = 0.6,
                        metric_lower_lim = 0,
                        metric_upper_lim = 0.3,
                        metric_breaks = 0.05,
                        point_size = base_point_size,
                        base_size = base_font_size,
                        error_bar_width = 0,
                        legend_spacing = 0.5,
                        save_format = "pdf",
                        fig_width = 7,
                        fig_height = 6) {

  effec_size_by_model_plot <- ggplot(effec_size_compare_df, 
                      aes(x = N_predictors, 
                          y = Est_scaleX, 
                          color = Outcome_phenotype)) +
    geom_hline(yintercept = 0, linetype = "dashed") + 
    geom_point(size = point_size) + 
    geom_errorbar(aes(ymin = Est_scaleX_LB, 
                      ymax = Est_scaleX_UB),
                  width = error_bar_width) + 
    geom_line() + 
    scale_y_continuous(limits = c(metric_lower_lim, metric_upper_lim), 
                       breaks = seq(metric_lower_lim, metric_upper_lim, 
                                    metric_breaks)) +
    scale_color_manual(limits = levels(effec_size_compare_df$Outcome_phenotype),
                       values = c(orange, blue)) +
    theme_bw(base_size = base_size) +
    theme( 
      legend.position="right",
      legend.key.spacing.y = unit(legend_spacing, 'cm'),
      #  panel.border = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank()
    ) + 
    guides(color = guide_legend(byrow = T)) +
    labs(x = "Number of covariate predictor diseases", 
         y = paste0("Effect size of\n", predictor_name), 
         color = 'Outcome')

  ggsave(filename = paste0(figure_dir, "/Comorbidity_similarity/Panel3_", predictor_name,".", save_format), 
         effec_size_by_model_plot, 
         width = fig_width, 
         height = fig_height, 
         limitsize = FALSE)
  
}

##### Make plot #####
CRS_effec_size_by_model(smoking_effec_size_df,
                        predictor_name = "Tobacco use disorder",
                        ref_linewidth = 0.6,
                        metric_lower_lim = -0.05,
                        metric_upper_lim = 0.3,
                        metric_breaks = 0.05,
                        point_size = base_point_size,
                        base_size = base_font_size,
                        error_bar_width = 0,
                        legend_spacing = 0.5,
                        save_format = "pdf",
                        fig_width = 11.8,
                        fig_height = 6.8)

CRS_effec_size_by_model(lipid_disorder_effec_size_df,
                        predictor_name = "Disorders of lipid metabolism",
                        ref_linewidth = 0.6,
                        metric_lower_lim = -0.05,
                        metric_upper_lim = 0.3,
                        metric_breaks = 0.05,
                        point_size = base_point_size,
                        base_size = base_font_size,
                        error_bar_width = 0,
                        legend_spacing = 0.5,
                        save_format = "pdf",
                        fig_width = 11,
                        fig_height = 6.8)


#### Fig. S12: Attenuation in effect sizes of target predictor ####
##### Read summ stats #####
target_effec_size_att_summ <- read.csv(paste0(read_dir,
                                              "Target_Outcomes_WeightedLogisticRegression_SummStats_ControlCase4to1_WhiteBritish_OddYOB.csv"))
target_effec_size_att_summ_top5 <- target_effec_size_att_summ %>%
  group_by(Outcome_phecode, Predictor_phecode) %>%
  arrange(Est) %>%
  mutate(Rank = row_number()) %>%
  filter(Rank <= 5) %>%
  ungroup()

target_effec_size_att_summ_top5_final <- target_effec_size_att_summ_top5 %>%
  mutate(Predictor = paste0("ds", Predictor_phecode)) %>%
  left_join(all_outcome_summ_coef_pred_ds %>% 
              select(Outcome_phecode, Predictor, Predictor_prev, Predictor_prev2020),
            by = c("Outcome_phecode","Predictor")) %>%
  mutate(Est_scaleX = if_else(Outcome_phecode != "COVID_hspt", 
                              Est * sqrt(Predictor_prev * (1 - Predictor_prev)),
                              Est * sqrt(Predictor_prev2020 * (1 - Predictor_prev2020))),
         SE_scaleX = if_else(Outcome_phecode != "COVID_hspt", 
                             SE * sqrt(Predictor_prev * (1 - Predictor_prev)),
                             SE * sqrt(Predictor_prev2020 * (1 - Predictor_prev2020)))) %>%
  left_join(phecode_parent_info %>% select(Predictor_name, phenotype_parent),
            by = join_by(covar_pred_name == Predictor_name)) %>%
  rename(covar_phenotype = phenotype_parent) %>%
  mutate(covar_phenotype = case_when(covar_phenotype == "Other diseases of respiratory system, not elsewhere classified" ~ "Other diseases of respiratory system",
                                     covar_phenotype == "Disorders of fluid, electrolyte, and acid-base balance" ~ "Disorders of fluid, electrolyte,\n and acid-base balance",
                                     .default = covar_phenotype))

## COVID-19 outcome  
target_effec_size_att_summ_covid <- target_effec_size_att_summ_top5_final %>%
  filter(Outcome_phecode == "COVID_hspt") %>%
  select(covar_phenotype, Est_scaleX, SE_scaleX) %>%
  rbind(data.frame(covar_phenotype = "None",
                   smoking_effec_size_df %>%
                     filter(Outcome_phenotype == "COVID-19 hospitalisation" &
                              N_predictors == 0 ) %>%
                     select(Est_scaleX, SE_scaleX) ) ) %>%
  mutate(Est_scaleX_LB = Est_scaleX - qnorm(0.975) * SE_scaleX,
         Est_scaleX_UB = Est_scaleX + qnorm(0.975) * SE_scaleX) 

target_effec_size_att_summ_covid_final <- target_effec_size_att_summ_covid %>%
  mutate(covar_phenotype = factor(covar_phenotype, 
                                  levels = c("None", target_effec_size_att_summ_covid$covar_phenotype[1:5])) ) %>%
  mutate(color_group = as.factor(if_else(covar_phenotype == "None", 1, 2)))


## Stroke outcome  
target_effec_size_att_summ_stroke <- target_effec_size_att_summ_top5_final %>%
  filter(Outcome_phecode == "433_21") %>%
  select(covar_phenotype, Est_scaleX, SE_scaleX) %>%
  rbind(data.frame(covar_phenotype = "None",
                   lipid_disorder_effec_size_df %>%
                     filter(Outcome_phenotype == "Ischaemic stroke" &
                              N_predictors == 0 ) %>%
                     select(Est_scaleX, SE_scaleX) ) ) %>%
  mutate(Est_scaleX_LB = Est_scaleX - qnorm(0.975) * SE_scaleX,
         Est_scaleX_UB = Est_scaleX + qnorm(0.975) * SE_scaleX) 

target_effec_size_att_summ_stroke_final <- target_effec_size_att_summ_stroke %>%
  mutate(covar_phenotype = factor(covar_phenotype, 
                                  levels = c("None", target_effec_size_att_summ_stroke$covar_phenotype[1:5])) ) %>%
  mutate(color_group = as.factor(if_else(covar_phenotype == "None", 1, 2)))


##### Plot function #####
effec_size_att_barplot <- function(target_effec_size_att_summ_stroke,
                       outcome_pheno = "Stroke",
                       ref_linewidth = 0.6,
                       metric_lower_lim = 0,
                       metric_upper_lim = 0.15,
                       metric_breaks = 0.05,
                       y_label = "Standardised log odds ratio",
                       point_size = base_point_size,
                       base_size = base_font_size,
                       bar_color = grey,
                       color_by_category = T,
                       add_error_bar = T,
                       error_bar_width = 0.3,
                       fig_width = 12.5,
                       fig_height = 6,
                       save_format = "pdf") {
  effec_size_barplot <- ggplot(target_effec_size_att_summ_stroke, 
                    aes(x = fct_rev(covar_phenotype), 
                        y = Est_scaleX)) + 
    geom_col(fill = bar_color) + 
    scale_y_continuous(limits = c(metric_lower_lim, metric_upper_lim), 
                       breaks = seq(metric_lower_lim, metric_upper_lim, metric_breaks)) +
    coord_flip() + 
    theme_bw(base_size = base_size) +
    theme( 
      legend.position="top",
      panel.border = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_blank()
    ) + 
    labs(x = "Covariate", y = y_label)
  
  if (color_by_category == T) {
    effec_size_barplot <- effec_size_barplot + 
      geom_col(aes(fill = color_group)) +
      scale_fill_manual(
        limits = levels(target_effec_size_att_summ_stroke$color_group),
        values = c(red, grey)) +
      guides(fill = "none") 
  }
  
  if (add_error_bar == T) {
    effec_size_barplot <- effec_size_barplot + geom_errorbar(aes(x = fct_rev(covar_phenotype), 
                                           ymin = Est_scaleX_LB, 
                                           ymax = Est_scaleX_UB),
                                       # position = dodge, 
                                       width = error_bar_width)
  }
  
  
  figure_name <- paste0("/Comorbidity_similarity/",
                        outcome_pheno,
                        "_Predictor_Effect_Size_Attenuation.", save_format)
   
  
  ggsave(filename = paste0(figure_dir, figure_name), 
         effec_size_barplot, width = fig_width, height = fig_height, limitsize = FALSE)
  
}


##### Make plot #####
effec_size_att_barplot(target_effec_size_att_summ_covid_final,
                       outcome_pheno = "COVID",
                       ref_linewidth = 0.6,
                       metric_lower_lim = 0,
                       metric_upper_lim = 0.25,
                       metric_breaks = 0.08,
                       y_label = "Standardised log odds ratio",
                       point_size = base_point_size,
                       base_size = base_font_size,
                       bar_color = grey,
                       color_by_category = T,
                       add_error_bar = T,
                       error_bar_width = 0.3,
                       fig_width = 10,
                       fig_height = 6,
                       save_format = "pdf") 

effec_size_att_barplot(target_effec_size_att_summ_stroke_final,
                                   outcome_pheno = "Stroke",
                                   ref_linewidth = 0.6,
                                   metric_lower_lim = 0,
                                   metric_upper_lim = 0.15,
                                   metric_breaks = 0.05,
                                   y_label = "Standardised log odds ratio",
                                   point_size = base_point_size,
                                   base_size = base_font_size,
                                   bar_color = grey,
                                   color_by_category = T,
                                   add_error_bar = T,
                                   error_bar_width = 0.3,
                                   fig_width = 10,
                                   fig_height = 6,
                                   save_format = "pdf") 







