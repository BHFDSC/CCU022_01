### Author: Pearl Liu
### Plot survival analysis results ###
### Index: 2015-01-01, follow-up: 10 years (all outcomes)
### Index: 2020-01-01, follow-up: 5 years (COVID hospitalization)

rm(list = ls())
gc()

#### Load libraries ####
library(data.table)
library(tidyverse)
library(survival)
library(survminer)
library(cowplot)

#### Set parameters ####
read_dir <- 'Outputs/'
summary_dir <- 'Summary_Stats/'
figure_dir <- 'Figures_Manuscript/'
file_date <- '2025_05_01'

##### Plot parameters #####
base_font_size <- 24
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

#### Read data - UPDATE TO LATEST ####
covid_surv_summ <- read.csv(paste0(read_dir, "OutcomeCOVID_hspt_SurvivalAnalysisSummaryV2_WhiteBritish_EvenYOB_Index2020_Followup5yr.csv"))
death_surv_summ <- read.csv(paste0(read_dir, "Outcomedeath_SurvivalAnalysisSummaryV2_WhiteBritish_EvenYOB_Index2015_Followup10yr.csv"))
mi_surv_summ <- read.csv(paste0(read_dir, "Outcome411_2_SurvivalAnalysisSummaryV2_WhiteBritish_EvenYOB_Index2015_Followup10yr.csv"))
lc_surv_summ <- read.csv(paste0(read_dir, "Outcome165_1_SurvivalAnalysisSummaryV2_WhiteBritish_EvenYOB_Index2015_Followup10yr.csv"))
t2d_surv_summ <- read.csv(paste0(read_dir, "Outcome250_2_SurvivalAnalysisSummaryV2_WhiteBritish_EvenYOB_Index2015_Followup10yr.csv"))

#sepsis_surv_summ <- read.csv(paste0(read_dir, "Outcome994_2_SurvivalAnalysisSummaryV2_WhiteBritish_EvenYOB_Index2015_Followup10yr.csv"))

#### Check numbers ####
##### MI #####
mi_surv_summ %>%
  filter(target_ds_pred_group == "Top 5%") %>%
  filter(time == min(time)) %>%
  pull(n.risk)
# 316954

n_top5_mi_cases <- mi_surv_summ %>%
  filter(target_ds_pred_group == "Top 5%") %>%
  pull(n.event) %>%
  sum()
n_top5_mi_cases
# 41870

n_total_mi_cases <- mi_surv_summ %>%
  pull(n.event) %>%
  sum()
n_total_mi_cases
# 215475

n_top5_mi_cases/n_total_mi_cases * 100
# 19.43149

##### T2D #####
t2d_surv_summ %>%
  filter(target_ds_pred_group == "Top 5%") %>%
  filter(time == min(time)) %>%
  pull(n.risk)
# 297420

n_top5_t2d_cases <- t2d_surv_summ %>%
  filter(target_ds_pred_group == "Top 5%") %>%
  pull(n.event) %>%
  sum()
n_top5_t2d_cases
# 68673

n_total_t2d_cases <- t2d_surv_summ %>%
  pull(n.event) %>%
  sum()
n_total_t2d_cases
# 521892

n_top5_t2d_cases/n_total_t2d_cases * 100
# 13.15847

#### Make plots ####
##### Plot functions #####
cuminc_plot <- function(target_ds, 
                        surv_summ, 
                        ylim_vec, 
                        y_axis_break,
                        plot_title = "",
                        fig_width = 6,
                        fig_height = 4.2,
                        step_size = 3,
                        surv_geom = "geom_line",
                        base_size = base_font_size,
                        save_format = "pdf",
                        plot_type = "plot only") {
  surv_summ <- surv_summ %>%
    mutate(strata = factor(target_ds_pred_group,
                             levels = c("Top 10%", "Middle 80%", "Bottom 10%")),
                          # levels = c("Top 5%", "Middle 90%", "Bottom 5%")),
           time = time/365.25) # %>%
  # group_by(strata) %>%
  # arrange(time, .by_group = TRUE) %>%
  # distinct(time, .keep_all = TRUE) %>%
  # ungroup() %>%
  # as.data.frame()
  
  y_axis_label <-  seq(ylim_vec[1], ylim_vec[2], y_axis_break) * 100 
  
  cuminc_p <- ggsurvplot_df(surv_summ, 
                            fun = "event",
                            size = step_size,
                            surv.geom = surv_geom,
                            conf.int = F, 
                            conf.int.alpha = 0.5,
                            censor = F,
                            legend = "right",
                            ylim = ylim_vec,
                            break.y.by = y_axis_break,
                            xlab = "Year",
                            ylab = "Cumulative events (%)",
                            title = plot_title,
                            legend.title = "Predicted risk based on CRS + Age + Sex",
                            palette = c(red, orange, blue),
                            font.x = c(24, "plain", "black"),
                            font.y = c(24, "plain", "black"),
                            font.tickslab = c(20, "plain", "black")) + 
    theme_bw(base_size = base_size) +
    theme(legend.title=element_text(size=24), 
          legend.text=element_text(size=20),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          legend.key.spacing.y = unit(0.3, 'cm')) + 
    scale_y_continuous(limits = c(ylim_vec[1], ylim_vec[2]), 
                       breaks = seq(ylim_vec[1], ylim_vec[2], y_axis_break),
                       labels = format(y_axis_label, nsmall = 0))
  
  #print(cuminc_p)
  
  if (plot_type == "plot only") {
    cuminc_plot <- cuminc_p + theme(legend.position="none")
    ggsave(filename = paste0(figure_dir, "/Example_outcomes/KM_", target_ds, ".", save_format), 
           cuminc_plot, width = fig_width, height = fig_height, limitsize = FALSE)
  } else if (plot_type == "legend only") {
    cuminc_legend <- get_legend(cuminc_p)
    cuminc_legend_plot <- ggdraw(cuminc_legend)
    ggsave(filename = paste0(figure_dir, "/Example_outcomes/KM_legend.", save_format),
           cuminc_legend_plot, width = 7, height = 5, limitsize = FALSE)
  }
  
}

## fig_width and fig_height consistent with calib_plot in Code 2.12
cuminc_plot_v2 <- function(target_ds, 
                        surv_summ, 
                        ylim_vec, 
                        y_axis_break,
                        plot_title = "",
                        fig_width = 5,
                        fig_height = 4.56,
                        step_size = 3,
                        surv_geom = "geom_line",
                        base_size = base_font_size,
                        save_format = "pdf",
                        plot_type = "plot only") {
  surv_summ <- surv_summ %>%
    mutate(strata = factor(target_ds_pred_group,
                         #  levels = c("Top 10%", "Middle 80%", "Bottom 10%")
                         levels = c("Top 5%", "Middle 90%", "Bottom 5%")),
           time = time/365.25) # %>%
    # group_by(strata) %>%
    # arrange(time, .by_group = TRUE) %>%
    # distinct(time, .keep_all = TRUE) %>%
    # ungroup() %>%
    # as.data.frame()
  
  y_axis_label <-  seq(ylim_vec[1], ylim_vec[2], y_axis_break) * 100 
  
  cuminc_p <- ggsurvplot_df(surv_summ, 
                fun = "event",
                size = step_size,
                surv.geom = surv_geom,
                conf.int = T, 
                conf.int.style = "ribbon",
                conf.int.alpha = 0.5,
                censor = F,
                legend = "right",
                ylim = ylim_vec,
                break.y.by = y_axis_break,
                xlab = "Year",
                ylab = "Cumulative events (%)",
                title = plot_title,
                legend.title = "Predicted risk based on CRS + Age + Sex",
                palette = c(red, orange, blue),
                font.x = c(24, "plain", "black"),
                font.y = c(24, "plain", "black"),
                font.tickslab = c(20, "plain", "black")) + 
    theme_bw(base_size = base_size) +
    theme(legend.title=element_text(size=24), 
          legend.text=element_text(size=20),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          legend.key.spacing.y = unit(0.3, 'cm')) + 
    scale_y_continuous(limits = c(ylim_vec[1], ylim_vec[2]), 
                       breaks = seq(ylim_vec[1], ylim_vec[2], y_axis_break),
                       labels = format(y_axis_label, nsmall = 0))
  
  #print(cuminc_p)
  
  if (plot_type == "plot only") {
    cuminc_plot <- cuminc_p + theme(legend.position="none")
    ggsave(filename = paste0(figure_dir, "/Example_outcomes/KM_v2_", target_ds, ".", save_format), 
           cuminc_plot, width = fig_width, height = fig_height, limitsize = FALSE)
  } else if (plot_type == "legend only") {
    cuminc_legend <- get_legend(cuminc_p)
    cuminc_legend_plot <- ggdraw(cuminc_legend)
    ggsave(filename = paste0(figure_dir, "/Example_outcomes/KM_v2_legend.", save_format),
           cuminc_legend_plot, width = 7, height = 5, limitsize = FALSE)
  }

}

##### Make plots #####
## Save legend only
cuminc_plot(target_ds = "COVID_hspt",
            covid_surv_summ, 
            ylim_vec = c(0, 0.05),
            y_axis_break = 0.01,
            plot_type = "legend only")

cuminc_plot_v2(target_ds = "COVID_hspt",
            covid_surv_summ, 
            ylim_vec = c(0, 0.05),
            y_axis_break = 0.01,
            plot_type = "legend only")

## Save plots for different outcomes - top 10%
cuminc_plot(target_ds = "COVID_hspt",
            covid_surv_summ, 
            ylim_vec = c(0, 0.05),
            y_axis_break = 0.01)
            #plot_title = "COVID-19 Hopitalisation"

cuminc_plot(target_ds = "death",
            death_surv_summ, 
            ylim_vec = c(0, 0.4),
            y_axis_break = 0.1)

cuminc_plot(target_ds = "165_1",
            lc_surv_summ, 
            ylim_vec = c(0, 0.07),
            y_axis_break = 0.02)
# plot_title = "Lung cancer")

cuminc_plot(target_ds = "411_2",
            mi_surv_summ, 
            ylim_vec = c(0, 0.15),
            y_axis_break = 0.05)
            #plot_title = "Myocardial infarction")

cuminc_plot(target_ds = "994_2",
            sepsis_surv_summ, 
            ylim_vec = c(0, 0.15),
            y_axis_break = 0.05)
          #  plot_title = "Sepsis")


###### Fig. 5a: Save plots for different outcomes - top 5%, middle 90%, bottom 5% ######
cuminc_plot_v2(target_ds = "COVID_hspt",
            covid_surv_summ, 
            ylim_vec = c(0, 0.1),
            y_axis_break = 0.02)
#plot_title = "COVID-19 Hopitalisation"

cuminc_plot_v2(target_ds = "death",
            death_surv_summ, 
            ylim_vec = c(0, 0.5),
            y_axis_break = 0.1)

cuminc_plot_v2(target_ds = "165_1",
            lc_surv_summ, 
            ylim_vec = c(0, 0.12),
            y_axis_break = 0.03)
# plot_title = "Lung cancer")

cuminc_plot_v2(target_ds = "411_2",
            mi_surv_summ, 
            ylim_vec = c(0, 0.2),
            y_axis_break = 0.04)
#plot_title = "Myocardial infarction")

cuminc_plot_v2(target_ds = "250_2",
            t2d_surv_summ, 
            ylim_vec = c(0, 0.30),
            y_axis_break = 0.06)
#  plot_title = "Type 2 diabetes")

