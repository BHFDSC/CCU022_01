### Author: Pearl Liu
### Make calibration plots ###
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

#### Read data ####
calib_summ_by_decile <- read.csv(paste0(read_dir, "All_outcomes_calibration_results_by_decile.csv"))
calib_summ_by_20tiles <- read.csv(paste0(read_dir, "All_outcomes_calibration_results_by_20tiles.csv"))


#### Make plots ####
##### Plot functions #####
## fig_width and fig_height consistent with cuminc_plot_v2 in Code 2.11
calib_plot <- function(target_ds, 
                        calib_summ, 
                       dodge = position_dodge(0.8),
                       point_size = base_point_size,
                        base_size = base_font_size,
                       ref_linewidth = 0.6,
                       metric_lower_lim = 0,
                       metric_upper_lim = 0.3,
                       metric_breaks = 0.05,
                        save_format = "pdf",
                       fig_width = 5,
                       fig_height = 4.56,
                        plot_type = "plot only") {
  target_ds_calib_summ <- calib_summ %>%
    filter(Outcome_phecode == target_ds)
  
  x_axis_label <-  seq(metric_lower_lim, metric_upper_lim, metric_breaks) * 100
  y_axis_label <-  seq(metric_lower_lim, metric_upper_lim, metric_breaks) * 100 
  
  calib_p <- ggplot(target_ds_calib_summ, 
                    aes(x = mean_pred_risk,
                        y = cuminc)) + 
    geom_abline(slope = 1, intercept = 0, 
                linewidth = ref_linewidth) +
    geom_point(size = point_size, color = grey) + 
  #  geom_errorbar(aes(x = mean_pred_risk, 
  #                    ymin = cuminc_LB, 
  #                    ymax = cuminc_UB),
                 # position = dodge, 
  #                width=.2) +
    geom_line(linewidth = ref_linewidth, color = grey) +
    theme_bw(base_size = base_size) +
    theme(legend.title=element_text(size=24), 
          legend.text=element_text(size=20),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          legend.key.spacing.y = unit(0.3, 'cm')) + 
    scale_y_continuous(limits = c(metric_lower_lim, metric_upper_lim), 
                       breaks = seq(metric_lower_lim, metric_upper_lim, metric_breaks),
                       labels = format(y_axis_label, nsmall = 0)) +
    scale_x_continuous(limits = c(metric_lower_lim, metric_upper_lim), 
                       breaks = seq(metric_lower_lim, metric_upper_lim, metric_breaks),
                       labels = format(x_axis_label, nsmall = 0)) + 
    labs(x = "Predicted events (%)",
         y = "Observed events (%)")
    
    ggsave(filename = paste0(figure_dir, "/Example_outcomes/Calib_", target_ds, ".", save_format), 
           calib_p, width = fig_width, height = fig_height, limitsize = FALSE)

  
}

##### Fig. 5b: Make plots #####
calib_plot(target_ds = "COVID_hspt", 
           calib_summ_by_20tiles, 
           metric_lower_lim = 0,
           metric_upper_lim = 0.1,
           metric_breaks = 0.02)

calib_plot(target_ds = "death", 
           calib_summ_by_20tiles, 
           metric_lower_lim = 0,
           metric_upper_lim = 0.5,
           metric_breaks = 0.1)

calib_plot(target_ds = "165_1", 
           calib_summ_by_20tiles, 
           metric_lower_lim = 0,
           metric_upper_lim = 0.12,
           metric_breaks = 0.03)

calib_plot(target_ds = "411_2", 
           calib_summ_by_20tiles, 
           metric_lower_lim = 0,
           metric_upper_lim = 0.2,
           metric_breaks = 0.04)

calib_plot(target_ds = "250_2", 
           calib_summ_by_20tiles, 
           metric_lower_lim = 0,
           metric_upper_lim = 0.32,
           metric_breaks = 0.06)


