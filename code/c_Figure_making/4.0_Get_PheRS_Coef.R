### Author: Pearl Liu
### Get predictors and coefficients from PheRS study

#### Load packages and parameters ####
library(tidyverse)
library(gtools)
library(readxl)

summary_dir <- 'Summary_Stats/'

#### Read input files ####
phers_coef_summ <- read_excel("PheRS_compare/41588_2025_2298_MOESM4_ESM_ForAnalysis.xlsx",
                              sheet = 12,
                              skip = 3, na = "NA")

#### Get list of predictor phecodes for target outcomes ####
phers_predictor_list <- phers_coef_summ %>%
  ## Remove not used outcomes
  filter(!(Disease %in% c("Major depression (MDD)", "Epilepsy"))) %>%
  select(UKB_coef, Code, Phenotype, Category) %>%
  na.omit() %>%
  select(Code, Phenotype, Category) %>%
  distinct() %>%
  filter(Category != "baseline") %>%
  arrange(as.numeric(Code))
  
write.csv(phers_predictor_list, 'Inputs/For SDE/PheRS_Predictor_PheCodes.csv',
          row.names = F)

#### Get coefficients of predictor phecodes for target outcomes ####
mixedrank <- function(x) order(gtools::mixedorder(x))

phers_coef_select <- phers_coef_summ %>%
  ## Remove not used outcomes
  filter(!(Disease %in% c("Major depression (MDD)", "Epilepsy"))) %>%
  select(Disease, UKB_coef, Code, Phenotype, Category) %>%
  na.omit() %>%
  distinct() %>%
  arrange(Disease, mixedrank(Code))

write.csv(phers_coef_select, 'Inputs/For SDE/PheRS_Coef_final.csv',
          row.names = F)

#### Fig. S7: CRS vs. PheRS effect sizes for T2D ####
##### Read effect sizes #####
crs_summ_final <- readRDS(paste0(summary_dir,
                                 "All_Outcomes+COVID_WeightedLogisticRegression_SummStats_ControlCase4to1_WhiteBritish_OddYOB_Index2015_Followup10yr.rds")) %>%
  mutate(Est_scaleX = Est * sqrt(Predictor_prev * (1 - Predictor_prev))) 

phers_coef_select <- read.csv('Inputs/For SDE/PheRS_Coef_final.csv')
phers_coef_compare <- phers_coef_select %>%
  filter(Category != "baseline") %>%
  mutate(Predictor = paste0("ds", Code))

##### Combine two effect sizes #####
t2d_crs_phers_coef_compare <- crs_summ_final %>%
  filter(Outcome == "250_2") %>%
  select(Predictor, Est, SE, Est_scaleX, Predictor_phenotype, Predictor_prev) %>%
  inner_join(phers_coef_compare[, c("Disease", "UKB_coef", "Predictor")] %>%
               filter(Disease == "Type 2 diabetes (T2D)"),
             by = "Predictor") %>%
  mutate(PheRS_coef_scaleX = UKB_coef * sqrt(Predictor_prev * (1 - Predictor_prev))) %>%
  mutate(CRS_coef_std = Est/sd(Est), 
         CRS_coef_SE_std = SE/sd(Est),
         PheRS_coef_std = UKB_coef/sd(UKB_coef)) %>%
  mutate(Predictor_phenotype = case_when(Predictor_phenotype == "Overweight, obesity and other hyperalimentation" ~ "Overweight and obesity",
                                         Predictor_phenotype == "Disorders of lipoid metabolism" ~ "Disorders of lipid metabolism",
                                         Predictor_phenotype == "Ischemic Heart Disease" ~ "Ischemic heart disease",
                                         .default = Predictor_phenotype)
         )

##### Plot function #####
CRS_vs_PheRS_effec_size_plot <- function(t2d_crs_phers_coef_compare,
                                                ref_linewidth = 0.6,
                                                y_lower_lim = -3,
                                                y_upper_lim = 5,
                                                y_breaks = 2,
                                                x_lower_lim = -3,
                                                x_upper_lim = 5,
                                                x_breaks = 2,
                                                point_size = base_point_size,
                                                base_size = base_font_size,
                                                text_label_size = 6,
                                                point_padding = 1,
                                                box_padding = 0.5,
                                                nudge_x = 0,
                                                nudge_y = 0,
                                                seed = 100,
                                                model_fit = F,
                                                label_top_N = T,
                                                save_format = "pdf",
                                                fig_width = 7,
                                                fig_height = 6) {
  
  CRS_vs_PheRS_effec_size_plot <- ggplot(t2d_crs_phers_coef_compare, 
                                            aes(x = PheRS_coef_std, 
                                                y = CRS_coef_std)) + 
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
    labs(x = paste0("Scaled effect size (PheRS)"), 
         y = paste0("Scaled effect size (CRS)"))
  
  if (label_top_N == T) {
    top_N_idx <- which(t2d_crs_phers_coef_compare$Predictor %in% c("ds401",
                                                                   "ds278",
                                                                   "ds496",
                                                                   "ds411",
                                                                   "ds272"))
    all_predictor_dt_label <- t2d_crs_phers_coef_compare[top_N_idx,]
    
    nudge_x <- case_when(all_predictor_dt_label$Predictor_phenotype == "Overweight and obesity" ~ nudge_x - 3,
                         all_predictor_dt_label$Predictor_phenotype == "Disorders of lipid metabolism" ~ nudge_x - 3,
                         all_predictor_dt_label$Predictor_phenotype == "Ischemic heart disease" ~ nudge_x + 1,
                         .default = nudge_x)
    nudge_y = case_when(all_predictor_dt_label$Predictor_phenotype == "Overweight and obesity" ~ nudge_y + 0.5,
                        all_predictor_dt_label$Predictor_phenotype == "Disorders of lipid metabolism" ~ nudge_y + 0.8,
                        all_predictor_dt_label$Predictor_phenotype == "Ischemic heart disease" ~ nudge_y - 0.7,
                       .default = nudge_y)
    
    target_predictor <- "Disorders of lipid metabolism"
    
    t2d_crs_phers_coef_compare$label_idx <- if_else(row_number(t2d_crs_phers_coef_compare) %in% top_N_idx, T, F)
    
    CRS_vs_PheRS_effec_size_plot <- CRS_vs_PheRS_effec_size_plot + 
      geom_point(color = ifelse(t2d_crs_phers_coef_compare$label_idx == T, red, grey),
                 size = point_size) + 
      geom_label_repel(data = t2d_crs_phers_coef_compare[top_N_idx,], 
                       aes(label = Predictor_phenotype, 
                           x = PheRS_coef_std, 
                           y = CRS_coef_std,
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
  
  figure_name <- paste0(figure_dir, "/Competing_methods/SuppFig7_CRS_vs_PheRS_effect_sizes_T2D.", save_format)
  
  ggsave(filename = figure_name, 
         CRS_vs_PheRS_effec_size_plot, 
         width = fig_width, 
         height = fig_height, 
         limitsize = FALSE)
}


##### Make plot #####
CRS_vs_PheRS_effec_size_plot(t2d_crs_phers_coef_compare,
                                         ref_linewidth = 0.6,
                                         y_lower_lim = -5,
                                         y_upper_lim = 6,
                                         y_breaks = 2,
                                         x_lower_lim = -5,
                                         x_upper_lim = 6,
                                         x_breaks = 2,
                                         point_size = base_point_size,
                                         base_size = base_font_size,
                                         text_label_size = 6,
                                         point_padding = 1,
                                         box_padding = 0.5,
                                         nudge_x = 0,
                                         nudge_y = 0,
                                         seed = 100,
                                         model_fit = F,
                                         label_top_N = T,
                                         save_format = "pdf",
                                         fig_width = 7.8,
                                         fig_height = 6.8) 


View(t2d_crs_phers_coef_compare)
## see ds272

cor.test(t2d_crs_phers_coef_compare$Est, t2d_crs_phers_coef_compare$UKB_coef)
plot(t2d_crs_phers_coef_compare$Est, t2d_crs_phers_coef_compare$UKB_coef,
     xlab = "CRS Coef", ylab = "PheRS Coef")

plot(
  t2d_crs_phers_coef_compare$UKB_coef / sd(t2d_crs_phers_coef_compare$UKB_coef),
  t2d_crs_phers_coef_compare$Est / sd(t2d_crs_phers_coef_compare$Est), 
  xlab = "PheRS Coef (SD-std)",
  ylab = "CRS Coef (SD-std)", 
  xlim = c(-3, 5), ylim = c(-3, 5)
)
abline(0,1)
text(t2d_crs_phers_coef_compare$UKB_coef / sd(t2d_crs_phers_coef_compare$UKB_coef),
     t2d_crs_phers_coef_compare$Est / sd(t2d_crs_phers_coef_compare$Est), 
     t2d_crs_phers_coef_compare$Predictor_phenotype,
     cex = 0.4, pos = 1)

cor.test(t2d_crs_phers_coef_compare$Est_scaleX, 
         t2d_crs_phers_coef_compare$PheRS_coef_scaleX)
plot(t2d_crs_phers_coef_compare$PheRS_coef_scaleX / sd(t2d_crs_phers_coef_compare$PheRS_coef_scaleX),
     t2d_crs_phers_coef_compare$Est_scaleX / sd(t2d_crs_phers_coef_compare$Est_scaleX), 
     xlab = "PheRS Coef (X-scaled, SD-std)",
     ylab = "CRS Coef (X-scaled, SD-std)",
     xlim = c(-2, 7), ylim = c(-2, 7)
)
abline(0,1)
text(t2d_crs_phers_coef_compare$PheRS_coef_scaleX / sd(t2d_crs_phers_coef_compare$PheRS_coef_scaleX),
     t2d_crs_phers_coef_compare$Est_scaleX / sd(t2d_crs_phers_coef_compare$Est_scaleX), 
     t2d_crs_phers_coef_compare$Predictor_phenotype,
     cex = 0.4, pos = 1)




#### Explore: compare coefficients between PheRS and CRS ####
crs_summ_final <- readRDS(paste0(summary_dir,
                                       "All_Outcomes+COVID_WeightedLogisticRegression_SummStats_ControlCase4to1_WhiteBritish_OddYOB_Index2015_Followup10yr.rds")) %>%
  mutate(Est_scaleX = Est * sqrt(Predictor_prev * (1 - Predictor_prev))) 

phers_coef_select <- read.csv('Inputs/For SDE/PheRS_Coef_final.csv')
phers_coef_compare <- phers_coef_select %>%
  filter(Category != "baseline") %>%
  mutate(Predictor = paste0("ds", Code))

##### Lung cancer #####
lc_crs_phers_coef_compare <- crs_summ_final %>%
  filter(Outcome == "165_1") %>%
  select(Predictor, Est, Est_scaleX, Predictor_phenotype, Predictor_prev) %>%
  inner_join(phers_coef_compare[, c("Disease", "UKB_coef", "Predictor")] %>%
               filter(Disease == "Lung cancer"),
            by = "Predictor") %>%
  mutate(PheRS_coef_scaleX = UKB_coef * sqrt(Predictor_prev * (1 - Predictor_prev)))


cor.test(lc_crs_phers_coef_compare$Est, lc_crs_phers_coef_compare$UKB_coef)
plot(lc_crs_phers_coef_compare$Est, lc_crs_phers_coef_compare$UKB_coef,
     xlab = "CRS Coef", ylab = "PheRS Coef")

plot(lc_crs_phers_coef_compare$UKB_coef, 
     lc_crs_phers_coef_compare$Est, 
     xlab = "PheRS Coef",
     ylab = "CRS Coef", 
     xlim = c(-0.5, 1.5), ylim = c(-0.5, 1.5))
abline(0,1)

cor.test(lc_crs_phers_coef_compare$Est_scaleX, 
         lc_crs_phers_coef_compare$PheRS_coef_scaleX)
plot(
     lc_crs_phers_coef_compare$PheRS_coef_scaleX / sd(lc_crs_phers_coef_compare$PheRS_coef_scaleX),
     lc_crs_phers_coef_compare$Est_scaleX / sd(lc_crs_phers_coef_compare$Est_scaleX), 
     xlab = "PheRS Coef (X-scaled, SD-std)",
     ylab = "CRS Coef (X-scaled, SD-std)", 
     xlim = c(-2, 7), ylim = c(-2, 7)
)
abline(0,1)

boxplot(lc_crs_phers_coef_compare$PheRS_coef_scaleX, 
        xlab = "PheRS Coef (X-scaled)")
boxplot(lc_crs_phers_coef_compare$Est_scaleX,
        xlab = "CRS Coef (X-scaled)")


##### Colorectal cancer #####
crc_crs_phers_coef_compare <- crs_summ_final %>%
  filter(Outcome == "153") %>%
  select(Predictor, Est, Est_scaleX, Predictor_phenotype, Predictor_prev) %>%
  inner_join(phers_coef_compare[, c("Disease", "UKB_coef", "Predictor")] %>%
               filter(Disease == "Colorectal cancer"),
             by = "Predictor") %>%
  mutate(PheRS_coef_scaleX = UKB_coef * sqrt(Predictor_prev * (1 - Predictor_prev)))


cor.test(crc_crs_phers_coef_compare$Est, crc_crs_phers_coef_compare$UKB_coef)
plot(crc_crs_phers_coef_compare$Est, crc_crs_phers_coef_compare$UKB_coef,
     xlab = "CRS Coef", ylab = "PheRS Coef")

plot(
  crc_crs_phers_coef_compare$UKB_coef / sd(crc_crs_phers_coef_compare$UKB_coef),
  crc_crs_phers_coef_compare$Est / sd(crc_crs_phers_coef_compare$Est), 
  xlab = "PheRS Coef (SD-std)",
  ylab = "CRS Coef (SD-std)", 
  xlim = c(-3, 5), ylim = c(-3, 5)
)
abline(0,1)


cor.test(crc_crs_phers_coef_compare$Est_scaleX, 
         crc_crs_phers_coef_compare$PheRS_coef_scaleX)
plot(
  crc_crs_phers_coef_compare$PheRS_coef_scaleX / sd(crc_crs_phers_coef_compare$PheRS_coef_scaleX),
  crc_crs_phers_coef_compare$Est_scaleX / sd(crc_crs_phers_coef_compare$Est_scaleX), 
  xlab = "PheRS Coef (X-scaled, SD-std)",
  ylab = "CRS Coef (X-scaled, SD-std)", 
  xlim = c(-4, 5), ylim = c(-4, 5)
)
abline(0,1)


##### Asthma #####
asthma_crs_phers_coef_compare <- crs_summ_final %>%
  filter(Outcome == "495") %>%
  select(Predictor, Est, Est_scaleX, Predictor_phenotype, Predictor_prev) %>%
  inner_join(phers_coef_compare[, c("Disease", "UKB_coef", "Predictor")] %>%
               filter(Disease == "Asthma"),
             by = "Predictor") %>%
  mutate(PheRS_coef_scaleX = UKB_coef * sqrt(Predictor_prev * (1 - Predictor_prev)))


cor.test(asthma_crs_phers_coef_compare$Est, asthma_crs_phers_coef_compare$UKB_coef)
plot(asthma_crs_phers_coef_compare$Est, asthma_crs_phers_coef_compare$UKB_coef,
     xlab = "CRS Coef", ylab = "PheRS Coef")

plot(
  asthma_crs_phers_coef_compare$UKB_coef / sd(asthma_crs_phers_coef_compare$UKB_coef),
  asthma_crs_phers_coef_compare$Est / sd(asthma_crs_phers_coef_compare$Est), 
  xlab = "PheRS Coef (SD-std)",
  ylab = "CRS Coef (SD-std)", 
  xlim = c(-5, 5), ylim = c(-5, 5)
)
abline(0,1)

plot(asthma_crs_phers_coef_compare$Est, asthma_crs_phers_coef_compare$UKB_coef,
     xlab = "CRS Coef", ylab = "PheRS Coef",
     xlim = c(-0.5, 0.5), ylim = c(-0.5, 0.5))
abline(0,1)

cor.test(asthma_crs_phers_coef_compare$Est_scaleX, 
         asthma_crs_phers_coef_compare$PheRS_coef_scaleX)
plot(
     asthma_crs_phers_coef_compare$PheRS_coef_scaleX / sd(asthma_crs_phers_coef_compare$PheRS_coef_scaleX),
     asthma_crs_phers_coef_compare$Est_scaleX / sd(asthma_crs_phers_coef_compare$Est_scaleX), 
     xlab = "PheRS Coef (X-scaled, SD-std)",
     ylab = "CRS Coef (X-scaled, SD-std)", 
     xlim = c(-3, 5), ylim = c(-3, 5)
)
abline(0,1)


##### Gout #####
gout_crs_phers_coef_compare <- crs_summ_final %>%
  filter(Outcome == "274_1") %>%
  select(Predictor, Est, Est_scaleX, Predictor_phenotype, Predictor_prev) %>%
  inner_join(phers_coef_compare[, c("Disease", "UKB_coef", "Predictor")] %>%
               filter(Disease == "Gout"),
             by = "Predictor") %>%
  mutate(PheRS_coef_scaleX = UKB_coef * sqrt(Predictor_prev * (1 - Predictor_prev)))

cor.test(gout_crs_phers_coef_compare$Est, gout_crs_phers_coef_compare$UKB_coef)
plot(gout_crs_phers_coef_compare$Est, gout_crs_phers_coef_compare$UKB_coef,
     xlab = "CRS Coef", ylab = "PheRS Coef")

plot(gout_crs_phers_coef_compare$Est, gout_crs_phers_coef_compare$UKB_coef,
     xlab = "CRS Coef", ylab = "PheRS Coef",
     xlim = c(-0.5, 0.5), ylim = c(-0.5, 0.5))
abline(0,1)

cor.test(gout_crs_phers_coef_compare$Est_scaleX, 
         gout_crs_phers_coef_compare$PheRS_coef_scaleX)
plot(gout_crs_phers_coef_compare$PheRS_coef_scaleX / sd(gout_crs_phers_coef_compare$PheRS_coef_scaleX),
     gout_crs_phers_coef_compare$Est_scaleX / sd(gout_crs_phers_coef_compare$Est_scaleX), 
     xlab = "PheRS Coef (X-scaled, SD-std)",
     ylab = "CRS Coef (X-scaled, SD-std)",
   #  xlim = c(-0.04, 0.36), ylim = c(-0.04, 0.36))
)
abline(0,1)

boxplot(gout_crs_phers_coef_compare$PheRS_coef_scaleX, 
        xlab = "PheRS Coef (X-scaled)")
boxplot(gout_crs_phers_coef_compare$Est_scaleX,
        xlab = "CRS Coef (X-scaled)")


##### T2D: example? #####
t2d_crs_phers_coef_compare <- crs_summ_final %>%
  filter(Outcome == "250_2") %>%
  select(Predictor, Est, SE, Est_scaleX, Predictor_phenotype, Predictor_prev) %>%
  inner_join(phers_coef_compare[, c("Disease", "UKB_coef", "Predictor")] %>%
               filter(Disease == "Type 2 diabetes (T2D)"),
             by = "Predictor") %>%
  mutate(PheRS_coef_scaleX = UKB_coef * sqrt(Predictor_prev * (1 - Predictor_prev))) %>%
  mutate(Est_std = Est/sd(Est), 
         SE_std = SE/sd(Est),
         PheRS_coef_std = UKB_coef/sd(UKB_coef))

View(t2d_crs_phers_coef_compare)
## see ds272

cor.test(t2d_crs_phers_coef_compare$Est, t2d_crs_phers_coef_compare$UKB_coef)
plot(t2d_crs_phers_coef_compare$Est, t2d_crs_phers_coef_compare$UKB_coef,
     xlab = "CRS Coef", ylab = "PheRS Coef")

plot(
     t2d_crs_phers_coef_compare$UKB_coef / sd(t2d_crs_phers_coef_compare$UKB_coef),
     t2d_crs_phers_coef_compare$Est / sd(t2d_crs_phers_coef_compare$Est), 
     xlab = "PheRS Coef (SD-std)",
     ylab = "CRS Coef (SD-std)", 
     xlim = c(-3, 5), ylim = c(-3, 5)
)
abline(0,1)
text(t2d_crs_phers_coef_compare$UKB_coef / sd(t2d_crs_phers_coef_compare$UKB_coef),
     t2d_crs_phers_coef_compare$Est / sd(t2d_crs_phers_coef_compare$Est), 
     t2d_crs_phers_coef_compare$Predictor_phenotype,
     cex = 0.4, pos = 1)

cor.test(t2d_crs_phers_coef_compare$Est_scaleX, 
         t2d_crs_phers_coef_compare$PheRS_coef_scaleX)
plot(t2d_crs_phers_coef_compare$PheRS_coef_scaleX / sd(t2d_crs_phers_coef_compare$PheRS_coef_scaleX),
     t2d_crs_phers_coef_compare$Est_scaleX / sd(t2d_crs_phers_coef_compare$Est_scaleX), 
     xlab = "PheRS Coef (X-scaled, SD-std)",
     ylab = "CRS Coef (X-scaled, SD-std)",
     xlim = c(-2, 7), ylim = c(-2, 7)
)
abline(0,1)
text(t2d_crs_phers_coef_compare$PheRS_coef_scaleX / sd(t2d_crs_phers_coef_compare$PheRS_coef_scaleX),
     t2d_crs_phers_coef_compare$Est_scaleX / sd(t2d_crs_phers_coef_compare$Est_scaleX), 
     t2d_crs_phers_coef_compare$Predictor_phenotype,
     cex = 0.4, pos = 1)


##### AF #####
af_crs_phers_coef_compare <- crs_summ_final %>%
  filter(Outcome == "427_2") %>%
  select(Predictor, Est, Est_scaleX, Predictor_phenotype, Predictor_prev) %>%
  inner_join(phers_coef_compare[, c("Disease", "UKB_coef", "Predictor")] %>%
               filter(Disease == "Atrial fibrillation (AF)"),
             by = "Predictor") %>%
  mutate(PheRS_coef_scaleX = UKB_coef * sqrt(Predictor_prev * (1 - Predictor_prev)))

cor.test(af_crs_phers_coef_compare$Est, af_crs_phers_coef_compare$UKB_coef)
plot(af_crs_phers_coef_compare$Est, af_crs_phers_coef_compare$UKB_coef,
     xlab = "CRS Coef", ylab = "PheRS Coef")

plot(
  af_crs_phers_coef_compare$UKB_coef / sd(af_crs_phers_coef_compare$UKB_coef),
  af_crs_phers_coef_compare$Est / sd(af_crs_phers_coef_compare$Est), 
  xlab = "PheRS Coef (SD-std)",
  ylab = "CRS Coef (SD-std)", 
  xlim = c(-3, 5), ylim = c(-3, 5)
)
abline(0,1)

cor.test(af_crs_phers_coef_compare$Est_scaleX, 
         af_crs_phers_coef_compare$PheRS_coef_scaleX)
plot(af_crs_phers_coef_compare$PheRS_coef_scaleX / sd(af_crs_phers_coef_compare$PheRS_coef_scaleX),
     af_crs_phers_coef_compare$Est_scaleX / sd(af_crs_phers_coef_compare$Est_scaleX), 
     xlab = "PheRS Coef (X-scaled, SD-std)",
     ylab = "CRS Coef (X-scaled, SD-std)",
     xlim = c(-2, 7), ylim = c(-2, 7)
)
abline(0,1)
