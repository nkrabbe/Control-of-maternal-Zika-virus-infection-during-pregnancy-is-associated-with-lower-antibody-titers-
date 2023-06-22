library(readxl)
library(tidyverse)
library(ggplot2)
library(scales)
library(ggthemes)
library(reshape2)
library(stringr)
library(dplyr)
library(plotrix)
library(stats)
library(gtable)
library(grid)
library(multipanelfigure)
library(ggpubr)
library(cowplot)
library(gridExtra)

###
# EC90 and EC50 titers overtime based on virologic control group
###
EC_values_group <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\EC Titers Overtime (Final)\\Combined EC90 and EC50 Values (Final).xlsx", sheet = 3, col_names = TRUE)
EC_values_group$Timepoint_DPI = as.character(EC_values_group$Timepoint_DPI)
EC_values_group$Timepoint_DPI <- factor(EC_values_group$Timepoint_DPI, levels = c('Pre-Infection', '2-4', '7-10', '13-17', '18-24', '27-38', '52-66', '84-94', '98-117', '121-136'))
EC_values_group$Virologic_Control_Group = as.character(EC_values_group$Virologic_Control_Group)
EC_values_group$Virologic_Control_Group <- factor(EC_values_group$Virologic_Control_Group, levels = c('Controller', 'Non-Controller'))
EC_values_group$Estimated_Reciprocal_Serum_Dilution = as.numeric(EC_values_group$Estimated_Reciprocal_Serum_Dilution)

#EC90 Titers
EC90_values_group <- filter(EC_values_group, EC == '90')
EC90_values_group_summary <- 
  EC90_values_group %>%
  group_by(Timepoint_DPI, Virologic_Control_Group) %>%
  summarize(median = median(Estimated_Reciprocal_Serum_Dilution, na.rm = TRUE))
#EC90 Titers- Statistical analyses comapring virologic control groups at each tested timepoint (Mann-Whitney U Test)
#pre-infection stats
EC90_values_group_preinfection <- filter(EC90_values_group, Timepoint_DPI == 'Pre-Infection')
EC90_values_group_preinfection_controller <- filter(EC90_values_group_preinfection, Virologic_Control_Group == 'Controller')
EC90_values_group_preinfection_noncontroller <- filter(EC90_values_group_preinfection, Virologic_Control_Group == 'Non-Controller')
EC90_values_group_preinfection_controller_quantile <-
  quantile(EC90_values_group_preinfection_controller$Estimated_Reciprocal_Serum_Dilution, probs= c(0.25, 0.75))
EC90_values_group_preinfection_controller_quantile
EC90_values_group_preinfection_noncontroller_quantile <-
  quantile(EC90_values_group_preinfection_noncontroller$Estimated_Reciprocal_Serum_Dilution, probs= c(0.25, 0.75), na.rm = TRUE)
EC90_values_group_preinfection_noncontroller_quantile
wilcox.test(Estimated_Reciprocal_Serum_Dilution~Virologic_Control_Group, data = EC90_values_group_preinfection, exact = FALSE)
# 2-4 DPI stats
EC90_values_group_2.4dpi <- filter(EC90_values_group, Timepoint_DPI == '2-4')
EC90_values_group_2.4dpi_controller <- filter(EC90_values_group_2.4dpi, Virologic_Control_Group == 'Controller')
EC90_values_group_2.4dpi_noncontroller <- filter(EC90_values_group_2.4dpi, Virologic_Control_Group == 'Non-Controller')
EC90_values_group_2.4dpi_controller_quantile <-
  quantile(EC90_values_group_2.4dpi_controller$Estimated_Reciprocal_Serum_Dilution, probs= c(0.25, 0.75))
EC90_values_group_2.4dpi_controller_quantile
EC90_values_group_2.4dpi_noncontroller_quantile <-
  quantile(EC90_values_group_2.4dpi_noncontroller$Estimated_Reciprocal_Serum_Dilution, probs= c(0.25, 0.75), na.rm = TRUE)
EC90_values_group_2.4dpi_noncontroller_quantile
wilcox.test(Estimated_Reciprocal_Serum_Dilution~Virologic_Control_Group, data = EC90_values_group_2.4dpi, exact = FALSE)
# 7-10 DPI stats
EC90_values_group_7.10dpi <- filter(EC90_values_group, Timepoint_DPI == '7-10')
EC90_values_group_7.10dpi_controller <- filter(EC90_values_group_7.10dpi, Virologic_Control_Group == 'Controller')
EC90_values_group_7.10dpi_noncontroller <- filter(EC90_values_group_7.10dpi, Virologic_Control_Group == 'Non-Controller')
EC90_values_group_7.10dpi_controller_quantile <-
  quantile(EC90_values_group_7.10dpi_controller$Estimated_Reciprocal_Serum_Dilution, probs= c(0.25, 0.75))
EC90_values_group_7.10dpi_controller_quantile
EC90_values_group_7.10dpi_noncontroller_quantile <-
  quantile(EC90_values_group_7.10dpi_noncontroller$Estimated_Reciprocal_Serum_Dilution, probs= c(0.25, 0.75), na.rm = TRUE)
EC90_values_group_7.10dpi_noncontroller_quantile
wilcox.test(Estimated_Reciprocal_Serum_Dilution~Virologic_Control_Group, data = EC90_values_group_7.10dpi, exact = FALSE)
# 13-17 DPI stats
EC90_values_group_13.17dpi <- filter(EC90_values_group, Timepoint_DPI == '13-17')
EC90_values_group_13.17dpi_controller <- filter(EC90_values_group_13.17dpi, Virologic_Control_Group == 'Controller')
EC90_values_group_13.17dpi_noncontroller <- filter(EC90_values_group_13.17dpi, Virologic_Control_Group == 'Non-Controller')
EC90_values_group_13.17dpi_controller_quantile <-
  quantile(EC90_values_group_13.17dpi_controller$Estimated_Reciprocal_Serum_Dilution, probs= c(0.25, 0.75))
EC90_values_group_13.17dpi_controller_quantile
EC90_values_group_13.17dpi_noncontroller_quantile <-
  quantile(EC90_values_group_13.17dpi_noncontroller$Estimated_Reciprocal_Serum_Dilution, probs= c(0.25, 0.75), na.rm = TRUE)
EC90_values_group_13.17dpi_noncontroller_quantile
wilcox.test(Estimated_Reciprocal_Serum_Dilution~Virologic_Control_Group, data = EC90_values_group_13.17dpi, exact = FALSE)
# 18-24 DPI stats
EC90_values_group_18.24dpi <- filter(EC90_values_group, Timepoint_DPI == '18-24')
EC90_values_group_18.24dpi_controller <- filter(EC90_values_group_18.24dpi, Virologic_Control_Group == 'Controller')
EC90_values_group_18.24dpi_noncontroller <- filter(EC90_values_group_18.24dpi, Virologic_Control_Group == 'Non-Controller')
EC90_values_group_18.24dpi_controller_quantile <-
  quantile(EC90_values_group_18.24dpi_controller$Estimated_Reciprocal_Serum_Dilution, probs= c(0.25, 0.75))
EC90_values_group_18.24dpi_controller_quantile
EC90_values_group_18.24dpi_noncontroller_quantile <-
  quantile(EC90_values_group_18.24dpi_noncontroller$Estimated_Reciprocal_Serum_Dilution, probs= c(0.25, 0.75), na.rm = TRUE)
EC90_values_group_18.24dpi_noncontroller_quantile
wilcox.test(Estimated_Reciprocal_Serum_Dilution~Virologic_Control_Group, data = EC90_values_group_18.24dpi, exact = FALSE)
# 27-38 DPI stats ### Significant
EC90_values_group_27.38dpi <- filter(EC90_values_group, Timepoint_DPI == '27-38')
EC90_values_group_27.38dpi_controller <- filter(EC90_values_group_27.38dpi, Virologic_Control_Group == 'Controller')
EC90_values_group_27.38dpi_noncontroller <- filter(EC90_values_group_27.38dpi, Virologic_Control_Group == 'Non-Controller')
EC90_values_group_27.38dpi_controller_quantile <-
  quantile(EC90_values_group_27.38dpi_controller$Estimated_Reciprocal_Serum_Dilution, probs= c(0.25, 0.75))
EC90_values_group_27.38dpi_controller_quantile
EC90_values_group_27.38dpi_noncontroller_quantile <-
  quantile(EC90_values_group_27.38dpi_noncontroller$Estimated_Reciprocal_Serum_Dilution, probs= c(0.25, 0.75), na.rm = TRUE)
EC90_values_group_27.38dpi_noncontroller_quantile
wilcox.test(Estimated_Reciprocal_Serum_Dilution~Virologic_Control_Group, data = EC90_values_group_27.38dpi, exact = FALSE)
# 52-66 DPI stats #Significant
EC90_values_group_52.66dpi <- filter(EC90_values_group, Timepoint_DPI == '52-66')
EC90_values_group_52.66dpi_controller <- filter(EC90_values_group_52.66dpi, Virologic_Control_Group == 'Controller')
EC90_values_group_52.66dpi_noncontroller <- filter(EC90_values_group_52.66dpi, Virologic_Control_Group == 'Non-Controller')
EC90_values_group_52.66dpi_controller_quantile <-
  quantile(EC90_values_group_52.66dpi_controller$Estimated_Reciprocal_Serum_Dilution, probs= c(0.25, 0.75), na.rm= TRUE)
EC90_values_group_52.66dpi_controller_quantile
EC90_values_group_52.66dpi_noncontroller_quantile <-
  quantile(EC90_values_group_52.66dpi_noncontroller$Estimated_Reciprocal_Serum_Dilution, probs= c(0.25, 0.75), na.rm = TRUE)
EC90_values_group_52.66dpi_noncontroller_quantile
wilcox.test(Estimated_Reciprocal_Serum_Dilution~Virologic_Control_Group, data = EC90_values_group_52.66dpi, exact = FALSE, na.rm= TRUE)
#84-94 DPI stats
EC90_values_group_84.94dpi <- filter(EC90_values_group, Timepoint_DPI == '84-94')
EC90_values_group_84.94dpi_controller <- filter(EC90_values_group_84.94dpi, Virologic_Control_Group == 'Controller')
EC90_values_group_84.94dpi_noncontroller <- filter(EC90_values_group_84.94dpi, Virologic_Control_Group == 'Non-Controller')
EC90_values_group_84.94dpi_controller_quantile <-
  quantile(EC90_values_group_84.94dpi_controller$Estimated_Reciprocal_Serum_Dilution, probs= c(0.25, 0.75), na.rm= TRUE)
EC90_values_group_84.94dpi_controller_quantile
EC90_values_group_84.94dpi_noncontroller_quantile <-
  quantile(EC90_values_group_84.94dpi_noncontroller$Estimated_Reciprocal_Serum_Dilution, probs= c(0.25, 0.75), na.rm = TRUE)
EC90_values_group_84.94dpi_noncontroller_quantile
wilcox.test(Estimated_Reciprocal_Serum_Dilution~Virologic_Control_Group, data = EC90_values_group_84.94dpi, exact = FALSE, na.rm= TRUE)
# 98-117 DPI stats #significant 
EC90_values_group_98.117dpi <- filter(EC90_values_group, Timepoint_DPI == '98-117')
EC90_values_group_98.117dpi_controller <- filter(EC90_values_group_98.117dpi, Virologic_Control_Group == 'Controller')
EC90_values_group_98.117dpi_noncontroller <- filter(EC90_values_group_98.117dpi, Virologic_Control_Group == 'Non-Controller')
EC90_values_group_98.117dpi_controller_quantile <-
  quantile(EC90_values_group_98.117dpi_controller$Estimated_Reciprocal_Serum_Dilution, probs= c(0.25, 0.75), na.rm= TRUE)
EC90_values_group_98.117dpi_controller_quantile
EC90_values_group_98.117dpi_noncontroller_quantile <-
  quantile(EC90_values_group_98.117dpi_noncontroller$Estimated_Reciprocal_Serum_Dilution, probs= c(0.25, 0.75), na.rm = TRUE)
EC90_values_group_98.117dpi_noncontroller_quantile
wilcox.test(Estimated_Reciprocal_Serum_Dilution~Virologic_Control_Group, data = EC90_values_group_98.117dpi, exact = FALSE, na.rm= TRUE)
# 121-136 DPI stats
EC90_values_group_121.136dpi <- filter(EC90_values_group, Timepoint_DPI == '121-136')
EC90_values_group_121.136dpi_controller <- filter(EC90_values_group_121.136dpi, Virologic_Control_Group == 'Controller')
EC90_values_group_121.136dpi_noncontroller <- filter(EC90_values_group_121.136dpi, Virologic_Control_Group == 'Non-Controller')
EC90_values_group_121.136dpi_controller_quantile <-
  quantile(EC90_values_group_121.136dpi_controller$Estimated_Reciprocal_Serum_Dilution, probs= c(0.25, 0.75), na.rm= TRUE)
EC90_values_group_121.136dpi_controller_quantile
EC90_values_group_121.136dpi_noncontroller_quantile <-
  quantile(EC90_values_group_121.136dpi_noncontroller$Estimated_Reciprocal_Serum_Dilution, probs= c(0.25, 0.75), na.rm = TRUE)
EC90_values_group_121.136dpi_noncontroller_quantile
wilcox.test(Estimated_Reciprocal_Serum_Dilution~Virologic_Control_Group, data = EC90_values_group_121.136dpi, exact = FALSE, na.rm= TRUE)
#EC90 values overtime figure
EC90_values_group_figure <-
  ggplot() + 
  geom_boxplot(data = EC90_values_group, aes(x= Timepoint_DPI, y= Estimated_Reciprocal_Serum_Dilution, color = Virologic_Control_Group), fill = 'white', outlier.color = 'white') + 
  geom_point(data = EC90_values_group, aes(x= Timepoint_DPI, y= Estimated_Reciprocal_Serum_Dilution, color = Virologic_Control_Group), position = position_jitterdodge(seed = 123, jitter.width = 0.35), size= 2, shape= 16) +
  theme_classic() + 
  annotation_logticks(base = 10, sides = 'l', scaled = TRUE, short = unit(-0.5, "cm"), mid = unit(-0.5, "cm"), long = unit(-0.5, "cm")) + 
  scale_y_log10(name = "IgG EC90 Titer", breaks = trans_breaks("log10", function(x) 10^x), labels = trans_format("log10", math_format(10^.x)), expand = c(0,0), limits = c(1,200000)) + 
  scale_color_manual(values = c("#000000", "#b5b5b5"), breaks = c('Controller', 'Non-Controller')) +
  scale_x_discrete(name = 'Timepoint (DPI)', expand = c(0,0)) + 
  guides(color = guide_legend('Virologic Control Group')) + 
  theme(axis.text.x = element_text(size = 20, angle = 45, hjust = 1, vjust = 1), axis.text.y = element_text(size = 20), axis.title = element_text(size = 20, face = 'bold'), aspect.ratio = 1, legend.title = element_text(size = 20, face = 'bold'), legend.text = element_text(size = 20), legend.position = 'top') +
  geom_hline(yintercept = 12.5, linetype= 'dotted') +
  geom_segment(aes(x= 0.75, xend = 1.75, y= 10.715, yend= 9.845), color = '#000000') + 
  geom_segment(aes(x= 1.75, xend = 2.75, y= 9.845, yend= 9.270), color = '#000000') + 
  geom_segment(aes(x= 2.75, xend = 3.75, y= 9.270, yend= 9.475), color = '#000000') + 
  geom_segment(aes(x= 3.75, xend = 4.75, y= 9.475, yend= 16.495), color = '#000000') +
  geom_segment(aes(x= 4.75, xend= 5.75, y= 16.495, yend= 59.59), color = '#000000') + 
  geom_segment(aes(x= 5.75, xend= 6.75, y= 59.59, yend= 112.2), color = '#000000') +
  geom_segment(aes(x= 6.75, xend= 7.75, y= 112.2, yend= 42.7), color = '#000000') +
  geom_segment(aes(x= 7.75, xend= 8.75, y= 42.7, yend= 26.15), color = '#000000') +
  geom_segment(aes(x= 8.75, xend= 9.75, y= 26.15, yend= 57.54), color = '#000000') +
  geom_segment(aes(x= 1.25, xend= 2.25, y= 8.65, yend= 9.86), color = '#b5b5b5') + 
  geom_segment(aes(x= 2.25, xend= 3.25, y= 9.86, yend= 9.07), color = '#b5b5b5') +
  geom_segment(aes(x= 3.25, xend= 4.25, y= 9.07, yend= 19.5), color = '#b5b5b5') + 
  geom_segment(aes(x= 4.25, xend= 5.25, y= 19.5, yend= 64.6), color = '#b5b5b5') +
  geom_segment(aes(x= 5.25, xend= 6.25, y= 64.6, yend= 234), color= '#b5b5b5') +
  geom_segment(aes(x= 6.25, xend= 7.25, y= 234, yend= 266.95), color = '#b5b5b5') +
  geom_segment(aes(x= 7.25, xend= 8.25, y= 266.95, yend= 229.1), color = '#b5b5b5') + 
  geom_segment(aes(x= 8.25, xend= 9.25, y= 229.1, yend= 194.6), color = '#b5b5b5') + 
  geom_segment(aes(x= 9.25, xend= 10.25, y= 194.6, yend= 145.8), color = '#b5b5b5') +
  geom_segment(aes(x= 4.75, xend= 5.25, y= 2500, yend= 2500), color = 'black') + 
  geom_segment(aes(x= 4.75, xend= 4.75, y= 2500, yend= 2200), color = 'black') +
  geom_segment(aes(x= 5.25, xend= 5.25, y= 2500, yend= 2200), color = 'black') + 
  geom_text(aes(x= 5, y= 2750, label = '*'), size= 6) +
  geom_segment(aes(x= 5.75, xend= 6.25, y= 2500, yend= 2500), color = 'black') + 
  geom_segment(aes(x= 5.75, xend= 5.75, y= 2500, yend= 2200), color = 'black') +
  geom_segment(aes(x= 6.25, xend= 6.25, y= 2500, yend= 2200), color = 'black') + 
  geom_text(aes(x= 6, y= 2750, label = '***'), size= 6) +
  geom_segment(aes(x= 6.75, xend= 7.25, y= 2500, yend= 2500), color = 'black') +
  geom_segment(aes(x= 6.75, xend= 6.75, y= 2500, yend= 2200), color = 'black') +
  geom_segment(aes(x= 7.25, xend= 7.25, y= 2500, yend= 2200), color = 'black') +
  geom_text(aes(x= 7, y= 2750, label = '*'), size= 6) +
  geom_segment(aes(x= 8.75, xend= 9.25, y= 2500, yend = 2500), color = 'black') +
  geom_segment(aes(x= 8.75, xend= 8.75, y= 2500, yend = 2200), color = 'black') +
  geom_segment(aes(x= 9.25, xend= 9.25, y= 2500, yend= 2200), color = 'black') +
  geom_text(aes(x=9, y= 2750, label = '**'), size= 6)
EC90_values_group_figure 

#EC50 Titers
EC50_values_group <- filter(EC_values_group, EC == '50')
EC50_valuess_group_summary <-
  EC50_values_group %>%
  group_by(Timepoint_DPI, Virologic_Control_Group) %>%
  summarize(median = median(Estimated_Reciprocal_Serum_Dilution, na.rm = TRUE))
#EC50 Titers- Statistical analyses comparing virologic control groups at each tested timepoint (Mann-Whitney U Test)
#pre-infection stats
EC50_values_group_preinfection <- filter(EC50_values_group, Timepoint_DPI == 'Pre-Infection')
EC50_values_group_preinfection_controller <- filter(EC50_values_group_preinfection, Virologic_Control_Group == 'Controller')
EC50_values_group_preinfection_noncontroller <- filter(EC50_values_group_preinfection, Virologic_Control_Group == 'Non-Controller')
EC50_values_group_preinfection_controller_quantile <-
  quantile(EC50_values_group_preinfection_controller$Estimated_Reciprocal_Serum_Dilution, probs= c(0.25, 0.75))
EC50_values_group_preinfection_controller_quantile
EC50_values_group_preinfection_noncontroller_quantile <-
  quantile(EC50_values_group_preinfection_noncontroller$Estimated_Reciprocal_Serum_Dilution, probs= c(0.25, 0.75), na.rm = TRUE)
EC50_values_group_preinfection_noncontroller_quantile
wilcox.test(Estimated_Reciprocal_Serum_Dilution~Virologic_Control_Group, data = EC50_values_group_preinfection, exact = FALSE)
# 2-4 DPI stats
EC50_values_group_2.4dpi <- filter(EC50_values_group, Timepoint_DPI == '2-4')
EC50_values_group_2.4dpi_controller <- filter(EC50_values_group_2.4dpi, Virologic_Control_Group == 'Controller')
EC50_values_group_2.4dpi_noncontroller <- filter(EC50_values_group_2.4dpi, Virologic_Control_Group == 'Non-Controller')
EC50_values_group_2.4dpi_controller_quantile <-
  quantile(EC50_values_group_2.4dpi_controller$Estimated_Reciprocal_Serum_Dilution, probs= c(0.25, 0.75))
EC50_values_group_2.4dpi_controller_quantile
EC50_values_group_2.4dpi_noncontroller_quantile <-
  quantile(EC50_values_group_2.4dpi_noncontroller$Estimated_Reciprocal_Serum_Dilution, probs= c(0.25, 0.75), na.rm = TRUE)
EC50_values_group_2.4dpi_noncontroller_quantile
wilcox.test(Estimated_Reciprocal_Serum_Dilution~Virologic_Control_Group, data = EC50_values_group_2.4dpi, exact = FALSE)
# 7-10 DPI stats
EC50_values_group_7.10dpi <- filter(EC50_values_group, Timepoint_DPI == '7-10')
EC50_values_group_7.10dpi_controller <- filter(EC50_values_group_7.10dpi, Virologic_Control_Group == 'Controller')
EC50_values_group_7.10dpi_noncontroller <- filter(EC50_values_group_7.10dpi, Virologic_Control_Group == 'Non-Controller')
EC50_values_group_7.10dpi_controller_quantile <-
  quantile(EC50_values_group_7.10dpi_controller$Estimated_Reciprocal_Serum_Dilution, probs= c(0.25, 0.75))
EC50_values_group_7.10dpi_controller_quantile
EC50_values_group_7.10dpi_noncontroller_quantile <-
  quantile(EC50_values_group_7.10dpi_noncontroller$Estimated_Reciprocal_Serum_Dilution, probs= c(0.25, 0.75), na.rm = TRUE)
EC50_values_group_7.10dpi_noncontroller_quantile
wilcox.test(Estimated_Reciprocal_Serum_Dilution~Virologic_Control_Group, data = EC50_values_group_7.10dpi, exact = FALSE)
# 13-17 DPI stats
EC50_values_group_13.17dpi <- filter(EC50_values_group, Timepoint_DPI == '13-17')
EC50_values_group_13.17dpi_controller <- filter(EC50_values_group_13.17dpi, Virologic_Control_Group == 'Controller')
EC50_values_group_13.17dpi_noncontroller <- filter(EC50_values_group_13.17dpi, Virologic_Control_Group == 'Non-Controller')
EC50_values_group_13.17dpi_controller_quantile <-
  quantile(EC50_values_group_13.17dpi_controller$Estimated_Reciprocal_Serum_Dilution, probs= c(0.25, 0.75))
EC50_values_group_13.17dpi_controller_quantile
EC50_values_group_13.17dpi_noncontroller_quantile <-
  quantile(EC50_values_group_13.17dpi_noncontroller$Estimated_Reciprocal_Serum_Dilution, probs= c(0.25, 0.75), na.rm = TRUE)
EC50_values_group_13.17dpi_noncontroller_quantile
wilcox.test(Estimated_Reciprocal_Serum_Dilution~Virologic_Control_Group, data = EC50_values_group_13.17dpi, exact = FALSE)
# 18-24 DPI stats
EC50_values_group_18.24dpi <- filter(EC50_values_group, Timepoint_DPI == '18-24')
EC50_values_group_18.24dpi_controller <- filter(EC50_values_group_18.24dpi, Virologic_Control_Group == 'Controller')
EC50_values_group_18.24dpi_noncontroller <- filter(EC50_values_group_18.24dpi, Virologic_Control_Group == 'Non-Controller')
EC50_values_group_18.24dpi_controller_quantile <-
  quantile(EC50_values_group_18.24dpi_controller$Estimated_Reciprocal_Serum_Dilution, probs= c(0.25, 0.75))
EC50_values_group_18.24dpi_controller_quantile
EC50_values_group_18.24dpi_noncontroller_quantile <-
  quantile(EC50_values_group_18.24dpi_noncontroller$Estimated_Reciprocal_Serum_Dilution, probs= c(0.25, 0.75), na.rm = TRUE)
EC50_values_group_18.24dpi_noncontroller_quantile
wilcox.test(Estimated_Reciprocal_Serum_Dilution~Virologic_Control_Group, data = EC50_values_group_18.24dpi, exact = FALSE)
# 27-38 DPI stats ### Significant
EC50_values_group_27.38dpi <- filter(EC50_values_group, Timepoint_DPI == '27-38')
EC50_values_group_27.38dpi_controller <- filter(EC50_values_group_27.38dpi, Virologic_Control_Group == 'Controller')
EC50_values_group_27.38dpi_noncontroller <- filter(EC50_values_group_27.38dpi, Virologic_Control_Group == 'Non-Controller')
EC50_values_group_27.38dpi_controller_quantile <-
  quantile(EC50_values_group_27.38dpi_controller$Estimated_Reciprocal_Serum_Dilution, probs= c(0.25, 0.75))
EC50_values_group_27.38dpi_controller_quantile
EC50_values_group_27.38dpi_noncontroller_quantile <-
  quantile(EC50_values_group_27.38dpi_noncontroller$Estimated_Reciprocal_Serum_Dilution, probs= c(0.25, 0.75), na.rm = TRUE)
EC50_values_group_27.38dpi_noncontroller_quantile
wilcox.test(Estimated_Reciprocal_Serum_Dilution~Virologic_Control_Group, data = EC50_values_group_27.38dpi, exact = FALSE)
# 52-66 DPI stats #Significant
EC50_values_group_52.66dpi <- filter(EC50_values_group, Timepoint_DPI == '52-66')
EC50_values_group_52.66dpi_controller <- filter(EC50_values_group_52.66dpi, Virologic_Control_Group == 'Controller')
EC50_values_group_52.66dpi_noncontroller <- filter(EC50_values_group_52.66dpi, Virologic_Control_Group == 'Non-Controller')
EC50_values_group_52.66dpi_controller_quantile <-
  quantile(EC50_values_group_52.66dpi_controller$Estimated_Reciprocal_Serum_Dilution, probs= c(0.25, 0.75), na.rm= TRUE)
EC50_values_group_52.66dpi_controller_quantile
EC50_values_group_52.66dpi_noncontroller_quantile <-
  quantile(EC50_values_group_52.66dpi_noncontroller$Estimated_Reciprocal_Serum_Dilution, probs= c(0.25, 0.75), na.rm = TRUE)
EC50_values_group_52.66dpi_noncontroller_quantile
wilcox.test(Estimated_Reciprocal_Serum_Dilution~Virologic_Control_Group, data = EC50_values_group_52.66dpi, exact = FALSE, na.rm= TRUE)
#84-94 DPI stats #significant
EC50_values_group_84.94dpi <- filter(EC50_values_group, Timepoint_DPI == '84-94')
EC50_values_group_84.94dpi_controller <- filter(EC50_values_group_84.94dpi, Virologic_Control_Group == 'Controller')
EC50_values_group_84.94dpi_noncontroller <- filter(EC50_values_group_84.94dpi, Virologic_Control_Group == 'Non-Controller')
EC50_values_group_84.94dpi_controller_quantile <-
  quantile(EC50_values_group_84.94dpi_controller$Estimated_Reciprocal_Serum_Dilution, probs= c(0.25, 0.75), na.rm= TRUE)
EC50_values_group_84.94dpi_controller_quantile
EC50_values_group_84.94dpi_noncontroller_quantile <-
  quantile(EC50_values_group_84.94dpi_noncontroller$Estimated_Reciprocal_Serum_Dilution, probs= c(0.25, 0.75), na.rm = TRUE)
EC50_values_group_84.94dpi_noncontroller_quantile
wilcox.test(Estimated_Reciprocal_Serum_Dilution~Virologic_Control_Group, data = EC50_values_group_84.94dpi, exact = FALSE, na.rm= TRUE)
# 98-117 DPI stats #significant 
EC50_values_group_98.117dpi <- filter(EC50_values_group, Timepoint_DPI == '98-117')
EC50_values_group_98.117dpi_controller <- filter(EC50_values_group_98.117dpi, Virologic_Control_Group == 'Controller')
EC50_values_group_98.117dpi_noncontroller <- filter(EC50_values_group_98.117dpi, Virologic_Control_Group == 'Non-Controller')
EC50_values_group_98.117dpi_controller_quantile <-
  quantile(EC50_values_group_98.117dpi_controller$Estimated_Reciprocal_Serum_Dilution, probs= c(0.25, 0.75), na.rm= TRUE)
EC50_values_group_98.117dpi_controller_quantile
EC50_values_group_98.117dpi_noncontroller_quantile <-
  quantile(EC50_values_group_98.117dpi_noncontroller$Estimated_Reciprocal_Serum_Dilution, probs= c(0.25, 0.75), na.rm = TRUE)
EC50_values_group_98.117dpi_noncontroller_quantile
wilcox.test(Estimated_Reciprocal_Serum_Dilution~Virologic_Control_Group, data = EC50_values_group_98.117dpi, exact = FALSE, na.rm= TRUE)
# 121-136 DPI stats
EC50_values_group_121.136dpi <- filter(EC50_values_group, Timepoint_DPI == '121-136')
EC50_values_group_121.136dpi_controller <- filter(EC50_values_group_121.136dpi, Virologic_Control_Group == 'Controller')
EC50_values_group_121.136dpi_noncontroller <- filter(EC50_values_group_121.136dpi, Virologic_Control_Group == 'Non-Controller')
EC50_values_group_121.136dpi_controller_quantile <-
  quantile(EC50_values_group_121.136dpi_controller$Estimated_Reciprocal_Serum_Dilution, probs= c(0.25, 0.75), na.rm= TRUE)
EC50_values_group_121.136dpi_controller_quantile
EC50_values_group_121.136dpi_noncontroller_quantile <-
  quantile(EC50_values_group_121.136dpi_noncontroller$Estimated_Reciprocal_Serum_Dilution, probs= c(0.25, 0.75), na.rm = TRUE)
EC50_values_group_121.136dpi_noncontroller_quantile
wilcox.test(Estimated_Reciprocal_Serum_Dilution~Virologic_Control_Group, data = EC50_values_group_121.136dpi, exact = FALSE, na.rm= TRUE)

#EC50 titers overtime Figure
EC50_values_group_figure <-
  ggplot() +
  geom_boxplot(data = EC50_values_group, aes(x= Timepoint_DPI, y= Estimated_Reciprocal_Serum_Dilution, color = Virologic_Control_Group), fill = 'white', outlier.colour = 'white') +
  geom_point(data = EC50_values_group, aes(x= Timepoint_DPI, y= Estimated_Reciprocal_Serum_Dilution, color = Virologic_Control_Group), position = position_jitterdodge(seed = 123, jitter.width = 0.35), size= 2, shape= 16) +
  theme_classic() + 
  annotation_logticks(base = 10, sides = 'l', scaled = TRUE, short = unit(-0.5, "cm"), mid = unit(-0.5, "cm"), long = unit(-0.5, "cm")) + 
  scale_y_log10(name = "IgG EC50 Titer", breaks = trans_breaks("log10", function(x) 10^x), labels = trans_format("log10", math_format(10^.x)), expand = c(0,0), limits = c(1,200000)) +
  scale_color_manual(values = c('#000000', '#b5b5b5'), breaks = c('Controller', 'Non-Controller')) +
  scale_x_discrete(name = 'Timepoint (DPI)', expand = c(0,0)) + 
  guides(color = 'none') +
  theme(axis.text.x = element_text(size = 20, angle = 45, hjust = 1, vjust = 1), axis.text.y = element_text(size = 20), axis.title = element_text(size = 20, face = 'bold'), aspect.ratio = 1) +
  geom_segment(aes(x= 0.75, xend= 1.75, y= 32.04, yend = 33.965), color = '#000000') + 
  geom_segment(aes(x= 1.75, xend= 2.75, y= 33.965, yend= 32.39), color = '#000000') +
  geom_segment(aes(x= 2.75, xend= 3.75, y= 32.39, yend= 66.223), color = '#000000') +
  geom_segment(aes(x= 3.75, xend= 4.75, y= 66.225, yend= 119.05), color = '#000000') +
  geom_segment(aes(x= 4.75, xend= 5.75, y= 119.05, yend= 410.45), color = '#000000') +
  geom_segment(aes(x= 5.75, xend= 6.75, y= 410.45, yend= 549.5), color = '#000000') +
  geom_segment(aes(x= 6.75, xend= 7.75, y= 549.5, yend= 195), color = '#000000') + 
  geom_segment(aes(x= 7.75, xend= 8.75, y= 195, yend= 188.35), color = '#000000') +
  geom_segment(aes(x= 8.75, xend= 9.75, y= 188.35, yend= 281.8), color = '#000000') +
  geom_segment(aes(x= 1.25, xend= 2.25, y= 29.51, yend= 29.51), color = '#b5b5b5') +
  geom_segment(aes(x= 2.25, xend= 3.25, y= 29.51, yend= 29.85), color = '#b5b5b5') + 
  geom_segment(aes(x= 3.25, xend= 4.25, y= 29.85, yend= 159.65), color = '#b5b5b5') + 
  geom_segment(aes(x= 4.25, xend= 5.25, y= 159.65, yend= 346.7), color = '#b5b5b5') + 
  geom_segment(aes(x= 5.25, xend= 6.25, y= 346.7, yend= 2158.4), color = '#b5b5b5') + 
  geom_segment(aes(x= 6.25, xend= 7.25, y= 2158.4, yend= 2398.8), color = '#b5b5b5') +
  geom_segment(aes(x= 7.25, xend= 8.25, y= 2398.8, yend= 2630.3), color = '#b5b5b5') +
  geom_segment(aes(x= 8.25, xend= 9.25, y= 2630.3, yend= 1571.8), color = '#b5b5b5') +
  geom_segment(aes(x= 9.25, xend= 10.25, y= 1571.8, yend= 934.25), color = '#b5b5b5') +
  geom_segment(aes(x=5.75, xend=6.25, y= 14000, yend= 14000), color = 'black') +
  geom_segment(aes(x= 5.75, xend= 5.75, y= 14000, yend= 12000), color = 'black') +
  geom_segment(aes(x= 6.25, xend= 6.25, y= 14000, yend= 12000), color = 'black') +
  geom_text(aes(x= 6, y= 15000, label = '**'), size= 6) +
  geom_segment(aes(x=6.75, xend=7.25, y= 14000, yend= 14000), color = 'black') +
  geom_segment(aes(x= 6.75, xend= 6.75, y= 14000, yend= 12000), color = 'black') +
  geom_segment(aes(x= 7.25, xend= 7.25, y= 14000, yend= 12000), color = 'black') +
  geom_text(aes(x= 7, y= 15000, label = '*'), size= 6) + 
  geom_segment(aes(x=7.75, xend=8.25, y= 14000, yend= 14000), color = 'black') +
  geom_segment(aes(x= 7.75, xend= 7.75, y= 14000, yend= 12000), color = 'black') +
  geom_segment(aes(x= 8.25, xend= 8.25, y= 14000, yend= 12000), color = 'black') +
  geom_text(aes(x= 8, y= 15000, label = '*'), size= 6) +
  geom_segment(aes(x=8.75, xend=9.25, y= 14000, yend= 14000), color = 'black') +
  geom_segment(aes(x= 8.75, xend= 8.75, y= 14000, yend= 12000), color = 'black') +
  geom_segment(aes(x= 9.25, xend= 9.25, y= 14000, yend= 12000), color = 'black') +
  geom_text(aes(x= 9, y= 15000, label = '**'), size = 6)
EC50_values_group_figure

###
# Combined EC titers overtime in a multipanel figure
###
tiff('EC Titers Overtime-Group (Final).tiff', units= 'in', width = 15, height= 10, res= 300)
ggarrange(EC90_values_group_figure, EC50_values_group_figure, nrow= 1, ncol= 2, common.legend= TRUE, legend= 'bottom', align = 'hv', labels = c('A', 'B'))
dev.off()

