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
# Combining IgG WVE EC titers overtime figures and PRNT titers figures into a single multipanel figure
###

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
  geom_point(data = EC90_values_group, aes(x= Timepoint_DPI, y= Estimated_Reciprocal_Serum_Dilution, color = Virologic_Control_Group), position = position_jitterdodge(seed = 123, jitter.width = 0.35), size= 1, shape= 16) +
  theme_classic() + 
  annotation_logticks(base = 10, sides = 'l', scaled = TRUE, short = unit(-0.5, "cm"), mid = unit(-0.5, "cm"), long = unit(-0.5, "cm")) + 
  scale_y_log10(name = "IgG EC90 Titer", breaks = trans_breaks("log10", function(x) 10^x), labels = trans_format("log10", math_format(10^.x)), expand = c(0,0), limits = c(1,200000)) + 
  scale_color_manual(values = c("black", "orange"), breaks = c('Controller', 'Non-Controller')) +
  scale_x_discrete(name = 'Timepoint (DPI)', expand = c(0,0)) + 
  guides(color = guide_legend('Virologic Control Group')) + 
  theme(axis.text.x = element_text(size = 20, angle = 45, hjust = 1, vjust = 1), axis.text.y = element_text(size = 20), axis.title = element_text(size = 20, face = 'bold'), aspect.ratio = 1, legend.title = element_text(size = 20, face = 'bold'), legend.text = element_text(size = 20), legend.position = 'top') +
  geom_hline(yintercept = 12.5, linetype= 'dotted') +
  geom_segment(aes(x= 0.75, xend = 1.75, y= 10.47, yend= 10), color = 'black') + 
  geom_segment(aes(x= 1.75, xend = 2.75, y= 10, yend= 9.12), color = 'black') + 
  geom_segment(aes(x= 2.75, xend = 3.75, y= 9.12, yend= 10), color = 'black') + 
  geom_segment(aes(x= 3.75, xend = 4.75, y= 10, yend= 18.19), color = 'black') +
  geom_segment(aes(x= 4.75, xend= 5.75, y= 18.19, yend= 60.3), color = 'black') + 
  geom_segment(aes(x= 5.75, xend= 6.75, y= 60.3, yend= 100.65), color = 'black') +
  geom_segment(aes(x= 6.75, xend= 7.75, y= 100.65, yend= 64.9), color = 'black') +
  geom_segment(aes(x= 7.75, xend= 8.75, y= 64.9, yend= 30.9), color = 'black') +
  geom_segment(aes(x= 8.75, xend= 9.75, y= 30.9, yend= 57.54), color = 'black') +
  geom_segment(aes(x= 1.25, xend= 2.25, y= 10.34, yend= 8.64), color = 'orange') + 
  geom_segment(aes(x= 2.25, xend= 3.25, y= 8.64, yend= 9.33), color = 'orange') +
  geom_segment(aes(x= 3.25, xend= 4.25, y= 9.33, yend= 19.05), color = 'orange') + 
  geom_segment(aes(x= 4.25, xend= 5.25, y= 19.05, yend= 71.03), color = 'orange') +
  geom_segment(aes(x= 5.25, xend= 6.25, y= 71.03, yend= 281.8), color= 'orange') +
  geom_segment(aes(x= 6.25, xend= 7.25, y= 281.8, yend= 288.4), color = 'orange') +
  geom_segment(aes(x= 7.25, xend= 8.25, y= 288.4, yend= 272.7), color = 'orange') + 
  geom_segment(aes(x= 8.25, xend= 9.25, y= 272.7, yend= 251.2), color = 'orange') + 
  geom_segment(aes(x= 9.25, xend= 10.25, y= 251.2, yend= 145.8), color = 'orange') +
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
  geom_point(data = EC50_values_group, aes(x= Timepoint_DPI, y= Estimated_Reciprocal_Serum_Dilution, color = Virologic_Control_Group), position = position_jitterdodge(seed = 123, jitter.width = 0.35), size= 1, shape= 16) +
  theme_classic() + 
  annotation_logticks(base = 10, sides = 'l', scaled = TRUE, short = unit(-0.5, "cm"), mid = unit(-0.5, "cm"), long = unit(-0.5, "cm")) + 
  scale_y_log10(name = "IgG EC50 Titer", breaks = trans_breaks("log10", function(x) 10^x), labels = trans_format("log10", math_format(10^.x)), expand = c(0,0), limits = c(1,200000)) +
  scale_color_manual(values = c('black', 'orange'), breaks = c('Controller', 'Non-Controller')) +
  scale_x_discrete(name = 'Timepoint (DPI)', expand = c(0,0)) + 
  guides(color = 'none') +
  theme(axis.text.x = element_text(size = 20, angle = 45, hjust = 1, vjust = 1), axis.text.y = element_text(size = 20), axis.title = element_text(size = 20, face = 'bold'), aspect.ratio = 1) +
  geom_segment(aes(x= 0.75, xend= 1.75, y= 30.2, yend = 31.62), color = 'black') + 
  geom_segment(aes(x= 1.75, xend= 2.75, y= 31.62, yend= 30.9), color = 'black') +
  geom_segment(aes(x= 2.75, xend= 3.75, y= 30.9, yend= 70.79), color = 'black') +
  geom_segment(aes(x= 3.75, xend= 4.75, y= 70.79, yend= 125.9), color = 'black') +
  geom_segment(aes(x= 4.75, xend= 5.75, y= 125.9, yend= 363.1), color = 'black') +
  geom_segment(aes(x= 5.75, xend= 6.75, y= 363.1, yend= 464.9), color = 'black') +
  geom_segment(aes(x= 6.75, xend= 7.75, y= 464.9, yend= 320.9), color = 'black') + 
  geom_segment(aes(x= 7.75, xend= 8.75, y= 320.9, yend= 190.5), color = 'black') +
  geom_segment(aes(x= 8.75, xend= 9.75, y= 190.5, yend= 281.8), color = 'black') +
  geom_segment(aes(x= 1.25, xend= 2.25, y= 32.5, yend= 28.22), color = 'orange') +
  geom_segment(aes(x= 2.25, xend= 3.25, y= 28.22, yend= 30.9), color = 'orange') + 
  geom_segment(aes(x= 3.25, xend= 4.25, y= 30.9, yend= 128.8), color = 'orange') + 
  geom_segment(aes(x= 4.25, xend= 5.25, y= 128.8, yend= 552.7), color = 'orange') + 
  geom_segment(aes(x= 5.25, xend= 6.25, y= 552.7, yend= 2454.7), color = 'orange') + 
  geom_segment(aes(x= 6.25, xend= 7.25, y= 2454.7, yend= 2398.8), color = 'orange') +
  geom_segment(aes(x= 7.25, xend= 8.25, y= 2398.8, yend= 3009.4), color = 'orange') +
  geom_segment(aes(x= 8.25, xend= 9.25, y= 3009.4, yend= 1698.2), color = 'orange') +
  geom_segment(aes(x= 9.25, xend= 10.25, y= 1698.2, yend= 934.25), color = 'orange') +
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
# PRNT90 and PRNT50 titers based on virologic control group
###
PRNT_values_group <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\PRNT\\Final Raw Data, figures, and code\\Combined PRNT90 and PRNT50 Values (Final).xlsx", sheet= 1, col_names = TRUE)
PRNT_values_group$Timepoint_MPI = as.character(PRNT_values_group$Timepoint_MPI)
PRNT_values_group$Timepoint_MPI <- factor(PRNT_values_group$Timepoint_MPI, levels = c('Pre-Infection', '1', '3-4'))
PRNT_values_group$Virologic_Control_Group = as.character(PRNT_values_group$Virologic_Control_Group)
PRNT_values_group$Virologic_Control_Group <- factor(PRNT_values_group$Virologic_Control_Group, levels = c('Controller', 'Non-Controller'))
PRNT_values_group$Estimated_Reciprocal_Serum_Dilution = as.numeric(PRNT_values_group$Estimated_Reciprocal_Serum_Dilution)

#PRNT90 titers
PRNT90_values_group <- filter(PRNT_values_group, PRNT == '90')
PRNT90_values_group_summary <- 
  PRNT90_values_group %>% 
  group_by(Timepoint_MPI, Virologic_Control_Group) %>%
  summarize(median = median(Estimated_Reciprocal_Serum_Dilution, na.rm= TRUE))
#PRNT90 titers- statistical analyses comparing virologic control groups at each tested timepoint (Mann-Whitney U Test)
#pre-infection
PRNT90_values_group_preinfection <- filter(PRNT90_values_group, Timepoint_MPI == 'Pre-Infection')
PRNT90_values_group_preinfection_controller <- filter(PRNT90_values_group_preinfection, Virologic_Control_Group == 'Controller')
PRNT90_values_group_preinfection_noncontroller <- filter(PRNT90_values_group_preinfection, Virologic_Control_Group == 'Non-Controller')
PRNT90_values_group_preinfection_controller_quantile <-
  quantile(PRNT90_values_group_preinfection_controller$Estimated_Reciprocal_Serum_Dilution, probs= c(0.25, 0.75))
PRNT90_values_group_preinfection_controller_quantile
PRNT90_values_group_preinfection_noncontroller_quantile <-
  quantile(PRNT90_values_group_preinfection_noncontroller$Estimated_Reciprocal_Serum_Dilution, probs= c(0.25, 0.75), na.rm = TRUE)
PRNT90_values_group_preinfection_noncontroller_quantile
wilcox.test(Estimated_Reciprocal_Serum_Dilution~Virologic_Control_Group, data = PRNT90_values_group_preinfection, exact = FALSE)
#1 MPI
PRNT90_values_group_1mpi <- filter(PRNT90_values_group, Timepoint_MPI == '1')
PRNT90_values_group_1mpi_controller <- filter(PRNT90_values_group_1mpi, Virologic_Control_Group == 'Controller')
PRNT90_values_group_1mpi_noncontroller <- filter(PRNT90_values_group_1mpi, Virologic_Control_Group == 'Non-Controller')
PRNT90_values_group_1mpi_controller_quantile <-
  quantile(PRNT90_values_group_1mpi_controller$Estimated_Reciprocal_Serum_Dilution, probs= c(0.25, 0.75))
PRNT90_values_group_1mpi_controller_quantile
PRNT90_values_group_1mpi_noncontroller_quantile <-
  quantile(PRNT90_values_group_1mpi_noncontroller$Estimated_Reciprocal_Serum_Dilution, probs= c(0.25, 0.75), na.rm = TRUE)
PRNT90_values_group_1mpi_noncontroller_quantile
wilcox.test(Estimated_Reciprocal_Serum_Dilution~Virologic_Control_Group, data = PRNT90_values_group_1mpi, exact = FALSE)
#3-4 MPI #Significant
PRNT90_values_group_3.4mpi <- filter(PRNT90_values_group, Timepoint_MPI == '3-4')
PRNT90_values_group_3.4mpi_controller <- filter(PRNT90_values_group_3.4mpi, Virologic_Control_Group == 'Controller')
PRNT90_values_group_3.4mpi_noncontroller <- filter(PRNT90_values_group_3.4mpi, Virologic_Control_Group == 'Non-Controller')
PRNT90_values_group_3.4mpi_controller_quantile <-
  quantile(PRNT90_values_group_3.4mpi_controller$Estimated_Reciprocal_Serum_Dilution, probs= c(0.25, 0.75))
PRNT90_values_group_3.4mpi_controller_quantile
PRNT90_values_group_3.4mpi_noncontroller_quantile <-
  quantile(PRNT90_values_group_3.4mpi_noncontroller$Estimated_Reciprocal_Serum_Dilution, probs= c(0.25, 0.75), na.rm = TRUE)
PRNT90_values_group_3.4mpi_noncontroller_quantile
wilcox.test(Estimated_Reciprocal_Serum_Dilution~Virologic_Control_Group, data = PRNT90_values_group_3.4mpi, exact = FALSE)
#PRNT90 titers figure
PRNT90_values_group_figure <-
  ggplot() + 
  geom_boxplot(data = PRNT90_values_group, aes(x= Timepoint_MPI, y= Estimated_Reciprocal_Serum_Dilution, color = Virologic_Control_Group), fill = 'white', outlier.color = 'white') + 
  geom_point(data = PRNT90_values_group, aes(x= Timepoint_MPI, y= Estimated_Reciprocal_Serum_Dilution, color = Virologic_Control_Group), position = position_jitterdodge(seed = 123, jitter.width = 0.35), size= 1, shape= 16) +
  theme_classic() + 
  annotation_logticks(base = 10, sides = 'l', scaled = TRUE, short = unit(-0.5, "cm"), mid = unit(-0.5, "cm"), long = unit(-0.5, "cm")) + 
  scale_y_log10(name = "ZIKV PRNT90 Titer", breaks = trans_breaks("log10", function(x) 10^x), labels = trans_format("log10", math_format(10^.x)), expand = c(0,0), limits = c(0.5,200000)) + 
  scale_color_manual(values = c("black", "orange"), breaks = c('Controller', 'Non-Controller')) +
  scale_x_discrete(name = 'Timepoint (MPI)', expand = c(0,0)) + 
  guides(color = guide_legend('Virologic Control Group')) + 
  theme(axis.text.x = element_text(size = 20, angle = 45, hjust = 1, vjust= 1), axis.text.y = element_text(size = 20), axis.title = element_text(size = 20, face = 'bold'), aspect.ratio = 1, legend.title = element_text(size = 20, face = 'bold'), legend.text = element_text(size = 20), legend.position = 'top') +
  geom_hline(yintercept = 10, linetype= 'dotted') +
  geom_segment(aes(x= 2.75, xend= 3.25, y= 20000, yend= 20000), color = 'black') + 
  geom_segment(aes(x= 2.75, xend= 2.75, y= 20000, yend= 15000), color = 'black') +
  geom_segment(aes(x= 3.25, xend= 3.25, y= 20000, yend= 15000), color = 'black') + 
  geom_text(aes(x= 3, y= 25000, label = '*'), size= 6) 
PRNT90_values_group_figure  

#PRNT50 titers
PRNT50_values_group <- filter(PRNT_values_group, PRNT == '50')
PRNT50_values_group_summary <- 
  PRNT50_values_group %>% 
  group_by(Timepoint_MPI, Virologic_Control_Group) %>%
  summarize(median = median(Estimated_Reciprocal_Serum_Dilution, na.rm= TRUE))
#PRNT50 titers- statistical analyses comparing virologic control groups at each tested timepoint (Mann-Whitney U Test)
#pre-infection
PRNT50_values_group_preinfection <- filter(PRNT50_values_group, Timepoint_MPI == 'Pre-Infection')
PRNT50_values_group_preinfection_controller <- filter(PRNT50_values_group_preinfection, Virologic_Control_Group == 'Controller')
PRNT50_values_group_preinfection_noncontroller <- filter(PRNT50_values_group_preinfection, Virologic_Control_Group == 'Non-Controller')
PRNT50_values_group_preinfection_controller_quantile <-
  quantile(PRNT50_values_group_preinfection_controller$Estimated_Reciprocal_Serum_Dilution, probs= c(0.25, 0.75))
PRNT50_values_group_preinfection_controller_quantile
PRNT50_values_group_preinfection_noncontroller_quantile <-
  quantile(PRNT50_values_group_preinfection_noncontroller$Estimated_Reciprocal_Serum_Dilution, probs= c(0.25, 0.75), na.rm = TRUE)
PRNT50_values_group_preinfection_noncontroller_quantile
wilcox.test(Estimated_Reciprocal_Serum_Dilution~Virologic_Control_Group, data = PRNT50_values_group_preinfection, exact = FALSE)
#1 MPI
PRNT50_values_group_1mpi <- filter(PRNT50_values_group, Timepoint_MPI == '1')
PRNT50_values_group_1mpi_controller <- filter(PRNT50_values_group_1mpi, Virologic_Control_Group == 'Controller')
PRNT50_values_group_1mpi_noncontroller <- filter(PRNT50_values_group_1mpi, Virologic_Control_Group == 'Non-Controller')
PRNT50_values_group_1mpi_controller_quantile <-
  quantile(PRNT50_values_group_1mpi_controller$Estimated_Reciprocal_Serum_Dilution, probs= c(0.25, 0.75))
PRNT50_values_group_1mpi_controller_quantile
PRNT50_values_group_1mpi_noncontroller_quantile <-
  quantile(PRNT50_values_group_1mpi_noncontroller$Estimated_Reciprocal_Serum_Dilution, probs= c(0.25, 0.75), na.rm = TRUE)
PRNT50_values_group_1mpi_noncontroller_quantile
wilcox.test(Estimated_Reciprocal_Serum_Dilution~Virologic_Control_Group, data = PRNT50_values_group_1mpi, exact = FALSE)
#3-4 MPI #Significant
PRNT50_values_group_3.4mpi <- filter(PRNT50_values_group, Timepoint_MPI == '3-4')
PRNT50_values_group_3.4mpi_controller <- filter(PRNT50_values_group_3.4mpi, Virologic_Control_Group == 'Controller')
PRNT50_values_group_3.4mpi_noncontroller <- filter(PRNT50_values_group_3.4mpi, Virologic_Control_Group == 'Non-Controller')
PRNT50_values_group_3.4mpi_controller_quantile <-
  quantile(PRNT50_values_group_3.4mpi_controller$Estimated_Reciprocal_Serum_Dilution, probs= c(0.25, 0.75))
PRNT50_values_group_3.4mpi_controller_quantile
PRNT50_values_group_3.4mpi_noncontroller_quantile <-
  quantile(PRNT50_values_group_3.4mpi_noncontroller$Estimated_Reciprocal_Serum_Dilution, probs= c(0.25, 0.75), na.rm = TRUE)
PRNT50_values_group_3.4mpi_noncontroller_quantile
wilcox.test(Estimated_Reciprocal_Serum_Dilution~Virologic_Control_Group, data = PRNT50_values_group_3.4mpi, exact = FALSE)
#PRNT50 titers figure
PRNT50_values_group_figure <-
  ggplot() + 
  geom_boxplot(data = PRNT50_values_group, aes(x= Timepoint_MPI, y= Estimated_Reciprocal_Serum_Dilution, color = Virologic_Control_Group), fill = 'white', outlier.color = 'white') + 
  geom_point(data = PRNT50_values_group, aes(x= Timepoint_MPI, y= Estimated_Reciprocal_Serum_Dilution, color = Virologic_Control_Group), position = position_jitterdodge(seed = 123, jitter.width = 0.35), size= 1, shape= 16) +
  theme_classic() + 
  annotation_logticks(base = 10, sides = 'l', scaled = TRUE, short = unit(-0.5, "cm"), mid = unit(-0.5, "cm"), long = unit(-0.5, "cm")) + 
  scale_y_log10(name = "ZIKV PRNT50 Titer", breaks = trans_breaks("log10", function(x) 10^x), labels = trans_format("log10", math_format(10^.x)), expand = c(0,0), limits = c(0.5,200000)) + 
  scale_color_manual(values = c("black", "orange"), breaks = c('Controller', 'Non-Controller')) +
  scale_x_discrete(name = 'Timepoint (MPI)', expand = c(0,0)) + 
  guides(color = guide_legend('Virologic Control Group')) + 
  theme(axis.text.x = element_text(size = 20, angle = 45, hjust = 1, vjust= 1), axis.text.y = element_text(size = 20), axis.title = element_text(size = 20, face = 'bold'), aspect.ratio = 1, legend.title = element_text(size = 20, face = 'bold'), legend.text = element_text(size = 20), legend.position = 'top') +
  geom_segment(aes(x= 2.75, xend= 3.25, y= 50000, yend= 50000), color = 'black') + 
  geom_segment(aes(x= 2.75, xend= 2.75, y= 50000, yend= 40000), color = 'black') +
  geom_segment(aes(x= 3.25, xend= 3.25, y= 50000, yend= 40000), color = 'black') + 
  geom_text(aes(x= 3, y= 55000, label = '*'), size= 6) 
PRNT50_values_group_figure  

tiff('WVE and PRNT Manuscript figure (Final).tiff', units= 'in', height= 10, width= 10, res= 300)
ggarrange(EC90_values_group_figure, EC50_values_group_figure, PRNT90_values_group_figure, PRNT50_values_group_figure, ncol= 2, nrow= 2, align= 'hv', common.legend = TRUE, labels = c('A', 'B', 'C', 'D'))
dev.off()

