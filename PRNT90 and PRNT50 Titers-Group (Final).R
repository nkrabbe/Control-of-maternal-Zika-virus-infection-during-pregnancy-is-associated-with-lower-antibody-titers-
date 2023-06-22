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
  geom_point(data = PRNT90_values_group, aes(x= Timepoint_MPI, y= Estimated_Reciprocal_Serum_Dilution, color = Virologic_Control_Group), position = position_jitterdodge(seed = 123, jitter.width = 0.35), size= 2, shape= 16) +
  theme_classic() + 
  annotation_logticks(base = 10, sides = 'l', scaled = TRUE, short = unit(-0.5, "cm"), mid = unit(-0.5, "cm"), long = unit(-0.5, "cm")) + 
  scale_y_log10(name = "ZIKV PRNT90 Titer", breaks = trans_breaks("log10", function(x) 10^x), labels = trans_format("log10", math_format(10^.x)), expand = c(0,0), limits = c(0.5,200000)) + 
  scale_color_manual(values = c("#000000", "#b5b5b5"), breaks = c('Controller', 'Non-Controller')) +
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
  geom_point(data = PRNT50_values_group, aes(x= Timepoint_MPI, y= Estimated_Reciprocal_Serum_Dilution, color = Virologic_Control_Group), position = position_jitterdodge(seed = 123, jitter.width = 0.35), size= 2, shape= 16) +
  theme_classic() + 
  annotation_logticks(base = 10, sides = 'l', scaled = TRUE, short = unit(-0.5, "cm"), mid = unit(-0.5, "cm"), long = unit(-0.5, "cm")) + 
  scale_y_log10(name = "ZIKV PRNT50 Titer", breaks = trans_breaks("log10", function(x) 10^x), labels = trans_format("log10", math_format(10^.x)), expand = c(0,0), limits = c(0.5,200000)) + 
  scale_color_manual(values = c("#000000", "#b5b5b5"), breaks = c('Controller', 'Non-Controller')) +
  scale_x_discrete(name = 'Timepoint (MPI)', expand = c(0,0)) + 
  guides(color = guide_legend('Virologic Control Group')) + 
  theme(axis.text.x = element_text(size = 20, angle = 45, hjust = 1, vjust= 1), axis.text.y = element_text(size = 20), axis.title = element_text(size = 20, face = 'bold'), aspect.ratio = 1, legend.title = element_text(size = 20, face = 'bold'), legend.text = element_text(size = 20), legend.position = 'top') +
  geom_segment(aes(x= 2.75, xend= 3.25, y= 50000, yend= 50000), color = 'black') + 
  geom_segment(aes(x= 2.75, xend= 2.75, y= 50000, yend= 40000), color = 'black') +
  geom_segment(aes(x= 3.25, xend= 3.25, y= 50000, yend= 40000), color = 'black') + 
  geom_text(aes(x= 3, y= 55000, label = '*'), size= 6) 
PRNT50_values_group_figure  

###
# combined PRNT90 and PRNT50 titers in a single multipanel figure
###
tiff('PRNT90 and PRNT50 Titers-Group (Final).tiff', units= 'in', width = 15, height= 10, res= 300)
ggarrange(PRNT90_values_group_figure, PRNT50_values_group_figure, nrow= 1, ncol= 2, common.legend= TRUE, legend= 'bottom', align = 'hv', labels = c('A', 'B'))
dev.off()

