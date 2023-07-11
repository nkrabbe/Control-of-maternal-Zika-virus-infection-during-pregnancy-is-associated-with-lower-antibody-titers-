library(tidyverse)
library(drc)
library(readxl)
library(ggplot2)
library(scales)
library(ggpubr)

###
# IgG WVE EC90 and EC50 titers over the course of pregancy for each animal based on virologic control group status
###

#EC90 and EC50 titers overtime for the controller dams
EC_titers_controllers <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\EC Titers Overtime (Final)\\Combined EC90 and EC50 Values (Final).xlsx", sheet = 1, col_names = TRUE)
EC_titers_controllers$Animal_ID = as.character(EC_titers_controllers$Animal_ID)
EC_titers_controllers$Animal_ID <- factor(EC_titers_controllers$Animal_ID, levels= c('044-102', '044-103', '044-110', '044-116', '044-117', '044-127', '044-130', '044-131', '044-132'))
EC_titers_controllers$Timepoint_DPI = as.numeric(EC_titers_controllers$Timepoint_DPI)
EC_titers_controllers$EC = as.character(EC_titers_controllers$EC)
EC_titers_controllers$EC <- factor(EC_titers_controllers$EC, levels = c('90', '50'))
EC_titers_controllers
lod <- data.frame(Animal_ID = c('044-102', '044-103', '044-110', '044-116', '044-117', '044-127', '044-130', '044-131', '044-132'), Z= c(12.5, 12.5, 12.5, 12.5, 12.5, 12.5, 12.5, 12.5, 12.5))
EC_titers_controllers_figure <-
  ggplot() + 
  geom_line(data= EC_titers_controllers, aes(x= Timepoint_DPI, y= Estimated_Reciprocal_Serum_Dilution, color = Animal_ID, linetype= EC), size = 1.5) +
  geom_point(data= EC_titers_controllers, aes(x= Timepoint_DPI, y= Estimated_Reciprocal_Serum_Dilution, color = Animal_ID, shape = EC, stroke = 1.5), size = 2.5) + 
  theme_classic() + 
  annotation_logticks(base = 10, sides = 'l', scaled = TRUE, short = unit(-0.5, "cm"), mid = unit(-0.5, "cm"), long = unit(-0.5, "cm")) + 
  scale_y_log10(name = "IgG EC Titers", breaks = trans_breaks("log10", function(x) 10^x), labels = trans_format("log10", math_format(10^.x)), expand = c(0,0), limits = c(1,1e5)) + 
  scale_x_continuous(name = 'DPI', expand = c(0,0), limits = c(-7,140), breaks = c(0, 20, 40, 60, 80, 100, 120, 140)) + 
  scale_color_manual(values= c('#ad0000', '#ff0000', '#ffd2d2', '#6c6cff', '#9c9cff', '#fffe04', '#ffd991', '#c7ad7c', '#6ca7a2'), 
                     breaks = c('044-102', '044-103','044-110', '044-116', '044-117', '044-127', '044-130', '044-131', '044-132')) +
  scale_shape_manual(values = c(0,1), breaks= c('90', '50')) +
  theme(axis.text = element_text(size = 16), axis.text.x = element_text(angle = 45, hjust = 1, vjust= 1), axis.title = element_text(size = 20, face = 'bold'), plot.title = element_text(size = 24, face = 'bold', hjust = 0.5), aspect.ratio = 1, legend.position = 'top', legend.title = element_text(face = 'bold', size = 20), legend.text = element_text(size = 20)) +
  guides(color = 'none')
EC_titers_controllers_figure_wrapped <-
  EC_titers_controllers_figure + facet_wrap(~ Animal_ID, nrow= 3, ncol= 3) + theme(strip.text = element_text(size = 24, face= 'bold'), strip.background = element_blank(), panel.spacing = unit(2, 'lines')) + 
  geom_hline(data= lod, aes(yintercept= Z), linetype= 'dotted')
EC_titers_controllers_figure_wrapped
#EC90 and EC50 titers for the non-controller dams
EC_titers_noncontrollers <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\EC Titers Overtime (Final)\\Combined EC90 and EC50 Values (Final).xlsx", sheet = 2, col_names = TRUE)
EC_titers_noncontrollers$Animal_ID = as.character(EC_titers_noncontrollers$Animal_ID)
EC_titers_noncontrollers$Animal_ID <- factor(EC_titers_noncontrollers$Animal_ID, levels= c('044-101', '044-104', '044-109', '044-112', '044-114', '044-118', '044-122', '044-126', '044-130', '044-133'))
EC_titers_noncontrollers$Timepoint_DPI = as.numeric(EC_titers_noncontrollers$Timepoint_DPI)
EC_titers_noncontrollers$EC = as.character(EC_titers_noncontrollers$EC)
EC_titers_noncontrollers$EC <- factor(EC_titers_noncontrollers$EC, levels = c('90', '50'))
lod <- data.frame(Animal_ID= c('044-101', '044-104', '044-109', '044-112', '044-114', '044-118', '044-122', '044-126', '044-133'), Z= c(12.5, 12.5, 12.5, 12.5, 12.5, 12.5, 50, 12.5, 12.5))
EC_titers_noncontrollers_figure <-
  ggplot() + 
  geom_line(data= EC_titers_noncontrollers, aes(x= Timepoint_DPI, y= Estimated_Reciprocal_Serum_Dilution, color = Animal_ID, linetype= EC), size = 1.5) +
  geom_point(data= EC_titers_noncontrollers, aes(x= Timepoint_DPI, y= Estimated_Reciprocal_Serum_Dilution, color = Animal_ID, shape = EC, stroke = 1.5), size = 2.5) + 
  theme_classic() + 
  annotation_logticks(base = 10, sides = 'l', scaled = TRUE, short = unit(-0.5, "cm"), mid = unit(-0.5, "cm"), long = unit(-0.5, "cm")) + 
  scale_y_log10(name = "IgG EC Titers", breaks = trans_breaks("log10", function(x) 10^x), labels = trans_format("log10", math_format(10^.x)), expand = c(0,0), limits = c(1,1e5)) + 
  scale_x_continuous(name = 'DPI', expand = c(0,0), limits = c(-7,140), breaks = c(0, 20, 40, 60, 80, 100, 120, 140)) + 
  scale_color_manual(values= c('#7b0000', '#ff5d5d', '#ffa3a3', '#000088', '#3434fb', '#d0d0ff', '#0ef000', '#c1c000', '#9c7a99'), 
                     breaks = c('044-101', '044-104','044-109', '044-112', '044-114', '044-118', '044-122', '044-126', '044-133')) +
  scale_shape_manual(values = c(0,1), breaks= c('90', '50')) +
  theme(axis.text = element_text(size = 16), axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1), axis.title = element_text(size = 20, face = 'bold'), plot.title = element_text(size = 24, face = 'bold', hjust = 0.5), aspect.ratio = 1, legend.position = 'top', legend.title = element_text(face = 'bold', size = 20), legend.text = element_text(size = 14)) +
  guides(color = 'none', shape ='none', linetype = 'none')
EC_titers_noncontrollers_figure_wrapped <-
  EC_titers_noncontrollers_figure + facet_wrap(~ Animal_ID, nrow= 3, ncol= 3) + theme(strip.text = element_text(size = 24, face= 'bold'), strip.background = element_blank(), panel.spacing = unit(2, 'lines')) + 
  geom_hline(data= lod, aes(yintercept= Z), linetype= 'dotted')
EC_titers_noncontrollers_figure_wrapped

###
# Combining EC titers overtime into a single multipanel figure
###
tiff('Combined EC Titers Overtime-Individual (Final).tiff', units = 'in', height = 20, width = 10, res= 300)
ggarrange(EC_titers_controllers_figure_wrapped, EC_titers_noncontrollers_figure_wrapped, common.legend = TRUE, labels = c('A', 'B'), ncol = 1, nrow = 2, align = 'hv')
dev.off()
