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
library(drc)

###
# Bar graphs showing the PRNT90 and PRNT50 titers for individual animals 
###
PRNT_individual <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\PRNT\\Final Raw Data, figures, and code\\Combined PRNT90 and PRNT50 Values (Final).xlsx", sheet = 1, col_names = TRUE)
PRNT_individual$PRNT = as.character(PRNT_individual$PRNT)
PRNT_individual$Estimated_Reciprocal_Serum_Dilution = as.numeric(PRNT_individual$Estimated_Reciprocal_Serum_Dilution)
PRNT_individual$Animal_ID = as.character(PRNT_individual$Animal_ID)
PRNT_individual$Animal_ID <- factor(PRNT_individual$Animal_ID, levels= c('044-102', '044-103', '044-110', '044-116', '044-117', '044-127','044-130', '044-131', '044-132', '044-101', '044-104', '044-109', '044-112', '044-114', '044-118', '044-122', '044-126', '044-133'))
##Pre-infection
PRNT_individual_preinfection <- filter(PRNT_individual, Timepoint_DPI == 'Pre-Infection')
#PRNT90
PRNT_individual_preinfection_PRNT90 <- filter(PRNT_individual_preinfection, PRNT == '90')
PRNT_individual_preinfection_PRNT90_figure <-
  ggplot() +
  geom_bar(data= PRNT_individual_preinfection_PRNT90, aes(x= Animal_ID, y= Estimated_Reciprocal_Serum_Dilution, color = Animal_ID, fill= Animal_ID), stat= 'identity', width= 0.25) +
  theme_classic() + 
  scale_x_discrete(name = 'Dam ID') + 
  scale_y_log10(name = 'ZIKV PRNT90', limits = c(1e0, 1e5), breaks = c(10, 20, 80, 320, 1280, 5120, 20480), expand = c(0,0)) + 
  geom_hline(yintercept = 10, linetype = 'dotted') + 
  geom_vline(xintercept = 9.5, linetype = 'solid') +
  geom_text(aes(x=1, y=1.25, label= 'ND'), size= 2) +
  geom_text(aes(x=2, y=1.25, label= 'ND'), size= 2) +
  geom_text(aes(x=3, y=1.25, label= 'ND'), size= 2) +
  geom_text(aes(x=5, y=1.25, label= 'ND'), size= 2) +
  geom_text(aes(x=6, y=1.25, label= 'ND'), size=2) +
  geom_text(aes(x= 7, y= 1.25, label= 'ND'), size=2) +
  geom_text(aes(x=8, y=1.25, label= 'ND'), size= 2) +
  geom_text(aes(x=10, y=1.25, label= 'ND'), size=2) +
  geom_text(aes(x=11, y=1.25, label= 'ND'), size=2) +
  geom_text(aes(x=12, y=1.25, label= 'ND'), size=2) +
  geom_text(aes(x=15, y=1.25, label= 'ND'), size=2) +
  geom_text(aes(x=16, y=1.25, label= 'ND'), size=2) +
  geom_text(aes(x=17, y=1.25, label= 'ND'), size=2) +
  geom_text(aes(x=18, y=1.25, label= 'ND'), size=2) +
  geom_text(aes(x= 4, y= 80000, label = 'Controller')) +
  geom_text(aes(x= 13.5, y= 80000, label= 'Non-Controller')) +
  scale_color_manual(values = c("#7b0000", "#ad0000", "#ff0000", "#ff5d5d", "#ffa3a3", "#ffd2d2", "#000088", "#3434fb", "#6c6cff", "#9c9cff", "#d0d0ff", "#0ef000", "#c1c000", "#fffe04", '#ffd991', '#c7ad7c', '#6ca7a2', '#9c7a99'),
                     breaks = c("044-101", "044-102", "044-103", "044-104", "044-109", "044-110", "044-112", "044-114", "044-116", "044-117", "044-118", "044-122", "044-126", "044-127", '044-130', '044-131', '044-132', '044-133')) +
  scale_fill_manual(values = c("#7b0000", "#ad0000", "#ff0000", "#ff5d5d", "#ffa3a3", "#ffd2d2", "#000088", "#3434fb", "#6c6cff", "#9c9cff", "#d0d0ff", "#0ef000", "#c1c000", "#fffe04", '#ffd991', '#c7ad7c', '#6ca7a2', '#9c7a99'),
                    breaks = c("044-101", "044-102", "044-103", "044-104", "044-109", "044-110", "044-112", "044-114", "044-116", "044-117", "044-118", "044-122", "044-126", "044-127", '044-130', '044-131', '044-132', '044-133')) +
  guides(color = 'none', fill = 'none') +
  theme(axis.text.x = element_text(size = 14, angle = 90, hjust = 1, vjust= 0.5), axis.text.y = element_text(size = 14), axis.title = element_text(size = 18, face = 'bold'), aspect.ratio = 1)
PRNT_individual_preinfection_PRNT90_figure 
tiff('Pre-Infection PRNT90 (Final).tiff', units= 'in', width= 5, height= 5, res= 300)
PRNT_individual_preinfection_PRNT90_figure
dev.off()
# PRNT50
PRNT_individual_preinfection_PRNT50 <- filter(PRNT_individual_preinfection, PRNT == '50')
PRNT_individual_preinfection_PRNT50_figure <-
  ggplot() +
  geom_bar(data= PRNT_individual_preinfection_PRNT50, aes(x= Animal_ID, y= Estimated_Reciprocal_Serum_Dilution, color = Animal_ID, fill= Animal_ID), stat= 'identity', width= 0.25) +
  theme_classic() + 
  scale_x_discrete(name = 'Dam ID') + 
  scale_y_log10(name = 'ZIKV PRNT50', limits = c(1e0, 1e5), breaks = c(10, 20, 80, 320, 1280, 5120, 20480), expand = c(0,0)) + 
  geom_vline(xintercept = 9.5, linetype = 'solid') +
  geom_text(aes(x=1, y=1.25, label= 'ND'), size= 2) +
  geom_text(aes(x=2, y=1.25, label= 'ND'), size= 2) +
  geom_text(aes(x=3, y=1.25, label= 'ND'), size= 2) +
  geom_text(aes(x=5, y=1.25, label= 'ND'), size= 2) +
  geom_text(aes(x=6, y=1.25, label= 'ND'), size=2) +
  geom_text(aes(x= 7, y= 1.25, label= 'ND'), size=2) +
  geom_text(aes(x=8, y=1.25, label= 'ND'), size= 2) +
  geom_text(aes(x=10, y=1.25, label= 'ND'), size=2) +
  geom_text(aes(x=11, y=1.25, label= 'ND'), size=2) +
  geom_text(aes(x=12, y=1.25, label= 'ND'), size=2) +
  geom_text(aes(x=15, y=1.25, label= 'ND'), size=2) +
  geom_text(aes(x=16, y=1.25, label= 'ND'), size=2) +
  geom_text(aes(x=17, y=1.25, label= 'ND'), size=2) +
  geom_text(aes(x=18, y=1.25, label= 'ND'), size=2) +
  geom_text(aes(x= 4, y= 80000, label = 'Controller')) +
  geom_text(aes(x= 13.5, y= 80000, label= 'Non-Controller')) +
  scale_color_manual(values = c("#7b0000", "#ad0000", "#ff0000", "#ff5d5d", "#ffa3a3", "#ffd2d2", "#000088", "#3434fb", "#6c6cff", "#9c9cff", "#d0d0ff", "#0ef000", "#c1c000", "#fffe04", '#ffd991', '#c7ad7c', '#6ca7a2', '#9c7a99'),
                     breaks = c("044-101", "044-102", "044-103", "044-104", "044-109", "044-110", "044-112", "044-114", "044-116", "044-117", "044-118", "044-122", "044-126", "044-127", '044-130', '044-131', '044-132', '044-133')) +
  scale_fill_manual(values = c("#7b0000", "#ad0000", "#ff0000", "#ff5d5d", "#ffa3a3", "#ffd2d2", "#000088", "#3434fb", "#6c6cff", "#9c9cff", "#d0d0ff", "#0ef000", "#c1c000", "#fffe04", '#ffd991', '#c7ad7c', '#6ca7a2', '#9c7a99'),
                    breaks = c("044-101", "044-102", "044-103", "044-104", "044-109", "044-110", "044-112", "044-114", "044-116", "044-117", "044-118", "044-122", "044-126", "044-127", '044-130', '044-131', '044-132', '044-133')) +
  guides(color = 'none', fill = 'none') +
  theme(axis.text.x = element_text(size = 14, angle = 90, hjust = 1, vjust= 0.5), axis.text.y = element_text(size = 14), axis.title = element_text(size = 18, face = 'bold'), aspect.ratio = 1)
PRNT_individual_preinfection_PRNT50_figure  
tiff('Pre-Infection PRNT50 (Final).tiff', units= 'in', width= 5, height= 5, res=300)
PRNT_individual_preinfection_PRNT50_figure
dev.off()
## 27-38 DPI
PRNT_individual_27.38dpi <- filter(PRNT_individual, Timepoint_DPI == '27-38')
#PRNT90
PRNT_individual_27.38dpi_PRNT90 <- filter(PRNT_individual_27.38dpi, PRNT == '90')
PRNT_individual_27.38dpi_PRNT90_figure <-
  ggplot() +
  geom_bar(data= PRNT_individual_27.38dpi_PRNT90, aes(x= Animal_ID, y= Estimated_Reciprocal_Serum_Dilution, color = Animal_ID, fill= Animal_ID), stat= 'identity', width= 0.25) +
  theme_classic() + 
  scale_x_discrete(name = 'Dam ID') + 
  scale_y_log10(name = 'ZIKV PRNT90', limits = c(1e0, 1e5), breaks = c(10, 20, 80, 320, 1280, 5120, 20480), expand = c(0,0)) + 
  geom_hline(yintercept = 10, linetype = 'dotted') + 
  geom_vline(xintercept = 9.5, linetype = 'solid') +
  geom_text(aes(x= 4, y= 80000, label = 'Controller')) +
  geom_text(aes(x= 13.5, y= 80000, label= 'Non-Controller')) +
  scale_color_manual(values = c("#7b0000", "#ad0000", "#ff0000", "#ff5d5d", "#ffa3a3", "#ffd2d2", "#000088", "#3434fb", "#6c6cff", "#9c9cff", "#d0d0ff", "#0ef000", "#c1c000", "#fffe04", '#ffd991', '#c7ad7c', '#6ca7a2', '#9c7a99'),
                     breaks = c("044-101", "044-102", "044-103", "044-104", "044-109", "044-110", "044-112", "044-114", "044-116", "044-117", "044-118", "044-122", "044-126", "044-127", '044-130', '044-131', '044-132', '044-133')) +
  scale_fill_manual(values = c("#7b0000", "#ad0000", "#ff0000", "#ff5d5d", "#ffa3a3", "#ffd2d2", "#000088", "#3434fb", "#6c6cff", "#9c9cff", "#d0d0ff", "#0ef000", "#c1c000", "#fffe04", '#ffd991', '#c7ad7c', '#6ca7a2', '#9c7a99'),
                    breaks = c("044-101", "044-102", "044-103", "044-104", "044-109", "044-110", "044-112", "044-114", "044-116", "044-117", "044-118", "044-122", "044-126", "044-127", '044-130', '044-131', '044-132', '044-133')) +
  guides(color = 'none', fill = 'none') +
  theme(axis.text.x = element_text(size = 14, angle = 90, hjust = 1, vjust= 0.5), axis.text.y = element_text(size = 14), axis.title = element_text(size = 18, face = 'bold'), aspect.ratio = 1)
PRNT_individual_27.38dpi_PRNT90_figure  
tiff('27-38 DPI PRNT90 (Final).tiff', units= 'in', height= 5, width= 5, res= 300)
PRNT_individual_27.38dpi_PRNT90_figure
dev.off()
#PRNT50
PRNT_individual_27.38dpi_PRNT50 <- filter(PRNT_individual_27.38dpi, PRNT == '50')
PRNT_individual_27.38dpi_PRNT50_figure <-
  ggplot() +
  geom_bar(data= PRNT_individual_27.38dpi_PRNT50, aes(x= Animal_ID, y= Estimated_Reciprocal_Serum_Dilution, color = Animal_ID, fill= Animal_ID), stat= 'identity', width= 0.25) +
  theme_classic() + 
  scale_x_discrete(name = 'Dam ID') + 
  scale_y_log10(name = 'ZIKV PRNT50', limits = c(1e0, 1e5), breaks = c(10, 20, 80, 320, 1280, 5120, 20480), expand = c(0,0)) + 
  geom_vline(xintercept = 9.5, linetype = 'solid') +
  geom_text(aes(x= 4, y= 80000, label = 'Controller')) +
  geom_text(aes(x= 13.5, y= 80000, label= 'Non-Controller')) +
  scale_color_manual(values = c("#7b0000", "#ad0000", "#ff0000", "#ff5d5d", "#ffa3a3", "#ffd2d2", "#000088", "#3434fb", "#6c6cff", "#9c9cff", "#d0d0ff", "#0ef000", "#c1c000", "#fffe04", '#ffd991', '#c7ad7c', '#6ca7a2', '#9c7a99'),
                     breaks = c("044-101", "044-102", "044-103", "044-104", "044-109", "044-110", "044-112", "044-114", "044-116", "044-117", "044-118", "044-122", "044-126", "044-127", '044-130', '044-131', '044-132', '044-133')) +
  scale_fill_manual(values = c("#7b0000", "#ad0000", "#ff0000", "#ff5d5d", "#ffa3a3", "#ffd2d2", "#000088", "#3434fb", "#6c6cff", "#9c9cff", "#d0d0ff", "#0ef000", "#c1c000", "#fffe04", '#ffd991', '#c7ad7c', '#6ca7a2', '#9c7a99'),
                    breaks = c("044-101", "044-102", "044-103", "044-104", "044-109", "044-110", "044-112", "044-114", "044-116", "044-117", "044-118", "044-122", "044-126", "044-127", '044-130', '044-131', '044-132', '044-133')) +
  guides(color = 'none', fill = 'none') +
  theme(axis.text.x = element_text(size = 14, angle = 90, hjust = 1, vjust= 0.5), axis.text.y = element_text(size = 14), axis.title = element_text(size = 18, face = 'bold'), aspect.ratio = 1)
PRNT_individual_27.38dpi_PRNT50_figure  
tiff('27-38 DPI PRNT50 (Final).tiff', units= 'in', width= 5, height= 5, res= 300)
PRNT_individual_27.38dpi_PRNT50_figure
dev.off()

## 98-136 DPI
PRNT_individual_98.136dpi <- filter(PRNT_individual, Timepoint_DPI == '98-136')
#PRNT90
PRNT_individual_98.136dpi_PRNT90 <- filter(PRNT_individual_98.136dpi, PRNT == '90')
PRNT_individual_98.136dpi_PRNT90_figure <-
  ggplot() +
  geom_bar(data= PRNT_individual_98.136dpi_PRNT90, aes(x= Animal_ID, y= Estimated_Reciprocal_Serum_Dilution, color = Animal_ID, fill= Animal_ID), stat= 'identity', width= 0.25) +
  theme_classic() + 
  scale_x_discrete(name = 'Dam ID') + 
  scale_y_log10(name = 'ZIKV PRNT90', limits = c(1e0, 1e5), breaks = c(10, 20, 80, 320, 1280, 5120, 20480), expand = c(0,0)) + 
  geom_hline(yintercept = 10, linetype = 'dotted') + 
  geom_vline(xintercept = 9.5, linetype = 'solid') +
  geom_text(aes(x= 4, y= 80000, label = 'Controller')) +
  geom_text(aes(x= 13.5, y= 80000, label= 'Non-Controller')) +
  scale_color_manual(values = c("#7b0000", "#ad0000", "#ff0000", "#ff5d5d", "#ffa3a3", "#ffd2d2", "#000088", "#3434fb", "#6c6cff", "#9c9cff", "#d0d0ff", "#0ef000", "#c1c000", "#fffe04", '#ffd991', '#c7ad7c', '#6ca7a2', '#9c7a99'),
                     breaks = c("044-101", "044-102", "044-103", "044-104", "044-109", "044-110", "044-112", "044-114", "044-116", "044-117", "044-118", "044-122", "044-126", "044-127", '044-130', '044-131', '044-132', '044-133')) +
  scale_fill_manual(values = c("#7b0000", "#ad0000", "#ff0000", "#ff5d5d", "#ffa3a3", "#ffd2d2", "#000088", "#3434fb", "#6c6cff", "#9c9cff", "#d0d0ff", "#0ef000", "#c1c000", "#fffe04", '#ffd991', '#c7ad7c', '#6ca7a2', '#9c7a99'),
                    breaks = c("044-101", "044-102", "044-103", "044-104", "044-109", "044-110", "044-112", "044-114", "044-116", "044-117", "044-118", "044-122", "044-126", "044-127", '044-130', '044-131', '044-132', '044-133')) +
  guides(color = 'none', fill = 'none') +
  theme(axis.text.x = element_text(size = 14, angle = 90, hjust = 1, vjust= 0.5), axis.text.y = element_text(size = 14), axis.title = element_text(size = 18, face = 'bold'), aspect.ratio = 1)
PRNT_individual_98.136dpi_PRNT90_figure  
tiff('98-136 DPI PRNT90 (Final).tiff', units= 'in', width= 5, height= 5, res= 300)
PRNT_individual_98.136dpi_PRNT90_figure
dev.off()
#PRNT50
PRNT_individual_98.136dpi_PRNT50 <- filter(PRNT_individual_98.136dpi, PRNT == '50')
PRNT_individual_98.136dpi_PRNT50_figure <-
  ggplot() +
  geom_bar(data= PRNT_individual_98.136dpi_PRNT50, aes(x= Animal_ID, y= Estimated_Reciprocal_Serum_Dilution, color = Animal_ID, fill= Animal_ID), stat= 'identity', width= 0.25) +
  theme_classic() + 
  scale_x_discrete(name = 'Dam ID') + 
  scale_y_log10(name = 'ZIKV PRNT50', limits = c(1e0, 1e5), breaks = c(10, 20, 80, 320, 1280, 5120, 20480), expand = c(0,0)) + 
  geom_vline(xintercept = 9.5, linetype = 'solid') +
  geom_text(aes(x= 4, y= 80000, label = 'Controller')) +
  geom_text(aes(x= 13.5, y= 80000, label= 'Non-Controller')) +
  scale_color_manual(values = c("#7b0000", "#ad0000", "#ff0000", "#ff5d5d", "#ffa3a3", "#ffd2d2", "#000088", "#3434fb", "#6c6cff", "#9c9cff", "#d0d0ff", "#0ef000", "#c1c000", "#fffe04", '#ffd991', '#c7ad7c', '#6ca7a2', '#9c7a99'),
                     breaks = c("044-101", "044-102", "044-103", "044-104", "044-109", "044-110", "044-112", "044-114", "044-116", "044-117", "044-118", "044-122", "044-126", "044-127", '044-130', '044-131', '044-132', '044-133')) +
  scale_fill_manual(values = c("#7b0000", "#ad0000", "#ff0000", "#ff5d5d", "#ffa3a3", "#ffd2d2", "#000088", "#3434fb", "#6c6cff", "#9c9cff", "#d0d0ff", "#0ef000", "#c1c000", "#fffe04", '#ffd991', '#c7ad7c', '#6ca7a2', '#9c7a99'),
                    breaks = c("044-101", "044-102", "044-103", "044-104", "044-109", "044-110", "044-112", "044-114", "044-116", "044-117", "044-118", "044-122", "044-126", "044-127", '044-130', '044-131', '044-132', '044-133')) +
  guides(color = 'none', fill = 'none') +
  theme(axis.text.x = element_text(size = 14, angle = 90, hjust = 1, vjust= 0.5), axis.text.y = element_text(size = 14), axis.title = element_text(size = 18, face = 'bold'), aspect.ratio = 1)
PRNT_individual_98.136dpi_PRNT50_figure  
tiff('98-136 DPI PRNT50 (Final).tiff', units= 'in', height= 5, width = 5, res= 300)
PRNT_individual_98.136dpi_PRNT50_figure
dev.off()
