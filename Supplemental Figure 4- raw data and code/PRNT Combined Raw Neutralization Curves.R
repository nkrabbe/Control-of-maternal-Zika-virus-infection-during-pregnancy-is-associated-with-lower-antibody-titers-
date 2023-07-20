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
# Combining the individual raw PRNT neutralization curves into a single multipanel figure
###

#Pre-Infection
#044-101
pre_044_101 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\PRNT\\Final Raw Data, figures, and code\\PRNT Raw Data Analysis (Final).xlsx", sheet = 1, col_names = TRUE)
pre_044_101$log_dilution = as.numeric(pre_044_101$log_dilution)
#044-102
pre_044_102 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\PRNT\\Final Raw Data, figures, and code\\PRNT Raw Data Analysis (Final).xlsx", sheet = 4, col_names = TRUE)
pre_044_102$log_dilution = as.numeric(pre_044_102$log_dilution)
#044-103
pre_044_103 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\PRNT\\Final Raw Data, figures, and code\\PRNT Raw Data Analysis (Final).xlsx", sheet = 7, col_names = TRUE)
pre_044_103$log_dilution = as.numeric(pre_044_103$log_dilution)
#044-104
pre_044_104 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\PRNT\\Final Raw Data, figures, and code\\PRNT Raw Data Analysis (Final).xlsx", sheet = 10, col_names = TRUE)
pre_044_104$log_dilution = as.numeric(pre_044_104$log_dilution)
#044-109
pre_044_109 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\PRNT\\Final Raw Data, figures, and code\\PRNT Raw Data Analysis (Final).xlsx", sheet = 13, col_names = TRUE)
pre_044_109$log_dilution = as.numeric(pre_044_109$log_dilution)
#044-110
pre_044_110 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\PRNT\\Final Raw Data, figures, and code\\PRNT Raw Data Analysis (Final).xlsx", sheet = 16, col_names = TRUE)
pre_044_110$log_dilution = as.numeric(pre_044_110$log_dilution)
#044-112
pre_044_112 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\PRNT\\Final Raw Data, figures, and code\\PRNT Raw Data Analysis (Final).xlsx", sheet = 19, col_names = TRUE)
pre_044_112$log_dilution = as.numeric(pre_044_112$log_dilution)
str(pre_044_112)
model_pre_044_112 <- drm(percent_plaque_reduction~log_dilution, data = pre_044_112, fct = LL.4(fixed = c(NA, 0, 100, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_pre_044_112, type = "all")
summary(model_pre_044_112)
ED(model_pre_044_112, c(10,50), interval = "delta")
newdata_pre_044_112 <- expand.grid(DF = exp(seq(log(1), log(4.3), length = 100)))
pm_pre_044_112 <- predict(model_pre_044_112, newdata = newdata_pre_044_112, interval = "confidence")
newdata_pre_044_112$p <- pm_pre_044_112[,1]
newdata_pre_044_112$pmin <- pm_pre_044_112[,2]
newdata_pre_044_112$pmax <- pm_pre_044_112[,3]
#044-114
pre_044_114 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\PRNT\\Final Raw Data, figures, and code\\PRNT Raw Data Analysis (Final).xlsx", sheet = 22, col_names = TRUE)
pre_044_114$log_dilution = as.numeric(pre_044_114$log_dilution)
str(pre_044_114)
model_pre_044_114 <- drm(percent_plaque_reduction~log_dilution, data = pre_044_114, fct = LL.4(fixed = c(NA, 0, 100, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_pre_044_114, type = "all")
summary(model_pre_044_114)
ED(model_pre_044_114, c(10,50), interval = "delta")
newdata_pre_044_114 <- expand.grid(DF = exp(seq(log(1), log(4.3), length = 100)))
pm_pre_044_114 <- predict(model_pre_044_114, newdata = newdata_pre_044_114, interval = "confidence")
newdata_pre_044_114$p <- pm_pre_044_114[,1]
newdata_pre_044_114$pmin <- pm_pre_044_114[,2]
newdata_pre_044_114$pmax <- pm_pre_044_114[,3]
#044-116
pre_044_116 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\PRNT\\Final Raw Data, figures, and code\\PRNT Raw Data Analysis (Final).xlsx", sheet = 25, col_names = TRUE)
pre_044_116$log_dilution = as.numeric(pre_044_116$log_dilution)
str(pre_044_116)
model_pre_044_116 <- drm(percent_plaque_reduction~log_dilution, data = pre_044_116, fct = LL.4(fixed = c(NA, 0, 100, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_pre_044_116, type = "all")
summary(model_pre_044_116)
ED(model_pre_044_116, c(10,50), interval = "delta")
newdata_pre_044_116 <- expand.grid(DF = exp(seq(log(1), log(4.3), length = 100)))
pm_pre_044_116 <- predict(model_pre_044_116, newdata = newdata_pre_044_116, interval = "confidence")
newdata_pre_044_116$p <- pm_pre_044_116[,1]
newdata_pre_044_116$pmin <- pm_pre_044_116[,2]
newdata_pre_044_116$pmax <- pm_pre_044_116[,3]
#044-117
pre_044_117 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\PRNT\\Final Raw Data, figures, and code\\PRNT Raw Data Analysis (Final).xlsx", sheet = 28, col_names = TRUE)
pre_044_117$log_dilution = as.numeric(pre_044_117$log_dilution)
#044-118
pre_044_118 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\PRNT\\Final Raw Data, figures, and code\\PRNT Raw Data Analysis (Final).xlsx", sheet = 31, col_names = TRUE)
pre_044_118$log_dilution = as.numeric(pre_044_118$log_dilution)
#044-122
pre_044_122 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\PRNT\\Final Raw Data, figures, and code\\PRNT Raw Data Analysis (Final).xlsx", sheet = 34, col_names = TRUE)
pre_044_122$log_dilution = as.numeric(pre_044_122$log_dilution)
#044-126
pre_044_126 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\PRNT\\Final Raw Data, figures, and code\\PRNT Raw Data Analysis (Final).xlsx", sheet = 37, col_names = TRUE)
pre_044_126$log_dilution = as.numeric(pre_044_126$log_dilution)
#044-127
pre_044_127 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\PRNT\\Final Raw Data, figures, and code\\PRNT Raw Data Analysis (Final).xlsx", sheet = 40, col_names = TRUE)
pre_044_127$log_dilution = as.numeric(pre_044_127$log_dilution)
#044-130
pre_044_130 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\PRNT\\Final Raw Data, figures, and code\\PRNT Raw Data Analysis (Final).xlsx", sheet = 43, col_names = TRUE)
pre_044_130$log_dilution = as.numeric(pre_044_130$log_dilution)
#044-131
pre_044_131 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\PRNT\\Final Raw Data, figures, and code\\PRNT Raw Data Analysis (Final).xlsx", sheet = 46, col_names = TRUE)
pre_044_131$log_dilution = as.numeric(pre_044_131$log_dilution)
#044-132
pre_044_132 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\PRNT\\Final Raw Data, figures, and code\\PRNT Raw Data Analysis (Final).xlsx", sheet = 49, col_names = TRUE)
pre_044_132$log_dilution = as.numeric(pre_044_132$log_dilution)
str(pre_044_132)
model_pre_044_132 <- drm(percent_plaque_reduction~log_dilution, data = pre_044_132, fct = LL.4(fixed = c(NA, 0, 100, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_pre_044_132, type = "all")
summary(model_pre_044_132)
ED(model_pre_044_132, c(10,50), interval = "delta")
newdata_pre_044_132 <- expand.grid(DF = exp(seq(log(1), log(4.3), length = 100)))
pm_pre_044_132 <- predict(model_pre_044_132, newdata = newdata_pre_044_132, interval = "confidence")
newdata_pre_044_132$p <- pm_pre_044_132[,1]
newdata_pre_044_132$pmin <- pm_pre_044_132[,2]
newdata_pre_044_132$pmax <- pm_pre_044_132[,3]
#044-133
pre_044_133 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\PRNT\\Final Raw Data, figures, and code\\PRNT Raw Data Analysis (Final).xlsx", sheet = 52, col_names = TRUE)
pre_044_133$log_dilution = as.numeric(pre_044_133$log_dilution)

#Combined Pre-infection Raw neutralization curves
Preinfection_rawcurves <- 
  ggplot() +
  geom_jitter(data= pre_044_101, aes(x= log_dilution, y= percent_plaque_reduction, color= '044-101')) +
  geom_jitter(data= pre_044_102, aes(x= log_dilution, y= percent_plaque_reduction, color= '044-102')) +
  geom_jitter(data= pre_044_103, aes(x= log_dilution, y= percent_plaque_reduction, color= '044-103')) +
  geom_jitter(data= pre_044_104, aes(x= log_dilution, y= percent_plaque_reduction, color= '044-104')) +
  geom_jitter(data= pre_044_109, aes(x= log_dilution, y= percent_plaque_reduction, color= '044-109')) +
  geom_jitter(data= pre_044_110, aes(x= log_dilution, y= percent_plaque_reduction, color= '044-110')) +
  geom_point(data= pre_044_112, aes(x= log_dilution, y= percent_plaque_reduction, color= '044-112')) + geom_line(data= newdata_pre_044_112, aes(x= DF, y= p, color = '044-112')) +
  geom_point(data= pre_044_114, aes(x= log_dilution, y= percent_plaque_reduction, color= '044-114')) + geom_line(data= newdata_pre_044_114, aes(x= DF, y= p, color = '044-114')) +
  geom_point(data= pre_044_116, aes(x= log_dilution, y= percent_plaque_reduction, color= '044-116')) + geom_line(data= newdata_pre_044_116, aes(x= DF, y= p, color = '044-116')) +
  geom_jitter(data= pre_044_117, aes(x= log_dilution, y= percent_plaque_reduction, color= '044-117')) +
  geom_jitter(data= pre_044_118, aes(x= log_dilution, y= percent_plaque_reduction, color= '044-118')) +
  geom_jitter(data= pre_044_122, aes(x= log_dilution, y= percent_plaque_reduction, color= '044-122')) +
  geom_jitter(data= pre_044_126, aes(x= log_dilution, y= percent_plaque_reduction, color= '044-126')) +
  geom_jitter(data= pre_044_127, aes(x= log_dilution, y= percent_plaque_reduction, color= '044-127')) +
  geom_jitter(data= pre_044_130, aes(x= log_dilution, y= percent_plaque_reduction, color= '044-130')) +
  geom_jitter(data= pre_044_131, aes(x= log_dilution, y= percent_plaque_reduction, color= '044-131')) +
  geom_point(data= pre_044_132, aes(x= log_dilution, y= percent_plaque_reduction, color= '044-132')) + geom_line(data= newdata_pre_044_132, aes(x= DF, y= p, color= '044-132')) +
  geom_jitter(data= pre_044_133, aes(x= log_dilution, y= percent_plaque_reduction, color= '044-133')) +
  theme_classic() +
  labs(x = "Log (reciprocal serum dilution)", y = "Plaque Reduction (%)", title= 'Pre-Infection') + 
  xlim(1,5) + scale_y_continuous(expand = c(0,0), limits = c(-1,100), breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)) + 
  scale_color_manual(values = c("#7b0000", "#ad0000", "#ff0000", "#ff5d5d", "#ffa3a3", "#ffd2d2", "#000088", "#3434fb", "#6c6cff", "#9c9cff", "#d0d0ff", "#0ef000", "#c1c000", "#fffe04", '#ffd991', '#c7ad7c', '#6ca7a2', '#9c7a99'), 
                     breaks = c("044-101", "044-102", "044-103", "044-104", "044-109", "044-110", "044-112", "044-114", "044-116", "044-117", "044-118", "044-122", "044-126", "044-127", '044-130', '044-131', '044-132', '044-133')) + 
  guides(color = guide_legend(title = "Dam ID")) + 
  geom_hline(yintercept = 90, linetype = "dashed") + 
  geom_hline(yintercept = 50, linetype = "dashed") + 
  theme(axis.text = element_text(size = 20), axis.title = element_text(face = 'bold', size = 20), plot.title = element_text(face = "bold", hjust = 0.5, size = 20), aspect.ratio = 1, legend.title = element_text(size = 20, face = 'bold'), legend.text = element_text(size= 20), legend.position = 'top')
Preinfection_rawcurves  

# 27-38 DPI

#044-101
mpi_1_044_101 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\PRNT\\Final Raw Data, figures, and code\\PRNT Raw Data Analysis (Final).xlsx", sheet = 2, col_names = TRUE)
mpi_1_044_101$log_dilution = as.numeric(mpi_1_044_101$log_dilution)
str(mpi_1_044_101)
model_mpi_1_044_101 <- drm(percent_plaque_reduction~log_dilution, data = mpi_1_044_101, fct = LL.4(fixed = c(NA, 0, 100, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_mpi_1_044_101, type = "all")
summary(model_mpi_1_044_101)
ED(model_mpi_1_044_101, c(10,50), interval = "delta")
newdata_mpi_1_044_101 <- expand.grid(DF = exp(seq(log(1), log(4.31), length = 100)))
pm_mpi_1_044_101 <- predict(model_mpi_1_044_101, newdata = newdata_mpi_1_044_101, interval = "confidence")
newdata_mpi_1_044_101$p <- pm_mpi_1_044_101[,1]
newdata_mpi_1_044_101$pmin <- pm_mpi_1_044_101[,2]
newdata_mpi_1_044_101$pmax <- pm_mpi_1_044_101[,3]
#044-102
mpi_1_044_102 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\PRNT\\Final Raw Data, figures, and code\\PRNT Raw Data Analysis (Final).xlsx", sheet = 5, col_names = TRUE)
mpi_1_044_102$log_dilution = as.numeric(mpi_1_044_102$log_dilution)
str(mpi_1_044_102)
model_mpi_1_044_102 <- drm(percent_plaque_reduction~log_dilution, data = mpi_1_044_102, fct = LL.4(fixed = c(NA, 0, 100, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_mpi_1_044_102, type = "all")
summary(model_mpi_1_044_102)
ED(model_mpi_1_044_102, c(10,50), interval = "delta")
newdata_mpi_1_044_102 <- expand.grid(DF = exp(seq(log(1), log(4.31), length = 100)))
pm_mpi_1_044_102 <- predict(model_mpi_1_044_102, newdata = newdata_mpi_1_044_102, interval = "confidence")
newdata_mpi_1_044_102$p <- pm_mpi_1_044_102[,1]
newdata_mpi_1_044_102$pmin <- pm_mpi_1_044_102[,2]
newdata_mpi_1_044_102$pmax <- pm_mpi_1_044_102[,3]
#044-103
mpi_1_044_103 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\PRNT\\Final Raw Data, figures, and code\\PRNT Raw Data Analysis (Final).xlsx", sheet = 8, col_names = TRUE)
mpi_1_044_103$log_dilution = as.numeric(mpi_1_044_103$log_dilution)
str(mpi_1_044_103)
model_mpi_1_044_103 <- drm(percent_plaque_reduction~log_dilution, data = mpi_1_044_103, fct = LL.4(fixed = c(NA, 0, 100, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_mpi_1_044_103, type = "all")
summary(model_mpi_1_044_103)
ED(model_mpi_1_044_103, c(10,50), interval = "delta")
newdata_mpi_1_044_103 <- expand.grid(DF = exp(seq(log(1), log(4.31), length = 100)))
pm_mpi_1_044_103 <- predict(model_mpi_1_044_103, newdata = newdata_mpi_1_044_103, interval = "confidence")
newdata_mpi_1_044_103$p <- pm_mpi_1_044_103[,1]
newdata_mpi_1_044_103$pmin <- pm_mpi_1_044_103[,2]
newdata_mpi_1_044_103$pmax <- pm_mpi_1_044_103[,3]
#044-104
mpi_1_044_104 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\PRNT\\Final Raw Data, figures, and code\\PRNT Raw Data Analysis (Final).xlsx", sheet = 11, col_names = TRUE)
mpi_1_044_104$log_dilution = as.numeric(mpi_1_044_104$log_dilution)
str(mpi_1_044_104)
model_mpi_1_044_104 <- drm(percent_plaque_reduction~log_dilution, data = mpi_1_044_104, fct = LL.4(fixed = c(NA, 0, 100, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_mpi_1_044_104, type = "all")
summary(model_mpi_1_044_104)
ED(model_mpi_1_044_104, c(10,50), interval = "delta")
newdata_mpi_1_044_104 <- expand.grid(DF = exp(seq(log(1), log(4.31), length = 100)))
pm_mpi_1_044_104 <- predict(model_mpi_1_044_104, newdata = newdata_mpi_1_044_104, interval = "confidence")
newdata_mpi_1_044_104$p <- pm_mpi_1_044_104[,1]
newdata_mpi_1_044_104$pmin <- pm_mpi_1_044_104[,2]
newdata_mpi_1_044_104$pmax <- pm_mpi_1_044_104[,3]
#044-109
mpi_1_044_109 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\PRNT\\Final Raw Data, figures, and code\\PRNT Raw Data Analysis (Final).xlsx", sheet = 14, col_names = TRUE)
mpi_1_044_109$log_dilution = as.numeric(mpi_1_044_109$log_dilution)
str(mpi_1_044_109)
model_mpi_1_044_109 <- drm(percent_plaque_reduction~log_dilution, data = mpi_1_044_109, fct = LL.4(fixed = c(NA, 0, 100, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_mpi_1_044_109, type = "all")
summary(model_mpi_1_044_109)
ED(model_mpi_1_044_109, c(10,50), interval = "delta")
newdata_mpi_1_044_109 <- expand.grid(DF = exp(seq(log(1), log(3.7), length = 100)))
pm_mpi_1_044_109 <- predict(model_mpi_1_044_109, newdata = newdata_mpi_1_044_109, interval = "confidence")
newdata_mpi_1_044_109$p <- pm_mpi_1_044_109[,1]
newdata_mpi_1_044_109$pmin <- pm_mpi_1_044_109[,2]
newdata_mpi_1_044_109$pmax <- pm_mpi_1_044_109[,3]
#044-110
mpi_1_044_110 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\PRNT\\Final Raw Data, figures, and code\\PRNT Raw Data Analysis (Final).xlsx", sheet = 17, col_names = TRUE)
mpi_1_044_110$log_dilution = as.numeric(mpi_1_044_110$log_dilution)
str(mpi_1_044_110)
model_mpi_1_044_110 <- drm(percent_plaque_reduction~log_dilution, data = mpi_1_044_110, fct = LL.4(fixed = c(NA, 0, 100, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_mpi_1_044_110, type = "all")
summary(model_mpi_1_044_110)
ED(model_mpi_1_044_110, c(10,50), interval = "delta")
newdata_mpi_1_044_110 <- expand.grid(DF = exp(seq(log(1), log(4.3), length = 100)))
pm_mpi_1_044_110 <- predict(model_mpi_1_044_110, newdata = newdata_mpi_1_044_110, interval = "confidence")
newdata_mpi_1_044_110$p <- pm_mpi_1_044_110[,1]
newdata_mpi_1_044_110$pmin <- pm_mpi_1_044_110[,2]
newdata_mpi_1_044_110$pmax <- pm_mpi_1_044_110[,3]
#044-112
mpi_1_044_112 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\PRNT\\Final Raw Data, figures, and code\\PRNT Raw Data Analysis (Final).xlsx", sheet = 20, col_names = TRUE)
mpi_1_044_112$log_dilution = as.numeric(mpi_1_044_112$log_dilution)
str(mpi_1_044_112)
model_mpi_1_044_112 <- drm(percent_plaque_reduction~log_dilution, data = mpi_1_044_112, fct = LL.4(fixed = c(NA, 0, 100, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_mpi_1_044_112, type = "all")
summary(model_mpi_1_044_112)
ED(model_mpi_1_044_112, c(10,50), interval = "delta")
newdata_mpi_1_044_112 <- expand.grid(DF = exp(seq(log(1), log(4.3), length = 100)))
pm_mpi_1_044_112 <- predict(model_mpi_1_044_112, newdata = newdata_mpi_1_044_112, interval = "confidence")
newdata_mpi_1_044_112$p <- pm_mpi_1_044_112[,1]
newdata_mpi_1_044_112$pmin <- pm_mpi_1_044_112[,2]
newdata_mpi_1_044_112$pmax <- pm_mpi_1_044_112[,3]
#044-114
mpi_1_044_114 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\PRNT\\Final Raw Data, figures, and code\\PRNT Raw Data Analysis (Final).xlsx", sheet = 23, col_names = TRUE)
mpi_1_044_114$log_dilution = as.numeric(mpi_1_044_114$log_dilution)
str(mpi_1_044_114)
model_mpi_1_044_114 <- drm(percent_plaque_reduction~log_dilution, data = mpi_1_044_114, fct = LL.4(fixed = c(NA, 0, 100, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_mpi_1_044_114, type = "all")
summary(model_mpi_1_044_114)
ED(model_mpi_1_044_114, c(10,50), interval = "delta")
newdata_mpi_1_044_114 <- expand.grid(DF = exp(seq(log(1), log(4.3), length = 100)))
pm_mpi_1_044_114 <- predict(model_mpi_1_044_114, newdata = newdata_mpi_1_044_114, interval = "confidence")
newdata_mpi_1_044_114$p <- pm_mpi_1_044_114[,1]
newdata_mpi_1_044_114$pmin <- pm_mpi_1_044_114[,2]
newdata_mpi_1_044_114$pmax <- pm_mpi_1_044_114[,3]
#044-116
mpi_1_044_116 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\PRNT\\Final Raw Data, figures, and code\\PRNT Raw Data Analysis (Final).xlsx", sheet = 26, col_names = TRUE)
mpi_1_044_116$log_dilution = as.numeric(mpi_1_044_116$log_dilution)
str(mpi_1_044_116)
model_mpi_1_044_116 <- drm(percent_plaque_reduction~log_dilution, data = mpi_1_044_116, fct = LL.4(fixed = c(NA, 0, 100, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_mpi_1_044_116, type = "all")
summary(model_mpi_1_044_116)
ED(model_mpi_1_044_116, c(10,50), interval = "delta")
newdata_mpi_1_044_116 <- expand.grid(DF = exp(seq(log(1), log(4.3), length = 100)))
pm_mpi_1_044_116 <- predict(model_mpi_1_044_116, newdata = newdata_mpi_1_044_116, interval = "confidence")
newdata_mpi_1_044_116$p <- pm_mpi_1_044_116[,1]
newdata_mpi_1_044_116$pmin <- pm_mpi_1_044_116[,2]
newdata_mpi_1_044_116$pmax <- pm_mpi_1_044_116[,3]
#044-117
mpi_1_044_117 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\PRNT\\Final Raw Data, figures, and code\\PRNT Raw Data Analysis (Final).xlsx", sheet = 29, col_names = TRUE)
mpi_1_044_117$log_dilution = as.numeric(mpi_1_044_117$log_dilution)
str(mpi_1_044_117)
model_mpi_1_044_117 <- drm(percent_plaque_reduction~log_dilution, data = mpi_1_044_117, fct = LL.4(fixed = c(NA, 0, 100, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_mpi_1_044_117, type = "all")
summary(model_mpi_1_044_117)
ED(model_mpi_1_044_117, c(10,50), interval = "delta")
newdata_mpi_1_044_117 <- expand.grid(DF = exp(seq(log(1), log(4.3), length = 100)))
pm_mpi_1_044_117 <- predict(model_mpi_1_044_117, newdata = newdata_mpi_1_044_117, interval = "confidence")
newdata_mpi_1_044_117$p <- pm_mpi_1_044_117[,1]
newdata_mpi_1_044_117$pmin <- pm_mpi_1_044_117[,2]
newdata_mpi_1_044_117$pmax <- pm_mpi_1_044_117[,3]
#044-118
mpi_1_044_118 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\PRNT\\Final Raw Data, figures, and code\\PRNT Raw Data Analysis (Final).xlsx", sheet = 32, col_names = TRUE)
mpi_1_044_118$log_dilution = as.numeric(mpi_1_044_118$log_dilution)
str(mpi_1_044_118)
model_mpi_1_044_118 <- drm(percent_plaque_reduction~log_dilution, data = mpi_1_044_118, fct = LL.4(fixed = c(NA, 0, 100, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_mpi_1_044_118, type = "all")
summary(model_mpi_1_044_118)
ED(model_mpi_1_044_118, c(10,50), interval = "delta")
newdata_mpi_1_044_118 <- expand.grid(DF = exp(seq(log(1), log(4.3), length = 100)))
pm_mpi_1_044_118 <- predict(model_mpi_1_044_118, newdata = newdata_mpi_1_044_118, interval = "confidence")
newdata_mpi_1_044_118$p <- pm_mpi_1_044_118[,1]
newdata_mpi_1_044_118$pmin <- pm_mpi_1_044_118[,2]
newdata_mpi_1_044_118$pmax <- pm_mpi_1_044_118[,3]
#044-122
mpi_1_044_122 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\PRNT\\Final Raw Data, figures, and code\\PRNT Raw Data Analysis (Final).xlsx", sheet = 35, col_names = TRUE)
mpi_1_044_122$log_dilution = as.numeric(mpi_1_044_122$log_dilution)
str(mpi_1_044_122)
model_mpi_1_044_122 <- drm(percent_plaque_reduction~log_dilution, data = mpi_1_044_122, fct = LL.4(fixed = c(NA, 0, 100, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_mpi_1_044_122, type = "all")
summary(model_mpi_1_044_122)
ED(model_mpi_1_044_122, c(10,50), interval = "delta")
newdata_mpi_1_044_122 <- expand.grid(DF = exp(seq(log(1), log(4.3), length = 100)))
pm_mpi_1_044_122 <- predict(model_mpi_1_044_122, newdata = newdata_mpi_1_044_122, interval = "confidence")
newdata_mpi_1_044_122$p <- pm_mpi_1_044_122[,1]
newdata_mpi_1_044_122$pmin <- pm_mpi_1_044_122[,2]
newdata_mpi_1_044_122$pmax <- pm_mpi_1_044_122[,3]
#044-126
mpi_1_044_126 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\PRNT\\Final Raw Data, figures, and code\\PRNT Raw Data Analysis (Final).xlsx", sheet = 38, col_names = TRUE)
mpi_1_044_126$log_dilution = as.numeric(mpi_1_044_126$log_dilution)
str(mpi_1_044_126)
model_mpi_1_044_126 <- drm(percent_plaque_reduction~log_dilution, data = mpi_1_044_126, fct = LL.4(fixed = c(NA, 0, 100, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_mpi_1_044_126, type = "all")
summary(model_mpi_1_044_126)
ED(model_mpi_1_044_126, c(10,50), interval = "delta")
newdata_mpi_1_044_126 <- expand.grid(DF = exp(seq(log(1), log(4.3), length = 100)))
pm_mpi_1_044_126 <- predict(model_mpi_1_044_126, newdata = newdata_mpi_1_044_126, interval = "confidence")
newdata_mpi_1_044_126$p <- pm_mpi_1_044_126[,1]
newdata_mpi_1_044_126$pmin <- pm_mpi_1_044_126[,2]
newdata_mpi_1_044_126$pmax <- pm_mpi_1_044_126[,3]
#044-127
mpi_1_044_127 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\PRNT\\Final Raw Data, figures, and code\\PRNT Raw Data Analysis (Final).xlsx", sheet = 41, col_names = TRUE)
mpi_1_044_127$log_dilution = as.numeric(mpi_1_044_127$log_dilution)
str(mpi_1_044_127)
model_mpi_1_044_127 <- drm(percent_plaque_reduction~log_dilution, data = mpi_1_044_127, fct = LL.4(fixed = c(NA, 0, 100, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_mpi_1_044_127, type = "all")
summary(model_mpi_1_044_127)
ED(model_mpi_1_044_127, c(10,50), interval = "delta")
newdata_mpi_1_044_127 <- expand.grid(DF = exp(seq(log(1), log(4.3), length = 100)))
pm_mpi_1_044_127 <- predict(model_mpi_1_044_127, newdata = newdata_mpi_1_044_127, interval = "confidence")
newdata_mpi_1_044_127$p <- pm_mpi_1_044_127[,1]
newdata_mpi_1_044_127$pmin <- pm_mpi_1_044_127[,2]
newdata_mpi_1_044_127$pmax <- pm_mpi_1_044_127[,3]
#044-130
mpi_1_044_130 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\PRNT\\Final Raw Data, figures, and code\\PRNT Raw Data Analysis (Final).xlsx", sheet = 44, col_names = TRUE)
mpi_1_044_130$log_dilution = as.numeric(mpi_1_044_130$log_dilution)
str(mpi_1_044_130)
model_mpi_1_044_130 <- drm(percent_plaque_reduction~log_dilution, data = mpi_1_044_130, fct = LL.4(fixed = c(NA, 0, 100, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_mpi_1_044_130, type = "all")
summary(model_mpi_1_044_130)
ED(model_mpi_1_044_130, c(10,50), interval = "delta")
newdata_mpi_1_044_130 <- expand.grid(DF = exp(seq(log(1), log(4.3), length = 100)))
pm_mpi_1_044_130 <- predict(model_mpi_1_044_130, newdata = newdata_mpi_1_044_130, interval = "confidence")
newdata_mpi_1_044_130$p <- pm_mpi_1_044_130[,1]
newdata_mpi_1_044_130$pmin <- pm_mpi_1_044_130[,2]
newdata_mpi_1_044_130$pmax <- pm_mpi_1_044_130[,3]
#044-131
mpi_1_044_131 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\PRNT\\Final Raw Data, figures, and code\\PRNT Raw Data Analysis (Final).xlsx", sheet = 47, col_names = TRUE)
mpi_1_044_131$log_dilution = as.numeric(mpi_1_044_131$log_dilution)
str(mpi_1_044_131)
model_mpi_1_044_131 <- drm(percent_plaque_reduction~log_dilution, data = mpi_1_044_131, fct = LL.4(fixed = c(NA, 0, 100, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_mpi_1_044_131, type = "all")
summary(model_mpi_1_044_131)
ED(model_mpi_1_044_131, c(10,50), interval = "delta")
newdata_mpi_1_044_131 <- expand.grid(DF = exp(seq(log(1), log(4.3), length = 100)))
pm_mpi_1_044_131 <- predict(model_mpi_1_044_131, newdata = newdata_mpi_1_044_131, interval = "confidence")
newdata_mpi_1_044_131$p <- pm_mpi_1_044_131[,1]
newdata_mpi_1_044_131$pmin <- pm_mpi_1_044_131[,2]
newdata_mpi_1_044_131$pmax <- pm_mpi_1_044_131[,3]
#044-132
mpi_1_044_132 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\PRNT\\Final Raw Data, figures, and code\\PRNT Raw Data Analysis (Final).xlsx", sheet = 50, col_names = TRUE)
mpi_1_044_132$log_dilution = as.numeric(mpi_1_044_132$log_dilution)
str(mpi_1_044_132)
model_mpi_1_044_132 <- drm(percent_plaque_reduction~log_dilution, data = mpi_1_044_132, fct = LL.4(fixed = c(NA, 0, 100, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_mpi_1_044_132, type = "all")
summary(model_mpi_1_044_132)
ED(model_mpi_1_044_132, c(10,50), interval = "delta")
newdata_mpi_1_044_132 <- expand.grid(DF = exp(seq(log(1), log(4.3), length = 100)))
pm_mpi_1_044_132 <- predict(model_mpi_1_044_132, newdata = newdata_mpi_1_044_132, interval = "confidence")
newdata_mpi_1_044_132$p <- pm_mpi_1_044_132[,1]
newdata_mpi_1_044_132$pmin <- pm_mpi_1_044_132[,2]
newdata_mpi_1_044_132$pmax <- pm_mpi_1_044_132[,3]
#044-133
mpi_1_044_133 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\PRNT\\Final Raw Data, figures, and code\\PRNT Raw Data Analysis (Final).xlsx", sheet = 53, col_names = TRUE)
mpi_1_044_133$log_dilution = as.numeric(mpi_1_044_133$log_dilution)
str(mpi_1_044_133)
model_mpi_1_044_133 <- drm(percent_plaque_reduction~log_dilution, data = mpi_1_044_133, fct = LL.4(fixed = c(NA, 0, 100, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_mpi_1_044_133, type = "all")
summary(model_mpi_1_044_133)
ED(model_mpi_1_044_133, c(10,50), interval = "delta")
newdata_mpi_1_044_133 <- expand.grid(DF = exp(seq(log(1), log(4.3), length = 100)))
pm_mpi_1_044_133 <- predict(model_mpi_1_044_133, newdata = newdata_mpi_1_044_133, interval = "confidence")
newdata_mpi_1_044_133$p <- pm_mpi_1_044_133[,1]
newdata_mpi_1_044_133$pmin <- pm_mpi_1_044_133[,2]
newdata_mpi_1_044_133$pmax <- pm_mpi_1_044_133[,3]

# Combined raw neutralization curves (27-38 DPI)
rawcurves_1mpi <-
  ggplot() +
  geom_point(data= mpi_1_044_101, aes(x= log_dilution, y= percent_plaque_reduction, color= '044-101')) + geom_line(data= newdata_mpi_1_044_101, aes(x= DF, y= p, color= '044-101')) +
  geom_point(data= mpi_1_044_102, aes(x= log_dilution, y= percent_plaque_reduction, color= '044-102')) + geom_line(data= newdata_mpi_1_044_102, aes(x= DF, y= p, color= '044-102')) +
  geom_point(data= mpi_1_044_103, aes(x= log_dilution, y= percent_plaque_reduction, color= '044-103')) + geom_line(data= newdata_mpi_1_044_103, aes(x= DF, y= p, color= '044-103')) +
  geom_point(data= mpi_1_044_104, aes(x= log_dilution, y= percent_plaque_reduction, color= '044-104')) + geom_line(data= newdata_mpi_1_044_104, aes(x= DF, y= p, color= '044-104')) +
  geom_point(data= mpi_1_044_109, aes(x= log_dilution, y= percent_plaque_reduction, color= '044-109')) + geom_line(data= newdata_mpi_1_044_109, aes(x= DF, y= p, color= '044-109')) +
  geom_point(data= mpi_1_044_110, aes(x= log_dilution, y= percent_plaque_reduction, color= '044-110')) + geom_line(data= newdata_mpi_1_044_110, aes(x= DF, y= p, color= '044-110')) +
  geom_point(data= mpi_1_044_112, aes(x= log_dilution, y= percent_plaque_reduction, color= '044-112')) + geom_line(data= newdata_mpi_1_044_112, aes(x= DF, y= p, color= '044-112')) +
  geom_point(data= mpi_1_044_114, aes(x= log_dilution, y= percent_plaque_reduction, color= '044-114')) + geom_line(data= newdata_mpi_1_044_114, aes(x= DF, y= p, color= '044-114')) +
  geom_point(data= mpi_1_044_116, aes(x= log_dilution, y= percent_plaque_reduction, color= '044-116')) + geom_line(data= newdata_mpi_1_044_116, aes(x= DF, y= p, color= '044-116')) +
  geom_point(data= mpi_1_044_117, aes(x= log_dilution, y= percent_plaque_reduction, color= '044-117')) + geom_line(data= newdata_mpi_1_044_117, aes(x= DF, y= p, color= '044-117')) +
  geom_point(data= mpi_1_044_118, aes(x= log_dilution, y= percent_plaque_reduction, color= '044-118')) + geom_line(data= newdata_mpi_1_044_118, aes(x= DF, y= p, color= '044-118')) +
  geom_point(data= mpi_1_044_122, aes(x= log_dilution, y= percent_plaque_reduction, color= '044-122')) + geom_line(data= newdata_mpi_1_044_122, aes(x= DF, y= p, color= '044-122')) +
  geom_point(data= mpi_1_044_126, aes(x= log_dilution, y= percent_plaque_reduction, color= '044-126')) + geom_line(data= newdata_mpi_1_044_126, aes(x= DF, y= p, color= '044-126')) +
  geom_point(data= mpi_1_044_127, aes(x= log_dilution, y= percent_plaque_reduction, color= '044-127')) + geom_line(data= newdata_mpi_1_044_127, aes(x= DF, y= p, color= '044-127')) +
  geom_point(data= mpi_1_044_130, aes(x= log_dilution, y= percent_plaque_reduction, color= '044-130')) + geom_line(data= newdata_mpi_1_044_130, aes(x= DF, y= p, color= '044-130')) +
  geom_point(data= mpi_1_044_131, aes(x= log_dilution, y= percent_plaque_reduction, color= '044-131')) + geom_line(data= newdata_mpi_1_044_131, aes(x= DF, y= p, color= '044-131')) +
  geom_point(data= mpi_1_044_132, aes(x= log_dilution, y= percent_plaque_reduction, color= '044-132')) + geom_line(data= newdata_mpi_1_044_132, aes(x= DF, y= p, color= '044-132')) +
  geom_point(data= mpi_1_044_133, aes(x= log_dilution, y= percent_plaque_reduction, color= '044-133')) + geom_line(data= newdata_mpi_1_044_133, aes(x= DF, y= p, color= '044-133')) +
  theme_classic() + labs(x = "Log (reciprocal serum dilution)", y = "Plaque Reduction (%)", title= '27-38 DPI') + xlim(1,5) +
  scale_y_continuous(expand = c(0,0), limits = c(-1,105), breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)) + 
  scale_color_manual(values = c("#7b0000", "#ad0000", "#ff0000", "#ff5d5d", "#ffa3a3", "#ffd2d2", "#000088", "#3434fb", "#6c6cff", "#9c9cff", "#d0d0ff", "#0ef000", "#c1c000", "#fffe04", '#ffd991', '#c7ad7c', '#6ca7a2', '#9c7a99'), 
                     breaks = c("044-101", "044-102", "044-103", "044-104", "044-109", "044-110", "044-112", "044-114", "044-116", "044-117", "044-118", "044-122", "044-126", "044-127", '044-130', '044-131', '044-132', '044-133')) + 
  guides(color = 'none') + geom_hline(yintercept = 90, linetype = "dashed") + geom_hline(yintercept = 50, linetype = "dashed") + 
  theme(axis.text = element_text(size = 20), axis.title = element_text(face = 'bold', size = 20), plot.title = element_text(face = "bold", hjust = 0.5, size = 20), aspect.ratio = 1)
rawcurves_1mpi  
  
# 98-136 DPI
#044-101
mpi_3.4_044_101 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\PRNT\\Final Raw Data, figures, and code\\PRNT Raw Data Analysis (Final).xlsx", sheet = 3, col_names = TRUE)
mpi_3.4_044_101$log_dilution = as.numeric(mpi_3.4_044_101$log_dilution)
str(mpi_3.4_044_101)
model_mpi_3.4_044_101 <- drm(percent_plaque_reduction~log_dilution, data = mpi_3.4_044_101, fct = LL.4(fixed = c(NA, 0, 100, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_mpi_3.4_044_101, type = "all")
summary(model_mpi_3.4_044_101)
ED(model_mpi_3.4_044_101, c(10,50), interval = "delta")
newdata_mpi_3.4_044_101 <- expand.grid(DF = exp(seq(log(1), log(4.31), length = 100)))
pm_mpi_3.4_044_101 <- predict(model_mpi_3.4_044_101, newdata = newdata_mpi_3.4_044_101, interval = "confidence")
newdata_mpi_3.4_044_101$p <- pm_mpi_3.4_044_101[,1]
newdata_mpi_3.4_044_101$pmin <- pm_mpi_3.4_044_101[,2]
newdata_mpi_3.4_044_101$pmax <- pm_mpi_3.4_044_101[,3]
#044-102
mpi_3.4_044_102 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\PRNT\\Final Raw Data, figures, and code\\PRNT Raw Data Analysis (Final).xlsx", sheet = 6, col_names = TRUE)
mpi_3.4_044_102$log_dilution = as.numeric(mpi_3.4_044_102$log_dilution)
str(mpi_3.4_044_102)
model_mpi_3.4_044_102 <- drm(percent_plaque_reduction~log_dilution, data = mpi_3.4_044_102, fct = LL.4(fixed = c(NA, 0, 100, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_mpi_3.4_044_102, type = "all")
summary(model_mpi_3.4_044_102)
ED(model_mpi_3.4_044_102, c(10,50), interval = "delta")
newdata_mpi_3.4_044_102 <- expand.grid(DF = exp(seq(log(1), log(4.31), length = 100)))
pm_mpi_3.4_044_102 <- predict(model_mpi_3.4_044_102, newdata = newdata_mpi_3.4_044_102, interval = "confidence")
newdata_mpi_3.4_044_102$p <- pm_mpi_3.4_044_102[,1]
newdata_mpi_3.4_044_102$pmin <- pm_mpi_3.4_044_102[,2]
newdata_mpi_3.4_044_102$pmax <- pm_mpi_3.4_044_102[,3]
#044-103
mpi_3.4_044_103 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\PRNT\\Final Raw Data, figures, and code\\PRNT Raw Data Analysis (Final).xlsx", sheet = 9, col_names = TRUE)
mpi_3.4_044_103$log_dilution = as.numeric(mpi_3.4_044_103$log_dilution)
str(mpi_3.4_044_103)
model_mpi_3.4_044_103 <- drm(percent_plaque_reduction~log_dilution, data = mpi_3.4_044_103, fct = LL.4(fixed = c(NA, 0, 100, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_mpi_3.4_044_103, type = "all")
summary(model_mpi_3.4_044_103)
ED(model_mpi_3.4_044_103, c(10,50), interval = "delta")
newdata_mpi_3.4_044_103 <- expand.grid(DF = exp(seq(log(1), log(4.31), length = 100)))
pm_mpi_3.4_044_103 <- predict(model_mpi_3.4_044_103, newdata = newdata_mpi_3.4_044_103, interval = "confidence")
newdata_mpi_3.4_044_103$p <- pm_mpi_3.4_044_103[,1]
newdata_mpi_3.4_044_103$pmin <- pm_mpi_3.4_044_103[,2]
newdata_mpi_3.4_044_103$pmax <- pm_mpi_3.4_044_103[,3]
#044-104
mpi_3.4_044_104 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\PRNT\\Final Raw Data, figures, and code\\PRNT Raw Data Analysis (Final).xlsx", sheet = 12, col_names = TRUE)
mpi_3.4_044_104$log_dilution = as.numeric(mpi_3.4_044_104$log_dilution)
str(mpi_3.4_044_104)
model_mpi_3.4_044_104 <- drm(percent_plaque_reduction~log_dilution, data = mpi_3.4_044_104, fct = LL.4(fixed = c(NA, 0, 100, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_mpi_3.4_044_104, type = "all")
summary(model_mpi_3.4_044_104)
ED(model_mpi_3.4_044_104, c(10,50), interval = "delta")
newdata_mpi_3.4_044_104 <- expand.grid(DF = exp(seq(log(1), log(4.31), length = 100)))
pm_mpi_3.4_044_104 <- predict(model_mpi_3.4_044_104, newdata = newdata_mpi_3.4_044_104, interval = "confidence")
newdata_mpi_3.4_044_104$p <- pm_mpi_3.4_044_104[,1]
newdata_mpi_3.4_044_104$pmin <- pm_mpi_3.4_044_104[,2]
newdata_mpi_3.4_044_104$pmax <- pm_mpi_3.4_044_104[,3]
#044-109
mpi_3.4_044_109 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\PRNT\\Final Raw Data, figures, and code\\PRNT Raw Data Analysis (Final).xlsx", sheet = 15, col_names = TRUE)
mpi_3.4_044_109$log_dilution = as.numeric(mpi_3.4_044_109$log_dilution)
str(mpi_3.4_044_109)
model_mpi_3.4_044_109 <- drm(percent_plaque_reduction~log_dilution, data = mpi_3.4_044_109, fct = LL.4(fixed = c(NA, 0, 100, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_mpi_3.4_044_109, type = "all")
summary(model_mpi_3.4_044_109)
ED(model_mpi_3.4_044_109, c(10,50), interval = "delta")
newdata_mpi_3.4_044_109 <- expand.grid(DF = exp(seq(log(1), log(4.31), length = 100)))
pm_mpi_3.4_044_109 <- predict(model_mpi_3.4_044_109, newdata = newdata_mpi_3.4_044_109, interval = "confidence")
newdata_mpi_3.4_044_109$p <- pm_mpi_3.4_044_109[,1]
newdata_mpi_3.4_044_109$pmin <- pm_mpi_3.4_044_109[,2]
newdata_mpi_3.4_044_109$pmax <- pm_mpi_3.4_044_109[,3]
#044-110
mpi_3.4_044_110 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\PRNT\\Final Raw Data, figures, and code\\PRNT Raw Data Analysis (Final).xlsx", sheet = 18, col_names = TRUE)
mpi_3.4_044_110$log_dilution = as.numeric(mpi_3.4_044_110$log_dilution)
str(mpi_3.4_044_110)
model_mpi_3.4_044_110 <- drm(percent_plaque_reduction~log_dilution, data = mpi_3.4_044_110, fct = LL.4(fixed = c(NA, 0, 100, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_mpi_3.4_044_110, type = "all")
summary(model_mpi_3.4_044_110)
ED(model_mpi_3.4_044_110, c(10,50), interval = "delta")
newdata_mpi_3.4_044_110 <- expand.grid(DF = exp(seq(log(1), log(4.31), length = 100)))
pm_mpi_3.4_044_110 <- predict(model_mpi_3.4_044_110, newdata = newdata_mpi_3.4_044_110, interval = "confidence")
newdata_mpi_3.4_044_110$p <- pm_mpi_3.4_044_110[,1]
newdata_mpi_3.4_044_110$pmin <- pm_mpi_3.4_044_110[,2]
newdata_mpi_3.4_044_110$pmax <- pm_mpi_3.4_044_110[,3]
#044-112
mpi_3.4_044_112 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\PRNT\\Final Raw Data, figures, and code\\PRNT Raw Data Analysis (Final).xlsx", sheet = 21, col_names = TRUE)
mpi_3.4_044_112$log_dilution = as.numeric(mpi_3.4_044_112$log_dilution)
str(mpi_3.4_044_112)
model_mpi_3.4_044_112 <- drm(percent_plaque_reduction~log_dilution, data = mpi_3.4_044_112, fct = LL.4(fixed = c(NA, 0, 100, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_mpi_3.4_044_112, type = "all")
summary(model_mpi_3.4_044_112)
ED(model_mpi_3.4_044_112, c(10,50), interval = "delta")
newdata_mpi_3.4_044_112 <- expand.grid(DF = exp(seq(log(1), log(4.31), length = 100)))
pm_mpi_3.4_044_112 <- predict(model_mpi_3.4_044_112, newdata = newdata_mpi_3.4_044_112, interval = "confidence")
newdata_mpi_3.4_044_112$p <- pm_mpi_3.4_044_112[,1]
newdata_mpi_3.4_044_112$pmin <- pm_mpi_3.4_044_112[,2]
newdata_mpi_3.4_044_112$pmax <- pm_mpi_3.4_044_112[,3]
#044-114
mpi_3.4_044_114 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\PRNT\\Final Raw Data, figures, and code\\PRNT Raw Data Analysis (Final).xlsx", sheet = 24, col_names = TRUE)
mpi_3.4_044_114$log_dilution = as.numeric(mpi_3.4_044_114$log_dilution)
str(mpi_3.4_044_114)
model_mpi_3.4_044_114 <- drm(percent_plaque_reduction~log_dilution, data = mpi_3.4_044_114, fct = LL.4(fixed = c(NA, 0, 100, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_mpi_3.4_044_114, type = "all")
summary(model_mpi_3.4_044_114)
ED(model_mpi_3.4_044_114, c(10,50), interval = "delta")
newdata_mpi_3.4_044_114 <- expand.grid(DF = exp(seq(log(1), log(4.31), length = 100)))
pm_mpi_3.4_044_114 <- predict(model_mpi_3.4_044_114, newdata = newdata_mpi_3.4_044_114, interval = "confidence")
newdata_mpi_3.4_044_114$p <- pm_mpi_3.4_044_114[,1]
newdata_mpi_3.4_044_114$pmin <- pm_mpi_3.4_044_114[,2]
newdata_mpi_3.4_044_114$pmax <- pm_mpi_3.4_044_114[,3]
#044-116
mpi_3.4_044_116 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\PRNT\\Final Raw Data, figures, and code\\PRNT Raw Data Analysis (Final).xlsx", sheet = 27, col_names = TRUE)
mpi_3.4_044_116$log_dilution = as.numeric(mpi_3.4_044_116$log_dilution)
str(mpi_3.4_044_116)
model_mpi_3.4_044_116 <- drm(percent_plaque_reduction~log_dilution, data = mpi_3.4_044_116, fct = LL.4(fixed = c(NA, 0, 100, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_mpi_3.4_044_116, type = "all")
summary(model_mpi_3.4_044_116)
ED(model_mpi_3.4_044_116, c(10,50), interval = "delta")
newdata_mpi_3.4_044_116 <- expand.grid(DF = exp(seq(log(1), log(4.31), length = 100)))
pm_mpi_3.4_044_116 <- predict(model_mpi_3.4_044_116, newdata = newdata_mpi_3.4_044_116, interval = "confidence")
newdata_mpi_3.4_044_116$p <- pm_mpi_3.4_044_116[,1]
newdata_mpi_3.4_044_116$pmin <- pm_mpi_3.4_044_116[,2]
newdata_mpi_3.4_044_116$pmax <- pm_mpi_3.4_044_116[,3]
#044-117
mpi_3.4_044_117 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\PRNT\\Final Raw Data, figures, and code\\PRNT Raw Data Analysis (Final).xlsx", sheet = 30, col_names = TRUE)
mpi_3.4_044_117$log_dilution = as.numeric(mpi_3.4_044_117$log_dilution)
str(mpi_3.4_044_117)
model_mpi_3.4_044_117 <- drm(percent_plaque_reduction~log_dilution, data = mpi_3.4_044_117, fct = LL.4(fixed = c(NA, 0, 100, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_mpi_3.4_044_117, type = "all")
summary(model_mpi_3.4_044_117)
ED(model_mpi_3.4_044_117, c(10,50), interval = "delta")
newdata_mpi_3.4_044_117 <- expand.grid(DF = exp(seq(log(1), log(4.31), length = 100)))
pm_mpi_3.4_044_117 <- predict(model_mpi_3.4_044_117, newdata = newdata_mpi_3.4_044_117, interval = "confidence")
newdata_mpi_3.4_044_117$p <- pm_mpi_3.4_044_117[,1]
newdata_mpi_3.4_044_117$pmin <- pm_mpi_3.4_044_117[,2]
newdata_mpi_3.4_044_117$pmax <- pm_mpi_3.4_044_117[,3]
#044-118
mpi_3.4_044_118 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\PRNT\\Final Raw Data, figures, and code\\PRNT Raw Data Analysis (Final).xlsx", sheet = 33, col_names = TRUE)
mpi_3.4_044_118$log_dilution = as.numeric(mpi_3.4_044_118$log_dilution)
str(mpi_3.4_044_118)
model_mpi_3.4_044_118 <- drm(percent_plaque_reduction~log_dilution, data = mpi_3.4_044_118, fct = LL.4(fixed = c(NA, 0, 100, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_mpi_3.4_044_118, type = "all")
summary(model_mpi_3.4_044_118)
ED(model_mpi_3.4_044_118, c(10,50), interval = "delta")
newdata_mpi_3.4_044_118 <- expand.grid(DF = exp(seq(log(1), log(4.31), length = 100)))
pm_mpi_3.4_044_118 <- predict(model_mpi_3.4_044_118, newdata = newdata_mpi_3.4_044_118, interval = "confidence")
newdata_mpi_3.4_044_118$p <- pm_mpi_3.4_044_118[,1]
newdata_mpi_3.4_044_118$pmin <- pm_mpi_3.4_044_118[,2]
newdata_mpi_3.4_044_118$pmax <- pm_mpi_3.4_044_118[,3]
#044-122
mpi_3.4_044_122 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\PRNT\\Final Raw Data, figures, and code\\PRNT Raw Data Analysis (Final).xlsx", sheet = 36, col_names = TRUE)
mpi_3.4_044_122$log_dilution = as.numeric(mpi_3.4_044_122$log_dilution)
str(mpi_3.4_044_122)
model_mpi_3.4_044_122 <- drm(percent_plaque_reduction~log_dilution, data = mpi_3.4_044_122, fct = LL.4(fixed = c(NA, 0, 100, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_mpi_3.4_044_122, type = "all")
summary(model_mpi_3.4_044_122)
ED(model_mpi_3.4_044_122, c(10,50), interval = "delta")
newdata_mpi_3.4_044_122 <- expand.grid(DF = exp(seq(log(1), log(4.31), length = 100)))
pm_mpi_3.4_044_122 <- predict(model_mpi_3.4_044_122, newdata = newdata_mpi_3.4_044_122, interval = "confidence")
newdata_mpi_3.4_044_122$p <- pm_mpi_3.4_044_122[,1]
newdata_mpi_3.4_044_122$pmin <- pm_mpi_3.4_044_122[,2]
newdata_mpi_3.4_044_122$pmax <- pm_mpi_3.4_044_122[,3]
#044-126
mpi_3.4_044_126 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\PRNT\\Final Raw Data, figures, and code\\PRNT Raw Data Analysis (Final).xlsx", sheet = 39, col_names = TRUE)
mpi_3.4_044_126$log_dilution = as.numeric(mpi_3.4_044_126$log_dilution)
str(mpi_3.4_044_126)
model_mpi_3.4_044_126 <- drm(percent_plaque_reduction~log_dilution, data = mpi_3.4_044_126, fct = LL.4(fixed = c(NA, 0, 100, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_mpi_3.4_044_126, type = "all")
summary(model_mpi_3.4_044_126)
ED(model_mpi_3.4_044_126, c(10,50), interval = "delta")
newdata_mpi_3.4_044_126 <- expand.grid(DF = exp(seq(log(1), log(4.31), length = 100)))
pm_mpi_3.4_044_126 <- predict(model_mpi_3.4_044_126, newdata = newdata_mpi_3.4_044_126, interval = "confidence")
newdata_mpi_3.4_044_126$p <- pm_mpi_3.4_044_126[,1]
newdata_mpi_3.4_044_126$pmin <- pm_mpi_3.4_044_126[,2]
newdata_mpi_3.4_044_126$pmax <- pm_mpi_3.4_044_126[,3]
#044-127
mpi_3.4_044_127 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\PRNT\\Final Raw Data, figures, and code\\PRNT Raw Data Analysis (Final).xlsx", sheet = 42, col_names = TRUE)
mpi_3.4_044_127$log_dilution = as.numeric(mpi_3.4_044_127$log_dilution)
str(mpi_3.4_044_127)
model_mpi_3.4_044_127 <- drm(percent_plaque_reduction~log_dilution, data = mpi_3.4_044_127, fct = LL.4(fixed = c(NA, 0, 100, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_mpi_3.4_044_127, type = "all")
summary(model_mpi_3.4_044_127)
ED(model_mpi_3.4_044_127, c(10,50), interval = "delta")
newdata_mpi_3.4_044_127 <- expand.grid(DF = exp(seq(log(1), log(4.31), length = 100)))
pm_mpi_3.4_044_127 <- predict(model_mpi_3.4_044_127, newdata = newdata_mpi_3.4_044_127, interval = "confidence")
newdata_mpi_3.4_044_127$p <- pm_mpi_3.4_044_127[,1]
newdata_mpi_3.4_044_127$pmin <- pm_mpi_3.4_044_127[,2]
newdata_mpi_3.4_044_127$pmax <- pm_mpi_3.4_044_127[,3]
#044-130
mpi_3.4_044_130 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\PRNT\\Final Raw Data, figures, and code\\PRNT Raw Data Analysis (Final).xlsx", sheet = 45, col_names = TRUE)
mpi_3.4_044_130$log_dilution = as.numeric(mpi_3.4_044_130$log_dilution)
str(mpi_3.4_044_130)
model_mpi_3.4_044_130 <- drm(percent_plaque_reduction~log_dilution, data = mpi_3.4_044_130, fct = LL.4(fixed = c(NA, 0, 100, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_mpi_3.4_044_130, type = "all")
summary(model_mpi_3.4_044_130)
ED(model_mpi_3.4_044_130, c(10,50), interval = "delta")
newdata_mpi_3.4_044_130 <- expand.grid(DF = exp(seq(log(1), log(4.31), length = 100)))
pm_mpi_3.4_044_130 <- predict(model_mpi_3.4_044_130, newdata = newdata_mpi_3.4_044_130, interval = "confidence")
newdata_mpi_3.4_044_130$p <- pm_mpi_3.4_044_130[,1]
newdata_mpi_3.4_044_130$pmin <- pm_mpi_3.4_044_130[,2]
newdata_mpi_3.4_044_130$pmax <- pm_mpi_3.4_044_130[,3]
#044-131
mpi_3.4_044_131 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\PRNT\\Final Raw Data, figures, and code\\PRNT Raw Data Analysis (Final).xlsx", sheet = 48, col_names = TRUE)
mpi_3.4_044_131$log_dilution = as.numeric(mpi_3.4_044_131$log_dilution)
str(mpi_3.4_044_131)
model_mpi_3.4_044_131 <- drm(percent_plaque_reduction~log_dilution, data = mpi_3.4_044_131, fct = LL.4(fixed = c(NA, 0, 100, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_mpi_3.4_044_131, type = "all")
summary(model_mpi_3.4_044_131)
ED(model_mpi_3.4_044_131, c(10,50), interval = "delta")
newdata_mpi_3.4_044_131 <- expand.grid(DF = exp(seq(log(1), log(4.31), length = 100)))
pm_mpi_3.4_044_131 <- predict(model_mpi_3.4_044_131, newdata = newdata_mpi_3.4_044_131, interval = "confidence")
newdata_mpi_3.4_044_131$p <- pm_mpi_3.4_044_131[,1]
newdata_mpi_3.4_044_131$pmin <- pm_mpi_3.4_044_131[,2]
newdata_mpi_3.4_044_131$pmax <- pm_mpi_3.4_044_131[,3]
#044-132
mpi_3.4_044_132 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\PRNT\\Final Raw Data, figures, and code\\PRNT Raw Data Analysis (Final).xlsx", sheet = 51, col_names = TRUE)
mpi_3.4_044_132$log_dilution = as.numeric(mpi_3.4_044_132$log_dilution)
str(mpi_3.4_044_132)
model_mpi_3.4_044_132 <- drm(percent_plaque_reduction~log_dilution, data = mpi_3.4_044_132, fct = LL.4(fixed = c(NA, 0, 100, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_mpi_3.4_044_132, type = "all")
summary(model_mpi_3.4_044_132)
ED(model_mpi_3.4_044_132, c(10,50), interval = "delta")
newdata_mpi_3.4_044_132 <- expand.grid(DF = exp(seq(log(1), log(4.31), length = 100)))
pm_mpi_3.4_044_132 <- predict(model_mpi_3.4_044_132, newdata = newdata_mpi_3.4_044_132, interval = "confidence")
newdata_mpi_3.4_044_132$p <- pm_mpi_3.4_044_132[,1]
newdata_mpi_3.4_044_132$pmin <- pm_mpi_3.4_044_132[,2]
newdata_mpi_3.4_044_132$pmax <- pm_mpi_3.4_044_132[,3]
#044-133
mpi_3.4_044_133 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\PRNT\\Final Raw Data, figures, and code\\PRNT Raw Data Analysis (Final).xlsx", sheet = 54, col_names = TRUE)
mpi_3.4_044_133$log_dilution = as.numeric(mpi_3.4_044_133$log_dilution)
str(mpi_3.4_044_133)
model_mpi_3.4_044_133 <- drm(percent_plaque_reduction~log_dilution, data = mpi_3.4_044_133, fct = LL.4(fixed = c(NA, 0, 100, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_mpi_3.4_044_133, type = "all")
summary(model_mpi_3.4_044_133)
ED(model_mpi_3.4_044_133, c(10,50), interval = "delta")
newdata_mpi_3.4_044_133 <- expand.grid(DF = exp(seq(log(1), log(4.31), length = 100)))
pm_mpi_3.4_044_133 <- predict(model_mpi_3.4_044_133, newdata = newdata_mpi_3.4_044_133, interval = "confidence")
newdata_mpi_3.4_044_133$p <- pm_mpi_3.4_044_133[,1]
newdata_mpi_3.4_044_133$pmin <- pm_mpi_3.4_044_133[,2]
newdata_mpi_3.4_044_133$pmax <- pm_mpi_3.4_044_133[,3]

# Combined raw neutralization curves (98-136 DPI)
rawcurves_3.4mpi <-
  ggplot() +
  geom_point(data= mpi_3.4_044_101, aes(x= log_dilution, y= percent_plaque_reduction, color= '044-101')) + geom_line(data= newdata_mpi_3.4_044_101, aes(x= DF, y= p, color= '044-101')) +
  geom_point(data= mpi_3.4_044_102, aes(x= log_dilution, y= percent_plaque_reduction, color= '044-102')) + geom_line(data= newdata_mpi_3.4_044_102, aes(x= DF, y= p, color= '044-102')) +
  geom_point(data= mpi_3.4_044_103, aes(x= log_dilution, y= percent_plaque_reduction, color= '044-103')) + geom_line(data= newdata_mpi_3.4_044_103, aes(x= DF, y= p, color= '044-103')) +
  geom_point(data= mpi_3.4_044_104, aes(x= log_dilution, y= percent_plaque_reduction, color= '044-104')) + geom_line(data= newdata_mpi_3.4_044_104, aes(x= DF, y= p, color= '044-104')) +
  geom_point(data= mpi_3.4_044_109, aes(x= log_dilution, y= percent_plaque_reduction, color= '044-109')) + geom_line(data= newdata_mpi_3.4_044_109, aes(x= DF, y= p, color= '044-109')) +
  geom_point(data= mpi_3.4_044_110, aes(x= log_dilution, y= percent_plaque_reduction, color= '044-110')) + geom_line(data= newdata_mpi_3.4_044_110, aes(x= DF, y= p, color= '044-110')) +
  geom_point(data= mpi_3.4_044_112, aes(x= log_dilution, y= percent_plaque_reduction, color= '044-112')) + geom_line(data= newdata_mpi_3.4_044_112, aes(x= DF, y= p, color= '044-112')) +
  geom_point(data= mpi_3.4_044_114, aes(x= log_dilution, y= percent_plaque_reduction, color= '044-114')) + geom_line(data= newdata_mpi_3.4_044_114, aes(x= DF, y= p, color= '044-114')) +
  geom_point(data= mpi_3.4_044_116, aes(x= log_dilution, y= percent_plaque_reduction, color= '044-116')) + geom_line(data= newdata_mpi_3.4_044_116, aes(x= DF, y= p, color= '044-116')) +
  geom_point(data= mpi_3.4_044_117, aes(x= log_dilution, y= percent_plaque_reduction, color= '044-117')) + geom_line(data= newdata_mpi_3.4_044_117, aes(x= DF, y= p, color= '044-117')) +
  geom_point(data= mpi_3.4_044_118, aes(x= log_dilution, y= percent_plaque_reduction, color= '044-118')) + geom_line(data= newdata_mpi_3.4_044_118, aes(x= DF, y= p, color= '044-118')) +
  geom_point(data= mpi_3.4_044_122, aes(x= log_dilution, y= percent_plaque_reduction, color= '044-122')) + geom_line(data= newdata_mpi_3.4_044_122, aes(x= DF, y= p, color= '044-122')) +
  geom_point(data= mpi_3.4_044_126, aes(x= log_dilution, y= percent_plaque_reduction, color= '044-126')) + geom_line(data= newdata_mpi_3.4_044_126, aes(x= DF, y= p, color= '044-126')) +
  geom_point(data= mpi_3.4_044_127, aes(x= log_dilution, y= percent_plaque_reduction, color= '044-127')) + geom_line(data= newdata_mpi_3.4_044_127, aes(x= DF, y= p, color= '044-127')) +
  geom_point(data= mpi_3.4_044_130, aes(x= log_dilution, y= percent_plaque_reduction, color= '044-130')) + geom_line(data= newdata_mpi_3.4_044_130, aes(x= DF, y= p, color= '044-130')) +
  geom_point(data= mpi_3.4_044_131, aes(x= log_dilution, y= percent_plaque_reduction, color= '044-131')) + geom_line(data= newdata_mpi_3.4_044_131, aes(x= DF, y= p, color= '044-131')) +
  geom_point(data= mpi_3.4_044_132, aes(x= log_dilution, y= percent_plaque_reduction, color= '044-132')) + geom_line(data= newdata_mpi_3.4_044_132, aes(x= DF, y= p, color= '044-132')) +
  geom_point(data= mpi_3.4_044_133, aes(x= log_dilution, y= percent_plaque_reduction, color= '044-133')) + geom_line(data= newdata_mpi_3.4_044_133, aes(x= DF, y= p, color= '044-133')) +
  theme_classic() + labs(x = "Log (reciprocal serum dilution)", y = "Plaque Reduction (%)", title= '98-136 DPI') + xlim(1,5) +
  scale_y_continuous(expand = c(0,0), limits = c(-1,105), breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)) + 
  scale_color_manual(values = c("#7b0000", "#ad0000", "#ff0000", "#ff5d5d", "#ffa3a3", "#ffd2d2", "#000088", "#3434fb", "#6c6cff", "#9c9cff", "#d0d0ff", "#0ef000", "#c1c000", "#fffe04", '#ffd991', '#c7ad7c', '#6ca7a2', '#9c7a99'), 
                     breaks = c("044-101", "044-102", "044-103", "044-104", "044-109", "044-110", "044-112", "044-114", "044-116", "044-117", "044-118", "044-122", "044-126", "044-127", '044-130', '044-131', '044-132', '044-133')) + 
  guides(color = 'none') + geom_hline(yintercept = 90, linetype = "dashed") + geom_hline(yintercept = 50, linetype = "dashed") + 
  theme(axis.text = element_text(size = 20), axis.title = element_text(face = 'bold', size = 20), plot.title = element_text(face = "bold", hjust = 0.5, size = 20), aspect.ratio = 1)
rawcurves_3.4mpi  

###
#Combining figures into a single multipanel figure
###
tiff("Combined Raw Curves (Final).tiff", units= 'in', width = 15, height = 5, res= 300)
ggarrange(Preinfection_rawcurves, rawcurves_1mpi, rawcurves_3.4mpi, common.legend= TRUE, legend = 'top', align = 'hv', nrow = 1, ncol= 3, labels = c('A', 'B', 'C'))
dev.off()

