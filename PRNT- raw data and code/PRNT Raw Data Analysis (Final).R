library(tidyverse)
library(drc)
library(readxl)
library(ggplot2)
library(egg)
library(gtable)
###
# PRNT raw data analysis, neutralization curves, and dose-response model generation for estimating PRNT90 and PRNT50 titers
###

### 044-101
#Pre-infection
pre_044_101 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\PRNT\\Final Raw Data, figures, and code\\PRNT Raw Data Analysis (Final).xlsx", sheet = 1, col_names = TRUE)
pre_044_101$log_dilution = as.numeric(pre_044_101$log_dilution)
#1 month post infection
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
# 3-4 months post infection
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
#PRNT Raw Neutralization curves
PR_044_101 <- 
  ggplot() + geom_point(data = pre_044_101, aes(x = log_dilution, y = percent_plaque_reduction, color = "044-101", shape = "Pre-Infection")) + geom_line(data = pre_044_101, aes(x = log_dilution, y = percent_plaque_reduction, color = "044-101", linetype = "Pre-Infection")) + 
  geom_point(data = mpi_1_044_101, aes(x = log_dilution, y = percent_plaque_reduction, color = "044-101", shape = "1 MPI")) + geom_line(data = newdata_mpi_1_044_101, aes(x = DF, y = p, color = "044-101", linetype = "1 MPI")) + 
  geom_point(data = mpi_3.4_044_101, aes(x = log_dilution, y = percent_plaque_reduction, color = "044-101", shape = "3-4 MPI")) + geom_line(data = newdata_mpi_3.4_044_101, aes(x = DF, y = p, color = "044-101", linetype = "3-4 MPI")) + 
  scale_x_continuous(expand = c(0,0), limits = c(0.5, 4.5), breaks = c(0, 1, 2, 3, 4)) + theme_classic() + 
  scale_color_manual(values = c("#7b0000"), breaks = c("044-101")) + xlab(bquote(Log[10] ~Sera ~Dilution)) + 
  scale_y_continuous(name = "Percent Plaque Reduction", expand = c(0,0), limits = c(-3, 103), breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)) + 
  labs(title = "044-101") + 
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14, face = "bold"), plot.title = element_text(face = 'bold', size = 16, hjust = 0.5), aspect.ratio = 1) + 
  guides(shape = guide_legend("Timepoint"), color = "none", linetype = guide_legend("Timepoint")) + 
  scale_shape_manual(values = c(16, 0, 2), breaks = c("Pre-Infection", "1 MPI", "3-4 MPI")) + 
  scale_linetype_manual(values = c("solid", "dashed", "dotted"), breaks = c("Pre-Infection", "1 MPI", "3-4 MPI"))
PR_044_101


### 044-102
#Pre-infection
pre_044_102 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\PRNT\\Final Raw Data, figures, and code\\PRNT Raw Data Analysis (Final).xlsx", sheet = 4, col_names = TRUE)
pre_044_102$log_dilution = as.numeric(pre_044_102$log_dilution)
#1 month post infection
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
# 3-4 months post infection
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
#PRNT Raw Neutralization curves
PR_044_102 <- 
  ggplot() + geom_point(data = pre_044_102, aes(x = log_dilution, y = percent_plaque_reduction, color = "044-102", shape = "Pre-Infection")) + geom_line(data = pre_044_102, aes(x = log_dilution, y = percent_plaque_reduction, color = "044-102", linetype = "Pre-Infection")) + 
  geom_point(data = mpi_1_044_102, aes(x = log_dilution, y = percent_plaque_reduction, color = "044-102", shape = "1 MPI")) + geom_line(data = newdata_mpi_1_044_102, aes(x = DF, y = p, color = "044-102", linetype = "1 MPI")) + 
  geom_point(data = mpi_3.4_044_102, aes(x = log_dilution, y = percent_plaque_reduction, color = "044-102", shape = "3-4 MPI")) + geom_line(data = newdata_mpi_3.4_044_102, aes(x = DF, y = p, color = "044-102", linetype = "3-4 MPI")) + 
  scale_x_continuous(expand = c(0,0), limits = c(0.5, 4.5), breaks = c(0, 1, 2, 3, 4)) + theme_classic() + 
  scale_color_manual(values = c("#Ad0000"), breaks = c("044-102")) + xlab(bquote(Log[10] ~Sera ~Dilution)) + 
  scale_y_continuous(name = "Percent Plaque Reduction", expand = c(0,0), limits = c(-3, 103), breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)) + 
  labs(title = "044-102") + 
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14, face = "bold"), plot.title = element_text(face = 'bold', size = 16, hjust = 0.5), aspect.ratio = 1) + 
  guides(shape = guide_legend("Timepoint"), color = "none", linetype = guide_legend("Timepoint")) + 
  scale_shape_manual(values = c(16, 0, 2), breaks = c("Pre-Infection", "1 MPI", "3-4 MPI")) + 
  scale_linetype_manual(values = c("solid", "dashed", "dotted"), breaks = c("Pre-Infection", "1 MPI", "3-4 MPI"))
PR_044_102

### 044-103
#Pre-infection
pre_044_103 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\PRNT\\Final Raw Data, figures, and code\\PRNT Raw Data Analysis (Final).xlsx", sheet = 7, col_names = TRUE)
pre_044_103$log_dilution = as.numeric(pre_044_103$log_dilution)
#1 month post infection
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
# 3-4 months post infection
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
#PRNT Raw Neutralization curves
PR_044_103 <- 
  ggplot() + geom_point(data = pre_044_103, aes(x = log_dilution, y = percent_plaque_reduction, color = "044-103", shape = "Pre-Infection")) + geom_line(data = pre_044_103, aes(x = log_dilution, y = percent_plaque_reduction, color = "044-103", linetype = "Pre-Infection")) + 
  geom_point(data = mpi_1_044_103, aes(x = log_dilution, y = percent_plaque_reduction, color = "044-103", shape = "1 MPI")) + geom_line(data = newdata_mpi_1_044_103, aes(x = DF, y = p, color = "044-103", linetype = "1 MPI")) + 
  geom_point(data = mpi_3.4_044_103, aes(x = log_dilution, y = percent_plaque_reduction, color = "044-103", shape = "3-4 MPI")) + geom_line(data = newdata_mpi_3.4_044_103, aes(x = DF, y = p, color = "044-103", linetype = "3-4 MPI")) + 
  scale_x_continuous(expand = c(0,0), limits = c(0.5, 4.5), breaks = c(0, 1, 2, 3, 4)) + theme_classic() + 
  scale_color_manual(values = c("#ff0000"), breaks = c("044-103")) + xlab(bquote(Log[10] ~Sera ~Dilution)) + 
  scale_y_continuous(name = "Percent Plaque Reduction", expand = c(0,0), limits = c(-3, 103), breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)) + 
  labs(title = "044-103") + 
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14, face = "bold"), plot.title = element_text(face = 'bold', size = 16, hjust = 0.5), aspect.ratio = 1) + 
  guides(shape = guide_legend("Timepoint"), color = "none", linetype = guide_legend("Timepoint")) + 
  scale_shape_manual(values = c(16, 0, 2), breaks = c("Pre-Infection", "1 MPI", "3-4 MPI")) + 
  scale_linetype_manual(values = c("solid", "dashed", "dotted"), breaks = c("Pre-Infection", "1 MPI", "3-4 MPI"))
PR_044_103

### 044-104
#Pre-infection
pre_044_104 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\PRNT\\Final Raw Data, figures, and code\\PRNT Raw Data Analysis (Final).xlsx", sheet = 10, col_names = TRUE)
pre_044_104$log_dilution = as.numeric(pre_044_104$log_dilution)
#1 month post infection
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
# 3-4 months post infection
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
#PRNT Raw Neutralization curves
PR_044_104 <- 
  ggplot() + geom_point(data = pre_044_104, aes(x = log_dilution, y = percent_plaque_reduction, color = "044-104", shape = "Pre-Infection")) + geom_line(data = pre_044_104, aes(x = log_dilution, y = percent_plaque_reduction, color = "044-104", linetype = "Pre-Infection")) + 
  geom_point(data = mpi_1_044_104, aes(x = log_dilution, y = percent_plaque_reduction, color = "044-104", shape = "1 MPI")) + geom_line(data = newdata_mpi_1_044_104, aes(x = DF, y = p, color = "044-104", linetype = "1 MPI")) + 
  geom_point(data = mpi_3.4_044_104, aes(x = log_dilution, y = percent_plaque_reduction, color = "044-104", shape = "3-4 MPI")) + geom_line(data = newdata_mpi_3.4_044_104, aes(x = DF, y = p, color = "044-104", linetype = "3-4 MPI")) + 
  scale_x_continuous(expand = c(0,0), limits = c(0.5, 4.5), breaks = c(0, 1, 2, 3, 4)) + theme_classic() + 
  scale_color_manual(values = c("#ff5d5d"), breaks = c("044-104")) + xlab(bquote(Log[10] ~Sera ~Dilution)) + 
  scale_y_continuous(name = "Percent Plaque Reduction", expand = c(0,0), limits = c(-3, 103), breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)) + 
  labs(title = "044-104") + 
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14, face = "bold"), plot.title = element_text(face = 'bold', size = 16, hjust = 0.5), aspect.ratio = 1) + 
  guides(shape = guide_legend("Timepoint"), color = "none", linetype = guide_legend("Timepoint")) + 
  scale_shape_manual(values = c(16, 0, 2), breaks = c("Pre-Infection", "1 MPI", "3-4 MPI")) + 
  scale_linetype_manual(values = c("solid", "dashed", "dotted"), breaks = c("Pre-Infection", "1 MPI", "3-4 MPI"))
PR_044_104

### 044-109
#Pre-infection
pre_044_109 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\PRNT\\Final Raw Data, figures, and code\\PRNT Raw Data Analysis (Final).xlsx", sheet = 13, col_names = TRUE)
pre_044_109$log_dilution = as.numeric(pre_044_109$log_dilution)
#1 month post infection
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
# 3-4 months post infection
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
#PRNT Raw Neutralization curves
PR_044_109 <- 
  ggplot() + geom_point(data = pre_044_109, aes(x = log_dilution, y = percent_plaque_reduction, color = "044-109", shape = "Pre-Infection")) + geom_line(data = pre_044_109, aes(x = log_dilution, y = percent_plaque_reduction, color = "044-109", linetype = "Pre-Infection")) + 
  geom_point(data = mpi_1_044_109, aes(x = log_dilution, y = percent_plaque_reduction, color = "044-109", shape = "1 MPI")) + geom_line(data = newdata_mpi_1_044_109, aes(x = DF, y = p, color = "044-109", linetype = "1 MPI")) + 
  geom_point(data = mpi_3.4_044_109, aes(x = log_dilution, y = percent_plaque_reduction, color = "044-109", shape = "3-4 MPI")) + geom_line(data = newdata_mpi_3.4_044_109, aes(x = DF, y = p, color = "044-109", linetype = "3-4 MPI")) + 
  scale_x_continuous(expand = c(0,0), limits = c(0.5, 4.5), breaks = c(0, 1, 2, 3, 4)) + theme_classic() + 
  scale_color_manual(values = c("#ffa3a3"), breaks = c("044-109")) + xlab(bquote(Log[10] ~Sera ~Dilution)) + 
  scale_y_continuous(name = "Percent Plaque Reduction", expand = c(0,0), limits = c(-3, 103), breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)) + 
  labs(title = "044-109") + 
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14, face = "bold"), plot.title = element_text(face = 'bold', size = 16, hjust = 0.5), aspect.ratio = 1) + 
  guides(shape = guide_legend("Timepoint"), color = "none", linetype = guide_legend("Timepoint")) + 
  scale_shape_manual(values = c(16, 0, 2), breaks = c("Pre-Infection", "1 MPI", "3-4 MPI")) + 
  scale_linetype_manual(values = c("solid", "dashed", "dotted"), breaks = c("Pre-Infection", "1 MPI", "3-4 MPI"))
PR_044_109

### 044-110
#Pre-infection
pre_044_110 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\PRNT\\Final Raw Data, figures, and code\\PRNT Raw Data Analysis (Final).xlsx", sheet = 16, col_names = TRUE)
pre_044_110$log_dilution = as.numeric(pre_044_110$log_dilution)
#1 month post infection
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
# 3-4 months post infection
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
#PRNT Raw Neutralization curves
PR_044_110 <- 
  ggplot() + geom_point(data = pre_044_110, aes(x = log_dilution, y = percent_plaque_reduction, color = "044-110", shape = "Pre-Infection")) + geom_line(data = pre_044_110, aes(x = log_dilution, y = percent_plaque_reduction, color = "044-110", linetype = "Pre-Infection")) + 
  geom_point(data = mpi_1_044_110, aes(x = log_dilution, y = percent_plaque_reduction, color = "044-110", shape = "1 MPI")) + geom_line(data = newdata_mpi_1_044_110, aes(x = DF, y = p, color = "044-110", linetype = "1 MPI")) + 
  geom_point(data = mpi_3.4_044_110, aes(x = log_dilution, y = percent_plaque_reduction, color = "044-110", shape = "3-4 MPI")) + geom_line(data = newdata_mpi_3.4_044_110, aes(x = DF, y = p, color = "044-110", linetype = "3-4 MPI")) + 
  scale_x_continuous(expand = c(0,0), limits = c(0.5, 4.5), breaks = c(0, 1, 2, 3, 4)) + theme_classic() + 
  scale_color_manual(values = c("#ffd2d2"), breaks = c("044-110")) + xlab(bquote(Log[10] ~Sera ~Dilution)) + 
  scale_y_continuous(name = "Percent Plaque Reduction", expand = c(0,0), limits = c(-3, 103), breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)) + 
  labs(title = "044-110") + 
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14, face = "bold"), plot.title = element_text(face = 'bold', size = 16, hjust = 0.5), aspect.ratio = 1) + 
  guides(shape = guide_legend("Timepoint"), color = "none", linetype = guide_legend("Timepoint")) + 
  scale_shape_manual(values = c(16, 0, 2), breaks = c("Pre-Infection", "1 MPI", "3-4 MPI")) + 
  scale_linetype_manual(values = c("solid", "dashed", "dotted"), breaks = c("Pre-Infection", "1 MPI", "3-4 MPI"))
PR_044_110

### 044-112
#Pre-infection
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
#1 month post infection
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
# 3-4 months post infection
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
#PRNT Raw Neutralization curves
PR_044_112 <- 
  ggplot() + geom_point(data = pre_044_112, aes(x = log_dilution, y = percent_plaque_reduction, color = "044-112", shape = "Pre-Infection")) + geom_line(data = newdata_pre_044_112, aes(x = DF, y = p, color = "044-112", linetype = "Pre-Infection")) + 
  geom_point(data = mpi_1_044_112, aes(x = log_dilution, y = percent_plaque_reduction, color = "044-112", shape = "1 MPI")) + geom_line(data = newdata_mpi_1_044_112, aes(x = DF, y = p, color = "044-112", linetype = "1 MPI")) + 
  geom_point(data = mpi_3.4_044_112, aes(x = log_dilution, y = percent_plaque_reduction, color = "044-112", shape = "3-4 MPI")) + geom_line(data = newdata_mpi_3.4_044_112, aes(x = DF, y = p, color = "044-112", linetype = "3-4 MPI")) + 
  scale_x_continuous(expand = c(0,0), limits = c(0.5, 4.5), breaks = c(0, 1, 2, 3, 4)) + theme_classic() + 
  scale_color_manual(values = c("#000088"), breaks = c("044-112")) + xlab(bquote(Log[10] ~Sera ~Dilution)) + 
  scale_y_continuous(name = "Percent Plaque Reduction", expand = c(0,0), limits = c(-3, 103), breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)) + 
  labs(title = "044-112") + 
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14, face = "bold"), plot.title = element_text(face = 'bold', size = 16, hjust = 0.5), aspect.ratio = 1) + 
  guides(shape = guide_legend("Timepoint"), color = "none", linetype = guide_legend("Timepoint")) + 
  scale_shape_manual(values = c(16, 0, 2), breaks = c("Pre-Infection", "1 MPI", "3-4 MPI")) + 
  scale_linetype_manual(values = c("solid", "dashed", "dotted"), breaks = c("Pre-Infection", "1 MPI", "3-4 MPI"))
PR_044_112

### 044-114
#Pre-infection
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
#1 month post infection
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
# 3-4 months post infection
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
#PRNT Raw Neutralization curves
PR_044_114 <- 
  ggplot() + geom_point(data = pre_044_114, aes(x = log_dilution, y = percent_plaque_reduction, color = "044-114", shape = "Pre-Infection")) + geom_line(data = newdata_pre_044_114, aes(x = DF, y = p, color = "044-114", linetype = "Pre-Infection")) + 
  geom_point(data = mpi_1_044_114, aes(x = log_dilution, y = percent_plaque_reduction, color = "044-114", shape = "1 MPI")) + geom_line(data = newdata_mpi_1_044_114, aes(x = DF, y = p, color = "044-114", linetype = "1 MPI")) + 
  geom_point(data = mpi_3.4_044_114, aes(x = log_dilution, y = percent_plaque_reduction, color = "044-114", shape = "3-4 MPI")) + geom_line(data = newdata_mpi_3.4_044_114, aes(x = DF, y = p, color = "044-114", linetype = "3-4 MPI")) + 
  scale_x_continuous(expand = c(0,0), limits = c(0.5, 4.5), breaks = c(0, 1, 2, 3, 4)) + theme_classic() + 
  scale_color_manual(values = c("#3434fb"), breaks = c("044-114")) + xlab(bquote(Log[10] ~Sera ~Dilution)) + 
  scale_y_continuous(name = "Percent Plaque Reduction", expand = c(0,0), limits = c(-3, 103), breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)) + 
  labs(title = "044-114") + 
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14, face = "bold"), plot.title = element_text(face = 'bold', size = 16, hjust = 0.5), aspect.ratio = 1) + 
  guides(shape = guide_legend("Timepoint"), color = "none", linetype = guide_legend("Timepoint")) + 
  scale_shape_manual(values = c(16, 0, 2), breaks = c("Pre-Infection", "1 MPI", "3-4 MPI")) + 
  scale_linetype_manual(values = c("solid", "dashed", "dotted"), breaks = c("Pre-Infection", "1 MPI", "3-4 MPI"))
PR_044_114

### 044-116
#Pre-infection
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
#1 month post infection
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
# 3-4 months post infection
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
#PRNT Raw Neutralization curves
PR_044_116 <- 
  ggplot() + geom_point(data = pre_044_116, aes(x = log_dilution, y = percent_plaque_reduction, color = "044-116", shape = "Pre-Infection")) + geom_line(data = newdata_pre_044_116, aes(x = DF, y = p, color = "044-116", linetype = "Pre-Infection")) + 
  geom_point(data = mpi_1_044_116, aes(x = log_dilution, y = percent_plaque_reduction, color = "044-116", shape = "1 MPI")) + geom_line(data = newdata_mpi_1_044_116, aes(x = DF, y = p, color = "044-116", linetype = "1 MPI")) + 
  geom_point(data = mpi_3.4_044_116, aes(x = log_dilution, y = percent_plaque_reduction, color = "044-116", shape = "3-4 MPI")) + geom_line(data = newdata_mpi_3.4_044_116, aes(x = DF, y = p, color = "044-116", linetype = "3-4 MPI")) + 
  scale_x_continuous(expand = c(0,0), limits = c(0.5, 4.5), breaks = c(0, 1, 2, 3, 4)) + theme_classic() + 
  scale_color_manual(values = c("#6c6cff"), breaks = c("044-116")) + xlab(bquote(Log[10] ~Sera ~Dilution)) + 
  scale_y_continuous(name = "Percent Plaque Reduction", expand = c(0,0), limits = c(-3, 103), breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)) + 
  labs(title = "044-116") + 
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14, face = "bold"), plot.title = element_text(face = 'bold', size = 16, hjust = 0.5), aspect.ratio = 1) + 
  guides(shape = guide_legend("Timepoint"), color = "none", linetype = guide_legend("Timepoint")) + 
  scale_shape_manual(values = c(16, 0, 2), breaks = c("Pre-Infection", "1 MPI", "3-4 MPI")) + 
  scale_linetype_manual(values = c("solid", "dashed", "dotted"), breaks = c("Pre-Infection", "1 MPI", "3-4 MPI"))
PR_044_116

### 044-117
#Pre-infection
pre_044_117 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\PRNT\\Final Raw Data, figures, and code\\PRNT Raw Data Analysis (Final).xlsx", sheet = 28, col_names = TRUE)
pre_044_117$log_dilution = as.numeric(pre_044_117$log_dilution)
#1 month post infection
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
# 3-4 months post infection
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
#PRNT Raw Neutralization curves
PR_044_117 <- 
  ggplot() + geom_point(data = pre_044_117, aes(x = log_dilution, y = percent_plaque_reduction, color = "044-117", shape = "Pre-Infection")) + geom_line(data = pre_044_117, aes(x = log_dilution, y = percent_plaque_reduction, color = "044-117", linetype = "Pre-Infection")) + 
  geom_point(data = mpi_1_044_117, aes(x = log_dilution, y = percent_plaque_reduction, color = "044-117", shape = "1 MPI")) + geom_line(data = newdata_mpi_1_044_117, aes(x = DF, y = p, color = "044-117", linetype = "1 MPI")) + 
  geom_point(data = mpi_3.4_044_117, aes(x = log_dilution, y = percent_plaque_reduction, color = "044-117", shape = "3-4 MPI")) + geom_line(data = newdata_mpi_3.4_044_117, aes(x = DF, y = p, color = "044-117", linetype = "3-4 MPI")) + 
  scale_x_continuous(expand = c(0,0), limits = c(0.5, 4.5), breaks = c(0, 1, 2, 3, 4)) + theme_classic() + 
  scale_color_manual(values = c("#9c9cff"), breaks = c("044-117")) + xlab(bquote(Log[10] ~Sera ~Dilution)) + 
  scale_y_continuous(name = "Percent Plaque Reduction", expand = c(0,0), limits = c(-3, 103), breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)) + 
  labs(title = "044-117") + 
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14, face = "bold"), plot.title = element_text(face = 'bold', size = 16, hjust = 0.5), aspect.ratio = 1) + 
  guides(shape = guide_legend("Timepoint"), color = "none", linetype = guide_legend("Timepoint")) + 
  scale_shape_manual(values = c(16, 0, 2), breaks = c("Pre-Infection", "1 MPI", "3-4 MPI")) + 
  scale_linetype_manual(values = c("solid", "dashed", "dotted"), breaks = c("Pre-Infection", "1 MPI", "3-4 MPI"))
PR_044_117

### 044-118
#Pre-infection
pre_044_118 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\PRNT\\Final Raw Data, figures, and code\\PRNT Raw Data Analysis (Final).xlsx", sheet = 31, col_names = TRUE)
pre_044_118$log_dilution = as.numeric(pre_044_118$log_dilution)
#1 month post infection
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
# 3-4 months post infection
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
#PRNT Raw Neutralization curves
PR_044_118 <- 
  ggplot() + geom_point(data = pre_044_118, aes(x = log_dilution, y = percent_plaque_reduction, color = "044-118", shape = "Pre-Infection")) + geom_line(data = pre_044_118, aes(x = log_dilution, y = percent_plaque_reduction, color = "044-118", linetype = "Pre-Infection")) + 
  geom_point(data = mpi_1_044_118, aes(x = log_dilution, y = percent_plaque_reduction, color = "044-118", shape = "1 MPI")) + geom_line(data = newdata_mpi_1_044_118, aes(x = DF, y = p, color = "044-118", linetype = "1 MPI")) + 
  geom_point(data = mpi_3.4_044_118, aes(x = log_dilution, y = percent_plaque_reduction, color = "044-118", shape = "3-4 MPI")) + geom_line(data = newdata_mpi_3.4_044_118, aes(x = DF, y = p, color = "044-118", linetype = "3-4 MPI")) + 
  scale_x_continuous(expand = c(0,0), limits = c(0.5, 4.5), breaks = c(0, 1, 2, 3, 4)) + theme_classic() + 
  scale_color_manual(values = c("#d0d0ff"), breaks = c("044-118")) + xlab(bquote(Log[10] ~Sera ~Dilution)) + 
  scale_y_continuous(name = "Percent Plaque Reduction", expand = c(0,0), limits = c(-3, 103), breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)) + 
  labs(title = "044-118") + 
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14, face = "bold"), plot.title = element_text(face = 'bold', size = 16, hjust = 0.5), aspect.ratio = 1) + 
  guides(shape = guide_legend("Timepoint"), color = "none", linetype = guide_legend("Timepoint")) + 
  scale_shape_manual(values = c(16, 0, 2), breaks = c("Pre-Infection", "1 MPI", "3-4 MPI")) + 
  scale_linetype_manual(values = c("solid", "dashed", "dotted"), breaks = c("Pre-Infection", "1 MPI", "3-4 MPI"))
PR_044_118

### 044-122
#Pre-infection
pre_044_122 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\PRNT\\Final Raw Data, figures, and code\\PRNT Raw Data Analysis (Final).xlsx", sheet = 34, col_names = TRUE)
pre_044_122$log_dilution = as.numeric(pre_044_122$log_dilution)
#1 month post infection
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
# 3-4 months post infection
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
#PRNT Raw Neutralization curves
PR_044_122 <- 
  ggplot() + geom_point(data = pre_044_122, aes(x = log_dilution, y = percent_plaque_reduction, color = "044-122", shape = "Pre-Infection")) + geom_line(data = pre_044_122, aes(x = log_dilution, y = percent_plaque_reduction, color = "044-122", linetype = "Pre-Infection")) + 
  geom_point(data = mpi_1_044_122, aes(x = log_dilution, y = percent_plaque_reduction, color = "044-122", shape = "1 MPI")) + geom_line(data = newdata_mpi_1_044_122, aes(x = DF, y = p, color = "044-122", linetype = "1 MPI")) + 
  geom_point(data = mpi_3.4_044_122, aes(x = log_dilution, y = percent_plaque_reduction, color = "044-122", shape = "3-4 MPI")) + geom_line(data = newdata_mpi_3.4_044_122, aes(x = DF, y = p, color = "044-122", linetype = "3-4 MPI")) + 
  scale_x_continuous(expand = c(0,0), limits = c(0.5, 4.5), breaks = c(0, 1, 2, 3, 4)) + theme_classic() + 
  scale_color_manual(values = c("#0ef000"), breaks = c("044-122")) + xlab(bquote(Log[10] ~Sera ~Dilution)) + 
  scale_y_continuous(name = "Percent Plaque Reduction", expand = c(0,0), limits = c(-3, 103), breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)) + 
  labs(title = "044-122") + 
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14, face = "bold"), plot.title = element_text(face = 'bold', size = 16, hjust = 0.5), aspect.ratio = 1) + 
  guides(shape = guide_legend("Timepoint"), color = "none", linetype = guide_legend("Timepoint")) + 
  scale_shape_manual(values = c(16, 0, 2), breaks = c("Pre-Infection", "1 MPI", "3-4 MPI")) + 
  scale_linetype_manual(values = c("solid", "dashed", "dotted"), breaks = c("Pre-Infection", "1 MPI", "3-4 MPI"))
PR_044_122

### 044-126
#Pre-infection
pre_044_126 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\PRNT\\Final Raw Data, figures, and code\\PRNT Raw Data Analysis (Final).xlsx", sheet = 37, col_names = TRUE)
pre_044_126$log_dilution = as.numeric(pre_044_126$log_dilution)
#1 month post infection
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
# 3-4 months post infection
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
#PRNT Raw Neutralization curves
PR_044_126 <- 
  ggplot() + geom_point(data = pre_044_126, aes(x = log_dilution, y = percent_plaque_reduction, color = "044-126", shape = "Pre-Infection")) + geom_line(data = pre_044_126, aes(x = log_dilution, y = percent_plaque_reduction, color = "044-126", linetype = "Pre-Infection")) + 
  geom_point(data = mpi_1_044_126, aes(x = log_dilution, y = percent_plaque_reduction, color = "044-126", shape = "1 MPI")) + geom_line(data = newdata_mpi_1_044_126, aes(x = DF, y = p, color = "044-126", linetype = "1 MPI")) + 
  geom_point(data = mpi_3.4_044_126, aes(x = log_dilution, y = percent_plaque_reduction, color = "044-126", shape = "3-4 MPI")) + geom_line(data = newdata_mpi_3.4_044_126, aes(x = DF, y = p, color = "044-126", linetype = "3-4 MPI")) + 
  scale_x_continuous(expand = c(0,0), limits = c(0.5, 4.5), breaks = c(0, 1, 2, 3, 4)) + theme_classic() + 
  scale_color_manual(values = c("#c1c000"), breaks = c("044-126")) + xlab(bquote(Log[10] ~Sera ~Dilution)) + 
  scale_y_continuous(name = "Percent Plaque Reduction", expand = c(0,0), limits = c(-3, 103), breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)) + 
  labs(title = "044-126") + 
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14, face = "bold"), plot.title = element_text(face = 'bold', size = 16, hjust = 0.5), aspect.ratio = 1) + 
  guides(shape = guide_legend("Timepoint"), color = "none", linetype = guide_legend("Timepoint")) + 
  scale_shape_manual(values = c(16, 0, 2), breaks = c("Pre-Infection", "1 MPI", "3-4 MPI")) + 
  scale_linetype_manual(values = c("solid", "dashed", "dotted"), breaks = c("Pre-Infection", "1 MPI", "3-4 MPI"))
PR_044_126

### 044-127
#Pre-infection
pre_044_127 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\PRNT\\Final Raw Data, figures, and code\\PRNT Raw Data Analysis (Final).xlsx", sheet = 40, col_names = TRUE)
pre_044_127$log_dilution = as.numeric(pre_044_127$log_dilution)
#1 month post infection
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
# 3-4 months post infection
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
#PRNT Raw Neutralization curves
PR_044_127 <- 
  ggplot() + geom_point(data = pre_044_127, aes(x = log_dilution, y = percent_plaque_reduction, color = "044-127", shape = "Pre-Infection")) + geom_line(data = pre_044_127, aes(x = log_dilution, y = percent_plaque_reduction, color = "044-127", linetype = "Pre-Infection")) + 
  geom_point(data = mpi_1_044_127, aes(x = log_dilution, y = percent_plaque_reduction, color = "044-127", shape = "1 MPI")) + geom_line(data = newdata_mpi_1_044_127, aes(x = DF, y = p, color = "044-127", linetype = "1 MPI")) + 
  geom_point(data = mpi_3.4_044_127, aes(x = log_dilution, y = percent_plaque_reduction, color = "044-127", shape = "3-4 MPI")) + geom_line(data = newdata_mpi_3.4_044_127, aes(x = DF, y = p, color = "044-127", linetype = "3-4 MPI")) + 
  scale_x_continuous(expand = c(0,0), limits = c(0.5, 4.5), breaks = c(0, 1, 2, 3, 4)) + theme_classic() + 
  scale_color_manual(values = c("#fffe04"), breaks = c("044-127")) + xlab(bquote(Log[10] ~Sera ~Dilution)) + 
  scale_y_continuous(name = "Percent Plaque Reduction", expand = c(0,0), limits = c(-3, 103), breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)) + 
  labs(title = "044-127") + 
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14, face = "bold"), plot.title = element_text(face = 'bold', size = 16, hjust = 0.5), aspect.ratio = 1) + 
  guides(shape = guide_legend("Timepoint"), color = "none", linetype = guide_legend("Timepoint")) + 
  scale_shape_manual(values = c(16, 0, 2), breaks = c("Pre-Infection", "1 MPI", "3-4 MPI")) + 
  scale_linetype_manual(values = c("solid", "dashed", "dotted"), breaks = c("Pre-Infection", "1 MPI", "3-4 MPI"))
PR_044_127

### 044-130
#Pre-infection
pre_044_130 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\PRNT\\Final Raw Data, figures, and code\\PRNT Raw Data Analysis (Final).xlsx", sheet = 43, col_names = TRUE)
pre_044_130$log_dilution = as.numeric(pre_044_130$log_dilution)
#1 month post infection
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
# 3-4 months post infection
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
#PRNT Raw Neutralization curves
PR_044_130 <- 
  ggplot() + geom_point(data = pre_044_130, aes(x = log_dilution, y = percent_plaque_reduction, color = "044-130", shape = "Pre-Infection")) + geom_line(data = pre_044_130, aes(x = log_dilution, y = percent_plaque_reduction, color = "044-130", linetype = "Pre-Infection")) + 
  geom_point(data = mpi_1_044_130, aes(x = log_dilution, y = percent_plaque_reduction, color = "044-130", shape = "1 MPI")) + geom_line(data = newdata_mpi_1_044_130, aes(x = DF, y = p, color = "044-130", linetype = "1 MPI")) + 
  geom_point(data = mpi_3.4_044_130, aes(x = log_dilution, y = percent_plaque_reduction, color = "044-130", shape = "3-4 MPI")) + geom_line(data = newdata_mpi_3.4_044_130, aes(x = DF, y = p, color = "044-130", linetype = "3-4 MPI")) + 
  scale_x_continuous(expand = c(0,0), limits = c(0.5, 4.5), breaks = c(0, 1, 2, 3, 4)) + theme_classic() + 
  scale_color_manual(values = c("#ffd991"), breaks = c("044-130")) + xlab(bquote(Log[10] ~Sera ~Dilution)) + 
  scale_y_continuous(name = "Percent Plaque Reduction", expand = c(0,0), limits = c(-3, 103), breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)) + 
  labs(title = "044-130") + 
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14, face = "bold"), plot.title = element_text(face = 'bold', size = 16, hjust = 0.5), aspect.ratio = 1) + 
  guides(shape = guide_legend("Timepoint"), color = "none", linetype = guide_legend("Timepoint")) + 
  scale_shape_manual(values = c(16, 0, 2), breaks = c("Pre-Infection", "1 MPI", "3-4 MPI")) + 
  scale_linetype_manual(values = c("solid", "dashed", "dotted"), breaks = c("Pre-Infection", "1 MPI", "3-4 MPI"))
PR_044_130

### 044-131
#Pre-infection
pre_044_131 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\PRNT\\Final Raw Data, figures, and code\\PRNT Raw Data Analysis (Final).xlsx", sheet = 46, col_names = TRUE)
pre_044_131$log_dilution = as.numeric(pre_044_131$log_dilution)
#1 month post infection
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
# 3-4 months post infection
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
#PRNT Raw Neutralization curves
PR_044_131 <- 
  ggplot() + geom_point(data = pre_044_131, aes(x = log_dilution, y = percent_plaque_reduction, color = "044-131", shape = "Pre-Infection")) + geom_line(data = pre_044_131, aes(x = log_dilution, y = percent_plaque_reduction, color = "044-131", linetype = "Pre-Infection")) + 
  geom_point(data = mpi_1_044_131, aes(x = log_dilution, y = percent_plaque_reduction, color = "044-131", shape = "1 MPI")) + geom_line(data = newdata_mpi_1_044_131, aes(x = DF, y = p, color = "044-131", linetype = "1 MPI")) + 
  geom_point(data = mpi_3.4_044_131, aes(x = log_dilution, y = percent_plaque_reduction, color = "044-131", shape = "3-4 MPI")) + geom_line(data = newdata_mpi_3.4_044_131, aes(x = DF, y = p, color = "044-131", linetype = "3-4 MPI")) + 
  scale_x_continuous(expand = c(0,0), limits = c(0.5, 4.5), breaks = c(0, 1, 2, 3, 4)) + theme_classic() + 
  scale_color_manual(values = c("#c7ad7c"), breaks = c("044-131")) + xlab(bquote(Log[10] ~Sera ~Dilution)) + 
  scale_y_continuous(name = "Percent Plaque Reduction", expand = c(0,0), limits = c(-3, 103), breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)) + 
  labs(title = "044-131") + 
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14, face = "bold"), plot.title = element_text(face = 'bold', size = 16, hjust = 0.5), aspect.ratio = 1) + 
  guides(shape = guide_legend("Timepoint"), color = "none", linetype = guide_legend("Timepoint")) + 
  scale_shape_manual(values = c(16, 0, 2), breaks = c("Pre-Infection", "1 MPI", "3-4 MPI")) + 
  scale_linetype_manual(values = c("solid", "dashed", "dotted"), breaks = c("Pre-Infection", "1 MPI", "3-4 MPI"))
PR_044_131

### 044-132
#Pre-infection
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
#1 month post infection
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
# 3-4 months post infection
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
#PRNT Raw Neutralization curves
PR_044_132 <- 
  ggplot() + geom_point(data = pre_044_132, aes(x = log_dilution, y = percent_plaque_reduction, color = "044-132", shape = "Pre-Infection")) + geom_line(data = newdata_pre_044_132, aes(x = DF, y = p, color = "044-132", linetype = "Pre-Infection")) + 
  geom_point(data = mpi_1_044_132, aes(x = log_dilution, y = percent_plaque_reduction, color = "044-132", shape = "1 MPI")) + geom_line(data = newdata_mpi_1_044_132, aes(x = DF, y = p, color = "044-132", linetype = "1 MPI")) + 
  geom_point(data = mpi_3.4_044_132, aes(x = log_dilution, y = percent_plaque_reduction, color = "044-132", shape = "3-4 MPI")) + geom_line(data = newdata_mpi_3.4_044_132, aes(x = DF, y = p, color = "044-132", linetype = "3-4 MPI")) + 
  scale_x_continuous(expand = c(0,0), limits = c(0.5, 4.5), breaks = c(0, 1, 2, 3, 4)) + theme_classic() + 
  scale_color_manual(values = c("#6ca7a2"), breaks = c("044-132")) + xlab(bquote(Log[10] ~Sera ~Dilution)) + 
  scale_y_continuous(name = "Percent Plaque Reduction", expand = c(0,0), limits = c(-3, 103), breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)) + 
  labs(title = "044-132") + 
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14, face = "bold"), plot.title = element_text(face = 'bold', size = 16, hjust = 0.5), aspect.ratio = 1) + 
  guides(shape = guide_legend("Timepoint"), color = "none", linetype = guide_legend("Timepoint")) + 
  scale_shape_manual(values = c(16, 0, 2), breaks = c("Pre-Infection", "1 MPI", "3-4 MPI")) + 
  scale_linetype_manual(values = c("solid", "dashed", "dotted"), breaks = c("Pre-Infection", "1 MPI", "3-4 MPI"))
PR_044_132

### 044-133
#Pre-infection
pre_044_133 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\PRNT\\Final Raw Data, figures, and code\\PRNT Raw Data Analysis (Final).xlsx", sheet = 52, col_names = TRUE)
pre_044_133$log_dilution = as.numeric(pre_044_133$log_dilution)
#1 month post infection
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
# 3-4 months post infection
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
#PRNT Raw Neutralization curves
PR_044_133 <- 
  ggplot() + geom_point(data = pre_044_133, aes(x = log_dilution, y = percent_plaque_reduction, color = "044-133", shape = "Pre-Infection")) + geom_line(data = pre_044_133, aes(x = log_dilution, y = percent_plaque_reduction, color = "044-133", linetype = "Pre-Infection")) + 
  geom_point(data = mpi_1_044_133, aes(x = log_dilution, y = percent_plaque_reduction, color = "044-133", shape = "1 MPI")) + geom_line(data = newdata_mpi_1_044_133, aes(x = DF, y = p, color = "044-133", linetype = "1 MPI")) + 
  geom_point(data = mpi_3.4_044_133, aes(x = log_dilution, y = percent_plaque_reduction, color = "044-133", shape = "3-4 MPI")) + geom_line(data = newdata_mpi_3.4_044_133, aes(x = DF, y = p, color = "044-133", linetype = "3-4 MPI")) + 
  scale_x_continuous(expand = c(0,0), limits = c(0.5, 4.5), breaks = c(0, 1, 2, 3, 4)) + theme_classic() + 
  scale_color_manual(values = c("#9c7a99"), breaks = c("044-133")) + xlab(bquote(Log[10] ~Sera ~Dilution)) + 
  scale_y_continuous(name = "Percent Plaque Reduction", expand = c(0,0), limits = c(-3, 103), breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)) + 
  labs(title = "044-133") + 
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14, face = "bold"), plot.title = element_text(face = 'bold', size = 16, hjust = 0.5), aspect.ratio = 1) + 
  guides(shape = guide_legend("Timepoint"), color = "none", linetype = guide_legend("Timepoint")) + 
  scale_shape_manual(values = c(16, 0, 2), breaks = c("Pre-Infection", "1 MPI", "3-4 MPI")) + 
  scale_linetype_manual(values = c("solid", "dashed", "dotted"), breaks = c("Pre-Infection", "1 MPI", "3-4 MPI"))
PR_044_133


