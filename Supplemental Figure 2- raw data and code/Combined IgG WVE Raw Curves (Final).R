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
# Combined IgG WVE Raw Binding Curves (Pre-Infection)
###
# 044-101
pre_044_101 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\Combined Raw Curves (Final)\\Pre-Infection IgG Raw Curves (Final).xlsx", sheet = 1, col_names = TRUE)
pre_044_101$Log_Reciprocal_Serum_Dilution = as.numeric(pre_044_101$Log_Reciprocal_Serum_Dilution)
str(pre_044_101)
model_pre_044_101 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = pre_044_101, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_pre_044_101, type = "all")
summary(model_pre_044_101)
ED(model_pre_044_101, c(10,50), interval = "delta")
newdata_pre_044_101 <- expand.grid(Log_Reciprocal_Serum_Dilution = exp(seq(log(1.09), log(5.31), length = 100)))
pm_pre_044_101 <- predict(model_pre_044_101, newdata = newdata_pre_044_101, interval = "confidence")
newdata_pre_044_101$p <- pm_pre_044_101[,1]
newdata_pre_044_101$pmin <- pm_pre_044_101[,2]
newdata_pre_044_101$pmax <- pm_pre_044_101[,3]
# 044-102
pre_044_102 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\Combined Raw Curves (Final)\\Pre-Infection IgG Raw Curves (Final).xlsx", sheet = 2, col_names = TRUE)
pre_044_102$Log_Reciprocal_Serum_Dilution = as.numeric(pre_044_102$Log_Reciprocal_Serum_Dilution)
str(pre_044_102)
model_pre_044_102 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = pre_044_102, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_pre_044_102, type = "all")
summary(model_pre_044_102)
ED(model_pre_044_102, c(10,50), interval = "delta")
newdata_pre_044_102 <- expand.grid(Log_Reciprocal_Serum_Dilution = exp(seq(log(1.09), log(5.31), length = 100)))
pm_pre_044_102 <- predict(model_pre_044_102, newdata = newdata_pre_044_102, interval = "confidence")
newdata_pre_044_102$p <- pm_pre_044_102[,1]
newdata_pre_044_102$pmin <- pm_pre_044_102[,2]
newdata_pre_044_102$pmax <- pm_pre_044_102[,3]
# 044-103
pre_044_103 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\Combined Raw Curves (Final)\\Pre-Infection IgG Raw Curves (Final).xlsx", sheet = 3, col_names = TRUE)
pre_044_103$Log_Reciprocal_Serum_Dilution = as.numeric(pre_044_103$Log_Reciprocal_Serum_Dilution)
str(pre_044_103)
model_pre_044_103 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = pre_044_103, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_pre_044_103, type = "all")
summary(model_pre_044_103)
ED(model_pre_044_103, c(10,50), interval = "delta")
newdata_pre_044_103 <- expand.grid(Log_Reciprocal_Serum_Dilution = exp(seq(log(1.09), log(5.31), length = 100)))
pm_pre_044_103 <- predict(model_pre_044_103, newdata = newdata_pre_044_103, interval = "confidence")
newdata_pre_044_103$p <- pm_pre_044_103[,1]
newdata_pre_044_103$pmin <- pm_pre_044_103[,2]
newdata_pre_044_103$pmax <- pm_pre_044_103[,3]
# 044-104 (No pre-infection sample run)
# 044-109
pre_044_109 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\Combined Raw Curves (Final)\\Pre-Infection IgG Raw Curves (Final).xlsx", sheet = 4, col_names = TRUE)
pre_044_109$Log_Reciprocal_Serum_Dilution = as.numeric(pre_044_109$Log_Reciprocal_Serum_Dilution)
str(pre_044_109)
model_pre_044_109 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = pre_044_109, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_pre_044_109, type = "all")
summary(model_pre_044_109)
ED(model_pre_044_109, c(10,50), interval = "delta")
newdata_pre_044_109 <- expand.grid(Log_Reciprocal_Serum_Dilution = exp(seq(log(1.09), log(5.31), length = 100)))
pm_pre_044_109 <- predict(model_pre_044_109, newdata = newdata_pre_044_109, interval = "confidence")
newdata_pre_044_109$p <- pm_pre_044_109[,1]
newdata_pre_044_109$pmin <- pm_pre_044_109[,2]
newdata_pre_044_109$pmax <- pm_pre_044_109[,3]
# 044-110
pre_044_110 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\Combined Raw Curves (Final)\\Pre-Infection IgG Raw Curves (Final).xlsx", sheet = 5, col_names = TRUE)
pre_044_110$Log_Reciprocal_Serum_Dilution = as.numeric(pre_044_110$Log_Reciprocal_Serum_Dilution)
str(pre_044_110)
model_pre_044_110 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = pre_044_110, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_pre_044_110, type = "all")
summary(model_pre_044_110)
ED(model_pre_044_110, c(10,50), interval = "delta")
newdata_pre_044_110 <- expand.grid(Log_Reciprocal_Serum_Dilution = exp(seq(log(1.09), log(5.31), length = 100)))
pm_pre_044_110 <- predict(model_pre_044_110, newdata = newdata_pre_044_110, interval = "confidence")
newdata_pre_044_110$p <- pm_pre_044_110[,1]
newdata_pre_044_110$pmin <- pm_pre_044_110[,2]
newdata_pre_044_110$pmax <- pm_pre_044_110[,3]
# 044-112
pre_044_112 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\Combined Raw Curves (Final)\\Pre-Infection IgG Raw Curves (Final).xlsx", sheet = 6, col_names = TRUE)
pre_044_112$Log_Reciprocal_Serum_Dilution = as.numeric(pre_044_112$Log_Reciprocal_Serum_Dilution)
str(pre_044_112)
model_pre_044_112 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = pre_044_112, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_pre_044_112, type = "all")
summary(model_pre_044_112)
ED(model_pre_044_112, c(10,50), interval = "delta")
newdata_pre_044_112 <- expand.grid(Log_Reciprocal_Serum_Dilution = exp(seq(log(1.09), log(5.31), length = 100)))
pm_pre_044_112 <- predict(model_pre_044_112, newdata = newdata_pre_044_112, interval = "confidence")
newdata_pre_044_112$p <- pm_pre_044_112[,1]
newdata_pre_044_112$pmin <- pm_pre_044_112[,2]
newdata_pre_044_112$pmax <- pm_pre_044_112[,3]
#044-114
pre_044_114 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\Combined Raw Curves (Final)\\Pre-Infection IgG Raw Curves (Final).xlsx", sheet = 7, col_names = TRUE)
pre_044_114$Log_Reciprocal_Serum_Dilution = as.numeric(pre_044_114$Log_Reciprocal_Serum_Dilution)
str(pre_044_114)
model_pre_044_114 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = pre_044_114, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_pre_044_114, type = "all")
summary(model_pre_044_114)
ED(model_pre_044_114, c(10,50), interval = "delta")
newdata_pre_044_114 <- expand.grid(Log_Reciprocal_Serum_Dilution = exp(seq(log(1.09), log(5.31), length = 100)))
pm_pre_044_114 <- predict(model_pre_044_114, newdata = newdata_pre_044_114, interval = "confidence")
newdata_pre_044_114$p <- pm_pre_044_114[,1]
newdata_pre_044_114$pmin <- pm_pre_044_114[,2]
newdata_pre_044_114$pmax <- pm_pre_044_114[,3]
# 044-116
pre_044_116 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\Combined Raw Curves (Final)\\Pre-Infection IgG Raw Curves (Final).xlsx", sheet = 8, col_names = TRUE)
pre_044_116$Log_Reciprocal_Serum_Dilution = as.numeric(pre_044_116$Log_Reciprocal_Serum_Dilution)
str(pre_044_116)
model_pre_044_116 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = pre_044_116, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_pre_044_116, type = "all")
summary(model_pre_044_116)
ED(model_pre_044_116, c(10,50), interval = "delta")
newdata_pre_044_116 <- expand.grid(Log_Reciprocal_Serum_Dilution = exp(seq(log(1.09), log(5.31), length = 100)))
pm_pre_044_116 <- predict(model_pre_044_116, newdata = newdata_pre_044_116, interval = "confidence")
newdata_pre_044_116$p <- pm_pre_044_116[,1]
newdata_pre_044_116$pmin <- pm_pre_044_116[,2]
newdata_pre_044_116$pmax <- pm_pre_044_116[,3]
# 044-117
pre_044_117 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\Combined Raw Curves (Final)\\Pre-Infection IgG Raw Curves (Final).xlsx", sheet = 9, col_names = TRUE)
pre_044_117$Log_Reciprocal_Serum_Dilution = as.numeric(pre_044_117$Log_Reciprocal_Serum_Dilution)
str(pre_044_117)
model_pre_044_117 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = pre_044_117, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_pre_044_117, type = "all")
summary(model_pre_044_117)
ED(model_pre_044_117, c(10,50), interval = "delta")
newdata_pre_044_117 <- expand.grid(Log_Reciprocal_Serum_Dilution = exp(seq(log(1.09), log(5.31), length = 100)))
pm_pre_044_117 <- predict(model_pre_044_117, newdata = newdata_pre_044_117, interval = "confidence")
newdata_pre_044_117$p <- pm_pre_044_117[,1]
newdata_pre_044_117$pmin <- pm_pre_044_117[,2]
newdata_pre_044_117$pmax <- pm_pre_044_117[,3]
# 044-118
pre_044_118 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\Combined Raw Curves (Final)\\Pre-Infection IgG Raw Curves (Final).xlsx", sheet = 10, col_names = TRUE)
pre_044_118$Log_Reciprocal_Serum_Dilution = as.numeric(pre_044_118$Log_Reciprocal_Serum_Dilution)
str(pre_044_118)
model_pre_044_118 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = pre_044_118, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_pre_044_118, type = "all")
summary(model_pre_044_118)
ED(model_pre_044_118, c(10,50), interval = "delta")
newdata_pre_044_118 <- expand.grid(Log_Reciprocal_Serum_Dilution = exp(seq(log(1.09), log(5.31), length = 100)))
pm_pre_044_118 <- predict(model_pre_044_118, newdata = newdata_pre_044_118, interval = "confidence")
newdata_pre_044_118$p <- pm_pre_044_118[,1]
newdata_pre_044_118$pmin <- pm_pre_044_118[,2]
newdata_pre_044_118$pmax <- pm_pre_044_118[,3]
# 044-122
pre_044_122 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\Combined Raw Curves (Final)\\Pre-Infection IgG Raw Curves (Final).xlsx", sheet = 11, col_names = TRUE)
pre_044_122$Log_Reciprocal_Serum_Dilution = as.numeric(pre_044_122$Log_Reciprocal_Serum_Dilution)
str(pre_044_122)
model_pre_044_122 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = pre_044_122, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_pre_044_122, type = "all")
summary(model_pre_044_122)
ED(model_pre_044_122, c(10,50), interval = "delta")
newdata_pre_044_122 <- expand.grid(Log_Reciprocal_Serum_Dilution = exp(seq(log(1.7), log(5.31), length = 100)))
pm_pre_044_122 <- predict(model_pre_044_122, newdata = newdata_pre_044_122, interval = "confidence")
newdata_pre_044_122$p <- pm_pre_044_122[,1]
newdata_pre_044_122$pmin <- pm_pre_044_122[,2]
newdata_pre_044_122$pmax <- pm_pre_044_122[,3]
# 044-126
pre_044_126 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\Combined Raw Curves (Final)\\Pre-Infection IgG Raw Curves (Final).xlsx", sheet = 12, col_names = TRUE)
pre_044_126$Log_Reciprocal_Serum_Dilution = as.numeric(pre_044_126$Log_Reciprocal_Serum_Dilution)
str(pre_044_126)
model_pre_044_126 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = pre_044_126, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_pre_044_126, type = "all")
summary(model_pre_044_126)
ED(model_pre_044_126, c(10,50), interval = "delta")
newdata_pre_044_126 <- expand.grid(Log_Reciprocal_Serum_Dilution = exp(seq(log(1.09), log(5.31), length = 100)))
pm_pre_044_126 <- predict(model_pre_044_126, newdata = newdata_pre_044_126, interval = "confidence")
newdata_pre_044_126$p <- pm_pre_044_126[,1]
newdata_pre_044_126$pmin <- pm_pre_044_126[,2]
newdata_pre_044_126$pmax <- pm_pre_044_126[,3]
# 044-127
pre_044_127 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\Combined Raw Curves (Final)\\Pre-Infection IgG Raw Curves (Final).xlsx", sheet = 13, col_names = TRUE)
pre_044_127$Log_Reciprocal_Serum_Dilution = as.numeric(pre_044_127$Log_Reciprocal_Serum_Dilution)
str(pre_044_127)
model_pre_044_127 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = pre_044_127, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_pre_044_127, type = "all")
summary(model_pre_044_127)
ED(model_pre_044_127, c(10,50), interval = "delta")
newdata_pre_044_127 <- expand.grid(Log_Reciprocal_Serum_Dilution = exp(seq(log(1.09), log(5.31), length = 100)))
pm_pre_044_127 <- predict(model_pre_044_127, newdata = newdata_pre_044_127, interval = "confidence")
newdata_pre_044_127$p <- pm_pre_044_127[,1]
newdata_pre_044_127$pmin <- pm_pre_044_127[,2]
newdata_pre_044_127$pmax <- pm_pre_044_127[,3]
# 044-130
pre_044_130 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\Combined Raw Curves (Final)\\Pre-Infection IgG Raw Curves (Final).xlsx", sheet = 14, col_names = TRUE)
pre_044_130$Log_Reciprocal_Serum_Dilution = as.numeric(pre_044_130$Log_Reciprocal_Serum_Dilution)
str(pre_044_130)
model_pre_044_130 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = pre_044_130, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_pre_044_130, type = "all")
summary(model_pre_044_130)
ED(model_pre_044_130, c(10,50), interval = "delta")
newdata_pre_044_130 <- expand.grid(Log_Reciprocal_Serum_Dilution = exp(seq(log(1.09), log(5.31), length = 100)))
pm_pre_044_130 <- predict(model_pre_044_130, newdata = newdata_pre_044_130, interval = "confidence")
newdata_pre_044_130$p <- pm_pre_044_130[,1]
newdata_pre_044_130$pmin <- pm_pre_044_130[,2]
newdata_pre_044_130$pmax <- pm_pre_044_130[,3]
# 044-131
pre_044_131 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\Combined Raw Curves (Final)\\Pre-Infection IgG Raw Curves (Final).xlsx", sheet = 15, col_names = TRUE)
pre_044_131$Log_Reciprocal_Serum_Dilution = as.numeric(pre_044_131$Log_Reciprocal_Serum_Dilution)
str(pre_044_131)
model_pre_044_131 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = pre_044_131, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_pre_044_131, type = "all")
summary(model_pre_044_131)
ED(model_pre_044_131, c(10,50), interval = "delta")
newdata_pre_044_131 <- expand.grid(Log_Reciprocal_Serum_Dilution = exp(seq(log(1.09), log(5.31), length = 100)))
pm_pre_044_131 <- predict(model_pre_044_131, newdata = newdata_pre_044_131, interval = "confidence")
newdata_pre_044_131$p <- pm_pre_044_131[,1]
newdata_pre_044_131$pmin <- pm_pre_044_131[,2]
newdata_pre_044_131$pmax <- pm_pre_044_131[,3]
# 044-132
pre_044_132 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\Combined Raw Curves (Final)\\Pre-Infection IgG Raw Curves (Final).xlsx", sheet = 16, col_names = TRUE)
pre_044_132$Log_Reciprocal_Serum_Dilution = as.numeric(pre_044_132$Log_Reciprocal_Serum_Dilution)
str(pre_044_132)
model_pre_044_132 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = pre_044_132, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_pre_044_132, type = "all")
summary(model_pre_044_132)
ED(model_pre_044_132, c(10,50), interval = "delta")
newdata_pre_044_132 <- expand.grid(Log_Reciprocal_Serum_Dilution = exp(seq(log(1.09), log(5.31), length = 100)))
pm_pre_044_132 <- predict(model_pre_044_132, newdata = newdata_pre_044_132, interval = "confidence")
newdata_pre_044_132$p <- pm_pre_044_132[,1]
newdata_pre_044_132$pmin <- pm_pre_044_132[,2]
newdata_pre_044_132$pmax <- pm_pre_044_132[,3]
# 044-133
pre_044_133 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\Combined Raw Curves (Final)\\Pre-Infection IgG Raw Curves (Final).xlsx", sheet = 17, col_names = TRUE)
pre_044_133$Log_Reciprocal_Serum_Dilution = as.numeric(pre_044_133$Log_Reciprocal_Serum_Dilution)
str(pre_044_133)
model_pre_044_133 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = pre_044_133, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_pre_044_133, type = "all")
summary(model_pre_044_133)
ED(model_pre_044_133, c(10,50), interval = "delta")
newdata_pre_044_133 <- expand.grid(Log_Reciprocal_Serum_Dilution = exp(seq(log(1.09), log(5.31), length = 100)))
pm_pre_044_133 <- predict(model_pre_044_133, newdata = newdata_pre_044_133, interval = "confidence")
newdata_pre_044_133$p <- pm_pre_044_133[,1]
newdata_pre_044_133$pmin <- pm_pre_044_133[,2]
newdata_pre_044_133$pmax <- pm_pre_044_133[,3]
# Combined IgG WVE Raw Binding Curves figure (Pre-Infection)
rawcurves_preinfection <- 
  ggplot() + 
  geom_point(data = pre_044_101, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "044-101")) + geom_line(data = newdata_pre_044_101, aes(x = Log_Reciprocal_Serum_Dilution, y = p, color = "044-101")) +
  geom_point(data = pre_044_102, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "044-102")) + geom_line(data = newdata_pre_044_102, aes(x = Log_Reciprocal_Serum_Dilution, y = p, color = "044-102")) +
  geom_point(data = pre_044_103, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "044-103")) + geom_line(data = newdata_pre_044_103, aes(x = Log_Reciprocal_Serum_Dilution, y = p, color = "044-103")) +
  geom_point(data = pre_044_109, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "044-109")) + geom_line(data = newdata_pre_044_109, aes(x = Log_Reciprocal_Serum_Dilution, y = p, color = "044-109")) +
  geom_point(data = pre_044_110, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "044-110")) + geom_line(data = newdata_pre_044_110, aes(x = Log_Reciprocal_Serum_Dilution, y = p, color = "044-110")) +
  geom_point(data = pre_044_112, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "044-112")) + geom_line(data = newdata_pre_044_112, aes(x = Log_Reciprocal_Serum_Dilution, y = p, color = "044-112")) +
  geom_point(data = pre_044_114, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "044-114")) + geom_line(data = newdata_pre_044_114, aes(x = Log_Reciprocal_Serum_Dilution, y = p, color = "044-114")) +
  geom_point(data = pre_044_116, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "044-116")) + geom_line(data = newdata_pre_044_116, aes(x = Log_Reciprocal_Serum_Dilution, y = p, color = "044-116")) +
  geom_point(data = pre_044_117, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "044-117")) + geom_line(data = newdata_pre_044_117, aes(x = Log_Reciprocal_Serum_Dilution, y = p, color = "044-117")) +
  geom_point(data = pre_044_118, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "044-118")) + geom_line(data = newdata_pre_044_118, aes(x = Log_Reciprocal_Serum_Dilution, y = p, color = "044-118")) +
  geom_point(data = pre_044_122, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "044-122")) + geom_line(data = newdata_pre_044_122, aes(x = Log_Reciprocal_Serum_Dilution, y = p, color = "044-122")) +
  geom_point(data = pre_044_126, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "044-126")) + geom_line(data = newdata_pre_044_126, aes(x = Log_Reciprocal_Serum_Dilution, y = p, color = "044-126")) +
  geom_point(data = pre_044_127, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "044-127")) + geom_line(data = newdata_pre_044_127, aes(x = Log_Reciprocal_Serum_Dilution, y = p, color = "044-127")) +
  geom_point(data = pre_044_130, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "044-130")) + geom_line(data = newdata_pre_044_130, aes(x = Log_Reciprocal_Serum_Dilution, y = p, color = "044-130")) +
  geom_point(data = pre_044_131, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "044-131")) + geom_line(data = newdata_pre_044_131, aes(x = Log_Reciprocal_Serum_Dilution, y = p, color = "044-131")) +
  geom_point(data = pre_044_132, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "044-132")) + geom_line(data = newdata_pre_044_132, aes(x = Log_Reciprocal_Serum_Dilution, y = p, color = "044-132")) +
  geom_point(data = pre_044_133, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "044-133")) + geom_line(data = newdata_pre_044_133, aes(x = Log_Reciprocal_Serum_Dilution, y = p, color = "044-133")) +
  theme_classic() + 
  labs(x = "Log(reciprocal serum dilution)", y = "OD450", title= 'Pre-Infection') + 
  xlim(1,6) + 
  scale_y_continuous(expand = c(0,0), limits = c(0,3.5), breaks = c(0, 0.5, 1, 1.5, 2, 2.5, 3, 3.5)) + 
  scale_color_manual(values = c("#7b0000", "#ad0000", "#ff0000", "#ff5d5d", "#ffa3a3", "#ffd2d2", "#000088", "#3434fb", "#6c6cff", "#9c9cff", "#d0d0ff", "#0ef000", "#c1c000", "#fffe04", "#ffd991", "#c7ad7c", "#6ca7a2", "#9c7a99"), breaks = c("044-101", "044-102", "044-103", "044-104", "044-109", "044-110", "044-112", "044-114", "044-116", "044-117", "044-118", "044-122", "044-126", "044-127", "044-130", "044-131", "044-132", "044-133")) + 
  guides(color = guide_legend(title = "Dam ID")) + theme(axis.text = element_text(size = 20), axis.title = element_text(face = 'bold', size = 20), plot.title = element_text(face = "bold", hjust = 0.5, size = 24), aspect.ratio = 1, legend.text = element_text(size= 20), legend.title = element_text(size= 20, face= 'bold'), legend.position = 'right')
rawcurves_preinfection

###
# Combined IgG WVE Raw Binding Curves (2-4 DPI)
###
# 044-101
DPI_2.4_044_101 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\Combined Raw Curves (Final)\\2-4 DPI IgG Raw Curves (Final).xlsx", sheet = 1, col_names = TRUE)
DPI_2.4_044_101$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_2.4_044_101$Log_Reciprocal_Serum_Dilution)
str(DPI_2.4_044_101)
model_DPI_2.4_044_101 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_2.4_044_101, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_2.4_044_101, type = "all")
summary(model_DPI_2.4_044_101)
ED(model_DPI_2.4_044_101, c(10,50), interval = "delta")
newdata_DPI_2.4_044_101 <- expand.grid(Log_Reciprocal_Serum_Dilution = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_2.4_044_101 <- predict(model_DPI_2.4_044_101, newdata = newdata_DPI_2.4_044_101, interval = "confidence")
newdata_DPI_2.4_044_101$p <- pm_DPI_2.4_044_101[,1]
newdata_DPI_2.4_044_101$pmin <- pm_DPI_2.4_044_101[,2]
newdata_DPI_2.4_044_101$pmax <- pm_DPI_2.4_044_101[,3]
# 044-102
DPI_2.4_044_102 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\Combined Raw Curves (Final)\\2-4 DPI IgG Raw Curves (Final).xlsx", sheet = 2, col_names = TRUE)
DPI_2.4_044_102$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_2.4_044_102$Log_Reciprocal_Serum_Dilution)
str(DPI_2.4_044_102)
model_DPI_2.4_044_102 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_2.4_044_102, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_2.4_044_102, type = "all")
summary(model_DPI_2.4_044_102)
ED(model_DPI_2.4_044_102, c(10,50), interval = "delta")
newdata_DPI_2.4_044_102 <- expand.grid(Log_Reciprocal_Serum_Dilution = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_2.4_044_102 <- predict(model_DPI_2.4_044_102, newdata = newdata_DPI_2.4_044_102, interval = "confidence")
newdata_DPI_2.4_044_102$p <- pm_DPI_2.4_044_102[,1]
newdata_DPI_2.4_044_102$pmin <- pm_DPI_2.4_044_102[,2]
newdata_DPI_2.4_044_102$pmax <- pm_DPI_2.4_044_102[,3]
# 044-103
DPI_2.4_044_103 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\Combined Raw Curves (Final)\\2-4 DPI IgG Raw Curves (Final).xlsx", sheet = 3, col_names = TRUE)
DPI_2.4_044_103$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_2.4_044_103$Log_Reciprocal_Serum_Dilution)
str(DPI_2.4_044_103)
model_DPI_2.4_044_103 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_2.4_044_103, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_2.4_044_103, type = "all")
summary(model_DPI_2.4_044_103)
ED(model_DPI_2.4_044_103, c(10,50), interval = "delta")
newdata_DPI_2.4_044_103 <- expand.grid(Log_Reciprocal_Serum_Dilution = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_2.4_044_103 <- predict(model_DPI_2.4_044_103, newdata = newdata_DPI_2.4_044_103, interval = "confidence")
newdata_DPI_2.4_044_103$p <- pm_DPI_2.4_044_103[,1]
newdata_DPI_2.4_044_103$pmin <- pm_DPI_2.4_044_103[,2]
newdata_DPI_2.4_044_103$pmax <- pm_DPI_2.4_044_103[,3]
# 044-104 (No 2-4 DPI sample run)
# 044-109
DPI_2.4_044_109 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\Combined Raw Curves (Final)\\2-4 DPI IgG Raw Curves (Final).xlsx", sheet = 4, col_names = TRUE)
DPI_2.4_044_109$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_2.4_044_109$Log_Reciprocal_Serum_Dilution)
str(DPI_2.4_044_109)
model_DPI_2.4_044_109 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_2.4_044_109, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_2.4_044_109, type = "all")
summary(model_DPI_2.4_044_109)
ED(model_DPI_2.4_044_109, c(10,50), interval = "delta")
newdata_DPI_2.4_044_109 <- expand.grid(Log_Reciprocal_Serum_Dilution = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_2.4_044_109 <- predict(model_DPI_2.4_044_109, newdata = newdata_DPI_2.4_044_109, interval = "confidence")
newdata_DPI_2.4_044_109$p <- pm_DPI_2.4_044_109[,1]
newdata_DPI_2.4_044_109$pmin <- pm_DPI_2.4_044_109[,2]
newdata_DPI_2.4_044_109$pmax <- pm_DPI_2.4_044_109[,3]
# 044-110
DPI_2.4_044_110 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\Combined Raw Curves (Final)\\2-4 DPI IgG Raw Curves (Final).xlsx", sheet = 5, col_names = TRUE)
DPI_2.4_044_110$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_2.4_044_110$Log_Reciprocal_Serum_Dilution)
str(DPI_2.4_044_110)
model_DPI_2.4_044_110 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_2.4_044_110, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_2.4_044_110, type = "all")
summary(model_DPI_2.4_044_110)
ED(model_DPI_2.4_044_110, c(10,50), interval = "delta")
newdata_DPI_2.4_044_110 <- expand.grid(Log_Reciprocal_Serum_Dilution = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_2.4_044_110 <- predict(model_DPI_2.4_044_110, newdata = newdata_DPI_2.4_044_110, interval = "confidence")
newdata_DPI_2.4_044_110$p <- pm_DPI_2.4_044_110[,1]
newdata_DPI_2.4_044_110$pmin <- pm_DPI_2.4_044_110[,2]
newdata_DPI_2.4_044_110$pmax <- pm_DPI_2.4_044_110[,3]
# 044-112
DPI_2.4_044_112 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\Combined Raw Curves (Final)\\2-4 DPI IgG Raw Curves (Final).xlsx", sheet = 6, col_names = TRUE)
DPI_2.4_044_112$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_2.4_044_112$Log_Reciprocal_Serum_Dilution)
str(DPI_2.4_044_112)
model_DPI_2.4_044_112 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_2.4_044_112, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_2.4_044_112, type = "all")
summary(model_DPI_2.4_044_112)
ED(model_DPI_2.4_044_112, c(10,50), interval = "delta")
newdata_DPI_2.4_044_112 <- expand.grid(Log_Reciprocal_Serum_Dilution = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_2.4_044_112 <- predict(model_DPI_2.4_044_112, newdata = newdata_DPI_2.4_044_112, interval = "confidence")
newdata_DPI_2.4_044_112$p <- pm_DPI_2.4_044_112[,1]
newdata_DPI_2.4_044_112$pmin <- pm_DPI_2.4_044_112[,2]
newdata_DPI_2.4_044_112$pmax <- pm_DPI_2.4_044_112[,3]
#044-114
DPI_2.4_044_114 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\Combined Raw Curves (Final)\\2-4 DPI IgG Raw Curves (Final).xlsx", sheet = 7, col_names = TRUE)
DPI_2.4_044_114$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_2.4_044_114$Log_Reciprocal_Serum_Dilution)
str(DPI_2.4_044_114)
model_DPI_2.4_044_114 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_2.4_044_114, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_2.4_044_114, type = "all")
summary(model_DPI_2.4_044_114)
ED(model_DPI_2.4_044_114, c(10,50), interval = "delta")
newdata_DPI_2.4_044_114 <- expand.grid(Log_Reciprocal_Serum_Dilution = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_2.4_044_114 <- predict(model_DPI_2.4_044_114, newdata = newdata_DPI_2.4_044_114, interval = "confidence")
newdata_DPI_2.4_044_114$p <- pm_DPI_2.4_044_114[,1]
newdata_DPI_2.4_044_114$pmin <- pm_DPI_2.4_044_114[,2]
newdata_DPI_2.4_044_114$pmax <- pm_DPI_2.4_044_114[,3]
# 044-116
DPI_2.4_044_116 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\Combined Raw Curves (Final)\\2-4 DPI IgG Raw Curves (Final).xlsx", sheet = 8, col_names = TRUE)
DPI_2.4_044_116$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_2.4_044_116$Log_Reciprocal_Serum_Dilution)
str(DPI_2.4_044_116)
model_DPI_2.4_044_116 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_2.4_044_116, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_2.4_044_116, type = "all")
summary(model_DPI_2.4_044_116)
ED(model_DPI_2.4_044_116, c(10,50), interval = "delta")
newdata_DPI_2.4_044_116 <- expand.grid(Log_Reciprocal_Serum_Dilution = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_2.4_044_116 <- predict(model_DPI_2.4_044_116, newdata = newdata_DPI_2.4_044_116, interval = "confidence")
newdata_DPI_2.4_044_116$p <- pm_DPI_2.4_044_116[,1]
newdata_DPI_2.4_044_116$pmin <- pm_DPI_2.4_044_116[,2]
newdata_DPI_2.4_044_116$pmax <- pm_DPI_2.4_044_116[,3]
# 044-117
DPI_2.4_044_117 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\Combined Raw Curves (Final)\\2-4 DPI IgG Raw Curves (Final).xlsx", sheet = 9, col_names = TRUE)
DPI_2.4_044_117$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_2.4_044_117$Log_Reciprocal_Serum_Dilution)
str(DPI_2.4_044_117)
model_DPI_2.4_044_117 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_2.4_044_117, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_2.4_044_117, type = "all")
summary(model_DPI_2.4_044_117)
ED(model_DPI_2.4_044_117, c(10,50), interval = "delta")
newdata_DPI_2.4_044_117 <- expand.grid(Log_Reciprocal_Serum_Dilution = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_2.4_044_117 <- predict(model_DPI_2.4_044_117, newdata = newdata_DPI_2.4_044_117, interval = "confidence")
newdata_DPI_2.4_044_117$p <- pm_DPI_2.4_044_117[,1]
newdata_DPI_2.4_044_117$pmin <- pm_DPI_2.4_044_117[,2]
newdata_DPI_2.4_044_117$pmax <- pm_DPI_2.4_044_117[,3]
# 044-118
DPI_2.4_044_118 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\Combined Raw Curves (Final)\\2-4 DPI IgG Raw Curves (Final).xlsx", sheet = 10, col_names = TRUE)
DPI_2.4_044_118$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_2.4_044_118$Log_Reciprocal_Serum_Dilution)
str(DPI_2.4_044_118)
model_DPI_2.4_044_118 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_2.4_044_118, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_2.4_044_118, type = "all")
summary(model_DPI_2.4_044_118)
ED(model_DPI_2.4_044_118, c(10,50), interval = "delta")
newdata_DPI_2.4_044_118 <- expand.grid(Log_Reciprocal_Serum_Dilution = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_2.4_044_118 <- predict(model_DPI_2.4_044_118, newdata = newdata_DPI_2.4_044_118, interval = "confidence")
newdata_DPI_2.4_044_118$p <- pm_DPI_2.4_044_118[,1]
newdata_DPI_2.4_044_118$pmin <- pm_DPI_2.4_044_118[,2]
newdata_DPI_2.4_044_118$pmax <- pm_DPI_2.4_044_118[,3]
# 044-122
DPI_2.4_044_122 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\Combined Raw Curves (Final)\\2-4 DPI IgG Raw Curves (Final).xlsx", sheet = 11, col_names = TRUE)
DPI_2.4_044_122$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_2.4_044_122$Log_Reciprocal_Serum_Dilution)
str(DPI_2.4_044_122)
model_DPI_2.4_044_122 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_2.4_044_122, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_2.4_044_122, type = "all")
summary(model_DPI_2.4_044_122)
ED(model_DPI_2.4_044_122, c(10,50), interval = "delta")
newdata_DPI_2.4_044_122 <- expand.grid(Log_Reciprocal_Serum_Dilution = exp(seq(log(1.7), log(5.31), length = 100)))
pm_DPI_2.4_044_122 <- predict(model_DPI_2.4_044_122, newdata = newdata_DPI_2.4_044_122, interval = "confidence")
newdata_DPI_2.4_044_122$p <- pm_DPI_2.4_044_122[,1]
newdata_DPI_2.4_044_122$pmin <- pm_DPI_2.4_044_122[,2]
newdata_DPI_2.4_044_122$pmax <- pm_DPI_2.4_044_122[,3]
# 044-126
DPI_2.4_044_126 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\Combined Raw Curves (Final)\\2-4 DPI IgG Raw Curves (Final).xlsx", sheet = 12, col_names = TRUE)
DPI_2.4_044_126$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_2.4_044_126$Log_Reciprocal_Serum_Dilution)
str(DPI_2.4_044_126)
model_DPI_2.4_044_126 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_2.4_044_126, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_2.4_044_126, type = "all")
summary(model_DPI_2.4_044_126)
ED(model_DPI_2.4_044_126, c(10,50), interval = "delta")
newdata_DPI_2.4_044_126 <- expand.grid(Log_Reciprocal_Serum_Dilution = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_2.4_044_126 <- predict(model_DPI_2.4_044_126, newdata = newdata_DPI_2.4_044_126, interval = "confidence")
newdata_DPI_2.4_044_126$p <- pm_DPI_2.4_044_126[,1]
newdata_DPI_2.4_044_126$pmin <- pm_DPI_2.4_044_126[,2]
newdata_DPI_2.4_044_126$pmax <- pm_DPI_2.4_044_126[,3]
# 044-127
DPI_2.4_044_127 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\Combined Raw Curves (Final)\\2-4 DPI IgG Raw Curves (Final).xlsx", sheet = 13, col_names = TRUE)
DPI_2.4_044_127$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_2.4_044_127$Log_Reciprocal_Serum_Dilution)
str(DPI_2.4_044_127)
model_DPI_2.4_044_127 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_2.4_044_127, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_2.4_044_127, type = "all")
summary(model_DPI_2.4_044_127)
ED(model_DPI_2.4_044_127, c(10,50), interval = "delta")
newdata_DPI_2.4_044_127 <- expand.grid(Log_Reciprocal_Serum_Dilution = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_2.4_044_127 <- predict(model_DPI_2.4_044_127, newdata = newdata_DPI_2.4_044_127, interval = "confidence")
newdata_DPI_2.4_044_127$p <- pm_DPI_2.4_044_127[,1]
newdata_DPI_2.4_044_127$pmin <- pm_DPI_2.4_044_127[,2]
newdata_DPI_2.4_044_127$pmax <- pm_DPI_2.4_044_127[,3]
# 044-130
DPI_2.4_044_130 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\Combined Raw Curves (Final)\\2-4 DPI IgG Raw Curves (Final).xlsx", sheet = 14, col_names = TRUE)
DPI_2.4_044_130$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_2.4_044_130$Log_Reciprocal_Serum_Dilution)
str(DPI_2.4_044_130)
model_DPI_2.4_044_130 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_2.4_044_130, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_2.4_044_130, type = "all")
summary(model_DPI_2.4_044_130)
ED(model_DPI_2.4_044_130, c(10,50), interval = "delta")
newdata_DPI_2.4_044_130 <- expand.grid(Log_Reciprocal_Serum_Dilution = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_2.4_044_130 <- predict(model_DPI_2.4_044_130, newdata = newdata_DPI_2.4_044_130, interval = "confidence")
newdata_DPI_2.4_044_130$p <- pm_DPI_2.4_044_130[,1]
newdata_DPI_2.4_044_130$pmin <- pm_DPI_2.4_044_130[,2]
newdata_DPI_2.4_044_130$pmax <- pm_DPI_2.4_044_130[,3]
# 044-131
DPI_2.4_044_131 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\Combined Raw Curves (Final)\\2-4 DPI IgG Raw Curves (Final).xlsx", sheet = 15, col_names = TRUE)
DPI_2.4_044_131$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_2.4_044_131$Log_Reciprocal_Serum_Dilution)
str(DPI_2.4_044_131)
model_DPI_2.4_044_131 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_2.4_044_131, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_2.4_044_131, type = "all")
summary(model_DPI_2.4_044_131)
ED(model_DPI_2.4_044_131, c(10,50), interval = "delta")
newdata_DPI_2.4_044_131 <- expand.grid(Log_Reciprocal_Serum_Dilution = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_2.4_044_131 <- predict(model_DPI_2.4_044_131, newdata = newdata_DPI_2.4_044_131, interval = "confidence")
newdata_DPI_2.4_044_131$p <- pm_DPI_2.4_044_131[,1]
newdata_DPI_2.4_044_131$pmin <- pm_DPI_2.4_044_131[,2]
newdata_DPI_2.4_044_131$pmax <- pm_DPI_2.4_044_131[,3]
# 044-132
DPI_2.4_044_132 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\Combined Raw Curves (Final)\\2-4 DPI IgG Raw Curves (Final).xlsx", sheet = 16, col_names = TRUE)
DPI_2.4_044_132$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_2.4_044_132$Log_Reciprocal_Serum_Dilution)
str(DPI_2.4_044_132)
model_DPI_2.4_044_132 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_2.4_044_132, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_2.4_044_132, type = "all")
summary(model_DPI_2.4_044_132)
ED(model_DPI_2.4_044_132, c(10,50), interval = "delta")
newdata_DPI_2.4_044_132 <- expand.grid(Log_Reciprocal_Serum_Dilution = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_2.4_044_132 <- predict(model_DPI_2.4_044_132, newdata = newdata_DPI_2.4_044_132, interval = "confidence")
newdata_DPI_2.4_044_132$p <- pm_DPI_2.4_044_132[,1]
newdata_DPI_2.4_044_132$pmin <- pm_DPI_2.4_044_132[,2]
newdata_DPI_2.4_044_132$pmax <- pm_DPI_2.4_044_132[,3]
# 044-133
DPI_2.4_044_133 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\Combined Raw Curves (Final)\\2-4 DPI IgG Raw Curves (Final).xlsx", sheet = 17, col_names = TRUE)
DPI_2.4_044_133$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_2.4_044_133$Log_Reciprocal_Serum_Dilution)
str(DPI_2.4_044_133)
model_DPI_2.4_044_133 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_2.4_044_133, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_2.4_044_133, type = "all")
summary(model_DPI_2.4_044_133)
ED(model_DPI_2.4_044_133, c(10,50), interval = "delta")
newdata_DPI_2.4_044_133 <- expand.grid(Log_Reciprocal_Serum_Dilution = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_2.4_044_133 <- predict(model_DPI_2.4_044_133, newdata = newdata_DPI_2.4_044_133, interval = "confidence")
newdata_DPI_2.4_044_133$p <- pm_DPI_2.4_044_133[,1]
newdata_DPI_2.4_044_133$pmin <- pm_DPI_2.4_044_133[,2]
newdata_DPI_2.4_044_133$pmax <- pm_DPI_2.4_044_133[,3]
# Combined IgG WVE Raw Binding Curves figure (2-4 DPI)
rawcurves_DPI_2.4 <- 
  ggplot() + 
  geom_point(data = DPI_2.4_044_101, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "044-101")) + geom_line(data = newdata_DPI_2.4_044_101, aes(x = Log_Reciprocal_Serum_Dilution, y = p, color = "044-101")) +
  geom_point(data = DPI_2.4_044_102, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "044-102")) + geom_line(data = newdata_DPI_2.4_044_102, aes(x = Log_Reciprocal_Serum_Dilution, y = p, color = "044-102")) +
  geom_point(data = DPI_2.4_044_103, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "044-103")) + geom_line(data = newdata_DPI_2.4_044_103, aes(x = Log_Reciprocal_Serum_Dilution, y = p, color = "044-103")) +
  geom_point(data = DPI_2.4_044_109, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "044-109")) + geom_line(data = newdata_DPI_2.4_044_109, aes(x = Log_Reciprocal_Serum_Dilution, y = p, color = "044-109")) +
  geom_point(data = DPI_2.4_044_110, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "044-110")) + geom_line(data = newdata_DPI_2.4_044_110, aes(x = Log_Reciprocal_Serum_Dilution, y = p, color = "044-110")) +
  geom_point(data = DPI_2.4_044_112, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "044-112")) + geom_line(data = newdata_DPI_2.4_044_112, aes(x = Log_Reciprocal_Serum_Dilution, y = p, color = "044-112")) +
  geom_point(data = DPI_2.4_044_114, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "044-114")) + geom_line(data = newdata_DPI_2.4_044_114, aes(x = Log_Reciprocal_Serum_Dilution, y = p, color = "044-114")) +
  geom_point(data = DPI_2.4_044_116, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "044-116")) + geom_line(data = newdata_DPI_2.4_044_116, aes(x = Log_Reciprocal_Serum_Dilution, y = p, color = "044-116")) +
  geom_point(data = DPI_2.4_044_117, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "044-117")) + geom_line(data = newdata_DPI_2.4_044_117, aes(x = Log_Reciprocal_Serum_Dilution, y = p, color = "044-117")) +
  geom_point(data = DPI_2.4_044_118, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "044-118")) + geom_line(data = newdata_DPI_2.4_044_118, aes(x = Log_Reciprocal_Serum_Dilution, y = p, color = "044-118")) +
  geom_point(data = DPI_2.4_044_122, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "044-122")) + geom_line(data = newdata_DPI_2.4_044_122, aes(x = Log_Reciprocal_Serum_Dilution, y = p, color = "044-122")) +
  geom_point(data = DPI_2.4_044_126, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "044-126")) + geom_line(data = newdata_DPI_2.4_044_126, aes(x = Log_Reciprocal_Serum_Dilution, y = p, color = "044-126")) +
  geom_point(data = DPI_2.4_044_127, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "044-127")) + geom_line(data = newdata_DPI_2.4_044_127, aes(x = Log_Reciprocal_Serum_Dilution, y = p, color = "044-127")) +
  geom_point(data = DPI_2.4_044_130, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "044-130")) + geom_line(data = newdata_DPI_2.4_044_130, aes(x = Log_Reciprocal_Serum_Dilution, y = p, color = "044-130")) +
  geom_point(data = DPI_2.4_044_131, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "044-131")) + geom_line(data = newdata_DPI_2.4_044_131, aes(x = Log_Reciprocal_Serum_Dilution, y = p, color = "044-131")) +
  geom_point(data = DPI_2.4_044_132, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "044-132")) + geom_line(data = newdata_DPI_2.4_044_132, aes(x = Log_Reciprocal_Serum_Dilution, y = p, color = "044-132")) +
  geom_point(data = DPI_2.4_044_133, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "044-133")) + geom_line(data = newdata_DPI_2.4_044_133, aes(x = Log_Reciprocal_Serum_Dilution, y = p, color = "044-133")) +
  theme_classic() + 
  labs(x = "Log(reciprocal serum dilution)", y = "OD450", title= '2-4 DPI') + 
  xlim(1,6) + 
  scale_y_continuous(expand = c(0,0), limits = c(0,3.5), breaks = c(0, 0.5, 1, 1.5, 2, 2.5, 3, 3.5)) + 
  scale_color_manual(values = c("#7b0000", "#ad0000", "#ff0000", "#ff5d5d", "#ffa3a3", "#ffd2d2", "#000088", "#3434fb", "#6c6cff", "#9c9cff", "#d0d0ff", "#0ef000", "#c1c000", "#fffe04", "#ffd991", "#c7ad7c", "#6ca7a2", "#9c7a99"), breaks = c("044-101", "044-102", "044-103", "044-104", "044-109", "044-110", "044-112", "044-114", "044-116", "044-117", "044-118", "044-122", "044-126", "044-127", "044-130", "044-131", "044-132", "044-133")) + 
  guides(color = guide_legend(title = "Dam ID")) + theme(axis.text = element_text(size = 20), axis.title = element_text(face = 'bold', size = 20), plot.title = element_text(face = "bold", hjust = 0.5, size = 24), aspect.ratio = 1, legend.text = element_text(size= 20), legend.title = element_text(size= 20, face= 'bold'), legend.position = 'right')
rawcurves_DPI_2.4

###
# Combined IgG WVE Raw Binding Curves (7-10 DPI)
###
# 044-101
DPI_7.10_044_101 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\Combined Raw Curves (Final)\\7-10 DPI IgG Raw Curves (Final).xlsx", sheet = 1, col_names = TRUE)
DPI_7.10_044_101$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_7.10_044_101$Log_Reciprocal_Serum_Dilution)
str(DPI_7.10_044_101)
model_DPI_7.10_044_101 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_7.10_044_101, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_7.10_044_101, type = "all")
summary(model_DPI_7.10_044_101)
ED(model_DPI_7.10_044_101, c(10,50), interval = "delta")
newdata_DPI_7.10_044_101 <- expand.grid(Log_Reciprocal_Serum_Dilution = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_7.10_044_101 <- predict(model_DPI_7.10_044_101, newdata = newdata_DPI_7.10_044_101, interval = "confidence")
newdata_DPI_7.10_044_101$p <- pm_DPI_7.10_044_101[,1]
newdata_DPI_7.10_044_101$pmin <- pm_DPI_7.10_044_101[,2]
newdata_DPI_7.10_044_101$pmax <- pm_DPI_7.10_044_101[,3]
# 044-102
DPI_7.10_044_102 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\Combined Raw Curves (Final)\\7-10 DPI IgG Raw Curves (Final).xlsx", sheet = 2, col_names = TRUE)
DPI_7.10_044_102$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_7.10_044_102$Log_Reciprocal_Serum_Dilution)
str(DPI_7.10_044_102)
model_DPI_7.10_044_102 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_7.10_044_102, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_7.10_044_102, type = "all")
summary(model_DPI_7.10_044_102)
ED(model_DPI_7.10_044_102, c(10,50), interval = "delta")
newdata_DPI_7.10_044_102 <- expand.grid(Log_Reciprocal_Serum_Dilution = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_7.10_044_102 <- predict(model_DPI_7.10_044_102, newdata = newdata_DPI_7.10_044_102, interval = "confidence")
newdata_DPI_7.10_044_102$p <- pm_DPI_7.10_044_102[,1]
newdata_DPI_7.10_044_102$pmin <- pm_DPI_7.10_044_102[,2]
newdata_DPI_7.10_044_102$pmax <- pm_DPI_7.10_044_102[,3]
# 044-103
DPI_7.10_044_103 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\Combined Raw Curves (Final)\\7-10 DPI IgG Raw Curves (Final).xlsx", sheet = 3, col_names = TRUE)
DPI_7.10_044_103$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_7.10_044_103$Log_Reciprocal_Serum_Dilution)
str(DPI_7.10_044_103)
model_DPI_7.10_044_103 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_7.10_044_103, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_7.10_044_103, type = "all")
summary(model_DPI_7.10_044_103)
ED(model_DPI_7.10_044_103, c(10,50), interval = "delta")
newdata_DPI_7.10_044_103 <- expand.grid(Log_Reciprocal_Serum_Dilution = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_7.10_044_103 <- predict(model_DPI_7.10_044_103, newdata = newdata_DPI_7.10_044_103, interval = "confidence")
newdata_DPI_7.10_044_103$p <- pm_DPI_7.10_044_103[,1]
newdata_DPI_7.10_044_103$pmin <- pm_DPI_7.10_044_103[,2]
newdata_DPI_7.10_044_103$pmax <- pm_DPI_7.10_044_103[,3]
# 044-104
DPI_7.10_044_104 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\Combined Raw Curves (Final)\\7-10 DPI IgG Raw Curves (Final).xlsx", sheet = 4, col_names = TRUE)
DPI_7.10_044_104$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_7.10_044_104$Log_Reciprocal_Serum_Dilution)
str(DPI_7.10_044_104)
model_DPI_7.10_044_104 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_7.10_044_104, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_7.10_044_104, type = "all")
summary(model_DPI_7.10_044_104)
ED(model_DPI_7.10_044_104, c(10,50), interval = "delta")
newdata_DPI_7.10_044_104 <- expand.grid(Log_Reciprocal_Serum_Dilution = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_7.10_044_104 <- predict(model_DPI_7.10_044_104, newdata = newdata_DPI_7.10_044_104, interval = "confidence")
newdata_DPI_7.10_044_104$p <- pm_DPI_7.10_044_104[,1]
newdata_DPI_7.10_044_104$pmin <- pm_DPI_7.10_044_104[,2]
newdata_DPI_7.10_044_104$pmax <- pm_DPI_7.10_044_104[,3]
# 044-109
DPI_7.10_044_109 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\Combined Raw Curves (Final)\\7-10 DPI IgG Raw Curves (Final).xlsx", sheet = 5, col_names = TRUE)
DPI_7.10_044_109$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_7.10_044_109$Log_Reciprocal_Serum_Dilution)
str(DPI_7.10_044_109)
model_DPI_7.10_044_109 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_7.10_044_109, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_7.10_044_109, type = "all")
summary(model_DPI_7.10_044_109)
ED(model_DPI_7.10_044_109, c(10,50), interval = "delta")
newdata_DPI_7.10_044_109 <- expand.grid(Log_Reciprocal_Serum_Dilution = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_7.10_044_109 <- predict(model_DPI_7.10_044_109, newdata = newdata_DPI_7.10_044_109, interval = "confidence")
newdata_DPI_7.10_044_109$p <- pm_DPI_7.10_044_109[,1]
newdata_DPI_7.10_044_109$pmin <- pm_DPI_7.10_044_109[,2]
newdata_DPI_7.10_044_109$pmax <- pm_DPI_7.10_044_109[,3]
# 044-110
DPI_7.10_044_110 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\Combined Raw Curves (Final)\\7-10 DPI IgG Raw Curves (Final).xlsx", sheet = 6, col_names = TRUE)
DPI_7.10_044_110$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_7.10_044_110$Log_Reciprocal_Serum_Dilution)
str(DPI_7.10_044_110)
model_DPI_7.10_044_110 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_7.10_044_110, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_7.10_044_110, type = "all")
summary(model_DPI_7.10_044_110)
ED(model_DPI_7.10_044_110, c(10,50), interval = "delta")
newdata_DPI_7.10_044_110 <- expand.grid(Log_Reciprocal_Serum_Dilution = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_7.10_044_110 <- predict(model_DPI_7.10_044_110, newdata = newdata_DPI_7.10_044_110, interval = "confidence")
newdata_DPI_7.10_044_110$p <- pm_DPI_7.10_044_110[,1]
newdata_DPI_7.10_044_110$pmin <- pm_DPI_7.10_044_110[,2]
newdata_DPI_7.10_044_110$pmax <- pm_DPI_7.10_044_110[,3]
# 044-112
DPI_7.10_044_112 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\Combined Raw Curves (Final)\\7-10 DPI IgG Raw Curves (Final).xlsx", sheet = 7, col_names = TRUE)
DPI_7.10_044_112$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_7.10_044_112$Log_Reciprocal_Serum_Dilution)
str(DPI_7.10_044_112)
model_DPI_7.10_044_112 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_7.10_044_112, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_7.10_044_112, type = "all")
summary(model_DPI_7.10_044_112)
ED(model_DPI_7.10_044_112, c(10,50), interval = "delta")
newdata_DPI_7.10_044_112 <- expand.grid(Log_Reciprocal_Serum_Dilution = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_7.10_044_112 <- predict(model_DPI_7.10_044_112, newdata = newdata_DPI_7.10_044_112, interval = "confidence")
newdata_DPI_7.10_044_112$p <- pm_DPI_7.10_044_112[,1]
newdata_DPI_7.10_044_112$pmin <- pm_DPI_7.10_044_112[,2]
newdata_DPI_7.10_044_112$pmax <- pm_DPI_7.10_044_112[,3]
#044-114
DPI_7.10_044_114 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\Combined Raw Curves (Final)\\7-10 DPI IgG Raw Curves (Final).xlsx", sheet = 8, col_names = TRUE)
DPI_7.10_044_114$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_7.10_044_114$Log_Reciprocal_Serum_Dilution)
str(DPI_7.10_044_114)
model_DPI_7.10_044_114 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_7.10_044_114, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_7.10_044_114, type = "all")
summary(model_DPI_7.10_044_114)
ED(model_DPI_7.10_044_114, c(10,50), interval = "delta")
newdata_DPI_7.10_044_114 <- expand.grid(Log_Reciprocal_Serum_Dilution = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_7.10_044_114 <- predict(model_DPI_7.10_044_114, newdata = newdata_DPI_7.10_044_114, interval = "confidence")
newdata_DPI_7.10_044_114$p <- pm_DPI_7.10_044_114[,1]
newdata_DPI_7.10_044_114$pmin <- pm_DPI_7.10_044_114[,2]
newdata_DPI_7.10_044_114$pmax <- pm_DPI_7.10_044_114[,3]
# 044-116
DPI_7.10_044_116 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\Combined Raw Curves (Final)\\7-10 DPI IgG Raw Curves (Final).xlsx", sheet = 9, col_names = TRUE)
DPI_7.10_044_116$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_7.10_044_116$Log_Reciprocal_Serum_Dilution)
str(DPI_7.10_044_116)
model_DPI_7.10_044_116 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_7.10_044_116, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_7.10_044_116, type = "all")
summary(model_DPI_7.10_044_116)
ED(model_DPI_7.10_044_116, c(10,50), interval = "delta")
newdata_DPI_7.10_044_116 <- expand.grid(Log_Reciprocal_Serum_Dilution = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_7.10_044_116 <- predict(model_DPI_7.10_044_116, newdata = newdata_DPI_7.10_044_116, interval = "confidence")
newdata_DPI_7.10_044_116$p <- pm_DPI_7.10_044_116[,1]
newdata_DPI_7.10_044_116$pmin <- pm_DPI_7.10_044_116[,2]
newdata_DPI_7.10_044_116$pmax <- pm_DPI_7.10_044_116[,3]
# 044-117
DPI_7.10_044_117 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\Combined Raw Curves (Final)\\7-10 DPI IgG Raw Curves (Final).xlsx", sheet = 10, col_names = TRUE)
DPI_7.10_044_117$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_7.10_044_117$Log_Reciprocal_Serum_Dilution)
str(DPI_7.10_044_117)
model_DPI_7.10_044_117 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_7.10_044_117, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_7.10_044_117, type = "all")
summary(model_DPI_7.10_044_117)
ED(model_DPI_7.10_044_117, c(10,50), interval = "delta")
newdata_DPI_7.10_044_117 <- expand.grid(Log_Reciprocal_Serum_Dilution = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_7.10_044_117 <- predict(model_DPI_7.10_044_117, newdata = newdata_DPI_7.10_044_117, interval = "confidence")
newdata_DPI_7.10_044_117$p <- pm_DPI_7.10_044_117[,1]
newdata_DPI_7.10_044_117$pmin <- pm_DPI_7.10_044_117[,2]
newdata_DPI_7.10_044_117$pmax <- pm_DPI_7.10_044_117[,3]
# 044-118
DPI_7.10_044_118 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\Combined Raw Curves (Final)\\7-10 DPI IgG Raw Curves (Final).xlsx", sheet = 11, col_names = TRUE)
DPI_7.10_044_118$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_7.10_044_118$Log_Reciprocal_Serum_Dilution)
str(DPI_7.10_044_118)
model_DPI_7.10_044_118 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_7.10_044_118, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_7.10_044_118, type = "all")
summary(model_DPI_7.10_044_118)
ED(model_DPI_7.10_044_118, c(10,50), interval = "delta")
newdata_DPI_7.10_044_118 <- expand.grid(Log_Reciprocal_Serum_Dilution = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_7.10_044_118 <- predict(model_DPI_7.10_044_118, newdata = newdata_DPI_7.10_044_118, interval = "confidence")
newdata_DPI_7.10_044_118$p <- pm_DPI_7.10_044_118[,1]
newdata_DPI_7.10_044_118$pmin <- pm_DPI_7.10_044_118[,2]
newdata_DPI_7.10_044_118$pmax <- pm_DPI_7.10_044_118[,3]
# 044-122
DPI_7.10_044_122 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\Combined Raw Curves (Final)\\7-10 DPI IgG Raw Curves (Final).xlsx", sheet = 12, col_names = TRUE)
DPI_7.10_044_122$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_7.10_044_122$Log_Reciprocal_Serum_Dilution)
str(DPI_7.10_044_122)
model_DPI_7.10_044_122 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_7.10_044_122, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_7.10_044_122, type = "all")
summary(model_DPI_7.10_044_122)
ED(model_DPI_7.10_044_122, c(10,50), interval = "delta")
newdata_DPI_7.10_044_122 <- expand.grid(Log_Reciprocal_Serum_Dilution = exp(seq(log(1.7), log(5.31), length = 100)))
pm_DPI_7.10_044_122 <- predict(model_DPI_7.10_044_122, newdata = newdata_DPI_7.10_044_122, interval = "confidence")
newdata_DPI_7.10_044_122$p <- pm_DPI_7.10_044_122[,1]
newdata_DPI_7.10_044_122$pmin <- pm_DPI_7.10_044_122[,2]
newdata_DPI_7.10_044_122$pmax <- pm_DPI_7.10_044_122[,3]
# 044-126
DPI_7.10_044_126 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\Combined Raw Curves (Final)\\7-10 DPI IgG Raw Curves (Final).xlsx", sheet = 13, col_names = TRUE)
DPI_7.10_044_126$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_7.10_044_126$Log_Reciprocal_Serum_Dilution)
str(DPI_7.10_044_126)
model_DPI_7.10_044_126 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_7.10_044_126, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_7.10_044_126, type = "all")
summary(model_DPI_7.10_044_126)
ED(model_DPI_7.10_044_126, c(10,50), interval = "delta")
newdata_DPI_7.10_044_126 <- expand.grid(Log_Reciprocal_Serum_Dilution = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_7.10_044_126 <- predict(model_DPI_7.10_044_126, newdata = newdata_DPI_7.10_044_126, interval = "confidence")
newdata_DPI_7.10_044_126$p <- pm_DPI_7.10_044_126[,1]
newdata_DPI_7.10_044_126$pmin <- pm_DPI_7.10_044_126[,2]
newdata_DPI_7.10_044_126$pmax <- pm_DPI_7.10_044_126[,3]
# 044-127
DPI_7.10_044_127 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\Combined Raw Curves (Final)\\7-10 DPI IgG Raw Curves (Final).xlsx", sheet = 14, col_names = TRUE)
DPI_7.10_044_127$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_7.10_044_127$Log_Reciprocal_Serum_Dilution)
str(DPI_7.10_044_127)
model_DPI_7.10_044_127 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_7.10_044_127, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_7.10_044_127, type = "all")
summary(model_DPI_7.10_044_127)
ED(model_DPI_7.10_044_127, c(10,50), interval = "delta")
newdata_DPI_7.10_044_127 <- expand.grid(Log_Reciprocal_Serum_Dilution = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_7.10_044_127 <- predict(model_DPI_7.10_044_127, newdata = newdata_DPI_7.10_044_127, interval = "confidence")
newdata_DPI_7.10_044_127$p <- pm_DPI_7.10_044_127[,1]
newdata_DPI_7.10_044_127$pmin <- pm_DPI_7.10_044_127[,2]
newdata_DPI_7.10_044_127$pmax <- pm_DPI_7.10_044_127[,3]
# 044-130
DPI_7.10_044_130 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\Combined Raw Curves (Final)\\7-10 DPI IgG Raw Curves (Final).xlsx", sheet = 15, col_names = TRUE)
DPI_7.10_044_130$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_7.10_044_130$Log_Reciprocal_Serum_Dilution)
str(DPI_7.10_044_130)
model_DPI_7.10_044_130 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_7.10_044_130, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_7.10_044_130, type = "all")
summary(model_DPI_7.10_044_130)
ED(model_DPI_7.10_044_130, c(10,50), interval = "delta")
newdata_DPI_7.10_044_130 <- expand.grid(Log_Reciprocal_Serum_Dilution = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_7.10_044_130 <- predict(model_DPI_7.10_044_130, newdata = newdata_DPI_7.10_044_130, interval = "confidence")
newdata_DPI_7.10_044_130$p <- pm_DPI_7.10_044_130[,1]
newdata_DPI_7.10_044_130$pmin <- pm_DPI_7.10_044_130[,2]
newdata_DPI_7.10_044_130$pmax <- pm_DPI_7.10_044_130[,3]
# 044-131
DPI_7.10_044_131 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\Combined Raw Curves (Final)\\7-10 DPI IgG Raw Curves (Final).xlsx", sheet = 16, col_names = TRUE)
DPI_7.10_044_131$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_7.10_044_131$Log_Reciprocal_Serum_Dilution)
str(DPI_7.10_044_131)
model_DPI_7.10_044_131 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_7.10_044_131, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_7.10_044_131, type = "all")
summary(model_DPI_7.10_044_131)
ED(model_DPI_7.10_044_131, c(10,50), interval = "delta")
newdata_DPI_7.10_044_131 <- expand.grid(Log_Reciprocal_Serum_Dilution = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_7.10_044_131 <- predict(model_DPI_7.10_044_131, newdata = newdata_DPI_7.10_044_131, interval = "confidence")
newdata_DPI_7.10_044_131$p <- pm_DPI_7.10_044_131[,1]
newdata_DPI_7.10_044_131$pmin <- pm_DPI_7.10_044_131[,2]
newdata_DPI_7.10_044_131$pmax <- pm_DPI_7.10_044_131[,3]
# 044-132
DPI_7.10_044_132 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\Combined Raw Curves (Final)\\7-10 DPI IgG Raw Curves (Final).xlsx", sheet = 17, col_names = TRUE)
DPI_7.10_044_132$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_7.10_044_132$Log_Reciprocal_Serum_Dilution)
str(DPI_7.10_044_132)
model_DPI_7.10_044_132 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_7.10_044_132, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_7.10_044_132, type = "all")
summary(model_DPI_7.10_044_132)
ED(model_DPI_7.10_044_132, c(10,50), interval = "delta")
newdata_DPI_7.10_044_132 <- expand.grid(Log_Reciprocal_Serum_Dilution = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_7.10_044_132 <- predict(model_DPI_7.10_044_132, newdata = newdata_DPI_7.10_044_132, interval = "confidence")
newdata_DPI_7.10_044_132$p <- pm_DPI_7.10_044_132[,1]
newdata_DPI_7.10_044_132$pmin <- pm_DPI_7.10_044_132[,2]
newdata_DPI_7.10_044_132$pmax <- pm_DPI_7.10_044_132[,3]
# 044-133
DPI_7.10_044_133 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\Combined Raw Curves (Final)\\7-10 DPI IgG Raw Curves (Final).xlsx", sheet = 18, col_names = TRUE)
DPI_7.10_044_133$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_7.10_044_133$Log_Reciprocal_Serum_Dilution)
str(DPI_7.10_044_133)
model_DPI_7.10_044_133 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_7.10_044_133, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_7.10_044_133, type = "all")
summary(model_DPI_7.10_044_133)
ED(model_DPI_7.10_044_133, c(10,50), interval = "delta")
newdata_DPI_7.10_044_133 <- expand.grid(Log_Reciprocal_Serum_Dilution = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_7.10_044_133 <- predict(model_DPI_7.10_044_133, newdata = newdata_DPI_7.10_044_133, interval = "confidence")
newdata_DPI_7.10_044_133$p <- pm_DPI_7.10_044_133[,1]
newdata_DPI_7.10_044_133$pmin <- pm_DPI_7.10_044_133[,2]
newdata_DPI_7.10_044_133$pmax <- pm_DPI_7.10_044_133[,3]
# Combined IgG WVE Raw Binding Curves figure (7-10 DPI)
rawcurves_DPI_7.10 <- 
  ggplot() + 
  geom_point(data = DPI_7.10_044_101, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "044-101")) + geom_line(data = newdata_DPI_7.10_044_101, aes(x = Log_Reciprocal_Serum_Dilution, y = p, color = "044-101")) +
  geom_point(data = DPI_7.10_044_102, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "044-102")) + geom_line(data = newdata_DPI_7.10_044_102, aes(x = Log_Reciprocal_Serum_Dilution, y = p, color = "044-102")) +
  geom_point(data = DPI_7.10_044_103, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "044-103")) + geom_line(data = newdata_DPI_7.10_044_103, aes(x = Log_Reciprocal_Serum_Dilution, y = p, color = "044-103")) +
  geom_point(data = DPI_7.10_044_104, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "044-104")) + geom_line(data = newdata_DPI_7.10_044_104, aes(x = Log_Reciprocal_Serum_Dilution, y = p, color = "044-104")) +
  geom_point(data = DPI_7.10_044_109, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "044-109")) + geom_line(data = newdata_DPI_7.10_044_109, aes(x = Log_Reciprocal_Serum_Dilution, y = p, color = "044-109")) +
  geom_point(data = DPI_7.10_044_110, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "044-110")) + geom_line(data = newdata_DPI_7.10_044_110, aes(x = Log_Reciprocal_Serum_Dilution, y = p, color = "044-110")) +
  geom_point(data = DPI_7.10_044_112, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "044-112")) + geom_line(data = newdata_DPI_7.10_044_112, aes(x = Log_Reciprocal_Serum_Dilution, y = p, color = "044-112")) +
  geom_point(data = DPI_7.10_044_114, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "044-114")) + geom_line(data = newdata_DPI_7.10_044_114, aes(x = Log_Reciprocal_Serum_Dilution, y = p, color = "044-114")) +
  geom_point(data = DPI_7.10_044_116, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "044-116")) + geom_line(data = newdata_DPI_7.10_044_116, aes(x = Log_Reciprocal_Serum_Dilution, y = p, color = "044-116")) +
  geom_point(data = DPI_7.10_044_117, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "044-117")) + geom_line(data = newdata_DPI_7.10_044_117, aes(x = Log_Reciprocal_Serum_Dilution, y = p, color = "044-117")) +
  geom_point(data = DPI_7.10_044_118, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "044-118")) + geom_line(data = newdata_DPI_7.10_044_118, aes(x = Log_Reciprocal_Serum_Dilution, y = p, color = "044-118")) +
  geom_point(data = DPI_7.10_044_122, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "044-122")) + geom_line(data = newdata_DPI_7.10_044_122, aes(x = Log_Reciprocal_Serum_Dilution, y = p, color = "044-122")) +
  geom_point(data = DPI_7.10_044_126, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "044-126")) + geom_line(data = newdata_DPI_7.10_044_126, aes(x = Log_Reciprocal_Serum_Dilution, y = p, color = "044-126")) +
  geom_point(data = DPI_7.10_044_127, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "044-127")) + geom_line(data = newdata_DPI_7.10_044_127, aes(x = Log_Reciprocal_Serum_Dilution, y = p, color = "044-127")) +
  geom_point(data = DPI_7.10_044_130, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "044-130")) + geom_line(data = newdata_DPI_7.10_044_130, aes(x = Log_Reciprocal_Serum_Dilution, y = p, color = "044-130")) +
  geom_point(data = DPI_7.10_044_131, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "044-131")) + geom_line(data = newdata_DPI_7.10_044_131, aes(x = Log_Reciprocal_Serum_Dilution, y = p, color = "044-131")) +
  geom_point(data = DPI_7.10_044_132, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "044-132")) + geom_line(data = newdata_DPI_7.10_044_132, aes(x = Log_Reciprocal_Serum_Dilution, y = p, color = "044-132")) +
  geom_point(data = DPI_7.10_044_133, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "044-133")) + geom_line(data = newdata_DPI_7.10_044_133, aes(x = Log_Reciprocal_Serum_Dilution, y = p, color = "044-133")) +
  theme_classic() + 
  labs(x = "Log(reciprocal serum dilution)", y = "OD450", title= '7-10 DPI') + 
  xlim(1,6) + 
  scale_y_continuous(expand = c(0,0), limits = c(0,3.5), breaks = c(0, 0.5, 1, 1.5, 2, 2.5, 3, 3.5)) + 
  scale_color_manual(values = c("#7b0000", "#ad0000", "#ff0000", "#ff5d5d", "#ffa3a3", "#ffd2d2", "#000088", "#3434fb", "#6c6cff", "#9c9cff", "#d0d0ff", "#0ef000", "#c1c000", "#fffe04", "#ffd991", "#c7ad7c", "#6ca7a2", "#9c7a99"), breaks = c("044-101", "044-102", "044-103", "044-104", "044-109", "044-110", "044-112", "044-114", "044-116", "044-117", "044-118", "044-122", "044-126", "044-127", "044-130", "044-131", "044-132", "044-133")) + 
  guides(color = guide_legend(title = "Dam ID")) + theme(axis.text = element_text(size = 20), axis.title = element_text(face = 'bold', size = 20), plot.title = element_text(face = "bold", hjust = 0.5, size = 24), aspect.ratio = 1, legend.text = element_text(size= 20), legend.title = element_text(size= 20, face= 'bold'), legend.position = 'right')
rawcurves_DPI_7.10

###
# Combined IgG WVE Raw Binding Curves (13-17 DPI)
###
# 044-101
DPI_13.17_044_101 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\Combined Raw Curves (Final)\\13-17 DPI IgG Raw Curves (Final).xlsx", sheet = 1, col_names = TRUE)
DPI_13.17_044_101$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_13.17_044_101$Log_Reciprocal_Serum_Dilution)
str(DPI_13.17_044_101)
model_DPI_13.17_044_101 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_13.17_044_101, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_13.17_044_101, type = "all")
summary(model_DPI_13.17_044_101)
ED(model_DPI_13.17_044_101, c(10,50), interval = "delta")
newdata_DPI_13.17_044_101 <- expand.grid(Log_Reciprocal_Serum_Dilution = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_13.17_044_101 <- predict(model_DPI_13.17_044_101, newdata = newdata_DPI_13.17_044_101, interval = "confidence")
newdata_DPI_13.17_044_101$p <- pm_DPI_13.17_044_101[,1]
newdata_DPI_13.17_044_101$pmin <- pm_DPI_13.17_044_101[,2]
newdata_DPI_13.17_044_101$pmax <- pm_DPI_13.17_044_101[,3]
# 044-102
DPI_13.17_044_102 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\Combined Raw Curves (Final)\\13-17 DPI IgG Raw Curves (Final).xlsx", sheet = 2, col_names = TRUE)
DPI_13.17_044_102$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_13.17_044_102$Log_Reciprocal_Serum_Dilution)
str(DPI_13.17_044_102)
model_DPI_13.17_044_102 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_13.17_044_102, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_13.17_044_102, type = "all")
summary(model_DPI_13.17_044_102)
ED(model_DPI_13.17_044_102, c(10,50), interval = "delta")
newdata_DPI_13.17_044_102 <- expand.grid(Log_Reciprocal_Serum_Dilution = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_13.17_044_102 <- predict(model_DPI_13.17_044_102, newdata = newdata_DPI_13.17_044_102, interval = "confidence")
newdata_DPI_13.17_044_102$p <- pm_DPI_13.17_044_102[,1]
newdata_DPI_13.17_044_102$pmin <- pm_DPI_13.17_044_102[,2]
newdata_DPI_13.17_044_102$pmax <- pm_DPI_13.17_044_102[,3]
# 044-103
DPI_13.17_044_103 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\Combined Raw Curves (Final)\\13-17 DPI IgG Raw Curves (Final).xlsx", sheet = 3, col_names = TRUE)
DPI_13.17_044_103$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_13.17_044_103$Log_Reciprocal_Serum_Dilution)
str(DPI_13.17_044_103)
model_DPI_13.17_044_103 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_13.17_044_103, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_13.17_044_103, type = "all")
summary(model_DPI_13.17_044_103)
ED(model_DPI_13.17_044_103, c(10,50), interval = "delta")
newdata_DPI_13.17_044_103 <- expand.grid(Log_Reciprocal_Serum_Dilution = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_13.17_044_103 <- predict(model_DPI_13.17_044_103, newdata = newdata_DPI_13.17_044_103, interval = "confidence")
newdata_DPI_13.17_044_103$p <- pm_DPI_13.17_044_103[,1]
newdata_DPI_13.17_044_103$pmin <- pm_DPI_13.17_044_103[,2]
newdata_DPI_13.17_044_103$pmax <- pm_DPI_13.17_044_103[,3]
# 044-104
DPI_13.17_044_104 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\Combined Raw Curves (Final)\\13-17 DPI IgG Raw Curves (Final).xlsx", sheet = 4, col_names = TRUE)
DPI_13.17_044_104$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_13.17_044_104$Log_Reciprocal_Serum_Dilution)
str(DPI_13.17_044_104)
model_DPI_13.17_044_104 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_13.17_044_104, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_13.17_044_104, type = "all")
summary(model_DPI_13.17_044_104)
ED(model_DPI_13.17_044_104, c(10,50), interval = "delta")
newdata_DPI_13.17_044_104 <- expand.grid(Log_Reciprocal_Serum_Dilution = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_13.17_044_104 <- predict(model_DPI_13.17_044_104, newdata = newdata_DPI_13.17_044_104, interval = "confidence")
newdata_DPI_13.17_044_104$p <- pm_DPI_13.17_044_104[,1]
newdata_DPI_13.17_044_104$pmin <- pm_DPI_13.17_044_104[,2]
newdata_DPI_13.17_044_104$pmax <- pm_DPI_13.17_044_104[,3]
# 044-109
DPI_13.17_044_109 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\Combined Raw Curves (Final)\\13-17 DPI IgG Raw Curves (Final).xlsx", sheet = 5, col_names = TRUE)
DPI_13.17_044_109$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_13.17_044_109$Log_Reciprocal_Serum_Dilution)
str(DPI_13.17_044_109)
model_DPI_13.17_044_109 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_13.17_044_109, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_13.17_044_109, type = "all")
summary(model_DPI_13.17_044_109)
ED(model_DPI_13.17_044_109, c(10,50), interval = "delta")
newdata_DPI_13.17_044_109 <- expand.grid(Log_Reciprocal_Serum_Dilution = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_13.17_044_109 <- predict(model_DPI_13.17_044_109, newdata = newdata_DPI_13.17_044_109, interval = "confidence")
newdata_DPI_13.17_044_109$p <- pm_DPI_13.17_044_109[,1]
newdata_DPI_13.17_044_109$pmin <- pm_DPI_13.17_044_109[,2]
newdata_DPI_13.17_044_109$pmax <- pm_DPI_13.17_044_109[,3]
# 044-110
DPI_13.17_044_110 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\Combined Raw Curves (Final)\\13-17 DPI IgG Raw Curves (Final).xlsx", sheet = 6, col_names = TRUE)
DPI_13.17_044_110$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_13.17_044_110$Log_Reciprocal_Serum_Dilution)
str(DPI_13.17_044_110)
model_DPI_13.17_044_110 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_13.17_044_110, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_13.17_044_110, type = "all")
summary(model_DPI_13.17_044_110)
ED(model_DPI_13.17_044_110, c(10,50), interval = "delta")
newdata_DPI_13.17_044_110 <- expand.grid(Log_Reciprocal_Serum_Dilution = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_13.17_044_110 <- predict(model_DPI_13.17_044_110, newdata = newdata_DPI_13.17_044_110, interval = "confidence")
newdata_DPI_13.17_044_110$p <- pm_DPI_13.17_044_110[,1]
newdata_DPI_13.17_044_110$pmin <- pm_DPI_13.17_044_110[,2]
newdata_DPI_13.17_044_110$pmax <- pm_DPI_13.17_044_110[,3]
# 044-112
DPI_13.17_044_112 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\Combined Raw Curves (Final)\\13-17 DPI IgG Raw Curves (Final).xlsx", sheet = 7, col_names = TRUE)
DPI_13.17_044_112$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_13.17_044_112$Log_Reciprocal_Serum_Dilution)
str(DPI_13.17_044_112)
model_DPI_13.17_044_112 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_13.17_044_112, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_13.17_044_112, type = "all")
summary(model_DPI_13.17_044_112)
ED(model_DPI_13.17_044_112, c(10,50), interval = "delta")
newdata_DPI_13.17_044_112 <- expand.grid(Log_Reciprocal_Serum_Dilution = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_13.17_044_112 <- predict(model_DPI_13.17_044_112, newdata = newdata_DPI_13.17_044_112, interval = "confidence")
newdata_DPI_13.17_044_112$p <- pm_DPI_13.17_044_112[,1]
newdata_DPI_13.17_044_112$pmin <- pm_DPI_13.17_044_112[,2]
newdata_DPI_13.17_044_112$pmax <- pm_DPI_13.17_044_112[,3]
#044-114
DPI_13.17_044_114 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\Combined Raw Curves (Final)\\13-17 DPI IgG Raw Curves (Final).xlsx", sheet = 8, col_names = TRUE)
DPI_13.17_044_114$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_13.17_044_114$Log_Reciprocal_Serum_Dilution)
str(DPI_13.17_044_114)
model_DPI_13.17_044_114 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_13.17_044_114, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_13.17_044_114, type = "all")
summary(model_DPI_13.17_044_114)
ED(model_DPI_13.17_044_114, c(10,50), interval = "delta")
newdata_DPI_13.17_044_114 <- expand.grid(Log_Reciprocal_Serum_Dilution = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_13.17_044_114 <- predict(model_DPI_13.17_044_114, newdata = newdata_DPI_13.17_044_114, interval = "confidence")
newdata_DPI_13.17_044_114$p <- pm_DPI_13.17_044_114[,1]
newdata_DPI_13.17_044_114$pmin <- pm_DPI_13.17_044_114[,2]
newdata_DPI_13.17_044_114$pmax <- pm_DPI_13.17_044_114[,3]
# 044-116
DPI_13.17_044_116 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\Combined Raw Curves (Final)\\13-17 DPI IgG Raw Curves (Final).xlsx", sheet = 9, col_names = TRUE)
DPI_13.17_044_116$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_13.17_044_116$Log_Reciprocal_Serum_Dilution)
str(DPI_13.17_044_116)
model_DPI_13.17_044_116 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_13.17_044_116, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_13.17_044_116, type = "all")
summary(model_DPI_13.17_044_116)
ED(model_DPI_13.17_044_116, c(10,50), interval = "delta")
newdata_DPI_13.17_044_116 <- expand.grid(Log_Reciprocal_Serum_Dilution = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_13.17_044_116 <- predict(model_DPI_13.17_044_116, newdata = newdata_DPI_13.17_044_116, interval = "confidence")
newdata_DPI_13.17_044_116$p <- pm_DPI_13.17_044_116[,1]
newdata_DPI_13.17_044_116$pmin <- pm_DPI_13.17_044_116[,2]
newdata_DPI_13.17_044_116$pmax <- pm_DPI_13.17_044_116[,3]
# 044-117
DPI_13.17_044_117 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\Combined Raw Curves (Final)\\13-17 DPI IgG Raw Curves (Final).xlsx", sheet = 10, col_names = TRUE)
DPI_13.17_044_117$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_13.17_044_117$Log_Reciprocal_Serum_Dilution)
str(DPI_13.17_044_117)
model_DPI_13.17_044_117 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_13.17_044_117, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_13.17_044_117, type = "all")
summary(model_DPI_13.17_044_117)
ED(model_DPI_13.17_044_117, c(10,50), interval = "delta")
newdata_DPI_13.17_044_117 <- expand.grid(Log_Reciprocal_Serum_Dilution = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_13.17_044_117 <- predict(model_DPI_13.17_044_117, newdata = newdata_DPI_13.17_044_117, interval = "confidence")
newdata_DPI_13.17_044_117$p <- pm_DPI_13.17_044_117[,1]
newdata_DPI_13.17_044_117$pmin <- pm_DPI_13.17_044_117[,2]
newdata_DPI_13.17_044_117$pmax <- pm_DPI_13.17_044_117[,3]
# 044-118
DPI_13.17_044_118 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\Combined Raw Curves (Final)\\13-17 DPI IgG Raw Curves (Final).xlsx", sheet = 11, col_names = TRUE)
DPI_13.17_044_118$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_13.17_044_118$Log_Reciprocal_Serum_Dilution)
str(DPI_13.17_044_118)
model_DPI_13.17_044_118 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_13.17_044_118, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_13.17_044_118, type = "all")
summary(model_DPI_13.17_044_118)
ED(model_DPI_13.17_044_118, c(10,50), interval = "delta")
newdata_DPI_13.17_044_118 <- expand.grid(Log_Reciprocal_Serum_Dilution = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_13.17_044_118 <- predict(model_DPI_13.17_044_118, newdata = newdata_DPI_13.17_044_118, interval = "confidence")
newdata_DPI_13.17_044_118$p <- pm_DPI_13.17_044_118[,1]
newdata_DPI_13.17_044_118$pmin <- pm_DPI_13.17_044_118[,2]
newdata_DPI_13.17_044_118$pmax <- pm_DPI_13.17_044_118[,3]
# 044-122
DPI_13.17_044_122 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\Combined Raw Curves (Final)\\13-17 DPI IgG Raw Curves (Final).xlsx", sheet = 12, col_names = TRUE)
DPI_13.17_044_122$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_13.17_044_122$Log_Reciprocal_Serum_Dilution)
str(DPI_13.17_044_122)
model_DPI_13.17_044_122 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_13.17_044_122, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_13.17_044_122, type = "all")
summary(model_DPI_13.17_044_122)
ED(model_DPI_13.17_044_122, c(10,50), interval = "delta")
newdata_DPI_13.17_044_122 <- expand.grid(Log_Reciprocal_Serum_Dilution = exp(seq(log(1.7), log(5.31), length = 100)))
pm_DPI_13.17_044_122 <- predict(model_DPI_13.17_044_122, newdata = newdata_DPI_13.17_044_122, interval = "confidence")
newdata_DPI_13.17_044_122$p <- pm_DPI_13.17_044_122[,1]
newdata_DPI_13.17_044_122$pmin <- pm_DPI_13.17_044_122[,2]
newdata_DPI_13.17_044_122$pmax <- pm_DPI_13.17_044_122[,3]
# 044-126
DPI_13.17_044_126 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\Combined Raw Curves (Final)\\13-17 DPI IgG Raw Curves (Final).xlsx", sheet = 13, col_names = TRUE)
DPI_13.17_044_126$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_13.17_044_126$Log_Reciprocal_Serum_Dilution)
str(DPI_13.17_044_126)
model_DPI_13.17_044_126 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_13.17_044_126, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_13.17_044_126, type = "all")
summary(model_DPI_13.17_044_126)
ED(model_DPI_13.17_044_126, c(10,50), interval = "delta")
newdata_DPI_13.17_044_126 <- expand.grid(Log_Reciprocal_Serum_Dilution = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_13.17_044_126 <- predict(model_DPI_13.17_044_126, newdata = newdata_DPI_13.17_044_126, interval = "confidence")
newdata_DPI_13.17_044_126$p <- pm_DPI_13.17_044_126[,1]
newdata_DPI_13.17_044_126$pmin <- pm_DPI_13.17_044_126[,2]
newdata_DPI_13.17_044_126$pmax <- pm_DPI_13.17_044_126[,3]
# 044-127
DPI_13.17_044_127 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\Combined Raw Curves (Final)\\13-17 DPI IgG Raw Curves (Final).xlsx", sheet = 14, col_names = TRUE)
DPI_13.17_044_127$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_13.17_044_127$Log_Reciprocal_Serum_Dilution)
str(DPI_13.17_044_127)
model_DPI_13.17_044_127 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_13.17_044_127, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_13.17_044_127, type = "all")
summary(model_DPI_13.17_044_127)
ED(model_DPI_13.17_044_127, c(10,50), interval = "delta")
newdata_DPI_13.17_044_127 <- expand.grid(Log_Reciprocal_Serum_Dilution = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_13.17_044_127 <- predict(model_DPI_13.17_044_127, newdata = newdata_DPI_13.17_044_127, interval = "confidence")
newdata_DPI_13.17_044_127$p <- pm_DPI_13.17_044_127[,1]
newdata_DPI_13.17_044_127$pmin <- pm_DPI_13.17_044_127[,2]
newdata_DPI_13.17_044_127$pmax <- pm_DPI_13.17_044_127[,3]
# 044-130
DPI_13.17_044_130 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\Combined Raw Curves (Final)\\13-17 DPI IgG Raw Curves (Final).xlsx", sheet = 15, col_names = TRUE)
DPI_13.17_044_130$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_13.17_044_130$Log_Reciprocal_Serum_Dilution)
str(DPI_13.17_044_130)
model_DPI_13.17_044_130 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_13.17_044_130, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_13.17_044_130, type = "all")
summary(model_DPI_13.17_044_130)
ED(model_DPI_13.17_044_130, c(10,50), interval = "delta")
newdata_DPI_13.17_044_130 <- expand.grid(Log_Reciprocal_Serum_Dilution = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_13.17_044_130 <- predict(model_DPI_13.17_044_130, newdata = newdata_DPI_13.17_044_130, interval = "confidence")
newdata_DPI_13.17_044_130$p <- pm_DPI_13.17_044_130[,1]
newdata_DPI_13.17_044_130$pmin <- pm_DPI_13.17_044_130[,2]
newdata_DPI_13.17_044_130$pmax <- pm_DPI_13.17_044_130[,3]
# 044-131
DPI_13.17_044_131 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\Combined Raw Curves (Final)\\13-17 DPI IgG Raw Curves (Final).xlsx", sheet = 16, col_names = TRUE)
DPI_13.17_044_131$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_13.17_044_131$Log_Reciprocal_Serum_Dilution)
str(DPI_13.17_044_131)
model_DPI_13.17_044_131 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_13.17_044_131, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_13.17_044_131, type = "all")
summary(model_DPI_13.17_044_131)
ED(model_DPI_13.17_044_131, c(10,50), interval = "delta")
newdata_DPI_13.17_044_131 <- expand.grid(Log_Reciprocal_Serum_Dilution = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_13.17_044_131 <- predict(model_DPI_13.17_044_131, newdata = newdata_DPI_13.17_044_131, interval = "confidence")
newdata_DPI_13.17_044_131$p <- pm_DPI_13.17_044_131[,1]
newdata_DPI_13.17_044_131$pmin <- pm_DPI_13.17_044_131[,2]
newdata_DPI_13.17_044_131$pmax <- pm_DPI_13.17_044_131[,3]
# 044-132
DPI_13.17_044_132 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\Combined Raw Curves (Final)\\13-17 DPI IgG Raw Curves (Final).xlsx", sheet = 17, col_names = TRUE)
DPI_13.17_044_132$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_13.17_044_132$Log_Reciprocal_Serum_Dilution)
str(DPI_13.17_044_132)
model_DPI_13.17_044_132 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_13.17_044_132, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_13.17_044_132, type = "all")
summary(model_DPI_13.17_044_132)
ED(model_DPI_13.17_044_132, c(10,50), interval = "delta")
newdata_DPI_13.17_044_132 <- expand.grid(Log_Reciprocal_Serum_Dilution = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_13.17_044_132 <- predict(model_DPI_13.17_044_132, newdata = newdata_DPI_13.17_044_132, interval = "confidence")
newdata_DPI_13.17_044_132$p <- pm_DPI_13.17_044_132[,1]
newdata_DPI_13.17_044_132$pmin <- pm_DPI_13.17_044_132[,2]
newdata_DPI_13.17_044_132$pmax <- pm_DPI_13.17_044_132[,3]
# 044-133
DPI_13.17_044_133 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\Combined Raw Curves (Final)\\13-17 DPI IgG Raw Curves (Final).xlsx", sheet = 18, col_names = TRUE)
DPI_13.17_044_133$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_13.17_044_133$Log_Reciprocal_Serum_Dilution)
str(DPI_13.17_044_133)
model_DPI_13.17_044_133 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_13.17_044_133, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_13.17_044_133, type = "all")
summary(model_DPI_13.17_044_133)
ED(model_DPI_13.17_044_133, c(10,50), interval = "delta")
newdata_DPI_13.17_044_133 <- expand.grid(Log_Reciprocal_Serum_Dilution = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_13.17_044_133 <- predict(model_DPI_13.17_044_133, newdata = newdata_DPI_13.17_044_133, interval = "confidence")
newdata_DPI_13.17_044_133$p <- pm_DPI_13.17_044_133[,1]
newdata_DPI_13.17_044_133$pmin <- pm_DPI_13.17_044_133[,2]
newdata_DPI_13.17_044_133$pmax <- pm_DPI_13.17_044_133[,3]
# Combined IgG WVE Raw Binding Curves figure (13-17 DPI)
rawcurves_DPI_13.17 <- 
  ggplot() + 
  geom_point(data = DPI_13.17_044_101, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "044-101")) + geom_line(data = newdata_DPI_13.17_044_101, aes(x = Log_Reciprocal_Serum_Dilution, y = p, color = "044-101")) +
  geom_point(data = DPI_13.17_044_102, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "044-102")) + geom_line(data = newdata_DPI_13.17_044_102, aes(x = Log_Reciprocal_Serum_Dilution, y = p, color = "044-102")) +
  geom_point(data = DPI_13.17_044_103, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "044-103")) + geom_line(data = newdata_DPI_13.17_044_103, aes(x = Log_Reciprocal_Serum_Dilution, y = p, color = "044-103")) +
  geom_point(data = DPI_13.17_044_104, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "044-104")) + geom_line(data = newdata_DPI_13.17_044_104, aes(x = Log_Reciprocal_Serum_Dilution, y = p, color = "044-104")) +
  geom_point(data = DPI_13.17_044_109, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "044-109")) + geom_line(data = newdata_DPI_13.17_044_109, aes(x = Log_Reciprocal_Serum_Dilution, y = p, color = "044-109")) +
  geom_point(data = DPI_13.17_044_110, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "044-110")) + geom_line(data = newdata_DPI_13.17_044_110, aes(x = Log_Reciprocal_Serum_Dilution, y = p, color = "044-110")) +
  geom_point(data = DPI_13.17_044_112, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "044-112")) + geom_line(data = newdata_DPI_13.17_044_112, aes(x = Log_Reciprocal_Serum_Dilution, y = p, color = "044-112")) +
  geom_point(data = DPI_13.17_044_114, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "044-114")) + geom_line(data = newdata_DPI_13.17_044_114, aes(x = Log_Reciprocal_Serum_Dilution, y = p, color = "044-114")) +
  geom_point(data = DPI_13.17_044_116, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "044-116")) + geom_line(data = newdata_DPI_13.17_044_116, aes(x = Log_Reciprocal_Serum_Dilution, y = p, color = "044-116")) +
  geom_point(data = DPI_13.17_044_117, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "044-117")) + geom_line(data = newdata_DPI_13.17_044_117, aes(x = Log_Reciprocal_Serum_Dilution, y = p, color = "044-117")) +
  geom_point(data = DPI_13.17_044_118, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "044-118")) + geom_line(data = newdata_DPI_13.17_044_118, aes(x = Log_Reciprocal_Serum_Dilution, y = p, color = "044-118")) +
  geom_point(data = DPI_13.17_044_122, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "044-122")) + geom_line(data = newdata_DPI_13.17_044_122, aes(x = Log_Reciprocal_Serum_Dilution, y = p, color = "044-122")) +
  geom_point(data = DPI_13.17_044_126, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "044-126")) + geom_line(data = newdata_DPI_13.17_044_126, aes(x = Log_Reciprocal_Serum_Dilution, y = p, color = "044-126")) +
  geom_point(data = DPI_13.17_044_127, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "044-127")) + geom_line(data = newdata_DPI_13.17_044_127, aes(x = Log_Reciprocal_Serum_Dilution, y = p, color = "044-127")) +
  geom_point(data = DPI_13.17_044_130, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "044-130")) + geom_line(data = newdata_DPI_13.17_044_130, aes(x = Log_Reciprocal_Serum_Dilution, y = p, color = "044-130")) +
  geom_point(data = DPI_13.17_044_131, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "044-131")) + geom_line(data = newdata_DPI_13.17_044_131, aes(x = Log_Reciprocal_Serum_Dilution, y = p, color = "044-131")) +
  geom_point(data = DPI_13.17_044_132, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "044-132")) + geom_line(data = newdata_DPI_13.17_044_132, aes(x = Log_Reciprocal_Serum_Dilution, y = p, color = "044-132")) +
  geom_point(data = DPI_13.17_044_133, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "044-133")) + geom_line(data = newdata_DPI_13.17_044_133, aes(x = Log_Reciprocal_Serum_Dilution, y = p, color = "044-133")) +
  theme_classic() + 
  labs(x = "Log(reciprocal serum dilution)", y = "OD450", title= '13-17 DPI') + 
  xlim(1,6) + 
  scale_y_continuous(expand = c(0,0), limits = c(0,3.5), breaks = c(0, 0.5, 1, 1.5, 2, 2.5, 3, 3.5)) + 
  scale_color_manual(values = c("#7b0000", "#ad0000", "#ff0000", "#ff5d5d", "#ffa3a3", "#ffd2d2", "#000088", "#3434fb", "#6c6cff", "#9c9cff", "#d0d0ff", "#0ef000", "#c1c000", "#fffe04", "#ffd991", "#c7ad7c", "#6ca7a2", "#9c7a99"), breaks = c("044-101", "044-102", "044-103", "044-104", "044-109", "044-110", "044-112", "044-114", "044-116", "044-117", "044-118", "044-122", "044-126", "044-127", "044-130", "044-131", "044-132", "044-133")) + 
  guides(color = guide_legend(title = "Dam ID")) + theme(axis.text = element_text(size = 20), axis.title = element_text(face = 'bold', size = 20), plot.title = element_text(face = "bold", hjust = 0.5, size = 24), aspect.ratio = 1, legend.text = element_text(size= 20), legend.title = element_text(size= 20, face= 'bold'), legend.position = 'right')
rawcurves_DPI_13.17

###
# Combined IgG WVE Raw Binding Curves (18-24 DPI)
###
# 044-101 (No 18-24 DPI sample run)
# 044-102
DPI_18.24_044_102 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\Combined Raw Curves (Final)\\18-24 DPI IgG Raw Curves (Final).xlsx", sheet = 2, col_names = TRUE)
DPI_18.24_044_102$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_18.24_044_102$Log_Reciprocal_Serum_Dilution)
str(DPI_18.24_044_102)
model_DPI_18.24_044_102 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_18.24_044_102, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_18.24_044_102, type = "all")
summary(model_DPI_18.24_044_102)
ED(model_DPI_18.24_044_102, c(10,50), interval = "delta")
newdata_DPI_18.24_044_102 <- expand.grid(Log_Reciprocal_Serum_Dilution = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_18.24_044_102 <- predict(model_DPI_18.24_044_102, newdata = newdata_DPI_18.24_044_102, interval = "confidence")
newdata_DPI_18.24_044_102$p <- pm_DPI_18.24_044_102[,1]
newdata_DPI_18.24_044_102$pmin <- pm_DPI_18.24_044_102[,2]
newdata_DPI_18.24_044_102$pmax <- pm_DPI_18.24_044_102[,3]
# 044-103
DPI_18.24_044_103 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\Combined Raw Curves (Final)\\18-24 DPI IgG Raw Curves (Final).xlsx", sheet = 3, col_names = TRUE)
DPI_18.24_044_103$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_18.24_044_103$Log_Reciprocal_Serum_Dilution)
str(DPI_18.24_044_103)
model_DPI_18.24_044_103 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_18.24_044_103, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_18.24_044_103, type = "all")
summary(model_DPI_18.24_044_103)
ED(model_DPI_18.24_044_103, c(10,50), interval = "delta")
newdata_DPI_18.24_044_103 <- expand.grid(Log_Reciprocal_Serum_Dilution = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_18.24_044_103 <- predict(model_DPI_18.24_044_103, newdata = newdata_DPI_18.24_044_103, interval = "confidence")
newdata_DPI_18.24_044_103$p <- pm_DPI_18.24_044_103[,1]
newdata_DPI_18.24_044_103$pmin <- pm_DPI_18.24_044_103[,2]
newdata_DPI_18.24_044_103$pmax <- pm_DPI_18.24_044_103[,3]
# 044-104
DPI_18.24_044_104 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\Combined Raw Curves (Final)\\18-24 DPI IgG Raw Curves (Final).xlsx", sheet = 4, col_names = TRUE)
DPI_18.24_044_104$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_18.24_044_104$Log_Reciprocal_Serum_Dilution)
str(DPI_18.24_044_104)
model_DPI_18.24_044_104 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_18.24_044_104, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_18.24_044_104, type = "all")
summary(model_DPI_18.24_044_104)
ED(model_DPI_18.24_044_104, c(10,50), interval = "delta")
newdata_DPI_18.24_044_104 <- expand.grid(Log_Reciprocal_Serum_Dilution = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_18.24_044_104 <- predict(model_DPI_18.24_044_104, newdata = newdata_DPI_18.24_044_104, interval = "confidence")
newdata_DPI_18.24_044_104$p <- pm_DPI_18.24_044_104[,1]
newdata_DPI_18.24_044_104$pmin <- pm_DPI_18.24_044_104[,2]
newdata_DPI_18.24_044_104$pmax <- pm_DPI_18.24_044_104[,3]
# 044-109
DPI_18.24_044_109 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\Combined Raw Curves (Final)\\18-24 DPI IgG Raw Curves (Final).xlsx", sheet = 5, col_names = TRUE)
DPI_18.24_044_109$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_18.24_044_109$Log_Reciprocal_Serum_Dilution)
str(DPI_18.24_044_109)
model_DPI_18.24_044_109 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_18.24_044_109, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_18.24_044_109, type = "all")
summary(model_DPI_18.24_044_109)
ED(model_DPI_18.24_044_109, c(10,50), interval = "delta")
newdata_DPI_18.24_044_109 <- expand.grid(Log_Reciprocal_Serum_Dilution = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_18.24_044_109 <- predict(model_DPI_18.24_044_109, newdata = newdata_DPI_18.24_044_109, interval = "confidence")
newdata_DPI_18.24_044_109$p <- pm_DPI_18.24_044_109[,1]
newdata_DPI_18.24_044_109$pmin <- pm_DPI_18.24_044_109[,2]
newdata_DPI_18.24_044_109$pmax <- pm_DPI_18.24_044_109[,3]
# 044-110
DPI_18.24_044_110 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\Combined Raw Curves (Final)\\18-24 DPI IgG Raw Curves (Final).xlsx", sheet = 6, col_names = TRUE)
DPI_18.24_044_110$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_18.24_044_110$Log_Reciprocal_Serum_Dilution)
str(DPI_18.24_044_110)
model_DPI_18.24_044_110 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_18.24_044_110, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_18.24_044_110, type = "all")
summary(model_DPI_18.24_044_110)
ED(model_DPI_18.24_044_110, c(10,50), interval = "delta")
newdata_DPI_18.24_044_110 <- expand.grid(Log_Reciprocal_Serum_Dilution = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_18.24_044_110 <- predict(model_DPI_18.24_044_110, newdata = newdata_DPI_18.24_044_110, interval = "confidence")
newdata_DPI_18.24_044_110$p <- pm_DPI_18.24_044_110[,1]
newdata_DPI_18.24_044_110$pmin <- pm_DPI_18.24_044_110[,2]
newdata_DPI_18.24_044_110$pmax <- pm_DPI_18.24_044_110[,3]
# 044-112
DPI_18.24_044_112 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\Combined Raw Curves (Final)\\18-24 DPI IgG Raw Curves (Final).xlsx", sheet = 7, col_names = TRUE)
DPI_18.24_044_112$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_18.24_044_112$Log_Reciprocal_Serum_Dilution)
str(DPI_18.24_044_112)
model_DPI_18.24_044_112 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_18.24_044_112, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_18.24_044_112, type = "all")
summary(model_DPI_18.24_044_112)
ED(model_DPI_18.24_044_112, c(10,50), interval = "delta")
newdata_DPI_18.24_044_112 <- expand.grid(Log_Reciprocal_Serum_Dilution = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_18.24_044_112 <- predict(model_DPI_18.24_044_112, newdata = newdata_DPI_18.24_044_112, interval = "confidence")
newdata_DPI_18.24_044_112$p <- pm_DPI_18.24_044_112[,1]
newdata_DPI_18.24_044_112$pmin <- pm_DPI_18.24_044_112[,2]
newdata_DPI_18.24_044_112$pmax <- pm_DPI_18.24_044_112[,3]
#044-114
DPI_18.24_044_114 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\Combined Raw Curves (Final)\\18-24 DPI IgG Raw Curves (Final).xlsx", sheet = 8, col_names = TRUE)
DPI_18.24_044_114$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_18.24_044_114$Log_Reciprocal_Serum_Dilution)
str(DPI_18.24_044_114)
model_DPI_18.24_044_114 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_18.24_044_114, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_18.24_044_114, type = "all")
summary(model_DPI_18.24_044_114)
ED(model_DPI_18.24_044_114, c(10,50), interval = "delta")
newdata_DPI_18.24_044_114 <- expand.grid(Log_Reciprocal_Serum_Dilution = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_18.24_044_114 <- predict(model_DPI_18.24_044_114, newdata = newdata_DPI_18.24_044_114, interval = "confidence")
newdata_DPI_18.24_044_114$p <- pm_DPI_18.24_044_114[,1]
newdata_DPI_18.24_044_114$pmin <- pm_DPI_18.24_044_114[,2]
newdata_DPI_18.24_044_114$pmax <- pm_DPI_18.24_044_114[,3]
# 044-116
DPI_18.24_044_116 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\Combined Raw Curves (Final)\\18-24 DPI IgG Raw Curves (Final).xlsx", sheet = 9, col_names = TRUE)
DPI_18.24_044_116$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_18.24_044_116$Log_Reciprocal_Serum_Dilution)
str(DPI_18.24_044_116)
model_DPI_18.24_044_116 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_18.24_044_116, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_18.24_044_116, type = "all")
summary(model_DPI_18.24_044_116)
ED(model_DPI_18.24_044_116, c(10,50), interval = "delta")
newdata_DPI_18.24_044_116 <- expand.grid(Log_Reciprocal_Serum_Dilution = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_18.24_044_116 <- predict(model_DPI_18.24_044_116, newdata = newdata_DPI_18.24_044_116, interval = "confidence")
newdata_DPI_18.24_044_116$p <- pm_DPI_18.24_044_116[,1]
newdata_DPI_18.24_044_116$pmin <- pm_DPI_18.24_044_116[,2]
newdata_DPI_18.24_044_116$pmax <- pm_DPI_18.24_044_116[,3]
# 044-117
DPI_18.24_044_117 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\Combined Raw Curves (Final)\\18-24 DPI IgG Raw Curves (Final).xlsx", sheet = 10, col_names = TRUE)
DPI_18.24_044_117$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_18.24_044_117$Log_Reciprocal_Serum_Dilution)
str(DPI_18.24_044_117)
model_DPI_18.24_044_117 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_18.24_044_117, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_18.24_044_117, type = "all")
summary(model_DPI_18.24_044_117)
ED(model_DPI_18.24_044_117, c(10,50), interval = "delta")
newdata_DPI_18.24_044_117 <- expand.grid(Log_Reciprocal_Serum_Dilution = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_18.24_044_117 <- predict(model_DPI_18.24_044_117, newdata = newdata_DPI_18.24_044_117, interval = "confidence")
newdata_DPI_18.24_044_117$p <- pm_DPI_18.24_044_117[,1]
newdata_DPI_18.24_044_117$pmin <- pm_DPI_18.24_044_117[,2]
newdata_DPI_18.24_044_117$pmax <- pm_DPI_18.24_044_117[,3]
# 044-118
DPI_18.24_044_118 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\Combined Raw Curves (Final)\\18-24 DPI IgG Raw Curves (Final).xlsx", sheet = 11, col_names = TRUE)
DPI_18.24_044_118$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_18.24_044_118$Log_Reciprocal_Serum_Dilution)
str(DPI_18.24_044_118)
model_DPI_18.24_044_118 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_18.24_044_118, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_18.24_044_118, type = "all")
summary(model_DPI_18.24_044_118)
ED(model_DPI_18.24_044_118, c(10,50), interval = "delta")
newdata_DPI_18.24_044_118 <- expand.grid(Log_Reciprocal_Serum_Dilution = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_18.24_044_118 <- predict(model_DPI_18.24_044_118, newdata = newdata_DPI_18.24_044_118, interval = "confidence")
newdata_DPI_18.24_044_118$p <- pm_DPI_18.24_044_118[,1]
newdata_DPI_18.24_044_118$pmin <- pm_DPI_18.24_044_118[,2]
newdata_DPI_18.24_044_118$pmax <- pm_DPI_18.24_044_118[,3]
# 044-122
DPI_18.24_044_122 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\Combined Raw Curves (Final)\\18-24 DPI IgG Raw Curves (Final).xlsx", sheet = 12, col_names = TRUE)
DPI_18.24_044_122$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_18.24_044_122$Log_Reciprocal_Serum_Dilution)
str(DPI_18.24_044_122)
model_DPI_18.24_044_122 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_18.24_044_122, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_18.24_044_122, type = "all")
summary(model_DPI_18.24_044_122)
ED(model_DPI_18.24_044_122, c(10,50), interval = "delta")
newdata_DPI_18.24_044_122 <- expand.grid(Log_Reciprocal_Serum_Dilution = exp(seq(log(1.7), log(5.31), length = 100)))
pm_DPI_18.24_044_122 <- predict(model_DPI_18.24_044_122, newdata = newdata_DPI_18.24_044_122, interval = "confidence")
newdata_DPI_18.24_044_122$p <- pm_DPI_18.24_044_122[,1]
newdata_DPI_18.24_044_122$pmin <- pm_DPI_18.24_044_122[,2]
newdata_DPI_18.24_044_122$pmax <- pm_DPI_18.24_044_122[,3]
# 044-126
DPI_18.24_044_126 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\Combined Raw Curves (Final)\\18-24 DPI IgG Raw Curves (Final).xlsx", sheet = 13, col_names = TRUE)
DPI_18.24_044_126$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_18.24_044_126$Log_Reciprocal_Serum_Dilution)
str(DPI_18.24_044_126)
model_DPI_18.24_044_126 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_18.24_044_126, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_18.24_044_126, type = "all")
summary(model_DPI_18.24_044_126)
ED(model_DPI_18.24_044_126, c(10,50), interval = "delta")
newdata_DPI_18.24_044_126 <- expand.grid(Log_Reciprocal_Serum_Dilution = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_18.24_044_126 <- predict(model_DPI_18.24_044_126, newdata = newdata_DPI_18.24_044_126, interval = "confidence")
newdata_DPI_18.24_044_126$p <- pm_DPI_18.24_044_126[,1]
newdata_DPI_18.24_044_126$pmin <- pm_DPI_18.24_044_126[,2]
newdata_DPI_18.24_044_126$pmax <- pm_DPI_18.24_044_126[,3]
# 044-127
DPI_18.24_044_127 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\Combined Raw Curves (Final)\\18-24 DPI IgG Raw Curves (Final).xlsx", sheet = 14, col_names = TRUE)
DPI_18.24_044_127$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_18.24_044_127$Log_Reciprocal_Serum_Dilution)
str(DPI_18.24_044_127)
model_DPI_18.24_044_127 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_18.24_044_127, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_18.24_044_127, type = "all")
summary(model_DPI_18.24_044_127)
ED(model_DPI_18.24_044_127, c(10,50), interval = "delta")
newdata_DPI_18.24_044_127 <- expand.grid(Log_Reciprocal_Serum_Dilution = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_18.24_044_127 <- predict(model_DPI_18.24_044_127, newdata = newdata_DPI_18.24_044_127, interval = "confidence")
newdata_DPI_18.24_044_127$p <- pm_DPI_18.24_044_127[,1]
newdata_DPI_18.24_044_127$pmin <- pm_DPI_18.24_044_127[,2]
newdata_DPI_18.24_044_127$pmax <- pm_DPI_18.24_044_127[,3]
# 044-130
DPI_18.24_044_130 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\Combined Raw Curves (Final)\\18-24 DPI IgG Raw Curves (Final).xlsx", sheet = 15, col_names = TRUE)
DPI_18.24_044_130$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_18.24_044_130$Log_Reciprocal_Serum_Dilution)
str(DPI_18.24_044_130)
model_DPI_18.24_044_130 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_18.24_044_130, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_18.24_044_130, type = "all")
summary(model_DPI_18.24_044_130)
ED(model_DPI_18.24_044_130, c(10,50), interval = "delta")
newdata_DPI_18.24_044_130 <- expand.grid(Log_Reciprocal_Serum_Dilution = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_18.24_044_130 <- predict(model_DPI_18.24_044_130, newdata = newdata_DPI_18.24_044_130, interval = "confidence")
newdata_DPI_18.24_044_130$p <- pm_DPI_18.24_044_130[,1]
newdata_DPI_18.24_044_130$pmin <- pm_DPI_18.24_044_130[,2]
newdata_DPI_18.24_044_130$pmax <- pm_DPI_18.24_044_130[,3]
# 044-131
DPI_18.24_044_131 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\Combined Raw Curves (Final)\\18-24 DPI IgG Raw Curves (Final).xlsx", sheet = 16, col_names = TRUE)
DPI_18.24_044_131$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_18.24_044_131$Log_Reciprocal_Serum_Dilution)
str(DPI_18.24_044_131)
model_DPI_18.24_044_131 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_18.24_044_131, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_18.24_044_131, type = "all")
summary(model_DPI_18.24_044_131)
ED(model_DPI_18.24_044_131, c(10,50), interval = "delta")
newdata_DPI_18.24_044_131 <- expand.grid(Log_Reciprocal_Serum_Dilution = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_18.24_044_131 <- predict(model_DPI_18.24_044_131, newdata = newdata_DPI_18.24_044_131, interval = "confidence")
newdata_DPI_18.24_044_131$p <- pm_DPI_18.24_044_131[,1]
newdata_DPI_18.24_044_131$pmin <- pm_DPI_18.24_044_131[,2]
newdata_DPI_18.24_044_131$pmax <- pm_DPI_18.24_044_131[,3]
# 044-132
DPI_18.24_044_132 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\Combined Raw Curves (Final)\\18-24 DPI IgG Raw Curves (Final).xlsx", sheet = 17, col_names = TRUE)
DPI_18.24_044_132$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_18.24_044_132$Log_Reciprocal_Serum_Dilution)
str(DPI_18.24_044_132)
model_DPI_18.24_044_132 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_18.24_044_132, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_18.24_044_132, type = "all")
summary(model_DPI_18.24_044_132)
ED(model_DPI_18.24_044_132, c(10,50), interval = "delta")
newdata_DPI_18.24_044_132 <- expand.grid(Log_Reciprocal_Serum_Dilution = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_18.24_044_132 <- predict(model_DPI_18.24_044_132, newdata = newdata_DPI_18.24_044_132, interval = "confidence")
newdata_DPI_18.24_044_132$p <- pm_DPI_18.24_044_132[,1]
newdata_DPI_18.24_044_132$pmin <- pm_DPI_18.24_044_132[,2]
newdata_DPI_18.24_044_132$pmax <- pm_DPI_18.24_044_132[,3]
# 044-133
DPI_18.24_044_133 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\Combined Raw Curves (Final)\\18-24 DPI IgG Raw Curves (Final).xlsx", sheet = 18, col_names = TRUE)
DPI_18.24_044_133$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_18.24_044_133$Log_Reciprocal_Serum_Dilution)
str(DPI_18.24_044_133)
model_DPI_18.24_044_133 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_18.24_044_133, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_18.24_044_133, type = "all")
summary(model_DPI_18.24_044_133)
ED(model_DPI_18.24_044_133, c(10,50), interval = "delta")
newdata_DPI_18.24_044_133 <- expand.grid(Log_Reciprocal_Serum_Dilution = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_18.24_044_133 <- predict(model_DPI_18.24_044_133, newdata = newdata_DPI_18.24_044_133, interval = "confidence")
newdata_DPI_18.24_044_133$p <- pm_DPI_18.24_044_133[,1]
newdata_DPI_18.24_044_133$pmin <- pm_DPI_18.24_044_133[,2]
newdata_DPI_18.24_044_133$pmax <- pm_DPI_18.24_044_133[,3]
# Combined IgG WVE Raw Binding Curves figure (18-24 DPI)
rawcurves_DPI_18.24 <- 
  ggplot() + 
  geom_point(data = DPI_18.24_044_102, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "044-102")) + geom_line(data = newdata_DPI_18.24_044_102, aes(x = Log_Reciprocal_Serum_Dilution, y = p, color = "044-102")) +
  geom_point(data = DPI_18.24_044_103, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "044-103")) + geom_line(data = newdata_DPI_18.24_044_103, aes(x = Log_Reciprocal_Serum_Dilution, y = p, color = "044-103")) +
  geom_point(data = DPI_18.24_044_104, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "044-104")) + geom_line(data = newdata_DPI_18.24_044_104, aes(x = Log_Reciprocal_Serum_Dilution, y = p, color = "044-104")) +
  geom_point(data = DPI_18.24_044_109, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "044-109")) + geom_line(data = newdata_DPI_18.24_044_109, aes(x = Log_Reciprocal_Serum_Dilution, y = p, color = "044-109")) +
  geom_point(data = DPI_18.24_044_110, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "044-110")) + geom_line(data = newdata_DPI_18.24_044_110, aes(x = Log_Reciprocal_Serum_Dilution, y = p, color = "044-110")) +
  geom_point(data = DPI_18.24_044_112, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "044-112")) + geom_line(data = newdata_DPI_18.24_044_112, aes(x = Log_Reciprocal_Serum_Dilution, y = p, color = "044-112")) +
  geom_point(data = DPI_18.24_044_114, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "044-114")) + geom_line(data = newdata_DPI_18.24_044_114, aes(x = Log_Reciprocal_Serum_Dilution, y = p, color = "044-114")) +
  geom_point(data = DPI_18.24_044_116, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "044-116")) + geom_line(data = newdata_DPI_18.24_044_116, aes(x = Log_Reciprocal_Serum_Dilution, y = p, color = "044-116")) +
  geom_point(data = DPI_18.24_044_117, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "044-117")) + geom_line(data = newdata_DPI_18.24_044_117, aes(x = Log_Reciprocal_Serum_Dilution, y = p, color = "044-117")) +
  geom_point(data = DPI_18.24_044_118, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "044-118")) + geom_line(data = newdata_DPI_18.24_044_118, aes(x = Log_Reciprocal_Serum_Dilution, y = p, color = "044-118")) +
  geom_point(data = DPI_18.24_044_122, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "044-122")) + geom_line(data = newdata_DPI_18.24_044_122, aes(x = Log_Reciprocal_Serum_Dilution, y = p, color = "044-122")) +
  geom_point(data = DPI_18.24_044_126, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "044-126")) + geom_line(data = newdata_DPI_18.24_044_126, aes(x = Log_Reciprocal_Serum_Dilution, y = p, color = "044-126")) +
  geom_point(data = DPI_18.24_044_127, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "044-127")) + geom_line(data = newdata_DPI_18.24_044_127, aes(x = Log_Reciprocal_Serum_Dilution, y = p, color = "044-127")) +
  geom_point(data = DPI_18.24_044_130, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "044-130")) + geom_line(data = newdata_DPI_18.24_044_130, aes(x = Log_Reciprocal_Serum_Dilution, y = p, color = "044-130")) +
  geom_point(data = DPI_18.24_044_131, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "044-131")) + geom_line(data = newdata_DPI_18.24_044_131, aes(x = Log_Reciprocal_Serum_Dilution, y = p, color = "044-131")) +
  geom_point(data = DPI_18.24_044_132, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "044-132")) + geom_line(data = newdata_DPI_18.24_044_132, aes(x = Log_Reciprocal_Serum_Dilution, y = p, color = "044-132")) +
  geom_point(data = DPI_18.24_044_133, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "044-133")) + geom_line(data = newdata_DPI_18.24_044_133, aes(x = Log_Reciprocal_Serum_Dilution, y = p, color = "044-133")) +
  theme_classic() + 
  labs(x = "Log(reciprocal serum dilution)", y = "OD450", title= '18-24 DPI') + 
  xlim(1,6) + 
  scale_y_continuous(expand = c(0,0), limits = c(0,3.5), breaks = c(0, 0.5, 1, 1.5, 2, 2.5, 3, 3.5)) + 
  scale_color_manual(values = c("#7b0000", "#ad0000", "#ff0000", "#ff5d5d", "#ffa3a3", "#ffd2d2", "#000088", "#3434fb", "#6c6cff", "#9c9cff", "#d0d0ff", "#0ef000", "#c1c000", "#fffe04", "#ffd991", "#c7ad7c", "#6ca7a2", "#9c7a99"), breaks = c("044-101", "044-102", "044-103", "044-104", "044-109", "044-110", "044-112", "044-114", "044-116", "044-117", "044-118", "044-122", "044-126", "044-127", "044-130", "044-131", "044-132", "044-133")) + 
  guides(color = guide_legend(title = "Dam ID")) + theme(axis.text = element_text(size = 20), axis.title = element_text(face = 'bold', size = 20), plot.title = element_text(face = "bold", hjust = 0.5, size = 24), aspect.ratio = 1, legend.text = element_text(size= 20), legend.title = element_text(size= 20, face= 'bold'), legend.position = 'right')
rawcurves_DPI_18.24

###
# Combined IgG WVE Raw Binding Curves (27-38 DPI)
###
# 044-101
DPI_27.38_044_101 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\Combined Raw Curves (Final)\\27-38 DPI IgG Raw Curves (Final).xlsx", sheet = 1, col_names = TRUE)
DPI_27.38_044_101$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_27.38_044_101$Log_Reciprocal_Serum_Dilution)
str(DPI_27.38_044_101)
model_DPI_27.38_044_101 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_27.38_044_101, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_27.38_044_101, type = "all")
summary(model_DPI_27.38_044_101)
ED(model_DPI_27.38_044_101, c(10,50), interval = "delta")
newdata_DPI_27.38_044_101 <- expand.grid(Log_Reciprocal_Serum_Dilution = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_27.38_044_101 <- predict(model_DPI_27.38_044_101, newdata = newdata_DPI_27.38_044_101, interval = "confidence")
newdata_DPI_27.38_044_101$p <- pm_DPI_27.38_044_101[,1]
newdata_DPI_27.38_044_101$pmin <- pm_DPI_27.38_044_101[,2]
newdata_DPI_27.38_044_101$pmax <- pm_DPI_27.38_044_101[,3]
# 044-102
DPI_27.38_044_102 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\Combined Raw Curves (Final)\\27-38 DPI IgG Raw Curves (Final).xlsx", sheet = 2, col_names = TRUE)
DPI_27.38_044_102$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_27.38_044_102$Log_Reciprocal_Serum_Dilution)
str(DPI_27.38_044_102)
model_DPI_27.38_044_102 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_27.38_044_102, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_27.38_044_102, type = "all")
summary(model_DPI_27.38_044_102)
ED(model_DPI_27.38_044_102, c(10,50), interval = "delta")
newdata_DPI_27.38_044_102 <- expand.grid(Log_Reciprocal_Serum_Dilution = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_27.38_044_102 <- predict(model_DPI_27.38_044_102, newdata = newdata_DPI_27.38_044_102, interval = "confidence")
newdata_DPI_27.38_044_102$p <- pm_DPI_27.38_044_102[,1]
newdata_DPI_27.38_044_102$pmin <- pm_DPI_27.38_044_102[,2]
newdata_DPI_27.38_044_102$pmax <- pm_DPI_27.38_044_102[,3]
# 044-103
DPI_27.38_044_103 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\Combined Raw Curves (Final)\\27-38 DPI IgG Raw Curves (Final).xlsx", sheet = 3, col_names = TRUE)
DPI_27.38_044_103$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_27.38_044_103$Log_Reciprocal_Serum_Dilution)
str(DPI_27.38_044_103)
model_DPI_27.38_044_103 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_27.38_044_103, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_27.38_044_103, type = "all")
summary(model_DPI_27.38_044_103)
ED(model_DPI_27.38_044_103, c(10,50), interval = "delta")
newdata_DPI_27.38_044_103 <- expand.grid(Log_Reciprocal_Serum_Dilution = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_27.38_044_103 <- predict(model_DPI_27.38_044_103, newdata = newdata_DPI_27.38_044_103, interval = "confidence")
newdata_DPI_27.38_044_103$p <- pm_DPI_27.38_044_103[,1]
newdata_DPI_27.38_044_103$pmin <- pm_DPI_27.38_044_103[,2]
newdata_DPI_27.38_044_103$pmax <- pm_DPI_27.38_044_103[,3]
# 044-104
DPI_27.38_044_104 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\Combined Raw Curves (Final)\\27-38 DPI IgG Raw Curves (Final).xlsx", sheet = 4, col_names = TRUE)
DPI_27.38_044_104$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_27.38_044_104$Log_Reciprocal_Serum_Dilution)
str(DPI_27.38_044_104)
model_DPI_27.38_044_104 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_27.38_044_104, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_27.38_044_104, type = "all")
summary(model_DPI_27.38_044_104)
ED(model_DPI_27.38_044_104, c(10,50), interval = "delta")
newdata_DPI_27.38_044_104 <- expand.grid(Log_Reciprocal_Serum_Dilution = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_27.38_044_104 <- predict(model_DPI_27.38_044_104, newdata = newdata_DPI_27.38_044_104, interval = "confidence")
newdata_DPI_27.38_044_104$p <- pm_DPI_27.38_044_104[,1]
newdata_DPI_27.38_044_104$pmin <- pm_DPI_27.38_044_104[,2]
newdata_DPI_27.38_044_104$pmax <- pm_DPI_27.38_044_104[,3]
# 044-109
DPI_27.38_044_109 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\Combined Raw Curves (Final)\\27-38 DPI IgG Raw Curves (Final).xlsx", sheet = 5, col_names = TRUE)
DPI_27.38_044_109$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_27.38_044_109$Log_Reciprocal_Serum_Dilution)
str(DPI_27.38_044_109)
model_DPI_27.38_044_109 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_27.38_044_109, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_27.38_044_109, type = "all")
summary(model_DPI_27.38_044_109)
ED(model_DPI_27.38_044_109, c(10,50), interval = "delta")
newdata_DPI_27.38_044_109 <- expand.grid(Log_Reciprocal_Serum_Dilution = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_27.38_044_109 <- predict(model_DPI_27.38_044_109, newdata = newdata_DPI_27.38_044_109, interval = "confidence")
newdata_DPI_27.38_044_109$p <- pm_DPI_27.38_044_109[,1]
newdata_DPI_27.38_044_109$pmin <- pm_DPI_27.38_044_109[,2]
newdata_DPI_27.38_044_109$pmax <- pm_DPI_27.38_044_109[,3]
# 044-110
DPI_27.38_044_110 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\Combined Raw Curves (Final)\\27-38 DPI IgG Raw Curves (Final).xlsx", sheet = 6, col_names = TRUE)
DPI_27.38_044_110$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_27.38_044_110$Log_Reciprocal_Serum_Dilution)
str(DPI_27.38_044_110)
model_DPI_27.38_044_110 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_27.38_044_110, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_27.38_044_110, type = "all")
summary(model_DPI_27.38_044_110)
ED(model_DPI_27.38_044_110, c(10,50), interval = "delta")
newdata_DPI_27.38_044_110 <- expand.grid(Log_Reciprocal_Serum_Dilution = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_27.38_044_110 <- predict(model_DPI_27.38_044_110, newdata = newdata_DPI_27.38_044_110, interval = "confidence")
newdata_DPI_27.38_044_110$p <- pm_DPI_27.38_044_110[,1]
newdata_DPI_27.38_044_110$pmin <- pm_DPI_27.38_044_110[,2]
newdata_DPI_27.38_044_110$pmax <- pm_DPI_27.38_044_110[,3]
# 044-112
DPI_27.38_044_112 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\Combined Raw Curves (Final)\\27-38 DPI IgG Raw Curves (Final).xlsx", sheet = 7, col_names = TRUE)
DPI_27.38_044_112$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_27.38_044_112$Log_Reciprocal_Serum_Dilution)
str(DPI_27.38_044_112)
model_DPI_27.38_044_112 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_27.38_044_112, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_27.38_044_112, type = "all")
summary(model_DPI_27.38_044_112)
ED(model_DPI_27.38_044_112, c(10,50), interval = "delta")
newdata_DPI_27.38_044_112 <- expand.grid(Log_Reciprocal_Serum_Dilution = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_27.38_044_112 <- predict(model_DPI_27.38_044_112, newdata = newdata_DPI_27.38_044_112, interval = "confidence")
newdata_DPI_27.38_044_112$p <- pm_DPI_27.38_044_112[,1]
newdata_DPI_27.38_044_112$pmin <- pm_DPI_27.38_044_112[,2]
newdata_DPI_27.38_044_112$pmax <- pm_DPI_27.38_044_112[,3]
#044-114
DPI_27.38_044_114 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\Combined Raw Curves (Final)\\27-38 DPI IgG Raw Curves (Final).xlsx", sheet = 8, col_names = TRUE)
DPI_27.38_044_114$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_27.38_044_114$Log_Reciprocal_Serum_Dilution)
str(DPI_27.38_044_114)
model_DPI_27.38_044_114 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_27.38_044_114, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_27.38_044_114, type = "all")
summary(model_DPI_27.38_044_114)
ED(model_DPI_27.38_044_114, c(10,50), interval = "delta")
newdata_DPI_27.38_044_114 <- expand.grid(Log_Reciprocal_Serum_Dilution = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_27.38_044_114 <- predict(model_DPI_27.38_044_114, newdata = newdata_DPI_27.38_044_114, interval = "confidence")
newdata_DPI_27.38_044_114$p <- pm_DPI_27.38_044_114[,1]
newdata_DPI_27.38_044_114$pmin <- pm_DPI_27.38_044_114[,2]
newdata_DPI_27.38_044_114$pmax <- pm_DPI_27.38_044_114[,3]
# 044-116
DPI_27.38_044_116 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\Combined Raw Curves (Final)\\27-38 DPI IgG Raw Curves (Final).xlsx", sheet = 9, col_names = TRUE)
DPI_27.38_044_116$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_27.38_044_116$Log_Reciprocal_Serum_Dilution)
str(DPI_27.38_044_116)
model_DPI_27.38_044_116 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_27.38_044_116, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_27.38_044_116, type = "all")
summary(model_DPI_27.38_044_116)
ED(model_DPI_27.38_044_116, c(10,50), interval = "delta")
newdata_DPI_27.38_044_116 <- expand.grid(Log_Reciprocal_Serum_Dilution = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_27.38_044_116 <- predict(model_DPI_27.38_044_116, newdata = newdata_DPI_27.38_044_116, interval = "confidence")
newdata_DPI_27.38_044_116$p <- pm_DPI_27.38_044_116[,1]
newdata_DPI_27.38_044_116$pmin <- pm_DPI_27.38_044_116[,2]
newdata_DPI_27.38_044_116$pmax <- pm_DPI_27.38_044_116[,3]
# 044-117
DPI_27.38_044_117 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\Combined Raw Curves (Final)\\27-38 DPI IgG Raw Curves (Final).xlsx", sheet = 10, col_names = TRUE)
DPI_27.38_044_117$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_27.38_044_117$Log_Reciprocal_Serum_Dilution)
str(DPI_27.38_044_117)
model_DPI_27.38_044_117 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_27.38_044_117, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_27.38_044_117, type = "all")
summary(model_DPI_27.38_044_117)
ED(model_DPI_27.38_044_117, c(10,50), interval = "delta")
newdata_DPI_27.38_044_117 <- expand.grid(Log_Reciprocal_Serum_Dilution = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_27.38_044_117 <- predict(model_DPI_27.38_044_117, newdata = newdata_DPI_27.38_044_117, interval = "confidence")
newdata_DPI_27.38_044_117$p <- pm_DPI_27.38_044_117[,1]
newdata_DPI_27.38_044_117$pmin <- pm_DPI_27.38_044_117[,2]
newdata_DPI_27.38_044_117$pmax <- pm_DPI_27.38_044_117[,3]
# 044-118
DPI_27.38_044_118 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\Combined Raw Curves (Final)\\27-38 DPI IgG Raw Curves (Final).xlsx", sheet = 11, col_names = TRUE)
DPI_27.38_044_118$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_27.38_044_118$Log_Reciprocal_Serum_Dilution)
str(DPI_27.38_044_118)
model_DPI_27.38_044_118 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_27.38_044_118, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_27.38_044_118, type = "all")
summary(model_DPI_27.38_044_118)
ED(model_DPI_27.38_044_118, c(10,50), interval = "delta")
newdata_DPI_27.38_044_118 <- expand.grid(Log_Reciprocal_Serum_Dilution = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_27.38_044_118 <- predict(model_DPI_27.38_044_118, newdata = newdata_DPI_27.38_044_118, interval = "confidence")
newdata_DPI_27.38_044_118$p <- pm_DPI_27.38_044_118[,1]
newdata_DPI_27.38_044_118$pmin <- pm_DPI_27.38_044_118[,2]
newdata_DPI_27.38_044_118$pmax <- pm_DPI_27.38_044_118[,3]
# 044-122
DPI_27.38_044_122 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\Combined Raw Curves (Final)\\27-38 DPI IgG Raw Curves (Final).xlsx", sheet = 12, col_names = TRUE)
DPI_27.38_044_122$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_27.38_044_122$Log_Reciprocal_Serum_Dilution)
str(DPI_27.38_044_122)
model_DPI_27.38_044_122 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_27.38_044_122, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_27.38_044_122, type = "all")
summary(model_DPI_27.38_044_122)
ED(model_DPI_27.38_044_122, c(10,50), interval = "delta")
newdata_DPI_27.38_044_122 <- expand.grid(Log_Reciprocal_Serum_Dilution = exp(seq(log(1.7), log(5.31), length = 100)))
pm_DPI_27.38_044_122 <- predict(model_DPI_27.38_044_122, newdata = newdata_DPI_27.38_044_122, interval = "confidence")
newdata_DPI_27.38_044_122$p <- pm_DPI_27.38_044_122[,1]
newdata_DPI_27.38_044_122$pmin <- pm_DPI_27.38_044_122[,2]
newdata_DPI_27.38_044_122$pmax <- pm_DPI_27.38_044_122[,3]
# 044-126
DPI_27.38_044_126 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\Combined Raw Curves (Final)\\27-38 DPI IgG Raw Curves (Final).xlsx", sheet = 13, col_names = TRUE)
DPI_27.38_044_126$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_27.38_044_126$Log_Reciprocal_Serum_Dilution)
str(DPI_27.38_044_126)
model_DPI_27.38_044_126 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_27.38_044_126, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_27.38_044_126, type = "all")
summary(model_DPI_27.38_044_126)
ED(model_DPI_27.38_044_126, c(10,50), interval = "delta")
newdata_DPI_27.38_044_126 <- expand.grid(Log_Reciprocal_Serum_Dilution = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_27.38_044_126 <- predict(model_DPI_27.38_044_126, newdata = newdata_DPI_27.38_044_126, interval = "confidence")
newdata_DPI_27.38_044_126$p <- pm_DPI_27.38_044_126[,1]
newdata_DPI_27.38_044_126$pmin <- pm_DPI_27.38_044_126[,2]
newdata_DPI_27.38_044_126$pmax <- pm_DPI_27.38_044_126[,3]
# 044-127
DPI_27.38_044_127 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\Combined Raw Curves (Final)\\27-38 DPI IgG Raw Curves (Final).xlsx", sheet = 14, col_names = TRUE)
DPI_27.38_044_127$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_27.38_044_127$Log_Reciprocal_Serum_Dilution)
str(DPI_27.38_044_127)
model_DPI_27.38_044_127 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_27.38_044_127, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_27.38_044_127, type = "all")
summary(model_DPI_27.38_044_127)
ED(model_DPI_27.38_044_127, c(10,50), interval = "delta")
newdata_DPI_27.38_044_127 <- expand.grid(Log_Reciprocal_Serum_Dilution = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_27.38_044_127 <- predict(model_DPI_27.38_044_127, newdata = newdata_DPI_27.38_044_127, interval = "confidence")
newdata_DPI_27.38_044_127$p <- pm_DPI_27.38_044_127[,1]
newdata_DPI_27.38_044_127$pmin <- pm_DPI_27.38_044_127[,2]
newdata_DPI_27.38_044_127$pmax <- pm_DPI_27.38_044_127[,3]
# 044-130
DPI_27.38_044_130 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\Combined Raw Curves (Final)\\27-38 DPI IgG Raw Curves (Final).xlsx", sheet = 15, col_names = TRUE)
DPI_27.38_044_130$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_27.38_044_130$Log_Reciprocal_Serum_Dilution)
str(DPI_27.38_044_130)
model_DPI_27.38_044_130 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_27.38_044_130, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_27.38_044_130, type = "all")
summary(model_DPI_27.38_044_130)
ED(model_DPI_27.38_044_130, c(10,50), interval = "delta")
newdata_DPI_27.38_044_130 <- expand.grid(Log_Reciprocal_Serum_Dilution = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_27.38_044_130 <- predict(model_DPI_27.38_044_130, newdata = newdata_DPI_27.38_044_130, interval = "confidence")
newdata_DPI_27.38_044_130$p <- pm_DPI_27.38_044_130[,1]
newdata_DPI_27.38_044_130$pmin <- pm_DPI_27.38_044_130[,2]
newdata_DPI_27.38_044_130$pmax <- pm_DPI_27.38_044_130[,3]
# 044-131
DPI_27.38_044_131 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\Combined Raw Curves (Final)\\27-38 DPI IgG Raw Curves (Final).xlsx", sheet = 16, col_names = TRUE)
DPI_27.38_044_131$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_27.38_044_131$Log_Reciprocal_Serum_Dilution)
str(DPI_27.38_044_131)
model_DPI_27.38_044_131 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_27.38_044_131, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_27.38_044_131, type = "all")
summary(model_DPI_27.38_044_131)
ED(model_DPI_27.38_044_131, c(10,50), interval = "delta")
newdata_DPI_27.38_044_131 <- expand.grid(Log_Reciprocal_Serum_Dilution = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_27.38_044_131 <- predict(model_DPI_27.38_044_131, newdata = newdata_DPI_27.38_044_131, interval = "confidence")
newdata_DPI_27.38_044_131$p <- pm_DPI_27.38_044_131[,1]
newdata_DPI_27.38_044_131$pmin <- pm_DPI_27.38_044_131[,2]
newdata_DPI_27.38_044_131$pmax <- pm_DPI_27.38_044_131[,3]
# 044-132
DPI_27.38_044_132 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\Combined Raw Curves (Final)\\27-38 DPI IgG Raw Curves (Final).xlsx", sheet = 17, col_names = TRUE)
DPI_27.38_044_132$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_27.38_044_132$Log_Reciprocal_Serum_Dilution)
str(DPI_27.38_044_132)
model_DPI_27.38_044_132 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_27.38_044_132, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_27.38_044_132, type = "all")
summary(model_DPI_27.38_044_132)
ED(model_DPI_27.38_044_132, c(10,50), interval = "delta")
newdata_DPI_27.38_044_132 <- expand.grid(Log_Reciprocal_Serum_Dilution = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_27.38_044_132 <- predict(model_DPI_27.38_044_132, newdata = newdata_DPI_27.38_044_132, interval = "confidence")
newdata_DPI_27.38_044_132$p <- pm_DPI_27.38_044_132[,1]
newdata_DPI_27.38_044_132$pmin <- pm_DPI_27.38_044_132[,2]
newdata_DPI_27.38_044_132$pmax <- pm_DPI_27.38_044_132[,3]
# 044-133
DPI_27.38_044_133 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\Combined Raw Curves (Final)\\27-38 DPI IgG Raw Curves (Final).xlsx", sheet = 18, col_names = TRUE)
DPI_27.38_044_133$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_27.38_044_133$Log_Reciprocal_Serum_Dilution)
str(DPI_27.38_044_133)
model_DPI_27.38_044_133 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_27.38_044_133, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_27.38_044_133, type = "all")
summary(model_DPI_27.38_044_133)
ED(model_DPI_27.38_044_133, c(10,50), interval = "delta")
newdata_DPI_27.38_044_133 <- expand.grid(Log_Reciprocal_Serum_Dilution = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_27.38_044_133 <- predict(model_DPI_27.38_044_133, newdata = newdata_DPI_27.38_044_133, interval = "confidence")
newdata_DPI_27.38_044_133$p <- pm_DPI_27.38_044_133[,1]
newdata_DPI_27.38_044_133$pmin <- pm_DPI_27.38_044_133[,2]
newdata_DPI_27.38_044_133$pmax <- pm_DPI_27.38_044_133[,3]
# Combined IgG WVE Raw Binding Curves figure (27-38 DPI)
rawcurves_DPI_27.38 <- 
  ggplot() + 
  geom_point(data = DPI_27.38_044_101, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "044-101")) + geom_line(data = newdata_DPI_27.38_044_101, aes(x = Log_Reciprocal_Serum_Dilution, y = p, color = "044-101")) +
  geom_point(data = DPI_27.38_044_102, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "044-102")) + geom_line(data = newdata_DPI_27.38_044_102, aes(x = Log_Reciprocal_Serum_Dilution, y = p, color = "044-102")) +
  geom_point(data = DPI_27.38_044_103, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "044-103")) + geom_line(data = newdata_DPI_27.38_044_103, aes(x = Log_Reciprocal_Serum_Dilution, y = p, color = "044-103")) +
  geom_point(data = DPI_27.38_044_104, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "044-104")) + geom_line(data = newdata_DPI_27.38_044_104, aes(x = Log_Reciprocal_Serum_Dilution, y = p, color = "044-104")) +
  geom_point(data = DPI_27.38_044_109, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "044-109")) + geom_line(data = newdata_DPI_27.38_044_109, aes(x = Log_Reciprocal_Serum_Dilution, y = p, color = "044-109")) +
  geom_point(data = DPI_27.38_044_110, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "044-110")) + geom_line(data = newdata_DPI_27.38_044_110, aes(x = Log_Reciprocal_Serum_Dilution, y = p, color = "044-110")) +
  geom_point(data = DPI_27.38_044_112, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "044-112")) + geom_line(data = newdata_DPI_27.38_044_112, aes(x = Log_Reciprocal_Serum_Dilution, y = p, color = "044-112")) +
  geom_point(data = DPI_27.38_044_114, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "044-114")) + geom_line(data = newdata_DPI_27.38_044_114, aes(x = Log_Reciprocal_Serum_Dilution, y = p, color = "044-114")) +
  geom_point(data = DPI_27.38_044_116, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "044-116")) + geom_line(data = newdata_DPI_27.38_044_116, aes(x = Log_Reciprocal_Serum_Dilution, y = p, color = "044-116")) +
  geom_point(data = DPI_27.38_044_117, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "044-117")) + geom_line(data = newdata_DPI_27.38_044_117, aes(x = Log_Reciprocal_Serum_Dilution, y = p, color = "044-117")) +
  geom_point(data = DPI_27.38_044_118, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "044-118")) + geom_line(data = newdata_DPI_27.38_044_118, aes(x = Log_Reciprocal_Serum_Dilution, y = p, color = "044-118")) +
  geom_point(data = DPI_27.38_044_122, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "044-122")) + geom_line(data = newdata_DPI_27.38_044_122, aes(x = Log_Reciprocal_Serum_Dilution, y = p, color = "044-122")) +
  geom_point(data = DPI_27.38_044_126, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "044-126")) + geom_line(data = newdata_DPI_27.38_044_126, aes(x = Log_Reciprocal_Serum_Dilution, y = p, color = "044-126")) +
  geom_point(data = DPI_27.38_044_127, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "044-127")) + geom_line(data = newdata_DPI_27.38_044_127, aes(x = Log_Reciprocal_Serum_Dilution, y = p, color = "044-127")) +
  geom_point(data = DPI_27.38_044_130, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "044-130")) + geom_line(data = newdata_DPI_27.38_044_130, aes(x = Log_Reciprocal_Serum_Dilution, y = p, color = "044-130")) +
  geom_point(data = DPI_27.38_044_131, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "044-131")) + geom_line(data = newdata_DPI_27.38_044_131, aes(x = Log_Reciprocal_Serum_Dilution, y = p, color = "044-131")) +
  geom_point(data = DPI_27.38_044_132, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "044-132")) + geom_line(data = newdata_DPI_27.38_044_132, aes(x = Log_Reciprocal_Serum_Dilution, y = p, color = "044-132")) +
  geom_point(data = DPI_27.38_044_133, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "044-133")) + geom_line(data = newdata_DPI_27.38_044_133, aes(x = Log_Reciprocal_Serum_Dilution, y = p, color = "044-133")) +
  theme_classic() + 
  labs(x = "Log(reciprocal serum dilution)", y = "OD450", title= '27-38 DPI') + 
  xlim(1,6) + 
  scale_y_continuous(expand = c(0,0), limits = c(0,3.5), breaks = c(0, 0.5, 1, 1.5, 2, 2.5, 3, 3.5)) + 
  scale_color_manual(values = c("#7b0000", "#ad0000", "#ff0000", "#ff5d5d", "#ffa3a3", "#ffd2d2", "#000088", "#3434fb", "#6c6cff", "#9c9cff", "#d0d0ff", "#0ef000", "#c1c000", "#fffe04", "#ffd991", "#c7ad7c", "#6ca7a2", "#9c7a99"), breaks = c("044-101", "044-102", "044-103", "044-104", "044-109", "044-110", "044-112", "044-114", "044-116", "044-117", "044-118", "044-122", "044-126", "044-127", "044-130", "044-131", "044-132", "044-133")) + 
  guides(color = guide_legend(title = "Dam ID")) + theme(axis.text = element_text(size = 20), axis.title = element_text(face = 'bold', size = 20), plot.title = element_text(face = "bold", hjust = 0.5, size = 24), aspect.ratio = 1, legend.text = element_text(size= 20), legend.title = element_text(size= 20, face= 'bold'), legend.position = 'right')
rawcurves_DPI_27.38

###
# Combined IgG WVE Raw Binding Curves (52-66 DPI)
###
# 044-101
DPI_52.66_044_101 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\Combined Raw Curves (Final)\\52-66 DPI IgG Raw Curves (Final).xlsx", sheet = 1, col_names = TRUE)
DPI_52.66_044_101$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_52.66_044_101$Log_Reciprocal_Serum_Dilution)
str(DPI_52.66_044_101)
model_DPI_52.66_044_101 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_52.66_044_101, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_52.66_044_101, type = "all")
summary(model_DPI_52.66_044_101)
ED(model_DPI_52.66_044_101, c(10,50), interval = "delta")
newdata_DPI_52.66_044_101 <- expand.grid(Log_Reciprocal_Serum_Dilution = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_52.66_044_101 <- predict(model_DPI_52.66_044_101, newdata = newdata_DPI_52.66_044_101, interval = "confidence")
newdata_DPI_52.66_044_101$p <- pm_DPI_52.66_044_101[,1]
newdata_DPI_52.66_044_101$pmin <- pm_DPI_52.66_044_101[,2]
newdata_DPI_52.66_044_101$pmax <- pm_DPI_52.66_044_101[,3]
# 044-102
DPI_52.66_044_102 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\Combined Raw Curves (Final)\\52-66 DPI IgG Raw Curves (Final).xlsx", sheet = 2, col_names = TRUE)
DPI_52.66_044_102$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_52.66_044_102$Log_Reciprocal_Serum_Dilution)
str(DPI_52.66_044_102)
model_DPI_52.66_044_102 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_52.66_044_102, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_52.66_044_102, type = "all")
summary(model_DPI_52.66_044_102)
ED(model_DPI_52.66_044_102, c(10,50), interval = "delta")
newdata_DPI_52.66_044_102 <- expand.grid(Log_Reciprocal_Serum_Dilution = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_52.66_044_102 <- predict(model_DPI_52.66_044_102, newdata = newdata_DPI_52.66_044_102, interval = "confidence")
newdata_DPI_52.66_044_102$p <- pm_DPI_52.66_044_102[,1]
newdata_DPI_52.66_044_102$pmin <- pm_DPI_52.66_044_102[,2]
newdata_DPI_52.66_044_102$pmax <- pm_DPI_52.66_044_102[,3]
# 044-103
DPI_52.66_044_103 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\Combined Raw Curves (Final)\\52-66 DPI IgG Raw Curves (Final).xlsx", sheet = 3, col_names = TRUE)
DPI_52.66_044_103$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_52.66_044_103$Log_Reciprocal_Serum_Dilution)
str(DPI_52.66_044_103)
model_DPI_52.66_044_103 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_52.66_044_103, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_52.66_044_103, type = "all")
summary(model_DPI_52.66_044_103)
ED(model_DPI_52.66_044_103, c(10,50), interval = "delta")
newdata_DPI_52.66_044_103 <- expand.grid(Log_Reciprocal_Serum_Dilution = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_52.66_044_103 <- predict(model_DPI_52.66_044_103, newdata = newdata_DPI_52.66_044_103, interval = "confidence")
newdata_DPI_52.66_044_103$p <- pm_DPI_52.66_044_103[,1]
newdata_DPI_52.66_044_103$pmin <- pm_DPI_52.66_044_103[,2]
newdata_DPI_52.66_044_103$pmax <- pm_DPI_52.66_044_103[,3]
# 044-104
DPI_52.66_044_104 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\Combined Raw Curves (Final)\\52-66 DPI IgG Raw Curves (Final).xlsx", sheet = 4, col_names = TRUE)
DPI_52.66_044_104$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_52.66_044_104$Log_Reciprocal_Serum_Dilution)
str(DPI_52.66_044_104)
model_DPI_52.66_044_104 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_52.66_044_104, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_52.66_044_104, type = "all")
summary(model_DPI_52.66_044_104)
ED(model_DPI_52.66_044_104, c(10,50), interval = "delta")
newdata_DPI_52.66_044_104 <- expand.grid(Log_Reciprocal_Serum_Dilution = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_52.66_044_104 <- predict(model_DPI_52.66_044_104, newdata = newdata_DPI_52.66_044_104, interval = "confidence")
newdata_DPI_52.66_044_104$p <- pm_DPI_52.66_044_104[,1]
newdata_DPI_52.66_044_104$pmin <- pm_DPI_52.66_044_104[,2]
newdata_DPI_52.66_044_104$pmax <- pm_DPI_52.66_044_104[,3]
# 044-109
DPI_52.66_044_109 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\Combined Raw Curves (Final)\\52-66 DPI IgG Raw Curves (Final).xlsx", sheet = 5, col_names = TRUE)
DPI_52.66_044_109$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_52.66_044_109$Log_Reciprocal_Serum_Dilution)
str(DPI_52.66_044_109)
model_DPI_52.66_044_109 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_52.66_044_109, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_52.66_044_109, type = "all")
summary(model_DPI_52.66_044_109)
ED(model_DPI_52.66_044_109, c(10,50), interval = "delta")
newdata_DPI_52.66_044_109 <- expand.grid(Log_Reciprocal_Serum_Dilution = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_52.66_044_109 <- predict(model_DPI_52.66_044_109, newdata = newdata_DPI_52.66_044_109, interval = "confidence")
newdata_DPI_52.66_044_109$p <- pm_DPI_52.66_044_109[,1]
newdata_DPI_52.66_044_109$pmin <- pm_DPI_52.66_044_109[,2]
newdata_DPI_52.66_044_109$pmax <- pm_DPI_52.66_044_109[,3]
# 044-110
DPI_52.66_044_110 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\Combined Raw Curves (Final)\\52-66 DPI IgG Raw Curves (Final).xlsx", sheet = 6, col_names = TRUE)
DPI_52.66_044_110$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_52.66_044_110$Log_Reciprocal_Serum_Dilution)
str(DPI_52.66_044_110)
model_DPI_52.66_044_110 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_52.66_044_110, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_52.66_044_110, type = "all")
summary(model_DPI_52.66_044_110)
ED(model_DPI_52.66_044_110, c(10,50), interval = "delta")
newdata_DPI_52.66_044_110 <- expand.grid(Log_Reciprocal_Serum_Dilution = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_52.66_044_110 <- predict(model_DPI_52.66_044_110, newdata = newdata_DPI_52.66_044_110, interval = "confidence")
newdata_DPI_52.66_044_110$p <- pm_DPI_52.66_044_110[,1]
newdata_DPI_52.66_044_110$pmin <- pm_DPI_52.66_044_110[,2]
newdata_DPI_52.66_044_110$pmax <- pm_DPI_52.66_044_110[,3]
# 044-112
DPI_52.66_044_112 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\Combined Raw Curves (Final)\\52-66 DPI IgG Raw Curves (Final).xlsx", sheet = 7, col_names = TRUE)
DPI_52.66_044_112$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_52.66_044_112$Log_Reciprocal_Serum_Dilution)
str(DPI_52.66_044_112)
model_DPI_52.66_044_112 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_52.66_044_112, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_52.66_044_112, type = "all")
summary(model_DPI_52.66_044_112)
ED(model_DPI_52.66_044_112, c(10,50), interval = "delta")
newdata_DPI_52.66_044_112 <- expand.grid(Log_Reciprocal_Serum_Dilution = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_52.66_044_112 <- predict(model_DPI_52.66_044_112, newdata = newdata_DPI_52.66_044_112, interval = "confidence")
newdata_DPI_52.66_044_112$p <- pm_DPI_52.66_044_112[,1]
newdata_DPI_52.66_044_112$pmin <- pm_DPI_52.66_044_112[,2]
newdata_DPI_52.66_044_112$pmax <- pm_DPI_52.66_044_112[,3]
#044-114
DPI_52.66_044_114 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\Combined Raw Curves (Final)\\52-66 DPI IgG Raw Curves (Final).xlsx", sheet = 8, col_names = TRUE)
DPI_52.66_044_114$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_52.66_044_114$Log_Reciprocal_Serum_Dilution)
str(DPI_52.66_044_114)
model_DPI_52.66_044_114 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_52.66_044_114, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_52.66_044_114, type = "all")
summary(model_DPI_52.66_044_114)
ED(model_DPI_52.66_044_114, c(10,50), interval = "delta")
newdata_DPI_52.66_044_114 <- expand.grid(Log_Reciprocal_Serum_Dilution = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_52.66_044_114 <- predict(model_DPI_52.66_044_114, newdata = newdata_DPI_52.66_044_114, interval = "confidence")
newdata_DPI_52.66_044_114$p <- pm_DPI_52.66_044_114[,1]
newdata_DPI_52.66_044_114$pmin <- pm_DPI_52.66_044_114[,2]
newdata_DPI_52.66_044_114$pmax <- pm_DPI_52.66_044_114[,3]
# 044-116
DPI_52.66_044_116 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\Combined Raw Curves (Final)\\52-66 DPI IgG Raw Curves (Final).xlsx", sheet = 9, col_names = TRUE)
DPI_52.66_044_116$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_52.66_044_116$Log_Reciprocal_Serum_Dilution)
str(DPI_52.66_044_116)
model_DPI_52.66_044_116 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_52.66_044_116, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_52.66_044_116, type = "all")
summary(model_DPI_52.66_044_116)
ED(model_DPI_52.66_044_116, c(10,50), interval = "delta")
newdata_DPI_52.66_044_116 <- expand.grid(Log_Reciprocal_Serum_Dilution = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_52.66_044_116 <- predict(model_DPI_52.66_044_116, newdata = newdata_DPI_52.66_044_116, interval = "confidence")
newdata_DPI_52.66_044_116$p <- pm_DPI_52.66_044_116[,1]
newdata_DPI_52.66_044_116$pmin <- pm_DPI_52.66_044_116[,2]
newdata_DPI_52.66_044_116$pmax <- pm_DPI_52.66_044_116[,3]
# 044-117 (No 52-66 DPI sample run)
# 044-118
DPI_52.66_044_118 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\Combined Raw Curves (Final)\\52-66 DPI IgG Raw Curves (Final).xlsx", sheet = 11, col_names = TRUE)
DPI_52.66_044_118$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_52.66_044_118$Log_Reciprocal_Serum_Dilution)
str(DPI_52.66_044_118)
model_DPI_52.66_044_118 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_52.66_044_118, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_52.66_044_118, type = "all")
summary(model_DPI_52.66_044_118)
ED(model_DPI_52.66_044_118, c(10,50), interval = "delta")
newdata_DPI_52.66_044_118 <- expand.grid(Log_Reciprocal_Serum_Dilution = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_52.66_044_118 <- predict(model_DPI_52.66_044_118, newdata = newdata_DPI_52.66_044_118, interval = "confidence")
newdata_DPI_52.66_044_118$p <- pm_DPI_52.66_044_118[,1]
newdata_DPI_52.66_044_118$pmin <- pm_DPI_52.66_044_118[,2]
newdata_DPI_52.66_044_118$pmax <- pm_DPI_52.66_044_118[,3]
# 044-122
DPI_52.66_044_122 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\Combined Raw Curves (Final)\\52-66 DPI IgG Raw Curves (Final).xlsx", sheet = 12, col_names = TRUE)
DPI_52.66_044_122$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_52.66_044_122$Log_Reciprocal_Serum_Dilution)
str(DPI_52.66_044_122)
model_DPI_52.66_044_122 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_52.66_044_122, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_52.66_044_122, type = "all")
summary(model_DPI_52.66_044_122)
ED(model_DPI_52.66_044_122, c(10,50), interval = "delta")
newdata_DPI_52.66_044_122 <- expand.grid(Log_Reciprocal_Serum_Dilution = exp(seq(log(1.7), log(5.31), length = 100)))
pm_DPI_52.66_044_122 <- predict(model_DPI_52.66_044_122, newdata = newdata_DPI_52.66_044_122, interval = "confidence")
newdata_DPI_52.66_044_122$p <- pm_DPI_52.66_044_122[,1]
newdata_DPI_52.66_044_122$pmin <- pm_DPI_52.66_044_122[,2]
newdata_DPI_52.66_044_122$pmax <- pm_DPI_52.66_044_122[,3]
# 044-126
DPI_52.66_044_126 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\Combined Raw Curves (Final)\\52-66 DPI IgG Raw Curves (Final).xlsx", sheet = 13, col_names = TRUE)
DPI_52.66_044_126$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_52.66_044_126$Log_Reciprocal_Serum_Dilution)
str(DPI_52.66_044_126)
model_DPI_52.66_044_126 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_52.66_044_126, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_52.66_044_126, type = "all")
summary(model_DPI_52.66_044_126)
ED(model_DPI_52.66_044_126, c(10,50), interval = "delta")
newdata_DPI_52.66_044_126 <- expand.grid(Log_Reciprocal_Serum_Dilution = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_52.66_044_126 <- predict(model_DPI_52.66_044_126, newdata = newdata_DPI_52.66_044_126, interval = "confidence")
newdata_DPI_52.66_044_126$p <- pm_DPI_52.66_044_126[,1]
newdata_DPI_52.66_044_126$pmin <- pm_DPI_52.66_044_126[,2]
newdata_DPI_52.66_044_126$pmax <- pm_DPI_52.66_044_126[,3]
# 044-127
DPI_52.66_044_127 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\Combined Raw Curves (Final)\\52-66 DPI IgG Raw Curves (Final).xlsx", sheet = 14, col_names = TRUE)
DPI_52.66_044_127$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_52.66_044_127$Log_Reciprocal_Serum_Dilution)
str(DPI_52.66_044_127)
model_DPI_52.66_044_127 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_52.66_044_127, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_52.66_044_127, type = "all")
summary(model_DPI_52.66_044_127)
ED(model_DPI_52.66_044_127, c(10,50), interval = "delta")
newdata_DPI_52.66_044_127 <- expand.grid(Log_Reciprocal_Serum_Dilution = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_52.66_044_127 <- predict(model_DPI_52.66_044_127, newdata = newdata_DPI_52.66_044_127, interval = "confidence")
newdata_DPI_52.66_044_127$p <- pm_DPI_52.66_044_127[,1]
newdata_DPI_52.66_044_127$pmin <- pm_DPI_52.66_044_127[,2]
newdata_DPI_52.66_044_127$pmax <- pm_DPI_52.66_044_127[,3]
# 044-130
DPI_52.66_044_130 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\Combined Raw Curves (Final)\\52-66 DPI IgG Raw Curves (Final).xlsx", sheet = 15, col_names = TRUE)
DPI_52.66_044_130$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_52.66_044_130$Log_Reciprocal_Serum_Dilution)
str(DPI_52.66_044_130)
model_DPI_52.66_044_130 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_52.66_044_130, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_52.66_044_130, type = "all")
summary(model_DPI_52.66_044_130)
ED(model_DPI_52.66_044_130, c(10,50), interval = "delta")
newdata_DPI_52.66_044_130 <- expand.grid(Log_Reciprocal_Serum_Dilution = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_52.66_044_130 <- predict(model_DPI_52.66_044_130, newdata = newdata_DPI_52.66_044_130, interval = "confidence")
newdata_DPI_52.66_044_130$p <- pm_DPI_52.66_044_130[,1]
newdata_DPI_52.66_044_130$pmin <- pm_DPI_52.66_044_130[,2]
newdata_DPI_52.66_044_130$pmax <- pm_DPI_52.66_044_130[,3]
# 044-131
DPI_52.66_044_131 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\Combined Raw Curves (Final)\\52-66 DPI IgG Raw Curves (Final).xlsx", sheet = 16, col_names = TRUE)
DPI_52.66_044_131$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_52.66_044_131$Log_Reciprocal_Serum_Dilution)
str(DPI_52.66_044_131)
model_DPI_52.66_044_131 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_52.66_044_131, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_52.66_044_131, type = "all")
summary(model_DPI_52.66_044_131)
ED(model_DPI_52.66_044_131, c(10,50), interval = "delta")
newdata_DPI_52.66_044_131 <- expand.grid(Log_Reciprocal_Serum_Dilution = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_52.66_044_131 <- predict(model_DPI_52.66_044_131, newdata = newdata_DPI_52.66_044_131, interval = "confidence")
newdata_DPI_52.66_044_131$p <- pm_DPI_52.66_044_131[,1]
newdata_DPI_52.66_044_131$pmin <- pm_DPI_52.66_044_131[,2]
newdata_DPI_52.66_044_131$pmax <- pm_DPI_52.66_044_131[,3]
# 044-132
DPI_52.66_044_132 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\Combined Raw Curves (Final)\\52-66 DPI IgG Raw Curves (Final).xlsx", sheet = 17, col_names = TRUE)
DPI_52.66_044_132$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_52.66_044_132$Log_Reciprocal_Serum_Dilution)
str(DPI_52.66_044_132)
model_DPI_52.66_044_132 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_52.66_044_132, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_52.66_044_132, type = "all")
summary(model_DPI_52.66_044_132)
ED(model_DPI_52.66_044_132, c(10,50), interval = "delta")
newdata_DPI_52.66_044_132 <- expand.grid(Log_Reciprocal_Serum_Dilution = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_52.66_044_132 <- predict(model_DPI_52.66_044_132, newdata = newdata_DPI_52.66_044_132, interval = "confidence")
newdata_DPI_52.66_044_132$p <- pm_DPI_52.66_044_132[,1]
newdata_DPI_52.66_044_132$pmin <- pm_DPI_52.66_044_132[,2]
newdata_DPI_52.66_044_132$pmax <- pm_DPI_52.66_044_132[,3]
# 044-133
DPI_52.66_044_133 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\Combined Raw Curves (Final)\\52-66 DPI IgG Raw Curves (Final).xlsx", sheet = 18, col_names = TRUE)
DPI_52.66_044_133$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_52.66_044_133$Log_Reciprocal_Serum_Dilution)
str(DPI_52.66_044_133)
model_DPI_52.66_044_133 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_52.66_044_133, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_52.66_044_133, type = "all")
summary(model_DPI_52.66_044_133)
ED(model_DPI_52.66_044_133, c(10,50), interval = "delta")
newdata_DPI_52.66_044_133 <- expand.grid(Log_Reciprocal_Serum_Dilution = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_52.66_044_133 <- predict(model_DPI_52.66_044_133, newdata = newdata_DPI_52.66_044_133, interval = "confidence")
newdata_DPI_52.66_044_133$p <- pm_DPI_52.66_044_133[,1]
newdata_DPI_52.66_044_133$pmin <- pm_DPI_52.66_044_133[,2]
newdata_DPI_52.66_044_133$pmax <- pm_DPI_52.66_044_133[,3]
# Combined IgG WVE Raw Binding Curves figure (52-66 DPI)
rawcurves_DPI_52.66 <- 
  ggplot() + 
  geom_point(data = DPI_52.66_044_101, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "044-101")) + geom_line(data = newdata_DPI_52.66_044_101, aes(x = Log_Reciprocal_Serum_Dilution, y = p, color = "044-101")) +
  geom_point(data = DPI_52.66_044_102, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "044-102")) + geom_line(data = newdata_DPI_52.66_044_102, aes(x = Log_Reciprocal_Serum_Dilution, y = p, color = "044-102")) +
  geom_point(data = DPI_52.66_044_103, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "044-103")) + geom_line(data = newdata_DPI_52.66_044_103, aes(x = Log_Reciprocal_Serum_Dilution, y = p, color = "044-103")) +
  geom_point(data = DPI_52.66_044_104, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "044-104")) + geom_line(data = newdata_DPI_52.66_044_104, aes(x = Log_Reciprocal_Serum_Dilution, y = p, color = "044-104")) +
  geom_point(data = DPI_52.66_044_109, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "044-109")) + geom_line(data = newdata_DPI_52.66_044_109, aes(x = Log_Reciprocal_Serum_Dilution, y = p, color = "044-109")) +
  geom_point(data = DPI_52.66_044_110, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "044-110")) + geom_line(data = newdata_DPI_52.66_044_110, aes(x = Log_Reciprocal_Serum_Dilution, y = p, color = "044-110")) +
  geom_point(data = DPI_52.66_044_112, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "044-112")) + geom_line(data = newdata_DPI_52.66_044_112, aes(x = Log_Reciprocal_Serum_Dilution, y = p, color = "044-112")) +
  geom_point(data = DPI_52.66_044_114, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "044-114")) + geom_line(data = newdata_DPI_52.66_044_114, aes(x = Log_Reciprocal_Serum_Dilution, y = p, color = "044-114")) +
  geom_point(data = DPI_52.66_044_116, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "044-116")) + geom_line(data = newdata_DPI_52.66_044_116, aes(x = Log_Reciprocal_Serum_Dilution, y = p, color = "044-116")) +
  geom_point(data = DPI_52.66_044_118, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "044-118")) + geom_line(data = newdata_DPI_52.66_044_118, aes(x = Log_Reciprocal_Serum_Dilution, y = p, color = "044-118")) +
  geom_point(data = DPI_52.66_044_122, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "044-122")) + geom_line(data = newdata_DPI_52.66_044_122, aes(x = Log_Reciprocal_Serum_Dilution, y = p, color = "044-122")) +
  geom_point(data = DPI_52.66_044_126, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "044-126")) + geom_line(data = newdata_DPI_52.66_044_126, aes(x = Log_Reciprocal_Serum_Dilution, y = p, color = "044-126")) +
  geom_point(data = DPI_52.66_044_127, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "044-127")) + geom_line(data = newdata_DPI_52.66_044_127, aes(x = Log_Reciprocal_Serum_Dilution, y = p, color = "044-127")) +
  geom_point(data = DPI_52.66_044_130, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "044-130")) + geom_line(data = newdata_DPI_52.66_044_130, aes(x = Log_Reciprocal_Serum_Dilution, y = p, color = "044-130")) +
  geom_point(data = DPI_52.66_044_131, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "044-131")) + geom_line(data = newdata_DPI_52.66_044_131, aes(x = Log_Reciprocal_Serum_Dilution, y = p, color = "044-131")) +
  geom_point(data = DPI_52.66_044_132, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "044-132")) + geom_line(data = newdata_DPI_52.66_044_132, aes(x = Log_Reciprocal_Serum_Dilution, y = p, color = "044-132")) +
  geom_point(data = DPI_52.66_044_133, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "044-133")) + geom_line(data = newdata_DPI_52.66_044_133, aes(x = Log_Reciprocal_Serum_Dilution, y = p, color = "044-133")) +
  theme_classic() + 
  labs(x = "Log(reciprocal serum dilution)", y = "OD450", title= '52-66 DPI') + 
  xlim(1,6) + 
  scale_y_continuous(expand = c(0,0), limits = c(0,3.5), breaks = c(0, 0.5, 1, 1.5, 2, 2.5, 3, 3.5)) + 
  scale_color_manual(values = c("#7b0000", "#ad0000", "#ff0000", "#ff5d5d", "#ffa3a3", "#ffd2d2", "#000088", "#3434fb", "#6c6cff", "#9c9cff", "#d0d0ff", "#0ef000", "#c1c000", "#fffe04", "#ffd991", "#c7ad7c", "#6ca7a2", "#9c7a99"), breaks = c("044-101", "044-102", "044-103", "044-104", "044-109", "044-110", "044-112", "044-114", "044-116", "044-117", "044-118", "044-122", "044-126", "044-127", "044-130", "044-131", "044-132", "044-133")) + 
  guides(color = guide_legend(title = "Dam ID")) + theme(axis.text = element_text(size = 20), axis.title = element_text(face = 'bold', size = 20), plot.title = element_text(face = "bold", hjust = 0.5, size = 24), aspect.ratio = 1, legend.text = element_text(size= 20), legend.title = element_text(size= 20, face= 'bold'), legend.position = 'right')
rawcurves_DPI_52.66

###
# Combined IgG WVE Raw Binding Curves (84-94 DPI)
###
# 044-101
DPI_84.94_044_101 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\Combined Raw Curves (Final)\\84-94 DPI IgG Raw Curves (Final).xlsx", sheet = 1, col_names = TRUE)
DPI_84.94_044_101$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_84.94_044_101$Log_Reciprocal_Serum_Dilution)
str(DPI_84.94_044_101)
model_DPI_84.94_044_101 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_84.94_044_101, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_84.94_044_101, type = "all")
summary(model_DPI_84.94_044_101)
ED(model_DPI_84.94_044_101, c(10,50), interval = "delta")
newdata_DPI_84.94_044_101 <- expand.grid(Log_Reciprocal_Serum_Dilution = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_84.94_044_101 <- predict(model_DPI_84.94_044_101, newdata = newdata_DPI_84.94_044_101, interval = "confidence")
newdata_DPI_84.94_044_101$p <- pm_DPI_84.94_044_101[,1]
newdata_DPI_84.94_044_101$pmin <- pm_DPI_84.94_044_101[,2]
newdata_DPI_84.94_044_101$pmax <- pm_DPI_84.94_044_101[,3]
# 044-102
DPI_84.94_044_102 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\Combined Raw Curves (Final)\\84-94 DPI IgG Raw Curves (Final).xlsx", sheet = 2, col_names = TRUE)
DPI_84.94_044_102$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_84.94_044_102$Log_Reciprocal_Serum_Dilution)
str(DPI_84.94_044_102)
model_DPI_84.94_044_102 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_84.94_044_102, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_84.94_044_102, type = "all")
summary(model_DPI_84.94_044_102)
ED(model_DPI_84.94_044_102, c(10,50), interval = "delta")
newdata_DPI_84.94_044_102 <- expand.grid(Log_Reciprocal_Serum_Dilution = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_84.94_044_102 <- predict(model_DPI_84.94_044_102, newdata = newdata_DPI_84.94_044_102, interval = "confidence")
newdata_DPI_84.94_044_102$p <- pm_DPI_84.94_044_102[,1]
newdata_DPI_84.94_044_102$pmin <- pm_DPI_84.94_044_102[,2]
newdata_DPI_84.94_044_102$pmax <- pm_DPI_84.94_044_102[,3]
# 044-103
DPI_84.94_044_103 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\Combined Raw Curves (Final)\\84-94 DPI IgG Raw Curves (Final).xlsx", sheet = 3, col_names = TRUE)
DPI_84.94_044_103$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_84.94_044_103$Log_Reciprocal_Serum_Dilution)
str(DPI_84.94_044_103)
model_DPI_84.94_044_103 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_84.94_044_103, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_84.94_044_103, type = "all")
summary(model_DPI_84.94_044_103)
ED(model_DPI_84.94_044_103, c(10,50), interval = "delta")
newdata_DPI_84.94_044_103 <- expand.grid(Log_Reciprocal_Serum_Dilution = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_84.94_044_103 <- predict(model_DPI_84.94_044_103, newdata = newdata_DPI_84.94_044_103, interval = "confidence")
newdata_DPI_84.94_044_103$p <- pm_DPI_84.94_044_103[,1]
newdata_DPI_84.94_044_103$pmin <- pm_DPI_84.94_044_103[,2]
newdata_DPI_84.94_044_103$pmax <- pm_DPI_84.94_044_103[,3]
# 044-104
DPI_84.94_044_104 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\Combined Raw Curves (Final)\\84-94 DPI IgG Raw Curves (Final).xlsx", sheet = 4, col_names = TRUE)
DPI_84.94_044_104$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_84.94_044_104$Log_Reciprocal_Serum_Dilution)
str(DPI_84.94_044_104)
model_DPI_84.94_044_104 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_84.94_044_104, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_84.94_044_104, type = "all")
summary(model_DPI_84.94_044_104)
ED(model_DPI_84.94_044_104, c(10,50), interval = "delta")
newdata_DPI_84.94_044_104 <- expand.grid(Log_Reciprocal_Serum_Dilution = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_84.94_044_104 <- predict(model_DPI_84.94_044_104, newdata = newdata_DPI_84.94_044_104, interval = "confidence")
newdata_DPI_84.94_044_104$p <- pm_DPI_84.94_044_104[,1]
newdata_DPI_84.94_044_104$pmin <- pm_DPI_84.94_044_104[,2]
newdata_DPI_84.94_044_104$pmax <- pm_DPI_84.94_044_104[,3]
# 044-109
DPI_84.94_044_109 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\Combined Raw Curves (Final)\\84-94 DPI IgG Raw Curves (Final).xlsx", sheet = 5, col_names = TRUE)
DPI_84.94_044_109$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_84.94_044_109$Log_Reciprocal_Serum_Dilution)
str(DPI_84.94_044_109)
model_DPI_84.94_044_109 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_84.94_044_109, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_84.94_044_109, type = "all")
summary(model_DPI_84.94_044_109)
ED(model_DPI_84.94_044_109, c(10,50), interval = "delta")
newdata_DPI_84.94_044_109 <- expand.grid(Log_Reciprocal_Serum_Dilution = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_84.94_044_109 <- predict(model_DPI_84.94_044_109, newdata = newdata_DPI_84.94_044_109, interval = "confidence")
newdata_DPI_84.94_044_109$p <- pm_DPI_84.94_044_109[,1]
newdata_DPI_84.94_044_109$pmin <- pm_DPI_84.94_044_109[,2]
newdata_DPI_84.94_044_109$pmax <- pm_DPI_84.94_044_109[,3]
# 044-110
DPI_84.94_044_110 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\Combined Raw Curves (Final)\\84-94 DPI IgG Raw Curves (Final).xlsx", sheet = 6, col_names = TRUE)
DPI_84.94_044_110$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_84.94_044_110$Log_Reciprocal_Serum_Dilution)
str(DPI_84.94_044_110)
model_DPI_84.94_044_110 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_84.94_044_110, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_84.94_044_110, type = "all")
summary(model_DPI_84.94_044_110)
ED(model_DPI_84.94_044_110, c(10,50), interval = "delta")
newdata_DPI_84.94_044_110 <- expand.grid(Log_Reciprocal_Serum_Dilution = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_84.94_044_110 <- predict(model_DPI_84.94_044_110, newdata = newdata_DPI_84.94_044_110, interval = "confidence")
newdata_DPI_84.94_044_110$p <- pm_DPI_84.94_044_110[,1]
newdata_DPI_84.94_044_110$pmin <- pm_DPI_84.94_044_110[,2]
newdata_DPI_84.94_044_110$pmax <- pm_DPI_84.94_044_110[,3]
# 044-112
DPI_84.94_044_112 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\Combined Raw Curves (Final)\\84-94 DPI IgG Raw Curves (Final).xlsx", sheet = 7, col_names = TRUE)
DPI_84.94_044_112$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_84.94_044_112$Log_Reciprocal_Serum_Dilution)
str(DPI_84.94_044_112)
model_DPI_84.94_044_112 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_84.94_044_112, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_84.94_044_112, type = "all")
summary(model_DPI_84.94_044_112)
ED(model_DPI_84.94_044_112, c(10,50), interval = "delta")
newdata_DPI_84.94_044_112 <- expand.grid(Log_Reciprocal_Serum_Dilution = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_84.94_044_112 <- predict(model_DPI_84.94_044_112, newdata = newdata_DPI_84.94_044_112, interval = "confidence")
newdata_DPI_84.94_044_112$p <- pm_DPI_84.94_044_112[,1]
newdata_DPI_84.94_044_112$pmin <- pm_DPI_84.94_044_112[,2]
newdata_DPI_84.94_044_112$pmax <- pm_DPI_84.94_044_112[,3]
#044-114
DPI_84.94_044_114 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\Combined Raw Curves (Final)\\84-94 DPI IgG Raw Curves (Final).xlsx", sheet = 8, col_names = TRUE)
DPI_84.94_044_114$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_84.94_044_114$Log_Reciprocal_Serum_Dilution)
str(DPI_84.94_044_114)
model_DPI_84.94_044_114 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_84.94_044_114, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_84.94_044_114, type = "all")
summary(model_DPI_84.94_044_114)
ED(model_DPI_84.94_044_114, c(10,50), interval = "delta")
newdata_DPI_84.94_044_114 <- expand.grid(Log_Reciprocal_Serum_Dilution = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_84.94_044_114 <- predict(model_DPI_84.94_044_114, newdata = newdata_DPI_84.94_044_114, interval = "confidence")
newdata_DPI_84.94_044_114$p <- pm_DPI_84.94_044_114[,1]
newdata_DPI_84.94_044_114$pmin <- pm_DPI_84.94_044_114[,2]
newdata_DPI_84.94_044_114$pmax <- pm_DPI_84.94_044_114[,3]
# 044-116
DPI_84.94_044_116 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\Combined Raw Curves (Final)\\84-94 DPI IgG Raw Curves (Final).xlsx", sheet = 9, col_names = TRUE)
DPI_84.94_044_116$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_84.94_044_116$Log_Reciprocal_Serum_Dilution)
str(DPI_84.94_044_116)
model_DPI_84.94_044_116 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_84.94_044_116, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_84.94_044_116, type = "all")
summary(model_DPI_84.94_044_116)
ED(model_DPI_84.94_044_116, c(10,50), interval = "delta")
newdata_DPI_84.94_044_116 <- expand.grid(Log_Reciprocal_Serum_Dilution = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_84.94_044_116 <- predict(model_DPI_84.94_044_116, newdata = newdata_DPI_84.94_044_116, interval = "confidence")
newdata_DPI_84.94_044_116$p <- pm_DPI_84.94_044_116[,1]
newdata_DPI_84.94_044_116$pmin <- pm_DPI_84.94_044_116[,2]
newdata_DPI_84.94_044_116$pmax <- pm_DPI_84.94_044_116[,3]
# 044-117 (No 84-94 DPI sample run)
# 044-118 (No 84-94 DPI Sample run)
# 044-122
DPI_84.94_044_122 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\Combined Raw Curves (Final)\\84-94 DPI IgG Raw Curves (Final).xlsx", sheet = 12, col_names = TRUE)
DPI_84.94_044_122$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_84.94_044_122$Log_Reciprocal_Serum_Dilution)
str(DPI_84.94_044_122)
model_DPI_84.94_044_122 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_84.94_044_122, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_84.94_044_122, type = "all")
summary(model_DPI_84.94_044_122)
ED(model_DPI_84.94_044_122, c(10,50), interval = "delta")
newdata_DPI_84.94_044_122 <- expand.grid(Log_Reciprocal_Serum_Dilution = exp(seq(log(1.7), log(5.31), length = 100)))
pm_DPI_84.94_044_122 <- predict(model_DPI_84.94_044_122, newdata = newdata_DPI_84.94_044_122, interval = "confidence")
newdata_DPI_84.94_044_122$p <- pm_DPI_84.94_044_122[,1]
newdata_DPI_84.94_044_122$pmin <- pm_DPI_84.94_044_122[,2]
newdata_DPI_84.94_044_122$pmax <- pm_DPI_84.94_044_122[,3]
# 044-126
DPI_84.94_044_126 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\Combined Raw Curves (Final)\\84-94 DPI IgG Raw Curves (Final).xlsx", sheet = 13, col_names = TRUE)
DPI_84.94_044_126$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_84.94_044_126$Log_Reciprocal_Serum_Dilution)
str(DPI_84.94_044_126)
model_DPI_84.94_044_126 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_84.94_044_126, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_84.94_044_126, type = "all")
summary(model_DPI_84.94_044_126)
ED(model_DPI_84.94_044_126, c(10,50), interval = "delta")
newdata_DPI_84.94_044_126 <- expand.grid(Log_Reciprocal_Serum_Dilution = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_84.94_044_126 <- predict(model_DPI_84.94_044_126, newdata = newdata_DPI_84.94_044_126, interval = "confidence")
newdata_DPI_84.94_044_126$p <- pm_DPI_84.94_044_126[,1]
newdata_DPI_84.94_044_126$pmin <- pm_DPI_84.94_044_126[,2]
newdata_DPI_84.94_044_126$pmax <- pm_DPI_84.94_044_126[,3]
# 044-127
DPI_84.94_044_127 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\Combined Raw Curves (Final)\\84-94 DPI IgG Raw Curves (Final).xlsx", sheet = 14, col_names = TRUE)
DPI_84.94_044_127$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_84.94_044_127$Log_Reciprocal_Serum_Dilution)
str(DPI_84.94_044_127)
model_DPI_84.94_044_127 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_84.94_044_127, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_84.94_044_127, type = "all")
summary(model_DPI_84.94_044_127)
ED(model_DPI_84.94_044_127, c(10,50), interval = "delta")
newdata_DPI_84.94_044_127 <- expand.grid(Log_Reciprocal_Serum_Dilution = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_84.94_044_127 <- predict(model_DPI_84.94_044_127, newdata = newdata_DPI_84.94_044_127, interval = "confidence")
newdata_DPI_84.94_044_127$p <- pm_DPI_84.94_044_127[,1]
newdata_DPI_84.94_044_127$pmin <- pm_DPI_84.94_044_127[,2]
newdata_DPI_84.94_044_127$pmax <- pm_DPI_84.94_044_127[,3]
# 044-130
DPI_84.94_044_130 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\Combined Raw Curves (Final)\\84-94 DPI IgG Raw Curves (Final).xlsx", sheet = 15, col_names = TRUE)
DPI_84.94_044_130$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_84.94_044_130$Log_Reciprocal_Serum_Dilution)
str(DPI_84.94_044_130)
model_DPI_84.94_044_130 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_84.94_044_130, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_84.94_044_130, type = "all")
summary(model_DPI_84.94_044_130)
ED(model_DPI_84.94_044_130, c(10,50), interval = "delta")
newdata_DPI_84.94_044_130 <- expand.grid(Log_Reciprocal_Serum_Dilution = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_84.94_044_130 <- predict(model_DPI_84.94_044_130, newdata = newdata_DPI_84.94_044_130, interval = "confidence")
newdata_DPI_84.94_044_130$p <- pm_DPI_84.94_044_130[,1]
newdata_DPI_84.94_044_130$pmin <- pm_DPI_84.94_044_130[,2]
newdata_DPI_84.94_044_130$pmax <- pm_DPI_84.94_044_130[,3]
# 044-131
DPI_84.94_044_131 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\Combined Raw Curves (Final)\\84-94 DPI IgG Raw Curves (Final).xlsx", sheet = 16, col_names = TRUE)
DPI_84.94_044_131$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_84.94_044_131$Log_Reciprocal_Serum_Dilution)
str(DPI_84.94_044_131)
model_DPI_84.94_044_131 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_84.94_044_131, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_84.94_044_131, type = "all")
summary(model_DPI_84.94_044_131)
ED(model_DPI_84.94_044_131, c(10,50), interval = "delta")
newdata_DPI_84.94_044_131 <- expand.grid(Log_Reciprocal_Serum_Dilution = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_84.94_044_131 <- predict(model_DPI_84.94_044_131, newdata = newdata_DPI_84.94_044_131, interval = "confidence")
newdata_DPI_84.94_044_131$p <- pm_DPI_84.94_044_131[,1]
newdata_DPI_84.94_044_131$pmin <- pm_DPI_84.94_044_131[,2]
newdata_DPI_84.94_044_131$pmax <- pm_DPI_84.94_044_131[,3]
# 044-132
DPI_84.94_044_132 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\Combined Raw Curves (Final)\\84-94 DPI IgG Raw Curves (Final).xlsx", sheet = 17, col_names = TRUE)
DPI_84.94_044_132$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_84.94_044_132$Log_Reciprocal_Serum_Dilution)
str(DPI_84.94_044_132)
model_DPI_84.94_044_132 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_84.94_044_132, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_84.94_044_132, type = "all")
summary(model_DPI_84.94_044_132)
ED(model_DPI_84.94_044_132, c(10,50), interval = "delta")
newdata_DPI_84.94_044_132 <- expand.grid(Log_Reciprocal_Serum_Dilution = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_84.94_044_132 <- predict(model_DPI_84.94_044_132, newdata = newdata_DPI_84.94_044_132, interval = "confidence")
newdata_DPI_84.94_044_132$p <- pm_DPI_84.94_044_132[,1]
newdata_DPI_84.94_044_132$pmin <- pm_DPI_84.94_044_132[,2]
newdata_DPI_84.94_044_132$pmax <- pm_DPI_84.94_044_132[,3]
# 044-133
DPI_84.94_044_133 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\Combined Raw Curves (Final)\\84-94 DPI IgG Raw Curves (Final).xlsx", sheet = 18, col_names = TRUE)
DPI_84.94_044_133$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_84.94_044_133$Log_Reciprocal_Serum_Dilution)
str(DPI_84.94_044_133)
model_DPI_84.94_044_133 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_84.94_044_133, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_84.94_044_133, type = "all")
summary(model_DPI_84.94_044_133)
ED(model_DPI_84.94_044_133, c(10,50), interval = "delta")
newdata_DPI_84.94_044_133 <- expand.grid(Log_Reciprocal_Serum_Dilution = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_84.94_044_133 <- predict(model_DPI_84.94_044_133, newdata = newdata_DPI_84.94_044_133, interval = "confidence")
newdata_DPI_84.94_044_133$p <- pm_DPI_84.94_044_133[,1]
newdata_DPI_84.94_044_133$pmin <- pm_DPI_84.94_044_133[,2]
newdata_DPI_84.94_044_133$pmax <- pm_DPI_84.94_044_133[,3]
# Combined IgG WVE Raw Binding Curves figure (84-94 DPI)
rawcurves_DPI_84.94 <- 
  ggplot() + 
  geom_point(data = DPI_84.94_044_101, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "044-101")) + geom_line(data = newdata_DPI_84.94_044_101, aes(x = Log_Reciprocal_Serum_Dilution, y = p, color = "044-101")) +
  geom_point(data = DPI_84.94_044_102, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "044-102")) + geom_line(data = newdata_DPI_84.94_044_102, aes(x = Log_Reciprocal_Serum_Dilution, y = p, color = "044-102")) +
  geom_point(data = DPI_84.94_044_103, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "044-103")) + geom_line(data = newdata_DPI_84.94_044_103, aes(x = Log_Reciprocal_Serum_Dilution, y = p, color = "044-103")) +
  geom_point(data = DPI_84.94_044_104, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "044-104")) + geom_line(data = newdata_DPI_84.94_044_104, aes(x = Log_Reciprocal_Serum_Dilution, y = p, color = "044-104")) +
  geom_point(data = DPI_84.94_044_109, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "044-109")) + geom_line(data = newdata_DPI_84.94_044_109, aes(x = Log_Reciprocal_Serum_Dilution, y = p, color = "044-109")) +
  geom_point(data = DPI_84.94_044_110, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "044-110")) + geom_line(data = newdata_DPI_84.94_044_110, aes(x = Log_Reciprocal_Serum_Dilution, y = p, color = "044-110")) +
  geom_point(data = DPI_84.94_044_112, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "044-112")) + geom_line(data = newdata_DPI_84.94_044_112, aes(x = Log_Reciprocal_Serum_Dilution, y = p, color = "044-112")) +
  geom_point(data = DPI_84.94_044_114, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "044-114")) + geom_line(data = newdata_DPI_84.94_044_114, aes(x = Log_Reciprocal_Serum_Dilution, y = p, color = "044-114")) +
  geom_point(data = DPI_84.94_044_116, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "044-116")) + geom_line(data = newdata_DPI_84.94_044_116, aes(x = Log_Reciprocal_Serum_Dilution, y = p, color = "044-116")) +
  geom_point(data = DPI_84.94_044_122, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "044-122")) + geom_line(data = newdata_DPI_84.94_044_122, aes(x = Log_Reciprocal_Serum_Dilution, y = p, color = "044-122")) +
  geom_point(data = DPI_84.94_044_126, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "044-126")) + geom_line(data = newdata_DPI_84.94_044_126, aes(x = Log_Reciprocal_Serum_Dilution, y = p, color = "044-126")) +
  geom_point(data = DPI_84.94_044_127, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "044-127")) + geom_line(data = newdata_DPI_84.94_044_127, aes(x = Log_Reciprocal_Serum_Dilution, y = p, color = "044-127")) +
  geom_point(data = DPI_84.94_044_130, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "044-130")) + geom_line(data = newdata_DPI_84.94_044_130, aes(x = Log_Reciprocal_Serum_Dilution, y = p, color = "044-130")) +
  geom_point(data = DPI_84.94_044_131, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "044-131")) + geom_line(data = newdata_DPI_84.94_044_131, aes(x = Log_Reciprocal_Serum_Dilution, y = p, color = "044-131")) +
  geom_point(data = DPI_84.94_044_132, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "044-132")) + geom_line(data = newdata_DPI_84.94_044_132, aes(x = Log_Reciprocal_Serum_Dilution, y = p, color = "044-132")) +
  geom_point(data = DPI_84.94_044_133, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "044-133")) + geom_line(data = newdata_DPI_84.94_044_133, aes(x = Log_Reciprocal_Serum_Dilution, y = p, color = "044-133")) +
  theme_classic() + 
  labs(x = "Log(reciprocal serum dilution)", y = "OD450", title= '84-94 DPI') + 
  xlim(1,6) + 
  scale_y_continuous(expand = c(0,0), limits = c(0,3.5), breaks = c(0, 0.5, 1, 1.5, 2, 2.5, 3, 3.5)) + 
  scale_color_manual(values = c("#7b0000", "#ad0000", "#ff0000", "#ff5d5d", "#ffa3a3", "#ffd2d2", "#000088", "#3434fb", "#6c6cff", "#9c9cff", "#d0d0ff", "#0ef000", "#c1c000", "#fffe04", "#ffd991", "#c7ad7c", "#6ca7a2", "#9c7a99"), breaks = c("044-101", "044-102", "044-103", "044-104", "044-109", "044-110", "044-112", "044-114", "044-116", "044-117", "044-118", "044-122", "044-126", "044-127", "044-130", "044-131", "044-132", "044-133")) + 
  guides(color = guide_legend(title = "Dam ID")) + theme(axis.text = element_text(size = 20), axis.title = element_text(face = 'bold', size = 20), plot.title = element_text(face = "bold", hjust = 0.5, size = 24), aspect.ratio = 1, legend.text = element_text(size= 20), legend.title = element_text(size= 20, face= 'bold'), legend.position = 'right')
rawcurves_DPI_84.94

###
# Combined IgG WVE Raw Binding Curves (98-117 DPI)
###
# 044-101
DPI_98.117_044_101 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\Combined Raw Curves (Final)\\98-117 DPI IgG Raw Curves (Final).xlsx", sheet = 1, col_names = TRUE)
DPI_98.117_044_101$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_98.117_044_101$Log_Reciprocal_Serum_Dilution)
str(DPI_98.117_044_101)
model_DPI_98.117_044_101 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_98.117_044_101, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_98.117_044_101, type = "all")
summary(model_DPI_98.117_044_101)
ED(model_DPI_98.117_044_101, c(10,50), interval = "delta")
newdata_DPI_98.117_044_101 <- expand.grid(Log_Reciprocal_Serum_Dilution = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_98.117_044_101 <- predict(model_DPI_98.117_044_101, newdata = newdata_DPI_98.117_044_101, interval = "confidence")
newdata_DPI_98.117_044_101$p <- pm_DPI_98.117_044_101[,1]
newdata_DPI_98.117_044_101$pmin <- pm_DPI_98.117_044_101[,2]
newdata_DPI_98.117_044_101$pmax <- pm_DPI_98.117_044_101[,3]
# 044-102
DPI_98.117_044_102 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\Combined Raw Curves (Final)\\98-117 DPI IgG Raw Curves (Final).xlsx", sheet = 2, col_names = TRUE)
DPI_98.117_044_102$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_98.117_044_102$Log_Reciprocal_Serum_Dilution)
str(DPI_98.117_044_102)
model_DPI_98.117_044_102 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_98.117_044_102, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_98.117_044_102, type = "all")
summary(model_DPI_98.117_044_102)
ED(model_DPI_98.117_044_102, c(10,50), interval = "delta")
newdata_DPI_98.117_044_102 <- expand.grid(Log_Reciprocal_Serum_Dilution = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_98.117_044_102 <- predict(model_DPI_98.117_044_102, newdata = newdata_DPI_98.117_044_102, interval = "confidence")
newdata_DPI_98.117_044_102$p <- pm_DPI_98.117_044_102[,1]
newdata_DPI_98.117_044_102$pmin <- pm_DPI_98.117_044_102[,2]
newdata_DPI_98.117_044_102$pmax <- pm_DPI_98.117_044_102[,3]
# 044-103
DPI_98.117_044_103 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\Combined Raw Curves (Final)\\98-117 DPI IgG Raw Curves (Final).xlsx", sheet = 3, col_names = TRUE)
DPI_98.117_044_103$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_98.117_044_103$Log_Reciprocal_Serum_Dilution)
str(DPI_98.117_044_103)
model_DPI_98.117_044_103 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_98.117_044_103, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_98.117_044_103, type = "all")
summary(model_DPI_98.117_044_103)
ED(model_DPI_98.117_044_103, c(10,50), interval = "delta")
newdata_DPI_98.117_044_103 <- expand.grid(Log_Reciprocal_Serum_Dilution = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_98.117_044_103 <- predict(model_DPI_98.117_044_103, newdata = newdata_DPI_98.117_044_103, interval = "confidence")
newdata_DPI_98.117_044_103$p <- pm_DPI_98.117_044_103[,1]
newdata_DPI_98.117_044_103$pmin <- pm_DPI_98.117_044_103[,2]
newdata_DPI_98.117_044_103$pmax <- pm_DPI_98.117_044_103[,3]
# 044-104
DPI_98.117_044_104 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\Combined Raw Curves (Final)\\98-117 DPI IgG Raw Curves (Final).xlsx", sheet = 4, col_names = TRUE)
DPI_98.117_044_104$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_98.117_044_104$Log_Reciprocal_Serum_Dilution)
str(DPI_98.117_044_104)
model_DPI_98.117_044_104 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_98.117_044_104, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_98.117_044_104, type = "all")
summary(model_DPI_98.117_044_104)
ED(model_DPI_98.117_044_104, c(10,50), interval = "delta")
newdata_DPI_98.117_044_104 <- expand.grid(Log_Reciprocal_Serum_Dilution = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_98.117_044_104 <- predict(model_DPI_98.117_044_104, newdata = newdata_DPI_98.117_044_104, interval = "confidence")
newdata_DPI_98.117_044_104$p <- pm_DPI_98.117_044_104[,1]
newdata_DPI_98.117_044_104$pmin <- pm_DPI_98.117_044_104[,2]
newdata_DPI_98.117_044_104$pmax <- pm_DPI_98.117_044_104[,3]
# 044-109
DPI_98.117_044_109 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\Combined Raw Curves (Final)\\98-117 DPI IgG Raw Curves (Final).xlsx", sheet = 5, col_names = TRUE)
DPI_98.117_044_109$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_98.117_044_109$Log_Reciprocal_Serum_Dilution)
str(DPI_98.117_044_109)
model_DPI_98.117_044_109 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_98.117_044_109, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_98.117_044_109, type = "all")
summary(model_DPI_98.117_044_109)
ED(model_DPI_98.117_044_109, c(10,50), interval = "delta")
newdata_DPI_98.117_044_109 <- expand.grid(Log_Reciprocal_Serum_Dilution = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_98.117_044_109 <- predict(model_DPI_98.117_044_109, newdata = newdata_DPI_98.117_044_109, interval = "confidence")
newdata_DPI_98.117_044_109$p <- pm_DPI_98.117_044_109[,1]
newdata_DPI_98.117_044_109$pmin <- pm_DPI_98.117_044_109[,2]
newdata_DPI_98.117_044_109$pmax <- pm_DPI_98.117_044_109[,3]
# 044-110
DPI_98.117_044_110 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\Combined Raw Curves (Final)\\98-117 DPI IgG Raw Curves (Final).xlsx", sheet = 6, col_names = TRUE)
DPI_98.117_044_110$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_98.117_044_110$Log_Reciprocal_Serum_Dilution)
str(DPI_98.117_044_110)
model_DPI_98.117_044_110 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_98.117_044_110, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_98.117_044_110, type = "all")
summary(model_DPI_98.117_044_110)
ED(model_DPI_98.117_044_110, c(10,50), interval = "delta")
newdata_DPI_98.117_044_110 <- expand.grid(Log_Reciprocal_Serum_Dilution = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_98.117_044_110 <- predict(model_DPI_98.117_044_110, newdata = newdata_DPI_98.117_044_110, interval = "confidence")
newdata_DPI_98.117_044_110$p <- pm_DPI_98.117_044_110[,1]
newdata_DPI_98.117_044_110$pmin <- pm_DPI_98.117_044_110[,2]
newdata_DPI_98.117_044_110$pmax <- pm_DPI_98.117_044_110[,3]
# 044-112
DPI_98.117_044_112 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\Combined Raw Curves (Final)\\98-117 DPI IgG Raw Curves (Final).xlsx", sheet = 7, col_names = TRUE)
DPI_98.117_044_112$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_98.117_044_112$Log_Reciprocal_Serum_Dilution)
str(DPI_98.117_044_112)
model_DPI_98.117_044_112 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_98.117_044_112, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_98.117_044_112, type = "all")
summary(model_DPI_98.117_044_112)
ED(model_DPI_98.117_044_112, c(10,50), interval = "delta")
newdata_DPI_98.117_044_112 <- expand.grid(Log_Reciprocal_Serum_Dilution = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_98.117_044_112 <- predict(model_DPI_98.117_044_112, newdata = newdata_DPI_98.117_044_112, interval = "confidence")
newdata_DPI_98.117_044_112$p <- pm_DPI_98.117_044_112[,1]
newdata_DPI_98.117_044_112$pmin <- pm_DPI_98.117_044_112[,2]
newdata_DPI_98.117_044_112$pmax <- pm_DPI_98.117_044_112[,3]
#044-114
DPI_98.117_044_114 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\Combined Raw Curves (Final)\\98-117 DPI IgG Raw Curves (Final).xlsx", sheet = 8, col_names = TRUE)
DPI_98.117_044_114$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_98.117_044_114$Log_Reciprocal_Serum_Dilution)
str(DPI_98.117_044_114)
model_DPI_98.117_044_114 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_98.117_044_114, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_98.117_044_114, type = "all")
summary(model_DPI_98.117_044_114)
ED(model_DPI_98.117_044_114, c(10,50), interval = "delta")
newdata_DPI_98.117_044_114 <- expand.grid(Log_Reciprocal_Serum_Dilution = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_98.117_044_114 <- predict(model_DPI_98.117_044_114, newdata = newdata_DPI_98.117_044_114, interval = "confidence")
newdata_DPI_98.117_044_114$p <- pm_DPI_98.117_044_114[,1]
newdata_DPI_98.117_044_114$pmin <- pm_DPI_98.117_044_114[,2]
newdata_DPI_98.117_044_114$pmax <- pm_DPI_98.117_044_114[,3]
# 044-116 (No 98-117 DPI sample run)
# 044-117 (No 98-117 DPI sample run)
# 044-118 
DPI_98.117_044_118 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\Combined Raw Curves (Final)\\98-117 DPI IgG Raw Curves (Final).xlsx", sheet = 11, col_names = TRUE)
DPI_98.117_044_118$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_98.117_044_118$Log_Reciprocal_Serum_Dilution)
str(DPI_98.117_044_118)
model_DPI_98.117_044_118 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_98.117_044_118, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_98.117_044_118, type = "all")
summary(model_DPI_98.117_044_118)
ED(model_DPI_98.117_044_118, c(10,50), interval = "delta")
newdata_DPI_98.117_044_118 <- expand.grid(Log_Reciprocal_Serum_Dilution = exp(seq(log(1.7), log(5.31), length = 100)))
pm_DPI_98.117_044_118 <- predict(model_DPI_98.117_044_118, newdata = newdata_DPI_98.117_044_118, interval = "confidence")
newdata_DPI_98.117_044_118$p <- pm_DPI_98.117_044_118[,1]
newdata_DPI_98.117_044_118$pmin <- pm_DPI_98.117_044_118[,2]
newdata_DPI_98.117_044_118$pmax <- pm_DPI_98.117_044_118[,3]
# 044-122
DPI_98.117_044_122 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\Combined Raw Curves (Final)\\98-117 DPI IgG Raw Curves (Final).xlsx", sheet = 12, col_names = TRUE)
DPI_98.117_044_122$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_98.117_044_122$Log_Reciprocal_Serum_Dilution)
str(DPI_98.117_044_122)
model_DPI_98.117_044_122 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_98.117_044_122, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_98.117_044_122, type = "all")
summary(model_DPI_98.117_044_122)
ED(model_DPI_98.117_044_122, c(10,50), interval = "delta")
newdata_DPI_98.117_044_122 <- expand.grid(Log_Reciprocal_Serum_Dilution = exp(seq(log(1.7), log(5.31), length = 100)))
pm_DPI_98.117_044_122 <- predict(model_DPI_98.117_044_122, newdata = newdata_DPI_98.117_044_122, interval = "confidence")
newdata_DPI_98.117_044_122$p <- pm_DPI_98.117_044_122[,1]
newdata_DPI_98.117_044_122$pmin <- pm_DPI_98.117_044_122[,2]
newdata_DPI_98.117_044_122$pmax <- pm_DPI_98.117_044_122[,3]
# 044-126
DPI_98.117_044_126 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\Combined Raw Curves (Final)\\98-117 DPI IgG Raw Curves (Final).xlsx", sheet = 13, col_names = TRUE)
DPI_98.117_044_126$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_98.117_044_126$Log_Reciprocal_Serum_Dilution)
str(DPI_98.117_044_126)
model_DPI_98.117_044_126 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_98.117_044_126, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_98.117_044_126, type = "all")
summary(model_DPI_98.117_044_126)
ED(model_DPI_98.117_044_126, c(10,50), interval = "delta")
newdata_DPI_98.117_044_126 <- expand.grid(Log_Reciprocal_Serum_Dilution = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_98.117_044_126 <- predict(model_DPI_98.117_044_126, newdata = newdata_DPI_98.117_044_126, interval = "confidence")
newdata_DPI_98.117_044_126$p <- pm_DPI_98.117_044_126[,1]
newdata_DPI_98.117_044_126$pmin <- pm_DPI_98.117_044_126[,2]
newdata_DPI_98.117_044_126$pmax <- pm_DPI_98.117_044_126[,3]
# 044-127
DPI_98.117_044_127 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\Combined Raw Curves (Final)\\98-117 DPI IgG Raw Curves (Final).xlsx", sheet = 14, col_names = TRUE)
DPI_98.117_044_127$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_98.117_044_127$Log_Reciprocal_Serum_Dilution)
str(DPI_98.117_044_127)
model_DPI_98.117_044_127 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_98.117_044_127, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_98.117_044_127, type = "all")
summary(model_DPI_98.117_044_127)
ED(model_DPI_98.117_044_127, c(10,50), interval = "delta")
newdata_DPI_98.117_044_127 <- expand.grid(Log_Reciprocal_Serum_Dilution = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_98.117_044_127 <- predict(model_DPI_98.117_044_127, newdata = newdata_DPI_98.117_044_127, interval = "confidence")
newdata_DPI_98.117_044_127$p <- pm_DPI_98.117_044_127[,1]
newdata_DPI_98.117_044_127$pmin <- pm_DPI_98.117_044_127[,2]
newdata_DPI_98.117_044_127$pmax <- pm_DPI_98.117_044_127[,3]
# 044-130
DPI_98.117_044_130 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\Combined Raw Curves (Final)\\98-117 DPI IgG Raw Curves (Final).xlsx", sheet = 15, col_names = TRUE)
DPI_98.117_044_130$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_98.117_044_130$Log_Reciprocal_Serum_Dilution)
str(DPI_98.117_044_130)
model_DPI_98.117_044_130 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_98.117_044_130, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_98.117_044_130, type = "all")
summary(model_DPI_98.117_044_130)
ED(model_DPI_98.117_044_130, c(10,50), interval = "delta")
newdata_DPI_98.117_044_130 <- expand.grid(Log_Reciprocal_Serum_Dilution = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_98.117_044_130 <- predict(model_DPI_98.117_044_130, newdata = newdata_DPI_98.117_044_130, interval = "confidence")
newdata_DPI_98.117_044_130$p <- pm_DPI_98.117_044_130[,1]
newdata_DPI_98.117_044_130$pmin <- pm_DPI_98.117_044_130[,2]
newdata_DPI_98.117_044_130$pmax <- pm_DPI_98.117_044_130[,3]
# 044-131
DPI_98.117_044_131 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\Combined Raw Curves (Final)\\98-117 DPI IgG Raw Curves (Final).xlsx", sheet = 16, col_names = TRUE)
DPI_98.117_044_131$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_98.117_044_131$Log_Reciprocal_Serum_Dilution)
str(DPI_98.117_044_131)
model_DPI_98.117_044_131 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_98.117_044_131, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_98.117_044_131, type = "all")
summary(model_DPI_98.117_044_131)
ED(model_DPI_98.117_044_131, c(10,50), interval = "delta")
newdata_DPI_98.117_044_131 <- expand.grid(Log_Reciprocal_Serum_Dilution = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_98.117_044_131 <- predict(model_DPI_98.117_044_131, newdata = newdata_DPI_98.117_044_131, interval = "confidence")
newdata_DPI_98.117_044_131$p <- pm_DPI_98.117_044_131[,1]
newdata_DPI_98.117_044_131$pmin <- pm_DPI_98.117_044_131[,2]
newdata_DPI_98.117_044_131$pmax <- pm_DPI_98.117_044_131[,3]
# 044-132
DPI_98.117_044_132 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\Combined Raw Curves (Final)\\98-117 DPI IgG Raw Curves (Final).xlsx", sheet = 17, col_names = TRUE)
DPI_98.117_044_132$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_98.117_044_132$Log_Reciprocal_Serum_Dilution)
str(DPI_98.117_044_132)
model_DPI_98.117_044_132 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_98.117_044_132, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_98.117_044_132, type = "all")
summary(model_DPI_98.117_044_132)
ED(model_DPI_98.117_044_132, c(10,50), interval = "delta")
newdata_DPI_98.117_044_132 <- expand.grid(Log_Reciprocal_Serum_Dilution = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_98.117_044_132 <- predict(model_DPI_98.117_044_132, newdata = newdata_DPI_98.117_044_132, interval = "confidence")
newdata_DPI_98.117_044_132$p <- pm_DPI_98.117_044_132[,1]
newdata_DPI_98.117_044_132$pmin <- pm_DPI_98.117_044_132[,2]
newdata_DPI_98.117_044_132$pmax <- pm_DPI_98.117_044_132[,3]
# 044-133
DPI_98.117_044_133 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\Combined Raw Curves (Final)\\98-117 DPI IgG Raw Curves (Final).xlsx", sheet = 18, col_names = TRUE)
DPI_98.117_044_133$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_98.117_044_133$Log_Reciprocal_Serum_Dilution)
str(DPI_98.117_044_133)
model_DPI_98.117_044_133 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_98.117_044_133, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_98.117_044_133, type = "all")
summary(model_DPI_98.117_044_133)
ED(model_DPI_98.117_044_133, c(10,50), interval = "delta")
newdata_DPI_98.117_044_133 <- expand.grid(Log_Reciprocal_Serum_Dilution = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_98.117_044_133 <- predict(model_DPI_98.117_044_133, newdata = newdata_DPI_98.117_044_133, interval = "confidence")
newdata_DPI_98.117_044_133$p <- pm_DPI_98.117_044_133[,1]
newdata_DPI_98.117_044_133$pmin <- pm_DPI_98.117_044_133[,2]
newdata_DPI_98.117_044_133$pmax <- pm_DPI_98.117_044_133[,3]
# Combined IgG WVE Raw Binding Curves figure (98-117 DPI)
rawcurves_DPI_98.117 <- 
  ggplot() + 
  geom_point(data = DPI_98.117_044_101, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "044-101")) + geom_line(data = newdata_DPI_98.117_044_101, aes(x = Log_Reciprocal_Serum_Dilution, y = p, color = "044-101")) +
  geom_point(data = DPI_98.117_044_102, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "044-102")) + geom_line(data = newdata_DPI_98.117_044_102, aes(x = Log_Reciprocal_Serum_Dilution, y = p, color = "044-102")) +
  geom_point(data = DPI_98.117_044_103, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "044-103")) + geom_line(data = newdata_DPI_98.117_044_103, aes(x = Log_Reciprocal_Serum_Dilution, y = p, color = "044-103")) +
  geom_point(data = DPI_98.117_044_104, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "044-104")) + geom_line(data = newdata_DPI_98.117_044_104, aes(x = Log_Reciprocal_Serum_Dilution, y = p, color = "044-104")) +
  geom_point(data = DPI_98.117_044_109, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "044-109")) + geom_line(data = newdata_DPI_98.117_044_109, aes(x = Log_Reciprocal_Serum_Dilution, y = p, color = "044-109")) +
  geom_point(data = DPI_98.117_044_110, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "044-110")) + geom_line(data = newdata_DPI_98.117_044_110, aes(x = Log_Reciprocal_Serum_Dilution, y = p, color = "044-110")) +
  geom_point(data = DPI_98.117_044_112, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "044-112")) + geom_line(data = newdata_DPI_98.117_044_112, aes(x = Log_Reciprocal_Serum_Dilution, y = p, color = "044-112")) +
  geom_point(data = DPI_98.117_044_114, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "044-114")) + geom_line(data = newdata_DPI_98.117_044_114, aes(x = Log_Reciprocal_Serum_Dilution, y = p, color = "044-114")) +
  geom_point(data = DPI_98.117_044_118, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "044-118")) + geom_line(data = newdata_DPI_98.117_044_118, aes(x = Log_Reciprocal_Serum_Dilution, y = p, color = "044-118")) +
  geom_point(data = DPI_98.117_044_122, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "044-122")) + geom_line(data = newdata_DPI_98.117_044_122, aes(x = Log_Reciprocal_Serum_Dilution, y = p, color = "044-122")) +
  geom_point(data = DPI_98.117_044_126, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "044-126")) + geom_line(data = newdata_DPI_98.117_044_126, aes(x = Log_Reciprocal_Serum_Dilution, y = p, color = "044-126")) +
  geom_point(data = DPI_98.117_044_127, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "044-127")) + geom_line(data = newdata_DPI_98.117_044_127, aes(x = Log_Reciprocal_Serum_Dilution, y = p, color = "044-127")) +
  geom_point(data = DPI_98.117_044_130, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "044-130")) + geom_line(data = newdata_DPI_98.117_044_130, aes(x = Log_Reciprocal_Serum_Dilution, y = p, color = "044-130")) +
  geom_point(data = DPI_98.117_044_131, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "044-131")) + geom_line(data = newdata_DPI_98.117_044_131, aes(x = Log_Reciprocal_Serum_Dilution, y = p, color = "044-131")) +
  geom_point(data = DPI_98.117_044_132, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "044-132")) + geom_line(data = newdata_DPI_98.117_044_132, aes(x = Log_Reciprocal_Serum_Dilution, y = p, color = "044-132")) +
  geom_point(data = DPI_98.117_044_133, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "044-133")) + geom_line(data = newdata_DPI_98.117_044_133, aes(x = Log_Reciprocal_Serum_Dilution, y = p, color = "044-133")) +
  theme_classic() + 
  labs(x = "Log(reciprocal serum dilution)", y = "OD450", title= '98-117 DPI') + 
  xlim(1,6) + 
  scale_y_continuous(expand = c(0,0), limits = c(0,3.5), breaks = c(0, 0.5, 1, 1.5, 2, 2.5, 3, 3.5)) + 
  scale_color_manual(values = c("#7b0000", "#ad0000", "#ff0000", "#ff5d5d", "#ffa3a3", "#ffd2d2", "#000088", "#3434fb", "#6c6cff", "#9c9cff", "#d0d0ff", "#0ef000", "#c1c000", "#fffe04", "#ffd991", "#c7ad7c", "#6ca7a2", "#9c7a99"), breaks = c("044-101", "044-102", "044-103", "044-104", "044-109", "044-110", "044-112", "044-114", "044-116", "044-117", "044-118", "044-122", "044-126", "044-127", "044-130", "044-131", "044-132", "044-133")) + 
  guides(color = guide_legend(title = "Dam ID")) + theme(axis.text = element_text(size = 20), axis.title = element_text(face = 'bold', size = 20), plot.title = element_text(face = "bold", hjust = 0.5, size = 24), aspect.ratio = 1, legend.text = element_text(size= 20), legend.title = element_text(size= 20, face= 'bold'), legend.position = 'right')
rawcurves_DPI_98.117

###
# Combined IgG WVE Raw Binding Curves (121-136 DPI)
###
# 044-101 (No 121-136 DPI sample run)
# 044-102 (No 121-136 DPI sample run)
# 044-103 (No 121-136 DPI sample run)
# 044-104 (No 121-136 DPI sample run)
# 044-109 (No 121-136 DPI sample run)
# 044-110
DPI_121.136_044_110 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\Combined Raw Curves (Final)\\121-136 DPI IgG Raw Curves (Final).xlsx", sheet = 6, col_names = TRUE)
DPI_121.136_044_110$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_121.136_044_110$Log_Reciprocal_Serum_Dilution)
str(DPI_121.136_044_110)
model_DPI_121.136_044_110 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_121.136_044_110, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_121.136_044_110, type = "all")
summary(model_DPI_121.136_044_110)
ED(model_DPI_121.136_044_110, c(10,50), interval = "delta")
newdata_DPI_121.136_044_110 <- expand.grid(Log_Reciprocal_Serum_Dilution = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_121.136_044_110 <- predict(model_DPI_121.136_044_110, newdata = newdata_DPI_121.136_044_110, interval = "confidence")
newdata_DPI_121.136_044_110$p <- pm_DPI_121.136_044_110[,1]
newdata_DPI_121.136_044_110$pmin <- pm_DPI_121.136_044_110[,2]
newdata_DPI_121.136_044_110$pmax <- pm_DPI_121.136_044_110[,3]
# 044-112
DPI_121.136_044_112 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\Combined Raw Curves (Final)\\121-136 DPI IgG Raw Curves (Final).xlsx", sheet = 7, col_names = TRUE)
DPI_121.136_044_112$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_121.136_044_112$Log_Reciprocal_Serum_Dilution)
str(DPI_121.136_044_112)
model_DPI_121.136_044_112 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_121.136_044_112, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_121.136_044_112, type = "all")
summary(model_DPI_121.136_044_112)
ED(model_DPI_121.136_044_112, c(10,50), interval = "delta")
newdata_DPI_121.136_044_112 <- expand.grid(Log_Reciprocal_Serum_Dilution = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_121.136_044_112 <- predict(model_DPI_121.136_044_112, newdata = newdata_DPI_121.136_044_112, interval = "confidence")
newdata_DPI_121.136_044_112$p <- pm_DPI_121.136_044_112[,1]
newdata_DPI_121.136_044_112$pmin <- pm_DPI_121.136_044_112[,2]
newdata_DPI_121.136_044_112$pmax <- pm_DPI_121.136_044_112[,3]
#044-114
DPI_121.136_044_114 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\Combined Raw Curves (Final)\\121-136 DPI IgG Raw Curves (Final).xlsx", sheet = 8, col_names = TRUE)
DPI_121.136_044_114$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_121.136_044_114$Log_Reciprocal_Serum_Dilution)
str(DPI_121.136_044_114)
model_DPI_121.136_044_114 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_121.136_044_114, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_121.136_044_114, type = "all")
summary(model_DPI_121.136_044_114)
ED(model_DPI_121.136_044_114, c(10,50), interval = "delta")
newdata_DPI_121.136_044_114 <- expand.grid(Log_Reciprocal_Serum_Dilution = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_121.136_044_114 <- predict(model_DPI_121.136_044_114, newdata = newdata_DPI_121.136_044_114, interval = "confidence")
newdata_DPI_121.136_044_114$p <- pm_DPI_121.136_044_114[,1]
newdata_DPI_121.136_044_114$pmin <- pm_DPI_121.136_044_114[,2]
newdata_DPI_121.136_044_114$pmax <- pm_DPI_121.136_044_114[,3]
# 044-116 
DPI_121.136_044_116 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\Combined Raw Curves (Final)\\121-136 DPI IgG Raw Curves (Final).xlsx", sheet = 9, col_names = TRUE)
DPI_121.136_044_116$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_121.136_044_116$Log_Reciprocal_Serum_Dilution)
str(DPI_121.136_044_116)
model_DPI_121.136_044_116 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_121.136_044_116, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_121.136_044_116, type = "all")
summary(model_DPI_121.136_044_116)
ED(model_DPI_121.136_044_116, c(10,50), interval = "delta")
newdata_DPI_121.136_044_116 <- expand.grid(Log_Reciprocal_Serum_Dilution = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_121.136_044_116 <- predict(model_DPI_121.136_044_116, newdata = newdata_DPI_121.136_044_116, interval = "confidence")
newdata_DPI_121.136_044_116$p <- pm_DPI_121.136_044_116[,1]
newdata_DPI_121.136_044_116$pmin <- pm_DPI_121.136_044_116[,2]
newdata_DPI_121.136_044_116$pmax <- pm_DPI_121.136_044_116[,3]
# 044-117
DPI_121.136_044_117 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\Combined Raw Curves (Final)\\121-136 DPI IgG Raw Curves (Final).xlsx", sheet = 10, col_names = TRUE)
DPI_121.136_044_117$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_121.136_044_117$Log_Reciprocal_Serum_Dilution)
str(DPI_121.136_044_117)
model_DPI_121.136_044_117 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_121.136_044_117, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_121.136_044_117, type = "all")
summary(model_DPI_121.136_044_117)
ED(model_DPI_121.136_044_117, c(10,50), interval = "delta")
newdata_DPI_121.136_044_117 <- expand.grid(Log_Reciprocal_Serum_Dilution = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_121.136_044_117 <- predict(model_DPI_121.136_044_117, newdata = newdata_DPI_121.136_044_117, interval = "confidence")
newdata_DPI_121.136_044_117$p <- pm_DPI_121.136_044_117[,1]
newdata_DPI_121.136_044_117$pmin <- pm_DPI_121.136_044_117[,2]
newdata_DPI_121.136_044_117$pmax <- pm_DPI_121.136_044_117[,3]
# 044-118 
DPI_121.136_044_118 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\Combined Raw Curves (Final)\\121-136 DPI IgG Raw Curves (Final).xlsx", sheet = 11, col_names = TRUE)
DPI_121.136_044_118$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_121.136_044_118$Log_Reciprocal_Serum_Dilution)
str(DPI_121.136_044_118)
model_DPI_121.136_044_118 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_121.136_044_118, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_121.136_044_118, type = "all")
summary(model_DPI_121.136_044_118)
ED(model_DPI_121.136_044_118, c(10,50), interval = "delta")
newdata_DPI_121.136_044_118 <- expand.grid(Log_Reciprocal_Serum_Dilution = exp(seq(log(1.7), log(5.31), length = 100)))
pm_DPI_121.136_044_118 <- predict(model_DPI_121.136_044_118, newdata = newdata_DPI_121.136_044_118, interval = "confidence")
newdata_DPI_121.136_044_118$p <- pm_DPI_121.136_044_118[,1]
newdata_DPI_121.136_044_118$pmin <- pm_DPI_121.136_044_118[,2]
newdata_DPI_121.136_044_118$pmax <- pm_DPI_121.136_044_118[,3]
# 044-122
DPI_121.136_044_122 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\Combined Raw Curves (Final)\\121-136 DPI IgG Raw Curves (Final).xlsx", sheet = 12, col_names = TRUE)
DPI_121.136_044_122$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_121.136_044_122$Log_Reciprocal_Serum_Dilution)
str(DPI_121.136_044_122)
model_DPI_121.136_044_122 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_121.136_044_122, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_121.136_044_122, type = "all")
summary(model_DPI_121.136_044_122)
ED(model_DPI_121.136_044_122, c(10,50), interval = "delta")
newdata_DPI_121.136_044_122 <- expand.grid(Log_Reciprocal_Serum_Dilution = exp(seq(log(1.7), log(5.31), length = 100)))
pm_DPI_121.136_044_122 <- predict(model_DPI_121.136_044_122, newdata = newdata_DPI_121.136_044_122, interval = "confidence")
newdata_DPI_121.136_044_122$p <- pm_DPI_121.136_044_122[,1]
newdata_DPI_121.136_044_122$pmin <- pm_DPI_121.136_044_122[,2]
newdata_DPI_121.136_044_122$pmax <- pm_DPI_121.136_044_122[,3]
# 044-126 (No 121-136 DPI sample)
# 044-127 (No 121-136 DPI sample)
# 044-130 (NO 121-136 DPI sample)
# 044-131
DPI_121.136_044_131 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\Combined Raw Curves (Final)\\121-136 DPI IgG Raw Curves (Final).xlsx", sheet = 16, col_names = TRUE)
DPI_121.136_044_131$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_121.136_044_131$Log_Reciprocal_Serum_Dilution)
str(DPI_121.136_044_131)
model_DPI_121.136_044_131 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_121.136_044_131, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_121.136_044_131, type = "all")
summary(model_DPI_121.136_044_131)
ED(model_DPI_121.136_044_131, c(10,50), interval = "delta")
newdata_DPI_121.136_044_131 <- expand.grid(Log_Reciprocal_Serum_Dilution = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_121.136_044_131 <- predict(model_DPI_121.136_044_131, newdata = newdata_DPI_121.136_044_131, interval = "confidence")
newdata_DPI_121.136_044_131$p <- pm_DPI_121.136_044_131[,1]
newdata_DPI_121.136_044_131$pmin <- pm_DPI_121.136_044_131[,2]
newdata_DPI_121.136_044_131$pmax <- pm_DPI_121.136_044_131[,3]
# 044-132
DPI_121.136_044_132 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\Combined Raw Curves (Final)\\121-136 DPI IgG Raw Curves (Final).xlsx", sheet = 17, col_names = TRUE)
DPI_121.136_044_132$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_121.136_044_132$Log_Reciprocal_Serum_Dilution)
str(DPI_121.136_044_132)
model_DPI_121.136_044_132 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_121.136_044_132, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_121.136_044_132, type = "all")
summary(model_DPI_121.136_044_132)
ED(model_DPI_121.136_044_132, c(10,50), interval = "delta")
newdata_DPI_121.136_044_132 <- expand.grid(Log_Reciprocal_Serum_Dilution = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_121.136_044_132 <- predict(model_DPI_121.136_044_132, newdata = newdata_DPI_121.136_044_132, interval = "confidence")
newdata_DPI_121.136_044_132$p <- pm_DPI_121.136_044_132[,1]
newdata_DPI_121.136_044_132$pmin <- pm_DPI_121.136_044_132[,2]
newdata_DPI_121.136_044_132$pmax <- pm_DPI_121.136_044_132[,3]
# 044-133 (No 121-136 DPI sample)
# Combined IgG WVE Raw Binding Curves figure (121-136 DPI)
rawcurves_DPI_121.136 <- 
  ggplot() + 
  geom_point(data = DPI_121.136_044_110, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "044-110")) + geom_line(data = newdata_DPI_121.136_044_110, aes(x = Log_Reciprocal_Serum_Dilution, y = p, color = "044-110")) +
  geom_point(data = DPI_121.136_044_112, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "044-112")) + geom_line(data = newdata_DPI_121.136_044_112, aes(x = Log_Reciprocal_Serum_Dilution, y = p, color = "044-112")) +
  geom_point(data = DPI_121.136_044_114, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "044-114")) + geom_line(data = newdata_DPI_121.136_044_114, aes(x = Log_Reciprocal_Serum_Dilution, y = p, color = "044-114")) +
  geom_point(data = DPI_121.136_044_118, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "044-118")) + geom_line(data = newdata_DPI_121.136_044_118, aes(x = Log_Reciprocal_Serum_Dilution, y = p, color = "044-118")) +
  geom_point(data = DPI_121.136_044_122, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "044-122")) + geom_line(data = newdata_DPI_121.136_044_122, aes(x = Log_Reciprocal_Serum_Dilution, y = p, color = "044-122")) +
  geom_point(data = DPI_121.136_044_131, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "044-131")) + geom_line(data = newdata_DPI_121.136_044_131, aes(x = Log_Reciprocal_Serum_Dilution, y = p, color = "044-131")) +
  geom_point(data = DPI_121.136_044_132, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "044-132")) + geom_line(data = newdata_DPI_121.136_044_132, aes(x = Log_Reciprocal_Serum_Dilution, y = p, color = "044-132")) +
  theme_classic() + 
  labs(x = "Log(reciprocal serum dilution)", y = "OD450", title= '121-136 DPI') + 
  xlim(1,6) + 
  scale_y_continuous(expand = c(0,0), limits = c(0,3.5), breaks = c(0, 0.5, 1, 1.5, 2, 2.5, 3, 3.5)) + 
  scale_color_manual(values = c("#7b0000", "#ad0000", "#ff0000", "#ff5d5d", "#ffa3a3", "#ffd2d2", "#000088", "#3434fb", "#6c6cff", "#9c9cff", "#d0d0ff", "#0ef000", "#c1c000", "#fffe04", "#ffd991", "#c7ad7c", "#6ca7a2", "#9c7a99"), breaks = c("044-101", "044-102", "044-103", "044-104", "044-109", "044-110", "044-112", "044-114", "044-116", "044-117", "044-118", "044-122", "044-126", "044-127", "044-130", "044-131", "044-132", "044-133")) + 
  guides(color = guide_legend(title = "Dam ID")) + theme(axis.text = element_text(size = 20), axis.title = element_text(face = 'bold', size = 20), plot.title = element_text(face = "bold", hjust = 0.5, size = 24), aspect.ratio = 1, legend.text = element_text(size= 20), legend.title = element_text(size= 20, face= 'bold'), legend.position = 'right')
rawcurves_DPI_121.136

###
# Combining raw curves into a multipanel figure
###
tiff("Combined IgG WVE Raw Binding Curves (Final).tiff", units = 'in', height = 28, width = 15, res = 300)
ggarrange(rawcurves_preinfection, rawcurves_DPI_2.4, rawcurves_DPI_7.10, rawcurves_DPI_13.17, rawcurves_DPI_18.24, rawcurves_DPI_27.38, rawcurves_DPI_52.66, rawcurves_DPI_84.94, rawcurves_DPI_98.117, rawcurves_DPI_121.136, nrow = 5, ncol = 2, align = 'hv', labels = c('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J'))
dev.off()

