library(tidyverse)
library(drc)
library(readxl)
library(ggplot2)
library(scales)
###
# Using raw OD450 readings to generate a 4-parameter dose-response model and estimate the dilution of serum
# to reduce the maximum OD450 readings by 90% and 50% (EC90 and EC50) for each timepoint tested.
###

#044-131 0 dpi
DPI_0 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\IgG Whole Virion ELISA Raw Data (044-131).xlsx", sheet = 1, col_names = TRUE)
DPI_0$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_0$Log_Reciprocal_Serum_Dilution)
str(DPI_0)
model_DPI_0 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_0, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_0, type = "all")
summary(model_DPI_0)
ED(model_DPI_0, c(10,50), interval = "delta")
newdata_DPI_0 <- expand.grid(DF = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_0 <- predict(model_DPI_0, newdata = newdata_DPI_0, interval = "confidence")
newdata_DPI_0$p <- pm_DPI_0[,1]
newdata_DPI_0$pmin <- pm_DPI_0[,2]
newdata_DPI_0$pmax <- pm_DPI_0[,3]
#044-131 4 dpi
DPI_4 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\IgG Whole Virion ELISA Raw Data (044-131).xlsx", sheet = 2, col_names = TRUE)
DPI_4$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_4$Log_Reciprocal_Serum_Dilution)
str(DPI_4)
model_DPI_4 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_4, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_4, type = "all")
summary(model_DPI_4)
ED(model_DPI_4, c(10,50), interval = "delta")
newdata_DPI_4 <- expand.grid(DF = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_4 <- predict(model_DPI_4, newdata = newdata_DPI_4, interval = "confidence")
newdata_DPI_4$p <- pm_DPI_4[,1]
newdata_DPI_4$pmin <- pm_DPI_4[,2]
newdata_DPI_4$pmax <- pm_DPI_4[,3]
#044-131 7 dpi
DPI_7 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\IgG Whole Virion ELISA Raw Data (044-131).xlsx", sheet = 3, col_names = TRUE)
DPI_7$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_7$Log_Reciprocal_Serum_Dilution)
str(DPI_7)
model_DPI_7 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_7, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_7, type = "all")
summary(model_DPI_7)
ED(model_DPI_7, c(10,50), interval = "delta")
newdata_DPI_7 <- expand.grid(DF = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_7 <- predict(model_DPI_7, newdata = newdata_DPI_7, interval = "confidence")
newdata_DPI_7$p <- pm_DPI_7[,1]
newdata_DPI_7$pmin <- pm_DPI_7[,2]
newdata_DPI_7$pmax <- pm_DPI_7[,3]
#044-131 13 dpi
DPI_13 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\IgG Whole Virion ELISA Raw Data (044-131).xlsx", sheet = 4, col_names = TRUE)
DPI_13$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_13$Log_Reciprocal_Serum_Dilution)
str(DPI_13)
model_DPI_13 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_13, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_13, type = "all")
summary(model_DPI_13)
ED(model_DPI_13, c(10,50), interval = "delta")
newdata_DPI_13 <- expand.grid(DF = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_13 <- predict(model_DPI_13, newdata = newdata_DPI_13, interval = "confidence")
newdata_DPI_13$p <- pm_DPI_13[,1]
newdata_DPI_13$pmin <- pm_DPI_13[,2]
newdata_DPI_13$pmax <- pm_DPI_13[,3]
#044-131 20 dpi
DPI_20 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\IgG Whole Virion ELISA Raw Data (044-131).xlsx", sheet = 5, col_names = TRUE)
DPI_20$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_20$Log_Reciprocal_Serum_Dilution)
str(DPI_20)
model_DPI_20 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_20, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_20, type = "all")
summary(model_DPI_20)
ED(model_DPI_20, c(10,50), interval = "delta")
newdata_DPI_20 <- expand.grid(DF = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_20 <- predict(model_DPI_20, newdata = newdata_DPI_20, interval = "confidence")
newdata_DPI_20$p <- pm_DPI_20[,1]
newdata_DPI_20$pmin <- pm_DPI_20[,2]
newdata_DPI_20$pmax <- pm_DPI_20[,3]
#044-131 31 dpi 
DPI_31 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\IgG Whole Virion ELISA Raw Data (044-131).xlsx", sheet = 6, col_names = TRUE)
DPI_31$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_31$Log_Reciprocal_Serum_Dilution)
str(DPI_31)
model_DPI_31 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_31, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_31, type = "all")
summary(model_DPI_31)
ED(model_DPI_31, c(10,50), interval = "delta")
newdata_DPI_31 <- expand.grid(DF = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_31 <- predict(model_DPI_31, newdata = newdata_DPI_31, interval = "confidence")
newdata_DPI_31$p <- pm_DPI_31[,1]
newdata_DPI_31$pmin <- pm_DPI_31[,2]
newdata_DPI_31$pmax <- pm_DPI_31[,3]
#044-131 55 dpi
DPI_55 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\IgG Whole Virion ELISA Raw Data (044-131).xlsx", sheet = 7, col_names = TRUE)
DPI_55$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_55$Log_Reciprocal_Serum_Dilution)
str(DPI_55)
model_DPI_55 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_55, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_55, type = "all")
summary(model_DPI_55)
ED(model_DPI_55, c(10,50), interval = "delta")
newdata_DPI_55 <- expand.grid(DF = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_55 <- predict(model_DPI_55, newdata = newdata_DPI_55, interval = "confidence")
newdata_DPI_55$p <- pm_DPI_55[,1]
newdata_DPI_55$pmin <- pm_DPI_55[,2]
newdata_DPI_55$pmax <- pm_DPI_55[,3]
#044-131 90 dpi
DPI_90 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\IgG Whole Virion ELISA Raw Data (044-131).xlsx", sheet = 8, col_names = TRUE)
DPI_90$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_90$Log_Reciprocal_Serum_Dilution)
str(DPI_90)
model_DPI_90 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_90, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_90, type = "all")
summary(model_DPI_90)
ED(model_DPI_90, c(10,50), interval = "delta")
newdata_DPI_90 <- expand.grid(DF = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_90 <- predict(model_DPI_90, newdata = newdata_DPI_90, interval = "confidence")
newdata_DPI_90$p <- pm_DPI_90[,1]
newdata_DPI_90$pmin <- pm_DPI_90[,2]
newdata_DPI_90$pmax <- pm_DPI_90[,3]
#044-131 115 dpi
DPI_115 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\IgG Whole Virion ELISA Raw Data (044-131).xlsx", sheet = 9, col_names = TRUE)
DPI_115$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_115$Log_Reciprocal_Serum_Dilution)
str(DPI_115)
model_DPI_115 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_115, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_115, type = "all")
summary(model_DPI_115)
ED(model_DPI_115, c(10,50), interval = "delta")
newdata_DPI_115 <- expand.grid(DF = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_115 <- predict(model_DPI_115, newdata = newdata_DPI_115, interval = "confidence")
newdata_DPI_115$p <- pm_DPI_115[,1]
newdata_DPI_115$pmin <- pm_DPI_115[,2]
newdata_DPI_115$pmax <- pm_DPI_115[,3]
#044-131 136 dpi
DPI_136 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\IgG Whole Virion ELISA Raw Data (044-131).xlsx", sheet = 10, col_names = TRUE)
DPI_136$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_136$Log_Reciprocal_Serum_Dilution)
str(DPI_136)
model_DPI_136 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_136, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_136, type = "all")
summary(model_DPI_136)
ED(model_DPI_136, c(10,50), interval = "delta")
newdata_DPI_136 <- expand.grid(DF = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_136 <- predict(model_DPI_136, newdata = newdata_DPI_136, interval = "confidence")
newdata_DPI_136$p <- pm_DPI_136[,1]
newdata_DPI_136$pmin <- pm_DPI_136[,2]
newdata_DPI_136$pmax <- pm_DPI_136[,3]
####### 
#044-131 raw IgG Whole Virion ELISA binding curves
######
rawcurves_044_131 <- 
  ggplot() + 
  geom_point(data = DPI_0, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "0 DPI")) + geom_line(data = newdata_DPI_0, aes(x = DF, y = p, color = "0 DPI")) +
  geom_point(data = DPI_4, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "4 DPI")) + geom_line(data = newdata_DPI_4, aes(x = DF, y = p, color = "4 DPI")) +
  geom_point(data = DPI_7, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "7 DPI")) + geom_line(data = newdata_DPI_7, aes(x = DF, y = p, color = "7 DPI")) +
  geom_point(data = DPI_13, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "13 DPI")) + geom_line(data = newdata_DPI_13, aes(x = DF, y = p, color = "13 DPI")) +
  geom_point(data = DPI_20, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "20 DPI")) + geom_line(data = newdata_DPI_20, aes(x = DF, y = p, color = "20 DPI")) +
  geom_point(data = DPI_31, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "31 DPI")) + geom_line(data = newdata_DPI_31, aes(x = DF, y = p, color = "31 DPI")) +
  geom_point(data = DPI_55, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "55 DPI")) + geom_line(data = newdata_DPI_55, aes(x = DF, y = p, color = "55 DPI")) +
  geom_point(data = DPI_90, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "90 DPI")) + geom_line(data = newdata_DPI_90, aes(x = DF, y = p, color = "90 DPI")) +
  geom_point(data = DPI_115, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "115 DPI")) + geom_line(data = newdata_DPI_115, aes(x = DF, y = p, color = "115 DPI")) +
  geom_point(data = DPI_136, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "136 DPI")) + geom_line(data = newdata_DPI_136, aes(x = DF, y = p, color = "136 DPI"))
rawcurves_044_131 + theme_classic() + labs(x = "Log(reciprocal serum dilution)", y = "OD450 Reading", title = "IgG WVE Raw Binding Curves (044-131)") + xlim(1,6) + scale_color_discrete(limits = c("0 DPI", "4 DPI", "7 DPI", "13 DPI", "20 DPI", "31 DPI", "55 DPI", "90 DPI", "115 DPI", "136 DPI")) + guides(color = guide_legend(title = "Days Post Infection"))
