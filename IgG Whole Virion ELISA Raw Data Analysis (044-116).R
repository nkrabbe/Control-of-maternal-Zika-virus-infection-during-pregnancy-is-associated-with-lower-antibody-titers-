library(tidyverse)
library(drc)
library(readxl)
library(ggplot2)
library(scales)
###
# Using raw OD450 readings to generate a 4-parameter dose-response model and estimate the dilution of serum
# to reduce the maximum OD450 readings by 90% and 50% (EC90 and EC50) for each timepoint tested.
###

#044-116 0 dpi
DPI_0 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\IgG Whole Virion ELISA Raw Data (044-116).xlsx", sheet = 1, col_names = TRUE)
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
#044-116 4 dpi
DPI_4 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\IgG Whole Virion ELISA Raw Data (044-116).xlsx", sheet = 2, col_names = TRUE)
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
#044-116 7 dpi
DPI_7 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\IgG Whole Virion ELISA Raw Data (044-116).xlsx", sheet = 3, col_names = TRUE)
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
#044-116 13 dpi
DPI_13 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\IgG Whole Virion ELISA Raw Data (044-116).xlsx", sheet = 4, col_names = TRUE)
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
#044-116 23 dpi
DPI_23 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\IgG Whole Virion ELISA Raw Data (044-116).xlsx", sheet = 5, col_names = TRUE)
DPI_23$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_23$Log_Reciprocal_Serum_Dilution)
str(DPI_23)
model_DPI_23 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_23, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_23, type = "all")
summary(model_DPI_23)
ED(model_DPI_23, c(10,50), interval = "delta")
newdata_DPI_23 <- expand.grid(DF = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_23 <- predict(model_DPI_23, newdata = newdata_DPI_23, interval = "confidence")
newdata_DPI_23$p <- pm_DPI_23[,1]
newdata_DPI_23$pmin <- pm_DPI_23[,2]
newdata_DPI_23$pmax <- pm_DPI_23[,3]
#044-116 30 dpi 
DPI_30 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\IgG Whole Virion ELISA Raw Data (044-116).xlsx", sheet = 6, col_names = TRUE)
DPI_30$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_30$Log_Reciprocal_Serum_Dilution)
str(DPI_30)
model_DPI_30 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_30, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_30, type = "all")
summary(model_DPI_30)
ED(model_DPI_30, c(10,50), interval = "delta")
newdata_DPI_30 <- expand.grid(DF = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_30 <- predict(model_DPI_30, newdata = newdata_DPI_30, interval = "confidence")
newdata_DPI_30$p <- pm_DPI_30[,1]
newdata_DPI_30$pmin <- pm_DPI_30[,2]
newdata_DPI_30$pmax <- pm_DPI_30[,3]
#044-116 58 dpi
DPI_58 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\IgG Whole Virion ELISA Raw Data (044-116).xlsx", sheet = 7, col_names = TRUE)
DPI_58$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_58$Log_Reciprocal_Serum_Dilution)
str(DPI_58)
model_DPI_58 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_58, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_58, type = "all")
summary(model_DPI_58)
ED(model_DPI_58, c(10,50), interval = "delta")
newdata_DPI_58 <- expand.grid(DF = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_58 <- predict(model_DPI_58, newdata = newdata_DPI_58, interval = "confidence")
newdata_DPI_58$p <- pm_DPI_58[,1]
newdata_DPI_58$pmin <- pm_DPI_58[,2]
newdata_DPI_58$pmax <- pm_DPI_58[,3]
#044-116 86 dpi
DPI_86 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\IgG Whole Virion ELISA Raw Data (044-116).xlsx", sheet = 8, col_names = TRUE)
DPI_86$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_86$Log_Reciprocal_Serum_Dilution)
str(DPI_86)
model_DPI_86 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_86, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_86, type = "all")
summary(model_DPI_86)
ED(model_DPI_86, c(10,50), interval = "delta")
newdata_DPI_86 <- expand.grid(DF = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_86 <- predict(model_DPI_86, newdata = newdata_DPI_86, interval = "confidence")
newdata_DPI_86$p <- pm_DPI_86[,1]
newdata_DPI_86$pmin <- pm_DPI_86[,2]
newdata_DPI_86$pmax <- pm_DPI_86[,3]
#044-116 135 dpi
DPI_135 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\IgG Whole Virion ELISA Raw Data (044-116).xlsx", sheet = 9, col_names = TRUE)
DPI_135$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_135$Log_Reciprocal_Serum_Dilution)
str(DPI_135)
model_DPI_135 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_135, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_135, type = "all")
summary(model_DPI_135)
ED(model_DPI_135, c(10,50), interval = "delta")
newdata_DPI_135 <- expand.grid(DF = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_135 <- predict(model_DPI_135, newdata = newdata_DPI_135, interval = "confidence")
newdata_DPI_135$p <- pm_DPI_135[,1]
newdata_DPI_135$pmin <- pm_DPI_135[,2]
newdata_DPI_135$pmax <- pm_DPI_135[,3]
####### 
#044-116 raw IgG Whole Virion ELISA binding curves
######
rawcurves_044_116 <- 
  ggplot() + 
  geom_point(data = DPI_0, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "0 DPI")) + geom_line(data = newdata_DPI_0, aes(x = DF, y = p, color = "0 DPI")) +
  geom_point(data = DPI_4, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "4 DPI")) + geom_line(data = newdata_DPI_4, aes(x = DF, y = p, color = "4 DPI")) +
  geom_point(data = DPI_7, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "7 DPI")) + geom_line(data = newdata_DPI_7, aes(x = DF, y = p, color = "7 DPI")) +
  geom_point(data = DPI_13, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "13 DPI")) + geom_line(data = newdata_DPI_13, aes(x = DF, y = p, color = "13 DPI")) +
  geom_point(data = DPI_23, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "23 DPI")) + geom_line(data = newdata_DPI_23, aes(x = DF, y = p, color = "23 DPI")) +
  geom_point(data = DPI_30, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "30 DPI")) + geom_line(data = newdata_DPI_30, aes(x = DF, y = p, color = "30 DPI")) +
  geom_point(data = DPI_58, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "58 DPI")) + geom_line(data = newdata_DPI_58, aes(x = DF, y = p, color = "58 DPI")) +
  geom_point(data = DPI_86, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "86 DPI")) + geom_line(data = newdata_DPI_86, aes(x = DF, y = p, color = "86 DPI")) +
  geom_point(data = DPI_135, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "135 DPI")) + geom_line(data = newdata_DPI_135, aes(x = DF, y = p, color = "135 DPI"))
rawcurves_044_116 + theme_classic() + labs(x = "Log(reciprocal serum dilution)", y = "OD450 Reading", title = "IgG WVE Raw Binding Curves (044-116)") + xlim(1,6) + scale_color_discrete(limits = c("0 DPI", "4 DPI", "7 DPI", "13 DPI", "23 DPI", "30 DPI", "58 DPI", "86 DPI", "135 DPI")) + guides(color = guide_legend(title = "Days Post Infection"))
