library(tidyverse)
library(drc)
library(readxl)
library(ggplot2)
library(scales)
###
# Using raw OD450 readings to generate a 4-parameter dose-response model and estimate the dilution of serum
# to reduce the maximum OD450 readings by 90% and 50% (EC90 and EC50) for each timepoint tested.
###

#044-122 0 dpi
DPI_0 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\IgG Whole Virion ELISA Raw Data (044-122).xlsx", sheet = 1, col_names = TRUE)
DPI_0$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_0$Log_Reciprocal_Serum_Dilution)
str(DPI_0)
model_DPI_0 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_0, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_0, type = "all")
summary(model_DPI_0)
ED(model_DPI_0, c(10,50), interval = "delta")
newdata_DPI_0 <- expand.grid(DF = exp(seq(log(1.70), log(5.31), length = 100)))
pm_DPI_0 <- predict(model_DPI_0, newdata = newdata_DPI_0, interval = "confidence")
newdata_DPI_0$p <- pm_DPI_0[,1]
newdata_DPI_0$pmin <- pm_DPI_0[,2]
newdata_DPI_0$pmax <- pm_DPI_0[,3]
#044-122 4 dpi
DPI_4 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\IgG Whole Virion ELISA Raw Data (044-122).xlsx", sheet = 2, col_names = TRUE)
DPI_4$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_4$Log_Reciprocal_Serum_Dilution)
str(DPI_4)
model_DPI_4 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_4, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_4, type = "all")
summary(model_DPI_4)
ED(model_DPI_4, c(10,50), interval = "delta")
newdata_DPI_4 <- expand.grid(DF = exp(seq(log(1.7), log(5.31), length = 100)))
pm_DPI_4 <- predict(model_DPI_4, newdata = newdata_DPI_4, interval = "confidence")
newdata_DPI_4$p <- pm_DPI_4[,1]
newdata_DPI_4$pmin <- pm_DPI_4[,2]
newdata_DPI_4$pmax <- pm_DPI_4[,3]
#044-122 8 dpi
DPI_8 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\IgG Whole Virion ELISA Raw Data (044-122).xlsx", sheet = 3, col_names = TRUE)
DPI_8$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_8$Log_Reciprocal_Serum_Dilution)
str(DPI_8)
model_DPI_8 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_8, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_8, type = "all")
summary(model_DPI_8)
ED(model_DPI_8, c(10,50), interval = "delta")
newdata_DPI_8 <- expand.grid(DF = exp(seq(log(1.7), log(5.31), length = 100)))
pm_DPI_8 <- predict(model_DPI_8, newdata = newdata_DPI_8, interval = "confidence")
newdata_DPI_8$p <- pm_DPI_8[,1]
newdata_DPI_8$pmin <- pm_DPI_8[,2]
newdata_DPI_8$pmax <- pm_DPI_8[,3]
#044-122 14 dpi
DPI_14 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\IgG Whole Virion ELISA Raw Data (044-122).xlsx", sheet = 4, col_names = TRUE)
DPI_14$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_14$Log_Reciprocal_Serum_Dilution)
str(DPI_14)
model_DPI_14 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_14, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_14, type = "all")
summary(model_DPI_14)
ED(model_DPI_14, c(10,50), interval = "delta")
newdata_DPI_14 <- expand.grid(DF = exp(seq(log(1.7), log(5.31), length = 100)))
pm_DPI_14 <- predict(model_DPI_14, newdata = newdata_DPI_14, interval = "confidence")
newdata_DPI_14$p <- pm_DPI_14[,1]
newdata_DPI_14$pmin <- pm_DPI_14[,2]
newdata_DPI_14$pmax <- pm_DPI_14[,3]
#044-122 21 dpi
DPI_21 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\IgG Whole Virion ELISA Raw Data (044-122).xlsx", sheet = 5, col_names = TRUE)
DPI_21$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_21$Log_Reciprocal_Serum_Dilution)
str(DPI_21)
model_DPI_21 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_21, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_21, type = "all")
summary(model_DPI_21)
ED(model_DPI_21, c(10,50), interval = "delta")
newdata_DPI_21 <- expand.grid(DF = exp(seq(log(1.7), log(5.31), length = 100)))
pm_DPI_21 <- predict(model_DPI_21, newdata = newdata_DPI_21, interval = "confidence")
newdata_DPI_21$p <- pm_DPI_21[,1]
newdata_DPI_21$pmin <- pm_DPI_21[,2]
newdata_DPI_21$pmax <- pm_DPI_21[,3]
#044-122 32 dpi 
DPI_32 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\IgG Whole Virion ELISA Raw Data (044-122).xlsx", sheet = 6, col_names = TRUE)
DPI_32$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_32$Log_Reciprocal_Serum_Dilution)
str(DPI_32)
model_DPI_32 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_32, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_32, type = "all")
summary(model_DPI_32)
ED(model_DPI_32, c(10,50), interval = "delta")
newdata_DPI_32 <- expand.grid(DF = exp(seq(log(1.7), log(5.31), length = 100)))
pm_DPI_32 <- predict(model_DPI_32, newdata = newdata_DPI_32, interval = "confidence")
newdata_DPI_32$p <- pm_DPI_32[,1]
newdata_DPI_32$pmin <- pm_DPI_32[,2]
newdata_DPI_32$pmax <- pm_DPI_32[,3]
#044-122 53 dpi
DPI_53 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\IgG Whole Virion ELISA Raw Data (044-122).xlsx", sheet = 7, col_names = TRUE)
DPI_53$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_53$Log_Reciprocal_Serum_Dilution)
str(DPI_53)
model_DPI_53 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_53, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_53, type = "all")
summary(model_DPI_53)
ED(model_DPI_53, c(10,50), interval = "delta")
newdata_DPI_53 <- expand.grid(DF = exp(seq(log(1.7), log(5.31), length = 100)))
pm_DPI_53 <- predict(model_DPI_53, newdata = newdata_DPI_53, interval = "confidence")
newdata_DPI_53$p <- pm_DPI_53[,1]
newdata_DPI_53$pmin <- pm_DPI_53[,2]
newdata_DPI_53$pmax <- pm_DPI_53[,3]
#044-122 88 dpi
DPI_88 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\IgG Whole Virion ELISA Raw Data (044-122).xlsx", sheet = 8, col_names = TRUE)
DPI_88$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_88$Log_Reciprocal_Serum_Dilution)
str(DPI_88)
model_DPI_88 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_88, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_88, type = "all")
summary(model_DPI_88)
ED(model_DPI_88, c(10,50), interval = "delta")
newdata_DPI_88 <- expand.grid(DF = exp(seq(log(1.7), log(5.31), length = 100)))
pm_DPI_88 <- predict(model_DPI_88, newdata = newdata_DPI_88, interval = "confidence")
newdata_DPI_88$p <- pm_DPI_88[,1]
newdata_DPI_88$pmin <- pm_DPI_88[,2]
newdata_DPI_88$pmax <- pm_DPI_88[,3]
#044-122 116 dpi
DPI_116 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\IgG Whole Virion ELISA Raw Data (044-122).xlsx", sheet = 9, col_names = TRUE)
DPI_116$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_116$Log_Reciprocal_Serum_Dilution)
str(DPI_116)
model_DPI_116 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_116, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_116, type = "all")
summary(model_DPI_116)
ED(model_DPI_116, c(10,50), interval = "delta")
newdata_DPI_116 <- expand.grid(DF = exp(seq(log(1.7), log(5.31), length = 100)))
pm_DPI_116 <- predict(model_DPI_116, newdata = newdata_DPI_116, interval = "confidence")
newdata_DPI_116$p <- pm_DPI_116[,1]
newdata_DPI_116$pmin <- pm_DPI_116[,2]
newdata_DPI_116$pmax <- pm_DPI_116[,3]
#044-122 123 DPI
DPI_123 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\IgG Whole Virion ELISA Raw Data (044-122).xlsx", sheet = 10, col_names = TRUE)
DPI_123$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_123$Log_Reciprocal_Serum_Dilution)
str(DPI_123)
model_DPI_123 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_123, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_123, type = "all")
summary(model_DPI_123)
ED(model_DPI_123, c(10,50), interval = "delta")
newdata_DPI_123 <- expand.grid(DF = exp(seq(log(1.7), log(5.31), length = 100)))
pm_DPI_123 <- predict(model_DPI_123, newdata = newdata_DPI_123, interval = "confidence")
newdata_DPI_123$p <- pm_DPI_123[,1]
newdata_DPI_123$pmin <- pm_DPI_123[,2]
newdata_DPI_123$pmax <- pm_DPI_123[,3]
####### 
#044-122 raw IgG Whole Virion ELISA binding curves
######
rawcurves_044_122 <- 
  ggplot() + 
  geom_point(data = DPI_0, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "0 DPI")) + geom_line(data = newdata_DPI_0, aes(x = DF, y = p, color = "0 DPI")) +
  geom_point(data = DPI_4, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "4 DPI")) + geom_line(data = newdata_DPI_4, aes(x = DF, y = p, color = "4 DPI")) +
  geom_point(data = DPI_8, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "8 DPI")) + geom_line(data = newdata_DPI_8, aes(x = DF, y = p, color = "8 DPI")) +
  geom_point(data = DPI_14, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "14 DPI")) + geom_line(data = newdata_DPI_14, aes(x = DF, y = p, color = "14 DPI")) +
  geom_point(data = DPI_21, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "21 DPI")) + geom_line(data = newdata_DPI_21, aes(x = DF, y = p, color = "21 DPI")) +
  geom_point(data = DPI_32, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "32 DPI")) + geom_line(data = newdata_DPI_32, aes(x = DF, y = p, color = "32 DPI")) +
  geom_point(data = DPI_53, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "53 DPI")) + geom_line(data = newdata_DPI_53, aes(x = DF, y = p, color = "53 DPI")) +
  geom_point(data = DPI_88, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "88 DPI")) + geom_line(data = newdata_DPI_88, aes(x = DF, y = p, color = "88 DPI")) +
  geom_point(data = DPI_116, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "116 DPI")) + geom_line(data = newdata_DPI_116, aes(x = DF, y = p, color = "116 DPI")) +
  geom_point(data = DPI_123, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "123 DPI")) + geom_line(data = newdata_DPI_123, aes(x = DF, y = p, color = "12 DPI"))
rawcurves_044_122 + theme_classic() + labs(x = "Log(reciprocal serum dilution)", y = "OD450 Reading", title = "IgG WVE Raw Binding Curves (044-122)") + xlim(1,6) + scale_color_discrete(limits = c("0 DPI", "4 DPI", "8 DPI", "14 DPI", "21 DPI", "32 DPI", "53 DPI", "88 DPI", "116 DPI", "123 DPI")) + guides(color = guide_legend(title = "Days Post Infection"))
