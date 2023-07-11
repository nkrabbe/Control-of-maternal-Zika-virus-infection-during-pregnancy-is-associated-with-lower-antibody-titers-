library(tidyverse)
library(drc)
library(readxl)
library(ggplot2)
library(scales)
###
# Using raw OD450 readings to generate a 4-parameter dose-response model and estimate the dilution of serum
# to reduce the maximum OD450 readings by 90% and 50% (EC90 and EC50) for each timepoint tested.
###

#044-126 0 dpi
DPI_0 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\IgG Whole Virion ELISA Raw Data (044-126).xlsx", sheet = 1, col_names = TRUE)
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
#044-126 4 dpi
DPI_4 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\IgG Whole Virion ELISA Raw Data (044-126).xlsx", sheet = 2, col_names = TRUE)
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
#044-126 7 dpi
DPI_7 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\IgG Whole Virion ELISA Raw Data (044-126).xlsx", sheet = 3, col_names = TRUE)
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
#044-126 14 dpi
DPI_14 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\IgG Whole Virion ELISA Raw Data (044-126).xlsx", sheet = 4, col_names = TRUE)
DPI_14$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_14$Log_Reciprocal_Serum_Dilution)
str(DPI_14)
model_DPI_14 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_14, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_14, type = "all")
summary(model_DPI_14)
ED(model_DPI_14, c(10,50), interval = "delta")
newdata_DPI_14 <- expand.grid(DF = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_14 <- predict(model_DPI_14, newdata = newdata_DPI_14, interval = "confidence")
newdata_DPI_14$p <- pm_DPI_14[,1]
newdata_DPI_14$pmin <- pm_DPI_14[,2]
newdata_DPI_14$pmax <- pm_DPI_14[,3]
#044-126 18 dpi
DPI_18 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\IgG Whole Virion ELISA Raw Data (044-126).xlsx", sheet = 5, col_names = TRUE)
DPI_18$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_18$Log_Reciprocal_Serum_Dilution)
str(DPI_18)
model_DPI_18 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_18, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_18, type = "all")
summary(model_DPI_18)
ED(model_DPI_18, c(10,50), interval = "delta")
newdata_DPI_18 <- expand.grid(DF = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_18 <- predict(model_DPI_18, newdata = newdata_DPI_18, interval = "confidence")
newdata_DPI_18$p <- pm_DPI_18[,1]
newdata_DPI_18$pmin <- pm_DPI_18[,2]
newdata_DPI_18$pmax <- pm_DPI_18[,3]
#044-126 28 dpi 
DPI_28 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\IgG Whole Virion ELISA Raw Data (044-126).xlsx", sheet = 6, col_names = TRUE)
DPI_28$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_28$Log_Reciprocal_Serum_Dilution)
str(DPI_28)
model_DPI_28 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_28, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_28, type = "all")
summary(model_DPI_28)
ED(model_DPI_28, c(10,50), interval = "delta")
newdata_DPI_28 <- expand.grid(DF = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_28 <- predict(model_DPI_28, newdata = newdata_DPI_28, interval = "confidence")
newdata_DPI_28$p <- pm_DPI_28[,1]
newdata_DPI_28$pmin <- pm_DPI_28[,2]
newdata_DPI_28$pmax <- pm_DPI_28[,3]
#044-126 53 dpi
DPI_53 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\IgG Whole Virion ELISA Raw Data (044-126).xlsx", sheet = 7, col_names = TRUE)
DPI_53$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_53$Log_Reciprocal_Serum_Dilution)
str(DPI_53)
model_DPI_53 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_53, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_53, type = "all")
summary(model_DPI_53)
ED(model_DPI_53, c(10,50), interval = "delta")
newdata_DPI_53 <- expand.grid(DF = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_53 <- predict(model_DPI_53, newdata = newdata_DPI_53, interval = "confidence")
newdata_DPI_53$p <- pm_DPI_53[,1]
newdata_DPI_53$pmin <- pm_DPI_53[,2]
newdata_DPI_53$pmax <- pm_DPI_53[,3]
#044-126 88 dpi
DPI_88 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\IgG Whole Virion ELISA Raw Data (044-126).xlsx", sheet = 8, col_names = TRUE)
DPI_88$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_88$Log_Reciprocal_Serum_Dilution)
str(DPI_88)
model_DPI_88 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_88, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_88, type = "all")
summary(model_DPI_88)
ED(model_DPI_88, c(10,50), interval = "delta")
newdata_DPI_88 <- expand.grid(DF = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_88 <- predict(model_DPI_88, newdata = newdata_DPI_88, interval = "confidence")
newdata_DPI_88$p <- pm_DPI_88[,1]
newdata_DPI_88$pmin <- pm_DPI_88[,2]
newdata_DPI_88$pmax <- pm_DPI_88[,3]
#044-126 117 dpi
DPI_117 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\IgG Whole Virion ELISA Raw Data (044-126).xlsx", sheet = 9, col_names = TRUE)
DPI_117$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_117$Log_Reciprocal_Serum_Dilution)
str(DPI_117)
model_DPI_117 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_117, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_117, type = "all")
summary(model_DPI_117)
ED(model_DPI_117, c(10,50), interval = "delta")
newdata_DPI_117 <- expand.grid(DF = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_117 <- predict(model_DPI_117, newdata = newdata_DPI_117, interval = "confidence")
newdata_DPI_117$p <- pm_DPI_117[,1]
newdata_DPI_117$pmin <- pm_DPI_117[,2]
newdata_DPI_117$pmax <- pm_DPI_117[,3]
####### 
#044-126 raw IgG Whole Virion ELISA binding curves
######
rawcurves_044_126 <- 
  ggplot() + 
  geom_point(data = DPI_0, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "0 DPI")) + geom_line(data = newdata_DPI_0, aes(x = DF, y = p, color = "0 DPI")) +
  geom_point(data = DPI_4, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "4 DPI")) + geom_line(data = newdata_DPI_4, aes(x = DF, y = p, color = "4 DPI")) +
  geom_point(data = DPI_7, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "7 DPI")) + geom_line(data = newdata_DPI_7, aes(x = DF, y = p, color = "7 DPI")) +
  geom_point(data = DPI_14, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "14 DPI")) + geom_line(data = newdata_DPI_14, aes(x = DF, y = p, color = "14 DPI")) +
  geom_point(data = DPI_18, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "18 DPI")) + geom_line(data = newdata_DPI_18, aes(x = DF, y = p, color = "18 DPI")) +
  geom_point(data = DPI_28, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "28 DPI")) + geom_line(data = newdata_DPI_28, aes(x = DF, y = p, color = "28 DPI")) +
  geom_point(data = DPI_53, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "53 DPI")) + geom_line(data = newdata_DPI_53, aes(x = DF, y = p, color = "53 DPI")) +
  geom_point(data = DPI_88, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "88 DPI")) + geom_line(data = newdata_DPI_88, aes(x = DF, y = p, color = "88 DPI")) +
  geom_point(data = DPI_117, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "117 DPI")) + geom_line(data = newdata_DPI_117, aes(x = DF, y = p, color = "117 DPI"))
rawcurves_044_126 + theme_classic() + labs(x = "Log(reciprocal serum dilution)", y = "OD450 Reading", title = "IgG WVE Raw Binding Curves (044-126)") + xlim(1,6) + scale_color_discrete(limits = c("0 DPI", "4 DPI", "7 DPI", "14 DPI", "18 DPI", "28 DPI", "53 DPI", "88 DPI", "117 DPI")) + guides(color = guide_legend(title = "Days Post Infection"))
