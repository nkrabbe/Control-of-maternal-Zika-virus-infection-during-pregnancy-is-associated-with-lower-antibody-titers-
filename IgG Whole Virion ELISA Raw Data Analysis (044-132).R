library(tidyverse)
library(drc)
library(readxl)
library(ggplot2)
library(scales)
###
# Using raw OD450 readings to generate a 4-parameter dose-response model and estimate the dilution of serum
# to reduce the maximum OD450 readings by 90% and 50% (EC90 and EC50) for each timepoint tested.
###

#044-132 0 dpi
DPI_0 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\IgG Whole Virion ELISA Raw Data (044-132).xlsx", sheet = 1, col_names = TRUE)
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
#044-132 4 dpi
DPI_4 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\IgG Whole Virion ELISA Raw Data (044-132).xlsx", sheet = 2, col_names = TRUE)
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
#044-132 7 dpi
DPI_7 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\IgG Whole Virion ELISA Raw Data (044-132).xlsx", sheet = 3, col_names = TRUE)
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
#044-132 14 dpi
DPI_14 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\IgG Whole Virion ELISA Raw Data (044-132).xlsx", sheet = 4, col_names = TRUE)
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
#044-132 21 dpi
DPI_21 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\IgG Whole Virion ELISA Raw Data (044-132).xlsx", sheet = 5, col_names = TRUE)
DPI_21$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_21$Log_Reciprocal_Serum_Dilution)
str(DPI_21)
model_DPI_21 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_21, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_21, type = "all")
summary(model_DPI_21)
ED(model_DPI_21, c(10,50), interval = "delta")
newdata_DPI_21 <- expand.grid(DF = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_21 <- predict(model_DPI_21, newdata = newdata_DPI_21, interval = "confidence")
newdata_DPI_21$p <- pm_DPI_21[,1]
newdata_DPI_21$pmin <- pm_DPI_21[,2]
newdata_DPI_21$pmax <- pm_DPI_21[,3]
#044-132 31 dpi 
DPI_31 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\IgG Whole Virion ELISA Raw Data (044-132).xlsx", sheet = 6, col_names = TRUE)
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
#044-132 56 dpi
DPI_56 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\IgG Whole Virion ELISA Raw Data (044-132).xlsx", sheet = 7, col_names = TRUE)
DPI_56$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_56$Log_Reciprocal_Serum_Dilution)
str(DPI_56)
model_DPI_56 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_56, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_56, type = "all")
summary(model_DPI_56)
ED(model_DPI_56, c(10,50), interval = "delta")
newdata_DPI_56 <- expand.grid(DF = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_56 <- predict(model_DPI_56, newdata = newdata_DPI_56, interval = "confidence")
newdata_DPI_56$p <- pm_DPI_56[,1]
newdata_DPI_56$pmin <- pm_DPI_56[,2]
newdata_DPI_56$pmax <- pm_DPI_56[,3]
#044-132 91 dpi
DPI_91 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\IgG Whole Virion ELISA Raw Data (044-132).xlsx", sheet = 8, col_names = TRUE)
DPI_91$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_91$Log_Reciprocal_Serum_Dilution)
str(DPI_91)
model_DPI_91 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_91, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_91, type = "all")
summary(model_DPI_91)
ED(model_DPI_91, c(10,50), interval = "delta")
newdata_DPI_91 <- expand.grid(DF = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_91 <- predict(model_DPI_91, newdata = newdata_DPI_91, interval = "confidence")
newdata_DPI_91$p <- pm_DPI_91[,1]
newdata_DPI_91$pmin <- pm_DPI_91[,2]
newdata_DPI_91$pmax <- pm_DPI_91[,3]
#044-132 115 dpi
DPI_115 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\IgG Whole Virion ELISA Raw Data (044-132).xlsx", sheet = 9, col_names = TRUE)
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
#044-132 129 dpi
DPI_129 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\IgG Whole Virion ELISA Raw Data (044-132).xlsx", sheet = 10, col_names = TRUE)
DPI_129$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_129$Log_Reciprocal_Serum_Dilution)
str(DPI_129)
model_DPI_129 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_129, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_129, type = "all")
summary(model_DPI_129)
ED(model_DPI_129, c(10,50), interval = "delta")
newdata_DPI_129 <- expand.grid(DF = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_129 <- predict(model_DPI_129, newdata = newdata_DPI_129, interval = "confidence")
newdata_DPI_129$p <- pm_DPI_129[,1]
newdata_DPI_129$pmin <- pm_DPI_129[,2]
newdata_DPI_129$pmax <- pm_DPI_129[,3]
####### 
#044-132 raw IgG Whole Virion ELISA binding curves
######
rawcurves_044_132 <- 
  ggplot() + 
  geom_point(data = DPI_0, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "0 DPI")) + geom_line(data = newdata_DPI_0, aes(x = DF, y = p, color = "0 DPI")) +
  geom_point(data = DPI_4, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "4 DPI")) + geom_line(data = newdata_DPI_4, aes(x = DF, y = p, color = "4 DPI")) +
  geom_point(data = DPI_7, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "7 DPI")) + geom_line(data = newdata_DPI_7, aes(x = DF, y = p, color = "7 DPI")) +
  geom_point(data = DPI_14, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "14 DPI")) + geom_line(data = newdata_DPI_14, aes(x = DF, y = p, color = "14 DPI")) +
  geom_point(data = DPI_21, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "21 DPI")) + geom_line(data = newdata_DPI_21, aes(x = DF, y = p, color = "21 DPI")) +
  geom_point(data = DPI_31, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "31 DPI")) + geom_line(data = newdata_DPI_31, aes(x = DF, y = p, color = "31 DPI")) +
  geom_point(data = DPI_56, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "56 DPI")) + geom_line(data = newdata_DPI_56, aes(x = DF, y = p, color = "56 DPI")) +
  geom_point(data = DPI_91, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "91 DPI")) + geom_line(data = newdata_DPI_91, aes(x = DF, y = p, color = "91 DPI")) +
  geom_point(data = DPI_115, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "115 DPI")) + geom_line(data = newdata_DPI_115, aes(x = DF, y = p, color = "115 DPI")) +
  geom_point(data = DPI_129, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "129 DPI")) + geom_line(data = newdata_DPI_129, aes(x = DF, y = p, color = "129 DPI"))
rawcurves_044_132 + theme_classic() + labs(x = "Log(reciprocal serum dilution)", y = "OD450 Reading", title = "IgG WVE Raw Binding Curves (044-132)") + xlim(1,6) + scale_color_discrete(limits = c("0 DPI", "4 DPI", "7 DPI", "14 DPI", "21 DPI", "31 DPI", "56 DPI", "91 DPI", "115 DPI", "129 DPI")) + guides(color = guide_legend(title = "Days Post Infection"))
