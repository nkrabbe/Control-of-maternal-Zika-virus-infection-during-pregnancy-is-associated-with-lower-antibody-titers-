library(tidyverse)
library(drc)
library(readxl)
library(ggplot2)
library(scales)
###
# Using raw OD450 readings to generate a 4-parameter dose-response model and estimate the dilution of serum
# to reduce the maximum OD450 readings by 90% and 50% (EC90 and EC50) for each timepoint tested.
###

#044-118 0 dpi
DPI_0 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\IgG Whole Virion ELISA Raw Data (044-118).xlsx", sheet = 1, col_names = TRUE)
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
#044-118 4 dpi
DPI_4 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\IgG Whole Virion ELISA Raw Data (044-118).xlsx", sheet = 2, col_names = TRUE)
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
#044-118 7 dpi
DPI_7 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\IgG Whole Virion ELISA Raw Data (044-118).xlsx", sheet = 3, col_names = TRUE)
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
#044-118 15 dpi
DPI_15 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\IgG Whole Virion ELISA Raw Data (044-118).xlsx", sheet = 4, col_names = TRUE)
DPI_15$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_15$Log_Reciprocal_Serum_Dilution)
str(DPI_15)
model_DPI_15 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_15, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_15, type = "all")
summary(model_DPI_15)
ED(model_DPI_15, c(10,50), interval = "delta")
newdata_DPI_15 <- expand.grid(DF = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_15 <- predict(model_DPI_15, newdata = newdata_DPI_15, interval = "confidence")
newdata_DPI_15$p <- pm_DPI_15[,1]
newdata_DPI_15$pmin <- pm_DPI_15[,2]
newdata_DPI_15$pmax <- pm_DPI_15[,3]
#044-118 18 dpi
DPI_18 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\IgG Whole Virion ELISA Raw Data (044-118).xlsx", sheet = 5, col_names = TRUE)
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
#044-118 29 dpi 
DPI_29 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\IgG Whole Virion ELISA Raw Data (044-118).xlsx", sheet = 6, col_names = TRUE)
DPI_29$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_29$Log_Reciprocal_Serum_Dilution)
str(DPI_29)
model_DPI_29 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_29, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_29, type = "all")
summary(model_DPI_29)
ED(model_DPI_29, c(10,50), interval = "delta")
newdata_DPI_29 <- expand.grid(DF = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_29 <- predict(model_DPI_29, newdata = newdata_DPI_29, interval = "confidence")
newdata_DPI_29$p <- pm_DPI_29[,1]
newdata_DPI_29$pmin <- pm_DPI_29[,2]
newdata_DPI_29$pmax <- pm_DPI_29[,3]
#044-118 53 dpi
DPI_53 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\IgG Whole Virion ELISA Raw Data (044-118).xlsx", sheet = 7, col_names = TRUE)
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
#044-118 116 dpi
DPI_116 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\IgG Whole Virion ELISA Raw Data (044-118).xlsx", sheet = 8, col_names = TRUE)
DPI_116$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_116$Log_Reciprocal_Serum_Dilution)
str(DPI_116)
model_DPI_116 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_116, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_116, type = "all")
summary(model_DPI_116)
ED(model_DPI_116, c(10,50), interval = "delta")
newdata_DPI_116 <- expand.grid(DF = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_116 <- predict(model_DPI_116, newdata = newdata_DPI_116, interval = "confidence")
newdata_DPI_116$p <- pm_DPI_116[,1]
newdata_DPI_116$pmin <- pm_DPI_116[,2]
newdata_DPI_116$pmax <- pm_DPI_116[,3]
#044-118 123 dpi
DPI_123 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\IgG Whole Virion ELISA Raw Data (044-118).xlsx", sheet = 9, col_names = TRUE)
DPI_123$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_123$Log_Reciprocal_Serum_Dilution)
str(DPI_123)
model_DPI_123 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_123, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_123, type = "all")
summary(model_DPI_123)
ED(model_DPI_123, c(10,50), interval = "delta")
newdata_DPI_123 <- expand.grid(DF = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_123 <- predict(model_DPI_123, newdata = newdata_DPI_123, interval = "confidence")
newdata_DPI_123$p <- pm_DPI_123[,1]
newdata_DPI_123$pmin <- pm_DPI_123[,2]
newdata_DPI_123$pmax <- pm_DPI_123[,3]
####### 
#044-118 raw IgG Whole Virion ELISA binding curves
######
rawcurves_044_118 <- 
  ggplot() + 
  geom_point(data = DPI_0, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "0 DPI")) + geom_line(data = newdata_DPI_0, aes(x = DF, y = p, color = "0 DPI")) +
  geom_point(data = DPI_4, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "4 DPI")) + geom_line(data = newdata_DPI_4, aes(x = DF, y = p, color = "4 DPI")) +
  geom_point(data = DPI_7, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "7 DPI")) + geom_line(data = newdata_DPI_7, aes(x = DF, y = p, color = "7 DPI")) +
  geom_point(data = DPI_15, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "15 DPI")) + geom_line(data = newdata_DPI_15, aes(x = DF, y = p, color = "15 DPI")) +
  geom_point(data = DPI_18, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "18 DPI")) + geom_line(data = newdata_DPI_18, aes(x = DF, y = p, color = "18 DPI")) +
  geom_point(data = DPI_29, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "29 DPI")) + geom_line(data = newdata_DPI_29, aes(x = DF, y = p, color = "29 DPI")) +
  geom_point(data = DPI_53, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "53 DPI")) + geom_line(data = newdata_DPI_53, aes(x = DF, y = p, color = "53 DPI")) +
  geom_point(data = DPI_116, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "116 DPI")) + geom_line(data = newdata_DPI_116, aes(x = DF, y = p, color = "116 DPI")) +
  geom_point(data = DPI_123, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "123 DPI")) + geom_line(data = newdata_DPI_123, aes(x = DF, y = p, color = "123 DPI"))
rawcurves_044_118 + theme_classic() + labs(x = "Log(reciprocal serum dilution)", y = "OD450 Reading", title = "IgG WVE Raw Binding Curves (044-118)") + xlim(1,6) + scale_color_discrete(limits = c("0 DPI", "4 DPI", "7 DPI", "15 DPI", "18 DPI", "29 DPI", "53 DPI", "116 DPI", "123 DPI")) + guides(color = guide_legend(title = "Days Post Infection"))
