library(tidyverse)
library(drc)
library(readxl)
library(ggplot2)
library(scales)
###
# Using raw OD450 readings to generate a 4-parameter dose-response model and estimate the dilution of serum
# to reduce the maximum OD450 readings by 90% and 50% (EC90 and EC50) for each timepoint tested.
###

#044-114 0 dpi
DPI_0 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\IgG Whole Virion ELISA Raw Data (044-114).xlsx", sheet = 1, col_names = TRUE)
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
#044-114 4 dpi
DPI_4 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\IgG Whole Virion ELISA Raw Data (044-114).xlsx", sheet = 2, col_names = TRUE)
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
#044-114 7 dpi
DPI_7 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\IgG Whole Virion ELISA Raw Data (044-114).xlsx", sheet = 3, col_names = TRUE)
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
#044-114 14 dpi
DPI_14 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\IgG Whole Virion ELISA Raw Data (044-114).xlsx", sheet = 4, col_names = TRUE)
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
#044-114 21 dpi
DPI_21 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\IgG Whole Virion ELISA Raw Data (044-114).xlsx", sheet = 5, col_names = TRUE)
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
#044-114 28 dpi 
DPI_28 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\IgG Whole Virion ELISA Raw Data (044-114).xlsx", sheet = 6, col_names = TRUE)
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
#044-114 59 dpi
DPI_59 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\IgG Whole Virion ELISA Raw Data (044-114).xlsx", sheet = 7, col_names = TRUE)
DPI_59$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_59$Log_Reciprocal_Serum_Dilution)
str(DPI_59)
model_DPI_59 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_59, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_59, type = "all")
summary(model_DPI_59)
ED(model_DPI_59, c(10,50), interval = "delta")
newdata_DPI_59 <- expand.grid(DF = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_59 <- predict(model_DPI_59, newdata = newdata_DPI_59, interval = "confidence")
newdata_DPI_59$p <- pm_DPI_59[,1]
newdata_DPI_59$pmin <- pm_DPI_59[,2]
newdata_DPI_59$pmax <- pm_DPI_59[,3]
#044-114 87 dpi
DPI_87 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\IgG Whole Virion ELISA Raw Data (044-114).xlsx", sheet = 8, col_names = TRUE)
DPI_87$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_87$Log_Reciprocal_Serum_Dilution)
str(DPI_87)
model_DPI_87 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_87, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_87, type = "all")
summary(model_DPI_87)
ED(model_DPI_87, c(10,50), interval = "delta")
newdata_DPI_87 <- expand.grid(DF = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_87 <- predict(model_DPI_87, newdata = newdata_DPI_87, interval = "confidence")
newdata_DPI_87$p <- pm_DPI_87[,1]
newdata_DPI_87$pmin <- pm_DPI_87[,2]
newdata_DPI_87$pmax <- pm_DPI_87[,3]
#044-114 108 dpi
DPI_108 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\IgG Whole Virion ELISA Raw Data (044-114).xlsx", sheet = 9, col_names = TRUE)
DPI_108$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_108$Log_Reciprocal_Serum_Dilution)
str(DPI_108)
model_DPI_108 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_108, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_108, type = "all")
summary(model_DPI_108)
ED(model_DPI_108, c(10,50), interval = "delta")
newdata_DPI_108 <- expand.grid(DF = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_108 <- predict(model_DPI_108, newdata = newdata_DPI_108, interval = "confidence")
newdata_DPI_108$p <- pm_DPI_108[,1]
newdata_DPI_108$pmin <- pm_DPI_108[,2]
newdata_DPI_108$pmax <- pm_DPI_108[,3]
#044-114 135 dpi
DPI_135 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\IgG Whole Virion ELISA Raw Data (044-114).xlsx", sheet = 10, col_names = TRUE)
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
#044-114 raw IgG Whole Virion ELISA binding curves
######
rawcurves_044_114 <- 
  ggplot() + 
  geom_point(data = DPI_0, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "0 DPI")) + geom_line(data = newdata_DPI_0, aes(x = DF, y = p, color = "0 DPI")) +
  geom_point(data = DPI_4, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "4 DPI")) + geom_line(data = newdata_DPI_4, aes(x = DF, y = p, color = "4 DPI")) +
  geom_point(data = DPI_7, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "7 DPI")) + geom_line(data = newdata_DPI_7, aes(x = DF, y = p, color = "7 DPI")) +
  geom_point(data = DPI_14, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "14 DPI")) + geom_line(data = newdata_DPI_14, aes(x = DF, y = p, color = "14 DPI")) +
  geom_point(data = DPI_21, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "21 DPI")) + geom_line(data = newdata_DPI_21, aes(x = DF, y = p, color = "21 DPI")) +
  geom_point(data = DPI_28, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "28 DPI")) + geom_line(data = newdata_DPI_28, aes(x = DF, y = p, color = "28 DPI")) +
  geom_point(data = DPI_59, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "59 DPI")) + geom_line(data = newdata_DPI_59, aes(x = DF, y = p, color = "59 DPI")) +
  geom_point(data = DPI_87, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "87 DPI")) + geom_line(data = newdata_DPI_87, aes(x = DF, y = p, color = "87 DPI")) +
  geom_point(data = DPI_108, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "108 DPI")) + geom_line(data = newdata_DPI_108, aes(x = DF, y = p, color = "108 DPI")) +
  geom_point(data = DPI_135, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "135 DPI")) + geom_line(data = newdata_DPI_135, aes(x = DF, y = p, color = "135 DPI"))
rawcurves_044_114 + theme_classic() + labs(x = "Log(reciprocal serum dilution)", y = "OD450 Reading", title = "IgG WVE Raw Binding Curves (044-114)") + xlim(1,6) + scale_color_discrete(limits = c("0 DPI", "4 DPI", "7 DPI", "14 DPI", "21 DPI", "28 DPI", "59 DPI", "87 DPI", "108 DPI", "135 DPI")) + guides(color = guide_legend(title = "Days Post Infection"))
