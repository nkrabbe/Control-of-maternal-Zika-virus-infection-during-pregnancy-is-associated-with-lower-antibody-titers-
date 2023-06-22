library(tidyverse)
library(drc)
library(readxl)
library(ggplot2)
library(scales)
###
# Using raw OD450 readings to generate a 4-parameter dose-response model and estimate the dilution of serum
# to reduce the maximum OD450 readings by 90% and 50% (EC90 and EC50) for each timepoint tested.
###

#044-101 0 dpi
DPI_0 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\IgG Whole Virion ELISA Raw Data (044-101).xlsx", sheet = 1, col_names = TRUE)
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
#044-101 2 dpi
DPI_2 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\IgG Whole Virion ELISA Raw Data (044-101).xlsx", sheet = 2, col_names = TRUE)
DPI_2$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_2$Log_Reciprocal_Serum_Dilution)
str(DPI_2)
model_DPI_2 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_2, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_2, type = "all")
summary(model_DPI_2)
ED(model_DPI_2, c(10,50), interval = "delta")
newdata_DPI_2 <- expand.grid(DF = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_2 <- predict(model_DPI_2, newdata = newdata_DPI_2, interval = "confidence")
newdata_DPI_2$p <- pm_DPI_2[,1]
newdata_DPI_2$pmin <- pm_DPI_2[,2]
newdata_DPI_2$pmax <- pm_DPI_2[,3]
#044-101 8 dpi
DPI_8 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\IgG Whole Virion ELISA Raw Data (044-101).xlsx", sheet = 3, col_names = TRUE)
DPI_8$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_8$Log_Reciprocal_Serum_Dilution)
str(DPI_8)
model_DPI_8 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_8, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_8, type = "all")
summary(model_DPI_8)
ED(model_DPI_8, c(10,50), interval = "delta")
newdata_DPI_8 <- expand.grid(DF = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_8 <- predict(model_DPI_8, newdata = newdata_DPI_8, interval = "confidence")
newdata_DPI_8$p <- pm_DPI_8[,1]
newdata_DPI_8$pmin <- pm_DPI_8[,2]
newdata_DPI_8$pmax <- pm_DPI_8[,3]
#044-101 17 dpi
DPI_17 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\IgG Whole Virion ELISA Raw Data (044-101).xlsx", sheet = 4, col_names = TRUE)
DPI_17$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_17$Log_Reciprocal_Serum_Dilution)
str(DPI_17)
model_DPI_17 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_17, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_17, type = "all")
summary(model_DPI_17)
ED(model_DPI_17, c(10,50), interval = "delta")
newdata_DPI_17 <- expand.grid(DF = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_17 <- predict(model_DPI_17, newdata = newdata_DPI_17, interval = "confidence")
newdata_DPI_17$p <- pm_DPI_17[,1]
newdata_DPI_17$pmin <- pm_DPI_17[,2]
newdata_DPI_17$pmax <- pm_DPI_17[,3]
#044-101 28 dpi
DPI_28 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\IgG Whole Virion ELISA Raw Data (044-101).xlsx", sheet = 5, col_names = TRUE)
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
#044-101 56 dpi 
DPI_56 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\IgG Whole Virion ELISA Raw Data (044-101).xlsx", sheet = 6, col_names = TRUE)
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
#044-101 94 dpi
DPI_94 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\IgG Whole Virion ELISA Raw Data (044-101).xlsx", sheet = 7, col_names = TRUE)
DPI_94$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_94$Log_Reciprocal_Serum_Dilution)
str(DPI_94)
model_DPI_94 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_94, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_94, type = "all")
summary(model_DPI_94)
ED(model_DPI_94, c(10,50), interval = "delta")
newdata_DPI_94 <- expand.grid(DF = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_94 <- predict(model_DPI_94, newdata = newdata_DPI_94, interval = "confidence")
newdata_DPI_94$p <- pm_DPI_94[,1]
newdata_DPI_94$pmin <- pm_DPI_94[,2]
newdata_DPI_94$pmax <- pm_DPI_94[,3]
#044-101 112 dpi
DPI_112 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\IgG Whole Virion ELISA Raw Data (044-101).xlsx", sheet = 8, col_names = TRUE)
DPI_112$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_112$Log_Reciprocal_Serum_Dilution)
str(DPI_112)
model_DPI_112 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_112, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_112, type = "all")
summary(model_DPI_112)
ED(model_DPI_112, c(10,50), interval = "delta")
newdata_DPI_112 <- expand.grid(DF = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_112 <- predict(model_DPI_112, newdata = newdata_DPI_112, interval = "confidence")
newdata_DPI_112$p <- pm_DPI_112[,1]
newdata_DPI_112$pmin <- pm_DPI_112[,2]
newdata_DPI_112$pmax <- pm_DPI_112[,3]
####### 
#044-101 raw IgG Whole Virion ELISA binding curves
######
rawcurves_044_101 <- 
  ggplot() + 
  geom_point(data = DPI_0, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "0 DPI")) + geom_line(data = newdata_DPI_0, aes(x = DF, y = p, color = "0 DPI")) +
  geom_point(data = DPI_2, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "2 DPI")) + geom_line(data = newdata_DPI_2, aes(x = DF, y = p, color = "2 DPI")) +
  geom_point(data = DPI_8, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "8 DPI")) + geom_line(data = newdata_DPI_8, aes(x = DF, y = p, color = "8 DPI")) +
  geom_point(data = DPI_17, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "17 DPI")) + geom_line(data = newdata_DPI_17, aes(x = DF, y = p, color = "17 DPI")) +
  geom_point(data = DPI_28, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "28 DPI")) + geom_line(data = newdata_DPI_28, aes(x = DF, y = p, color = "28 DPI")) +
  geom_point(data = DPI_56, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "56 DPI")) + geom_line(data = newdata_DPI_56, aes(x = DF, y = p, color = "56 DPI")) +
  geom_point(data = DPI_94, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "94 DPI")) + geom_line(data = newdata_DPI_94, aes(x = DF, y = p, color = "94 DPI")) +
  geom_point(data = DPI_112, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "112 DPI")) + geom_line(data = newdata_DPI_112, aes(x = DF, y = p, color = "112 DPI"))
rawcurves_044_101 + theme_classic() + labs(x = "Log(reciprocal serum dilution)", y = "OD450 Reading", title = "IgG WVE Raw Binding Curves (044-101)") + xlim(1,6) + scale_color_discrete(limits = c("0 DPI", "2 DPI", "8 DPI", "17 DPI", "28 DPI", "56 DPI", "94 DPI", "112 DPI")) + guides(color = guide_legend(title = "Days Post Infection"))
