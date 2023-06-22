library(tidyverse)
library(drc)
library(readxl)
library(ggplot2)
library(scales)
###
# Using raw OD450 readings to generate a 4-parameter dose-response model and estimate the dilution of serum
# to reduce the maximum OD450 readings by 90% and 50% (EC90 and EC50) for each timepoint tested.
###

#044-127 0 dpi
DPI_0 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\IgG Whole Virion ELISA Raw Data (044-127).xlsx", sheet = 1, col_names = TRUE)
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
#044-127 2 dpi
DPI_2 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\IgG Whole Virion ELISA Raw Data (044-127).xlsx", sheet = 2, col_names = TRUE)
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
#044-127 8 dpi
DPI_8 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\IgG Whole Virion ELISA Raw Data (044-127).xlsx", sheet = 3, col_names = TRUE)
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
#044-127 14 dpi
DPI_14 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\IgG Whole Virion ELISA Raw Data (044-127).xlsx", sheet = 4, col_names = TRUE)
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
#044-127 21 dpi
DPI_21 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\IgG Whole Virion ELISA Raw Data (044-127).xlsx", sheet = 5, col_names = TRUE)
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
#044-127 31 dpi 
DPI_31 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\IgG Whole Virion ELISA Raw Data (044-127).xlsx", sheet = 6, col_names = TRUE)
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
#044-127 52 dpi
DPI_52 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\IgG Whole Virion ELISA Raw Data (044-127).xlsx", sheet = 7, col_names = TRUE)
DPI_52$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_52$Log_Reciprocal_Serum_Dilution)
str(DPI_52)
model_DPI_52 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_52, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_52, type = "all")
summary(model_DPI_52)
ED(model_DPI_52, c(10,50), interval = "delta")
newdata_DPI_52 <- expand.grid(DF = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_52 <- predict(model_DPI_52, newdata = newdata_DPI_52, interval = "confidence")
newdata_DPI_52$p <- pm_DPI_52[,1]
newdata_DPI_52$pmin <- pm_DPI_52[,2]
newdata_DPI_52$pmax <- pm_DPI_52[,3]
#044-127 91 dpi
DPI_91 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\IgG Whole Virion ELISA Raw Data (044-127).xlsx", sheet = 8, col_names = TRUE)
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
#044-127 112 dpi
DPI_112 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\IgG Whole Virion ELISA Raw Data (044-127).xlsx", sheet = 9, col_names = TRUE)
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
#044-127 raw IgG Whole Virion ELISA binding curves
######
rawcurves_044_127 <- 
  ggplot() + 
  geom_point(data = DPI_0, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "0 DPI")) + geom_line(data = newdata_DPI_0, aes(x = DF, y = p, color = "0 DPI")) +
  geom_point(data = DPI_2, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "2 DPI")) + geom_line(data = newdata_DPI_2, aes(x = DF, y = p, color = "2 DPI")) +
  geom_point(data = DPI_8, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "8 DPI")) + geom_line(data = newdata_DPI_8, aes(x = DF, y = p, color = "8 DPI")) +
  geom_point(data = DPI_14, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "14 DPI")) + geom_line(data = newdata_DPI_14, aes(x = DF, y = p, color = "14 DPI")) +
  geom_point(data = DPI_21, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "21 DPI")) + geom_line(data = newdata_DPI_21, aes(x = DF, y = p, color = "21 DPI")) +
  geom_point(data = DPI_31, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "31 DPI")) + geom_line(data = newdata_DPI_31, aes(x = DF, y = p, color = "31 DPI")) +
  geom_point(data = DPI_52, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "52 DPI")) + geom_line(data = newdata_DPI_52, aes(x = DF, y = p, color = "52 DPI")) +
  geom_point(data = DPI_91, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "91 DPI")) + geom_line(data = newdata_DPI_91, aes(x = DF, y = p, color = "91 DPI")) +
  geom_point(data = DPI_112, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "112 DPI")) + geom_line(data = newdata_DPI_112, aes(x = DF, y = p, color = "112 DPI"))
rawcurves_044_127 + theme_classic() + labs(x = "Log(reciprocal serum dilution)", y = "OD450 Reading", title = "IgG WVE Raw Binding Curves (044-127)") + xlim(1,6) + scale_color_discrete(limits = c("0 DPI", "2 DPI", "8 DPI", "14 DPI", "21 DPI", "31 DPI", "52 DPI", "91 DPI", "112 DPI")) + guides(color = guide_legend(title = "Days Post Infection"))
