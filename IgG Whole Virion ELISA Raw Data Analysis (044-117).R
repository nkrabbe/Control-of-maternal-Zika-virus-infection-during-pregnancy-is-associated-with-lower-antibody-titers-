library(tidyverse)
library(drc)
library(readxl)
library(ggplot2)
library(scales)
###
# Using raw OD450 readings to generate a 4-parameter dose-response model and estimate the dilution of serum
# to reduce the maximum OD450 readings by 90% and 50% (EC90 and EC50) for each timepoint tested.
###

#044-117 0 dpi
DPI_0 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\IgG Whole Virion ELISA Raw Data (044-117).xlsx", sheet = 1, col_names = TRUE)
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
#044-117 4 dpi
DPI_4 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\IgG Whole Virion ELISA Raw Data (044-117).xlsx", sheet = 2, col_names = TRUE)
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
#044-117 8 dpi
DPI_8 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\IgG Whole Virion ELISA Raw Data (044-117).xlsx", sheet = 3, col_names = TRUE)
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
#044-117 15 dpi
DPI_15 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\IgG Whole Virion ELISA Raw Data (044-117).xlsx", sheet = 4, col_names = TRUE)
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
#044-117 22 dpi
DPI_22 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\IgG Whole Virion ELISA Raw Data (044-117).xlsx", sheet = 5, col_names = TRUE)
DPI_22$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_22$Log_Reciprocal_Serum_Dilution)
str(DPI_22)
model_DPI_22 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_22, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_22, type = "all")
summary(model_DPI_22)
ED(model_DPI_22, c(10,50), interval = "delta")
newdata_DPI_22 <- expand.grid(DF = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_22 <- predict(model_DPI_22, newdata = newdata_DPI_22, interval = "confidence")
newdata_DPI_22$p <- pm_DPI_22[,1]
newdata_DPI_22$pmin <- pm_DPI_22[,2]
newdata_DPI_22$pmax <- pm_DPI_22[,3]
#044-117 33 dpi 
DPI_33 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\IgG Whole Virion ELISA Raw Data (044-117).xlsx", sheet = 6, col_names = TRUE)
DPI_33$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_33$Log_Reciprocal_Serum_Dilution)
str(DPI_33)
model_DPI_33 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_33, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_33, type = "all")
summary(model_DPI_33)
ED(model_DPI_33, c(10,50), interval = "delta")
newdata_DPI_33 <- expand.grid(DF = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_33 <- predict(model_DPI_33, newdata = newdata_DPI_33, interval = "confidence")
newdata_DPI_33$p <- pm_DPI_33[,1]
newdata_DPI_33$pmin <- pm_DPI_33[,2]
newdata_DPI_33$pmax <- pm_DPI_33[,3]
#044-117 125 dpi
DPI_125 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\IgG Whole Virion ELISA Raw Data (044-117).xlsx", sheet = 7, col_names = TRUE)
DPI_125$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_125$Log_Reciprocal_Serum_Dilution)
str(DPI_125)
model_DPI_125 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_125, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_125, type = "all")
summary(model_DPI_125)
ED(model_DPI_125, c(10,50), interval = "delta")
newdata_DPI_125 <- expand.grid(DF = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_125 <- predict(model_DPI_125, newdata = newdata_DPI_125, interval = "confidence")
newdata_DPI_125$p <- pm_DPI_125[,1]
newdata_DPI_125$pmin <- pm_DPI_125[,2]
newdata_DPI_125$pmax <- pm_DPI_125[,3]
####### 
#044-117 raw IgG Whole Virion ELISA binding curves
######
rawcurves_044_117 <- 
  ggplot() + 
  geom_point(data = DPI_0, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "0 DPI")) + geom_line(data = newdata_DPI_0, aes(x = DF, y = p, color = "0 DPI")) +
  geom_point(data = DPI_4, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "4 DPI")) + geom_line(data = newdata_DPI_4, aes(x = DF, y = p, color = "4 DPI")) +
  geom_point(data = DPI_8, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "8 DPI")) + geom_line(data = newdata_DPI_8, aes(x = DF, y = p, color = "8 DPI")) +
  geom_point(data = DPI_15, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "15 DPI")) + geom_line(data = newdata_DPI_15, aes(x = DF, y = p, color = "15 DPI")) +
  geom_point(data = DPI_22, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "22 DPI")) + geom_line(data = newdata_DPI_22, aes(x = DF, y = p, color = "22 DPI")) +
  geom_point(data = DPI_33, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "33 DPI")) + geom_line(data = newdata_DPI_33, aes(x = DF, y = p, color = "33 DPI")) +
  geom_point(data = DPI_125, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "125 DPI")) + geom_line(data = newdata_DPI_125, aes(x = DF, y = p, color = "125 DPI"))
rawcurves_044_117 + theme_classic() + labs(x = "Log(reciprocal serum dilution)", y = "OD450 Reading", title = "IgG WVE Raw Binding Curves (044-117)") + xlim(1,6) + scale_color_discrete(limits = c("0 DPI", "4 DPI", "8 DPI", "15 DPI", "22 DPI", "33 DPI", "125 DPI")) + guides(color = guide_legend(title = "Days Post Infection"))
