library(tidyverse)
library(drc)
library(readxl)
library(ggplot2)
library(scales)
###
# Using raw OD450 readings to generate a 4-parameter dose-response model and estimate the dilution of serum
# to reduce the maximum OD450 readings by 90% and 50% (EC90 and EC50) for each timepoint tested.
###

#044-103 -6 dpi
DPI_0 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\IgG Whole Virion ELISA Raw Data (044-103).xlsx", sheet = 1, col_names = TRUE)
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
#044-103 4 dpi
DPI_4 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\IgG Whole Virion ELISA Raw Data (044-103).xlsx", sheet = 2, col_names = TRUE)
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
#044-103 7 dpi
DPI_7 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\IgG Whole Virion ELISA Raw Data (044-103).xlsx", sheet = 3, col_names = TRUE)
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
#044-103 15 dpi
DPI_15 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\IgG Whole Virion ELISA Raw Data (044-103).xlsx", sheet = 4, col_names = TRUE)
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
#044-103 22 dpi
DPI_22 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\IgG Whole Virion ELISA Raw Data (044-103).xlsx", sheet = 5, col_names = TRUE)
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
#044-103 35 dpi 
DPI_35 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\IgG Whole Virion ELISA Raw Data (044-103).xlsx", sheet = 6, col_names = TRUE)
DPI_35$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_35$Log_Reciprocal_Serum_Dilution)
str(DPI_35)
model_DPI_35 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_35, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_35, type = "all")
summary(model_DPI_35)
ED(model_DPI_35, c(10,50), interval = "delta")
newdata_DPI_35 <- expand.grid(DF = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_35 <- predict(model_DPI_35, newdata = newdata_DPI_35, interval = "confidence")
newdata_DPI_35$p <- pm_DPI_35[,1]
newdata_DPI_35$pmin <- pm_DPI_35[,2]
newdata_DPI_35$pmax <- pm_DPI_35[,3]
#044-103 52 dpi
DPI_52 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\IgG Whole Virion ELISA Raw Data (044-103).xlsx", sheet = 7, col_names = TRUE)
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
#044-103 87 dpi
DPI_87 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\IgG Whole Virion ELISA Raw Data (044-103).xlsx", sheet = 8, col_names = TRUE)
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
#044-103 114 dpi
DPI_114 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\IgG Whole Virion ELISA Raw Data (044-103).xlsx", sheet = 9, col_names = TRUE)
DPI_114$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_114$Log_Reciprocal_Serum_Dilution)
str(DPI_114)
model_DPI_114 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_114, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_114, type = "all")
summary(model_DPI_114)
ED(model_DPI_114, c(10,50), interval = "delta")
newdata_DPI_114 <- expand.grid(DF = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_114 <- predict(model_DPI_114, newdata = newdata_DPI_114, interval = "confidence")
newdata_DPI_114$p <- pm_DPI_114[,1]
newdata_DPI_114$pmin <- pm_DPI_114[,2]
newdata_DPI_114$pmax <- pm_DPI_114[,3]

####### 
#044-103 raw IgG Whole Virion ELISA binding curves
######
rawcurves_044_103 <- 
  ggplot() + 
  geom_point(data = DPI_0, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "-6 DPI")) + geom_line(data = newdata_DPI_0, aes(x = DF, y = p, color = "-6 DPI")) +
  geom_point(data = DPI_4, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "4 DPI")) + geom_line(data = newdata_DPI_4, aes(x = DF, y = p, color = "4 DPI")) +
  geom_point(data = DPI_7, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "7 DPI")) + geom_line(data = newdata_DPI_7, aes(x = DF, y = p, color = "7 DPI")) +
  geom_point(data = DPI_15, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "15 DPI")) + geom_line(data = newdata_DPI_15, aes(x = DF, y = p, color = "15 DPI")) +
  geom_point(data = DPI_22, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "22 DPI")) + geom_line(data = newdata_DPI_22, aes(x = DF, y = p, color = "22 DPI")) +
  geom_point(data = DPI_35, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "35 DPI")) + geom_line(data = newdata_DPI_35, aes(x = DF, y = p, color = "35 DPI")) +
  geom_point(data = DPI_52, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "52 DPI")) + geom_line(data = newdata_DPI_52, aes(x = DF, y = p, color = "52 DPI")) +
  geom_point(data = DPI_87, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "87 DPI")) + geom_line(data = newdata_DPI_87, aes(x = DF, y = p, color = "87 DPI")) +
  geom_point(data = DPI_114, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "114 DPI")) + geom_line(data = newdata_DPI_114, aes(x = DF, y = p, color = "114 DPI"))
rawcurves_044_103 + theme_classic() + labs(x = "Log(reciprocal serum dilution)", y = "OD450 Reading", title = "IgG WVE Raw Binding Curves (044-103)") + xlim(1,6) + scale_color_discrete(limits = c("-6 DPI", "4 DPI", "7 DPI", "15 DPI", "22 DPI", "35 DPI", "52 DPI", "87 DPI", "114 DPI")) + guides(color = guide_legend(title = "Days Post Infection"))
