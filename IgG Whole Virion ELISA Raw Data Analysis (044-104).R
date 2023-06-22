library(tidyverse)
library(drc)
library(readxl)
library(ggplot2)
library(scales)
###
# Using raw OD450 readings to generate a 4-parameter dose-response model and estimate the dilution of serum
# to reduce the maximum OD450 readings by 90% and 50% (EC90 and EC50) for each timepoint tested.
###

#044-104 10 dpi
DPI_10 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\IgG Whole Virion ELISA Raw Data (044-104).xlsx", sheet = 1, col_names = TRUE)
DPI_10$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_10$Log_Reciprocal_Serum_Dilution)
str(DPI_10)
model_DPI_10 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_10, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_10, type = "all")
summary(model_DPI_10)
ED(model_DPI_10, c(10,50), interval = "delta")
newdata_DPI_10 <- expand.grid(DF = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_10 <- predict(model_DPI_10, newdata = newdata_DPI_10, interval = "confidence")
newdata_DPI_10$p <- pm_DPI_10[,1]
newdata_DPI_10$pmin <- pm_DPI_10[,2]
newdata_DPI_10$pmax <- pm_DPI_10[,3]
#044-104 15 dpi
DPI_15 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\IgG Whole Virion ELISA Raw Data (044-104).xlsx", sheet = 2, col_names = TRUE)
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
#044-104 22 dpi
DPI_22 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\IgG Whole Virion ELISA Raw Data (044-104).xlsx", sheet = 3, col_names = TRUE)
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
#044-104 29 dpi
DPI_29 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\IgG Whole Virion ELISA Raw Data (044-104).xlsx", sheet = 4, col_names = TRUE)
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
#044-104 59 dpi
DPI_59 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\IgG Whole Virion ELISA Raw Data (044-104).xlsx", sheet = 5, col_names = TRUE)
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
#044-104 87 dpi 
DPI_87 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\IgG Whole Virion ELISA Raw Data (044-104).xlsx", sheet = 6, col_names = TRUE)
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
#044-104 114 dpi
DPI_114 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\IgG Whole Virion ELISA Raw Data (044-104).xlsx", sheet = 7, col_names = TRUE)
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
#044-104 raw IgG Whole Virion ELISA binding curves
######
rawcurves_044_104 <- 
  ggplot() + 
  geom_point(data = DPI_10, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "10 DPI")) + geom_line(data = newdata_DPI_10, aes(x = DF, y = p, color = "10 DPI")) +
  geom_point(data = DPI_15, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "15 DPI")) + geom_line(data = newdata_DPI_15, aes(x = DF, y = p, color = "15 DPI")) +
  geom_point(data = DPI_22, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "22 DPI")) + geom_line(data = newdata_DPI_22, aes(x = DF, y = p, color = "22 DPI")) +
  geom_point(data = DPI_29, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "29 DPI")) + geom_line(data = newdata_DPI_29, aes(x = DF, y = p, color = "29 DPI")) +
  geom_point(data = DPI_59, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "59 DPI")) + geom_line(data = newdata_DPI_59, aes(x = DF, y = p, color = "59 DPI")) +
  geom_point(data = DPI_87, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "87 DPI")) + geom_line(data = newdata_DPI_87, aes(x = DF, y = p, color = "87 DPI")) +
  geom_point(data = DPI_114, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "114 DPI")) + geom_line(data = newdata_DPI_114, aes(x = DF, y = p, color = "114 DPI"))
rawcurves_044_104 + theme_classic() + labs(x = "Log(reciprocal serum dilution)", y = "OD450 Reading", title = "IgG WVE Raw Binding Curves (044-104)") + xlim(1,6) + scale_color_discrete(limits = c("10 DPI", "15 DPI", "22 DPI", "29 DPI", "59 DPI", "87 DPI", "114 DPI")) + guides(color = guide_legend(title = "Days Post Infection"))
