library(tidyverse)
library(drc)
library(readxl)
library(ggplot2)
library(scales)
###
# Using raw OD450 readings to generate a 4-parameter dose-response model and estimate the dilution of serum
# to reduce the maximum OD450 readings by 90% and 50% (EC90 and EC50) for each timepoint tested.
###

#044-102 0 dpi
DPI_0 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\IgG Whole Virion ELISA Raw Data (044-102).xlsx", sheet = 1, col_names = TRUE)
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
#044-102 4 dpi
DPI_4 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\IgG Whole Virion ELISA Raw Data (044-102).xlsx", sheet = 2, col_names = TRUE)
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
#044-102 7 dpi
DPI_7 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\IgG Whole Virion ELISA Raw Data (044-102).xlsx", sheet = 3, col_names = TRUE)
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
#044-102 15 dpi
DPI_15 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\IgG Whole Virion ELISA Raw Data (044-102).xlsx", sheet = 4, col_names = TRUE)
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
#044-102 24 dpi
DPI_24 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\IgG Whole Virion ELISA Raw Data (044-102).xlsx", sheet = 5, col_names = TRUE)
DPI_24$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_24$Log_Reciprocal_Serum_Dilution)
str(DPI_24)
model_DPI_24 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_24, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_24, type = "all")
summary(model_DPI_24)
ED(model_DPI_24, c(10,50), interval = "delta")
newdata_DPI_24 <- expand.grid(DF = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_24 <- predict(model_DPI_24, newdata = newdata_DPI_24, interval = "confidence")
newdata_DPI_24$p <- pm_DPI_24[,1]
newdata_DPI_24$pmin <- pm_DPI_24[,2]
newdata_DPI_24$pmax <- pm_DPI_24[,3]
#044-102 28 dpi 
DPI_28 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\IgG Whole Virion ELISA Raw Data (044-102).xlsx", sheet = 6, col_names = TRUE)
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
#044-102 66 dpi
DPI_66 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\IgG Whole Virion ELISA Raw Data (044-102).xlsx", sheet = 7, col_names = TRUE)
DPI_66$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_66$Log_Reciprocal_Serum_Dilution)
str(DPI_66)
model_DPI_66 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_66, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_66, type = "all")
summary(model_DPI_66)
ED(model_DPI_66, c(10,50), interval = "delta")
newdata_DPI_66 <- expand.grid(DF = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_66 <- predict(model_DPI_66, newdata = newdata_DPI_66, interval = "confidence")
newdata_DPI_66$p <- pm_DPI_66[,1]
newdata_DPI_66$pmin <- pm_DPI_66[,2]
newdata_DPI_66$pmax <- pm_DPI_66[,3]
#044-102 91 dpi
DPI_91 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\IgG Whole Virion ELISA Raw Data (044-102).xlsx", sheet = 8, col_names = TRUE)
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
#044-102 115 dpi
DPI_115 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\IgG Whole Virion ELISA Raw Data (044-102).xlsx", sheet = 9, col_names = TRUE)
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

####### 
#044-102 raw IgG Whole Virion ELISA binding curves
######
rawcurves_044_102 <- 
  ggplot() + 
  geom_point(data = DPI_0, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "0 DPI")) + geom_line(data = newdata_DPI_0, aes(x = DF, y = p, color = "0 DPI")) +
  geom_point(data = DPI_4, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "4 DPI")) + geom_line(data = newdata_DPI_4, aes(x = DF, y = p, color = "4 DPI")) +
  geom_point(data = DPI_7, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "7 DPI")) + geom_line(data = newdata_DPI_7, aes(x = DF, y = p, color = "7 DPI")) +
  geom_point(data = DPI_15, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "15 DPI")) + geom_line(data = newdata_DPI_15, aes(x = DF, y = p, color = "15 DPI")) +
  geom_point(data = DPI_24, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "24 DPI")) + geom_line(data = newdata_DPI_24, aes(x = DF, y = p, color = "24 DPI")) +
  geom_point(data = DPI_28, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "28 DPI")) + geom_line(data = newdata_DPI_28, aes(x = DF, y = p, color = "28 DPI")) +
  geom_point(data = DPI_66, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "66 DPI")) + geom_line(data = newdata_DPI_66, aes(x = DF, y = p, color = "66 DPI")) +
  geom_point(data = DPI_91, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "91 DPI")) + geom_line(data = newdata_DPI_91, aes(x = DF, y = p, color = "91 DPI")) +
  geom_point(data = DPI_115, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "115 DPI")) + geom_line(data = newdata_DPI_115, aes(x = DF, y = p, color = "115 DPI"))
rawcurves_044_102 + theme_classic() + labs(x = "Log(reciprocal serum dilution)", y = "OD450 Reading", title = "IgG WVE Raw Binding Curves (044-102)") + xlim(1,6) + scale_color_discrete(limits = c("0 DPI", "4 DPI", "7 DPI", "15 DPI", "24 DPI", "28 DPI", "66 DPI", "91 DPI", "115 DPI")) + guides(color = guide_legend(title = "Days Post Infection"))
