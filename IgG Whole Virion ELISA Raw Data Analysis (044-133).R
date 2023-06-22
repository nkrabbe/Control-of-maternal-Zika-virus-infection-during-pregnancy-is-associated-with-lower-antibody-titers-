library(tidyverse)
library(drc)
library(readxl)
library(ggplot2)
library(scales)
###
# Using raw OD450 readings to generate a 4-parameter dose-response model and estimate the dilution of serum
# to reduce the maximum OD450 readings by 90% and 50% (EC90 and EC50) for each timepoint tested.
###

#044-133 0 dpi
DPI_0 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\IgG Whole Virion ELISA Raw Data (044-133).xlsx", sheet = 1, col_names = TRUE)
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
#044-133 2 dpi
DPI_2 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\IgG Whole Virion ELISA Raw Data (044-133).xlsx", sheet = 2, col_names = TRUE)
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
#044-133 8 dpi
DPI_8 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\IgG Whole Virion ELISA Raw Data (044-133).xlsx", sheet = 3, col_names = TRUE)
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
#044-133 15 dpi
DPI_15 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\IgG Whole Virion ELISA Raw Data (044-133).xlsx", sheet = 4, col_names = TRUE)
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
#044-133 22 dpi
DPI_22 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\IgG Whole Virion ELISA Raw Data (044-133).xlsx", sheet = 5, col_names = TRUE)
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
#044-133 29 dpi 
DPI_29 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\IgG Whole Virion ELISA Raw Data (044-133).xlsx", sheet = 6, col_names = TRUE)
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
#044-133 57 dpi
DPI_57 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\IgG Whole Virion ELISA Raw Data (044-133).xlsx", sheet = 7, col_names = TRUE)
DPI_57$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_57$Log_Reciprocal_Serum_Dilution)
str(DPI_57)
model_DPI_57 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_57, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_57, type = "all")
summary(model_DPI_57)
ED(model_DPI_57, c(10,50), interval = "delta")
newdata_DPI_57 <- expand.grid(DF = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_57 <- predict(model_DPI_57, newdata = newdata_DPI_57, interval = "confidence")
newdata_DPI_57$p <- pm_DPI_57[,1]
newdata_DPI_57$pmin <- pm_DPI_57[,2]
newdata_DPI_57$pmax <- pm_DPI_57[,3]
#044-133 92 dpi
DPI_92 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\IgG Whole Virion ELISA Raw Data (044-133).xlsx", sheet = 8, col_names = TRUE)
DPI_92$Log_Reciprocal_Serum_Dilution = as.numeric(DPI_92$Log_Reciprocal_Serum_Dilution)
str(DPI_92)
model_DPI_92 <- drm(OD450_Reading~Log_Reciprocal_Serum_Dilution, data = DPI_92, fct = LL.4(fixed = c(NA, NA, NA, NA), names = c("slope", "lower limit", "upper limit", "ED50")))
plot(model_DPI_92, type = "all")
summary(model_DPI_92)
ED(model_DPI_92, c(10,50), interval = "delta")
newdata_DPI_92 <- expand.grid(DF = exp(seq(log(1.09), log(5.31), length = 100)))
pm_DPI_92 <- predict(model_DPI_92, newdata = newdata_DPI_92, interval = "confidence")
newdata_DPI_92$p <- pm_DPI_92[,1]
newdata_DPI_92$pmin <- pm_DPI_92[,2]
newdata_DPI_92$pmax <- pm_DPI_92[,3]
#044-133 117 dpi
DPI_117 <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\WVE and PRNT\\IgG WVE (exp 1971)\\Final Raw Data, figures, and code\\IgG Whole Virion ELISA Raw Data (044-133).xlsx", sheet = 9, col_names = TRUE)
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
#044-133 raw IgG Whole Virion ELISA binding curves
######
rawcurves_044_133 <- 
  ggplot() + 
  geom_point(data = DPI_0, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "0 DPI")) + geom_line(data = newdata_DPI_0, aes(x = DF, y = p, color = "0 DPI")) +
  geom_point(data = DPI_2, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "2 DPI")) + geom_line(data = newdata_DPI_2, aes(x = DF, y = p, color = "2 DPI")) +
  geom_point(data = DPI_8, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "8 DPI")) + geom_line(data = newdata_DPI_8, aes(x = DF, y = p, color = "8 DPI")) +
  geom_point(data = DPI_15, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "15 DPI")) + geom_line(data = newdata_DPI_15, aes(x = DF, y = p, color = "15 DPI")) +
  geom_point(data = DPI_22, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "22 DPI")) + geom_line(data = newdata_DPI_22, aes(x = DF, y = p, color = "22 DPI")) +
  geom_point(data = DPI_29, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "29 DPI")) + geom_line(data = newdata_DPI_29, aes(x = DF, y = p, color = "29 DPI")) +
  geom_point(data = DPI_57, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "57 DPI")) + geom_line(data = newdata_DPI_57, aes(x = DF, y = p, color = "57 DPI")) +
  geom_point(data = DPI_92, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "92 DPI")) + geom_line(data = newdata_DPI_92, aes(x = DF, y = p, color = "92 DPI")) +
  geom_point(data = DPI_117, aes(x = Log_Reciprocal_Serum_Dilution, y = OD450_Reading, color = "117 DPI")) + geom_line(data = newdata_DPI_117, aes(x = DF, y = p, color = "117 DPI"))
rawcurves_044_133 + theme_classic() + labs(x = "Log(reciprocal serum dilution)", y = "OD450 Reading", title = "IgG WVE Raw Binding Curves (044-133)") + xlim(1,6) + scale_color_discrete(limits = c("0 DPI", "2 DPI", "8 DPI", "15 DPI", "22 DPI", "29 DPI", "57 DPI", "92 DPI", "117 DPI")) + guides(color = guide_legend(title = "Days Post Infection"))
