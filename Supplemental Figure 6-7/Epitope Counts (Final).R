library(readxl)
library(ggplot2)
library(tidyverse)
library(ggpubr)
library(gtable)

epitope_counts_final <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\Peptide Array\\Final Raw Data, figures, and code\\Peptide Array-Epitope Counts (Final).xlsx", sheet= 2, col_names = TRUE)
epitope_counts_final$Virologic_Control_Group = as.character(epitope_counts_final$Virologic_Control_Group)
epitope_counts_final$Virologic_Control_Group <- factor(epitope_counts_final$Virologic_Control_Group, levels = c('Controller', 'Non-Controller'))
epitope_counts_final$ZIKV_protein = as.character(epitope_counts_final$ZIKV_protein)
epitope_counts_final$ZIKV_protein <- factor(epitope_counts_final$ZIKV_protein, levels = c('Capsid', 'PrM', 'M', 'E', 'NS1', 'NS2A', 'NS2B', 'NS3', 'NS4A', 'NS4B', 'NS5', 'Total'))
epitope_counts_final$Epitope_Count = as.numeric(epitope_counts_final$Epitope_Count)

###
#supplemental figure comparing epitope counts between virologic control groups within individual 
#regions of the ZIKV polyprotein
###

###IgM

## 13-17 days post-infection 
epitope_counts_final_IgM_13.17dpi <- filter(epitope_counts_final, Timepoint == '13-17',
                                        ZIKV_protein == 'Capsid' |
                                          ZIKV_protein == 'PrM' |
                                          ZIKV_protein == 'M' |
                                          ZIKV_protein == 'E' |
                                          ZIKV_protein == 'NS1' |
                                          ZIKV_protein == 'NS2A' |
                                          ZIKV_protein == 'NS2B' |
                                          ZIKV_protein == 'NS3' |
                                          ZIKV_protein == 'NS4A' |
                                          ZIKV_protein == 'NS4B' |
                                          ZIKV_protein == 'NS5')
# Statistical analyses comparing epitope counts within each region of the ZIKV polyprotein between virologic control groups (Mann-Whitney U-Tests)
#Capsid
epitope_counts_final_IgM_13.17dpi_capsid <- filter(epitope_counts_final_IgM_13.17dpi, ZIKV_protein == 'Capsid')
epitope_counts_final_IgM_13.17dpi_capsid_controller <- filter(epitope_counts_final_IgM_13.17dpi_capsid, Virologic_Control_Group == 'Controller')
epitope_counts_final_IgM_13.17dpi_capsid_controller_quantile <- quantile(epitope_counts_final_IgM_13.17dpi_capsid_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgM_13.17dpi_capsid_controller_quantile
epitope_counts_final_IgM_13.17dpi_capsid_non_controller <- filter(epitope_counts_final_IgM_13.17dpi_capsid, Virologic_Control_Group == 'Non-Controller')
epitope_counts_final_IgM_13.17dpi_capsid_non_controller_quantile <- quantile(epitope_counts_final_IgM_13.17dpi_capsid_non_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgM_13.17dpi_capsid_non_controller_quantile
wilcox.test(Epitope_Count~Virologic_Control_Group, data = epitope_counts_final_IgM_13.17dpi_capsid, exact = FALSE)
#Pre-membrane (PrM)
epitope_counts_final_IgM_13.17dpi_PrM <- filter(epitope_counts_final_IgM_13.17dpi, ZIKV_protein == 'PrM')
epitope_counts_final_IgM_13.17dpi_PrM_controller <- filter(epitope_counts_final_IgM_13.17dpi_PrM, Virologic_Control_Group == 'Controller')
epitope_counts_final_IgM_13.17dpi_PrM_controller_quantile <- quantile(epitope_counts_final_IgM_13.17dpi_PrM_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgM_13.17dpi_PrM_controller_quantile
epitope_counts_final_IgM_13.17dpi_PrM_non_controller <- filter(epitope_counts_final_IgM_13.17dpi_PrM, Virologic_Control_Group == 'Non-Controller')
epitope_counts_final_IgM_13.17dpi_PrM_non_controller_quantile <- quantile(epitope_counts_final_IgM_13.17dpi_PrM_non_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgM_13.17dpi_PrM_non_controller_quantile
wilcox.test(Epitope_Count~Virologic_Control_Group, data = epitope_counts_final_IgM_13.17dpi_PrM, exact = FALSE)
#Membrane (M)
epitope_counts_final_IgM_13.17dpi_M <- filter(epitope_counts_final_IgM_13.17dpi, ZIKV_protein == 'M')
epitope_counts_final_IgM_13.17dpi_M_controller <- filter(epitope_counts_final_IgM_13.17dpi_M, Virologic_Control_Group == 'Controller')
epitope_counts_final_IgM_13.17dpi_M_controller_quantile <- quantile(epitope_counts_final_IgM_13.17dpi_M_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgM_13.17dpi_M_controller_quantile
epitope_counts_final_IgM_13.17dpi_M_non_controller <- filter(epitope_counts_final_IgM_13.17dpi_M, Virologic_Control_Group == 'Non-Controller')
epitope_counts_final_IgM_13.17dpi_M_non_controller_quantile <- quantile(epitope_counts_final_IgM_13.17dpi_M_non_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgM_13.17dpi_M_non_controller_quantile
wilcox.test(Epitope_Count~Virologic_Control_Group, data = epitope_counts_final_IgM_13.17dpi_M, exact = FALSE)
#Envelope (E)
epitope_counts_final_IgM_13.17dpi_E <- filter(epitope_counts_final_IgM_13.17dpi, ZIKV_protein == 'E')
epitope_counts_final_IgM_13.17dpi_E_controller <- filter(epitope_counts_final_IgM_13.17dpi_E, Virologic_Control_Group == 'Controller')
epitope_counts_final_IgM_13.17dpi_E_controller_quantile <- quantile(epitope_counts_final_IgM_13.17dpi_E_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgM_13.17dpi_E_controller_quantile
epitope_counts_final_IgM_13.17dpi_E_non_controller <- filter(epitope_counts_final_IgM_13.17dpi_E, Virologic_Control_Group == 'Non-Controller')
epitope_counts_final_IgM_13.17dpi_E_non_controller_quantile <- quantile(epitope_counts_final_IgM_13.17dpi_E_non_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgM_13.17dpi_E_non_controller_quantile
wilcox.test(Epitope_Count~Virologic_Control_Group, data = epitope_counts_final_IgM_13.17dpi_E, exact = FALSE)
#NS1 
epitope_counts_final_IgM_13.17dpi_NS1 <- filter(epitope_counts_final_IgM_13.17dpi, ZIKV_protein == 'NS1')
epitope_counts_final_IgM_13.17dpi_NS1_controller <- filter(epitope_counts_final_IgM_13.17dpi_NS1, Virologic_Control_Group == 'Controller')
epitope_counts_final_IgM_13.17dpi_NS1_controller_quantile <- quantile(epitope_counts_final_IgM_13.17dpi_NS1_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgM_13.17dpi_NS1_controller_quantile
epitope_counts_final_IgM_13.17dpi_NS1_non_controller <- filter(epitope_counts_final_IgM_13.17dpi_NS1, Virologic_Control_Group == 'Non-Controller')
epitope_counts_final_IgM_13.17dpi_NS1_non_controller_quantile <- quantile(epitope_counts_final_IgM_13.17dpi_NS1_non_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgM_13.17dpi_NS1_non_controller_quantile
wilcox.test(Epitope_Count~Virologic_Control_Group, data = epitope_counts_final_IgM_13.17dpi_NS1, exact = FALSE)
#NS2A
epitope_counts_final_IgM_13.17dpi_NS2A <- filter(epitope_counts_final_IgM_13.17dpi, ZIKV_protein == 'NS2A')
epitope_counts_final_IgM_13.17dpi_NS2A_controller <- filter(epitope_counts_final_IgM_13.17dpi_NS2A, Virologic_Control_Group == 'Controller')
epitope_counts_final_IgM_13.17dpi_NS2A_controller_quantile <- quantile(epitope_counts_final_IgM_13.17dpi_NS2A_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgM_13.17dpi_NS2A_controller_quantile
epitope_counts_final_IgM_13.17dpi_NS2A_non_controller <- filter(epitope_counts_final_IgM_13.17dpi_NS2A, Virologic_Control_Group == 'Non-Controller')
epitope_counts_final_IgM_13.17dpi_NS2A_non_controller_quantile <- quantile(epitope_counts_final_IgM_13.17dpi_NS2A_non_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgM_13.17dpi_NS2A_non_controller_quantile
wilcox.test(Epitope_Count~Virologic_Control_Group, data = epitope_counts_final_IgM_13.17dpi_NS2A, exact = FALSE)
#N2B
epitope_counts_final_IgM_13.17dpi_NS2B <- filter(epitope_counts_final_IgM_13.17dpi, ZIKV_protein == 'NS2B')
epitope_counts_final_IgM_13.17dpi_NS2B_controller <- filter(epitope_counts_final_IgM_13.17dpi_NS2B, Virologic_Control_Group == 'Controller')
epitope_counts_final_IgM_13.17dpi_NS2B_controller_quantile <- quantile(epitope_counts_final_IgM_13.17dpi_NS2B_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgM_13.17dpi_NS2B_controller_quantile
epitope_counts_final_IgM_13.17dpi_NS2B_non_controller <- filter(epitope_counts_final_IgM_13.17dpi_NS2B, Virologic_Control_Group == 'Non-Controller')
epitope_counts_final_IgM_13.17dpi_NS2B_non_controller_quantile <- quantile(epitope_counts_final_IgM_13.17dpi_NS2B_non_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgM_13.17dpi_NS2B_non_controller_quantile
wilcox.test(Epitope_Count~Virologic_Control_Group, data = epitope_counts_final_IgM_13.17dpi_NS2B, exact = FALSE)
#NS3
epitope_counts_final_IgM_13.17dpi_NS3 <- filter(epitope_counts_final_IgM_13.17dpi, ZIKV_protein == 'NS3')
epitope_counts_final_IgM_13.17dpi_NS3_controller <- filter(epitope_counts_final_IgM_13.17dpi_NS3, Virologic_Control_Group == 'Controller')
epitope_counts_final_IgM_13.17dpi_NS3_controller_quantile <- quantile(epitope_counts_final_IgM_13.17dpi_NS3_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgM_13.17dpi_NS3_controller_quantile
epitope_counts_final_IgM_13.17dpi_NS3_non_controller <- filter(epitope_counts_final_IgM_13.17dpi_NS3, Virologic_Control_Group == 'Non-Controller')
epitope_counts_final_IgM_13.17dpi_NS3_non_controller_quantile <- quantile(epitope_counts_final_IgM_13.17dpi_NS3_non_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgM_13.17dpi_NS3_non_controller_quantile
wilcox.test(Epitope_Count~Virologic_Control_Group, data = epitope_counts_final_IgM_13.17dpi_NS3, exact = FALSE)
#NS4A
epitope_counts_final_IgM_13.17dpi_NS4A <- filter(epitope_counts_final_IgM_13.17dpi, ZIKV_protein == 'NS4A')
epitope_counts_final_IgM_13.17dpi_NS4A_controller <- filter(epitope_counts_final_IgM_13.17dpi_NS4A, Virologic_Control_Group == 'Controller')
epitope_counts_final_IgM_13.17dpi_NS4A_controller_quantile <- quantile(epitope_counts_final_IgM_13.17dpi_NS4A_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgM_13.17dpi_NS4A_controller_quantile
epitope_counts_final_IgM_13.17dpi_NS4A_non_controller <- filter(epitope_counts_final_IgM_13.17dpi_NS4A, Virologic_Control_Group == 'Non-Controller')
epitope_counts_final_IgM_13.17dpi_NS4A_non_controller_quantile <- quantile(epitope_counts_final_IgM_13.17dpi_NS4A_non_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgM_13.17dpi_NS4A_non_controller_quantile
wilcox.test(Epitope_Count~Virologic_Control_Group, data = epitope_counts_final_IgM_13.17dpi_NS4A, exact = FALSE)
#NS4B
epitope_counts_final_IgM_13.17dpi_NS4B <- filter(epitope_counts_final_IgM_13.17dpi, ZIKV_protein == 'NS4B')
epitope_counts_final_IgM_13.17dpi_NS4B_controller <- filter(epitope_counts_final_IgM_13.17dpi_NS4B, Virologic_Control_Group == 'Controller')
epitope_counts_final_IgM_13.17dpi_NS4B_controller_quantile <- quantile(epitope_counts_final_IgM_13.17dpi_NS4B_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgM_13.17dpi_NS4B_controller_quantile
epitope_counts_final_IgM_13.17dpi_NS4B_non_controller <- filter(epitope_counts_final_IgM_13.17dpi_NS4B, Virologic_Control_Group == 'Non-Controller')
epitope_counts_final_IgM_13.17dpi_NS4B_non_controller_quantile <- quantile(epitope_counts_final_IgM_13.17dpi_NS4B_non_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgM_13.17dpi_NS4B_non_controller_quantile
wilcox.test(Epitope_Count~Virologic_Control_Group, data = epitope_counts_final_IgM_13.17dpi_NS4B, exact = FALSE)
#NS5
epitope_counts_final_IgM_13.17dpi_NS5 <- filter(epitope_counts_final_IgM_13.17dpi, ZIKV_protein == 'NS5')
epitope_counts_final_IgM_13.17dpi_NS5_controller <- filter(epitope_counts_final_IgM_13.17dpi_NS5, Virologic_Control_Group == 'Controller')
epitope_counts_final_IgM_13.17dpi_NS5_controller_quantile <- quantile(epitope_counts_final_IgM_13.17dpi_NS5_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgM_13.17dpi_NS5_controller_quantile
epitope_counts_final_IgM_13.17dpi_NS5_non_controller <- filter(epitope_counts_final_IgM_13.17dpi_NS5, Virologic_Control_Group == 'Non-Controller')
epitope_counts_final_IgM_13.17dpi_NS5_non_controller_quantile <- quantile(epitope_counts_final_IgM_13.17dpi_NS5_non_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgM_13.17dpi_NS5_non_controller_quantile
wilcox.test(Epitope_Count~Virologic_Control_Group, data = epitope_counts_final_IgM_13.17dpi_NS5, exact = FALSE)
#13-17 days post-infection IgM epitope counts (group) figure
epitope_counts_final_IgM_13.17dpi_group_figure <-
  ggplot() +
  geom_boxplot(data = epitope_counts_final_IgM_13.17dpi, aes(x= ZIKV_protein, y= Epitope_Count, color = Virologic_Control_Group), fill = 'white', outlier.colour =  'white') +
  geom_point(data = epitope_counts_final_IgM_13.17dpi, aes(x= ZIKV_protein, y= Epitope_Count, color = Virologic_Control_Group), position = position_jitterdodge(seed = 123, jitter.width = 0.5, jitter.height = 0.5), size= 1, shape= 16) +
  theme_classic() + 
  scale_color_manual(values = c('black', 'orange'), breaks = c('Controller', 'Non-Controller')) +
  guides(color = guide_legend('Virologic Control Group')) + 
  scale_x_discrete(name = 'Region in ZIKV Polyprotein', expand = c(0,0)) + 
  scale_y_continuous(name = 'IgM Epitope Count', expand = c(0,0), limits = c(-2,30), breaks = c(0, 5, 10, 15, 20, 25, 30)) + 
  labs(title= '13-17 DPI') +
  theme(axis.text.x= element_text(size = 20, angle = 45, hjust = 1, vjust = 1), axis.text.y = element_text(size = 20), axis.title = element_text(size = 20, face = 'bold'), aspect.ratio= 1, legend.title = element_text(size = 20, face = 'bold'), legend.text = element_text(size = 20), legend.position = 'top', plot.title = element_text(size= 24, face = 'bold', hjust = 0.5))
epitope_counts_final_IgM_13.17dpi_group_figure  
## 21-24 days post-infection
epitope_counts_final_IgM_21.24dpi <- filter(epitope_counts_final, Timepoint == '21-24',
                                        ZIKV_protein == 'Capsid' |
                                          ZIKV_protein == 'PrM' |
                                          ZIKV_protein == 'M' |
                                          ZIKV_protein == 'E' |
                                          ZIKV_protein == 'NS1' |
                                          ZIKV_protein == 'NS2A' |
                                          ZIKV_protein == 'NS2B' |
                                          ZIKV_protein == 'NS3' |
                                          ZIKV_protein == 'NS4A' |
                                          ZIKV_protein == 'NS4B' |
                                          ZIKV_protein == 'NS5')
# Statistical analyses comparing epitope counts within each region of the ZIKV polyprotein between virologic control groups (Mann-Whitney U-Tests)
#Capsid
epitope_counts_final_IgM_21.24dpi_capsid <- filter(epitope_counts_final_IgM_21.24dpi, ZIKV_protein == 'Capsid')
epitope_counts_final_IgM_21.24dpi_capsid_controller <- filter(epitope_counts_final_IgM_21.24dpi_capsid, Virologic_Control_Group == 'Controller')
epitope_counts_final_IgM_21.24dpi_capsid_controller_quantile <- quantile(epitope_counts_final_IgM_21.24dpi_capsid_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgM_21.24dpi_capsid_controller_quantile
epitope_counts_final_IgM_21.24dpi_capsid_non_controller <- filter(epitope_counts_final_IgM_21.24dpi_capsid, Virologic_Control_Group == 'Non-Controller')
epitope_counts_final_IgM_21.24dpi_capsid_non_controller_quantile <- quantile(epitope_counts_final_IgM_21.24dpi_capsid_non_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgM_21.24dpi_capsid_non_controller_quantile
wilcox.test(Epitope_Count~Virologic_Control_Group, data = epitope_counts_final_IgM_21.24dpi_capsid, exact = FALSE)
#Pre-membrane (PrM)
epitope_counts_final_IgM_21.24dpi_PrM <- filter(epitope_counts_final_IgM_21.24dpi, ZIKV_protein == 'PrM')
epitope_counts_final_IgM_21.24dpi_PrM_controller <- filter(epitope_counts_final_IgM_21.24dpi_PrM, Virologic_Control_Group == 'Controller')
epitope_counts_final_IgM_21.24dpi_PrM_controller_quantile <- quantile(epitope_counts_final_IgM_21.24dpi_PrM_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgM_21.24dpi_PrM_controller_quantile
epitope_counts_final_IgM_21.24dpi_PrM_non_controller <- filter(epitope_counts_final_IgM_21.24dpi_PrM, Virologic_Control_Group == 'Non-Controller')
epitope_counts_final_IgM_21.24dpi_PrM_non_controller_quantile <- quantile(epitope_counts_final_IgM_21.24dpi_PrM_non_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgM_21.24dpi_PrM_non_controller_quantile
wilcox.test(Epitope_Count~Virologic_Control_Group, data = epitope_counts_final_IgM_21.24dpi_PrM, exact = FALSE)
#Membrane (M)
epitope_counts_final_IgM_21.24dpi_M <- filter(epitope_counts_final_IgM_21.24dpi, ZIKV_protein == 'M')
epitope_counts_final_IgM_21.24dpi_M_controller <- filter(epitope_counts_final_IgM_21.24dpi_M, Virologic_Control_Group == 'Controller')
epitope_counts_final_IgM_21.24dpi_M_controller_quantile <- quantile(epitope_counts_final_IgM_21.24dpi_M_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgM_21.24dpi_M_controller_quantile
epitope_counts_final_IgM_21.24dpi_M_non_controller <- filter(epitope_counts_final_IgM_21.24dpi_M, Virologic_Control_Group == 'Non-Controller')
epitope_counts_final_IgM_21.24dpi_M_non_controller_quantile <- quantile(epitope_counts_final_IgM_21.24dpi_M_non_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgM_21.24dpi_M_non_controller_quantile
wilcox.test(Epitope_Count~Virologic_Control_Group, data = epitope_counts_final_IgM_21.24dpi_M, exact = FALSE)
#Envelope (E)
epitope_counts_final_IgM_21.24dpi_E <- filter(epitope_counts_final_IgM_21.24dpi, ZIKV_protein == 'E')
epitope_counts_final_IgM_21.24dpi_E_controller <- filter(epitope_counts_final_IgM_21.24dpi_E, Virologic_Control_Group == 'Controller')
epitope_counts_final_IgM_21.24dpi_E_controller_quantile <- quantile(epitope_counts_final_IgM_21.24dpi_E_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgM_21.24dpi_E_controller_quantile
epitope_counts_final_IgM_21.24dpi_E_non_controller <- filter(epitope_counts_final_IgM_21.24dpi_E, Virologic_Control_Group == 'Non-Controller')
epitope_counts_final_IgM_21.24dpi_E_non_controller_quantile <- quantile(epitope_counts_final_IgM_21.24dpi_E_non_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgM_21.24dpi_E_non_controller_quantile
wilcox.test(Epitope_Count~Virologic_Control_Group, data = epitope_counts_final_IgM_21.24dpi_E, exact = FALSE)
#NS1 
epitope_counts_final_IgM_21.24dpi_NS1 <- filter(epitope_counts_final_IgM_21.24dpi, ZIKV_protein == 'NS1')
epitope_counts_final_IgM_21.24dpi_NS1_controller <- filter(epitope_counts_final_IgM_21.24dpi_NS1, Virologic_Control_Group == 'Controller')
epitope_counts_final_IgM_21.24dpi_NS1_controller_quantile <- quantile(epitope_counts_final_IgM_21.24dpi_NS1_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgM_21.24dpi_NS1_controller_quantile
epitope_counts_final_IgM_21.24dpi_NS1_non_controller <- filter(epitope_counts_final_IgM_21.24dpi_NS1, Virologic_Control_Group == 'Non-Controller')
epitope_counts_final_IgM_21.24dpi_NS1_non_controller_quantile <- quantile(epitope_counts_final_IgM_21.24dpi_NS1_non_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgM_21.24dpi_NS1_non_controller_quantile
wilcox.test(Epitope_Count~Virologic_Control_Group, data = epitope_counts_final_IgM_21.24dpi_NS1, exact = FALSE)
#NS2A
epitope_counts_final_IgM_21.24dpi_NS2A <- filter(epitope_counts_final_IgM_21.24dpi, ZIKV_protein == 'NS2A')
epitope_counts_final_IgM_21.24dpi_NS2A_controller <- filter(epitope_counts_final_IgM_21.24dpi_NS2A, Virologic_Control_Group == 'Controller')
epitope_counts_final_IgM_21.24dpi_NS2A_controller_quantile <- quantile(epitope_counts_final_IgM_21.24dpi_NS2A_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgM_21.24dpi_NS2A_controller_quantile
epitope_counts_final_IgM_21.24dpi_NS2A_non_controller <- filter(epitope_counts_final_IgM_21.24dpi_NS2A, Virologic_Control_Group == 'Non-Controller')
epitope_counts_final_IgM_21.24dpi_NS2A_non_controller_quantile <- quantile(epitope_counts_final_IgM_21.24dpi_NS2A_non_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgM_21.24dpi_NS2A_non_controller_quantile
wilcox.test(Epitope_Count~Virologic_Control_Group, data = epitope_counts_final_IgM_21.24dpi_NS2A, exact = FALSE)
#N2B
epitope_counts_final_IgM_21.24dpi_NS2B <- filter(epitope_counts_final_IgM_21.24dpi, ZIKV_protein == 'NS2B')
epitope_counts_final_IgM_21.24dpi_NS2B_controller <- filter(epitope_counts_final_IgM_21.24dpi_NS2B, Virologic_Control_Group == 'Controller')
epitope_counts_final_IgM_21.24dpi_NS2B_controller_quantile <- quantile(epitope_counts_final_IgM_21.24dpi_NS2B_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgM_21.24dpi_NS2B_controller_quantile
epitope_counts_final_IgM_21.24dpi_NS2B_non_controller <- filter(epitope_counts_final_IgM_21.24dpi_NS2B, Virologic_Control_Group == 'Non-Controller')
epitope_counts_final_IgM_21.24dpi_NS2B_non_controller_quantile <- quantile(epitope_counts_final_IgM_21.24dpi_NS2B_non_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgM_21.24dpi_NS2B_non_controller_quantile
wilcox.test(Epitope_Count~Virologic_Control_Group, data = epitope_counts_final_IgM_21.24dpi_NS2B, exact = FALSE)
#NS3
epitope_counts_final_IgM_21.24dpi_NS3 <- filter(epitope_counts_final_IgM_21.24dpi, ZIKV_protein == 'NS3')
epitope_counts_final_IgM_21.24dpi_NS3_controller <- filter(epitope_counts_final_IgM_21.24dpi_NS3, Virologic_Control_Group == 'Controller')
epitope_counts_final_IgM_21.24dpi_NS3_controller_quantile <- quantile(epitope_counts_final_IgM_21.24dpi_NS3_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgM_21.24dpi_NS3_controller_quantile
epitope_counts_final_IgM_21.24dpi_NS3_non_controller <- filter(epitope_counts_final_IgM_21.24dpi_NS3, Virologic_Control_Group == 'Non-Controller')
epitope_counts_final_IgM_21.24dpi_NS3_non_controller_quantile <- quantile(epitope_counts_final_IgM_21.24dpi_NS3_non_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgM_21.24dpi_NS3_non_controller_quantile
wilcox.test(Epitope_Count~Virologic_Control_Group, data = epitope_counts_final_IgM_21.24dpi_NS3, exact = FALSE)
#NS4A
epitope_counts_final_IgM_21.24dpi_NS4A <- filter(epitope_counts_final_IgM_21.24dpi, ZIKV_protein == 'NS4A')
epitope_counts_final_IgM_21.24dpi_NS4A_controller <- filter(epitope_counts_final_IgM_21.24dpi_NS4A, Virologic_Control_Group == 'Controller')
epitope_counts_final_IgM_21.24dpi_NS4A_controller_quantile <- quantile(epitope_counts_final_IgM_21.24dpi_NS4A_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgM_21.24dpi_NS4A_controller_quantile
epitope_counts_final_IgM_21.24dpi_NS4A_non_controller <- filter(epitope_counts_final_IgM_21.24dpi_NS4A, Virologic_Control_Group == 'Non-Controller')
epitope_counts_final_IgM_21.24dpi_NS4A_non_controller_quantile <- quantile(epitope_counts_final_IgM_21.24dpi_NS4A_non_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgM_21.24dpi_NS4A_non_controller_quantile
wilcox.test(Epitope_Count~Virologic_Control_Group, data = epitope_counts_final_IgM_21.24dpi_NS4A, exact = FALSE)
#NS4B
epitope_counts_final_IgM_21.24dpi_NS4B <- filter(epitope_counts_final_IgM_21.24dpi, ZIKV_protein == 'NS4B')
epitope_counts_final_IgM_21.24dpi_NS4B_controller <- filter(epitope_counts_final_IgM_21.24dpi_NS4B, Virologic_Control_Group == 'Controller')
epitope_counts_final_IgM_21.24dpi_NS4B_controller_quantile <- quantile(epitope_counts_final_IgM_21.24dpi_NS4B_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgM_21.24dpi_NS4B_controller_quantile
epitope_counts_final_IgM_21.24dpi_NS4B_non_controller <- filter(epitope_counts_final_IgM_21.24dpi_NS4B, Virologic_Control_Group == 'Non-Controller')
epitope_counts_final_IgM_21.24dpi_NS4B_non_controller_quantile <- quantile(epitope_counts_final_IgM_21.24dpi_NS4B_non_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgM_21.24dpi_NS4B_non_controller_quantile
wilcox.test(Epitope_Count~Virologic_Control_Group, data = epitope_counts_final_IgM_21.24dpi_NS4B, exact = FALSE)
#NS5
epitope_counts_final_IgM_21.24dpi_NS5 <- filter(epitope_counts_final_IgM_21.24dpi, ZIKV_protein == 'NS5')
epitope_counts_final_IgM_21.24dpi_NS5_controller <- filter(epitope_counts_final_IgM_21.24dpi_NS5, Virologic_Control_Group == 'Controller')
epitope_counts_final_IgM_21.24dpi_NS5_controller_quantile <- quantile(epitope_counts_final_IgM_21.24dpi_NS5_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgM_21.24dpi_NS5_controller_quantile
epitope_counts_final_IgM_21.24dpi_NS5_non_controller <- filter(epitope_counts_final_IgM_21.24dpi_NS5, Virologic_Control_Group == 'Non-Controller')
epitope_counts_final_IgM_21.24dpi_NS5_non_controller_quantile <- quantile(epitope_counts_final_IgM_21.24dpi_NS5_non_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgM_21.24dpi_NS5_non_controller_quantile
wilcox.test(Epitope_Count~Virologic_Control_Group, data = epitope_counts_final_IgM_21.24dpi_NS5, exact = FALSE)
#21-24 days post-infection IgM epitope counts (group) figure
epitope_counts_final_IgM_21.24dpi_group_figure <-
  ggplot() +
  geom_boxplot(data = epitope_counts_final_IgM_21.24dpi, aes(x= ZIKV_protein, y= Epitope_Count, color = Virologic_Control_Group), fill = 'white', outlier.colour =  'white') +
  geom_point(data = epitope_counts_final_IgM_21.24dpi, aes(x= ZIKV_protein, y= Epitope_Count, color = Virologic_Control_Group), position = position_jitterdodge(seed = 123, jitter.width = 0.5, jitter.height = 0.5), size= 1, shape= 16) +
  theme_classic() + 
  scale_color_manual(values = c('black', 'orange'), breaks = c('Controller', 'Non-Controller')) +
  guides(color = 'none') + 
  scale_x_discrete(name = 'Region in ZIKV Polyprotein', expand = c(0,0)) + 
  scale_y_continuous(name = 'IgM Epitope Count', expand = c(0,0), limits = c(-2,32), breaks = c(0, 5, 10, 15, 20, 25, 30)) + 
  labs(title= '21-24 DPI') +
  theme(axis.text.x= element_text(size = 20, angle = 45, hjust = 1, vjust = 1), axis.text.y = element_text(size = 20), axis.title = element_text(size = 20, face = 'bold'), aspect.ratio= 1, plot.title = element_text(size= 24, face = 'bold', hjust = 0.5))
epitope_counts_final_IgM_21.24dpi_group_figure  


###IgG

## 27-31 days post-infection
epitope_counts_final_IgG_27.31dpi <- filter(epitope_counts_final, Timepoint == '27-31',
                                        ZIKV_protein == 'Capsid' |
                                          ZIKV_protein == 'PrM' |
                                          ZIKV_protein == 'M' |
                                          ZIKV_protein == 'E' |
                                          ZIKV_protein == 'NS1' |
                                          ZIKV_protein == 'NS2A' |
                                          ZIKV_protein == 'NS2B' |
                                          ZIKV_protein == 'NS3' |
                                          ZIKV_protein == 'NS4A' |
                                          ZIKV_protein == 'NS4B' |
                                          ZIKV_protein == 'NS5')
# Statistical analyses comparing epitope counts within each region of the ZIKV polyprotein between virologic control groups (Mann-Whitney U-Tests)
#Capsid
epitope_counts_final_IgG_27.31dpi_capsid <- filter(epitope_counts_final_IgG_27.31dpi, ZIKV_protein == 'Capsid')
epitope_counts_final_IgG_27.31dpi_capsid_controller <- filter(epitope_counts_final_IgG_27.31dpi_capsid, Virologic_Control_Group == 'Controller')
epitope_counts_final_IgG_27.31dpi_capsid_controller_quantile <- quantile(epitope_counts_final_IgG_27.31dpi_capsid_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgG_27.31dpi_capsid_controller_quantile
epitope_counts_final_IgG_27.31dpi_capsid_non_controller <- filter(epitope_counts_final_IgG_27.31dpi_capsid, Virologic_Control_Group == 'Non-Controller')
epitope_counts_final_IgG_27.31dpi_capsid_non_controller_quantile <- quantile(epitope_counts_final_IgG_27.31dpi_capsid_non_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgG_27.31dpi_capsid_non_controller_quantile
wilcox.test(Epitope_Count~Virologic_Control_Group, data = epitope_counts_final_IgG_27.31dpi_capsid, exact = FALSE)
#Pre-membrane (PrM)
epitope_counts_final_IgG_27.31dpi_PrM <- filter(epitope_counts_final_IgG_27.31dpi, ZIKV_protein == 'PrM')
epitope_counts_final_IgG_27.31dpi_PrM_controller <- filter(epitope_counts_final_IgG_27.31dpi_PrM, Virologic_Control_Group == 'Controller')
epitope_counts_final_IgG_27.31dpi_PrM_controller_quantile <- quantile(epitope_counts_final_IgG_27.31dpi_PrM_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgG_27.31dpi_PrM_controller_quantile
epitope_counts_final_IgG_27.31dpi_PrM_non_controller <- filter(epitope_counts_final_IgG_27.31dpi_PrM, Virologic_Control_Group == 'Non-Controller')
epitope_counts_final_IgG_27.31dpi_PrM_non_controller_quantile <- quantile(epitope_counts_final_IgG_27.31dpi_PrM_non_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgG_27.31dpi_PrM_non_controller_quantile
wilcox.test(Epitope_Count~Virologic_Control_Group, data = epitope_counts_final_IgG_27.31dpi_PrM, exact = FALSE)
#Membrane (M)
epitope_counts_final_IgG_27.31dpi_M <- filter(epitope_counts_final_IgG_27.31dpi, ZIKV_protein == 'M')
epitope_counts_final_IgG_27.31dpi_M_controller <- filter(epitope_counts_final_IgG_27.31dpi_M, Virologic_Control_Group == 'Controller')
epitope_counts_final_IgG_27.31dpi_M_controller_quantile <- quantile(epitope_counts_final_IgG_27.31dpi_M_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgG_27.31dpi_M_controller_quantile
epitope_counts_final_IgG_27.31dpi_M_non_controller <- filter(epitope_counts_final_IgG_27.31dpi_M, Virologic_Control_Group == 'Non-Controller')
epitope_counts_final_IgG_27.31dpi_M_non_controller_quantile <- quantile(epitope_counts_final_IgG_27.31dpi_M_non_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgG_27.31dpi_M_non_controller_quantile
wilcox.test(Epitope_Count~Virologic_Control_Group, data = epitope_counts_final_IgG_27.31dpi_M, exact = FALSE)
#Envelope (E)
epitope_counts_final_IgG_27.31dpi_E <- filter(epitope_counts_final_IgG_27.31dpi, ZIKV_protein == 'E')
epitope_counts_final_IgG_27.31dpi_E_controller <- filter(epitope_counts_final_IgG_27.31dpi_E, Virologic_Control_Group == 'Controller')
epitope_counts_final_IgG_27.31dpi_E_controller_quantile <- quantile(epitope_counts_final_IgG_27.31dpi_E_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgG_27.31dpi_E_controller_quantile
epitope_counts_final_IgG_27.31dpi_E_non_controller <- filter(epitope_counts_final_IgG_27.31dpi_E, Virologic_Control_Group == 'Non-Controller')
epitope_counts_final_IgG_27.31dpi_E_non_controller_quantile <- quantile(epitope_counts_final_IgG_27.31dpi_E_non_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgG_27.31dpi_E_non_controller_quantile
wilcox.test(Epitope_Count~Virologic_Control_Group, data = epitope_counts_final_IgG_27.31dpi_E, exact = FALSE)
#NS1 
epitope_counts_final_IgG_27.31dpi_NS1 <- filter(epitope_counts_final_IgG_27.31dpi, ZIKV_protein == 'NS1')
epitope_counts_final_IgG_27.31dpi_NS1_controller <- filter(epitope_counts_final_IgG_27.31dpi_NS1, Virologic_Control_Group == 'Controller')
epitope_counts_final_IgG_27.31dpi_NS1_controller_quantile <- quantile(epitope_counts_final_IgG_27.31dpi_NS1_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgG_27.31dpi_NS1_controller_quantile
epitope_counts_final_IgG_27.31dpi_NS1_non_controller <- filter(epitope_counts_final_IgG_27.31dpi_NS1, Virologic_Control_Group == 'Non-Controller')
epitope_counts_final_IgG_27.31dpi_NS1_non_controller_quantile <- quantile(epitope_counts_final_IgG_27.31dpi_NS1_non_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgG_27.31dpi_NS1_non_controller_quantile
wilcox.test(Epitope_Count~Virologic_Control_Group, data = epitope_counts_final_IgG_27.31dpi_NS1, exact = FALSE)
#NS2A
epitope_counts_final_IgG_27.31dpi_NS2A <- filter(epitope_counts_final_IgG_27.31dpi, ZIKV_protein == 'NS2A')
epitope_counts_final_IgG_27.31dpi_NS2A_controller <- filter(epitope_counts_final_IgG_27.31dpi_NS2A, Virologic_Control_Group == 'Controller')
epitope_counts_final_IgG_27.31dpi_NS2A_controller_quantile <- quantile(epitope_counts_final_IgG_27.31dpi_NS2A_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgG_27.31dpi_NS2A_controller_quantile
epitope_counts_final_IgG_27.31dpi_NS2A_non_controller <- filter(epitope_counts_final_IgG_27.31dpi_NS2A, Virologic_Control_Group == 'Non-Controller')
epitope_counts_final_IgG_27.31dpi_NS2A_non_controller_quantile <- quantile(epitope_counts_final_IgG_27.31dpi_NS2A_non_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgG_27.31dpi_NS2A_non_controller_quantile
wilcox.test(Epitope_Count~Virologic_Control_Group, data = epitope_counts_final_IgG_27.31dpi_NS2A, exact = FALSE)
#NS2B
epitope_counts_final_IgG_27.31dpi_NS2B <- filter(epitope_counts_final_IgG_27.31dpi, ZIKV_protein == 'NS2B')
epitope_counts_final_IgG_27.31dpi_NS2B_controller <- filter(epitope_counts_final_IgG_27.31dpi_NS2B, Virologic_Control_Group == 'Controller')
epitope_counts_final_IgG_27.31dpi_NS2B_controller_quantile <- quantile(epitope_counts_final_IgG_27.31dpi_NS2B_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgG_27.31dpi_NS2B_controller_quantile
epitope_counts_final_IgG_27.31dpi_NS2B_non_controller <- filter(epitope_counts_final_IgG_27.31dpi_NS2B, Virologic_Control_Group == 'Non-Controller')
epitope_counts_final_IgG_27.31dpi_NS2B_non_controller_quantile <- quantile(epitope_counts_final_IgG_27.31dpi_NS2B_non_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgG_27.31dpi_NS2B_non_controller_quantile
wilcox.test(Epitope_Count~Virologic_Control_Group, data = epitope_counts_final_IgG_27.31dpi_NS2B, exact = FALSE)
#NS3
epitope_counts_final_IgG_27.31dpi_NS3 <- filter(epitope_counts_final_IgG_27.31dpi, ZIKV_protein == 'NS3')
epitope_counts_final_IgG_27.31dpi_NS3_controller <- filter(epitope_counts_final_IgG_27.31dpi_NS3, Virologic_Control_Group == 'Controller')
epitope_counts_final_IgG_27.31dpi_NS3_controller_quantile <- quantile(epitope_counts_final_IgG_27.31dpi_NS3_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgG_27.31dpi_NS3_controller_quantile
epitope_counts_final_IgG_27.31dpi_NS3_non_controller <- filter(epitope_counts_final_IgG_27.31dpi_NS3, Virologic_Control_Group == 'Non-Controller')
epitope_counts_final_IgG_27.31dpi_NS3_non_controller_quantile <- quantile(epitope_counts_final_IgG_27.31dpi_NS3_non_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgG_27.31dpi_NS3_non_controller_quantile
wilcox.test(Epitope_Count~Virologic_Control_Group, data = epitope_counts_final_IgG_27.31dpi_NS3, exact = FALSE)
#NS4A
epitope_counts_final_IgG_27.31dpi_NS4A <- filter(epitope_counts_final_IgG_27.31dpi, ZIKV_protein == 'NS4A')
epitope_counts_final_IgG_27.31dpi_NS4A_controller <- filter(epitope_counts_final_IgG_27.31dpi_NS4A, Virologic_Control_Group == 'Controller')
epitope_counts_final_IgG_27.31dpi_NS4A_controller_quantile <- quantile(epitope_counts_final_IgG_27.31dpi_NS4A_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgG_27.31dpi_NS4A_controller_quantile
epitope_counts_final_IgG_27.31dpi_NS4A_non_controller <- filter(epitope_counts_final_IgG_27.31dpi_NS4A, Virologic_Control_Group == 'Non-Controller')
epitope_counts_final_IgG_27.31dpi_NS4A_non_controller_quantile <- quantile(epitope_counts_final_IgG_27.31dpi_NS4A_non_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgG_27.31dpi_NS4A_non_controller_quantile
wilcox.test(Epitope_Count~Virologic_Control_Group, data = epitope_counts_final_IgG_27.31dpi_NS4A, exact = FALSE)
#NS4B
epitope_counts_final_IgG_27.31dpi_NS4B <- filter(epitope_counts_final_IgG_27.31dpi, ZIKV_protein == 'NS4B')
epitope_counts_final_IgG_27.31dpi_NS4B_controller <- filter(epitope_counts_final_IgG_27.31dpi_NS4B, Virologic_Control_Group == 'Controller')
epitope_counts_final_IgG_27.31dpi_NS4B_controller_quantile <- quantile(epitope_counts_final_IgG_27.31dpi_NS4B_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgG_27.31dpi_NS4B_controller_quantile
epitope_counts_final_IgG_27.31dpi_NS4B_non_controller <- filter(epitope_counts_final_IgG_27.31dpi_NS4B, Virologic_Control_Group == 'Non-Controller')
epitope_counts_final_IgG_27.31dpi_NS4B_non_controller_quantile <- quantile(epitope_counts_final_IgG_27.31dpi_NS4B_non_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgG_27.31dpi_NS4B_non_controller_quantile
wilcox.test(Epitope_Count~Virologic_Control_Group, data = epitope_counts_final_IgG_27.31dpi_NS4B, exact = FALSE)
#NS5
epitope_counts_final_IgG_27.31dpi_NS5 <- filter(epitope_counts_final_IgG_27.31dpi, ZIKV_protein == 'NS5')
epitope_counts_final_IgG_27.31dpi_NS5_controller <- filter(epitope_counts_final_IgG_27.31dpi_NS5, Virologic_Control_Group == 'Controller')
epitope_counts_final_IgG_27.31dpi_NS5_controller_quantile <- quantile(epitope_counts_final_IgG_27.31dpi_NS5_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgG_27.31dpi_NS5_controller_quantile
epitope_counts_final_IgG_27.31dpi_NS5_non_controller <- filter(epitope_counts_final_IgG_27.31dpi_NS5, Virologic_Control_Group == 'Non-Controller')
epitope_counts_final_IgG_27.31dpi_NS5_non_controller_quantile <- quantile(epitope_counts_final_IgG_27.31dpi_NS5_non_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgG_27.31dpi_NS5_non_controller_quantile
wilcox.test(Epitope_Count~Virologic_Control_Group, data = epitope_counts_final_IgG_27.31dpi_NS5, exact = FALSE)
#27-31 days post-infection IgG epitope counts (group) figure
epitope_counts_final_IgG_27.31dpi_group_figure <-
  ggplot() +
  geom_boxplot(data = epitope_counts_final_IgG_27.31dpi, aes(x= ZIKV_protein, y= Epitope_Count, color = Virologic_Control_Group), fill = 'white', outlier.colour =  'white') +
  geom_point(data = epitope_counts_final_IgG_27.31dpi, aes(x= ZIKV_protein, y= Epitope_Count, color = Virologic_Control_Group), position = position_jitterdodge(seed = 123, jitter.width = 0.5, jitter.height = 0.5), size= 1, shape= 16) +
  theme_classic() + 
  scale_color_manual(values = c('black', 'orange'), breaks = c('Controller', 'Non-Controller')) +
  guides(color = 'none') + 
  scale_x_discrete(name = 'Region in ZIKV Polyprotein', expand = c(0,0)) + 
  scale_y_continuous(name = 'IgG Epitope Count', expand = c(0,0), limits = c(-2,15), breaks = c(0, 5, 10, 15)) + 
  labs(title= '27-31 DPI') +
  theme(axis.text.x= element_text(size = 20, angle = 45, hjust = 1, vjust = 1), axis.text.y = element_text(size = 20), axis.title = element_text(size = 20, face = 'bold'), aspect.ratio= 1, plot.title = element_text(size= 24, face = 'bold', hjust = 0.5))
epitope_counts_final_IgG_27.31dpi_group_figure  
## 108-135 days post-infection
epitope_counts_final_IgG_108.135dpi<- filter(epitope_counts_final, Timepoint == '108-135',
                                       ZIKV_protein == 'Capsid' |
                                         ZIKV_protein == 'PrM' |
                                         ZIKV_protein == 'M' |
                                         ZIKV_protein == 'E' |
                                         ZIKV_protein == 'NS1' |
                                         ZIKV_protein == 'NS2A' |
                                         ZIKV_protein == 'NS2B' |
                                         ZIKV_protein == 'NS3' |
                                         ZIKV_protein == 'NS4A' |
                                         ZIKV_protein == 'NS4B' |
                                         ZIKV_protein == 'NS5')
# Statistical analyses comparing epitope counts within each region of the ZIKV polyprotein between virologic control groups (Mann-Whitney U-Tests)
#Capsid
epitope_counts_final_IgG_108.135dpi_capsid <- filter(epitope_counts_final_IgG_108.135dpi, ZIKV_protein == 'Capsid')
epitope_counts_final_IgG_108.135dpi_capsid_controller <- filter(epitope_counts_final_IgG_108.135dpi_capsid, Virologic_Control_Group == 'Controller')
epitope_counts_final_IgG_108.135dpi_capsid_controller_quantile <- quantile(epitope_counts_final_IgG_108.135dpi_capsid_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgG_108.135dpi_capsid_controller_quantile
epitope_counts_final_IgG_108.135dpi_capsid_non_controller <- filter(epitope_counts_final_IgG_108.135dpi_capsid, Virologic_Control_Group == 'Non-Controller')
epitope_counts_final_IgG_108.135dpi_capsid_non_controller_quantile <- quantile(epitope_counts_final_IgG_108.135dpi_capsid_non_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgG_108.135dpi_capsid_non_controller_quantile
wilcox.test(Epitope_Count~Virologic_Control_Group, data = epitope_counts_final_IgG_108.135dpi_capsid, exact = FALSE)
#Pre-membrane (PrM)
epitope_counts_final_IgG_108.135dpi_PrM <- filter(epitope_counts_final_IgG_108.135dpi, ZIKV_protein == 'PrM')
epitope_counts_final_IgG_108.135dpi_PrM_controller <- filter(epitope_counts_final_IgG_108.135dpi_PrM, Virologic_Control_Group == 'Controller')
epitope_counts_final_IgG_108.135dpi_PrM_controller_quantile <- quantile(epitope_counts_final_IgG_108.135dpi_PrM_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgG_108.135dpi_PrM_controller_quantile
epitope_counts_final_IgG_108.135dpi_PrM_non_controller <- filter(epitope_counts_final_IgG_108.135dpi_PrM, Virologic_Control_Group == 'Non-Controller')
epitope_counts_final_IgG_108.135dpi_PrM_non_controller_quantile <- quantile(epitope_counts_final_IgG_108.135dpi_PrM_non_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgG_108.135dpi_PrM_non_controller_quantile
wilcox.test(Epitope_Count~Virologic_Control_Group, data = epitope_counts_final_IgG_108.135dpi_PrM, exact = FALSE)
#Membrane (M)
epitope_counts_final_IgG_108.135dpi_M <- filter(epitope_counts_final_IgG_108.135dpi, ZIKV_protein == 'M')
epitope_counts_final_IgG_108.135dpi_M_controller <- filter(epitope_counts_final_IgG_108.135dpi_M, Virologic_Control_Group == 'Controller')
epitope_counts_final_IgG_108.135dpi_M_controller_quantile <- quantile(epitope_counts_final_IgG_108.135dpi_M_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgG_108.135dpi_M_controller_quantile
epitope_counts_final_IgG_108.135dpi_M_non_controller <- filter(epitope_counts_final_IgG_108.135dpi_M, Virologic_Control_Group == 'Non-Controller')
epitope_counts_final_IgG_108.135dpi_M_non_controller_quantile <- quantile(epitope_counts_final_IgG_108.135dpi_M_non_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgG_108.135dpi_M_non_controller_quantile
wilcox.test(Epitope_Count~Virologic_Control_Group, data = epitope_counts_final_IgG_108.135dpi_M, exact = FALSE)
#Envelope (E)
epitope_counts_final_IgG_108.135dpi_E <- filter(epitope_counts_final_IgG_108.135dpi, ZIKV_protein == 'E')
epitope_counts_final_IgG_108.135dpi_E_controller <- filter(epitope_counts_final_IgG_108.135dpi_E, Virologic_Control_Group == 'Controller')
epitope_counts_final_IgG_108.135dpi_E_controller_quantile <- quantile(epitope_counts_final_IgG_108.135dpi_E_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgG_108.135dpi_E_controller_quantile
epitope_counts_final_IgG_108.135dpi_E_non_controller <- filter(epitope_counts_final_IgG_108.135dpi_E, Virologic_Control_Group == 'Non-Controller')
epitope_counts_final_IgG_108.135dpi_E_non_controller_quantile <- quantile(epitope_counts_final_IgG_108.135dpi_E_non_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgG_108.135dpi_E_non_controller_quantile
wilcox.test(Epitope_Count~Virologic_Control_Group, data = epitope_counts_final_IgG_108.135dpi_E, exact = FALSE)
#NS1 
epitope_counts_final_IgG_108.135dpi_NS1 <- filter(epitope_counts_final_IgG_108.135dpi, ZIKV_protein == 'NS1')
epitope_counts_final_IgG_108.135dpi_NS1_controller <- filter(epitope_counts_final_IgG_108.135dpi_NS1, Virologic_Control_Group == 'Controller')
epitope_counts_final_IgG_108.135dpi_NS1_controller_quantile <- quantile(epitope_counts_final_IgG_108.135dpi_NS1_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgG_108.135dpi_NS1_controller_quantile
epitope_counts_final_IgG_108.135dpi_NS1_non_controller <- filter(epitope_counts_final_IgG_108.135dpi_NS1, Virologic_Control_Group == 'Non-Controller')
epitope_counts_final_IgG_108.135dpi_NS1_non_controller_quantile <- quantile(epitope_counts_final_IgG_108.135dpi_NS1_non_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgG_108.135dpi_NS1_non_controller_quantile
wilcox.test(Epitope_Count~Virologic_Control_Group, data = epitope_counts_final_IgG_108.135dpi_NS1, exact = FALSE)
#NS2A
epitope_counts_final_IgG_108.135dpi_NS2A <- filter(epitope_counts_final_IgG_108.135dpi, ZIKV_protein == 'NS2A')
epitope_counts_final_IgG_108.135dpi_NS2A_controller <- filter(epitope_counts_final_IgG_108.135dpi_NS2A, Virologic_Control_Group == 'Controller')
epitope_counts_final_IgG_108.135dpi_NS2A_controller_quantile <- quantile(epitope_counts_final_IgG_108.135dpi_NS2A_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgG_108.135dpi_NS2A_controller_quantile
epitope_counts_final_IgG_108.135dpi_NS2A_non_controller <- filter(epitope_counts_final_IgG_108.135dpi_NS2A, Virologic_Control_Group == 'Non-Controller')
epitope_counts_final_IgG_108.135dpi_NS2A_non_controller_quantile <- quantile(epitope_counts_final_IgG_108.135dpi_NS2A_non_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgG_108.135dpi_NS2A_non_controller_quantile
wilcox.test(Epitope_Count~Virologic_Control_Group, data = epitope_counts_final_IgG_108.135dpi_NS2A, exact = FALSE)
#NS2B
epitope_counts_final_IgG_108.135dpi_NS2B <- filter(epitope_counts_final_IgG_108.135dpi, ZIKV_protein == 'NS2B')
epitope_counts_final_IgG_108.135dpi_NS2B_controller <- filter(epitope_counts_final_IgG_108.135dpi_NS2B, Virologic_Control_Group == 'Controller')
epitope_counts_final_IgG_108.135dpi_NS2B_controller_quantile <- quantile(epitope_counts_final_IgG_108.135dpi_NS2B_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgG_108.135dpi_NS2B_controller_quantile
epitope_counts_final_IgG_108.135dpi_NS2B_non_controller <- filter(epitope_counts_final_IgG_108.135dpi_NS2B, Virologic_Control_Group == 'Non-Controller')
epitope_counts_final_IgG_108.135dpi_NS2B_non_controller_quantile <- quantile(epitope_counts_final_IgG_108.135dpi_NS2B_non_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgG_108.135dpi_NS2B_non_controller_quantile
wilcox.test(Epitope_Count~Virologic_Control_Group, data = epitope_counts_final_IgG_108.135dpi_NS2B, exact = FALSE)
#NS3
epitope_counts_final_IgG_108.135dpi_NS3 <- filter(epitope_counts_final_IgG_108.135dpi, ZIKV_protein == 'NS3')
epitope_counts_final_IgG_108.135dpi_NS3_controller <- filter(epitope_counts_final_IgG_108.135dpi_NS3, Virologic_Control_Group == 'Controller')
epitope_counts_final_IgG_108.135dpi_NS3_controller_quantile <- quantile(epitope_counts_final_IgG_108.135dpi_NS3_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgG_108.135dpi_NS3_controller_quantile
epitope_counts_final_IgG_108.135dpi_NS3_non_controller <- filter(epitope_counts_final_IgG_108.135dpi_NS3, Virologic_Control_Group == 'Non-Controller')
epitope_counts_final_IgG_108.135dpi_NS3_non_controller_quantile <- quantile(epitope_counts_final_IgG_108.135dpi_NS3_non_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgG_108.135dpi_NS3_non_controller_quantile
wilcox.test(Epitope_Count~Virologic_Control_Group, data = epitope_counts_final_IgG_108.135dpi_NS3, exact = FALSE)
#NS4A
epitope_counts_final_IgG_108.135dpi_NS4A <- filter(epitope_counts_final_IgG_108.135dpi, ZIKV_protein == 'NS4A')
epitope_counts_final_IgG_108.135dpi_NS4A_controller <- filter(epitope_counts_final_IgG_108.135dpi_NS4A, Virologic_Control_Group == 'Controller')
epitope_counts_final_IgG_108.135dpi_NS4A_controller_quantile <- quantile(epitope_counts_final_IgG_108.135dpi_NS4A_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgG_108.135dpi_NS4A_controller_quantile
epitope_counts_final_IgG_108.135dpi_NS4A_non_controller <- filter(epitope_counts_final_IgG_108.135dpi_NS4A, Virologic_Control_Group == 'Non-Controller')
epitope_counts_final_IgG_108.135dpi_NS4A_non_controller_quantile <- quantile(epitope_counts_final_IgG_108.135dpi_NS4A_non_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgG_108.135dpi_NS4A_non_controller_quantile
wilcox.test(Epitope_Count~Virologic_Control_Group, data = epitope_counts_final_IgG_108.135dpi_NS4A, exact = FALSE)
#NS4B
epitope_counts_final_IgG_108.135dpi_NS4B <- filter(epitope_counts_final_IgG_108.135dpi, ZIKV_protein == 'NS4B')
epitope_counts_final_IgG_108.135dpi_NS4B_controller <- filter(epitope_counts_final_IgG_108.135dpi_NS4B, Virologic_Control_Group == 'Controller')
epitope_counts_final_IgG_108.135dpi_NS4B_controller_quantile <- quantile(epitope_counts_final_IgG_108.135dpi_NS4B_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgG_108.135dpi_NS4B_controller_quantile
epitope_counts_final_IgG_108.135dpi_NS4B_non_controller <- filter(epitope_counts_final_IgG_108.135dpi_NS4B, Virologic_Control_Group == 'Non-Controller')
epitope_counts_final_IgG_108.135dpi_NS4B_non_controller_quantile <- quantile(epitope_counts_final_IgG_108.135dpi_NS4B_non_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgG_108.135dpi_NS4B_non_controller_quantile
wilcox.test(Epitope_Count~Virologic_Control_Group, data = epitope_counts_final_IgG_108.135dpi_NS4B, exact = FALSE)
#NS5
epitope_counts_final_IgG_108.135dpi_NS5 <- filter(epitope_counts_final_IgG_108.135dpi, ZIKV_protein == 'NS5')
epitope_counts_final_IgG_108.135dpi_NS5_controller <- filter(epitope_counts_final_IgG_108.135dpi_NS5, Virologic_Control_Group == 'Controller')
epitope_counts_final_IgG_108.135dpi_NS5_controller_quantile <- quantile(epitope_counts_final_IgG_108.135dpi_NS5_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgG_108.135dpi_NS5_controller_quantile
epitope_counts_final_IgG_108.135dpi_NS5_non_controller <- filter(epitope_counts_final_IgG_108.135dpi_NS5, Virologic_Control_Group == 'Non-Controller')
epitope_counts_final_IgG_108.135dpi_NS5_non_controller_quantile <- quantile(epitope_counts_final_IgG_108.135dpi_NS5_non_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgG_108.135dpi_NS5_non_controller_quantile
wilcox.test(Epitope_Count~Virologic_Control_Group, data = epitope_counts_final_IgG_108.135dpi_NS5, exact = FALSE)
#108-135 days post-infection IgG epitope counts (group) figure
epitope_counts_final_IgG_108.135dpi_group_figure <-
  ggplot() +
  geom_boxplot(data = epitope_counts_final_IgG_108.135dpi, aes(x= ZIKV_protein, y= Epitope_Count, color = Virologic_Control_Group), fill = 'white', outlier.colour =  'white') +
  geom_point(data = epitope_counts_final_IgG_108.135dpi, aes(x= ZIKV_protein, y= Epitope_Count, color = Virologic_Control_Group), position = position_jitterdodge(seed = 123, jitter.width = 0.5, jitter.height = 0.5), size= 1, shape= 16) +
  theme_classic() + 
  scale_color_manual(values = c('black', 'orange'), breaks = c('Controller', 'Non-Controller')) +
  guides(color = 'none') + 
  scale_x_discrete(name = 'Region in ZIKV Polyprotein', expand = c(0,0)) + 
  scale_y_continuous(name = 'IgG Epitope Count', expand = c(0,0), limits = c(-2,15), breaks = c(0, 5, 10, 15)) + 
  labs(title= '108-135 DPI') +
  theme(axis.text.x= element_text(size = 20, angle = 45, hjust = 1, vjust = 1), axis.text.y = element_text(size = 20), axis.title = element_text(size = 20, face = 'bold'), aspect.ratio= 1, plot.title = element_text(size= 24, face = 'bold', hjust = 0.5))
epitope_counts_final_IgG_108.135dpi_group_figure  

###
# Combining epitope counts by virologic control group into a single multipanel figure
###
tiff('Epitope Counts-Group (final).tiff', units= 'in', height= 15, width= 15, res= 300)
ggarrange(epitope_counts_final_IgM_13.17dpi_group_figure, epitope_counts_final_IgM_21.24dpi_group_figure, epitope_counts_final_IgG_27.31dpi_group_figure, epitope_counts_final_IgG_108.135dpi_group_figure, nrow=2, ncol=2, common.legend = TRUE, legend= 'top', align= 'hv', labels = c('A', 'B', 'C', 'D'))
dev.off()



###
#Main manuscript figure showing total IgM and IgG linear epitope counts across the entire ZIKV polyprotein for each virologic control group.
#Also, figures showing total epitope counts in each region of the ZIKV polyprotein combining all animals
###

### IgM total epitope counts
IgM_total_epitope_counts_group_final <- filter(epitope_counts_final, Antibody_Isotype == 'IgM',
                                               ZIKV_protein == 'Total')
## Statistical analyses comparing total linear IgM epitope counts between virologic control groups (Mann-Whitney U-Tests)
# 13-17 days post-infection
IgM_total_epitope_counts_group_final_13.17dpi <- filter(IgM_total_epitope_counts_group_final, Timepoint == '13-17')
IgM_total_epitope_counts_group_final_13.17dpi_controller <- filter(IgM_total_epitope_counts_group_final_13.17dpi, Virologic_Control_Group == 'Controller')
IgM_total_epitope_counts_group_final_13.17dpi_controller_quantile <- quantile(IgM_total_epitope_counts_group_final_13.17dpi_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
IgM_total_epitope_counts_group_final_13.17dpi_controller_quantile
IgM_total_epitope_counts_group_final_13.17dpi_non_controller <- filter(IgM_total_epitope_counts_group_final, Virologic_Control_Group == 'Non-Controller')
IgM_total_epitope_counts_group_final_13.17dpi_non_controller_quantile <- quantile(IgM_total_epitope_counts_group_final_13.17dpi_non_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
IgM_total_epitope_counts_group_final_13.17dpi_non_controller_quantile
wilcox.test(Epitope_Count~Virologic_Control_Group, data = IgM_total_epitope_counts_group_final_13.17dpi, exact = FALSE)
# 21-24 days post-infection
IgM_total_epitope_counts_group_final_21.24dpi <- filter(IgM_total_epitope_counts_group_final, Timepoint == '21-24')
IgM_total_epitope_counts_group_final_21.24dpi_controller <- filter(IgM_total_epitope_counts_group_final_21.24dpi, Virologic_Control_Group == 'Controller')
IgM_total_epitope_counts_group_final_21.24dpi_controller_quantile <- quantile(IgM_total_epitope_counts_group_final_21.24dpi_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
IgM_total_epitope_counts_group_final_21.24dpi_controller_quantile
IgM_total_epitope_counts_group_final_21.24dpi_non_controller <- filter(IgM_total_epitope_counts_group_final, Virologic_Control_Group == 'Non-Controller')
IgM_total_epitope_counts_group_final_21.24dpi_non_controller_quantile <- quantile(IgM_total_epitope_counts_group_final_21.24dpi_non_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
IgM_total_epitope_counts_group_final_21.24dpi_non_controller_quantile
wilcox.test(Epitope_Count~Virologic_Control_Group, data = IgM_total_epitope_counts_group_final_21.24dpi, exact = FALSE)
# IgM total linear epitope counts figure
IgM_total_epitope_counts_group_final_figure <-
  ggplot() +
  geom_boxplot(data = IgM_total_epitope_counts_group_final, aes(x= Timepoint, y= Epitope_Count, color = Virologic_Control_Group), fill = 'white', outlier.colour =  'white') +
  geom_point(data = IgM_total_epitope_counts_group_final, aes(x= Timepoint, y= Epitope_Count, color = Virologic_Control_Group), position = position_jitterdodge(seed = 123, jitter.width = 0.5, jitter.height = 0.5), size= 2, shape= 16) +
  theme_classic() + 
  scale_color_manual(values = c('black', 'orange'), breaks = c('Controller', 'Non-Controller')) +
  guides(color = guide_legend('Virologic Control Group')) + 
  scale_x_discrete(name = 'Timepoint (DPI)') + 
  scale_y_continuous(name = 'Total IgM Epitope Count', expand = c(0,0), limits = c(-2,100), breaks = c(0, 25, 50, 75, 100)) + 
  theme(axis.text.x= element_text(size = 20, angle=45, hjust=1),axis.text.y= element_text(size= 20), axis.title = element_text(size = 20, face = 'bold'), legend.text = element_text(size= 12), aspect.ratio= 1, legend.title = element_text(face = 'bold', size = 12), legend.position = 'top')
IgM_total_epitope_counts_group_final_figure

### IgG total epitope counts
IgG_total_epitope_counts_group_final <- filter(epitope_counts_final, Antibody_Isotype == 'IgG',
                                               ZIKV_protein == 'Total')
IgG_total_epitope_counts_group_final$Timepoint = as.character(IgG_total_epitope_counts_group_final$Timepoint)
IgG_total_epitope_counts_group_final$Timepoint <- factor(IgG_total_epitope_counts_group_final$Timepoint, levels= c('27-31', '108-135'))
## Statistical analyses comparing total linear IgG epitope counts between virologic control groups (Mann-Whitney U-Tests)
# 27-31 days post-infection
IgG_total_epitope_counts_group_final_27.31dpi <- filter(IgG_total_epitope_counts_group_final, Timepoint == '27-31')
IgG_total_epitope_counts_group_final_27.31dpi_controller <- filter(IgG_total_epitope_counts_group_final_27.31dpi, Virologic_Control_Group == 'Controller')
IgG_total_epitope_counts_group_final_27.31dpi_controller_quantile <- quantile(IgG_total_epitope_counts_group_final_27.31dpi_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
IgG_total_epitope_counts_group_final_27.31dpi_controller_quantile
IgG_total_epitope_counts_group_final_27.31dpi_non_controller <- filter(IgG_total_epitope_counts_group_final, Virologic_Control_Group == 'Non-Controller')
IgG_total_epitope_counts_group_final_27.31dpi_non_controller_quantile <- quantile(IgG_total_epitope_counts_group_final_27.31dpi_non_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
IgG_total_epitope_counts_group_final_27.31dpi_non_controller_quantile
wilcox.test(Epitope_Count~Virologic_Control_Group, data = IgG_total_epitope_counts_group_final_27.31dpi, exact = FALSE)
# 108-135 days post-infection
IgG_total_epitope_counts_group_final_108.135dpi <- filter(IgG_total_epitope_counts_group_final, Timepoint == '108-135')
IgG_total_epitope_counts_group_final_108.135dpi_controller <- filter(IgG_total_epitope_counts_group_final_108.135dpi, Virologic_Control_Group == 'Controller')
IgG_total_epitope_counts_group_final_108.135dpi_controller_quantile <- quantile(IgG_total_epitope_counts_group_final_108.135dpi_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
IgG_total_epitope_counts_group_final_108.135dpi_controller_quantile
IgG_total_epitope_counts_group_final_108.135dpi_non_controller <- filter(IgG_total_epitope_counts_group_final, Virologic_Control_Group == 'Non-Controller')
IgG_total_epitope_counts_group_final_108.135dpi_non_controller_quantile <- quantile(IgG_total_epitope_counts_group_final_108.135dpi_non_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
IgG_total_epitope_counts_group_final_108.135dpi_non_controller_quantile
wilcox.test(Epitope_Count~Virologic_Control_Group, data = IgG_total_epitope_counts_group_final_108.135dpi, exact = FALSE)
# IgG total linear epitope counts figure
IgG_total_epitope_counts_group_final_figure <-
  ggplot() +
  geom_boxplot(data = IgG_total_epitope_counts_group_final, aes(x= Timepoint, y= Epitope_Count, color = Virologic_Control_Group), fill = 'white', outlier.colour =  'white') +
  geom_point(data = IgG_total_epitope_counts_group_final, aes(x= Timepoint, y= Epitope_Count, color = Virologic_Control_Group), position = position_jitterdodge(seed = 123, jitter.width = 0.5, jitter.height = 0.5), size= 2, shape= 16) +
  theme_classic() + 
  scale_color_manual(values = c('black', 'orange'), breaks = c('Controller', 'Non-Controller')) +
  guides(color = guide_legend('Virologic Control Group')) + 
  scale_x_discrete(name = 'Timepoint (DPI)') + 
  scale_y_continuous(name = 'Total IgG Epitope Count', expand = c(0,0), limits = c(-2,50), breaks = c(0, 10, 20, 30, 40, 50)) + 
  theme(axis.text.x= element_text(size = 20, angle= 45, hjust= 1), axis.text.y= element_text(size= 20), axis.title = element_text(size = 20, face = 'bold'), aspect.ratio= 1, legend.title = element_text(face = 'bold', size = 12), legend.text = element_text(size= 12), legend.position = 'top')
IgG_total_epitope_counts_group_final_figure


### IgM epitope counts in each region of the ZIKV polyprotein with dams combined into a single group
epitope_counts_final_IgM_combined <- filter(epitope_counts_final, Antibody_Isotype == 'IgM',
                                            ZIKV_protein == 'Capsid' |
                                              ZIKV_protein == 'PrM' |
                                              ZIKV_protein == 'M' |
                                              ZIKV_protein == 'E' |
                                              ZIKV_protein == 'NS1' |
                                              ZIKV_protein == 'NS2A' |
                                              ZIKV_protein == 'NS2B' |
                                              ZIKV_protein == 'NS3' |
                                              ZIKV_protein == 'NS4A' |
                                              ZIKV_protein == 'NS4B' |
                                              ZIKV_protein == 'NS5')
## 13-17 days post-infection
epitope_counts_final_IgM_combined_13.17dpi <- filter(epitope_counts_final_IgM_combined, Timepoint == '13-17')
# Statistical analysis to compare epitope counts across all regions of the ZIKV polyprotein (Kruskal-Wallis test, followed by a pairwise Mann-Whitney U-test)
kruskal.test(Epitope_Count~ZIKV_protein, data= epitope_counts_final_IgM_combined_13.17dpi)
pairwise.wilcox.test(epitope_counts_final_IgM_combined_13.17dpi$Epitope_Count, epitope_counts_final_IgM_combined_13.17dpi$ZIKV_protein, p.adjust.method = 'BH')
# 13-17 days post-infection figure
epitope_counts_final_IgM_combined_13.17dpi_figure <-
  ggplot() +
  geom_boxplot(data = epitope_counts_final_IgM_combined_13.17dpi, aes(x= ZIKV_protein, y= Epitope_Count, color = ZIKV_protein), fill = 'white', outlier.colour =  'white') +
  geom_point(data = epitope_counts_final_IgM_combined_13.17dpi, aes(x= ZIKV_protein, y= Epitope_Count, color = ZIKV_protein), position = position_jitterdodge(seed = 123, jitter.width = 0.5, jitter.height = 0.5), size= 2, shape= 16) +
  theme_classic() + 
  scale_color_manual(values = c('#d81b60', '#ff0000', '#b85151', '#002dff', '#818fea', '#000e64', '#19feff', '#00c128', '#793183', '#640043', '#d3a074'), 
                     breaks = c('Capsid', 'PrM', 'M', 'E', 'NS1', 'NS2A', 'NS2B', 'NS3', 'NS4A', 'NS4B', 'NS5')) +
  guides(color = guide_legend('ZIKV Protein')) + 
  scale_x_discrete(name = 'Region in ZIKV Polyprotein') + 
  scale_y_continuous(name = 'IgM Epitope Count', expand = c(0,0), limits = c(-2,32), breaks = c(0, 5, 10, 15, 20, 25, 30)) + 
  labs(title = '13-17 DPI') +
  theme(axis.text.x= element_text(size = 20, angle= 45, hjust= 1), axis.text.y= element_text(size= 20), axis.title = element_text(size = 20, face = 'bold'), aspect.ratio= 1, legend.title = element_text(face = 'bold', size = 12), legend.text = element_text(size= 12), legend.position = 'top', plot.title = element_text(size= 24, face= 'bold', hjust= 0.5))
epitope_counts_final_IgM_combined_13.17dpi_figure

## 21-24 days post-infection
epitope_counts_final_IgM_combined_21.24dpi <- filter(epitope_counts_final_IgM_combined, Timepoint == '21-24')
# Statistical analysis to compare epitope counts across all regions of the ZIKV polyprotein (Kruskal-Wallis test, followed by a pairwise Mann-Whitney U-test)
kruskal.test(Epitope_Count~ZIKV_protein, data= epitope_counts_final_IgM_combined_21.24dpi)
pairwise.wilcox.test(epitope_counts_final_IgM_combined_21.24dpi$Epitope_Count, epitope_counts_final_IgM_combined_21.24dpi$ZIKV_protein, p.adjust.method = 'BH')
# 21-24 days post-infection figure
epitope_counts_final_IgM_combined_21.24dpi_figure <-
  ggplot() +
  geom_boxplot(data = epitope_counts_final_IgM_combined_21.24dpi, aes(x= ZIKV_protein, y= Epitope_Count, color = ZIKV_protein), fill = 'white', outlier.colour =  'white') +
  geom_point(data = epitope_counts_final_IgM_combined_21.24dpi, aes(x= ZIKV_protein, y= Epitope_Count, color = ZIKV_protein), position = position_jitterdodge(seed = 123, jitter.width = 0.5, jitter.height = 0.5), size= 2, shape= 16) +
  theme_classic() + 
  scale_color_manual(values = c('#d81b60', '#ff0000', '#b85151', '#002dff', '#818fea', '#000e64', '#19feff', '#00c128', '#793183', '#640043', '#d3a074'), 
                     breaks = c('Capsid', 'PrM', 'M', 'E', 'NS1', 'NS2A', 'NS2B', 'NS3', 'NS4A', 'NS4B', 'NS5')) +
  guides(color = guide_legend('ZIKV Protein')) + 
  scale_x_discrete(name = 'Region in ZIKV Polyprotein') + 
  scale_y_continuous(name = 'IgM Epitope Count', expand = c(0,0), limits = c(-2,32), breaks = c(0, 5, 10, 15, 20, 25, 30)) + 
  labs(title = '21-24 DPI') +
  theme(axis.text.x= element_text(size = 20, angle= 45, hjust= 1), axis.text.y= element_text(size= 20), axis.title = element_text(size = 20, face = 'bold'), aspect.ratio= 1, legend.title = element_text(face = 'bold', size = 12), legend.text = element_text(size= 12), legend.position = 'top', plot.title = element_text(size= 24, face= 'bold', hjust= 0.5))
epitope_counts_final_IgM_combined_21.24dpi_figure

### IgG epitope counts in each region of the ZIKV polyprotein with dams combined into a single group
epitope_counts_final_IgG_combined <- filter(epitope_counts_final, Antibody_Isotype == 'IgG',
                                            ZIKV_protein == 'Capsid' |
                                              ZIKV_protein == 'PrM' |
                                              ZIKV_protein == 'M' |
                                              ZIKV_protein == 'E' |
                                              ZIKV_protein == 'NS1' |
                                              ZIKV_protein == 'NS2A' |
                                              ZIKV_protein == 'NS2B' |
                                              ZIKV_protein == 'NS3' |
                                              ZIKV_protein == 'NS4A' |
                                              ZIKV_protein == 'NS4B' |
                                              ZIKV_protein == 'NS5')
## 27-31 days post-infection
epitope_counts_final_IgG_combined_27.31dpi <- filter(epitope_counts_final_IgG_combined, Timepoint == '27-31')
# Statistical analysis to compare epitope counts across all regions of the ZIKV polyprotein (Kruskal-Wallis test, followed by a pairwise Mann-Whitney U-test)
kruskal.test(Epitope_Count~ZIKV_protein, data= epitope_counts_final_IgG_combined_27.31dpi)
pairwise.wilcox.test(epitope_counts_final_IgG_combined_27.31dpi$Epitope_Count, epitope_counts_final_IgG_combined_27.31dpi$ZIKV_protein, p.adjust.method = 'BH')
# 27-31 days post-infection figure
epitope_counts_final_IgG_combined_27.31dpi_figure <-
  ggplot() +
  geom_boxplot(data = epitope_counts_final_IgG_combined_27.31dpi, aes(x= ZIKV_protein, y= Epitope_Count, color = ZIKV_protein), fill = 'white', outlier.colour =  'white') +
  geom_point(data = epitope_counts_final_IgG_combined_27.31dpi, aes(x= ZIKV_protein, y= Epitope_Count, color = ZIKV_protein), position = position_jitterdodge(seed = 123, jitter.width = 0.5, jitter.height = 0.5), size= 2, shape= 16) +
  theme_classic() + 
  scale_color_manual(values = c('#d81b60', '#ff0000', '#b85151', '#002dff', '#818fea', '#000e64', '#19feff', '#00c128', '#793183', '#640043', '#d3a074'), 
                     breaks = c('Capsid', 'PrM', 'M', 'E', 'NS1', 'NS2A', 'NS2B', 'NS3', 'NS4A', 'NS4B', 'NS5')) +
  guides(color = guide_legend('ZIKV Protein')) + 
  scale_x_discrete(name = 'Region in ZIKV Polyprotein') + 
  scale_y_continuous(name = 'IgG Epitope Count', expand = c(0,0), limits = c(-2,15), breaks = c(0, 5, 10, 15)) + 
  labs(title = '27-31 DPI') +
  theme(axis.text.x= element_text(size = 20, angle= 45, hjust= 1), axis.text.y= element_text(size= 20), axis.title = element_text(size = 20, face = 'bold'), aspect.ratio= 1, legend.title = element_text(face = 'bold', size = 12), legend.text = element_text(size= 12), legend.position = 'top', plot.title = element_text(size= 24, face= 'bold', hjust= 0.5))
epitope_counts_final_IgG_combined_27.31dpi_figure

## 108-135 days post-infection
epitope_counts_final_IgG_combined_108.135dpi <- filter(epitope_counts_final_IgG_combined, Timepoint == '108-135')
# Statistical analysis to compare epitope counts across all regions of the ZIKV polyprotein (Kruskal-Wallis test, followed by a pairwise Mann-Whitney U-test)
kruskal.test(Epitope_Count~ZIKV_protein, data= epitope_counts_final_IgG_combined_108.135dpi)
pairwise.wilcox.test(epitope_counts_final_IgG_combined_108.135dpi$Epitope_Count, epitope_counts_final_IgG_combined_108.135dpi$ZIKV_protein, p.adjust.method = 'BH')
# 108-135 days post-infection figure
epitope_counts_final_IgG_combined_108.135dpi_figure <-
  ggplot() +
  geom_boxplot(data = epitope_counts_final_IgG_combined_108.135dpi, aes(x= ZIKV_protein, y= Epitope_Count, color = ZIKV_protein), fill = 'white', outlier.colour =  'white') +
  geom_point(data = epitope_counts_final_IgG_combined_108.135dpi, aes(x= ZIKV_protein, y= Epitope_Count, color = ZIKV_protein), position = position_jitterdodge(seed = 123, jitter.width = 0.5, jitter.height = 0.5), size= 2, shape= 16) +
  theme_classic() + 
  scale_color_manual(values = c('#d81b60', '#ff0000', '#b85151', '#002dff', '#818fea', '#000e64', '#19feff', '#00c128', '#793183', '#640043', '#d3a074'), 
                     breaks = c('Capsid', 'PrM', 'M', 'E', 'NS1', 'NS2A', 'NS2B', 'NS3', 'NS4A', 'NS4B', 'NS5')) +
  guides(color = guide_legend('ZIKV Protein')) + 
  scale_x_discrete(name = 'Region in ZIKV Polyprotein') + 
  scale_y_continuous(name = 'IgG Epitope Count', expand = c(0,0), limits = c(-2,15), breaks = c(0, 5, 10, 15)) + 
  labs(title = '108-135 DPI') +
  theme(axis.text.x= element_text(size = 20, angle= 45, hjust= 1), axis.text.y= element_text(size= 20), axis.title = element_text(size = 20, face = 'bold'), aspect.ratio= 1, legend.title = element_text(face = 'bold', size = 12), legend.text = element_text(size= 12), legend.position = 'top', plot.title = element_text(size= 24, face= 'bold', hjust= 0.5))
epitope_counts_final_IgG_combined_108.135dpi_figure

###
# Combining total epitope counts (by group) and epitope counts in each region of the ZIKV polyprotein (dams combined) into a single multipanel figure
###
tiff('Epitope Counts-Main Figure (Final).tiff', units= 'in', height= 16, width= 18, res= 300)
ggarrange(IgM_total_epitope_counts_group_final_figure, epitope_counts_final_IgM_combined_13.17dpi_figure, epitope_counts_final_IgM_combined_21.24dpi_figure, IgG_total_epitope_counts_group_final_figure, epitope_counts_final_IgG_combined_27.31dpi_figure, epitope_counts_final_IgG_combined_108.135dpi_figure, nrow=2, ncol=3, align= 'hv', labels= c('A', 'B', 'C', 'D', 'E', 'F'))
dev.off()

