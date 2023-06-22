library(readxl)
library(ggplot2)
library(tidyverse)
library(ggpubr)
library(gtable)

epitope_counts_final <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\Peptide Array\\Final Raw Data, figures, and code\\Peptide Array-Epitope Counts (Final).xlsx", sheet= 2, col_names = TRUE)
epitope_counts_final$Virologic_Control_Group = as.character(epitope_counts_final$Virologic_Control_Group)
epitope_counts_final$Virologic_Control_Group <- factor(epitope_counts_final$Virologic_Control_Group, levels = c('Controller', 'Non-Controller'))
epitope_counts_final$ZIKV_protein = as.character(epitope_counts_final$ZIKV_protein)
epitope_counts_final$ZIKV_protein <- factor(epitope_counts_final$ZIKV_protein, levels = c('Capsid', 'PrM', 'Mem', 'Env', 'NS1', 'NS2A', 'NS2B', 'NS3', 'NS4A', 'NS4B', 'NS5', 'Total'))
epitope_counts_final$Epitope_Count = as.numeric(epitope_counts_final$Epitope_Count)

###
#supplemental figure comparing epitope counts between virologic control groups within individual 
#regions of the ZIKV polyprotein
###

###IgM

## 2 Weeks post-infection (WPI)
epitope_counts_final_IgM_2wpi <- filter(epitope_counts_final, Timepoint == '2',
                                        ZIKV_protein == 'Capsid' |
                                          ZIKV_protein == 'PrM' |
                                          ZIKV_protein == 'Mem' |
                                          ZIKV_protein == 'Env' |
                                          ZIKV_protein == 'NS1' |
                                          ZIKV_protein == 'NS2A' |
                                          ZIKV_protein == 'NS2B' |
                                          ZIKV_protein == 'NS3' |
                                          ZIKV_protein == 'NS4A' |
                                          ZIKV_protein == 'NS4B' |
                                          ZIKV_protein == 'NS5')
# Statistical analyses comparing epitope counts within each region of the ZIKV polyprotein between virologic control groups (Mann-Whitney U-Tests)
#Capsid
epitope_counts_final_IgM_2wpi_capsid <- filter(epitope_counts_final_IgM_2wpi, ZIKV_protein == 'Capsid')
epitope_counts_final_IgM_2wpi_capsid_controller <- filter(epitope_counts_final_IgM_2wpi_capsid, Virologic_Control_Group == 'Controller')
epitope_counts_final_IgM_2wpi_capsid_controller_quantile <- quantile(epitope_counts_final_IgM_2wpi_capsid_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgM_2wpi_capsid_controller_quantile
epitope_counts_final_IgM_2wpi_capsid_non_controller <- filter(epitope_counts_final_IgM_2wpi_capsid, Virologic_Control_Group == 'Non-Controller')
epitope_counts_final_IgM_2wpi_capsid_non_controller_quantile <- quantile(epitope_counts_final_IgM_2wpi_capsid_non_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgM_2wpi_capsid_non_controller_quantile
wilcox.test(Epitope_Count~Virologic_Control_Group, data = epitope_counts_final_IgM_2wpi_capsid, exact = FALSE)
#Pre-membrane (PrM)
epitope_counts_final_IgM_2wpi_PrM <- filter(epitope_counts_final_IgM_2wpi, ZIKV_protein == 'PrM')
epitope_counts_final_IgM_2wpi_PrM_controller <- filter(epitope_counts_final_IgM_2wpi_PrM, Virologic_Control_Group == 'Controller')
epitope_counts_final_IgM_2wpi_PrM_controller_quantile <- quantile(epitope_counts_final_IgM_2wpi_PrM_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgM_2wpi_PrM_controller_quantile
epitope_counts_final_IgM_2wpi_PrM_non_controller <- filter(epitope_counts_final_IgM_2wpi_PrM, Virologic_Control_Group == 'Non-Controller')
epitope_counts_final_IgM_2wpi_PrM_non_controller_quantile <- quantile(epitope_counts_final_IgM_2wpi_PrM_non_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgM_2wpi_PrM_non_controller_quantile
wilcox.test(Epitope_Count~Virologic_Control_Group, data = epitope_counts_final_IgM_2wpi_PrM, exact = FALSE)
#Membrane (mem)
epitope_counts_final_IgM_2wpi_Mem <- filter(epitope_counts_final_IgM_2wpi, ZIKV_protein == 'Mem')
epitope_counts_final_IgM_2wpi_Mem_controller <- filter(epitope_counts_final_IgM_2wpi_Mem, Virologic_Control_Group == 'Controller')
epitope_counts_final_IgM_2wpi_Mem_controller_quantile <- quantile(epitope_counts_final_IgM_2wpi_Mem_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgM_2wpi_Mem_controller_quantile
epitope_counts_final_IgM_2wpi_Mem_non_controller <- filter(epitope_counts_final_IgM_2wpi_Mem, Virologic_Control_Group == 'Non-Controller')
epitope_counts_final_IgM_2wpi_Mem_non_controller_quantile <- quantile(epitope_counts_final_IgM_2wpi_Mem_non_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgM_2wpi_Mem_non_controller_quantile
wilcox.test(Epitope_Count~Virologic_Control_Group, data = epitope_counts_final_IgM_2wpi_Mem, exact = FALSE)
#Envelope (Env)
epitope_counts_final_IgM_2wpi_Env <- filter(epitope_counts_final_IgM_2wpi, ZIKV_protein == 'Env')
epitope_counts_final_IgM_2wpi_Env_controller <- filter(epitope_counts_final_IgM_2wpi_Env, Virologic_Control_Group == 'Controller')
epitope_counts_final_IgM_2wpi_Env_controller_quantile <- quantile(epitope_counts_final_IgM_2wpi_Env_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgM_2wpi_Env_controller_quantile
epitope_counts_final_IgM_2wpi_Env_non_controller <- filter(epitope_counts_final_IgM_2wpi_Env, Virologic_Control_Group == 'Non-Controller')
epitope_counts_final_IgM_2wpi_Env_non_controller_quantile <- quantile(epitope_counts_final_IgM_2wpi_Env_non_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgM_2wpi_Env_non_controller_quantile
wilcox.test(Epitope_Count~Virologic_Control_Group, data = epitope_counts_final_IgM_2wpi_Env, exact = FALSE)
#NS1 
epitope_counts_final_IgM_2wpi_NS1 <- filter(epitope_counts_final_IgM_2wpi, ZIKV_protein == 'NS1')
epitope_counts_final_IgM_2wpi_NS1_controller <- filter(epitope_counts_final_IgM_2wpi_NS1, Virologic_Control_Group == 'Controller')
epitope_counts_final_IgM_2wpi_NS1_controller_quantile <- quantile(epitope_counts_final_IgM_2wpi_NS1_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgM_2wpi_NS1_controller_quantile
epitope_counts_final_IgM_2wpi_NS1_non_controller <- filter(epitope_counts_final_IgM_2wpi_NS1, Virologic_Control_Group == 'Non-Controller')
epitope_counts_final_IgM_2wpi_NS1_non_controller_quantile <- quantile(epitope_counts_final_IgM_2wpi_NS1_non_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgM_2wpi_NS1_non_controller_quantile
wilcox.test(Epitope_Count~Virologic_Control_Group, data = epitope_counts_final_IgM_2wpi_NS1, exact = FALSE)
#NS2A
epitope_counts_final_IgM_2wpi_NS2A <- filter(epitope_counts_final_IgM_2wpi, ZIKV_protein == 'NS2A')
epitope_counts_final_IgM_2wpi_NS2A_controller <- filter(epitope_counts_final_IgM_2wpi_NS2A, Virologic_Control_Group == 'Controller')
epitope_counts_final_IgM_2wpi_NS2A_controller_quantile <- quantile(epitope_counts_final_IgM_2wpi_NS2A_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgM_2wpi_NS2A_controller_quantile
epitope_counts_final_IgM_2wpi_NS2A_non_controller <- filter(epitope_counts_final_IgM_2wpi_NS2A, Virologic_Control_Group == 'Non-Controller')
epitope_counts_final_IgM_2wpi_NS2A_non_controller_quantile <- quantile(epitope_counts_final_IgM_2wpi_NS2A_non_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgM_2wpi_NS2A_non_controller_quantile
wilcox.test(Epitope_Count~Virologic_Control_Group, data = epitope_counts_final_IgM_2wpi_NS2A, exact = FALSE)
#N2B
epitope_counts_final_IgM_2wpi_NS2B <- filter(epitope_counts_final_IgM_2wpi, ZIKV_protein == 'NS2B')
epitope_counts_final_IgM_2wpi_NS2B_controller <- filter(epitope_counts_final_IgM_2wpi_NS2B, Virologic_Control_Group == 'Controller')
epitope_counts_final_IgM_2wpi_NS2B_controller_quantile <- quantile(epitope_counts_final_IgM_2wpi_NS2B_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgM_2wpi_NS2B_controller_quantile
epitope_counts_final_IgM_2wpi_NS2B_non_controller <- filter(epitope_counts_final_IgM_2wpi_NS2B, Virologic_Control_Group == 'Non-Controller')
epitope_counts_final_IgM_2wpi_NS2B_non_controller_quantile <- quantile(epitope_counts_final_IgM_2wpi_NS2B_non_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgM_2wpi_NS2B_non_controller_quantile
wilcox.test(Epitope_Count~Virologic_Control_Group, data = epitope_counts_final_IgM_2wpi_NS2B, exact = FALSE)
#NS3
epitope_counts_final_IgM_2wpi_NS3 <- filter(epitope_counts_final_IgM_2wpi, ZIKV_protein == 'NS3')
epitope_counts_final_IgM_2wpi_NS3_controller <- filter(epitope_counts_final_IgM_2wpi_NS3, Virologic_Control_Group == 'Controller')
epitope_counts_final_IgM_2wpi_NS3_controller_quantile <- quantile(epitope_counts_final_IgM_2wpi_NS3_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgM_2wpi_NS3_controller_quantile
epitope_counts_final_IgM_2wpi_NS3_non_controller <- filter(epitope_counts_final_IgM_2wpi_NS3, Virologic_Control_Group == 'Non-Controller')
epitope_counts_final_IgM_2wpi_NS3_non_controller_quantile <- quantile(epitope_counts_final_IgM_2wpi_NS3_non_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgM_2wpi_NS3_non_controller_quantile
wilcox.test(Epitope_Count~Virologic_Control_Group, data = epitope_counts_final_IgM_2wpi_NS3, exact = FALSE)
#NS4A
epitope_counts_final_IgM_2wpi_NS4A <- filter(epitope_counts_final_IgM_2wpi, ZIKV_protein == 'NS4A')
epitope_counts_final_IgM_2wpi_NS4A_controller <- filter(epitope_counts_final_IgM_2wpi_NS4A, Virologic_Control_Group == 'Controller')
epitope_counts_final_IgM_2wpi_NS4A_controller_quantile <- quantile(epitope_counts_final_IgM_2wpi_NS4A_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgM_2wpi_NS4A_controller_quantile
epitope_counts_final_IgM_2wpi_NS4A_non_controller <- filter(epitope_counts_final_IgM_2wpi_NS4A, Virologic_Control_Group == 'Non-Controller')
epitope_counts_final_IgM_2wpi_NS4A_non_controller_quantile <- quantile(epitope_counts_final_IgM_2wpi_NS4A_non_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgM_2wpi_NS4A_non_controller_quantile
wilcox.test(Epitope_Count~Virologic_Control_Group, data = epitope_counts_final_IgM_2wpi_NS4A, exact = FALSE)
#NS4B
epitope_counts_final_IgM_2wpi_NS4B <- filter(epitope_counts_final_IgM_2wpi, ZIKV_protein == 'NS4B')
epitope_counts_final_IgM_2wpi_NS4B_controller <- filter(epitope_counts_final_IgM_2wpi_NS4B, Virologic_Control_Group == 'Controller')
epitope_counts_final_IgM_2wpi_NS4B_controller_quantile <- quantile(epitope_counts_final_IgM_2wpi_NS4B_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgM_2wpi_NS4B_controller_quantile
epitope_counts_final_IgM_2wpi_NS4B_non_controller <- filter(epitope_counts_final_IgM_2wpi_NS4B, Virologic_Control_Group == 'Non-Controller')
epitope_counts_final_IgM_2wpi_NS4B_non_controller_quantile <- quantile(epitope_counts_final_IgM_2wpi_NS4B_non_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgM_2wpi_NS4B_non_controller_quantile
wilcox.test(Epitope_Count~Virologic_Control_Group, data = epitope_counts_final_IgM_2wpi_NS4B, exact = FALSE)
#NS5
epitope_counts_final_IgM_2wpi_NS5 <- filter(epitope_counts_final_IgM_2wpi, ZIKV_protein == 'NS5')
epitope_counts_final_IgM_2wpi_NS5_controller <- filter(epitope_counts_final_IgM_2wpi_NS5, Virologic_Control_Group == 'Controller')
epitope_counts_final_IgM_2wpi_NS5_controller_quantile <- quantile(epitope_counts_final_IgM_2wpi_NS5_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgM_2wpi_NS5_controller_quantile
epitope_counts_final_IgM_2wpi_NS5_non_controller <- filter(epitope_counts_final_IgM_2wpi_NS5, Virologic_Control_Group == 'Non-Controller')
epitope_counts_final_IgM_2wpi_NS5_non_controller_quantile <- quantile(epitope_counts_final_IgM_2wpi_NS5_non_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgM_2wpi_NS5_non_controller_quantile
wilcox.test(Epitope_Count~Virologic_Control_Group, data = epitope_counts_final_IgM_2wpi_NS5, exact = FALSE)
#2 WPI IgM epitope counts (group) figure
epitope_counts_final_IgM_2wpi_group_figure <-
  ggplot() +
  geom_boxplot(data = epitope_counts_final_IgM_2wpi, aes(x= ZIKV_protein, y= Epitope_Count, color = Virologic_Control_Group), fill = 'white', outlier.colour =  'white') +
  geom_point(data = epitope_counts_final_IgM_2wpi, aes(x= ZIKV_protein, y= Epitope_Count, color = Virologic_Control_Group), position = position_jitterdodge(seed = 123, jitter.width = 0.5, jitter.height = 0.5), size= 1, shape= 16) +
  theme_classic() + 
  scale_color_manual(values = c('#000000', '#b5b5b5'), breaks = c('Controller', 'Non-Controller')) +
  guides(color = guide_legend('Virologic Control Group')) + 
  scale_x_discrete(name = 'Region in ZIKV Polyprotein', expand = c(0,0)) + 
  scale_y_continuous(name = 'IgM Epitope Count', expand = c(0,0), limits = c(-2,30), breaks = c(0, 5, 10, 15, 20, 25, 30)) + 
  labs(title= '2 WPI') +
  theme(axis.text.x= element_text(size = 20, angle = 45, hjust = 1, vjust = 1), axis.text.y = element_text(size = 20), axis.title = element_text(size = 20, face = 'bold'), aspect.ratio= 1, legend.title = element_text(size = 20, face = 'bold'), legend.text = element_text(size = 20), legend.position = 'top', plot.title = element_text(size= 24, face = 'bold', hjust = 0.5))
epitope_counts_final_IgM_2wpi_group_figure  
## 3 Weeks post-infection (WPI)
epitope_counts_final_IgM_3.4mpi <- filter(epitope_counts_final, Timepoint == '3',
                                        ZIKV_protein == 'Capsid' |
                                          ZIKV_protein == 'PrM' |
                                          ZIKV_protein == 'Mem' |
                                          ZIKV_protein == 'Env' |
                                          ZIKV_protein == 'NS1' |
                                          ZIKV_protein == 'NS2A' |
                                          ZIKV_protein == 'NS2B' |
                                          ZIKV_protein == 'NS3' |
                                          ZIKV_protein == 'NS4A' |
                                          ZIKV_protein == 'NS4B' |
                                          ZIKV_protein == 'NS5')
# Statistical analyses comparing epitope counts within each region of the ZIKV polyprotein between virologic control groups (Mann-Whitney U-Tests)
#Capsid
epitope_counts_final_IgM_3wpi_capsid <- filter(epitope_counts_final_IgM_3wpi, ZIKV_protein == 'Capsid')
epitope_counts_final_IgM_3wpi_capsid_controller <- filter(epitope_counts_final_IgM_3wpi_capsid, Virologic_Control_Group == 'Controller')
epitope_counts_final_IgM_3wpi_capsid_controller_quantile <- quantile(epitope_counts_final_IgM_3wpi_capsid_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgM_3wpi_capsid_controller_quantile
epitope_counts_final_IgM_3wpi_capsid_non_controller <- filter(epitope_counts_final_IgM_3wpi_capsid, Virologic_Control_Group == 'Non-Controller')
epitope_counts_final_IgM_3wpi_capsid_non_controller_quantile <- quantile(epitope_counts_final_IgM_3wpi_capsid_non_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgM_3wpi_capsid_non_controller_quantile
wilcox.test(Epitope_Count~Virologic_Control_Group, data = epitope_counts_final_IgM_3wpi_capsid, exact = FALSE)
#Pre-membrane (PrM)
epitope_counts_final_IgM_3wpi_PrM <- filter(epitope_counts_final_IgM_3wpi, ZIKV_protein == 'PrM')
epitope_counts_final_IgM_3wpi_PrM_controller <- filter(epitope_counts_final_IgM_3wpi_PrM, Virologic_Control_Group == 'Controller')
epitope_counts_final_IgM_3wpi_PrM_controller_quantile <- quantile(epitope_counts_final_IgM_3wpi_PrM_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgM_3wpi_PrM_controller_quantile
epitope_counts_final_IgM_3wpi_PrM_non_controller <- filter(epitope_counts_final_IgM_3wpi_PrM, Virologic_Control_Group == 'Non-Controller')
epitope_counts_final_IgM_3wpi_PrM_non_controller_quantile <- quantile(epitope_counts_final_IgM_3wpi_PrM_non_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgM_3wpi_PrM_non_controller_quantile
wilcox.test(Epitope_Count~Virologic_Control_Group, data = epitope_counts_final_IgM_3wpi_PrM, exact = FALSE)
#Membrane (mem)
epitope_counts_final_IgM_3wpi_Mem <- filter(epitope_counts_final_IgM_3wpi, ZIKV_protein == 'Mem')
epitope_counts_final_IgM_3wpi_Mem_controller <- filter(epitope_counts_final_IgM_3wpi_Mem, Virologic_Control_Group == 'Controller')
epitope_counts_final_IgM_3wpi_Mem_controller_quantile <- quantile(epitope_counts_final_IgM_3wpi_Mem_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgM_3wpi_Mem_controller_quantile
epitope_counts_final_IgM_3wpi_Mem_non_controller <- filter(epitope_counts_final_IgM_3wpi_Mem, Virologic_Control_Group == 'Non-Controller')
epitope_counts_final_IgM_3wpi_Mem_non_controller_quantile <- quantile(epitope_counts_final_IgM_3wpi_Mem_non_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgM_3wpi_Mem_non_controller_quantile
wilcox.test(Epitope_Count~Virologic_Control_Group, data = epitope_counts_final_IgM_3wpi_Mem, exact = FALSE)
#Envelope (Env)
epitope_counts_final_IgM_3wpi_Env <- filter(epitope_counts_final_IgM_3wpi, ZIKV_protein == 'Env')
epitope_counts_final_IgM_3wpi_Env_controller <- filter(epitope_counts_final_IgM_3wpi_Env, Virologic_Control_Group == 'Controller')
epitope_counts_final_IgM_3wpi_Env_controller_quantile <- quantile(epitope_counts_final_IgM_3wpi_Env_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgM_3wpi_Env_controller_quantile
epitope_counts_final_IgM_3wpi_Env_non_controller <- filter(epitope_counts_final_IgM_3wpi_Env, Virologic_Control_Group == 'Non-Controller')
epitope_counts_final_IgM_3wpi_Env_non_controller_quantile <- quantile(epitope_counts_final_IgM_3wpi_Env_non_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgM_3wpi_Env_non_controller_quantile
wilcox.test(Epitope_Count~Virologic_Control_Group, data = epitope_counts_final_IgM_3wpi_Env, exact = FALSE)
#NS1 
epitope_counts_final_IgM_3wpi_NS1 <- filter(epitope_counts_final_IgM_3wpi, ZIKV_protein == 'NS1')
epitope_counts_final_IgM_3wpi_NS1_controller <- filter(epitope_counts_final_IgM_3wpi_NS1, Virologic_Control_Group == 'Controller')
epitope_counts_final_IgM_3wpi_NS1_controller_quantile <- quantile(epitope_counts_final_IgM_3wpi_NS1_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgM_3wpi_NS1_controller_quantile
epitope_counts_final_IgM_3wpi_NS1_non_controller <- filter(epitope_counts_final_IgM_3wpi_NS1, Virologic_Control_Group == 'Non-Controller')
epitope_counts_final_IgM_3wpi_NS1_non_controller_quantile <- quantile(epitope_counts_final_IgM_3wpi_NS1_non_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgM_3wpi_NS1_non_controller_quantile
wilcox.test(Epitope_Count~Virologic_Control_Group, data = epitope_counts_final_IgM_3wpi_NS1, exact = FALSE)
#NS2A
epitope_counts_final_IgM_3wpi_NS2A <- filter(epitope_counts_final_IgM_3wpi, ZIKV_protein == 'NS2A')
epitope_counts_final_IgM_3wpi_NS2A_controller <- filter(epitope_counts_final_IgM_3wpi_NS2A, Virologic_Control_Group == 'Controller')
epitope_counts_final_IgM_3wpi_NS2A_controller_quantile <- quantile(epitope_counts_final_IgM_3wpi_NS2A_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgM_3wpi_NS2A_controller_quantile
epitope_counts_final_IgM_3wpi_NS2A_non_controller <- filter(epitope_counts_final_IgM_3wpi_NS2A, Virologic_Control_Group == 'Non-Controller')
epitope_counts_final_IgM_3wpi_NS2A_non_controller_quantile <- quantile(epitope_counts_final_IgM_3wpi_NS2A_non_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgM_3wpi_NS2A_non_controller_quantile
wilcox.test(Epitope_Count~Virologic_Control_Group, data = epitope_counts_final_IgM_3wpi_NS2A, exact = FALSE)
#N2B
epitope_counts_final_IgM_3wpi_NS2B <- filter(epitope_counts_final_IgM_3wpi, ZIKV_protein == 'NS2B')
epitope_counts_final_IgM_3wpi_NS2B_controller <- filter(epitope_counts_final_IgM_3wpi_NS2B, Virologic_Control_Group == 'Controller')
epitope_counts_final_IgM_3wpi_NS2B_controller_quantile <- quantile(epitope_counts_final_IgM_3wpi_NS2B_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgM_3wpi_NS2B_controller_quantile
epitope_counts_final_IgM_3wpi_NS2B_non_controller <- filter(epitope_counts_final_IgM_3wpi_NS2B, Virologic_Control_Group == 'Non-Controller')
epitope_counts_final_IgM_3wpi_NS2B_non_controller_quantile <- quantile(epitope_counts_final_IgM_3wpi_NS2B_non_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgM_3wpi_NS2B_non_controller_quantile
wilcox.test(Epitope_Count~Virologic_Control_Group, data = epitope_counts_final_IgM_3wpi_NS2B, exact = FALSE)
#NS3
epitope_counts_final_IgM_3wpi_NS3 <- filter(epitope_counts_final_IgM_3wpi, ZIKV_protein == 'NS3')
epitope_counts_final_IgM_3wpi_NS3_controller <- filter(epitope_counts_final_IgM_3wpi_NS3, Virologic_Control_Group == 'Controller')
epitope_counts_final_IgM_3wpi_NS3_controller_quantile <- quantile(epitope_counts_final_IgM_3wpi_NS3_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgM_3wpi_NS3_controller_quantile
epitope_counts_final_IgM_3wpi_NS3_non_controller <- filter(epitope_counts_final_IgM_3wpi_NS3, Virologic_Control_Group == 'Non-Controller')
epitope_counts_final_IgM_3wpi_NS3_non_controller_quantile <- quantile(epitope_counts_final_IgM_3wpi_NS3_non_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgM_3wpi_NS3_non_controller_quantile
wilcox.test(Epitope_Count~Virologic_Control_Group, data = epitope_counts_final_IgM_3wpi_NS3, exact = FALSE)
#NS4A
epitope_counts_final_IgM_3wpi_NS4A <- filter(epitope_counts_final_IgM_3wpi, ZIKV_protein == 'NS4A')
epitope_counts_final_IgM_3wpi_NS4A_controller <- filter(epitope_counts_final_IgM_3wpi_NS4A, Virologic_Control_Group == 'Controller')
epitope_counts_final_IgM_3wpi_NS4A_controller_quantile <- quantile(epitope_counts_final_IgM_3wpi_NS4A_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgM_3wpi_NS4A_controller_quantile
epitope_counts_final_IgM_3wpi_NS4A_non_controller <- filter(epitope_counts_final_IgM_3wpi_NS4A, Virologic_Control_Group == 'Non-Controller')
epitope_counts_final_IgM_3wpi_NS4A_non_controller_quantile <- quantile(epitope_counts_final_IgM_3wpi_NS4A_non_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgM_3wpi_NS4A_non_controller_quantile
wilcox.test(Epitope_Count~Virologic_Control_Group, data = epitope_counts_final_IgM_3wpi_NS4A, exact = FALSE)
#NS4B
epitope_counts_final_IgM_3wpi_NS4B <- filter(epitope_counts_final_IgM_3wpi, ZIKV_protein == 'NS4B')
epitope_counts_final_IgM_3wpi_NS4B_controller <- filter(epitope_counts_final_IgM_3wpi_NS4B, Virologic_Control_Group == 'Controller')
epitope_counts_final_IgM_3wpi_NS4B_controller_quantile <- quantile(epitope_counts_final_IgM_3wpi_NS4B_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgM_3wpi_NS4B_controller_quantile
epitope_counts_final_IgM_3wpi_NS4B_non_controller <- filter(epitope_counts_final_IgM_3wpi_NS4B, Virologic_Control_Group == 'Non-Controller')
epitope_counts_final_IgM_3wpi_NS4B_non_controller_quantile <- quantile(epitope_counts_final_IgM_3wpi_NS4B_non_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgM_3wpi_NS4B_non_controller_quantile
wilcox.test(Epitope_Count~Virologic_Control_Group, data = epitope_counts_final_IgM_3wpi_NS4B, exact = FALSE)
#NS5
epitope_counts_final_IgM_3wpi_NS5 <- filter(epitope_counts_final_IgM_3wpi, ZIKV_protein == 'NS5')
epitope_counts_final_IgM_3wpi_NS5_controller <- filter(epitope_counts_final_IgM_3wpi_NS5, Virologic_Control_Group == 'Controller')
epitope_counts_final_IgM_3wpi_NS5_controller_quantile <- quantile(epitope_counts_final_IgM_3wpi_NS5_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgM_3wpi_NS5_controller_quantile
epitope_counts_final_IgM_3wpi_NS5_non_controller <- filter(epitope_counts_final_IgM_3wpi_NS5, Virologic_Control_Group == 'Non-Controller')
epitope_counts_final_IgM_3wpi_NS5_non_controller_quantile <- quantile(epitope_counts_final_IgM_3wpi_NS5_non_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgM_3wpi_NS5_non_controller_quantile
wilcox.test(Epitope_Count~Virologic_Control_Group, data = epitope_counts_final_IgM_3wpi_NS5, exact = FALSE)
#3 WPI IgM epitope counts (group) figure
epitope_counts_final_IgM_3wpi_group_figure <-
  ggplot() +
  geom_boxplot(data = epitope_counts_final_IgM_3wpi, aes(x= ZIKV_protein, y= Epitope_Count, color = Virologic_Control_Group), fill = 'white', outlier.colour =  'white') +
  geom_point(data = epitope_counts_final_IgM_3wpi, aes(x= ZIKV_protein, y= Epitope_Count, color = Virologic_Control_Group), position = position_jitterdodge(seed = 123, jitter.width = 0.5, jitter.height = 0.5), size= 1, shape= 16) +
  theme_classic() + 
  scale_color_manual(values = c('#000000', '#b5b5b5'), breaks = c('Controller', 'Non-Controller')) +
  guides(color = 'none') + 
  scale_x_discrete(name = 'Region in ZIKV Polyprotein', expand = c(0,0)) + 
  scale_y_continuous(name = 'IgM Epitope Count', expand = c(0,0), limits = c(-2,32), breaks = c(0, 5, 10, 15, 20, 25, 30)) + 
  labs(title= '3 WPI') +
  theme(axis.text.x= element_text(size = 20, angle = 45, hjust = 1, vjust = 1), axis.text.y = element_text(size = 20), axis.title = element_text(size = 20, face = 'bold'), aspect.ratio= 1, plot.title = element_text(size= 24, face = 'bold', hjust = 0.5))
epitope_counts_final_IgM_3wpi_group_figure  


###IgG

## 1 Month post-infection (MPI)
epitope_counts_final_IgG_1mpi<- filter(epitope_counts_final, Timepoint == '1',
                                        ZIKV_protein == 'Capsid' |
                                          ZIKV_protein == 'PrM' |
                                          ZIKV_protein == 'Mem' |
                                          ZIKV_protein == 'Env' |
                                          ZIKV_protein == 'NS1' |
                                          ZIKV_protein == 'NS2A' |
                                          ZIKV_protein == 'NS2B' |
                                          ZIKV_protein == 'NS3' |
                                          ZIKV_protein == 'NS4A' |
                                          ZIKV_protein == 'NS4B' |
                                          ZIKV_protein == 'NS5')
# Statistical analyses comparing epitope counts within each region of the ZIKV polyprotein between virologic control groups (Mann-Whitney U-Tests)
#Capsid
epitope_counts_final_IgG_1mpi_capsid <- filter(epitope_counts_final_IgG_1mpi, ZIKV_protein == 'Capsid')
epitope_counts_final_IgG_1mpi_capsid_controller <- filter(epitope_counts_final_IgG_1mpi_capsid, Virologic_Control_Group == 'Controller')
epitope_counts_final_IgG_1mpi_capsid_controller_quantile <- quantile(epitope_counts_final_IgG_1mpi_capsid_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgG_1mpi_capsid_controller_quantile
epitope_counts_final_IgG_1mpi_capsid_non_controller <- filter(epitope_counts_final_IgG_1mpi_capsid, Virologic_Control_Group == 'Non-Controller')
epitope_counts_final_IgG_1mpi_capsid_non_controller_quantile <- quantile(epitope_counts_final_IgG_1mpi_capsid_non_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgG_1mpi_capsid_non_controller_quantile
wilcox.test(Epitope_Count~Virologic_Control_Group, data = epitope_counts_final_IgG_1mpi_capsid, exact = FALSE)
#Pre-membrane (PrM)
epitope_counts_final_IgG_1mpi_PrM <- filter(epitope_counts_final_IgG_1mpi, ZIKV_protein == 'PrM')
epitope_counts_final_IgG_1mpi_PrM_controller <- filter(epitope_counts_final_IgG_1mpi_PrM, Virologic_Control_Group == 'Controller')
epitope_counts_final_IgG_1mpi_PrM_controller_quantile <- quantile(epitope_counts_final_IgG_1mpi_PrM_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgG_1mpi_PrM_controller_quantile
epitope_counts_final_IgG_1mpi_PrM_non_controller <- filter(epitope_counts_final_IgG_1mpi_PrM, Virologic_Control_Group == 'Non-Controller')
epitope_counts_final_IgG_1mpi_PrM_non_controller_quantile <- quantile(epitope_counts_final_IgG_1mpi_PrM_non_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgG_1mpi_PrM_non_controller_quantile
wilcox.test(Epitope_Count~Virologic_Control_Group, data = epitope_counts_final_IgG_1mpi_PrM, exact = FALSE)
#Membrane (mem)
epitope_counts_final_IgG_1mpi_Mem <- filter(epitope_counts_final_IgG_1mpi, ZIKV_protein == 'Mem')
epitope_counts_final_IgG_1mpi_Mem_controller <- filter(epitope_counts_final_IgG_1mpi_Mem, Virologic_Control_Group == 'Controller')
epitope_counts_final_IgG_1mpi_Mem_controller_quantile <- quantile(epitope_counts_final_IgG_1mpi_Mem_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgG_1mpi_Mem_controller_quantile
epitope_counts_final_IgG_1mpi_Mem_non_controller <- filter(epitope_counts_final_IgG_1mpi_Mem, Virologic_Control_Group == 'Non-Controller')
epitope_counts_final_IgG_1mpi_Mem_non_controller_quantile <- quantile(epitope_counts_final_IgG_1mpi_Mem_non_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgG_1mpi_Mem_non_controller_quantile
wilcox.test(Epitope_Count~Virologic_Control_Group, data = epitope_counts_final_IgG_1mpi_Mem, exact = FALSE)
#Envelope (Env)
epitope_counts_final_IgG_1mpi_Env <- filter(epitope_counts_final_IgG_1mpi, ZIKV_protein == 'Env')
epitope_counts_final_IgG_1mpi_Env_controller <- filter(epitope_counts_final_IgG_1mpi_Env, Virologic_Control_Group == 'Controller')
epitope_counts_final_IgG_1mpi_Env_controller_quantile <- quantile(epitope_counts_final_IgG_1mpi_Env_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgG_1mpi_Env_controller_quantile
epitope_counts_final_IgG_1mpi_Env_non_controller <- filter(epitope_counts_final_IgG_1mpi_Env, Virologic_Control_Group == 'Non-Controller')
epitope_counts_final_IgG_1mpi_Env_non_controller_quantile <- quantile(epitope_counts_final_IgG_1mpi_Env_non_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgG_1mpi_Env_non_controller_quantile
wilcox.test(Epitope_Count~Virologic_Control_Group, data = epitope_counts_final_IgG_1mpi_Env, exact = FALSE)
#NS1 
epitope_counts_final_IgG_1mpi_NS1 <- filter(epitope_counts_final_IgG_1mpi, ZIKV_protein == 'NS1')
epitope_counts_final_IgG_1mpi_NS1_controller <- filter(epitope_counts_final_IgG_1mpi_NS1, Virologic_Control_Group == 'Controller')
epitope_counts_final_IgG_1mpi_NS1_controller_quantile <- quantile(epitope_counts_final_IgG_1mpi_NS1_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgG_1mpi_NS1_controller_quantile
epitope_counts_final_IgG_1mpi_NS1_non_controller <- filter(epitope_counts_final_IgG_1mpi_NS1, Virologic_Control_Group == 'Non-Controller')
epitope_counts_final_IgG_1mpi_NS1_non_controller_quantile <- quantile(epitope_counts_final_IgG_1mpi_NS1_non_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgG_1mpi_NS1_non_controller_quantile
wilcox.test(Epitope_Count~Virologic_Control_Group, data = epitope_counts_final_IgG_1mpi_NS1, exact = FALSE)
#NS2A
epitope_counts_final_IgG_1mpi_NS2A <- filter(epitope_counts_final_IgG_1mpi, ZIKV_protein == 'NS2A')
epitope_counts_final_IgG_1mpi_NS2A_controller <- filter(epitope_counts_final_IgG_1mpi_NS2A, Virologic_Control_Group == 'Controller')
epitope_counts_final_IgG_1mpi_NS2A_controller_quantile <- quantile(epitope_counts_final_IgG_1mpi_NS2A_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgG_1mpi_NS2A_controller_quantile
epitope_counts_final_IgG_1mpi_NS2A_non_controller <- filter(epitope_counts_final_IgG_1mpi_NS2A, Virologic_Control_Group == 'Non-Controller')
epitope_counts_final_IgG_1mpi_NS2A_non_controller_quantile <- quantile(epitope_counts_final_IgG_1mpi_NS2A_non_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgG_1mpi_NS2A_non_controller_quantile
wilcox.test(Epitope_Count~Virologic_Control_Group, data = epitope_counts_final_IgG_1mpi_NS2A, exact = FALSE)
#NS2B
epitope_counts_final_IgG_1mpi_NS2B <- filter(epitope_counts_final_IgG_1mpi, ZIKV_protein == 'NS2B')
epitope_counts_final_IgG_1mpi_NS2B_controller <- filter(epitope_counts_final_IgG_1mpi_NS2B, Virologic_Control_Group == 'Controller')
epitope_counts_final_IgG_1mpi_NS2B_controller_quantile <- quantile(epitope_counts_final_IgG_1mpi_NS2B_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgG_1mpi_NS2B_controller_quantile
epitope_counts_final_IgG_1mpi_NS2B_non_controller <- filter(epitope_counts_final_IgG_1mpi_NS2B, Virologic_Control_Group == 'Non-Controller')
epitope_counts_final_IgG_1mpi_NS2B_non_controller_quantile <- quantile(epitope_counts_final_IgG_1mpi_NS2B_non_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgG_1mpi_NS2B_non_controller_quantile
wilcox.test(Epitope_Count~Virologic_Control_Group, data = epitope_counts_final_IgG_1mpi_NS2B, exact = FALSE)
#NS3
epitope_counts_final_IgG_1mpi_NS3 <- filter(epitope_counts_final_IgG_1mpi, ZIKV_protein == 'NS3')
epitope_counts_final_IgG_1mpi_NS3_controller <- filter(epitope_counts_final_IgG_1mpi_NS3, Virologic_Control_Group == 'Controller')
epitope_counts_final_IgG_1mpi_NS3_controller_quantile <- quantile(epitope_counts_final_IgG_1mpi_NS3_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgG_1mpi_NS3_controller_quantile
epitope_counts_final_IgG_1mpi_NS3_non_controller <- filter(epitope_counts_final_IgG_1mpi_NS3, Virologic_Control_Group == 'Non-Controller')
epitope_counts_final_IgG_1mpi_NS3_non_controller_quantile <- quantile(epitope_counts_final_IgG_1mpi_NS3_non_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgG_1mpi_NS3_non_controller_quantile
wilcox.test(Epitope_Count~Virologic_Control_Group, data = epitope_counts_final_IgG_1mpi_NS3, exact = FALSE)
#NS4A
epitope_counts_final_IgG_1mpi_NS4A <- filter(epitope_counts_final_IgG_1mpi, ZIKV_protein == 'NS4A')
epitope_counts_final_IgG_1mpi_NS4A_controller <- filter(epitope_counts_final_IgG_1mpi_NS4A, Virologic_Control_Group == 'Controller')
epitope_counts_final_IgG_1mpi_NS4A_controller_quantile <- quantile(epitope_counts_final_IgG_1mpi_NS4A_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgG_1mpi_NS4A_controller_quantile
epitope_counts_final_IgG_1mpi_NS4A_non_controller <- filter(epitope_counts_final_IgG_1mpi_NS4A, Virologic_Control_Group == 'Non-Controller')
epitope_counts_final_IgG_1mpi_NS4A_non_controller_quantile <- quantile(epitope_counts_final_IgG_1mpi_NS4A_non_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgG_1mpi_NS4A_non_controller_quantile
wilcox.test(Epitope_Count~Virologic_Control_Group, data = epitope_counts_final_IgG_1mpi_NS4A, exact = FALSE)
#NS4B
epitope_counts_final_IgG_1mpi_NS4B <- filter(epitope_counts_final_IgG_1mpi, ZIKV_protein == 'NS4B')
epitope_counts_final_IgG_1mpi_NS4B_controller <- filter(epitope_counts_final_IgG_1mpi_NS4B, Virologic_Control_Group == 'Controller')
epitope_counts_final_IgG_1mpi_NS4B_controller_quantile <- quantile(epitope_counts_final_IgG_1mpi_NS4B_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgG_1mpi_NS4B_controller_quantile
epitope_counts_final_IgG_1mpi_NS4B_non_controller <- filter(epitope_counts_final_IgG_1mpi_NS4B, Virologic_Control_Group == 'Non-Controller')
epitope_counts_final_IgG_1mpi_NS4B_non_controller_quantile <- quantile(epitope_counts_final_IgG_1mpi_NS4B_non_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgG_1mpi_NS4B_non_controller_quantile
wilcox.test(Epitope_Count~Virologic_Control_Group, data = epitope_counts_final_IgG_1mpi_NS4B, exact = FALSE)
#NS5
epitope_counts_final_IgG_1mpi_NS5 <- filter(epitope_counts_final_IgG_1mpi, ZIKV_protein == 'NS5')
epitope_counts_final_IgG_1mpi_NS5_controller <- filter(epitope_counts_final_IgG_1mpi_NS5, Virologic_Control_Group == 'Controller')
epitope_counts_final_IgG_1mpi_NS5_controller_quantile <- quantile(epitope_counts_final_IgG_1mpi_NS5_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgG_1mpi_NS5_controller_quantile
epitope_counts_final_IgG_1mpi_NS5_non_controller <- filter(epitope_counts_final_IgG_1mpi_NS5, Virologic_Control_Group == 'Non-Controller')
epitope_counts_final_IgG_1mpi_NS5_non_controller_quantile <- quantile(epitope_counts_final_IgG_1mpi_NS5_non_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgG_1mpi_NS5_non_controller_quantile
wilcox.test(Epitope_Count~Virologic_Control_Group, data = epitope_counts_final_IgG_1mpi_NS5, exact = FALSE)
#1 MPI IgG epitope counts (group) figure
epitope_counts_final_IgG_1mpi_group_figure <-
  ggplot() +
  geom_boxplot(data = epitope_counts_final_IgG_1mpi, aes(x= ZIKV_protein, y= Epitope_Count, color = Virologic_Control_Group), fill = 'white', outlier.colour =  'white') +
  geom_point(data = epitope_counts_final_IgG_1mpi, aes(x= ZIKV_protein, y= Epitope_Count, color = Virologic_Control_Group), position = position_jitterdodge(seed = 123, jitter.width = 0.5, jitter.height = 0.5), size= 1, shape= 16) +
  theme_classic() + 
  scale_color_manual(values = c('#000000', '#b5b5b5'), breaks = c('Controller', 'Non-Controller')) +
  guides(color = 'none') + 
  scale_x_discrete(name = 'Region in ZIKV Polyprotein', expand = c(0,0)) + 
  scale_y_continuous(name = 'IgG Epitope Count', expand = c(0,0), limits = c(-2,15), breaks = c(0, 5, 10, 15)) + 
  labs(title= '1 MPI') +
  theme(axis.text.x= element_text(size = 20, angle = 45, hjust = 1, vjust = 1), axis.text.y = element_text(size = 20), axis.title = element_text(size = 20, face = 'bold'), aspect.ratio= 1, plot.title = element_text(size= 24, face = 'bold', hjust = 0.5))
epitope_counts_final_IgG_1mpi_group_figure  
## 1 Month post-infection (MPI)
epitope_counts_final_IgG_3.4mpi<- filter(epitope_counts_final, Timepoint == '3-4',
                                       ZIKV_protein == 'Capsid' |
                                         ZIKV_protein == 'PrM' |
                                         ZIKV_protein == 'Mem' |
                                         ZIKV_protein == 'Env' |
                                         ZIKV_protein == 'NS1' |
                                         ZIKV_protein == 'NS2A' |
                                         ZIKV_protein == 'NS2B' |
                                         ZIKV_protein == 'NS3' |
                                         ZIKV_protein == 'NS4A' |
                                         ZIKV_protein == 'NS4B' |
                                         ZIKV_protein == 'NS5')
# Statistical analyses comparing epitope counts within each region of the ZIKV polyprotein between virologic control groups (Mann-Whitney U-Tests)
#Capsid
epitope_counts_final_IgG_3.4mpi_capsid <- filter(epitope_counts_final_IgG_3.4mpi, ZIKV_protein == 'Capsid')
epitope_counts_final_IgG_3.4mpi_capsid_controller <- filter(epitope_counts_final_IgG_3.4mpi_capsid, Virologic_Control_Group == 'Controller')
epitope_counts_final_IgG_3.4mpi_capsid_controller_quantile <- quantile(epitope_counts_final_IgG_3.4mpi_capsid_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgG_3.4mpi_capsid_controller_quantile
epitope_counts_final_IgG_3.4mpi_capsid_non_controller <- filter(epitope_counts_final_IgG_3.4mpi_capsid, Virologic_Control_Group == 'Non-Controller')
epitope_counts_final_IgG_3.4mpi_capsid_non_controller_quantile <- quantile(epitope_counts_final_IgG_3.4mpi_capsid_non_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgG_3.4mpi_capsid_non_controller_quantile
wilcox.test(Epitope_Count~Virologic_Control_Group, data = epitope_counts_final_IgG_3.4mpi_capsid, exact = FALSE)
#Pre-membrane (PrM)
epitope_counts_final_IgG_3.4mpi_PrM <- filter(epitope_counts_final_IgG_3.4mpi, ZIKV_protein == 'PrM')
epitope_counts_final_IgG_3.4mpi_PrM_controller <- filter(epitope_counts_final_IgG_3.4mpi_PrM, Virologic_Control_Group == 'Controller')
epitope_counts_final_IgG_3.4mpi_PrM_controller_quantile <- quantile(epitope_counts_final_IgG_3.4mpi_PrM_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgG_3.4mpi_PrM_controller_quantile
epitope_counts_final_IgG_3.4mpi_PrM_non_controller <- filter(epitope_counts_final_IgG_3.4mpi_PrM, Virologic_Control_Group == 'Non-Controller')
epitope_counts_final_IgG_3.4mpi_PrM_non_controller_quantile <- quantile(epitope_counts_final_IgG_3.4mpi_PrM_non_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgG_3.4mpi_PrM_non_controller_quantile
wilcox.test(Epitope_Count~Virologic_Control_Group, data = epitope_counts_final_IgG_3.4mpi_PrM, exact = FALSE)
#Membrane (mem)
epitope_counts_final_IgG_3.4mpi_Mem <- filter(epitope_counts_final_IgG_3.4mpi, ZIKV_protein == 'Mem')
epitope_counts_final_IgG_3.4mpi_Mem_controller <- filter(epitope_counts_final_IgG_3.4mpi_Mem, Virologic_Control_Group == 'Controller')
epitope_counts_final_IgG_3.4mpi_Mem_controller_quantile <- quantile(epitope_counts_final_IgG_3.4mpi_Mem_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgG_3.4mpi_Mem_controller_quantile
epitope_counts_final_IgG_3.4mpi_Mem_non_controller <- filter(epitope_counts_final_IgG_3.4mpi_Mem, Virologic_Control_Group == 'Non-Controller')
epitope_counts_final_IgG_3.4mpi_Mem_non_controller_quantile <- quantile(epitope_counts_final_IgG_3.4mpi_Mem_non_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgG_3.4mpi_Mem_non_controller_quantile
wilcox.test(Epitope_Count~Virologic_Control_Group, data = epitope_counts_final_IgG_3.4mpi_Mem, exact = FALSE)
#Envelope (Env)
epitope_counts_final_IgG_3.4mpi_Env <- filter(epitope_counts_final_IgG_3.4mpi, ZIKV_protein == 'Env')
epitope_counts_final_IgG_3.4mpi_Env_controller <- filter(epitope_counts_final_IgG_3.4mpi_Env, Virologic_Control_Group == 'Controller')
epitope_counts_final_IgG_3.4mpi_Env_controller_quantile <- quantile(epitope_counts_final_IgG_3.4mpi_Env_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgG_3.4mpi_Env_controller_quantile
epitope_counts_final_IgG_3.4mpi_Env_non_controller <- filter(epitope_counts_final_IgG_3.4mpi_Env, Virologic_Control_Group == 'Non-Controller')
epitope_counts_final_IgG_3.4mpi_Env_non_controller_quantile <- quantile(epitope_counts_final_IgG_3.4mpi_Env_non_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgG_3.4mpi_Env_non_controller_quantile
wilcox.test(Epitope_Count~Virologic_Control_Group, data = epitope_counts_final_IgG_3.4mpi_Env, exact = FALSE)
#NS1 
epitope_counts_final_IgG_3.4mpi_NS1 <- filter(epitope_counts_final_IgG_3.4mpi, ZIKV_protein == 'NS1')
epitope_counts_final_IgG_3.4mpi_NS1_controller <- filter(epitope_counts_final_IgG_3.4mpi_NS1, Virologic_Control_Group == 'Controller')
epitope_counts_final_IgG_3.4mpi_NS1_controller_quantile <- quantile(epitope_counts_final_IgG_3.4mpi_NS1_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgG_3.4mpi_NS1_controller_quantile
epitope_counts_final_IgG_3.4mpi_NS1_non_controller <- filter(epitope_counts_final_IgG_3.4mpi_NS1, Virologic_Control_Group == 'Non-Controller')
epitope_counts_final_IgG_3.4mpi_NS1_non_controller_quantile <- quantile(epitope_counts_final_IgG_3.4mpi_NS1_non_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgG_3.4mpi_NS1_non_controller_quantile
wilcox.test(Epitope_Count~Virologic_Control_Group, data = epitope_counts_final_IgG_3.4mpi_NS1, exact = FALSE)
#NS2A
epitope_counts_final_IgG_3.4mpi_NS2A <- filter(epitope_counts_final_IgG_3.4mpi, ZIKV_protein == 'NS2A')
epitope_counts_final_IgG_3.4mpi_NS2A_controller <- filter(epitope_counts_final_IgG_3.4mpi_NS2A, Virologic_Control_Group == 'Controller')
epitope_counts_final_IgG_3.4mpi_NS2A_controller_quantile <- quantile(epitope_counts_final_IgG_3.4mpi_NS2A_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgG_3.4mpi_NS2A_controller_quantile
epitope_counts_final_IgG_3.4mpi_NS2A_non_controller <- filter(epitope_counts_final_IgG_3.4mpi_NS2A, Virologic_Control_Group == 'Non-Controller')
epitope_counts_final_IgG_3.4mpi_NS2A_non_controller_quantile <- quantile(epitope_counts_final_IgG_3.4mpi_NS2A_non_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgG_3.4mpi_NS2A_non_controller_quantile
wilcox.test(Epitope_Count~Virologic_Control_Group, data = epitope_counts_final_IgG_3.4mpi_NS2A, exact = FALSE)
#NS2B
epitope_counts_final_IgG_3.4mpi_NS2B <- filter(epitope_counts_final_IgG_3.4mpi, ZIKV_protein == 'NS2B')
epitope_counts_final_IgG_3.4mpi_NS2B_controller <- filter(epitope_counts_final_IgG_3.4mpi_NS2B, Virologic_Control_Group == 'Controller')
epitope_counts_final_IgG_3.4mpi_NS2B_controller_quantile <- quantile(epitope_counts_final_IgG_3.4mpi_NS2B_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgG_3.4mpi_NS2B_controller_quantile
epitope_counts_final_IgG_3.4mpi_NS2B_non_controller <- filter(epitope_counts_final_IgG_3.4mpi_NS2B, Virologic_Control_Group == 'Non-Controller')
epitope_counts_final_IgG_3.4mpi_NS2B_non_controller_quantile <- quantile(epitope_counts_final_IgG_3.4mpi_NS2B_non_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgG_3.4mpi_NS2B_non_controller_quantile
wilcox.test(Epitope_Count~Virologic_Control_Group, data = epitope_counts_final_IgG_3.4mpi_NS2B, exact = FALSE)
#NS3
epitope_counts_final_IgG_3.4mpi_NS3 <- filter(epitope_counts_final_IgG_3.4mpi, ZIKV_protein == 'NS3')
epitope_counts_final_IgG_3.4mpi_NS3_controller <- filter(epitope_counts_final_IgG_3.4mpi_NS3, Virologic_Control_Group == 'Controller')
epitope_counts_final_IgG_3.4mpi_NS3_controller_quantile <- quantile(epitope_counts_final_IgG_3.4mpi_NS3_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgG_3.4mpi_NS3_controller_quantile
epitope_counts_final_IgG_3.4mpi_NS3_non_controller <- filter(epitope_counts_final_IgG_3.4mpi_NS3, Virologic_Control_Group == 'Non-Controller')
epitope_counts_final_IgG_3.4mpi_NS3_non_controller_quantile <- quantile(epitope_counts_final_IgG_3.4mpi_NS3_non_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgG_3.4mpi_NS3_non_controller_quantile
wilcox.test(Epitope_Count~Virologic_Control_Group, data = epitope_counts_final_IgG_3.4mpi_NS3, exact = FALSE)
#NS4A
epitope_counts_final_IgG_3.4mpi_NS4A <- filter(epitope_counts_final_IgG_3.4mpi, ZIKV_protein == 'NS4A')
epitope_counts_final_IgG_3.4mpi_NS4A_controller <- filter(epitope_counts_final_IgG_3.4mpi_NS4A, Virologic_Control_Group == 'Controller')
epitope_counts_final_IgG_3.4mpi_NS4A_controller_quantile <- quantile(epitope_counts_final_IgG_3.4mpi_NS4A_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgG_3.4mpi_NS4A_controller_quantile
epitope_counts_final_IgG_3.4mpi_NS4A_non_controller <- filter(epitope_counts_final_IgG_3.4mpi_NS4A, Virologic_Control_Group == 'Non-Controller')
epitope_counts_final_IgG_3.4mpi_NS4A_non_controller_quantile <- quantile(epitope_counts_final_IgG_3.4mpi_NS4A_non_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgG_3.4mpi_NS4A_non_controller_quantile
wilcox.test(Epitope_Count~Virologic_Control_Group, data = epitope_counts_final_IgG_3.4mpi_NS4A, exact = FALSE)
#NS4B
epitope_counts_final_IgG_3.4mpi_NS4B <- filter(epitope_counts_final_IgG_3.4mpi, ZIKV_protein == 'NS4B')
epitope_counts_final_IgG_3.4mpi_NS4B_controller <- filter(epitope_counts_final_IgG_3.4mpi_NS4B, Virologic_Control_Group == 'Controller')
epitope_counts_final_IgG_3.4mpi_NS4B_controller_quantile <- quantile(epitope_counts_final_IgG_3.4mpi_NS4B_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgG_3.4mpi_NS4B_controller_quantile
epitope_counts_final_IgG_3.4mpi_NS4B_non_controller <- filter(epitope_counts_final_IgG_3.4mpi_NS4B, Virologic_Control_Group == 'Non-Controller')
epitope_counts_final_IgG_3.4mpi_NS4B_non_controller_quantile <- quantile(epitope_counts_final_IgG_3.4mpi_NS4B_non_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgG_3.4mpi_NS4B_non_controller_quantile
wilcox.test(Epitope_Count~Virologic_Control_Group, data = epitope_counts_final_IgG_3.4mpi_NS4B, exact = FALSE)
#NS5
epitope_counts_final_IgG_3.4mpi_NS5 <- filter(epitope_counts_final_IgG_3.4mpi, ZIKV_protein == 'NS5')
epitope_counts_final_IgG_3.4mpi_NS5_controller <- filter(epitope_counts_final_IgG_3.4mpi_NS5, Virologic_Control_Group == 'Controller')
epitope_counts_final_IgG_3.4mpi_NS5_controller_quantile <- quantile(epitope_counts_final_IgG_3.4mpi_NS5_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgG_3.4mpi_NS5_controller_quantile
epitope_counts_final_IgG_3.4mpi_NS5_non_controller <- filter(epitope_counts_final_IgG_3.4mpi_NS5, Virologic_Control_Group == 'Non-Controller')
epitope_counts_final_IgG_3.4mpi_NS5_non_controller_quantile <- quantile(epitope_counts_final_IgG_3.4mpi_NS5_non_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
epitope_counts_final_IgG_3.4mpi_NS5_non_controller_quantile
wilcox.test(Epitope_Count~Virologic_Control_Group, data = epitope_counts_final_IgG_3.4mpi_NS5, exact = FALSE)
#3-4 MPI IgG epitope counts (group) figure
epitope_counts_final_IgG_3.4mpi_group_figure <-
  ggplot() +
  geom_boxplot(data = epitope_counts_final_IgG_3.4mpi, aes(x= ZIKV_protein, y= Epitope_Count, color = Virologic_Control_Group), fill = 'white', outlier.colour =  'white') +
  geom_point(data = epitope_counts_final_IgG_3.4mpi, aes(x= ZIKV_protein, y= Epitope_Count, color = Virologic_Control_Group), position = position_jitterdodge(seed = 123, jitter.width = 0.5, jitter.height = 0.5), size= 1, shape= 16) +
  theme_classic() + 
  scale_color_manual(values = c('#000000', '#b5b5b5'), breaks = c('Controller', 'Non-Controller')) +
  guides(color = 'none') + 
  scale_x_discrete(name = 'Region in ZIKV Polyprotein', expand = c(0,0)) + 
  scale_y_continuous(name = 'IgG Epitope Count', expand = c(0,0), limits = c(-2,15), breaks = c(0, 5, 10, 15)) + 
  labs(title= '3-4 MPI') +
  theme(axis.text.x= element_text(size = 20, angle = 45, hjust = 1, vjust = 1), axis.text.y = element_text(size = 20), axis.title = element_text(size = 20, face = 'bold'), aspect.ratio= 1, plot.title = element_text(size= 24, face = 'bold', hjust = 0.5))
epitope_counts_final_IgG_3.4mpi_group_figure  

###
# Combining epitope counts by virologic control group into a single multipanel figure
###
tiff('Epitope Counts-Group (final).tiff', units= 'in', height= 15, width= 15, res= 300)
ggarrange(epitope_counts_final_IgM_2wpi_group_figure, epitope_counts_final_IgM_3wpi_group_figure, epitope_counts_final_IgG_1mpi_group_figure, epitope_counts_final_IgG_3.4mpi_group_figure, nrow=2, ncol=2, common.legend = TRUE, legend= 'top', align= 'hv', labels = c('A', 'B', 'C', 'D'))
dev.off()



###
#Main manuscript figure showing total IgM and IgG linear epitope counts across the entire ZIKV polyprotein for each virologic control group.
#Also, figures showing total epitope counts in each region of the ZIKV polyprotein combining all animals
###

### IgM total epitope counts
IgM_total_epitope_counts_group_final <- filter(epitope_counts_final, Antibody_Isotype == 'IgM',
                                               ZIKV_protein == 'Total')
## Statistical analyses comparing total linear IgM epitope counts between virologic control groups (Mann-Whitney U-Tests)
# 2 WPI
IgM_total_epitope_counts_group_final_2wpi <- filter(IgM_total_epitope_counts_group_final, Timepoint == '2')
IgM_total_epitope_counts_group_final_2wpi_controller <- filter(IgM_total_epitope_counts_group_final_2wpi, Virologic_Control_Group == 'Controller')
IgM_total_epitope_counts_group_final_2wpi_controller_quantile <- quantile(IgM_total_epitope_counts_group_final_2wpi_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
IgM_total_epitope_counts_group_final_2wpi_controller_quantile
IgM_total_epitope_counts_group_final_2wpi_non_controller <- filter(IgM_total_epitope_counts_group_final, Virologic_Control_Group == 'Non-Controller')
IgM_total_epitope_counts_group_final_2wpi_non_controller_quantile <- quantile(IgM_total_epitope_counts_group_final_2wpi_non_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
IgM_total_epitope_counts_group_final_2wpi_non_controller_quantile
wilcox.test(Epitope_Count~Virologic_Control_Group, data = IgM_total_epitope_counts_group_final_2wpi, exact = FALSE)
# 3 WPI
IgM_total_epitope_counts_group_final_3wpi <- filter(IgM_total_epitope_counts_group_final, Timepoint == '3')
IgM_total_epitope_counts_group_final_3wpi_controller <- filter(IgM_total_epitope_counts_group_final_3wpi, Virologic_Control_Group == 'Controller')
IgM_total_epitope_counts_group_final_3wpi_controller_quantile <- quantile(IgM_total_epitope_counts_group_final_3wpi_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
IgM_total_epitope_counts_group_final_3wpi_controller_quantile
IgM_total_epitope_counts_group_final_3wpi_non_controller <- filter(IgM_total_epitope_counts_group_final, Virologic_Control_Group == 'Non-Controller')
IgM_total_epitope_counts_group_final_3wpi_non_controller_quantile <- quantile(IgM_total_epitope_counts_group_final_3wpi_non_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
IgM_total_epitope_counts_group_final_3wpi_non_controller_quantile
wilcox.test(Epitope_Count~Virologic_Control_Group, data = IgM_total_epitope_counts_group_final_3wpi, exact = FALSE)
# IgM total linear epitope counts figure
IgM_total_epitope_counts_group_final_figure <-
  ggplot() +
  geom_boxplot(data = IgM_total_epitope_counts_group_final, aes(x= Timepoint, y= Epitope_Count, color = Virologic_Control_Group), fill = 'white', outlier.colour =  'white') +
  geom_point(data = IgM_total_epitope_counts_group_final, aes(x= Timepoint, y= Epitope_Count, color = Virologic_Control_Group), position = position_jitterdodge(seed = 123, jitter.width = 0.5, jitter.height = 0.5), size= 2, shape= 16) +
  theme_classic() + 
  scale_color_manual(values = c('#000000', '#b5b5b5'), breaks = c('Controller', 'Non-Controller')) +
  guides(color = guide_legend('Virologic Control Group')) + 
  scale_x_discrete(name = 'Timepoint (WPI)') + 
  scale_y_continuous(name = 'Total IgM Epitope Count', expand = c(0,0), limits = c(-2,100), breaks = c(0, 25, 50, 75, 100)) + 
  theme(axis.text= element_text(size = 20), axis.title = element_text(size = 20, face = 'bold'), legend.text = element_text(size= 12), aspect.ratio= 1, legend.title = element_text(face = 'bold', size = 12), legend.position = 'top')
IgM_total_epitope_counts_group_final_figure

### IgG total epitope counts
IgG_total_epitope_counts_group_final <- filter(epitope_counts_final, Antibody_Isotype == 'IgG',
                                               ZIKV_protein == 'Total')
## Statistical analyses comparing total linear IgG epitope counts between virologic control groups (Mann-Whitney U-Tests)
# 1 MPI
IgG_total_epitope_counts_group_final_1mpi <- filter(IgG_total_epitope_counts_group_final, Timepoint == '1')
IgG_total_epitope_counts_group_final_1mpi_controller <- filter(IgG_total_epitope_counts_group_final_1mpi, Virologic_Control_Group == 'Controller')
IgG_total_epitope_counts_group_final_1mpi_controller_quantile <- quantile(IgG_total_epitope_counts_group_final_1mpi_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
IgG_total_epitope_counts_group_final_1mpi_controller_quantile
IgG_total_epitope_counts_group_final_1mpi_non_controller <- filter(IgG_total_epitope_counts_group_final, Virologic_Control_Group == 'Non-Controller')
IgG_total_epitope_counts_group_final_1mpi_non_controller_quantile <- quantile(IgG_total_epitope_counts_group_final_1mpi_non_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
IgG_total_epitope_counts_group_final_1mpi_non_controller_quantile
wilcox.test(Epitope_Count~Virologic_Control_Group, data = IgG_total_epitope_counts_group_final_1mpi, exact = FALSE)
# 3 WPI
IgG_total_epitope_counts_group_final_3.4mpi <- filter(IgG_total_epitope_counts_group_final, Timepoint == '3-4')
IgG_total_epitope_counts_group_final_3.4mpi_controller <- filter(IgG_total_epitope_counts_group_final_3.4mpi, Virologic_Control_Group == 'Controller')
IgG_total_epitope_counts_group_final_3.4mpi_controller_quantile <- quantile(IgG_total_epitope_counts_group_final_3.4mpi_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
IgG_total_epitope_counts_group_final_3.4mpi_controller_quantile
IgG_total_epitope_counts_group_final_3.4mpi_non_controller <- filter(IgG_total_epitope_counts_group_final, Virologic_Control_Group == 'Non-Controller')
IgG_total_epitope_counts_group_final_3.4mpi_non_controller_quantile <- quantile(IgG_total_epitope_counts_group_final_3.4mpi_non_controller$Epitope_Count, probs= c(0.25, 0.75), na.rm= TRUE)
IgG_total_epitope_counts_group_final_3.4mpi_non_controller_quantile
wilcox.test(Epitope_Count~Virologic_Control_Group, data = IgG_total_epitope_counts_group_final_3.4mpi, exact = FALSE)
# IgG total linear epitope counts figure
IgG_total_epitope_counts_group_final_figure <-
  ggplot() +
  geom_boxplot(data = IgG_total_epitope_counts_group_final, aes(x= Timepoint, y= Epitope_Count, color = Virologic_Control_Group), fill = 'white', outlier.colour =  'white') +
  geom_point(data = IgG_total_epitope_counts_group_final, aes(x= Timepoint, y= Epitope_Count, color = Virologic_Control_Group), position = position_jitterdodge(seed = 123, jitter.width = 0.5, jitter.height = 0.5), size= 2, shape= 16) +
  theme_classic() + 
  scale_color_manual(values = c('#000000', '#b5b5b5'), breaks = c('Controller', 'Non-Controller')) +
  guides(color = guide_legend('Virologic Control Group')) + 
  scale_x_discrete(name = 'Timepoint (MPI)') + 
  scale_y_continuous(name = 'Total IgG Epitope Count', expand = c(0,0), limits = c(-2,50), breaks = c(0, 10, 20, 30, 40, 50)) + 
  theme(axis.text= element_text(size = 20), axis.title = element_text(size = 20, face = 'bold'), aspect.ratio= 1, legend.title = element_text(face = 'bold', size = 12), legend.text = element_text(size= 12), legend.position = 'top')
IgG_total_epitope_counts_group_final_figure


### IgM epitope counts in each region of the ZIKV polyprotein with dams combined into a single group
epitope_counts_final_IgM_combined <- filter(epitope_counts_final, Antibody_Isotype == 'IgM',
                                            ZIKV_protein == 'Capsid' |
                                              ZIKV_protein == 'PrM' |
                                              ZIKV_protein == 'Mem' |
                                              ZIKV_protein == 'Env' |
                                              ZIKV_protein == 'NS1' |
                                              ZIKV_protein == 'NS2A' |
                                              ZIKV_protein == 'NS2B' |
                                              ZIKV_protein == 'NS3' |
                                              ZIKV_protein == 'NS4A' |
                                              ZIKV_protein == 'NS4B' |
                                              ZIKV_protein == 'NS5')
## 2 WPI
epitope_counts_final_IgM_combined_2wpi <- filter(epitope_counts_final_IgM_combined, Timepoint == '2')
# Statistical analysis to compare epitope counts across all regions of the ZIKV polyprotein (Kruskal-Wallis test, followed by a pairwise Mann-Whitney U-test)
kruskal.test(Epitope_Count~ZIKV_protein, data= epitope_counts_final_IgM_combined_2wpi)
pairwise.wilcox.test(epitope_counts_final_IgM_combined_2wpi$Epitope_Count, epitope_counts_final_IgM_combined_2wpi$ZIKV_protein, p.adjust.method = 'BH')
# 2 WPI figure
epitope_counts_final_IgM_combined_2wpi_figure <-
  ggplot() +
  geom_boxplot(data = epitope_counts_final_IgM_combined_2wpi, aes(x= ZIKV_protein, y= Epitope_Count, color = ZIKV_protein), fill = 'white', outlier.colour =  'white') +
  geom_point(data = epitope_counts_final_IgM_combined_2wpi, aes(x= ZIKV_protein, y= Epitope_Count, color = ZIKV_protein), position = position_jitterdodge(seed = 123, jitter.width = 0.5, jitter.height = 0.5), size= 2, shape= 16) +
  theme_classic() + 
  scale_color_manual(values = c('#d81b60', '#ff0000', '#b85151', '#002dff', '#818fea', '#000e64', '#19feff', '#00c128', '#793183', '#640043', '#d3a074'), 
                     breaks = c('Capsid', 'PrM', 'Mem', 'Env', 'NS1', 'NS2A', 'NS2B', 'NS3', 'NS4A', 'NS4B', 'NS5')) +
  guides(color = guide_legend('ZIKV Protein')) + 
  scale_x_discrete(name = 'Region in ZIKV Polyprotein') + 
  scale_y_continuous(name = 'IgM Epitope Count', expand = c(0,0), limits = c(-2,32), breaks = c(0, 5, 10, 15, 20, 25, 30)) + 
  labs(title = '2 WPI') +
  theme(axis.text.x= element_text(size = 20, angle= 45, hjust= 1), axis.text.y= element_text(size= 20), axis.title = element_text(size = 20, face = 'bold'), aspect.ratio= 1, legend.title = element_text(face = 'bold', size = 12), legend.text = element_text(size= 12), legend.position = 'top', plot.title = element_text(size= 24, face= 'bold', hjust= 0.5))
epitope_counts_final_IgM_combined_2wpi_figure

## 3 WPI
epitope_counts_final_IgM_combined_3wpi <- filter(epitope_counts_final_IgM_combined, Timepoint == '3')
# Statistical analysis to compare epitope counts across all regions of the ZIKV polyprotein (Kruskal-Wallis test, followed by a pairwise Mann-Whitney U-test)
kruskal.test(Epitope_Count~ZIKV_protein, data= epitope_counts_final_IgM_combined_3wpi)
pairwise.wilcox.test(epitope_counts_final_IgM_combined_3wpi$Epitope_Count, epitope_counts_final_IgM_combined_3wpi$ZIKV_protein, p.adjust.method = 'BH')
# 3 WPI figure
epitope_counts_final_IgM_combined_3wpi_figure <-
  ggplot() +
  geom_boxplot(data = epitope_counts_final_IgM_combined_3wpi, aes(x= ZIKV_protein, y= Epitope_Count, color = ZIKV_protein), fill = 'white', outlier.colour =  'white') +
  geom_point(data = epitope_counts_final_IgM_combined_3wpi, aes(x= ZIKV_protein, y= Epitope_Count, color = ZIKV_protein), position = position_jitterdodge(seed = 123, jitter.width = 0.5, jitter.height = 0.5), size= 2, shape= 16) +
  theme_classic() + 
  scale_color_manual(values = c('#d81b60', '#ff0000', '#b85151', '#002dff', '#818fea', '#000e64', '#19feff', '#00c128', '#793183', '#640043', '#d3a074'), 
                     breaks = c('Capsid', 'PrM', 'Mem', 'Env', 'NS1', 'NS2A', 'NS2B', 'NS3', 'NS4A', 'NS4B', 'NS5')) +
  guides(color = guide_legend('ZIKV Protein')) + 
  scale_x_discrete(name = 'Region in ZIKV Polyprotein') + 
  scale_y_continuous(name = 'IgM Epitope Count', expand = c(0,0), limits = c(-2,32), breaks = c(0, 5, 10, 15, 20, 25, 30)) + 
  labs(title = '3 WPI') +
  theme(axis.text.x= element_text(size = 20, angle= 45, hjust= 1), axis.text.y= element_text(size= 20), axis.title = element_text(size = 20, face = 'bold'), aspect.ratio= 1, legend.title = element_text(face = 'bold', size = 12), legend.text = element_text(size= 12), legend.position = 'top', plot.title = element_text(size= 24, face= 'bold', hjust= 0.5))
epitope_counts_final_IgM_combined_3wpi_figure

### IgG epitope counts in each region of the ZIKV polyprotein with dams combined into a single group
epitope_counts_final_IgG_combined <- filter(epitope_counts_final, Antibody_Isotype == 'IgG',
                                            ZIKV_protein == 'Capsid' |
                                              ZIKV_protein == 'PrM' |
                                              ZIKV_protein == 'Mem' |
                                              ZIKV_protein == 'Env' |
                                              ZIKV_protein == 'NS1' |
                                              ZIKV_protein == 'NS2A' |
                                              ZIKV_protein == 'NS2B' |
                                              ZIKV_protein == 'NS3' |
                                              ZIKV_protein == 'NS4A' |
                                              ZIKV_protein == 'NS4B' |
                                              ZIKV_protein == 'NS5')
## 1 MPI
epitope_counts_final_IgG_combined_1mpi <- filter(epitope_counts_final_IgG_combined, Timepoint == '1')
# Statistical analysis to compare epitope counts across all regions of the ZIKV polyprotein (Kruskal-Wallis test, followed by a pairwise Mann-Whitney U-test)
kruskal.test(Epitope_Count~ZIKV_protein, data= epitope_counts_final_IgG_combined_1mpi)
pairwise.wilcox.test(epitope_counts_final_IgG_combined_1mpi$Epitope_Count, epitope_counts_final_IgG_combined_1mpi$ZIKV_protein, p.adjust.method = 'BH')
# 1 MPI figure
epitope_counts_final_IgG_combined_1mpi_figure <-
  ggplot() +
  geom_boxplot(data = epitope_counts_final_IgG_combined_1mpi, aes(x= ZIKV_protein, y= Epitope_Count, color = ZIKV_protein), fill = 'white', outlier.colour =  'white') +
  geom_point(data = epitope_counts_final_IgG_combined_1mpi, aes(x= ZIKV_protein, y= Epitope_Count, color = ZIKV_protein), position = position_jitterdodge(seed = 123, jitter.width = 0.5, jitter.height = 0.5), size= 2, shape= 16) +
  theme_classic() + 
  scale_color_manual(values = c('#d81b60', '#ff0000', '#b85151', '#002dff', '#818fea', '#000e64', '#19feff', '#00c128', '#793183', '#640043', '#d3a074'), 
                     breaks = c('Capsid', 'PrM', 'Mem', 'Env', 'NS1', 'NS2A', 'NS2B', 'NS3', 'NS4A', 'NS4B', 'NS5')) +
  guides(color = guide_legend('ZIKV Protein')) + 
  scale_x_discrete(name = 'Region in ZIKV Polyprotein') + 
  scale_y_continuous(name = 'IgG Epitope Count', expand = c(0,0), limits = c(-2,15), breaks = c(0, 5, 10, 15)) + 
  labs(title = '1 MPI') +
  theme(axis.text.x= element_text(size = 20, angle= 45, hjust= 1), axis.text.y= element_text(size= 20), axis.title = element_text(size = 20, face = 'bold'), aspect.ratio= 1, legend.title = element_text(face = 'bold', size = 12), legend.text = element_text(size= 12), legend.position = 'top', plot.title = element_text(size= 24, face= 'bold', hjust= 0.5))
epitope_counts_final_IgG_combined_1mpi_figure

## 3-4 MPI
epitope_counts_final_IgG_combined_3.4mpi <- filter(epitope_counts_final_IgG_combined, Timepoint == '3-4')
# Statistical analysis to compare epitope counts across all regions of the ZIKV polyprotein (Kruskal-Wallis test, followed by a pairwise Mann-Whitney U-test)
kruskal.test(Epitope_Count~ZIKV_protein, data= epitope_counts_final_IgG_combined_3.4mpi)
pairwise.wilcox.test(epitope_counts_final_IgG_combined_3.4mpi$Epitope_Count, epitope_counts_final_IgG_combined_3.4mpi$ZIKV_protein, p.adjust.method = 'BH')
# 3 WPI figure
epitope_counts_final_IgG_combined_3.4mpi_figure <-
  ggplot() +
  geom_boxplot(data = epitope_counts_final_IgG_combined_3.4mpi, aes(x= ZIKV_protein, y= Epitope_Count, color = ZIKV_protein), fill = 'white', outlier.colour =  'white') +
  geom_point(data = epitope_counts_final_IgG_combined_3.4mpi, aes(x= ZIKV_protein, y= Epitope_Count, color = ZIKV_protein), position = position_jitterdodge(seed = 123, jitter.width = 0.5, jitter.height = 0.5), size= 2, shape= 16) +
  theme_classic() + 
  scale_color_manual(values = c('#d81b60', '#ff0000', '#b85151', '#002dff', '#818fea', '#000e64', '#19feff', '#00c128', '#793183', '#640043', '#d3a074'), 
                     breaks = c('Capsid', 'PrM', 'Mem', 'Env', 'NS1', 'NS2A', 'NS2B', 'NS3', 'NS4A', 'NS4B', 'NS5')) +
  guides(color = guide_legend('ZIKV Protein')) + 
  scale_x_discrete(name = 'Region in ZIKV Polyprotein') + 
  scale_y_continuous(name = 'IgG Epitope Count', expand = c(0,0), limits = c(-2,15), breaks = c(0, 5, 10, 15)) + 
  labs(title = '3-4 MPI') +
  theme(axis.text.x= element_text(size = 20, angle= 45, hjust= 1), axis.text.y= element_text(size= 20), axis.title = element_text(size = 20, face = 'bold'), aspect.ratio= 1, legend.title = element_text(face = 'bold', size = 12), legend.text = element_text(size= 12), legend.position = 'top', plot.title = element_text(size= 24, face= 'bold', hjust= 0.5))
epitope_counts_final_IgG_combined_3.4mpi_figure

###
# Combining total epitope counts (by group) and epitope counts in each region of the ZIKV polyprotein (dams combined) into a single multipanel figure
###
tiff('Epitope Counts-Main Figure (Final).tiff', units= 'in', height= 16, width= 18, res= 300)
ggarrange(IgM_total_epitope_counts_group_final_figure, epitope_counts_final_IgM_combined_2wpi_figure, epitope_counts_final_IgM_combined_3wpi_figure, IgG_total_epitope_counts_group_final_figure, epitope_counts_final_IgG_combined_1mpi_figure, epitope_counts_final_IgG_combined_3.4mpi_figure, nrow=2, ncol=3, align= 'hv', labels= c('A', 'B', 'C', 'D', 'E', 'F'))
dev.off()

