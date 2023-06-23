library(readxl)
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(gtable)

###
# Generating epitope maps to determine the percentage of dams in each virologic control group
# that show reactivity to amino acids that are included in linear epitopes.
###

### IgM
## 2 WPI
IgM_epitope_maps_2wpi <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\Peptide Array\\Final Raw Data, figures, and code\\Peptide Array-Positive Positions.xlsx", sheet= 5, col_names= TRUE)
IgM_epitope_maps_2wpi$Peptide_Position = as.numeric(IgM_epitope_maps_2wpi$Peptide_Position)
IgM_epitope_maps_2wpi$Percent_Positive_Animals = as.numeric(IgM_epitope_maps_2wpi$Percent_Positive_Animals)
IgM_epitope_maps_2wpi$Virologic_Control_Group = as.character(IgM_epitope_maps_2wpi$Virologic_Control_Group)
IgM_epitope_maps_2wpi$Virologic_Control_Group <- factor(IgM_epitope_maps_2wpi$Virologic_Control_Group, levels = c('Controller', 'Non-Controller'))
# Capsid (AA position 1-122)
IgM_epitope_maps_2wpi_capsid <- filter(IgM_epitope_maps_2wpi[c(1:122, 3404:3525),])
IgM_epitope_maps_2wpi_capsid_figure <-
  ggplot() +
  geom_bar(data= IgM_epitope_maps_2wpi_capsid, aes(x= Peptide_Position, y= Percent_Positive_Animals, color= Virologic_Control_Group, fill= Virologic_Control_Group), stat= 'identity') +
  theme_classic() +
  scale_x_continuous(name= 'Amino Acid Position', expand= c(0,0), limits= c(0, 125), breaks= c(0, 30, 60, 90, 120)) +
  scale_y_continuous(name= "Positive Dams (%)", expand= c(0,0), limits= c(0,100), breaks= c(0, 25, 50, 75, 100)) +
  labs(title= '2 WPI (IgM)') +
  scale_color_manual(values = c('#000000', '#b5b5b5'), breaks = c('Controller', 'Non-Controller')) +
  scale_fill_manual(values = c('#000000', '#b5b5b5'), breaks = c('Controller', 'Non-Controller')) +
  guides(color = 'none', fill = 'none') +
  theme(axis.text.y = element_text(size = 18), axis.text.x = element_text(angle= 45, hjust= 1, size= 18), axis.title = element_text(size = 20, face = 'bold'), plot.title = element_text(size = 24, face = 'bold', hjust = 0.5), aspect.ratio = 1)
IgM_epitope_maps_2wpi_capsid_figure
IgM_epitope_maps_2wpi_capsid_figure_wrapped <-
  IgM_epitope_maps_2wpi_capsid_figure +
  facet_grid(Virologic_Control_Group ~ .) +
  theme(strip.background = element_blank(), strip.text = element_blank(), panel.spacing = unit(2, 'lines'))
IgM_epitope_maps_2wpi_capsid_figure_wrapped
# Pre-membrane (AA position 123-215)
IgM_epitope_maps_2wpi_PrM <- filter(IgM_epitope_maps_2wpi[c(123:215, 3526:3618),])
IgM_epitope_maps_2wpi_PrM_figure <-
  ggplot() +
  geom_bar(data= IgM_epitope_maps_2wpi_PrM, aes(x= Peptide_Position, y= Percent_Positive_Animals, color= Virologic_Control_Group, fill= Virologic_Control_Group), stat= 'identity') +
  theme_classic() +
  scale_x_continuous(name= 'Amino Acid Position', expand= c(0,0), limits= c(120, 220), breaks= c(130, 150, 170, 190, 210)) +
  scale_y_continuous(name= "Positive Dams (%)", expand= c(0,0), limits= c(0,100), breaks= c(0, 25, 50, 75, 100)) +
  labs(title= '2 WPI (IgM)') +
  scale_color_manual(values = c('#000000', '#b5b5b5'), breaks = c('Controller', 'Non-Controller')) +
  scale_fill_manual(values = c('#000000', '#b5b5b5'), breaks = c('Controller', 'Non-Controller')) +
  guides(color = 'none', fill = 'none') +
  theme(axis.text.y = element_text(size = 18), axis.text.x = element_text(angle= 45, hjust= 1, size= 18), axis.title = element_text(size = 20, face = 'bold'), plot.title = element_text(size = 24, face = 'bold', hjust = 0.5), aspect.ratio = 1)
IgM_epitope_maps_2wpi_PrM_figure
IgM_epitope_maps_2wpi_PrM_figure_wrapped <-
  IgM_epitope_maps_2wpi_PrM_figure +
  facet_grid(Virologic_Control_Group ~ .) +
  theme(strip.background = element_blank(), strip.text = element_blank(), panel.spacing = unit(2, 'lines'))
IgM_epitope_maps_2wpi_PrM_figure_wrapped
# Membrane (AA position 216-290)
IgM_epitope_maps_2wpi_mem <- filter(IgM_epitope_maps_2wpi[c(216:290, 3619:3693),])
IgM_epitope_maps_2wpi_mem_figure <-
  ggplot() +
  geom_bar(data= IgM_epitope_maps_2wpi_mem, aes(x= Peptide_Position, y= Percent_Positive_Animals, color= Virologic_Control_Group, fill= Virologic_Control_Group), stat= 'identity') +
  theme_classic() +
  scale_x_continuous(name= 'Amino Acid Position', expand= c(0,0), limits= c(210, 300), breaks= c(220, 240, 260, 280, 300)) +
  scale_y_continuous(name= "Positive Dams (%)", expand= c(0,0), limits= c(0,100), breaks= c(0, 25, 50, 75, 100)) +
  labs(title= '2 WPI (IgM)') +
  scale_color_manual(values = c('#000000', '#b5b5b5'), breaks = c('Controller', 'Non-Controller')) +
  scale_fill_manual(values = c('#000000', '#b5b5b5'), breaks = c('Controller', 'Non-Controller')) +
  guides(color = 'none', fill = 'none') +
  theme(axis.text.y = element_text(size = 18), axis.text.x = element_text(angle= 45, hjust= 1, size= 18), axis.title = element_text(size = 20, face = 'bold'), plot.title = element_text(size = 24, face = 'bold', hjust = 0.5), aspect.ratio = 1)
IgM_epitope_maps_2wpi_mem_figure
IgM_epitope_maps_2wpi_mem_figure_wrapped <-
  IgM_epitope_maps_2wpi_mem_figure +
  facet_grid(Virologic_Control_Group ~ .) +
  theme(strip.background = element_blank(), strip.text = element_blank(), panel.spacing = unit(2, 'lines'))
IgM_epitope_maps_2wpi_mem_figure_wrapped
# Envelope (AA postion 291:794, EDI: 291-341, 422-482, 570-585, EDII: 342-421, 483-569, EDIII: 586-693, TM: 698-794)
IgM_epitope_maps_2wpi_env <- filter(IgM_epitope_maps_2wpi[c(291:794, 3694:4197),])
IgM_epitope_maps_2wpi_env_figure <-
  ggplot() +
  geom_bar(data= IgM_epitope_maps_2wpi_env, aes(x= Peptide_Position, y= Percent_Positive_Animals, color= Virologic_Control_Group, fill= Virologic_Control_Group), stat= 'identity') +
  theme_classic() +
  scale_x_continuous(name= 'Amino Acid Position', expand= c(0,0), limits= c(280, 805), breaks= c(300, 400, 500, 600, 700, 800)) +
  scale_y_continuous(name= "Positive Dams (%)", expand= c(0,0), limits= c(0,100), breaks= c(0, 25, 50, 75, 100)) +
  labs(title= '2 WPI (IgM)') +
  annotate('rect', xmin= 698, xmax= 794, ymin= -Inf, ymax= Inf, alpha= 0.1, fill = 'red') +
  annotate('rect', xmin= 586, xmax= 693, ymin= -Inf, ymax= Inf, alpha= 0.1, fill = 'yellow') +
  annotate('rect', xmin= 291, xmax= 341, ymin= -Inf, ymax= Inf, alpha= 0.1, fill = 'green') +
  annotate('rect', xmin= 422, xmax= 482, ymin= -Inf, ymax= Inf, alpha= 0.1, fill = 'green') +
  annotate('rect', xmin= 570, xmax= 585, ymin= -Inf, ymax= Inf, alpha= 0.1, fill = 'green') +
  annotate('rect', xmin= 342, xmax= 421, ymin= -Inf, ymax= Inf, alpha= 0.1, fill = 'blue') +
  annotate('rect', xmin= 483, xmax= 569, ymin= -Inf, ymax= Inf, alpha= 0.1, fill = 'blue') +
  scale_color_manual(values = c('#000000', '#b5b5b5'), breaks = c('Controller', 'Non-Controller')) +
  scale_fill_manual(values = c('#000000', '#b5b5b5'), breaks = c('Controller', 'Non-Controller')) +
  guides(color = 'none', fill = 'none') +
  theme(axis.text.y = element_text(size = 18), axis.text.x = element_text(angle= 45, hjust= 1, size= 18), axis.title = element_text(size = 20, face = 'bold'), plot.title = element_text(size = 24, face = 'bold', hjust = 0.5), aspect.ratio = 1)
IgM_epitope_maps_2wpi_env_figure
IgM_epitope_maps_2wpi_env_figure_wrapped <-
  IgM_epitope_maps_2wpi_env_figure +
  facet_grid(Virologic_Control_Group ~ .) +
  theme(strip.background = element_blank(), strip.text = element_blank(), panel.spacing = unit(2, 'lines'))
IgM_epitope_maps_2wpi_env_figure_wrapped
#NS1 (AA position 795:1146, beta-roll: 795-825, wing: 826-976, beta-ladder: 977-1146)
IgM_epitope_maps_2wpi_NS1 <- filter(IgM_epitope_maps_2wpi[c(795:1146, 4198:4549),])
IgM_epitope_maps_2wpi_NS1_figure <-
  ggplot() +
  geom_bar(data= IgM_epitope_maps_2wpi_NS1, aes(x= Peptide_Position, y= Percent_Positive_Animals, color= Virologic_Control_Group, fill= Virologic_Control_Group), stat= 'identity') +
  theme_classic() +
  scale_x_continuous(name= 'Amino Acid Position', expand= c(0,0), limits= c(785, 1165), breaks= c(800, 900, 1000, 1100)) +
  scale_y_continuous(name= "Positive Dams (%)", expand= c(0,0), limits= c(0,100), breaks= c(0, 25, 50, 75, 100)) +
  labs(title= '2 WPI (IgM)') +
  annotate('rect', xmin= 795, xmax= 825, ymin= -Inf, ymax= Inf, alpha= 0.1, fill = 'green') +
  annotate('rect', xmin= 826, xmax= 976, ymin= -Inf, ymax= Inf, alpha= 0.1, fill = 'blue') +
  annotate('rect', xmin= 977, xmax= 1146, ymin= -Inf, ymax= Inf, alpha= 0.1, fill = 'red') +
  scale_color_manual(values = c('#000000', '#b5b5b5'), breaks = c('Controller', 'Non-Controller')) +
  scale_fill_manual(values = c('#000000', '#b5b5b5'), breaks = c('Controller', 'Non-Controller')) +
  guides(color = 'none', fill = 'none') +
  theme(axis.text.y = element_text(size = 18), axis.text.x = element_text(angle= 45, hjust= 1, size= 18), axis.title = element_text(size = 20, face = 'bold'), plot.title = element_text(size = 24, face = 'bold', hjust = 0.5), aspect.ratio = 1)
IgM_epitope_maps_2wpi_NS1_figure
IgM_epitope_maps_2wpi_NS1_figure_wrapped <-
  IgM_epitope_maps_2wpi_NS1_figure +
  facet_grid(Virologic_Control_Group ~ .) +
  theme(strip.background = element_blank(), strip.text = element_blank(), panel.spacing = unit(2, 'lines'))
IgM_epitope_maps_2wpi_NS1_figure_wrapped
# NS2A (AA position: 1147-1372)
IgM_epitope_maps_2wpi_NS2A <- filter(IgM_epitope_maps_2wpi[c(1147:1372, 4550:4775),])
IgM_epitope_maps_2wpi_NS2A_figure <-
  ggplot() +
  geom_bar(data= IgM_epitope_maps_2wpi_NS2A, aes(x= Peptide_Position, y= Percent_Positive_Animals, color= Virologic_Control_Group, fill= Virologic_Control_Group), stat= 'identity') +
  theme_classic() +
  scale_x_continuous(name= 'Amino Acid Position', expand= c(0,0), limits= c(1150, 1385), breaks= c(1200, 1250, 1300, 1350)) +
  scale_y_continuous(name= "Positive Dams (%)", expand= c(0,0), limits= c(0,100), breaks= c(0, 25, 50, 75, 100)) +
  labs(title= '2 WPI (IgM)') +
  scale_color_manual(values = c('#000000', '#b5b5b5'), breaks = c('Controller', 'Non-Controller')) +
  scale_fill_manual(values = c('#000000', '#b5b5b5'), breaks = c('Controller', 'Non-Controller')) +
  guides(color = 'none', fill = 'none') +
  theme(axis.text.y = element_text(size = 18), axis.text.x = element_text(angle= 45, hjust= 1, size= 18), axis.title = element_text(size = 20, face = 'bold'), plot.title = element_text(size = 24, face = 'bold', hjust = 0.5), aspect.ratio = 1)
IgM_epitope_maps_2wpi_NS2A_figure
IgM_epitope_maps_2wpi_NS2A_figure_wrapped <-
  IgM_epitope_maps_2wpi_NS2A_figure +
  facet_grid(Virologic_Control_Group ~ .) +
  theme(strip.background = element_blank(), strip.text = element_blank(), panel.spacing = unit(2, 'lines'))
IgM_epitope_maps_2wpi_NS2A_figure_wrapped
# NS2B (AA position: 1373-1502)
IgM_epitope_maps_2wpi_NS2B <- filter(IgM_epitope_maps_2wpi[c(1373:1502, 4776:4905),])
IgM_epitope_maps_2wpi_NS2B_figure <-
  ggplot() +
  geom_bar(data= IgM_epitope_maps_2wpi_NS2B, aes(x= Peptide_Position, y= Percent_Positive_Animals, color= Virologic_Control_Group, fill= Virologic_Control_Group), stat= 'identity') +
  theme_classic() +
  scale_x_continuous(name= 'Amino Acid Position', expand= c(0,0), limits= c(1370, 1530), breaks= c(1380, 1420, 1460, 1500)) +
  scale_y_continuous(name= "Positive Dams (%)", expand= c(0,0), limits= c(0,100), breaks= c(0, 25, 50, 75, 100)) +
  labs(title= '2 WPI (IgM)') +
  scale_color_manual(values = c('#000000', '#b5b5b5'), breaks = c('Controller', 'Non-Controller')) +
  scale_fill_manual(values = c('#000000', '#b5b5b5'), breaks = c('Controller', 'Non-Controller')) +
  guides(color = 'none', fill = 'none') +
  theme(axis.text.y = element_text(size = 18), axis.text.x = element_text(angle= 45, hjust= 1, size= 18), axis.title = element_text(size = 20, face = 'bold'), plot.title = element_text(size = 24, face = 'bold', hjust = 0.5), aspect.ratio = 1)
IgM_epitope_maps_2wpi_NS2B_figure
IgM_epitope_maps_2wpi_NS2B_figure_wrapped <-
  IgM_epitope_maps_2wpi_NS2B_figure +
  facet_grid(Virologic_Control_Group ~ .) +
  theme(strip.background = element_blank(), strip.text = element_blank(), panel.spacing = unit(2, 'lines'))
IgM_epitope_maps_2wpi_NS2B_figure_wrapped
# NS3 (AA position: 1503-2119)
IgM_epitope_maps_2wpi_NS3 <- filter(IgM_epitope_maps_2wpi[c(1503:2119, 4906:5522),])
IgM_epitope_maps_2wpi_NS3_figure <-
  ggplot() +
  geom_bar(data= IgM_epitope_maps_2wpi_NS3, aes(x= Peptide_Position, y= Percent_Positive_Animals, color= Virologic_Control_Group, fill= Virologic_Control_Group), stat= 'identity') +
  theme_classic() +
  scale_x_continuous(name= 'Amino Acid Position', expand= c(0,0), limits= c(1500, 2150), breaks= c(1600, 1700, 1800, 1900, 2000, 2100)) +
  scale_y_continuous(name= "Positive Dams (%)", expand= c(0,0), limits= c(0,100), breaks= c(0, 25, 50, 75, 100)) +
  labs(title= '2 WPI (IgM)') +
  scale_color_manual(values = c('#000000', '#b5b5b5'), breaks = c('Controller', 'Non-Controller')) +
  scale_fill_manual(values = c('#000000', '#b5b5b5'), breaks = c('Controller', 'Non-Controller')) +
  guides(color = 'none', fill = 'none') +
  theme(axis.text.y = element_text(size = 18), axis.text.x = element_text(angle= 45, hjust= 1, size= 18), axis.title = element_text(size = 20, face = 'bold'), plot.title = element_text(size = 24, face = 'bold', hjust = 0.5), aspect.ratio = 1)
IgM_epitope_maps_2wpi_NS3_figure
IgM_epitope_maps_2wpi_NS3_figure_wrapped <-
  IgM_epitope_maps_2wpi_NS3_figure +
  facet_grid(Virologic_Control_Group ~ .) +
  theme(strip.background = element_blank(), strip.text = element_blank(), panel.spacing = unit(2, 'lines'))
IgM_epitope_maps_2wpi_NS3_figure_wrapped
# NS4A (AA position: 2120-2269)
IgM_epitope_maps_2wpi_NS4A <- filter(IgM_epitope_maps_2wpi[c(2120:2269, 5523:5672),])
IgM_epitope_maps_2wpi_NS4A_figure <-
  ggplot() +
  geom_bar(data= IgM_epitope_maps_2wpi_NS4A, aes(x= Peptide_Position, y= Percent_Positive_Animals, color= Virologic_Control_Group, fill= Virologic_Control_Group), stat= 'identity') +
  theme_classic() +
  scale_x_continuous(name= 'Amino Acid Position', expand= c(0,0), limits= c(2115, 2280), breaks= c(2125, 2175, 2225, 2275)) +
  scale_y_continuous(name= "Positive Dams (%)", expand= c(0,0), limits= c(0,100), breaks= c(0, 25, 50, 75, 100)) +
  labs(title= '2 WPI (IgM)') +
  scale_color_manual(values = c('#000000', '#b5b5b5'), breaks = c('Controller', 'Non-Controller')) +
  scale_fill_manual(values = c('#000000', '#b5b5b5'), breaks = c('Controller', 'Non-Controller')) +
  guides(color = 'none', fill = 'none') +
  theme(axis.text.y = element_text(size = 18), axis.text.x = element_text(angle= 45, hjust= 1, size= 18), axis.title = element_text(size = 20, face = 'bold'), plot.title = element_text(size = 24, face = 'bold', hjust = 0.5), aspect.ratio = 1)
IgM_epitope_maps_2wpi_NS4A_figure
IgM_epitope_maps_2wpi_NS4A_figure_wrapped <-
  IgM_epitope_maps_2wpi_NS4A_figure +
  facet_grid(Virologic_Control_Group ~ .) +
  theme(strip.background = element_blank(), strip.text = element_blank(), panel.spacing = unit(2, 'lines'))
IgM_epitope_maps_2wpi_NS4A_figure_wrapped
# NS4B (AA position: 2270-2520)
IgM_epitope_maps_2wpi_NS4B <- filter(IgM_epitope_maps_2wpi[c(2270:2520, 5673:5923),])
IgM_epitope_maps_2wpi_NS4B_figure <-
  ggplot() +
  geom_bar(data= IgM_epitope_maps_2wpi_NS4B, aes(x= Peptide_Position, y= Percent_Positive_Animals, color= Virologic_Control_Group, fill= Virologic_Control_Group), stat= 'identity') +
  theme_classic() +
  scale_x_continuous(name= 'Amino Acid Position', expand= c(0,0), limits= c(2265, 2550), breaks= c(2300, 2350, 2400, 2450, 2500)) +
  scale_y_continuous(name= "Positive Dams (%)", expand= c(0,0), limits= c(0,100), breaks= c(0, 25, 50, 75, 100)) +
  labs(title= '2 WPI (IgM)') +
  scale_color_manual(values = c('#000000', '#b5b5b5'), breaks = c('Controller', 'Non-Controller')) +
  scale_fill_manual(values = c('#000000', '#b5b5b5'), breaks = c('Controller', 'Non-Controller')) +
  guides(color = 'none', fill = 'none') +
  theme(axis.text.y = element_text(size = 18), axis.text.x = element_text(angle= 45, hjust= 1, size= 18), axis.title = element_text(size = 20, face = 'bold'), plot.title = element_text(size = 24, face = 'bold', hjust = 0.5), aspect.ratio = 1)
IgM_epitope_maps_2wpi_NS4B_figure
IgM_epitope_maps_2wpi_NS4B_figure_wrapped <-
  IgM_epitope_maps_2wpi_NS4B_figure +
  facet_grid(Virologic_Control_Group ~ .) +
  theme(strip.background = element_blank(), strip.text = element_blank(), panel.spacing = unit(2, 'lines'))
IgM_epitope_maps_2wpi_NS4B_figure_wrapped
#NS5 (AA position: 2521-3403)
IgM_epitope_maps_2wpi_NS5 <- filter(IgM_epitope_maps_2wpi[c(2521:3403, 5924:6806),])
IgM_epitope_maps_2wpi_NS5_figure <-
  ggplot() +
  geom_bar(data= IgM_epitope_maps_2wpi_NS5, aes(x= Peptide_Position, y= Percent_Positive_Animals, color= Virologic_Control_Group, fill= Virologic_Control_Group), stat= 'identity') +
  theme_classic() +
  scale_x_continuous(name= 'Amino Acid Position', expand= c(0,0), limits= c(2515, 3415), breaks= c(2520, 2720, 2920, 3120, 3320)) +
  scale_y_continuous(name= "Positive Dams (%)", expand= c(0,0), limits= c(0,100), breaks= c(0, 25, 50, 75, 100)) +
  labs(title= '2 WPI (IgM)') +
  scale_color_manual(values = c('#000000', '#b5b5b5'), breaks = c('Controller', 'Non-Controller')) +
  scale_fill_manual(values = c('#000000', '#b5b5b5'), breaks = c('Controller', 'Non-Controller')) +
  guides(color = 'none', fill = 'none') +
  theme(axis.text.y = element_text(size = 18), axis.text.x = element_text(angle= 45, hjust= 1, size= 18), axis.title = element_text(size = 20, face = 'bold'), plot.title = element_text(size = 24, face = 'bold', hjust = 0.5), aspect.ratio = 1)
IgM_epitope_maps_2wpi_NS5_figure
IgM_epitope_maps_2wpi_NS5_figure_wrapped <-
  IgM_epitope_maps_2wpi_NS5_figure +
  facet_grid(Virologic_Control_Group ~ .) +
  theme(strip.background = element_blank(), strip.text = element_blank(), panel.spacing = unit(2, 'lines'))
IgM_epitope_maps_2wpi_NS5_figure_wrapped
## 3 WPI
IgM_epitope_maps_3wpi <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\Peptide Array\\Final Raw Data, figures, and code\\Peptide Array-Positive Positions.xlsx", sheet= 8, col_names= TRUE)
IgM_epitope_maps_3wpi$Peptide_Position = as.numeric(IgM_epitope_maps_3wpi$Peptide_Position)
IgM_epitope_maps_3wpi$Percent_Positive_Animals = as.numeric(IgM_epitope_maps_3wpi$Percent_Positive_Animals)
IgM_epitope_maps_3wpi$Virologic_Control_Group = as.character(IgM_epitope_maps_3wpi$Virologic_Control_Group)
IgM_epitope_maps_3wpi$Virologic_Control_Group <- factor(IgM_epitope_maps_3wpi$Virologic_Control_Group, levels = c('Controller', 'Non-Controller'))
# Capsid (AA position 1-122)
IgM_epitope_maps_3wpi_capsid <- filter(IgM_epitope_maps_3wpi[c(1:122, 3404:3525),])
IgM_epitope_maps_3wpi_capsid_figure <-
  ggplot() +
  geom_bar(data= IgM_epitope_maps_3wpi_capsid, aes(x= Peptide_Position, y= Percent_Positive_Animals, color= Virologic_Control_Group, fill= Virologic_Control_Group), stat= 'identity') +
  theme_classic() +
  scale_x_continuous(name= 'Amino Acid Position', expand= c(0,0), limits= c(0, 125), breaks= c(0, 30, 60, 90, 120)) +
  scale_y_continuous(name= "Positive Dams (%)", expand= c(0,0), limits= c(0,100), breaks= c(0, 25, 50, 75, 100)) +
  labs(title= '3 WPI (IgM)') +
  scale_color_manual(values = c('#000000', '#b5b5b5'), breaks = c('Controller', 'Non-Controller')) +
  scale_fill_manual(values = c('#000000', '#b5b5b5'), breaks = c('Controller', 'Non-Controller')) +
  guides(color = 'none', fill = 'none') +
  theme(axis.text.y = element_text(size = 18), axis.text.x = element_text(angle= 45, hjust= 1, size= 18), axis.title = element_text(size = 20, face = 'bold'), plot.title = element_text(size = 24, face = 'bold', hjust = 0.5), aspect.ratio = 1)
IgM_epitope_maps_3wpi_capsid_figure
IgM_epitope_maps_3wpi_capsid_figure_wrapped <-
  IgM_epitope_maps_3wpi_capsid_figure +
  facet_grid(Virologic_Control_Group ~ .) +
  theme(strip.background = element_blank(), strip.text = element_blank(), panel.spacing = unit(2, 'lines'))
IgM_epitope_maps_3wpi_capsid_figure_wrapped
# Pre-membrane (AA position 123-215)
IgM_epitope_maps_3wpi_PrM <- filter(IgM_epitope_maps_3wpi[c(123:215, 3526:3618),])
IgM_epitope_maps_3wpi_PrM_figure <-
  ggplot() +
  geom_bar(data= IgM_epitope_maps_3wpi_PrM, aes(x= Peptide_Position, y= Percent_Positive_Animals, color= Virologic_Control_Group, fill= Virologic_Control_Group), stat= 'identity') +
  theme_classic() +
  scale_x_continuous(name= 'Amino Acid Position', expand= c(0,0), limits= c(120, 220), breaks= c(130, 150, 170, 190, 210)) +
  scale_y_continuous(name= "Positive Dams (%)", expand= c(0,0), limits= c(0,100), breaks= c(0, 25, 50, 75, 100)) +
  labs(title= '3 WPI (IgM)') +
  scale_color_manual(values = c('#000000', '#b5b5b5'), breaks = c('Controller', 'Non-Controller')) +
  scale_fill_manual(values = c('#000000', '#b5b5b5'), breaks = c('Controller', 'Non-Controller')) +
  guides(color = 'none', fill = 'none') +
  theme(axis.text.y = element_text(size = 18), axis.text.x = element_text(angle= 45, hjust= 1, size= 18), axis.title = element_text(size = 20, face = 'bold'), plot.title = element_text(size = 24, face = 'bold', hjust = 0.5), aspect.ratio = 1)
IgM_epitope_maps_3wpi_PrM_figure
IgM_epitope_maps_3wpi_PrM_figure_wrapped <-
  IgM_epitope_maps_3wpi_PrM_figure +
  facet_grid(Virologic_Control_Group ~ .) +
  theme(strip.background = element_blank(), strip.text = element_blank(), panel.spacing = unit(2, 'lines'))
IgM_epitope_maps_3wpi_PrM_figure_wrapped
# Membrane (AA position 216-290)
IgM_epitope_maps_3wpi_mem <- filter(IgM_epitope_maps_3wpi[c(216:290, 3619:3693),])
IgM_epitope_maps_3wpi_mem_figure <-
  ggplot() +
  geom_bar(data= IgM_epitope_maps_3wpi_mem, aes(x= Peptide_Position, y= Percent_Positive_Animals, color= Virologic_Control_Group, fill= Virologic_Control_Group), stat= 'identity') +
  theme_classic() +
  scale_x_continuous(name= 'Amino Acid Position', expand= c(0,0), limits= c(210, 300), breaks= c(220, 240, 260, 280, 300)) +
  scale_y_continuous(name= "Positive Dams (%)", expand= c(0,0), limits= c(0,100), breaks= c(0, 25, 50, 75, 100)) +
  labs(title= '3 WPI (IgM)') +
  scale_color_manual(values = c('#000000', '#b5b5b5'), breaks = c('Controller', 'Non-Controller')) +
  scale_fill_manual(values = c('#000000', '#b5b5b5'), breaks = c('Controller', 'Non-Controller')) +
  guides(color = 'none', fill = 'none') +
  theme(axis.text.y = element_text(size = 18), axis.text.x = element_text(angle= 45, hjust= 1, size= 18), axis.title = element_text(size = 20, face = 'bold'), plot.title = element_text(size = 24, face = 'bold', hjust = 0.5), aspect.ratio = 1)
IgM_epitope_maps_3wpi_mem_figure
IgM_epitope_maps_3wpi_mem_figure_wrapped <-
  IgM_epitope_maps_3wpi_mem_figure +
  facet_grid(Virologic_Control_Group ~ .) +
  theme(strip.background = element_blank(), strip.text = element_blank(), panel.spacing = unit(2, 'lines'))
IgM_epitope_maps_3wpi_mem_figure_wrapped
# Envelope (AA postion 291:794, EDI: 291-341, 422-482, 570-585, EDII: 342-421, 483-569, EDIII: 586-693, TM: 698-794)
IgM_epitope_maps_3wpi_env <- filter(IgM_epitope_maps_3wpi[c(291:794, 3694:4197),])
IgM_epitope_maps_3wpi_env_figure <-
  ggplot() +
  geom_bar(data= IgM_epitope_maps_3wpi_env, aes(x= Peptide_Position, y= Percent_Positive_Animals, color= Virologic_Control_Group, fill= Virologic_Control_Group), stat= 'identity') +
  theme_classic() +
  scale_x_continuous(name= 'Amino Acid Position', expand= c(0,0), limits= c(280, 805), breaks= c(300, 400, 500, 600, 700, 800)) +
  scale_y_continuous(name= "Positive Dams (%)", expand= c(0,0), limits= c(0,100), breaks= c(0, 25, 50, 75, 100)) +
  labs(title= '3 WPI (IgM)') +
  annotate('rect', xmin= 698, xmax= 794, ymin= -Inf, ymax= Inf, alpha= 0.1, fill = 'red') +
  annotate('rect', xmin= 586, xmax= 693, ymin= -Inf, ymax= Inf, alpha= 0.1, fill = 'yellow') +
  annotate('rect', xmin= 291, xmax= 341, ymin= -Inf, ymax= Inf, alpha= 0.1, fill = 'green') +
  annotate('rect', xmin= 422, xmax= 482, ymin= -Inf, ymax= Inf, alpha= 0.1, fill = 'green') +
  annotate('rect', xmin= 570, xmax= 585, ymin= -Inf, ymax= Inf, alpha= 0.1, fill = 'green') +
  annotate('rect', xmin= 342, xmax= 421, ymin= -Inf, ymax= Inf, alpha= 0.1, fill = 'blue') +
  annotate('rect', xmin= 483, xmax= 569, ymin= -Inf, ymax= Inf, alpha= 0.1, fill = 'blue') +
  scale_color_manual(values = c('#000000', '#b5b5b5'), breaks = c('Controller', 'Non-Controller')) +
  scale_fill_manual(values = c('#000000', '#b5b5b5'), breaks = c('Controller', 'Non-Controller')) +
  guides(color = 'none', fill = 'none') +
  theme(axis.text.y = element_text(size = 18), axis.text.x = element_text(angle= 45, hjust= 1, size= 18), axis.title = element_text(size = 20, face = 'bold'), plot.title = element_text(size = 24, face = 'bold', hjust = 0.5), aspect.ratio = 1)
IgM_epitope_maps_3wpi_env_figure
IgM_epitope_maps_3wpi_env_figure_wrapped <-
  IgM_epitope_maps_3wpi_env_figure +
  facet_grid(Virologic_Control_Group ~ .) +
  theme(strip.background = element_blank(), strip.text = element_blank(), panel.spacing = unit(2, 'lines'))
IgM_epitope_maps_3wpi_env_figure_wrapped
#NS1 (AA position 795:1146, beta-roll: 795-825, wing: 826-976, beta-ladder: 977-1146)
IgM_epitope_maps_3wpi_NS1 <- filter(IgM_epitope_maps_3wpi[c(795:1146, 4198:4549),])
IgM_epitope_maps_3wpi_NS1_figure <-
  ggplot() +
  geom_bar(data= IgM_epitope_maps_3wpi_NS1, aes(x= Peptide_Position, y= Percent_Positive_Animals, color= Virologic_Control_Group, fill= Virologic_Control_Group), stat= 'identity') +
  theme_classic() +
  scale_x_continuous(name= 'Amino Acid Position', expand= c(0,0), limits= c(785, 1165), breaks= c(800, 900, 1000, 1100)) +
  scale_y_continuous(name= "Positive Dams (%)", expand= c(0,0), limits= c(0,100), breaks= c(0, 25, 50, 75, 100)) +
  labs(title= '3 WPI (IgM)') +
  annotate('rect', xmin= 795, xmax= 825, ymin= -Inf, ymax= Inf, alpha= 0.1, fill = 'green') +
  annotate('rect', xmin= 826, xmax= 976, ymin= -Inf, ymax= Inf, alpha= 0.1, fill = 'blue') +
  annotate('rect', xmin= 977, xmax= 1146, ymin= -Inf, ymax= Inf, alpha= 0.1, fill = 'red') +
  scale_color_manual(values = c('#000000', '#b5b5b5'), breaks = c('Controller', 'Non-Controller')) +
  scale_fill_manual(values = c('#000000', '#b5b5b5'), breaks = c('Controller', 'Non-Controller')) +
  guides(color = 'none', fill = 'none') +
  theme(axis.text.y = element_text(size = 18), axis.text.x = element_text(angle= 45, hjust= 1, size= 18), axis.title = element_text(size = 20, face = 'bold'), plot.title = element_text(size = 24, face = 'bold', hjust = 0.5), aspect.ratio = 1)
IgM_epitope_maps_3wpi_NS1_figure
IgM_epitope_maps_3wpi_NS1_figure_wrapped <-
  IgM_epitope_maps_3wpi_NS1_figure +
  facet_grid(Virologic_Control_Group ~ .) +
  theme(strip.background = element_blank(), strip.text = element_blank(), panel.spacing = unit(2, 'lines'))
IgM_epitope_maps_3wpi_NS1_figure_wrapped
# NS2A (AA position: 1147-1372)
IgM_epitope_maps_3wpi_NS2A <- filter(IgM_epitope_maps_3wpi[c(1147:1372, 4550:4775),])
IgM_epitope_maps_3wpi_NS2A_figure <-
  ggplot() +
  geom_bar(data= IgM_epitope_maps_3wpi_NS2A, aes(x= Peptide_Position, y= Percent_Positive_Animals, color= Virologic_Control_Group, fill= Virologic_Control_Group), stat= 'identity') +
  theme_classic() +
  scale_x_continuous(name= 'Amino Acid Position', expand= c(0,0), limits= c(1150, 1385), breaks= c(1200, 1250, 1300, 1350)) +
  scale_y_continuous(name= "Positive Dams (%)", expand= c(0,0), limits= c(0,100), breaks= c(0, 25, 50, 75, 100)) +
  labs(title= '3 WPI (IgM)') +
  scale_color_manual(values = c('#000000', '#b5b5b5'), breaks = c('Controller', 'Non-Controller')) +
  scale_fill_manual(values = c('#000000', '#b5b5b5'), breaks = c('Controller', 'Non-Controller')) +
  guides(color = 'none', fill = 'none') +
  theme(axis.text.y = element_text(size = 18), axis.text.x = element_text(angle= 45, hjust= 1, size= 18), axis.title = element_text(size = 20, face = 'bold'), plot.title = element_text(size = 24, face = 'bold', hjust = 0.5), aspect.ratio = 1)
IgM_epitope_maps_3wpi_NS2A_figure
IgM_epitope_maps_3wpi_NS2A_figure_wrapped <-
  IgM_epitope_maps_3wpi_NS2A_figure +
  facet_grid(Virologic_Control_Group ~ .) +
  theme(strip.background = element_blank(), strip.text = element_blank(), panel.spacing = unit(2, 'lines'))
IgM_epitope_maps_3wpi_NS2A_figure_wrapped
# NS2B (AA position: 1373-1502)
IgM_epitope_maps_3wpi_NS2B <- filter(IgM_epitope_maps_3wpi[c(1373:1502, 4776:4905),])
IgM_epitope_maps_3wpi_NS2B_figure <-
  ggplot() +
  geom_bar(data= IgM_epitope_maps_3wpi_NS2B, aes(x= Peptide_Position, y= Percent_Positive_Animals, color= Virologic_Control_Group, fill= Virologic_Control_Group), stat= 'identity') +
  theme_classic() +
  scale_x_continuous(name= 'Amino Acid Position', expand= c(0,0), limits= c(1370, 1530), breaks= c(1380, 1420, 1460, 1500)) +
  scale_y_continuous(name= "Positive Dams (%)", expand= c(0,0), limits= c(0,100), breaks= c(0, 25, 50, 75, 100)) +
  labs(title= '3 WPI (IgM)') +
  scale_color_manual(values = c('#000000', '#b5b5b5'), breaks = c('Controller', 'Non-Controller')) +
  scale_fill_manual(values = c('#000000', '#b5b5b5'), breaks = c('Controller', 'Non-Controller')) +
  guides(color = 'none', fill = 'none') +
  theme(axis.text.y = element_text(size = 18), axis.text.x = element_text(angle= 45, hjust= 1, size= 18), axis.title = element_text(size = 20, face = 'bold'), plot.title = element_text(size = 24, face = 'bold', hjust = 0.5), aspect.ratio = 1)
IgM_epitope_maps_3wpi_NS2B_figure
IgM_epitope_maps_3wpi_NS2B_figure_wrapped <-
  IgM_epitope_maps_3wpi_NS2B_figure +
  facet_grid(Virologic_Control_Group ~ .) +
  theme(strip.background = element_blank(), strip.text = element_blank(), panel.spacing = unit(2, 'lines'))
IgM_epitope_maps_3wpi_NS2B_figure_wrapped
# NS3 (AA position: 1503-2119)
IgM_epitope_maps_3wpi_NS3 <- filter(IgM_epitope_maps_3wpi[c(1503:2119, 4906:5522),])
IgM_epitope_maps_3wpi_NS3_figure <-
  ggplot() +
  geom_bar(data= IgM_epitope_maps_3wpi_NS3, aes(x= Peptide_Position, y= Percent_Positive_Animals, color= Virologic_Control_Group, fill= Virologic_Control_Group), stat= 'identity') +
  theme_classic() +
  scale_x_continuous(name= 'Amino Acid Position', expand= c(0,0), limits= c(1500, 2150), breaks= c(1600, 1700, 1800, 1900, 2000, 2100)) +
  scale_y_continuous(name= "Positive Dams (%)", expand= c(0,0), limits= c(0,100), breaks= c(0, 25, 50, 75, 100)) +
  labs(title= '3 WPI (IgM)') +
  scale_color_manual(values = c('#000000', '#b5b5b5'), breaks = c('Controller', 'Non-Controller')) +
  scale_fill_manual(values = c('#000000', '#b5b5b5'), breaks = c('Controller', 'Non-Controller')) +
  guides(color = 'none', fill = 'none') +
  theme(axis.text.y = element_text(size = 18), axis.text.x = element_text(angle= 45, hjust= 1, size= 18), axis.title = element_text(size = 20, face = 'bold'), plot.title = element_text(size = 24, face = 'bold', hjust = 0.5), aspect.ratio = 1)
IgM_epitope_maps_3wpi_NS3_figure
IgM_epitope_maps_3wpi_NS3_figure_wrapped <-
  IgM_epitope_maps_3wpi_NS3_figure +
  facet_grid(Virologic_Control_Group ~ .) +
  theme(strip.background = element_blank(), strip.text = element_blank(), panel.spacing = unit(2, 'lines'))
IgM_epitope_maps_3wpi_NS3_figure_wrapped
# NS4A (AA position: 2120-2269)
IgM_epitope_maps_3wpi_NS4A <- filter(IgM_epitope_maps_3wpi[c(2120:2269, 5523:5672),])
IgM_epitope_maps_3wpi_NS4A_figure <-
  ggplot() +
  geom_bar(data= IgM_epitope_maps_3wpi_NS4A, aes(x= Peptide_Position, y= Percent_Positive_Animals, color= Virologic_Control_Group, fill= Virologic_Control_Group), stat= 'identity') +
  theme_classic() +
  scale_x_continuous(name= 'Amino Acid Position', expand= c(0,0), limits= c(2115, 2280), breaks= c(2125, 2175, 2225, 2275)) +
  scale_y_continuous(name= "Positive Dams (%)", expand= c(0,0), limits= c(0,100), breaks= c(0, 25, 50, 75, 100)) +
  labs(title= '3 WPI (IgM)') +
  scale_color_manual(values = c('#000000', '#b5b5b5'), breaks = c('Controller', 'Non-Controller')) +
  scale_fill_manual(values = c('#000000', '#b5b5b5'), breaks = c('Controller', 'Non-Controller')) +
  guides(color = 'none', fill = 'none') +
  theme(axis.text.y = element_text(size = 18), axis.text.x = element_text(angle= 45, hjust= 1, size= 18), axis.title = element_text(size = 20, face = 'bold'), plot.title = element_text(size = 24, face = 'bold', hjust = 0.5), aspect.ratio = 1)
IgM_epitope_maps_3wpi_NS4A_figure
IgM_epitope_maps_3wpi_NS4A_figure_wrapped <-
  IgM_epitope_maps_3wpi_NS4A_figure +
  facet_grid(Virologic_Control_Group ~ .) +
  theme(strip.background = element_blank(), strip.text = element_blank(), panel.spacing = unit(2, 'lines'))
IgM_epitope_maps_3wpi_NS4A_figure_wrapped
# NS4B (AA position: 2270-2520)
IgM_epitope_maps_3wpi_NS4B <- filter(IgM_epitope_maps_3wpi[c(2270:2520, 5673:5923),])
IgM_epitope_maps_3wpi_NS4B_figure <-
  ggplot() +
  geom_bar(data= IgM_epitope_maps_3wpi_NS4B, aes(x= Peptide_Position, y= Percent_Positive_Animals, color= Virologic_Control_Group, fill= Virologic_Control_Group), stat= 'identity') +
  theme_classic() +
  scale_x_continuous(name= 'Amino Acid Position', expand= c(0,0), limits= c(2265, 2550), breaks= c(2300, 2350, 2400, 2450, 2500)) +
  scale_y_continuous(name= "Positive Dams (%)", expand= c(0,0), limits= c(0,100), breaks= c(0, 25, 50, 75, 100)) +
  labs(title= '3 WPI (IgM)') +
  scale_color_manual(values = c('#000000', '#b5b5b5'), breaks = c('Controller', 'Non-Controller')) +
  scale_fill_manual(values = c('#000000', '#b5b5b5'), breaks = c('Controller', 'Non-Controller')) +
  guides(color = 'none', fill = 'none') +
  theme(axis.text.y = element_text(size = 18), axis.text.x = element_text(angle= 45, hjust= 1, size= 18), axis.title = element_text(size = 20, face = 'bold'), plot.title = element_text(size = 24, face = 'bold', hjust = 0.5), aspect.ratio = 1)
IgM_epitope_maps_3wpi_NS4B_figure
IgM_epitope_maps_3wpi_NS4B_figure_wrapped <-
  IgM_epitope_maps_3wpi_NS4B_figure +
  facet_grid(Virologic_Control_Group ~ .) +
  theme(strip.background = element_blank(), strip.text = element_blank(), panel.spacing = unit(2, 'lines'))
IgM_epitope_maps_3wpi_NS4B_figure_wrapped
#NS5 (AA position: 2521-3403)
IgM_epitope_maps_3wpi_NS5 <- filter(IgM_epitope_maps_3wpi[c(2521:3403, 5924:6806),])
IgM_epitope_maps_3wpi_NS5_figure <-
  ggplot() +
  geom_bar(data= IgM_epitope_maps_3wpi_NS5, aes(x= Peptide_Position, y= Percent_Positive_Animals, color= Virologic_Control_Group, fill= Virologic_Control_Group), stat= 'identity') +
  theme_classic() +
  scale_x_continuous(name= 'Amino Acid Position', expand= c(0,0), limits= c(2515, 3415), breaks= c(2520, 2720, 2920, 3120, 3320)) +
  scale_y_continuous(name= "Positive Dams (%)", expand= c(0,0), limits= c(0,100), breaks= c(0, 25, 50, 75, 100)) +
  labs(title= '3 WPI (IgM)') +
  scale_color_manual(values = c('#000000', '#b5b5b5'), breaks = c('Controller', 'Non-Controller')) +
  scale_fill_manual(values = c('#000000', '#b5b5b5'), breaks = c('Controller', 'Non-Controller')) +
  guides(color = 'none', fill = 'none') +
  theme(axis.text.y = element_text(size = 18), axis.text.x = element_text(angle= 45, hjust= 1, size= 18), axis.title = element_text(size = 20, face = 'bold'), plot.title = element_text(size = 24, face = 'bold', hjust = 0.5), aspect.ratio = 1)
IgM_epitope_maps_3wpi_NS5_figure
IgM_epitope_maps_3wpi_NS5_figure_wrapped <-
  IgM_epitope_maps_3wpi_NS5_figure +
  facet_grid(Virologic_Control_Group ~ .) +
  theme(strip.background = element_blank(), strip.text = element_blank(), panel.spacing = unit(2, 'lines'))
IgM_epitope_maps_3wpi_NS5_figure_wrapped


### IgG
## 1 MPI
IgG_epitope_maps_1mpi <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\Peptide Array\\Final Raw Data, figures, and code\\Peptide Array-Positive Positions.xlsx", sheet= 11, col_names= TRUE)
IgG_epitope_maps_1mpi$Peptide_Position = as.numeric(IgG_epitope_maps_1mpi$Peptide_Position)
IgG_epitope_maps_1mpi$Percent_Positive_Animals = as.numeric(IgG_epitope_maps_1mpi$Percent_Positive_Animals)
IgG_epitope_maps_1mpi$Virologic_Control_Group = as.character(IgG_epitope_maps_1mpi$Virologic_Control_Group)
IgG_epitope_maps_1mpi$Virologic_Control_Group <- factor(IgG_epitope_maps_1mpi$Virologic_Control_Group, levels = c('Controller', 'Non-Controller'))
# Capsid (AA position 1-122)
IgG_epitope_maps_1mpi_capsid <- filter(IgG_epitope_maps_1mpi[c(1:122, 3404:3525),])
IgG_epitope_maps_1mpi_capsid_figure <-
  ggplot() +
  geom_bar(data= IgG_epitope_maps_1mpi_capsid, aes(x= Peptide_Position, y= Percent_Positive_Animals, color= Virologic_Control_Group, fill= Virologic_Control_Group), stat= 'identity') +
  theme_classic() +
  scale_x_continuous(name= 'Amino Acid Position', expand= c(0,0), limits= c(0, 125), breaks= c(0, 30, 60, 90, 120)) +
  scale_y_continuous(name= "Positive Dams (%)", expand= c(0,0), limits= c(0,100), breaks= c(0, 25, 50, 75, 100)) +
  labs(title= '1 MPI (IgG)') +
  scale_color_manual(values = c('#000000', '#b5b5b5'), breaks = c('Controller', 'Non-Controller')) +
  scale_fill_manual(values = c('#000000', '#b5b5b5'), breaks = c('Controller', 'Non-Controller')) +
  guides(color = 'none', fill = 'none') +
  theme(axis.text.y = element_text(size = 18), axis.text.x = element_text(angle= 45, hjust= 1, size= 18), axis.title = element_text(size = 20, face = 'bold'), plot.title = element_text(size = 24, face = 'bold', hjust = 0.5), aspect.ratio = 1)
IgG_epitope_maps_1mpi_capsid_figure
IgG_epitope_maps_1mpi_capsid_figure_wrapped <-
  IgG_epitope_maps_1mpi_capsid_figure +
  facet_grid(Virologic_Control_Group ~ .) +
  theme(strip.background = element_blank(), strip.text = element_blank(), panel.spacing = unit(2, 'lines'))
IgG_epitope_maps_1mpi_capsid_figure_wrapped
# Pre-membrane (AA position 123-215)
IgG_epitope_maps_1mpi_PrM <- filter(IgG_epitope_maps_1mpi[c(123:215, 3526:3618),])
IgG_epitope_maps_1mpi_PrM_figure <-
  ggplot() +
  geom_bar(data= IgG_epitope_maps_1mpi_PrM, aes(x= Peptide_Position, y= Percent_Positive_Animals, color= Virologic_Control_Group, fill= Virologic_Control_Group), stat= 'identity') +
  theme_classic() +
  scale_x_continuous(name= 'Amino Acid Position', expand= c(0,0), limits= c(120, 220), breaks= c(130, 150, 170, 190, 210)) +
  scale_y_continuous(name= "Positive Dams (%)", expand= c(0,0), limits= c(0,100), breaks= c(0, 25, 50, 75, 100)) +
  labs(title= '1 MPI (IgG)') +
  scale_color_manual(values = c('#000000', '#b5b5b5'), breaks = c('Controller', 'Non-Controller')) +
  scale_fill_manual(values = c('#000000', '#b5b5b5'), breaks = c('Controller', 'Non-Controller')) +
  guides(color = 'none', fill = 'none') +
  theme(axis.text.y = element_text(size = 18), axis.text.x = element_text(angle= 45, hjust= 1, size= 18), axis.title = element_text(size = 20, face = 'bold'), plot.title = element_text(size = 24, face = 'bold', hjust = 0.5), aspect.ratio = 1)
IgG_epitope_maps_1mpi_PrM_figure
IgG_epitope_maps_1mpi_PrM_figure_wrapped <-
  IgG_epitope_maps_1mpi_PrM_figure +
  facet_grid(Virologic_Control_Group ~ .) +
  theme(strip.background = element_blank(), strip.text = element_blank(), panel.spacing = unit(2, 'lines'))
IgG_epitope_maps_1mpi_PrM_figure_wrapped
# Membrane (AA position 216-290)
IgG_epitope_maps_1mpi_mem <- filter(IgG_epitope_maps_1mpi[c(216:290, 3619:3693),])
IgG_epitope_maps_1mpi_mem_figure <-
  ggplot() +
  geom_bar(data= IgG_epitope_maps_1mpi_mem, aes(x= Peptide_Position, y= Percent_Positive_Animals, color= Virologic_Control_Group, fill= Virologic_Control_Group), stat= 'identity') +
  theme_classic() +
  scale_x_continuous(name= 'Amino Acid Position', expand= c(0,0), limits= c(210, 300), breaks= c(220, 240, 260, 280, 300)) +
  scale_y_continuous(name= "Positive Dams (%)", expand= c(0,0), limits= c(0,100), breaks= c(0, 25, 50, 75, 100)) +
  labs(title= '1 MPI (IgG)') +
  scale_color_manual(values = c('#000000', '#b5b5b5'), breaks = c('Controller', 'Non-Controller')) +
  scale_fill_manual(values = c('#000000', '#b5b5b5'), breaks = c('Controller', 'Non-Controller')) +
  guides(color = 'none', fill = 'none') +
  theme(axis.text.y = element_text(size = 18), axis.text.x = element_text(angle= 45, hjust= 1, size= 18), axis.title = element_text(size = 20, face = 'bold'), plot.title = element_text(size = 24, face = 'bold', hjust = 0.5), aspect.ratio = 1)
IgG_epitope_maps_1mpi_mem_figure
IgG_epitope_maps_1mpi_mem_figure_wrapped <-
  IgG_epitope_maps_1mpi_mem_figure +
  facet_grid(Virologic_Control_Group ~ .) +
  theme(strip.background = element_blank(), strip.text = element_blank(), panel.spacing = unit(2, 'lines'))
IgG_epitope_maps_1mpi_mem_figure_wrapped
# Envelope (AA postion 291:794, EDI: 291-341, 422-482, 570-585, EDII: 342-421, 483-569, EDIII: 586-693, TM: 698-794)
IgG_epitope_maps_1mpi_env <- filter(IgG_epitope_maps_1mpi[c(291:794, 3694:4197),])
IgG_epitope_maps_1mpi_env_figure <-
  ggplot() +
  geom_bar(data= IgG_epitope_maps_1mpi_env, aes(x= Peptide_Position, y= Percent_Positive_Animals, color= Virologic_Control_Group, fill= Virologic_Control_Group), stat= 'identity') +
  theme_classic() +
  scale_x_continuous(name= 'Amino Acid Position', expand= c(0,0), limits= c(280, 805), breaks= c(300, 400, 500, 600, 700, 800)) +
  scale_y_continuous(name= "Positive Dams (%)", expand= c(0,0), limits= c(0,100), breaks= c(0, 25, 50, 75, 100)) +
  labs(title= '1 MPI (IgG)') +
  annotate('rect', xmin= 698, xmax= 794, ymin= -Inf, ymax= Inf, alpha= 0.1, fill = 'red') +
  annotate('rect', xmin= 586, xmax= 693, ymin= -Inf, ymax= Inf, alpha= 0.1, fill = 'yellow') +
  annotate('rect', xmin= 291, xmax= 341, ymin= -Inf, ymax= Inf, alpha= 0.1, fill = 'green') +
  annotate('rect', xmin= 422, xmax= 482, ymin= -Inf, ymax= Inf, alpha= 0.1, fill = 'green') +
  annotate('rect', xmin= 570, xmax= 585, ymin= -Inf, ymax= Inf, alpha= 0.1, fill = 'green') +
  annotate('rect', xmin= 342, xmax= 421, ymin= -Inf, ymax= Inf, alpha= 0.1, fill = 'blue') +
  annotate('rect', xmin= 483, xmax= 569, ymin= -Inf, ymax= Inf, alpha= 0.1, fill = 'blue') +
  scale_color_manual(values = c('#000000', '#b5b5b5'), breaks = c('Controller', 'Non-Controller')) +
  scale_fill_manual(values = c('#000000', '#b5b5b5'), breaks = c('Controller', 'Non-Controller')) +
  guides(color = 'none', fill = 'none') +
  theme(axis.text.y = element_text(size = 18), axis.text.x = element_text(angle= 45, hjust= 1, size= 18), axis.title = element_text(size = 20, face = 'bold'), plot.title = element_text(size = 24, face = 'bold', hjust = 0.5), aspect.ratio = 1)
IgG_epitope_maps_1mpi_env_figure
IgG_epitope_maps_1mpi_env_figure_wrapped <-
  IgG_epitope_maps_1mpi_env_figure +
  facet_grid(Virologic_Control_Group ~ .) +
  theme(strip.background = element_blank(), strip.text = element_blank(), panel.spacing = unit(2, 'lines'))
IgG_epitope_maps_1mpi_env_figure_wrapped
#NS1 (AA position 795:1146, beta-roll: 795-825, wing: 826-976, beta-ladder: 977-1146)
IgG_epitope_maps_1mpi_NS1 <- filter(IgG_epitope_maps_1mpi[c(795:1146, 4198:4549),])
IgG_epitope_maps_1mpi_NS1_figure <-
  ggplot() +
  geom_bar(data= IgG_epitope_maps_1mpi_NS1, aes(x= Peptide_Position, y= Percent_Positive_Animals, color= Virologic_Control_Group, fill= Virologic_Control_Group), stat= 'identity') +
  theme_classic() +
  scale_x_continuous(name= 'Amino Acid Position', expand= c(0,0), limits= c(785, 1165), breaks= c(800, 900, 1000, 1100)) +
  scale_y_continuous(name= "Positive Dams (%)", expand= c(0,0), limits= c(0,100), breaks= c(0, 25, 50, 75, 100)) +
  labs(title= '1 MPI (IgG)') +
  annotate('rect', xmin= 795, xmax= 825, ymin= -Inf, ymax= Inf, alpha= 0.1, fill = 'green') +
  annotate('rect', xmin= 826, xmax= 976, ymin= -Inf, ymax= Inf, alpha= 0.1, fill = 'blue') +
  annotate('rect', xmin= 977, xmax= 1146, ymin= -Inf, ymax= Inf, alpha= 0.1, fill = 'red') +
  scale_color_manual(values = c('#000000', '#b5b5b5'), breaks = c('Controller', 'Non-Controller')) +
  scale_fill_manual(values = c('#000000', '#b5b5b5'), breaks = c('Controller', 'Non-Controller')) +
  guides(color = 'none', fill = 'none') +
  theme(axis.text.y = element_text(size = 18), axis.text.x = element_text(angle= 45, hjust= 1, size= 18), axis.title = element_text(size = 20, face = 'bold'), plot.title = element_text(size = 24, face = 'bold', hjust = 0.5), aspect.ratio = 1)
IgG_epitope_maps_1mpi_NS1_figure
IgG_epitope_maps_1mpi_NS1_figure_wrapped <-
  IgG_epitope_maps_1mpi_NS1_figure +
  facet_grid(Virologic_Control_Group ~ .) +
  theme(strip.background = element_blank(), strip.text = element_blank(), panel.spacing = unit(2, 'lines'))
IgG_epitope_maps_1mpi_NS1_figure_wrapped
# NS2A (AA position: 1147-1372)
IgG_epitope_maps_1mpi_NS2A <- filter(IgG_epitope_maps_1mpi[c(1147:1372, 4550:4775),])
IgG_epitope_maps_1mpi_NS2A_figure <-
  ggplot() +
  geom_bar(data= IgG_epitope_maps_1mpi_NS2A, aes(x= Peptide_Position, y= Percent_Positive_Animals, color= Virologic_Control_Group, fill= Virologic_Control_Group), stat= 'identity') +
  theme_classic() +
  scale_x_continuous(name= 'Amino Acid Position', expand= c(0,0), limits= c(1150, 1385), breaks= c(1200, 1250, 1300, 1350)) +
  scale_y_continuous(name= "Positive Dams (%)", expand= c(0,0), limits= c(0,100), breaks= c(0, 25, 50, 75, 100)) +
  labs(title= '1 MPI (IgG)') +
  scale_color_manual(values = c('#000000', '#b5b5b5'), breaks = c('Controller', 'Non-Controller')) +
  scale_fill_manual(values = c('#000000', '#b5b5b5'), breaks = c('Controller', 'Non-Controller')) +
  guides(color = 'none', fill = 'none') +
  theme(axis.text.y = element_text(size = 18), axis.text.x = element_text(angle= 45, hjust= 1, size= 18), axis.title = element_text(size = 20, face = 'bold'), plot.title = element_text(size = 24, face = 'bold', hjust = 0.5), aspect.ratio = 1)
IgG_epitope_maps_1mpi_NS2A_figure
IgG_epitope_maps_1mpi_NS2A_figure_wrapped <-
  IgG_epitope_maps_1mpi_NS2A_figure +
  facet_grid(Virologic_Control_Group ~ .) +
  theme(strip.background = element_blank(), strip.text = element_blank(), panel.spacing = unit(2, 'lines'))
IgG_epitope_maps_1mpi_NS2A_figure_wrapped
# NS2B (AA position: 1373-1502)
IgG_epitope_maps_1mpi_NS2B <- filter(IgG_epitope_maps_1mpi[c(1373:1502, 4776:4905),])
IgG_epitope_maps_1mpi_NS2B_figure <-
  ggplot() +
  geom_bar(data= IgG_epitope_maps_1mpi_NS2B, aes(x= Peptide_Position, y= Percent_Positive_Animals, color= Virologic_Control_Group, fill= Virologic_Control_Group), stat= 'identity') +
  theme_classic() +
  scale_x_continuous(name= 'Amino Acid Position', expand= c(0,0), limits= c(1370, 1530), breaks= c(1380, 1420, 1460, 1500)) +
  scale_y_continuous(name= "Positive Dams (%)", expand= c(0,0), limits= c(0,100), breaks= c(0, 25, 50, 75, 100)) +
  labs(title= '1 MPI (IgG)') +
  scale_color_manual(values = c('#000000', '#b5b5b5'), breaks = c('Controller', 'Non-Controller')) +
  scale_fill_manual(values = c('#000000', '#b5b5b5'), breaks = c('Controller', 'Non-Controller')) +
  guides(color = 'none', fill = 'none') +
  theme(axis.text.y = element_text(size = 18), axis.text.x = element_text(angle= 45, hjust= 1, size= 18), axis.title = element_text(size = 20, face = 'bold'), plot.title = element_text(size = 24, face = 'bold', hjust = 0.5), aspect.ratio = 1)
IgG_epitope_maps_1mpi_NS2B_figure
IgG_epitope_maps_1mpi_NS2B_figure_wrapped <-
  IgG_epitope_maps_1mpi_NS2B_figure +
  facet_grid(Virologic_Control_Group ~ .) +
  theme(strip.background = element_blank(), strip.text = element_blank(), panel.spacing = unit(2, 'lines'))
IgG_epitope_maps_1mpi_NS2B_figure_wrapped
# NS3 (AA position: 1503-2119)
IgG_epitope_maps_1mpi_NS3 <- filter(IgG_epitope_maps_1mpi[c(1503:2119, 4906:5522),])
IgG_epitope_maps_1mpi_NS3_figure <-
  ggplot() +
  geom_bar(data= IgG_epitope_maps_1mpi_NS3, aes(x= Peptide_Position, y= Percent_Positive_Animals, color= Virologic_Control_Group, fill= Virologic_Control_Group), stat= 'identity') +
  theme_classic() +
  scale_x_continuous(name= 'Amino Acid Position', expand= c(0,0), limits= c(1500, 2150), breaks= c(1600, 1700, 1800, 1900, 2000, 2100)) +
  scale_y_continuous(name= "Positive Dams (%)", expand= c(0,0), limits= c(0,100), breaks= c(0, 25, 50, 75, 100)) +
  labs(title= '1 MPI (IgG)') +
  scale_color_manual(values = c('#000000', '#b5b5b5'), breaks = c('Controller', 'Non-Controller')) +
  scale_fill_manual(values = c('#000000', '#b5b5b5'), breaks = c('Controller', 'Non-Controller')) +
  guides(color = 'none', fill = 'none') +
  theme(axis.text.y = element_text(size = 18), axis.text.x = element_text(angle= 45, hjust= 1, size= 18), axis.title = element_text(size = 20, face = 'bold'), plot.title = element_text(size = 24, face = 'bold', hjust = 0.5), aspect.ratio = 1)
IgG_epitope_maps_1mpi_NS3_figure
IgG_epitope_maps_1mpi_NS3_figure_wrapped <-
  IgG_epitope_maps_1mpi_NS3_figure +
  facet_grid(Virologic_Control_Group ~ .) +
  theme(strip.background = element_blank(), strip.text = element_blank(), panel.spacing = unit(2, 'lines'))
IgG_epitope_maps_1mpi_NS3_figure_wrapped
# NS4A (AA position: 2120-2269)
IgG_epitope_maps_1mpi_NS4A <- filter(IgG_epitope_maps_1mpi[c(2120:2269, 5523:5672),])
IgG_epitope_maps_1mpi_NS4A_figure <-
  ggplot() +
  geom_bar(data= IgG_epitope_maps_1mpi_NS4A, aes(x= Peptide_Position, y= Percent_Positive_Animals, color= Virologic_Control_Group, fill= Virologic_Control_Group), stat= 'identity') +
  theme_classic() +
  scale_x_continuous(name= 'Amino Acid Position', expand= c(0,0), limits= c(2115, 2280), breaks= c(2125, 2175, 2225, 2275)) +
  scale_y_continuous(name= "Positive Dams (%)", expand= c(0,0), limits= c(0,100), breaks= c(0, 25, 50, 75, 100)) +
  labs(title= '1 MPI (IgG)') +
  scale_color_manual(values = c('#000000', '#b5b5b5'), breaks = c('Controller', 'Non-Controller')) +
  scale_fill_manual(values = c('#000000', '#b5b5b5'), breaks = c('Controller', 'Non-Controller')) +
  guides(color = 'none', fill = 'none') +
  theme(axis.text.y = element_text(size = 18), axis.text.x = element_text(angle= 45, hjust= 1, size= 18), axis.title = element_text(size = 20, face = 'bold'), plot.title = element_text(size = 24, face = 'bold', hjust = 0.5), aspect.ratio = 1)
IgG_epitope_maps_1mpi_NS4A_figure
IgG_epitope_maps_1mpi_NS4A_figure_wrapped <-
  IgG_epitope_maps_1mpi_NS4A_figure +
  facet_grid(Virologic_Control_Group ~ .) +
  theme(strip.background = element_blank(), strip.text = element_blank(), panel.spacing = unit(2, 'lines'))
IgG_epitope_maps_1mpi_NS4A_figure_wrapped
# NS4B (AA position: 2270-2520)
IgG_epitope_maps_1mpi_NS4B <- filter(IgG_epitope_maps_1mpi[c(2270:2520, 5673:5923),])
IgG_epitope_maps_1mpi_NS4B_figure <-
  ggplot() +
  geom_bar(data= IgG_epitope_maps_1mpi_NS4B, aes(x= Peptide_Position, y= Percent_Positive_Animals, color= Virologic_Control_Group, fill= Virologic_Control_Group), stat= 'identity') +
  theme_classic() +
  scale_x_continuous(name= 'Amino Acid Position', expand= c(0,0), limits= c(2265, 2550), breaks= c(2300, 2350, 2400, 2450, 2500)) +
  scale_y_continuous(name= "Positive Dams (%)", expand= c(0,0), limits= c(0,100), breaks= c(0, 25, 50, 75, 100)) +
  labs(title= '1 MPI (IgG)') +
  scale_color_manual(values = c('#000000', '#b5b5b5'), breaks = c('Controller', 'Non-Controller')) +
  scale_fill_manual(values = c('#000000', '#b5b5b5'), breaks = c('Controller', 'Non-Controller')) +
  guides(color = 'none', fill = 'none') +
  theme(axis.text.y = element_text(size = 18), axis.text.x = element_text(angle= 45, hjust= 1, size= 18), axis.title = element_text(size = 20, face = 'bold'), plot.title = element_text(size = 24, face = 'bold', hjust = 0.5), aspect.ratio = 1)
IgG_epitope_maps_1mpi_NS4B_figure
IgG_epitope_maps_1mpi_NS4B_figure_wrapped <-
  IgG_epitope_maps_1mpi_NS4B_figure +
  facet_grid(Virologic_Control_Group ~ .) +
  theme(strip.background = element_blank(), strip.text = element_blank(), panel.spacing = unit(2, 'lines'))
IgG_epitope_maps_1mpi_NS4B_figure_wrapped
#NS5 (AA position: 2521-3403)
IgG_epitope_maps_1mpi_NS5 <- filter(IgG_epitope_maps_1mpi[c(2521:3403, 5924:6806),])
IgG_epitope_maps_1mpi_NS5_figure <-
  ggplot() +
  geom_bar(data= IgG_epitope_maps_1mpi_NS5, aes(x= Peptide_Position, y= Percent_Positive_Animals, color= Virologic_Control_Group, fill= Virologic_Control_Group), stat= 'identity') +
  theme_classic() +
  scale_x_continuous(name= 'Amino Acid Position', expand= c(0,0), limits= c(2515, 3415), breaks= c(2520, 2720, 2920, 3120, 3320)) +
  scale_y_continuous(name= "Positive Dams (%)", expand= c(0,0), limits= c(0,100), breaks= c(0, 25, 50, 75, 100)) +
  labs(title= '1 MPI (IgG)') +
  scale_color_manual(values = c('#000000', '#b5b5b5'), breaks = c('Controller', 'Non-Controller')) +
  scale_fill_manual(values = c('#000000', '#b5b5b5'), breaks = c('Controller', 'Non-Controller')) +
  guides(color = 'none', fill = 'none') +
  theme(axis.text.y = element_text(size = 18), axis.text.x = element_text(angle= 45, hjust= 1, size= 18), axis.title = element_text(size = 20, face = 'bold'), plot.title = element_text(size = 24, face = 'bold', hjust = 0.5), aspect.ratio = 1)
IgG_epitope_maps_1mpi_NS5_figure
IgG_epitope_maps_1mpi_NS5_figure_wrapped <-
  IgG_epitope_maps_1mpi_NS5_figure +
  facet_grid(Virologic_Control_Group ~ .) +
  theme(strip.background = element_blank(), strip.text = element_blank(), panel.spacing = unit(2, 'lines'))
IgG_epitope_maps_1mpi_NS5_figure_wrapped
## 3-4 MPI
IgG_epitope_maps_3.4mpi <- read_excel("R:\\Antibody and Cellular Assays\\Antibody Work\\Correlates of Protection (P1 Animals)\\Peptide Array\\Final Raw Data, figures, and code\\Peptide Array-Positive Positions.xlsx", sheet= 14, col_names= TRUE)
IgG_epitope_maps_3.4mpi$Peptide_Position = as.numeric(IgG_epitope_maps_3.4mpi$Peptide_Position)
IgG_epitope_maps_3.4mpi$Percent_Positive_Animals = as.numeric(IgG_epitope_maps_3.4mpi$Percent_Positive_Animals)
IgG_epitope_maps_3.4mpi$Virologic_Control_Group = as.character(IgG_epitope_maps_3.4mpi$Virologic_Control_Group)
IgG_epitope_maps_3.4mpi$Virologic_Control_Group <- factor(IgG_epitope_maps_3.4mpi$Virologic_Control_Group, levels = c('Controller', 'Non-Controller'))
# Capsid (AA position 1-122)
IgG_epitope_maps_3.4mpi_capsid <- filter(IgG_epitope_maps_3.4mpi[c(1:122, 3404:3525),])
IgG_epitope_maps_3.4mpi_capsid_figure <-
  ggplot() +
  geom_bar(data= IgG_epitope_maps_3.4mpi_capsid, aes(x= Peptide_Position, y= Percent_Positive_Animals, color= Virologic_Control_Group, fill= Virologic_Control_Group), stat= 'identity') +
  theme_classic() +
  scale_x_continuous(name= 'Amino Acid Position', expand= c(0,0), limits= c(0, 125), breaks= c(0, 30, 60, 90, 120)) +
  scale_y_continuous(name= "Positive Dams (%)", expand= c(0,0), limits= c(0,100), breaks= c(0, 25, 50, 75, 100)) +
  labs(title= '3-4 MPI (IgG)') +
  scale_color_manual(values = c('#000000', '#b5b5b5'), breaks = c('Controller', 'Non-Controller')) +
  scale_fill_manual(values = c('#000000', '#b5b5b5'), breaks = c('Controller', 'Non-Controller')) +
  guides(color = 'none', fill = 'none') +
  theme(axis.text.y = element_text(size = 18), axis.text.x = element_text(angle= 45, hjust= 1, size= 18), axis.title = element_text(size = 20, face = 'bold'), plot.title = element_text(size = 24, face = 'bold', hjust = 0.5), aspect.ratio = 1)
IgG_epitope_maps_3.4mpi_capsid_figure
IgG_epitope_maps_3.4mpi_capsid_figure_wrapped <-
  IgG_epitope_maps_3.4mpi_capsid_figure +
  facet_grid(Virologic_Control_Group ~ .) +
  theme(strip.background = element_blank(), strip.text = element_blank(), panel.spacing = unit(2, 'lines'))
IgG_epitope_maps_3.4mpi_capsid_figure_wrapped
# Pre-membrane (AA position 123-215)
IgG_epitope_maps_3.4mpi_PrM <- filter(IgG_epitope_maps_3.4mpi[c(123:215, 3526:3618),])
IgG_epitope_maps_3.4mpi_PrM_figure <-
  ggplot() +
  geom_bar(data= IgG_epitope_maps_3.4mpi_PrM, aes(x= Peptide_Position, y= Percent_Positive_Animals, color= Virologic_Control_Group, fill= Virologic_Control_Group), stat= 'identity') +
  theme_classic() +
  scale_x_continuous(name= 'Amino Acid Position', expand= c(0,0), limits= c(120, 220), breaks= c(130, 150, 170, 190, 210)) +
  scale_y_continuous(name= "Positive Dams (%)", expand= c(0,0), limits= c(0,100), breaks= c(0, 25, 50, 75, 100)) +
  labs(title= '3-4 MPI (IgG)') +
  scale_color_manual(values = c('#000000', '#b5b5b5'), breaks = c('Controller', 'Non-Controller')) +
  scale_fill_manual(values = c('#000000', '#b5b5b5'), breaks = c('Controller', 'Non-Controller')) +
  guides(color = 'none', fill = 'none') +
  theme(axis.text.y = element_text(size = 18), axis.text.x = element_text(angle= 45, hjust= 1, size= 18), axis.title = element_text(size = 20, face = 'bold'), plot.title = element_text(size = 24, face = 'bold', hjust = 0.5), aspect.ratio = 1)
IgG_epitope_maps_3.4mpi_PrM_figure
IgG_epitope_maps_3.4mpi_PrM_figure_wrapped <-
  IgG_epitope_maps_3.4mpi_PrM_figure +
  facet_grid(Virologic_Control_Group ~ .) +
  theme(strip.background = element_blank(), strip.text = element_blank(), panel.spacing = unit(2, 'lines'))
IgG_epitope_maps_3.4mpi_PrM_figure_wrapped
# Membrane (AA position 216-290)
IgG_epitope_maps_3.4mpi_mem <- filter(IgG_epitope_maps_3.4mpi[c(216:290, 3619:3693),])
IgG_epitope_maps_3.4mpi_mem_figure <-
  ggplot() +
  geom_bar(data= IgG_epitope_maps_3.4mpi_mem, aes(x= Peptide_Position, y= Percent_Positive_Animals, color= Virologic_Control_Group, fill= Virologic_Control_Group), stat= 'identity') +
  theme_classic() +
  scale_x_continuous(name= 'Amino Acid Position', expand= c(0,0), limits= c(210, 300), breaks= c(220, 240, 260, 280, 300)) +
  scale_y_continuous(name= "Positive Dams (%)", expand= c(0,0), limits= c(0,100), breaks= c(0, 25, 50, 75, 100)) +
  labs(title= '3-4 MPI (IgG)') +
  scale_color_manual(values = c('#000000', '#b5b5b5'), breaks = c('Controller', 'Non-Controller')) +
  scale_fill_manual(values = c('#000000', '#b5b5b5'), breaks = c('Controller', 'Non-Controller')) +
  guides(color = 'none', fill = 'none') +
  theme(axis.text.y = element_text(size = 18), axis.text.x = element_text(angle= 45, hjust= 1, size= 18), axis.title = element_text(size = 20, face = 'bold'), plot.title = element_text(size = 24, face = 'bold', hjust = 0.5), aspect.ratio = 1)
IgG_epitope_maps_3.4mpi_mem_figure
IgG_epitope_maps_3.4mpi_mem_figure_wrapped <-
  IgG_epitope_maps_3.4mpi_mem_figure +
  facet_grid(Virologic_Control_Group ~ .) +
  theme(strip.background = element_blank(), strip.text = element_blank(), panel.spacing = unit(2, 'lines'))
IgG_epitope_maps_3.4mpi_mem_figure_wrapped
# Envelope (AA postion 291:794, EDI: 291-341, 422-482, 570-585, EDII: 342-421, 483-569, EDIII: 586-693, TM: 698-794)
IgG_epitope_maps_3.4mpi_env <- filter(IgG_epitope_maps_3.4mpi[c(291:794, 3694:4197),])
IgG_epitope_maps_3.4mpi_env_figure <-
  ggplot() +
  geom_bar(data= IgG_epitope_maps_3.4mpi_env, aes(x= Peptide_Position, y= Percent_Positive_Animals, color= Virologic_Control_Group, fill= Virologic_Control_Group), stat= 'identity') +
  theme_classic() +
  scale_x_continuous(name= 'Amino Acid Position', expand= c(0,0), limits= c(280, 805), breaks= c(300, 400, 500, 600, 700, 800)) +
  scale_y_continuous(name= "Positive Dams (%)", expand= c(0,0), limits= c(0,100), breaks= c(0, 25, 50, 75, 100)) +
  labs(title= '3-4 (MPI)') +
  annotate('rect', xmin= 698, xmax= 794, ymin= -Inf, ymax= Inf, alpha= 0.1, fill = 'red') +
  annotate('rect', xmin= 586, xmax= 693, ymin= -Inf, ymax= Inf, alpha= 0.1, fill = 'yellow') +
  annotate('rect', xmin= 291, xmax= 341, ymin= -Inf, ymax= Inf, alpha= 0.1, fill = 'green') +
  annotate('rect', xmin= 422, xmax= 482, ymin= -Inf, ymax= Inf, alpha= 0.1, fill = 'green') +
  annotate('rect', xmin= 570, xmax= 585, ymin= -Inf, ymax= Inf, alpha= 0.1, fill = 'green') +
  annotate('rect', xmin= 342, xmax= 421, ymin= -Inf, ymax= Inf, alpha= 0.1, fill = 'blue') +
  annotate('rect', xmin= 483, xmax= 569, ymin= -Inf, ymax= Inf, alpha= 0.1, fill = 'blue') +
  scale_color_manual(values = c('#000000', '#b5b5b5'), breaks = c('Controller', 'Non-Controller')) +
  scale_fill_manual(values = c('#000000', '#b5b5b5'), breaks = c('Controller', 'Non-Controller')) +
  guides(color = 'none', fill = 'none') +
  theme(axis.text.y = element_text(size = 18), axis.text.x = element_text(angle= 45, hjust= 1, size= 18), axis.title = element_text(size = 20, face = 'bold'), plot.title = element_text(size = 24, face = 'bold', hjust = 0.5), aspect.ratio = 1)
IgG_epitope_maps_3.4mpi_env_figure
IgG_epitope_maps_3.4mpi_env_figure_wrapped <-
  IgG_epitope_maps_3.4mpi_env_figure +
  facet_grid(Virologic_Control_Group ~ .) +
  theme(strip.background = element_blank(), strip.text = element_blank(), panel.spacing = unit(2, 'lines'))
IgG_epitope_maps_3.4mpi_env_figure_wrapped
#NS1 (AA position 795:1146, beta-roll: 795-825, wing: 826-976, beta-ladder: 977-1146)
IgG_epitope_maps_3.4mpi_NS1 <- filter(IgG_epitope_maps_3.4mpi[c(795:1146, 4198:4549),])
IgG_epitope_maps_3.4mpi_NS1_figure <-
  ggplot() +
  geom_bar(data= IgG_epitope_maps_3.4mpi_NS1, aes(x= Peptide_Position, y= Percent_Positive_Animals, color= Virologic_Control_Group, fill= Virologic_Control_Group), stat= 'identity') +
  theme_classic() +
  scale_x_continuous(name= 'Amino Acid Position', expand= c(0,0), limits= c(785, 1165), breaks= c(800, 900, 1000, 1100)) +
  scale_y_continuous(name= "Positive Dams (%)", expand= c(0,0), limits= c(0,100), breaks= c(0, 25, 50, 75, 100)) +
  labs(title= '3-4 MPI (IgG)') +
  annotate('rect', xmin= 795, xmax= 825, ymin= -Inf, ymax= Inf, alpha= 0.1, fill = 'green') +
  annotate('rect', xmin= 826, xmax= 976, ymin= -Inf, ymax= Inf, alpha= 0.1, fill = 'blue') +
  annotate('rect', xmin= 977, xmax= 1146, ymin= -Inf, ymax= Inf, alpha= 0.1, fill = 'red') +
  scale_color_manual(values = c('#000000', '#b5b5b5'), breaks = c('Controller', 'Non-Controller')) +
  scale_fill_manual(values = c('#000000', '#b5b5b5'), breaks = c('Controller', 'Non-Controller')) +
  guides(color = 'none', fill = 'none') +
  theme(axis.text.y = element_text(size = 18), axis.text.x = element_text(angle= 45, hjust= 1, size= 18), axis.title = element_text(size = 20, face = 'bold'), plot.title = element_text(size = 24, face = 'bold', hjust = 0.5), aspect.ratio = 1)
IgG_epitope_maps_3.4mpi_NS1_figure
IgG_epitope_maps_3.4mpi_NS1_figure_wrapped <-
  IgG_epitope_maps_3.4mpi_NS1_figure +
  facet_grid(Virologic_Control_Group ~ .) +
  theme(strip.background = element_blank(), strip.text = element_blank(), panel.spacing = unit(2, 'lines'))
IgG_epitope_maps_3.4mpi_NS1_figure_wrapped
# NS2A (AA position: 1147-1372)
IgG_epitope_maps_3.4mpi_NS2A <- filter(IgG_epitope_maps_3.4mpi[c(1147:1372, 4550:4775),])
IgG_epitope_maps_3.4mpi_NS2A_figure <-
  ggplot() +
  geom_bar(data= IgG_epitope_maps_3.4mpi_NS2A, aes(x= Peptide_Position, y= Percent_Positive_Animals, color= Virologic_Control_Group, fill= Virologic_Control_Group), stat= 'identity') +
  theme_classic() +
  scale_x_continuous(name= 'Amino Acid Position', expand= c(0,0), limits= c(1150, 1385), breaks= c(1200, 1250, 1300, 1350)) +
  scale_y_continuous(name= "Positive Dams (%)", expand= c(0,0), limits= c(0,100), breaks= c(0, 25, 50, 75, 100)) +
  labs(title= '3-4 MPI (IgG)') +
  scale_color_manual(values = c('#000000', '#b5b5b5'), breaks = c('Controller', 'Non-Controller')) +
  scale_fill_manual(values = c('#000000', '#b5b5b5'), breaks = c('Controller', 'Non-Controller')) +
  guides(color = 'none', fill = 'none') +
  theme(axis.text.y = element_text(size = 18), axis.text.x = element_text(angle= 45, hjust= 1, size= 18), axis.title = element_text(size = 20, face = 'bold'), plot.title = element_text(size = 24, face = 'bold', hjust = 0.5), aspect.ratio = 1)
IgG_epitope_maps_3.4mpi_NS2A_figure
IgG_epitope_maps_3.4mpi_NS2A_figure_wrapped <-
  IgG_epitope_maps_3.4mpi_NS2A_figure +
  facet_grid(Virologic_Control_Group ~ .) +
  theme(strip.background = element_blank(), strip.text = element_blank(), panel.spacing = unit(2, 'lines'))
IgG_epitope_maps_3.4mpi_NS2A_figure_wrapped
# NS2B (AA position: 1373-1502)
IgG_epitope_maps_3.4mpi_NS2B <- filter(IgG_epitope_maps_3.4mpi[c(1373:1502, 4776:4905),])
IgG_epitope_maps_3.4mpi_NS2B_figure <-
  ggplot() +
  geom_bar(data= IgG_epitope_maps_3.4mpi_NS2B, aes(x= Peptide_Position, y= Percent_Positive_Animals, color= Virologic_Control_Group, fill= Virologic_Control_Group), stat= 'identity') +
  theme_classic() +
  scale_x_continuous(name= 'Amino Acid Position', expand= c(0,0), limits= c(1370, 1530), breaks= c(1380, 1420, 1460, 1500)) +
  scale_y_continuous(name= "Positive Dams (%)", expand= c(0,0), limits= c(0,100), breaks= c(0, 25, 50, 75, 100)) +
  labs(title= '3-4 MPI (IgG)') +
  scale_color_manual(values = c('#000000', '#b5b5b5'), breaks = c('Controller', 'Non-Controller')) +
  scale_fill_manual(values = c('#000000', '#b5b5b5'), breaks = c('Controller', 'Non-Controller')) +
  guides(color = 'none', fill = 'none') +
  theme(axis.text.y = element_text(size = 18), axis.text.x = element_text(angle= 45, hjust= 1, size= 18), axis.title = element_text(size = 20, face = 'bold'), plot.title = element_text(size = 24, face = 'bold', hjust = 0.5), aspect.ratio = 1)
IgG_epitope_maps_3.4mpi_NS2B_figure
IgG_epitope_maps_3.4mpi_NS2B_figure_wrapped <-
  IgG_epitope_maps_3.4mpi_NS2B_figure +
  facet_grid(Virologic_Control_Group ~ .) +
  theme(strip.background = element_blank(), strip.text = element_blank(), panel.spacing = unit(2, 'lines'))
IgG_epitope_maps_3.4mpi_NS2B_figure_wrapped
# NS3 (AA position: 1503-2119)
IgG_epitope_maps_3.4mpi_NS3 <- filter(IgG_epitope_maps_3.4mpi[c(1503:2119, 4906:5522),])
IgG_epitope_maps_3.4mpi_NS3_figure <-
  ggplot() +
  geom_bar(data= IgG_epitope_maps_3.4mpi_NS3, aes(x= Peptide_Position, y= Percent_Positive_Animals, color= Virologic_Control_Group, fill= Virologic_Control_Group), stat= 'identity') +
  theme_classic() +
  scale_x_continuous(name= 'Amino Acid Position', expand= c(0,0), limits= c(1500, 2150), breaks= c(1600, 1700, 1800, 1900, 2000, 2100)) +
  scale_y_continuous(name= "Positive Dams (%)", expand= c(0,0), limits= c(0,100), breaks= c(0, 25, 50, 75, 100)) +
  labs(title= '3-4 MPI (IgG)') +
  scale_color_manual(values = c('#000000', '#b5b5b5'), breaks = c('Controller', 'Non-Controller')) +
  scale_fill_manual(values = c('#000000', '#b5b5b5'), breaks = c('Controller', 'Non-Controller')) +
  guides(color = 'none', fill = 'none') +
  theme(axis.text.y = element_text(size = 18), axis.text.x = element_text(angle= 45, hjust= 1, size= 18), axis.title = element_text(size = 20, face = 'bold'), plot.title = element_text(size = 24, face = 'bold', hjust = 0.5), aspect.ratio = 1)
IgG_epitope_maps_3.4mpi_NS3_figure
IgG_epitope_maps_3.4mpi_NS3_figure_wrapped <-
  IgG_epitope_maps_3.4mpi_NS3_figure +
  facet_grid(Virologic_Control_Group ~ .) +
  theme(strip.background = element_blank(), strip.text = element_blank(), panel.spacing = unit(2, 'lines'))
IgG_epitope_maps_3.4mpi_NS3_figure_wrapped
# NS4A (AA position: 2120-2269)
IgG_epitope_maps_3.4mpi_NS4A <- filter(IgG_epitope_maps_3.4mpi[c(2120:2269, 5523:5672),])
IgG_epitope_maps_3.4mpi_NS4A_figure <-
  ggplot() +
  geom_bar(data= IgG_epitope_maps_3.4mpi_NS4A, aes(x= Peptide_Position, y= Percent_Positive_Animals, color= Virologic_Control_Group, fill= Virologic_Control_Group), stat= 'identity') +
  theme_classic() +
  scale_x_continuous(name= 'Amino Acid Position', expand= c(0,0), limits= c(2115, 2280), breaks= c(2125, 2175, 2225, 2275)) +
  scale_y_continuous(name= "Positive Dams (%)", expand= c(0,0), limits= c(0,100), breaks= c(0, 25, 50, 75, 100)) +
  labs(title= '3-4 MPI (IgG)') +
  scale_color_manual(values = c('#000000', '#b5b5b5'), breaks = c('Controller', 'Non-Controller')) +
  scale_fill_manual(values = c('#000000', '#b5b5b5'), breaks = c('Controller', 'Non-Controller')) +
  guides(color = 'none', fill = 'none') +
  theme(axis.text.y = element_text(size = 18), axis.text.x = element_text(angle= 45, hjust= 1, size= 18), axis.title = element_text(size = 20, face = 'bold'), plot.title = element_text(size = 24, face = 'bold', hjust = 0.5), aspect.ratio = 1)
IgG_epitope_maps_3.4mpi_NS4A_figure
IgG_epitope_maps_3.4mpi_NS4A_figure_wrapped <-
  IgG_epitope_maps_3.4mpi_NS4A_figure +
  facet_grid(Virologic_Control_Group ~ .) +
  theme(strip.background = element_blank(), strip.text = element_blank(), panel.spacing = unit(2, 'lines'))
IgG_epitope_maps_3.4mpi_NS4A_figure_wrapped
# NS4B (AA position: 2270-2520)
IgG_epitope_maps_3.4mpi_NS4B <- filter(IgG_epitope_maps_3.4mpi[c(2270:2520, 5673:5923),])
IgG_epitope_maps_3.4mpi_NS4B_figure <-
  ggplot() +
  geom_bar(data= IgG_epitope_maps_3.4mpi_NS4B, aes(x= Peptide_Position, y= Percent_Positive_Animals, color= Virologic_Control_Group, fill= Virologic_Control_Group), stat= 'identity') +
  theme_classic() +
  scale_x_continuous(name= 'Amino Acid Position', expand= c(0,0), limits= c(2265, 2550), breaks= c(2300, 2350, 2400, 2450, 2500)) +
  scale_y_continuous(name= "Positive Dams (%)", expand= c(0,0), limits= c(0,100), breaks= c(0, 25, 50, 75, 100)) +
  labs(title= '3-4 MPI (IgG)') +
  scale_color_manual(values = c('#000000', '#b5b5b5'), breaks = c('Controller', 'Non-Controller')) +
  scale_fill_manual(values = c('#000000', '#b5b5b5'), breaks = c('Controller', 'Non-Controller')) +
  guides(color = 'none', fill = 'none') +
  theme(axis.text.y = element_text(size = 18), axis.text.x = element_text(angle= 45, hjust= 1, size= 18), axis.title = element_text(size = 20, face = 'bold'), plot.title = element_text(size = 24, face = 'bold', hjust = 0.5), aspect.ratio = 1)
IgG_epitope_maps_3.4mpi_NS4B_figure
IgG_epitope_maps_3.4mpi_NS4B_figure_wrapped <-
  IgG_epitope_maps_3.4mpi_NS4B_figure +
  facet_grid(Virologic_Control_Group ~ .) +
  theme(strip.background = element_blank(), strip.text = element_blank(), panel.spacing = unit(2, 'lines'))
IgG_epitope_maps_3.4mpi_NS4B_figure_wrapped
#NS5 (AA position: 2521-3403)
IgG_epitope_maps_3.4mpi_NS5 <- filter(IgG_epitope_maps_3.4mpi[c(2521:3403, 5924:6806),])
IgG_epitope_maps_3.4mpi_NS5_figure <-
  ggplot() +
  geom_bar(data= IgG_epitope_maps_3.4mpi_NS5, aes(x= Peptide_Position, y= Percent_Positive_Animals, color= Virologic_Control_Group, fill= Virologic_Control_Group), stat= 'identity') +
  theme_classic() +
  scale_x_continuous(name= 'Amino Acid Position', expand= c(0,0), limits= c(2515, 3415), breaks= c(2520, 2720, 2920, 3120, 3320)) +
  scale_y_continuous(name= "Positive Dams (%)", expand= c(0,0), limits= c(0,100), breaks= c(0, 25, 50, 75, 100)) +
  labs(title= '3-4 MPI (IgG)') +
  scale_color_manual(values = c('#000000', '#b5b5b5'), breaks = c('Controller', 'Non-Controller')) +
  scale_fill_manual(values = c('#000000', '#b5b5b5'), breaks = c('Controller', 'Non-Controller')) +
  guides(color = 'none', fill = 'none') +
  theme(axis.text.y = element_text(size = 18), axis.text.x = element_text(angle= 45, hjust= 1, size= 18), axis.title = element_text(size = 20, face = 'bold'), plot.title = element_text(size = 24, face = 'bold', hjust = 0.5), aspect.ratio = 1)
IgG_epitope_maps_3.4mpi_NS5_figure
IgG_epitope_maps_3.4mpi_NS5_figure_wrapped <-
  IgG_epitope_maps_3.4mpi_NS5_figure +
  facet_grid(Virologic_Control_Group ~ .) +
  theme(strip.background = element_blank(), strip.text = element_blank(), panel.spacing = unit(2, 'lines'))
IgG_epitope_maps_3.4mpi_NS5_figure_wrapped

###
#Combine epitope profiles for each viral protein into a single multipanel figure
###
#Capsid
tiff('Capsid Epitope Map (Final).tiff', units= 'in', width= 20, height= 10, res= 300)
ggarrange(IgM_epitope_maps_2wpi_capsid_figure_wrapped, IgM_epitope_maps_3wpi_capsid_figure_wrapped, IgG_epitope_maps_1mpi_capsid_figure_wrapped, IgG_epitope_maps_3.4mpi_capsid_figure_wrapped, ncol=4, nrow=1, labels= c('A', 'B', 'C', 'D'), align = 'hv')
dev.off()
#PrM
tiff('PrM Epitope Map (Final).tiff', units= 'in', width= 20, height= 10, res= 300)
ggarrange(IgM_epitope_maps_2wpi_PrM_figure_wrapped, IgM_epitope_maps_3wpi_PrM_figure_wrapped, IgG_epitope_maps_1mpi_PrM_figure_wrapped, IgG_epitope_maps_3.4mpi_PrM_figure_wrapped, ncol=4, nrow=1, labels= c('A', 'B', 'C', 'D'), align = 'hv')
dev.off()
#mem
tiff('Membrane Epitope Map (Final).tiff', units= 'in', width= 20, height= 10, res= 300)
ggarrange(IgM_epitope_maps_2wpi_mem_figure_wrapped, IgM_epitope_maps_3wpi_mem_figure_wrapped, IgG_epitope_maps_1mpi_mem_figure_wrapped, IgG_epitope_maps_3.4mpi_mem_figure_wrapped, ncol=4, nrow=1, labels= c('A', 'B', 'C', 'D'), align = 'hv')
dev.off()
#envelope
tiff('Envelope Epitope Map (Final).tiff', units= 'in', width= 20, height= 10, res= 300)
ggarrange(IgM_epitope_maps_2wpi_env_figure_wrapped, IgM_epitope_maps_3wpi_env_figure_wrapped, IgG_epitope_maps_1mpi_env_figure_wrapped, IgG_epitope_maps_3.4mpi_env_figure_wrapped, ncol=4, nrow=1, labels= c('A', 'B', 'C', 'D'), align = 'hv')
dev.off()
#NS1
tiff('NS1 Epitope Map (Final).tiff', units= 'in', width= 20, height= 10, res= 300)
ggarrange(IgM_epitope_maps_2wpi_NS1_figure_wrapped, IgM_epitope_maps_3wpi_NS1_figure_wrapped, IgG_epitope_maps_1mpi_NS1_figure_wrapped, IgG_epitope_maps_3.4mpi_NS1_figure_wrapped, ncol=4, nrow=1, labels= c('A', 'B', 'C', 'D'), align = 'hv')
dev.off()
#NS2A
tiff('NS2A Epitope Map (Final).tiff', units= 'in', width= 20, height= 10, res= 300)
ggarrange(IgM_epitope_maps_2wpi_NS2A_figure_wrapped, IgM_epitope_maps_3wpi_NS2A_figure_wrapped, IgG_epitope_maps_1mpi_NS2A_figure_wrapped, IgG_epitope_maps_3.4mpi_NS2A_figure_wrapped, ncol=4, nrow=1, labels= c('A', 'B', 'C', 'D'), align = 'hv')
dev.off()
#NS2B
tiff('NS2B Epitope Map (Final).tiff', units= 'in', width= 20, height= 10, res= 300)
ggarrange(IgM_epitope_maps_2wpi_NS2B_figure_wrapped, IgM_epitope_maps_3wpi_NS2B_figure_wrapped, IgG_epitope_maps_1mpi_NS2B_figure_wrapped, IgG_epitope_maps_3.4mpi_NS2B_figure_wrapped, ncol=4, nrow=1, labels= c('A', 'B', 'C', 'D'), align = 'hv')
dev.off()
#NS3
tiff('NS3 Epitope Map (Final).tiff', units= 'in', width= 20, height= 10, res= 300)
ggarrange(IgM_epitope_maps_2wpi_NS3_figure_wrapped, IgM_epitope_maps_3wpi_NS3_figure_wrapped, IgG_epitope_maps_1mpi_NS3_figure_wrapped, IgG_epitope_maps_3.4mpi_NS3_figure_wrapped, ncol=4, nrow=1, labels= c('A', 'B', 'C', 'D'), align = 'hv')
dev.off()
#NS4A
tiff('NS4A Epitope Map (Final).tiff', units= 'in', width= 20, height= 10, res= 300)
ggarrange(IgM_epitope_maps_2wpi_NS4A_figure_wrapped, IgM_epitope_maps_3wpi_NS4A_figure_wrapped, IgG_epitope_maps_1mpi_NS4A_figure_wrapped, IgG_epitope_maps_3.4mpi_NS4A_figure_wrapped, ncol=4, nrow=1, labels= c('A', 'B', 'C', 'D'), align = 'hv')
dev.off()
#NS4B
tiff('NS4B Epitope Map (Final).tiff', units= 'in', width= 20, height= 10, res= 300)
ggarrange(IgM_epitope_maps_2wpi_NS4B_figure_wrapped, IgM_epitope_maps_3wpi_NS4B_figure_wrapped, IgG_epitope_maps_1mpi_NS4B_figure_wrapped, IgG_epitope_maps_3.4mpi_NS4B_figure_wrapped, ncol=4, nrow=1, labels= c('A', 'B', 'C', 'D'), align = 'hv')
dev.off()
#NS5
tiff('NS5 Epitope Map (Final).tiff', units= 'in', width= 20, height= 10, res= 300)
ggarrange(IgM_epitope_maps_2wpi_NS5_figure_wrapped, IgM_epitope_maps_3wpi_NS5_figure_wrapped, IgG_epitope_maps_1mpi_NS5_figure_wrapped, IgG_epitope_maps_3.4mpi_NS5_figure_wrapped, ncol=4, nrow=1, labels= c('A', 'B', 'C', 'D'), align = 'hv')
dev.off()


