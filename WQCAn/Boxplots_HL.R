# exploring a combined WQ data for high low flow comparison, first load the required packages
library(readr) 
library(ggplot2)
library(tidyverse)
library(DataExplorer)
library(psych)
library(DescTools)
library(pastecs)
library(gridExtra)

#set working directory to subfolder WQCAn
setwd("D:/R/R.code/WQCAn")

# first load ICP data
# import data a bit differently
WQ_samples_2019_HL <- read_csv("WQ_samples__2019_HL.csv", 
                               col_types = cols(Country = col_factor(levels = c("TJ",
                                "UZ", "KG", "KZ")), month = col_factor()))

# first try the simple boxplots for high-low flow and arragne them on one page

a1<-WQ_samples_2019_HL %>%
  ggplot() +
  aes(x = Period, y =`T [C]` ) +
  geom_boxplot(fill = "#ffffff") +
  geom_point(aes(y=`T [C]`), alpha=0.7, position=position_jitter(0.1))+
  labs(title = "T [C]") +
  ggthemes::theme_base()+  
  theme(plot.title = element_text(color="blue", size=13, face="plain", vjust = -10, hjust = 0.5))+
  theme(axis.title = element_text(size=11))

a2<-WQ_samples_2019_HL %>%
  ggplot() +
  aes(x = Period, y =pH ) +
  geom_boxplot(fill = "#ffffff") +
  geom_point(aes(y=pH), alpha=0.7, position=position_jitter(0.1))+
  labs(title = "pH") +
  ggthemes::theme_base()+  
  theme(plot.title = element_text(color="blue", size=13, face="plain", vjust = -10, hjust = 0.5))+
  theme(axis.title = element_text(size=11))

a3<-WQ_samples_2019_HL %>%
  ggplot() +
  aes(x = Period, y =`EC [microS/cm]` ) +
  geom_boxplot(fill = "#ffffff") +
  geom_point(aes(y=`EC [microS/cm]`), alpha=0.7, position=position_jitter(0.1))+
  labs(title = "EC [microS/cm]") +
  ggthemes::theme_base()+  
  theme(plot.title = element_text(color="blue", size=13, face="plain", vjust = -10, hjust = 0.5))+
  theme(axis.title = element_text(size=11))

a4<-WQ_samples_2019_HL %>%
  ggplot() +
  aes(x = Period, y =`TDS [mg/L]` ) +
  geom_boxplot(fill = "#ffffff") +
  geom_point(aes(y=`TDS [mg/L]`), alpha=0.7, position=position_jitter(0.1))+
  labs(title = "TDS [mg/L]") +
  ggthemes::theme_base()+  
  theme(plot.title = element_text(color="blue", size=13, face="plain", vjust = -10, hjust = 0.5))+
  theme(axis.title = element_text(size=11))

a5<-WQ_samples_2019_HL %>%
  ggplot() +
  aes(x = Period, y =`PO3-4-P [mg/L]` ) +
  geom_boxplot(fill = "#ffffff") +
  geom_point(aes(y=`PO3-4-P [mg/L]`), alpha=0.7, position=position_jitter(0.1))+
  labs(title = "PO3-4-P [mg/L]") +
  ggthemes::theme_base()+  
  theme(plot.title = element_text(color="blue", size=13, face="plain", vjust = -10, hjust = 0.5))+
  theme(axis.title = element_text(size=11))

a6<-WQ_samples_2019_HL %>%
  ggplot() +
  aes(x = Period, y =`NO+3-N [mg/L]` ) +
  geom_boxplot(fill = "#ffffff") +
  geom_point(aes(y=`NO+3-N [mg/L]`), alpha=0.7, position=position_jitter(0.1))+
  labs(title = "NO+3-N [mg/L]") +
  ggthemes::theme_base()+  
  theme(plot.title = element_text(color="blue", size=13, face="plain", vjust = -10, hjust = 0.5))+
  theme(axis.title = element_text(size=11))


grid.arrange(a1, a2, a3, a4, a5, a6, nrow = 2, ncol = 3)

# or save as file
boxHL1<-grid.arrange(a1, a2, a3, a4, a5, a6, nrow = 2, ncol = 3)
ggsave(boxHL1, filename = "boxHL1.pdf", dpi = 70)
dev.off()

a7<-WQ_samples_2019_HL %>%
  ggplot() +
  aes(x = Period, y =`Fe-Dis [mg/L]` ) +
  geom_boxplot(fill = "#ffffff") +
  geom_point(aes(y=`Fe-Dis [mg/L]`), alpha=0.7, position=position_jitter(0.1))+
  labs(title = "Fe-Dis [mg/L]") +
  ggthemes::theme_base()+  
  theme(plot.title = element_text(color="blue", size=13, face="plain", vjust = -10, hjust = 0.5))+
  theme(axis.title = element_text(size=11))

a8<-WQ_samples_2019_HL %>%
  ggplot() +
  aes(x = Period, y =`Ca [mg/l]` ) +
  geom_boxplot(fill = "#ffffff") +
  geom_point(aes(y=`Ca [mg/l]`), alpha=0.7, position=position_jitter(0.1))+
  labs(title = "Ca [mg/l]") +
  ggthemes::theme_base()+  
  theme(plot.title = element_text(color="blue", size=13, face="plain", vjust = -10, hjust = 0.5))+
  theme(axis.title = element_text(size=11))

a9<-WQ_samples_2019_HL %>%
  ggplot() +
  aes(x = Period, y =`Mg [mg/l]` ) +
  geom_boxplot(fill = "#ffffff") +
  geom_point(aes(y=`Mg [mg/l]`), alpha=0.7, position=position_jitter(0.1))+
  labs(title = "Mg [mg/l]") +
  ggthemes::theme_base()+  
  theme(plot.title = element_text(color="blue", size=13, face="plain", vjust = -10, hjust = 0.5))+
  theme(axis.title = element_text(size=11))


a10<-WQ_samples_2019_HL %>%
  ggplot() +
  aes(x = Period, y =`Na [mg/l]` ) +
  geom_boxplot(fill = "#ffffff") +
  geom_point(aes(y=`Na [mg/l]`), alpha=0.7, position=position_jitter(0.1))+
  labs(title = "Na [mg/l]") +
  ggthemes::theme_base()+  
  theme(plot.title = element_text(color="blue", size=13, face="plain", vjust = -10, hjust = 0.5))+
  theme(axis.title = element_text(size=11))

a11<-WQ_samples_2019_HL %>%
  ggplot() +
  aes(x = Period, y =`K [mg/L]` ) +
  geom_boxplot(fill = "#ffffff") +
  geom_point(aes(y=`K [mg/L]`), alpha=0.7, position=position_jitter(0.1))+
  labs(title = "K [mg/L]") +
  ggthemes::theme_base()+  
  theme(plot.title = element_text(color="blue", size=13, face="plain", vjust = -10, hjust = 0.5))+
  theme(axis.title = element_text(size=11))

a12<-WQ_samples_2019_HL %>%
  ggplot() +
  aes(x = Period, y =`Fe-Tot [mg/L]` ) +
  geom_boxplot(fill = "#ffffff") +
  geom_point(aes(y=`Fe-Tot [mg/L]`), alpha=0.7, position=position_jitter(0.1))+
  labs(title = "Fe-Tot [mg/L]") +
  ggthemes::theme_base()+  
  theme(plot.title = element_text(color="blue", size=13, face="plain", vjust = -10, hjust = 0.5))+
  theme(axis.title = element_text(size=11))

a13<-WQ_samples_2019_HL %>%
  ggplot() +
  aes(x = Period, y =`Al [ug/L]` ) +
  geom_boxplot(fill = "#ffffff") +
  geom_point(aes(y=`Al [ug/L]`), alpha=0.7, position=position_jitter(0.1))+
  labs(title = "Al [ug/L]") +
  ggthemes::theme_base()+  
  theme(plot.title = element_text(color="blue", size=13, face="plain", vjust = -10, hjust = 0.5))+
  theme(axis.title = element_text(size=11))

a14<-WQ_samples_2019_HL %>%
  ggplot() +
  aes(x = Period, y =`As [ug/L]` ) +
  geom_boxplot(fill = "#ffffff") +
  geom_point(aes(y=`As [ug/L]`), alpha=0.7, position=position_jitter(0.1))+
  labs(title = "As [ug/L]") +
  ggthemes::theme_base()+  
  theme(plot.title = element_text(color="blue", size=13, face="plain", vjust = -10, hjust = 0.5))+
  theme(axis.title = element_text(size=11))

a15<-WQ_samples_2019_HL %>%
  ggplot() +
  aes(x = Period, y =`Ba [ug/L]` ) +
  geom_boxplot(fill = "#ffffff") +
  geom_point(aes(y=`Ba [ug/L]`), alpha=0.7, position=position_jitter(0.1))+
  labs(title = "Ba [ug/L]]") +
  ggthemes::theme_base()+  
  theme(plot.title = element_text(color="blue", size=13, face="plain", vjust = -10, hjust = 0.5))+
  theme(axis.title = element_text(size=11))

grid.arrange(a7, a8, a9, a10, a11, a12, a13, a14, a15, nrow = 3, ncol = 3)

a16<-WQ_samples_2019_HL %>%
  ggplot() +
  aes(x = Period, y =`Cu [ug/L]` ) +
  geom_boxplot(fill = "#ffffff") +
  geom_point(aes(y=`Cu [ug/L]`), alpha=0.7, position=position_jitter(0.1))+
  labs(title = "Cu [ug/L]") +
  ggthemes::theme_base()+  
  theme(plot.title = element_text(color="blue", size=13, face="plain", vjust = -10, hjust = 0.5))+
  theme(axis.title = element_text(size=11))

a17<-WQ_samples_2019_HL %>%
  ggplot() +
  aes(x = Period, y =`Li [ug/L]` ) +
  geom_boxplot(fill = "#ffffff") +
  geom_point(aes(y=`Li [ug/L]`), alpha=0.7, position=position_jitter(0.1))+
  labs(title = "Li [ug/L]") +
  ggthemes::theme_base()+  
  theme(plot.title = element_text(color="blue", size=13, face="plain", vjust = -10, hjust = 0.5))+
  theme(axis.title = element_text(size=11))

a18<-WQ_samples_2019_HL %>%
  ggplot() +
  aes(x = Period, y =`Mn [ug/L]` ) +
  geom_boxplot(fill = "#ffffff") +
  geom_point(aes(y=`Mn [ug/L]`), alpha=0.7, position=position_jitter(0.1))+
  labs(title = "Mn [ug/L]") +
  ggthemes::theme_base()+  
  theme(plot.title = element_text(color="blue", size=13, face="plain", vjust = -10, hjust = 0.5))+
  theme(axis.title = element_text(size=11))

a19<-WQ_samples_2019_HL %>%
  ggplot() +
  aes(x = Period, y =`Sr [ug/L]` ) +
  geom_boxplot(fill = "#ffffff") +
  geom_point(aes(y=`Sr [ug/L]`), alpha=0.7, position=position_jitter(0.1))+
  labs(title = "Sr [ug/L]") +
  ggthemes::theme_base()+  
  theme(plot.title = element_text(color="blue", size=13, face="plain", vjust = -10, hjust = 0.5))+
  theme(axis.title = element_text(size=11))

a20<-WQ_samples_2019_HL %>%
  ggplot() +
  aes(x = Period, y =`Zn [ug/L]` ) +
  geom_boxplot(fill = "#ffffff") +
  geom_point(aes(y=`Zn [ug/L]`), alpha=0.7, position=position_jitter(0.1))+
  labs(title = "Zn [ug/L]") +
  ggthemes::theme_base()+  
  theme(plot.title = element_text(color="blue", size=13, face="plain", vjust = -10, hjust = 0.5))+
  theme(axis.title = element_text(size=11))

grid.arrange(a16, a17,a18, a19, a20, nrow = 2, ncol = 3)

a21<-WQ_samples_2019_HL %>%
  ggplot() +
  aes(x = Period, y =`Cd [ug/L]` ) +
  geom_boxplot(fill = "#ffffff") +
  geom_point(aes(y=`Cd [ug/L]`), alpha=0.7, position=position_jitter(0.1))+
  labs(title = "Cd [ug/L]") +
  ggthemes::theme_base()+  
  theme(plot.title = element_text(color="blue", size=13, face="plain", vjust = -10, hjust = 0.5))+
  theme(axis.title = element_text(size=11))

a22<-WQ_samples_2019_HL %>%
  ggplot() +
  aes(x = Period, y =`Co [ug/L]` ) +
  geom_boxplot(fill = "#ffffff") +
  geom_point(aes(y=`Co [ug/L]`), alpha=0.7, position=position_jitter(0.1))+
  labs(title = "Co [ug/L]") +
  ggthemes::theme_base()+  
  theme(plot.title = element_text(color="blue", size=13, face="plain", vjust = -10, hjust = 0.5))+
  theme(axis.title = element_text(size=11))

a23<-WQ_samples_2019_HL %>%
  ggplot() +
  aes(x = Period, y =`Cr [ug/L]` ) +
  geom_boxplot(fill = "#ffffff") +
  geom_point(aes(y=`Cr [ug/L]`), alpha=0.7, position=position_jitter(0.1))+
  labs(title = "Cr [ug/L]") +
  ggthemes::theme_base()+  
  theme(plot.title = element_text(color="blue", size=13, face="plain", vjust = -10, hjust = 0.5))+
  theme(axis.title = element_text(size=11))

a24<-WQ_samples_2019_HL %>%
  ggplot() +
  aes(x = Period, y =`Ni [ug/L]` ) +
  geom_boxplot(fill = "#ffffff") +
  geom_point(aes(y=`Ni [ug/L]`), alpha=0.7, position=position_jitter(0.1))+
  labs(title = "Ni [ug/L]") +
  ggthemes::theme_base()+  
  theme(plot.title = element_text(color="blue", size=13, face="plain", vjust = -10, hjust = 0.5))+
  theme(axis.title = element_text(size=11))

a25<-WQ_samples_2019_HL %>%
  ggplot() +
  aes(x = Period, y =`Pb [ug/L]` ) +
  geom_boxplot(fill = "#ffffff") +
  geom_point(aes(y=`Pb [ug/L]`), alpha=0.7, position=position_jitter(0.1))+
  labs(title = "Pb [ug/L]") +
  ggthemes::theme_base()+  
  theme(plot.title = element_text(color="blue", size=13, face="plain", vjust = -10, hjust = 0.5))+
  theme(axis.title = element_text(size=11))

grid.arrange(a21, a22,a23, a24, a25, nrow = 2, ncol = 3)
