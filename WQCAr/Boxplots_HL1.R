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

# bulding boxplots by sample type for high - low flow periods

b1<-WQ_samples_2019_HL %>%
  filter(`Sample type` %in% 
           c("streamwater", "groundwater", "reservoir")) %>%
  ggplot() +
  aes(x = Period, y = `T [C]`) +
  geom_boxplot(fill = "#ffffff") +
  geom_jitter(shape=16, position=position_jitter(0.2))+
  labs(title = "T [C]") +
  ggthemes::theme_base() +
  theme(plot.title = element_text(color="Blue", size=13, face="plain", vjust = -20, hjust = 0.5))+
  theme(axis.title = element_text(size=11))+
  facet_wrap(vars(`Sample type`))

b2<-WQ_samples_2019_HL %>%
  filter(`Sample type` %in% 
           c("streamwater", "groundwater", "reservoir")) %>%
  ggplot() +
  aes(x = Period, y =pH) +
  geom_boxplot(fill = "#ffffff") +
  geom_jitter(shape=16, position=position_jitter(0.2))+
  labs(title = "pH") +
  ggthemes::theme_base() +
  theme(plot.title = element_text(color="Blue", size=13, face="plain", vjust = -20, hjust = 0.5))+
  theme(axis.title = element_text(size=11))+
  facet_wrap(vars(`Sample type`))

b3<-WQ_samples_2019_HL %>%
  filter(`Sample type` %in% 
           c("streamwater", "groundwater", "reservoir")) %>%
  ggplot() +
  aes(x = Period, y =`EC [microS/cm]`) +
  geom_boxplot(fill = "#ffffff") +
  geom_jitter(shape=16, position=position_jitter(0.2))+
  labs(title = "EC [microS/cm]") +
  ggthemes::theme_base() +
  theme(plot.title = element_text(color="Blue", size=13, face="plain", vjust = -20, hjust = 0.5))+
  theme(axis.title = element_text(size=11))+
  facet_wrap(vars(`Sample type`))

# grid.arrange(b1, b2, b3, nrow = 3, ncol = 1)

b4<-WQ_samples_2019_HL %>%
  filter(`Sample type` %in% 
           c("streamwater", "groundwater", "reservoir")) %>%
  ggplot() +
  aes(x = Period, y = `TDS [mg/L]`) +
  geom_boxplot(fill = "#ffffff") +
  geom_jitter(shape=16, position=position_jitter(0.2))+
  labs(title = "TDS [mg/L]") +
  ggthemes::theme_base() +
  theme(plot.title = element_text(color="Blue", size=13, face="plain", vjust = -20, hjust = 0.5))+
  theme(axis.title = element_text(size=11))+
  facet_wrap(vars(`Sample type`))

b5<-WQ_samples_2019_HL %>%
  filter(`Sample type` %in% 
           c("streamwater", "groundwater", "reservoir")) %>%
  ggplot() +
  aes(x = Period, y = `PO3-4-P [mg/L]`) +
  geom_boxplot(fill = "#ffffff") +
  geom_jitter(shape=16, position=position_jitter(0.2))+
  labs(title = "PO3-4-P [mg/L]") +
  ggthemes::theme_base() +
  theme(plot.title = element_text(color="Blue", size=13, face="plain", vjust = -20, hjust = 0.5))+
  theme(axis.title = element_text(size=11))+
  facet_wrap(vars(`Sample type`))

b6<-WQ_samples_2019_HL %>%
  filter(`Sample type` %in% 
           c("streamwater", "groundwater", "reservoir")) %>%
  ggplot() +
  aes(x = Period, y = `NO+3-N [mg/L]`) +
  geom_boxplot(fill = "#ffffff") +
  geom_jitter(shape=16, position=position_jitter(0.2))+
  labs(title = "NO+3-N [mg/L]") +
  ggthemes::theme_base() +
  theme(plot.title = element_text(color="Blue", size=13, face="plain", vjust = -20, hjust = 0.5))+
  theme(axis.title = element_text(size=11))+
  facet_wrap(vars(`Sample type`))

grid.arrange(b1, b2, b3, b4, b5, b6, nrow = 2, ncol = 3)


b7<-WQ_samples_2019_HL %>%
  filter(`Sample type` %in% 
           c("streamwater", "groundwater", "reservoir")) %>%
  ggplot() +
  aes(x = Period, y = `Fe-Dis [mg/L]`) +
  geom_boxplot(fill = "#ffffff") +
  geom_jitter(shape=16, position=position_jitter(0.2))+
  labs(title = "Fe-Dis [mg/L]") +
  ggthemes::theme_base() +
  theme(plot.title = element_text(color="Blue", size=13, face="plain", vjust = -20, hjust = 0.5))+
  theme(axis.title = element_text(size=11))+
  facet_wrap(vars(`Sample type`))

b8<-WQ_samples_2019_HL %>%
  filter(`Sample type` %in% 
           c("streamwater", "groundwater", "reservoir")) %>%
  ggplot() +
  aes(x = Period, y = `Ca [mg/l]`) +
  geom_boxplot(fill = "#ffffff") +
  geom_jitter(shape=16, position=position_jitter(0.2))+
  labs(title = "Ca [mg/l]") +
  ggthemes::theme_base() +
  theme(plot.title = element_text(color="Blue", size=13, face="plain", vjust = -20, hjust = 0.5))+
  theme(axis.title = element_text(size=11))+
  facet_wrap(vars(`Sample type`))

b9<-WQ_samples_2019_HL %>%
  filter(`Sample type` %in% 
           c("streamwater", "groundwater", "reservoir")) %>%
  ggplot() +
  aes(x = Period, y = `Mg [mg/l]`) +
  geom_boxplot(fill = "#ffffff") +
  geom_jitter(shape=16, position=position_jitter(0.2))+
  labs(title = "Mg [mg/l]") +
  ggthemes::theme_base() +
  theme(plot.title = element_text(color="Blue", size=13, face="plain", vjust = -20, hjust = 0.5))+
  theme(axis.title = element_text(size=11))+
  facet_wrap(vars(`Sample type`))

b10<-WQ_samples_2019_HL %>%
  filter(`Sample type` %in% 
           c("streamwater", "groundwater", "reservoir")) %>%
  ggplot() +
  aes(x = Period, y = `Na [mg/l]`) +
  geom_boxplot(fill = "#ffffff") +
  geom_jitter(shape=16, position=position_jitter(0.2))+
  labs(title = "Na [mg/l]") +
  ggthemes::theme_base() +
  theme(plot.title = element_text(color="Blue", size=13, face="plain", vjust = -20, hjust = 0.5))+
  theme(axis.title = element_text(size=11))+
  facet_wrap(vars(`Sample type`))

b11<-WQ_samples_2019_HL %>%
  filter(`Sample type` %in% 
           c("streamwater", "groundwater", "reservoir")) %>%
  ggplot() +
  aes(x = Period, y = `K [mg/L]`) +
  geom_boxplot(fill = "#ffffff") +
  geom_jitter(shape=16, position=position_jitter(0.2))+
  labs(title = "K [mg/L]") +
  ggthemes::theme_base() +
  theme(plot.title = element_text(color="Blue", size=13, face="plain", vjust = -20, hjust = 0.5))+
  theme(axis.title = element_text(size=11))+
  facet_wrap(vars(`Sample type`))

b12<-WQ_samples_2019_HL %>%
  filter(`Sample type` %in% 
           c("streamwater", "groundwater", "reservoir")) %>%
  ggplot() +
  aes(x = Period, y = `Fe-Tot [mg/L]`) +
  geom_boxplot(fill = "#ffffff") +
  geom_jitter(shape=16, position=position_jitter(0.2))+
  labs(title = "Fe-Tot [mg/L]") +
  ggthemes::theme_base() +
  theme(plot.title = element_text(color="Blue", size=13, face="plain", vjust = -20, hjust = 0.5))+
  theme(axis.title = element_text(size=11))+
  facet_wrap(vars(`Sample type`))

grid.arrange(b7, b8, b9, b10, b11, b12, nrow = 2, ncol = 3)

b13<-WQ_samples_2019_HL %>%
  filter(`Sample type` %in% 
           c("streamwater", "groundwater", "reservoir")) %>%
  ggplot() +
  aes(x = Period, y = `Al [ug/L]`) +
  geom_boxplot(fill = "#ffffff") +
  geom_jitter(shape=16, position=position_jitter(0.2))+
  labs(title = "Al [ug/L]") +
  ggthemes::theme_base() +
  theme(plot.title = element_text(color="Blue", size=13, face="plain", vjust = -20, hjust = 0.5))+
  theme(axis.title = element_text(size=11))+
  facet_wrap(vars(`Sample type`))

b14<-WQ_samples_2019_HL %>%
  filter(`Sample type` %in% 
           c("streamwater", "groundwater", "reservoir")) %>%
  ggplot() +
  aes(x = Period, y = `As [ug/L]`) +
  geom_boxplot(fill = "#ffffff") +
  geom_jitter(shape=16, position=position_jitter(0.2))+
  labs(title = "As [ug/L]") +
  ggthemes::theme_base() +
  theme(plot.title = element_text(color="Blue", size=13, face="plain", vjust = -20, hjust = 0.5))+
  theme(axis.title = element_text(size=11))+
  facet_wrap(vars(`Sample type`))

b15<-WQ_samples_2019_HL %>%
  filter(`Sample type` %in% 
           c("streamwater", "groundwater", "reservoir")) %>%
  ggplot() +
  aes(x = Period, y = `Ba [ug/L]`) +
  geom_boxplot(fill = "#ffffff") +
  geom_jitter(shape=16, position=position_jitter(0.2))+
  labs(title = "Ba [ug/L]") +
  ggthemes::theme_base() +
  theme(plot.title = element_text(color="Blue", size=13, face="plain", vjust = -20, hjust = 0.5))+
  theme(axis.title = element_text(size=11))+
  facet_wrap(vars(`Sample type`))

b16<-WQ_samples_2019_HL %>%
  filter(`Sample type` %in% 
           c("streamwater", "groundwater", "reservoir")) %>%
  ggplot() +
  aes(x = Period, y = `Cu [ug/L]`) +
  geom_boxplot(fill = "#ffffff") +
  geom_jitter(shape=16, position=position_jitter(0.2))+
  labs(title = "Cu [ug/L]") +
  ggthemes::theme_base() +
  theme(plot.title = element_text(color="Blue", size=13, face="plain", vjust = -20, hjust = 0.5))+
  theme(axis.title = element_text(size=11))+
  facet_wrap(vars(`Sample type`))

b17<-WQ_samples_2019_HL %>%
  filter(`Sample type` %in% 
           c("streamwater", "groundwater", "reservoir")) %>%
  ggplot() +
  aes(x = Period, y = `Mn [ug/L]`) +
  geom_boxplot(fill = "#ffffff") +
  geom_jitter(shape=16, position=position_jitter(0.2))+
  labs(title = "Mn [ug/L]") +
  ggthemes::theme_base() +
  theme(plot.title = element_text(color="Blue", size=13, face="plain", vjust = -20, hjust = 0.5))+
  theme(axis.title = element_text(size=11))+
  facet_wrap(vars(`Sample type`))

b18<-WQ_samples_2019_HL %>%
  filter(`Sample type` %in% 
           c("streamwater", "groundwater", "reservoir")) %>%
  ggplot() +
  aes(x = Period, y = `Zn [ug/L]`) +
  geom_boxplot(fill = "#ffffff") +
  geom_jitter(shape=16, position=position_jitter(0.2))+
  labs(title = "Zn [ug/L]") +
  ggthemes::theme_base() +
  theme(plot.title = element_text(color="Blue", size=13, face="plain", vjust = -20, hjust = 0.5))+
  theme(axis.title = element_text(size=11))+
  facet_wrap(vars(`Sample type`))

grid.arrange(b13, b14, b15, b16, b17, b18, nrow = 2, ncol = 3)

b19<-WQ_samples_2019_HL %>%
  filter(`Sample type` %in% 
           c("streamwater", "groundwater", "reservoir")) %>%
  ggplot() +
  aes(x = Period, y = `Cd [ug/L]`) +
  geom_boxplot(fill = "#ffffff") +
  geom_jitter(shape=16, position=position_jitter(0.2))+
  labs(title = "Cd [ug/L]") +
  ggthemes::theme_base() +
  theme(plot.title = element_text(color="Blue", size=13, face="plain", vjust = -20, hjust = 0.5))+
  theme(axis.title = element_text(size=11))+
  facet_wrap(vars(`Sample type`))

b20<-WQ_samples_2019_HL %>%
  filter(`Sample type` %in% 
           c("streamwater", "groundwater", "reservoir")) %>%
  ggplot() +
  aes(x = Period, y = `Co [ug/L]`) +
  geom_boxplot(fill = "#ffffff") +
  geom_jitter(shape=16, position=position_jitter(0.2))+
  labs(title = "Co [ug/L]") +
  ggthemes::theme_base() +
  theme(plot.title = element_text(color="Blue", size=13, face="plain", vjust = -20, hjust = 0.5))+
  theme(axis.title = element_text(size=11))+
  facet_wrap(vars(`Sample type`))

b21<-WQ_samples_2019_HL %>%
  filter(`Sample type` %in% 
           c("streamwater", "groundwater", "reservoir")) %>%
  ggplot() +
  aes(x = Period, y = `Cr [ug/L]`) +
  geom_boxplot(fill = "#ffffff") +
  geom_jitter(shape=16, position=position_jitter(0.2))+
  labs(title = "Cr [ug/L]") +
  ggthemes::theme_base() +
  theme(plot.title = element_text(color="Blue", size=13, face="plain", vjust = -20, hjust = 0.5))+
  theme(axis.title = element_text(size=11))+
  facet_wrap(vars(`Sample type`))


b22<-WQ_samples_2019_HL %>%
  filter(`Sample type` %in% 
           c("streamwater", "groundwater", "reservoir")) %>%
  ggplot() +
  aes(x = Period, y = `Li [ug/L]`) +
  geom_boxplot(fill = "#ffffff") +
  geom_jitter(shape=16, position=position_jitter(0.2))+
  labs(title = "Li [ug/L]") +
  ggthemes::theme_base() +
  theme(plot.title = element_text(color="Blue", size=13, face="plain", vjust = -20, hjust = 0.5))+
  theme(axis.title = element_text(size=11))+
  facet_wrap(vars(`Sample type`))

b23<-WQ_samples_2019_HL %>%
  filter(`Sample type` %in% 
           c("streamwater", "groundwater", "reservoir")) %>%
  ggplot() +
  aes(x = Period, y = `Ni [ug/L]`) +
  geom_boxplot(fill = "#ffffff") +
  geom_jitter(shape=16, position=position_jitter(0.2))+
  labs(title = "Ni [ug/L]") +
  ggthemes::theme_base() +
  theme(plot.title = element_text(color="Blue", size=13, face="plain", vjust = -20, hjust = 0.5))+
  theme(axis.title = element_text(size=11))+
  facet_wrap(vars(`Sample type`))

b24<-WQ_samples_2019_HL %>%
  filter(`Sample type` %in% 
           c("streamwater", "groundwater", "reservoir")) %>%
  ggplot() +
  aes(x = Period, y = `Pb [ug/L]`) +
  geom_boxplot(fill = "#ffffff") +
  geom_jitter(shape=16, position=position_jitter(0.2))+
  labs(title = "Pb [ug/L]") +
  ggthemes::theme_base() +
  theme(plot.title = element_text(color="Blue", size=13, face="plain", vjust = -20, hjust = 0.5))+
  theme(axis.title = element_text(size=11))+
  facet_wrap(vars(`Sample type`))

grid.arrange(b19, b20, b21, b22, b23, b24, nrow = 2, ncol = 3)

b24<-WQ_samples_2019_HL %>%
  filter(`Sample type` %in% 
           c("streamwater", "groundwater", "reservoir")) %>%
  ggplot() +
  aes(x = Period, y = `Sr [ug/L]`) +
  geom_boxplot(fill = "#ffffff") +
  geom_jitter(shape=16, position=position_jitter(0.2))+
  labs(title = "Sr [ug/L]") +
  ggthemes::theme_base() +
  theme(plot.title = element_text(color="Blue", size=13, face="plain", vjust = -20, hjust = 0.5))+
  theme(axis.title = element_text(size=11))+
  facet_wrap(vars(`Sample type`))
