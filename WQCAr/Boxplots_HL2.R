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

# try to do boxplots by samply type differentiating by country

c1<-WQ_samples_2019_HL %>%
  filter(`Sample type` %in% 
           c("streamwater", "groundwater", "reservoir")) %>%
  ggplot() +
  aes(x = Period, y = `T [C]`, colour = Country) +
  geom_boxplot(fill = "#ffffff") +
  scale_color_hue() +
  labs(title = "T [C]") +
  ggthemes::theme_base() +
  facet_wrap(vars(`Sample type`), scales = "fixed")+
  theme(plot.title = element_text(color="red", size=11, face="plain", vjust = -20, hjust = 0.5))+
  theme(axis.title = element_text(size=11))+
  theme(legend.title = element_text(size = 11))+
  geom_jitter(shape=1, position=position_jitter(0.2))

c2<-WQ_samples_2019_HL %>%
  filter(`Sample type` %in% 
           c("streamwater", "groundwater", "reservoir")) %>%
  ggplot() +
  aes(x = Period, y = pH, colour = Country) +
  geom_boxplot(fill = "#ffffff") +
  scale_color_hue() +
  labs(title = "pH") +
  ggthemes::theme_base() +
  facet_wrap(vars(`Sample type`), scales = "fixed")+
  theme(plot.title = element_text(color="red", size=11, face="plain", vjust = -20, hjust = 0.5))+
  theme(axis.title = element_text(size=11))+
  theme(legend.title = element_text(size = 11))+
  geom_jitter(shape=1, position=position_jitter(0.2))

c3<-WQ_samples_2019_HL %>%
  filter(`Sample type` %in% 
           c("streamwater", "groundwater", "reservoir")) %>%
  ggplot() +
  aes(x = Period, y = `EC [microS/cm]`, colour = Country) +
  geom_boxplot(fill = "#ffffff") +
  scale_color_hue() +
  labs(title = "EC [microS/cm]") +
  ggthemes::theme_base() +
  facet_wrap(vars(`Sample type`), scales = "fixed")+
  theme(plot.title = element_text(color="red", size=11, face="plain", vjust = -20, hjust = 0.5))+
  theme(axis.title = element_text(size=11))+
  theme(legend.title = element_text(size = 11))+
  geom_jitter(shape=1, position=position_jitter(0.2))

c4<-WQ_samples_2019_HL %>%
  filter(`Sample type` %in% 
           c("streamwater", "groundwater", "reservoir")) %>%
  ggplot() +
  aes(x = Period, y = `TDS [mg/L]`, colour = Country) +
  geom_boxplot(fill = "#ffffff") +
  scale_color_hue() +
  labs(title = "TDS [mg/L]") +
  ggthemes::theme_base() +
  facet_wrap(vars(`Sample type`), scales = "fixed")+
  theme(plot.title = element_text(color="red", size=11, face="plain", vjust = -20, hjust = 0.5))+
  theme(axis.title = element_text(size=11))+
  theme(legend.title = element_text(size = 11))+
  geom_jitter(shape=1, position=position_jitter(0.2))

c5<-WQ_samples_2019_HL %>%
  filter(`Sample type` %in% 
           c("streamwater", "groundwater", "reservoir")) %>%
  ggplot() +
  aes(x = Period, y = `NO+3-N [mg/L]`, colour = Country) +
  geom_boxplot(fill = "#ffffff") +
  scale_color_hue() +
  labs(title = "NO+3-N [mg/L]") +
  ggthemes::theme_base() +
  facet_wrap(vars(`Sample type`), scales = "fixed")+
  theme(plot.title = element_text(color="red", size=11, face="plain", vjust = -20, hjust = 0.5))+
  theme(axis.title = element_text(size=11))+
  theme(legend.title = element_text(size = 11))+
  geom_jitter(shape=1, position=position_jitter(0.2))

c6<-WQ_samples_2019_HL %>%
  filter(`Sample type` %in% 
           c("streamwater", "groundwater", "reservoir")) %>%
  ggplot() +
  aes(x = Period, y = `PO3-4-P [mg/L]`, colour = Country) +
  geom_boxplot(fill = "#ffffff") +
  scale_color_hue() +
  labs(title = "PO3-4-P [mg/L]") +
  ggthemes::theme_base() +
  facet_wrap(vars(`Sample type`), scales = "fixed")+
  theme(plot.title = element_text(color="red", size=11, face="plain", vjust = -20, hjust = 0.5))+
  theme(axis.title = element_text(size=11))+
  theme(legend.title = element_text(size = 11))+
  geom_jitter(shape=1, position=position_jitter(0.2))

grid.arrange(c1, c2, c3, c4, c5, c6, nrow = 3, ncol = 2)

c7<-WQ_samples_2019_HL %>%
  filter(`Sample type` %in% 
           c("streamwater", "groundwater", "reservoir")) %>%
  ggplot() +
  aes(x = Period, y = `Fe-Dis [mg/L]`, colour = Country) +
  geom_boxplot(fill = "#ffffff") +
  scale_color_hue() +
  labs(title = "Fe-Dis [mg/L]") +
  ggthemes::theme_base() +
  facet_wrap(vars(`Sample type`), scales = "fixed")+
  theme(plot.title = element_text(color="red", size=11, face="plain", vjust = -20, hjust = 0.5))+
  theme(axis.title = element_text(size=11))+
  theme(legend.title = element_text(size = 11))+
  geom_jitter(shape=1, position=position_jitter(0.15))
              
c8<-WQ_samples_2019_HL %>%
  filter(`Sample type` %in% 
           c("streamwater", "groundwater", "reservoir")) %>%
  ggplot() +
  aes(x = Period, y = `Fe-Tot [mg/L]`, colour = Country) +
  geom_boxplot(fill = "#ffffff") +
  scale_color_hue() +
  labs(title = "Fe Tot [mg/L] ") +
  ggthemes::theme_base() +
  facet_wrap(vars(`Sample type`), scales = "fixed")+
  theme(plot.title = element_text(color="red", size=11, face="plain", vjust = -20, hjust = 0.5))+
  theme(axis.title = element_text(size=11))+
  theme(legend.title = element_text(size = 11))+
  geom_jitter(shape=1, position=position_jitter(0.15))

c9<-WQ_samples_2019_HL %>%
  filter(`Sample type` %in% 
           c("streamwater", "groundwater", "reservoir")) %>%
  ggplot() +
  aes(x = Period, y = `Ca [mg/l]`, colour = Country) +
  geom_boxplot(fill = "#ffffff") +
  scale_color_hue() +
  labs(title = "Ca [mg/l]") +
  ggthemes::theme_base() +
  facet_wrap(vars(`Sample type`), scales = "fixed")+
  theme(plot.title = element_text(color="red", size=11, face="plain", vjust = -20, hjust = 0.5))+
  theme(axis.title = element_text(size=11))+
  theme(legend.title = element_text(size = 11))+
  geom_jitter(shape=1, position=position_jitter(0.15))


c10<-WQ_samples_2019_HL %>%
  filter(`Sample type` %in% 
           c("streamwater", "groundwater", "reservoir")) %>%
  ggplot() +
  aes(x = Period, y = `Mg [mg/l]`, colour = Country) +
  geom_boxplot(fill = "#ffffff") +
  scale_color_hue() +
  labs(title = "Mg [mg/l") +
  ggthemes::theme_base() +
  facet_wrap(vars(`Sample type`), scales = "fixed")+
  theme(plot.title = element_text(color="red", size=11, face="plain", vjust = -20, hjust = 0.5))+
  theme(axis.title = element_text(size=11))+
  theme(legend.title = element_text(size = 11))+
  geom_jitter(shape=1, position=position_jitter(0.15))

c11<-WQ_samples_2019_HL %>%
  filter(`Sample type` %in% 
           c("streamwater", "groundwater", "reservoir")) %>%
  ggplot() +
  aes(x = Period, y = `Na [mg/l]`, colour = Country) +
  geom_boxplot(fill = "#ffffff") +
  scale_color_hue() +
  labs(title = "Na [mg/l]") +
  ggthemes::theme_base() +
  facet_wrap(vars(`Sample type`), scales = "fixed")+
  theme(plot.title = element_text(color="red", size=11, face="plain", vjust = -20, hjust = 0.5))+
  theme(axis.title = element_text(size=11))+
  theme(legend.title = element_text(size = 11))+
  geom_jitter(shape=1, position=position_jitter(0.15))

c12<-WQ_samples_2019_HL %>%
  filter(`Sample type` %in% 
           c("streamwater", "groundwater", "reservoir")) %>%
  ggplot() +
  aes(x = Period, y = `K [mg/L]`, colour = Country) +
  geom_boxplot(fill = "#ffffff") +
  scale_color_hue() +
  labs(title = "K [mg/L]") +
  ggthemes::theme_base() +
  facet_wrap(vars(`Sample type`), scales = "fixed")+
  theme(plot.title = element_text(color="red", size=11, face="plain", vjust = -20, hjust = 0.5))+
  theme(axis.title = element_text(size=11))+
  theme(legend.title = element_text(size = 11))+
  geom_jitter(shape=1, position=position_jitter(0.15))

grid.arrange(c7, c8, c9, c10, c11, c12, nrow = 3, ncol = 2)

c13<-WQ_samples_2019_HL %>%
  filter(`Sample type` %in% 
           c("streamwater", "groundwater", "reservoir")) %>%
  ggplot() +
  aes(x = Period, y = `Al [ug/L]`, colour = Country) +
  geom_boxplot(fill = "#ffffff") +
  scale_color_hue() +
  labs(title = "Al [ug/L]") +
  ggthemes::theme_base() +
  facet_wrap(vars(`Sample type`), scales = "fixed")+
  theme(plot.title = element_text(color="red", size=11, face="plain", vjust = -20, hjust = 0.5))+
  theme(axis.title = element_text(size=11))+
  theme(legend.title = element_text(size = 11))+
  geom_jitter(shape=1, position=position_jitter(0.15))


c14<-WQ_samples_2019_HL %>%
  filter(`Sample type` %in% 
           c("streamwater", "groundwater", "reservoir")) %>%
  ggplot() +
  aes(x = Period, y = `As [ug/L]`, colour = Country) +
  geom_boxplot(fill = "#ffffff") +
  scale_color_hue() +
  labs(title = "As [ug/L]") +
  ggthemes::theme_base() +
  facet_wrap(vars(`Sample type`), scales = "fixed")+
  theme(plot.title = element_text(color="red", size=11, face="plain", vjust = -20, hjust = 0.5))+
  theme(axis.title = element_text(size=11))+
  theme(legend.title = element_text(size = 11))+
  geom_jitter(shape=1, position=position_jitter(0.15))


c15<-WQ_samples_2019_HL %>%
  filter(`Sample type` %in% 
           c("streamwater", "groundwater", "reservoir")) %>%
  ggplot() +
  aes(x = Period, y = `Ba [ug/L]`, colour = Country) +
  geom_boxplot(fill = "#ffffff") +
  scale_color_hue() +
  labs(title = "Ba [ug/L]") +
  ggthemes::theme_base() +
  facet_wrap(vars(`Sample type`), scales = "fixed")+
  theme(plot.title = element_text(color="red", size=11, face="plain", vjust = -20, hjust = 0.5))+
  theme(axis.title = element_text(size=11))+
  theme(legend.title = element_text(size = 11))+
  geom_jitter(shape=1, position=position_jitter(0.15))


c16<-WQ_samples_2019_HL %>%
  filter(`Sample type` %in% 
           c("streamwater", "groundwater", "reservoir")) %>%
  ggplot() +
  aes(x = Period, y = `Cd [ug/L]`, colour = Country) +
  geom_boxplot(fill = "#ffffff") +
  scale_color_hue() +
  labs(title = "Cd [ug/L]") +
  ggthemes::theme_base() +
  facet_wrap(vars(`Sample type`), scales = "fixed")+
  theme(plot.title = element_text(color="red", size=11, face="plain", vjust = -20, hjust = 0.5))+
  theme(axis.title = element_text(size=11))+
  theme(legend.title = element_text(size = 11))+
  geom_jitter(shape=1, position=position_jitter(0.15))


c17<-WQ_samples_2019_HL %>%
  filter(`Sample type` %in% 
           c("streamwater", "groundwater", "reservoir")) %>%
  ggplot() +
  aes(x = Period, y = `Co [ug/L]`, colour = Country) +
  geom_boxplot(fill = "#ffffff") +
  scale_color_hue() +
  labs(title = "Co [ug/L]") +
  ggthemes::theme_base() +
  facet_wrap(vars(`Sample type`), scales = "fixed")+
  theme(plot.title = element_text(color="red", size=11, face="plain", vjust = -20, hjust = 0.5))+
  theme(axis.title = element_text(size=11))+
  theme(legend.title = element_text(size = 11))+
  geom_jitter(shape=1, position=position_jitter(0.15))


c18<-WQ_samples_2019_HL %>%
  filter(`Sample type` %in% 
           c("streamwater", "groundwater", "reservoir")) %>%
  ggplot() +
  aes(x = Period, y = `Cr [ug/L]`, colour = Country) +
  geom_boxplot(fill = "#ffffff") +
  scale_color_hue() +
  labs(title = "Cr [ug/L]") +
  ggthemes::theme_base() +
  facet_wrap(vars(`Sample type`), scales = "fixed")+
  theme(plot.title = element_text(color="red", size=11, face="plain", vjust = -20, hjust = 0.5))+
  theme(axis.title = element_text(size=11))+
  theme(legend.title = element_text(size = 11))+
  geom_jitter(shape=1, position=position_jitter(0.15))

grid.arrange(c13, c14, c15, c16, c17, c18, nrow = 3, ncol = 2)

c19<-WQ_samples_2019_HL %>%
  filter(`Sample type` %in% 
           c("streamwater", "groundwater", "reservoir")) %>%
  ggplot() +
  aes(x = Period, y = `Cu [ug/L]`, colour = Country) +
  geom_boxplot(fill = "#ffffff") +
  scale_color_hue() +
  labs(title = "Cu [ug/L]") +
  ggthemes::theme_base() +
  facet_wrap(vars(`Sample type`), scales = "fixed")+
  theme(plot.title = element_text(color="red", size=11, face="plain", vjust = -20, hjust = 0.5))+
  theme(axis.title = element_text(size=11))+
  theme(legend.title = element_text(size = 11))+
  geom_jitter(shape=1, position=position_jitter(0.15))

c20<-WQ_samples_2019_HL %>%
  filter(`Sample type` %in% 
           c("streamwater", "groundwater", "reservoir")) %>%
  ggplot() +
  aes(x = Period, y = `Li [ug/L]`, colour = Country) +
  geom_boxplot(fill = "#ffffff") +
  scale_color_hue() +
  labs(title = "Li [ug/L]") +
  ggthemes::theme_base() +
  facet_wrap(vars(`Sample type`), scales = "fixed")+
  theme(plot.title = element_text(color="red", size=11, face="plain", vjust = -20, hjust = 0.5))+
  theme(axis.title = element_text(size=11))+
  theme(legend.title = element_text(size = 11))+
  geom_jitter(shape=1, position=position_jitter(0.15))


c21<-WQ_samples_2019_HL %>%
  filter(`Sample type` %in% 
           c("streamwater", "groundwater", "reservoir")) %>%
  ggplot() +
  aes(x = Period, y = `Mn [ug/L]`, colour = Country) +
  geom_boxplot(fill = "#ffffff") +
  scale_color_hue() +
  labs(title = "Mn [ug/L]") +
  ggthemes::theme_base() +
  facet_wrap(vars(`Sample type`), scales = "fixed")+
  theme(plot.title = element_text(color="red", size=11, face="plain", vjust = -20, hjust = 0.5))+
  theme(axis.title = element_text(size=11))+
  theme(legend.title = element_text(size = 11))+
  geom_jitter(shape=1, position=position_jitter(0.15))


c22<-WQ_samples_2019_HL %>%
  filter(`Sample type` %in% 
           c("streamwater", "groundwater", "reservoir")) %>%
  ggplot() +
  aes(x = Period, y = `Ni [ug/L]`, colour = Country) +
  geom_boxplot(fill = "#ffffff") +
  scale_color_hue() +
  labs(title = "Ni [ug/L]") +
  ggthemes::theme_base() +
  facet_wrap(vars(`Sample type`), scales = "fixed")+
  theme(plot.title = element_text(color="red", size=11, face="plain", vjust = -20, hjust = 0.5))+
  theme(axis.title = element_text(size=11))+
  theme(legend.title = element_text(size = 11))+
  geom_jitter(shape=1, position=position_jitter(0.15))

c23<-WQ_samples_2019_HL %>%
  filter(`Sample type` %in% 
           c("streamwater", "groundwater", "reservoir")) %>%
  ggplot() +
  aes(x = Period, y = `Pb [ug/L]`, colour = Country) +
  geom_boxplot(fill = "#ffffff") +
  scale_color_hue() +
  labs(title = "Pb [ug/L]") +
  ggthemes::theme_base() +
  facet_wrap(vars(`Sample type`), scales = "fixed")+
  theme(plot.title = element_text(color="red", size=11, face="plain", vjust = -20, hjust = 0.5))+
  theme(axis.title = element_text(size=11))+
  theme(legend.title = element_text(size = 11))+
  geom_jitter(shape=1, position=position_jitter(0.15))



c24<-WQ_samples_2019_HL %>%
  filter(`Sample type` %in% 
           c("streamwater", "groundwater", "reservoir")) %>%
  ggplot() +
  aes(x = Period, y = `Zn [ug/L]`, colour = Country) +
  geom_boxplot(fill = "#ffffff") +
  scale_color_hue() +
  labs(title = "Zn [ug/L]") +
  ggthemes::theme_base() +
  facet_wrap(vars(`Sample type`), scales = "fixed")+
  theme(plot.title = element_text(color="red", size=11, face="plain", vjust = -20, hjust = 0.5))+
  theme(axis.title = element_text(size=11))+
  theme(legend.title = element_text(size = 11))+
  geom_jitter(shape=1, position=position_jitter(0.15))

grid.arrange(c19, c20, c21, c22, c23, c24, nrow = 3, ncol = 2)

c25<-WQ_samples_2019_HL %>%
  filter(`Sample type` %in% 
           c("streamwater", "groundwater", "reservoir")) %>%
  ggplot() +
  aes(x = Period, y = `Sr [ug/L]`, colour = Country) +
  geom_boxplot(fill = "#ffffff") +
  scale_color_hue() +
  labs(title = "Sr [ug/L]") +
  ggthemes::theme_base() +
  facet_wrap(vars(`Sample type`), scales = "fixed")+
  theme(plot.title = element_text(color="red", size=11, face="plain", vjust = -20, hjust = 0.5))+
  theme(axis.title = element_text(size=11))+
  theme(legend.title = element_text(size = 11))+
  geom_jitter(shape=1, position=position_jitter(0.15))