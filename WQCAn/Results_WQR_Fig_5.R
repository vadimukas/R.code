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

# try to do boxplots by samply type differentiating by country first the most interesting ones
# starting with major ions

c1<-WQ_samples_2019_HL %>%
  filter(`Sample type` %in% 
           c("streamwater")) %>%
  ggplot() +
  aes(x = Period, y = `Ca [mg/l]`, colour = Country) +
  geom_boxplot(fill = "#ffffff", show.legend = FALSE) +
  scale_color_hue() +
  labs(title = quote(Ca~mg~l^-1))+
  ylab(expression(Ca~mg~l^-1)) +
  ggthemes::theme_base() +
  theme(plot.title = element_text(color="red", size=11, face="plain", vjust = -20, hjust = 0.35))+
  theme(axis.title = element_text(size=11))+
  theme(legend.title = element_text(size = 11))

c2<-WQ_samples_2019_HL %>%
  filter(`Sample type` %in% 
           c("streamwater")) %>%
  ggplot() +
  aes(x = Period, y = `Mg [mg/l]`, colour = Country) +
  geom_boxplot(fill = "#ffffff", show.legend = FALSE) +
  scale_color_hue() +
  labs(title = "Mg [mg/l") +
  ggthemes::theme_base() +
  theme(plot.title = element_text(color="red", size=11, face="plain", vjust = -20, hjust = 0.5))+
  theme(axis.title = element_text(size=11))+
  theme(legend.title = element_text(size = 11))

c3<-WQ_samples_2019_HL %>%
  filter(`Sample type` %in% 
           c("streamwater")) %>%
  ggplot() +
  aes(x = Period, y = `Na [mg/l]`, colour = Country) +
  geom_boxplot(fill = "#ffffff", show.legend = FALSE) +
  scale_color_hue() +
  labs(title = "Na [mg/l]") +
  ggthemes::theme_base() +
  theme(plot.title = element_text(color="red", size=11, face="plain", vjust = -20, hjust = 0.35))+
  theme(axis.title = element_text(size=11))+
  theme(legend.title = element_text(size = 11))

c4<-WQ_samples_2019_HL %>%
  filter(`Sample type` %in% 
           c("streamwater")) %>%
  ggplot() +
  aes(x = Period, y = `K [mg/L]`, colour = Country) +
  geom_boxplot(fill = "#ffffff") +
  scale_color_hue() +
  labs(title = "K [mg/L]") +
  ggthemes::theme_base() +
  theme(plot.title = element_text(color="red", size=11, face="plain", vjust = -20, hjust = 0.45))+
  theme(axis.title = element_text(size=11))+
  theme(legend.title = element_text(size = 11), legend.position = c(0.75, 0.75))

grid.arrange(c1, c2, c3, c4, nrow = 1, ncol = 4) 

# crustal elements

c5<-WQ_samples_2019_HL %>%
  filter(`Sample type` %in% 
           c("streamwater")) %>%
  ggplot() +
  aes(x = Period, y = `Fe-Tot [mg/L]`, colour = Country) +
  geom_boxplot(fill = "#ffffff", show.legend = FALSE) +
  scale_color_hue() +
  labs(title = "Fe Tot [mg/L] ") +
  ggthemes::theme_base() +
  theme(plot.title = element_text(color="red", size=11, face="plain", vjust = -20, hjust = 0.55))+
  theme(axis.title = element_text(size=11))+
  theme(legend.title = element_text(size = 11))

c6<-WQ_samples_2019_HL %>%
  filter(`Sample type` %in% 
           c("streamwater")) %>%
  ggplot() +
  aes(x = Period, y = `Al [ug/L]`, colour = Country) +
  geom_boxplot(fill = "#ffffff",show.legend = FALSE) +
  scale_color_hue() +
  labs(title = "Al [ug/L]") +
  ggthemes::theme_base() +
  theme(plot.title = element_text(color="red", size=11, face="plain", vjust = -20, hjust = 0.55))+
  theme(axis.title = element_text(size=11))+
  theme(legend.title = element_text(size = 11))

c7<-WQ_samples_2019_HL %>%
  filter(`Sample type` %in% 
           c("streamwater")) %>%
  ggplot() +
  aes(x = Period, y = `Cr [ug/L]`, colour = Country, show.legend = FALSE) +
  geom_boxplot(fill = "#ffffff",show.legend = FALSE) +
  scale_color_hue()+
  labs(title = "Cr [ug/L]") +
  ggthemes::theme_base() +
  theme(plot.title = element_text(color="red", size=11, face="plain", vjust = -20, hjust = 0.35))+
  theme(axis.title = element_text(size=11))+
  theme(legend.title = element_text(size = 11))

c8<-WQ_samples_2019_HL %>%
  filter(`Sample type` %in% 
           c("streamwater")) %>%
  ggplot() +
  aes(x = Period, y = `Mn [ug/L]`, colour = Country) +
  geom_boxplot(fill = "#ffffff", show.legend = FALSE) +
  scale_color_hue() +
  labs(title = "Mn [ug/L]") +
  ggthemes::theme_base() +
  theme(plot.title = element_text(color="red", size=11, face="plain", vjust = -20, hjust = 0.5))+
  theme(axis.title = element_text(size=11))+
  theme(legend.title = element_text(size = 11))


grid.arrange(c5, c6, c7, c8, nrow = 1, ncol = 4)

# now potentially toxic elements only stream water

c9<-WQ_samples_2019_HL %>%
  filter(`Sample type` %in% 
           c("streamwater")) %>%
  ggplot() +
  aes(x = Period, y = `As [ug/L]`, colour = Country) +
  geom_boxplot(fill = "#ffffff", show.legend = FALSE) +
  scale_color_hue() +
  labs(title = "As [ug/L]") +
  ggthemes::theme_base() +
  theme(plot.title = element_text(color="red", size=11, face="plain", vjust = -20, hjust = 0.5))+
  theme(axis.title = element_text(size=11))+
  theme(legend.title = element_text(size = 11))


c10<-WQ_samples_2019_HL %>%
  filter(`Sample type` %in% 
           c("streamwater")) %>%
  ggplot() +
  aes(x = Period, y = `Pb [ug/L]`, colour = Country) +
  geom_boxplot(fill = "#ffffff", show.legend = FALSE) +
  scale_color_hue() +
  labs(title = "Pb [ug/L]") +
  ggthemes::theme_base() +
  theme(plot.title = element_text(color="red", size=11, face="plain", vjust = -20, hjust = 0.55))+
  theme(axis.title = element_text(size=11))+
  theme(legend.title = element_text(size = 11))

c11<-WQ_samples_2019_HL %>%
  filter(`Sample type` %in% 
           c("streamwater", "groundwater")) %>%
  ggplot() +
  aes(x = Period, y = `Cd [ug/L]`, colour = Country) +
  geom_boxplot(fill = "#ffffff", show.legend = FALSE) +
  scale_color_hue() +
  labs(title = "Cd [ug/L]") +
  ggthemes::theme_base() +
  theme(plot.title = element_text(color="red", size=11, face="plain", vjust = -20, hjust = 0.55))+
  theme(axis.title = element_text(size=11))+
  theme(legend.title = element_text(size = 11))


c12<-WQ_samples_2019_HL %>%
  filter(`Sample type` %in% 
           c("streamwater", "groundwater")) %>%
  ggplot() +
  aes(x = Period, y = `Ni [ug/L]`, colour = Country) +
  geom_boxplot(fill = "#ffffff", show.legend = FALSE) +
  scale_color_hue() +
  labs(title = "Ni [ug/L]") +
  ggthemes::theme_base() +
  theme(plot.title = element_text(color="red", size=11, face="plain", vjust = -20, hjust = 0.35))+
  theme(axis.title = element_text(size=11))+
  theme(legend.title = element_text(size = 11))

grid.arrange(c9, c10, c11, c12, nrow = 1, ncol = 4) 
  
    c13<-WQ_samples_2019_HL %>%
      filter(`Sample type` %in% 
               c("streamwater")) %>%
      ggplot() +
      aes(x = Period, y = `Cu [ug/L]`, colour = Country) +
      geom_boxplot(fill = "#ffffff", show.legend = FALSE) +
      scale_color_hue() +
      labs(title = "Cu [ug/L]") +
      ggthemes::theme_base() +
      theme(plot.title = element_text(color="red", size=11, face="plain", vjust = -20, hjust = 0.5))+
      theme(axis.title = element_text(size=11))+
      theme(legend.title = element_text(size = 11))
    
    
    c14<-WQ_samples_2019_HL %>%
      filter(`Sample type` %in% 
               c("streamwater")) %>%
      ggplot() +
      aes(x = Period, y = `Zn [ug/L]`, colour = Country) +
      geom_boxplot(fill = "#ffffff",show.legend = FALSE) +
      scale_color_hue() +
      labs(title = "Zn [ug/L]") +
      ggthemes::theme_base() +
      theme(plot.title = element_text(color="red", size=11, face="plain", vjust = -20, hjust = 0.5))+
      theme(axis.title = element_text(size=11))+
      theme(legend.title = element_text(size = 11))
     
 
    grid.arrange(c13, c14, nrow = 1, ncol = 2)   
    
    WQ_samples_2019_HL %>%
      filter(`Sample type` %in% 
               c("streamwater", "groundwater")) %>%
      ggplot() +
      aes(x = Period, y = `Co [ug/L]`, colour = Country) +
      geom_boxplot(fill = "#ffffff") +
      scale_color_hue() +
      labs(title = "Co [ug/L]") +
      ggthemes::theme_base() +
      facet_wrap(vars(`Sample type`), scales = "fixed")+
      theme(plot.title = element_text(color="red", size=11, face="plain", vjust = -20, hjust = 0.35))+
      theme(axis.title = element_text(size=11))+
      theme(legend.title = element_text(size = 11))    
   
     

    

















