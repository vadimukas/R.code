# Before we start install the required packages

# exploring new WQ data, first load the required packages
library(readr) 
library(ggplot2)
library(tidyverse)
#https://boxuancui.github.io/DataExplorer/ - package DataExplorer to explore the dataset
library(DataExplorer)
library(psych)
library(DescTools)
library(pastecs)
library(gridExtra)

#set working directory to subfolder WQCAloc
setwd("D:/R/R.code/WQCAloc")

# load local combined dataset for GW by groundwater types

local_WQ_GW <- read_csv("local_WQ_GW.csv", 
                        col_types = cols(Time = col_time(format = "%H:%M"), 
                                         Month = col_character()))
View(local_WQ_GW)

# in addition explore local combined dataset for GW by groundwater types

plot_boxplot(local_WQ_GW [c(11, 14:15, 18:19)], by = "GW type", ggtheme = theme_test(), geom_boxplot_args = list("outlier.color" = "red"))

g1<-ggplot(local_WQ_GW, aes(`GW type`, `T [C]`))+
  geom_boxplot(outlier.color = "red")+
  coord_flip()+
  theme_test()

g2<-ggplot(local_WQ_GW, aes(`GW type`, `EC [microS/cm]`))+
  geom_boxplot(outlier.color = "red")+
   coord_flip()+
  theme_test()

g3<-ggplot(local_WQ_GW, aes(`GW type`,`NO3-N [mg/L]`))+
  geom_boxplot(outlier.color = "red")+
  geom_hline(yintercept = 11.3, color="red", size= 0.5)+
  coord_flip()+
   theme_test()

g4<-ggplot(local_WQ_GW, aes(`GW type`,`PO4-P [mg/L]`))+
geom_boxplot(outlier.color = "red")+
  coord_flip()+
  theme_test()

fig_7<-grid.arrange(g1, g2, g3, g4, nrow = 2, ncol = 2)
ggsave(fig_7, filename = "fig_7.png", width = 6, height = 5, units="in", dpi = 600)

# get some stats for local GW measurements
local_WQ_GWm <- read_csv("local_WQ_GWm.csv", 
                        col_types = cols(Time = col_time(format = "%H:%M"), 
                                         Month = col_character()))

local_WQ_GW %>% 
  filter(`GW type` == "urban") %>% 
  summarize(median(`NO3-N [mg/L]`))


local_WQ_GWm %>% 
  filter(GW == "urban") %>% 
  summarize(median(`NO3-N [mg/L]`))

local_WQ_GWm %>% 
  filter(GW == "urban") %>% 
  summarize(mean(`NO3-N [mg/L]`))

local_WQ_GWm %>% 
  filter(GW == "urban") %>% 
  summarize(median(`EC [microS/cm]`))

local_WQ_GWm %>% 
  filter(GW == "urban") %>% 
  summarize(mean(`EC [microS/cm]`))

local_WQ_GWm %>% 
  filter(GW == "urban") %>% 
  summarize(median(`T [C]`))

local_WQ_GWm %>% 
  filter(GW == "urban") %>% 
  summarize(mean(`T [C]`))



