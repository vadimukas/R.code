# exploring a combined WQ data for high low flow comparison, first load the required packages
library(readr) 
library(ggplot2)
library(tidyverse)
library(DataExplorer)
library(psych)
library(DescTools)
library(pastecs)
library(gridExtra)
library(ggcorrplot)
library(ggbeeswarm)

#set working directory to subfolder WQCAn
setwd("D:/R/R.code/WQCAr")

# first load ICP and IC data for streamwater
# on subscripts and superscripts
#https://www.dataanalytics.org.uk/axis-labels-in-r-plots-using-expression/
# import data a bit differently

HF_LF <- read_csv("HF_LF_WQdata__2019-2020.csv", 
                  col_types = cols(Month = col_factor(levels = c("3","7", "8", "10", "11")), 
                                   Period = col_factor(levels = c("HF","LF")), 
                                   Country = col_factor(levels = c("KZ","KG", "UZ", "TJ"))), na = "NA")

library(lubridate)
HF_LF$Date<-parse_date_time(HF_LF$Date, order = "dmy")
is.POSIXct(HF_LF$Date)


# some experiments

HF_LF %>%
  filter(`Sample type` %in% 
           c("streamwater")) %>%
  ggplot() +
  aes(x = Period, y = `HCO3 [mg/L]`, colour = Country) +
  geom_boxplot(fill = "#ffffff", show.legend = FALSE) +
  scale_color_manual(values = c("blue", "red" , "black","#00AFBB")) +
  geom_text(aes(label = Country), size = 4, position = position_jitter(0.2), show.legend = FALSE)+
  labs(title = expression(HCO[3]^`-` ~ mg ~ L^-1))+
  ylab(expression()) +
  ggthemes::theme_base() +
  theme(plot.title = element_text(color="red", size=11, face="plain", vjust = -20, hjust = 0.55))+
  theme(axis.title = element_text(size=11))+
  theme(legend.title = element_text(size = 11))

HF_LF %>%
  filter(`Sample type` %in% 
           c("streamwater")) %>%
  ggplot() +
  aes(x = Period, y = `HCO3 [mg/L]`, colour = Country) +
  geom_violin(fill = "#ffffff", show.legend = FALSE) +
  scale_color_manual(values = c("blue", "red" , "black","#00AFBB")) +
  geom_text(aes(label = Country), size = 4, position = position_jitter(0.2), show.legend = FALSE)+
  labs(title = expression(HCO[3]^`-` ~ mg ~ L^-1))+
  ylab(expression()) +
  ggthemes::theme_base() +
  theme(plot.title = element_text(color="red", size=11, face="plain", vjust = -20, hjust = 0.55))+
  theme(axis.title = element_text(size=11))+
  theme(legend.title = element_text(size = 11))

HF_LF %>%
  filter(`Sample type` %in% 
           c("streamwater")) %>%
  ggplot() +
  aes(x = Period, y = `Ca [mg/l]`, colour = Country) +
  geom_violin(fill = "#ffffff", show.legend = FALSE) +
  scale_color_manual(values = c("blue", "red" , "black","#00AFBB")) +
  geom_text(aes(label = Country), size = 4, position = position_jitter(0.2), show.legend = FALSE)+
  labs(title = expression(Ca^`2+` ~ mg ~ L^-1))+
  ylab(expression()) +
  ggthemes::theme_base() +
  theme(plot.title = element_text(color="red", size=11, face="plain", vjust = -20, hjust = 0.55))+
  theme(axis.title = element_text(size=11))+
  theme(legend.title = element_text(size = 11))

HF_LF %>%
  filter(`Sample type` %in% 
           c("streamwater", "groundwater")) %>%
  ggplot() +
  aes(x = Period, y = `Ni [ug/L]`, colour = Country) +
  geom_violin(fill = "#ffffff", show.legend = TRUE, draw_quantiles = c(0.5)) + 
  geom_text(aes(label = Country), size = 3, position = position_jitter(0.5), show.legend = FALSE)+
  scale_color_manual(values = c("blue", "red" , "black","#00AFBB")) +
  ylab(expression()) +
  labs(title = quote(Ni~ug~L^-1)) +
  ggthemes::theme_base() +
  theme(plot.title = element_text(color="red", size=11, face="plain", vjust = -20, hjust = 0.35))+
  theme(axis.title = element_text(size=11))+
  theme(legend.title = element_text(size = 11),legend.position = c(0.90, 0.70))

# 
HF_LF %>%
  filter(`Sample type` %in% 
           c("streamwater", "groundwater")) %>%
  ggplot() +
  aes(x = Period, y = `Cd [ug/L]`, colour = Country) +
  geom_violin(show.legend = TRUE, draw_quantiles = c(0.5)) +
  geom_text(aes(label = Country), size = 3, position = position_jitter(0.5), show.legend = FALSE)+
  scale_color_manual(values = c("blue", "red" , "black","#00AFBB")) +
  labs(title = quote(Cd~ug~L^-1)) +
  ggthemes::theme_base() +
  theme(plot.title = element_text(color="red", size=11, face="plain", vjust = -20, hjust = 0.55))+
  theme(axis.title = element_text(size=11))+
  theme(legend.title = element_text(size = 11))

# correlation plots with ggplotcorrplot package

HFLFcor <-cor(HF_LF[,14:42])
ggcorrplot(HFLFcor)

WQRcor <-cor(WQ_samples_2019_2020[,13:41])
ggcorrplot(WQRcor)


# figure 6 with mapping color to Country

WQ_samples_2019_2020 %>%
  filter(`Sample type` %in% 
           c("streamwater", "groundwater", "glaciermelt", "snowpack")) %>%
  ggplot() +
  aes(x = month, y = `Fe [ug/L]`) +
  geom_boxplot(fill = "#ffffff") +
  geom_jitter()+
  ylim(0, 200)+
  ylab(expression()) +
  labs(title = quote(Fe~ug~L^-1)) +
  ggthemes::theme_base() +
  theme(plot.title = element_text(color="Blue", size=13, face="plain", vjust = -20, hjust = 0.4))+
  theme(axis.title = element_text(size=11))+
  facet_wrap(vars(`Sample type`))

