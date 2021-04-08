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

c1<-HF_LF %>%
  filter(`Sample type` %in% 
           c("streamwater")) %>%
  ggplot() +
  aes(x = Period, y = `HCO3 [mg/L]`, colour = Country) +
  geom_boxplot(fill = "#ffffff", show.legend = FALSE) +
  scale_color_manual(values = c("blue", "red" , "black","#008B8B")) +
  labs(title = expression(HCO[3]^`-` ~ mg ~ L^-1))+
  ylab(expression()) +
  ggthemes::theme_base() +
  theme(plot.title = element_text(color="blue", size=15, face="plain", vjust = 0, hjust = 0.50))+
  theme(axis.title = element_text(size=11))+
  theme(legend.title = element_text(size = 11),  legend.position = c(0.85, 0.75))+
  theme(axis.title.x = element_blank())

c2<-HF_LF %>%
  filter(`Sample type` %in% 
           c("streamwater")) %>%
  ggplot() +
  aes(x = Period, y = `Cl [mg/L]`, colour = Country) +
  geom_boxplot(fill = "#ffffff", show.legend = FALSE) +
  scale_color_manual(values = c("blue", "red" , "black","#00AFBB")) +
  labs(title = quote(Cl^'-' ~ mg~L^-1)) +
  ylab(expression()) +
  ggthemes::theme_base() +
  theme(plot.title = element_text(color="blue", size=15, face="plain", vjust = 0, hjust = 0.50))+
  theme(axis.title = element_text(size=11))+
  theme(legend.title = element_text(size = 11))+
  theme(axis.title.x = element_blank())

# filtering KZ-8 July 2020 (Ulken Almaty) 504.5 mg/L by limiting the scale to 250 mg\L
c3<-HF_LF %>%
  filter(`Sample type` %in% 
           c("streamwater")) %>%
  ggplot() +
  aes(x = Period, y = `SO4[mg/L]`, colour = Country) +
  geom_boxplot(fill = "#ffffff", show.legend = FALSE) +
  ylim(0, 250)+
  scale_color_manual(values = c("blue", "red" , "black","#00AFBB")) +
  labs(title = expression(SO[4]^'2-'~mg~L^-1)) +
  ylab(expression()) +
  ggthemes::theme_base() +
  theme(plot.title = element_text(color="blue", size=15, face="plain", vjust = 0, hjust = 0.50))+
  theme(axis.title = element_text(size=11))+
  theme(legend.title = element_text(size = 11),  legend.position = c(0.85, 0.75))+
  theme(axis.title.x = element_blank())

c4<-HF_LF %>%
  filter(`Sample type` %in% 
           c("streamwater")) %>%
  ggplot() +
  aes(x = Period, y = `Si [mg/L]`, colour = Country) +
  geom_boxplot(fill = "#ffffff", show.legend = FALSE) +
  scale_color_manual(values = c("blue", "red" , "black","#00AFBB")) +
  labs(title = quote(Si ~ mg~L^-1)) +
  ylab(expression()) +
  ggthemes::theme_base() +
  theme(plot.title = element_text(color="blue", size=15, face="plain", vjust = 0, hjust = 0.50))+
  theme(axis.title = element_text(size=11))+
  theme(legend.title = element_text(size = 11), legend.position = c(0.85, 0.20))+
  theme(axis.title.x = element_blank())

fig_7a<-grid.arrange(c1, c4, c2, c3, nrow = 2, ncol = 2)
ggsave(fig_7a, filename = "fig_7a.png",  width = 10, height = 7, units="in", dpi = 300)


# try to do boxplots by samply type differentiating by country first the most interesting ones
# starting with major ions

c5<-HF_LF %>%
  filter(`Sample type` %in% 
           c("streamwater")) %>%
  ggplot() +
  aes(x = Period, y = `Ca [mg/l]`, colour = Country) +
  geom_boxplot(fill = "#ffffff", show.legend = FALSE) +
  scale_color_manual(values = c("blue", "red" , "black","#00AFBB")) +
  labs(title = quote(Ca~mg~L^-1))+
  ylab(expression()) +
  ggthemes::theme_base() +
  theme(plot.title = element_text(color="blue", size=15, face="plain", vjust = 0, hjust = 0.50))+
  theme(axis.title = element_text(size=11))+
  theme(legend.title = element_text(size = 11), legend.position = c(0.85, 0.20))+
  theme(axis.title.x = element_blank())


c6<-HF_LF %>%
  filter(`Sample type` %in% 
           c("streamwater")) %>%
  ggplot() +
  aes(x = Period, y = `Mg [mg/L]`, colour = Country) +
  geom_boxplot(fill = "#ffffff", show.legend = FALSE) +
  scale_color_manual(values = c("blue", "red" , "black","#00AFBB")) +
  labs(title = quote(Mg~mg~L^-1)) +
  ylab(expression()) +
  ggthemes::theme_base() +
  theme(plot.title = element_text(color="blue", size=15, face="plain", vjust = 0, hjust = 0.50))+
  theme(axis.title = element_text(size=11))+
  theme(legend.title = element_text(size = 11), legend.position = c(0.85, 0.20))+
  theme(axis.title.x = element_blank())

# removing the outlier UA KZ-8, 160 mg Na 28 July 2020
c7<-HF_LF %>%
  filter(`Sample type` %in% 
           c("streamwater")) %>%
  ggplot() +
  aes(x = Period, y = `Na [mg/l]`, colour = Country) +
  geom_boxplot(fill = "#ffffff", show.legend = FALSE) +
  ylim(0,80)+
  scale_color_manual(values = c("blue", "red" , "black","#00AFBB")) +
  labs(title = quote(Na~mg~L^-1)) +
  ylab(expression()) +
  ggthemes::theme_base() +
  theme(plot.title = element_text(color="blue", size=15, face="plain", vjust = 0, hjust = 0.50))+
  theme(axis.title = element_text(size=11))+
  theme(legend.title = element_text(size = 11), legend.position = c(0.85, 0.20))+
  theme(axis.title.x = element_blank())

c8<-HF_LF %>%
  filter(`Sample type` %in% 
           c("streamwater")) %>%
  ggplot() +
  aes(x = Period, y = `K [mg/L]`, colour = Country) +
  geom_boxplot(fill = "#ffffff") +
  scale_color_manual(values = c("blue", "red" , "black","#00AFBB")) +
  labs(title = quote(K~mg~L^-1)) +
  ylab(expression()) +
  ggthemes::theme_base() +
  theme(plot.title = element_text(color="blue", size=15, face="plain", vjust = 0, hjust = 0.50))+
  theme(axis.title = element_text(size=11))+
  theme(legend.title = element_text(size = 14), legend.position = c(0.85, 0.70))+
  theme(axis.title.x = element_blank())

#manual export 1747x447 pixel
fig_7b<-grid.arrange(c5, c6, c7, c8, nrow = 2, ncol = 2)
ggsave(fig_7b, filename = "fig_7b.png",  width = 10, height = 7, units="in", dpi = 300)

# crustal elements

#  note 2 outliers in streamwater july 28-07-20, Kazcik	KZ-7, 350.77 microgr/L and 
# Baityk 05-08-19 89.16 mg/L filtered out

c9<-HF_LF %>%
  filter(`Sample type` %in% 
           c("streamwater")) %>%
  ggplot() +
  aes(x = Period, y = `Fe [ug/L]`, colour = Country) +
  geom_boxplot(fill = "#ffffff", show.legend = FALSE) +
  ylim(0, 40)+
  scale_color_manual(values = c("blue", "red" , "black","#00AFBB")) +
  labs(title = quote(Fe~ug~L^-1)) +
  ylab(expression()) +
  ggthemes::theme_base() +
  theme(plot.title = element_text(color="blue", size=15, face="plain", vjust = 0, hjust = 0.50))+
  theme(axis.title = element_text(size=11))+
  theme(legend.title = element_text(size = 14), legend.position = c(0.85, 0.70))+
  theme(axis.title.x = element_blank())

#  note 2 outliers in streamwater july 28-07-20, Kazcik	KZ-7, 306 microgr/L
# and Baitik July 2019 - 114 microgr/L

c10<-HF_LF %>%
  filter(`Sample type` %in% 
           c("streamwater")) %>%
  ggplot() +
  aes(x = Period, y = `Al [ug/L]`, colour = Country) +
  geom_boxplot(fill = "#ffffff",show.legend = FALSE) +
  ylim(0, 50)+
  scale_color_manual(values = c("blue", "red" , "black","#00AFBB")) +
  labs(title = quote(Al~ug~L^-1)) +
  ylab(expression()) +
  ggthemes::theme_base() +
  theme(plot.title = element_text(color="blue", size=15, face="plain", vjust = 0, hjust = 0.50))+
  theme(axis.title = element_text(size=11))+
  theme(legend.title = element_text(size = 14), legend.position = c(0.85, 0.70))+
  theme(axis.title.x = element_blank())

# one sample is filtered out 27.07.2020 - KA, KZ-2, 17.22 microgram
c11<-HF_LF %>%
  filter(`Sample type` %in% 
           c("streamwater")) %>%
  ggplot() +
  aes(x = Period, y = `Cr [ug/L]`, colour = Country, show.legend = FALSE) +
  geom_boxplot(fill = "#ffffff",show.legend = FALSE) +
  ylim(0, 6)+
  scale_color_manual(values = c("blue", "red" , "black","#00AFBB")) +
  labs(title = quote(Cr~ug~L^-1)) +
  ylab(expression()) +
  ggthemes::theme_base() +
  theme(plot.title = element_text(color="blue", size=15, face="plain", vjust = 0, hjust = 0.50))+
  theme(axis.title = element_text(size=11))+
  theme(legend.title = element_text(size = 14), legend.position = c(0.85, 0.70))+
  theme(axis.title.x = element_blank())

# filtered out Kazcik 17-05-2019  537 microgram and Chirchik 31-07-19 UZ-10 - 175 microgram
c12<-HF_LF %>%
  filter(`Sample type` %in% 
           c("streamwater")) %>%
  ggplot() +
  aes(x = Period, y = `Mn [ug/L]`, colour = Country) +
  geom_boxplot(fill = "#ffffff", show.legend = FALSE) +
  ylim(0, 80)+
  scale_color_manual(values = c("blue", "red" , "black","#00AFBB")) +
  labs(title = quote(Mn~ug~L^-1)) +
  ylab(expression()) +
  ggthemes::theme_base() +
  theme(plot.title = element_text(color="blue", size=15, face="plain", vjust = 0, hjust = 0.50))+
  theme(axis.title = element_text(size=11))+
  theme(legend.title = element_text(size = 14), legend.position = c(0.85, 0.70))+
  theme(axis.title.x = element_blank())

fig_7c<-grid.arrange(c9, c10, c11, c12, nrow = 2, ncol = 2)
ggsave(fig_7c, filename = "fig_7c.png",  width = 10, height = 7, units="in", dpi = 300)

# now potentially toxic elements only stream water

c13<-HF_LF %>%
  filter(`Sample type` %in% 
           c("streamwater")) %>%
  ggplot() +
  aes(x = Period, y = `As [ug/L]`, colour = Country) +
  geom_boxplot(fill = "#ffffff", show.legend = FALSE) +
  scale_color_manual(values = c("blue", "red" , "black","#00AFBB")) +
  labs(title = quote(As~ug~L^-1)) +
  ylab(expression()) +
  ggthemes::theme_base() +
  theme(plot.title = element_text(color="blue", size=15, face="plain", vjust = 0, hjust = 0.50))+
  theme(axis.title = element_text(size=11))+
  theme(legend.title = element_text(size = 14), legend.position = c(0.85, 0.70))+
  theme(axis.title.x = element_blank())

c14<-HF_LF %>%
  filter(`Sample type` %in% 
           c("streamwater")) %>%
  ggplot() +
  aes(x = Period, y = `Pb [ug/L]`, colour = Country) +
  geom_boxplot(fill = "#ffffff", show.legend = FALSE) +
  scale_color_manual(values = c("blue", "red" , "black","#00AFBB")) +
  labs(title = quote(Pb~ug~L^-1)) +
  ylab(expression()) +
  ggthemes::theme_base() +
  theme(plot.title = element_text(color="blue", size=15, face="plain", vjust = 0, hjust = 0.50))+
  theme(axis.title = element_text(size=11))+
  theme(legend.title = element_text(size = 14), legend.position = c(0.85, 0.70))+
  theme(axis.title.x = element_blank())

c15<-HF_LF %>%
  filter(`Sample type` %in% 
           c("streamwater", "groundwater")) %>%
  ggplot() +
  aes(x = Period, y = `Cd [ug/L]`, colour = Country) +
  geom_boxplot(fill = "#ffffff", show.legend = FALSE, width=0.7) +
  geom_hline(yintercept = 3, color="red", size= 0.5, linetype = 2)+
  scale_color_manual(values = c("blue", "red" , "black","#00AFBB")) +
  ylab(expression()) +
  labs(title = quote(Cd~ug~L^-1)) +
  ggthemes::theme_base() +
  theme(plot.title = element_text(color="blue", size=15, face="plain", vjust = 0, hjust = 0.50))+
  theme(axis.title = element_text(size=11))+
  theme(legend.title = element_text(size = 14), legend.position = c(0.85, 0.70))+
  theme(axis.title.x = element_blank())

c16<-HF_LF %>%
  filter(`Sample type` %in% 
           c("streamwater", "groundwater")) %>%
  ggplot() +
  aes(x = Period, y = `Ni [ug/L]`, colour = Country) +
  geom_boxplot(fill = "#ffffff", show.legend = TRUE) +
  scale_color_manual(values = c("blue", "red" , "black","#00AFBB")) +
  ylab(expression()) +
  labs(title = quote(Ni~ug~L^-1)) +
  ggthemes::theme_base() +
  theme(plot.title = element_text(color="blue", size=15, face="plain", vjust = 0, hjust = 0.50))+
  theme(axis.title = element_text(size=11))+
  theme(legend.title = element_text(size = 14), legend.position = c(0.85, 0.70))+
  theme(axis.title.x = element_blank())

fig_7d<-grid.arrange(c13, c14, c15, c16, nrow = 2, ncol = 2) 
ggsave(fig_7d, filename = "fig_7d.png",  width = 10, height = 7, units="in", dpi = 300)
  
 # and the lastly Cu and Zn


  c17<-HF_LF %>%
      filter(`Sample type` %in% 
               c("streamwater")) %>%
      ggplot() +
      aes(x = Period, y = `Cu [ug/L]`, colour = Country) +
      geom_boxplot(fill = "#ffffff", show.legend = FALSE) +
      ylab(expression()) +
      scale_color_manual(values = c("blue", "red" , "black","#00AFBB")) +
      labs(title = quote(Cu~ug~L^-1)) +
      ggthemes::theme_base() +
    theme(plot.title = element_text(color="blue", size=15, face="plain", vjust = 0, hjust = 0.50))+
    theme(axis.title = element_text(size=11))+
    theme(legend.title = element_text(size = 14), legend.position = c(0.85, 0.70))+
    theme(axis.title.x = element_blank())
   
  # please note three outliers are removed all in KZ KA, - KZ-1,2 and UA - KZ-5 the first president park 
  # 193.93, 129.31, 180.63 microgram # - all May and July 2020 
    
    c18<-HF_LF %>%
      filter(`Sample type` %in% 
               c("streamwater")) %>%
      ggplot() +
      aes(x = Period, y = `Zn [ug/L]`, colour = Country) +
      geom_boxplot(fill = "#ffffff",show.legend = FALSE) +
      ylim(0, 100)+
      ylab(expression()) +
      scale_color_manual(values = c("blue", "red" , "black","#00AFBB")) +
      labs(title = Zn~ug~L^-1) +
      ggthemes::theme_base() +
      theme(plot.title = element_text(color="blue", size=15, face="plain", vjust = 0, hjust = 0.50))+
      theme(axis.title = element_text(size=11))+
      theme(legend.title = element_text(size = 14), legend.position = c(0.85, 0.70))+
      theme(axis.title.x = element_blank())
     
     fig_7e<-grid.arrange(c17, c18, nrow = 1, ncol = 2)   
     ggsave(fig_7e, filename = "fig_7e.png",  width = 10, height = 3.5, units="in", dpi = 300)
     # the end    

# Anova test for stat significance between groups? 
     

     