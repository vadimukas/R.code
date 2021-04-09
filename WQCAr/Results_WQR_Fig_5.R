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

WQ_samples_2019_2020 <- read_csv("WQdata__2019-2020.csv", 
                  col_types = cols(month = col_factor(levels = c("1","3","5","6","7", "8","9","10", "11")),
                  `Sample type` = col_factor(levels = c("glaciermelt", "snowpack","streamwater", "groundwater","reservoir", "tapwater" )),
                  Country = col_factor(levels = c("KZ","KG", "UZ", "TJ"))), na = "NA")

head(WQ_samples_2019_2020)

# first start with major anions

s1<-WQ_samples_2019_2020 %>%
  filter(`Sample type` %in% 
           c("streamwater", "groundwater", "glaciermelt", "snowpack")) %>%
  ggplot() +
  aes(x = month, y = `HCO3 [mg/L]`) +
  geom_boxplot(fill = "#ffffff") +
  ylab(expression()) +
  geom_point(shape=16)+
  labs(title = expression(HCO[3]^`-` ~ mg ~ L^-1)) +
  ggthemes::theme_base() +
  theme(strip.text.x = element_text(size = 14))+
  theme(plot.title = element_text(color="Blue", size=15, face="plain", vjust = -8, hjust = 0.53))+
  theme(axis.title = element_text(size=12))+
  facet_wrap(vars(`Sample type`))

s2<-WQ_samples_2019_2020 %>%
  filter(`Sample type` %in% 
           c("streamwater", "groundwater", "glaciermelt", "snowpack")) %>%
  ggplot() +
  aes(x = month, y = `Cl [mg/L]`) +
  geom_boxplot(fill = "#ffffff") +
  ylab(expression()) +
  geom_point(shape=16)+
  labs(title = quote(Cl^'-' ~ mg~L^-1)) +
  ggthemes::theme_base() +
  theme(strip.text.x = element_text(size = 14))+
  theme(plot.title = element_text(color="Blue", size=15, face="plain", vjust = -8, hjust = 0.53))+
  theme(axis.title = element_text(size=12))+
  facet_wrap(vars(`Sample type`))

# filtering KZ-8 July 2020 (Ulken Almaty) 504.5 mg/L by limiting the scale to 250 mg\L
s3<-WQ_samples_2019_2020 %>%
  filter(`Sample type` %in% 
           c("streamwater", "groundwater", "glaciermelt", "snowpack")) %>%
  ggplot() +
  aes(x = month, y = `SO4[mg/L]`) +
  geom_boxplot(fill = "#ffffff") +
  ylab(expression()) +
  ylim(0, 250)+
  geom_point(shape=16)+
  labs(title = expression(SO[4]^'2-'~mg~L^-1)) +
  ggthemes::theme_base() +
  theme(strip.text.x = element_text(size = 14))+
  theme(plot.title = element_text(color="Blue", size=15, face="plain", vjust = -8, hjust = 0.53))+
  theme(axis.title = element_text(size=12))+
  facet_wrap(vars(`Sample type`))

s4<-WQ_samples_2019_2020 %>%
  filter(`Sample type` %in% 
           c("streamwater", "groundwater", "glaciermelt", "snowpack")) %>%
  ggplot() +
  aes(x = month, y = `Si [mg/L]`) +
  geom_boxplot(fill = "#ffffff") +
  ylab(expression()) +
  geom_point(shape=16)+
  labs(title = quote(Si ~ mg~L^-1)) +
  ggthemes::theme_base() +
  theme(strip.text.x = element_text(size = 14))+
  theme(plot.title = element_text(color="Blue", size=15, face="plain", vjust = -8, hjust = 0.53))+
  theme(axis.title = element_text(size=12))+
  facet_wrap(vars(`Sample type`))

fig_5a<-grid.arrange(s1, s4, s2, s3, nrow = 2, ncol = 2)
ggsave(fig_5a, filename = "fig_5a.png",  width = 10, height = 7, units="in", dpi = 300)

s5<-WQ_samples_2019_2020 %>%
  filter(`Sample type` %in% 
           c("streamwater", "groundwater", "glaciermelt", "snowpack")) %>%
  ggplot() +
  aes(x = month, y = `Ca [mg/l]`) +
  geom_boxplot(fill = "#ffffff") +
  ylab(expression()) +
  geom_point(shape=16)+
  labs(title = quote(Ca~mg~L^-1)) +
  ggthemes::theme_base() +
  theme(strip.text.x = element_text(size = 14))+
  theme(plot.title = element_text(color="Blue", size=15, face="plain", vjust = -8, hjust = 0.53))+
  theme(axis.title = element_text(size=12))+
  facet_wrap(vars(`Sample type`))

s6<-WQ_samples_2019_2020 %>%
  filter(`Sample type` %in% 
           c("streamwater", "groundwater", "glaciermelt", "snowpack")) %>%
  ggplot() +
  aes(x = month, y = `Mg [mg/L]`) +
  ylab(expression()) +
  geom_boxplot(fill = "#ffffff") +
  geom_point(shape=16)+
  labs(title = quote(Mg~mg~L^-1)) +
  ggthemes::theme_base() +
  theme(strip.text.x = element_text(size = 14))+
  theme(plot.title = element_text(color="Blue", size=15, face="plain", vjust = -8, hjust = 0.53))+
  theme(axis.title = element_text(size=12))+
  facet_wrap(vars(`Sample type`))

# removing the outlier UA KZ-8, 160 mg Na 28 July 2020
s7<-WQ_samples_2019_2020 %>%
  filter(`Sample type` %in% 
           c("streamwater", "groundwater", "glaciermelt", "snowpack")) %>%
  ggplot() +
  aes(x = month, y = `Na [mg/l]`) +
  ylim(0, 80)+
  geom_boxplot(fill = "#ffffff") +
  ylab(expression()) +
  geom_point(shape=16)+
  labs(title = quote(Na~mg~L^-1)) +
  ggthemes::theme_base() +
  theme(strip.text.x = element_text(size = 14))+
  theme(plot.title = element_text(color="Blue", size=15, face="plain", vjust = -8, hjust = 0.53))+
  theme(axis.title = element_text(size=12))+
  facet_wrap(vars(`Sample type`))

s8<-WQ_samples_2019_2020 %>%
  filter(`Sample type` %in% 
           c("streamwater", "groundwater","glaciermelt", "snowpack")) %>%
  ggplot() +
  aes(x = month, y = `K [mg/L]`) +
  geom_boxplot(fill = "#ffffff") +
  ylab(expression()) +
  geom_point(shape=16)+
  labs(title = quote(K~mg~L^-1)) +
  ggthemes::theme_base() +
  theme(strip.text.x = element_text(size = 14))+
  theme(plot.title = element_text(color="Blue", size=15, face="plain", vjust = -8, hjust = 0.53))+
  theme(axis.title = element_text(size=12))+
  facet_wrap(vars(`Sample type`))

fig_5b<-grid.arrange(s5, s6, s7, s8, nrow = 2, ncol = 2)
ggsave(fig_5b, filename = "fig_5b.png",  width = 10, height = 7, units="in", dpi = 300)

# then crustal elements

#  note the outlier in streamwater july 28-07-20, Kazcik	KZ-7, 350.77 mg/L filtered out

s9<-WQ_samples_2019_2020 %>%
  filter(`Sample type` %in% 
           c("streamwater", "groundwater", "glaciermelt", "snowpack")) %>%
  ggplot() +
  aes(x = month, y = `Fe [ug/L]`) +
  geom_boxplot(fill = "#ffffff") +
  ylim(0, 200)+
  ylab(expression()) +
  geom_point(shape=16)+
  labs(title = quote(Fe~ug~L^-1)) +
  ggthemes::theme_base() +
  theme(strip.text.x = element_text(size = 14))+
  theme(plot.title = element_text(color="Blue", size=15, face="plain", vjust = -8, hjust = 0.53))+
  theme(axis.title = element_text(size=12))+
  facet_wrap(vars(`Sample type`))

#  note filtered out 2 outliers in streamwater july 28-07-20, Kazcik	KZ-7, 306 microgr/L
# and BAO spring July 2020 - 265 microgr/L. 

s10<-WQ_samples_2019_2020 %>%
  filter(`Sample type` %in% 
           c("streamwater", "groundwater", "glaciermelt", "snowpack")) %>%
  ggplot() +
  aes(x = month, y =`Al [ug/L]`) +
  geom_boxplot(fill = "#ffffff") +
  ylim(0, 150)+
  ylab(expression()) +
  geom_point(shape=16)+
  labs(title = quote(Al~ug~L^-1)) +
  ggthemes::theme_base() +
  theme(strip.text.x = element_text(size = 14))+
  theme(plot.title = element_text(color="Blue", size=15, face="plain", vjust = -8, hjust = 0.53))+
  theme(axis.title = element_text(size=12))+
  facet_wrap(vars(`Sample type`))

# one sample is filtered out 27.07.2020 - KA, KZ-2, 17.22 microgram
s11<-WQ_samples_2019_2020 %>%
  filter(`Sample type` %in% 
           c("streamwater", "groundwater", "glaciermelt", "snowpack")) %>%
  ggplot() +
  aes(x = month, y =`Cr [ug/L]`) +
  geom_boxplot(fill = "#ffffff") +
  ylab(expression()) +
  ylim(0, 8)+
  geom_point(shape=16) +
  labs(title = quote(Cr~ug~L^-1)) +
  ggthemes::theme_base() +
  theme(strip.text.x = element_text(size = 14))+
  theme(plot.title = element_text(color="Blue", size=15, face="plain", vjust = -8, hjust = 0.53))+
  theme(axis.title = element_text(size=12))+
  facet_wrap(vars(`Sample type`))

# filtered out Kazcik 17-05-2019  537 microgram and Chirchik 31-07-19 UZ-10 - 175 microgram
s12<-WQ_samples_2019_2020 %>%
  filter(`Sample type` %in% 
           c("streamwater", "groundwater", "glaciermelt", "snowpack")) %>%
  ggplot() +
  aes(x = month, y = `Mn [ug/L]`) +
  geom_boxplot(fill = "#ffffff") +
  ylim(0, 100)+
  ylab(expression()) +
  geom_point(shape=16)+
  labs(title = quote(Mn~ug~L^-1)) +
  ggthemes::theme_base() +
  theme(strip.text.x = element_text(size = 14))+
  theme(plot.title = element_text(color="Blue", size=15, face="plain", vjust = -8, hjust = 0.53))+
  theme(axis.title = element_text(size=12))+
  facet_wrap(vars(`Sample type`))

fig_5c<-grid.arrange(s9, s10, s11, s12, nrow = 2, ncol = 2)
ggsave(fig_5c, filename = "fig_5c.png",  width = 10, height = 7, units="in", dpi = 300)

# finally PET

s13<-WQ_samples_2019_2020 %>%
  filter(`Sample type` %in% 
           c("streamwater", "groundwater", "glaciermelt", "snowpack")) %>%
  ggplot() +
  aes(x = month, y = `As [ug/L]`) +
  ylab(expression()) +
  geom_boxplot(fill = "#ffffff") +
  geom_hline(yintercept = 10, color="red", size= 0.5)+
  geom_point(shape=16)+
  labs(title = quote(As~ug~L^-1)) +
  ggthemes::theme_base() +
  theme(strip.text.x = element_text(size = 14))+
  theme(plot.title = element_text(color="Blue", size=15, face="plain", vjust = -8, hjust = 0.53))+
  theme(axis.title = element_text(size=12))+
  facet_wrap(vars(`Sample type`))

s14<-WQ_samples_2019_2020 %>%
  filter(`Sample type` %in% 
           c("streamwater", "groundwater", "glaciermelt", "snowpack")) %>%
  ggplot() +
  aes(x = month, y = `Pb [ug/L]`) +
  geom_boxplot(fill = "#ffffff") +
  ylab(expression()) +
  geom_point(shape=16)+
  labs(title = quote(Pb~ug~L^-1)) +
  ggthemes::theme_base() +
  theme(strip.text.x = element_text(size = 14))+
  theme(plot.title = element_text(color="Blue", size=15, face="plain", vjust = -8, hjust = 0.53))+
  theme(axis.title = element_text(size=12))+
  facet_wrap(vars(`Sample type`))

s15<-WQ_samples_2019_2020 %>%
  filter(`Sample type` %in% 
           c("streamwater", "groundwater", "glaciermelt", "snowpack")) %>%
  ggplot() +
  aes(x = month, y = `Cd [ug/L]`) +
  geom_boxplot(fill = "#ffffff") +
  geom_hline(yintercept = 3, color="red", size= 0.5)+
  ylab(expression()) +
  geom_point(shape=16)+
  labs(title = quote(Cd~ug~L^-1)) +
  ggthemes::theme_base() +
  theme(strip.text.x = element_text(size = 14))+
  theme(plot.title = element_text(color="Blue", size=15, face="plain", vjust = -8, hjust = 0.53))+
  theme(axis.title = element_text(size=12))+
  facet_wrap(vars(`Sample type`))

# note one outlier 320 microgram is removed -Spring K-2 28 July 2020
s16<-WQ_samples_2019_2020 %>%
  filter(`Sample type` %in% 
           c("streamwater", "groundwater", "glaciermelt", "snowpack")) %>%
  ggplot() +
  aes(x = month, y = `Ni [ug/L]`) +
  geom_boxplot(fill = "#ffffff") +
  ylab(expression()) +
  ylim(0, 20) +
  geom_point(shape=16)+
  labs(title = quote(Ni~ug~L^-1)) +
  ggthemes::theme_base() +
  theme(strip.text.x = element_text(size = 14))+
  theme(plot.title = element_text(color="Blue", size=15, face="plain", vjust = -8, hjust = 0.53))+
  theme(axis.title = element_text(size=12))+
  facet_wrap(vars(`Sample type`))

fig_5d<-grid.arrange(s13, s14, s15, s16, nrow = 2, ncol = 2)
ggsave(fig_5d, filename = "fig_5d.png",  width = 10, height = 7, units="in", dpi = 300)

# outliers: 27.5 - snowpit KZ 2019, spring Ala-Archa July 2020 - 20.72
s17<-WQ_samples_2019_2020 %>%
  filter(`Sample type` %in% 
           c("streamwater", "groundwater", "glaciermelt", "snowpack")) %>%
  ggplot() +
  aes(x = month, y = `Cu [ug/L]`) +
  geom_boxplot(fill = "#ffffff") +
  ylab(expression()) +
  ylim(0, 15) +
  geom_point(shape=16)+
  labs(title = quote(Cu~ug~L^-1)) +
  ggthemes::theme_base() +
  theme(strip.text.x = element_text(size = 14))+
  theme(plot.title = element_text(color="Blue", size=15, face="plain", vjust = -8, hjust = 0.53))+
  theme(axis.title = element_text(size=12))+
  facet_wrap(vars(`Sample type`))

# please note three outliers are removed all in KZ KA, - KZ-1,2 and UA - KZ-5 the first president park 
# 193.93, 129.31, 180.63 microgram # - all May and July 2020

s18<-WQ_samples_2019_2020 %>%
  filter(`Sample type` %in% 
           c("streamwater", "groundwater", "glaciermelt", "snowpack")) %>%
  ggplot() +
  aes(x = month, y = `Zn [ug/L]`) +
  geom_boxplot(fill = "#ffffff") +
  ylim(0, 100)+
  ylab(expression()) +
  geom_point(shape=16)+
  labs(title = quote(Zn~ug~L^-1)) +
  ggthemes::theme_base() +
  theme(strip.text.x = element_text(size = 14))+
  theme(plot.title = element_text(color="Blue", size=15, face="plain", vjust = -8, hjust = 0.53))+
  theme(axis.title = element_text(size=12))+
  facet_wrap(vars(`Sample type`))

grid.arrange(s17 , s18, nrow = 1, ncol = 2)
fig_5e<-grid.arrange(s17, s18, nrow = 1, ncol = 2)
ggsave(fig_5e, filename = "fig_5e.png",  width = 10, height = 3.5, units="in", dpi = 300)

# making new figure 5 mapping to catchments-countries
# first start with major anions 

l1<-WQ_samples_2019_2020 %>%
  filter(`Sample type` %in% 
           c("streamwater", "groundwater", "glaciermelt", "snowpack")) %>%
  ggplot() +
  aes(x = Country, y = `HCO3 [mg/L]`,  colour = Country) +
  geom_boxplot(show.legend = FALSE)+
  scale_color_manual(values = c("blue", "red" , "black","#008B8B")) +
  ylab(expression()) +
  geom_point(shape=16, show.legend = FALSE)+
  labs(title = expression(HCO[3]^`-` ~ mg ~ L^-1)) +
  ggthemes::theme_base() +
  theme(strip.text.x = element_text(size = 14))+
  theme(plot.title = element_text(color="Blue", size=15, face="plain", vjust = -8, hjust = 0.53))+
  theme(axis.title = element_text(size=12))+
  facet_wrap(vars(`Sample type`))

l2<-WQ_samples_2019_2020 %>%
  filter(`Sample type` %in% 
           c("streamwater", "groundwater", "glaciermelt", "snowpack")) %>%
  ggplot() +
  aes(x = Country, y = `Cl [mg/L]`, color = Country) +
  geom_boxplot(show.legend = FALSE) +
  scale_color_manual(values = c("blue", "red" , "black","#008B8B")) +
  ylab(expression()) +
  geom_point(shape=16, show.legend = FALSE)+
  labs(title = quote(Cl^'-' ~ mg~L^-1)) +
  ggthemes::theme_base() +
  theme(strip.text.x = element_text(size = 14))+
  theme(plot.title = element_text(color="Blue", size=15, face="plain", vjust = -8, hjust = 0.53))+
  theme(axis.title = element_text(size=12))+
  facet_wrap(vars(`Sample type`))

# filtering KZ-8 July 2020 (Ulken Almaty) 504.5 mg/L by limiting the scale to 250 mg\L
l3<-WQ_samples_2019_2020 %>%
  filter(`Sample type` %in% 
           c("streamwater", "groundwater", "glaciermelt", "snowpack")) %>%
  ggplot() +
  aes(x = Country, y = `SO4[mg/L]`, color = Country) +
  geom_boxplot(show.legend = FALSE) +
  scale_color_manual(values = c("blue", "red" , "black","#008B8B")) +
  ylab(expression()) +
  ylim(0, 250)+
  geom_point(shape=16, show.legend = FALSE)+
  labs(title = expression(SO[4]^'2-'~mg~L^-1)) +
  ggthemes::theme_base() +
  theme(strip.text.x = element_text(size = 14))+
  theme(plot.title = element_text(color="blue", size=15, face="plain", vjust = -8, hjust = 0.53))+
  theme(axis.title = element_text(size=12))+
  facet_wrap(vars(`Sample type`))

l4<-WQ_samples_2019_2020 %>%
  filter(`Sample type` %in% 
           c("streamwater", "groundwater", "glaciermelt", "snowpack")) %>%
  ggplot() +
  aes(x = Country, y = `Si [mg/L]`, color = Country) +
  geom_boxplot(show.legend = FALSE) +
  scale_color_manual(values = c("blue", "red" , "black","#008B8B")) +
  ylab(expression()) +
  geom_point(shape=16, show.legend = FALSE)+
  labs(title = quote(Si ~ mg~L^-1)) +
  ggthemes::theme_base() +
  theme(strip.text.x = element_text(size = 14))+
  theme(plot.title = element_text(color="Blue", size=15, face="plain", vjust = -8, hjust = 0.53))+
  theme(axis.title = element_text(size=12))+
  facet_wrap(vars(`Sample type`))

fig_5a<-grid.arrange(l1, l4, l2, l3, nrow = 2, ncol = 2)
ggsave(fig_5a, filename = "fig_5a.png",  width = 10, height = 7, units="in", dpi = 300)


l5<-WQ_samples_2019_2020 %>%
  filter(`Sample type` %in% 
           c("streamwater", "groundwater", "glaciermelt", "snowpack")) %>%
  ggplot() +
  aes(x = Country, y = `Ca [mg/l]`, color = Country) +
  geom_boxplot(show.legend = FALSE) +
  scale_color_manual(values = c("blue", "red" , "black","#008B8B")) +
  ylab(expression()) +
  geom_point(shape=16, show.legend = FALSE)+
  labs(title = quote(Ca~mg~L^-1)) +
  ggthemes::theme_base() +
  theme(strip.text.x = element_text(size = 14))+
  theme(plot.title = element_text(color="Blue", size=15, face="plain", vjust = -8, hjust = 0.53))+
  theme(axis.title = element_text(size=12))+
  facet_wrap(vars(`Sample type`))

l6<-WQ_samples_2019_2020 %>%
  filter(`Sample type` %in% 
           c("streamwater", "groundwater", "glaciermelt", "snowpack")) %>%
  ggplot() +
  aes(x = Country, y = `Mg [mg/L]`, color = Country) +
  ylab(expression()) +
  geom_boxplot(show.legend = FALSE) +
  scale_color_manual(values = c("blue", "red" , "black","#008B8B")) +
  geom_point(shape=16, show.legend = FALSE)+
  labs(title = quote(Mg~mg~L^-1)) +
  ggthemes::theme_base() +
  theme(strip.text.x = element_text(size = 14))+
  theme(plot.title = element_text(color="Blue", size=15, face="plain", vjust = -8, hjust = 0.53))+
  theme(axis.title = element_text(size=12))+
  facet_wrap(vars(`Sample type`))

# removing the outlier UA KZ-8, 160 mg Na 28 July 2020
l7<-WQ_samples_2019_2020 %>%
  filter(`Sample type` %in% 
           c("streamwater", "groundwater", "glaciermelt", "snowpack")) %>%
  ggplot() +
  aes(x = Country, y = `Na [mg/l]`, color = Country) +
  ylim(0, 80)+
  geom_boxplot(show.legend = FALSE) +
  scale_color_manual(values = c("blue", "red" , "black","#008B8B")) +
  ylab(expression()) +
  geom_point(shape=16, show.legend = FALSE)+
  labs(title = quote(Na~mg~L^-1)) +
  ggthemes::theme_base() +
  theme(strip.text.x = element_text(size = 14))+
  theme(plot.title = element_text(color="Blue", size=15, face="plain", vjust = -8, hjust = 0.53))+
  theme(axis.title = element_text(size=12))+
  facet_wrap(vars(`Sample type`))

l8<-WQ_samples_2019_2020 %>%
  filter(`Sample type` %in% 
           c("streamwater", "groundwater","glaciermelt", "snowpack")) %>%
  ggplot() +
  aes(x = Country, y = `K [mg/L]`, color = Country) +
  geom_boxplot(show.legend = FALSE) +
  scale_color_manual(values = c("blue", "red" , "black","#008B8B")) +
  ylab(expression()) +
  geom_point(shape=16, show.legend = FALSE)+
  labs(title = quote(K~mg~L^-1)) +
  ggthemes::theme_base() +
  theme(strip.text.x = element_text(size = 14))+
  theme(plot.title = element_text(color="Blue", size=15, face="plain", vjust = -8, hjust = 0.53))+
  theme(axis.title = element_text(size=12))+
  facet_wrap(vars(`Sample type`))

fig_5b<-grid.arrange(l5, l6, l7, l8, nrow = 2, ncol = 2)
ggsave(fig_5b, filename = "fig_5b.png",  width = 10, height = 7, units="in", dpi = 300)

# then crustal elements

#  note the outlier in streamwater july 28-07-20, Kazcik	KZ-7, 350.77 mg/L filtered out

l9<-WQ_samples_2019_2020 %>%
  filter(`Sample type` %in% 
           c("streamwater", "groundwater", "glaciermelt", "snowpack")) %>%
  ggplot() +
  aes(x = Country, y = `Fe [ug/L]`, color = Country) +
  geom_boxplot(show.legend = FALSE) +
  scale_color_manual(values = c("blue", "red" , "black","#008B8B")) +
  ylim(0, 200)+
  ylab(expression()) +
  geom_point(shape=16, show.legend = FALSE)+
  labs(title = quote(Fe~ug~L^-1)) +
  ggthemes::theme_base() +
  theme(strip.text.x = element_text(size = 14))+
  theme(plot.title = element_text(color="Blue", size=15, face="plain", vjust = -8, hjust = 0.53))+
  theme(axis.title = element_text(size=12))+
  facet_wrap(vars(`Sample type`))

#  note filtered out 2 outliers in streamwater july 28-07-20, Kazcik	KZ-7, 306 microgr/L
# and BAO spring July 2020 - 265 microgr/L. 

l10<-WQ_samples_2019_2020 %>%
  filter(`Sample type` %in% 
           c("streamwater", "groundwater", "glaciermelt", "snowpack")) %>%
  ggplot() +
  aes(x = Country, y =`Al [ug/L]`, color = Country) +
  geom_boxplot(show.legend = FALSE) +
  scale_color_manual(values = c("blue", "red" , "black","#008B8B")) +
  ylim(0, 150)+
  ylab(expression()) +
  geom_point(shape=16, show.legend = FALSE)+
  labs(title = quote(Al~ug~L^-1)) +
  ggthemes::theme_base() +
  theme(strip.text.x = element_text(size = 14))+
  theme(plot.title = element_text(color="Blue", size=15, face="plain", vjust = -8, hjust = 0.53))+
  theme(axis.title = element_text(size=12))+
  facet_wrap(vars(`Sample type`))

# one sample is filtered out 27.07.2020 - KA, KZ-2, 17.22 microgram
l11<-WQ_samples_2019_2020 %>%
  filter(`Sample type` %in% 
           c("streamwater", "groundwater", "glaciermelt", "snowpack")) %>%
  ggplot() +
  aes(x = Country, y =`Cr [ug/L]`, color = Country) +
  geom_boxplot(show.legend = FALSE) +
  scale_color_manual(values = c("blue", "red" , "black","#008B8B")) +
  ylab(expression()) +
  ylim(0, 8)+
  geom_point(shape=16, show.legend = FALSE) +
  labs(title = quote(Cr~ug~L^-1)) +
  ggthemes::theme_base() +
  theme(strip.text.x = element_text(size = 14))+
  theme(plot.title = element_text(color="Blue", size=15, face="plain", vjust = -8, hjust = 0.53))+
  theme(axis.title = element_text(size=12))+
  facet_wrap(vars(`Sample type`))

# filtered out Kazcik 17-05-2019  537 microgram and Chirchik 31-07-19 UZ-10 - 175 microgram
l12<-WQ_samples_2019_2020 %>%
  filter(`Sample type` %in% 
           c("streamwater", "groundwater", "glaciermelt", "snowpack")) %>%
  ggplot() +
  aes(x = Country, y = `Mn [ug/L]`, color = Country) +
  geom_boxplot(show.legend = FALSE) +
  scale_color_manual(values = c("blue", "red" , "black","#008B8B")) +
  ylim(0, 80)+
  ylab(expression()) +
  geom_point(shape=16, show.legend = FALSE)+
  labs(title = quote(Mn~ug~L^-1)) +
  ggthemes::theme_base() +
  theme(strip.text.x = element_text(size = 14))+
  theme(plot.title = element_text(color="Blue", size=15, face="plain", vjust = -8, hjust = 0.53))+
  theme(axis.title = element_text(size=12))+
  facet_wrap(vars(`Sample type`))

fig_5c<-grid.arrange(l9, l10, l11, l12, nrow = 2, ncol = 2)
ggsave(fig_5c, filename = "fig_5c.png",  width = 10, height = 7, units="in", dpi = 300)

# finally PET

l13<-WQ_samples_2019_2020 %>%
  filter(`Sample type` %in% 
           c("streamwater", "groundwater", "glaciermelt", "snowpack")) %>%
  ggplot() +
  aes(x = Country, y = `As [ug/L]`, color = Country) +
  ylab(expression()) +
  geom_boxplot(show.legend = FALSE) +
  scale_color_manual(values = c("blue", "red" , "black","#008B8B")) +
  geom_hline(yintercept = 10, color="red", size= 0.5)+
  geom_point(shape=16, show.legend = FALSE)+
  labs(title = quote(As~ug~L^-1)) +
  ggthemes::theme_base() +
  theme(strip.text.x = element_text(size = 14))+
  theme(plot.title = element_text(color="Blue", size=15, face="plain", vjust = -8, hjust = 0.53))+
  theme(axis.title = element_text(size=12))+
  facet_wrap(vars(`Sample type`))

l14<-WQ_samples_2019_2020 %>%
  filter(`Sample type` %in% 
           c("streamwater", "groundwater", "glaciermelt", "snowpack")) %>%
  ggplot() +
  aes(x = Country, y = `Pb [ug/L]`, color = Country) +
  geom_boxplot(show.legend = FALSE) +
  scale_color_manual(values = c("blue", "red" , "black","#008B8B")) +
  ylab(expression()) +
  geom_point(shape=16, show.legend = FALSE)+
  labs(title = quote(Pb~ug~L^-1)) +
  ggthemes::theme_base() +
  theme(strip.text.x = element_text(size = 14))+
  theme(plot.title = element_text(color="Blue", size=15, face="plain", vjust = -8, hjust = 0.53))+
  theme(axis.title = element_text(size=12))+
  facet_wrap(vars(`Sample type`))

l15<-WQ_samples_2019_2020 %>%
  filter(`Sample type` %in% 
           c("streamwater", "groundwater", "glaciermelt", "snowpack")) %>%
  ggplot() +
  aes(x = Country, y = `Cd [ug/L]`, color = Country) +
  geom_boxplot(show.legend = FALSE) +
  scale_color_manual(values = c("blue", "red" , "black","#008B8B")) +
  geom_hline(yintercept = 3, color="red", size= 0.5)+
  ylab(expression()) +
  geom_point(shape=16, show.legend = FALSE)+
  labs(title = quote(Cd~ug~L^-1)) +
  ggthemes::theme_base() +
  theme(strip.text.x = element_text(size = 14))+
  theme(plot.title = element_text(color="Blue", size=15, face="plain", vjust = -8, hjust = 0.53))+
  theme(axis.title = element_text(size=12))+
  facet_wrap(vars(`Sample type`))

# note one outlier 320 microgram is removed -Spring K-2 28 July 2020
l16<-WQ_samples_2019_2020 %>%
  filter(`Sample type` %in% 
           c("streamwater", "groundwater", "glaciermelt", "snowpack")) %>%
  ggplot() +
  aes(x = Country, y = `Ni [ug/L]`, color = Country) +
  geom_boxplot(show.legend = FALSE) +
  scale_color_manual(values = c("blue", "red" , "black","#008B8B")) +
  ylab(expression()) +
  ylim(0, 20) +
  geom_point(shape=16, show.legend = FALSE)+
  labs(title = quote(Ni~ug~L^-1)) +
  ggthemes::theme_base() +
  theme(strip.text.x = element_text(size = 14))+
  theme(plot.title = element_text(color="Blue", size=15, face="plain", vjust = -8, hjust = 0.53))+
  theme(axis.title = element_text(size=12))+
  facet_wrap(vars(`Sample type`))

fig_5d<-grid.arrange(l13, l14, l15, l16, nrow = 2, ncol = 2)
ggsave(fig_5d, filename = "fig_5d.png",  width = 10, height = 7, units="in", dpi = 300)

# outliers: 27.5 - snowpit KZ 2019, spring Ala-Archa July 2020 - 20.72
l17<-WQ_samples_2019_2020 %>%
  filter(`Sample type` %in% 
           c("streamwater", "groundwater", "glaciermelt", "snowpack")) %>%
  ggplot() +
  aes(x = Country, y = `Cu [ug/L]`, color = Country) +
  geom_boxplot(show.legend = FALSE) +
  scale_color_manual(values = c("blue", "red" , "black","#008B8B")) +
  ylab(expression()) +
  ylim(0, 30) +
  geom_point(shape=16, show.legend = FALSE)+
  labs(title = quote(Cu~ug~L^-1)) +
  ggthemes::theme_base() +
  theme(strip.text.x = element_text(size = 14))+
  theme(plot.title = element_text(color="Blue", size=15, face="plain", vjust = -8, hjust = 0.53))+
  theme(axis.title = element_text(size=12))+
  facet_wrap(vars(`Sample type`))


# please note three outliers are removed all in KZ KA, - KZ-1,2 and UA - KZ-5 the first president park 
# 193.93, 129.31, 180.63 microgram # - all May and July 2020

l18<-WQ_samples_2019_2020 %>%
  filter(`Sample type` %in% 
           c("streamwater", "groundwater", "glaciermelt", "snowpack")) %>%
  ggplot() +
  aes(x = Country, y = `Zn [ug/L]`, color = Country) +
  geom_boxplot(show.legend = FALSE) +
  scale_color_manual(values = c("blue", "red" , "black","#008B8B")) +
  ylim(0, 100)+
  ylab(expression()) +
  geom_point(shape=16, show.legend = FALSE)+
  labs(title = quote(Zn~ug~L^-1)) +
  ggthemes::theme_base() +
  theme(strip.text.x = element_text(size = 14))+
  theme(plot.title = element_text(color="Blue", size=15, face="plain", vjust = -8, hjust = 0.53))+
  theme(axis.title = element_text(size=12))+
  facet_wrap(vars(`Sample type`))

grid.arrange(l17 , l18, nrow = 1, ncol = 2)
fig_5e<-grid.arrange(l17, l18, nrow = 1, ncol = 2)
ggsave(fig_5e, filename = "fig_5e.png",  width = 10, height = 3.5, units="in", dpi = 300)


