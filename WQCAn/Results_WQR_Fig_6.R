# exploring a combined data for cryosphere and the other samples
# first load the required packages
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

# first load  data
WQ_samples_2019_2020 <- read_csv("ICP_samples__2019_2020.csv", 
                               col_types = cols(Country = col_factor(levels = c("TJ",
"UZ", "KG", "KZ")), month = col_factor()))

# first start with major ions
s1<-WQ_samples_2019_2020 %>%
  filter(`Sample type` %in% 
           c("streamwater", "groundwater", "glaciermelt", "snowpack")) %>%
  ggplot() +
  aes(x = month, y = `Ca [mg/l]`) +
  geom_boxplot(fill = "#ffffff") +
  geom_jitter(shape=16, position=position_jitter(0.2))+
  labs(title = "Ca [mg/l]") +
  ggthemes::theme_base() +
  theme(plot.title = element_text(color="Blue", size=13, face="plain", vjust = -20, hjust = 0.35))+
  theme(axis.title = element_text(size=11))+
  facet_wrap(vars(`Sample type`))

s2<-WQ_samples_2019_2020 %>%
  filter(`Sample type` %in% 
           c("streamwater", "groundwater", "glaciermelt", "snowpack")) %>%
  ggplot() +
  aes(x = month, y = `Mg [mg/L]`) +
  geom_boxplot(fill = "#ffffff") +
  geom_jitter(shape=16, position=position_jitter(0.2))+
  labs(title = "Mg [mg/L]") +
  ggthemes::theme_base() +
  theme(plot.title = element_text(color="Blue", size=13, face="plain", vjust = -20, hjust = 0.35))+
  theme(axis.title = element_text(size=11))+
  facet_wrap(vars(`Sample type`))

s3<-WQ_samples_2019_2020 %>%
  filter(`Sample type` %in% 
           c("streamwater", "groundwater", "glaciermelt", "snowpack")) %>%
  ggplot() +
  aes(x = month, y = `Na [mg/l]`) +
  geom_boxplot(fill = "#ffffff") +
  geom_jitter(shape=16, position=position_jitter(0.2))+
  labs(title = "Na [mg/l]") +
  ggthemes::theme_base() +
  theme(plot.title = element_text(color="Blue", size=13, face="plain", vjust = -20, hjust = 0.35))+
  theme(axis.title = element_text(size=11))+
  facet_wrap(vars(`Sample type`))

s4<-WQ_samples_2019_2020 %>%
  filter(`Sample type` %in% 
           c("streamwater", "groundwater","glaciermelt", "snowpack")) %>%
  ggplot() +
  aes(x = month, y = `K [mg/L]`) +
  geom_boxplot(fill = "#ffffff") +
  geom_jitter(shape=16, position=position_jitter(0.2))+
  labs(title = "K [mg/L]") +
  ggthemes::theme_base() +
  theme(plot.title = element_text(color="Blue", size=13, face="plain", vjust = -20, hjust = 0.35))+
  theme(axis.title = element_text(size=11))+
  facet_wrap(vars(`Sample type`))

grid.arrange(s1, s2, s3, s4, nrow = 2, ncol = 2)

# then crustal elements


s5<-WQ_samples_2019_2020 %>%
  filter(`Sample type` %in% 
           c("streamwater", "groundwater", "glaciermelt", "snowpack")) %>%
  ggplot() +
  aes(x = month, y = `Fe-Tot [mg/L]`) +
  geom_boxplot(fill = "#ffffff") +
  geom_jitter(shape=16, position=position_jitter(0.2))+
  labs(title = "Fe-Tot [mg/L]") +
  ggthemes::theme_base() +
  theme(plot.title = element_text(color="Blue", size=13, face="plain", vjust = -20, hjust = 0.4))+
  theme(axis.title = element_text(size=11))+
  facet_wrap(vars(`Sample type`))

s6<-WQ_samples_2019_2020 %>%
  filter(`Sample type` %in% 
           c("streamwater", "groundwater", "glaciermelt", "snowpack")) %>%
  ggplot() +
  aes(x = month, y =`Al [ug/L]`) +
  geom_boxplot(fill = "#ffffff") +
  geom_jitter(shape=16, position=position_jitter(0.2))+
  labs(title = "Al [ug/L]") +
  ggthemes::theme_base() +
  theme(plot.title = element_text(color="Blue", size=13, face="plain", vjust = -20, hjust = 0.35))+
  theme(axis.title = element_text(size=11))+
  facet_wrap(vars(`Sample type`))

s7<-WQ_samples_2019_2020 %>%
  filter(`Sample type` %in% 
           c("streamwater", "groundwater", "glaciermelt", "snowpack")) %>%
  ggplot() +
  aes(x = month, y =`Cr [ug/L]`) +
  geom_boxplot(fill = "#ffffff") +
  geom_jitter(shape=16, position=position_jitter(0.2))+
  labs(title = "Cr [ug/L]") +
  ggthemes::theme_base() +
  theme(plot.title = element_text(color="Blue", size=13, face="plain", vjust = -20, hjust = 0.35))+
  theme(axis.title = element_text(size=11))+
  facet_wrap(vars(`Sample type`))

s8<-WQ_samples_2019_2020 %>%
  filter(`Sample type` %in% 
           c("streamwater", "groundwater", "glaciermelt", "snowpack")) %>%
  ggplot() +
  aes(x = month, y = `Mn [ug/L]`) +
  geom_boxplot(fill = "#ffffff") +
  geom_jitter(shape=16, position=position_jitter(0.2))+
  labs(title = "Mn [ug/L]") +
  ggthemes::theme_base() +
  theme(plot.title = element_text(color="Blue", size=13, face="plain", vjust = -20, hjust = 0.35))+
  theme(axis.title = element_text(size=11))+
  facet_wrap(vars(`Sample type`))

grid.arrange(s5, s6, s7, s8, nrow = 2, ncol = 2)



s9<-WQ_samples_2019_2020 %>%
  filter(`Sample type` %in% 
           c("streamwater", "groundwater", "glaciermelt", "snowpack")) %>%
  ggplot() +
  aes(x = month, y = `As [ug/L]`) +
  geom_boxplot(fill = "#ffffff") +
  geom_jitter(shape=16, position=position_jitter(0.2))+
  labs(title = "As [ug/L]") +
  ggthemes::theme_base() +
  theme(plot.title = element_text(color="Blue", size=13, face="plain", vjust = -20, hjust = 0.35))+
  theme(axis.title = element_text(size=11))+
  facet_wrap(vars(`Sample type`))

s10<-WQ_samples_2019_2020 %>%
  filter(`Sample type` %in% 
           c("streamwater", "groundwater", "glaciermelt", "snowpack")) %>%
  ggplot() +
  aes(x = month, y = `Pb [ug/L]`) +
  geom_boxplot(fill = "#ffffff") +
  geom_jitter(shape=16, position=position_jitter(0.2))+
  labs(title = "Pb [ug/L]") +
  ggthemes::theme_base() +
  theme(plot.title = element_text(color="Blue", size=13, face="plain", vjust = -20, hjust = 0.35))+
  theme(axis.title = element_text(size=11))+
  facet_wrap(vars(`Sample type`))

s11<-WQ_samples_2019_2020 %>%
  filter(`Sample type` %in% 
           c("streamwater", "groundwater", "glaciermelt", "snowpack")) %>%
  ggplot() +
  aes(x = month, y = `Cd [ug/L]`) +
  geom_boxplot(fill = "#ffffff") +
  geom_jitter(shape=16, position=position_jitter(0.2))+
  labs(title = "Cd [ug/L]") +
  ggthemes::theme_base() +
  theme(plot.title = element_text(color="Blue", size=13, face="plain", vjust = -20, hjust = 0.35))+
  theme(axis.title = element_text(size=11))+
  facet_wrap(vars(`Sample type`))

s12<-WQ_samples_2019_2020 %>%
  filter(`Sample type` %in% 
           c("streamwater", "groundwater", "glaciermelt", "snowpack")) %>%
  ggplot() +
  aes(x = month, y = `Ni [ug/L]`) +
  geom_boxplot(fill = "#ffffff") +
  geom_jitter(shape=16, position=position_jitter(0.2))+
  labs(title = "Ni [ug/L]") +
  ggthemes::theme_base() +
  theme(plot.title = element_text(color="Blue", size=13, face="plain", vjust = -20, hjust = 0.35))+
  theme(axis.title = element_text(size=11))+
  facet_wrap(vars(`Sample type`))

grid.arrange(s9, s10, s11, s12, nrow = 2, ncol = 2)
`

s13<-WQ_samples_2019_2020 %>%
  filter(`Sample type` %in% 
           c("streamwater", "groundwater", "glaciermelt", "snowpack")) %>%
  ggplot() +
  aes(x = month, y = `Cu [ug/L]`) +
  geom_boxplot(fill = "#ffffff") +
  geom_jitter(shape=16, position=position_jitter(0.2))+
  labs(title = "Cu [ug/L]") +
  ggthemes::theme_base() +
  theme(plot.title = element_text(color="Blue", size=13, face="plain", vjust = -20, hjust = 0.35))+
  theme(axis.title = element_text(size=11))+
  facet_wrap(vars(`Sample type`))


s14<-WQ_samples_2019_2020 %>%
  filter(`Sample type` %in% 
           c("streamwater", "groundwater", "glaciermelt", "snowpack")) %>%
  ggplot() +
  aes(x = month, y = `Zn [ug/L]`) +
  geom_boxplot(fill = "#ffffff") +
  geom_jitter(shape=16, position=position_jitter(0.2))+
  labs(title = "Zn [ug/L]") +
  ggthemes::theme_base() +
  theme(plot.title = element_text(color="Blue", size=13, face="plain", vjust = -20, hjust = 0.35))+
  theme(axis.title = element_text(size=11))+
  facet_wrap(vars(`Sample type`))

grid.arrange(s13, s14, nrow = 1, ncol = 2)





