# exploring a combined data for cryosphere and the other sample
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

# do some exloratory boxplots first
plot_boxplot(WQ_samples_2019_2020 [c(7, 11:34)], by = "Sample type", title = "WQ_samples_2019_2020 sample type", ggtheme = theme_test(), geom_boxplot_args = list("outlier.color" = "red"))
plot_boxplot(WQ_samples_2019_2020 [c(3, 11:34)], by = "month", title = "WQ_samples_2019_2020 by Month", ggtheme = theme_test(),geom_boxplot_args = list("outlier.color" = "blue"))
plot_boxplot(WQ_samples_2019_2020 [c(8, 11:34)], by = "Country", title = "WQ_samples_2019_2020 HL by Country", ggtheme = theme_test(),geom_boxplot_args = list("outlier.color" = "blue"))


s1<-WQ_samples_2019_2020 %>%
  filter(`Sample type` %in% 
           c("streamwater", "groundwater", "reservoir", "glaciermelt", "snowpack", "icemelt")) %>%
  ggplot() +
  aes(x = month, y =pH) +
  geom_boxplot(fill = "#ffffff", outlier.colour = "red") +
  geom_jitter(shape=16, position=position_jitter(0.2))+
  labs(title = "pH") +
  ggthemes::theme_base() +
  theme(plot.title = element_text(color="Blue", size=13, face="plain", vjust = -20, hjust = 0.5))+
  theme(axis.title = element_text(size=11))+
  facet_wrap(vars(`Sample type`))

s2<-WQ_samples_2019_2020 %>%
  filter(`Sample type` %in% 
           c("streamwater", "groundwater", "reservoir", "glaciermelt", "snowpack", "icemelt")) %>%
  ggplot() +
  aes(x = month, y =`EC [microS/cm]`) +
  geom_boxplot(fill = "#ffffff", outlier.colour = "red") +
  geom_jitter(shape=16, position=position_jitter(0.2))+
  labs(title = "EC [microS/cm]") +
  ggthemes::theme_base() +
  theme(plot.title = element_text(color="Blue", size=13, face="plain", vjust = -20, hjust = 0.5))+
  theme(axis.title = element_text(size=11))+
  facet_wrap(vars(`Sample type`))

s3<-WQ_samples_2019_2020 %>%
  filter(`Sample type` %in% 
           c("streamwater", "groundwater", "reservoir", "glaciermelt", "snowpack", "icemelt")) %>%
  ggplot() +
  aes(x = month, y =`Al [ug/L]`) +
  geom_boxplot(fill = "#ffffff", outlier.colour = "red") +
  geom_jitter(shape=16, position=position_jitter(0.2))+
  labs(title = "Al [ug/L]") +
  ggthemes::theme_base() +
  theme(plot.title = element_text(color="Blue", size=13, face="plain", vjust = -20, hjust = 0.5))+
  theme(axis.title = element_text(size=11))+
  facet_wrap(vars(`Sample type`))

s4<-WQ_samples_2019_2020 %>%
  filter(`Sample type` %in% 
           c("streamwater", "groundwater", "reservoir", "glaciermelt", "snowpack", "icemelt")) %>%
  ggplot() +
  aes(x = month, y =`Cr [ug/L]`) +
  geom_boxplot(fill = "#ffffff", outlier.color ="red") +
  geom_jitter(shape=16, position=position_jitter(0.2))+
  labs(title = "Cr [ug/L]") +
  ggthemes::theme_base() +
  theme(plot.title = element_text(color="Blue", size=13, face="plain", vjust = -20, hjust = 0.5))+
  theme(axis.title = element_text(size=11))+
  facet_wrap(vars(`Sample type`))

 grid.arrange(s1, s2, s3, s4, nrow = 2, ncol = 2)

s5<-WQ_samples_2019_2020 %>%
  filter(`Sample type` %in% 
           c("streamwater", "groundwater", "reservoir", "glaciermelt", "snowpack", "icemelt")) %>%
  ggplot() +
  aes(x = month, y =`PO3-4-P [mg/L]`) +
  geom_boxplot(fill = "#ffffff", outlier.color ="red") +
  geom_jitter(shape=16, position=position_jitter(0.2))+
  labs(title = "PO3-4-P [mg/L]") +
  ggthemes::theme_base() +
  theme(plot.title = element_text(color="Blue", size=13, face="plain", vjust = -20, hjust = 0.5))+
  theme(axis.title = element_text(size=11))+
  facet_wrap(vars(`Sample type`))

s6<-WQ_samples_2019_2020 %>%
  filter(`Sample type` %in% 
           c("streamwater", "groundwater", "reservoir", "glaciermelt", "snowpack", "icemelt")) %>%
  ggplot() +
  aes(x = month, y =`NO+3-N [mg/L]`) +
  geom_boxplot(fill = "#ffffff", outlier.color ="red") +
  geom_jitter(shape=16, position=position_jitter(0.2))+
  labs(title = "NO+3-N [mg/L]") +
  ggthemes::theme_base() +
  theme(plot.title = element_text(color="Blue", size=13, face="plain", vjust = -20, hjust = 0.5))+
  theme(axis.title = element_text(size=11))+
  facet_wrap(vars(`Sample type`))


s7<-WQ_samples_2019_2020 %>%
  filter(`Sample type` %in% 
           c("streamwater", "groundwater", "reservoir", "glaciermelt", "snowpack", "icemelt")) %>%
  ggplot() +
  aes(x = month, y =`Fe-Dis [mg/L]`) +
  geom_boxplot(fill = "#ffffff", outlier.colour = "red") +
  geom_jitter(shape=16, position=position_jitter(0.2))+
  labs(title = "Fe-Dis [mg/L]") +
  ggthemes::theme_base() +
  theme(plot.title = element_text(color="Blue", size=13, face="plain", vjust = -20, hjust = 0.5))+
  theme(axis.title = element_text(size=11))+
  facet_wrap(vars(`Sample type`))


s8<-WQ_samples_2019_2020 %>%
  filter(`Sample type` %in% 
           c("streamwater", "groundwater", "reservoir", "glaciermelt", "snowpack", "icemelt")) %>%
  ggplot() +
  aes(x = month, y = `Fe-Tot [mg/L]`) +
  geom_boxplot(fill = "#ffffff", outlier.colour = "red") +
  geom_jitter(shape=16, position=position_jitter(0.2))+
  labs(title = "Fe-Tot [mg/L]") +
  ggthemes::theme_base() +
  theme(plot.title = element_text(color="Blue", size=13, face="plain", vjust = -20, hjust = 0.5))+
  theme(axis.title = element_text(size=11))+
  facet_wrap(vars(`Sample type`))

grid.arrange(s5, s6, s7, s8, nrow = 2, ncol = 2)

s9<-WQ_samples_2019_2020 %>%
  filter(`Sample type` %in% 
           c("streamwater", "groundwater", "reservoir", "glaciermelt", "snowpack", "icemelt")) %>%
  ggplot() +
  aes(x = month, y = `Ca [mg/l]`) +
  geom_boxplot(fill = "#ffffff", outlier.colour = "red") +
  geom_jitter(shape=16, position=position_jitter(0.2))+
  labs(title = "Ca [mg/l]") +
  ggthemes::theme_base() +
  theme(plot.title = element_text(color="Blue", size=13, face="plain", vjust = -20, hjust = 0.5))+
  theme(axis.title = element_text(size=11))+
  facet_wrap(vars(`Sample type`))

s10<-WQ_samples_2019_2020 %>%
  filter(`Sample type` %in% 
           c("streamwater", "groundwater", "reservoir", "glaciermelt", "snowpack", "icemelt")) %>%
  ggplot() +
  aes(x = month, y = `Mg [mg/L]`) +
  geom_boxplot(fill = "#ffffff", outlier.colour = "red") +
  geom_jitter(shape=16, position=position_jitter(0.2))+
  labs(title = "Mg [mg/L]") +
  ggthemes::theme_base() +
  theme(plot.title = element_text(color="Blue", size=13, face="plain", vjust = -20, hjust = 0.5))+
  theme(axis.title = element_text(size=11))+
  facet_wrap(vars(`Sample type`))

s11<-WQ_samples_2019_2020 %>%
  filter(`Sample type` %in% 
           c("streamwater", "groundwater", "reservoir", "glaciermelt", "snowpack", "icemelt")) %>%
  ggplot() +
  aes(x = month, y = `Na [mg/l]`) +
  geom_boxplot(fill = "#ffffff", outlier.colour = "red") +
  geom_jitter(shape=16, position=position_jitter(0.2))+
  labs(title = "Na [mg/l]") +
  ggthemes::theme_base() +
  theme(plot.title = element_text(color="Blue", size=13, face="plain", vjust = -20, hjust = 0.5))+
  theme(axis.title = element_text(size=11))+
  facet_wrap(vars(`Sample type`))

s12<-WQ_samples_2019_2020 %>%
  filter(`Sample type` %in% 
           c("streamwater", "groundwater", "reservoir", "glaciermelt", "snowpack", "icemelt")) %>%
  ggplot() +
  aes(x = month, y = `K [mg/L]`) +
  geom_boxplot(fill = "#ffffff", outlier.colour = "red") +
  geom_jitter(shape=16, position=position_jitter(0.2))+
  labs(title = "K [mg/L]") +
  ggthemes::theme_base() +
  theme(plot.title = element_text(color="Blue", size=13, face="plain", vjust = -20, hjust = 0.5))+
  theme(axis.title = element_text(size=11))+
  facet_wrap(vars(`Sample type`))

grid.arrange(s9, s10, s11, s12, nrow = 2, ncol = 2)

s13<-WQ_samples_2019_2020 %>%
  filter(`Sample type` %in% 
           c("streamwater", "groundwater", "reservoir", "glaciermelt", "snowpack", "icemelt")) %>%
  ggplot() +
  aes(x = month, y = `As [ug/L]`) +
  geom_boxplot(fill = "#ffffff", outlier.colour = "red") +
  geom_jitter(shape=16, position=position_jitter(0.2))+
  labs(title = "As [ug/L]") +
  ggthemes::theme_base() +
  theme(plot.title = element_text(color="Blue", size=13, face="plain", vjust = -20, hjust = 0.5))+
  theme(axis.title = element_text(size=11))+
  facet_wrap(vars(`Sample type`))

s14<-WQ_samples_2019_2020 %>%
  filter(`Sample type` %in% 
           c("streamwater", "groundwater", "reservoir", "glaciermelt", "snowpack", "icemelt")) %>%
  ggplot() +
  aes(x = month, y = `Cd [ug/L]`) +
  geom_boxplot(fill = "#ffffff", outlier.colour = "red") +
  geom_jitter(shape=16, position=position_jitter(0.2))+
  labs(title = "Cd [ug/L]") +
  ggthemes::theme_base() +
  theme(plot.title = element_text(color="Blue", size=13, face="plain", vjust = -20, hjust = 0.5))+
  theme(axis.title = element_text(size=11))+
  facet_wrap(vars(`Sample type`))

s15<-WQ_samples_2019_2020 %>%
  filter(`Sample type` %in% 
           c("streamwater", "groundwater", "reservoir", "glaciermelt", "snowpack", "icemelt")) %>%
  ggplot() +
  aes(x = month, y = `Co [ug/L]`) +
  geom_boxplot(fill = "#ffffff", outlier.colour = "red") +
  geom_jitter(shape=16, position=position_jitter(0.2))+
  labs(title = "Cd [ug/L]") +
  ggthemes::theme_base() +
  theme(plot.title = element_text(color="Blue", size=13, face="plain", vjust = -20, hjust = 0.5))+
  theme(axis.title = element_text(size=11))+
  facet_wrap(vars(`Sample type`))

s16<-WQ_samples_2019_2020 %>%
  filter(`Sample type` %in% 
           c("streamwater", "groundwater", "reservoir", "glaciermelt", "snowpack", "icemelt")) %>%
  ggplot() +
  aes(x = month, y = `Pb [ug/L]`) +
  geom_boxplot(fill = "#ffffff", outlier.colour = "red") +
  geom_jitter(shape=16, position=position_jitter(0.2))+
  labs(title = "Pb [ug/L]") +
  ggthemes::theme_base() +
  theme(plot.title = element_text(color="Blue", size=13, face="plain", vjust = -20, hjust = 0.5))+
  theme(axis.title = element_text(size=11))+
  facet_wrap(vars(`Sample type`))

grid.arrange(s13, s14, s15, s16, nrow = 2, ncol = 2)

#`Ba [ug/L]`
#`Cu [ug/L]`
#`Mn [ug/L]`
#`Zn [ug/L]`

s17<-WQ_samples_2019_2020 %>%
  filter(`Sample type` %in% 
           c("streamwater", "groundwater", "reservoir", "glaciermelt", "snowpack", "icemelt")) %>%
  ggplot() +
  aes(x = month, y = `Ba [ug/L]`) +
  geom_boxplot(fill = "#ffffff", outlier.colour = "red") +
  geom_jitter(shape=16, position=position_jitter(0.2))+
  labs(title = "Ba [ug/L]") +
  ggthemes::theme_base() +
  theme(plot.title = element_text(color="Blue", size=13, face="plain", vjust = -20, hjust = 0.5))+
  theme(axis.title = element_text(size=11))+
  facet_wrap(vars(`Sample type`))


s18<-WQ_samples_2019_2020 %>%
  filter(`Sample type` %in% 
           c("streamwater", "groundwater", "reservoir", "glaciermelt", "snowpack", "icemelt")) %>%
  ggplot() +
  aes(x = month, y = `Mn [ug/L]`) +
  geom_boxplot(fill = "#ffffff", outlier.colour = "red") +
  geom_jitter(shape=16, position=position_jitter(0.2))+
  labs(title = "Mn [ug/L]") +
  ggthemes::theme_base() +
  theme(plot.title = element_text(color="Blue", size=13, face="plain", vjust = -20, hjust = 0.5))+
  theme(axis.title = element_text(size=11))+
  facet_wrap(vars(`Sample type`))

s19<-WQ_samples_2019_2020 %>%
  filter(`Sample type` %in% 
           c("streamwater", "groundwater", "reservoir", "glaciermelt", "snowpack", "icemelt")) %>%
  ggplot() +
  aes(x = month, y = `Cu [ug/L]`) +
  geom_boxplot(fill = "#ffffff", outlier.colour = "red") +
  geom_jitter(shape=16, position=position_jitter(0.2))+
  labs(title = "Cu [ug/L]") +
  ggthemes::theme_base() +
  theme(plot.title = element_text(color="Blue", size=13, face="plain", vjust = -20, hjust = 0.5))+
  theme(axis.title = element_text(size=11))+
  facet_wrap(vars(`Sample type`))

s20<-WQ_samples_2019_2020 %>%
  filter(`Sample type` %in% 
           c("streamwater", "groundwater", "reservoir", "glaciermelt", "snowpack", "icemelt")) %>%
  ggplot() +
  aes(x = month, y = `Zn [ug/L]`) +
  geom_boxplot(fill = "#ffffff", outlier.colour = "red") +
  geom_jitter(shape=16, position=position_jitter(0.2))+
  labs(title = "Zn [ug/L]") +
  ggthemes::theme_base() +
  theme(plot.title = element_text(color="Blue", size=13, face="plain", vjust = -20, hjust = 0.5))+
  theme(axis.title = element_text(size=11))+
  facet_wrap(vars(`Sample type`))

grid.arrange(s17, s18, s19, s20, nrow = 2, ncol = 2)

#`Li [ug/L]`
#`Ni [ug/L]`
#`Sr [ug/L]`

s21<-WQ_samples_2019_2020 %>%
  filter(`Sample type` %in% 
           c("streamwater", "groundwater", "reservoir", "glaciermelt", "snowpack", "icemelt")) %>%
  ggplot() +
  aes(x = month, y = `Li [ug/L]`) +
  geom_boxplot(fill = "#ffffff", outlier.colour = "red") +
  geom_jitter(shape=16, position=position_jitter(0.2))+
  labs(title = "Li [ug/L]") +
  ggthemes::theme_base() +
  theme(plot.title = element_text(color="Blue", size=13, face="plain", vjust = -20, hjust = 0.5))+
  theme(axis.title = element_text(size=11))+
  facet_wrap(vars(`Sample type`))

s22<-WQ_samples_2019_2020 %>%
  filter(`Sample type` %in% 
           c("streamwater", "groundwater", "reservoir", "glaciermelt", "snowpack", "icemelt")) %>%
  ggplot() +
  aes(x = month, y = `Sr [ug/L]`) +
  geom_boxplot(fill = "#ffffff", outlier.colour = "red") +
  geom_jitter(shape=16, position=position_jitter(0.2))+
  labs(title = "Sr [ug/L]") +
  ggthemes::theme_base() +
  theme(plot.title = element_text(color="Blue", size=13, face="plain", vjust = -20, hjust = 0.5))+
  theme(axis.title = element_text(size=11))+
  facet_wrap(vars(`Sample type`))

s23<-WQ_samples_2019_2020 %>%
  filter(`Sample type` %in% 
           c("streamwater", "groundwater", "reservoir", "glaciermelt", "snowpack", "icemelt")) %>%
  ggplot() +
  aes(x = month, y = `Ni [ug/L]`) +
  geom_boxplot(fill = "#ffffff", outlier.colour = "red") +
  geom_jitter(shape=16, position=position_jitter(0.2))+
  labs(title = "Ni [ug/L]") +
  ggthemes::theme_base() +
  theme(plot.title = element_text(color="Blue", size=13, face="plain", vjust = -20, hjust = 0.5))+
  theme(axis.title = element_text(size=11))+
  facet_wrap(vars(`Sample type`))

grid.arrange(s21, s22, s23, nrow = 2, ncol = 2)

# maybe do some PCA
plot_prcomp(na.omit(WQ_samples_2019_2020), maxcat = 5L, ggtheme = theme_bw())
