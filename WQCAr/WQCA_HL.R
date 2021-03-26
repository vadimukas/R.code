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


plot_boxplot(WQ_samples_2019_HL [c(7, 10:34)], by = "Sample type", title = "WQ_samples__2019_HL sample type", ggtheme = theme_test(), geom_boxplot_args = list("outlier.color" = "red"))
plot_boxplot(WQ_samples_2019_HL [c(3, 10:34)], by = "month", title = "WQ_samples__2019_HL by Month", ggtheme = theme_test(), geom_boxplot_args = list("outlier.color" = "blue"))
plot_boxplot(WQ_samples_2019_HL [c(8, 10:34)], by = "Country", title = "WQ_samples__2019_HL by Country", ggtheme = theme_test(),geom_boxplot_args = list("outlier.color" = "blue"))


plot_boxplot(WQ_samples_2019_HL [c(3, 10:34)], by = "month", title = "WQ_samples_2019_HL by Month", ggtheme = theme_bw())

library(dplyr)
library(ggplot2)

grid.arrange(p1, p2, nrow = 2, ncol = 2)

p1<-WQ_samples_2019_HL %>%
 ggplot() +
 aes(x = Period, y = `TDS [mg/L]`) +
 geom_boxplot(fill = "#ffffff", outlier.colour="red") +
  labs(title = "TDS") +
  ggthemes::theme_base()+  
  theme(plot.title = element_text(color="blue", size=14, face="plain", vjust = -10, hjust = 0.5))+
  theme(axis.title = element_text(size=11))+
geom_jitter(shape=1, position=position_jitter(0.2))


# trying to map country to point
p2<-WQ_samples_2019_HL %>%
  ggplot() +
  aes(x = Period, y = `TDS [mg/L]`, color=Country) +
  geom_boxplot(fill = "#ffffff", outlier.colour="red") +
  geom_point(aes(y=`TDS [mg/L]`), colour='black', alpha=0.7, position=position_jitter(0.2))+
  labs(title = "TDS") +
  ggthemes::theme_base()+  
  theme(plot.title = element_text(color="blue", size=14, face="plain", vjust = -10, hjust = 0.5))+
  theme(axis.title = element_text(size=11))

# setting the plot style first
WQ_samples_2019_HL %>%
  ggplot() +
  aes(x = Period, y = `TDS [mg/L]`, shape=Country, color=Country) +
  geom_boxplot(fill = "#ffffff", outlier.colour="red") +
  geom_point(aes(y=`TDS [mg/L]`), colour='black', alpha=0.8, position=position_jitter(0.1))+
  labs(title = "TDS") +
   ggthemes::theme_base()+  
  theme(plot.title = element_text(color="blue", size=14, face="plain", vjust = -10, hjust = 0.5))+
  theme(axis.title = element_text(size=11))


WQ_samples_2019_HL %>%
  ggplot() +
  aes(x = Period, y = `TDS [mg/L]`) +
  geom_violin(fill = "#ffffff", outlier.colour="red") +
  geom_point(aes(y=`TDS [mg/L]`), alpha=0.7, position=position_jitter(0.1))+
  labs(title = "TDS") +
  ggthemes::theme_base()+  
  theme(plot.title = element_text(color="blue", size=13, face="plain", vjust = -10, hjust = 0.5))+
  theme(axis.title = element_text(size=11))
  

# experiment with significance tests
tMg<-WQ_samples_2019_HL %>% 
  select(Period, `Mg [mg/L]`)

t.test(`Mg [mg/L]` ~ Period, data = t)

ggplot(t, aes(Period,`Mg [mg/L]` ))+
  geom_boxplot()

tTDS<-WQ_samples_2019_HL %>% 
  select(Period, `TDS [mg/L]`)

t.test(`TDS [mg/L]` ~ Period, data = tTDS)

tAl<-WQ_samples_2019_HL %>% 
  select(Period, `Al [ug/L]`)

t.test(`Al [ug/L]` ~ Period, data = tAl)
ggplot(tAl, aes(Period,`Al [ug/L]` ))+
  geom_boxplot()

# trying to use ggstat for statictical plots 
# https://indrajeetpatil.github.io/ggstatsplot/

ggstatsplot::ggbetweenstats(
  data = tAl,
  x = Period,
  y = 'Al [ug/L]',
  messages = FALSE
)

ggstatsplot::ggbetweenstats(
  data = tTDS,
  x = Period,
  y = 'TDS [mg/L]',
  messages = FALSE
)

# working to make the best plot
ggstatsplot::ggbetweenstats(
  data = tTDS,
  x = Period,
  y = 'TDS [mg/L]',
   type = "p",
  mean.ci = FALSE,
  plot.type = "box",
  pairwise.comparisons = FALSE,
  ggplot.component = list(),
  var.equal = TRUE,
  pairwise.display = "all",
  outlier.tagging = TRUE,
  mean.label.size = 3.5,
  outlier.coef = 2,
  ggstatsplot.layer = FALSE,
  bf.message = FALSE,
  results.subtitle = FALSE,
  stat.title = (''),
  sample.size.label = TRUE,
      messages = TRUE
)

# try to do two plots 
par(mfrow = c(2,2))

ggstatsplot::ggbetweenstats(
  data = tTDS,
  x = Period,
  y = 'TDS [mg/L]',
  type = "p",
  mean.ci = FALSE,
  pairwise.comparisons = TRUE,
  pairwise.display = "s",
  outlier.tagging = TRUE,
  outlier.coef = 2,
  outlier.label.color = "darkgreen",
  ggstatsplot.layer = FALSE,
  pairwise.annotation = "p.value",
  messages = TRUE
)

ggstatsplot::ggbetweenstats(
  data = tAl,
  x = Period,
  y = 'Al [ug/L]',
  type = "p",
  mean.ci = FALSE,
  pairwise.comparisons = TRUE,
  pairwise.display = "s",
  outlier.tagging = TRUE,
  outlier.coef = 2,
  outlier.label.color = "darkgreen",
  ggstatsplot.layer = FALSE,
  pairwise.annotation = "p.value",
  messages = TRUE
)


WQ_samples_2019_HL %>%
  ggplot() +
  aes(x = Period, y =`Mg [mg/L]` ) +
  geom_boxplot(fill = "#ffffff") +
    geom_point(aes(y=`Mg [mg/L]`), alpha=0.7, position=position_jitter(0.1))+
  labs(title = "Mg") +
  ggthemes::theme_base()+  
  theme(plot.title = element_text(color="blue", size=13, face="plain", vjust = -10, hjust = 0.5))+
  theme(axis.title = element_text(size=11))


ggplot(WQ_samples_2019_HL, aes(x=`Al [ug/L]`, y=`TDS [mg/L]`)) + geom_point(shape=5)


library(ggplot2)

ggplot(WQ_samples_2019_HL) +
  aes(x = Period, y = `Al [ug/L]`) +
  geom_boxplot(fill = "#ffffff") +
  ggthemes::theme_base()

WQ_samples_2019_HL %>%
  ggplot() +
  aes(x = Period, y = `Al [ug/L]`, color=Country, shape=Country) +
  geom_boxplot(fill = "#ffffff", outlier.colour="red") +
  geom_point(aes(y=`Al [ug/L]`), colour='black', alpha=0.7, position=position_jitter(0.2))+
  labs(title = "Al") +
  ggthemes::theme_base()+  
  theme(plot.title = element_text(color="blue", size=14, face="plain", vjust = -10, hjust = 0.5))+
  theme(axis.title = element_text(size=11))

WQ_samples_2019_HL %>%
  ggplot() +
  aes(x = Period, y = `Al [ug/L]`, shape=Country) +
  geom_boxplot(fill = "#ffffff", outlier.colour="red") +
  geom_point(aes(y=`Al [ug/L]`), colour='black', alpha=0.7, position=position_jitter(0.2))+
  labs(title = "Al") +
  ggthemes::theme_base()+
  theme(plot.title = element_text(color="blue", size=14, face="plain", vjust = -10, hjust = 0.5))+
  theme(axis.title = element_text(size=11))

library(dplyr)
library(ggplot2)

WQ_samples_2019_HL %>%
  filter(`Sample type` %in% 
    c("streamwater", "groundwater", "reservoir")) %>%
 ggplot() +
 aes(x = Period, y = `TDS [mg/L]`) +
 geom_boxplot(fill = "#ffffff", outlier.colour="red") +
  geom_jitter(shape=16, position=position_jitter(0.2))+
 ggthemes::theme_base() +
  labs(title = "TDS") +
  ggthemes::theme_base()+  
  theme(plot.title = element_text(color="black", size=13, face="bold", vjust = -20, hjust = 0.5))+
  theme(axis.title = element_text(size=11))+
 facet_wrap(vars(`Sample type`))

WQ_samples_2019_HL %>%
    filter(`Sample type` %in% 
           c("streamwater", "groundwater", "reservoir")) %>%
  ggplot() +
  aes(x = Period, y = `NO+3-N [mg/L]`) +
  geom_boxplot(fill = "#ffffff", outlier.colour="red") +
  geom_jitter(shape=16, position=position_jitter(0.2))+
  labs(title = "NO3-N") +
  ggthemes::theme_base() +
  theme(plot.title = element_text(color="black", size=13, face="bold", vjust = -20, hjust = 0.5))+
  theme(axis.title = element_text(size=11))+
  facet_wrap(vars(`Sample type`))

WQ_samples_2019_HL %>%
  filter(`Sample type` %in% 
           c("streamwater", "groundwater", "reservoir")) %>%
  ggplot() +
  aes(x = Period, y = `PO3-4-P [mg/L]`) +
  geom_boxplot(fill = "#ffffff", outlier.colour="red") +
  geom_jitter(shape=16, position=position_jitter(0.2))+
  labs(title = "PO3-4-P") +
  ggthemes::theme_base() +
  theme(plot.title = element_text(color="black", size=13, face="bold", vjust = -20, hjust = 0.5))+
  theme(axis.title = element_text(size=11))+
  facet_wrap(vars(`Sample type`))

WQ_samples_2019_HL %>%
  filter(`Sample type` %in% 
           c("streamwater", "groundwater", "reservoir")) %>%
  ggplot() +
  aes(x = Period, y = `Ca [mg/l]`) +
  geom_boxplot(fill = "#ffffff", outlier.colour="red") +
  geom_jitter(shape=16, position=position_jitter(0.2))+
  labs(title = "Ca") +
  ggthemes::theme_base() +
  theme(plot.title = element_text(color="black", size=13, face="bold", vjust = -20, hjust = 0.5))+
  theme(axis.title = element_text(size=11))+
  facet_wrap(vars(`Sample type`))

  WQ_samples_2019_HL %>%
 filter(`Sample type` %in% 
    c("streamwater", "groundwater", "reservoir")) %>%
 ggplot() +
 aes(x = Period, y = `Fe-Tot [mg/L]`, colour = Country) +
 geom_boxplot(fill = "#ffffff") +
 scale_color_hue() +
 labs(title = "Fe ICP ") +
 ggthemes::theme_base() +
 facet_wrap(vars(`Sample type`), scales = "fixed")+
    theme(plot.title = element_text(color="red", size=11, face="bold", vjust = -20, hjust = 0.5))+
    theme(axis.title = element_text(size=11))+
    theme(legend.title = element_text(size = 11))+
  geom_jitter(shape=1, position=position_jitter(0.2))
  
  WQ_samples_2019_HL %>%
    filter(`Sample type` %in% 
             c("streamwater", "groundwater", "reservoir")) %>%
    ggplot() +
    aes(x = Period, y = `Fe-Dis [mg/L]`, colour = Country) +
    geom_boxplot(fill = "#ffffff") +
    scale_color_hue() +
    labs(title = "Fe-Dis") +
    ggthemes::theme_base() +
    facet_wrap(vars(`Sample type`), scales = "fixed")+
    theme(plot.title = element_text(color="red", size=11, face="bold", vjust = -30, hjust = 0.5))+
    theme(axis.title = element_text(size=11))+
    theme(legend.title = element_text(size = 11))+
    geom_jitter(shape=1, position=position_jitter(0.2))
  

WQ_samples_2019_HL %>%
 filter(`Sample type` %in% 
    c("streamwater", "groundwater", "reservoir")) %>%
 ggplot() +
 aes(x = Period, y = `Fe-Tot [mg/L]`, colour = Country) +
 geom_boxplot(fill = "#ffffff") +
 scale_color_hue() +
 labs(title = "Fe ICP ") +
 ggthemes::theme_base() +
  theme(plot.title = element_text(color="red", size=11, face="bold", vjust = -20, hjust = 0.5))+
  theme(axis.title = element_text(size=11))+
  theme(legend.title = element_text(size = 11))+
 facet_wrap(vars(`Sample type`), scales = "fixed")+
geom_jitter(shape=1, position=position_jitter(0.2))

library(dplyr)
library(ggplot2)

WQ_samples_2019_HL %>%
  filter(`Sample type` %in% 
    c("streamwater", "groundwater", "reservoir")) %>%
    ggplot() +
 aes(x = Period, y = `TDS [mg/L]`) +
 geom_boxplot(fill = "#ffffff") +
  labs(title = "TDS ") +
  ggthemes::theme_base()+
  theme(plot.title = element_text(color="red", size=11, face="bold", vjust = -20, hjust = 0.5))+
  theme(axis.title = element_text(size=11))+
  theme(legend.title = element_text(size = 11))+
  geom_jitter(shape=16, position=position_jitter(0.2))


# try another way to import data with months as integers and Country as factor

library(ggplot2)

ggplot(WQ_samples_2019_HL) +
 aes(x = Period, y = `TDS [mg/L]`, fill = Country) +
 geom_boxplot() +
 scale_fill_hue() +
  labs(title = "TDS ") +
  ggthemes::theme_base()+
  theme(plot.title = element_text(color="red", size=11, face="bold", vjust = -20, hjust = 0.5))+
  theme(axis.title = element_text(size=11))+
  theme(legend.title = element_text(size = 11))+
geom_jitter(shape=1, position=position_jitter(0.2))

ggplot(WQ_samples_2019_HL) +
  aes(x = Period, y = `TDS [mg/L]`, fill = Country) +
  geom_boxplot() +
  scale_fill_hue() +
  ggthemes::theme_base()+
  labs(title = "TDS ") +
  ggthemes::theme_base()+
  theme(plot.title = element_text(color="red", size=11, face="bold", vjust = -20, hjust = 0.5))+
  theme(axis.title = element_text(size=11))+
  theme(legend.title = element_text(size = 11))+
  geom_jitter(shape=1, position=position_jitter(0.2))

WQ_samples_2019_HL <- read_csv("WQ_samples__2019_HL.csv", 
                               col_types = cols(Country = col_factor(levels = c("TJ", 
                                                                                "UZ", "KG", "KZ")), month = col_integer()))
View(WQ_samples_2019_HL)

WQ_samples_2019_HL %>%
  filter(!(month %in% "5")) %>%
  filter(`Sample type` %in% 
           c("streamwater", "groundwater", "reservoir")) %>%
  ggplot() +
  aes(x = Period, y = `TDS [mg/L]`) +
  geom_boxplot(fill = "#ffffff") +
  ggthemes::theme_base()+
  geom_jitter(shape=1, position=position_jitter(0.2))

WQ_samples_2019_HL %>%
    filter(`Sample type` %in% 
           c("streamwater", "groundwater", "reservoir")) %>%
  ggplot() +
  aes(x = Period, y = `TDS [mg/L]`, colour = Country) +
  geom_boxplot(fill = "#ffffff") +
  scale_color_hue() +
  labs(title = "TDS ") +
  ggthemes::theme_base()+
  theme(plot.title = element_text(color="red", size=11, face="bold", vjust = -25, hjust = 0.5))+
  theme(axis.title = element_text(size=11))+
  theme(legend.title = element_text(size = 11))+
  facet_wrap(vars(`Sample type`), scales = "fixed")+
  geom_jitter(shape=1, position=position_jitter(0.1))

WQ_samples_2019_HL %>%
  filter(`Sample type` %in% 
           c("streamwater", "groundwater")) %>%
  ggplot() +
  aes(x = Period, y = `TDS [mg/L]`, colour = Country) +
  geom_boxplot(fill = "#ffffff") +
  scale_color_hue() +
  labs(title = "TDS ") +
  ggthemes::theme_base()+
  theme(plot.title = element_text(color="red", size=11, face="bold", vjust = -25, hjust = 0.7))+
  theme(axis.title = element_text(size=11))+
  theme(legend.title = element_text(size = 11))+
  facet_wrap(vars(`Sample type`), scales = "fixed")+
  geom_jitter(shape=1, position=position_jitter(0.1))


library(ggplot2)

ggplot(WQ_samples_2019_HL) +
 aes(x = month, y = `TDS [mg/L]`) +
 geom_boxplot(fill = "white") +
  labs(title = "TDS ") +
     ggthemes::theme_base()+
theme(plot.title = element_text(color="red", size=11, face="bold", vjust = -20, hjust = 0.5))+
  theme(axis.title = element_text(size=11))+
  theme(legend.title = element_text(size = 11))+
  geom_jitter(shape=1, position=position_jitter(0.1))
