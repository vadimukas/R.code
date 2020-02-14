# exploring a combined WQ data for high low flow comparison, first load the required packages
library(readr) 
library(ggplot2)
library(tidyverse)
library(DataExplorer)
library(psych)
library(DescTools)
library(pastecs)

#set working directory to subfolder WQCAn
setwd("D:/R/R.code/WQCAn")

# first load ICP data

View(WQ_samples__2019_HL)

plot_boxplot(WQ_samples_2019_HL [c(7, 10:34)], by = "Sample type", title = "WQ_samples__2019_HL sample type", ggtheme = theme_test(), geom_boxplot_args = list("outlier.color" = "red"))
plot_boxplot(WQ_samples_2019_HL [c(3, 10:34)], by = "month", title = "WQ_samples__2019_HL by Month", ggtheme = theme_test(),geom_boxplot_args = list("outlier.color" = "blue"))
plot_boxplot(WQ_samples_2019_HL [c(8, 10:34)], by = "Country", title = "WQ_samples__2019_HL by Country", ggtheme = theme_test(),geom_boxplot_args = list("outlier.color" = "blue"))
WQ_samples_2019_HL_sorted<-WQ_samples_2019_HL %>% arrange(month)



plot_boxplot(WQ_samples_2019_HL [c(3, 10:34)], by = "month", title = "WQ_samples_2019_HL by Month", ggtheme = theme_bw())

library(dplyr)
library(ggplot2)

# import data a bit differently
WQ_samples_2019_HL <- read_csv("WQ_samples__2019_HL.csv", 
                               col_types = cols(Country = col_factor(levels = c("TJ", 
                                                                                "UZ", "KG", "KZ")), month = col_factor()))

WQ_samples_2019_HL %>%
 filter(!(month %in% "5")) %>%
 ggplot() +
 aes(x = Period, y = `TDS [mg/L]`) +
 geom_boxplot(fill = "#ffffff", outlier.colour="red") +
 labs(title = "TDS difference by periods") +
  ggthemes::theme_base()+
geom_jitter(shape=1, position=position_jitter(0.2))


library(dplyr)
library(ggplot2)

WQ_samples_2019_HL %>%
  filter(`Sample type` %in% 
    c("streamwater", "groundwater", "reservoir")) %>%
 ggplot() +
 aes(x = Period, y = `TDS [mg/L]`) +
 geom_boxplot(fill = "#ffffff", outlier.colour="red") +
  geom_jitter(shape=16, position=position_jitter(0.2))+
  labs(title = "TDS difference by periods and sources") +
 ggthemes::theme_base() +
 facet_wrap(vars(`Sample type`))

WQ_samples_2019_HL %>%
    filter(`Sample type` %in% 
           c("streamwater", "groundwater", "reservoir")) %>%
  ggplot() +
  aes(x = Period, y = `NO+3-N [mg/L]`) +
  geom_boxplot(fill = "#ffffff", outlier.colour="red") +
  geom_jitter(shape=16, position=position_jitter(0.2))+
  labs(title = "NO3 difference by periods and sources") +
  ggthemes::theme_base() +
  facet_wrap(vars(`Sample type`))

WQ_samples_2019_HL %>%
  filter(`Sample type` %in% 
           c("streamwater", "groundwater", "reservoir")) %>%
  ggplot() +
  aes(x = Period, y = `PO3-4-P [mg/L]`) +
  geom_boxplot(fill = "#ffffff", outlier.colour="red") +
  geom_jitter(shape=16, position=position_jitter(0.2))+
  labs(title = "PO3-4-P  difference by periods and sources") +
  ggthemes::theme_base() +
  facet_wrap(vars(`Sample type`))

WQ_samples_2019_HL %>%
  filter(`Sample type` %in% 
           c("streamwater", "groundwater", "reservoir")) %>%
  ggplot() +
  aes(x = Period, y = `Ca [mg/l]`) +
  geom_boxplot(fill = "#ffffff", outlier.colour="red") +
  geom_jitter(shape=16, position=position_jitter(0.2))+
  labs(title = "Ca  difference by periods and sources") +
  ggthemes::theme_base() +
  facet_wrap(vars(`Sample type`))

WQ_samples_2019_HL %>%
  filter(`Sample type` %in% 
           c("streamwater", "groundwater", "reservoir")) %>%
  ggplot() +
  aes(x = Period, y = `Mg [mg/L]`) +
  geom_boxplot(fill = "#ffffff", outlier.colour="red") +
  geom_jitter(shape=('Country'), position=position_jitter(0.2))+
  labs(title = "Mg  difference by periods and sources") +
  ggthemes::theme_base() +
  facet_wrap(vars(`Sample type`))

WQ_samples_2019_HL %>%
 filter(!(month %in% "5")) %>%
 filter(!(`Sample type` %in% 
    c("drinking", "icemelt"))) %>%
 ggplot() +
 aes(x = Period, y = `Al [ug/L]`, fill = Country) +
 geom_boxplot(outlier.colour="red") +
 scale_fill_viridis_d(option = "viridis") +
 labs(fill = "Country") +
 ggthemes::theme_base() +
 facet_wrap(vars(`Sample type`), scales = "free_y")

WQ_samples_2019_HL %>%
 filter(!(month %in% "5")) %>%
 filter(`Sample type` %in% 
    c("streamwater", "groundwater", "reservoir")) %>%
 ggplot() +
 aes(x = Period, y = `Fe-Tot [mg/L]`, fill = Country) +
 geom_boxplot() +
 scale_fill_hue() +
 labs(title = "Fe ICP ", fill = "Country") +
 ggthemes::theme_base() +
 theme(legend.position = "none") +
 facet_wrap(vars(`Sample type`), scales = "free_y")+
geom_jitter(shape=1, position=position_jitter(0.2))

WQ_samples_2019_HL %>%
 filter(!(month %in% "5")) %>%
 filter(`Sample type` %in% 
    c("streamwater", "groundwater", "reservoir")) %>%
 ggplot() +
 aes(x = Period, y = `Fe-Tot [mg/L]`, colour = Country) +
 geom_boxplot(fill = "#ffffff") +
 scale_color_hue() +
 labs(title = "Fe ICP ") +
 ggthemes::theme_base() +
 facet_wrap(vars(`Sample type`), scales = "free_y")+
  geom_jitter(shape=1, position=position_jitter(0.2))

WQ_samples_2019_HL %>%
 filter(!(month %in% "5")) %>%
 filter(`Sample type` %in% 
    c("streamwater", "groundwater", "reservoir")) %>%
 ggplot() +
 aes(x = Period, y = `Fe-Tot [mg/L]`, colour = Country) +
 geom_boxplot(fill = "#ffffff") +
 scale_color_hue() +
 labs(title = "Fe ICP ") +
 ggthemes::theme_base() +
 facet_wrap(vars(`Sample type`), scales = "free_y")+
geom_jitter(shape=1, position=position_jitter(0.2))

library(dplyr)
library(ggplot2)

WQ_samples_2019_HL %>%
 filter(!(month %in% "5")) %>%
 filter(`Sample type` %in% 
    c("streamwater", "groundwater", "reservoir")) %>%
    ggplot() +
 aes(x = Period, y = `TDS [mg/L]`) +
 geom_boxplot(fill = "#ffffff") +
 ggthemes::theme_base()+
  geom_jitter(shape=16, position=position_jitter(0.2))


# try another way to import data with months as integers and Country as factor

library(ggplot2)

ggplot(WQ_samples_2019_HL) +
 aes(x = Period, y = `TDS [mg/L]`, fill = Country) +
 geom_boxplot() +
 scale_fill_hue() +
  ggthemes::theme_base()+
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
  filter(!(month %in% "5")) %>%
  filter(`Sample type` %in% 
           c("streamwater", "groundwater", "reservoir")) %>%
  ggplot() +
  aes(x = Period, y = `TDS [mg/L]`, colour = Country) +
  geom_boxplot(fill = "#ffffff") +
  scale_color_hue() +
  labs(title = "TDS by country") +
  ggthemes::theme_base() +
  facet_wrap(vars(`Sample type`), scales = "free_y")+
  geom_jitter(shape=1, position=position_jitter(0.1))


library(ggplot2)

ggplot(WQ_samples_2019_HL) +
 aes(x = month, y = `TDS [mg/L]`) +
 geom_boxplot(fill = "#0c4c8a") +
  ggthemes::theme_base()



