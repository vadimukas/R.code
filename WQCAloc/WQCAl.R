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

#set working directory to subfolder WQCAloc
setwd("D:/R/R.code/WQCAloc")

# first load updated local measurements data

library(readr)

local_WQ <- read_csv("local_WQ.csv", col_types = cols(Month = col_factor(levels = c("1", 
                                                                                    "2", "3", "4", "5", "6", "7", "8", "9", 
                                                                                    "10", "11", "12")), Country = col_factor(levels = c("KZ", 
                                                                                                                                        "KG", "UZ", "TJ"))))
local_WQm <- read_csv("local_WQm.csv", col_types = cols(Month = col_factor(levels = c("1", 
                                                                                    "2", "3", "4", "5", "6", "7", "8", "9", 
                                                                                    "10", "11", "12")), Country = col_factor(levels = c("KZ", 
                                                                                                                                        "KG", "UZ", "TJ"))))
local_WQm_up <- read_csv("local_WQm_up.csv", col_types = cols(Month = col_factor(levels = c("1", 
                                                                                      "2", "3", "4", "5", "6", "7", "8", "9", 
                                                                                      "10", "11", "12")), Country = col_factor(levels = c("KZ", 
                                                                                                                                          "KG", "UZ", "TJ"))))
local_WQm_down <- read_csv("local_WQm_down.csv", col_types = cols(Month = col_factor(levels = c("1", 
                                                                                            "2", "3", "4", "5", "6", "7", "8", "9", 
                                                                                            "10", "11", "12")), Country = col_factor(levels = c("KZ", 
                                                                                                                                                "KG", "UZ", "TJ"))))


View(local_WQ)

library(lubridate)
local_WQd <-local_WQ %>% 
  mutate(date = make_date(Year, Month, Day))

View(local_WQd)

is.Date(local_WQd$Date)
is.Date(local_WQd$date)

summary(local_WQ)
describe(local_WQ)

CoefVar(local_WQ$`EC [microS/cm]`, na.rm = TRUE)
stat_local_WQ<-stat.desc(local_WQ [c(12:18)])

# I did it
dev.print(png, file = "NO3.png", width = 6, height = 6, units="in", res=600)
png(filename = "NO3.png", bg="transparent")
ggplot(data = local_WQd)+
  geom_point(mapping = aes(x = `EC [microS/cm]`, y = `NO+3-N [mg/L]`))+
  geom_abline(aes(intercept = -0.392218, slope =  0.006663), data =local_WQd, color = "red")+
  geom_smooth(mapping = aes(x =`EC [microS/cm]` , y = `NO+3-N [mg/L]`))+
  theme_test()

dev.off()

ggplot(data = local_WQd)+
  geom_point(mapping = aes(x = `EC [microS/cm]`, y = `NO+3-N [mg/L]`))+
  theme_test()

lm(data=local_WQd, `NO+3-N [mg/L]` ~ `EC [microS/cm]`)
ggplot(data = local_WQd)+
  geom_point(mapping = aes(x = `EC [microS/cm]`, y = `NO+3-N [mg/L]`))+
  geom_abline(aes(intercept = -0.392218, slope =  0.006663), data =local_WQd, color = "red")+
  geom_smooth(mapping = aes(x =`EC [microS/cm]` , y = `NO+3-N [mg/L]`))+
  theme_test()

# png(filename = "NO+3-N [mg/L]` vs EC [microS/cm].png", width = 1024, height=1024, units="px", res=300)

ggplot(local_WQd, aes(Month, pH))+
  geom_point()+
  theme_test()

# seem to be an interesting plot
ggplot(local_WQd, aes(Month, pH))+
  geom_violin()+
  geom_point(position = "jitter", size=1)+
  theme_test()

ggplot(local_WQd, aes(Month, pH))+
  geom_boxplot(geom_boxplot_args = list("outlier.color" = "red"))+    
  theme_test()

ggplot(local_WQd, aes(date, `EC [microS/cm]`))+
  geom_point()+
  theme_test()

ggplot(local_WQd, aes(Month, `EC [microS/cm]`))+
  geom_point(position = "jitter")+
  theme_test()

ggplot(local_WQd, aes(Month, `EC [microS/cm]`))+
  geom_boxplot(geom_boxplot_args = list("outlier.color" = "blue"))+
  theme_test()

ggplot(local_WQd, aes(date, `NO+3-N [mg/L]`))+
  geom_point()+
  theme_test()

# trying some more plots
ggplot(local_WQd, aes(Month, `EC [microS/cm]`))+
  geom_violin(color= "red")+
  geom_point(position = "jitter", size=1)+
  theme_test()

# trying some more plots
ggplot (local_WQd, aes(Month, `EC [microS/cm]`))+
  geom_boxplot(color= "red")+
  geom_point(position = "jitter", size=1)+
  theme_test()+
  facet_wrap(~Location)

# trying upstream sites 

ggplot (local_WQm_up, aes(Month, `EC [microS/cm]`))+
    geom_point(position = "jitter", size=1)+
  theme_test()+
  facet_wrap(~Location)

ggplot (local_WQm_up, aes(Month, `EC [microS/cm]`))+
  geom_boxplot(color= "blue")+
  geom_point(position = "jitter", size=1)+
  theme_test()+
  facet_wrap(~Location)

ggplot (local_WQm_up, aes(Month, `EC [microS/cm]`))+
  geom_count (color= "blue")+
  geom_point(position = "jitter", size=1)+
  theme_test()+
  facet_wrap(~Location)

# plotting figure 3 upstream EC different ways
ggplot (local_WQm_up, aes(Month, `EC [microS/cm]`))+
  geom_line (color= "blue")+
  geom_point(position = "jitter", size=1)+
  theme_test()+
  facet_wrap(~Location)

ggplot (local_WQm_up, aes(Month, `EC [microS/cm]`))+
  geom_violin (color= "blue")+
  geom_point(size=1)+
  theme_test()+
  facet_wrap(~Location)

ggplot (local_WQm_up, aes(Month, `EC [microS/cm]`))+
  geom_point(position = "jitter", size=1.1)+
  theme_test()+
  facet_wrap(~Location)

ggplot (local_WQm_up, aes(Month, `EC [microS/cm]`))+
  geom_violin ()+
  geom_point(position = "jitter", size=1, width = 0.1)+
  theme_test()+
  facet_wrap(~Location)


# figure 3 without jitter EC, NO3 and PO4 for upstream - NOW this WORKING Version
ggplot (local_WQm_up, aes(Month, `EC [microS/cm]`))+
  geom_line (color="black")+
  geom_point(size=1)+
  theme_test()+
  facet_wrap(~Location)

ggplot (local_WQm_up, aes(Month, `NO+3-N [mg/L]`))+
  geom_line (color="red")+
  geom_point(size=1)+
  theme_test()+
  facet_wrap(~Location)

ggplot (local_WQm_up, aes(Month, `PO3-4-P [mg/L]`))+
  geom_line (color="blue")+
  geom_point(size=1)+
  theme_test()+
  facet_wrap(~Location)


# figure 3 without jitter EC, NO3 and PO4 for downstream - NOW this WORKING Version 

ggplot (local_WQm_down, aes(Month, `EC [microS/cm]`))+
  geom_line (color="black")+
  geom_point(size=1)+
  theme_test()+
  facet_wrap(~Location)

ggplot (local_WQm_down, aes(Month, `NO+3-N [mg/L]`))+
  geom_line (color="red")+
  geom_point(size=1)+
  theme_test()+
  facet_wrap(~Location)

ggplot (local_WQm_down, aes(Month, `PO3-4-P [mg/L]`))+
  geom_line (color="blue")+
  geom_point(size=1)+
  theme_test()+
  facet_wrap(~Location)

# the end of figure #3

# figure 3 without jitter EC, NO3 and PO4 for UPSTREAM - NOW this WORKING Version
# this time  making a figure 3 on a single pane as a grid

f3a <- local_WQm_up %>% 
  ggplot()+
  aes(Month, `EC [microS/cm]`)+
  geom_line (color="black")+
  geom_point(size=1)+
  theme_test()+
  facet_wrap(~Location)

fig_3a <- local_WQm_up %>% 
  ggplot()+
  aes(Month, `EC [microS/cm]`)+
  geom_line (color="black")+
  geom_point(size=1)+
  theme_test()+
  facet_wrap(~Location) 
ggsave(fig_3a, filename = "fig_3a.png", width = 5, height = 4, units="in", dpi = 300)
dev.off()

f3b <- local_WQm_up %>% 
  ggplot()+
  aes(Month, `NO3-N [mg/L]`)+
  geom_line (color="red")+
  geom_point(size=1)+
  theme_test()+
  facet_wrap(~Location)

fig_3b <- local_WQm_up %>% 
  ggplot()+
  aes(Month, `NO3-N [mg/L]`)+
  geom_line (color="red")+
  geom_point(size=1)+
  theme_test()+
  facet_wrap(~Location)
ggsave(fig_3b, filename = "fig_3b.png", width = 5, height = 4, units="in", dpi = 300)
dev.off()

f3c <- local_WQm_up %>% 
  ggplot()+
  aes(Month, `PO4-P [mg/L]`)+
  geom_line (color="blue")+
  geom_point(size=1)+
  theme_test()+
  facet_wrap(~Location)

fig_3c <- local_WQm_up %>% 
  ggplot()+
  aes(Month, `PO4-P [mg/L]`)+
  geom_line (color="blue")+
  geom_point(size=1)+
  theme_test()+
  facet_wrap(~Location)
ggsave(fig_3c, filename = "fig_3c.png", width = 5, height = 4, units="in", dpi = 300)
dev.off()

# figure 3 for a pane second column - DOWNSTREAM

f3d <- local_WQm_down %>%
ggplot () +
  aes(Month, `EC [microS/cm]`)+
  geom_line (color="black")+
  geom_point(size=1)+
  theme_test()+
  facet_wrap(~Location)

fig_3d <- local_WQm_down %>%
  ggplot () +
  aes(Month, `EC [microS/cm]`)+
  geom_line (color="black")+
  geom_point(size=1)+
  theme_test()+
  facet_wrap(~Location)

ggsave(fig_3d, filename = "fig_3d.png", width = 5, height = 4, units="in", dpi = 300)
dev.off()

f3e <- local_WQm_down %>%
ggplot ()+
  aes(Month, `NO3-N [mg/L]`)+
  geom_line (color="red")+
  geom_point(size=1)+
  theme_test()+
  facet_wrap(~Location)

fig_3e <- local_WQm_down %>%
  ggplot ()+
  aes(Month, `NO3-N [mg/L]`)+
  geom_line (color="red")+
  geom_point(size=1)+
  theme_test()+
  facet_wrap(~Location)
ggsave(fig_3e, filename = "fig_3e.png", width = 5, height = 4, units="in", dpi = 300)
dev.off()

f3f <- local_WQm_down %>%
  ggplot ()+
  aes(Month, `PO4-P [mg/L]`)+
  geom_line (color="blue")+
  geom_point(size=1)+
  theme_test()+
  facet_wrap(~Location)

fig_3f <- local_WQm_down %>%
  ggplot ()+
  aes(Month, `PO4-P [mg/L]`)+
  geom_line (color="blue")+
  geom_point(size=1)+
  theme_test()+
  facet_wrap(~Location)
ggsave(fig_3f, filename = "fig_3f.png", width = 5, height = 4, units="in", dpi = 300)
dev.off()


grid.arrange(f3a, f3b, f3c, f3d, f3e, f3f, nrow = 2, ncol = 3)

# or save as file
fig_3<-grid.arrange(f3a, f3b, f3c, f3d, f3e, f3f, nrow = 3, ncol = 2)
ggsave(fig_3, filename = "fig_3.png", dpi = 600)
dev.off()


#dotplots 
ggplot (local_WQm_up, aes(Month, `EC [microS/cm]`))+
  geom_dotplot (color= "blue",binaxis = "y",tackdir = "center", binpositions = "all")+
  geom_smooth()+
  theme_test()+
  facet_wrap(~Location)

# now explore local combined dataset
summary(local_WQ)
stat_local_WQ_<-stat.desc(local_WQ [c(11:16)])
plot_boxplot(local_WQ [c(10, 11:16)], by = "Sample type", title = "local WQ by sample type", ggtheme = theme_test(), geom_boxplot_args = list("outlier.color" = "red"))
plot_boxplot(local_WQ [c(5, 11:16)], by = "Month", title = "local_WQ by Month", ggtheme = theme_test(),geom_boxplot_args = list("outlier.color" = "blue"))



pairs.panels(local_WQ [c(13:18)],
             method="pearson",
             hist.col="green",
             density=TRUE,
             main = "local WQ correlations, Peason"
)
pairs.panels(local_WQ[c(13:18)],
             method="kendall",
             hist.col="#00AFBB",
             density=TRUE,
             main = "local WQ correlations, Kendall"
)         

# make summary statistics for bubble maps using describeBy function from psych package
summary_by_location <-describeBy(local_WQ, group = "Location", mat = TRUE)
# write output in txt file
write.table(summary_by_location, file = 'summary_by_location.txt', sep = ' ')


local_WQ_str <- read_csv("local_WQ_str.csv", 
                         col_types = cols(Sample_n = col_integer(), 
                                          Month = col_factor(levels = c("1", 
                                                                        "2", "3", "4", "5", "6", "7", 
                                                                        "8", "9", "10", "11", "12")), 
                                          Country = col_factor(levels = c("KZ", 
                                                                          "KG", "UZ", "TJ"))))
View(local_WQ_str)


plot_boxplot(local_WQ_str [c(4, 11:16)], by = "Month", title = "local_WQ in streamwater by Month", ggtheme = theme_test(),geom_boxplot_args = list("outlier.color" = "blue"))

# make some plots exploring the data of parameters in streams
plot_scatterplot (local_WQ_str [c(4, 11:16)], by = "Month", ggtheme = theme_test()) 
plot_scatterplot (local_WQ_str [c(6, 11:16)], by = "Country",ggtheme = theme_test())

plot_scatterplot (local_WQ_str [c(5, 13)], by = "Location", ggtheme = theme_test())
plot_scatterplot (local_WQ_str [c(5, 14)], by = "Location", ggtheme = theme_test())
plot_boxplot (local_WQ_str [c(5, 15)], by = "Location", ggtheme = theme_test())
plot_boxplot (local_WQ_str [c(5, 16)], by = "Location", ggtheme = theme_test())
plot_boxplot (local_WQm [c(9, 17)], by = "Site", ggtheme = theme_test())
plot_boxplot (local_WQm [c(9, 18)], by = "Site", ggtheme = theme_test())

# vertical panel of boxplots by site id
plot_boxplot (local_WQm [c(9, 14)], by = "Site", ggtheme = theme_test())
plot_boxplot (local_WQm [c(9, 15)], by = "Site", ggtheme = theme_test())
plot_boxplot (local_WQm [c(9, 16)], by = "Site", ggtheme = theme_test())

pairs.panels(local_WQ_str [c(11:16)],
             method="pearson",
             hist.col="green",
             density=TRUE,
             main = "local WQ in streams correlations, Peason"
)

pairs.panels(local_WQ_str [c(11:16)],
             method="kendall",
             hist.col="green",
             density=TRUE,
             main = "local WQ in streams correlations, Kendall"
)

# fitting the EC vs NO3-N only for streamwater data with linear and smooth curves

lm(data=local_WQ_str, `NO+3-N [mg/L]`~ `EC [microS/cm]`)
ggplot(data = local_WQ_str)+
  geom_point(mapping = aes(x = `EC [microS/cm]`, y = `NO+3-N [mg/L]`))+
  geom_abline(aes(intercept = 0.254, slope =  0.00404), data =local_WQ_str, color = "red")+
  geom_smooth(mapping = aes(x =`EC [microS/cm]` , y = `NO+3-N [mg/L]`))+
  theme_test()

# making time series for bubble plots
# starting with Phosphate in KG 
library(readr)
Phosphate_KG_TS <- read_csv("Phosphate KG TS.csv", 
                            col_types = cols(year = col_integer(), 
                                             month = col_integer(), day = col_integer()))

library(lubridate)
Phosphate_KG_TSd <-Phosphate_KG_TS %>% 
  mutate(Date = make_date(year, month, day))

is.Date(Phosphate_KG_TSd$Date)


library(ggplot2)

library(dplyr)
Phosphate_KG_TSd <- Phosphate_KG_TSd %>%
  select(Date, `KG-1`, `KG-5`) %>%
  gather(key = "variable", value = "value", -Date)

fig_4c<-ggplot(Phosphate_KG_TSd, aes(x = Date, y = value)) + 
  geom_line(aes(color = variable), size = 0.7, show.legend = FALSE) +
  geom_point(size = 1.5, color= "black") +
  scale_color_manual(values = c("blue", "red")) +
   labs(y = "P04-P [mg/L]") +
  scale_x_date(date_labels = "%m-%y", date_breaks = "1 month")+
      ggthemes::theme_base()+
  theme(text=element_text(size = 12),axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(fig_4c, filename = "fig_4c.png", width = 6, height = 4, units="in", dpi = 300)
dev.off()

library(lattice)

xyplot(`KG-1`+`KG-5` ~ Date, data = Phosphate_KG_TSd, type = "l", auto.key = TRUE)

ggplot(Phosphate_KG_TSd) +
 aes(x = Date, y = `KG-1`) +
 geom_line(size = 0.5, colour = "blue") +
  geom_point(size = 1.5, color = "blue")+
 labs(y = "P04-P [mg/L]") +
  scale_x_date(date_labels = "%m-%y", date_breaks = "2 month")+
  theme(axis.text.x=element_text(angle=60, hjust=1))+ 
 ggthemes::theme_base()

ggplot(Phosphate_KG_TSd) +
 aes(x = Date, y = `KG-5`) +
 geom_line(size = 1L, colour = "#0c4c8a") +
 labs(y = "P04-P [mg\\L]") +
 ggthemes::theme_base()

# continue with nitrate in Groundwater TJ-6
Nitrate_TJ_TS <- read_csv("Nitrate TJ TS.csv", 
                            col_types = cols(year = col_integer(), 
                                             month = col_integer(), day = col_integer()))

library(lubridate)
Nitrate_TJ_TSd <-Nitrate_TJ_TS %>% 
  mutate(Date = make_date(year, month, day))

is.Date(Nitrate_TJ_TSd$Date)


fig_4b<-ggplot(Nitrate_TJ_TSd, aes(x = Date, y = `TJ-6 NO3-N [mg/L]`)) + 
  geom_line(size = 0.7, color="red") +
  geom_point(size = 1.5, color= "black") +
  labs(y = "N03-N [mg/L]", title = "TJ-6") +
  scale_x_date(date_labels = "%m-%y", date_breaks = "1 month")+
  geom_hline(yintercept = 10, color="red", size= 0.5, linetype="dashed")+
  ggthemes::theme_base()+
  theme(text=element_text(size = 12),axis.text.x = element_text(angle = 45, hjust = 1), plot.title = element_text(color="red", size=12, face="plain", vjust = -20, hjust = 0.35))

ggsave(fig_4b, filename = "fig_4b.png", width = 6, height = 4, units="in", dpi = 300)
dev.off()

# and finishing with EC for KZ site in streamwater 
EC_KZ_TS <- read_csv("EC KZ TS.csv", 
                          col_types = cols(year = col_integer(), 
                                           month = col_integer(), day = col_integer()))

library(lubridate)
EC_KZ_TSd <-EC_KZ_TS %>% 
  mutate(Date = make_date(year, month, day))

is.Date(EC_KZ_TSd$Date)

library(ggplot2)

library(dplyr)
EC_KZ_TSd <- EC_KZ_TSd %>%
  select(Date, `KZ-8`, `KZ-9`) %>%
  gather(key = "variable", value = "value", -Date)

fig_4a<-ggplot(EC_KZ_TSd, aes(x = Date, y = value)) + 
  geom_line(aes(color = variable), size = 0.7, show.legend = FALSE) +
  geom_point(size = 1.5, color= "black") +
  scale_color_manual(values = c("red", "blue")) +
  labs(y = "EC [microS/cm]") +
  scale_x_date(date_labels = "%m-%y", date_breaks = "1 month")+
  ggthemes::theme_base()+
  theme(text=element_text(size = 12),axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(fig_4a, filename = "fig_4a.png", width = 6, height = 4, units="in", dpi = 300)
dev.off()


# Making a summary table of local WQ data
# What are the options?

install.packages("flextable")
library(flextable)

install.packages("ExPanDaR")
library(ExPanDaR)
# export data and code to a notebook
ExPanD(local_WQ, export_nb_option = TRUE)

# open a shiny app
ExPanD(local_WQ) 

install.packages("skimr")
library(skimr)
skim(local_WQ)

library(DescTools)
Desc(local_WQ, plotit = TRUE)

install.packages("stargazer")
library(stargazer)

stargazer(local_WQm, type = "text", title = "Descriptive stats", digits = 1, out = "table.txt")
stargazer(local_WQ, type = "text", title = "Descriptive stats", digits = 1, summary = FALSE)




