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
library(xts)

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
library(readr)
local_WQm <- read_csv("local_WQm.csv", col_types = cols(Month = col_factor(levels = c("1", 
                                                                                      "2", "3", "4", "5", "6", "7", "8", "9", 
                                                                                      "10", "11", "12")),
                                                        Country = col_factor(levels = c("KZ", "KG", "UZ", "TJ")),
                                                        `Sample description` = col_factor(levels = c("KZ-1", "KZ-2", "KZ-3", "KZ-4", "KZ-5", "KZ-6", "KZ-12", "KG-1", "KG-2", "KG-3", "KG-4",                                                                                                                                                                                     "KZ-7", "KZ-8", "KZ-9", "KZ-10", "KZ-11", 
                                                                                                     "KG-5", "KG-6", "KG-7", "UZ-1", "UZ-2",  "UZ-3", "UZ-4", "UZ-5", "UZ-6", "UZ-7",                                                                                                                                                                                                           
                                                                                                     "UZ-8", "UZ-9", "UZ-10", "TJ-1", "TJ-2",  "TJ-3", "TJ-4", "TJ-5", "TJ-6", "TJ-7"))))                                                                                                       
                                                                                                                                                                                                          
                                                                                                                                                                                                           
                                                                                                                                                                                                          
View(local_WQm)
local_WQm_up <- read_csv("local_WQm_up.csv", col_types = cols(Month = col_factor(levels = c("1", 
                                                                                      "2", "3", "4", "5", "6", "7", "8", "9", 
                                                                                      "10", "11", "12")), Country = col_factor(levels = c("KZ", 
                                                                                                                                          "KG", "UZ", "TJ"))))
local_WQm_down <- read_csv("local_WQm_down.csv", col_types = cols(Month = col_factor(levels = c("1", 
                                                                                            "2", "3", "4", "5", "6", "7", "8", "9", 
                                                                                            "10", "11", "12")), Country = col_factor(levels = c("KZ", 
                                                                                                                                                "KG", "UZ", "TJ"))))
local_WQm_down2 <- read_csv("local_WQm_down2.csv", col_types = cols(Month = col_factor(levels = c("1", 
                                                                                                "2", "3", "4", "5", "6", "7", "8", "9", 
                                                                                                "10", "11", "12")), Country = col_factor(levels = c("KZ", 
                                                                                                                                                    "KG", "UZ", "TJ"))))

library(lubridate)
local_WQd <-local_WQm %>% 
  mutate(date = make_date(Year, Month, Day))





# experimenting with Date 
local_WQt <- read_csv("local_WQm.csv", col_types = cols(Month = col_factor(levels = c("1", 
                                                                                    "2", "3", "4", "5", "6", "7", "8", "9", 
                                                                                    "10", "11", "12")), Country = col_factor(levels = c("KZ", 
                                                                                                                "KG", "UZ", "TJ"))))


library(lubridate)
date<-parse_date_time(local_WQm$Date, order = "dmy")
is.POSIXct(date)

local_WQt <- read.delim('local_WQm.txt')
local_WQt <- read.delim('local_WQm.txt')
Sys.timezone()
local_WQt$Date<-as.POSIXct(local_WQt$Date, tz="Asia/Dhaka", format = "%Y-%m-%d")

local_WQx$Date<-parse_date_time(local_WQm$Date, order = "dmy")
is.POSIXct(local_WQx$Date)

local_WQt$Date<-as.POSIXct(local_WQm$Date, tz="Asia/Dhaka")
tz(local_WQt$Date)
is.POSIXct(local_WQt$Date)
is.Date(local_WQt$Date)
as.Date(local_WQt$Date)
is.Date(local_WQt$Date)

head(local_WQt)

local_WQm %>% parse_date_time(Date, order = "dmy", tz = "UTC")  
is.Date(local_WQx$date)
is.POSIXct(local_WQx$date)
is.Date(local_WQx$Date)
is.POSIXct(local_WQx$Date)

mutate(date=parse_date_time(local_WQm$Date, order = "dmy"))  

library(anytime)
anydate(local_WQm$Date)

is.Date(local_WQm$Date)

View(local_WQd)

is.Date(local_WQd$Date)
is.Date(local_WQd$date)

summary(local_WQm)
describe(local_WQm)

CoefVar(local_WQm$`EC [microS/cm]`, na.rm = TRUE)
stat_local_WQ<-stat.desc(local_WQm [c(12:18)])

# I did it
dev.print(png, file = "NO3.png", width = 6, height = 6, units="in", res=600)
png(filename = "NO3.png", bg="transparent")
ggplot(data = local_WQd)+
  geom_point(mapping = aes(x = `EC [microS/cm]`, y = `NO3-N [mg/L]`))+
  geom_abline(aes(intercept = -0.392218, slope =  0.006663), data =local_WQd, color = "red")+
  geom_smooth(mapping = aes(x =`EC [microS/cm]` , y = `NO3-N [mg/L]`))+
  theme_test()

dev.off()

ggplot(data = local_WQd)+
  geom_point(mapping = aes(x = `EC [microS/cm]`, y = `NO3-N [mg/L]`))+
  theme_test()

lm(data=local_WQd, `NO3-N [mg/L]` ~ `EC [microS/cm]`)
ggplot(data = local_WQd)+
  geom_point(mapping = aes(x = `EC [microS/cm]`, y = `NO3-N [mg/L]`))+
  geom_abline(aes(intercept = -0.390938, slope =  0.006602), data =local_WQd, color = "red")+
  geom_smooth(mapping = aes(x =`EC [microS/cm]` , y = `NO3-N [mg/L]`))+
  theme_test()

lm(data=local_WQd, `EC [microS/cm]`~ `NO3-N [mg/L]`)
ggplot(data = local_WQd)+
  geom_point(mapping = aes(x = `NO3-N [mg/L]`, y = `EC [microS/cm]`))+
  geom_abline(aes(intercept = 224.86, slope =  70.77), data =local_WQd, color = "red")+
  geom_smooth(mapping = aes(x = `NO3-N [mg/L]`, y = `EC [microS/cm]`))+
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

ggplot(local_WQd, aes(date, `NO3-N [mg/L]`))+
  geom_point()+
  theme_test()

# trying some more plots
ggplot(local_WQd, aes(Month, `EC [microS/cm]`))+
  geom_violin(color= "red")+
  geom_point(position = "jitter", size=1)+
  theme_test()

# The boxplots of EC across all sites by faceted by month
figEC<-ggplot (local_WQd, aes(Month, `EC [microS/cm]`))+
  geom_boxplot(color= "black")+
  geom_point(position = "jitter", size=0.5)+
  theme_test()+
  facet_wrap(~Location)
ggsave(figEC, filename = "fig_EC.png", width = 8, height = 7, units="in", dpi = 600)
dev.off()

figEC<-ggplot (local_WQd, aes(Month, `EC [microS/cm]`))+
  geom_boxplot(color= "red")+
  geom_point(position = "jitter", size=0.5)+
  theme_test()+
  facet_wrap(~Site)
ggsave(figEC, filename = "fig_EC.png", width = 9, height = 8, units="in", dpi = 600)
dev.off()

# The boxplots of NO3-N across all sites by faceted by month
figNO3<-ggplot (local_WQd, aes(Month, `NO3-N [mg/L]`))+
  geom_boxplot(color= "red")+
  geom_point(position = "jitter", size=0.5)+
  theme_test()+
  facet_wrap(~Location)
ggsave(figNO3, filename = "fig_NO3.png", width = 8, height = 7, units="in", dpi = 600)
dev.off()

# The boxplots of NO3-N across all sites by faceted by month
figPO4<-ggplot (local_WQd, aes(Month, `PO4-P [mg/L]`))+
  geom_boxplot(color= "blue")+
  geom_point(position = "jitter", size=0.5, color= "black")+
  theme_test()+
  facet_wrap(~Location)
ggsave(figPO4, filename = "fig_PO4.png", width = 8, height = 7, units="in", dpi = 600)
dev.off()


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

fig3a <- local_WQm_up %>% 
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
  facet_wrap(~Site) 
ggsave(fig_3a, filename = "fig_3a.png", width = 5, height = 4, units="in", dpi = 300)
dev.off()

f3c <- local_WQm_up %>% 
  ggplot()+
  aes(Month, `NO3-N [mg/L]`)+
  geom_line (color="red")+
  geom_point(size=1)+
  theme_test()+
  facet_wrap(~Location)

fig_3c <- local_WQm_up %>% 
  ggplot()+
  aes(Month, `NO3-N [mg/L]`)+
  geom_line (color="red")+
  geom_point(size=1)+
  theme_test()+
  facet_wrap(~Site)
ggsave(fig_3c, filename = "fig_3c.png", width = 5, height = 4, units="in", dpi = 300)
dev.off()

f3e <- local_WQm_up %>% 
  ggplot()+
  aes(Month, `PO4-P [mg/L]`)+
  geom_line (color="blue")+
  geom_point(size=1)+
  theme_test()+
  facet_wrap(~Location)

fig_3e <- local_WQm_up %>% 
  ggplot()+
  aes(Month, `PO4-P [mg/L]`)+
  geom_line (color="blue")+
  geom_point(size=1)+
  theme_test()+
  facet_wrap(~Site)
ggsave(fig_3e, filename = "fig_3e.png", width = 5, height = 4, units="in", dpi = 300)
dev.off()

# figure 3 for a pane second column - DOWNSTREAM

f3b <- local_WQm_down %>%
ggplot () +
  aes(Month, `EC [microS/cm]`)+
  geom_line (color="black")+
  geom_point(size=1)+
  theme_test()+
  facet_wrap(~Location)

fig_3b <- local_WQm_down %>%
  ggplot () +
  aes(Month, `EC [microS/cm]`)+
  geom_line (color="black")+
  geom_point(size=1)+
  theme_test()+
  facet_wrap(~Site)
ggsave(fig_3b, filename = "fig_3b.png", width = 5, height = 4, units="in", dpi = 300)
dev.off()

f3d <- local_WQm_down %>%
ggplot ()+
  aes(Month, `NO3-N [mg/L]`)+
  geom_line (color="red")+
  geom_point(size=1)+
  theme_test()+
  facet_wrap(~Location)

fig_3d <- local_WQm_down %>%
  ggplot ()+
  aes(Month, `NO3-N [mg/L]`)+
  geom_line (color="red")+
  geom_point(size=1)+
  theme_test()+
  facet_wrap(~Site)
ggsave(fig_3d, filename = "fig_3d.png", width = 5, height = 4, units="in", dpi = 300)
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
  facet_wrap(~Site)
ggsave(fig_3f, filename = "fig_3f.png", width = 5, height = 4, units="in", dpi = 300)
dev.off()


fig_3v1<-grid.arrange(fig_3a, fig_3b, fig_3c, fig_3d, fig_3e, fig_3f, nrow = 3, ncol = 2)
ggsave(fig_3v1, filename = "fig_3v1.png", width = 10, height = 9, units="in", dpi = 600)
dev.off()

# figure 3 for a pane second column - DOWNSTREAM with Otegen Batyr

f3b2 <- local_WQm_down2 %>%
  ggplot () +
  aes(Month, `EC [microS/cm]`)+
  geom_line (color="black")+
  geom_point(size=1)+
  theme_test()+
  facet_wrap(~Location)

fig_3b2 <- local_WQm_down2 %>%
  ggplot () +
  aes(Month, `EC [microS/cm]`)+
  geom_line (color="black")+
  geom_point(size=1)+
  theme_test()+
  facet_wrap(~Site)
ggsave(fig_3b2, filename = "fig_3b2.png", width = 5, height = 4, units="in", dpi = 300)
dev.off()

f3d2 <- local_WQm_down2 %>%
  ggplot ()+
  aes(Month, `NO3-N [mg/L]`)+
  geom_line (color="red")+
  geom_point(size=1)+
  theme_test()+
  facet_wrap(~Location)

fig_3b2 <- local_WQm_down2 %>%
  ggplot ()+
  aes(Month, `NO3-N [mg/L]`)+
  geom_line (color="red")+
  geom_point(size=1)+
  theme_test()+
  facet_wrap(~Site)
ggsave(fig_3d2, filename = "fig_3d2.png", width = 5, height = 4, units="in", dpi = 300)
dev.off()

f3f2 <- local_WQm_down2 %>%
  ggplot ()+
  aes(Month, `PO4-P [mg/L]`)+
  geom_line (color="blue")+
  geom_point(size=1)+
  theme_test()+
  facet_wrap(~Location)

fig_3f2 <- local_WQm_down2 %>%
  ggplot ()+
  aes(Month, `PO4-P [mg/L]`)+
  geom_line (color="blue")+
  geom_point(size=1)+
  theme_test()+
  facet_wrap(~Site)
ggsave(fig_3f2, filename = "fig_3f2.png", width = 5, height = 4, units="in", dpi = 300)
dev.off()

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

pairs.panels(local_WQ_str [c(13:18)],
             method="kendall",
             hist.col="green",
             density=TRUE,
             main = "local WQ in streams correlations, Kendall"
)

# fitting the EC vs NO3-N only for streamwater data with linear and smooth curves

head (local_WQ_str)

lmNO3N<-lm(data=local_WQ_str, `EC [microS/cm]` ~ `NO3-N [mg/L]`)

summary(lmNO3N)$r.squared
print(corr.test(local_WQ_strm$`NO3-N [mg/L]`, local_WQ_strm$`EC [microS/cm]`,
                use = "pairwise", method = "kendall", alpha = .05, ci = TRUE), short = FALSE) 

c1<-ggplot(data = local_WQ_str)+
  geom_point(mapping = aes(x =`NO3-N [mg/L]` , y = `EC [microS/cm]`))+
  geom_abline(aes(intercept = 154.6, slope =  120.4), data =local_WQ_str, color = "red")+
  geom_smooth(mapping = aes(x =`NO3-N [mg/L]`, y = `EC [microS/cm]`), method = "loess", se = )+
  labs(title = expression(R^`2` ~'='~ '0.5165'))+
  xlab(expression(NO[3]-N~mg~L^-1))+
  ylab(expression(paste("EC ("~uS~cm^-1,")")))+
  ggthemes::theme_base() +
  theme(plot.title = element_text(color="black", size=10, face="bold", vjust = -20, hjust = 0.70))+
  theme(axis.title = element_text(size=11))+
  theme(legend.title = element_text(size = 11))


ggsave(c1, filename = "fig_0.png", width = 6, height = 5, units="in", dpi = 600)
  

local_WQ_str <-local_WQ_str %>% 
  mutate(`NO3 [mg/L]`=`NO3-N [mg/L]` * 4.427)
lmNO3<-lm(data=local_WQ_strm, `EC [microS/cm]` ~ `NO3 [mg/L]`)
summary(lmNO3)$r.squared


ggplot(data = local_WQ_strm)+
  geom_point(mapping = aes(x =`NO3 [mg/L]` , y = `EC [microS/cm]`))+
  geom_abline(aes(intercept = 154.61, slope =  27.19), data =local_WQ_str, color = "red")+
  geom_smooth(mapping = aes(x =`NO3 [mg/L]`, y = `EC [microS/cm]`))+
  theme_test()

# and for Phosphate
lmPO4P<-lm(data=local_WQ_str, `EC [microS/cm]` ~ `PO4-P [mg/L]`)

summary(lmPO4P)$r.squared
print(corr.test(local_WQ_strm$`PO4-P [mg/L]`, local_WQ_strm$`EC [microS/cm]`,
                use = "pairwise", method = "kendall", alpha = .05, ci = TRUE), short = FALSE) 

c2<-ggplot(data = local_WQ_str)+
  geom_point(mapping = aes(x =`PO4-P [mg/L]` , y = `EC [microS/cm]`))+
  geom_abline(aes(intercept = 369.64, slope =  -13.88), data =local_WQ_str, color = "red")+
  geom_smooth(mapping = aes(x =`PO4-P [mg/L]`, y = `EC [microS/cm]`))+
  labs(title = expression(R^`2` ~'='~ '0.0002'))+
  xlab(expression(PO[4]- P~mg~L^-1))+
  ylab(expression(paste("EC ("~uS~cm^-1,")")))+
  ggthemes::theme_base() +
  theme(plot.title = element_text(color="black", size=10, face="bold", vjust = -20, hjust = 0.70))+
  theme(axis.title = element_text(size=11))+
  theme(legend.title = element_text(size = 11))

grid.arrange(c1, c2, nrow = 1, ncol = 2)


pairs.panels(local_WQ_str [c(15,17)],
             method="kendall",
             hist.col="green",
             density=TRUE,
             main = "local WQ in streams correlations, Kendall"
)
pairs.panels(local_WQ_str [c(17,15)],
             method="kendall",
             hist.col="green",
             density=TRUE,
             main = "local WQ in streams correlations, Kendall"
)

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

