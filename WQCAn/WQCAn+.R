# Before we start install the required packages

install.packages('tidyverse')
install.packages('DataExplorer')
install.packages('psych')
install.packages('DescTools')
install.packages('pastecs')

# exploring new WQ data, first load the required packages
library(readr) 
library(ggplot2)
library(tidyverse)
#https://boxuancui.github.io/DataExplorer/ - package DataExplorer to explore the dataset
library(DataExplorer)
library(psych)
library(DescTools)
library(pastecs)

#set working directory to subfolder WQCAn
setwd("D:/R/R.code/WQCAn")

# first load ICP data
ICP_samples_2019 <- read_csv("ICP_samples__2019.csv", 
                             col_types = cols(month = col_character(), Date = col_date(format = "%d-%m-%Y"), 
                                              Time = col_time(format = "%H:%M")))
View(ICP_samples_2019)

# start exploring the data
summary(ICP_samples_2019)
describe(ICP_samples_2019)
CoefVar(ICP_samples_2019$pH, na.rm = TRUE)
stat_ICP_samples_2019<-stat.desc(ICP_samples_2019 [c(12:36)])
plot_histogram(ICP_samples_2019 [c(12:36)], title="ICP_samples_2019", ggtheme = theme_bw())
plot_boxplot(ICP_samples_2019 [c(4, 12:36)], by = "month", title = "ICP_samples_2019 by Month", ggtheme = theme_bw())
plot_correlation(ICP_samples_2019 [c(12:36)])
pairs(ICP_samples_2019 [c(12:20)])
pairs(ICP_samples_2019 [c(21:28)])
pairs(ICP_samples_2019 [c(29:36)])
plot(ICP_samples_2019$`Fe-Dis [mg/L]`,ICP_samples_2019$`Fe-Tot [mg/L]`)


pairs.panels(ICP_samples_2019 [c(14:20)],
             method="pearson",
             hist.col="#00AFBB",
             density=TRUE
)

pairs.panels(ICP_samples_2019 [c(14:15,19:22)],
             method="pearson",
             hist.col="#00AFBB",
             density=TRUE
)
pairs.panels(ICP_samples_2019 [c(14:15,19:22, 25:26)],
             method="pearson",
             hist.col="#00AFBB",
             density=TRUE
)
pairs.panels(ICP_samples_2019 [c(14:15,19:22, 31,35)],
             method="pearson",
             hist.col="#00AFBB",
             density=TRUE
)
# the major ions with Kendall correlation
pairs.panels(ICP_samples_2019 [c(14:15,19:22, 31,35)],
             method="kendall",
             hist.col="#00AFBB",
             density=TRUE
)
pairs.panels(ICP_samples_2019 [c(31:36)],
             method="pearson",
             hist.col="#00AFBB",
             density=TRUE
)


corPlot(ICP_samples_2019 [c(12:36)], numbers = TRUE, xlas = 2)
plot_boxplot(ICP_samples_2019 [c(9, 12:36)], by = "Country", title = "ICP_samples_2019", ggtheme = theme_bw())
plot_boxplot(ICP_samples_2019 [c(8, 12:36)], by = "Sample type", title = "ICP_samples_2019", ggtheme = theme_bw())
plot_prcomp(na.omit(ICP_samples_2019), maxcat = 5L, ggtheme = theme_bw())

# load a cropped dataset for four study catchments only
ICP_samples_2019s <- read_csv("ICP_samples__2019s.csv", 
                             col_types = cols(Date = col_date(format = "%d-%m-%Y"), 
                                              Time = col_time(format = "%H:%M"), month = col_character()))
View(ICP_samples_2019s)

# and explore it in the same way
summary(ICP_samples_2019s)
describe(ICP_samples_2019s)
stat_ICP_samples_2019s<-stat.desc(ICP_samples_2019s [c(12:36)])
plot_histogram(ICP_samples_2019s [c(12:36)], title="ICP_samples_2019", ggtheme = theme_bw())
plot_correlation(ICP_samples_2019s [c(12:36)])
pairs(ICP_samples_2019s [c(12:20)])
pairs(ICP_samples_2019s [c(21:28)])
pairs(ICP_samples_2019s [c(29:36)])
plot(ICP_samples_2019s$`Fe-Dis [mg/L]`,ICP_samples_2019s$`Fe-Tot [mg/L]`)

pairs.panels(ICP_samples_2019s [c(14:20)],
             method="pearson",
             hist.col="#00AFBB",
             density=TRUE
)

pairs.panels(ICP_samples_2019s [c(14:15,19:22)],
             method="kendall",
             hist.col="#00AFBB",
             density=TRUE
)
pairs.panels(ICP_samples_2019s [c(14:15,19:22, 25:26)],
             method="pearson",
             hist.col="#00AFBB",
             density=TRUE
)
pairs.panels(ICP_samples_2019s [c(14:15,19:22, 31,35)],
             method="pearson",
             hist.col="#00AFBB",
             density=TRUE
)
# the major ions with Kendall correlation
pairs.panels(ICP_samples_2019 [c(14:15,19:22, 31,35)],
             method="kendall",
             hist.col="#00AFBB",
             density=TRUE
)
pairs.panels(ICP_samples_2019s [c(31:36)],
             method="pearson",
             hist.col="#00AFBB",
             density=TRUE
)

corPlot(ICP_samples_2019s [c(12:36)], numbers = TRUE, xlas = 2)
plot_boxplot(ICP_samples_2019s [c(9, 12:36)], by = "Country", title = "ICP samples 2019s", ggtheme = theme_test(),geom_boxplot_args = list("outlier.color" = "red"))
plot_boxplot(ICP_samples_2019s [c(8, 12:36)], by = "Sample type", title = "ICP samples 2019s", ggtheme = theme_text(), geom_boxplot_args = list("outlier.color" = "red"))
plot_prcomp(na.omit(ICP_samples_2019s), maxcat = 5L, ggtheme = theme_bw())

# load a cryosphere samples dataset for Kazakhstan only
ICP_samples_2019c <- read_csv("ICP_samples__2019c.csv", 
                              col_types = cols(Date = col_date(format = "%d-%m-%Y"), 
                                               Time = col_time(format = "%H:%M")))
View(ICP_samples_2019c)

# and explore it in the same way
summary(ICP_samples_2019c)
describe(ICP_samples_2019c)
plot_histogram(ICP_samples_2019c [c(12:33)], title="ICP_samples_2019c", ggtheme = theme_bw())
plot_correlation(ICP_samples_2019c [c(12:33)])
pairs(ICP_samples_2019c [c(12:20)])
pairs(ICP_samples_2019c [c(21:28)])
pairs(ICP_samples_2019c [c(29:33)])


pairs.panels(ICP_samples_2019c [c(14:20)],
             method="pearson",
             hist.col="#00AFBB",
             density=TRUE
)

pairs.panels(ICP_samples_2019c [c(14:15,21:25)],
             method="pearson",
             hist.col="#00AFBB",
             density=TRUE
)
pairs.panels(ICP_samples_2019c [c(14:15,26:29)],
             method="pearson",
             hist.col="#00AFBB",
             density=TRUE
)
pairs.panels(ICP_samples_2019c [c(14:15,26:29)],
             method="kendall",
             hist.col="#00AFBB",
             density=TRUE
)
pairs.panels(ICP_samples_2019c [c(14:15, 30:33)],
             method="pearson",
             hist.col="#00AFBB",
             density=TRUE
)
# the major ions with pearson
pairs.panels(ICP_samples_2019c [c(14:19)],
             method="pearson",
             hist.col="#00AFBB",
             density=TRUE
)
# the major ions with Kendall correlation
pairs.panels(ICP_samples_2019c [c(14:19, 28, 32)],
             method="kendall",
             hist.col="#00AFBB",
             density=TRUE
)

# the some other 
pairs.panels(ICP_samples_2019c [c(14:19, 22, 33)],
             method="kendall",
             hist.col="#00AFBB",
             density=TRUE
)

corPlot(ICP_samples_2019c [c(12:33)], numbers = TRUE, xlas = 2)
plot_boxplot(ICP_samples_2019c [c(8, 12:33)], by = "Sample type", title = "ICP_samples_2019c", ggtheme = theme_bw())
plot_prcomp(na.omit(ICP_samples_2019c), maxcat = 5L, ggtheme = theme_bw())


# start exploring local data
# first KG data
library(readr)
KG_WQ <- read_csv("KG_WQ.csv", col_types = cols(Month = col_character(), `Coordinate 1` = col_double(), 
                                                `Coordinate 2` = col_double(), `Date sampled` = col_date(format = "%d-%m-%Y"), 
                                                Time = col_time(format = "%H:%M")))

View(KG_WQ)

# explore local KG
# and explore it in the same way
par(mfrow=c(1,1))
summary(KG_WQ)
describe(KG_WQ)
plot_histogram(KG_WQ [c(10:16)], title="KG WQ", ggtheme = theme_bw(), geom_boxplot_args = list("outlier.color" = "red"))
plot_correlation(KG_WQ [c(10:16)])
pairs(KG_WQ [c(10:16)])

pairs.panels(KG_WQ[c(10:16)],
             method="pearson",
             hist.col="#00AFBB",
             density=TRUE,
             main="KG WQ correlations"
)

pairs.panels(KG_WQ[c(10:16)],
             method="kendall",
             hist.col="#00AFBB",
             density=TRUE
)
plot_boxplot(KG_WQ [c(7, 10:16)], by = "Sample type", title = "KG_WQ by sample type", ggtheme = theme_bw(), geom_boxplot_args = list("outlier.color" = "red"))
plot_boxplot(KG_WQ [c(4, 10:16)], by = "Month", title = "KG WQ by Month", ggtheme = theme_bw(), geom_boxplot_args = list("outlier.color" = "red"))
plot.ts(KG_WQ$`PO3-4-P [mg/L]`, title(main = "KG PO3-4-P "))
par(mfrow=c(2,3))
boxplot(KG_WQ$pH, main = "KG pH")
boxplot(KG_WQ$`EC [microS/cm]`, main = "KG EC")
boxplot(KG_WQ$`TDS    [mg/L]`, main = "KG TDS")
boxplot(KG_WQ$`NO+3-N [mg/L]`, main = "KG NO+3-N")
boxplot(KG_WQ$`PO3-4-P [mg/L]`, main = "KG PO3-4-P")
boxplot(KG_WQ$`Fe [mg/L]`, main = "KG Fe")

# next UZ data
library(readr)
UZ_WQ <- read_csv("UZ_WQ.csv", col_types = cols(Month = col_character(), `Coordinate 1` = col_double(), 
                                                `Coordinate 2` = col_double(), `Date sampled` = col_date(format = "%d-%m-%Y"), 
                                                Time = col_time(format = "%H:%M")))
View(UZ_WQ)

# explore local UZ
# and explore it in the same way
summary(UZ_WQ)
describe(UZ_WQ)
plot_histogram(UZ_WQ [c(10:16)], title="UZ_WQ", ggtheme = theme_bw())
plot_correlation(UZ_WQ [c(10:16)])

pairs(UZ_WQ [c(10:16)])

pairs.panels(UZ_WQ[c(10:16)],
             method="pearson",
             hist.col="#00AFBB",
             density=TRUE,
             main="UZ WQ correlations"
)

pairs.panels(UZ_WQ[c(10:16)],
             method="kendall",
             hist.col="#00AFBB",
             density=TRUE
)
plot_boxplot(UZ_WQ [c(7, 10:16)], by = "Sample type", title = "UZ WQ by sample type", ggtheme = theme_bw(), geom_boxplot_args = list("outlier.color" = "red"))
plot_boxplot(UZ_WQ [c(4, 10:16)], by = "Month", title = "UZ WQ by Month", ggtheme = theme_bw(), geom_boxplot_args = list("outlier.color" = "red"))
plot.ts(UZ_WQ$`PO3-4-P [mg/L]`, title(main = "UZ PO3-4-P "))
par(mfrow=c(2,3))
boxplot(UZ_WQ$pH, main = "UZ pH")
boxplot(UZ_WQ$`EC [microS/cm]`, main = "UZ EC")
boxplot(UZ_WQ$`TDS    [mg/L]`, main = "UZ TDS")
boxplot(UZ_WQ$`NO+3-N [mg/L]`, main = "UZ NO+3-N")
boxplot(UZ_WQ$`PO3-4-P [mg/L]`, main = "UZ PO3-4-P")
boxplot(UZ_WQ$`Fe [mg/L]`, main = "UZ Fe")

# next KZ data
KZ_WQ <- read_csv("KZ_WQ.csv", col_types = cols(Month = col_character(),`Coordinate 1` = col_double(), 
                                                `Coordinate 2` = col_double(), `Date sampled` = col_date(format = "%d-%m-%Y"), 
                                                Time = col_time(format = "%H:%M")))
View(KZ_WQ)

# explore local KZ
# and explore KZ datset in the same way
summary(KZ_WQ)
describe(KZ_WQ)
plot_histogram(KZ_WQ [c(10:16)], title="KZ_WQ", ggtheme = theme_bw())
plot_density(KZ_WQ [c(10:16)], title="KZ_WQ", ggtheme = theme_bw())
plot_correlation(KZ_WQ [c(10:16)])

pairs(KZ_WQ [c(10:16)])

pairs.panels(KZ_WQ[c(10:16)],
             method="pearson",
             hist.col="#00AFBB",
             density=TRUE,
             main="KZ WQ correlations"
)

pairs.panels(KZ_WQ[c(10:16)],
             method="kendall",
             hist.col="#00AFBB",
             density=TRUE
)
plot_boxplot(KZ_WQ [c(7, 10:16)], by = "Sample type", title = "KZ_WQ by sample type", ggtheme = theme_test(), geom_boxplot_args = list("outlier.color" = "red"))
plot.ts(x = KZ_WQ$Month, y = KZ_WQ$`NO+3-N [mg/L]`, title(main = "KZ NO+3-N"))
plot_boxplot(KZ_WQ [c(4, 10:16)], by = "Month", title = "KZ WQ by Month", ggtheme = theme_bw(), geom_boxplot_args = list("outlier.color" = "red"))

par(mfrow=c(2,3))
boxplot(KZ_WQ$pH, main = "KZ pH")
boxplot(KZ_WQ$`EC [microS/cm]`, main = "KZ EC")
boxplot(KZ_WQ$`TDS    [mg/L]`, main = "KZ TDS")
boxplot(KZ_WQ$`NO+3-N [mg/L]`, main = "KZ NO+3-N")
boxplot(KZ_WQ$`PO3-4-P [mg/L]`, main = "Kz PO3-4-P")
boxplot(KZ_WQ$`Fe [mg/L]`, main = "KZ Fe")

# finally TJ data
TJ_WQ <- read_csv("TJ_WQ.csv", col_types = cols(Month = col_character(),`Coordinate 1` = col_double(), 
                                                `Coordinate 2` = col_double(), `Date sampled` = col_date(format = "%d-%m-%Y"), 
                                                Time = col_time(format = "%H:%M")))
View(TJ_WQ)

# explore local TJ
# and explore KZ datset in the same way
summary(TJ_WQ)
describe(TJ_WQ)
plot_histogram(TJ_WQ [c(10:16)], title="TJ WQ", ggtheme = theme_classic())
plot_correlation(TJ_WQ [c(10:16)])

pairs(TJ_WQ [c(10:16)])

pairs.panels(TJ_WQ[c(10:16)],
             method="pearson",
             hist.col="green",
             density=TRUE,
             main = "TJ WQ"
)

pairs.panels(TJ_WQ[c(10:16)],
             method="kendall",
             hist.col="#00AFBB",
             density=TRUE
)
plot_boxplot(TJ_WQ [c(7, 10:16)], by = "Sample type", title = "TJ WQ", ggtheme = theme_test(), geom_boxplot_args = list("outlier.color" = "red"))
plot_boxplot(TJ_WQ [c(4, 10:16)], by = "Month", title = "TJ WQ by Month", ggtheme = theme_test(), geom_boxplot_args = list("outlier.color" = "red"))
plot.ts(TJ_WQ$pH, title(main = "TJ pH"))
par(mfrow=c(2,3))
boxplot(TJ_WQ$pH, main = "TJ pH")
boxplot(TJ_WQ$`EC [microS/cm]`, main = "TJ EC")
boxplot(TJ_WQ$`TDS    [mg/L]`, main = "TJ TDS")
boxplot(TJ_WQ$`NO+3-N [mg/L]`, main = "TJ NO+3-N")
boxplot(TJ_WQ$`PO3-4-P [mg/L]`, main = "TJ PO3-4-P")
boxplot(TJ_WQ$`Fe [mg/L]`, main = "TJ Fe")

# now put boxplots across countries
# for pH
par(mfrow=c(1,4), cex = 0.8)
boxplot(KZ_WQ$pH, main = "KZ pH", ylim=c(7,8.8), col = "light blue")
boxplot(KG_WQ$pH, main = "KG pH", ylim=c(7,8.8), col = 'green')
boxplot(UZ_WQ$pH, main = "UZ pH", ylim=c(7,8.8), col = 'red')
boxplot(TJ_WQ$pH, main = "TJ pH", ylim=c(7,8.8), col = 'yellow')

# for EC
par(mfrow=c(1,4) ,cex = 0.9)
boxplot(KZ_WQ$`EC [microS/cm]`, main = "KZ EC", ylim=c(0,1000), col = "light blue")
boxplot(KG_WQ$`EC [microS/cm]`, main = "KG EC", ylim=c(0,1000), col = 'green')
boxplot(UZ_WQ$`EC [microS/cm]`, main = "UZ EC", ylim=c(0,1000), col = 'red')
boxplot(TJ_WQ$`EC [microS/cm]`, main = "TJ EC", ylim=c(0,1000), col = 'yellow')

# for TDS

boxplot(KZ_WQ$`TDS    [mg/L]`, main = "KZ TDS", ylim=c(0,600), col = 'light blue')
boxplot(KG_WQ$`TDS    [mg/L]`, main = "KG TDS", ylim=c(0,600), col = 'green')
boxplot(UZ_WQ$`TDS    [mg/L]`, main = "UZ TDS", ylim=c(0,600), col = 'red')
boxplot(TJ_WQ$`TDS    [mg/L]`, main = "TJ TDS", ylim=c(0,600), col = 'yellow')

# for NO3-N

boxplot(KZ_WQ$`NO+3-N [mg/L]`, main = "KZ NO+3-N", ylim=c(0,17), col = "blue", args = list("outlier.color" = "red"))
boxplot(KG_WQ$`NO+3-N [mg/L]`, main = "KG NO+3-N", ylim=c(0,17), col = 'green', args = list("outlier.color" = "green"))
boxplot(UZ_WQ$`NO+3-N [mg/L]`, main = "UZ NO+3-N", ylim=c(0,17), col = 'red', args = list("outlier.color" = "red"))
boxplot(TJ_WQ$`NO+3-N [mg/L]`, main = "TJ NO+3-N", ylim=c(0,17), col = 'yellow', args = list("outlier.color" = "red"))

# for Phosphate
boxplot(KZ_WQ$`PO3-4-P [mg/L]`, main = "Kz PO3-4-P", ylim=c(0,2.0), col = "light blue")
boxplot(KG_WQ$`PO3-4-P [mg/L]`, main = "KG PO3-4-P", ylim=c(0,2.0), col = 'green')
boxplot(UZ_WQ$`PO3-4-P [mg/L]`, main = "UZ PO3-4-P", ylim=c(0,5), col = 'red')
boxplot(TJ_WQ$`PO3-4-P [mg/L]`, main = "TJ PO3-4-P",  ylim=c(0,1), col = 'yellow')

boxplot(KZ_WQ$`PO3-4-P [mg/L]`, main = "Kz PO3-4-P", ylim=c(0,0.2), col = "blue")
boxplot(KG_WQ$`PO3-4-P [mg/L]`, main = "KG PO3-4-P", ylim=c(0,1.2), col = 'green')
boxplot(UZ_WQ$`PO3-4-P [mg/L]`, main = "UZ PO3-4-P", ylim=c(0,4.5), col = 'red')
boxplot(TJ_WQ$`PO3-4-P [mg/L]`, main = "TJ PO3-4-P",  ylim=c(0,0.2), col = 'yellow')

# Iron
boxplot(KZ_WQ$`Fe [mg/L]`, main = "KZ Fe", col = "blue")
boxplot(KG_WQ$`Fe [mg/L]`, main = "KG Fe", col = 'green')
boxplot(UZ_WQ$`Fe [mg/L]`, main = "UZ Fe", col = 'red')
boxplot(TJ_WQ$`Fe [mg/L]`, main = "TJ Fe", col = 'yellow')

# Iron
boxplot(KZ_WQ$`Fe [mg/L]`, main = "KZ Fe", ylim=c(0,0.25), col = "light blue")
boxplot(KG_WQ$`Fe [mg/L]`, main = "KG Fe", ylim=c(0,0.25), col = 'green')
boxplot(UZ_WQ$`Fe [mg/L]`, main = "UZ Fe", ylim=c(0,0.25), col = 'red')
boxplot(TJ_WQ$`Fe [mg/L]`, main = "TJ Fe",ylim=c(0,0.25), col = 'yellow')

# and now load local combined dataset 
local_WQ <- read_csv("local_WQ.csv", col_types = cols(`Date sampled` = col_date(format = "%d-%m-%Y"), 
                                                      Month = col_character()))
View(local_WQ)



# now explore local combined dataset
summary(local_WQ)
stat_local_WQ_<-stat.desc(local_WQ [c(11:17)])
plot_boxplot(local_WQ [c(8, 11:17)], by = "Sample type", title = "local WQ by sample type", ggtheme = theme_test(), geom_boxplot_args = list("outlier.color" = "red"))
plot_boxplot(local_WQ [c(4, 11:17)], by = "Month", title = "local_WQ by Month", ggtheme = theme_test(),geom_boxplot_args = list("outlier.color" = "blue"))
local_WQ_group<-local_WQ %>% group_by(Month) 
plot_boxplot(local_WQ_group [c(4, 11:17)], by = "Month", title = "local_WQ by Month", ggtheme = theme_bw())
  
pairs.panels(local_WQ[c(11:17)],
             method="pearson",
             hist.col="green",
             density=TRUE,
             main = "local WQ correlations, Peason"
)

pairs.panels(local_WQ[c(11:17)],
             method="kendall",
             hist.col="#00AFBB",
             density=TRUE
)         

# in addition load local combined dataset for GW by groundwater types
local_WQ_GW <- read_csv("local_WQ_GW.csv", col_types = cols(`Date sampled` = col_date(format = "%d-%m-%Y"), 
                                                      Month = col_character()))
View(local_WQ_GW)

# in addition explore local combined dataset for GW by groundwater types
 summary(local_WQ_GW)
stat_local_WQ_GW<-stat.desc(local_WQ [c(11:17)])
plot_boxplot(local_WQ_GW [c(8, 11:17)], by = "GW type", title = "local WQ by sample type", ggtheme = theme_test(), geom_boxplot_args = list("outlier.color" = "red"))
plot_boxplot(local_WQ_GW [c(4, 11:17)], by = "Month", title = "local GW WQ by Month", ggtheme = theme_test(),geom_boxplot_args = list("outlier.color" = "red"))
local_WQ_WG_group<-local_WQ_GW %>% group_by(Month) 
plot_boxplot(local_WQ_GW_group [c(4, 11:17)], by = "Month", title = "local_WQ by Month", ggtheme = theme_bw())

 pairs.panels(local_WQ_GW[c(11:17)],
             method="pearson",
             hist.col="green",
             density=TRUE,
             main = "local WQ WG correlations, Peason"
)

pairs.panels(local_WQ_GW [c(11:17)],
             method="kendall",
             hist.col="#00AFBB",
             density=TRUE,
             main = "local WQ WG correlations, Kendall"
)

# make summary statistics for bubble maps using describeBy function from psych package
summary_by_location <-describeBy(local_WQ, group = "Location", mat = TRUE)
# write output in txt file
write.table(summary_by_location, file = 'summary_by_location.txt', sep = ',')


# load the second round of ICP and IC - low flow period
ICP_IC_samples_2019 <- read_csv("ICP_IC_samples__2019.csv", 
                              col_types = cols(Date = col_date(format = "%d-%m-%Y"), 
                                               Time = col_time(format = "%H:%M"), month = col_character()))

# exlore the second round
# start exploring the data
summary(ICP_IC_samples_2019)
describe(ICP_IC_samples_2019)
CoefVar(ICP_IC_samples_2019$pH, na.rm = TRUE)
stat_ICP_IC_samples_2019<-stat.desc(ICP_IC_samples_2019 [c(12:40)])
plot_histogram(ICP_IC_samples_2019 [c(12:40)], title="ICP_IC_samples_2019", ggtheme = theme_bw())
plot_boxplot(ICP_IC_samples_2019 [c(4, 12:40)], by = "month", title = "ICP_IC_samples_2019 by Month", ggtheme = theme_bw())
plot_correlation(ICP_IC_samples_2019 [c(12:40)])
pairs(ICP_IC_samples_2019 [c(12:20)])
pairs(ICP_IC_samples_2019 [c(21:28)])
pairs(ICP_IC_samples_2019 [c(29:40)])
pairs(ICP_IC_samples_2019 [c(12:17, 19:26)])

pairs.panels(ICP_IC_samples_2019 [c(12:20)],
             method="pearson",
             hist.col="#00AFBB",
             density=TRUE
)

pairs.panels(ICP_IC_samples_2019 [c(14:15,19:22)],
             method="pearson",
             hist.col="#00AFBB",
             density=TRUE
)
pairs.panels(ICP_IC_samples_2019 [c(12:15,19:22, 25:26)],
             method="pearson",
             hist.col="#00AFBB",
             density=TRUE
)
pairs.panels(ICP_IC_samples_2019 [c(12:17, 19:26)],
             method="pearson",
             hist.col="#00AFBB",
             density=TRUE
)
# the major ions with Kendall correlation
pairs.panels(ICP_IC_samples_2019 [c(12:15,19:22, 31,35)],
             method="kendall",
             hist.col="#00AFBB",
             density=TRUE
)
pairs.panels(ICP_IC_samples_2019 [c(31:40)],
             method="pearson",
             hist.col="#00AFBB",
             density=TRUE
)


corPlot(ICP_IC_samples_2019 [c(14:40)], numbers = TRUE, xlas = 2)
plot_boxplot(ICP_IC_samples_2019 [c(9, 12:40)], by = "Country", title = "ICP_IC_samples_2019", ggtheme = theme_bw())
plot_boxplot(ICP_IC_samples_2019 [c(8, 12:40)], by = "Sample type", title = "ICP_IC_samples_2019", ggtheme = theme_bw())
plot_prcomp(na.omit(ICP_IC_samples_2019), maxcat = 5L, ggtheme = theme_bw())
