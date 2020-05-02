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

# experiment with significance tests
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

# now make 6 plots with statistical significance
# first subset the data and do t-tests

tTDS<-WQ_samples_2019_HL %>% 
  select(Period, `TDS [mg/L]`)

t.test(`TDS [mg/L]` ~ Period, data = tTDS)

tPO4<-WQ_samples_2019_HL %>% 
  select(Period, `PO3-4-P [mg/L]`)

t.test(`PO3-4-P [mg/L]` ~ Period, data = tPO4)

tNO3<-WQ_samples_2019_HL %>% 
  select(Period, `NO+3-N [mg/L]`)

t.test(`NO+3-N [mg/L]` ~ Period, data = tNO3)

tMg<-WQ_samples_2019_HL %>% 
  select(Period, `Mg [mg/l]`)

t.test(`Mg [mg/l]` ~ Period, data = tMg)

ggplot(t, aes(Period,`Mg [mg/l]` ))+
  geom_boxplot()

tAl<-WQ_samples_2019_HL %>% 
  select(Period, `Al [ug/L]`)

t.test(`Al [ug/L]` ~ Period, data = tAl)
ggplot(tAl, aes(Period,`Al [ug/L]` ))+
  geom_boxplot()

tCr<-WQ_samples_2019_HL %>% 
  select(Period, `Cr [ug/L]`)

t.test(`Cr [ug/L]` ~ Period, data = tCr)



# trying to use ggstat for statictical plots 
# https://indrajeetpatil.github.io/ggstatsplot/

d1<-ggstatsplot::ggbetweenstats(
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
  bf.message = FALSE,
  pairwise.annotation = "p.value",
  messages = TRUE
)

d2<-ggstatsplot::ggbetweenstats(
  data = tPO4,
  x = Period,
  y = 'PO3-4-P [mg/L]',
  type = "p",
  mean.ci = FALSE,
  pairwise.comparisons = TRUE,
  pairwise.display = "s",
  outlier.tagging = FALSE,
  outlier.coef = 2,
  outlier.label.color = "darkgreen",
  ggstatsplot.layer = FALSE,
  bf.message = FALSE,
  pairwise.annotation = "p.value",
  messages = TRUE
)


d3<-ggstatsplot::ggbetweenstats(
  data = tNO3,
  x = Period,
  y = 'NO+3-N [mg/L]',
  type = "p",
  mean.ci = FALSE,
  pairwise.comparisons = TRUE,
  pairwise.display = "s",
  outlier.tagging = FALSE,
  outlier.coef = 2,
  outlier.label.color = "darkgreen",
  ggstatsplot.layer = FALSE,
  bf.message = FALSE,
  pairwise.annotation = "p.value",
  messages = TRUE
)

d4<-ggstatsplot::ggbetweenstats(
  data = tMg,
  x = Period,
  y = 'Mg [mg/l]',
  type = "p",
  mean.ci = FALSE,
  pairwise.comparisons = TRUE,
  pairwise.display = "s",
  outlier.tagging = FALSE,
  outlier.coef = 2,
  outlier.label.color = "darkgreen",
  ggstatsplot.layer = FALSE,
  bf.message = FALSE,
  pairwise.annotation = "p.value",
  messages = TRUE
)

d5<-ggstatsplot::ggbetweenstats(
  data = tAl,
  x = Period,
  y = 'Al [ug/L]',
  type = "p",
  mean.ci = FALSE,
  pairwise.comparisons = TRUE,
  pairwise.display = "s",
  outlier.tagging = FALSE,
  outlier.coef = 2,
  outlier.label.color = "darkgreen",
  ggstatsplot.layer = FALSE,
  bf.message = FALSE,
  pairwise.annotation = "p.value",
  messages = TRUE
)

d6<-ggstatsplot::ggbetweenstats(
  data = tCr,
  x = Period,
  y = 'Cr [ug/L]',
  type = "p",
  mean.ci = FALSE,
  pairwise.comparisons = TRUE,
  pairwise.display = "s",
  outlier.tagging = TRUE,
  outlier.coef = 2,
  outlier.label.color = "darkgreen",
  ggstatsplot.layer = FALSE,
  bf.message = FALSE,
  pairwise.annotation = "p.value",
  messages = TRUE
)

grid.arrange(d1, d2, d3, d4, d5, d6, nrow = 3, ncol = 2)

# some more
tAs<-WQ_samples_2019_HL %>% 
  select(Period, `As [ug/L]`)

t.test(`As [ug/L]` ~ Period, data = tAs)

tPb<-WQ_samples_2019_HL %>% 
  select(Period, `Pb [ug/L]`)

t.test(`Pb [ug/L]` ~ Period, data = tPb)

tNi<-WQ_samples_2019_HL %>% 
  select(Period, `Ni [ug/L]`)

t.test(`Ni [ug/L]` ~ Period, data = tNi)

tFeD<-WQ_samples_2019_HL %>% 
  select(Period, `Fe-Dis [mg/L]`)

t.test(`Fe-Dis [mg/L]` ~ Period, data = tFeD)


tFeT<-WQ_samples_2019_HL %>% 
  select(Period, `Fe-Tot [mg/L]`)

t.test(`Fe-Tot [mg/L]` ~ Period, data = tFeT)


d7<-ggstatsplot::ggbetweenstats(
  data = tAs,
  x = Period,
  y = 'As [ug/L]',
  type = "p",
  mean.ci = FALSE,
  pairwise.comparisons = TRUE,
  pairwise.display = "s",
  outlier.tagging = TRUE,
  outlier.coef = 2,
  outlier.label.color = "darkgreen",
  ggstatsplot.layer = FALSE,
  bf.message = FALSE,
  pairwise.annotation = "p.value",
  messages = TRUE
)

d8<-ggstatsplot::ggbetweenstats(
  data = tPb,
  x = Period,
  y = 'Pb [ug/L]',
  type = "p",
  mean.ci = FALSE,
  pairwise.comparisons = TRUE,
  pairwise.display = "s",
  outlier.tagging = TRUE,
  outlier.coef = 2,
  outlier.label.color = "darkgreen",
  ggstatsplot.layer = FALSE,
  bf.message = FALSE,
  pairwise.annotation = "p.value",
  messages = TRUE
)

d9<-ggstatsplot::ggbetweenstats(
  data = tNi,
  x = Period,
  y = 'Ni [ug/L]',
  type = "p",
  mean.ci = FALSE,
  pairwise.comparisons = TRUE,
  pairwise.display = "s",
  outlier.tagging = TRUE,
  outlier.coef = 2,
  outlier.label.color = "darkgreen",
  ggstatsplot.layer = FALSE,
  bf.message = FALSE,
  pairwise.annotation = "p.value",
  messages = TRUE
)

d10<-ggstatsplot::ggbetweenstats(
  data = tFeD,
  x = Period,
  y = 'Fe-Dis [mg/L]',
  type = "p",
  mean.ci = FALSE,
  pairwise.comparisons = TRUE,
  pairwise.display = "s",
  outlier.tagging = TRUE,
  outlier.coef = 2,
  outlier.label.color = "darkgreen",
  ggstatsplot.layer = FALSE,
  bf.message = FALSE,
  pairwise.annotation = "p.value",
  messages = TRUE
)

d11<-ggstatsplot::ggbetweenstats(
  data = tFeT,
  x = Period,
  y = 'Fe-Tot [mg/L]',
  type = "p",
  mean.ci = FALSE,
  pairwise.comparisons = TRUE,
  pairwise.display = "s",
  outlier.tagging = TRUE,
  outlier.coef = 2,
  outlier.label.color = "darkgreen",
  ggstatsplot.layer = FALSE,
  bf.message = FALSE,
  pairwise.annotation = "p.value",
  messages = TRUE
)


grid.arrange(d7, d8, d9, d10, d11, nrow = 3, ncol = 2)

plot(x=WQ_samples_2019_HL$`Fe-Dis [mg/L]`, y=WQ_samples_2019_HL$`Fe-Tot [mg/L]`)

