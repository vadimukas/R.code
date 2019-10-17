library(readr)
library(ggplot2)
library(tidyverse)

#load Kishi Almaty upper data monthly
library(readr)
KA_1_monthly_KZ <- read_csv("WQCA/KA-1_monthly_KZ.csv", 
                            col_types = cols(TDS_ppm = col_double()))
View(KA_1_monthly_KZ)
#https://boxuancui.github.io/DataExplorer/ - package DataExplorer to explore the dataset
library(DataExplorer)

summary(KA_1_monthly_KZ)
plot_histogram(KA_1_monthly_KZ, title="KA_1_monthly_KZ")
plot(KA_1_monthly_KZ$TDS_ppm,KA_1_monthly_KZ$Mg_ppm)
plot_scatterplot(KA_1_monthly_KZ, by="Q", title = "KA_1_monthly_KZ by Q", ggtheme = theme_bw())
pairs(KA_1_monthly_KZ [c(3:8)], col=KA_1_monthly_KZ$year)
# library(psych)
install.packages("psych")
library(psych)
pairs.panels(KA_1_monthly_KZ [c(3:8)],
             method="pearson",
             hist.col="#00AFBB",
             density=TRUE
             )

plot_boxplot(KA_1_monthly_KZ, by = "year", title = "KA_1_monthly_KZ by year")
plot_boxplot(KA_1_monthly_KZ, by = "month", title = "KA_1_monthly_KZ by month")
plot_correlation(KA_1_monthly_KZ, title = "KA_1_monthly_KZ corrlations")
create_report(KA_1_monthly_KZ)
plot.ts(KA_1_monthly_KZ$Fe_total_ppm)
plot.ts(KA_1_monthly_KZ$Q)
trend_lm<-lm(KA_1_monthly_KZ$Q~KA_1_monthly_KZ$Q)
trend_lm
abline (trend_lm, col="red", size=15)
plot.ts(KA_1_monthly_KZ$SO4_ppm)
plot.ts(KA_1_monthly_KZ$Fe_total_ppm, title = "KA_1_monthly_KZ$Fe_total_ppm")

#load Kishi Almaty downsntream data monthly
KA_2_monthly_KZ <- read_csv("WQCA/KA-2_monthly_KZ.csv", 
                            col_types = cols(TDS_ppm = col_double()))
View(KA_2_monthly_KZ)
summary(KA_2_monthly_KZ)
plot_histogram(KA_2_monthly_KZ, title = "KA 2 monthly")
plot_scatterplot(KA_2_monthly_KZ, by="Q", title="KA_2_monthly_KZ by Q")
plot(KA_2_monthly_KZ$BOD,KA_2_monthly_KZ$PO4)
ggplot(data=KA_2_monthly_KZ, mapping = aes(x=BOD, y=PO4), ylim=1, xlim=0.4)+
  geom_point()
pairs.panels(KA_2_monthly_KZ [c(3:9)],
             method="pearson",
             hist.col="#00AFBB",
             density=TRUE
)
plot_boxplot(KA_2_monthly_KZ, by = "year", title = "KA 2 monthly by year")
plot_boxplot(KA_2_monthly_KZ, by = "month", title = "KA_2_monthly_KZ by month", ggtheme = theme_bw())
plot_correlation(KA_2_monthly_KZ, title="KA_2_monthly_KZ")
create_report(KA_2_monthly_KZ)
plot.ts(KA_2_monthly_KZ$Fe_total, title(main = "KA_2_monthly_KZ$Fe_total"))
plot.ts(KA_2_monthly_KZ$Ca, title(main = "KA_2_monthly_KZ$Ca"))
plot.ts(KA_2_monthly_KZ$Mg, title(main = "KA_2_monthly_KZ$Mg"))
plot.ts(KA_2_monthly_KZ$TPH, title(main = "KA_2_monthly_KZ$TPH"))


#load Kishi Almaty upstream data yearly and explore it
KA_1_yearly_KZ <- read_csv("WQCA/KA-1_yearly_KZ.csv", 
                            col_types = cols(TDS_ppm = col_double()))
View(KA_1_yearly_KZ)
summary(KA_1_yearly_KZ)
plot_histogram(KA_1_yearly_KZ, title = "KA 1 yearly")
plot(KA_1_year_KZ$BOD,KA_1_yearly_KZ$PO4)
plot_boxplot(KA_1_yearly_KZ, by = "year", title = "KA 1 yearly by year")
plot_correlation(KA_1_monthly_KZ)
create_report(KA_1_yearly_KZ)

# next load Kishi Almaty downstream data yearly and explore it
KA_2_yearly_KZ <- read_csv("WQCA/KA-2_yearly_KZ.csv", 
                           col_types = cols(TDS_ppm = col_double()))
View(KA_2_yearly_KZ)
summary(KA_2_yearly_KZ)
plot_histogram(KA_2_yearly_KZ, title = "KA 2 yearly")
plot(KA_2_year_KZ$BOD,KA_2_yearly_KZ$PO4)
plot_boxplot(KA_2_yearly_KZ, by = "year", title = "KA 2 yearly by year")
plot_correlation(KA_2_monthly_KZ, title = "KA 2 yearly")
create_report(KA_2_yearly_KZ)

#load Ulken Almaty upper data yearly
UA_1_yearly_KZ <- read_csv("WQCA/UA-1_yearly_KZ.csv")
View(UA_1_yearly_KZ)
summary(UA_1_yearly_KZ)
plot_histogram(UA_1_yearly_KZ, title = "UA_1_yearly_KZ")
plot_boxplot(UA_1_yearly_KZ, by="year", title = "UA_1_yearly_kz")
plot_correlation(UA_1_yearly_KZ, title = "UA1_yearly_KZ")
plot.ts(UA_1_yearly_KZ$Fe_total, title(main = "UA1_yearly_KZ$Fe_total"))
plot.ts(UA_1_yearly_KZ$Ca, title(main = "UA1_yearly_KZ$Ca"))
plot.ts(UA_1_yearly_KZ$Mg, title(main = "UA1_yearly_KZ$Mg"))
plot.ts(UA_1_yearly_KZ$TPH, title(main = "UA1_yearly_KZ$TPH"))
plot(UA_1_yearly_KZ$SO4,UA_1_yearly_KZ$TPH)

#load Ulken Almaty downstream data yearly
UA_2_yearly_KZ <- read_csv("WQCA/UA-2_yearly_KZ.csv")
View(UA_2_yearly_KZ)
summary(UA_2_yearly_KZ)
plot_histogram(UA_2_yearly_KZ, title = "UA_2_yearly_KZ")
plot_boxplot(UA_2_yearly_KZ, by="year", title = "UA_2_yearly_kz")
plot.ts(UA_2_yearly_KZ$Fe_total, title(main = "UA_2_yearly_KZ$Fe_total"))
plot.ts(UA_2_yearly_KZ$TPH, title(main = "UA_2_yearly_KZ$TPH"))
plot_correlation(UA_2_yearly_KZ, title = "UA2_yearly_KZ")
plot(UA_2_yearly_KZ$Mg, UA_2_yearly_KZ$SO4, title(main = "UA_2_yearly_KZ$Mg, UA_2_yearly_KZ$SO4"))
plot(UA_2_yearly_KZ$Fe_total, UA_2_yearly_KZ$PO4, title(main = "UA_2_yearly_KZ$Fe, UA_2_yearly_KZ$PO4"))
create_report(UA_2_yearly_KZ)

#load Chirchik at Mulala upstream monthly data and explore
Chirchik_1_Mulala_UZ <- read_csv("WQCA/Chirchik_1_Mulala_UZ.csv", 
                           col_types = cols(TDS_ppm = col_double()))
View(Chirchik_1_Mulala_UZ)
summary(Chirchik_1_Mulala_UZ)
plot_histogram(Chirchik_1_Mulala_UZ, title = "Chirchik_1_Mulala_UZ")
# do some 1:1 plots with flow
plot_scatterplot(Chirchik_1_Mulala_UZ, by = "Q", title="Chirchik_1_Mulala_UZ by Q")
plot.ts(Chirchik_1_Mulala_UZ$`Q`, x=Chirchik_1_Mulala_UZ$month)
plot.ts(Chirchik_1_Mulala_UZ$`NO3-N_ppm`)
plot.ts(y=Chirchik_1_Mulala_UZ$PO4_ppm, x=Chirchik_1_Mulala_UZ$year)
plot.ts(Chirchik_1_Mulala_UZ$`NH4-N_ppm`)
plot.ts(Chirchik_1_Mulala_UZ$Cu_ppb)
plot.ts(Chirchik_1_Mulala_UZ$F_ppm, title(main = "Chirchik_1_Mulala_UZ$F_ppm"))
plot.ts(Chirchik_1_Mulala_UZ$Fe_total_ppm)
plot(Chirchik_1_Mulala_UZ$`Q`,Chirchik_1_Mulala_UZ$`NO3-N_ppm`,title(main ="Q - NO3-N" ))
plot(Chirchik_1_Mulala_UZ$`Q`,Chirchik_1_Mulala_UZ$PO4_ppm)
plot(Chirchik_1_Mulala_UZ$`Q`,Chirchik_1_Mulala_UZ$Ca_ppm)
plot(Chirchik_1_Mulala_UZ$`Q`,Chirchik_1_Mulala_UZ$Mg_ppm)
plot(Chirchik_1_Mulala_UZ$`Q`,Chirchik_1_Mulala_UZ$SO4_ppm)
plot(Chirchik_1_Mulala_UZ$`Q`,Chirchik_1_Mulala_UZ$HCO3_ppm)
plot(Chirchik_1_Mulala_UZ$`Q`,Chirchik_1_Mulala_UZ$Fe_total_ppm)
plot(Chirchik_1_Mulala_UZ$HCO3_ppm,Chirchik_1_Mulala_UZ$Ca_ppm, title(main ="HCO3_ppm - Ca_ppm" ))
plot_boxplot(Chirchik_1_Mulala_UZ, by = "year", title = "Chirchik_1_Mulala_UZ by year")
plot_boxplot(Chirchik_1_Mulala_UZ, by = "month", title = "Chirchik_1_Mulala_UZ by month", ggtheme = theme_bw())
plot_correlation(Chirchik_1_Mulala_UZ, title = "Chirchik_1_Mulala_UZ correlations")
create_report(Chirchik_1_Mulala_UZ)

#load Chirchik at Chinaz downstream monthly data and explore
Chirchik_2_Chinaz_UZ <- read_csv("WQCA/Chirchik_2_Chinaz_UZ.csv", 
                                 col_types = cols(TDS_ppm = col_double()))
View(Chirchik_2_Chinaz_UZ)
summary(Chirchik_2_Chinaz_UZ)
plot_histogram(Chirchik_2_Chinaz_UZ, title = "Chirchik_2_Chinaz_UZ")

# do some 1:1 plots with flow
plot_scatterplot(Chirchik_2_Chinaz_UZ, by = "Q", title="Chirchik_2_Chinaz_UZ by Q")
plot.ts(Chirchik_2_Chinaz_UZ$`Q`)
plot.ts(Chirchik_2_Chinaz_UZ$`NO3-N_ppm`)
plot.ts(y=Chirchik_2_Chinaz_UZ$PO4_ppm, x=Chirchik_2_Chinaz_UZ$year)
plot.ts(Chirchik_2_Chinaz_UZ$`NH4-N_ppm`, title(main = 'Chirchik_2_Chinaz_UZ$`NH4-N_ppm'))
plot.ts(Chirchik_2_Chinaz_UZ$Cu_ppb)
plot.ts(Chirchik_2_Chinaz_UZ$F_ppm, title(main = "Chirchik_2_Chinaz_UZ$F_ppm"))
plot.ts(Chirchik_2_Chinaz_UZ$Cd_ppb)
plot.ts(Chirchik_2_Chinaz_UZ$Q, x=Chirchik_2_Chinaz_UZ$month)

plot(Chirchik_2_Chinaz_UZ$`Q`,Chirchik_2_Chinaz_UZ$`NO3-N_ppm`)
plot(Chirchik_2_Chinaz_UZ$`Q`,Chirchik_2_Chinaz_UZ$PO4_ppm)
plot(Chirchik_2_Chinaz_UZ$`Q`,Chirchik_2_Chinaz_UZ$Ca_ppm)
plot(Chirchik_2_Chinaz_UZ$`Q`,Chirchik_2_Chinaz_UZ$Mg_ppm)
plot(Chirchik_2_Chinaz_UZ$`Q`,Chirchik_2_Chinaz_UZ$SO4_ppm)
plot(Chirchik_2_Chinaz_UZ$`Q`,Chirchik_2_Chinaz_UZ$HCO3_ppm, title(main = "Q - HCO3_ppm"))
plot(Chirchik_2_Chinaz_UZ$`Q`,Chirchik_2_Chinaz_UZ$Cd_ppb)
plot(Chirchik_2_Chinaz_UZ$HCO3_ppm,Chirchik_2_Chinaz_UZ$Ca_ppm, title(main = "Chirchik_2_Chinaz_UZ$HCO3_ppm,Chirchik_2_Chinaz_UZ$Ca_ppm"))
plot_boxplot(Chirchik_2_Chinaz_UZ, by = "year", title = "Chirchik_2_Chinaz_UZ by year")
plot_boxplot(Chirchik_2_Chinaz_UZ, by = "month", title = "Chirchik_2_Chinaz_UZ by month", ggtheme = theme_bw())
ggplot(data=Chirchik_2_Chinaz_UZ, mapping = aes(x = month, y = `NO3-N_ppm`)) +
  geom_boxplot()
plot_correlation(Chirchik_2_Chinaz_UZ, title = "Chirchik_2_Chinaz_UZ correlations")
create_report(Chirchik_2_Chinaz_UZ)

#load Tajik GW data monthly
TJ_GWP <- read_csv("WQCA/TJ-GWP.csv", col_types = cols(X10 = col_skip(), 
                                                       X11 = col_skip(), X12 = col_skip(), X13 = col_skip(), 
                                                       year = col_date(format = "%Y")))
View(TJ_GWP)
plot_histogram(TJ_GWP, title = "TJ_GWP")
summary(TJ_GWP)
create_report(TJ_GWP)
plot_boxplot(TJ_GWP, by ="site", title = "TJ_GWP by site")

#load Afgan GW data monthly
AF_GW_2013 <- read_csv("WQCA/AF-GW_2013.csv", 
                       col_types = cols(Nr_sample = col_integer()))
View(AF_GW_2013)
summary(AF_GW_2013)
plot_boxplot(AF_GW_2013, by ="District", title = "AF_GW_2013 by district")
create_report(AF_GW_2013)

#making boxplots for pH, NO3-N and PO4, BOD across sites for Kishi Almaty and Chirchik
data_boxplots_pH <- read_csv("WQCA/data_boxplots_pH.csv")
boxplot(data_boxplots_pH, use.cols = TRUE)
data_boxplots_NO3_N <- read_csv("WQCA/data_boxplots_NO3_N.csv")
boxplot(data_boxplots_NO3_N, use.cols = TRUE)
plot.ts(data_boxplots_NO3_N$`NO3-N_KA2`, title(main = "Ts KA2 NO3-N"))
boxplot(data_boxplots_NO3_N, use.cols = TRUE, ylim=c(0,15))
data_boxplots_P04 <- read_csv("WQCA/data_boxplots_P04.csv")
boxplot(data_boxplots_P04, use.cols = TRUE, title(main = "PO4"))
boxplot(data_boxplots_P04, use.cols = TRUE, ylim=c(0,1))
plot.ts(data_boxplots_P04$PO4_KA_2, title(main = "Ts KA2 PO4"))
boxplot(data_boxplots_P04, use.cols = TRUE, ylim=c(0,0.3), title(main = "boxplots PO4"))
data_boxplots_BOD <- read_csv("WQCA/data_boxplots_BOD.csv")
boxplot(data_boxplots_BOD, use.cols = TRUE,title(main = "boxplots BOD"))
plot.ts(data_boxplots_BOD$BOD_ppm_KA_1,title(main = "TS KA1 BOD" ))
boxplot(data_boxplots_BOD, use.cols = TRUE, ylim=c(0, 4))
summary(data_boxplots_pH)        
summary(data_boxplots_NO3_N)
summary(data_boxplots_P04)

