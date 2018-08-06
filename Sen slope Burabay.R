# library zyp
library(zyp)
# load T air time-series
library(readr)
Shuchinsk_Ta <- read_delim("Shuchinsk_Ta.csv", ";", escape_double = FALSE, na = "NaN", trim_ws = TRUE)
  cols(
    Years = col_integer(),
    Temp = col_double(),
    DJF = col_double(),
    MAM = col_double(),
    JJA = col_double(),
    SON = col_double()
    )
  
View(Shuchinsk_Ta)
# Linear regression models
# Tair annual Shuchisk with trendline
plot(Shuchinsk_Ta$Years, Shuchinsk_Ta$Temp, type="l", main = "T air annual Shuchinsk", ylab = 'T,C', xlab = 'Years', col='red', ylim = c(-2,5))
trend_lm<-lm(Shuchinsk_Ta$Temp~Shuchinsk_Ta$Years)
trend_lm
abline (trend_lm, col="black")
# Sen's slope annual
library(zyp)
x<-Shuchinsk_Ta$Years
y<-Shuchinsk_Ta$Temp
sen_slope_ann<-zyp.sen(y~x)
sen_slope_ann
#add sen$slopes to plot
abline(sen_slope_ann$coefficients[1], sen_slope_ann$coefficients[2], col="violet")

# Tair seasonal Shuchisk with trendline and Sen's slope
# winter
plot(Shuchinsk_Ta$Years, Shuchinsk_Ta$DJF, type="l", main = "T air winter  Shuchinsk", ylab = 'T,C', xlab = 'Years', col='blue', ylim = c(-10,-25))
trend_lm<-lm(Shuchinsk_Ta$DJF~Shuchinsk_Ta$Years)
trend_lm
abline (trend_lm, col="black")

temp_djf_subset<-subset(Shuchinsk_Ta,Years>1935)
d<-temp_djf_subset$Years
j<-temp_djf_subset$DJF
sen_slope_djf<-zyp.sen(j~d)
sen_slope_djf
abline(sen_slope_djf$coefficients[1], sen_slope_djf$coefficients[2], col="violet")

# spring
plot(Shuchinsk_Ta$Years, Shuchinsk_Ta$MAM, type="l", main = "T air spring  Shuchinsk", ylab = 'T,C', xlab = 'Years', col='green', ylim = c(-5,10))
trend_lm<-lm(Shuchinsk_Ta$MAM~Shuchinsk_Ta$Years)
trend_lm
abline (trend_lm, col="black")

a<-Shuchinsk_Ta$Years
m<-Shuchinsk_Ta$MAM
sen_slope_mam<-zyp.sen(m~a)
sen_slope_mam
abline(sen_slope_mam$coefficients[1], sen_slope_mam$coefficients[2], col="violet")

# summer
plot(Shuchinsk_Ta$Years, Shuchinsk_Ta$JJA, type="l", main = "T air summer  Shuchinsk", ylab = 'T,C', xlab = 'Years', col='red', ylim = c(12.5,22.5))
trend_lm<-lm(Shuchinsk_Ta$JJA~Shuchinsk_Ta$Years)
trend_lm
abline (trend_lm, col="black")
s<-Shuchinsk_Ta$Years
u<-Shuchinsk_Ta$JJA
sen_slope_jja<-zyp.sen(u~s)
sen_slope_jja
abline(sen_slope_jja$coefficients[1], sen_slope_jja$coefficients[2], col="violet")

# autumn
plot(Shuchinsk_Ta$Years, Shuchinsk_Ta$SON, type="l", main = "T air autumn  Shuchinsk", ylab = 'T,C', xlab = 'Years', col='brown', ylim = c(-5,5))
trend_lm<-lm(Shuchinsk_Ta$SON~Shuchinsk_Ta$Years)
trend_lm
abline (trend_lm, col="black")
f<-Shuchinsk_Ta$Years
o<-Shuchinsk_Ta$SON
sen_slope_son<-zyp.sen(o~f)
sen_slope_son
abline(sen_slope_son$coefficients[1], sen_slope_son$coefficients[2], col="violet")


# load Precip  time-series
library(readr)
Shuchinsk_Pa <- read_delim("Shuchinsk_Pa.csv", ";", escape_double = FALSE, na = "NaN", trim_ws = TRUE)
cols(
  Years = col_integer(),
  Temp = col_double(),
  DJF = col_double(),
  MAM = col_double(),
  JJA = col_double(),
  SON = col_double()
)

View(Shuchinsk_Pa)

# Linear regression models
# P annual Shuchisk with trendline
plot(Shuchinsk_Pa$Years, Shuchinsk_Pa$P, type="l", main = "P annual Shuchinsk", ylab = 'P, mm', xlab = 'Years', col='red', ylim = c(150,650))
trend_lm<-lm(Shuchinsk_Pa$P~Shuchinsk_Pa$Years)
trend_lm
abline (trend_lm, col="black")
# Sen's slope annual
library(zyp)
x<-Shuchinsk_Pa$Years
y<-Shuchinsk_Pa$P
sen_slope_ann<-zyp.sen(y~x)
sen_slope_ann
#add sen$slopes to plot
abline(sen_slope_ann$coefficients[1], sen_slope_ann$coefficients[2], col="violet")

# P seasonal Shuchisk with trendline and Sen's slope
# winter
plot(Shuchinsk_Pa$Years, Shuchinsk_Pa$DJF, type="l", main = "P winter  Shuchinsk", ylab = 'P, mm', xlab = 'Years', col='blue', ylim = c(5,100))
trend_lm<-lm(Shuchinsk_Pa$DJF~Shuchinsk_Pa$Years)
trend_lm
abline (trend_lm, col="black")

temp_djf_subset<-subset(Shuchinsk_Pa,Years>1935)
d<-temp_djf_subset$Years
j<-temp_djf_subset$DJF
sen_slope_djf<-zyp.sen(j~d)
sen_slope_djf
abline(sen_slope_djf$coefficients[1], sen_slope_djf$coefficients[2], col="violet")

# spring
plot(Shuchinsk_Pa$Years, Shuchinsk_Pa$MAM, type="l", main = "P spring  Shuchinsk", ylab = 'P, mm', xlab = 'Years', col='green', ylim = c(15,150))
trend_lm<-lm(Shuchinsk_Pa$MAM~Shuchinsk_Pa$Years)
trend_lm
abline (trend_lm, col="black")

a<-Shuchinsk_Pa$Years
m<-Shuchinsk_Pa$MAM
sen_slope_mam<-zyp.sen(m~a)
sen_slope_mam
abline(sen_slope_mam$coefficients[1], sen_slope_mam$coefficients[2], col="violet")

# summer
plot(Shuchinsk_Pa$Years, Shuchinsk_Pa$JJA, type="l", main = "P summer  Shuchinsk", ylab = 'P,mm', xlab = 'Years', col='red', ylim = c(15,400))
trend_lm<-lm(Shuchinsk_Pa$JJA~Shuchinsk_Pa$Years)
trend_lm
abline (trend_lm, col="black")
s<-Shuchinsk_Pa$Years
u<-Shuchinsk_Pa$JJA
sen_slope_jja<-zyp.sen(u~s)
sen_slope_jja
abline(sen_slope_jja$coefficients[1], sen_slope_jja$coefficients[2], col="violet")

# autumn
plot(Shuchinsk_Pa$Years, Shuchinsk_Pa$SON, type="l", main = "P autumn  Shuchinsk", ylab = 'P, mm', xlab = 'Years', col='brown', ylim = c(20,150))
trend_lm<-lm(Shuchinsk_Pa$SON~Shuchinsk_Pa$Years)
trend_lm
abline (trend_lm, col="black")
f<-Shuchinsk_Pa$Years
o<-Shuchinsk_Pa$SON
sen_slope_son<-zyp.sen(o~f)
sen_slope_son
abline(sen_slope_son$coefficients[1], sen_slope_son$coefficients[2], col="violet")
