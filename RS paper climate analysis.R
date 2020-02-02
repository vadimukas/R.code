# library zyp
library(zyp)
# load T air time-series
library(readr)
setwd("D:/R/R.code")
Shuchinsk_Ta <- read_delim("Shuchinsk_Ta.csv", ";", escape_double = FALSE, na = "NaN", trim_ws = TRUE)
cols(
  Years = col_integer(),
  Temp = col_double(),
  DJF = col_double(),
  MAM = col_double(),
  JJA = col_double(),
  SON = col_double()
)

#View(Shuchinsk_Ta)
tiff(file = "Ta_and_P_Shuchisk_Fig_6 .tiff", width = 18, height=8, units="in", res=600 )
par(mfrow = c(2,5), cex.main=1.3, cex.lab=1.2, cex.axis=1.3, lwd=1.2,par(mgp=c(2.5,1,0)))


# Linear regression models
# Tair annual Shuchisk with trendline

plot(Shuchinsk_Ta$Years, Shuchinsk_Ta$Temp, type="l", main = "Ta annual Shuchinsk", ylab = expression(paste("Temperature (",degree,"C)")),xlab = '', col='red', ylim = c(-2,5))
trend_lm<-lm(Shuchinsk_Ta$Temp~Shuchinsk_Ta$Years)
trend_lm
abline (trend_lm, col="black", size=15)
# Sen's slope annual
library(zyp)
x<-Shuchinsk_Ta$Years
y<-Shuchinsk_Ta$Temp
sen_slope_ann<-zyp.sen(y~x)
sen_slope_ann
#add sen$slopes to plot
abline(sen_slope_ann$coefficients[1], sen_slope_ann$coefficients[2], col="violet")
# add R squared coefficient
r2 <- round(summary(trend_lm)$r.squared, 4)
eqn<-bquote(R^2 == .(r2))
text(x=1990, y=4, eqn, pos=4, cex=1.4)
summary (trend_lm)

# Tair seasonal Shuchisk with trendline and Sen's slope

# winter
plot(Shuchinsk_Ta$Years, Shuchinsk_Ta$DJF, type="l", main = "Ta DJF  Shuchinsk", ylab = expression(paste("Temperature (",degree,"C)")),xlab = '', col='blue', ylim = c(-10,-25), size=5)
trend_lm<-lm(Shuchinsk_Ta$DJF~Shuchinsk_Ta$Years)
trend_lm
abline (trend_lm, col="black", size=5)
temp_djf_subset<-subset(Shuchinsk_Ta,Years>1935)
d<-temp_djf_subset$Years
j<-temp_djf_subset$DJF
sen_slope_djf<-zyp.sen(j~d)
sen_slope_djf
abline(sen_slope_djf$coefficients[1], sen_slope_djf$coefficients[2], col="violet", size=5)
# add R squared coefficient
r2 <- round(summary(trend_lm)$r.squared, 4)
eqn<-bquote(R^2 == .(r2))
text(x=1980, y=-22, eqn, pos=4, cex=1.4)
summary (trend_lm)

# spring
plot(Shuchinsk_Ta$Years, Shuchinsk_Ta$MAM, type="l", main = "Ta MAM  Shuchinsk", ylab = expression(paste("Temperature (",degree,"C)")),xlab = '', col='green', ylim = c(-5,10), size=3)
trend_lm<-lm(Shuchinsk_Ta$MAM~Shuchinsk_Ta$Years)
trend_lm
abline (trend_lm, col="black", size=15)
a<-Shuchinsk_Ta$Years
m<-Shuchinsk_Ta$MAM
sen_slope_mam<-zyp.sen(m~a)
sen_slope_mam
abline(sen_slope_mam$coefficients[1], sen_slope_mam$coefficients[2], col="violet", size=10)
# add R squared coefficient
r2 <- round(summary(trend_lm)$r.squared, 4)
eqn<-bquote(R^2 == .(r2))
text(x=2000, y=8, eqn, pos=3, cex=1.4)
summary (trend_lm)

# summer
plot(Shuchinsk_Ta$Years, Shuchinsk_Ta$JJA, type="l", main = "Ta JJA  Shuchinsk",ylab = expression(paste("Temperature (",degree,"C)")),xlab = '', col='red', ylim = c(12.5,22.5), size=3)
trend_lm<-lm(Shuchinsk_Ta$JJA~Shuchinsk_Ta$Years)
trend_lm
abline (trend_lm, col="black", size=3)
s<-Shuchinsk_Ta$Years
u<-Shuchinsk_Ta$JJA
sen_slope_jja<-zyp.sen(u~s)
sen_slope_jja
abline(sen_slope_jja$coefficients[1], sen_slope_jja$coefficients[2], col="violet", size=3)
# add R squared coefficient
r2 <- round(summary(trend_lm)$r.squared, 4)
eqn<-bquote(R^2 == .(r2))
text(x=1980, y=21.5, eqn, pos=4, cex=1.4)
summary (trend_lm)


# autumn
plot(Shuchinsk_Ta$Years, Shuchinsk_Ta$SON, type="l", main = "Ta SON  Shuchinsk", ylab = expression(paste("Temperature (",degree,"C)")),xlab = '', col='brown', ylim = c(-5,5), size=10)
trend_lm<-lm(Shuchinsk_Ta$SON~Shuchinsk_Ta$Years)
trend_lm
abline (trend_lm, col="black", size=10)
f<-Shuchinsk_Ta$Years
o<-Shuchinsk_Ta$SON
sen_slope_son<-zyp.sen(o~f)
sen_slope_son
abline(sen_slope_son$coefficients[1], sen_slope_son$coefficients[2], col="violet", size=10)
# add R squared coefficient
r2 <- round(summary(trend_lm)$r.squared, 4)
eqn<-bquote(R^2 == .(r2))
text(x=1980, y=-3, eqn, pos=4,cex=1.4)
summary (trend_lm)


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

#View(Shuchinsk_Pa)

# Linear regression models
# P annual Shuchisk with trendline
plot(Shuchinsk_Pa$Years, Shuchinsk_Pa$P, type="l", main = "P annual Shuchinsk", ylab = 'Precipitation, (mm)', xlab = '', col='red', ylim = c(150,650), size=10)
trend_lm<-lm(Shuchinsk_Pa$P~Shuchinsk_Pa$Years)
trend_lm
abline (trend_lm, col="black", size=10)
# Sen's slope annual
library(zyp)
x<-Shuchinsk_Pa$Years
y<-Shuchinsk_Pa$P
sen_slope_ann<-zyp.sen(y~x)
sen_slope_ann
#add sen$slopes to plot
abline(sen_slope_ann$coefficients[1], sen_slope_ann$coefficients[2], col="violet", size=10)
# add R squared coefficient
r2 <- round(summary(trend_lm)$r.squared, 4)
eqn<-bquote(R^2 == .(r2))
text(x=1980, y=500, eqn, pos=4, cex=1.4)
summary (trend_lm)

# P seasonal Shuchisk with trendline and Sen's slope
# winter
plot(Shuchinsk_Pa$Years, Shuchinsk_Pa$DJF, type="l", main = "P DJF  Shuchinsk", ylab = 'Precipitation, (mm)', xlab = '', col='blue', ylim = c(5,100), cex=1.2)
trend_lm<-lm(Shuchinsk_Pa$DJF~Shuchinsk_Pa$Years)
trend_lm
abline (trend_lm, col="black", size=10)
temp_djf_subset<-subset(Shuchinsk_Pa,Years>1935)
d<-temp_djf_subset$Years
j<-temp_djf_subset$DJF
sen_slope_djf<-zyp.sen(j~d)
sen_slope_djf
abline(sen_slope_djf$coefficients[1], sen_slope_djf$coefficients[2], col="violet", size=10)
# add R squared coefficient
r2 <- round(summary(trend_lm)$r.squared, 4)
eqn<-bquote(R^2 == .(r2))
text(x=1980, y=80, eqn, pos=4, cex=1.4)
summary (trend_lm)

# spring
plot(Shuchinsk_Pa$Years, Shuchinsk_Pa$MAM, type="l", main = "P MAM  Shuchinsk", ylab = 'Precipitation (mm)', xlab = '', col='green', ylim = c(15,150), size=3)
trend_lm<-lm(Shuchinsk_Pa$MAM~Shuchinsk_Pa$Years)
trend_lm
abline (trend_lm, col="black", size=10)
a<-Shuchinsk_Pa$Years
m<-Shuchinsk_Pa$MAM
sen_slope_mam<-zyp.sen(m~a)
sen_slope_mam
abline(sen_slope_mam$coefficients[1], sen_slope_mam$coefficients[2], col="violet", size=10)
# add R squared coefficient
r2 <- round(summary(trend_lm)$r.squared, 4)
eqn<-bquote(R^2 == .(r2))
text(x=1985, y=130, eqn, pos=4,cex=1.4)
summary (trend_lm)

# summer
plot(Shuchinsk_Pa$Years, Shuchinsk_Pa$JJA, type="l", main = "P JJA  Shuchinsk", ylab = 'Precipitation (mm)', xlab = '', col='red', ylim = c(15,400),size=3)
trend_lm<-lm(Shuchinsk_Pa$JJA~Shuchinsk_Pa$Years)
trend_lm
abline (trend_lm, col="black", size=10)
s<-Shuchinsk_Pa$Years
u<-Shuchinsk_Pa$JJA
sen_slope_jja<-zyp.sen(u~s)
sen_slope_jja
abline(sen_slope_jja$coefficients[1], sen_slope_jja$coefficients[2], col="violet",size=10)
# add R squared coefficient
r2 <- round(summary(trend_lm)$r.squared, 4)
eqn<-bquote(R^2 == .(r2))
text(x=1980, y=350, eqn, pos=4, cex=1.4)
summary (trend_lm)

# autumn
plot(Shuchinsk_Pa$Years, Shuchinsk_Pa$SON, type="l", main = "P SON  Shuchinsk", ylab = 'Precipitation (mm)', xlab = '', col='brown', ylim = c(20,150), size=3)
trend_lm<-lm(Shuchinsk_Pa$SON~Shuchinsk_Pa$Years)
trend_lm
abline (trend_lm, col="black", size=10)
f<-Shuchinsk_Pa$Years
o<-Shuchinsk_Pa$SON
sen_slope_son<-zyp.sen(o~f)
sen_slope_son
abline(sen_slope_son$coefficients[1], sen_slope_son$coefficients[2], col="violet", size=10)
# add R squared coefficient
r2 <- round(summary(trend_lm)$r.squared, 4)
eqn<-bquote(R^2 == .(r2))
text(x=1980, y=140, eqn, pos=4, cex=1.4)
summary (trend_lm)
dev.off()

