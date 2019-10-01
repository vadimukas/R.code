# 1.2 Basic graph 
# load, install libs and data
install.packages("pacman") 
library(pacman)
install.packages("ggthemes")
p_load(ggplot2, ggthemes, dplyr, readr)
chilean_exports <- "year,product,export,percentage 2006,copper,4335009500,81 2006,others,1016726518,19 2007,copper,9005361914,86 2007,others,1523085299,14 2008,copper,6907056354,80 2008,others,1762684216,20 2009,copper,10529811075,81 2009,others,2464094241,19 2010,copper,14828284450,85 2010,others,2543015596,15 2011,copper,15291679086,82 2011,others,3447972354,18 2012,copper,14630686732,80 2012,others,3583968218,20 2013,copper,15244038840,79 2013,others,4051281128,21 2014,copper,14703374241,78 2014,others,4251484600,22 2015,copper,13155922363,78 2015,others,3667286912,22"
exports_data <- read_csv(chilean_exports)
p1 <- ggplot(aes(y = export, x = year, colour = product), data = exports_data) + geom_line()
p1