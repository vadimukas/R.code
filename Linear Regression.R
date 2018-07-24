# How to add Trend Lines in R Using Plotly
# EuStockMarkets
# first load plotly
# https://www.r-bloggers.com/how-to-add-trend-lines-in-r-using-plotly/

library(plotly)

# 1. Global trend lines
data("EuStockMarkets")
xx = EuStockMarkets[,1]
x.info = attr(xx, "tsp")
tt = seq(from=x.info[1], to = x.info[2], by=1/x.info[3])
data.fmt = list(color=rgb(0.8,0.8,0.8,0.8), width=4)
line.fmt = list(dash="solid", width = 1.5, color=NULL)

ti = 1:length(xx)
m1 = lm(xx~ti)
m2 = lm(xx~ti+I(ti^2))
m3 = lm(xx~ti+I(ti^2)+I(ti^3))

library(plotly)
p.glob = plot_ly(x=tt, y=xx, type="scatter", mode="lines", line=data.fmt, name="Data")
p.glob = add_lines(p.glob, x=tt, y=predict(m1), line=line.fmt, name="Linear")
p.glob = add_lines(p.glob, x=tt, y=predict(m2), line=line.fmt, name="Quadratic")
p.glob = add_lines(p.glob, x=tt, y=predict(m3), line=line.fmt, name="Cubic")
p.glob = layout(p.glob, title = "Global smoothers")
print(p.glob)

# 2. Local smoothers
# i. Moving averages

data("Nile")
xx = Nile
x.info = attr(xx, "tsp")
tt = seq(from=x.info[1], to = x.info[2], by=1/x.info[3])

rmean20 = stats::filter(xx, rep(1/20, 20), side=2)
rmean5 = stats::filter(xx, rep(1/5, 5), side=2)

require(plotly)
p.rm = plot_ly(x=tt, y=xx, type="scatter", mode="lines", line=data.fmt, name="Data")
p.rm = add_lines(p.rm, x=tt, y=rmean20, line=line.fmt, name="Bandwidth = 20")
p.rm = add_lines(p.rm, x=tt, y=rmean5, line=line.fmt, name="Bandwidth = 5")
p.rm = layout(p.rm, title = "Running mean")
print(p.rm)