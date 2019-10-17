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

# ii. Running line smoothers
rlcv = supsmu(tt, xx)
rlst = supsmu(tt, xx, span = 0.05)
rllt = supsmu(tt, xx, span = 0.75)

p.rl = plot_ly(x=tt, y=xx, type="scatter", mode="lines", line = data.fmt, name="Data")
p.rl = add_lines(p.rl, x=tt, y=rllt$y, line=line.fmt, name="Span = 0.75")
p.rl = add_lines(p.rl, x=tt, y=rlst$y, line=line.fmt, name="Span = 0.05")
p.rl = add_lines(p.rl, x=tt, y=rlcv$y, line=line.fmt, name="Cross-validated span")
p.rl = layout(p.rl, title = "Running line smoothers")
print(p.rl)

# iii. Kernel smoothers
ks1 = ksmooth(tt, xx, "normal", 20, x.points=tt)
ks2 = ksmooth(tt, xx, "normal", 5, x.points=tt)

p.ks = plot_ly(x=tt, y=xx, type="scatter", mode="lines", line=data.fmt, name="Data")
p.ks = add_lines(p.ks, x=ks1$x, y=ks1$y, line=line.fmt, name="Bandwidth = 20")
p.ks = add_lines(p.ks, x=ks1$x, y=ks2$y, line=line.fmt, name="Bandwidth = 5")
p.ks = layout(p.ks, title = "Kernel smoother")
print(p.ks)

#iv. Smoothing splines
library(gam)
require(mgcv)
sp.base = smooth.spline(tt, xx)
sp.cr = gam(xx~s(tt, bs="cr"))
sp.gam = gam(xx~s(tt))
sp.pred = predict(sp.gam, type="response", se.fit=TRUE)
sp.df = data.frame(x=sp.gam$model[,2], y=sp.pred$fit,
                   lb=as.numeric(sp.pred$fit - (1.96 * sp.pred$se.fit)),
                   ub=as.numeric(sp.pred$fit + (1.96 * sp.pred$se.fit)))
sp.df = sp.df[order(sp.df$x),]

pp = plot_ly(x=tt, y=xx, type="scatter", mode="lines", line=data.fmt, name="Data")
pp = add_lines(pp, x=tt, y=sp.pred$fit, name="GAM", line=list(color="#366092", width=2))
pp = add_ribbons(pp, x=sp.df$x, ymin=sp.df$lb, ymax=sp.df$ub, name="GAM 95% CI", line=list(color="#366092", opacity=0.4, width=0))
pp = add_lines(pp, x=tt, y=predict(sp.base)$y, name="smooth.spline", line=list(color="orange", width=2))
pp = layout(pp, title="Smoothing splines")
print(pp)

# v. LOESS(Locally Estimated Scatterplot Smoother) 
ll.rough = loess(xx~tt, span=0.1)
ll.smooth = loess(xx~tt, span=0.75)

p.ll = plot_ly(x=tt, y=xx, type="scatter", mode="lines", line=data.fmt, name="Data")
p.ll = add_lines(p.ll, x=tt, y=predict(ll.smooth), line=line.fmt, name="Span = 0.75")
p.ll = add_lines(p.ll, x=tt, y=predict(ll.rough), line=line.fmt, name="Span = 0.10")

# some more references ot General Additive models
# http://environmentalcomputing.net/intro-to-gams/
p.ll = layout(p.ll, title="LOESS")
print(p.ll)

# LOESS with confidence intervals
ll.smooth = loess(xx~tt, span=0.75)
ll.pred = predict(ll.smooth, se = TRUE)
ll.df = data.frame(x=ll.smooth$x, fit=ll.pred$fit,
                   lb = ll.pred$fit - (1.96 * ll.pred$se),
                   ub = ll.pred$fit + (1.96 * ll.pred$se))
ll.df = ll.df[order(ll.df$tt),]

p.llci = plot_ly(x=tt, y=xx, type="scatter", mode="lines", line=data.fmt, name="Data")
p.llci = add_lines(p.llci, x=tt, y=ll.pred$fit, name="Mean", line=list(color="#366092", width=2))
p.llci = add_ribbons(p.llci, x=ll.df$tt, ymin=ll.df$lb, ymax=ll.df$ub, name="95% CI", line=list(opacity=0.4, width=0, color="#366092"))
p.llci = layout(p.llci, title = "LOESS with confidence intervals")
print(p.llci)

