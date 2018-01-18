library(hydroTSM)
library(qmap)
library(xts)
library(hydroGOF)

setwd("C:\\Users\\Ranju\\Desktop\\Bias Correction Practise")


#generate data
dates<-seq.Date(as.Date("1976-01-01"),as.Date("2005-12-31"),by=1)
dates_fut<-seq.Date(as.Date("2016-01-01"),as.Date("2040-12-31"),by=1)

obs_data<-read.csv("tmax_obs.csv",header=T)
sim_data<-read.csv("tmax_raw.csv",header=T)
fut<-read.csv("tmax_rcp45_NF.csv",header=T)
obs<-xts(obs_data[,2],dates)
sim<-xts(sim_data[,2],dates)
future<-xts(fut[,2],dates_fut)


allcorrected_b=NULL
allcorrected_f=NULL

for(m in 0:11){
  
  print(m)
  month_obs<-obs[.indexmon(obs) %in% c(m)]
  month_sim<-sim[.indexmon(sim) %in% c(m)]
  month_future<-future[.indexmon(future) %in% c(m)]
  dmonth_obs<-data.frame(month_obs,row.names=NULL)
  dmonth_sim<-data.frame(month_sim,row.names=NULL)
  
  dmonth_future<-data.frame(month_future,row.names=NULL)
  
  fitQ<-fitQmapRQUANT(dmonth_obs[,1],dmonth_sim[,1],qstep=0.01,nboot=1,wet.day=T)
  doQ_b<-doQmapRQUANT(dmonth_sim[,1],fitQ,"tricub")
  doQ_f<-doQmapRQUANT(dmonth_future[,1],fitQ,"tricub")
  
  xdata_b<-merge(month_obs,month_sim,doQ_b)
  xdata_f<-merge(month_future,doQ_f)
  allcorrected_b=rbind(allcorrected_b,xdata_b)
  allcorrected_f=rbind(allcorrected_f,xdata_f)
  
}

write.csv(allcorrected_b,"base_corrected.csv",row.names=F)
write.csv(allcorrected_f,"future_corrected.csv",row.names=F)


