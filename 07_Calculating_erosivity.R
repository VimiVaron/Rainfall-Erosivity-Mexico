rm(list=ls())
period=c(1988,2017)
setwd(paste0("C:\\Users\\vimiv\\OneDrive - AGROSAVIA - CORPORACION COLOMBIANA DE INVESTIGACION AGROPECUARIA\\Documentos\\DOCTORADO\\Proyecto_CONAHCYT\\Climate_Data\\Daily_Resolution\\P_",period[1],"_",period[2]))

library(climatol)
library(sf)
library(dplyr)
library(stringr)
library(lubridate)
library(psych)#



valid_comple_series=read.csv("valid_comple_series.csv")
valid_comple_series=valid_comple_series[,-1]
valid_comple_series$Date=ymd(valid_comple_series$Date)
valid_in_series=read.csv("valid_in_series.csv")
valid_in_series=valid_in_series[,-1]

Stations=read.csv("RMSE_WS.csv",sep = ",")
Stations=Stations[,-1]


#counting the number of day with precipitation greater than 12.5 mm
day_greater_12.5=function(x){length(which(x>=12.5))}

Stations$No_day_greater_12.5=apply(valid_comple_series[,1:dim(Stations)[1]],MARGIN = 2,day_greater_12.5)






#calculating erosivity by day
df=valid_comple_series[,1:dim(Stations)[1]]
df[df<12.5]<-NA
df$month=month(valid_comple_series$Date)

R_day_YunXie2016=function(x,m){0.2686*(1+0.5412*(cos((pi*m/6)-(7*pi/6))) )*(x**1.7265)}#defining the function

#Calculating Daily erosivity
Erosivity_day=R_day_YunXie2016(df[,1:dim(Stations)[1]],df$month)
write.csv(Erosivity_day,"YunXie2016_Erosivity_day.csv")

#adding Daily erosivity to obtain monthly erosivity
df=data.frame(ref=unique(valid_comple_series$ref))
for (i in 1:dim(Stations)[1]) {
  p<-tapply(Erosivity_day[,i], as.factor(valid_comple_series$ref), sum,na.rm=TRUE)
  tmp<- data.frame( ref=names(p),p=as.vector(p))
  df=plyr::join(df, tmp, by="ref")
}
colnames(df)[2:length(df)]=names(Erosivity_day[,1:(length(Erosivity_day))])
write.csv(df,"YunXie2016_Erosivity_Month.csv")



Erosivity_month=read.csv("YunXie2016_Erosivity_Month.csv")
Erosivity_month=Erosivity_month[,-1]
Erosivity_month$ref=as.numeric(sapply(Erosivity_month$ref, function(x){strsplit(x,"-")[[1]][1]}))#creating a column with year
df=data.frame(ref=unique(Erosivity_month$ref))
for (i in 1:dim(Stations)[1]) {
  p<-tapply(Erosivity_month[,i+1], as.factor(Erosivity_month$ref), sum,na.rm=TRUE)
  tmp<- data.frame( ref=names(p),p=as.vector(p))
  df=plyr::join(df, tmp, by="ref")
}
colnames(df)[2:length(df)]=names(Erosivity_month[,2:(length(Erosivity_month))])
write.csv(df,"YunXie2016_Erosivity_Year.csv")

prom_erosivity=apply(df[,2:length(df)],MARGIN = 2,mean)
summary(prom_erosivity)
Stations$Erosivity_year_YunXie2016=prom_erosivity

write.csv(Stations,"Erosivity_WS.csv")

