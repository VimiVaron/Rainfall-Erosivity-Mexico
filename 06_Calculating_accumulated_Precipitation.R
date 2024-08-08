rm(list=ls())
period=c(1968,1997)
setwd(paste0("C:\\Users\\vimiv\\OneDrive - AGROSAVIA - CORPORACION COLOMBIANA DE INVESTIGACION AGROPECUARIA\\Documentos\\DOCTORADO\\Proyecto_CONAHCYT\\Climate_Data\\Daily_Resolution\\P_",period[1],"_",period[2]))

library(climatol)
library(sf)
library(dplyr)
library(stringr)
library(lubridate)
library(Metrics)#RMSE
library(psych)#describeBy function


###loading complete series
comp_series=read.csv("complete_series_whitout_percent_changes.csv")
comp_series=comp_series[,-1]
valid_stat=read.csv("valid_WS_to_calculate_R.csv")
valid_stat$station=paste0("X",valid_stat$x)
valid_stat=valid_stat[,-1]
valid_comple_series=comp_series[,which(names(comp_series)%in%valid_stat$station)]


##loading incomplete series
#loading incomplete series
in_series=read.csv(paste0("Prec_",period[1],"-",period[2],".csv"))#incomplete series
in_series=in_series[,-1]
in_series=in_series[,c(length(in_series),1:(length(in_series)-2))]
valid_in_series=in_series[,which(names(in_series)%in%valid_stat$station)]
valid_in_series=valid_in_series[,names(valid_in_series)%in%names(valid_comple_series)]


valid_comple_series=valid_comple_series[,names(valid_in_series)] #organizing in the same order.

periodt=c(paste0(period[1],"-1-1"),paste0(period[2],"-12-31"))
valid_comple_series$Date=ymd(format(seq(as.Date(paste0(periodt[1])), as.Date(paste0(periodt[2])), "days"),"%Y-%m-%d"))
valid_in_series$Date=ymd(format(seq(as.Date(paste0(periodt[1])), as.Date(paste0(periodt[2])), "days"),"%Y-%m-%d"))
valid_comple_series$ref=paste0(year(valid_comple_series$Date),"-",month(valid_comple_series$Date))
valid_in_series$ref=paste0(year(valid_in_series$Date),"-",month(valid_in_series$Date))

#write.csv(valid_comple_series,"valid_comple_series.csv")
#write.csv(valid_in_series,"valid_in_series.csv")


#####Calculating accumulated precipitation by month
df=data.frame(ref=unique(valid_comple_series$ref))
for (i in 1:(length(valid_in_series)-2)) {
p<-tapply(valid_in_series[,i], as.factor(valid_in_series$ref), sum)
tmp<- data.frame( ref=names(p),p=as.vector(p))
df=plyr::join(df, tmp, by="ref")
}
rm(p,tmp)
colnames(df)[2:length(df)]=names(valid_in_series[,1:(length(valid_in_series)-2)])
write.csv(df,"Accum_prec_monthly_incomplete.csv")


df=data.frame(ref=unique(valid_comple_series$ref))
for (i in 1:(length(valid_comple_series)-2)) {
  p<-tapply(valid_comple_series[,i], as.factor(valid_comple_series$ref), sum)
  tmp<- data.frame( ref=names(p),p=as.vector(p))
  df=plyr::join(df, tmp, by="ref")
}
colnames(df)[2:length(df)]=names(valid_comple_series[,1:(length(valid_comple_series)-2)])
write.csv(df,"Accum_prec_monthly_complete.csv")




valid_comple_series$ref=paste0(year(valid_comple_series$Date))
valid_in_series$ref=paste0(year(valid_in_series$Date))
df=data.frame(ref=unique(valid_comple_series$ref))

for (i in 1:(length(valid_in_series)-2)) {
  p<-tapply(valid_in_series[,i], as.factor(valid_in_series$ref), sum)
  tmp<- data.frame( ref=names(p),p=as.vector(p))
  df=plyr::join(df, tmp, by="ref")
}
rm(p,tmp)
colnames(df)[2:length(df)]=names(valid_in_series[,1:(length(valid_in_series)-2)])
write.csv(df,"Accum_prec_annual_incomplete.csv")

df=data.frame(ref=unique(valid_comple_series$ref))
for (i in 1:(length(valid_comple_series)-2)) {
  p<-tapply(valid_comple_series[,i], as.factor(valid_comple_series$ref), sum)
  tmp<- data.frame( ref=names(p),p=as.vector(p))
  df=plyr::join(df, tmp, by="ref")
}
colnames(df)[2:length(df)]=names(valid_comple_series[,1:(length(valid_comple_series)-2)])
write.csv(df,"Accum_prec_annual_complete.csv")

rm(p,tmp,df)

a_m_incomplete=read.csv("Accum_prec_monthly_incomplete.csv")
a_m_incomplete=a_m_incomplete[,-1]
a_m_complete=read.csv("Accum_prec_monthly_complete.csv")
a_m_complete=a_m_complete[,-1]
a_a_incomplete=read.csv("Accum_prec_annual_incomplete.csv")
a_a_complete=read.csv("Accum_prec_annual_complete.csv")

a_m_incomplete$ref<-as.numeric(sapply(a_m_incomplete$ref, function(x){strsplit(x,"-")[[1]][2]}))

df=data.frame(ref=c(1:12))
for (i in 2:length(a_m_incomplete)) {
  p<-tapply(a_m_incomplete[,i], as.factor(a_m_incomplete$ref), mean,na.rm=TRUE)
  tmp<- data.frame( ref=names(p),p=as.vector(p))
  df=plyr::join(df, tmp, by="ref")
}
colnames(df)[2:length(df)]=names(a_m_incomplete[,2:(length(a_m_incomplete))])
write.csv(df,"Prom_prec_monthly_incomplete.csv")

a_m_complete$ref<-as.numeric(sapply(a_m_complete$ref, function(x){strsplit(x,"-")[[1]][2]}))
df=data.frame(ref=c(1:12))
for (i in 2:length(a_m_complete)) {
  p<-tapply(a_m_complete[,i], as.factor(a_m_complete$ref), mean,na.rm=TRUE)
  tmp<- data.frame( ref=names(p),p=as.vector(p))
  df=plyr::join(df, tmp, by="ref")
}
colnames(df)[2:length(df)]=names(a_m_complete[,2:(length(a_m_complete))])
write.csv(df,"Prom_prec_monthly_complete.csv")


prom_in_monthly=read.csv("Prom_prec_monthly_incomplete.csv")
prom_in_monthly=prom_in_monthly[,-1]
prom_comp_monthly=read.csv("Prom_prec_monthly_complete.csv")
prom_comp_monthly=prom_comp_monthly[,-1]

df=data.frame(ref=c(1:12))
for (i in 2:length(prom_comp_monthly)) {
  per_change=data.frame(Change=((prom_comp_monthly[,i]-prom_in_monthly[,i])/prom_comp_monthly[,i])*100)
  per_change$ref=df$ref
  df=left_join(df,per_change,by="ref")
}
colnames(df)[2:length(df)]=names(a_m_complete[,2:(length(a_m_complete))])
write.csv(df,"percent_change_Prom_prec_monthly.csv")




##loading WS information
est=read.table(paste0("Prec_",period[1],"-",period[2],".est"))

#loading ecoregions shp 
Ecoregions <- sf::st_read("C:\\Users\\vimiv\\OneDrive - AGROSAVIA - CORPORACION COLOMBIANA DE INVESTIGACION AGROPECUARIA\\Documentos\\DOCTORADO\\Proyecto_CONAHCYT\\SHP\\ecort08gw\\ecort08gw.shp") 
plot(Ecoregions[8])

#getting spatial data
est <- st_as_sf(est,coords=c("V1","V2"), crs=4326, remove=FALSE) 
#making spatial join
est_Ecoregions <- st_join(est, left = TRUE, Ecoregions["DESECON1"])
#write.csv(est_Ecoregions,"est_ecoregions.csv")

#obtaining vectors with code station by ecoregion
Eco=as.data.frame(table(est_Ecoregions$DESECON1))
Eco
Eco=as.vector(Eco[,1])

colnames(est_Ecoregions)[4]="Code"

Stations=as.data.frame(names(prom_in_monthly[,-1]))
colnames(Stations)="Code"
Stations$Code<-as.numeric(sapply(Stations$Code, function(x){strsplit(x,"X")[[1]][2]}))
Stations=left_join(Stations,est_Ecoregions[,c(1,2,3,4,5,6)],by="Code")


###calculating RMSE by month by ecoregion

df2=matrix(data=NA,nrow = 7,ncol = 12) #ncol equal to number of WS plus one
df2=as.data.frame(df2)


for (i in 2:length(Eco)) {
  for (j in 1:12) {
    
    
    obs=t(prom_in_monthly[prom_in_monthly$ref==j,(which(Stations$DESECON1==Eco[i])+1)])
    pred=t(prom_comp_monthly[prom_comp_monthly$ref==j,(which(Stations$DESECON1==Eco[i])+1)])
    
   RMSE<-rmse(obs,pred)
  df2[i,j]=RMSE
  
 
  }
}

rownames(df2)=c(1:7)
colnames(df2)=c("January","February","March","April","May","June","July","August","September","October","November","December")
write.csv(df2,"RMSE_month_ecoregion.csv")



###calculating RMSE WS

Stations$RMSE=NA
#

for (i in 2:length(prom_comp_monthly)) {
  
    obs=prom_in_monthly[,i]
    pred=prom_comp_monthly[,i]
    
    RMSE<-rmse(obs,pred)
    Stations[i-1,"RMSE"]=RMSE
    
    
  }

summary(Stations$RMSE)
colnames(Stations)[c(2,3,4,5,6)]=c("Longitude","Latitude","Altitude","Name","Ecoregion")
Stations=Stations[,-7]
write.csv(Stations,"RMSE_WS.csv")



statistics <- describeBy(Stations[, c("RMSE")], group = Stations$DESECON1)

table=as.data.frame(statistics[[names(statistics)[1]]])
table=rbind(table,statistics[[names(statistics)[2]]])
table=rbind(table,statistics[[names(statistics)[3]]])
table=rbind(table,statistics[[names(statistics)[4]]])
table=rbind(table,statistics[[names(statistics)[5]]])
table=rbind(table,statistics[[names(statistics)[6]]])
table=rbind(table,statistics[[names(statistics)[7]]])

rownames(table)=names(statistics)

write.csv(table,"statistic_RMSE_WS_ecoregion.csv")







