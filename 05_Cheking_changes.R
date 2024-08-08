##########McQueen Test for  precipitation filled series  ########
#################################################################
##Authors: Varón-Ramírez, Vivian Marcela#########################

rm(list=ls())
period=c(1968,1997)
setwd(paste0("C:\\Users\\vimiv\\OneDrive - AGROSAVIA - CORPORACION COLOMBIANA DE INVESTIGACION AGROPECUARIA\\Documentos\\DOCTORADO\\Proyecto_CONAHCYT\\Climate_Data\\Daily_Resolution\\P_",period[1],"_",period[2]))

library(climatol)
library(sf)
library(dplyr)
library(stringr)
library(lubridate)

mean_NA=function(x){mean(x,na.rm=TRUE)}
sum_NA=function(x){sum(x,na.rm=TRUE)}

#loading stations of the period
est=read.table(paste0("Prec_",period[1],"-",period[2],".est"))
ref_station=read.csv("ref_stations.csv")#loading the reference stations
df=data.frame(Station=est$V4)

##Giving date format to df dates
periodt=c(paste0(period[1],"-1-1"),paste0(period[2],"-12-31"))
dates=data.frame(Fecha=format(seq(as.Date(paste0(periodt[1])), as.Date(paste0(periodt[2])), "days"),"%Y-%m-%d"))
dates$Fecha=ymd(dates$Fecha)


#loading clusters
index=c("hartigan")

E_clust=read.csv(paste0("Eco_Clusters","_",index,"_",".csv"))
E_clust=E_clust[,-1]
#E_clust[3,2]=5 #for period 2
#E_clust[2,2]=4 #for period 3

for (i in 2:7) {
  
  if(i%in%c(1,4)){
  
  setwd(paste0("C:\\Users\\vimiv\\OneDrive - AGROSAVIA - CORPORACION COLOMBIANA DE INVESTIGACION AGROPECUARIA\\Documentos\\DOCTORADO\\Proyecto_CONAHCYT\\Climate_Data\\Daily_Resolution\\P_",period[1],"_",period[2]))
  #loading stations of each Eco
  est_Eco=read.table(paste0(paste0("PrecEco",i,"_"),period[1],"-",period[2],".est"))
  #loading incomplete series
  in_ts_Eco=read.csv(paste0("PrecEco",i,"_",period[1],"-",period[2],".csv"))#incomplete series
  in_ts_Eco=in_ts_Eco[,-1]
  
  #loading complete series
  com_ts_Eco=read.csv(paste0("PrecEco",i,"_",period[1],"-",period[2],"_series.csv"))
  com_ts_Eco=com_ts_Eco[,-1]              
  
  #calculating the mean of each complete and incomplete series
  prom_in=apply(in_ts_Eco,MARGIN=2,mean_NA)
  prom_com=apply(com_ts_Eco,MARGIN=2,mean_NA)
  per_change=data.frame(Change=((prom_com-prom_in)/prom_com)*100)
  per_change$Station=est_Eco$V4
  
  df=left_join(df, per_change, by="Station")
  colnames(df)[length(df)]=paste0("Eco_",i)#changing name of the last column
  dates=cbind(dates,com_ts_Eco) 
  }
  else{
    
    N_cluster=E_clust[i,2]
    
    for (j in 1:N_cluster) {
      
      setwd(paste0("C:\\Users\\vimiv\\OneDrive - AGROSAVIA - CORPORACION COLOMBIANA DE INVESTIGACION AGROPECUARIA\\Documentos\\DOCTORADO\\Proyecto_CONAHCYT\\Climate_Data\\Daily_Resolution\\P_",period[1],"_",period[2],"\\Eco",i,"\\Sub",j))  
    
      #loading stations of each Eco
      est_Eco=read.table(paste0(paste0("PrecEco",i,"_"),period[1],"-",period[2],".est"))
      #loading incomplete series
      in_ts_Eco=read.csv(paste0("PrecEco",i,"_",period[1],"-",period[2],".csv"))#incomplete series
      in_ts_Eco=in_ts_Eco[,-1]
      
      #loading complete series
      com_ts_Eco=read.csv(paste0("PrecEco",i,"_",period[1],"-",period[2],"_series.csv"))
      com_ts_Eco=com_ts_Eco[,-1]  
      
      #comparing names
      in_ts_Eco=in_ts_Eco[,names(in_ts_Eco) %in% names(com_ts_Eco)]
      
            #calculating the mean of each complete and incomplete series
      prom_in=apply(in_ts_Eco,MARGIN=2,mean_NA)
      prom_com=apply(com_ts_Eco,MARGIN=2,mean_NA)
      per_change=data.frame(Change=((prom_com-prom_in)/prom_com)*100)
      #per_change=data.frame(Change=(prom_com-prom_in))
      per_change$Station=est_Eco$V4
      
      df=left_join(df, per_change, by="Station")
      colnames(df)[length(df)]=paste0("Eco_",i,"-",j)#changing name of the last column
      dates=cbind(dates,com_ts_Eco) 
    }
   
  }
  
}

setwd(paste0("C:\\Users\\vimiv\\OneDrive - AGROSAVIA - CORPORACION COLOMBIANA DE INVESTIGACION AGROPECUARIA\\Documentos\\DOCTORADO\\Proyecto_CONAHCYT\\Climate_Data\\Daily_Resolution\\P_",period[1],"_",period[2]))

df$compiled=apply(df[,-c(1)],1,sum_NA)
#df=df[which(df$compiled!=0),]
write.csv(df,paste0("Eco_Group_Perc_Change",period[1],"_",period[2],".csv"))
#write.csv(df,paste0("Change_mm",period[1],"_",period[2],".csv"))

###
dat_perc=read.csv(paste0("Eco_Group_Perc_Change",period[1],"_",period[2],".csv"))
problems=dat_perc[dat_perc$compiled < -10|dat_perc$compiled>10, ]
colnames(est)[5]="Station"
head(problems)

##selecting WS with less than 10% of changes in the mean
dat_perc1=dat_perc[-which(dat_perc$compiled < -10),]
dat_perc1=dat_perc1[-which(dat_perc1$compiled >10),]
#write.csv(dat_perc1$Station,"valid_WS_to_calculate_R.csv")##saving the list of valid stations


##calculating mean of changes by eco and group . Table in A2
prom_eco=as.data.frame(apply(dat_perc1[,3:length(dat_perc1)], 2, mean_NA))
#as.data.frame(apply(dat[,3:5], 2, mean_NA))
write.csv(prom_eco,"Eco_Group_prom_Perc_Change.csv")

##saving the file with the complete series
write.csv(dates,"complete_series_whitout_percent_changes.csv")




#### To identify the ecoregion of changes greater than 10%
 Ecoregions <- sf::st_read("C:\\Users\\vimiv\\OneDrive - AGROSAVIA - CORPORACION COLOMBIANA DE INVESTIGACION AGROPECUARIA\\Documentos\\DOCTORADO\\SHP\\ecort08gw\\ecort08gw.shp") 
 table(Ecoregions$DESECON1)
 
 est=read.table(paste0("Prec_",period[1],"-",period[2],".est"))
 colnames(est)[4]="Station"
 
 problems_cords=left_join(problems,est,by="Station")
 problems_cords <- st_as_sf(problems_cords,coords=c("V1","V2"), crs=4326, remove=FALSE)  
 problems_cords <- st_join(problems_cords, left = TRUE, Ecoregions["DESECON1"])
 table(problems_cords$DESECON1)
# 
# plot(problems_cords$geometry, pch=21, cex=0.4, col="purple",bg="purple")
# plot(Ecoregions$geometry, border="gray80", col=NA, add=T)



# ####Checking values by ecoregion
# 
# setwd(paste0("C:\\Users\\vimiv\\OneDrive - AGROSAVIA - CORPORACION COLOMBIANA DE INVESTIGACION AGROPECUARIA\\Documentos\\DOCTORADO\\Proyecto_CONAHCYT\\Climate_Data\\Daily_Resolution\\P_",period[1],"_",period[2]))
# 
# #loading stations of the period
# est=read.table(paste0("Prec_",period[1],"-",period[2],".est"))
# df=data.frame(Station=est$V4)
# 
# 
# for (i in 1:7) {
#  i=2 
# setwd(paste0("C:\\Users\\vimiv\\OneDrive - AGROSAVIA - CORPORACION COLOMBIANA DE INVESTIGACION AGROPECUARIA\\Documentos\\DOCTORADO\\Proyecto_CONAHCYT\\Climate_Data\\Daily_Resolution\\P_",period[1],"_",period[2]))
# #loading stations of each Eco
# est_Eco=read.table(paste0(paste0("PrecEco",i,"_"),period[1],"-",period[2],".est"))
# #loading incomplete series
# in_ts_Eco=read.csv(paste0("PrecEco",i,"_",period[1],"-",period[2],".csv"))#incomplete series
# in_ts_Eco=in_ts_Eco[,-1]
# 
# #loading complete series
# com_ts_Eco=read.csv(paste0("PrecEco",i,"_",period[1],"-",period[2],"_series.csv"))
# com_ts_Eco=com_ts_Eco[,-1]  
# 
# #comparing names
# in_ts_Eco=in_ts_Eco[,names(in_ts_Eco) %in% names(com_ts_Eco)]
# 
# #calculating the mean of each complete and incomplete series
# prom_in=apply(in_ts_Eco,MARGIN=2,mean_NA)
# prom_com=apply(com_ts_Eco,MARGIN=2,mean_NA)
# per_change=data.frame(Change=((prom_com-prom_in)/prom_com)*100)
# per_change$Station=est_Eco$V4
# 
# df=left_join(df, per_change, by="Station")
# colnames(df)[length(df)]=paste0("Eco_",i)#changing name of the last column
# }
# 
# df$compiled=apply(df[,-c(1)],1,sum_NA)
# #df=df[which(df$compiled!=0),]
# write.csv(df,paste0("Eco_Perc_Change",period[1],"_",period[2],".csv"))
# #write.csv(df,paste0("Change_mm",period[1],"_",period[2],".csv"))
# 
# ###
# dat_perc=read.csv(paste0("Eco_Perc_Change",period[1],"_",period[2],".csv"))
# problems=dat_perc[dat_perc$compiled < -10|dat_perc$compiled>10, ]
# colnames(est)[5]="Station"
# head(problems)
# 
# 
# dat_perc1=dat_perc[-which(dat_perc$compiled < -10),]
# dat_perc1=dat_perc1[-which(dat_perc1$compiled >10),]
# 
# prom_eco=as.data.frame(apply(dat_perc1[,3:length(dat_perc1)], 2, mean_NA))
# #as.data.frame(apply(dat[,3:5], 2, mean_NA))
# write.csv(prom_eco,"Eco_prom_Perc_Change.csv")
# 
# 
# dat_mm=read.csv(paste0("Change_mm",period[1],"_",period[2],".csv"))
# dat_mm=dat_mm[-problems$X,]
# prom_eco_mm=as.data.frame(apply(dat_mm[,3:length(dat_mm)], 2, mean_NA))
# write.csv(prom_eco_mm,"prom_eco_mm.csv")
# 
# 
# 
