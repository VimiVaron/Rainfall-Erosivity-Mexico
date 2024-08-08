############# Calculating statistics for missing values in time series  ##################
##########################################################################################
########Authors: Varón-Ramírez, Viviana Marcela; Arroyo Cruz, Carlos Eduardo##############

rm(list=ls())
#################

#directory
dir="C:\\Users\\vimiv\\OneDrive - AGROSAVIA - CORPORACION COLOMBIANA DE INVESTIGACION AGROPECUARIA\\Documentos\\DOCTORADO\\Proyecto_CONAHCYT"
setwd(dir)

#loading libraries
library(imputeTS)#ts
library(stringr)#to removing structures
library(lubridate)#managing date
library(Tides)#To identify gaps in TS
library(sf)
library(climatol)
library(dplyr)

#Defining a period
period=c(1988,2017)

#####
####Generating statistics for missing values by station
#####

# #creating a list of files. Each file corresponds to a meteorological station
# files <- list.files(path= 'Climate_Data/Stations/', pattern = ".csv", full.names = F)
# #removing ".csv" of files names
# files <- str_remove(files, ".csv")
# 
# #Defining an empty dataframe
# df <- data.frame(Station=integer(),
#                  Length_TS=integer(),#Length of time series
#                  Number_Miss_Val=integer(),#Number of Missing Values
#                  Number_Gaps=integer(),#Number of gaps
#                  Average_Gap_Size=numeric(),#Average Gap Size
#                  Perc_Miss_Val=numeric(),#Percentage of Missing Values
#                  Longest_Gap_Size=integer(),
#                  Most_Freq_Gap_Size=integer(),
#                  Number_Gap_Gt_5=integer(),#Number of gaps greater than 11
#                  Initial_date=character(),
#                  Final_date=character()
#                  )
# 
# 
# for (i in c(1:length(files))) {
#     #defining the station
#   Station=files[i]
#   #reading the file
#   est=read.csv(paste0("Climate_Data/Stations/",Station,".csv"))
#   #giving format to dates
#   est[,1]=dmy(est[,1])
#   
#   #replacing "Nulo" by NA
#   for (j in 2:dim(est)[2]) {
#     est[,j]=replace(est[,j],est[,j]=="Nulo",NA)
#     est[,j]=as.numeric(est[,j])
#   }
#   
#    #Creating a date sequence from start to end
   periodt=c(paste0(period[1],"-1-1"),paste0(period[2],"-12-31"))
#   dates=data.frame(Fecha=format(seq(as.Date(paste0(periodt[1])), as.Date(paste0(periodt[2])), "days"),"%Y-%m-%d"))
#   dim(dates)
#    #join variable data
#   timeS<- plyr::join(dates, est, by="Fecha")
#   #defining start date 
#   start_date=c(year(min(dates$Fecha)),month(min(dates$Fecha)),day(min(dates$Fecha)))
#   
#   #creating a time series
#   daily.ts=ts(timeS$Prec,start=c(start_date),frequency = 365)
#   #calculating some statistics
#   stat=as.data.frame(statsNA(daily.ts,print_only=FALSE))
#   
#   #filling the empty df with the information of each station
#   df[i,1]=Station
#   df[i,2]=stat[1,1]
#   df[i,3]=stat[1,2]
#   df[i,4]=stat[1,3]
#   df[i,5]=stat[1,4]
#   df[i,6]=(stat[1,2]*100)/stat[1,1]#Calculating percentage of missing values
#   df[i,7]=stat[1,6]
#   df[i,8]=stat[1,7]
#   df[i,9]=sum(stat[5:dim(stat)[1],9])#adding the number of gaps equal or greater than 5 NA
#   df[i,10]=paste0(min(est[,1]))
#   df[i,11]=paste0(max(est[,1]))
# 
#   print(i)
#   
# }
# 
# write.csv(df,file=paste0("Climate_Data/Daily_Resolution/NA_Stats_",period[1],"_",period[2],".csv"))

#loading MS statistics (to filter climate series with less than 80% of missing data)
statistics=read.csv(paste0("Climate_Data/Daily_Resolution/NA_Stats_",period[1],"_",period[2],".csv"))

#loading MS data (location, elevation, and code)
MS=read.csv("Climate_Data/DatosEstacion.csv")

# 
# ##Identifying climate series with less than 20% of missing data
Valid_stations=statistics[which(statistics$Perc_Miss_Val<=20),]

##Selecction MS information of identified climate series
MS_1=MS[which(MS$ESTACION%in%Valid_stations$Station),]
MS_2=MS_1[,c(1,9,8,10,1,2)]
MS_2$ESTACION=paste0(MS_2$ESTACION,".csv")
MS_2$ALTITUD_msnm=as.numeric(gsub(",", "", MS_2$ALTITUD_msnm))
MS_2=MS_2[,-c(1)]
MS_2$ALTITUD_msnm=as.integer(MS_2$ALTITUD_msnm)

#saving .est file
setwd(paste0("C:\\Users\\vimiv\\OneDrive - AGROSAVIA - CORPORACION COLOMBIANA DE INVESTIGACION AGROPECUARIA\\Documentos\\DOCTORADO\\Proyecto_CONAHCYT\\Climate_Data\\Daily_Resolution\\P_",period[1],"_",period[2]))
write.table(MS_2,paste0("0.1_Prec_",period[1],"-",period[2],".est"),row.names=FALSE,col.names=FALSE)

###creating a file with climatic series
#loading .est file
est=read.table(paste0("0.1_Prec_",period[1],"-",period[2],".est"))

##Giving date format to df dates
dates=data.frame(Fecha=format(seq(as.Date(paste0(periodt[1])), as.Date(paste0(periodt[2])), "days"),"%Y-%m-%d"))
dates$Fecha=ymd(dates$Fecha)

#creating a file with the available WS for the period
for (i in c(1:dim(est)[1])) {

  #dim(est)[1]
  #defining the station
  Station=est$V4
  #reading the file, remember to change the directory
  data=read.csv(paste0("C:/Users/vimiv/OneDrive - AGROSAVIA - CORPORACION COLOMBIANA DE INVESTIGACION AGROPECUARIA/Documentos/DOCTORADO/Proyecto_CONAHCYT/Climate_Data/Stations/",Station[i],".csv"),header=TRUE)
  Prec=data[,c(1,2)]
  Prec$Fecha=dmy(Prec$Fecha)

  dates<- left_join(dates, Prec, by="Fecha",multiple = "first")
  print(paste0(i))
}

dat_prec=dates
#removing "Nulo" for missing data
for (j in 2:dim(dat_prec)[2]) {
  dat_prec[,j]=replace(dat_prec[,j],dat_prec[,j]=="Nulo",NA)
  dat_prec[,j]=as.numeric(dat_prec[,j])
}

#saving .csv with climatic series
colnames(dat_prec)[2:length(dat_prec)] =est$V4
write.csv(dat_prec,paste0("0.1_Prec_",period[1],"-",period[2],".csv"))

#saving the .dat file
dat_prec1=as.matrix(dat_prec[,-c(1)])
write(dat_prec1,paste0("0.1_Prec_",period[1],"-",period[2],".dat"))
rm(dat_prec1)


### Identifying long sequences with zeros and NA´s 
#load functions of the file "functions"

#loading

dat=read.csv(paste0("0.1_Prec","_",period[1],"-",period[2],".csv"))
dat=dat[,-c(1,2)]

### creating a df with code station and a colum with -9999 values to be replaced
df=data.frame(Station=est$V4,
              longest=rep(-9999,dim(dat)[2]))

#identifying NA and zero longest sequences for each series of the period
for (i in 1:dim(dat)[2]) {
  #dim(dat)[2]
  vector=as.vector(dat[,i])
  df[i,2]=length(identify_largest_zero_na_sequence(vector))
}

break_years=seq(from=0,to= 10958, by=365)
hist=hist(df$longest,breaks=break_years)
hist$counts
hist$breaks

#write.csv(df,"longest_zero_NA_sequences.csv")
df=read.csv("longest_zero_NA_sequences.csv")

stat_rem=df[which(df$longest>2190),]#stations to remove (2190 corresponds to break greather than six years)
pos=which(est$V4%in%stat_rem$Station)
est=est[-pos,]
dat=dat[,-pos]

write.table(est,paste0("0.2_Prec_",period[1],"-",period[2],".est"),row.names=FALSE,col.names=FALSE)
dat$Fecha=dat_prec$Fecha
dat=dat[,c(length(dat),1:length(dat)-1)]
write.csv(dat,paste0("0.2_Prec_",period[1],"-",period[2],".csv"))

#saving the .dat file
dat_1=as.matrix(dat[,-c(1)])
write(dat_1,paste0("0.2_Prec_",period[1],"-",period[2],".dat"))
rm(dat_1)
### Identifying years with 0 precipitation

dat$Year=year(dates$Fecha)#creating a column with year
annual_prec=data.frame(Year=seq(from=period[1],to=period[2],by=1))

dat=dat[,-1]

#calculating the accumulated precipitation by year
for (i in 1:length(dat[,-1])) {
t=tapply(dat[,i], as.factor(dat[,length(dat)]), sum, na.rm=T)
annual_prec=cbind(annual_prec,t)
}

colnames(annual_prec)[2:length(annual_prec)]=est$V4
write.csv(annual_prec,"annual_prec.csv")

annual_prec=read.csv("annual_prec.csv")
annual_prec=annual_prec[,-1]

##looking for years with zero precipitation
which(annual_prec[2]==0)
df1=data.frame(Stations=est$V4,
               zero_seq=NA,
               start=NA,
               end=NA,
               long_seq=NA)

for (i in 2:length(annual_prec)) {
sequences <- identify_zero_sequences(annual_prec[,i])
if(length(sequences)==0){
  df1[i-1,2]=0
  df1[i-1,3]=NA
  df1[i-1,4]=NA
  df1[i-1,5]=NA
}else{
  larg=identify_largest_consecutive_zero_sequence_positions(annual_prec[,i])
df1[i-1,2]=length(sequences)
df1[i-1,3]=annual_prec[larg[1],1]
df1[i-1,4]=annual_prec[larg[length(larg)],1]
df1[i-1,5]=length(larg)
}
}

write.csv(df1,"zero_seq_annual.csv")
df1=read.csv("zero_seq_annual.csv")
df1=df1[,-1]
table(df1$long_seq)
ref_posit=which(df1$long_seq>=2)
ref_stations=df1[which(df1$long_seq>=2),1]#list of reference stations
write.csv(ref_stations,"ref_stations.csv")

#adding a Date column to series
periodt=c(paste0(period[1],"-1-1"),paste0(period[2],"-12-31"))
dat$Date=format(seq(as.Date(paste0(periodt[1])), as.Date(paste0(periodt[2])), "days"),"%Y-%m-%d")
dat$Date=ymd(dat$Date)

dat_copy=dat

for (i in ref_posit) {
years_replace_NA=seq(from=df1[i,3],to=df1[i,4],by=1)
for (j in 1:1957) {
  if(year(dat[j,dim(dat)[2]])%in%years_replace_NA){
    dat[j,i]=NA
  }else{
    dat[j,i]=dat[j,i]
   }
  }
}
NA_sum=function(x){sum(is.na(x))}

na_mis_dat_copy=apply(dat_copy,MARGIN = 2,NA_sum)
na_mis_dat=apply(dat,MARGIN = 2,NA_sum)

positions=which(na_mis_dat!=na_mis_dat_copy)
na_mis_dat[positions]

na_mis_dat_copy[positions]
#saving files
write.csv(dat,paste0("Prec_",period[1],"-",period[2],".csv"))
dat_1=dat[,-c(dim(dat)[2])]
dat_1=dat[,-c(dim(dat)[2])]
dat_1=as.matrix(dat_1)
write(dat_1,paste0("Prec_",period[1],"-",period[2],".dat"))
write.table(est,paste0("Prec_",period[1],"-",period[2],".est"),row.names=FALSE,col.names=FALSE)


### Separating stations by ecoregion

#reading available stations for the period
dat_prec=read.csv(paste0("Prec_",period[1],"-",period[2],".csv"))
dat_prec=dat_prec[,-c(1,dim(dat_prec)[2])]
est=read.table(paste0("Prec_",period[1],"-",period[2],".est"))

#loading ecoregions shp 
Ecoregions <- sf::st_read("C:\\Users\\vimiv\\OneDrive - AGROSAVIA - CORPORACION COLOMBIANA DE INVESTIGACION AGROPECUARIA\\Documentos\\DOCTORADO\\Proyecto_CONAHCYT\\SHP\\ecort08gw\\ecort08gw.shp") 
plot(Ecoregions[8])

#getting spatial data
est <- st_as_sf(est,coords=c("V1","V2"), crs=4326, remove=FALSE)  

#making spatial join
est_Ecoregions <- st_join(est, left = TRUE, Ecoregions["DESECON1"])

#obtaining vectors with code station by ecoregion
Eco=as.data.frame(table(est_Ecoregions$DESECON1))
Eco
sum(Eco$Freq)
Eco=as.vector(Eco[,1])


summary(est_Ecoregions$DESECON1)

for (i in 1:length(Eco)) {
  
  est_e=as.data.frame(est_Ecoregions[which(est_Ecoregions$DESECON1==Eco[i]),c(1,2,3,4,5)]);est_e=est_e[,1:5]
  write.table(est_e,paste0("PrecEco",i,"_",period[1],"-",period[2],".est"),row.names=FALSE,col.names=FALSE)
  
  dat_prec_Eco=dat_prec[,which(est_Ecoregions$DESECON1==Eco[i])]
  write.csv(dat_prec_Eco,paste0("PrecEco",i,"_",period[1],"-",period[2],".csv"))
  dat_prec_Eco=as.matrix(dat_prec_Eco)
  write(dat_prec_Eco,paste0(paste0("PrecEco",i,"_"),period[1],"-",period[2],".dat"))
  print(paste0(i))
}

