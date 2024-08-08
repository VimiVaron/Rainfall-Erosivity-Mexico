rm(list=ls())

period=c(1968,1997)
setwd(paste0("C:\\Users\\vimiv\\OneDrive - AGROSAVIA - CORPORACION COLOMBIANA DE INVESTIGACION AGROPECUARIA\\Documentos\\DOCTORADO\\Proyecto_CONAHCYT\\Climate_Data\\Daily_Resolution\\P_",period[1],"_",period[2]))

library(climatol)
library(sf)
library(dplyr)
library(cluster)
library(factoextra)
library(NbClust)

index=c("hartigan")
E_clust=read.csv(paste0("Eco_Clusters","_",index,"_",".csv"))
E_clust=E_clust[,-1]

###daily homogenization from monthly metadata by group
#para llamar el archivo llevara un sufijo con el cluster

#Doing the exploratory analysis
for (i in 4:7) {
  
  N_cluster=E_clust[i,2]

  for (j in 1:N_cluster) {
    
    setwd(paste0("C:\\Users\\vimiv\\OneDrive - AGROSAVIA - CORPORACION COLOMBIANA DE INVESTIGACION AGROPECUARIA\\Documentos\\DOCTORADO\\Proyecto_CONAHCYT\\Climate_Data\\Daily_Resolution\\P_",period[1],"_",period[2],"\\Eco",i,"\\Sub",j))  

    homogen(paste0("PrecEco",i), period[1], period[2],std=2,expl = T,onlyQC=TRUE)
    homogen(paste0("PrecEco",i), period[1], period[2], metad = T,std=2,expl = T)
    outrename(paste0("PrecEco",i), period[1], period[2], "x")
  }
}

#homogenization
for (i in 1:7) {
  
  N_cluster=E_clust[i,2]

    for (j in 2:N_cluster) {   
     homogen(paste0("PrecEco",i), period[1], period[2], metad = T,std=2,dz.max = c(25,25),dz.min=c(-25,-25),inht=15)
    
    dahstat(paste0("PrecEco",i), period[1], period[2], stat='series', long = T) #precipitaciones diarias
    dahstat(paste0("PrecEco",i), period[1], period[2], stat='me', long = T)
    dahstat(paste0("PrecEco",i), period[1], period[2], vala = 2, long = T)
    print(paste0(j))
    }
}

i=6
homogen(paste0("PrecEco",i), period[1], period[2],std=2,expl = T,onlyQC=TRUE)
homogen(paste0("PrecEco",i), period[1], period[2], metad = T,std=2,expl = T)
outrename(paste0("PrecEco",i), period[1], period[2], "x")




i=5
homogen(paste0("PrecEco",i), period[1], period[2], metad = T,std=2,dz.max = c(30,35),dz.min=c(-25,-30),inht=60)

dahstat(paste0("PrecEco",i), period[1], period[2], stat='series', long = T) #precipitaciones diarias
dahstat(paste0("PrecEco",i), period[1], period[2], stat='me', long = T)
dahstat(paste0("PrecEco",i), period[1], period[2], vala = 2, long = T)

i=6
homogen(paste0("PrecEco",i), period[1], period[2], metad = T,std=2,dz.max = c(50,55),dz.min=c(-30,-35),inht=60)

dahstat(paste0("PrecEco",i), period[1], period[2], stat='series', long = T) #precipitaciones diarias
dahstat(paste0("PrecEco",i), period[1], period[2], stat='me', long = T)
dahstat(paste0("PrecEco",i), period[1], period[2], vala = 2, long = T)

