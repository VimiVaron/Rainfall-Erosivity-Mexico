rm(list=ls())

###Functions
get_mode <- function(x) {
  freq_table <- table(x)
  modes <- as.numeric(names(freq_table)[freq_table == max(freq_table)])
  return(modes)
}

period=c(1978,2007)
setwd(paste0("C:\\Users\\vimiv\\OneDrive - AGROSAVIA - CORPORACION COLOMBIANA DE INVESTIGACION AGROPECUARIA\\Documentos\\DOCTORADO\\Proyecto_CONAHCYT\\Climate_Data\\Daily_Resolution\\P_",period[1],"_",period[2]))

library(climatol)
library(sf)
library(dplyr)
library(cluster)
library(factoextra)
library(NbClust)

Stations=read.table(paste0("Prec_",period[1],"-",period[2],".est"))#defining a 
colnames(Stations)=c("Longitude","Latitude","Altitude","Code","Name")
Eco_Clusters=data.frame(Eco=seq(1,7,by=1),
                        X1=NA,X2=NA)

for (i in 1:7) {
  load(file=paste0("PrecEco",i,"-m","_", period[1],"-", period[2],".rda"))

hms=read.csv(paste0("PrecEco",i,"-m","_", period[1],"-", period[2],"_series.csv"))
hms=hms[,-1]
mean_scaled_data <- apply(hms, 2, function(x) (x / mean(x,na.rm=TRUE)))
mean_scaled_data=as.matrix(mean_scaled_data)
mean_scaled_data=t(mean_scaled_data)

dist_matrix <- dist(mean_scaled_data, method = "euclidean")

hc <- hclust(dist_matrix, method = "ward.D2")
plot(hc)

index=c("hartigan")
#index=c("kl", "hartigan", "cindex", "db", "silhouette", "duda", "pseudot2", "mcclain", "gamma", "gplus", "dunn")
#index=c("hartigan","db", "dunn", "silhouette")
N_clusters=vector(mode="numeric",length = length(index))
for (j in 1:length(index)) {
    res.nbclust <- NbClust(mean_scaled_data, distance = "euclidean",
                         min.nc = 2, max.nc =round(sqrt(dim(hms)[2])) , #
                         method = "ward.D2", index =paste0(index[j]) )
  #paste0(index[i])
  N_clusters[j] =res.nbclust$Best.nc[1]
  Eco_Clusters[i,j+1]=N_clusters[j]
  print(paste0(j))
}

#N_clusters
k_clusters=max(get_mode(N_clusters))
#k_clusters
Eco_Clusters[i,1+length(index)+1]=k_clusters


clusters <- cutree(hc, k = k_clusters)  # Cut dendrogram to obtain 3 clusters

temp=data.frame(Code=as.numeric(est.c[1:nei,4]),
                  Group=clusters)

colnames(temp)[2]=paste0("Eco",i)
Stations=left_join(Stations,temp,by="Code")
print(paste0(j))

}

write.csv(Eco_Clusters,paste0("Eco_Clusters","_",paste0(index),"_",".csv"))
write.csv(Stations,paste0("Station_Clusters","_",paste0(index),"_",".csv"))

#####Creating the new .est and .dat files
S_clust=read.csv(paste0("Station_Clusters","_",index,"_",".csv"))
S_clust=S_clust[,-1]
E_clust=read.csv(paste0("Eco_Clusters","_",index,"_",".csv"))
E_clust=E_clust[,-1]

for (i in 2:7) {
  
  setwd(paste0("C:\\Users\\vimiv\\OneDrive - AGROSAVIA - CORPORACION COLOMBIANA DE INVESTIGACION AGROPECUARIA\\Documentos\\DOCTORADO\\Proyecto_CONAHCYT\\Climate_Data\\Daily_Resolution\\P_",period[1],"_",period[2]))
  N_cluster=E_clust[i,2]
  est=read.table(paste0("PrecEco",i,"_",period[1],"-",period[2],".est"))
  dat=read.csv(paste0("PrecEco",i,"_",period[1],"-",period[2],".csv"))
  dat=dat[,-1]
  
  for (j in 1:N_cluster) {
    
    codes=S_clust[which(S_clust[,paste0("Eco",i)]==j),"Code"]
    sub_est=est[which(est[,4]%in%codes),]
    sub_dat=dat[,which(est[,4]%in%codes)]
    setwd(paste0("C:\\Users\\vimiv\\OneDrive - AGROSAVIA - CORPORACION COLOMBIANA DE INVESTIGACION AGROPECUARIA\\Documentos\\DOCTORADO\\Proyecto_CONAHCYT\\Climate_Data\\Daily_Resolution\\P_",period[1],"_",period[2],"\\Eco",i,"\\Sub",j))
    
    write.table(sub_est,paste0("PrecEco",i,"_",period[1],"-",period[2],".est"),row.names=FALSE,col.names=FALSE)
    write.csv(sub_dat,paste0("PrecEco",i,"_",period[1],"-",period[2],".csv"))
    sub_dat=as.matrix(sub_dat)
    write(sub_dat,paste0(paste0("PrecEco",i,"_"),period[1],"-",period[2],".dat"))
    print(paste0(i))
    
  }
}


