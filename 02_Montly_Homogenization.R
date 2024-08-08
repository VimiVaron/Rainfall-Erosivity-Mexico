rm(list=ls())

period=c(1978,2007)
setwd(paste0("C:\\Users\\vimiv\\OneDrive - AGROSAVIA - CORPORACION COLOMBIANA DE INVESTIGACION AGROPECUARIA\\Documentos\\DOCTORADO\\Proyecto_CONAHCYT\\Climate_Data\\Daily_Resolution\\P_",period[1],"_",period[2]))

library(climatol)
library(sf)
library(dplyr)
library(deldir)#Thiessen polygons

###Quality control, monthly aggregation and exploratory analysis
for (i in 1:7) {
#i=1
homogen(paste0("PrecEco",i), period[1], period[2], onlyQC=TRUE)
dd2m(paste0("PrecEco",i), period[1],period[2],valm=1) ##obtaining monthly series. This function adds a sufix "-m" to .dat files
homogen(paste0("PrecEco",i,"-m"), period[1], period[2],expl=T,std=2,annual="total") ##Exploring the monthly series
outrename(paste0("PrecEco",i,"-m"), period[1], period[2], "x")#renaming the monthly exploratory analysis
}

### filling a matrix Tengo que hacer todos los ficheros de todos las ecoregions, hacer el exploratorio mensual y llenar una matriz 

#calling monthly parameters
param=read.csv("Montly_Parameters.csv",sep = ";")
### monthly homogenization and 

for (i in 1:7){
  #i=1
  homogen(paste0("PrecEco",i,"-m"), period[1], period[2],std=2,annual="total",inht=param[i,2],dz.max = c(param[i,3],param[i,4]),dz.min =c(param[i,5],param[i,6]),nclust = 500 ) ##Montly Homogenization
  dahstat(paste0("PrecEco",i,"-m"), period[1], period[2], stat='series', long = T) #monthly precipitation
  }
 


### We´ll modify clusters by thiessen poligons or calculating centroids
est=read.table(paste0("PrecEco",i,"_",period[1],"-",period[2],".est"))
clust=read.csv("Clusters.csv")
clust=clust[,-1]

i=2
points=clust[which(!is.na(clust[paste0("Eco",i)])),c(1,2)]
v2 <- voronoipolygons(points)
v2@data$clust <- clust[which(!is.na(clust[paste0("Eco",i)])),paste0("Eco",i)]

# Define a color palette based on a gradient of colors
color_palette1 <- colorRampPalette(c("lightblue1", "olivedrab1", "lightpink"))

# Plot the SpatialPolygonsDataFrame with colors based on the numeric categories
plot(v2, col = color_palette1(length(unique(v2@data$clust)))[as.numeric(v2@data$clust)])

points$Eco=clust[which(!is.na(clust[paste0("Eco",i)])),paste0("Eco",i)]
#getting spatial data
points1 <- st_as_sf(points,coords=c("Longitude","Latitude"), crs=4326, remove=FALSE) 
color_palette <- colorRampPalette(c("blue", "olivedrab4", "red"))
plot(points1$geometry,add=TRUE,col = color_palette(length(unique(points1$Eco)))[as.numeric(points1$Eco)])


selected_polygons <- v2[v2@data$clust == 1, ]

# Plot the selected polygons
plot(selected_polygons, col = "red")



