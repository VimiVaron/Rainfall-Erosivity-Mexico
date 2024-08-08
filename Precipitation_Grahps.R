
rm(list=ls())
period=c(1988,2017)
setwd(paste0("C:\\Users\\vimiv\\OneDrive - AGROSAVIA - CORPORACION COLOMBIANA DE INVESTIGACION AGROPECUARIA\\Documentos\\DOCTORADO\\Proyecto_CONAHCYT\\Climate_Data\\Daily_Resolution\\P_",period[1],"_",period[2]))

library(sf)
library(dplyr)
library(stringr)
library(ggplot2)
library(tidyr)
library(ggdist)


prom_in_monthly=read.csv("Prom_prec_monthly_incomplete.csv")
prom_in_monthly=prom_in_monthly[,-1]

prom_comp_monthly=read.csv("Prom_prec_monthly_complete.csv")
prom_comp_monthly=prom_comp_monthly[,-1]

##loading WS information
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
Eco=as.vector(Eco[,1])

colnames(est_Ecoregions)[4]="Code"

Stations=as.data.frame(names(prom_in_monthly[,-1]))
colnames(Stations)="Code"
Stations$Code<-as.numeric(sapply(Stations$Code, function(x){strsplit(x,"X")[[1]][2]}))
Stations=left_join(Stations,est_Ecoregions[,c(1,2,3,4,5,6)],by="Code")

#for (i in 1:7) {
  i=7
  accum_prec_Eco=prom_comp_monthly[,c(1,which(Stations$DESECON1==Eco[i])+1)] #plus 1 because the first column is the ref
  df_long <- accum_prec_Eco %>%
    gather(key = "Station", value = "Prec", -ref)

df_long$ref=as.factor(df_long$ref)

#graph 3
fig=ggplot(df_long, aes(x = ref, y = Prec)) +
  geom_violin(
    fill = "deepskyblue", 
    color = NA, 
    scale = "count", 
    bw = .5
  ) +
  geom_boxplot(
    width = 0.3, 
    outlier.size = 0.2,
    ## remove white filling
    fill = NA,
    linewidth = 0.3,
    colour = "lightskyblue4"
  )+
  labs(
    x = "Month",  # Change the label for the x-axis
    y = "Total rainfall (mm)"   # Change the label for the y-axis
  )+
  ylim(0, 900)+
  scale_x_discrete(label=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))+
theme_minimal()+ #no background annotations+
theme( axis.title.x = element_text(size = 6, color = "black", face = "bold"),
       axis.title.y = element_text(size = 6, color = "black", face = "bold"),
       axis.text.x = element_text(size=6, color="black"),
       axis.text.y = element_text(size=6, color="black"),
       panel.grid.major = element_line(linewidth = 0.1),
       plot.margin=unit(c(0,0,0,0), 'cm')
       )

# save a png with high res
ppi <- 500# final: 600 # resolution
w <- 8.5 # width in cm
h<- 4.5
png(paste0("C:/Users/vimiv/OneDrive - AGROSAVIA - CORPORACION COLOMBIANA DE INVESTIGACION AGROPECUARIA/Documentos/DOCTORADO/Proyecto_CONAHCYT/Figures_Article/Monthly_Accum_prec_Eco_",i,"_",period[1],"_",period[2],".png"),
    width=w,
    height=h,
    units = "cm",
    res=ppi)
fig
dev.off()

rm(fig)
#}


#c("January","February","March","April","May","June","July","August","September","October","November","December")