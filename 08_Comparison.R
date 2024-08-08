rm(list=ls())


setwd(paste0("C:\\Users\\vimiv\\OneDrive - AGROSAVIA - CORPORACION COLOMBIANA DE INVESTIGACION AGROPECUARIA\\Documentos\\DOCTORADO\\Proyecto_CONAHCYT"))

library(sf)
library(dplyr)
library(terra)
library(psych)#
library(ggtext)
library(colorspace)
library(ragg)
library(dplyr)
library(ggplot2)
library(forcats)
library(lhs)

########################################
#############GloREDa####################
########################################


#1. Comparison with raster files by ecoregion

#loading GloREDa data
GloREDa_annual=rast("SHP/GloREDa_Raster/Rfactor_Annual.tif")
GloREDa_annual_points=as.points(GloREDa_annual,values=TRUE)
#GloREDa_annual_points=st_as_sf(GloREDa_annual_points,crs=4326)


#loading ecoregions shp 
Ecoregions <- vect("C:\\Users\\vimiv\\OneDrive - AGROSAVIA - CORPORACION COLOMBIANA DE INVESTIGACION AGROPECUARIA\\Documentos\\DOCTORADO\\Proyecto_CONAHCYT\\SHP\\ecort08gw\\ecort08gw.shp") 

#making spatial join
joined <- intersect(GloREDa_annual_points, Ecoregions)
table=as.data.frame(joined)
#write.csv(table,"Climate_Data/Daily_Resolution/GloREDa_Ecoregions.csv")

R_GloREDa=read.csv("Climate_Data/Daily_Resolution/GloREDa_Ecoregions.csv")
R_GloREDa=R_GloREDa[,c(2,10)]
colnames(R_GloREDa)=c("RFactor","Ecoregion")
R_GloREDa$Dataset="GloREDa"

summary=R_GloREDa%>% group_by(Ecoregion)%>% 
  summarize(
    n=n(),
    m=mean(RFactor),
    sd=sd(RFactor),
    min=min(RFactor),
    max=max(RFactor)
  )


R_P3=read.csv("C:/Users/vimiv/OneDrive - AGROSAVIA - CORPORACION COLOMBIANA DE INVESTIGACION AGROPECUARIA/Documentos/DOCTORADO/Proyecto_CONAHCYT/Climate_Data/Daily_Resolution/P_1978_2007/Erosivity_WS.csv")
R_P3=R_P3[,c(10,7)]
colnames(R_P3)=c("RFactor","Ecoregion")
R_P3$Dataset="Mexico-CN3"


combined_df <- bind_rows(R_GloREDa,R_P3)

# Calculate means and standard errors by dataset and ecoregion
plotdata <- combined_df %>%
  group_by(Dataset, Ecoregion) %>%
  summarize(n = n(),
            mean = mean(RFactor),
            sd = sd(RFactor),
            se = sd / sqrt(n))

# Plot with dodge position for error bars
# Define custom colors for each dataset
colors <- c("GloREDa" = "#CD69C9", "Mexico-CN3" = "#159090")
library(stringr)
# Wrap labels into two lines
wrapped_labels <- str_wrap(c("North American Deserts", "Semi-arid Elevations", 
                             "Great Plains", "Tropical Rain Forest", "Tropical Dry Forest", "Temperate Sierras"), width = 14)


# Plot with custom colors
fig <- ggplot(plotdata[-1,], aes(x = Ecoregion, y = mean, group = Dataset, color = Dataset)) +
  geom_point(size = 1, position = position_dodge(width = 0.2)) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.01, position = position_dodge(width = 0.2),size = 0.3) +
  labs(x = "Ecoregions", y = "Erosivity (MJ mm)/(ha h yr)") +
  scale_x_discrete(labels = wrapped_labels) +
  scale_color_manual(values = colors) +  # Apply custom colors
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 6, color = "black", face = "bold"),
    axis.title.y = element_text(size = 6, color = "black", face = "bold"),
    axis.text.x = element_text(size = 6, color = "black"),
    axis.text.y = element_text(size = 6, color = "black"),
    panel.grid.major = element_line(linewidth = 0.1),
    plot.margin = unit(c(0, 0, 0, 0), 'cm'),
    legend.title = element_text(size = 6, face = "bold"),
    legend.text = element_text(size = 5),
    legend.key.size = unit(0.2, "cm"),
    legend.margin = margin(0, 0, 0, 0),
    legend.position = "bottom"
  ) +
  guides(color = guide_legend(override.aes = list(size = 2, key.width = 0.3, key.height = 0.3)),
         fill = guide_legend(override.aes = list(size = 2, key.width = 0.3, key.height = 0.3)))

# Save the plot
ppi <- 500
w <- 11
h <- 5

png(filename = "C:/Users/vimiv/OneDrive - AGROSAVIA - CORPORACION COLOMBIANA DE INVESTIGACION AGROPECUARIA/Documentos/DOCTORADO/Proyecto_CONAHCYT/Figures_Article/Comp_GloREDa_MexicoP3.png",
    width = w,
    height = h,
    units = "cm",
    res = ppi)

print(fig)

dev.off()







################################################################
#############Erosivity Mexico Cortés (1991) ####################
################################################################

Point_Cortes=read.csv("C:/Users/vimiv/OneDrive - AGROSAVIA - CORPORACION COLOMBIANA DE INVESTIGACION AGROPECUARIA/Documentos/DOCTORADO/Proyecto_CONAHCYT/Erosividad_Cortes.csv",sep = ";")
Point_Cortes=Point_Cortes[Point_Cortes$Years>=5,c(8,7)]

colnames(Point_Cortes)=c("RFactor","Annual_Rainfall")
Point_Cortes$Annual_Rainfall_C=Point_Cortes$Annual_Rainfall-mean(Point_Cortes$Annual_Rainfall)

m_Cortes=lm(RFactor~Annual_Rainfall_C,data = Point_Cortes)
summary(m_Cortes)
#plot(m_Cortes)


Point_Cortes_1=Point_Cortes[-which(rownames(Point_Cortes)%in%c(28,41,43)),]
m_Cortes_1=lm(RFactor~Annual_Rainfall_C,data = Point_Cortes_1)
summary(m_Cortes_1)
#plot(m_Cortes_1)
#plot(Point_Cortes_1$Annual_Rainfall,Point_Cortes_1$RFactor)


Point_Cortes_1 <- Point_Cortes_1 %>%
  mutate(
    model = "Erosivity-Cortés",
    y_pred = predict(m_Cortes_1),
    y_se = predict(m_Cortes_1, se.fit = TRUE)$se.fit
  )



R_P2=read.csv("C:/Users/vimiv/OneDrive - AGROSAVIA - CORPORACION COLOMBIANA DE INVESTIGACION AGROPECUARIA/Documentos/DOCTORADO/Proyecto_CONAHCYT/Climate_Data/Daily_Resolution/P_1978_2007/Erosivity_WS.csv")
Annual_Rainfall=read.csv("C:/Users/vimiv/OneDrive - AGROSAVIA - CORPORACION COLOMBIANA DE INVESTIGACION AGROPECUARIA/Documentos/DOCTORADO/Proyecto_CONAHCYT/Climate_Data/Daily_Resolution/P_1978_2007/Accum_prec_annual_complete.csv")
f=apply(Annual_Rainfall[,c(3:length(Annual_Rainfall))],MARGIN = 2, mean)
tmp=data.frame(Code=names(f),Annual_Rainfall=f)
tmp$Code=as.numeric(sapply(tmp$Code, function(x){strsplit(x,"X")[[1]][2]}))

Point_Mex_P2=left_join(R_P2[,c(2,10)],tmp,by="Code")
colnames(Point_Mex_P2)[2]="RFactor"
#Point_Mex_P2=Point_Mex_P2[Point_Mex_P2$Annual_Rainfall>range(Point_Cortes_1$Annual_Rainfall)[1]&Point_Mex_P2$Annual_Rainfall<range(Point_Cortes_1$Annual_Rainfall)[2],]
#Point_Mex_P2=Point_Mex_P2[complete.cases(Point_Mex_P2),]

#write.csv(Point_Mex_P2,"C:/Users/vimiv/OneDrive - AGROSAVIA - CORPORACION COLOMBIANA DE INVESTIGACION AGROPECUARIA/Documentos/DOCTORADO/Proyecto_CONAHCYT/Climate_Data/Daily_Resolution/P_1978_2007/Erosivity_AnnualRainfall_1978_2007.csv")

summary=Point_Mex_P2%>% group_by(Ecoregion)%>% 
  summarize(
    n=n(),
    m=mean(Annual_Rainfall),
    sd=sd(Annual_Rainfall),
    min=min(Annual_Rainfall),
    max=max(Annual_Rainfall),
    cv=sd(Annual_Rainfall)/mean(Annual_Rainfall)
  )



Point_Mex_P2$Annual_Rainfall_C=Point_Mex_P2$Annual_Rainfall-mean(Point_Mex_P2$Annual_Rainfall)

m_Mex_P2=lm(RFactor~Annual_Rainfall_C,data =Point_Mex_P2)
summary(m_Mex_P2)
plot(Point_Mex_P2$Annual_Rainfall,Point_Mex_P2$RFactor)

Point_Mex_P2 <- Point_Mex_P2 %>%
  mutate(
    model = "Mexico-CN2",
    y_pred = predict(m_Mex_P2),
    y_se = predict(m_Mex_P2, se.fit = TRUE)$se.fit
  )



R_P1=read.csv("C:/Users/vimiv/OneDrive - AGROSAVIA - CORPORACION COLOMBIANA DE INVESTIGACION AGROPECUARIA/Documentos/DOCTORADO/Proyecto_CONAHCYT/Climate_Data/Daily_Resolution/P_1968_1997/Erosivity_WS.csv")
Annual_Rainfall=read.csv("C:/Users/vimiv/OneDrive - AGROSAVIA - CORPORACION COLOMBIANA DE INVESTIGACION AGROPECUARIA/Documentos/DOCTORADO/Proyecto_CONAHCYT/Climate_Data/Daily_Resolution/P_1968_1997/Accum_prec_annual_complete.csv")
f=apply(Annual_Rainfall[,c(3:length(Annual_Rainfall))],MARGIN = 2, mean)
tmp=data.frame(Code=names(f),Annual_Rainfall=f)
tmp$Code=as.numeric(sapply(tmp$Code, function(x){strsplit(x,"X")[[1]][2]}))

Point_Mex_P1=left_join(R_P1[,c(2,10)],tmp,by="Code")
colnames(Point_Mex_P1)[2]="RFactor"
#Point_Mex_P1=Point_Mex_P1[Point_Mex_P1$Annual_Rainfall>range(Point_Cortes_1$Annual_Rainfall)[1]&Point_Mex_P1$Annual_Rainfall<range(Point_Cortes_1$Annual_Rainfall)[2],]
#Point_Mex_P1=Point_Mex_P1[complete.cases(Point_Mex_P1),]

#write.csv(Point_Mex_P1,"C:/Users/vimiv/OneDrive - AGROSAVIA - CORPORACION COLOMBIANA DE INVESTIGACION AGROPECUARIA/Documentos/DOCTORADO/Proyecto_CONAHCYT/Climate_Data/Daily_Resolution/P_1968_1997/Erosivity_AnnualRainfall_1968_1997.csv")


Point_Mex_P1$Annual_Rainfall_C=Point_Mex_P1$Annual_Rainfall-mean(Point_Mex_P1$Annual_Rainfall)

m_Mex_P1=lm(RFactor~Annual_Rainfall_C,data =Point_Mex_P1)
summary(m_Mex_P1)
#plot(Point_Mex_P1$Annual_Rainfall,Point_Mex_P1$RFactor)



Point_Mex_P1 <- Point_Mex_P1 %>%
  mutate(
    model = "Mexico-CN1",
    y_pred = predict(m_Mex_P1),
    y_se = predict(m_Mex_P1, se.fit = TRUE)$se.fit
  )




combined_df <- bind_rows(Point_Cortes_1, Point_Mex_P1,Point_Mex_P2)



# Calculate the equations and R^2 values for each model
r2_Cortes_1 <- summary(m_Cortes_1)$r.squared
r2_Mex_P1 <- summary(m_Mex_P1)$r.squared
r2_Mex_P2 <- summary(m_Mex_P2)$r.squared

eq1 <- paste("R(EI30) = ", round(coef(m_Cortes_1)[1], 2), " + ", round(coef(m_Cortes_1)[2], 2), " * Annual Rainfall", sep = "")
eq1 <- paste(eq1, ", R² = ", round(r2_Cortes_1, 2), sep = "")

eq2 <- paste("R(Xie et al., 2016) = ", round(coef(m_Mex_P1)[1], 2), " + ", round(coef(m_Mex_P1)[2], 2), " * Annual Rainfall", sep = "")
eq2 <- paste(eq2, ", R² = ", round(r2_Mex_P1, 2), sep = "")

eq3 <- paste("R(Xie et al., 2016) = ", round(coef(m_Mex_P2)[1], 2), " + ", round(coef(m_Mex_P2)[2], 2), " * Annual Rainfall", sep = "")
eq3 <- paste(eq3, ", R² = ", round(r2_Mex_P2, 2), sep = "")

# Create the plot
fig <- ggplot(combined_df, aes(x = Annual_Rainfall, y = y_pred, color = model)) +
  geom_line(size = 0.1) +  # Adjust line width here
  geom_ribbon(aes(ymin = y_pred - 1.96 * y_se, ymax = y_pred + 1.96 * y_se, fill = model), alpha = 0.2, size = 0.1) +
  labs(
    x = "Annual Rainfall (mm)", 
    y = "Erosivity (MJ mm)/(ha h yr)"
  ) +
  scale_x_continuous(limits = c(250, range(Point_Cortes_1$Annual_Rainfall)[2])) + # Set x-axis limits
  scale_y_continuous(limits = c(0, 11000)) + # Set y-axis limits
  scale_color_manual(name = "Dataset", values = c("Erosivity-Cortés" = "#8B8378", "Mexico-CN1" = "#FF8C00", "Mexico-CN2" = "#A034F0")) + # Update this line with your actual model names and desired colors
  scale_fill_manual(name = "Dataset", values = c("Erosivity-Cortés" = "#8B8378", "Mexico-CN1" = "#FF8C00", "Mexico-CN2" = "#A034F0")) + # Update this line with your actual model names and desired colors
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 6, color = "black", face = "bold"),
    axis.title.y = element_text(size = 6, color = "black", face = "bold"),
    axis.text.x = element_text(size = 6, color = "black"),
    axis.text.y = element_text(size = 6, color = "black"),
    panel.grid.major = element_line(linewidth = 0.1),
    plot.margin = unit(c(0, 0, 0, 0), 'cm'),
    legend.title = element_text(size = 6, face = "bold"), # Reduce legend title size
    legend.text = element_text(size = 5), # Reduce legend text size
    legend.key.size = unit(0.2, "cm"), # Reduce legend key size
    legend.margin = margin(0, 0, 0, 0),  # Reduce margin around legend
    legend.position = "bottom" 
  ) +
  guides(color = guide_legend(override.aes = list(size = 2, key.width = 0.3, key.height = 0.3)), # Adjust size of line symbols in legend
         fill = guide_legend(override.aes = list(size = 2, key.width = 0.3, key.height = 0.3))) + # Adjust size of fill symbols in legend
  annotate("text", x = 310, y = 10000, label = eq1, color = "#8B8378", size = 1.8, hjust = 0.1) + # Adjust x and y to desired location
  annotate("text", x = 310, y = 9300, label = eq2, color = "#FF8C00", size = 1.8, hjust = 0.1) + # Adjust x and y to desired location
  annotate("text", x = 310, y = 8600, label = eq3, color = "#A034F0", size = 1.8, hjust = 0.1)  # Adjust x and y to desired location

# Save the plot as a PNG file with high resolution
ppi <- 500 # resolution
w <- 8.5 # width in cm
h <- 5   # height in cm

# Save the plot
png(filename = "C:/Users/vimiv/OneDrive - AGROSAVIA - CORPORACION COLOMBIANA DE INVESTIGACION AGROPECUARIA/Documentos/DOCTORADO/Proyecto_CONAHCYT/Figures_Article/Comp_Cortes_P1_P2.png",
    width = w,
    height = h,
    units = "cm",
    res = ppi)

print(fig)

dev.off()



################################################################
#######################Michoacan ##############################
################################################################

Point_Michoacan=read.csv("C:/Users/vimiv/OneDrive - AGROSAVIA - CORPORACION COLOMBIANA DE INVESTIGACION AGROPECUARIA/Documentos/DOCTORADO/Proyecto_CONAHCYT/Michoacan_Rainfall_Erosivity_RXieEtal.2016.csv",sep = ";")
Point_Michoacan=Point_Michoacan[Point_Michoacan$Years_available>=5,c(9,10,11)]

colnames(Point_Michoacan)=c("RFactor","R_Xie","Annual_Rainfall")
Point_Michoacan=Point_Michoacan[complete.cases(Point_Michoacan),]

m_Michoacan=lm(RFactor~R_Xie,data=Point_Michoacan_1)
summary(m_Michoacan)
Point_Michoacan_1=Point_Michoacan[-which(rownames(Point_Michoacan)%in%c(13,27)),]

# Fit the model
m_Michoacan <- lm(RFactor ~ R_Xie, data = Point_Michoacan_1)
summary(m_Michoacan)

# Calculate predictions and standard errors
Point_Michoacan_1 <- Point_Michoacan_1 %>%
  mutate(
    model = "Erosivity-Michoacan",
    y_pred = predict(m_Michoacan),
    y_se = predict(m_Michoacan, se.fit = TRUE)$se.fit
  )

# Check for NAs in predictions and standard errors
print(any(is.na(Point_Michoacan_1$y_pred)))
print(any(is.na(Point_Michoacan_1$y_se)))

# Calculate the equation and R^2 values for the model
r2_Michoacan <- summary(m_Michoacan)$r.squared
eq1 <- paste("R(EI30) = ", round(coef(m_Michoacan)[1], 2), " + ", round(coef(m_Michoacan)[2], 2), " * R(Xie et al., 2016)", sep = "")
eq1 <- paste(eq1, ", R² = ", round(r2_Michoacan, 2), sep = "")

# Determine appropriate annotation coordinates
x_annotate <- min(Point_Michoacan_1$R_Xie) + 0.1 * diff(range(Point_Michoacan_1$R_Xie))
y_annotate <- max(Point_Michoacan_1$RFactor) - 0.1 * diff(range(Point_Michoacan_1$RFactor))

# Create the plot
fig <- ggplot(Point_Michoacan_1, aes(x = R_Xie, y = y_pred, color = model)) +
  geom_line(linewidth = 0.1) +  # Adjust line width here
  geom_ribbon(aes(ymin = y_pred - 1.96 * y_se, ymax = y_pred + 1.96 * y_se, fill = model), alpha = 0.2,size=0.1) +
  labs(
    x = "R(Xie et al., 2016) (MJ mm)/(ha h yr)", 
    y = "R(EI30) (MJ mm)/(ha h yr)"
  ) +
  scale_x_continuous(limits = c(min(Point_Michoacan_1$R_Xie), max(Point_Michoacan_1$R_Xie))) + # Set x-axis limits
  scale_y_continuous(limits = c(3000, 17000)) + # Set y-axis limits
  #scale_y_continuous(limits = c(min(Point_Michoacan_1$RFactor), max(Point_Michoacan_1$RFactor))) + # Set y-axis limits
  scale_color_manual(name = "Dataset", values = c("Erosivity-Michoacan" = "#8B3E2F")) + # Update this line with your actual model names and desired colors
  scale_fill_manual(name = "Dataset", values = c("Erosivity-Michoacan" = "#8B3E2F")) + # Update this line with your actual model names and desired colors
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 6, color = "black", face = "bold"),
    axis.title.y = element_text(size = 6, color = "black", face = "bold"),
    axis.text.x = element_text(size = 6, color = "black"),
    axis.text.y = element_text(size = 6, color = "black"),
    panel.grid.major = element_line(linewidth = 0.1),
    plot.margin = unit(c(0, 0, 0, 0), 'cm'),
    legend.title = element_text(size = 6, face = "bold"), # Reduce legend title size
    legend.text = element_text(size = 5), # Reduce legend text size
    legend.key.size = unit(0.2, "cm"), # Reduce legend key size
    legend.margin = margin(0, 0, 0, 0),  # Reduce margin around legend
    legend.position = "bottom"  # Position legend at the bottom
  ) +
  guides(color = guide_legend(override.aes = list(size = 2, key.width = 0.3, key.height = 0.3))) + # Adjust size of symbols in legend
  annotate("text", x = x_annotate, y = y_annotate, label = eq1, color = "#8B3E2F", size = 1.8, hjust = 0.1)  # Adjust x and y to desired location

# Save the plot as a PNG file with high resolution
ppi <- 500 # resolution
w <- 8.5 # width in cm
h <- 5   # height in cm

# Save the plot
png(filename = "C:/Users/vimiv/OneDrive - AGROSAVIA - CORPORACION COLOMBIANA DE INVESTIGACION AGROPECUARIA/Documentos/DOCTORADO/Proyecto_CONAHCYT/Figures_Article/Comp_Michoacan.png",
    width = w,
    height = h,
    units = "cm",
    res = ppi)

print(fig)

dev.off()



