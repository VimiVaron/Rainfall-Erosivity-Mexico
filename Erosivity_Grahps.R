
rm(list=ls())
#period=c(1968,1997)
setwd("C:\\Users\\vimiv\\OneDrive - AGROSAVIA - CORPORACION COLOMBIANA DE INVESTIGACION AGROPECUARIA\\Documentos\\DOCTORADO\\Proyecto_CONAHCYT\\Climate_Data\\Daily_Resolution")

library(sf)
library(dplyr)
library(stringr)
library(ggplot2)
library(tidyr)
library(ggdist)
library(moments)
library(ggtext)
library(colorspace)
library(ragg)
library(dplyr)
library(ggplot2)
library(forcats)
library(lhs)


R_P1=read.csv("P_1968_1997/Erosivity_WS.csv")
colnames(R_P1)[1]="Period"
R_P1$Period="1968-1997"
R_P2=read.csv("P_1978_2007/Erosivity_WS.csv")
colnames(R_P2)[1]="Period"
R_P2$Period="1978-2007"
R_P3=read.csv("P_1988_2017/Erosivity_WS.csv")
colnames(R_P3)[1]="Period"
R_P3$Period="1988-2017"

df_long=rbind(R_P1,R_P2,R_P3)


b=boxplot(R_P2$Erosivity_year_YunXie2016)
b

df_long$Period=as.factor(df_long$Period)

#graph 3
fig=ggplot(df_long, aes(x = Period, y = Erosivity_year_YunXie2016)) +
  geom_violin(
    fill = "cyan", 
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
    colour = "cyan4"
  )+
    labs(
    x = "Period",  # Change the label for the x-axis
    y = "Erosivity (MJ mm)/(ha h yr)"   # Change the label for the y-axis
  )+
  ylim(0, 35000)+
  scale_x_discrete(label=c("1968-1997","1978-2007","1988-2017"))+
theme_minimal()+ #no background annotations+
theme( axis.title.x = element_text(size = 6, color = "black", face = "bold"),
       axis.title.y = element_text(size = 6, color = "black", face = "bold"),
       axis.text.x = element_text(size=5, color="black"),
       axis.text.y = element_text(size=5, color="black"),
       panel.grid.major = element_line(linewidth = 0.1)
       )

# save a png with high res
ppi <- 500# final: 600 # resolution
w <- 15 # width in cm
h<- 8
png(paste0("C:/Users/vimiv/OneDrive - AGROSAVIA - CORPORACION COLOMBIANA DE INVESTIGACION AGROPECUARIA/Documentos/DOCTORADO/Proyecto_CONAHCYT/Figures_Article/Erosivity_period_boxplot.png"),
    width=w,
    height=h,
    units = "cm",
    res=ppi)
fig
dev.off()

rm(fig)


#### Figure

pal <- c("#FF8C00", "#A034F0", "#159090")

add_sample <- function(x){
  return(c(y = max(x) + .025, 
           label = length(x)))
}

fig_comp=df_long %>% 
  group_by(Period) %>% 
  ggplot(aes(x = fct_rev(Period), y = Erosivity_year_YunXie2016)) + 
  ggdist::stat_halfeye(
    aes(color = Period,
        fill = after_scale(lighten(color, .5))),
    adjust = .5, 
    width = .6, 
    .width = 0,
    justification = -.4, 
    point_color = NA) + 
  geom_boxplot(
    aes(color = Period,
        color = after_scale(darken(color, .1, space = "HLS")),
        fill = after_scale(desaturate(lighten(color, .8), .4))),
    width = .2, 
    outlier.shape = 8,
    outlier.size = 0.5,
    lwd = 0.3
  ) +
  # geom_point(
  #   aes(color = Period,
  #       color = after_scale(darken(color, .1, space = "HLS"))),
  #   fill = "white",
  #   color = "transparent",
  #   shape = 21,
  #   stroke = .4,
  #   size = 1,
  #   position = position_jitter(seed = 1, width = .1)
  # ) + 
  # geom_point(
  #   aes(fill = Period),
  #   color = "transparent",
  #   shape = 21,
  #   stroke = .4,
  #   size = 1,
  #   alpha = .3,
  #   position = position_jitter(seed = 1, width = .1)
  # ) + 
  stat_summary(
    geom = "text",
    fun = "median",
    aes(label = paste("median=",round(..y.., 2)),
        color = Period,
        color = after_scale(darken(color, .1, space = "HLS"))),
    family = "Roboto Mono",
    fontface = "bold",
    size = 2,
    vjust = -2
  ) +
  stat_summary(
    geom = "text",
    fun.data = add_sample,
    aes(label = paste("n =", ..label..),
        color = Period,
        color = after_scale(darken(color, .1, space = "HLS"))),
    family = "Roboto Condensed",
    fontface = "bold",
    size = 2,
    hjust = 1,
    vjust = -3
  ) +
  coord_flip(xlim = c(1, NA), clip = "off") +
  scale_color_manual(values = pal, guide = "none") +
  scale_fill_manual(values = pal, guide = "none") +
  labs(
    x = NULL,
    y = "R Factor (MJ mm)/(ha h yr)") +
  theme_minimal(base_family = "Zilla Slab", base_size = 8) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_text(family = "Roboto Mono"),
    axis.text.y = element_text(
      color = rev(darken(pal, .1, space = "HLS")), 
      size = 8
    ),
    axis.title.x = element_text(margin = margin(t = 8),
                                size = 7),
    plot.margin=unit(c(0.3,0.3,0.3,0.3), 'cm')
  )


# save a png with high res
ppi <- 500# final: 600 # resolution
w <- 15 # width in cm
h<- 10
png(paste0("C:/Users/vimiv/OneDrive - AGROSAVIA - CORPORACION COLOMBIANA DE INVESTIGACION AGROPECUARIA/Documentos/DOCTORADO/Proyecto_CONAHCYT/Figures_Article/densityPlot_P1P2P3.png"),
    width=w,
    height=h,
    units = "cm",
    res=ppi)
fig_comp
dev.off()






#####some statistic

summary=df_long%>% group_by(Period,Ecoregion)%>% 
  summarize(
    n=n(),
    m=mean(Erosivity_year_YunXie2016),
    sd=sd(Erosivity_year_YunXie2016),
    min=min(Erosivity_year_YunXie2016),
    max=max(Erosivity_year_YunXie2016)
  )

write.csv(summary,"Summary_erosivity_by_Ecoregion.csv")


summary=df_long%>% group_by(Period)%>% 
  summarize(
    n=n(),
    m=mean(Erosivity_year_YunXie2016),
    sd=sd(Erosivity_year_YunXie2016),
    min=min(Erosivity_year_YunXie2016),
    max=max(Erosivity_year_YunXie2016),
    skewness(Erosivity_year_YunXie2016),
    kurtosis(Erosivity_year_YunXie2016)
  )

write.csv(summary,"Summary_erosivity_by_Period.csv")

kruskal.test <- kruskal.test(Erosivity_year_YunXie2016 ~ Period, data = df_long)
print(kruskal.test)

library(dunn.test)
dunn_result <- dunn.test(df_long$Erosivity_year_YunXie2016, df_long$Period, method = "bonferroni")
print(dunn_result)


library(terra)
R_Gloreda=rast("C:\\Users\\vimiv\\OneDrive - AGROSAVIA - CORPORACION COLOMBIANA DE INVESTIGACION AGROPECUARIA\\Documentos\\DOCTORADO\\Proyecto_CONAHCYT\\SHP\\GloREDa_Raster\\Rfactor_Annual.tif")

reclass_matrix <- matrix(c(
  0, 700, 1,   # 
  700.000001, 1100, 2, # 
  1100.000001, 1400, 3, # 
  1400.000001, 1800, 4,
  1800.000001, 2300, 5,
  2300.000001, 3000, 6,
  3000.000001, 3900, 7,
  3900.000001, 5100, 8,
  5100.000001, 8100, 9,
  8100.000001, 32000, 10
), ncol = 3, byrow = TRUE)


classified_raster <- classify(R_Gloreda, rcl = reclass_matrix)

writeRaster(classified_raster,"C:\\Users\\vimiv\\OneDrive - AGROSAVIA - CORPORACION COLOMBIANA DE INVESTIGACION AGROPECUARIA\\Documentos\\DOCTORADO\\Proyecto_CONAHCYT\\SHP\\GloREDa_Raster\\RFactor_Annual_classified.tif")
