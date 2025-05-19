rm(list=ls())
period=c(1968,1997)
setwd(paste0("C:\\Users\\vimiv\\OneDrive - AGROSAVIA - CORPORACION COLOMBIANA DE INVESTIGACION AGROPECUARIA\\Documentos\\DOCTORADO\\Proyecto_CONAHCYT\\Climate_Data\\Daily_Resolution\\P_",period[1],"_",period[2]))

library(climatol)
library(sf)
library(dplyr)
library(stringr)
library(lubridate)
library(psych)#
library(terra)


valid_comple_series=read.csv("valid_comple_series.csv")
valid_comple_series=valid_comple_series[,-1]
valid_comple_series$Date=ymd(valid_comple_series$Date)
valid_in_series=read.csv("valid_in_series.csv")
valid_in_series=valid_in_series[,-1]

Stations=read.csv("RMSE_WS.csv",sep = ",")
Stations=Stations[,-1]


#counting the number of day with precipitation greater than 12.5 mm
day_greater_12.5=function(x){length(which(x>=12.7))}

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

####
####calculating Erosivity according to Liu2020 Climate Classification###### 
####

r <- rast("C:/Users/vimiv/OneDrive - AGROSAVIA - CORPORACION COLOMBIANA DE INVESTIGACION AGROPECUARIA/Documentos/DOCTORADO/Proyecto_CONAHCYT/SHP/K_G/K_G_Mexico_0p0083_Present.tif")  # replace with your file path
points=read.csv("Erosivity_WS.csv")
points_sf <- st_as_sf(points, coords = c("Longitude", "Latitude"), crs = 4326)

# 4. Convert to terra's SpatVector format
points_vect <- vect(points_sf)

# 5. Extract raster values at point locations
extracted <- extract(r, points_vect)
points$Climate_Class_K_G=extracted[,2]
table(points$Climate_Class_K_G)


which(points$Climate_Class_K_G == 0)
points[which(points$Climate_Class_K_G == 0),]



#points$Climate_Class_K_G[1453]  <- 6   # forCN3
#points$Climate_Class_K_G[1270]  <- 3
#points$Climate_Class_K_G[1454]  <- 3
#points$Climate_Class_K_G[1236]  <- 4
#points$Climate_Class_K_G[1339]  <- 4



#points$Climate_Class_K_G[1308]  <- 4   # fOR CN2
#points$Climate_Class_K_G[1342]  <- 3
#points$Climate_Class_K_G[1404]  <- 4
#points$Climate_Class_K_G[1471]  <- 6
#points$Climate_Class_K_G[1472]  <- 3
#points$Climate_Class_K_G[1612]  <- 2

points$Climate_Class_K_G[712]  <- 3 #For CN1
points$Climate_Class_K_G[1116]  <- 3
points$Climate_Class_K_G[1177]  <- 6
points$Climate_Class_K_G[1178]  <- 3
points$Climate_Class_K_G[1307]  <- 2

# Create a data frame with code and description
koppen_legend <- data.frame(
  code = 1:30,
  classification = c(
    "Af - Tropical rainforest",
    "Am - Tropical monsoon",
    "Aw - Tropical savannah",
    "BWh - Arid desert hot",
    "BWk - Arid desert cold",
    "BSh - Arid steppe hot",
    "BSk - Arid steppe cold",
    "Csa - Temperate dry summer hot summer",
    "Csb - Temperate dry summer warm summer",
    "Csc - Temperate dry summer cold summer",
    "Cwa - Temperate dry winter hot summer",
    "Cwb - Temperate dry winter warm summer",
    "Cwc - Temperate dry winter cold summer",
    "Cfa - Temperate no dry season hot summer",
    "Cfb - Temperate no dry season warm summer",
    "Cfc - Temperate no dry season cold summer",
    "Dsa - Cold dry summer hot summer",
    "Dsb - Cold dry summer warm summer",
    "Dsc - Cold dry summer cold summer",
    "Dsd - Cold dry summer very cold winter",
    "Dwa - Cold dry winter hot summer",
    "Dwb - Cold dry winter warm summer",
    "Dwc - Cold dry winter cold summer",
    "Dwd - Cold dry winter very cold winter",
    "Dfa - Cold no dry season hot summer",
    "Dfb - Cold no dry season warm summer",
    "Dfc - Cold no dry season cold summer",
    "Dfd - Cold no dry season very cold winter",
    "ET - Polar tundra",
    "EF - Polar frost"
  )
)

# Merge to add the description column
points$Climate_Class_K_G_name <- koppen_legend$classification[match(points$Climate_Class_K_G, koppen_legend$code)]

which(is.na(points$Climate_Class_K_G_name))
points$Koppen_Code_Short <- sub(" -.*", "", points$Climate_Class_K_G_name)

get_ab_values <- function(koppen, lat, lon) {
  if (koppen %in% c("Af", "Am", "Aw")) {
    b <- 1.964 + 0.013 * lat
    a <- 10^(2.363 - 1.561 * b)
  } else if (koppen %in% c("BSh", "BSk")) {
    b <- 1.73
    a <- 0.3296
  } else if (koppen %in% c("BWh", "BWk")) {
    b <- 1.514
    a <- (2.123 - 0.04 * lat) * 10^(1.781 - 1.341 * b)
  } else if (koppen == "Cs") {
    b <- 1.563
    a <- 0.2735
  } else if (koppen %in% c("Cwa", "Cwb")) {
    b <- 1.558
    a <- 0.817
  } else if (koppen %in% c("Cfa", "Cfb", "Cfc")) {
    b <- 1.5
    a <- (3.792 - 0.012 * lon - 0.037 * lat) * 10^(3.016 - 2.079 * b)
  } else {
    b <- NA
    a <- NA
  }
  return(c(a = a, b = b))
}

ab_matrix <- t(mapply(get_ab_values, points$Koppen_Code_Short, points$Latitude, points$Longitude))

# Add the columns "a" and "b" to the dataframe
points$a <- ab_matrix[, "a"]
points$b <- ab_matrix[, "b"]


#write.csv(points,"Erosivity_WS_2.csv")


#calculating erosivity by day
df=valid_comple_series[,1:dim(points)[1]]
df[df<12.5]<-NA
#df$month=month(valid_comple_series$Date)

# 1. Extract rainfall values (first 1676 columns)
rain_mat <- as.matrix(df[, 1:dim(points)[1]]) #1676 FOR cn3

# 2. Get numeric station codes from column names
station_codes <- as.integer(gsub("X", "", names(valid_comple_series)[1:dim(points)[1]]))

# 3. Match to rows in `points`
points_ordered <- points[match(station_codes, points$Code), ]

# 4. Extract a and b
a_vals <- points_ordered$a
b_vals <- points_ordered$b


# 6. Compute erosivity
erosivity_mat <- sweep(rain_mat^matrix(b_vals, nrow = nrow(rain_mat), ncol = ncol(rain_mat), byrow = TRUE),
                       2, a_vals, `*`)

erosivity_df <- as.data.frame(erosivity_mat)
names(erosivity_df) <- names(valid_comple_series)[1:dim(points)[1]]
erosivity_df$Date <- valid_comple_series$Date
erosivity_df$ref <- valid_comple_series$ref  # optional

write.csv(erosivity_df,"Liuetal2020_Erosivity_day.csv")

#Monthly

# 1. Drop the Date column (we’ll group by ref)
erosivity_data <- erosivity_df[, !(names(erosivity_df) %in% "Date")]

# 3. Group by ref and sum all daily values to get monthly erosivity
monthly_erosivity <- erosivity_data %>%
  group_by(ref) %>%
  summarise(across(.cols = everything(), .fns = ~ sum(.x, na.rm = TRUE)))

monthly_erosivity <- monthly_erosivity %>%
  arrange(as.Date(paste0(ref, "-01")))

write.csv(monthly_erosivity,"Liuetal2020_Erosivity_Month.csv")

#Year
monthly_erosivity$year <- substr(monthly_erosivity$ref, 1, 4)

yearly_erosivity <- monthly_erosivity %>%
  group_by(year) %>%
  summarise(across(.cols = -ref, .fns = ~ sum(.x, na.rm = TRUE))) %>%
  arrange(as.integer(year))

write.csv(yearly_erosivity,"Liuetal2020_Erosivity_Year.csv")

mean_erosivity <- colMeans(yearly_erosivity[, -1], na.rm = TRUE)
points$Erosivity_year_Liu2020 <- mean_erosivity

write.csv(points,"Erosivity_WS_2.csv")




####
#### Daily erosivity according to Richardson 1983
####

#calculating erosivity by day
valid_comple_series$Date <- as.Date(valid_comple_series$Date)
df=valid_comple_series[,1:dim(points)[1]]
df[df<12.5]<-NA

# Step 2: Convert to matrix
rain_mat <- as.matrix(df)

# Step 3: Create monthly 'a' vector and constant 'b'
month_vec <- as.integer(format(valid_comple_series$Date, "%m"))
a_vec <- ifelse(month_vec %in% c(10, 11, 12, 1, 2, 3), 0.1809, 0.41)
b <- 1.81

# Step 4: Calculate erosivity
erosivity_mat_new <- (rain_mat ^ b) * a_vec

# Step 5: Convert to dataframe and add Date
erosivity_df_new <- as.data.frame(erosivity_mat_new)
names(erosivity_df_new) <- names(valid_comple_series)[1:dim(points)[1]]  # or names(valid_comple_series)[1:1676]
erosivity_df_new$Date <- valid_comple_series$Date
erosivity_df_new$ref <- valid_comple_series$ref

write.csv(erosivity_df_new,"Richardson1983_Erosivity_day.csv")

monthly_erosivity <- erosivity_df_new %>%
  group_by(ref) %>%
  summarise(across(.cols = -Date, .fns = ~ sum(.x, na.rm = TRUE))) 

monthly_erosivity <- monthly_erosivity %>%
  arrange(as.Date(paste0(ref, "-01")))

write.csv(monthly_erosivity,"Richardson1983_Erosivity_Month.csv")


#Year
monthly_erosivity$year <- substr(monthly_erosivity$ref, 1, 4)

yearly_erosivity <- monthly_erosivity %>%
  group_by(year) %>%
  summarise(across(.cols = -ref, .fns = ~ sum(.x, na.rm = TRUE))) %>%
  arrange(as.integer(year))

write.csv(yearly_erosivity,"Richardson1983_Erosivity_Year.csv")

mean_erosivity <- colMeans(yearly_erosivity[, -1], na.rm = TRUE)
points$Erosivity_year_Richardson1983 <- mean_erosivity

write.csv(points,"Erosivity_WS_3.csv")



### Some Graphs

library(ggplot2)
library(tidyr)

# 1. Reshape the data to long format
points_long <- points %>%
  pivot_longer(cols = c(Erosivity_year_YunXie2016,
                        Erosivity_year_Richardson1983,
                        Erosivity_year_Liu2020),
               names_to = "Method", values_to = "Erosivity")

# 2. Create density plot
ggplot(points_long, aes(x = Erosivity, color = Method, fill = Method)) +
  geom_density(alpha = 0.3, linewidth = 1) +
  labs(title = "Density Plot of Annual Erosivity Estimates",
       x = "Annual Erosivity (MJ·mm/ha·h·year)",
       y = "Density") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1") +
  scale_color_brewer(palette = "Set1")

summary_table <- points_long %>%
  group_by(Method) %>%
  summarise(
    count = sum(!is.na(Erosivity)),
    mean = mean(Erosivity, na.rm = TRUE),
    median = median(Erosivity, na.rm = TRUE),
    sd = sd(Erosivity, na.rm = TRUE),
    min = min(Erosivity, na.rm = TRUE),
    max = max(Erosivity, na.rm = TRUE)
  )
boxplot(points$Erosivity_year_Liu2020)

