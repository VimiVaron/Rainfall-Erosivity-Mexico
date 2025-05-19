# Load required libraries
library(sf)
library(nngeo)
library(terra)
library(tidyr)
library(dplyr)
library(ggplot2)


# Load the two dataframes from CSV files
R_GloREDa <- read.csv("C:/Users/vimiv/OneDrive - AGROSAVIA - CORPORACION COLOMBIANA DE INVESTIGACION AGROPECUARIA/Documentos/DOCTORADO/Proyecto_CONAHCYT/GloREDa_MXN.csv",dec = ".",sep = ";")
R_GloREDa$Dataset="GloREDa"
colnames(R_GloREDa)[13]="EI30"
colnames(R_GloREDa)[c(4,5,6)] <- c("longitude", "latitude","altitude")

R_GloREDa_sf <- st_as_sf(R_GloREDa, coords = c("longitude", "latitude"), crs = 4326)

# Load ecoregions shapefile as sf object 
Ecoregions <- vect("C:/Users/vimiv/OneDrive - AGROSAVIA - CORPORACION COLOMBIANA DE INVESTIGACION AGROPECUARIA/Documentos/DOCTORADO/Proyecto_CONAHCYT/SHP/ecort08gw/ecort08gw.shp")
Ecoregions_sf <- st_as_sf(Ecoregions)

# Ensure CRS match
R_GloREDa_sf <- st_transform(R_GloREDa_sf, crs = st_crs(Ecoregions_sf))

# Perform spatial join
joined <- st_join(R_GloREDa_sf, Ecoregions_sf[, c("DESECON1")])  # Use correct name here!

# Add new "Ecoregion" column to the original dataframe
R_GloREDa$Ecoregion <- joined$DESECON1
R_GloREDa_sf$Ecoregion <- joined$DESECON1




R_P3=read.csv("C:/Users/vimiv/OneDrive - AGROSAVIA - CORPORACION COLOMBIANA DE INVESTIGACION AGROPECUARIA/Documentos/DOCTORADO/Proyecto_CONAHCYT/Climate_Data/Daily_Resolution/P_1988_2017/Erosivity_WS_3.csv")
R_P3$Dataset="Mexico-CN3"
colnames(R_P3)[c(4,5,6)] <- c("longitude", "latitude","altitude")

R_P3_sf <- st_as_sf(R_P3, coords = c("longitude", "latitude"), crs = 4326)


# Find nearest neighbor from df1450 to each point in df15
nearest <- st_nn(R_GloREDa_sf, R_P3_sf, k = 1, returnDist = TRUE)

# Extract index and distance
nearest_index <- unlist(nearest$nn)
nearest_distance <- unlist(nearest$dist)

# Extract nearest points and add to df15
nearest_points <- R_P3_sf[nearest_index, ]
R_GloREDa_R_P3 <- cbind(R_GloREDa, nearest_points, distance_km = nearest_distance / 1000)


# Save result to CSV if needed
write.csv(R_GloREDa_R_P3, "C:/Users/vimiv/OneDrive - AGROSAVIA - CORPORACION COLOMBIANA DE INVESTIGACION AGROPECUARIA/Documentos/DOCTORADO/Proyecto_CONAHCYT/R_GloREDa_R_P3.csv", row.names = FALSE)




# Step 1: Reshape your data to long format
data_long <- R_GloREDa_R_P3 %>%
  select(EI30,
         Erosivity_year_Richardson1983,
         Erosivity_year_Liu2020,
         Erosivity_year_YunXie2016)          %>%
  pivot_longer(cols = starts_with("Erosivity_year"),
               names_to = "Method",
               values_to = "Estimated_Erosivity")

# Step 2: Set method factor order and labels
data_long$Method <- factor(data_long$Method,
                           levels = c("Erosivity_year_Richardson1983",
                                      "Erosivity_year_Liu2020",
                                      "Erosivity_year_YunXie2016"),
                           labels = c("Model I", "Model II", "Model III"))



# Step 3: Fit models and extract slope + R²
model_summaries <- data_long %>%
  group_by(Method) %>%
  summarise(
    model = list(lm(Estimated_Erosivity ~ EI30, data = pick(everything()))),
    .groups = "drop"
  ) %>%
  mutate(
    slope = sapply(model, function(m) coef(m)[2]),
    r2 = sapply(model, function(m) summary(m)$r.squared),
    label = paste0("Slope = ", round(slope, 2), ",  R² = ", round(r2, 2))
  )

# Step 4: Label positions
label_positions <- tribble(
  ~Method,      ~x,     ~y,
  "Model I",    2500,  45000,
  "Model II",   2500,  42500,
  "Model III",  2500,  40000
)

# Step 5: Combine labels and positions
model_labels <- left_join(model_summaries, label_positions, by = "Method")

# Step 6: Plot

fig <- ggplot(data_long, aes(x = EI30, y = Estimated_Erosivity, color = Method)) +
  geom_point(size = 0.5, alpha = 0.8) +
  geom_smooth(
    method = "lm",
    se = TRUE,
    linewidth = 0.1,
    alpha = 0.2,
    aes(color = Method, fill = Method)
  ) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray30",linewidth = 0.2) +
  geom_text(data = model_labels,
            aes(x = x, y = y, label = label, color = Method),
            hjust = 0, vjust = 1, size = 1.8, show.legend = FALSE) +
  labs(x = expression(EI30~Factor~(MJ~mm~ha^{-1}~h^{-1}~yr^{-1})),
       y = expression(R~Factor~(MJ~mm~ha^{-1}~h^{-1}~yr^{-1}))) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 6, color = "black", face = "bold"),
    axis.title.y = element_text(size = 6, color = "black", face = "bold"),
    axis.text.x = element_text(size = 5, color = "black"),
    axis.text.y = element_text(size = 5, color = "black"),
    panel.grid.major = element_line(linewidth = 0.1),
    plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), 'cm'),
    legend.title = element_blank(),
    legend.text = element_text(size = 6),
    legend.key.size = unit(0.2, "cm"),
    legend.margin = margin(0.2, 0.2, 0.2, 0.2),
    legend.position = "bottom"
  ) +
  guides(color = guide_legend(override.aes = list(size = 2, key.width = 0.3, key.height = 0.3))) +
  scale_color_manual(values = c("Model I" = "#AC3EC1",
                                "Model II" = "#477BD1",
                                "Model III" = "#46B298")) +
  scale_fill_manual(values = c("Model I" = "#AC3EC1",
                               "Model II" = "#477BD1",
                               "Model III" = "#46B298"))+
  coord_cartesian(ylim = c(0, 50000),xlim = c(0,23000))






# Step 7: Save the figure
ppi <- 500
w <- 8.5
h <- 6

png(filename = "C:/Users/vimiv/OneDrive - AGROSAVIA - CORPORACION COLOMBIANA DE INVESTIGACION AGROPECUARIA/Documentos/DOCTORADO/Proyecto_CONAHCYT/Figures_Article/points_GloREDa_MexicoP3.png",
    width = w,
    height = h,
    units = "cm",
    res = ppi)

print(fig)
dev.off()


#### Performance metrics
library(dplyr)
library(purrr)

# Define models
models <- c("Erosivity_year_Richardson1983",
            "Erosivity_year_Liu2020",
            "Erosivity_year_YunXie2016")

# Function to compute all metrics
compute_metrics <- function(pred, obs) {
  me   <- mean(pred - obs, na.rm = TRUE)
  rmse <- sqrt(mean((pred - obs)^2, na.rm = TRUE))
   re   <- sum(abs(pred - obs), na.rm = TRUE) / sum(obs, na.rm = TRUE)
  nse  <- 1 - sum((obs - pred)^2, na.rm = TRUE) / sum((obs - mean(obs, na.rm = TRUE))^2, na.rm = TRUE)
  data.frame(ME = me, RMSE = rmse, RE = re, NSE = nse)
}

# Apply to each model
results <- map_dfr(models, function(m) {
  compute_metrics(R_GloREDa_R_P3[[m]], R_GloREDa_R_P3$EI30)
}, .id = "Model")

# Rename model names
results$Model <- c("Model I", "Model II", "Model III")

# Round for clarity
results <- results %>%
  mutate(across(where(is.numeric), ~ round(., 3)))

# Print
print(results)




#########################Cortés - P1 #############

# Load the two dataframes from CSV files
R_Cortes=read.csv("C:/Users/vimiv/OneDrive - AGROSAVIA - CORPORACION COLOMBIANA DE INVESTIGACION AGROPECUARIA/Documentos/DOCTORADO/Proyecto_CONAHCYT/Erosividad_Cortes.csv",sep = ";")
R_Cortes$Dataset="Cortés1991"
colnames(R_Cortes)[8]="EI30"
colnames(R_Cortes)[c(4,3,5)] <- c("longitude", "latitude","altitude")
R_Cortes=R_Cortes[R_Cortes$Years>=5,]

R_Cortes_sf <- st_as_sf(R_Cortes, coords = c("longitude", "latitude"), crs = 4326)

# Ensure CRS match
R_Cortes_sf  <- st_transform(R_Cortes_sf , crs = st_crs(Ecoregions_sf))

# Perform spatial join
joined <- st_join(R_Cortes_sf , Ecoregions_sf[, c("DESECON1")])  # Use correct name here!

# Add new "Ecoregion" column to the original dataframe
R_Cortes$Ecoregion <- joined$DESECON1
R_Cortes_sf$Ecoregion <- joined$DESECON1




R_P1=read.csv("C:/Users/vimiv/OneDrive - AGROSAVIA - CORPORACION COLOMBIANA DE INVESTIGACION AGROPECUARIA/Documentos/DOCTORADO/Proyecto_CONAHCYT/Climate_Data/Daily_Resolution/P_1968_1997/Erosivity_WS_3.csv")
R_P1$Dataset="Mexico-CN1"
colnames(R_P1)[c(4,5,6)] <- c("longitude", "latitude","altitude")

R_P1_sf <- st_as_sf(R_P1, coords = c("longitude", "latitude"), crs = 4326)


# Find nearest neighbor from df1450 to each point in df15
nearest <- st_nn(R_Cortes_sf, R_P1_sf, k = 1, returnDist = TRUE)

# Extract index and distance
nearest_index <- unlist(nearest$nn)
nearest_distance <- unlist(nearest$dist)

# Extract nearest points and add to df15
nearest_points <- R_P1_sf[nearest_index, ]
R_Cortes_R_P1 <- cbind(R_Cortes, nearest_points, distance_km = nearest_distance / 1000)

# Save result to CSV if needed
write.csv(R_Cortes_R_P1, "C:/Users/vimiv/OneDrive - AGROSAVIA - CORPORACION COLOMBIANA DE INVESTIGACION AGROPECUARIA/Documentos/DOCTORADO/Proyecto_CONAHCYT/R_Cortes_R_P1.csv", row.names = FALSE)



# Step 1: Reshape your data to long format
data_long <- R_Cortes_R_P1 %>%
  select(EI30,
         Erosivity_year_Richardson1983,
         Erosivity_year_Liu2020,
         Erosivity_year_YunXie2016) %>%
  pivot_longer(cols = starts_with("Erosivity_year"),
               names_to = "Method",
               values_to = "Estimated_Erosivity")

# Step 2: Set method factor order and labels
data_long$Method <- factor(data_long$Method,
                           levels = c("Erosivity_year_Richardson1983",
                                      "Erosivity_year_Liu2020",
                                      "Erosivity_year_YunXie2016"),
                           labels = c("Model I", "Model II", "Model III"))

# Step 3: Fit models and extract slope + R²
model_summaries <- data_long %>%
  group_by(Method) %>%
  summarise(
    model = list(lm(Estimated_Erosivity ~ EI30, data = pick(everything()))),
    .groups = "drop"
  ) %>%
  mutate(
    slope = sapply(model, function(m) coef(m)[2]),
    r2 = sapply(model, function(m) summary(m)$r.squared),
    label = paste0("Slope = ", round(slope, 2), ",  R² = ", round(r2, 2))
  )

# Step 4: Label positions
label_positions <- tribble(
  ~Method,      ~x,     ~y,
  "Model I",    2500,  35000,
  "Model II",   2500,  33000,
  "Model III",  2500,  31000
)

# Step 5: Combine labels and positions
model_labels <- left_join(model_summaries, label_positions, by = "Method")

# Step 6: Plot
fig <- ggplot(data_long, aes(x = EI30 , y = Estimated_Erosivity, color = Method)) +
  geom_point(size = 0.5, alpha = 0.8) +
  geom_smooth(
    method = "lm",
    se = TRUE,
    linewidth = 0.1,
    alpha = 0.2,
    aes(color = Method, fill = Method)
  ) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray30",linewidth = 0.2) +
  geom_text(data = model_labels,
            aes(x = x, y = y, label = label, color = Method),
            hjust = 0, vjust = 1, size = 1.8, show.legend = FALSE) +
  labs(x = expression(EI30~Factor~(MJ~mm~ha^{-1}~h^{-1}~yr^{-1})),
       y = expression(R~Factor~(MJ~mm~ha^{-1}~h^{-1}~yr^{-1}))) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 6, color = "black", face = "bold"),
    axis.title.y = element_text(size = 6, color = "black", face = "bold"),
    axis.text.x = element_text(size = 5, color = "black"),
    axis.text.y = element_text(size = 5, color = "black"),
    panel.grid.major = element_line(linewidth = 0.1),
    plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), 'cm'),
    legend.title = element_blank(),
    legend.text = element_text(size = 6),
    legend.key.size = unit(0.2, "cm"),
    legend.margin = margin(0.2, 0.2, 0.2, 0.2),
    legend.position = "bottom"
  ) +
  guides(color = guide_legend(override.aes = list(size = 2, key.width = 0.3, key.height = 0.3))) +
  scale_color_manual(values = c("Model I" = "#AC3EC1",
                                "Model II" = "#477BD1",
                                "Model III" = "#46B298")) +
  scale_fill_manual(values = c("Model I" = "#AC3EC1",
                               "Model II" = "#477BD1",
                               "Model III" = "#46B298"))+
  coord_cartesian(ylim = c(0, 40000),xlim = c(0,27000))

# Step 7: Save the figure
ppi <- 500
w <- 8.5
h <- 6

png(filename = "C:/Users/vimiv/OneDrive - AGROSAVIA - CORPORACION COLOMBIANA DE INVESTIGACION AGROPECUARIA/Documentos/DOCTORADO/Proyecto_CONAHCYT/Figures_Article/points_Cortes_MexicoP1.png",
    width = w,
    height = h,
    units = "cm",
    res = ppi)

print(fig)
dev.off()

# Apply to each model
results <- map_dfr(models, function(m) {
  compute_metrics(R_Cortes_R_P1[[m]], R_Cortes_R_P1$EI30)
}, .id = "Model")

# Rename model names
results$Model <- c("Model I", "Model II", "Model III")

# Round for clarity
results <- results %>%
  mutate(across(where(is.numeric), ~ round(., 3)))

# Print
print(results)



########################

#########################Cortés - P2 #############

# Load the two dataframes from CSV files
R_Cortes=read.csv("C:/Users/vimiv/OneDrive - AGROSAVIA - CORPORACION COLOMBIANA DE INVESTIGACION AGROPECUARIA/Documentos/DOCTORADO/Proyecto_CONAHCYT/Erosividad_Cortes.csv",sep = ";")
R_Cortes$Dataset="Cortés1991"
colnames(R_Cortes)[8]="EI30"
colnames(R_Cortes)[c(4,3,5)] <- c("longitude", "latitude","altitude")
R_Cortes=R_Cortes[R_Cortes$Years>=5,]

R_Cortes_sf <- st_as_sf(R_Cortes, coords = c("longitude", "latitude"), crs = 4326)

# Ensure CRS match
R_Cortes_sf  <- st_transform(R_Cortes_sf , crs = st_crs(Ecoregions_sf))

# Perform spatial join
joined <- st_join(R_Cortes_sf , Ecoregions_sf[, c("DESECON1")])  # Use correct name here!

# Add new "Ecoregion" column to the original dataframe
R_Cortes$Ecoregion <- joined$DESECON1
R_Cortes_sf$Ecoregion <- joined$DESECON1


R_P2=read.csv("C:/Users/vimiv/OneDrive - AGROSAVIA - CORPORACION COLOMBIANA DE INVESTIGACION AGROPECUARIA/Documentos/DOCTORADO/Proyecto_CONAHCYT/Climate_Data/Daily_Resolution/P_1978_2007/Erosivity_WS_3.csv")
R_P2$Dataset="Mexico-CN2"
colnames(R_P2)[c(4,5,6)] <- c("longitude", "latitude","altitude")

R_P2_sf <- st_as_sf(R_P2, coords = c("longitude", "latitude"), crs = 4326)


# Find nearest neighbor from df1450 to each point in df15
nearest <- st_nn(R_Cortes_sf, R_P2_sf, k = 1, returnDist = TRUE)

# Extract index and distance
nearest_index <- unlist(nearest$nn)
nearest_distance <- unlist(nearest$dist)

# Extract nearest points and add to df15
nearest_points <- R_P2_sf[nearest_index, ]
R_Cortes_R_P2 <- cbind(R_Cortes, nearest_points, distance_km = nearest_distance / 1000)

# Save result to CSV if needed
write.csv(R_Cortes_R_P2, "C:/Users/vimiv/OneDrive - AGROSAVIA - CORPORACION COLOMBIANA DE INVESTIGACION AGROPECUARIA/Documentos/DOCTORADO/Proyecto_CONAHCYT/R_Cortes_R_P2.csv", row.names = FALSE)



# Step 1: Reshape your data to long format
data_long <- R_Cortes_R_P2 %>%
  select(EI30,
         Erosivity_year_Richardson1983,
         Erosivity_year_Liu2020,
         Erosivity_year_YunXie2016) %>%
  pivot_longer(cols = starts_with("Erosivity_year"),
               names_to = "Method",
               values_to = "Estimated_Erosivity")

# Step 2: Set method factor order and labels
data_long$Method <- factor(data_long$Method,
                           levels = c("Erosivity_year_Richardson1983",
                                      "Erosivity_year_Liu2020",
                                      "Erosivity_year_YunXie2016"),
                           labels = c("Model I", "Model II", "Model III"))

# Step 3: Fit models and extract slope + R²
model_summaries <- data_long %>%
  group_by(Method) %>%
  summarise(
    model = list(lm(Estimated_Erosivity ~ EI30, data = pick(everything()))),
    .groups = "drop"
  ) %>%
  mutate(
    slope = sapply(model, function(m) coef(m)[2]),
    r2 = sapply(model, function(m) summary(m)$r.squared),
    label = paste0("Slope = ", round(slope, 2), ",  R² = ", round(r2, 2))
  )

# Step 4: Label positions
label_positions <- tribble(
  ~Method,      ~x,     ~y,
  "Model I",    2500,  35000,
  "Model II",   2500,  33000,
  "Model III",  2500,  31000
)

# Step 5: Combine labels and positions
model_labels <- left_join(model_summaries, label_positions, by = "Method")

# Step 6: Plot
fig <- ggplot(data_long, aes(x =EI30 , y = Estimated_Erosivity, color = Method)) +
  geom_point(size = 0.5, alpha = 0.8) +
  geom_smooth(
    method = "lm",
    se = TRUE,
    linewidth = 0.1,
    alpha = 0.2,
    aes(color = Method, fill = Method)
  ) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray30",linewidth = 0.2) +
  geom_text(data = model_labels,
            aes(x = x, y = y, label = label, color = Method),
            hjust = 0, vjust = 1, size = 1.8, show.legend = FALSE) +
  labs(x = expression(EI30~Factor~(MJ~mm~ha^{-1}~h^{-1}~yr^{-1})),
       y = expression(R~Factor~(MJ~mm~ha^{-1}~h^{-1}~yr^{-1}))) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 6, color = "black", face = "bold"),
    axis.title.y = element_text(size = 6, color = "black", face = "bold"),
    axis.text.x = element_text(size = 5, color = "black"),
    axis.text.y = element_text(size = 5, color = "black"),
    panel.grid.major = element_line(linewidth = 0.1),
    plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), 'cm'),
    legend.title = element_blank(),
    legend.text = element_text(size = 6),
    legend.key.size = unit(0.2, "cm"),
    legend.margin = margin(0.2, 0.2, 0.2, 0.2),
    legend.position = "bottom"
  ) +
  guides(color = guide_legend(override.aes = list(size = 2, key.width = 0.3, key.height = 0.3))) +
  scale_color_manual(values = c("Model I" = "#AC3EC1",
                                "Model II" = "#477BD1",
                                "Model III" = "#46B298")) +
  scale_fill_manual(values = c("Model I" = "#AC3EC1",
                               "Model II" = "#477BD1",
                               "Model III" = "#46B298"))+
  coord_cartesian(ylim = c(0, 40000),xlim = c(0,27000))

# Step 7: Save the figure
ppi <- 500
w <- 8.5
h <- 6

png(filename = "C:/Users/vimiv/OneDrive - AGROSAVIA - CORPORACION COLOMBIANA DE INVESTIGACION AGROPECUARIA/Documentos/DOCTORADO/Proyecto_CONAHCYT/Figures_Article/points_Cortes_MexicoP2.png",
    width = w,
    height = h,
    units = "cm",
    res = ppi)

print(fig)
dev.off()

# Apply to each model
results <- map_dfr(models, function(m) {
  compute_metrics(R_Cortes_R_P2[[m]], R_Cortes_R_P2$EI30)
}, .id = "Model")

# Rename model names
results$Model <- c("Model I", "Model II", "Model III")

# Round for clarity
results <- results %>%
  mutate(across(where(is.numeric), ~ round(., 3)))

# Print
print(results)




##### Michoacán
R_Mich=read.csv("C:/Users/vimiv/OneDrive - AGROSAVIA - CORPORACION COLOMBIANA DE INVESTIGACION AGROPECUARIA/Documentos/DOCTORADO/Proyecto_CONAHCYT/Erosividad_Michoacan.csv")
R_Mich=R_Mich[complete.cases(R_Mich[,c(10,11,27,28)]),]

# Step 1: Reshape your data to long format
data_long <- R_Mich %>%
  select(EI30,
         Erosivity_year_Richardson1983,
         Erosivity_year_Liu2020,
         Erosivity_year_YunXie2016) %>%
  pivot_longer(cols = starts_with("Erosivity_year"),
               names_to = "Method",
               values_to = "Estimated_Erosivity")

# Step 2: Set method factor order and labels
data_long$Method <- factor(data_long$Method,
                           levels = c("Erosivity_year_Richardson1983",
                                      "Erosivity_year_Liu2020",
                                      "Erosivity_year_YunXie2016"),
                           labels = c("Model I", "Model II", "Model III"))

# Step 3: Fit models and extract slope + R²
model_summaries <- data_long %>%
  group_by(Method) %>%
  summarise(
    model = list(lm(Estimated_Erosivity ~ EI30, data = pick(everything()))),
    .groups = "drop"
  ) %>%
  mutate(
    slope = sapply(model, function(m) coef(m)[2]),
    r2 = sapply(model, function(m) summary(m)$r.squared),
    label = paste0("Slope = ", round(slope, 2), ",  R² = ", round(r2, 2))
  )

# Step 4: Label positions
label_positions <- tribble(
  ~Method,      ~x,     ~y,
  "Model I",    2500,  20000,
  "Model II",   2500,  18800,
  "Model III",  2500,  17600
)

# Step 5: Combine labels and positions
model_labels <- left_join(model_summaries, label_positions, by = "Method")

# Step 6: Plot
fig <- ggplot(data_long, aes(x = EI30, y = Estimated_Erosivity, color = Method)) +
  geom_point(size = 0.5, alpha = 0.8) +
  geom_smooth(
    method = "lm",
    se = TRUE,
    linewidth = 0.1,
    alpha = 0.2,
    aes(color = Method, fill = Method)
  ) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray30",linewidth = 0.2) +
  geom_text(data = model_labels,
            aes(x = x, y = y, label = label, color = Method),
            hjust = 0, vjust = 1, size = 1.8, show.legend = FALSE) +
  labs(x = expression(EI30~Factor~(MJ~mm~ha^{-1}~h^{-1}~yr^{-1})),
       y = expression(R~Factor~(MJ~mm~ha^{-1}~h^{-1}~yr^{-1}))) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 6, color = "black", face = "bold"),
    axis.title.y = element_text(size = 6, color = "black", face = "bold"),
    axis.text.x = element_text(size = 5, color = "black"),
    axis.text.y = element_text(size = 5, color = "black"),
    panel.grid.major = element_line(linewidth = 0.1),
    plot.margin = unit(c(0.1, 0.3, 0.1, 0.1), 'cm'),
    legend.title = element_blank(),
    legend.text = element_text(size = 6),
    legend.key.size = unit(0.2, "cm"),
    legend.margin = margin(0.2, 0.4, 0.2, 0.2),
    legend.position = "bottom"
  ) +
  guides(color = guide_legend(override.aes = list(size = 2, key.width = 0.3, key.height = 0.3))) +
  scale_color_manual(values = c("Model I" = "#AC3EC1",
                                "Model II" = "#477BD1",
                                "Model III" = "#46B298")) +
  scale_fill_manual(values = c("Model I" = "#AC3EC1",
                               "Model II" = "#477BD1",
                               "Model III" = "#46B298"))+
  coord_cartesian(ylim = c(0, 23000),xlim = c(0,23000))

# Step 7: Save the figure
ppi <- 500
w <- 8.5
h <- 6

png(filename = "C:/Users/vimiv/OneDrive - AGROSAVIA - CORPORACION COLOMBIANA DE INVESTIGACION AGROPECUARIA/Documentos/DOCTORADO/Proyecto_CONAHCYT/Figures_Article/points_Michoacan.png",
    width = w,
    height = h,
    units = "cm",
    res = ppi)

print(fig)
dev.off()


# Apply to each model
results <- map_dfr(models, function(m) {
  compute_metrics(R_Mich[[m]], R_Mich$EI30)
}, .id = "Model")

# Rename model names
results$Model <- c("Model I", "Model II", "Model III")

# Round for clarity
results <- results %>%
  mutate(across(where(is.numeric), ~ round(., 3)))

# Print
print(results)




###### Density plot ############

library(dplyr)
library(ggplot2)

# Step 1: Select EI30 and label each dataset
df1_selected <- R_GloREDa_R_P3 %>% select(EI30) %>% mutate(Source = "GloREDa")
df2_selected <- R_Cortes_R_P1 %>% select(EI30) %>% mutate(Source = "Cortés")
df3_selected <- R_Mich %>% select(EI30) %>% mutate(Source = "Michoacán")

# Step 2: Combine all into one dataframe
combined_df <- bind_rows(df1_selected, df2_selected, df3_selected)

fig=ggplot(combined_df, aes(x = EI30, color = Source, fill = Source)) +
  geom_density(alpha = 0.3, linewidth = 0.8) +
  theme_minimal() +
  labs(
    x = expression(EI30~Factor~(MJ~mm~ha^{-1}~h^{-1}~yr^{-1})),
    y = "Density"
  )+
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 8, color = "black", face = "bold"),
    axis.title.y = element_text(size = 8, color = "black", face = "bold"),
    axis.text.x = element_text(size = 7, color = "black"),
    axis.text.y = element_text(size = 7, color = "black"),
    panel.grid.major = element_line(linewidth = 0.1),
    plot.margin = unit(c(0.1, 0.3, 0.1, 0.1), 'cm'),
    legend.title = element_blank(),
    legend.text = element_text(size = 8),
    legend.key.size = unit(0.2, "cm"),
    legend.margin = margin(0.2, 0.4, 0.2, 0.2),
    legend.position = "bottom"
  ) +
  guides(color = guide_legend(override.aes = list(size = 2, key.width = 0.3, key.height = 0.3))) +
  scale_color_manual(values = c("GloREDa" = "#EE9572",
                                "Cortés" = "#7CCD7C",
                                "Michoacán" = "#CDCD00")) +
  scale_fill_manual(values = c("GloREDa" = "#EE9572",
                               "Cortés" = "#7CCD7C",
                               "Michoacán" = "#CDCD00"))+
  coord_cartesian(xlim = c(0,27000))

# Step 7: Save the figure
ppi <- 500
w <- 15
h <- 10

png(filename = "C:/Users/vimiv/OneDrive - AGROSAVIA - CORPORACION COLOMBIANA DE INVESTIGACION AGROPECUARIA/Documentos/DOCTORADO/Proyecto_CONAHCYT/Figures_Article/density_validation_datasets.png",
    width = w,
    height = h,
    units = "cm",
    res = ppi)

print(fig)
dev.off()

 



