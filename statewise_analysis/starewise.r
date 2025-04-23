# ============================================================================
# SPATIAL ANALYSIS OF JHUM AND NON-JHUM CULTIVATION IN INDIA (LISA + MORAN'S I)
# ============================================================================

# Title:    Spatial Analysis of Jhum and Non-Jhum Cultivation in India (LISA + Moran's I)
# Author:   Somdeep Kundu
# Date:     21 April 2025
# Description: Reproducible R pipeline to compute and visualize LISA clusters,
#              p-value maps, and Moran's I statistics for jhum and non-jhum
#              cultivation across Indian States based on NSS 77th Round data
# Repository: https://github.com/somdeepkundu/lisa-moran-nss77/

# ----------------------------------------------------------------------------
# 1. SETUP & LIBRARIES
# ----------------------------------------------------------------------------

# Install required packages (uncomment if not already installed)
# install.packages(c("sf", "spdep", "tmap", "tidyverse", "ggplot2"))

library(sf)         # For spatial vector data
library(spdep)      # For spatial weights and Moran's I
library(tmap)       # For thematic mapping
library(tidyverse)  # For data wrangling
library(ggplot2)    # For plotting

# Define output directory
output_dir <- "YOUR DIRECTERY"

# ----------------------------------------------------------------------------
# 2. DATA INPUT
# ----------------------------------------------------------------------------

# Read shapefile
spatial_data_path <- file.path(output_dir, "state_j_nj.shp")
shp <- st_read(spatial_data_path)

# Split data into Jhum and Non-Jhum subsets
shp_jhum <- shp[!is.na(shp$pc_jhum), ]
shp_crop <- shp[!is.na(shp$pc_crop), ]

# ----------------------------------------------------------------------------
# 3. SPATIAL WEIGHTS
# ----------------------------------------------------------------------------

queen_connectivity <- TRUE
snap_tolerance <- 0.005
weight_style <- "W"
zero_policy_setting <- TRUE

nb_jhum <- poly2nb(shp_jhum, queen = queen_connectivity, snap = snap_tolerance)
lw_jhum <- nb2listw(nb_jhum, style = weight_style, zero.policy = zero_policy_setting)

nb_crop <- poly2nb(shp_crop, queen = queen_connectivity, snap = snap_tolerance)
lw_crop <- nb2listw(nb_crop, style = weight_style, zero.policy = zero_policy_setting)

# ----------------------------------------------------------------------------
# 4. LOCAL MORAN'S I CALCULATION
# ----------------------------------------------------------------------------

lisa_jhum <- localmoran(shp_jhum$pc_jhum, lw_jhum, zero.policy = zero_policy_setting)
shp_jhum <- shp_jhum %>% mutate(Ii_jhum = lisa_jhum[,1], ZIi_jhum = lisa_jhum[,4], pval_jhum = lisa_jhum[,5])

lisa_crop <- localmoran(shp_crop$pc_crop, lw_crop, zero.policy = zero_policy_setting)
shp_crop <- shp_crop %>% mutate(Ii_crop = lisa_crop[,1], ZIi_crop = lisa_crop[,4], pval_crop = lisa_crop[,5])

# ----------------------------------------------------------------------------
# 5. LISA CLUSTER CATEGORIZATION
# ----------------------------------------------------------------------------

z_jhum <- scale(shp_jhum$pc_jhum)[, 1]
z_crop <- scale(shp_crop$pc_crop)[, 1]

assign_lisa_clusters <- function(z_var, moran_z, p_val, threshold = 0.01) {
  case_when(
    z_var > 0 & moran_z > 0 & p_val <= threshold ~ "High-High",
    z_var < 0 & moran_z > 0 & p_val <= threshold ~ "Low-Low",
    z_var > 0 & moran_z < 0 & p_val <= threshold ~ "High-Low",
    z_var < 0 & moran_z < 0 & p_val <= threshold ~ "Low-High",
    TRUE ~ "Not Significant"
  )
}

shp_jhum <- shp_jhum %>% mutate(cluster_jhum = assign_lisa_clusters(z_jhum, ZIi_jhum, pval_jhum))
shp_crop <- shp_crop %>% mutate(cluster_crop = assign_lisa_clusters(z_crop, ZIi_crop, pval_crop))

# ----------------------------------------------------------------------------
# 6. COLOR PALETTES
# ----------------------------------------------------------------------------

lisa_cluster_palette <- c("High-High" = "#b2182b", "Low-Low" = "#2166ac", "High-Low" = "#ef8a62", "Low-High" = "#67a9cf", "Not Significant" = "lightgrey")
p_value_palette_jhum <- c("p < 0.01" = "#67001f", "p < 0.05" = "#b2182b", "p < 0.10" = "#ef8a62", "Not Significant" = "grey80")
p_value_palette_crop <- c("p < 0.01" = "#053061", "p < 0.05" = "#2166ac", "p < 0.10" = "#67a9cf", "Not Significant" = "grey80")

# ----------------------------------------------------------------------------
# 7. CLUSTER MAP VISUALIZATION
# ----------------------------------------------------------------------------

tmap_mode("plot")

jhum_map <- tm_shape(shp_jhum) +
  tm_fill("cluster_jhum", palette = lisa_cluster_palette, title = "Jhum Cluster Type") +
  tm_borders() +
  tm_layout(title = "LISA Clusters of \nJhum Cultivation", title.position = c(0.45, 0.93), main.title = "(NSS 77th Round, 2018)", main.title.size = 0.8, legend.position = c(0.50, 0.25))
print(jhum_map)
tmap_save(jhum_map, filename = file.path(output_dir, "Jhum_LISA_Cluster.png"), width = 8, height = 6, dpi = 600)

crop_map <- tm_shape(shp_crop) +
  tm_fill("cluster_crop", palette = lisa_cluster_palette, title = "Crop Cluster Type") +
  tm_borders() +
  tm_layout(title = "LISA Clusters of\nNon-Jhum Cultivation", title.position = c(0.45, 0.93), main.title = "(NSS 77th Round, 2018)", main.title.size = 0.8, legend.position = c(0.5, 0.25))
print(crop_map)
tmap_save(crop_map, filename = file.path(output_dir, "Crop_LISA_Cluster.png"), width = 8, height = 6, dpi = 600)

# ----------------------------------------------------------------------------
# 8. P-VALUE SIGNIFICANCE MAP VISUALIZATION
# ----------------------------------------------------------------------------

shp_jhum <- shp_jhum %>% mutate(pval_class_jhum = cut(pval_jhum, breaks = c(0, 0.01, 0.05, 0.10, 1), labels = c("p < 0.01", "p < 0.05", "p < 0.10", "Not Significant"), right = FALSE))
shp_crop <- shp_crop %>% mutate(pval_class_crop = cut(pval_crop, breaks = c(0, 0.01, 0.05, 0.10, 1), labels = c("p < 0.01", "p < 0.05", "p < 0.10", "Not Significant"), right = FALSE))

jhum_pval_map <- tm_shape(shp_jhum) +
  tm_fill("pval_class_jhum", palette = p_value_palette_jhum, title = "Jhum LISA p-values") +
  tm_borders() +
  tm_layout(title = "LISA Significance\n(Jhum Cultivation)", title.position = c(0.45, 0.90), legend.position = c(0.50, 0.25))
print(jhum_pval_map)
tmap_save(jhum_pval_map, filename = file.path(output_dir, "Jhum_PValue_Map.png"), width = 8, height = 6, dpi = 600)

crop_pval_map <- tm_shape(shp_crop) +
  tm_fill("pval_class_crop", palette = p_value_palette_crop, title = "Crop LISA p-values") +
  tm_borders() +
  tm_layout(title = "LISA Significance\n(Non-Jhum Crop)", title.position = c(0.45, 0.90), legend.position = c(0.50, 0.25))
print(crop_pval_map)
tmap_save(crop_pval_map, filename = file.path(output_dir, "Crop_PValue_Map.png"), width = 8, height = 6, dpi = 600)

# ----------------------------------------------------------------------------
# 9. GLOBAL MORAN'S I AND SCATTER PLOTS
# ----------------------------------------------------------------------------

moran_jhum_test <- moran.test(shp_jhum$pc_jhum, lw_jhum, zero.policy = zero_policy_setting)
moran_crop_test <- moran.test(shp_crop$pc_crop, lw_crop, zero.policy = zero_policy_setting)

cat("Global Moran's I for Jhum Cultivation:\n")
print(moran_jhum_test)

cat("\nGlobal Moran's I for Non-Jhum Cultivation:\n")
print(moran_crop_test)

# Moran Scatter Plot: Jhum
lag_jhum <- lag.listw(lw_jhum, shp_jhum$pc_jhum, zero.policy = TRUE)
scatter_jhum_data <- tibble(x = scale(shp_jhum$pc_jhum)[,1], y = scale(lag_jhum)[,1])

p_moran_jhum <- ggplot(scatter_jhum_data, aes(x = x, y = y)) +
  geom_point(shape = 21, color = "#1f78b4", fill = "#a6cee3", size = 2, alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "darkred", linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey60") +
  labs(title = "Moran Scatter Plot: Jhum Cultivation", x = "Standardized Jhum (%)", y = "Spatial Lag of Jhum (%)") +
  theme_bw(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
print(p_moran_jhum)
ggsave(filename = file.path(output_dir, "Jhum_Moran_Scatter.png"), plot = p_moran_jhum, width = 6, height = 6, dpi = 600)

# Moran Scatter Plot: Non-Jhum
lag_crop <- lag.listw(lw_crop, shp_crop$pc_crop, zero.policy = TRUE)
scatter_crop_data <- tibble(x = scale(shp_crop$pc_crop)[,1], y = scale(lag_crop)[,1])

p_moran_crop <- ggplot(scatter_crop_data, aes(x = x, y = y)) +
  geom_point(shape = 21, color = "#33a02c", fill = "#a6cee3", size = 2, alpha = 0.8) +
  geom_smooth(method = "lm", se = FALSE, color = "darkred", linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey60") +
  labs(title = "Moran Scatter Plot: Non-Jhum Crop", x = "Standardized Non-Jhum (%)", y = "Spatial Lag of Non-Jhum (%)") +
  theme_bw(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
print(p_moran_crop)
ggsave(filename = file.path(output_dir, "Crop_Moran_Scatter.png"), plot = p_moran_crop, width = 6, height = 6, dpi = 600)

# ----------------------------------------------------------------------------
# 10. EXPORT FINAL SHAPEFILES (OPTIONAL)
# ----------------------------------------------------------------------------

st_write(shp_jhum, dsn = file.path(output_dir, "Jhum_LISA_Analysis.shp"), delete_layer = TRUE)
st_write(shp_crop, dsn = file.path(output_dir, "Crop_LISA_Analysis.shp"), delete_layer = TRUE)

