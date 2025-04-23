# ============================================================================
# SPATIAL ANALYSIS OF JHUM AND NON-JHUM CULTIVATION IN INDIA (LISA + MORAN'S I)
# ============================================================================

# Title:    Spatial Analysis of Jhum and Non-Jhum Cultivation in India (LISA + Moran's I)
# Author:   Somdeep Kundu
# Date:     21 April 2025
# Description: Reproducible R pipeline to compute and visualize LISA clusters,
#              p-value maps, and Moran's I statistics for jhum and non-jhum
#              cultivation across Indian regions based on NSS 77th Round data
# Submitted to Prof. Satish B. Agnihotri, special thanks to Priyangshhi Biswas, IITB

# ----------------------------------------------------------------------------
# 1. SETUP & LIBRARIES
# ----------------------------------------------------------------------------

# Install necessary packages (uncomment if not already installed)
# install.packages(c("sf", "spdep", "tmap", "tidyverse", "ggplot2"))

# Load libraries
library(sf)
library(spdep)
library(tmap)
library(tidyverse)
library(ggplot2)

# ----------------------------------------------------------------------------
# 2. DATA INPUT
# ----------------------------------------------------------------------------

# Define file path
spatial_data_path <- "YOUR FILE PATH/NSS_jnj.shp"

# Read spatial data
shp <- st_read(spatial_data_path)

# Split data for Jhum and Non-Jhum cultivation
shp_jhum <- shp[!is.na(shp$jhum_pc_jh), ]
shp_crop <- shp[!is.na(shp$pc_crop_no), ]

# ----------------------------------------------------------------------------
# 3. SPATIAL WEIGHTS
# ----------------------------------------------------------------------------

# Define neighbor and weight parameters
queen_connectivity <- FALSE
snap_tolerance <- 0.0005
weight_style <- "W"
zero_policy_setting <- TRUE

# Create neighbors and weights for Jhum data
nb_jhum <- poly2nb(shp_jhum, queen = queen_connectivity, snap = snap_tolerance)
lw_jhum <- nb2listw(nb_jhum, style = weight_style, zero.policy = zero_policy_setting)

# Create neighbors and weights for Non-Jhum data
nb_crop <- poly2nb(shp_crop, queen = queen_connectivity, snap = snap_tolerance)
lw_crop <- nb2listw(nb_crop, style = weight_style, zero.policy = zero_policy_setting)

# ----------------------------------------------------------------------------
# 4. LOCAL MORAN'S I CALCULATION
# ----------------------------------------------------------------------------

# Calculate Local Moran's I for Jhum cultivation
lisa_jhum <- localmoran(shp_jhum$jhum_pc_jh, lw_jhum, 
                        zero.policy = zero_policy_setting)
shp_jhum <- shp_jhum %>%
  mutate(
    Ii_jhum = lisa_jhum[, 1],
    Z.Ii_jhum = lisa_jhum[, 4],
    P.Ii_jhum = lisa_jhum[, 5]
  )

# Calculate Local Moran's I for Non-Jhum cultivation
lisa_crop <- localmoran(shp_crop$pc_crop_no, lw_crop, 
                        zero.policy = zero_policy_setting)
shp_crop <- shp_crop %>%
  mutate(
    Ii_crop = lisa_crop[, 1],
    Z.Ii_crop = lisa_crop[, 4],
    P.Ii_crop = lisa_crop[, 5]
  )

# ----------------------------------------------------------------------------
# 5. LISA CLUSTER CATEGORIZATION
# ----------------------------------------------------------------------------

# Standardize variables
z_jhum <- scale(shp_jhum$jhum_pc_jh)[, 1]
z_crop <- scale(shp_crop$pc_crop_no)[, 1]

# Define cluster assignment function
assign_lisa_clusters <- function(z_score, moran_z, p_value) {
  cluster <- NA_character_
  cluster[z_score > 0 & moran_z > 0 & p_value <= 0.05] <- "High-High"
  cluster[z_score < 0 & moran_z > 0 & p_value <= 0.05] <- "Low-Low"
  cluster[z_score > 0 & moran_z < 0 & p_value <= 0.05] <- "High-Low"
  cluster[z_score < 0 & moran_z < 0 & p_value <= 0.05] <- "Low-High"
  cluster[is.na(cluster)] <- "Not Significant"
  return(cluster)
}

# Assign LISA clusters for Jhum
shp_jhum <- shp_jhum %>%
  mutate(cluster_jhum = assign_lisa_clusters(z_jhum, Z.Ii_jhum, P.Ii_jhum))

# Assign LISA clusters for Non-Jhum
shp_crop <- shp_crop %>%
  mutate(cluster_crop = assign_lisa_clusters(z_crop, Z.Ii_crop, P.Ii_crop))

# ----------------------------------------------------------------------------
# 6. COLOR PALETTES
# ----------------------------------------------------------------------------

# Define color palette for LISA clusters
lisa_cluster_palette <- c(
  "High-High" = "#b2182b",
  "Low-Low" = "#2166ac",
  "High-Low" = "#ef8a62",
  "Low-High" = "#67a9cf",
  "Not Significant" = "lightgrey"
)

# Define color palette for p-value significance
p_value_palette_jhum <- c(
  "p < 0.01" = "#67001f",
  "p < 0.05" = "#b2182b",
  "p < 0.10" = "#ef8a62",
  "Not Significant" = "grey80"
)

p_value_palette_crop <- c(
  "p < 0.01" = "#053061",
  "p < 0.05" = "#2166ac",
  "p < 0.10" = "#67a9cf",
  "Not Significant" = "grey80"
)

# ----------------------------------------------------------------------------
# 7. CLUSTER MAP VISUALIZATION
# ----------------------------------------------------------------------------

tmap_mode("plot")

# Jhum LISA Cluster Map

print(
  jhum_map <- tm_shape(shp_jhum) +
    tm_fill("cluster_jhum", 
            palette = lisa_cluster_palette, title = "Jhum Cluster Type") +
    tm_borders() +
    tm_layout(title = "LISA Clusters of\nJhum Cultivation\n(NSS 77th Round, 2018)",
              title.position = c(0.45, 0.93), 
              legend.position = c(0.50, 0.25))
)
tmap_save(jhum_map, "YOUR FILE PATH/Jhum_LISA_Cluster.png", width = 8, height = 6, dpi = 600)

# Non-Jhum (Crop) LISA Cluster Map
print(
  crop_map <- tm_shape(shp_crop) +
    tm_fill("cluster_crop", 
            palette = lisa_cluster_palette, 
            title = "Crop Cluster Type") +
    tm_borders() +
    tm_layout(title = "LISA Clusters of\nNon-Jhum Cultivation\n(NSS 77th Round, 2018)",
              title.position = c(0.45, 0.93), 
              legend.position = c(0.5, 0.25))
)
tmap_save(crop_map, "YOUR FILE PATH/Crop_LISA_Cluster.png", width = 8, height = 6, dpi = 600)

# ----------------------------------------------------------------------------
# 8. P-VALUE SIGNIFICANCE MAP VISUALIZATION
# ----------------------------------------------------------------------------

# Jhum LISA p-value Map
shp_jhum <- shp_jhum %>%
  mutate(
    pval_class_jhum = cut(
      P.Ii_jhum,
      breaks = c(0, 0.01, 0.05, 0.1, 1),
      labels = c("p < 0.01", "p < 0.05", "p < 0.10", "Not Significant"),
      right = FALSE # Include left endpoint
    )
  )


print(
  jhum_pval_map <- tm_shape(shp_jhum) +
    tm_fill("pval_class_jhum", 
            palette = p_value_palette_jhum, 
            title = "Jhum LISA p-values") +
    tm_borders() +
    tm_layout(title = "LISA Significance\n(Jhum Cultivation)", 
              title.position = c(0.45, 0.90), legend.position = c(0.50, 0.25))
)
tmap_save(jhum_pval_map, "YOUR FILE PATH/Jhum_PValue_Map.png", 
          width = 8, height = 6, dpi = 600)


# Non-Jhum (Crop) LISA p-value Map
shp_crop <- shp_crop %>%
  mutate(
    pval_class_crop = cut(
      P.Ii_crop,
      breaks = c(0, 0.01, 0.05, 0.1, 1),
      labels = c("p < 0.01", "p < 0.05", "p < 0.10", "Not Significant"),
      right = FALSE # Include left endpoint
    )
  )

print(
crop_pval_map <- tm_shape(shp_crop) +
  tm_fill("pval_class_crop", 
          palette = p_value_palette_crop, 
          title = "Crop LISA p-values") +
  tm_borders() +
  tm_layout(title = "LISA Significance\n(Non-Jhum Crop)", 
            title.position = c(0.45, 0.90), 
            legend.position = c(0.50, 0.25))
)
tmap_save(crop_pval_map, "YOUR FILE PATH/Crop_PValue_Map.png", width = 8, height = 6, dpi = 600)

# ----------------------------------------------------------------------------
# 9. GLOBAL MORAN'S I AND SCATTER PLOTS
# ----------------------------------------------------------------------------

# --- Global Moran's I ---
moran_jhum_test <- moran.test(shp_jhum$jhum_pc_jh, lw_jhum, zero.policy = zero_policy_setting)
moran_crop_test <- moran.test(shp_crop$pc_crop_no, lw_crop, zero.policy = zero_policy_setting)

cat("Global Moran's I for Jhum Cultivation:\n")
print(moran_jhum_test)
cat("\nGlobal Moran's I for Non-Jhum Cultivation:\n")
print(moran_crop_test)

# --- Moran Scatter Plot: Jhum ---
lag_jhum <- lag.listw(lw_jhum, shp_jhum$jhum_pc_jh, zero.policy = zero_policy_setting)
scatter_jhum_data <- tibble(
  x = scale(shp_jhum$jhum_pc_jh)[, 1],
  y = scale(lag_jhum)[, 1]
)

print(
  p_moran_jhum <- ggplot(scatter_jhum_data, aes(x = x, y = y)) +
    geom_point(shape = 21, color = "#1f78b4", fill = "#a6cee3", size = 2, alpha = 0.6) +
    geom_smooth(method = "lm", se = FALSE, color = "darkred", linewidth = 1) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey60") +
    labs(
      title = "Moran Scatter Plot: Jhum Cultivation",
      x = "Standardized Jhum (%)",
      y = "Spatial Lag of Jhum (%)"
    ) +
    theme_bw(base_size = 14) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
    xlim(-1, 7) + ylim(-1, 7)   # Adjust X-Y axis limits 

)


ggsave("YOUR FILE PATH/Jhum_Moran_Scatter.png", 
       plot = p_moran_jhum, width = 6, height = 6, dpi = 600)

# --- Moran Scatter Plot: Non-Jhum ---

lag_crop <- lag.listw(lw_crop, shp_crop$pc_crop_no, zero.policy = zero_policy_setting)
scatter_crop_data <- tibble(
  x = scale(shp_crop$pc_crop_no)[, 1],
  y = scale(lag_crop)[, 1]
)

print(
  p_moran_crop <- ggplot(scatter_crop_data, aes(x = x, y = y)) +
    geom_point(shape = 21, color = "#33a02c",fill = "#a6cee3", size = 2, alpha = 0.8) +
    geom_smooth(method = "lm", se = FALSE, color = "darkred", linewidth = 1) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey60") +
    labs(
      title = "Moran Scatter Plot: Non-Jhum Crop",
      x = "Standardized Non-Jhum (%)",
      y = "Spatial Lag of Non-Jhum (%)"
    ) +
    theme_bw(base_size = 14) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"))+
    xlim(-3, 3) + ylim(-3, 3)   # Adjust X-Y axis limits
)

ggsave("YOUR FILE PATH/Crop_Moran_Scatter.png", 
       plot = p_moran_crop, width = 6, height = 6, dpi = 600)

