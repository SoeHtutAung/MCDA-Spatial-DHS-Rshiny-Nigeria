###########################
# Title: Spatial analysis
# Purpose: check spatial clustering
# Description: Using a default weighting for vulnerability score, we checked clustering. But the result was not presented during MSc project. Because the purpose of the MSc project is to be able to define parameters for vulnerability score, and thus there is no fixed vulnerability score at this step
###########################

# --- Load library and shape file ---
library(spdep) # for Moran's I calculations
library(tmap) # for viewing map
source("scripts/map_maker.R") # for map_maker function

# --- load datasets ----
adm3 <- st_read("data/shp/NGA_wards_dashboard/NGA_wards_dashboard_poly.shp")
# vul <- na.omit(adm3$vul)

# --- define neighbours and weights ---
# define which wards are neighbours to one another based on queen contiguity
neighbours <- poly2nb(adm3)
# update the neighbour pattern to a row standardised weights matrix.
weights_queen <- nb2listw(neighbours, style = "W", zero.policy = T) ## 9,308 wards, 6.06789 avg. neighbours

# --- Moran's I test ---
# Global Moran's I test for spatial autocorrelation
global <- moran.test(adm3$vul, listw = weights_queen, zero.policy = T) 
# Moran I statistic 0.7884665, p-value < 2.2e-16
# Hypothesis testing with Mote Carlo simulation
# Simulate the 9999 times
set.seed(123)
mc_moran <- moran.mc(adm3$vul, listw = weights_queen,
                     nsim = 9999, zero.policy = T)
# statistic = 0.78847, observed rank = 10000, p-value = 1e-04

# --- Plot a Moran's Scatter plot using scaled data ---
# scale the vulnerability parameter
adm3$scaled_score <- scale(adm3$vul)
# create a lag vector from the neighbour list and the scaled od values
adm3$lag_scaled_score <- lag.listw(weights_queen,adm3$scaled_score, zero.policy = T)
# plot the output
ggplot(data = adm3, aes(x = scaled_score, y = lag_scaled_score)) +
  geom_smooth(method = "lm", se = FALSE, colour = "#fd673a", linetype = "dashed") +
  geom_point(size = 2, alpha = 0.5, col = "blue") +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  xlab("Raw vulnerability score (scaled)") +
  ylab("Vulnerability score lag vector") +
  theme_bw()

# --- local Moran's I ---
# Calculate the local Moran statistic for each region using queen contiguity
local_moran <- localmoran(adm3$vul,
                          weights_queen, # our weights object
                          zero.policy = TRUE, alternative = "two.sided")
# Replace the column names to make it clearer
colnames(local_moran) <- c("local_I", "expected_I", "variance_I", "z_statistic", "p_value")
# Join our local moran outputs to the main data and select columns for analysis
local_map <- adm3 %>% cbind(., local_moran) %>% #
  dplyr::select(scaled_score, lag_scaled_score, p_value, geometry)

# --- Run the map_maker function ---
local_map <- map_maker(local_map)
my_palette <- c("red","yellow","green","grey") # build palette
# check number of clusters
table(local_map$cluster)
# create map
tmap_mode("view")
# plot the data to look for hot and cold spot clusters based on p_values
tm_shape(local_map) +
  tm_polygons(col = "cluster",
              title = "Local Moran of vulnerability",
              style = "fixed",
              palette = my_palette,
              lwd = 0.2,
              border.col = 1)
