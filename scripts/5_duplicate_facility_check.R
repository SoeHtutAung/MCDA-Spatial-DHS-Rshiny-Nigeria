###########################
# Title: Duplicate check
# Purpose: To check duplicates of health facilities and deduplicate if possible
# Description: We checked possible duplicates and found 648 (3%) out of 21,329 PHC facilities locate within 100 meters from the nearest PHC facilities. There was no duplicate name wihtin 250 m. We could not do deduplicaiton because there was no field team to verify.
###########################

# --- Load library and shape file ---
# # load libraries
library(sf)
library(dplyr)
library(ggplot2)
library(fedmatch) # for fuzzy matching
library(geosphere)
library(data.table)
library(scales) # for label modification
library(stringdist)

# # load dataset
phc <- st_read("data/shp/NGA_facilities/NGA_facilities.shp")

# --- calculate distance between PHC facilities locations ---
# # calculate distance between nearest points
# get coordinates from the sf object
coords <- st_coordinates(phc)
# compute distance matrix using geosphere
dist_matrix <- geosphere::distm(coords, fun = distHaversine) 
# saveRDS(dist_matrix, "C:/Users/drsoe/Downloads/New folder/dist_matrix_facilities.rds")
# extract nearest distance for each point
diag(dist_matrix) <- NA # remove diagonal 
nearest_distances <- apply(dist_matrix, 1, min, na.rm = TRUE)
nearest_distances_dt <- data.table(nearest_distance = nearest_distances)
# append to the original dataset
phc <- cbind(phc, nearest_distances_dt)
# st_write(phc,"data/shp/NGA_facilities/NGA_facilities.shp")

# # extract points closer than 100m
threshold <- 100 # 100 meters
# pairs of points closer than the threshold
close_pairs <- which(dist_matrix < threshold, arr.ind = TRUE)
close_pairs %>% as.data.frame() %>% pull(row) %>% unique() #648 
# saveRDS(close_pairs, "outputs/data-output/close_pairs_facilities.rds")
## simple calculation from nearest distance dt
# nearest_distances_dt[nearest_distance < threshold] # 648

# --- plot distances ---
# get values
mean_distance <- mean(phc$nrst_ds / 1000, na.rm = TRUE)
iqr_values <- quantile(phc$nrst_ds / 1000, probs = c(0.25, 0.75), na.rm = TRUE)
iqr_range <- iqr_values[2] - iqr_values[1]
# plot
phc %>% mutate(nrst_ds = nrst_ds / 1000) %>%
  ggplot(aes(nrst_ds)) +
  geom_histogram(bins = 100) +
  # mean and IQR
  geom_vline(xintercept = mean_distance, color = "red", linetype = "solid", size = 0.5) + 
  geom_vline(xintercept = iqr_values[1], color = "black", linetype = "dotted", size = 0.25) +
  geom_vline(xintercept = iqr_values[2], color = "black", linetype = "dotted", size = 0.25) +
  annotate("text", x = mean_distance, y = Inf, label = paste("Average distance =", round(mean_distance, 2), "Km"),
           color = "Black", hjust = -0.1, vjust = 1.5, size = 3.5) +
  # label
  labs(# title = "Distance to nearest PHC facility",
       x = "Distance (Kilometers)", y = "Number of facilities") +
  theme_classic() +
  theme(
    plot.title = element_text(size = 11),  # Title font size
    axis.title.x = element_text(size = 10),  # X-axis title font size
    axis.title.y = element_text(size = 10)) +   # Y-axis title font size
  scale_y_continuous(labels = label_comma())  # Format y-axis labels with commas
 # plot
 phc %>%
   plot_ly(x = ~statenm, y = ~nrst_ds,
          type = 'box', boxpoints = 'all',
          jitter = 0.3, pointpos = 0, marker = list(size = 3),
          hoverinfo = 'text', text = ~paste('Name of PHC: ', prmry_n,
                                            '<br>Distance: ', nrst_ds),
          line = list(width = 1)
   )
#    (~ownrshp) %>%
#   layout(
#     xaxis = list(title = "", tickangle = -45), # Change x-axis label direction
#     yaxis = list(title = "Distance to nearest other PHC facility"),
#     margin = list(b = 0) # Adjust margins
#   )

# --- check possible duplicates for facilities within 250m apart from each other ---
# # fuzzy matching for names of PHC 
# filter for close pairs
phc_250 <- phc %>% filter (nrst_ds < 250)
# clean names and append with wardname
phc_250$cleaned_name <- paste(clean_strings(phc_250$wardnam), sep = "_", clean_strings(phc_250$prmry_n))
# match using stringdist
matches <- stringdist::stringdistmatrix (phc_250$cleaned_name, phc_250$cleaned_name, method = "jw")
# check how many names matche by more than 75%
which(matches >0.75, arr.ind = TRUE) # 0
# If 50% is used,
percent50 <- which(matches > 0.5, arr.ind = TRUE) %>% as.data.frame()
n_distinct(percent50$row) # 2,479 matches as 'health' 'center' 'primary' suffixes are overlapped
