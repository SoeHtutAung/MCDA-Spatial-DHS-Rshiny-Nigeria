###########################
# Title: Ward-level shape file
# Purpose: Extract and append ward-level total population to admin 3 shape file. Create a csv file of ward-level population at different travel time categories
# Description: We have made ready-to-analyze spatial datasets using '1_dataprep.R'. Now we will load those datasets and analyze.
###########################

# --- load datasets ---
# ward-level boundaries
adm3 <- vect("data/shp/NGA_wards/NGA_wards.shp") # boundaries
adm3_point <- vect("data/shp/NGA_wards_points/NGA_wards_points.shp") # points
# define file paths in a named list for raster files
raster_files <- list(
  # population
  pop = "outputs/plots/pop_sur_final.tif",
  # travel time to PHCs
  tt = "outputs/plots/tt_sur_final.tif",
  tt_cat = "outputs/plots/tt_cat.tif",
  tt_cat_2 = "outputs/plots/tt_cat_60.tif",
  # malaria burden
  inc = "outputs/plots/mal_inc_final.tif",
  mor = "outputs/plots/mal_mor_final.tif",
  pfrate = "outputs/plots/mal_pfrate_final.tif",
  itnaccess = "outputs/plots/mal_itnaccess_final.tif",
  itnuse = "outputs/plots/mal_itnuse_final.tif",
  irs = "outputs/plots/mal_irs_final.tif",
  amt = "outputs/plots/mal_amt_final.tif"
)
# batch processing using terra package
rasters <- lapply(raster_files, rast)
# save individual object from the list into environment
list2env(rasters, envir = .GlobalEnv)

# --- extract and append population data, travel time and disease burden data to ward boundary vector file  ---
# # ward-level total population is extracted from raster using exactextractr package
ward_pop <- exact_extract(pop, st_as_sf(adm3), "sum") # total pop 216,025,594 
adm3$population <- ward_pop # append population column in vector file
# save as csv file as well
write.csv(ward_pop,"outputs/data-output/wardlevel_pop.csv") # csv file for ward-level population data

# # weighted mean travel time at each ward using exactextractr package and update the shape file
adm3$weightedtt <- exact_extract(tt, st_as_sf(adm3), "weighted_mean", weights = pop) # weighted mean travel time

# # weighted mean incidence and mortality at each ward using exactextractr package and update the shape file
adm3$inc <- exact_extract(inc, st_as_sf(adm3), "weighted_mean", weights = pop) # weighted mean incidence
adm3$mor <- exact_extract(mor, st_as_sf(adm3), "weighted_mean", weights = pop) # weighted mean mortality
adm3$pfrate <- exact_extract(pfrate, st_as_sf(adm3), "weighted_mean", weights = pop) # weighted mean parasite prevalence rate
adm3$itnaccess <- exact_extract(itnaccess, st_as_sf(adm3), "weighted_mean", weights = pop) # weighted mean of access to ITN
adm3$itnuse <- exact_extract(itnuse, st_as_sf(adm3), "weighted_mean", weights = pop) # weighted mean of use of ITN
adm3$irs <- exact_extract(irs, st_as_sf(adm3), "weighted_mean", weights = pop) # weighted mean of IRS
adm3$amt <- exact_extract(amt, st_as_sf(adm3), "weighted_mean", weights = pop) # weighted mean of ACTs

# --- travel time by driving to capital city (Abuja) by each ward ---
# According to request from NMCP, MOH, they would like to add criteria according to ***driving distance*** to Abuja
# # create a sf object (point) for Abuja
# get lat long for Abuja
abuja <- data.frame(LABEL = "abuja", X = 7.491302, Y = 9.072264) # https://www.latlong.net/place/abuja-nigeria-3321.html
# transform data frame as sf object
abuja <- st_as_sf(abuja, coords = c("X", "Y"), crs = 4326)
# extract coordinates and append as separate columns
abuja$X <- st_coordinates(abuja)[, 1]
abuja$Y <- st_coordinates(abuja)[, 2]

# # create travel time raster surface to Abuja
# when extracting motorized friction surface from MAP, there was a problem with PATHtools::get_friction_surface(). So, motorized friction surface was downloaded from MAP and saved 
fri_sur <- raster("data/raster/202001_Global_Motorized_Friction_Surface_NGA.tiff") # load friction surface
# create travel surface using PATHtools
create_travel_surface(friction_surface = "data/raster/202001_Global_Motorized_Friction_Surface_NGA.tiff",
                      points = abuja, # sf object with only one point, which is Abuja
                      extent_file = fri_sur,
                      id_col = "LABEL", x_col = "X", y_col = "Y",
                      output_dir = "outputs/plots/",
                      overwrite = TRUE) # same name as walk only friction surface, thus need to be cautious when processing
# load travel surface
tt_abj <- raster("outputs/plots/HF_accessibility.tif") # Note: name 'HF_accessibility' is given inside create_travel_surface function. This surface is travel surface to Abuja, not HF

# # extract and append travel time to Abuja in ward-level file 
# ensure that crs are the same between vector (points) and raster
adm3_point <- st_transform(adm3_point, crs(tt_abj))
# extract travel time values at the points
travel_times <- raster::extract(tt_abj, st_coordinates(adm3_point))
# add the extracted values as a new column to the ward level point file
adm3_point$tt_abuja <- round(travel_times/60,2) # convert minutes to hours
summary(adm3_point$tt_abuja) # mean 5.336 with 9 NAs

# add the extracted values as a new column,tt_abuja , to ward level polygon file
adm3 <- adm3 %>%
  st_as_sf() %>%                        # convert SpatVector to sf
  st_transform(st_crs(adm3_point)) %>%  # align CRS with adm3_point
  st_join(adm3_point, left = TRUE) %>%  # perform spatial join
  select(names(adm3), tt_abj) %>%       # retain original columns and add tt_abj
  vect()                                # convert adm3 polygon back to SpatVector

# --- save as shape file for final ward level dataset with population, travel time and malaria information ---
# # polygon shape file
writeVector (adm3, "data/shp/NGA_wards/NGA_wards.shp",filetype = "ESRI Shapefile",
             overwrite = TRUE)
# # save final ward level point file with driving time informaiton
st_write(adm3_point, "data/shp/NGA_wards_points/NGA_wards_points.shp", driver = "ESRI Shapefile")

# --- create population raster for tt categories ---
# # create population raster surfaces for travel time categories (total population 216,442,127)
# for different travel time categories (4 categories)
pop_below30 <- pop * (tt_cat == 0) # population for less than 30 minutes 130,212,929
pop_30_60 <- pop * (tt_cat == 1) # population for 30-60 minutes 43,126,068
pop_60_110 <- pop * (tt_cat == 2) # population for 60-110 minutes 25,283,649
pop_above110 <- pop * (tt_cat == 3) # population for above 110 minutes 17,819,481
# for different travel time categories (2 categories)
pop_60above <- pop * tt_cat_2 # population for 1 hour and above 43,103,130
pop_below60 <- pop * (tt_cat_2 == 0) # population for less than 1 hour 173,338,997

# # extract ward-level population for tt categories 
# ward-level population in different travel time categories (4 categories) is extracted
below30 <- exact_extract(pop_below30, st_as_sf(adm3), "sum")
tt_30_60 <- exact_extract(pop_30_60, st_as_sf(adm3), "sum")
tt_60_110 <- exact_extract(pop_60_110, st_as_sf(adm3), "sum")
tt_above110 <- exact_extract(pop_above110, st_as_sf(adm3), "sum")
# ward-level population in different travel time categories (2 categories) is extracted
above60 <- exact_extract(pop_60above, st_as_sf(adm3), "sum")
below60 <- exact_extract(pop_below60, st_as_sf(adm3), "sum")

# # save the ward-level population for different tt categories
# combine the vectors into ward-level data frame
wardlevel_tt <- data.frame(
  ID = as.character(adm3$wardcode),
  # two categories
  tt_above60 = above60,
  tt_below60 = below60,
  # four categories
  tt_below30 = below30,
  tt_30_60 = tt_30_60,
  tt_60_110 = tt_60_110,
  tt_above110 = tt_above110,
  total = above60 + below60, #215,982,062
  stringsAsFactors = FALSE
)
# write to csv
write.csv(wardlevel_tt, "outputs/data-output/wardlevel_tt.csv", row.names = FALSE)
