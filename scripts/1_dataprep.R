###########################
# Title: Data preparation
# Purpose: ETL process to produce final raster and vector datasets for analysis.
###########################

# --- extract and load datasets ---
# refer to 'docs/methodology.md' to identify data sources. Assume that all raw datasets are already saved in 'data' folder.
# # load spatial files 
adm0 <- load_shapefile(country = "Nigeria", admin_level = 0) # adm0 boundary using PATHtools
adm3_v <- vect("data/shp/GRID3_NGA_wards/Nigeria_-_Ward_Boundaries.shp") # ward boundary SpatVector
pop_r <- rast("data/raster/NGA_population_v2_1_gridded.tif") # population SpatRaster
fact_grid3 <- st_read("data/shp/GRID3_NGA_facilities/GRID3_NGA_-_Health_Facilities_.shp") # health facilities locations from GRID3
# batch load modelled surfaces from MAP, 5x5km
map_raster_files <- list.files("data/raster", pattern = "202406_.*_NGA_2022.tiff$", full.names = TRUE) # define file paths
map_rasters <- rast(map_raster_files) # load all surfaces
names(map_rasters) <- c("mal_inc", "mal_mor", "mal_itnaccess", "mal_itnuse", "mal_pfrate", "mal_irs", "mal_amt")
raster_list <- setNames(as.list(mal_rasters), raster_names) # create name list
list2env(raster_list, envir = .GlobalEnv) # assign each raster to the global environment
# get friction surface function from PATHtools package is used
get_friction_surface(shp = adm0, 
                     transport_type = "walking",
                     export = TRUE,
                     output_dir = "outputs/plots/fri_sur.tif")
fri_sur <- raster("outputs/plots/fri_sur.tif") # load friction surface

# # create a csv file of health facilities
# clean phc_point files
phc_point <- fact_grid3 %>% 
  filter (category == "Primary Health Center", func_stats == "Functional") %>% # functional PHC only
  dplyr::select(FID,longitude,latitude) %>%
  rename(LABEL = FID, X = longitude, Y = latitude) 
# # write phc_point as csv
phc_final <- fact_grid3 %>% 
  filter (category == "Primary Health Center", func_stats == "Functional")
  st_write(phc_final, "data/shp/NGA_facilities/NGA_facilities.shp") # export

# # create travel time surface
# friction surface and csv file of health facilities are used
create_travel_surface(friction_surface = "outputs/plots/fri_sur.tif",
                      points = phc_point, # sf dataframe
                      extent_file = fri_sur,
                      id_col = "LABEL", x_col = "X", y_col = "Y",
                      output_dir = "outputs/plots/") # output is saved in 'outputs' folder
tt_r <- rast("outputs/plots/HF_accessibility.tif") # load travel surface as SpatRaster

# --- data cleaning ---
# # check NA
# check population raster file 'pop_r' 100x100m
global(pop_r, "sum",na.rm=T) # total population 216,678,334
freq(pop_r, value = NA); minmax(pop_r) # 159237427 cells with NA value, min 0.159194 max 2641.578125
# check travel time raster file 'tt_r' 1x1km
global(tt_r, "mean",na.rm=T) # mean travel time 110.4524
sum(is.na(values(tt_r))) # 576512 cells with NA value
range(tt_r) # 1154x1438, min 0.00 max 1035.739

# # remove NA
# remove 102 invalid polygons from adm3 shape file
adm3_v <- adm3_v[adm3_v$status != "Invalid", ] # 9,308 valid wards
# remove layer with NA values from raster layers
mal_inc <- mal_inc[[1]] # keep first layer only as only NA values in second layer
mal_mor <- mal_mor[[1]] # keep first layer only as only NA values in second layer
mal_pfrate <- mal_pfrate[[1]] # keep first layer only as only NA values in second layer

# --- data transformation ---
# # simplify sptial files for efficiency
# remove unnecessary columns
adm3_v1 <- adm3_v [,c("FID","statecode","lgacode","wardcode","wardname","urban")]
# reduce resolution of raster to 1x1km
pop_r_1km <- aggregate(pop_r,fact = c(10,10), fun = sum_no_na,# from 100x100m to 1x1km, combined
                       filename = "outputs/plots/pop_sur_1km.tif") # export

# # rescaling and resampling raster files
# aligning ward boundaries and two raster files (pop and tt) by modifying extents of population raster to match extent of ward boundaries 
ext(pop_r_1km) <- ext(adm3_v1) # resample to match with raster file
# batch resampling surfaces to match extent and resolution of population raster, 1x1km
rasters_to_resample <- list(tt_r, mal_inc, mal_mor, mal_pfrate, mal_itnaccess, mal_itnuse, mal_irs, mal_amt) # list of rasters to resample
# resample each raster to match population raster using lapply and bilinear interpolation
resampled_rasters <- lapply(rasters_to_resample, function(raster) {
  resample(raster, pop_r_1km, method = "bilinear")
})
names(resampled_rasters) <- c("tt_r", "mal_inc", "mal_mor", "mal_pfrate", "mal_itnaccess", "mal_itnuse", "mal_irs", "mal_amt") # assign names to list
list2env(resampled_rasters, envir = .GlobalEnv) # assign each raster to the global environment

# --- saved output raster files ---
# Ready-to-analyze raster datasets are saved in 'output' folder for further analysis.  
rasters_to_save <- list(tt_r, pop_r_1km, mal_inc, mal_mor, mal_pfrate, mal_itnaccess, mal_itnuse, mal_irs, mal_amt) # list of rasters to save
# corresponding file names
output_files <- c("tt_sur_final.tif", "pop_sur_final.tif", "mal_inc_final.tif", 
                  "mal_mor_final.tif", "mal_pfrate_final.tif", "mal_itnaccess_final.tif", 
                  "mal_itnuse_final.tif", "mal_irs_final.tif", "mal_amt_final.tif")
# save each raster with corresponding filename using lapply
lapply(seq_along(rasters_to_save), function(i) {
  writeRaster(rasters_to_save[[i]], file.path("outputs/plots", output_files[i]), overwrite = TRUE)
})

# --- append population data to ward boundary (cleaned - v1) vector file  ---
# ward-level population data extraction from raster using exactextractr package
ward_pop <- exact_extract(pop_r_1km, st_as_sf(adm3_v1), "sum") # total pop 216,025,594 
adm3_v1$population <- ward_pop # append population column in vector file
# save as shape file for final ward level dataset
writeVector (adm3_v1, "data/shp/NGA_wards/NGA_wards.shp",filetype = "ESRI Shapefile",
             overwrite = TRUE)

# --- save other neccessary files ---
write.csv(ward_pop,"outputs/data-output/wardlevel_pop.csv") # csv file for ward-level population data
# # create a travel time surface raster file with 4 travel time categories
tt_r_threshold <- c(0, 30, 60, 110, max(values(tt_r$HF_accessibility), na.rm = TRUE))
tt_r_cat <- classify(tt_r, tt_r_threshold, include.lowest = TRUE)
writeRaster(tt_r_cat, "outputs/plots/tt_cat.tif", overwrite = TRUE)
# # create a travel time surface raster file with 2 travel time categories
tt_r_threshold <- c(0, 60, max(values(tt_r$HF_accessibility), na.rm = TRUE))
tt_r_cat <- classify(tt_r, tt_r_threshold, include.lowest = TRUE)
writeRaster(tt_r_cat, "outputs/plots/tt_cat_60.tif", overwrite = TRUE)
