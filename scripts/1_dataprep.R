###########################
# Title: Data preparation
# Purpose: Install and load libraries. Extract and load datasets.
###########################

# install and load libraries

# --- extract and load datasets ---
# refer to 'docs/methodology.md' to identify data sources. Assume that all datasets are already saved in 'data' folder.
# # load spatial files 
adm0 <- load_shapefile(country = "Nigeria", admin_level = 0) # adm0 boundary using PATHtools
pop_r <- rast("data/raster/NGA_population_v2_1_gridded.tif") # population SpatRaster
adm3_v <- vect("data/shp/GRID3_NGA_wards/Nigeria_-_Ward_Boundaries.shp") # ward boundary SpatVector
mal_inc <- rast("data/raster/202406_Global_Pf_Incidence_Rate_NGA_2022.tiff") # 5x5km incidence surface from MAP
mal_mor <- rast("data/raster/202406_Global_Pf_Mortality_Rate_NGA_2022.tiff") # 5x5km mortality surface from MAP
mal_itnaccess <- rast("data/raster/202406_Africa_Insecticide_Treated_Net_Access_NGA_2022.tiff") # 5x5km ITN access surface from MAP
mal_itnuse <- rast("data/raster/202406_Africa_Insecticide_Treated_Net_Use_Rate_NGA_2022.tiff") # 5x5km ITN use surface from MAP
mal_pfrate <- rast("data/raster/202406_Global_Pf_Parasite_Rate_NGA_2022.tiff") # 5x5km Pf rate surface from MAP
mal_irs <- rast("data/raster/202406_Africa_IRS_Coverage_NGA_2022.tiff") # 5x5km IRS surface from MAP
mal_amt <- rast("data/raster/202406_Global_Antimalarial_Effective_Treatment_NGA_2022.tiff") # 5x5km ACT surface from MAP
fact_grid3 <- st_read("data/shp/GRID3_NGA_facilities/GRID3_NGA_-_Health_Facilities_.shp") # health facilities locations from GRID3
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
# remove 102 invalid polygons from adm3 shape file
adm3_v <- adm3_v[adm3_v$status != "Invalid", ] # 9,308 valid wards
# remove layer with NA values from raster layers
mal_inc <- mal_inc[[1]] # keep first layer only as only NA values in second layer
mal_mor <- mal_mor[[1]] # keep first layer only as only NA values in second layer
mal_pfrate <- mal_pfrate[[1]] # keep first layer only as only NA values in second layer
# check population raster file 'pop_r' 100x100m
global(pop_r, "sum",na.rm=T) # total population 216,678,334
sum(is.na(values(pop_r))) # 159237427 cells with NA value
range(pop_r) # 11546 x 14413, min 0.159194 max 2641.578125
# check travel time raster file 'tt_r' 1x1km
global(tt_r, "mean",na.rm=T) # mean travel time 110.4524
sum(is.na(values(tt_r))) # 576512 cells with NA value
range(tt_r) # 1154x1438, min 0.00 max 1035.739

# --- data transformation ---
# # simplify sptial files for efficiency
# remove unnecessary columns
adm3_v1 <- adm3_v [,c("FID","statecode","lgacode","wardcode","wardname","urban")]
# reduce resolution of raster to 1x1km
pop_r_1km <- aggregate(pop_r,fact = c(10,10), fun = sum_no_na,# from 100x100m to 1x1km, combined
                       filename = "outputs/plots/pop_sur_1km.tif") # export
# # rescaling raster files
# aligning ward boundaries and two raster files (pop and tt) by modifying extents of population raster to match extent of ward boundaries 
ext(pop_r_1km) <- ext(adm3_v1) # resample to match with raster file
# resample travel time raster to match extent and resolution of population raster
tt_r <- resample(tt_r, pop_r_1km, method = "bilinear") # default is bilinear for continuous tt raster
mal_inc <- resample(mal_inc, pop_r_1km, method = "bilinear") # resample to match with vector file
mal_mor <- resample(mal_mor, pop_r_1km, method = "bilinear") # resample to match with vector file
mal_pfrate <- resample(mal_pfrate, pop_r_1km, method = "bilinear") # resample to match with vector file
mal_itnaccess <- resample(mal_itnaccess, pop_r_1km, method = "bilinear") # resample to match with vector file
mal_itnuse <- resample(mal_itnuse, pop_r_1km, method = "bilinear") # resample to match with vector file
mal_irs <- resample(mal_irs, pop_r_1km, method = "bilinear") # resample to match with vector file
mal_amt <- resample(mal_amt, pop_r_1km, method = "bilinear") # resample to match with vector file

#################################################### 20250319 #########################################################

## saved aligned files
writeRaster(tt_r, "outputs/plots/tt_sur_final.tif",overwrite=TRUE) 
writeRaster(pop_r_1km, "outputs/plots/pop_sur_final.tif",overwrite=TRUE)
writeRaster(mal_inc, "outputs/plots/mal_inc_final.tif",overwrite=TRUE)
writeRaster(mal_mor, "outputs/plots/mal_mor_final.tif",overwrite=TRUE)
writeRaster(mal_pfrate, "outputs/plots/mal_pfrate_final.tif",overwrite=TRUE)
writeRaster(mal_itnaccess, "outputs/plots/mal_itnaccess_final.tif",overwrite=TRUE)
writeRaster(mal_itnuse, "outputs/plots/mal_itnuse_final.tif",overwrite=TRUE)
writeRaster(mal_irs, "outputs/plots/mal_irs_final.tif",overwrite=TRUE)
writeRaster(mal_amt, "outputs/plots/mal_amt_final.tif",overwrite=TRUE)

# Extract basic demographic and travel time data and append to ward boundary vector file
## ward level population data extraction from raster 
# ward_pop_rv <- terra::extract(pop_r_1km, adm3_v1, fun = "sum", exact = TRUE) # total pop 216,025,592
ward_pop <- exact_extract(pop_r_1km, st_as_sf(adm3_v1), "sum") # total pop 216,025,594 *** more efficient

write.csv(ward_pop,"outputs/data-output/wardlevel_pop.csv") 
#adm3_v1$population <- ward_pop_rv$NGA_population_v2_1_gridded # insert the population column in vector file
adm3_v1$population <- ward_pop # insert the population column in vector file

## save as shape file for final ward level dataset
writeVector (adm3_v1, "data/shp/NGA_wards/NGA_wards.shp",filetype = "ESRI Shapefile",
             overwrite = TRUE)

# Create a travel time surface raster file with 4 travel time categories
tt_r_threshold <- c(0, 30, 60, 110, max(values(tt_r$HF_accessibility), na.rm = TRUE))
tt_r_cat <- classify(tt_r, tt_r_threshold, include.lowest = TRUE)
writeRaster(tt_r_cat, "outputs/plots/tt_cat.tif", overwrite = TRUE)

# Create a travel time surface raster file with 2 travel time categories
tt_r_threshold <- c(0, 60, max(values(tt_r$HF_accessibility), na.rm = TRUE))
tt_r_cat <- classify(tt_r, tt_r_threshold, include.lowest = TRUE)
writeRaster(tt_r_cat, "outputs/plots/tt_cat_60.tif", overwrite = TRUE)
