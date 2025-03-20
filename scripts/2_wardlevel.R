###########################
# Title: Ward-level shape file
# Purpose: Extract and append ward-level total population to admin 3 shape file. Create a csv file of ward-level population at different travel time categories
# Description: We have made ready-to-analyze spatial datasets using '1_dataprep.R'. Now we will load those datasets and analyze.
###########################

# --- load datasets ---
# ward-level boundaries
adm3 <- vect("data/shp/NGA_wards/NGA_wards.shp") # boundaries
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

# --- save as shape file for final ward level dataset with population, travel time and malaria information ---
writeVector (adm3, "data/shp/NGA_wards/NGA_wards.shp",filetype = "ESRI Shapefile",
             overwrite = TRUE)

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
