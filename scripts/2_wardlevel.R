###########################
# Title: Ward-level shape file
# Purpose: Extract and append ward-level population, malaria indicators, travel time and conflict data to admin 3 shape file. Create a csv file of ward-level population at different travel time categories
# Description: We have made ready-to-analyze spatial datasets using '1_dataprep.R'. Now we will load those datasets and analyze. 
###########################

# --- load datasets ---
# # administrative boundaries
# state-level
adm1 <- st_read("data/shp/GRID3_NGA_states/NGA_states.shp",stringsAsFactors = F)
# lga-level
adm2 <- st_read("data/shp/GRID3_NGA_LGA/GRID3_NGA_LGA.shp",stringsAsFactors = F)
# # ward-level
adm3 <- st_read("data/shp/NGA_wards/NGA_wards.shp",stringsAsFactors = F) # boundaries
adm3_point <- vect("data/shp/NGA_wards_points/NGA_wards_points.shp") # points

# # population and malaria information
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

# # conflict information
# load geo-referenced csv file for conflict locations
conflict <- read.csv("data/acled_points.csv", as.is=T)

# --- extract and append population data, travel time and disease burden data to ward boundary vector file  ---
# # ward-level total population is extracted from raster using exactextractr package
ward_pop <- exact_extract(pop, adm3, "sum") # total pop 216,025,594 
adm3$population <- ward_pop # append population column in vector file
# save as csv file as well
write.csv(ward_pop,"outputs/data-output/wardlevel_pop.csv") # csv file for ward-level population data

# # weighted mean travel time at each ward using exactextractr package and update the shape file
adm3$weightedtt <- exact_extract(tt, adm3, "weighted_mean", weights = pop) # weighted mean travel time

# # weighted mean incidence and mortality at each ward using exactextractr package and update the shape file
adm3$inc <- exact_extract(inc, adm3, "weighted_mean", weights = pop) # weighted mean incidence
adm3$mor <- exact_extract(mor, adm3, "weighted_mean", weights = pop) # weighted mean mortality
adm3$pfrate <- exact_extract(pfrate, adm3, "weighted_mean", weights = pop) # weighted mean parasite prevalence rate
adm3$itnaccess <- exact_extract(itnaccess, adm3, "weighted_mean", weights = pop) # weighted mean of access to ITN
adm3$itnuse <- exact_extract(itnuse, adm3, "weighted_mean", weights = pop) # weighted mean of use of ITN
adm3$irs <- exact_extract(irs, adm3, "weighted_mean", weights = pop) # weighted mean of IRS
adm3$amt <- exact_extract(amt, adm3, "weighted_mean", weights = pop) # weighted mean of ACTs

# --- replace missing value with average of respective lga ---
adm3 <- adm3 %>% group_by(lgacode) %>% mutate(
  weightedtt = ifelse(is.na(weightedtt), mean(weightedtt, na.rm = TRUE), weightedtt), # traveltime
  inc = ifelse(is.na(inc), mean(inc, na.rm = TRUE), inc), # incidence
  mor = ifelse(is.na(mor), mean(mor, na.rm = TRUE), mor), # mortality
  pfrate = ifelse(is.na(pfrate), mean(pfrate, na.rm = TRUE), pfrate), # pf parasite rate
  itnaccess = ifelse(is.na(itnaccess), mean(itnaccess, na.rm = TRUE), itnaccess), # itn access rate
  itnuse = ifelse(is.na(itnuse), mean(itnuse, na.rm = TRUE), itnuse), # itn use rate
  irs = ifelse(is.na(irs), mean(irs, na.rm = TRUE), irs), # irs
  amt = ifelse(is.na(amt), mean(amt, na.rm = TRUE), amt), # antimalarial treatment
) %>%
  ungroup()

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
  st_transform(st_crs(adm3_point)) %>%  # align CRS with adm3_point
  st_join(adm3_point, left = TRUE) %>%  # perform spatial join
  select(names(adm3), tt_abj) %>%       # retain original columns and add tt_abj

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
below30 <- exact_extract(pop_below30, adm3, "sum")
tt_30_60 <- exact_extract(pop_30_60, adm3, "sum")
tt_60_110 <- exact_extract(pop_60_110, adm3, "sum")
tt_above110 <- exact_extract(pop_above110, adm3, "sum")
# ward-level population in different travel time categories (2 categories) is extracted
above60 <- exact_extract(pop_60above, adm3, "sum")
below60 <- exact_extract(pop_below60, adm3, "sum")

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

# # write to csv
write.csv(wardlevel_tt, "outputs/data-output/wardlevel_tt.csv", row.names = FALSE)

# # append proportion of population beyond 60 minutes to ward-level shape file
# create column for percentage of population beyond 60 minutes travel time and add to shape file
wardlevel_tt$percent <- with(wardlevel_tt, ifelse(total == 0, 0, tt_above60 / total)) # avoid NA values
# include percent column and enter value for each ward
adm3$percent <- wardlevel_tt$percent[match(adm3$wardcode, wardlevel$ID)]

# --- conflict index ---
# # extract selected columns from conflict dataframe and convert to sf object
conflict <- conflict %>% select (event_date, event_type, assoc_actor_2, admin1, admin2,
                                 fatalities, latitude, longitude ,geo_precision, population_best) %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

# # create ward-level conflict dataset
# join between conflict data point file and ward boundaries and summarize 3 major indicators at ward-level
join_conflict <- st_join(conflict, adm3) %>% # join with adm3 shape file
  group_by(wardcode) %>% summarize (events = n(), # 1 - 87, mean 4.228
                                    fatalities = sum(fatalities, na.rm = T), # 0 - 453, mean 11.06
                                    population = first(population)) %>%
  as.data.frame() %>% select(-geometry)
# calculate conflict index
conflict_final <-  join_conflict %>%
  mutate(event_rate = if_else(population == 0, 0, (events / population) * 1000) , # 0 - 1099.1883, mean 1.0841
         severity = if_else(events == 0, 0, fatalities / events), # 0 -53 mean 2.2324
         # log transformation for skewed data
         log_event = log(event_rate + 1), # +1 to avoid log(0) error, 0 - 7.00324, mean 0.23427
         log_severity = log(severity + 1), # +1 to avoid log(0) error, 0 - 3.9890, mean 0.8434
         # score calculation using ntile
         event_score = if_else (events == 0, 0, ntile(log_event, 4)),  # 1 - 4 as there was at least one event 
         severity_score = if_else (fatalities == 0, 0, ntile(log_severity, 4)),  # starts at 0 as not every event has fatality
         # calculate conflict index 
         conflict_index = round(event_score * 1 + severity_score * 1,0)) %>% # add weights
  select (wardcode, conflict_index)

# # save as csv file
write.csv(conflict_final, "outputs/data-output/wardlevel_conflict.csv", row.names = FALSE)

# # append conflict index to ward-level shape file
# create column for conflict index and add to shape file
adm3 <- adm3 %>%
  left_join(conflict_final, by = "wardcode") %>%  # left join with conflict dataset
  mutate(conflict_index = if_else(is.na(conflict_index), 0, conflict_index)) %>%       # assign NA values to 0

# --- append information from state- and lga-level datasets ---
adm3_poly <- adm3 %>% 
  # join state name and state level indicators from adm1
  left_join(as.data.frame(adm1) %>% dplyr::select(statecode,statename,geozone), by = "statecode") %>%
  mutate (zone = case_when(
    geozone == "SSZ" ~ "South South",
    geozone == "SEZ" ~ "South East",
    geozone == "SWZ" ~ "South West",
    geozone == "NEZ" ~ "North East",
    geozone == "NWZ" ~ "North West",
    geozone == "NCZ" ~ "North Central",
  )) %>% dplyr::select(-geozone) %>%
  # join lga name 
  left_join(as.data.frame(adm2) %>% dplyr::select (lgacode,lganame), by = "lgacode") %>%
  # safe as sf object
  st_as_sf() 

# --- save as shape file for final ward-level dataset with population, travel time and malaria information ---
# # polygon shape file
st_write(adm3_poly, "data/shp/NGA_wards/NGA_wards.shp", driver = "ESRI Shapefile")
# # save final ward level point file with driving time informaiton
st_write(adm3_point, "data/shp/NGA_wards_points/NGA_wards_points.shp", driver = "ESRI Shapefile")
