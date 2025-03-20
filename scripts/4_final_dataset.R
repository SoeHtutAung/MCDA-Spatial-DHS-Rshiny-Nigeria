###########################
# Title: Final dataset for dashboard
# Purpose: Prepare final dataset to be loaded into dashboard
# Description: 
###########################

# --- load datasets ---
# # shape files
adm3 <- st_read("data/shp/NGA_wards/NGA_wards.shp",stringsAsFactors = F) # ward-level boundaries
adm3_point <- st_read("data/shp/NGA_wards_points/NGA_wards_points.shp",stringsAsFactors = F) # ward-level points
adm1 <- adm1 <- st_read("data/shp/NGA_states/NGA_states.shp",stringsAsFactors = F) # state-level boundaries

# --- data manipulation ---
# # improve efficiency
adm3 <- ms_simplify(adm3, keep = 0.50) # reduce the number of vertices in the shapefile

# --- calculate vulnerability scores ---
# # disease burden scores
# incidence
adm3$inc_score <- cut(adm3$inc,
                     breaks = quantile(adm3$inc, probs = seq(0, 1, by = 0.25), na.rm = TRUE),
                     include.lowest = TRUE, labels = FALSE)
# mortality
adm3$mor_score <- cut(adm3$mor,
                     breaks = quantile(adm3$mor, probs = seq(0, 1, by = 0.25), na.rm = TRUE),
                     include.lowest = TRUE, labels = FALSE)
# parasite prevalence rate
adm3$pfrate_score <- cut(adm3$pfrate,
                     breaks = quantile(adm3$pfrate, probs = seq(0, 1, by = 0.25), na.rm = TRUE),
                     include.lowest = TRUE, labels = FALSE)

# # intervention coverage
# access to ITN
adm3$itnaccess_score <- cut(adm3$itnaccess,
                     breaks = quantile(adm3$itnaccess, probs = seq(0, 1, by = 0.25), na.rm = TRUE),
                     include.lowest = TRUE, labels = FALSE)
adm3$itnaccess_score <- 5 - adm3$itnaccess_score # make it reverse
# use of ITN
adm3$itnuse_score <- cut(adm3$itnuse,
                     breaks = quantile(adm3$itnuse, probs = seq(0, 1, by = 0.25), na.rm = TRUE),
                     include.lowest = TRUE, labels = FALSE)
adm3$itnuse_score <- 5 - adm3$itnuse_score # make it reverse
# score for IRS is not done due to only zero value
# score for antimalarial treatment is not done due to only one value across the entire country

# # accessibility to PHC facilities
# population beyond 60 minutes
adm3$pop_score <- cut(adm3$percent,
                      breaks = seq(0, 1, by = 0.25),
                      include.lowest = TRUE, labels = FALSE)
## travel time *** not used. Instead, we used mean travel time
adm3$tt_score <- cut(adm3$weightedtt,
                     breaks = c(0, 30, 60, 110, max(adm3$weightedtt,na.rm = T)),
                     include.lowest = TRUE, labels = FALSE)

# --- add new variables ---
adm3_final <- adm3 %>% mutate(
  traveltime = round(weightedtt,1),
  # calculate rate for incidence and mortality
  inc_rate = round(inc*1000),
  mor_rate = round(mor*100000,2),
  # calculate population
  population = round(population,0),
  pop_above60 = round(population * percent,0),
  # calculate CHW needed per population
  chw = ceiling(popultn/1000), # based on total population
  chw_above60 = ceiling(pop_above60/1000), # based on population above 60 minutes
  chw_ward = if_else(population > 0, 10, 0), # 10 CHW per ward if there is any population
  chw_final = 0, # to be filled later
  vul_final = 0, # to be filled later
  ) %>% 
  # join state level indicators
  left_join(as.data.frame(adm1) %>% dplyr::select(statecode,itn,malaria,fever,seek), by = "statecode") %>%
  st_as_sf()

# --- prepare final datasets for dashboard ---
# merge information with ward-level point file
adm3_point <- st_join(adm3_point, adm3_final) %>% dplyr::select(-wardcode.y) %>% rename(wardcode = wardcode.x)
adm3_poly <- adm3_final
# save as shape file
st_write(adm3_poly, "data/shp/NGA_wards_dashboard/NGA_wards_dashboard_poly.shp")
st_write(adm3_point, "data/shp/NGA_wards_dashboard/NGA_wards_dashboard_point.shp")
