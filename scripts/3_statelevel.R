###########################
# Title: State-level shape file
# Purpose: Extract and append state-level indicators from DHS
# Description: DHS indicators are generated through MLmain.R script from DHSProgram/DHS-Indicators-R. Where DHS datasets are saved in 'data/dhs' in working computer and output tables are saved in "outputs/data-output/Tables_ML.xlsx".
###########################

# --- load datasets ---
# # state-level boundaries from GRID3
adm1 <- st_read("data/shp/GRID3_NGA_states/NGA_states.shp",stringsAsFactors = F)

# # DHS indicators
# extract and load desired indicator sheet from DHS output excel file
itn <- xlsx::read.xlsx("outputs/data-output/Tables_ML.xlsx", sheetName = "hh_ITN", header = TRUE)
malaria <- xlsx::read.xlsx("outputs/data-output/Tables_ML.xlsx", sheetName = "pr_micro_chld", header = TRUE)
fever <- xlsx::read.xlsx("outputs/data-output/Tables_ML.xlsx", sheetName = "kr_fever", header = TRUE)
seek <- xlsx::read.xlsx("outputs/data-output/Tables_ML.xlsx", sheetName = "kr_fev_day", header = TRUE)

# --- manipulate datasets ---
## create dataframe for state level malaria indicators
# ITN possession
itn <- itn %>% slice (4:40) %>% 
  mutate(region = sapply(strsplit(row_labels, "\\|"), function(x) x[3]),
         itn = (Household.owns.at.least.one.ITN.Yes)/100) %>%
  dplyr::select (region,itn)
# malaria prevalence (parasitemia among 6 mths to 5 yrs children)
malaria <- malaria %>% slice (7:43) %>% 
  mutate(region = sapply(strsplit(row_labels, "\\|"), function(x) x[3]),
         malaria = (Parasitemia..via.microscopy..in.children.6.59.months.1)/100) %>%
  dplyr::select (region,malaria)
# prevalence of fever
fever <- fever %>% slice (13:49) %>% 
  mutate(region = sapply(strsplit(row_labels, "\\|"), function(x) x[3]),
         fever = (Fever.symptoms.in.the.2.weeks.before.the.surveyvs.1)/100) %>%
  dplyr::select (region,fever)
# immediate treatment seeking behaviour
seek <- seek %>% slice (13:49) %>% 
  mutate(region = sapply(strsplit(row_labels, "\\|"), function(x) x[3]),
         seek = (Advice.or.treatment.sought.for.fever.symptoms.on.the.same.or.next.day.1)/100) %>%
  dplyr::select (region,seek)

## combine columns
state_ml <- itn %>%
  left_join(malaria, by = "region") %>%
  left_join(fever, by = "region") %>%
  left_join(seek, by = "region") %>%
  mutate(region = str_to_title(region)) # to match with statename of shape file

## add data to shape file
adm1_poly <- adm1 %>% left_join(state_ml, by = c("statename" = "region")) %>%
  st_as_sf()

# --- save state-level final dataset ---
st_write (adm1_poly, "data/shp/NGA_states/NGA_states.shp", driver="ESRI Shapefile")
