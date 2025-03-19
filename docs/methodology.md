# MCDA-Rshiny-Nigeria
Multi-criteria decision analysis tool **(MCDA)** to support strengthening community health workforce using a blended approach, including data extractions, spatial optimization, and R Shiny dashboard. 

# 1. Data sources
## 1.1 Spatial dataset 
### 1.1.1 Administrative boundaries (vector - polygons)
We used a national level boundary (administrative-level 1) dataset, a LGA boundaries (administrative-level 2) dataset with 774 spatial areas and a ward boundaries (administrative-level 3) dataset with 9,410 spatial areas. These datasets were released in March 2021 and available through Geo-Referenced Infrastructure and Demographic Data for Development (GRID3). 

### 1.1.2 Health facilities list (vector - points)
We acquired geo-referenced list of PHC facilities from GRID3 which was released in September 2020. There are total of 46,146 primary, secondary, and tertiary health care facilities nation-wide.

### 1.1.3 Conflict dataset (vector - points)
Geo-referenced data from the Armed Conflict Location & Event Data project (ACLED) during 1 May 2021 to 30 April 2024 were included in this project. Disorder type of ‘political violence’ is defined as the use of force by a group with a political purpose or motivation, or with distinct political effects. In this analysis, we used two political violence event types; ‘Battles’ – Violent interactions between two organized armed groups at a particular time and location, and ‘Violence against civilians’ – Violent events where an organized armed group inflicts violence upon unarmed non-combatants. For the academic purpose, the georeferenced dataset is available via ACLED’s data export tool on ACLED Access Portal.

### 1.1.4 Population surface (raster - 100x100m)
We used gridded population estimates (version 2.1) which is a GeoTIFF raster at a spatial resolution of 3 arc-seconds (approximately 100m at the equator), containing estimates of total population size per grid cell across Nigeria. It was produced with bottom-up approach using sampled data from recent survey datasets to build a statistical model to estimate population in unsampled locations. NA values represent areas that were mapped as unsettled based on a gridded settlement layer derived from building footprints (Maxar Technologies, Inc. and Ecopia Tech Corporation, 2021). This dataset was produced by WorldPop Research Group at the University of Southampton which was initially released in 2019 and updated in 2023.

### 1.1.5 Friction surface (raster - 1x1km)
We used walking-only surface to more realistically measure for access to health care services due to limited access to motorized transportation in remote areas. The ‘get_friction_surface’ function from PATHtools package in R was used to extract walking-only friction surface. Alternatively, we can download this raster surface from Malaria Atlas Project (MAP).

### 1.1.6 Modelled surfaces for malaria indicators (raster - 5x5km)
Raster layers from Malaria Atlas Project (MAP), the number of newly diagnosed Plasmodium falciparum cases per 1,000 population and deaths per 100,000 population, proportion of population with access to and use of an Insecticide-Treated Net (ITN) in their household (on the basis that one net provides coverage for two people) in Nigeria during 2022. In 36 high-burden countries in sub-Saharan Africa, including Nigeria, cartographic approach was used to map Plasmodium falciparum parasite rate at the pixel level and subsequently converted these results into estimates of clinical incidence and mortality. Geostatistical models were applied to datasets consisting of parasite rate points and routine surveillance reports, and a rich set of temporally dynamic geospatial environmental and socioeconomic data. For ITN related indicators, Bayesian mixed modelling framework built upon data from net manufacturers, national programs, and cross-sectional household surveys over the past 20 years to estimate the history of ITN coverage metrics in 40 sub-Saharan African countries are applied.

## 1.2 Other datasets
### 1.2.1 State-level Malaria indicators from DHS
Survey datasets from Nigeria Malaria Indicator Survey 2021 (MIS-VIII) were accessed through DHS program. Following datasets were used:
| Dataset |	Unit of analysis | Indicators |
| --- | --- | --- |
| Household Data - Household Recode (HR) |	Household	|	Household ownership of ITNs ‘hh_ITN’ |
| Household Listing Data - Household Member Recode (PR) |	Household member |	Parasitaemia (via microscopy) in children 6-59 months ‘pr_micro_chld’ |
| Children's Data - Children's Recode (KR) | Children of women born in the last 5 years (0-59 months)	|	Children under age 5 years with fever in the 2 weeks preceding the survey ‘kr_fever’ <br />	Advice or treatment was sought the same or next day ‘kr_fev_day’ |

### 1.2.2 LGA-level Malaria prevalence data from MAP
We used updated dataset on percentage of children under 5 years of age who have PF parasitaemia with RDT test at LGA-level by Malaria Atlas Project (MAP).

# 2. Data manipulation for analysis
