#######################
# Title : Report
# Purpose: To generate tables and plots for report
#######################

# load libraries
library(dplyr)
library(tidyr)
library(sf)
library(stringr)
library(ggplot2)
library(mapview)
library(leaflet)

# load dataset
adm1 <- st_read("data/shp/NGA_states/NGA_states.shp")
adm3 <- st_read("data/shp/NGA_wards_dashboard/NGA_wards_dashboard_poly.shp")
phc <- st_read ("data/shp/NGA_facilities/NGA_facilities.shp")

# 3.1 Descriptive
## summary information of phc
phc_sum <- phc %>% st_drop_geometry() %>% 
  group_by(statenm) %>% 
  summarize (
    lgas = n_distinct(lgacode),
    wards = n_distinct(wardcod),
    total = n(),
    moh = sum(str_detect(ownrshp, regex("primary|health", ignore_case = TRUE))),
    private = sum(str_detect(ownrshp, regex("private", ignore_case = TRUE))),
    ngo = sum(str_detect(ownrshp, regex("ngo", ignore_case = TRUE))),
    others = total - (private + moh + ngo)) %>%
  left_join(adm3 %>% st_drop_geometry() %>% group_by(statenm) %>% summarize (
    pop = sum(pop_24), zone = first(zone)) , by = "statenm") %>%
  mutate (p_per_pop = total/pop*100000)

## plot
### pop and phc facilities
pop_phc <- phc_sum %>%
  ggplot(aes(x = statenm)) +
  geom_col(aes(y = total, fill = "Number of PHC facilities")) +
  geom_point(aes(y = p_per_pop*40, color = "Facility per population"), size = 2) +
  facet_wrap(~zone, nrow = 1, scales = "free_x", strip.position = "bottom") +
  labs(y = NULL, x = NULL, fill = NULL) +
  scale_y_continuous(name = "Number of PHC facilities", labels = scales::comma,
                     sec.axis = sec_axis(~ ./40, name = "Facility per population",
                                         labels = scales::comma)) +
  scale_fill_manual(values = c("Number of PHC facilities" = "#35965D",
                               "Facility per population" = "#FF0000"),
                    labels = c("Number of PHC facilities", "Facility per population")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0, size = 9),
        axis.text.y = element_text(size = 9),
        axis.title = element_text(size = 10),
        axis.text = element_text(size = 10),
        legend.title = element_blank(), 
        legend.position = "bottom",
        panel.grid.major.x = element_blank())

### phc with ownerships
phc_staked <- phc_sum %>% 
  pivot_longer(cols = c(moh, private, ngo, others),
               names_to = "ownership_type",
               values_to = "count") %>% 
  ggplot(aes(x = statenm, y = count, fill = ownership_type)) +
  geom_bar(stat = "identity") +
  facet_wrap(~zone, nrow = 1, scales = "free_x", strip.position = "bottom") +
  labs(y = "Number of PHC facilities",
       x = NULL,
       fill = "Ownership Type") +
  scale_fill_manual(values = c("moh" = "#35965D", "private" = "#C97064", "ngo" = "#A6B07E", "others" = "#E8BB63"),
                    labels = c("MOH", "NGO", "Others","Private")) +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal() + # Adjust font family and size
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0, size = 9),
        axis.text.y = element_text(size = 9),
        axis.title = element_text(size = 10),
        axis.text = element_text(size = 10),
        legend.title = element_text(size = 10), 
        legend.position = "bottom",
        panel.grid.major.x = element_blank(),
        )

### avg distance between nearest two phc facilities
#### change unit
phc$nrst_ds <- phc$nrst_ds/1000
#### incidence stats
dist_stats <- phc %>% st_drop_geometry() %>%
  group_by(statenm) %>% 
  summarize(
    q1 = quantile(nrst_ds, 0.25),
    q3 = quantile(nrst_ds, 0.75),
    iqr = q3 - q1,
    lower = q1 - 1.5*iqr,
    upper = q3 + 1.5*iqr,
    median = median(nrst_ds)
  )
#### plot
phc_distance <- phc %>% st_drop_geometry() %>% 
  left_join(dist_stats, by = "statenm") %>%
  left_join(adm3%>%st_drop_geometry()%>% group_by(statenm)%>%summarize(zone = first(zone)), by = "statenm") %>%
  filter (nrst_ds > q1 - 1.5*iqr & nrst_ds < q3 + 1.5*iqr) %>%
  ggplot(aes(x = statenm, y = nrst_ds)) +
  geom_jitter(width = 0.2, alpha = 0.25, color = "#35965D") +
  geom_errorbar(aes(ymin = q1, ymax = q3), width = 0.2, color = "red") + # IQR lines
  geom_point(aes(y = median), color = "green", size = 2) + # Median points
  facet_wrap(~zone, nrow = 1, scales = "free_x", strip.position = "bottom") +
  labs(y = "Distance between two nearest\nPHC facilities (in kilometers)",
       x = NULL) +
  theme_minimal() + # Adjust font family and size
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0, size = 9),
        axis.title = element_text(size = 10, vjust = 0.5),
        axis.text = element_text(size = 10),
        legend.title = element_text(size = 10), 
        panel.grid.major.x = element_blank(),
  )

## ward level travel time with state summary
### state summary stats
stats <- adm3 %>%
  st_drop_geometry() %>%
  group_by(statenm) %>%
  summarise(
    median = median(travltm, na.rm = TRUE),
    q1 = quantile(travltm, 0.25, na.rm = TRUE),
    q3 = quantile(travltm, 0.75, na.rm = TRUE),
    iqr = q3 - q1
  )
### ward level jitter plot
ward_access <- adm3 %>% st_drop_geometry() %>% 
  left_join(stats, by = "statenm") %>%
  filter (travltm > q1 - 1.5*iqr & travltm < q3 + 1.5*iqr) %>%
  ggplot(aes(x = statenm, y = travltm)) +
  geom_jitter(width = 0.2, alpha = 0.25, color = "#35965D") +
  geom_errorbar(aes(ymin = q1, ymax = q3), width = 0.2, color = "red") + # IQR lines
  geom_point(aes(y = median), color = "green", size = 2) + # Median points
  facet_wrap(~zone, nrow = 1, scales = "free_x", strip.position = "bottom") +
  labs(y = "Travel time in minutes",
       x = NULL) +
  theme_minimal() + # Adjust font family and size
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0, size = 9),
        axis.title = element_text(size = 10),
        axis.text = element_text(size = 10),
        legend.title = element_text(size = 10), 
        panel.grid.major.x = element_blank(),
  )
### ward level access with pop
 ward_access_2 <- adm3 %>% st_drop_geometry() %>% 
   group_by(statenm, zone) %>%
   summarise(pop = sum(pop_24),
             pop_remote = sum(pp_bv60),
             pop_close = pop - pop_remote) %>%
   pivot_longer(cols = c(pop_remote,pop_close),
                names_to = "population_type",
                values_to = "count") %>%
   ggplot(aes(x = statenm, y = count, fill = population_type)) +
   geom_bar(stat = "identity") +
   facet_wrap(~zone, nrow = 1, scales = "free_x", strip.position = "bottom") +
   labs(y = "Population",
        x = NULL,
        fill = "Population Type") +
   scale_fill_manual(values = c(pop_close = "#35965D", pop_remote = "#C97064"),
                     labels = c(pop_close = "Within 1 hour distance", pop_remote = "More than 1 hour distance")) +
   scale_y_continuous(labels = scales::comma) +
   theme_minimal() + 
   theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 9),
         axis.title.y = element_text(size = 10),
         axis.title.x = element_text(size = 10),
         axis.title = element_text(size = 10),
         axis.text = element_text(size = 10),
         #strip.background = element_rect(fill = "#fff4df", linetype = 0),
         legend.title = element_blank(),
         legend.position = "bottom",
         panel.grid.major.x = element_blank())

## map: percent of population beyond 1 hour travel time
### define breaks 
breaks <- c(0, 0.25, 0.5, 0.75,1)
color_palette <- c('#49FF00', '#FBFF00', '#FF9300','#FF0000')
pal <- colorBin(color_palette, domain = adm3$percent, bins = breaks)
legend_labels <- c("Less than 25%", "25 to 50%", "50 to 75%", "75% and above")
### map with leaflet: percent of population beyond 1 hour travel time
map_access_percent <- leaflet() %>% 
  addProviderTiles(providers$CartoDB.Positron) %>% 
  addPolygons(data = adm3, 
              fillColor = ~pal(percent), fillOpacity = 0.7, color = '#808080', weight = 0.5) %>%
  addPolygons(data = adm1, color = '#36454F', fillOpacity = 0, weight = 0.75,
              # label = ~statename, labelOptions = labelOptions(
              #   noHide = TRUE,
              #   style = list("font-size" = "8px", "background-color" = "rgba(255, 255, 255, 0.7)"))
              ) %>%
  addLegend(position = "bottomright",colors = color_palette,
            labels = legend_labels, title = "Percentage of population") %>%
  addScaleBar(position = "bottomleft")

## map: population beyond 1 hour travel time
### define breaks
adm3$pop_24_above60 <- adm3$pop_24 * adm3$percent
pop_breaks <- c(0, 1000, 5000, 10000, Inf)
pop_pal <- colorBin(color_palette, domain = adm3$pop_24_above60, bins = pop_breaks)
pop_legend_labels <- c("Less than 1,000", "1,000 to 5,000", "5,000 to 10,000", "10,000 and above")
### map with leaflet: percent of population beyond 1 hour travel time
map_access_number <- leaflet() %>% 
  addProviderTiles(providers$CartoDB.Positron) %>% 
  addPolygons(data = adm3, 
              fillColor = ~pop_pal(pop_24_above60), fillOpacity = 0.7, color = '#808080', weight = 0.5) %>%
  addPolygons(data = adm1, color = '#36454F', fillOpacity = 0, weight = 0.75,
              # label = ~statename, labelOptions = labelOptions(
              #   noHide = TRUE,
              #   style = list("font-size" = "8px", "background-color" = "rgba(255, 255, 255, 0.7)"))
  ) %>%
  addLegend(position = "bottomright",colors = color_palette,
            labels = pop_legend_labels, title = "Population") %>%
  addScaleBar(position = "bottomleft")

## map: malaria indicators
### ward-level > incidence, mortality, lga-level > prevalence
#### define palette
inc_palette <- colorQuantile (palette = color_palette, domain = adm3$inc_rat, n = 4)
mor_palette <- colorQuantile (palette = color_palette, domain = adm3$mor_rat, n = 4)
pre_palette <- colorQuantile (palette = color_palette, domain = adm3$pfrate, n = 4)
inc_legend_labels <- c("Less than 252", "252 to 292", "293 to 333", "334 and above")
mor_legend_labels <- c("Less than 0.61", "0.61 to 0.94", "0.95 to 1.35", "1.36 and above")
pre_legend_labels <- c("Less than 29%", "29% to 33%", "34% to 37%", "38% and above")

#### map with leaflet: incidence
map_mal_inc <- leaflet() %>% 
  addProviderTiles(providers$CartoDB.Positron) %>% 
  addPolygons(data = adm3, 
              fillColor = ~inc_palette(inc_rat), fillOpacity = 0.7, color = '#808080', weight = 0.5) %>%
  addPolygons(data = adm1, color = '#36454F', fillOpacity = 0, weight = 0.75,
              # label = ~statename, labelOptions = labelOptions(
              #   noHide = TRUE,
              #   style = list("font-size" = "8px", "background-color" = "rgba(255, 255, 255, 0.7)"))
  ) %>%
  addLegend(position = "bottomright",colors = color_palette,
            labels = inc_legend_labels, title = "Incidence rate per 1,000") %>%
  addScaleBar(position = "bottomleft")
#### map with leaflet: mortality
map_mal_mor <- leaflet() %>% 
  addProviderTiles(providers$CartoDB.Positron) %>% 
  addPolygons(data = adm3, 
              fillColor = ~mor_palette(mor_rat), fillOpacity = 0.7, color = '#808080', weight = 0.5) %>%
  addPolygons(data = adm1, color = '#36454F', fillOpacity = 0, weight = 0.75,
              # label = ~statename, labelOptions = labelOptions(
              #   noHide = TRUE,
              #   style = list("font-size" = "8px", "background-color" = "rgba(255, 255, 255, 0.7)"))
  ) %>%
  addLegend(position = "bottomright",colors = color_palette,
            labels = mor_legend_labels, title = "Mortality rate per 100,000") %>%
  addScaleBar(position = "bottomleft")
#### map with leaflet: prevalence
map_mal_pre <- leaflet() %>% 
  addProviderTiles(providers$CartoDB.Positron) %>% 
  addPolygons(data = adm3, stroke = FALSE, smoothFactor = 0,
              fillColor = ~pre_palette(pfrate), fillOpacity = 0.7, color = '#808080', weight = 0.5) %>%
  addPolygons(data = adm1, color = '#36454F', fillOpacity = 0, weight = 0.75,
              ) %>%
  addLegend(position = "bottomright",colors = color_palette,
            labels = pre_legend_labels, title = "Percentge of Under 5 children") %>%
  addScaleBar(position = "bottomleft")

### ward-level > itn access, itn use
#### define palette
itn_breaks <- c(0, 0.25, 0.50, 0.75, Inf)
itna_palette <- colorBin (palette = rev(color_palette), domain = adm3$itnccss, bins = itn_breaks)
itnu_palette <- colorBin (palette = rev(color_palette), domain = adm3$itnuse, bins = itn_breaks)
itn_legend_labels <- c("Less than 25%", "25% to 50%", "50% to 75%", "75% and above")
#### map with leaflet: itn access
map_itn_acc <- leaflet() %>% 
  addProviderTiles(providers$CartoDB.Positron) %>% 
  addPolygons(data = adm3, 
              fillColor = ~itna_palette(itnccss), fillOpacity = 0.7, color = '#808080', weight = 0.5) %>%
  addPolygons(data = adm1, color = '#36454F', fillOpacity = 0, weight = 0.75,
              ) %>%
  addLegend(position = "bottomright",colors = rev(color_palette),
            labels = itn_legend_labels, title = "Access to ITN") %>%
  addScaleBar(position = "bottomleft")

#### map with leaflet: itn use
map_itn_use <- leaflet() %>% 
  addProviderTiles(providers$CartoDB.Positron) %>% 
  addPolygons(data = adm3, 
              fillColor = ~itnu_palette(itnuse), fillOpacity = 0.7, color = '#808080', weight = 0.5) %>%
  addPolygons(data = adm1, color = '#36454F', fillOpacity = 0, weight = 0.75,
              ) %>%
  addLegend(position = "bottomright",colors = rev(color_palette),
            labels = itn_legend_labels, title = "Use of ITN") %>%
  addScaleBar(position = "bottomleft")

## plot: malaria indicators
### incidence jitter plot
#### incidence stats
inc_stats <- adm3 %>% st_drop_geometry() %>%
  group_by(statenm) %>% 
  summarize(
    q1 = quantile(inc_rat, 0.25),
    q3 = quantile(inc_rat, 0.75),
    iqr = q3 - q1,
    lower = q1 - 1.5*iqr,
    upper = q3 + 1.5*iqr,
    median = median(inc_rat)
  )
#### plot
ward_mal_inc <- adm3 %>% st_drop_geometry() %>% 
  left_join(inc_stats, by = "statenm") %>%
  filter (inc_rat > q1 - 1.5*iqr & inc_rat < q3 + 1.5*iqr) %>%
  ggplot(aes(x = statenm, y = inc_rat)) +
  geom_jitter(width = 0.2, alpha = 0.25, color = "#35965D") +
  geom_errorbar(aes(ymin = q1, ymax = q3), width = 0.2, color = "red") + # IQR lines
  geom_point(aes(y = median), color = "green", size = 2) + # Median points
  facet_wrap(~zone, nrow = 1, scales = "free_x", strip.position = "bottom") +
  labs(y = "Malaria incidence rate\nper 1,000 population",
       x = NULL) +
  theme_minimal() + # Adjust font family and size
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0, size = 9),
        axis.title = element_text(size = 10, vjust = 0.5),
        axis.text = element_text(size = 10),
        legend.title = element_text(size = 10), 
        panel.grid.major.x = element_blank(),
  )

### mortality jitter plot
#### mortality stats
mor_stats <- adm3 %>% st_drop_geometry() %>%
  group_by(statenm) %>% 
  summarize(
    q1 = quantile(mor_rat, 0.25),
    q3 = quantile(mor_rat, 0.75),
    iqr = q3 - q1,
    lower = q1 - 1.5*iqr,
    upper = q3 + 1.5*iqr,
    median = median(mor_rat)
  )
#### plot
ward_mal_mor <- adm3 %>% st_drop_geometry() %>% 
  left_join(mor_stats, by = "statenm") %>%
  filter (mor_rat > q1 - 1.5*iqr & mor_rat < q3 + 1.5*iqr) %>%
  ggplot(aes(x = statenm, y = mor_rat)) +
  geom_jitter(width = 0.2, alpha = 0.25, color = "#35965D") +
  geom_errorbar(aes(ymin = q1, ymax = q3), width = 0.2, color = "red") + # IQR lines
  geom_point(aes(y = median), color = "green", size = 2) + # Median points
  facet_wrap(~zone, nrow = 1, scales = "free_x", strip.position = "bottom") +
  labs(y = "Malaria mortality rate\nper 100,000 population",
       x = NULL) +
  theme_minimal() + # Adjust font family and size
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0, size = 9),
        axis.title = element_text(size = 10, vjust = 0.5),
        axis.text = element_text(size = 10),
        legend.title = element_text(size = 10), 
        panel.grid.major.x = element_blank(),
  )

### prevalence jitter plot
#### prevalence stats
pre_stats <- adm3 %>% st_drop_geometry() %>%
  group_by(statenm) %>% 
  summarize(
    q1 = quantile(pfrate, 0.25),
    q3 = quantile(pfrate, 0.75),
    iqr = q3 - q1,
    lower = q1 - 1.5*iqr,
    upper = q3 + 1.5*iqr,
    median = median(pfrate)
  )
#### plot
ward_mal_pre <- adm3 %>% st_drop_geometry() %>% group_by(lgacode) %>% 
  summarise(pfrate = first(pfrate), statenm = first(statenm), zone = first(zone)) %>%
  left_join(pre_stats, by = "statenm") %>%
  filter (pfrate > q1 - 1.5*iqr & pfrate < q3 + 1.5*iqr) %>%
  ggplot(aes(x = statenm, y = pfrate)) +
  geom_jitter(width = 0.2, alpha = 0.25, color = "#35965D") +
  geom_errorbar(aes(ymin = q1, ymax = q3), width = 0.2, color = "red") + # IQR lines
  geom_point(aes(y = median), color = "green", size = 2) + # Median points
  facet_wrap(~zone, nrow = 1, scales = "free_x", strip.position = "bottom") +
  scale_y_continuous(labels = scales::percent) +
  labs(y = "Malaria prevalence rate",
       x = NULL) +
  theme_minimal() + # Adjust font family and size
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0, size = 9),
        axis.title = element_text(size = 10),
        axis.text = element_text(size = 10),
        legend.title = element_text(size = 10), 
        panel.grid.major.x = element_blank(),
  )

### itn jitter plot
#### itn stats
itn_stats <- adm3 %>% st_drop_geometry() %>%
  group_by(statenm) %>% 
  summarize(
    q1 = quantile(itnccss, 0.25),
    q3 = quantile(itnccss, 0.75),
    iqr = q3 - q1,
    lower = q1 - 1.5*iqr,
    upper = q3 + 1.5*iqr,
    median = median(itnccss)
  )
#### plot
ward_itn <- adm3 %>% st_drop_geometry() %>% 
  left_join(itn_stats, by = "statenm") %>%
  filter (itnccss > q1 - 1.5*iqr & itnccss < q3 + 1.5*iqr) %>%
  ggplot(aes(x = statenm, y = itnccss)) +
  geom_jitter(width = 0.2, alpha = 0.25, color = "#35965D") +
  geom_errorbar(aes(ymin = q1, ymax = q3), width = 0.2, color = "red") + # IQR lines
  geom_point(aes(y = median), color = "green", size = 2) + # Median points
  facet_wrap(~zone, nrow = 1, scales = "free_x", strip.position = "bottom") +
  labs(y = "Percentage of household\nin possession of ITNs",
       x = NULL) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() + # Adjust font family and size
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0, size = 9),
        axis.title = element_text(size = 10, vjust = 0.5),
        axis.text = element_text(size = 10),
        legend.title = element_text(size = 10), 
        panel.grid.major.x = element_blank(),
  )

## map: conflict indicators
#### define palette
con_breaks <- c(0, 1, 3,5, 8)
con_palette <- colorBin (palette = color_palette, domain = adm3$cnflct_, bins = con_breaks)
con_legend_labels <- c("No conflict", "1 to 2", "3 to 4", "5 and above")
#### map with leaflet: conflict
map_con <- leaflet() %>% 
  addProviderTiles(providers$CartoDB.Positron) %>% 
  addPolygons(data = adm3, 
              fillColor = ~con_palette(cnflct_), fillOpacity = 0.7, color = '#808080', weight = 0.5) %>%
  addPolygons(data = adm1, color = '#36454F', fillOpacity = 0, weight = 0.75,
              ) %>%
  addLegend(position = "bottomright",colors = color_palette,
            labels = con_legend_labels, title = "Conflict index") %>%
  addScaleBar(position = "bottomleft")
#### plot
ward_con <- adm3 %>% st_drop_geometry() %>% 
  left_join(itn_stats, by = "statenm") %>%
  filter (cnflct_ > 0) %>%
  ggplot(aes(x = statenm, y = cnflct_)) +
  geom_boxplot() +
  geom_jitter(width = 0.2, alpha = 0.25, color = "#35965D") +
  # geom_errorbar(aes(ymin = q1, ymax = q3), width = 0.2, color = "red") + # IQR lines
  # geom_point(aes(y = median), color = "green", size = 2) + # Median points
  facet_wrap(~zone, nrow = 1, scales = "free_x", strip.position = "bottom") +
  labs(y = "Conflict Index",
       x = NULL) +
  theme_minimal() + # Adjust font family and size
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0, size = 9),
        axis.title = element_text(size = 10, vjust = 0.5),
        axis.text = element_text(size = 10),
        legend.title = element_text(size = 10), 
        panel.grid.major.x = element_blank(),
  )

########################### numbers and tables ###########################
# 3.1 
## no. faciliites per zone
phc %>% st_drop_geometry() %>% group_by(statenm) %>% summarize (phc = n()) %>% 
  left_join(adm3 %>% st_drop_geometry() %>% group_by(statenm) %>% summarize (pop = sum(pop_24), zone = first(zone)), by = "statenm") %>%
  group_by(zone) %>% summarize (phc = sum(phc), pop = sum(pop))

## Distribution of primary health care (PHC) facilities
phc %>% st_drop_geometry() %>% group_by(statenm) %>% summarize (phc = n()) %>% 
  left_join(adm3 %>% st_drop_geometry() %>% group_by(statenm) %>% summarize (pop = sum(pop_24), zone = first(zone)), by = "statenm") %>%
  mutate(phc_per_100k = phc/pop*100000) %>% View()
## travel time
adm3 %>% st_drop_geometry() %>% group_by(statenm) %>% summarize (
    tt = median(travltm), 
    pop = sum(pop_24),
    pop_60 = sum(pop_24*percent),
    percent = pop_60/pop) %>% View()
## malaria indicators
adm3 %>% st_drop_geometry() %>% group_by(statenm) %>% summarize (
    mor = mean(mor_rat, na.rm = TRUE),
    pre = mean(pfrate, na.rm = TRUE),
    itnccss = mean(itnccss),
    itnuse = mean(itnuse)) %>% View()

