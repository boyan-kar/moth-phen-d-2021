
# Script to explore moth abundance data
# and create some graphs and animations

# Load libraries ----
library(data.table) # dataframes, wrangling
library(tidyverse) # data wrangling and visualisation
library(lubridate) # handling dates and times
library(arrow) # for reading/writing parquet files (compressed tabular data)
library(sf) # geospatial data
library(units) # for handling units
library(gganimate)
library(rnaturalearth)

# Functions ----

# Load data ----

# Moth abundance data
moths <-
  open_dataset("data/RIS_transformed/parquet") %>% 
  select(binomial, site_no, brood_start_yr, ordinal_day, moth_count) %>% 
  filter(moth_count > 0) %>% 
  collect()
setDT(moths)

nrow(moths)
nrow(unique(moths))


# Trap data
traps_all_data <- read_sf("data/RIS_tidy_parquet/RIS_light_traps.gpkg") %>% 
  st_transform(crs=27700)

traps <- read_sf("data/RIS_transformed/light_traps.gpkg")

# Load hadUK grid
haduk_grid <- st_read(dsn = "data/hadUK_tidy/haduk_grid.gpkg",
         layer="haduk_grid")

# Check object size in memory
# print(object.size(moths), units = "auto", standard = "SI")

# Get UK shape
UK_sf <- ne_countries(returnclass='sf', scale = "medium") %>% 
  filter(name == "United Kingdom") %>% 
  st_transform(27700)

# Plot Rothamsted trap locations
RIS_map <- ggplot() +
  geom_sf(data=UK_sf) +
  geom_sf(data=traps, size = 1, colour = "red") +
  theme_void()

plot(RIS_map)
ggsave("graphs/RIS_locations.png", RIS_map, device = "png",
       height = 6, width = 3, units="in")

# Phenology distribution graphs ----

moths_all <- copy(moths)

# Merge moths with grid cells
# Match traps to grid cells
moths <- 
  traps %>% 
  as.data.table() %>% 
  select(site_no, grid_cell_no) %>% 
  right_join(moths)

# Keep copy of all records before filtering
moths_all <- copy(moths)

# Use only years with at least 5 moths caught yearly in the grid cell
# For exploration of the data only
moths[, yearly_spp_catches := sum(moth_count),
      by = list(binomial, grid_cell_no, brood_start_yr)]

moths <- moths[yearly_spp_catches >= 5, ]

# Also remove Smerinthus ocellatus due to not a lot of data available
moths <- moths[binomial != "Smerinthus ocellatus", ]

# Calculate centered year day
moths[, mean_day := round(weighted.mean(ordinal_day, moth_count)),
                                by = list(binomial, grid_cell_no)]

moths[, ordinal_day_centered := ordinal_day - mean_day]

# Extract only needed info (binomial, daily count, centered mean day)
moth_spp_gcell_centr_days <- moths[, .(binomial, moth_count, ordinal_day_centered)]
moth_spp_gcell_centr_days[, total_abundance := sum(moth_count),
                          by = list(binomial, ordinal_day_centered)]
moth_spp_gcell_centr_days[, moth_count := NULL]
moth_spp_gcell_centr_days <- unique(moth_spp_gcell_centr_days)

# Plot with x axis between day -50 and day 50
(centered_phenology_plot <- 
  ggplot(moth_spp_gcell_centr_days) +
  geom_line(aes(x = ordinal_day_centered, y = total_abundance)) +
  facet_wrap(~binomial, scales="free") +
  xlab("Ordinal day (centered for each annual population)") + ylab("Abundance") +
  scale_x_continuous(limits=c(-50, 50)) +
  theme_classic())

ggsave(filename = "graphs/centered_phenology_plot.png",
       plot = centered_phenology_plot,
       width = 10, height = 8, units = "in")

# Plot ordinal day - latitude ----
moths_toplot <- copy(moths_all)

# Remove outer 1% of data
moths_toplot[, `:=`(day_1percentile = quantile(ordinal_day, 0.005),
             day_99percentile = quantile(ordinal_day, 0.995)),
             by = list(binomial)]

moths_toplot <- moths_toplot[ordinal_day > day_1percentile &
                               ordinal_day < day_99percentile, ]

moths_toplot <- moths_toplot[binomial != "Smerinthus ocellatus", ]

moths_toplot[, `:=`(day_1percentile = NULL,
                  day_99percentile = NULL)]

# Remove year 1960 as they won't be modelled anyway (weather data starts at 1960)
moths_toplot <- moths_toplot[brood_start_yr > 1960, ]

# Add column for decade
moths_toplot[, decade := ordered(case_when(
  brood_start_yr <= 1970 ~ "1961-1970",
  brood_start_yr <= 1980 ~ "1971-1980",
  brood_start_yr <= 1990 ~ "1981-1990",
  brood_start_yr <= 2000 ~ "1991-2000",
  brood_start_yr <= 2010 ~ "2001-2010",
  brood_start_yr <= 2020 ~ "2011-2020",),
  levels = c("1961-1970","1971-1980","1981-1990",
             "1991-2000","2001-2010","2011-2020")
)]

# Summarise for plotting (total daily abundance for each ordinal day and lat)

# Add latitude to table
moths_toplot <-
  traps %>% 
  st_transform(4326) %>%  # transform to long-lat CRS
  cbind(., st_coordinates(.)) %>% # extract long and lat as columns
  as.data.table() %>% 
  select(grid_cell_no, latitude = Y) %>% 
  right_join(moths_toplot)
  
# rounded latitude
moths_toplot[, latitude := round(latitude, 1)]

moths_toplot <- 
  moths_toplot[, .(abundance = sum(moth_count)),
               by = .(binomial, decade, latitude, ordinal_day)]

# Standardize so it looks nice
moths_toplot[, stdised_abund := (abundance-mean(abundance))/sd(abundance),
             by = .(binomial, decade)]

moths_toplot[, stdised_abund := sqrt(stdised_abund-min(stdised_abund)),
             by = .(binomial, decade)]

# ggplot(moths_toplot[decade == "1971-1980",]) +
#   geom_raster(aes(x = ordinal_day, y = latitude, fill = stdised_abund),
# #              interpolate = TRUE
#               ) +
#   scale_fill_viridis_c() +
#   facet_wrap(vars(binomial), scales = "free_x") +
#   labs(x = "Ordinal day", y = "Latitude")
#   theme_bw() + 
#   theme(legend.position = "none")

ordday_lat_plot_anim <- 
  ggplot(moths_toplot) +
    geom_raster(aes(x = ordinal_day, y = latitude, fill = stdised_abund),
                #              interpolate = TRUE
    ) +
    scale_fill_viridis_c() +
    facet_wrap(vars(binomial), scales = "free_x",
               nrow = 3, ncol = 5) +
    transition_states(decade,
                      transition_length = 1,
                      state_length = 1) +
    enter_appear() +
    labs(title = "{closest_state}",
         x = "Ordinal day", y = "Latitude") +
    theme_bw() + 
    theme(legend.position = "none")


animate(ordday_lat_plot_anim,
        fps = 1,
        detail = 3,
        duration = 8, # seconds
        start_pause = 1, # frames
        end_pause = 1, # frames
        renderer = gifski_renderer(),
        height = 500,
        width = 900)

anim_save("graphs/ordday_lat_phenology.gif")    
    