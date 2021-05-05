
# Script to combine multiple files produced by script 04: extract hadUK data
# Into one table (.parquet) and a matching spatial file (.pgkg)

# Load libraries ----
library(data.table) # dataframes, wrangling
library(tidyverse) # data wrangling and visualisation
library(lubridate) # times and dates
library(arrow) # for reading/writing parquet files (compressed tabular data)
library(sf) # geospatial data
library(stars) # spatiotempral arrays
library(units) # for handling units
library(yaml) # for loading configuration

# Use max threads for data.table
setDTthreads(0)

# If the working directory is /scripts, set it to
# the parent directory (main dir of repo)
setwd(gsub("/scripts$", "", getwd()))

# Configure script ----

# Data directory (containing stars objects saved as .rds )
data_dir = "data/hadUK/"

# Variables
weather_vars = c("tasmin", "tasmax", "rainfall")

# Year range
first_year <- 1960
last_year <- 2018

# Load data ----

# char vector with filepath for each of the weather variables
weather_vars_paths <- character(0)
for (i in 1:length(weather_vars)) {
  
  weather_vars_paths[i] <- file.path(data_dir,
                                     str_subset(list.files(data_dir),
                                      weather_vars[i])[1])
  
}

# Load the grid from the first file in the vector.
haduk_grid <- 
  readRDS(file = weather_vars_paths[1]) %>%
  st_as_sf() %>% 
  add_column(., grid_cell_no = 1:nrow(.)) %>% 
  select(grid_cell_no)
# Can be improved by first removing all the weather data before converting to sf

# Function to convert all of the stars arrays to a tidy data.table
# Note: Only works for first attribute (1 weather variable) of stars objects
# grid_sf should contain a grid_cell_no column
stars_to_gridno_table <- function(temporal_stars_array, grid_sf,
                                  grid_cell_no_column) {
  
  weather_variable_name <- names(temporal_stars_array)[1]
  
  temporal_stars_array <- temporal_stars_array[weather_variable_name]
  
  weather_data_sf <- st_as_sf(temporal_stars_array)
  
  rm(temporal_stars_array)
  
  weather_data_sf_gridcell <- st_join(weather_data_sf, grid_sf,
                                join = st_equals)
  
  rm(weather_data_sf, grid_sf)
  
  weather_data_gridcell <- weather_data_sf_gridcell %>%
    as.data.frame() %>%
    select(all_of(grid_cell_no_column),
           matches("[[:digit:]]{4}-[[:digit:]]{2}-[[:digit:]]{2}")) %>%
    pivot_longer(cols = matches("[[:digit:]]{4}-[[:digit:]]{2}-[[:digit:]]{2}"),
                 names_to = "date",
                 values_to = weather_variable_name) %>%
    mutate(date = as.Date(date))
  
  return(as.data.table(weather_data_gridcell))
  
  
}

# Load and apply the function to all stars arrays
weather_data_list <-
  weather_vars_paths %>% 
  as.vector(mode = "list") %>% 
  `names<-`(weather_vars) %>% 
  map(~readRDS(.)) %>% 
  map(~stars_to_gridno_table(temporal_stars_array = .,
                             grid_sf = haduk_grid,
                             grid_cell_no_column = "grid_cell_no"))

# Get the first table from the list and then left-join all of the others
weather_data_dt <- copy(weather_data_list[[1]])

for (i in 2:length(weather_data_list)) {
  weather_data_dt <- copy(weather_data_list[[i]])[weather_data_dt,
                                                  on = .(grid_cell_no = grid_cell_no,
                                                         date = date)]
}
rm(weather_data_list)

# Keep only weather data 1960-2018
weather_data_dt <-
  weather_data_dt[year(date) >= first_year & year(date) <= last_year, ]

# Save all files ----
st_write(haduk_grid, dsn = "data/hadUK_tidy/haduk_grid.gpkg",
         layer="haduk_grid", delete_dsn = TRUE)
write_parquet(weather_data_dt,
              "data/hadUK_tidy/parquet/haduk_all_per_gridcellno.parquet")

