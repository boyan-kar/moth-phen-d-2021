
# Script to extract hadUK weather data to match specific point locations
# Requires: Downloaded HadUK grid data (NetCDF .nc files); point locations to match

# Load libraries ----
library(data.table) # dataframes, wrangling
library(tidyverse) # data wrangling and visualisation
library(lubridate) # handling dates and times
library(arrow) # for reading/writing parquet files (compressed tabular data)
library(lubridate) # times and dates
library(sf) # simple features - spatial data
library(stars) # spatiotempral arrays
library(tidync) # for NetCDF4 files
library(units) # for handling units
library(yaml) # for loading YAML configuration

# Use max threads for data.table
setDTthreads(0)

# If the working directory is /scripts, set it to
# the parent directory (main dir of repo)
setwd(gsub("/scripts$", "", getwd()))


# Functions ----
create_dateseries <- function(period, start_year, end_year) {

  if (period == "day") {

    first_dates_of_month <-
      seq.Date(from = as.Date(paste0(start_year, "-01-01")),
               length = (end_year - start_year + 1)*12, by = "month")
    last_dates_of_month <-
      seq.Date(from = as.Date(paste0(start_year, "-02-01")),
               length = (end_year - start_year + 1)*12, by = "month") - 1
    dateseries <- paste(
      as.character(format(first_dates_of_month, format = "%Y%m%d")),
      as.character(format(last_dates_of_month, format = "%Y%m%d")),
      sep = "-"
    )

  }

  else {

    first_months_of_year <-
      seq.Date(from = as.Date(paste0(start_year, "-01-01")),
               length = end_year - start_year + 1, by = "year")
    last_months_of_year <-
      seq.Date(from = as.Date(paste0(start_year, "-12-01")),
               length = end_year - start_year + 1, by = "year")
    dateseries <- paste(
      as.character(format(first_months_of_year, format = "%Y%m")),
      as.character(format(last_months_of_year, format = "%Y%m")),
      sep = "-"
    )

  }

  return (dateseries)

}

create_filename_series <- function(period, weather_variable, grid_scale,
                                   start_year, end_year, data_address) {

  dates <- create_dateseries(period, start_year, end_year)

  filename_series <- paste0(weather_variable,
                            "_hadukgrid_uk_",
                            grid_scale,
                            "km_",
                            period,
                            "_",
                            dates,
                            ".nc")

  filename_series <- file.path(data_address, filename_series)
  return(filename_series)

}


read_haduk_nc_as_stars <- function(filepath, as_squares = TRUE, variable="default") {

  if (variable == "default") {
    haduk_stars <- read_ncdf(filepath)
    st_crs(haduk_stars) <- 27700
  }

  else {
    haduk_stars <- read_ncdf(filepath, var=variable)
    st_crs(haduk_stars) <- 27700
  }

  haduk_stars <- st_xy2sfc(haduk_stars, as_points = !as_squares, na.rm = TRUE)

  return(haduk_stars)

}


# Load configuration for data to extract, match
datasets_configs_list = yaml.load_file("scripts/config/02_extract_hadUK_data.yaml")

for (dataset_config in datasets_configs_list) {

  # Extract the variables
  shp_address = dataset_config[[1]][["shp_address"]]
  data_address = dataset_config[[1]][["data_address"]]
  output_address = dataset_config[[1]][["output_address"]]
  start_year = dataset_config[[1]][["start_year"]]
  end_year = dataset_config[[1]][["end_year"]]
  grid_scale = dataset_config[[1]][["grid_scale"]]
  period = dataset_config[[1]][["period"]]
  weather_var = dataset_config[[1]][["variable"]]

  filenames <- create_filename_series(period, weather_variable = weather_var,
                                      grid_scale, start_year, end_year, data_address)
  # TODO remove filenames from this vector that don't exist
  print(filenames[1])
  # Read the first .nc file as a stars spatiotemporal array
  first_nc_stars <- read_haduk_nc_as_stars(filenames[1], variable=weather_var)
  # second_nc_stars <- read_haduk_nc_as_stars(filenames[2])

  haduk_grid_sf <- st_as_sf(first_nc_stars, na.rm = FALSE) %>%
    select() %>% # drop all columns except geometry
    mutate(., geometry_index = 1:nrow(.))
    # index number will be used to fast filter out the stars objects


  # Load "target" points weather data will be matched to
  target_points <-
    st_read(shp_address, quiet = TRUE) %>%
    st_transform(crs=27700)

  # "Target" grid squares that are nearest to target points
  haduk_target_grid_squares <-
    haduk_grid_sf[st_nearest_feature(target_points, haduk_grid_sf),] %>%
    select(geometry_index) %>% # keep geometry and geometry_index
    unique()

  geometry_indices <- haduk_target_grid_squares$geometry_index

  haduk_target_stars <-
    filter(first_nc_stars, geometry %in% geometry_indices)

  for (i in 2:length(filenames)) {

    haduk_nc_to_add <- read_haduk_nc_as_stars(filenames[i], variable=weather_var)
    haduk_nc_to_add_target <- filter(haduk_nc_to_add, geometry %in% geometry_indices)
    haduk_target_stars <- c(haduk_target_stars, haduk_nc_to_add_target)

  }

  # # To rasterize if we want to for some reason
  # haduk_target_stars_geometries <- attr(haduk_target_stars, "dimensions")$geometry$values
  # haduk_target_points_stars <- haduk_target_stars
  # attr(haduk_target_points_stars, "dimensions")$geometry$values <-
  #   st_centroid(haduk_target_stars_geometries)
  # haduk_target_raster_stars <- st_sfc2xy(haduk_target_points_stars) # takes a few mins

  # Save object to disk
  saveRDS(haduk_target_stars,
          file=file.path(output_address,
                         paste0(weather_var, "_",
                                grid_scale, "km_",
                                start_year, "-",
                                end_year,
                                "_stars.Rds")),
          compress="xz")

}
