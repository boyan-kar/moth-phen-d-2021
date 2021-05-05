
# Script to calculate time windows from time-window analysis

print(paste("Script started:", Sys.time()))

# Load libraries ----
library(data.table) # dataframes, wrangling
library(tidyverse) # data wrangling and visualisation
library(lubridate) # handling dates and times
library(arrow) # for reading/writing parquet files (compressed tabular data)
library(units) # for handling units
library(microbenchmark)
library(Hmisc) # weighted median

# Use max available threads for data.table
setDTthreads(0)

# If the working directory is /scripts, set it to
# the parent directory (main dir of repo)
setwd(gsub("/scripts$", "", getwd()))

# Functions ----
source("scripts/00_functions.R")

# Settings ----
weather_period <- 366 # (days)
tw_lengths <- c(30, 40, 50, 60) # length of time windows in days
tw_gap <- c(5, 5, 5, 5) # calculate time window every x days

# Load data ----

# Weather data
haduk_weather <-
  read_parquet("data/hadUK_tidy/parquet/haduk_all_per_gridcellno.parquet")
setDT(haduk_weather)

# Calculate daily mean temperature (mean of daily mean and daily max)
haduk_weather <-
  haduk_weather[, temp := (tasmax + tasmin) / 2][
    , .(grid_cell_no, date, temp, rainfall)] # onlt keep these 3 cols

# Moth AGP data
moths_agp <- read_parquet("data/models/window/moth_data/moths_agp.parquet")

# Get the moths tw period
moths_tw_period <- fread("data/models/window/moth_data/moths_tw_period.csv")

# Ordinal days - dates table
ordday_dates <- fread("data/ordinal_days/orddays_table.csv")

moths_agp_lst <-
  split(moths_agp, by = "binomial", keep = TRUE)

# Create a list of species, with data.table elements with binomial,
# grid cell number, brood start year, and last close and first open ordinal days
# for the time windows for the species.
bnml_gcll_yr_tw_lst <- split(unique(moths_agp[, .(binomial, grid_cell_no, brood_start_yr)]),
                          by = "binomial", keep = TRUE) %>%
  map(., ~left_join(., moths_tw_period, by = "binomial"))

# Create a list with relevant weather data for each species.
bnml_weather <- bnml_gcll_yr_tw_lst %>%
  map(., function(x) {

    # vector of all ordinal days interested
    all_orddays_climate <- x$tw_first_open[1]:x$tw_last_close[1]
    x <- x[, .(binomial, grid_cell_no, brood_start_yr)]
    x[, ordinal_day := rep(list(all_orddays_climate), nrow(x))]
    x <- x[, list(ordinal_day = as.integer(unlist(ordinal_day))),
           by = list(binomial, grid_cell_no, brood_start_yr)]

    # Add calendar dates to the ordinal days depending on whether the species
    # is over_year (ordinal day 1 is July 1) or within_year (Jan 1)
    x <- ordday_dates[x, on = .(brood_start_yr = brood_start_yr,
                                ordinal_day = ordinal_day)]
    # Add weather

    x <- haduk_weather[x, on = .(date = date, grid_cell_no = grid_cell_no)]

    # Calculate mean temperature for each grid cell
    # x[, temperature_mean_gridc := mean(temp), by = list(grid_cell_no)]

    # Keep only relevant columns
    x <- x[, .(binomial, grid_cell_no, brood_start_yr, ordinal_day,
               temp, rainfall)]

    return(x)

  }) # takes 10s or so

# List of all time windows that need to be calculated
bnml_tw <-
  bnml_weather %>%
  map(., function(x) {

    # Goal: data frame with time window periods listed.
    # $tw_length
    # $tw_open
    # $tw_close
    # $tw_string

    tw_period_firstday <- min(x$ordinal_day)
    tw_period_lastday <- max(x$ordinal_day)

    tw_dt <- data.table(tw_length = integer(0),
                        tw_open = integer(0),
                        tw_close = integer(0)
                        # tw_string = character(0)
                        )

    for (i in 1:length(tw_lengths)) {
      tw_length_i <- tw_lengths[i]
      tw_close_days <- seq.int(from = tw_period_lastday,
                               to = tw_period_firstday + tw_length_i - 1,
                               by = -tw_gap[i])
      tw_open_days <- tw_close_days - tw_length_i + 1
      tw_dt_i <- data.table(tw_length = tw_length_i,
                            tw_close = tw_close_days,
                            tw_open = tw_open_days)
      tw_dt <- rbindlist(list(tw_dt, tw_dt_i),
                         use.names = TRUE)

    }

    return(tw_dt)

  })

# If true, elements in the two list match (same order)
unique(names(bnml_tw) == names(bnml_weather))

# Calculate mean temperatures for each of the time windows
moths_tw_temperature_list <-
  map2(bnml_tw, bnml_weather, function(tw_dt, weather_dt) {

    # Create an empty list to add results to
    result_lst <- vector(mode = "list", length = 0)

    # Run for each time window
    for (i in 1:nrow(tw_dt)) {

      tw_open <- tw_dt[i]$tw_open
      tw_close <- tw_dt[i]$tw_close
      tw_length <- tw_dt[i]$tw_length

      weather_filtered <- unique(
          weather_dt[ordinal_day >= tw_open & ordinal_day <= tw_close, ][

            , `:=`(temperature_tw_year = mean(temp), rainfall_tw_year = mean(rainfall)),
            by = list(binomial, grid_cell_no, brood_start_yr)

          ][
            , `:=`(temperature_spatial = mean(temp), rainfall_spatial = mean(rainfall)),
            by = list(binomial, grid_cell_no)

          ][
		    , .(binomial, grid_cell_no, brood_start_yr,
		        temperature_tw_year, rainfall_tw_year,
		        temperature_spatial, rainfall_spatial)

          ][
		    , `:=`(tw_open = as.integer(tw_open),
               tw_close = as.integer(tw_close),
               tw_length = as.integer(tw_length))]
      )

      result_lst[[i]] <- weather_filtered

    }

    return(rbindlist(result_lst))
  }
)
# should only take a few minutes at most


# Turn from list into one data.table
moths_tw_weather <- rbindlist(moths_tw_temperature_list)

# Calculate within-subject mean-centered temperature for the time window
# (Time window grid cell mean temperature (degrees C) - Grid cell mean temperature
# over all years (degrees C))
moths_tw_weather[, `:=`(temperature_temporal =
                          as.numeric(temperature_tw_year - temperature_spatial),
                        rainfall_temporal = as.numeric(rainfall_tw_year - rainfall_spatial))]

# Merge with mean dates
moths_tw_weather <- moths_agp[moths_tw_weather,
                              on = .(binomial = binomial,
                                     grid_cell_no = grid_cell_no,
                                     brood_start_yr = brood_start_yr)]

# Reorder columns
setcolorder(moths_tw_weather,
            c("binomial", "grid_cell_no", "brood_start_yr",
              "mean_od", "mean_od_se", "median_od",
              "agp_total",
              "tw_open", "tw_close", "tw_length",
              "temperature_tw_year", "temperature_spatial", "temperature_temporal",
              "rainfall_tw_year", "rainfall_spatial", "rainfall_temporal"))

# Check correlation between temperature and rainfall
# moths_tw_weather <- read_parquet("data/models/window/calculated_tw/parquet/sliding_window_data.parquet")
tw_weather_cor <-
  moths_tw_weather %>%
  copy() %>%
  .[, .(grid_cell_no, temperature_spatial, temperature_temporal,
        rainfall_spatial, rainfall_temporal)] %>%
  unique() %>%
  .[, .(cor_spatial = cor(temperature_spatial, rainfall_spatial),
        cor_temporal = cor(temperature_temporal, rainfall_temporal))]
# TODO Fix this.



# Save as parquet file ----
write_parquet(x = moths_tw_weather,
              sink = "data/models/window/calculated_tw/parquet/sliding_window_data.parquet")
