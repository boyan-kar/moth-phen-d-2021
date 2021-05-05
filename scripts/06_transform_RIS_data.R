
# Script to transform RIS data to "usable" format, ready for modelling.
# 1. ordinal day; 2. "disaggregate"; 3. reorder, rename columns, etc. 4. save

# Load libraries ----
library(data.table) # very fast dataframes
library(tidyverse) # data wrangling and visualisation
library(lubridate) # handling dates and times
library(arrow) # for reading/writing parquet files (compressed tabular data)
library(sf) # simple features - spatial data
library(units) # for handling units

# Use max threads for data.table
setDTthreads(0)

# If the working directory is /scripts, set it to
# the parent directory (main dir of repo)
setwd(gsub("/scripts$", "", getwd()))

# Create directory for parquet files if it doesn't exist
ifelse(!dir.exists("data/RIS_transformed"),
       dir.create("data/RIS_transformed"), FALSE)

# IMPORTANT: Needs to be fixed to work with new tidy file.

# Functions ----
source("scripts/00_functions.R")

# Load data ----

# Load moth abundance data
moth_data <- read_parquet("data/RIS_tidy_parquet/RIS_moths_tidy.parquet")

# Load ordinal days - dates table
ordday_dates_wy <-
  fread("data/ordinal_days/orddays_table.csv")[year(date) == brood_start_yr, ]
# ordday_dates_oy <-
#   fread("data/ordinal_days/orddays_table.csv")[ordinal_day >= 182 & ordinal_day < (182 + 365), ]
ordday_dates_oy <-
  fread("data/ordinal_days/orddays_table.csv")[((year(date) == brood_start_yr) & (month(date) >= 7)) |
                                                 ((year(date) == brood_start_yr + 1) & (month(date) < 7)) , ]

# Load trap data
traps <- st_read("data/RIS_tidy_parquet/RIS_light_traps.gpkg") %>% 
  st_transform(crs = 27700)

# Load hadUK grid
haduk_grid <- st_read("data/hadUK_tidy/haduk_grid.gpkg")

# ORDINAL DAYS  ----

# IMPORTANT
# Removed moth data from 1960 as the weather data used starts in 1960.
moth_data <- moth_data[year(CalDate) > 1960, ]

# Vector of "over-year" species
# Species that have broods in December and January
# Ordinal days will start on July 1
over_year_species <- get_over_year_species()

# To deal with this issue, the following columns will be added
# to the table for all species
# ordinal_day: starting Jan 1 for most species, Jul 1 for over-year spp
# brood_start_yr: the year the brood starts.

# Split the two types into separate data.tables
moths_wy <- moth_data[!(binomial %in% over_year_species), ]
moths_oy <- moth_data[binomial %in% over_year_species, ]
rm(moth_data) # to save memory

# Add ordinal days to moth data here ----
moths_wy <- ordday_dates_wy[moths_wy, on=.(date=CalDate)] # TODO fix this
moths_oy <- ordday_dates_oy[moths_oy, on=.(date=CalDate)]

moth_data <- rbind(moths_wy, moths_oy)
rm(moths_wy, moths_oy)

# Check if any records have not been matched
moth_data[is.na(ordinal_day) & DailyCount > 0, ]
moth_data[is.na(ordinal_day), ] # only first half of 1961 for over-year species
# did not match to ordinal days - as brood starts in 1960 and will not be used.


# Empty. Can filter out unmatched rows (only over_year species: June 30 and
# early 1961 that cannot be used.)
moth_data <- moth_data[!is.na(ordinal_day), ]

# Fix types
moth_data[, `:=`(
  RIS_TrapCode = as.integer(RIS_TrapCode),
  DaysForCount = as.integer(DaysForCount),
  brood_start_yr = as.integer(brood_start_yr),
  ordinal_day = as.integer(ordinal_day)
)]

# Get rid of traps 805-808 (Merlewood traps that there are no spatiotemporal data for,
# only moth records)
moth_data <- moth_data[!(RIS_TrapCode %in% 805:808), ]

# DISAGGREGATE DAYS ----

moth_data <-
  moth_data %>%
  group_by(binomial, RIS_TrapCode, brood_start_yr) %>%
  nest() %>%
  mutate(data = map(data, disaggregate_days)) %>%
  # may take a while
  unnest(data) %>%
  ungroup() %>%
  select(-DaysForCount) %>% 
  setDT()
# This can be re-written to be more efficient.
 
# Trap data ----

# Only keep trap numbers found in the data
traps <- filter(traps, site_no %in% unique(moth_data$RIS_TrapCode))

# Add grid cell number to traps sf
traps <- st_join(traps, haduk_grid, left = TRUE, join = st_within) %>% 
  rename(grid_cell_no.within = grid_cell_no) %>% 
  st_join(haduk_grid, left = TRUE, join = st_nearest_feature) %>% 
  add_column(distance_to_gridcell = st_distance(traps, st_union(haduk_grid)))

# For traps that are not within grid cells: Check the distances
filter(traps, distance_to_gridcell > as_units(0, "m"))
# At most 505m, only 4 traps over 200m from grid cell, others up to 7m

# Add grid cell numbers to moth data
traps_dt <- as.data.table(traps)
traps_dt <- traps_dt[, .(site_no, grid_cell_no)]

moth_data <- traps_dt[moth_data, on = .(site_no = RIS_TrapCode)]

# Rename Smerinthus ocellata to Smerinthus ocellatus
moth_data[, binomial := as.character(binomial)]
moth_data[, binomial := ifelse(binomial == "Smerinthus ocellata",
                               "Smerinthus ocellatus", binomial)]

# Rename DailyCount column to moth_count
setnames(moth_data, "DailyCount", "moth_count")

# Set the order of the columns
setcolorder(moth_data, c("binomial", "grid_cell_no", "site_no", "brood_start_yr",
                         "date", "ordinal_day", "moth_count", "is_disaggr",
                         "date_of_aggr"))

# Sort by some columns
setorderv(moth_data)

# Convert binomial to factor
moth_data[, binomial := as.factor(binomial)]

# Save files
write_parquet(x = moth_data, 
              sink = "data/RIS_transformed/parquet/RIS_transformed.parquet")

st_write(obj = traps,
         dsn = "data/RIS_transformed/light_traps.gpkg",
         delete_dsn = TRUE)
