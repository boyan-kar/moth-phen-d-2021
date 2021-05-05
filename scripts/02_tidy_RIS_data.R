
# Script to transform RIS data to tidy format

# Load libraries
library(data.table) # dataframes, wrangling
library(tidyverse) # data wrangling and visualisation
library(arrow) # reading/writing parquet files
library(lubridate) # times and dates
library(sf) # simple features - spatial data

# Use max threads for data.table
setDTthreads(0)

# If the working directory is /scripts, set it to
# the parent directory (main dir of repo)
setwd(gsub("/scripts$", "", getwd()))

# Load abundance data
RIS_moth_data <- 
  read_parquet("data/RIS_raw_as_parquet/moth_catches_selected_species.parquet")
# load data
setDT(RIS_moth_data)
# convert to data.table
RIS_moth_data[, year := as.integer(lubridate::year(CalDate))]
# extract year as a separate column
setnames(RIS_moth_data, function(x) sub("-", "_", x))
# change dashes in column names to underscores


# RIS data codes ----

# Explore the codes
# unique(RIS_moth_data[, .(Code_Agassiz, Code_RISgeneric, binomial, RIS_TrapCode, TrapName)])
# unique(RIS_moth_data[, .(Code_Agassiz, Code_RISgeneric, binomial)])
# unique(RIS_moth_data[, .(RIS_TrapCode, TrapName)])
# unique(RIS_moth_data[, .(RIS_TrapCode)])
# unique(RIS_moth_data[, .(TrapName)])

# Each RIS_TrapCode is matched by exactly one TrapName. Will use RIS_TrapCode for abundance table.

# unique(RIS_moth_data[, .(CountType)])
# Only "Total" count type so can be removed

# Species codes
RIS_species_codes <- unique(RIS_moth_data[, .(Code_Agassiz, Code_RISgeneric, binomial)])
# Trap codes
RIS_trap_codes <- unique(RIS_moth_data[, .(RIS_TrapCode, TrapName)])
# Trap names only
RIS_trap_names <- unique(RIS_moth_data[, .(TrapName)])
# Trap numbers only
RIS_trap_numbers <- unique(RIS_moth_data[, .(RIS_TrapCode)])

# Remove species codes, CountType, TrapName from datatable
RIS_moth_data[, c("Code_Agassiz", "Code_RISgeneric", "CountType", "TrapName"):=NULL]

# Convert binomial to factor
RIS_moth_data[, `:=`(binomial = factor(binomial))]

# Fill dates 0s
# Years that each species has been recorded, each trap
species_traps_all_years_with_records <- 
  RIS_moth_data[, .(RIS_TrapCode, binomial, year)] %>% 
  unique()

species_traps_years_only <-
  unique(species_traps_all_years_with_records$year)

unique(species_traps_all_years_with_records$year)

# Data.table of all dates
dates_all_years <- seq(dmy(paste("01 Jan", min(species_traps_years_only))),
                       dmy(paste("31 Dec", max(species_traps_years_only))),
                       by = "days")

dates_all_years <- data.table(CalDate = dates_all_years,
                              year = year(dates_all_years))

# Left join
species_traps_all_dates <- dates_all_years[species_traps_all_years_with_records,
                                           on = "year", allow.cartesian = TRUE]

# data.table with 0 observations for all dates
species_traps_all_dates[, `:=`(DailyCount = 0L, DaysForCount = 1L)]

# Bind with RIS data
RIS_moth_data <- rbind(RIS_moth_data, species_traps_all_dates)
remove(species_traps_all_dates)

# Sort RIS_moth_data descending by abundance so that 0s can be removed easily
# where an abundance observation is present
setorder(RIS_moth_data, binomial, RIS_TrapCode, CalDate, -DailyCount)

# Remove 0s where an observation is present
RIS_moth_data <- distinct(RIS_moth_data, binomial, RIS_TrapCode, CalDate,
                          .keep_all = TRUE)


# Inoperative reports  ----
# Load inoperative reports

inop_reports <- 
  read_parquet("data/RIS_raw_as_parquet/inoperative_reports.parquet")
setDT(inop_reports)

# Explore

# unique(inop_reports[, .(InopType, EventType)])
# Only one kind of InopType and EventType (-1, Inoperative)

# Remove CalDay, InopType, EventType columns (Trap, Year, Date remain)
inop_reports <- inop_reports[, .(Trap, Year, Date)]

inop_reports <- RIS_trap_codes[inop_reports,
                               on = .(TrapName = Trap),
                               allow.cartesian = TRUE] %>% 
  .[is.na(RIS_TrapCode) == FALSE, ] 
  # remove traps that don't appear in abundance data
inop_reports[, TrapName := NULL] # remove trap name column

# Left join with species_traps_all_years_with_records
species_traps_all_inoperative <- inop_reports[species_traps_all_years_with_records,
                                              on = .(RIS_TrapCode = RIS_TrapCode, Year = year),
                                              allow.cartesian = TRUE]

remove(inop_reports)

species_traps_all_inoperative <- species_traps_all_inoperative[is.na(Date) == FALSE, ]

# Add columns to match RIS_data
species_traps_all_inoperative[, `:=`(DailyCount = -1L, DaysForCount = 1L)]
setnames(species_traps_all_inoperative, "Year", "year")
setnames(species_traps_all_inoperative, "Date", "CalDate")

RIS_moth_data <- rbind(RIS_moth_data, species_traps_all_inoperative)
remove(species_traps_all_inoperative)

# Sort RIS_moth_data descending by abundance so that -1s can be removed easily
# where an abundance observation or a 0 is present
setorder(RIS_moth_data, binomial, RIS_TrapCode, CalDate, DailyCount)

# Remove 0s where an observation is present
RIS_moth_data <- distinct(RIS_moth_data, binomial, RIS_TrapCode, CalDate,
                          .keep_all = TRUE)

# Remove 0s within "aggregated" days  ----
# When the trap was run for several days before collecting
# (e.g. day 1: abundance 0, day 2: abundance 0, day 3: abundance 5 DaysForCount 3)
# Day 1 and Day 2 rows should be removed

# Add index column for easier manipulation
index_col <- 1:nrow(RIS_moth_data)
RIS_moth_data <- cbind(RIS_moth_data, index_col)
remove(index_col)

aggregated_RIS <- RIS_moth_data[DaysForCount > 1, ]

rows_to_remove_all <- aggregated_RIS[FALSE, ]
rows_to_remove_all[, index_col := NULL]

for (row_n in 1:nrow(aggregated_RIS)) {
  
  aggregated_row <- aggregated_RIS[row_n, ]
  
  days_for_count <- aggregated_RIS[row_n, ]$DaysForCount
  date_of_aggregate <- aggregated_RIS[row_n, ]$CalDate
  
  first_day_to_remove <- date_of_aggregate - days(days_for_count) + days(1)
  last_day_to_remove <- date_of_aggregate - days(1)
  days_to_remove <- lubridate::as_date(first_day_to_remove:last_day_to_remove)
  
  rows_to_remove <- data.table(binomial = aggregated_row$binomial,
                               RIS_TrapCode = aggregated_row$RIS_TrapCode,
                               CalDate = days_to_remove,
                               DailyCount = 0L,
                               DaysForCount = 1L,
                               year = aggregated_row$year)
  
  rows_to_remove_all <- rbind(rows_to_remove_all, rows_to_remove, fill = TRUE)
  
}


rows_to_remove_all_withIndex <- RIS_moth_data[rows_to_remove_all,
                                               on = .(binomial = binomial,
                                                      RIS_TrapCode = RIS_TrapCode,
                                                      CalDate = CalDate)]

# Check if all matched rows had 0 abundance in RIS_moth_data
# unique(rows_to_remove_all_withIndex$i.DailyCount)
# yes

# Get the indexes
aggr_indexes_to_remove <- rows_to_remove_all_withIndex$index_col

# Remove the rows
RIS_moth_data <- RIS_moth_data[-aggr_indexes_to_remove, ]

# Remove year column before saving
RIS_moth_data[, `:=`(year = NULL, index_col = NULL)]

write_parquet(RIS_moth_data, "data/RIS_tidy_parquet/RIS_moths_tidy.parquet")


# Save trap coordinates as geopackage shapefile ----
# Load coordinates
RIS_light_traps_all <- 
  read_parquet("data/RIS_raw_as_parquet/light_traps_spatiotemporal_data.parquet")

# Keep only traps from the abundance data
RIS_light_traps <- 
  RIS_light_traps_all[RIS_trap_codes, on=.(site_no = RIS_TrapCode)]

# Check where codes were the same but the names do not match exactly
RIS_light_traps[site_name != TrapName, .(site_no, site_name, TrapName)]
# Seems they are all the same

# Check where latitude or longitude are missing
RIS_light_traps[is.na(latitude) | is.na(longitude), ]

# Check where sites in the abundance data were not matched
RIS_light_traps[is.na(site_name), ]

# sites 805-808 (different Merlewood traps) 
# do not exist in light trap spreadsheet, though an original Merlewood site does: 
RIS_light_traps[site_name == "Merlewood", ]
# Merlewood traps without available metadata (4 traps) will be excluses

RIS_light_traps <- RIS_light_traps[is.na(latitude) == FALSE & is.na(longitude) == FALSE, ]
RIS_light_traps[is.na(latitude) | is.na(longitude), ]

RIS_light_traps_sf <- st_as_sf(RIS_light_traps, coords = c("longitude", "latitude"),
                               crs = 4326,
                               na.fail = FALSE)

# Important: Trap names differ in the trap data and the moth data spreadsheet.
# In the tidy light traps data, the name from the moth data is TrapName.
# There are also the four Merlewood traps which have a number but no match in the
# light trap data.

st_write(obj = RIS_light_traps_sf,
         dsn = "data/RIS_tidy_parquet/RIS_light_traps.gpkg",
         delete_dsn = TRUE)

