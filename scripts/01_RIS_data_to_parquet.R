
# Script to convert RIS data from csv/excel to parquet format
# Much more efficient for storage and reading of large files
# This script doesn't change the data

# Load libraries ----
library(data.table) # very fast dataframes
library(tidyverse) # data wrangling and visualisation
library(readxl) # reading Excel files
library(arrow) # reading/writing parquet files
library(lubridate) # times and dates

setDTthreads(0) # Set maximum available threads to be used by data.table

# If the working directory is /scripts, set it to
# the parent directory (main dir of repo)
setwd(gsub("/scripts$", "", getwd()))

# Create directory for parquet files if it doesn't exist
ifelse(!dir.exists("data/RIS_raw_as_parquet"),
       dir.create("data/RIS_raw_as_parquet"), FALSE)

# Moth trap catches ----
# Load abundance data

# Light-trap moth abundance data from RIS
RIS_moth_data <-
  fread("data/RIS_raw_files/1960_to_2018_selected_species_ESW.csv") %>%
  # load data
  .[, CalDate := lubridate::dmy(CalDate)]
  # convert to Date type

# Reorder by trap name, then species, then calendar date
setorder(RIS_moth_data, TrapName, binomial, CalDate)

# Save to parquet format
write_parquet(RIS_moth_data, "data/RIS_raw_as_parquet/moth_catches_selected_species.parquet")

# RIS light-trap inoperative reports ----
# Consists of a .csv file for each year
# - Get list of files
inop_reports_files <- list.files(path = "data/RIS_raw_files/inoperative_reports",
                                 pattern = NULL)

# - Read first file into a data.table
inop_reports <- fread(paste0("data/RIS_raw_files/inoperative_reports/", inop_reports_files[1]))
# - Read the rest and bind to the data.table
for (i in 2:length(inop_reports_files)) {
  inop_reports <- rbind(inop_reports,
                        fread(paste0("data/RIS_raw_files/inoperative_reports/",
                                     inop_reports_files[i])))
}

inop_reports[, Date := lubridate::dmy(Date)] # convert date to Date type

# - Reorder by trap, date
setorder(inop_reports, Trap, Date)

# Save inoperative reports to parquet format
write_parquet(inop_reports, "data/RIS_raw_as_parquet/inoperative_reports.parquet")

# Moth trap locations ----

# Load light trap metadata
moth_traps <-
  read_excel("data/RIS_raw_files/Light_trap_locations_and_spatio-temporal_data_a2.xlsx",
             range = "A1:Q624")

setDT(moth_traps) # convert to data.table

# Rewrite column names
setnames(moth_traps, new = c("site_no", "site_name", "grid_ref",
                             "latitude", "longitude", "VC_no",
                             "altitude", "primary_landcover", "secondary_landcover",
                             "start1", "stop1", "start2",
                             "stop2", "start3", "stop3",
                             "start4", "stop4"))

# Save to parquet
write_parquet(moth_traps,
              "data/RIS_raw_as_parquet/light_traps_spatiotemporal_data.parquet")
