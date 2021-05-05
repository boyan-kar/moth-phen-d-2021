
# Script to create tables matching ordinal days, dates and brood years

# Load libraries ----
library(data.table) # dataframes, wrangling
library(tidyverse) # data wrangling and visualisation
library(lubridate) # times and dates

# Setup ---
# If the working directory is /scripts, set it to
# the parent directory (main dir of repo)
setwd(gsub("/scripts$", "", getwd()))

# Load functions ----
source("scripts/00_functions.R")

# Configurations

# Years of moth data that will be used
first_year <- 1961
last_year <- 2018

# Table matching ordinal days to dates for ALL species ----

# $date - calendar date
# $ordinal_day - day 1 is Jan 1st
orddays_table <- 
  data.table(date = seq.Date(from = as.Date(paste0(first_year, "-01-01")),
                             to = as.Date(paste0(last_year, "-12-31")),
                             by = 1))

orddays_table[, `:=`(brood_start_yr = lubridate::year(date),
                     ordinal_day = lubridate::yday(date))]

orddays_table <- 
  copy(orddays_table) %>%
  split(by = "brood_start_yr", keep = TRUE) %>% 
  map(function(x) {
    
    yr <- x$brood_start_yr[1]
    
    previour_yr_diff <- 
      as.numeric(as.Date(paste0(yr, "-01-01")) - as.Date(paste0(yr-1, "-01-01")))
    
    previous_yr_dt <- 
      data.table(date = seq.Date(from = as.Date(paste0(yr - 1, "-01-01")),
                                 to = as.Date(paste0(yr - 1, "-12-31")),
                                 by = 1))
    
    previous_yr_dt[, `:=`(ordinal_day = lubridate::yday(date))][
                            , ordinal_day := ordinal_day - previour_yr_diff
                          ]
    
    next_yr_diff <- 
      as.numeric(as.Date(paste0(yr+1, "-01-01")) - as.Date(paste0(yr, "-01-01")))
    
    next_yr_dt <- 
      data.table(date = seq.Date(from = as.Date(paste0(yr + 1, "-01-01")),
                                 to = as.Date(paste0(yr + 1, "-06-30")),
                                 by = 1))
    
    next_yr_dt[, `:=`(ordinal_day = lubridate::yday(date))][
                            , ordinal_day := ordinal_day + next_yr_diff
                          ]
    
    return_dt <- rbindlist(l = list(previous_yr_dt, x, next_yr_dt),
                              fill = TRUE)
    #is.data.table(return_dt)
    return(return_dt[
      , `:=`(brood_start_yr = as.integer(yr),
             ordinal_day = as.integer(ordinal_day))])
    # return(next_yr_dt)
  }) %>%
  rbindlist(fill = TRUE)
  
# Check if it worked ----

# Check if it worked correctly
(check <-
  orddays_table %>% 
  split(by = "brood_start_yr", keep = TRUE) %>% 
  map(function(x) {
    return(length(unique(x$date)) == nrow(x) &
      length(unique(x$ordinal_day)) == nrow(x) &
      length(unique(diff(x$ordinal_day))) == 1)
  }) %>% 
  as.logical() %>% 
  unique())
# Should return TRUE if each row for each brood year, each row
# has a unique date and ordinal day column is consecutive integers


# Table to convert ordinal days to days of year strings/numbers ----

orddays_table_rev <- orddays_table[brood_start_yr == 1962, .(date, ordinal_day)]
# (non-leap year surrounded by non-leap years)

orddays_table_rev[, `:=`(day_of_month_num = lubridate::day(date),
                         month_num = lubridate::month(date),
                         month_str = format(date, "%b"),
                         month_str_long = format(date, "%B"),
                         date_od_str = format(date, "%d %b"),
                         date_od_str_long = format(date, "%d %B"))]
orddays_table_rev[, month_letter := str_sub(month_str_long, 1, 1)]


# Save to files

fwrite(orddays_table,
       "data/ordinal_days/orddays_table.csv", eol = "\n")


fwrite(orddays_table_rev,
       "data/ordinal_days/orddays_table_rev.csv", eol = "\n")





