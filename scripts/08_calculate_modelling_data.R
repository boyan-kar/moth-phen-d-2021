
# Script to calculate moth data for models
#

library(data.table) # dataframes, wrangling
library(tidyverse) # data wrangling and visualisation
library(furrr) # apply functions to elements of lists with parallel computing
library(lubridate) # handling dates and times
library(arrow) # for reading/writing parquet files (compressed tabular data)
# library(sf) # geospatial data
library(units) # for handling units
library(microbenchmark)
library(Hmisc) # weighted median

# Use max threads for data.table and futures
setDTthreads(0)
# if (Sys.info()['sysname'] == "Windows") {plan(multisession)} else {plan(multicore)}

# If the working directory is /scripts, set it to
# the parent directory (main dir of repo)
setwd(gsub("/scripts$", "", getwd()))

# Functions ----
source("scripts/00_functions.R")

# Load data ----

# Moth abundance data
moths <-
  open_dataset("data/RIS_transformed/parquet") %>%
  select(binomial, grid_cell_no, site_no, brood_start_yr, ordinal_day, moth_count) %>%
  filter(moth_count > 0, binomial != "Smerinthus ocellatus") %>%
  # excluding eyed hawk-moth due to not enough data
  # (only 1 grid cell and a few records)
  collect() %>%
  droplevels()
setDT(moths)

# Set weather period for tw analysis
weather_period <- 366

# Eliminate AGPs with too many inoperative days ----
moths_all <- copy(moths) # data before filtering
moths <- copy(moths_all) # for quick re-running


moths_tw_period <-
  copy(moths_all) %>%
  .[, tw_last_close := round(weighted.mean(ordinal_day, moth_count)),
    by = .(binomial)] %>%
  .[,.(binomial, tw_last_close)] %>%
  unique() %>%
  .[, tw_first_open := tw_last_close - weather_period + 1]

# Calculate 95% ordinal day range for each species
# (Range of ordinal days within which 95% of moths of the species
# are caught.)
moths_od_95_range <-
  copy(moths_all) %>%
  .[, `:=`(lower = Hmisc::wtd.quantile(ordinal_day, moth_count,
                                       probs = 0.025,
                                       normwt = FALSE),
           upper = Hmisc::wtd.quantile(ordinal_day, moth_count,
                                       probs = 0.975,
                                       normwt = FALSE)),
    by = list(binomial)] %>%
  .[, .(binomial, lower, upper)] %>%
  unique() %>%
  .[, range_length_incl := upper - lower + 1]

# Load all data within the range for each species
moths_full_inrange <-
  moths_od_95_range %>%
  split(by = "binomial") %>% # split into list by binomial
  map(function(x) {

    # Function to load the data for each element of the list (i.e. each species).
    species <- x$binomial[1]
    days_vector <- (x$lower[1]):(x$upper[1])

    inrange_dt <-
      open_dataset("data/RIS_transformed/parquet") %>%
      select(binomial, grid_cell_no, site_no, brood_start_yr, ordinal_day, moth_count) %>%
      # only keep needed columns
      filter(binomial == species) %>%
      # only keep data for that species
      collect() %>%
      as.data.table() %>%
      droplevels() %>%
      select(-binomial) # remove binomial column (as it is already in list element names)

    inrange_dt <- inrange_dt[ordinal_day %in% days_vector, ]

    return(inrange_dt)

  }) %>%
  rbindlist(fill = TRUE, idcol = "binomial")

# Recode missing values as NAs
moths_full_inrange[, moth_count := ifelse(moth_count == -1, NA, moth_count)]

# Summarise number, proportion of NA records in the 95% OD-range
moths_r_missing_summary <-
  copy(moths_full_inrange)[, .(agp_r_nrows = .N, agp_r_na = sum(is.na(moth_count))),
                           by = list(binomial, grid_cell_no, brood_start_yr)][
                             , agp_r_prop := agp_r_na/agp_r_nrows
                           ][
                             , agp_r_prop_10perc_ceiling := ceiling(agp_r_prop*10)/10
                           ]

moths_r_missing_props <-
  copy(moths_r_missing_summary)[, .(count = .N), by = "agp_r_prop_10perc_ceiling"][
    , prop := count/sum(count)
  ] %>%
  setorderv(cols = "agp_r_prop_10perc_ceiling")


# Eliminate AGPs with >10% missing data.
moths_elim_10perc_nas <-
  copy(moths_r_missing_summary) %>%
  .[agp_r_prop > 0.1, ] %>%
  moths[., on = .(binomial, grid_cell_no, brood_start_yr)] %>%
  .[, .(binomial, grid_cell_no, brood_start_yr, agp_r_prop)] %>%
  unique()

# Remove the eliminated AGPs from main dataframe (via anti-join)
moths <-
  moths[!moths_elim_10perc_nas, on = .(binomial, grid_cell_no, brood_start_yr)]


# Calculate mean ODs ----

# Aggregate for each grid cell (sum abundances for multiple traps for each OD, each year)
moths <- moths[, .(moth_count = sum(moth_count)),
               by = list(binomial, grid_cell_no, brood_start_yr, ordinal_day)]

# Calculate total catches per AGP
moths[, agp_total := sum(moth_count),
      by = list(binomial, grid_cell_no, brood_start_yr)]

# Calculate phenology stats
moths_agp <- moths[, .(
  mean_od = weighted.mean(ordinal_day, moth_count),

  mean_od_se = Hmisc::wtd.var(ordinal_day, moth_count, normwt = FALSE)/sqrt(sum(moth_count)),

  median_od = Hmisc::wtd.quantile(ordinal_day, moth_count,
                                  probs = 0.5,
                                  normwt = FALSE)
), by = list(binomial, grid_cell_no, brood_start_yr, agp_total)]

# Check non-finite values produced for SE
unique(moths_agp[!(is.finite(mean_od_se)), ]$mean_od_se)


# Eliminate very high SEs ----
# Replace non-finite values for SE with NA
moths_agp[, mean_od_se := ifelse(is.finite(mean_od_se), mean_od_se, NA)]
glimpse(moths_agp)
moths_agp_before_se_elim <- copy(moths_agp)
moths_agp <- copy(moths_agp_before_se_elim)

# Check what the total AGP abundance was for non-finite SEs
# unique((moths_agp[is.na(mean_od_se), .(agp_total)]))
# All 1s

# Eliminate AGPs with abundance of 1 and/or SE in top 10%
moths_elimination_se_threshold <-
  quantile(moths_agp$mean_od_se[is.finite(moths_agp$mean_od_se)], probs=0.90)
moths_eliminated_se <- moths_agp[mean_od_se > moths_elimination_se_threshold |
                                   agp_total <= 1, ]

# Proportion of AGPs eliminated due to SE relative to all AGPs
nrow(moths_eliminated_se)/nrow(unique(moths_all[, .(binomial, grid_cell_no, brood_start_yr)]))

moths_agp <-
  moths_agp[!moths_eliminated_se, on = .(binomial, grid_cell_no, brood_start_yr)]

moths_agp_eliminated_all <-
  rbind(moths_elim_10perc_nas, moths_eliminated_se, fill = TRUE)

# Merge and save ----

# Save files
write_parquet(x = moths_agp_eliminated_all,
              sink = "data/models/window/moth_data/moths_agp_eliminated_all.parquet")

write_parquet(x = moths_agp,
              sink = "data/models/window/moth_data/moths_agp.parquet")

fwrite(moths_r_missing_props, file = "data/models/window/moth_data/moths_r_missing_props.csv",
       eol = "\n")

fwrite(moths_tw_period, file = "data/models/window/moth_data/moths_tw_period.csv",
       eol = "\n")

fwrite(moths_od_95_range, file = "data/models/window/moth_data/moths_od_95_range.csv",
       eol = "\n")
