
# Script to run single absolute time window analysis
# (Fit mixed models on already existing time windows and mean dates data)

print(paste("Script started:", Sys.time()))

# Load libraries ----
library(data.table) # dataframes, wrangling
library(tidyverse) # data wrangling and visualisation
library(lme4) # linear mixed effects models
library(lmerTest)
library(broom.mixed) # extract model results into tidy tables
library(MuMIn) # Pseudo R Sq values for mixed models
library(arrow) # for reading/writing parquet files (compressed tabular data)
library(units) # for handling units
library(viridis)
# library(lubridate) # handling dates and times
# library(microbenchmark)

setDTthreads(0)

source("scripts/00_functions.R")

# Load data ----
moth_tw <-
  read_parquet("data/models/window/calculated_tw/parquet/sliding_window_data.parquet")

# Transform data ----

# Make a string for each unique time window
moth_tw[, tw_string := str_c("twlen", tw_length, "wo", tw_open, "wc", tw_close)]

# Make a string for each binomial-length combination
moth_tw[, bnml_length := str_c(as.character(binomial), "_", tw_length)]

# Split by species
moth_tw_lst <- split(moth_tw, by = "bnml_length") %>%
  map(~split(., by = "tw_string"))

# print(paste("Model fitting started:", Sys.time()))

weather_vars <- c("temperature", "rainfall")

# Function for time-window analysis for one weather variable
analyse_time_windows <- function(weather_variable, moth_tw_lst, moth_tw) {

  spatial_varname <- paste0(weather_variable, "_spatial")
  temporal_varname <- paste0(weather_variable, "_temporal")

  # Specify formula (dependend on weather variable specified in function args)
  tw_model_formula <- as.formula(
    paste("mean_od ~", spatial_varname, "+", temporal_varname,
          "+ (1|grid_cell_no) + (1|brood_start_yr)", sep = " ")
  )

  # Fit all of the regular models
  moth_tw_models <-
    map(moth_tw_lst, function(x) { # apply to each species
      return(map(x, function(x) { # apply to each time window
        return(lmer(data = x,
                    formula = tw_model_formula,
                    REML = FALSE))
      }))
    })

  # Fit the null models
  moth_tw_null_models <-
    moth_tw[, .(binomial, grid_cell_no, brood_start_yr, mean_od, agp_total)] %>%
    unique() %>% # keep only relevant data (no time windows)
    split(by = "binomial", keep = TRUE) %>% # split by species
    map(function(x) { # apply function to each species
      return(lmer(data = x,
                  formula = mean_od ~ 1 + (1|grid_cell_no) + (1|brood_start_yr),
                  REML = FALSE))
    })

  # Create a summary table for all (non-null) models
  moth_tw_models_summarised <-
    modify_depth(moth_tw_models, .depth = 2, .f = summarise_lmer) %>%
    # apply to each model (depth 2: time window element within species element)
    map(~rbindlist(., fill = TRUE, idcol = "tw_string")) %>%
    # merge for all time windows
    rbindlist(fill = TRUE, idcol = "binomial_tw_length")
    # merge for all species

  # Split binomial_tw_length and tw_string into separate columns
  # for binomial, time window length, open, and close
  moth_tw_models_summarised[, `:=`(
    binomial = str_extract(binomial_tw_length, "^.+(?=_)"),
    tw_length = as.integer(str_extract(binomial_tw_length, "(?<=_)[[:digit:]]+$")),
    tw_open = as.integer(str_extract(tw_string, "(?<=wo).+(?=wc)")),
    tw_close = as.integer(str_extract(tw_string, "(?<=wc).+$"))
  )]

  # Model id string for each model (including binomial and time window)
  moth_tw_models_summarised[, model_id := str_c(weather_variable,
                                                binomial, tw_string,
                                                sep = "_")]
  moth_tw_models_summarised[, binomial_tw_length := NULL]


  # Summarise null models
  moth_tw_null_models_summarised <-
    map(moth_tw_null_models, summarise_lmer) %>% # apply function to each null model
    rbindlist(fill = TRUE, idcol = "binomial") # merge all species into one table

  moth_tw_null_models_summarised[, tw_string := "null"]

  # Model id string for each model (includes binomial and time window)
  moth_tw_null_models_summarised[, model_id := str_c(weather_variable,
                                                     binomial, tw_string,
                                                     sep = "_")]

  tw_analysis_tidy <-
    rbind(moth_tw_models_summarised,
          moth_tw_null_models_summarised,
          fill = TRUE)

  return(tw_analysis_tidy)

}


tw_analysis_tidy <-
  weather_vars %>%
  map(~analyse_time_windows(., moth_tw_lst, moth_tw)) %>%
  rbindlist(fill = TRUE)

# Extract convergence warnings as string and logical columns
tw_analysis_tidy$has_conv_warning <- !(map_lgl(tw_analysis_tidy$wrns, is.null))
tw_analysis_tidy$conv_warn_str <- as.character(tw_analysis_tidy$wrns)
tw_analysis_tidy$wrns <- NULL

# Look at where convergence warnings occur
(tw_analysis_conv_warning_breakdown <-
  copy(tw_analysis_tidy) %>%
  .[, .(binomial, model_id, has_conv_warning)] %>%
  unique() %>%
  .[, .(n_conv_warnings = sum(has_conv_warning),
        n_all = .N),
    by = list(binomial)])

write_parquet(x = tw_analysis_tidy,
              sink = "data/models/window/results/tw_analysis_tidy_all.parquet")

fwrite(tw_analysis_conv_warning_breakdown,
       file = "data/models/window/results/tw_analysis_conv_warning_breakdown.csv",
       eol = "\n")

# Clean up time windows ----

# Remove non-converged ones
tw_analysis_tidy <- tw_analysis_tidy[has_conv_warning == F, ]

# Get weather variable as a separate column

tw_analysis_tidy[, weather_var := str_extract(model_id, "temperature|rainfall")]

# Get lowest AICc for each species, each weather variable
tw_analysis_tidy[, weathervar_spp_min_aicc := min(AICc), by = .(binomial, weather_var)][
  ,is_lowest_aicc := (ifelse(AICc == weathervar_spp_min_aicc, T, F))]

write_parquet(x = tw_analysis_tidy,
              sink = "data/models/window/results/tw_analysis_tidy.parquet")

# tw_analysis_tidy <- read_parquet("data/models/window/results/tw_analysis_tidy.parquet")

# Table with lowest AICc models for each species and weather var
tw_analysis_tidy_lowest_aicc <- tw_analysis_tidy[is_lowest_aicc == T,]

tw_analysis_tidy_lowest_aicc[, is_null := str_detect(model_id, "null")]
table(tw_analysis_tidy_lowest_aicc$is_null)
# None of the best models are null models. Proceed.

# Tidy moth modeling data with best time window
# Each rows is an AGP; contains both temp and weather BEST TWs
tw_best_data <-
  tw_analysis_tidy_lowest_aicc %>%
  copy() %>%
  .[, .(binomial, tw_open, tw_close, weather_var)] %>%
  unique() %>%
  split(by = "weather_var") %>%
  map(function(x, weather_variables_vector = weather_vars) {

    weather_variable <- x$weather_var[1]
    x[, weather_var := NULL] # remove weather var column

    other_weather_vars <- weather_variables_vector[weather_variable != weather_variables_vector]

    # Merge with raw moth data
    x <- moth_tw[x, on = .(binomial = binomial, tw_open = tw_open, tw_close = tw_close)]

    # Vector of weather variables other than in this list element
    other_weather_vars_pattern <- paste(other_weather_vars, collapse = "|")

    # Remove columns for other weather variables
    drop_cols <- grep(other_weather_vars_pattern, colnames(x))
    x[, (drop_cols) := NULL]

    # Only keep needed columns
    kept_cols <- c("binomial", "grid_cell_no", "brood_start_yr", "mean_od",
                   "mean_od_se", "median_od", "agp_total", "tw_string")

    kept_cols_2 <- paste0(weather_variable, "_", c("spatial", "temporal"))

    kept_cols <- c(kept_cols, kept_cols_2)

    x <- x[, ..kept_cols]

    setnames(x, "tw_string", paste0(weather_variable, "_tw_string"))

    setkeyv(x, c("binomial", "grid_cell_no", "brood_start_yr", "mean_od",
                 "mean_od_se", "median_od", "agp_total"))

    return(x)

  })

tw_best_data <- tw_best_data[[2]][tw_best_data[[1]]]  # merge


# Add temperature2 (temperature in rainfall window)
tw_temp2_data <-
  tw_best_data %>%
  copy() %>%
  .[, .(binomial, grid_cell_no, brood_start_yr, rainfall_tw_string)] %>%
  unique()
setnames(tw_temp2_data, "rainfall_tw_string", "temperature_tw_string")

tw_temp2_data <- moth_tw[tw_temp2_data, on=.(binomial = binomial,
                                             grid_cell_no = grid_cell_no,
                                             brood_start_yr = brood_start_yr,
                                             tw_string = temperature_tw_string)]

tw_temp2_data <- tw_temp2_data[, .(binomial, grid_cell_no, brood_start_yr,
                                   temperature_spatial, temperature_temporal)]

setnames(tw_temp2_data, c("temperature_spatial", "temperature_temporal"),
         c("temperature2_spatial", "temperature2_temporal"))


tw_best_data <- tw_best_data[tw_temp2_data,
                             on = .(binomial, grid_cell_no, brood_start_yr)]

# Save best tw data as parquet
write_parquet(x = tw_best_data,
              sink = "data/models/window/results/best_tw_data.parquet")
