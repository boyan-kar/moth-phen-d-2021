
# Script to fit different models on the best time windows

# Load libraries ----
library(data.table) # fast dataframes, wrangling
library(tidyverse) # data wrangling and visualisation
library(lme4) # linear mixed effects models
library(broom.mixed) # extract model results into tidy tables
library(MuMIn) # Pseudo R Sq values, AICc for mixed models
library(performance) # Model checks and comparisons
library(arrow) # for reading/writing parquet files (compressed tabular data)
library(units) # for handling units
library(viridis) # colourblind-friendly palettes
library(sf) # Simple features spatial data

setDTthreads(0)

source("scripts/00_functions.R")

# Load data ----

# Data for the models

# AGP data
moth_best_tw_agp <- read_parquet("data/models/window/results/best_tw_data.parquet")

# Moth traits
moth_traits <- load_moth_traits()

# Add latitude to AGP data

haduk_grid_points <-
  st_read(dsn = "data/hadUK_tidy/haduk_grid.gpkg",
          layer="haduk_grid") %>%  # Read grid data
  st_centroid() %>% # Get centroid for each grid cell
  st_transform(crs = 4236) %>% # transform to long-lat 
  cbind(., st_coordinates(.)) %>% # extract long and lat as columns
  as.data.table() %>% 
  select(grid_cell_no, latitude = Y)

moth_best_tw_agp <- haduk_grid_points[moth_best_tw_agp,
                                      on = .(grid_cell_no = grid_cell_no)]

# Models ----
# Turn data into list by species
moth_lst <- split(moth_best_tw_agp, by = "binomial")

# Get sample size N (number of AGPs) for each species
moth_n_agp <-
  moth_lst %>% 
  map(., .f = nrow) %>% 
  as.data.table() %>% 
  pivot_longer(cols = everything(), names_to = "binomial", values_to = "n_agps")

# Model formulas
model_formulas <- list(null = formula(mean_od ~ 1 + 
                                        (1|grid_cell_no) + (1|brood_start_yr)),
                       latyear = formula(mean_od ~ latitude + brood_start_yr + 
                                           (1|grid_cell_no) + (1|brood_start_yr)),
                       temperature = formula(mean_od ~
                                               temperature_spatial + temperature_temporal +
                                               (1|grid_cell_no) + (1|brood_start_yr)),
                       rainfall = formula(mean_od ~ rainfall_spatial + rainfall_temporal +
                                            (1|grid_cell_no) + (1|brood_start_yr)),
                       temperature2 = formula(mean_od ~
                                                temperature2_spatial + temperature2_temporal +
                                                (1|grid_cell_no) + (1|brood_start_yr)),
                       temperature.rainfall = formula(mean_od ~ 
                                                        temperature_spatial + temperature_temporal +
                                                        rainfall_spatial + rainfall_temporal + 
                                                        (1|grid_cell_no) + (1|brood_start_yr)))

# Fit the models
moth_mods_best_tw_all <-
  model_formulas %>%
  map(., function(model_formula, dataset_lst = moth_lst) {
    all_models <- map(dataset_lst, ~lmer(formula = model_formula,
                                         data = .,
                                         REML = F))
    return(all_models)
  })


# Summary tables
moth_all_mods_best_tw_summ <-
  moth_mods_best_tw_all %>% 
  modify_depth(., .depth = 2, .f = summarise_lmer) %>% 
  map(~rbindlist(l = ., fill = TRUE, idcol = "binomial")) %>% 
  rbindlist(l = ., fill = TRUE, idcol = "model_fef") %>% 
  left_join(., moth_n_agp, by = "binomial")
moth_all_mods_best_tw_summ[, wrns := as.character(wrns)][, wrns := ifelse(wrns=="NULL", NA, wrns)]

# Save as csv
fwrite(moth_all_mods_best_tw_summ,
       file = "data/models/window/results/moth_all_mods_best_tw_summ.csv", eol = "\n")


# Check multicollinearity

# Temperature + rainfall
temperature_rainfall_vif <- 
  moth_mods_best_tw_all$temperature.rainfall %>% 
  map(., .f = check_collinearity) %>% 
  rbindlist(idcol = "binomial")
max(temperature_rainfall_vif$VIF) # 1.21
