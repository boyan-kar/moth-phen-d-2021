
# Tables

# Libararies
library(data.table)
library(tidyverse)
library(lubridate)
library(flextable)

setDTthreads(0)

# Functions
source("scripts/00_functions.R")

# Load data
moth_traits <- load_moth_traits()
moth_traits_all <- load_moth_traits(only_used = F)

od_ranges_tws <- fread("tables/od_ranges_tws.csv")

orddays_table_rev <- fread("data/ordinal_days/orddays_table_rev.csv")


# Traits for all moths ----
moth_traits_first <-
  moth_traits_all[, .(binomial, common_name, flight_months, wintering_stage)]
setnames(moth_traits_first, old = names(moth_traits_first),
         new = c("Latin name", "Common name", "Adult stage months", "Overwintering stage"))

fwrite(moth_traits_first, "tables/moth_traits_all.csv", eol = "\n")

# Best time windows table ----
od_ranges_tws <-
  left_join(moth_traits[, .(binomial, full_name, wintering_stage)], od_ranges_tws, by = "binomial")
setorder(od_ranges_tws, full_name)
od_ranges_tws[, binomial := as_factor(binomial)]

od_ranges_tws_all <- copy(od_ranges_tws)
od_ranges_tws <- copy(od_ranges_tws_all)

od_ranges_tws <-
  od_ranges_tws %>%
  pivot_longer(., cols = c(lower, upper),
               names_to = "lower_or_upper", values_to = "ordinal_day") %>%
  left_join(orddays_table_rev[, .(ordinal_day, date_od_str)], on = .(ordinal_day = ordinal_day)) %>%
  select(binomial, wintering_stage, range_var, lower_or_upper, date_od_str) %>%
  pivot_wider(names_from = lower_or_upper, values_from = date_od_str) %>%
  mutate(od_range_str = str_c(lower, upper, sep = " - ")) %>%
  select(wintering_stage, binomial, range_var, od_range_str) %>%
  pivot_wider(names_from = range_var, values_from = od_range_str)

fwrite(od_ranges_tws, "tables/od_ranges_tws.csv", eol = "\n")


# Read all models data ----
moth_all_mods_best_tw_summ <-
  fread("data/models/window/results/moth_all_mods_best_tw_summ.csv")

# Merge with traits
moth_all_mods_best_tw_summ <-
  left_join(moth_traits, moth_all_mods_best_tw_summ, by = "binomial")

# Make binomial into a factor ordered correctly (by wintering stage, then alphabetical)
# using the full_name factor as order (as it kept its order when merging)
setorder(moth_all_mods_best_tw_summ, full_name)

moth_all_mods_best_tw_summ[, `:=`(model_fef = ifelse(model_fef == "temperature.rainfall",
                                                     "temperature + rainfall", model_fef))]
moth_all_mods_best_tw_summ[, `:=`(binomial = as_factor(binomial),
                                  model_fef = factor(model_fef,
                                                     levels = c("null",
                                                                "latyear",
                                                                "temperature",
                                                                "rainfall",
                                                                "temperature + rainfall",
                                                                "temperature2")))]
setorder(moth_all_mods_best_tw_summ, wintering_stage, binomial, model_fef)

# Prettify / rename terms for plotting
moth_all_mods_best_tw_summ[, term_pretty := case_when(
  term == "(Intercept)" ~ "Intercept",
  term == "latitude" ~ "Latitude",
  term == "brood_start_yr" ~ "Year",
  term == "temperature_spatial" ~ "Temperature (spatial)",
  term == "temperature_temporal" ~ "Temperature (temporal)",
  term == "rainfall_spatial" ~ "Rainfall (spatial)",
  term == "rainfall_temporal" ~ "Rainfall (temporal)",
  term == "temperature2_spatial" ~ "Temperature2 (spatial)",
  term == "temperature2_temporal" ~ "Temperature2 (temporal)",
  term == "sd__(Intercept)" ~ "St.Dev. (Intercept)",
  term == "sd__Observation" ~ "St.Dev. Observation",
  TRUE ~ term)]

moth_all_mods_best_tw_summ[, group_pretty := case_when(
  group == "brood_start_yr" ~ "Year",
  group == "grid_cell_no" ~ "Grid-cell",
  TRUE ~ group)]

# Add significance level string
moth_all_mods_best_tw_summ[, p_value_string := case_when(
  p.value < 0.001 ~ "***",
  p.value < 0.01 ~ "**",
  p.value < 0.05 ~ "*",
  TRUE ~ ""
)][, is_significant_05 := ifelse(p.value < 0.05, T, F)]

# AICc table, ALL models, ALL species ----

moth_all_mods_aic <-
  moth_all_mods_best_tw_summ[, .(binomial, model_fef, AICc)] %>%
  unique() %>%
  dcast(., binomial ~ model_fef, value.var = "AICc") %>%
  setcolorder(., c("binomial", "null", "rainfall", "latyear", "temperature",
                   "temperature + rainfall", "temperature2")) %>%
  select(Species = binomial, Null = null,
         "Latitude + Year" = latyear, Temperature = temperature,
         Rainfall = rainfall, "Temperature + Rainfall" = "temperature + rainfall",
         "Temperature (in RF window)" = temperature2)

# Save as csv
fwrite(moth_all_mods_aic, "tables/moth_all_mods_aic.csv",
       eol = "\n")

# Get only models to present in report appendix tables ----
# (lat + year for all species and best weather model for each species)
moth_models_present_latyear <-
  moth_all_mods_best_tw_summ[model_fef == "latyear", ]

moth_models_present_bestweather <-
  moth_all_mods_best_tw_summ[model_fef %in% c("temperature",
                                              "rainfall",
                                              "temperature + rainfall")]
moth_models_present_bestweather[, lowest_weather_AICc := min(AICc),
                                by = .(binomial)]

moth_models_present_bestweather <-
  moth_models_present_bestweather[(AICc - lowest_weather_AICc) < 2, ]

moth_models_present <-
  rbind(moth_models_present_latyear, moth_models_present_bestweather, fill = T)

moth_models_present[, model_fef_pretty := case_when(
  model_fef == "null" ~ "Null",
  model_fef == "latyear" ~ "Latitude + Year",
  model_fef == "temperature" ~ "Temperature",
  model_fef == "rainfall" ~ "Rainfall",
  model_fef == "temperature + rainfall" ~ "Temperature + Rainfall")]

moth_models_present[, model_fef_pretty :=
                      factor(model_fef_pretty,
                             levels = c("Latitude + Year",
                                        "Temperature",
                                        "Temperature + Rainfall"))]

# Moth models appendix table - model summary statistics ----
moth_models_present_stats <-
  moth_models_present[, .(binomial, model_fef_pretty,
                                      df.residual, R2m, R2c, RMSE, n_agps)] %>%
  unique()

moth_models_present_stats <-
  moth_models_present_stats %>%
  mutate(across(c(df.residual, R2m, R2c, RMSE, n_agps), .fns = ~round(., digits = 2)))

setorder(moth_models_present_stats, binomial, model_fef_pretty)
setnames(moth_models_present_stats,
         c("Latin Name", "Model", "Residual DFs",
           "R2-marginal", "R2-conditional", "RMSE",
           "N (number of AGPs)"))
fwrite(moth_models_present_stats, "tables/moth_models_present_stats.csv")

# Save as word document
moth_models_present_stats_flex <-
  flextable(moth_models_present_stats) %>%
  merge_v(j = c("Latin Name", "Model"))

save_as_docx(moth_models_present_stats_flex, path =
               "tables/moth_models_present_stats_flex.docx")

# Moth models appendix table - model TERMS ----
moth_models_present_terms <-
  moth_models_present[, .(binomial, model_fef_pretty, term_pretty,
                          df.residual, R2m, R2c, RMSE, n_agps)] %>%
  unique()

# Includes a row for each model term, including confidence interval and p-value
moth_models_present_terms <-
  moth_models_present[effect=="fixed", .(binomial, model_fef_pretty, term_pretty,
                                         estimate, conf.low, conf.high, p.value, df)]

moth_models_present_terms <-
  moth_models_present_terms %>%
  mutate(across(c(estimate, conf.low, conf.high, df), .fns = ~round(., digits = 2)))

moth_models_present_terms[, p.value := ifelse(p.value < 0.001, "<0.001",
                                              as.character(round(p.value, digits = 3)))]

moth_models_present_terms[, CI := str_c(conf.low, " - ", conf.high)]

# Remove intercepts
moth_models_present_terms <- moth_models_present_terms[term_pretty != "Intercept", ]

# Select/reorder cols
moth_models_present_terms <- moth_models_present_terms[
  , .(binomial, model_fef_pretty, term_pretty, estimate, CI, df, p.value)
]

# Shorten model fixed effect string
moth_models_present_terms[, model_fef_pretty :=
                            fct_recode(model_fef_pretty,
                                       "L+Y" = "Latitude + Year",
                                       "T" = "Temperature",
                                       "T+R" = "Temperature + Rainfall")]

# Order effects
moth_models_present_terms[, term_pretty := factor(term_pretty,
                                                  levels = c("Latitude",
                                                             "Year",
                                                             "Temperature (spatial)",
                                                             "Temperature (temporal)",
                                                             "Rainfall (spatial)",
                                                             "Rainfall (temporal)"))]
# Rename columns
setnames(moth_models_present_terms,
         c("Latin Name", "Model", "Effect",
           "Estimate", "CI (95%)", "DF",
           "p"))

# Order tables by columns
setorderv(moth_models_present_terms,
          c("Latin Name", "Model", "Effect"))

# Create flextable
moth_models_present_terms_flex <-
  flextable(moth_models_present_terms) %>%
  merge_v(j = c("Latin Name", "Model"))

# Save as word document
save_as_docx(moth_models_present_terms_flex, path =
               "tables/moth_models_present_terms_flex.docx")
