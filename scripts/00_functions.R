
# File containing functions that can be loaded from any script via source("scripts/00_functions.R")

library(tidyverse) # data wrangling and visualisation
library(lubridate) # times and dates
library(data.table) # very fast dataframes
library(lme4) # hierarchical (mixed) models
library(lmerTest) # additional information for lme4 models
library(broom) # create tidy tables for models
library(broom.mixed) # tidy tables for mixed models
library(performance) # models performance metrics
library(MuMIn) # models performance metrics

# Function to load moth trait data, add more columns,
# convert some columns to factors
load_moth_traits <- function(only_used = T) {
  
  # Load the table from csv
  moth_traits <- fread("data/moth_traits/moth_traits.csv")
  
  # New column: Common name (Latin binomial)
  moth_traits[, full_name := paste0(binomial, " (", common_name, ")")]
  
  # Convert wintering stage to factor, ordered ovum, larva, chrysalis
  moth_traits[, wintering_stage := factor(wintering_stage,
                                          levels = c("ovum", "larva", "chrysalis"))]
  
  # New column early_or_late with values "early" or "late"
  moth_traits[, early_or_late := ifelse(early_season == T, "early-year", "late-year")]
  
  # Order by wintering stage first, then alphabetically by binomial
  setorderv(moth_traits, c("wintering_stage", "binomial"))
  
  # Convert binomial, common name, full name into factors
  # ordered by wintering stage first, then alphabetically by binomial
  moth_traits <-
    mutate(moth_traits, across(c(binomial, common_name, full_name), as_factor))
  
  # Keep only moths used in the analysis
  if (only_used) {
    moth_traits <- moth_traits[binomial != "Smerinthus ocellatus", ]
  }
  
  return(moth_traits)
}

# Function to create comprehensive summary table for lmerMod objects
# including warning messages (e.g. regarding failure to converge)
summarise_lmer <- function(lmer_model) {
  
  # Extract list of warnings from lmerMod object
  
  warnings_lst <- as.list(lmer_model@optinfo[["conv"]][["lme4"]][["messages"]])
  if (is_empty(warnings_lst)) {warnings_lst <- list(NULL)}
  warnings_tbl <- data.table(wrns = warnings_lst)
  
  # Summary table
  summary_dt <- as.data.table(cbind(broom.mixed::tidy(lmer_model,
                                       conf.int = TRUE,
                                       conf.method = "Wald"), # term statistics
                     glance(lmer_model), # model statistics
                     as.data.table(MuMIn::r.squaredGLMM(lmer_model)), # pseudo R_squared
                     data.table(AICc = MuMIn::AICc(lmer_model)), # AICc
                     data.table(RMSE = performance::performance_rmse(lmer_model)),
                     # data.table(ICC = performance::icc(lmer_model)),
                     warnings_tbl)) # convergence warnings
  
  # Calculate standardized coefficients (by dividing by standard deviation)
  sdy <- sd(getME(lmer_model,"y"))
  sdx <- apply(getME(lmer_model,"X"), 2, sd)
  
  sc <- fixef(lmer_model)*sdx/sdy
  
  conf.intervals <- summary_dt[effect == "fixed", .(term, conf.low, conf.high)]
  conf.intervals[, `:=`(std.conf.low = conf.low*sdx/sdy,
                        std.conf.high = conf.high*sdx/sdy)]
  
  conf.intervals <- conf.intervals[, .(term, std.conf.low, std.conf.high)]
  
  se.fixef <- coef(summary(lmer_model))[,"Std. Error"]
  se <- se.fixef*sdx/sdy
  
  std_coefficients_df <- data.frame(stdcoef=sc, stdse=se)
  std_coefficients_df$term <- rownames(std_coefficients_df)
  std_coefficients_df <- left_join(std_coefficients_df, conf.intervals,
                                   by = "term")
  
  return(left_join(summary_dt, std_coefficients_df, by = "term"))
  
}

## Test summarise function
# data("starwars")
#  
# model1 <- lmer(mass ~ height + birth_year + (1|homeworld), data = starwars)
# model1_summary <- summarise_lmer(model1)

# Function to disaggregate "aggregated data" ----
# (referring to data collected over a few days as "aggregated".
# e.g. 3 moths caught in total in 3 days)
# The function returns same table but with the mean of the aggregate at each day.
disaggregate_days <- function(abundance_dataframe,
                              # dataframe containing daily abundance data
                              daily_count_col = "DailyCount",
                              # column where the daily count is located
                              days_for_count_col = "DaysForCount", 
                              # column where the days for count are located (days)
                              ordinal_day_col = "ordinal_day",
                              # ordinal day integer
                              calendar_date_col = "date"
) {
  
  # Remember if input was a tibble
  is_input_tibble <- is_tibble(abundance_dataframe)
  # Convert to data.table
  setDT(abundance_dataframe)
  
  # Set all current observations as not disaggregated
  abundance_dataframe[, is_disaggr := FALSE]
  
  # If no disaggregation needed, return original
  if (sum(abundance_dataframe$DaysForCount > 1) == 0) {return(abundance_dataframe)}
  
  # Set names to defaults for easier wrangling
  setnames(abundance_dataframe, 
           c(daily_count_col,days_for_count_col, ordinal_day_col, calendar_date_col),
           c("DailyCount", "DaysForCount", "ordinal_day", "CalDate"))
  
  # Take out the aggregated days
  abundance_dt_aggr_days <- abundance_dataframe[DaysForCount > 1, ]
  abundance_dt_notaggr <- abundance_dataframe[DaysForCount == 1, ]
  
  # Empty dt for all disaggregated rows to be added to
  disaggr_all <- abundance_dt_aggr_days[FALSE, ]
  
  # Disaggregate all 
  for (i in 1:nrow(abundance_dt_aggr_days)) {
    
    aggregated_row <- abundance_dt_aggr_days[i, ]
    
    aggregated_row_other_cols <- copy(aggregated_row)
    aggregated_row_other_cols[ , c("DailyCount", "ordinal_day",
                                   "DaysForCount", "is_disaggr",
                                   "CalDate"):=NULL]
    
    total_count <- aggregated_row$DailyCount
    coll_day <- aggregated_row$ordinal_day # collection day (ordinal)
    coll_Date <- aggregated_row$CalDate # collection calendar date
    aggr_length <- aggregated_row$DaysForCount
    
    mean_abundance <- total_count/aggr_length
    
    aggr_days_set <- (coll_day - aggr_length + 1):coll_day
    calendar_days_set <- seq.Date(from = (coll_Date - days(aggr_length) + days(1)),
                                  to = coll_Date,
                                  by = 1)
    
    disaggr_rows <- data.table(ordinal_day = aggr_days_set,
                               DailyCount = mean_abundance,
                               DaysForCount = 1,
                               CalDate = calendar_days_set,
                               date_of_aggr = coll_Date,
                               is_disaggr = TRUE)
    
    disaggr_rows_all_cols <- cbind(aggregated_row_other_cols, disaggr_rows)
    disaggr_all <- rbind(disaggr_all, disaggr_rows_all_cols, fill = TRUE)
    
  }
  
  abundance_dataframe <- abundance_dataframe[!(ordinal_day %in% disaggr_all$ordinal_day), ]

  abundance_dataframe <- rbind(abundance_dataframe, disaggr_all, fill = TRUE)
  
  abundance_dataframe <- abundance_dataframe[order(ordinal_day), ]
  
  
  
  # Set names back to what they were
  setnames(abundance_dataframe,
           c("DailyCount","DaysForCount","ordinal_day", "CalDate"),
           c(daily_count_col, days_for_count_col, ordinal_day_col,
             calendar_date_col)
  )
  
  if (is_input_tibble) {abundance_dataframe <- as_tibble(abundance_dataframe)} 
  return(abundance_dataframe)
  
}

# Function to get a vector of "over_year" species
# Species whose broods are in the winter and overlap with two calendar years
# Therefore brood_start_yr is the first year that the brood occurs
get_over_year_species <- function() {
  require(data.table)
  moth_traits <- fread("data/moth_traits/moth_traits.csv")
  moth_traits[, over_year := as.logical(over_year)]
  return(moth_traits[over_year == TRUE]$binomial)
}
