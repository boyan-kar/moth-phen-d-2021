

# Script to visualize final models

# Libraries ----
library(data.table) # fast dataframes, wrangling
library(tidyverse) # data wrangling and visualisation
library(scales) # addon for ggplot
library(ggrepel) # addon for ggplot
library(arrow) # read parquet files

setDTthreads(0) # max threads for data.table

# Load functions
source("scripts/00_functions.R")

# Load data
moth_all_mods_best_tw_summ <-
  fread("data/models/window/results/moth_all_mods_best_tw_summ.csv")

moth_traits <- load_moth_traits()

# Merge

moth_all_mods_best_tw_summ <-
  left_join(moth_traits, moth_all_mods_best_tw_summ, by = "binomial")

# Make binomial into a factor ordered correctly (by wintering stage, then alphabetical)
# using the full_name factor
setorder(moth_all_mods_best_tw_summ, full_name)
moth_all_mods_best_tw_summ[, `:=`(binomial = as_factor(binomial),
                                  model_fef = factor(model_fef,
                                                     levels = c("null",
                                                                "latyear",
                                                                "temperature",
                                                                "rainfall",
                                                                "temperature.rainfall",
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

# Add significance level string
moth_all_mods_best_tw_summ[, p_value_string := case_when(
  p.value < 0.001 ~ "***",
  p.value < 0.01 ~ "**",
  p.value < 0.05 ~ "*",
  TRUE ~ ""
)][, is_significant_05 := ifelse(p.value < 0.05, T, F)]

# Colour palettes
wintering_stage_colours <- c("ovum" = "#785EF0", "larva" = "#DC267F", "chrysalis" = "#FE6100")

# For terms extract weather variable and dimension (spatial/temporal) as separate columns
moth_all_mods_best_tw_summ <-
  moth_all_mods_best_tw_summ[, `:=`(effect_weather_variable = str_to_title(str_extract(term, "temperature|rainfall")),
                                    effect_dimension = str_to_title(str_extract(term, "spatial|temporal")))][
                                      ,`:=`(effect_weather_variable = factor(effect_weather_variable,
                                                                             levels = c("Temperature", "Rainfall")),
                                            effect_dimension = factor(effect_dimension,
                                                                      levels = c("Spatial", "Temporal")))]

# Plot latyear model ----
latyear_mods <-
  moth_all_mods_best_tw_summ[model_fef == "latyear", ]

latyear_mods_forplot <-
  latyear_mods[term_pretty %in% c("Latitude", "Year")]
latyear_mods_forplot[, `:=`(term_pretty = factor(term_pretty,
                                                 levels = c("Latitude", "Year")),
                            full_name = fct_rev(full_name),
                            binomial = fct_rev(binomial))]
(latyear_plot <-
    ggplot(latyear_mods_forplot) +
    geom_point(aes(x = estimate, y = binomial,
                   shape = early_or_late, colour = wintering_stage), size = 2) +
    geom_text_repel(aes(x = estimate, y = binomial,
                  label = p_value_string), direction = "x",
                  colour = "gray50",
                  force = 0.5, force_pull = 0.1,
                  nudge_y = 0.2, size = 4,
                  min.segment.length = unit(5, "in")) +
    geom_linerange(aes(xmin = conf.low, xmax = conf.high, y = binomial,
                       colour = wintering_stage)) +
    geom_vline(xintercept = 0, linetype = "dashed", colour = "grey", size = 1) +
    labs(x = "Effect on phenology ([days/°N] or [days/yr])", y = "Species") +
    facet_wrap(vars(term_pretty), scales = "free_x") +
    scale_color_manual(values = wintering_stage_colours, guide = guide_legend()) +
    scale_shape_discrete(guide = guide_legend()) +
    # scale_colour_viridis_d(option="C") +
    theme_bw() +
    theme(legend.position = "bottom",
          axis.text.y = element_text(face = "italic")) +
    guides(color = guide_legend(title = "Overwintering stage",
                                title.position = "top"),
           shape = guide_legend(title = "Seasonality",
                                title.position = "top"))
)

ggsave("graphs/latyear_plot.png",
       plot = latyear_plot, width = 7, height = 4.5, units = "in")



# Plot weather-based models: best one for each species ----

weather_mods_core_summ <-
  moth_all_mods_best_tw_summ[model_fef %in% c("temperature", "rainfall", "temperature.rainfall"), ]

weather_mods_core_summ[, weather_lowest_aicc := min(AICc),
                       by = list(binomial)][
                         , is_weather_lowest_aicc := ifelse(AICc == weather_lowest_aicc, T, F)
                       ]

weather_best_mods_core_summ <- weather_mods_core_summ[is_weather_lowest_aicc == T,]

weather_best_mods_forplot <-
  weather_best_mods_core_summ[is.na(effect_weather_variable) == F,]

weather_best_mods_forplot[, `:=`(effect_weather_variable = factor(effect_weather_variable,
                                                      levels = c("Temperature", "Rainfall")),
                                 effect_dimension = factor(effect_dimension,
                                                           levels = c("Spatial", "Temporal")),
                            full_name = fct_rev(full_name),
                            binomial = fct_rev(binomial))]

# Dataframe for blank drawing (to make sure same scale for both spatial and temporal
# for each weather variable)
weather_best_mods_forplot_blank <-
  weather_best_mods_forplot[, .(max = max(conf.high),
                                min = min(conf.low)),
                            by = list(binomial,
                                      effect_dimension,
                                      effect_weather_variable)]
weather_best_mods_forplot_blank[, `:=`(max = max(max), min = min(min)),
                                        by = list(effect_weather_variable)]
weather_best_mods_forplot_blank <-
  weather_best_mods_forplot_blank %>%
  pivot_longer(., cols=c(min, max), names_to="stat", values_to="estimate")


(weather_best_mods_plot <-
    ggplot(weather_best_mods_forplot) +
    geom_point(aes(x = estimate, y = binomial,
                   shape = early_or_late, colour = wintering_stage), size = 2) +
    geom_text_repel(aes(x = estimate, y = binomial,
                        label = p_value_string), direction = "x",
                    colour = "gray50",
                    force = 1, force_pull = 0.1,
                    nudge_y = 0.2, size = 4) +
    geom_linerange(aes(xmin = conf.low, xmax = conf.high, y = binomial,
                       colour = wintering_stage)) +
    geom_vline(xintercept = 0, linetype = "dashed", colour = "grey", size = 1) +
    geom_blank(data = weather_best_mods_forplot_blank,
               aes(x = estimate, y = binomial)) +
    labs(x = "Effect on phenology ([days/°C] or [days/mm])", y = "Species") +
    facet_grid(cols = vars(effect_weather_variable,
                           effect_dimension), scales = "free_x") +
    scale_color_manual(values = wintering_stage_colours, guide = guide_legend()) +
    scale_shape_discrete(guide = guide_legend()) +
    # scale_colour_viridis_d(option="C") +
    theme_bw() +
    theme(legend.position = "bottom",
          axis.text.y = element_text(face = "italic")) +
    guides(color = guide_legend(title = "Overwintering stage",
                                title.position = "top"),
           shape = guide_legend(title = "Seasonality",
                                title.position = "top"))
)

ggsave("graphs/weather_mods_plot.png",
       plot = weather_best_mods_plot, width = 7, height = 4.5, units = "in")


# Different layout weather plot 2x2 ----


(weather_best_mods_plot2 <-
    ggplot(weather_best_mods_forplot) +
    geom_vline(xintercept = 0, linetype = "dashed", colour = "grey", size = 1) +
    geom_point(aes(x = estimate, y = binomial,
                   shape = early_or_late, colour = wintering_stage), size = 2) +
    geom_text_repel(aes(x = estimate, y = binomial,
                        label = p_value_string), direction = "x",
                    colour = "gray50",
                    force = 0.5, force_pull = 0.1,
                    nudge_y = 0.2, size = 4,
                    min.segment.length = unit(5, "in")) +
    geom_linerange(aes(xmin = conf.low, xmax = conf.high, y = binomial,
                       colour = wintering_stage)) +
    geom_blank(data = weather_best_mods_forplot_blank,
               aes(x = estimate, y = binomial)) +
    labs(x = "Effect on phenology ([days/°C] or [days/mm])", y = "Species") +
    facet_wrap(vars(effect_weather_variable, effect_dimension),
               nrow = 2, ncol = 2, scales = "free_x") +
    scale_color_manual(values = wintering_stage_colours, guide = guide_legend()) +
    scale_shape_discrete(guide = guide_legend()) +
    # scale_colour_viridis_d(option="C") +
    theme_bw() +
    theme(legend.position = "bottom",
          axis.text.y = element_text(face = "italic")) +
    guides(color = guide_legend(title = "Overwintering stage",
                                title.position = "top"),
           shape = guide_legend(title = "Seasonality",
                                title.position = "top"))
)

ggsave("graphs/weather_mods_plot2.png",
       plot = weather_best_mods_plot2, width = 6, height = 6, units = "in")

# Temperature arrow plot (temporal effect) ----

# Temperature arrow plot grouped by wintering stage ----
(temperature_arrow_plot_owstage <-
    ggplot(weather_best_mods_forplot[term == "temperature_temporal", ]) +
    geom_segment(aes(x = 0, y = binomial, xend = estimate, yend = binomial),
                 arrow = arrow(length = unit(0.1, "npc")), size = 1.3,
                 colour = "#D81B60") +
   # geom_text_repel(aes(x = estimate, y = binomial,
   #                     label = p_value_string), direction = "x",
   #                 colour = "gray50",
   #                 force = 1, force_pull = 0.1,
   #                 nudge_y = 0.4, size = 4) +
    geom_vline(xintercept = 0, linetype = "dashed", colour = "grey", size = 1) +
    facet_wrap(vars(wintering_stage),
               nrow = 3, ncol = 1, scales = "free_y") +
    labs(x = "Shift in adult-stage phenology [days/?C]", y = "Species") +
    scale_color_manual(values = wintering_stage_colours, guide = guide_legend()) +
    scale_shape_discrete(guide = guide_legend()) +
    theme_bw() +
    theme(legend.position = "none",
          strip.background = element_blank(),
          strip.text.x = element_blank(),
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(face="italic")
          )

)

ggsave(filename = "graphs/temperature_arrow_plot_owstage.png",
       plot = temperature_arrow_plot_owstage,
       height = 4, width = 5)

# Rainfall arrow plot grouped by wintering stage ----
# "#1E88E5"
(rainfall_arrow_plot_owstage <-
    ggplot(weather_best_mods_forplot[term == "rainfall_temporal", ]) +
    geom_segment(aes(x = 0, y = binomial, xend = estimate, yend = binomial),
                 arrow = arrow(length = unit(0.1, "npc")), size = 1.3,
                 colour = "#1E88E5") +
    geom_vline(xintercept = 0, linetype = "dashed", colour = "grey", size = 1) +
    geom_text_repel(aes(x = estimate, y = binomial,
                        label = p_value_string), direction = "x",
                    colour = "gray50",
                    force = 0.2, force_pull = 0.1,
                    min.segment.length = 10, # get rid of segments
                    nudge_y = 0.4, size = 4) +
    facet_wrap(vars(wintering_stage),
               nrow = 3, ncol = 1, scales = "free_y") +
    labs(x = "Shift in adult-stage phenology [days/mm]", y = "Species") +
    scale_color_manual(values = wintering_stage_colours, guide = guide_legend()) +
    scale_shape_discrete(guide = guide_legend()) +
    theme_bw() +
    theme(legend.position = "none",
          strip.background = element_blank(),
          strip.text.x = element_blank(),
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(face="italic")
    )
)

ggsave(filename = "graphs/rainfall_arrow_plot_owstage.png",
       plot = rainfall_arrow_plot_owstage,
       height = 4, width = 5)


# Time window chart ----

# Get upper and lower ordinal date on time window
moths_od_95_range <- fread("data/models/window/moth_data/moths_od_95_range.csv")

moths_od_95_range[, plot_variable := "Moth adults (95% of records)"]

best_tw_data <- read_parquet("data/models/window/results/best_tw_data.parquet")
setDT(best_tw_data)
best_tw <- unique(best_tw_data[, .(binomial, temperature_tw_string, rainfall_tw_string)]) %>%
  pivot_longer(c(temperature_tw_string, rainfall_tw_string),
               names_to = "plot_variable", values_to = "tw_string") %>%
  mutate(weather_variable = str_extract(plot_variable,"temperature|rainfall"),
         plot_variable = str_c(str_to_title(weather_variable), " (time window)"),
         lower = as.integer(str_extract(tw_string, "(?<=wo).+(?=wc)")),
         upper = as.integer(str_extract(tw_string, "(?<=wc).+$")),
         length_incl = upper - lower + 1) %>%
  select(-tw_string)


tw_weather_vars <- rbind(moths_od_95_range, best_tw, fill = T)

tw_weather_vars <- moth_traits[tw_weather_vars, on = .(binomial = binomial)]
setorder(tw_weather_vars, full_name)
tw_weather_vars[, binomial := as_factor(binomial)]


# Save tw ranges etc
tw_weather_vars_tosave <- tw_weather_vars[, .(binomial, plot_variable, lower, upper)]
tw_weather_vars_tosave[, range_var := str_extract(str_to_lower(plot_variable),
                                                  "temperature|rainfall|adults")]
tw_weather_vars_tosave[, plot_variable := NULL]

fwrite(tw_weather_vars_tosave, "tables/od_ranges_tws.csv", eol = "\n")



# TWs and OD ranges for plotting ----

tw_weather_vars[, plot_variable := factor(plot_variable,
                                          levels = c("Moth adults (95% of records)",
                                                     "Temperature (time window)",
                                                     "Rainfall (time window)"))]

tw_weather_vars_plot_colours <- c("Moth adults (95% of records)" = "#000000",
                                  "Temperature (time window)" = "#D81B60",
                                  "Rainfall (time window)" = "#1E88E5")

tw_weather_vars_forplot <-
  copy(tw_weather_vars) %>%
  .[, binomial := fct_rev(binomial)]

# Load table to convert ordinal days to strings
orddays_table_rev <- fread("data/ordinal_days/orddays_table_rev.csv")

orddays_month_rev <- orddays_table_rev[day_of_month_num == 1, ]
orddays_month_rev[, month_letter := str_c("  ", month_letter)]

year_vlines <- orddays_month_rev[day_of_month_num == 1 & month_num == 1, ]

dodge <- position_dodge(width=0.3)
(time_window_plot <- ggplot(tw_weather_vars_forplot) +
    geom_vline(data = year_vlines, aes(xintercept = ordinal_day),
               colour = "gray", size = 1.2) +
    geom_linerange(aes(xmin = lower, xmax = upper, y = binomial,
                       colour = plot_variable), position = dodge, size = 2) +

    xlab("Month") + ylab("Species") +
    facet_grid(rows = vars(wintering_stage), scales = "free_y") +
    scale_x_continuous(breaks = orddays_month_rev$ordinal_day,
                       labels = orddays_month_rev$month_letter,
                       minor_breaks = orddays_month_rev$ordinal_day,
                       sec.axis = sec_axis(~ ., name = "Ordinal day")) +
    coord_cartesian(xlim = c(min(tw_weather_vars_forplot$lower) - 5,
                             max(tw_weather_vars_forplot$upper) + 5)) +
    scale_colour_manual(breaks = c("Moth adults (95% of records)",
                                   "Temperature (time window)",
                                   "Rainfall (time window)"),
                        values = c("#000000", "#D81B60", "#1E88E5")) +
    theme_bw() +
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          legend.direction = "vertical",
          axis.text.x = element_text(hjust = 0),
          axis.text.y = element_text(face = "italic")
          )
)

ggsave(filename = "graphs/time_windows_and_moth_flight.png",
       plot = time_window_plot,
       height = 6, width = 6)


# Synthesised density plots for time window periods ----
tw_weather_vars_forplot2 <-
  tw_weather_vars_forplot[plot_variable == "Moth adults (95% of records)",
                          .(binomial, plot_variable, lower, upper)]
tw_weather_vars_forplot2[, mean_adult := (lower + upper)/2]
tw_weather_vars_forplot2 <- tw_weather_vars_forplot2[, .(binomial, mean_adult)]


tw_density_forplot <-
  left_join(tw_weather_vars_forplot,
            tw_weather_vars_forplot2,
            by = "binomial")
setorder(tw_density_forplot, full_name)
tw_density_forplot[, `:=`(lower_c = lower - mean_adult,
                                upper_c = upper - mean_adult,
                                binomial = as_factor(binomial))]
tw_density_forplot <- tw_density_forplot[, .(binomial, wintering_stage,
                                                         plot_variable, lower_c, upper_c)]

tw_density_forplot[, o_days := list()]
for (i in 1:nrow(tw_density_forplot)) {
  # Make sure all ranges have the same number of points
  # So when plotting density plots all species are weighted equally.
  # This is for plotting purposes only.
  lower <- tw_density_forplot[i, ]$lower_c
  upper <- tw_density_forplot[i, ]$upper_c
  range <- seq(lower, upper, length.out = 60)
  tw_density_forplot[i, "o_days"] <- list(range)
  rm(lower, upper, range)

}
tw_density_forplot <- unnest(tw_density_forplot, o_days)
setDT(tw_density_forplot)
tw_density_forplot[, months_before := -o_days/30.5]

(tw_density_plot <-
  ggplot(data = tw_density_forplot) +
  geom_density(aes(x = months_before, fill = plot_variable), alpha = 0.3) +
  scale_fill_manual(breaks = c("Moth adults (95% of records)",
                               "Temperature (time window)",
                               "Rainfall (time window)"),
                    values = c("#000000", "#D81B60", "#1E88E5")) +
  facet_wrap(vars(plot_variable), ncol = 1, nrow = 3,
             scales = "free_x"
             ) +
  labs(x = "Months before adult median", y = "Density") +
  scale_x_continuous(trans = "reverse",
                     breaks = seq(12, 0, by = -1)) +
  coord_cartesian(xlim = c(12,-1)) +
  theme_bw() +
  theme(
        axis.line.y = element_blank(),
        # axis.title.y = element_blank(),
        # axis.text.y = element_blank(),
        # axis.ticks.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(colour = "gray"),
        legend.position = "none"
        )
)

ggsave(plot = tw_density_plot,
       filename = "graphs/tw_density_plot.png",
       width = 5, height = 4, units = "in")

# Akaike weights table ----

moth_model_weights_table <- copy(moth_all_mods_best_tw_summ)

moth_model_weights_table <- moth_model_weights_table[
  model_fef %in% c("null", "latyear", "temperature", "rainfall",
                   "temperature.rainfall")
]

moth_model_weights_table[, `:=`(model_fef = case_when(
  model_fef == "null" ~ "Null",
  model_fef == "latyear" ~ "Latitude + Year",
  model_fef == "temperature" ~ "Temperature",
  model_fef == "rainfall" ~ "Rainfall",
  model_fef == "temperature.rainfall" ~ "Temperature + Rainfall"))]

moth_model_weights_table[, `:=`(binomial = as_factor(binomial),
                                  model_fef = factor(model_fef,
                                                     levels = c("Null",
                                                                "Latitude + Year",
                                                                "Temperature",
                                                                "Rainfall",
                                                                "Temperature + Rainfall")))]

moth_model_weights_table[, akaike_weights := MuMIn::Weights(AICc),
                              by = .(binomial)]

# Reverse factor before plotting so they appears in right order
moth_model_weights_forplot <- copy(moth_model_weights_table)
moth_model_weights_forplot[, binomial := fct_rev(binomial)]
# Colourblind-friendly palette
weights_plot_colours <- c("Null" = "#7E5852",
                          "Latitude + Year" = "#FFC107",
                          "Temperature" = "#D81B60",
                          "Rainfall" = "#1E88E5",
                          "Temperature + Rainfall" = "#004D40")
(model_weights_plot <-
    ggplot(data = moth_model_weights_forplot) +
    geom_col(aes(x = akaike_weights, y = binomial,
                 fill = model_fef)) +
    labs(x = "Akaike weight", y = "Species") +
    scale_fill_manual(values = weights_plot_colours,
                      guide = guide_legend(title = "Model")) +
    facet_grid(rows = vars(wintering_stage), scales = "free_y") +
    coord_cartesian(expand = F) +
    theme_bw() +
    theme(axis.text.y = element_text(face = "italic"),
          legend.position = "right",
          legend.direction = "vertical")
)

ggsave(plot = model_weights_plot,
       filename = "graphs/model_weights_plot.png",
       width = 6, height = 4, units = "in")
