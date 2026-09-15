# =============================================================================
# 08_calibration_ramp_plots.R
#
# Purpose: Visual calibration diagnostics for the June 2026 ramping assay.
#          Sensors were placed in a water bath ramped from ~0 °C to ~51 °C
#          over ~71 minutes. Plots compare each sensor to the NIST-traceable
#          reference thermometer across the full temperature range.
#          No calibration corrections are applied here -- this is diagnostic
#          only.
#
# Inputs:  ../raw_data/{cal_name}/NIST.xlsx         (1-min spot readings)
#          ../raw_data/{cal_name}/iButtons/*.csv     (DS1925, 3-min logging)
#          ../raw_data/{cal_name}/HOBOs/*.xlsx       (MX2302, 1-min logging)
#
# Outputs: results/calibration/fig1_ramp_timeseries.png
#          results/calibration/fig2_sensor_vs_nist.png
#          results/calibration/fig3_bias_vs_nist.png
#
# Figures:
#   1. Full-ramp time series: NIST (bold black) + all sensors coloured by
#      model, so any stray sensors or logger dropouts are immediately visible.
#   2. Sensor temperature vs. NIST (°C): one point per paired reading, 1:1
#      reference line, per-model OLS line. The slope and intercept of each
#      model's cloud relative to the 1:1 line characterise systematic bias
#      and gain error across the ramp.
#   3. Bias (sensor − NIST, °C) vs. NIST temperature: shows whether sensor
#      error is constant or temperature-dependent. A flat loess near zero is
#      ideal; curvature indicates non-linear bias.
#
# Notes:
#   - NIST.xlsx stores time-of-day only (Excel epoch 1899-12-31); the
#     calibration date (cal_date) is spliced in at load time.
#   - Sensor timestamps are matched to the nearest NIST reading within a
#     ±TOL_MIN tolerance window. Readings with no NIST match within the
#     window are silently dropped from Figs 2-3 but remain in Fig 1.
#   - Both HOBO channels are included as independent observations, labelled
#     ch1 / ch2.
# =============================================================================

suppressPackageStartupMessages({
  library(readr)
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(lubridate)
  library(ggplot2)
  library(scales)
})

# =============================================================================
# CONFIG
# =============================================================================

cal_name <- "calibration_06_2026"
cal_date <- as.Date("2026-06-02")   # actual date of the ramping assay
                                    # (NIST.xlsx stores time-only)

TOL_MIN  <- 2.5    # nearest-time join tolerance in minutes; should be no
                   # more than half the iButton sample interval (3 min)

TZ        <- "US/Eastern"
raw_dir   <- file.path("..", "raw_data", cal_name)
out_dir   <- file.path("results", "calibration")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# =============================================================================
# HELPERS
# =============================================================================

retag <- function(df) {
  if ("datetime" %in% names(df)) df$datetime <- force_tz(df$datetime, TZ)
  df
}

# Nearest-time join: for each row in sensor_df, find the closest NIST reading
# within TOL_MIN minutes and attach it as a new column `nist_c`. Rows with no
# match within tolerance get NA and are excluded from the bias plots.
join_to_nist <- function(sensor_df, nist_df, tol_min = TOL_MIN) {
  tol_sec    <- tol_min * 60
  nist_times <- nist_df$datetime
  nist_temps <- nist_df$nist_c
  sensor_df %>%
    mutate(nist_c = sapply(datetime, function(t) {
      diffs <- abs(as.numeric(difftime(nist_times, t, units = "secs")))
      best  <- which.min(diffs)
      if (diffs[best] <= tol_sec) nist_temps[best] else NA_real_
    }))
}

wrap_caption <- function(text, width = 110) {
  text <- gsub("\\s+", " ", text)
  paste(strwrap(text, width = width), collapse = "\n")
}

# =============================================================================
# 1. LOAD NIST REFERENCE
# =============================================================================
# NIST.xlsx: columns Time (time-only, Excel epoch), Temp (°C).
# Splice cal_date into the time column to get real datetimes.

nist_raw <- read_excel(file.path(raw_dir, "NIST.xlsx"))
nist <- nist_raw %>%
  transmute(
    datetime = as.POSIXct(
      paste(cal_date, format(Time, "%H:%M:%S")),
      tz = TZ
    ),
    nist_c = Temp
  )

cat(sprintf("NIST: %d readings, %.1f – %.1f °C, %s to %s\n",
            nrow(nist),
            min(nist$nist_c), max(nist$nist_c),
            format(min(nist$datetime), "%H:%M"),
            format(max(nist$datetime), "%H:%M")))

# =============================================================================
# 2. LOAD iBUTTON DATA
# =============================================================================

ibutton_files <- list.files(file.path(raw_dir, "iButtons"),
                            pattern = "\\.csv$", full.names = TRUE)
cat(sprintf("iButtons: %d files found\n", length(ibutton_files)))

ibutton_list <- lapply(ibutton_files, function(f) {
  raw_lines    <- readLines(f, warn = FALSE)
  sensor_model <- sub(".*Part Number:\\s*",       "", raw_lines[1])
  sensor_id    <- sub(".*Registration Number:\\s*", "", raw_lines[2])
  data_start   <- which(grepl("^Date/Time", raw_lines))
  dat          <- read.csv(f, skip = data_start - 1, stringsAsFactors = FALSE)
  dat$datetime <- parse_date_time(dat$Date.Time, orders = "dmyIMSp",
                                  tz = TZ)
  data.frame(
    sensor_id    = sensor_id,
    sensor_model = sensor_model,
    channel      = NA_character_,
    datetime     = dat$datetime,
    temp_c       = dat$Value,
    stringsAsFactors = FALSE
  )
})

ibuttons <- bind_rows(ibutton_list)
cat(sprintf("  Parsed %d iButton readings across %d sensors\n",
            nrow(ibuttons), n_distinct(ibuttons$sensor_id)))

# =============================================================================
# 3. LOAD HOBO DATA
# =============================================================================

hobo_files <- list.files(file.path(raw_dir, "HOBOs"),
                         pattern = "^\\d+.*\\.xlsx$", full.names = TRUE)
cat(sprintf("HOBOs: %d files found\n", length(hobo_files)))

hobo_list <- lapply(hobo_files, function(f) {
  dat       <- read_excel(f)
  hobo_id   <- sub("^(\\d+)\\s.*", "\\1", basename(f))
  col_names <- colnames(dat)
  temp_cols <- grep("Temperature", col_names, value = TRUE)
  if (length(temp_cols) < 2) {
    warning("File ", basename(f), " has fewer than 2 temperature columns. Skipping.")
    return(NULL)
  }
  data.frame(
    sensor_id = hobo_id,
    datetime  = dat[["Date-Time (EDT)"]],
    ch1       = dat[[temp_cols[1]]],
    ch2       = dat[[temp_cols[2]]],
    stringsAsFactors = FALSE
  )
})

# Pivot to long so each channel is one row, matching iButton structure
# read_excel returns datetimes labeled UTC (Excel has no timezone); the column
# header says EDT so we force-tag before binding with iButtons (which are
# already in US/Eastern) to avoid a 4-hour shift on bind_rows.
hobos <- bind_rows(hobo_list) %>%
  mutate(datetime = force_tz(datetime, TZ)) %>%
  pivot_longer(c(ch1, ch2),
               names_to  = "channel",
               values_to = "temp_c") %>%
  mutate(sensor_model = "HOBO") %>%
  select(sensor_id, sensor_model, channel, datetime, temp_c)

cat(sprintf("  Parsed %d HOBO readings across %d loggers (both channels)\n",
            nrow(hobos), n_distinct(hobos$sensor_id)))

# =============================================================================
# 4. COMBINE AND JOIN TO NIST
# =============================================================================

all_sensors <- bind_rows(ibuttons, hobos) %>%
  retag() %>%
  filter(!is.na(temp_c))

# Trim to NIST window before joining (keeps plots tidy)
nist_start <- min(nist$datetime)
nist_end   <- max(nist$datetime)

sensors_in_window <- all_sensors %>%
  filter(datetime >= nist_start, datetime <= nist_end)

# Nearest-time join
sensors_joined <- join_to_nist(sensors_in_window, nist)

n_matched <- sum(!is.na(sensors_joined$nist_c))
n_total   <- nrow(sensors_joined)
cat(sprintf("\nNearest-time join (±%.1f min): %d / %d sensor readings matched\n",
            TOL_MIN, n_matched, n_total))

# =============================================================================
# STYLE
# =============================================================================

model_palette <- c("DS1925"     = "#D7263D",
                   "HOBO"       = "#1B9AAA",
                   "NIST"       = "black")

base_theme <- theme_bw(base_size = 11) +
  theme(legend.position  = "bottom",
        plot.title       = element_text(face = "bold"),
        plot.caption     = element_text(hjust = 0, colour = "grey25",
                                        size = rel(0.85),
                                        margin = margin(t = 8)),
        strip.background = element_rect(fill = "grey92", colour = NA))

# =============================================================================
# FIGURE 1 -- FULL RAMP TIME SERIES
# =============================================================================

p1 <- ggplot() +
  geom_line(data = all_sensors,
            aes(x = datetime, y = temp_c,
                colour = sensor_model,
                group  = interaction(sensor_id, channel)),
            alpha = 0.45, linewidth = 0.35) +
  geom_line(data = nist,
            aes(x = datetime, y = nist_c, colour = "NIST"),
            linewidth = 1.1) +
  scale_colour_manual(values = model_palette, name = "Sensor model") +
  scale_x_datetime(date_labels = "%H:%M", date_breaks = "10 min") +
  labs(title   = paste("Ramping calibration session –", cal_date),
       x       = "Time (EDT)", y = "Temperature (°C)",
       caption = wrap_caption(
         "Figure 1. All loggers and the NIST reference over the full ramping
          assay (~0 to ~51 °C). NIST trace (bold black) shows the bath
          temperature profile. Sensor traces that diverge visibly from the
          NIST line across the ramp indicate systematic bias or gain error.",
         width = 110)) +
  base_theme

ggsave(file.path(out_dir, "fig1_ramp_timeseries.png"),
       p1, width = 9, height = 5, dpi = 200)
print(p1)

# =============================================================================
# FIGURE 2 -- SENSOR TEMPERATURE vs. NIST (scatter, 1:1 line)
# =============================================================================
# x = NIST temperature at the moment of the sensor reading (nearest-time
# match). y = sensor temperature. A perfect sensor lies on the 1:1 dashed
# line. The OLS line per sensor model reveals systematic offset (intercept
# shift) and gain error (slope != 1).

sensors_paired <- sensors_joined %>%
  filter(!is.na(nist_c))

# Per-model OLS line for annotation in Figure 2
ols_fits <- sensors_paired %>%
  group_by(sensor_model) %>%
  summarise(
    intercept = coef(lm(temp_c ~ nist_c))[1],
    slope     = coef(lm(temp_c ~ nist_c))[2],
    .groups   = "drop"
  )

cat("\nOLS fit per sensor model (temp_c ~ nist_c):\n")
print(ols_fits %>% mutate(across(c(intercept, slope), \(x) round(x, 4))))

p2 <- ggplot(sensors_paired,
             aes(x = nist_c, y = temp_c, colour = sensor_model)) +
  # 1:1 reference line
  geom_abline(slope = 1, intercept = 0,
              linetype = "dashed", colour = "grey40", linewidth = 0.6) +
  geom_point(alpha = 0.35, size = 0.9) +
  # Per-model OLS trend
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.9,
              formula = y ~ x) +
  facet_wrap(~ sensor_model) +
  scale_colour_manual(values = model_palette, guide = "none") +
  scale_x_continuous(breaks = seq(0, 55, 10)) +
  scale_y_continuous(breaks = seq(0, 55, 10)) +
  labs(title   = "Sensor temperature vs. NIST reference",
       x       = "NIST temperature (°C)", y = "Sensor temperature (°C)",
       caption = wrap_caption(
         "Figure 2. Each point is one sensor reading paired to the nearest
          NIST reading (within ±2.5 min). Dashed line = perfect agreement
          (slope 1, intercept 0). Solid lines = per-sensor-model OLS fits.
          Points above the dashed line indicate positive bias (sensor reads
          warm); slope > 1 indicates gain error that grows with temperature.",
         width = 110)) +
  base_theme +
  coord_equal() +
  theme(legend.position = "none")

ggsave(file.path(out_dir, "fig2_sensor_vs_nist.png"),
       p2, width = 9, height = 5, dpi = 200)
print(p2)

# =============================================================================
# FIGURE 3 -- BIAS (sensor − NIST) vs. NIST TEMPERATURE
# =============================================================================
# A flat bias near zero across the full ramp range is ideal. Curvature in the
# loess smoother indicates non-linear, temperature-dependent error that a
# simple additive offset would not fully correct.

sensors_bias <- sensors_paired %>%
  mutate(bias_c = temp_c - nist_c)

# Per-model mean bias for annotation
mean_bias <- sensors_bias %>%
  group_by(sensor_model) %>%
  summarise(mean_bias = mean(bias_c, na.rm = TRUE), .groups = "drop")

cat("\nMean bias per sensor model (sensor - NIST, °C):\n")
print(mean_bias %>% mutate(mean_bias = round(mean_bias, 3)))

p3 <- ggplot(sensors_bias,
             aes(x = nist_c, y = bias_c, colour = sensor_model)) +
  geom_hline(yintercept = 0, linetype = "dashed",
             colour = "grey40", linewidth = 0.6) +
  geom_hline(data = mean_bias,
             aes(yintercept = mean_bias, colour = sensor_model),
             linetype = "dotted", linewidth = 0.7, show.legend = FALSE) +
  geom_point(alpha = 0.25, size = 0.9) +
  geom_smooth(method = "loess", se = TRUE, span = 0.5,
              linewidth = 0.9, alpha = 0.15,
              formula   = y ~ x) +
  facet_wrap(~ sensor_model) +
  scale_colour_manual(values = model_palette, guide = "none") +
  scale_x_continuous(breaks = seq(0, 55, 10)) +
  labs(title   = "Sensor bias vs. NIST reference temperature",
       x       = "NIST temperature (°C)",
       y       = "Bias: sensor − NIST (°C)",
       caption = wrap_caption(
         "Figure 3. Sensor error (sensor − NIST) across the full ramp range.
          Dashed line = zero bias. Dotted lines = per-model mean bias.
          Shaded ribbon = loess 95% confidence interval. A flat, near-zero
          loess suggests a simple additive offset is sufficient to correct
          the sensor; curvature indicates temperature-dependent error
          requiring a linear or non-linear correction.",
         width = 110)) +
  base_theme

ggsave(file.path(out_dir, "fig3_bias_vs_nist.png"),
       p3, width = 9, height = 5, dpi = 200)
print(p3)

cat("\n--- Done. Plots written to", out_dir, "---\n")
