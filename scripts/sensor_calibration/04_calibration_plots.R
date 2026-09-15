# =============================================================================
# 04_calibration_plots.R
#
# Purpose: Visual diagnostics for the May 1 2026 sensor calibration. Produces
#          four publication-quality figures from the tidy tables built by
#          03_calibrate_sensors.R.
#
# Inputs:  clean_data/calibration/reference_clean.csv
#          clean_data/calibration/sensor_session_long.csv
#          clean_data/calibration/sensor_calibration_long.csv
#          clean_data/calibration/sensor_offsets.csv
#          clean_data/calibration/sensor_corrections.csv
#
# Outputs: results/calibration/fig1_session_timeseries.png
#          results/calibration/fig2_offset_dotplot.png
#          results/calibration/fig3_correction_coefs.png
#          results/calibration/fig4_bland_altman.png
#
# Figures:
#   1. Full-session time series overlay: NIST + every logger, with the three
#      stable windows (hot plateau, thermocouple hot, ice) shaded.
#   2. Dot plot of per-sensor offset from NIST, faceted by bath. Zero line
#      drawn. Sensors ranked within each panel for quick outlier reading.
#   3. Two-point linear correction coefficients: slope vs. intercept, with
#      the ideal sensor (slope=1, intercept=0) marked. Reveals which sensors
#      are biased uniformly vs. which have a gain problem.
#   4. Bland-Altman per sensor type: per-reading difference (logger - NIST
#      anchor) vs. mean, with mean bias and 95% limits of agreement.
#
# Notes:
#   - Uses theme_bw() and a consistent palette across figures.
#   - The hot-bath anchor for the thermocouple is the matched 16:25-16:30
#     window (NIST drifting); for all other sensors it is the static plateau
#     mean. The Bland-Altman panel for Thermocouple reflects this.
# =============================================================================

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tidyr)
  library(lubridate)
  library(ggplot2)
  library(scales)
})

# --- Paths -------------------------------------------------------------------

calib_dir <- file.path("clean_data", "calibration")
out_dir   <- file.path("results", "calibration")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# --- Load tidy outputs from 03 ----------------------------------------------

# read_csv reads bare datetimes as UTC; the CSVs from 03 contain wall-clock
# EDT, so we re-tag them without shifting the clock.
TZ <- "US/Eastern"
retag <- function(df) {
  if ("datetime" %in% names(df)) df$datetime <- force_tz(df$datetime, TZ)
  df
}

ref          <- retag(suppressMessages(read_csv(file.path(calib_dir, "reference_clean.csv"))))
session_long <- retag(suppressMessages(read_csv(file.path(calib_dir, "sensor_session_long.csv"))))
calib_long   <- retag(suppressMessages(read_csv(file.path(calib_dir, "sensor_calibration_long.csv"))))
offsets      <- suppressMessages(read_csv(file.path(calib_dir, "sensor_offsets.csv")))
corrections  <- suppressMessages(read_csv(file.path(calib_dir, "sensor_corrections.csv")))

# Window definitions kept in one place so the shaded rectangles in Fig 1
# match what 03 used.
HOT_WINDOW        <- as.POSIXct(c("2026-05-01 15:54:00", "2026-05-01 16:13:00"),
                                tz = TZ)
ICE_WINDOW        <- as.POSIXct(c("2026-05-01 16:56:00", "2026-05-01 17:11:00"),
                                tz = TZ)
THERMO_HOT_WINDOW <- as.POSIXct(c("2026-05-01 16:25:00", "2026-05-01 16:30:00"),
                                tz = TZ)

# Shared style. Colour by sensor model 
model_palette <- c("DS1925"       = "#D7263D",
                   "DS1921G-F5"   = "#F4A261",
                   "HOBO"         = "#1B9AAA",
                   "Thermocouple" = "#6A4C93",
                   "NIST"         = "black")

base_theme <- theme_bw(base_size = 11) +
  theme(legend.position = "bottom",
        plot.title      = element_text(face = "bold"),
        plot.caption    = element_text(hjust = 0, colour = "grey25",
                                       size = rel(0.85),
                                       margin = margin(t = 8)),
        strip.background = element_rect(fill = "grey92", colour = NA))

# Caption helper: collapse whitespace then wrap to ~`width` chars per line so
# captions fit the saved figure width. Tune `width` per plot if needed; the
# defaults below assume 9-inch-wide figures (~110 chars at base size 11).
wrap_caption <- function(text, width = 110) {
  text <- gsub("\\s+", " ", text)
  paste(strwrap(text, width = width), collapse = "\n")
}

# =============================================================================
# FIGURE 1 -- FULL SESSION TIME SERIES
# =============================================================================

shade_df <- tibble(
  xmin = c(HOT_WINDOW[1], THERMO_HOT_WINDOW[1], ICE_WINDOW[1]),
  xmax = c(HOT_WINDOW[2], THERMO_HOT_WINDOW[2], ICE_WINDOW[2]),
  fill = c("hot plateau",  "thermo hot",        "ice plateau")
)

p1 <- ggplot() +
  geom_rect(data = shade_df,
            aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf,
                fill = fill),
            alpha = 0.18, inherit.aes = FALSE) +
  geom_line(data = session_long,
            aes(x = datetime, y = temp_c,
                group = sensor_id, colour = sensor_model),
            alpha = 0.6, linewidth = 0.35) +
  geom_line(data = ref,
            aes(x = datetime, y = nist_c, colour = "NIST"),
            linewidth = 0.9) +
  scale_colour_manual(values = model_palette, name = "Sensor model") +
  scale_fill_manual(values = c("hot plateau" = "#F4A261",
                               "thermo hot"  = "#E76F51",
                               "ice plateau" = "#74C0FC"),
                    name = "Stable window") +
  scale_x_datetime(date_labels = "%H:%M", date_breaks = "30 min") +
  labs(title   = "Calibration session, 2026-05-01",
       x       = "Time (EDT)", y = "Temperature (°C)",
       caption = wrap_caption(
         "Figure 1. All loggers and the NIST reference over the full
          calibration session. Shaded bands mark the stable analysis
          windows used to compute per-sensor offsets.",
         width = 105)) +
  base_theme +
  # Fig 1 has two legends (sensor model + stable window); stack them
  # vertically in a box to the right so the plot keeps its full width.
  theme(legend.position  = "right",
        legend.box       = "vertical",
        legend.direction = "vertical")

ggsave(file.path(out_dir, "fig1_session_timeseries.png"),
       p1, width = 9, height = 5, dpi = 200)
print(p1)

# =============================================================================
# FIGURE 2 -- OFFSET DOT PLOT
# =============================================================================

# Order sensors by hot-bath offset for readability
sensor_order <- offsets %>%
  filter(bath == "hot") %>%
  arrange(offset_c) %>%
  pull(sensor_id)
# Append any sensors not in hot
sensor_order <- c(sensor_order,
                  setdiff(unique(offsets$sensor_id), sensor_order))

offsets_plot <- offsets %>%
  mutate(sensor_id = factor(sensor_id, levels = sensor_order),
         bath      = factor(bath, levels = c("hot", "ice"),
                            labels = c("Hot bath (~42 °C)",
                                       "Ice bath (0 °C)")))

p2 <- ggplot(offsets_plot,
             aes(x = offset_c, y = sensor_id, colour = sensor_model)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey40") +
  geom_segment(aes(xend = 0, yend = sensor_id), alpha = 0.4) +
  geom_point(size = 2.4) +
  facet_wrap(~ bath, scales = "free_x") +
  scale_colour_manual(values = model_palette, name = "Sensor model") +
  labs(title   = "Per-sensor offset from NIST reference",
       x       = "Offset (sensor – NIST, °C)", y = NULL,
       caption = wrap_caption(
         "Figure 2. Each point is one sensor's mean reading in the stable
          bath window, minus the NIST anchor for that bath. Sensors are
          ordered by hot-bath offset.",
         width = 105)) +
  base_theme +
  theme(axis.text.y = element_text(size = 7))

ggsave(file.path(out_dir, "fig2_offset_dotplot.png"),
       p2, width = 9, height = 6.5, dpi = 200)
print(p2)

# =============================================================================
# FIGURE 3 -- CORRECTION COEFFICIENTS (SLOPE vs INTERCEPT)
# =============================================================================

p3 <- ggplot(corrections %>% filter(has_both_baths),
             aes(x = intercept, y = slope, colour = sensor_model)) +
  geom_hline(yintercept = 1, linetype = "dashed", colour = "grey40") +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey40") +
  geom_point(size = 2.6, alpha = 0.85) +
  annotate("point", x = 0, y = 1, shape = 21,
           fill = "white", colour = "black", size = 3.2, stroke = 0.8) +
  annotate("text",  x = 0, y = 1, label = "  ideal",
           hjust = 0, vjust = -0.7, size = 3.3, colour = "grey30") +
  scale_colour_manual(values = model_palette, name = "Sensor model") +
  labs(title   = "Two-point linear correction coefficients",
       x       = "Intercept (°C)", y = "Slope",
       caption = wrap_caption(
         "Figure 3. Linear correction T_true = slope × T_raw + intercept,
          derived from the (ice, hot) plateau means. Dashed lines mark the
          ideal sensor (slope = 1, intercept = 0). Each point is one
          sensor or HOBO channel.",
         width = 85)) +
  base_theme

ggsave(file.path(out_dir, "fig3_correction_coefs.png"),
       p3, width = 7.5, height = 5.5, dpi = 200)
print(p3)

# =============================================================================
# FIGURE 4 -- BLAND-ALTMAN PER SENSOR TYPE
# =============================================================================
# Treat each stable-window logger reading as one paired observation against
# the NIST anchor for that (sensor_type, bath). Limits of agreement = mean
# diff +- 1.96 SD computed per sensor_type.
# =============================================================================

# Re-join the NIST anchor (per sensor_type x bath) so we can compute per-row
# differences. Limits of agreement are computed per model so iButton
# models are separated.

nist_anchors <- offsets %>%
  distinct(sensor_type, bath, nist_c)

ba <- calib_long %>%
  left_join(nist_anchors, by = c("sensor_type", "bath")) %>%
  mutate(diff = temp_c - nist_c,
         avg  = (temp_c + nist_c) / 2)

loa <- ba %>%
  group_by(sensor_model) %>%
  summarise(mean_diff = mean(diff, na.rm = TRUE),
            sd_diff   = sd(diff,   na.rm = TRUE),
            loa_lo    = mean_diff - 1.96 * sd_diff,
            loa_hi    = mean_diff + 1.96 * sd_diff,
            .groups   = "drop")

p4 <- ggplot(ba, aes(x = avg, y = diff, colour = sensor_model)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40") +
  geom_hline(data = loa, aes(yintercept = mean_diff),
             colour = "grey20", linewidth = 0.5) +
  geom_hline(data = loa, aes(yintercept = loa_lo),
             colour = "grey50", linetype = "dotted") +
  geom_hline(data = loa, aes(yintercept = loa_hi),
             colour = "grey50", linetype = "dotted") +
  geom_point(alpha = 0.55, size = 1.3) +
  facet_wrap(~ sensor_model, scales = "free") +
  scale_colour_manual(values = model_palette, guide = "none") +
  labs(title   = "Bland-Altman: logger vs. NIST anchor",
       x       = "Mean of logger and NIST (°C)",
       y       = "Logger – NIST (°C)",
       caption = wrap_caption(
         "Figure 4. Each point is one stable-window reading. Dashed line
          at 0 = perfect agreement. Solid grey line = mean bias
          (systematic offset). Dotted lines = 95% limits of agreement
          (mean ± 1.96 SD).",
         width = 105)) +
  base_theme

ggsave(file.path(out_dir, "fig4_bland_altman.png"),
       p4, width = 9, height = 5, dpi = 200)
print(p4)

# --- Print LoA summary for the record ---------------------------------------

cat("\n=== Limits of agreement per sensor type ===\n")
print(loa %>%
        mutate(across(c(mean_diff, sd_diff, loa_lo, loa_hi),
                      \(x) round(x, 3))))

cat("\n--- Done. Plots written to", out_dir, "---\n")
