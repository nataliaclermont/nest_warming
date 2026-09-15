# =============================================================================
# 06_pilot_descriptive_plots.R
#
# Purpose: First-look descriptive plots for a pilot test. Calibration
#          corrections are NOT applied yet -- this is raw-data exploration.
#          iButtons are mapped to trap nests via roof_colour and trap_style.
#          Thermocouple uses the nest_id <-> colour mapping from the HOBO map.
#
# Inputs:  clean_data/{test_name}_ibuttons_trimmed.csv
#          ../raw_data/Pilot Test/nest_hobo_map.xlsx
#          ../raw_data/Pilot Test/pilot_thermocouple.xlsx
#
# Outputs: results/pilot/fig1_ibutton_timeseries.png
#          results/pilot/fig2_ibutton_daily_summary.png
#          results/pilot/fig3_ibutton_diurnal.png
#          results/pilot/fig4_thermocouple_timeseries.png
#          results/pilot/fig5_thermocouple_by_colour.png
#          results/pilot/fig6_thermocouple_back_front_diff.png
#          results/pilot/fig7_thermocouple_back_front_sidebyside.png
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

# --- Paths -------------------------------------------------------------------

test_name <- "pilot1"

clean_dir <- "clean_data"
raw_dir   <- file.path("..", "raw_data", "Pilot Test")
out_dir   <- file.path("results", "pilot")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

TZ <- "US/Eastern"

retag <- function(df) {
  if ("datetime" %in% names(df)) df$datetime <- force_tz(df$datetime, TZ)
  df
}

# --- Style -------------------------------------------------------------------

# Roof colours. White gets a near-white fill -- always paired with a black
# outline (shape 21) so it stays visible on white backgrounds.
trap_palette <- c(b = "#2b2b2b", t = "#C19A6B", w = "#ECEFF1")
trap_labels  <- c(b = "Black",   t = "Tan",     w = "White")

trap_style_labels <- c(r = "Roisin", t = "Tovah")
trap_style_lines  <- c(r = "solid",  t = "dashed")

base_theme <- theme_bw(base_size = 11) +
  theme(legend.position = "bottom",
        plot.title      = element_text(face = "bold"),
        plot.caption    = element_text(hjust = 0, colour = "grey25",
                                       size = rel(0.85),
                                       margin = margin(t = 8)),
        strip.background = element_rect(fill = "grey92", colour = NA))

wrap_caption <- function(text, width = 105) {
  text <- gsub("\\s+", " ", text)
  paste(strwrap(text, width = width), collapse = "\n")
}

# =============================================================================
# LOAD DATA
# =============================================================================

# iButtons (trimmed, with mapping) -------------------------------------------

ib <- retag(suppressMessages(
  read_csv(file.path(clean_dir, paste0(test_name, "_ibuttons_clean.csv")))
)) %>%
  mutate(roof_colour = factor(roof_colour, levels = c("b", "t", "w")),
         trap_style  = factor(trap_style,  levels = c("r", "t")))

# Nest <-> colour lookup (from HOBO map) -------------------------------------

hobo_map <- suppressMessages(
  read_excel(file.path(raw_dir, "nest_hobo_map.xlsx"))
)
nest_colour <- hobo_map %>%
  distinct(nest_id, colour) %>%
  mutate(nest_id = as.integer(nest_id))

# Thermocouple spot measurements ---------------------------------------------

thermo_raw <- suppressMessages(
  read_excel(file.path(raw_dir, "pilot_thermocouple.xlsx"))
)

# `date` has the real date (time = 00:00); `time` carries time-of-day on the
# 1899-12-31 epoch. Splice them into a real EDT datetime.
thermo <- thermo_raw %>%
  mutate(time_str = sprintf("%02d:%02d:%02d",
                            hour(time), minute(time), second(time)),
         datetime = as.POSIXct(paste(as.Date(date), time_str), tz = TZ),
         nest_id  = as.integer(nest_id)) %>%
  select(datetime, nest_id, t_back, t_front) %>%
  left_join(nest_colour, by = "nest_id")

# Long format for plotting (one row per probe position).
thermo_long <- thermo %>%
  pivot_longer(c(t_back, t_front),
               names_to  = "position",
               values_to = "temp_c") %>%
  mutate(position = sub("t_", "", position),
         colour   = factor(colour, levels = c("b", "t", "w")))

# =============================================================================
# FIGURE 1 -- iBUTTON FULL PILOT TIME SERIES
# =============================================================================

p1 <- ggplot(ib, aes(x = datetime, y = temp_c,
                     colour    = roof_colour,
                     linetype  = trap_style,
                     group     = sensor_id)) +
  geom_line(alpha = 0.85, linewidth = 0.55) +
  scale_colour_manual(values = trap_palette, labels = trap_labels,
                      name = "Roof colour") +
  scale_linetype_manual(values = trap_style_lines, labels = trap_style_labels,
                        name = "Trap style") +
  scale_x_datetime(date_labels = "%b %d\n%H:%M", date_breaks = "12 hours") +
  labs(title = "iButton pilot: full time series",
       x = NULL, y = "Temperature (°C)",
       caption = wrap_caption(
         "Figure 1. Raw (uncalibrated) iButton temperature traces over the
          pilot period. Line colour indicates roof colour treatment
          (black/tan/white); line type indicates trap style (Roisin/Tovah).
          Look for systematic separation between colours (a roof-colour
          signature) and convergence across styles within the same colour.",
         width = 105)) +
  base_theme

ggsave(file.path(out_dir, "fig1_ibutton_timeseries.png"),
       p1, width = 9, height = 5, dpi = 200)
print(p1)

# =============================================================================
# FIGURE 2 -- DAILY SUMMARY STATS PER iBUTTON
# =============================================================================

ib_daily <- ib %>%
  mutate(date = as.Date(datetime)) %>%
  group_by(sensor_id, roof_colour, trap_style, date) %>%
  summarise(daily_max   = max(temp_c,  na.rm = TRUE),
            daily_min   = min(temp_c,  na.rm = TRUE),
            daily_mean  = mean(temp_c, na.rm = TRUE),
            diurnal_rng = daily_max - daily_min,
            .groups = "drop") %>%
  pivot_longer(c(daily_max, daily_mean, daily_min, diurnal_rng),
               names_to = "metric", values_to = "value") %>%
  mutate(metric = factor(metric,
                         levels = c("daily_max", "daily_mean",
                                    "daily_min", "diurnal_rng"),
                         labels = c("Daily max", "Daily mean",
                                    "Daily min", "Diurnal range")))

p2 <- ggplot(ib_daily, aes(x = date, y = value,
                           colour   = roof_colour,
                           linetype = trap_style,
                           group    = sensor_id)) +
  geom_line(alpha = 0.7) +
  geom_point(size = 2.4) +
  facet_wrap(~ metric, scales = "free_y") +
  scale_colour_manual(values = trap_palette, labels = trap_labels,
                      name = "Roof colour") +
  scale_linetype_manual(values = trap_style_lines, labels = trap_style_labels,
                        name = "Trap style") +
  scale_x_date(date_labels = "%b %d", date_breaks = "1 day") +
  labs(title = "iButton daily summary statistics",
       x = NULL, y = "Temperature (°C)",
       caption = wrap_caption(
         "Figure 2. Daily max, mean, min, and diurnal range per logger.
          Daily max is the strongest candidate proxy for a roof-colour
          signature -- look for a black > tan > white ordering. Diurnal
          range should follow the same pattern if the colour treatment
          is working.",
         width = 105)) +
  base_theme

ggsave(file.path(out_dir, "fig2_ibutton_daily_summary.png"),
       p2, width = 9, height = 6, dpi = 200)
print(p2)

# =============================================================================
# FIGURE 3 -- DIURNAL PATTERN PER iBUTTON
# =============================================================================

ib_hourly <- ib %>%
  mutate(hour = hour(datetime)) %>%
  group_by(sensor_id, roof_colour, trap_style, hour) %>%
  summarise(mean_c = mean(temp_c, na.rm = TRUE),
            sd_c   = sd(temp_c,   na.rm = TRUE),
            .groups = "drop")

p3 <- ggplot(ib_hourly, aes(x = hour, y = mean_c,
                            colour   = roof_colour,
                            fill     = roof_colour,
                            linetype = trap_style,
                            group    = sensor_id)) +
  geom_ribbon(aes(ymin = mean_c - sd_c, ymax = mean_c + sd_c),
              alpha = 0.18, colour = NA) +
  geom_line(linewidth = 0.7) +
  scale_colour_manual(values = trap_palette, labels = trap_labels,
                      name = "Roof colour",
                      aesthetics = c("colour", "fill")) +
  scale_linetype_manual(values = trap_style_lines, labels = trap_style_labels,
                        name = "Trap style") +
  scale_x_continuous(breaks = seq(0, 23, 4)) +
  labs(title = "iButton diurnal pattern (mean ± SD by hour of day)",
       x = "Hour of day (EDT)", y = "Temperature (°C)",
       caption = wrap_caption(
         "Figure 3. Mean temperature by hour of day, pooled over the pilot
          period, with ±1 SD ribbon. The afternoon peak (13:00-16:00) is
          when roof-colour effects should be largest; flat overnight curves
          should converge across all sensors.",
         width = 105)) +
  base_theme

ggsave(file.path(out_dir, "fig3_ibutton_diurnal.png"),
       p3, width = 9, height = 5, dpi = 200)
print(p3)

# =============================================================================
# FIGURE 4 -- THERMOCOUPLE SPOT MEASUREMENTS OVER TIME
# =============================================================================

p4 <- ggplot(thermo_long,
             aes(x = datetime, y = temp_c,
                 colour = colour, fill = colour,
                 shape  = position,
                 group  = interaction(nest_id, position))) +
  geom_line(alpha = 0.6, linewidth = 0.4) +
  geom_point(size = 3, stroke = 0.6, colour = "black") +
  scale_colour_manual(values = trap_palette, labels = trap_labels,
                      name = "Trap colour") +
  scale_fill_manual(values = trap_palette, labels = trap_labels,
                    name = "Trap colour") +
  scale_shape_manual(values = c(back = 21, front = 24),
                     labels = c(back = "Back", front = "Front"),
                     name = "Probe position") +
  scale_x_datetime(date_labels = "%b %d\n%H:%M", date_breaks = "6 hours") +
  labs(title = "Thermocouple spot measurements per nest",
       x = NULL, y = "Temperature (°C)",
       caption = wrap_caption(
         "Figure 4. Manual thermocouple readings at the back and front of
          each of the 6 pilot nests, recorded May 7-8. Lines connect the
          same (nest, position) across timepoints. Black-coloured nests are
          expected to read warmer than tan, which in turn should be warmer
          than white at any given sampling time.",
         width = 105)) +
  base_theme

ggsave(file.path(out_dir, "fig4_thermocouple_timeseries.png"),
       p4, width = 9, height = 5, dpi = 200)
print(p4)

# =============================================================================
# FIGURE 5 -- THERMOCOUPLE TEMPERATURE BY TRAP COLOUR
# =============================================================================

p5 <- ggplot(thermo_long,
             aes(x = colour, y = temp_c, fill = colour)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA, width = 0.55,
               colour = "black") +
  geom_jitter(aes(shape = position),
              width = 0.15, height = 0,
              size = 2.4, stroke = 0.5, colour = "black") +
  scale_fill_manual(values = trap_palette,
                    labels = trap_labels, name = "Trap colour") +
  scale_shape_manual(values = c(back = 21, front = 24),
                     labels = c(back = "Back", front = "Front"),
                     name = "Probe position") +
  scale_x_discrete(labels = trap_labels) +
  labs(title = "Thermocouple readings by trap colour",
       x = NULL, y = "Temperature (°C)",
       caption = wrap_caption(
         "Figure 5. All thermocouple readings pooled across timepoints and
          nests, split by trap colour. Boxes show the median and IQR;
          points are individual readings (triangles = front probe, circles =
          back probe). A clear black > tan > white ordering is the pilot's
          headline result.",
         width = 105)) +
  base_theme

ggsave(file.path(out_dir, "fig5_thermocouple_by_colour.png"),
       p5, width = 7, height = 5, dpi = 200)
print(p5)

# =============================================================================
# FIGURE 6 -- BACK MINUS FRONT GRADIENT BY TRAP COLOUR
# =============================================================================
# Per spot measurement (nest, datetime), the within-nest spatial gradient is
# t_back - t_front. A positive value means the back of the cavity is warmer
# than the front. Plotted as a box-and-whisker by trap colour so colour-level
# differences in gradient size are visible.
# =============================================================================

thermo_diff <- thermo %>%
  mutate(diff_c = t_back - t_front,
         colour = factor(colour, levels = c("b", "t", "w")))

p6 <- ggplot(thermo_diff, aes(x = colour, y = diff_c, fill = colour)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40") +
  geom_boxplot(alpha = 0.6, outlier.shape = NA, width = 0.55,
               colour = "black") +
  geom_jitter(width = 0.15, height = 0,
              size = 2.4, stroke = 0.5, shape = 21, colour = "black") +
  scale_fill_manual(values = trap_palette,
                    labels = trap_labels, name = "Trap colour") +
  scale_x_discrete(labels = trap_labels) +
  labs(title = "Within-nest back–front temperature gradient by trap colour",
       x = NULL, y = "Back − front (°C)",
       caption = wrap_caption(
         "Figure 6. Within-nest spatial gradient computed per spot
          measurement as (back probe − front probe), grouped by trap colour.
          Positive values indicate the back of the cavity is warmer than the
          front; dashed line marks zero gradient. Larger gradients suggest
          stronger solar heating reaching the back of the nest box.",
         width = 105)) +
  base_theme

ggsave(file.path(out_dir, "fig6_thermocouple_back_front_diff.png"),
       p6, width = 7, height = 5, dpi = 200)
print(p6)

# =============================================================================
# FIGURE 7 -- BACK AND FRONT TEMPERATURES SIDE BY SIDE BY COLOUR
# =============================================================================
# Same data as Figure 5, but with back and front shown as separate dodged
# boxes within each colour group so the raw temperatures (not the gradient)
# are directly comparable.
# =============================================================================

p7 <- ggplot(thermo_long,
             aes(x = colour, y = temp_c,
                 fill = colour, alpha = position)) +
  geom_boxplot(position = position_dodge(width = 0.75),
               width    = 0.6,
               colour   = "black",
               outlier.shape = NA) +
  geom_point(aes(group = position),
             position = position_jitterdodge(jitter.width = 0.12,
                                             dodge.width  = 0.75),
             size = 2.2, stroke = 0.5, shape = 21, colour = "black") +
  scale_fill_manual(values = trap_palette,
                    labels = trap_labels, name = "Trap colour") +
  scale_alpha_manual(values = c(back = 0.85, front = 0.4),
                     labels = c(back = "Back", front = "Front"),
                     name   = "Probe position") +
  scale_x_discrete(labels = trap_labels) +
  labs(title = "Thermocouple back and front temperatures, side by side",
       x = NULL, y = "Temperature (°C)",
       caption = wrap_caption(
         "Figure 7. Raw thermocouple readings split into back (darker) and
          front (lighter) probes within each trap colour. Boxes show median
          and IQR; points are individual readings. This view preserves the
          absolute temperatures so colour-level differences in both back and
          front can be read directly off the y-axis.",
         width = 105)) +
  base_theme

ggsave(file.path(out_dir, "fig7_thermocouple_back_front_sidebyside.png"),
       p7, width = 8, height = 5, dpi = 200)
print(p7)

# --- Print quick numerical summaries ----------------------------------------

cat("\n=== iButton daily max by roof colour and trap style ===\n")
print(ib %>% mutate(date = as.Date(datetime)) %>%
        group_by(roof_colour, trap_style, date) %>%
        summarise(daily_max = max(temp_c, na.rm = TRUE), .groups = "drop") %>%
        pivot_wider(names_from = date, values_from = daily_max))

cat("\n=== Thermocouple mean per colour x position ===\n")
print(thermo_long %>%
        group_by(colour, position) %>%
        summarise(n       = n(),
                  mean_c  = round(mean(temp_c, na.rm = TRUE), 2),
                  sd_c    = round(sd(temp_c,   na.rm = TRUE), 2),
                  .groups = "drop"))

cat("\n=== Back - front gradient per colour ===\n")
print(thermo_diff %>%
        group_by(colour) %>%
        summarise(n         = n(),
                  mean_diff = round(mean(diff_c, na.rm = TRUE), 2),
                  sd_diff   = round(sd(diff_c,   na.rm = TRUE), 2),
                  min_diff  = round(min(diff_c,  na.rm = TRUE), 2),
                  max_diff  = round(max(diff_c,  na.rm = TRUE), 2),
                  .groups = "drop"))
