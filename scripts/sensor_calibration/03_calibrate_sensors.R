# =============================================================================
# 03_calibrate_sensors.R
#
# Purpose: Characterize per-sensor offsets for the iButtons, HOBO channels, and
#          the field thermocouple against the NIST reference thermometer using
#          the May 1 2026 two-point bath calibration (hot ~42 C + ice 0 C).
#          Produces a SEPARATE offset / correction table -- raw clean data is
#          NOT modified. Corrections can be applied downstream as desired.
#
# Inputs:  ../raw_data/Calibration Test May 2026/reference_and_thermocouple.xlsx
#          ../raw_data/Calibration Test May 2026/DS1925/*.csv
#          ../raw_data/Calibration Test May 2026/DS19121G-F5/*.csv
#          ../raw_data/Calibration Test May 2026/HOBOs/*.xlsx
#
# Outputs: clean_data/calibration/reference_clean.csv
#          clean_data/calibration/sensor_session_long.csv
#          clean_data/calibration/sensor_calibration_long.csv
#          clean_data/calibration/sensor_offsets.csv
#          clean_data/calibration/sensor_corrections.csv
#
# Method (best practice for low-resolution thermologgers):
#   1. Reference clock in source file is time-of-day only; the calibration
#      session ran in the afternoon of 2026-05-01, so PM (+12 h) is assumed
#      and timestamps are anchored to that date in America/Toronto (EDT).
#   2. Two bath classes from NIST: hot = NIST > 30 C, ice = NIST < 5 C.
#   3. For each bath, restrict to a documented STABLE window (NIST plateau,
#      post-equilibration) to avoid biasing the offset with transient ramps.
#   4. Per sensor x bath: mean temperature, SD, n, and offset relative to
#      mean NIST in the same window.
#   5. Per sensor: 2-point linear correction
#          T_true = slope * T_raw + intercept
#      derived from the (hot, ice) plateau means. Saved as a tidy table to be
#      applied (or not) downstream.
#
# Notes:
#   - Raw clean files (pilot_*_clean.csv) are NEVER overwritten here.
#   - DS1921G-F5 iButtons live in a folder mis-typed "DS19121G-F5"; sensor
#     model is taken from each file header, which is authoritative.
#   - HOBO files have two probes per logger; each channel is treated as its
#     own sensor with id "<serial>_ch1" / "<serial>_ch2", matching the column
#     naming in clean_data/pilot_hobos_clean.csv.
#   - readxl returns datetimes labelled UTC, but the column is "Date-Time
#     (EDT)" -- we force_tz() to America/Eastern without shifting the clock.
#   - The HOBOs folder may contain stray launch-test files with only a handful
#     of rows. Files whose datetime range does not overlap the calibration day
#     are dropped (kept readable in raw_data, just excluded here).
#   - The thermocouple was only co-logged for a subset of the session; it is
#     included where data exists but produces no two-point correction if it
#     lacks one of the baths.
# =============================================================================

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(lubridate)
})

# --- Paths -------------------------------------------------------------------

calib_root   <- file.path("..", "raw_data", "Calibration Test May 2026")
ref_file     <- file.path(calib_root, "reference_and_thermocouple.xlsx")
ibutton_dirs <- file.path(calib_root, c("DS1925", "DS19121G-F5"))
hobo_dir     <- file.path(calib_root, "HOBOs")

out_dir <- file.path("clean_data", "calibration")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# --- Constants ---------------------------------------------------------------

CALIB_DATE <- as.Date("2026-05-01")
TZ         <- "US/Eastern"

# Stable plateau windows (PM clock-time on 2026-05-01), chosen from NIST trace:
#   - Hot bath: NIST stabilized 15:54-16:13 between heating ramp and the
#     decline that began when the bath was swapped out at ~16:21. Used as
#     the hot anchor for the slow-sampling loggers (iButtons + HOBOs).
#   - Ice bath: 16:56-17:11 (NIST steady at 0.0 +/- 0.1 C the entire time).
#   - Thermocouple-only hot window: 16:25-16:30, the 5-min stretch where the
#     thermocouple was co-logged with NIST during the hot-bath cool-down.
#     NIST is drifting here, so we use a paired anchor: NIST mean over the
#     SAME 5-min window (not the static plateau mean). This is appropriate
#     because the thermocouple samples every minute and tracks NIST 1:1.
HOT_WINDOW        <- as.POSIXct(c("2026-05-01 15:54:00", "2026-05-01 16:13:00"),
                                tz = TZ)
ICE_WINDOW        <- as.POSIXct(c("2026-05-01 16:56:00", "2026-05-01 17:11:00"),
                                tz = TZ)
THERMO_HOT_WINDOW <- as.POSIXct(c("2026-05-01 16:25:00", "2026-05-01 16:30:00"),
                                tz = TZ)

# =============================================================================
# 1. REFERENCE + THERMOCOUPLE
# =============================================================================

cat("--- Reading reference thermometer + thermocouple ---\n")

ref_raw <- suppressMessages(read_excel(ref_file))

# Convert clock-only Time (anchored to 1899-12-31 in Excel) to a real
# datetime on the calibration date, shifted to PM (+12 h).
clock_str  <- format(ref_raw$Time, "%H:%M:%S")
ref <- data.frame(
  datetime = as.POSIXct(paste(CALIB_DATE, clock_str), tz = TZ) + hours(12),
  nist_c   = as.numeric(ref_raw$NIST_Reference),
  thermo_c = suppressWarnings(as.numeric(ref_raw$Thermocouple)),
  stringsAsFactors = FALSE
)

# Bath class from NIST temperature (ambient room temp is excluded as NA).
ref$bath <- case_when(
  ref$nist_c > 30 ~ "hot",
  ref$nist_c < 5  ~ "ice",
  TRUE            ~ NA_character_
)

cat("  Rows:", nrow(ref),
    "| Hot:", sum(ref$bath == "hot", na.rm = TRUE),
    "| Ice:", sum(ref$bath == "ice", na.rm = TRUE), "\n")
cat("  NIST hot range:",
    sprintf("%.2f-%.2f C", min(ref$nist_c[ref$bath == "hot"], na.rm = TRUE),
            max(ref$nist_c[ref$bath == "hot"], na.rm = TRUE)), "\n")
cat("  NIST ice range:",
    sprintf("%.2f-%.2f C", min(ref$nist_c[ref$bath == "ice"], na.rm = TRUE),
            max(ref$nist_c[ref$bath == "ice"], na.rm = TRUE)), "\n")

write.csv(ref, file.path(out_dir, "reference_clean.csv"), row.names = FALSE)
cat("  Saved:", file.path(out_dir, "reference_clean.csv"), "\n\n")

# Mean NIST inside each window -- the "true" value the logger means are
# compared against. Hot has two anchors: the static plateau for loggers,
# and a separate paired window for the thermocouple cool-down period.
nist_hot_mean        <- mean(ref$nist_c[ref$datetime >= HOT_WINDOW[1] &
                                        ref$datetime <= HOT_WINDOW[2]],
                             na.rm = TRUE)
nist_ice_mean        <- mean(ref$nist_c[ref$datetime >= ICE_WINDOW[1] &
                                        ref$datetime <= ICE_WINDOW[2]],
                             na.rm = TRUE)
nist_thermo_hot_mean <- mean(ref$nist_c[ref$datetime >= THERMO_HOT_WINDOW[1] &
                                        ref$datetime <= THERMO_HOT_WINDOW[2]],
                             na.rm = TRUE)
cat(sprintf(
  "  NIST anchors: hot-plateau = %.3f C, hot-thermo = %.3f C, ice = %.3f C\n\n",
  nist_hot_mean, nist_thermo_hot_mean, nist_ice_mean))

# =============================================================================
# 2. LOAD LOGGER DATA (iButtons + HOBOs)
# =============================================================================

cat("--- Loading iButton calibration files ---\n")

read_ibutton <- function(f) {
  raw_lines <- readLines(f, warn = FALSE)
  sensor_model <- sub(".*Part Number:\\s*", "", raw_lines[1])
  sensor_id    <- sub(".*Registration Number:\\s*", "", raw_lines[2])
  data_start   <- which(grepl("^Date/Time", raw_lines))
  dat <- read.csv(f, skip = data_start - 1, stringsAsFactors = FALSE)
  dat$datetime <- suppressWarnings(
    parse_date_time(dat$Date.Time, orders = "dmyIMSp", tz = TZ)
  )
  data.frame(
    sensor_type  = "iButton",
    sensor_model = sensor_model,
    sensor_id    = sensor_id,
    datetime     = dat$datetime,
    temp_c       = suppressWarnings(as.numeric(dat$Value)),
    stringsAsFactors = FALSE
  )
}

ibutton_files <- unlist(lapply(ibutton_dirs,
                               list.files, pattern = "\\.csv$",
                               full.names = TRUE))
cat("  Found", length(ibutton_files), "iButton file(s)\n")
ibutton_long <- bind_rows(lapply(ibutton_files, read_ibutton))
cat("  Rows:", nrow(ibutton_long),
    "| Sensors:", length(unique(ibutton_long$sensor_id)), "\n\n")

cat("--- Loading HOBO calibration files ---\n")

read_hobo <- function(f) {
  dat <- suppressMessages(read_excel(f))
  hobo_id   <- sub("^(\\d+)\\s.*", "\\1", basename(f))
  temp_cols <- grep("Temperature", colnames(dat), value = TRUE)
  if (length(temp_cols) < 2) {
    warning("File ", basename(f), " has < 2 temperature columns. Skipping.")
    return(NULL)
  }
  # readxl labels the datetime as UTC; the column is actually wall-clock EDT.
  dt <- force_tz(dat[["Date-Time (EDT)"]], tzone = TZ)
  # Drop launch-test files whose data doesn't overlap the calibration day.
  if (!any(as.Date(dt) == CALIB_DATE, na.rm = TRUE)) {
    message("  Skipping ", basename(f),
            " (no data on ", CALIB_DATE, ")")
    return(NULL)
  }
  bind_rows(
    data.frame(sensor_type = "HOBO", sensor_model = "HOBO",
               sensor_id   = paste0(hobo_id, "_ch1"),
               datetime    = dt,
               temp_c      = as.numeric(dat[[temp_cols[1]]]),
               stringsAsFactors = FALSE),
    data.frame(sensor_type = "HOBO", sensor_model = "HOBO",
               sensor_id   = paste0(hobo_id, "_ch2"),
               datetime    = dt,
               temp_c      = as.numeric(dat[[temp_cols[2]]]),
               stringsAsFactors = FALSE)
  )
}

hobo_files <- list.files(hobo_dir, pattern = "\\.xlsx$", full.names = TRUE)
cat("  Found", length(hobo_files), "HOBO file(s)\n")
hobo_long <- bind_rows(lapply(hobo_files, read_hobo))
cat("  Rows:", nrow(hobo_long),
    "| Sensor-channels:", length(unique(hobo_long$sensor_id)), "\n\n")

# Thermocouple as a "sensor" too, so it flows through the same pipeline.
thermo_long <- ref %>%
  filter(!is.na(thermo_c)) %>%
  transmute(sensor_type = "Thermocouple",
            sensor_model = "Thermocouple",
            sensor_id    = "thermocouple",
            datetime     = datetime,
            temp_c       = thermo_c)

logger_long <- bind_rows(ibutton_long, hobo_long, thermo_long) %>%
  filter(!is.na(datetime), !is.na(temp_c))

# Tag each reading with its bath window (NA if outside). The thermocouple
# uses its own hot window (paired cool-down period); all other sensors use
# the static hot plateau.
logger_long <- logger_long %>%
  mutate(
    in_hot_plateau = datetime >= HOT_WINDOW[1]        & datetime <= HOT_WINDOW[2],
    in_hot_thermo  = datetime >= THERMO_HOT_WINDOW[1] & datetime <= THERMO_HOT_WINDOW[2],
    in_ice         = datetime >= ICE_WINDOW[1]        & datetime <= ICE_WINDOW[2],
    bath = case_when(
      sensor_type == "Thermocouple" & in_hot_thermo  ~ "hot",
      sensor_type != "Thermocouple" & in_hot_plateau ~ "hot",
      in_ice                                         ~ "ice",
      TRUE                                           ~ NA_character_
    )
  ) %>%
  select(-in_hot_plateau, -in_hot_thermo, -in_ice)

calib_long <- logger_long %>% filter(!is.na(bath))

# Full session view (kept for downstream time-series plotting). Trimmed to
# the calibration date so we don't carry along deployment-period data that
# happens to share the same logger file.
session_long <- logger_long %>%
  filter(as.Date(datetime) == CALIB_DATE)
write.csv(session_long,
          file.path(out_dir, "sensor_session_long.csv"),
          row.names = FALSE)
cat("  Saved:", file.path(out_dir, "sensor_session_long.csv"),
    "(", nrow(session_long), "rows )\n")

cat("--- Stable-window readings per sensor x bath ---\n")
print(calib_long %>% count(sensor_type, bath))
cat("\n")

write.csv(calib_long,
          file.path(out_dir, "sensor_calibration_long.csv"),
          row.names = FALSE)
cat("  Saved:", file.path(out_dir, "sensor_calibration_long.csv"), "\n\n")

# =============================================================================
# 3. PER-SENSOR x BATH OFFSETS
# =============================================================================

cat("--- Computing offsets ---\n")

# NIST anchor depends on (sensor_type, bath) because the thermocouple uses a
# different hot window than the slow-sampling loggers.
nist_means <- tibble(
  sensor_type = c("HOBO",         "iButton",      "Thermocouple",
                  "HOBO",         "iButton",      "Thermocouple"),
  bath        = c("hot",          "hot",          "hot",
                  "ice",          "ice",          "ice"),
  nist_c      = c(nist_hot_mean,  nist_hot_mean,  nist_thermo_hot_mean,
                  nist_ice_mean,  nist_ice_mean,  nist_ice_mean)
)

offsets <- calib_long %>%
  group_by(sensor_type, sensor_model, sensor_id, bath) %>%
  summarise(n          = n(),
            mean_temp  = mean(temp_c, na.rm = TRUE),
            sd_temp    = sd(temp_c,   na.rm = TRUE),
            .groups    = "drop") %>%
  left_join(nist_means, by = c("sensor_type", "bath")) %>%
  mutate(offset_c = mean_temp - nist_c) %>%
  arrange(sensor_type, sensor_id, bath)

cat("  ", nrow(offsets), "sensor x bath rows\n")
write.csv(offsets, file.path(out_dir, "sensor_offsets.csv"), row.names = FALSE)
cat("  Saved:", file.path(out_dir, "sensor_offsets.csv"), "\n\n")

# =============================================================================
# 4. TWO-POINT LINEAR CORRECTION PER SENSOR
# =============================================================================
# Fit y = slope * x + intercept where y = NIST true value and x = logger reading.
# Requires both bath plateaus; sensors missing one bath are flagged but kept.
# =============================================================================

cat("--- Building 2-point corrections ---\n")

corrections <- offsets %>%
  select(sensor_type, sensor_model, sensor_id, bath, mean_temp, nist_c, n) %>%
  pivot_wider(names_from = bath,
              values_from = c(mean_temp, nist_c, n),
              names_glue  = "{bath}_{.value}") %>%
  mutate(
    slope     = (hot_nist_c - ice_nist_c) / (hot_mean_temp - ice_mean_temp),
    intercept = ice_nist_c - slope * ice_mean_temp,
    has_both_baths = !is.na(slope) & !is.na(intercept),
    calib_date     = CALIB_DATE
  ) %>%
  select(sensor_type, sensor_model, sensor_id,
         slope, intercept, has_both_baths,
         hot_n, hot_mean_temp, hot_nist_c,
         ice_n, ice_mean_temp, ice_nist_c,
         calib_date) %>%
  arrange(sensor_type, sensor_id)

n_ok      <- sum(corrections$has_both_baths)
n_missing <- sum(!corrections$has_both_baths)
cat("  Sensors with 2-point correction:", n_ok, "\n")
if (n_missing > 0) {
  cat("  Sensors missing a bath (no correction fit):", n_missing, "\n")
  print(corrections %>% filter(!has_both_baths) %>%
          select(sensor_id, hot_n, ice_n))
}

write.csv(corrections,
          file.path(out_dir, "sensor_corrections.csv"), row.names = FALSE)
cat("  Saved:", file.path(out_dir, "sensor_corrections.csv"), "\n\n")

# =============================================================================
# 5. SUMMARY
# =============================================================================

cat("=== SUMMARY ===\n")
cat("Offsets (mean +/- SD across sensors, per bath):\n")
print(offsets %>%
        group_by(sensor_type, bath) %>%
        summarise(n_sensors     = n(),
                  mean_offset_c = round(mean(offset_c, na.rm = TRUE), 3),
                  sd_offset_c   = round(sd(offset_c,   na.rm = TRUE), 3),
                  .groups       = "drop"))

cat("\nLargest |offset| outliers (top 5):\n")
print(offsets %>%
        arrange(desc(abs(offset_c))) %>%
        select(sensor_type, sensor_id, bath, mean_temp, nist_c, offset_c) %>%
        head(5))

cat("\n--- Done. Corrections stored separately; raw clean data untouched. ---\n")
