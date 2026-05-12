# =============================================================================
# 01_clean_pilot_data.R
#
# Purpose: Clean raw pilot test data from two sensor types (iButtons and HOBOs)
#          and export tidy CSVs ready for downstream analysis.
#
# Inputs:  ../raw_data/Pilot Test/iButtons/*.csv      (DS1925 iButtons)
#          ../raw_data/Pilot Test/*.xlsx               (HOBO loggers)
#
# Outputs: ../clean_data/pilot_ibuttons_clean.csv
#          ../clean_data/pilot_hobos_clean.csv
#
# Sensor details:
#   - iButtons (DS1925): measure trap-nest thermal mass. CSV files with a
#     metadata header followed by Date/Time, Unit, Value columns. Temperature
#     resolution 0.5°C, sample rate 5 min.
#   - HOBOs: dual-channel temperature probes used for spatial variation
#     measurements. XLSX files with columns: #, Date-Time (EDT), and two
#     Temperature °C columns (Channel 1 and Channel 2).
#
# Notes:
#   - All timestamps are standardized to EDT.
#   - Temporal resolution is kept raw (no averaging).
#   - Roof color mapping is handled in a separate downstream script.
#   - Some iButtons span both calibration and pilot tests; splitting/trimming
#     will be handled in a later step.
# =============================================================================

library(readxl)
library(dplyr)
library(lubridate)

# --- Paths -------------------------------------------------------------------

# Use paths relative to the R project root (nest_warming/)
raw_dir   <- "../raw_data/Pilot Test"
clean_dir <- "clean_data"

# Create clean_data directory if it doesn't exist
if (!dir.exists(clean_dir)) dir.create(clean_dir, recursive = TRUE)

# =============================================================================
# 1. CLEAN iBUTTON DATA
# =============================================================================

cat("--- Cleaning iButton data ---\n")

ibutton_dir   <- file.path(raw_dir, "iButtons")
ibutton_files <- list.files(ibutton_dir, pattern = "\\.csv$", full.names = TRUE)

cat("Found", length(ibutton_files), "iButton file(s)\n")

ibutton_list <- lapply(ibutton_files, function(f) {

  # Read the full file to extract header metadata
  raw_lines <- readLines(f, warn = FALSE)

  # Extract sensor info from header
  sensor_model <- sub(".*Part Number:\\s*", "", raw_lines[1])
  sensor_id    <- sub(".*Registration Number:\\s*", "", raw_lines[2])

  # Find where the data starts (line after the blank line, starting with
  # "Date/Time")
  data_start <- which(grepl("^Date/Time", raw_lines))

  # Read the data portion
  dat <- read.csv(f, skip = data_start - 1, stringsAsFactors = FALSE)

  # Parse timestamp: format is DD/MM/YY H:MM:SS AM/PM
  dat$datetime <- parse_date_time(dat$Date.Time, orders = "dmyIMSp", tz = "US/Eastern")

  # Build clean data frame
  data.frame(
    sensor_id    = sensor_id,
    sensor_model = sensor_model,
    datetime     = dat$datetime,
    temp_c       = dat$Value,
    stringsAsFactors = FALSE
  )
})

ibutton_clean <- bind_rows(ibutton_list)

cat("  Total iButton records:", nrow(ibutton_clean), "\n")
cat("  Sensors:", paste(unique(ibutton_clean$sensor_id), collapse = ", "), "\n")
cat("  Date range:", as.character(min(ibutton_clean$datetime)),
    "to", as.character(max(ibutton_clean$datetime)), "\n")

# --- Export ------------------------------------------------------------------

write.csv(ibutton_clean,
          file.path(clean_dir, "pilot_ibuttons_clean.csv"),
          row.names = FALSE)
cat("  Saved:", file.path(clean_dir, "pilot_ibuttons_clean.csv"), "\n\n")

# =============================================================================
# 2. CLEAN HOBO DATA
# =============================================================================

cat("--- Cleaning HOBO data ---\n")

hobo_files <- list.files(raw_dir, pattern = "\\.xlsx$", full.names = TRUE)

cat("Found", length(hobo_files), "HOBO file(s)\n")

hobo_list <- lapply(hobo_files, function(f) {

  dat <- read_excel(f)

  # Extract the HOBO serial number from the filename (first token before space)
  hobo_id <- sub("^(\\d+)\\s.*", "\\1", basename(f))

  # Standardize column names — readxl appends ...3 / ...4 to duplicate names
  col_names <- colnames(dat)
  temp_cols <- grep("Temperature", col_names, value = TRUE)

  if (length(temp_cols) < 2) {
    warning("File ", basename(f), " has fewer than 2 temperature columns. Skipping.")
    return(NULL)
  }

  # Build clean data frame
  data.frame(
    sensor_id  = hobo_id,
    datetime   = dat[["Date-Time (EDT)"]],
    temp_c_ch1 = dat[[temp_cols[1]]],
    temp_c_ch2 = dat[[temp_cols[2]]],
    stringsAsFactors = FALSE
  )
})

hobo_clean <- bind_rows(hobo_list)

cat("  Total HOBO records:", nrow(hobo_clean), "\n")
cat("  Sensors:", paste(unique(hobo_clean$sensor_id), collapse = ", "), "\n")
cat("  Date range:", as.character(min(hobo_clean$datetime)),
    "to", as.character(max(hobo_clean$datetime)), "\n")

# --- Export ------------------------------------------------------------------

write.csv(hobo_clean,
          file.path(clean_dir, "pilot_hobos_clean.csv"),
          row.names = FALSE)
cat("  Saved:", file.path(clean_dir, "pilot_hobos_clean.csv"), "\n\n")

cat("--- Done! ---\n")
