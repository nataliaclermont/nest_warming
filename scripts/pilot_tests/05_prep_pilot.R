# =============================================================================
# 05_prep_pilot.R
#
# Purpose: Trim the tidy pilot data to the experimental period and prepare an
#          analysis-ready dataset. Does NOT apply calibration corrections --
#          that is a separate downstream step.
#
# Inputs:  clean_data/{test_name}_ibuttons_clean.csv
#          clean_data/{test_name}_hobos_clean.csv
#          ../raw_data/test_windows.csv   (start/end times per test)
#
# Outputs: clean_data/{test_name}_ibuttons_trimmed.csv
#          clean_data/{test_name}_hobos_trimmed.csv
#          clean_data/{test_name}_long.csv   (combined long format, one row
#                                             per sensor x channel x timestamp;
#                                             ready for plots)
# =============================================================================

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tidyr)
  library(lubridate)
})

# --- Config ------------------------------------------------------------------

test_name <- "pilot1"

TZ        <- "US/Eastern"
clean_dir <- "clean_data"

# Load test window from CSV
windows   <- read.csv("../raw_data/test_windows.csv", stringsAsFactors = FALSE)
test_row  <- windows[windows$test_name == test_name, ]

if (nrow(test_row) == 0) stop("No entry found in test_windows.csv for test_name: ", test_name)

TEST_START <- as.POSIXct(test_row$start, tz = TZ)
TEST_END   <- as.POSIXct(test_row$end,   tz = TZ)

# --- Load --------------------------------------------------------------------

retag <- function(df) {
  df$datetime <- force_tz(df$datetime, TZ)
  df
}

ib <- retag(suppressMessages(
  read_csv(file.path(clean_dir, paste0(test_name, "_ibuttons_clean.csv")))
))
ho <- retag(suppressMessages(
  read_csv(file.path(clean_dir, paste0(test_name, "_hobos_clean.csv")))
))

# --- Trim --------------------------------------------------------------------

ib_trim <- ib %>% filter(datetime >= TEST_START, datetime <= TEST_END) %>% arrange(sensor_id, datetime)
ho_trim <- ho %>% filter(datetime >= TEST_START, datetime <= TEST_END) %>% arrange(sensor_id, datetime)

write.csv(ib_trim, file.path(clean_dir, paste0(test_name, "_ibuttons_trimmed.csv")),
          row.names = FALSE)
write.csv(ho_trim, file.path(clean_dir, paste0(test_name, "_hobos_trimmed.csv")),
          row.names = FALSE)

# --- Combined long format (one row per sensor channel x timestamp) ----------
# Makes it trivial to overlay everything on one plot and to facet by sensor
# type without juggling wide HOBO columns.

ib_long <- ib_trim %>%
  transmute(sensor_type  = "iButton",
            sensor_model = sensor_model,
            sensor_id    = sensor_id,
            channel      = NA_character_,
            datetime     = datetime,
            temp_c       = temp_c,
            roof_colour  = roof_colour,
            trap_style   = trap_style,
            trap_id      = trap_id)

ho_long <- ho_trim %>%
  pivot_longer(c(temp_c_ch1, temp_c_ch2),
               names_to  = "channel",
               values_to = "temp_c") %>%
  mutate(channel      = sub("temp_c_", "", channel),
         sensor_type  = "HOBO",
         sensor_model = "HOBO",
         sensor_id    = as.character(sensor_id)) %>%
  select(sensor_type, sensor_model, sensor_id, channel, datetime, temp_c)

test_long <- bind_rows(ib_long, ho_long)
write.csv(test_long, file.path(clean_dir, paste0(test_name, "_long.csv")),
          row.names = FALSE)

# --- Summary -----------------------------------------------------------------

cat("=== Dataset summary ===\n")
cat("Test:", test_name, "\n")
cat("Window:", as.character(TEST_START), "to", as.character(TEST_END), TZ, "\n\n")

ib_summary <- ib_trim %>%
  group_by(sensor_id, sensor_model, roof_colour, trap_style, trap_id) %>%
  summarise(n      = n(),
            first  = min(datetime),
            last   = max(datetime),
            days   = round(as.numeric(difftime(max(datetime), min(datetime),
                                               units = "days")), 2),
            min_c  = min(temp_c, na.rm = TRUE),
            max_c  = max(temp_c, na.rm = TRUE),
            .groups = "drop")
cat("iButtons:\n"); print(as.data.frame(ib_summary), row.names = FALSE)

ho_summary <- ho_trim %>%
  group_by(sensor_id) %>%
  summarise(n      = n(),
            first  = min(datetime),
            last   = max(datetime),
            days   = round(as.numeric(difftime(max(datetime), min(datetime),
                                               units = "days")), 2),
            min_ch1 = min(temp_c_ch1, na.rm = TRUE),
            max_ch1 = max(temp_c_ch1, na.rm = TRUE),
            min_ch2 = min(temp_c_ch2, na.rm = TRUE),
            max_ch2 = max(temp_c_ch2, na.rm = TRUE),
            .groups = "drop")
cat("\nHOBOs:\n"); print(as.data.frame(ho_summary), row.names = FALSE)
