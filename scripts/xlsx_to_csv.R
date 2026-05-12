# xlsx_to_csv.R
# Converts all .xlsx files in raw_data subfolders to .csv
# Output CSVs are saved alongside the originals with the same base name.

library(readxl)

# --- Configuration -----------------------------------------------------------

data_dir <- "raw_data"   # relative to this script's location
overwrite <- FALSE        # set TRUE to re-convert already-existing CSVs

# -----------------------------------------------------------------------------

# Find all xlsx files recursively under data_dir
script_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
search_root <- file.path(script_dir, data_dir)

xlsx_files <- list.files(
  path       = search_root,
  pattern    = "\\.xlsx$",
  full.names = TRUE,
  recursive  = TRUE
)

if (length(xlsx_files) == 0) {
  message("No .xlsx files found under: ", search_root)
} else {
  message("Found ", length(xlsx_files), " .xlsx file(s). Converting...\n")
}

# Convert each file
results <- lapply(xlsx_files, function(f) {
  csv_path <- sub("\\.xlsx$", ".csv", f)

  if (file.exists(csv_path) && !overwrite) {
    message("  [SKIP] ", basename(f), " (CSV already exists)")
    return(invisible(NULL))
  }

  tryCatch({
    dat <- read_excel(f)
    write.csv(dat, csv_path, row.names = FALSE)
    message("  [OK]   ", basename(f), " -> ", basename(csv_path))
  }, error = function(e) {
    message("  [FAIL] ", basename(f), ": ", conditionMessage(e))
  })
})

message("\nDone.")
