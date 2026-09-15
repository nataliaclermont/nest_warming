# =============================================================================
# 01_merge_nest_checks.R
#
# Purpose: Reshape a raw KoboToolbox nest-check export (two sheets: one trap
#          submission + its repeating cavity group) into a single long table,
#          one row per cavity observation carrying its trap-level context.
#          The output is an XLSX intended for additional manual cleaning.
#
# Inputs:  ../raw_data/<EXPORT_FOLDER>/NestChecks_nest_warming2026_*.xlsx
#            sheet "NestChecks_nest_warming2026" = one row per trap submission
#            sheet "group_sx7vm75"               = one row per cavity (repeat)
#          (XLSX is required: the CSV export drops the cavity repeat group.)
#
# Output:  ../clean_data/<EXPORT_FOLDER>/nest_checks_long_for_cleaning.xlsx
#
# Join key: cavity `_parent_index` == trap `_index`.
#
# Form history:
#  - Jun 2026: added cavity fields `bee_tracking`, `bee_id`.
#  - Jul 2026: added cavity fields `nest_construction`, `nest_provision`,
#              `initial_species_id`; to be backfilled manually from notes
#  - Sep 2026: renamed the `nest_construction` choice "resin" to "cellophane";
#              since there were no resin nests, resin is converted in this form
# =============================================================================

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(writexl)
})

# --- Paths -------------------------------------------------------------------
# Run from project root (nest_warming/). raw_data lives one level up.
export_folder <- "field_10_2026"
raw_dir       <- file.path("..", "raw_data", export_folder)
out_dir       <- file.path("clean_data", export_folder)

if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# Use the most recently modified XLSX export in the folder.
# (Excel lock files, e.g. "~$foo.xlsx", are excluded: they're not real exports.)
xlsx_files <- list.files(raw_dir, pattern = "\\.xlsx$", full.names = TRUE)
xlsx_files <- xlsx_files[!grepl("^~\\$", basename(xlsx_files))]
if (length(xlsx_files) == 0) stop("No XLSX export found in ", raw_dir)
xlsx <- xlsx_files[order(file.info(xlsx_files)$mtime, decreasing = TRUE)][1]
cat("Reading:", basename(xlsx), "\n")

# --- Read both sheets --------------------------------------------------------
# guess_max is set high (default is 1000) because readxl guesses each column's
# type from only the first `guess_max` rows, then coerces the rest to match. In
# the cavity sheet, sparsely-filled columns (e.g. nest_construction, bee_id) are
# blank for thousands of rows before their first real value; with the default,
# readxl guessed those columns as logical and silently discarded every real
# value below row 1000 as NA (visible as many "Expecting logical, got ..."
# warnings). Scanning the full sheet avoids that.
trap   <- read_excel(xlsx, sheet = "NestChecks_nest_warming2026", guess_max = 1e6)
cavity <- read_excel(xlsx, sheet = "group_sx7vm75", guess_max = 1e6)

# Keep only the meaningful trap-level fields as context for each cavity row.
# (The one-hot observer/* columns and Kobo bookkeeping are dropped for a tidy,
#  human-readable cleaning sheet; the raw file remains the source of record.)
trap_context <- trap %>%
  transmute(
    submission_id = `_id`,
    submission_uuid = `_uuid`,
    parent_index = `_index`,        # join key on the cavity side
    start, end, date_time,
    observer, site, trap_color, trap_notes
  )

# Keep the cavity fields that need to be cleaned; carry its parent join key.
# nest_construction is recoded so any legacy "resin" values (pre-rename) line
# up with the current "cellophane" choice; NA values pass through unchanged.
cavity_obs <- cavity %>%
  mutate(
    nest_construction = if_else(
      tolower(nest_construction) %in% "resin", "cellophane", nest_construction
    )
  ) %>%
  transmute(
    parent_index = `_parent_index`,
    cavity_index = `_index`,
    cavity_number, nest_succession, nest_completion, cell_number, nest_notes,
    nest_construction, nest_provision, initial_species_id,
    bee_tracking, bee_id,
    transfer_type, transfer_location,
    transfer_location_front, transfer_location_mid, transfer_location_back
  )

# --- Join: one row per cavity, carrying its trap-level context --------------
long <- cavity_obs %>%
  left_join(trap_context, by = "parent_index") %>%
  select(
    submission_id, submission_uuid, parent_index, start, end, date_time,
    observer, site, trap_color, trap_notes,
    cavity_index, cavity_number, nest_succession, nest_completion, cell_number,
    nest_notes, nest_construction, nest_provision, initial_species_id,
    bee_tracking, bee_id,
    transfer_type, transfer_location,
    transfer_location_front, transfer_location_mid, transfer_location_back
  )

# --- Write -------------------------------------------------------------------
out_file <- file.path(out_dir, "nest_checks_long_for_cleaning.xlsx")
write_xlsx(long, out_file)

cat("Wrote:", out_file, "\n")
cat("  trap submissions :", nrow(trap), "\n")
cat("  cavity rows      :", nrow(cavity), "\n")
cat("  long rows        :", nrow(long), "x", ncol(long), "cols\n")
cat("--- Done. ---\n")
