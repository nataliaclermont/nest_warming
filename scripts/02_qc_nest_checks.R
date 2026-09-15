# =============================================================================
# 02_qc_nest_checks.R
#
# Purpose: Quality-control script for KoboToolbox nest-check exports. Flags
#          suspect submissions and cavity records so they can be reviewed and
#          fixed manually in Kobo. Does NOT modify or clean the raw data.
#
# Inputs:  ../raw_data/<EXPORT_FOLDER>/NestChecks_nest_warming2026_*.xlsx
#          (XLSX is required: the CSV export drops the cavity repeat group)
#
# Outputs: ../clean_data/qc/<EXPORT_FOLDER>/qc_trap_flags.csv
#          ../clean_data/qc/<EXPORT_FOLDER>/qc_cavity_flags.csv
#          ../clean_data/qc/<EXPORT_FOLDER>/qc_coverage.csv
#
# Design assumptions (see project notes):
#   - 2 sites (A, B) x 4 subsites (1-4) x 3 trap colors (B, W, T) = 24 traps.
#   - Each trap has 10 nesting cavities (cavity_number 1-10).
#   - Each Kobo submission is one trap inspection with 10 cavity sub-rows.
# =============================================================================

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(lubridate)
})

# --- Paths -------------------------------------------------------------------

# Run from project root (nest_warming/). raw_data lives one level up.
export_folder  <- "May25_2026"
raw_dir        <- file.path("..", "raw_data", export_folder)
out_dir        <- file.path("clean_data", "qc", export_folder)

if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

xlsx_files <- list.files(raw_dir, pattern = "\\.xlsx$", full.names = TRUE)
if (length(xlsx_files) == 0) stop("No XLSX export found in ", raw_dir)
# Use the most recently modified file
xlsx <- xlsx_files[order(file.info(xlsx_files)$mtime, decreasing = TRUE)][1]

cat("Reading:", basename(xlsx), "\n\n")

trap   <- read_excel(xlsx, sheet = "NestChecks_nest_warming2026")
cavity <- read_excel(xlsx, sheet = "group_sx7vm75")

# Coerce common types up front
trap <- trap %>%
  mutate(
    start_dt     = ymd_hms(start, tz = "America/Toronto", quiet = TRUE),
    end_dt       = ymd_hms(end,   tz = "America/Toronto", quiet = TRUE),
    visit_dt     = ymd_hms(date_time, tz = "America/Toronto", quiet = TRUE),
    submit_dt    = ymd_hms(`_submission_time`, tz = "UTC", quiet = TRUE),
    visit_date   = as.Date(visit_dt),
    duration_min = as.numeric(difftime(end_dt, start_dt, units = "mins"))
  )

cavity <- cavity %>%
  mutate(
    cavity_number_int = suppressWarnings(as.integer(cavity_number)),
    cell_number_num   = suppressWarnings(as.numeric(cell_number))
  )

# Storage for flag rows ------------------------------------------------------

trap_flags   <- list()
cavity_flags <- list()

add_trap_flag <- function(df, code, msg) {
  if (nrow(df) == 0) return(invisible())
  trap_flags[[length(trap_flags) + 1]] <<- df %>%
    transmute(
      flag = code, message = msg,
      `_index`, `_uuid`, visit_date, visit_dt, site, trap_color, observer,
      duration_min, trap_notes
    )
}
add_cavity_flag <- function(df, code, msg) {
  if (nrow(df) == 0) return(invisible())
  cavity_flags[[length(cavity_flags) + 1]] <<- df %>%
    transmute(
      flag = code, message = msg,
      `_parent_index`, cavity_number, nest_succession, nest_completion,
      cell_number, nest_notes
    )
}

# =============================================================================
# TRAP-LEVEL CHECKS
# =============================================================================

cat("=== TRAP-LEVEL CHECKS ===\n")

# (T1) Same (site, color, date) submitted more than once -- likely accidental
#      re-submission when re-opening the Kobo form link.
dup_keys <- trap %>%
  count(site, trap_color, visit_date) %>%
  filter(n > 1) %>%
  select(-n)

dups <- trap %>%
  semi_join(dup_keys, by = c("site", "trap_color", "visit_date")) %>%
  arrange(site, trap_color, visit_dt)

cat("T1 DUP_SAME_DAY:", nrow(dups), "rows (",
    nrow(dup_keys), "site x color x date groups )\n")
add_trap_flag(dups, "DUP_SAME_DAY",
              "Same trap submitted >1x on same date - check for accidental re-submission")

# (T2) Site or trap_color missing
missing_id <- trap %>% filter(is.na(site) | is.na(trap_color))
cat("T2 MISSING_ID:", nrow(missing_id), "rows\n")
add_trap_flag(missing_id, "MISSING_ID", "site or trap_color is NA")

# (T3) Observer string vs one-hot mismatch
obs_cols <- grep("^observer/", colnames(trap), value = TRUE)
expected_obs_str <- apply(trap[obs_cols], 1, function(r) {
  sel <- obs_cols[as.integer(r) == 1 & !is.na(r)]
  paste(sub("^observer/", "", sel), collapse = " ")
})
obs_mismatch <- trap %>%
  mutate(expected_obs = expected_obs_str) %>%
  filter((observer %||% "") != expected_obs)
# %||% may not exist on older R; replace with explicit:
obs_mismatch <- trap %>%
  mutate(expected_obs = expected_obs_str) %>%
  filter(ifelse(is.na(observer), "", observer) != expected_obs)
cat("T3 OBSERVER_MISMATCH:", nrow(obs_mismatch), "rows\n")
add_trap_flag(obs_mismatch, "OBSERVER_MISMATCH",
              "observer string does not match one-hot columns")

# (T4) Submission durations
short_dur <- trap %>% filter(!is.na(duration_min), duration_min < 1)
long_dur  <- trap %>% filter(!is.na(duration_min), duration_min > 30)
cat("T4 SHORT_DURATION (<1 min):", nrow(short_dur), "rows\n")
cat("T4 LONG_DURATION  (>30 min):", nrow(long_dur), "rows\n")
add_trap_flag(short_dur, "SHORT_DURATION",
              "start->end < 1 min - partial form / re-submission?")
add_trap_flag(long_dur, "LONG_DURATION",
              "start->end > 30 min - form left open?")

# (T5) Submitted long after the visit -- signals re-submission or late entry
late_submit <- trap %>%
  mutate(submit_lag_hr = as.numeric(difftime(submit_dt, end_dt, units = "hours"))) %>%
  filter(!is.na(submit_lag_hr), submit_lag_hr > 24)
cat("T5 LATE_SUBMIT (>24 h after end):", nrow(late_submit), "rows\n")
add_trap_flag(late_submit, "LATE_SUBMIT",
              "Submitted >24h after form 'end' time")

# (T6) Rapid subsite hop -- same observer's consecutive submissions
#      to different subsites within 3 min. Possible mis-selection from dropdown.
trap_sorted <- trap %>%
  filter(!is.na(visit_dt)) %>%
  arrange(observer, visit_dt) %>%
  group_by(observer) %>%
  mutate(
    prev_site  = lag(site),
    prev_color = lag(trap_color),
    prev_dt    = lag(visit_dt),
    gap_min    = as.numeric(difftime(visit_dt, prev_dt, units = "mins"))
  ) %>%
  ungroup()

rapid_hop <- trap_sorted %>%
  filter(!is.na(prev_site),
         site != prev_site,
         gap_min < 3)
cat("T6 RAPID_SUBSITE_HOP:", nrow(rapid_hop),
    "rows (same observer changed subsite <3 min apart)\n")
add_trap_flag(rapid_hop, "RAPID_SUBSITE_HOP",
              "Same observer hopped subsites <3 min apart - mis-selected site?")

# =============================================================================
# CAVITY-LEVEL CHECKS
# =============================================================================

cat("\n=== CAVITY-LEVEL CHECKS ===\n")

# (C1) Duplicate cavity_number within a single parent visit
cav_dups <- cavity %>%
  group_by(`_parent_index`, cavity_number) %>%
  filter(n() > 1) %>%
  ungroup()
cat("C1 CAVITY_DUP:", nrow(cav_dups), "rows\n")
add_cavity_flag(cav_dups, "CAVITY_DUP",
                "cavity_number appears more than once in the same visit")

# (C2) Cavity number out of expected range 1-10
cav_oor <- cavity %>%
  filter(is.na(cavity_number_int) |
           cavity_number_int < 1 | cavity_number_int > 10)
cat("C2 CAVITY_OUT_OF_RANGE:", nrow(cav_oor), "rows\n")
add_cavity_flag(cav_oor, "CAVITY_OUT_OF_RANGE",
                "cavity_number not in 1-10")

# (C3) Parent visit with not-exactly-10 cavity rows
cav_counts <- cavity %>%
  count(`_parent_index`, name = "n_cavity") %>%
  filter(n_cavity != 10)
bad_visits <- cavity %>%
  semi_join(cav_counts, by = "_parent_index") %>%
  left_join(cav_counts, by = "_parent_index")
cat("C3 NON_10_CAVITIES:", nrow(cav_counts),
    "visit(s) with !=10 cavity rows\n")
add_cavity_flag(bad_visits, "NON_10_CAVITIES",
                "Parent visit does not have exactly 10 cavity rows")

# (C4) nest_completion is NA
nc_na <- cavity %>% filter(is.na(nest_completion))
cat("C4 COMPLETION_NA:", nrow(nc_na), "rows\n")
add_cavity_flag(nc_na, "COMPLETION_NA", "nest_completion is NA")

# (C5) Logical inconsistencies between completion + cell_number
empty_but_cells <- cavity %>%
  filter(nest_completion == "empty",
         !is.na(cell_number_num), cell_number_num > 0)
cat("C5a EMPTY_BUT_CELLS:", nrow(empty_but_cells), "rows\n")
add_cavity_flag(empty_but_cells, "EMPTY_BUT_CELLS",
                "nest_completion=empty but cell_number > 0")

active_no_cell <- cavity %>%
  filter(nest_completion %in% c("incomplete", "abandoned"),
         is.na(cell_number_num))
cat("C5b ACTIVE_NO_CELL:", nrow(active_no_cell), "rows\n")
add_cavity_flag(active_no_cell, "ACTIVE_NO_CELL",
                "nest_completion=incomplete/abandoned but cell_number missing")

# (C6) Partial-cell info hidden in nest_notes (legacy: cell_number was integer).
#      Flags rows where nest_notes contains a decimal pattern like 0.5, 1.3, etc.
partial_in_notes <- cavity %>%
  filter(!is.na(nest_notes),
         grepl("\\b\\d+\\.\\d+\\b", nest_notes))
cat("C6 PARTIAL_IN_NOTES:", nrow(partial_in_notes),
    "rows (decimals in nest_notes - re-enter once form allows decimals)\n")
add_cavity_flag(partial_in_notes, "PARTIAL_IN_NOTES",
                "nest_notes contains decimal value - cell_number should be re-entered as decimal")

# =============================================================================
# COVERAGE: visits per site x color (expected >= 4 per trap by 2026-05-25)
# =============================================================================

cat("\n=== COVERAGE ===\n")

EXPECTED_MIN_VISITS <- 4L

sites  <- c(paste0("A", 1:4), paste0("B", 1:4))
colors <- c("B", "W", "T")
expected_grid <- expand.grid(site = sites, trap_color = colors,
                             stringsAsFactors = FALSE)

# Counting unique submissions (not deduped) -- duplicates inflate, but we
# already flag them above.
coverage <- trap %>%
  count(site, trap_color, name = "n_visits") %>%
  right_join(expected_grid, by = c("site", "trap_color")) %>%
  mutate(
    n_visits     = ifelse(is.na(n_visits), 0L, n_visits),
    under_target = n_visits < EXPECTED_MIN_VISITS
  ) %>%
  arrange(site, trap_color)

cat("Visits per trap (target >=", EXPECTED_MIN_VISITS, "):\n")
print(as.data.frame(coverage), row.names = FALSE)

n_under <- sum(coverage$under_target)
cat("\nTraps under target:", n_under, "/", nrow(coverage), "\n")

# =============================================================================
# WRITE OUTPUTS
# =============================================================================

cat("\n=== WRITING OUTPUTS ===\n")

trap_flags_df   <- if (length(trap_flags))   bind_rows(trap_flags)   else NULL
cavity_flags_df <- if (length(cavity_flags)) bind_rows(cavity_flags) else NULL

write.csv(coverage,
          file.path(out_dir, "qc_coverage.csv"),
          row.names = FALSE)
cat("  Saved:", file.path(out_dir, "qc_coverage.csv"), "\n")

if (!is.null(trap_flags_df) && nrow(trap_flags_df) > 0) {
  write.csv(trap_flags_df,
            file.path(out_dir, "qc_trap_flags.csv"),
            row.names = FALSE)
  cat("  Saved:", file.path(out_dir, "qc_trap_flags.csv"),
      "(", nrow(trap_flags_df), "flag rows )\n")
} else {
  cat("  No trap-level flags raised.\n")
}

if (!is.null(cavity_flags_df) && nrow(cavity_flags_df) > 0) {
  write.csv(cavity_flags_df,
            file.path(out_dir, "qc_cavity_flags.csv"),
            row.names = FALSE)
  cat("  Saved:", file.path(out_dir, "qc_cavity_flags.csv"),
      "(", nrow(cavity_flags_df), "flag rows )\n")
} else {
  cat("  No cavity-level flags raised.\n")
}

# =============================================================================
# SUMMARY
# =============================================================================

cat("\n=== SUMMARY ===\n")
if (!is.null(trap_flags_df)) {
  cat("Trap-level flag counts:\n")
  print(trap_flags_df %>% count(flag, name = "n") %>% arrange(desc(n)))
}
if (!is.null(cavity_flags_df)) {
  cat("\nCavity-level flag counts:\n")
  print(cavity_flags_df %>% count(flag, name = "n") %>% arrange(desc(n)))
}

cat("\n--- Done. Review the flag CSVs and fix records in Kobo as needed. ---\n")
