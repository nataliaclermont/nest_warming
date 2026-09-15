# =============================================================================
# 07_pilot_stats.R
#
# Purpose: Inferential tests for the pilot-test thermocouple data.
#
#   Q1: Do back and front of the nest cavity differ in temperature?
#       -> Paired test on (back, front) within each (nest, datetime).
#
#   Q2: Do trap colours (black/tan/white) differ in temperature, pooling
#       back and front?
#       -> Linear mixed model with colour and position as fixed effects and
#          nest_id as a random intercept, to account for repeated sampling
#          within nests. Tukey-adjusted pairwise contrasts follow.
#
# Inputs:  ../raw_data/Pilot Test/nest_hobo_map.xlsx
#          ../raw_data/Pilot Test/pilot_thermocouple.xlsx
#
# Outputs: results/pilot/thermocouple_test_summary.csv
#
# Caveats:
#   - n is small (6 nests, 2 per colour, 4 timepoints, 2 positions = 48 obs).
#     Power is low; treat all p-values as exploratory pilot evidence.
#   - The mixed model accounts for within-nest correlation but does not
#     adjust for the small number of nests at each colour level.
# =============================================================================

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(lubridate)
  library(lme4)
  library(lmerTest)
  library(emmeans)
})

# --- Paths -------------------------------------------------------------------

raw_dir <- file.path("..", "raw_data", "Pilot Test")
out_dir <- file.path("results", "pilot")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

TZ <- "US/Eastern"

# =============================================================================
# LOAD + RESHAPE
# =============================================================================

hobo_map <- suppressMessages(read_excel(file.path(raw_dir, "nest_hobo_map.xlsx")))
nest_colour <- hobo_map %>% distinct(nest_id, colour) %>%
  mutate(nest_id = as.integer(nest_id))

thermo_raw <- suppressMessages(read_excel(file.path(raw_dir, "pilot_thermocouple.xlsx")))

thermo <- thermo_raw %>%
  mutate(time_str = sprintf("%02d:%02d:%02d",
                            hour(time), minute(time), second(time)),
         datetime = as.POSIXct(paste(as.Date(date), time_str), tz = TZ),
         nest_id  = as.integer(nest_id)) %>%
  select(datetime, nest_id, t_back, t_front) %>%
  left_join(nest_colour, by = "nest_id")

thermo_long <- thermo %>%
  pivot_longer(c(t_back, t_front),
               names_to = "position", values_to = "temp_c") %>%
  mutate(position = sub("t_", "", position),
         colour   = factor(colour,   levels = c("b", "t", "w")),
         position = factor(position, levels = c("back", "front")),
         nest_id  = factor(nest_id))

cat("Data: ", nrow(thermo_long), "rows | nests:",
    nlevels(thermo_long$nest_id), "| colours:",
    nlevels(thermo_long$colour), "| positions:",
    nlevels(thermo_long$position), "\n\n")

# =============================================================================
# Q1 -- BACK vs FRONT (PAIRED)
# =============================================================================

cat("=== Q1: BACK vs FRONT (paired within nest x datetime) ===\n")

paired_t <- t.test(thermo$t_back, thermo$t_front, paired = TRUE)
paired_w <- suppressWarnings(
  wilcox.test(thermo$t_back, thermo$t_front, paired = TRUE)
)

cat(sprintf(
  "Paired t-test:  n = %d pairs | mean diff = %+.2f °C | 95%% CI [%+.2f, %+.2f] | t = %.2f, df = %d, p = %.4g\n",
  nrow(thermo),
  paired_t$estimate,
  paired_t$conf.int[1], paired_t$conf.int[2],
  paired_t$statistic, paired_t$parameter, paired_t$p.value
))
cat(sprintf(
  "Wilcoxon signed-rank: V = %.0f, p = %.4g\n",
  paired_w$statistic, paired_w$p.value
))

# Same test stratified by colour (descriptive only).
cat("\nStratified by colour (paired t-test):\n")
strat <- thermo %>%
  group_by(colour) %>%
  summarise(n          = n(),
            mean_diff  = mean(t_back - t_front),
            sd_diff    = sd(t_back - t_front),
            t          = t.test(t_back, t_front, paired = TRUE)$statistic,
            df         = t.test(t_back, t_front, paired = TRUE)$parameter,
            p          = t.test(t_back, t_front, paired = TRUE)$p.value,
            .groups    = "drop") %>%
  mutate(across(c(mean_diff, sd_diff, t, p), \(x) round(x, 3)))
print(as.data.frame(strat))

# =============================================================================
# Q2 -- COLOUR EFFECT (POOLED BACK + FRONT) WITH MIXED MODEL
# =============================================================================

cat("\n=== Q2: COLOUR EFFECT (mixed model) ===\n")
cat("Model: temp_c ~ colour + position + (1 | nest_id)\n")
cat("Random intercept for nest_id accounts for repeated sampling.\n\n")

m1 <- lmer(temp_c ~ colour + position + (1 | nest_id),
           data = thermo_long, REML = TRUE)

# Type III F-test on fixed effects (lmerTest provides Satterthwaite df).
cat("Fixed effects (Type III F-tests, Satterthwaite df):\n")
print(anova(m1, type = 3))

cat("\nVariance components:\n")
print(VarCorr(m1))

cat("\nFixed-effect coefficient table:\n")
print(round(summary(m1)$coefficients, 4))

# --- Tukey pairwise comparisons between colours -----------------------------

cat("\nPairwise contrasts between colours (Tukey-adjusted):\n")
em_col <- emmeans(m1, ~ colour)
print(summary(em_col, infer = c(TRUE, TRUE)))
pw_col <- pairs(em_col, adjust = "tukey")
print(summary(pw_col, infer = c(TRUE, TRUE)))

# --- Position effect (single contrast) --------------------------------------

cat("\nPosition contrast (back − front, model-adjusted):\n")
em_pos <- emmeans(m1, ~ position)
print(summary(pairs(em_pos), infer = c(TRUE, TRUE)))

# --- Robustness check: collapsed one-way ANOVA on per-nest means ------------

cat("\n--- Robustness check: one-way ANOVA on per-nest mean temperature ---\n")
cat("(Avoids pseudoreplication entirely; n = 6 nests, df small.)\n")
per_nest <- thermo_long %>%
  group_by(nest_id, colour) %>%
  summarise(mean_c = mean(temp_c, na.rm = TRUE), .groups = "drop")
aov_simple <- aov(mean_c ~ colour, data = per_nest)
print(summary(aov_simple))
cat("\nTukey on collapsed data:\n")
print(TukeyHSD(aov_simple))

# =============================================================================
# WRITE A FLAT SUMMARY CSV
# =============================================================================

# Tidy summary row per test/comparison for the lab notebook / supplementary.
out_rows <- list()

# Q1: overall paired t-test
out_rows[[length(out_rows) + 1]] <- tibble(
  question  = "Q1 back vs front",
  test      = "Paired t-test (all data)",
  group     = "all",
  estimate  = unname(paired_t$estimate),
  ci_lo     = paired_t$conf.int[1],
  ci_hi     = paired_t$conf.int[2],
  statistic = unname(paired_t$statistic),
  df        = unname(paired_t$parameter),
  p_value   = paired_t$p.value
)

# Q1 stratified
for (i in seq_len(nrow(strat))) {
  out_rows[[length(out_rows) + 1]] <- tibble(
    question  = "Q1 back vs front",
    test      = "Paired t-test (within colour)",
    group     = as.character(strat$colour[i]),
    estimate  = strat$mean_diff[i],
    ci_lo     = NA_real_,
    ci_hi     = NA_real_,
    statistic = strat$t[i],
    df        = strat$df[i],
    p_value   = strat$p[i]
  )
}

# Q2: colour pairwise (Tukey from mixed model)
pw_df <- as.data.frame(pw_col)
for (i in seq_len(nrow(pw_df))) {
  out_rows[[length(out_rows) + 1]] <- tibble(
    question  = "Q2 colour effect",
    test      = "Tukey contrast (mixed model)",
    group     = pw_df$contrast[i],
    estimate  = pw_df$estimate[i],
    ci_lo     = NA_real_,
    ci_hi     = NA_real_,
    statistic = pw_df$t.ratio[i],
    df        = pw_df$df[i],
    p_value   = pw_df$p.value[i]
  )
}

# Q2: position effect (model-adjusted)
pos_df <- as.data.frame(pairs(em_pos))
out_rows[[length(out_rows) + 1]] <- tibble(
  question  = "Q2 position effect",
  test      = "Mixed-model contrast",
  group     = pos_df$contrast[1],
  estimate  = pos_df$estimate[1],
  ci_lo     = NA_real_,
  ci_hi     = NA_real_,
  statistic = pos_df$t.ratio[1],
  df        = pos_df$df[1],
  p_value   = pos_df$p.value[1]
)

out_df <- bind_rows(out_rows) %>%
  mutate(across(c(estimate, ci_lo, ci_hi, statistic, df, p_value),
                \(x) round(x, 4)))

write.csv(out_df, file.path(out_dir, "thermocouple_test_summary.csv"),
          row.names = FALSE)
cat("\nSaved:", file.path(out_dir, "thermocouple_test_summary.csv"), "\n")
