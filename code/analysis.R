# Title: Main data analysis
# Description: Bind information from separate sources to put together main analysis
# Author: Toby Kramer
# Date: 2025-10-29


# ---- Setup ----
source(here::here("code", "src", "setup.R"))
source(here::here("code", "src", "data.R"))
source(here::here("code", "src", "func.R"))
source(here::here("code", "src", "stat.R"))


# Demographics ------------------------------------------------------------

# NOTE: subjects.csv (public data tier) is already filtered to participants
# who completed all four sessions, so the original `filter(sessions_complete
# == TRUE)` step is not needed here (no such column exists in the export).

demographic_d <- subjects %>%
  dplyr::mutate(bmi = weight_kg / (height_m^2)) %>%
  group_by(sex) %>%
  summarise(
    n = n(),
    age = sprintf("%.1f ± %.1f", mean(age, na.rm = TRUE), sd(age, na.rm = TRUE)),
    height = sprintf("%.2f ± %.2f", mean(height_m, na.rm = TRUE), sd(height_m, na.rm = TRUE)),
    weight = sprintf("%.1f ± %.1f", mean(weight_kg, na.rm = TRUE), sd(weight_kg, na.rm = TRUE)),
    bmi = sprintf("%.1f ± %.1f", mean(bmi, na.rm = TRUE), sd(bmi, na.rm = TRUE))
  ) %>% # add top row with general information and SD values
  bind_rows(
    subjects %>%
      dplyr::mutate(bmi = weight_kg / (height_m^2)) %>%
      summarise(
        sex = "all",
        n = n(),
        age = sprintf("%.1f ± %.1f", mean(age, na.rm = TRUE), sd(age, na.rm = TRUE)),
        height = sprintf("%.2f ± %.2f", mean(height_m, na.rm = TRUE), sd(height_m, na.rm = TRUE)),
        weight = sprintf("%.1f ± %.1f", mean(weight_kg, na.rm = TRUE), sd(weight_kg, na.rm = TRUE)),
        bmi = sprintf("%.1f ± %.1f", mean(bmi, na.rm = TRUE), sd(bmi, na.rm = TRUE))
      ),
    .
  )


# A priori power analysis -------------------------------------------------
# Porras-Salazar et al. (2024) report F(3, 138) = 0.52 for their CAT-equivalent
# test, giving ηp² = 0.011 (back-calculated). Their power analysis assumed d = 0.20,
# which corresponds to Cohen's f ≈ 0.10 (ηp² ≈ 0.01). We use ηp² = 0.01 as the
# equivalence bound: the Cohen (1988) "negligible" threshold and the smallest effect
# the Singapore study was designed to detect.
porras_f <- sqrt(0.01 / (1 - 0.01))   # Cohen's f = 0.100 (used by TOST functions below)
# RM-ANOVA power computed after LMM fit (requires ICC); see power_rm below.


# Environmental Variables -------------------------------------------------

# NOTE: env_sessions is already averaged to one row per session (the public
# data tier does not include per-reading sensor logs). The SD below therefore
# reflects between-session variability in session-mean temperature, not the
# finer within-session, per-reading variability reported in the manuscript's
# environmental conditions table. See data/README.md.

env_summary <- env_sessions %>%
  group_by(session_type) %>%
  summarise(
    t_air = sprintf("%.1f (%.1f)", mean(t_air_c, na.rm = TRUE), sd(t_air_c, na.rm = TRUE)),
    rh    = sprintf("%.1f (%.1f)", mean(rh_percent, na.rm = TRUE), sd(rh_percent, na.rm = TRUE)),
    co2   = sprintf("%.0f (%.0f)", mean(co2_ppm, na.rm = TRUE), sd(co2_ppm, na.rm = TRUE)),
  ) %>%
  ungroup()



# Thermal Perception ------------------------------------------------------

# Dataframes for Rmarkdown reference
thermal_sensation_d <- stacked_pct_data(survey, thermal_sensation)
thermal_preference_d <- stacked_pct_data(survey, thermal_preference)
thermal_acceptability_d <- stacked_pct_data(survey, acceptability_thermal)

# Plots
thermal_sensation_p <- plot_stacked_pct(survey, thermal_sensation, thermal_sensation_palette, "I feel ...") + labs(subtitle = "Thermal sensation")
thermal_preference_p <- plot_stacked_pct(survey, thermal_preference, thermal_preference_palette, "I prefer to be ...") + labs(subtitle = "Thermal preference")
thermal_acceptability_p <- plot_stacked_pct(survey, acceptability_thermal, acceptability_palette, "I find the thermal\nenvironment ...") + labs(subtitle = "Thermal acceptability")

thermal_perception <- (thermal_sensation_p / thermal_preference_p / thermal_acceptability_p) +
  plot_layout(axis_titles = "collect") +
  plot_annotation(tag_levels = "a", tag_suffix = ".") &
  theme(
    plot.subtitle = element_text(hjust = 0.1, margin = margin(b = 3, unit = "mm")),
    plot.tag = element_text(size = 7, face = "bold", ),
    axis.title = element_text(margin = margin(r = 2, unit = "mm")),
    legend.position = "right",
    legend.justification = "left",
    legend.box.margin = margin(l = 0)
  )

ggsave(
  here::here("manuscript", "figs", "thermal_perception.png"),
  plot = thermal_perception,
  dpi = 500,
  width = single_col_width,
  height = 170,
  units = "mm",
  bg = "transparent"
)


# Friedman tests for ordinal thermal comfort ------------------------------

friedman_sensation     <- get_friedman_thermal(survey, "thermal_sensation")
friedman_preference    <- get_friedman_thermal(survey, "thermal_preference")
friedman_acceptability <- get_friedman_thermal(survey, "acceptability_thermal")


# Cognitive Tests ---------------------------------------------------------

# 1. Cognitive Ability Test (CAT)

# 1.1 ANOVA Test

# Getting performer subsets

# Filter for subjects who completed all sessions
# (subjects.csv is already filtered to complete cases; see note above)
completed_subjects <- subjects %>%
  pull(subject_id)

# Calculate subject means only for those who completed all sessions
subject_means <- cat %>%
  filter(subject_id %in% completed_subjects) %>%
  group_by(subject_id) %>%
  summarise(avg_score = mean(test_cat_score), .groups = "drop")

# Calculate how many subjects for each group (top/bottom 25%)
n_per_group <- floor(nrow(subject_means) * 0.25)

# Rank subjects and identify top/bottom performers
subject_means <- subject_means %>%
  arrange(avg_score) %>%
  mutate(rank = row_number()) %>%
  mutate(
    is_low = rank <= n_per_group,
    is_high = rank > (nrow(subject_means) - n_per_group)
  )

# Filter the original data
high_performers <- cat %>%
  semi_join(
    subject_means %>% filter(is_high),
    by = "subject_id"
  )

low_performers <- cat %>%
  semi_join(
    subject_means %>% filter(is_low),
    by = "subject_id"
  )

# get subject IDs for later
high_performers_subjects <- high_performers %>% 
  dplyr::select(subject_id) %>% 
  dplyr::distinct(subject_id)

low_performers_subjects <- low_performers %>% 
  dplyr::select(subject_id) %>% 
  dplyr::distinct(subject_id)


# Session order distribution for performer subgroups ---------------------
# Cross-tabulates temperature condition × trial position for transparency
# reporting (Reviewer 1, line 330). Shows whether any condition was
# disproportionately assigned to early or late positions within each group.

.make_order_table <- function(data, group_label) {
  data %>%
    group_by(session_type, trial_number) %>%
    summarise(n = n_distinct(subject_id), .groups = "drop") %>%
    pivot_wider(names_from = trial_number, values_from = n,
                names_prefix = "pos_", values_fill = 0) %>%
    mutate(Group = group_label) %>%
    select(Group, session_type, everything())
}

order_table_high <- .make_order_table(high_performers, "High performers")
order_table_low  <- .make_order_table(low_performers,  "Low performers")
order_table_all  <- .make_order_table(
  cat %>% filter(subject_id %in% completed_subjects), "Full cohort"
)

order_table_combined <- bind_rows(order_table_all, order_table_high, order_table_low)


# IMPORTANT: Because we were not able to balance the sessio order for the high
# performers ahead of the sessions, we need to account for a potential imbalance
# In the following, we use the calculated learning affect to do so.

# getting idea of starting session (INFORMATIVE ONLY)
high_performers_start_session <- high_performers %>%
  left_join(subjects %>% select(subject_id, session_start), 
            by = "subject_id") %>% 
  dplyr::distinct(subject_id, session_start)


# Adjust for imbalanced session order using average learning effects calculated below
learning_effect <- data.frame(
  trial_number = c(1, 2, 3, 4),
  avg_improvement = c(0, 6.7, 9.7, 13.3)  # manually extract mean differences
)

high_performers_adjusted <- high_performers %>%
  left_join(learning_effect, by = "trial_number") %>%
  mutate(test_cat_score = test_cat_score - avg_improvement)

low_performers_adjusted <- low_performers %>%
  left_join(learning_effect, by = "trial_number") %>%
  mutate(test_cat_score = test_cat_score - avg_improvement) %>% 
  dplyr::filter(test_cat_score > 0) # adjust for neg values


rm(subject_means)


# Get ANOVA results
# Note: high/low performer ANOVAs are kept for PLOT generation only.
# Statistical reporting for subgroups uses the LMM below (cat_lmm_high/low),
# which models trial_number as a covariate rather than applying a mean correction.
anova_cat_all <- get_cat_anova(cat, session_type_palette)
anova_cat_high_performer <- get_cat_anova(high_performers_adjusted, session_type_palette)
anova_cat_low_performer  <- get_cat_anova(low_performers_adjusted,  session_type_palette)

# LMM for performer subgroups — trial_number as covariate (Reviewer 1) -------
# Avoids mean-correction bias: each participant's learning trajectory is
# estimated individually via the random slope (falls back to random intercept
# if the slope model is singular at n ≈ 19).
cat_lmm_high <- get_cat_lmm_subgroup(high_performers)
cat_lmm_low  <- get_cat_lmm_subgroup(low_performers)

# Reference dataframe
cat_all_d <- get_cat_summary(cat, test_cat_score, session_type)
anova_stats_all <- anova_cat_all$stats
sphericity_all <- anova_cat_all$sphericity
corrections_all <- anova_cat_all$corrections

cat_high_performer_d <- get_cat_summary(high_performers, test_cat_score, session_type)
anova_stats_high_performer <- data.frame(
  DFn = cat_lmm_high$DFn, DFd = cat_lmm_high$DFd,
  F   = cat_lmm_high$F,   p   = cat_lmm_high$p,  pes = cat_lmm_high$pes
)

cat_low_performer_d <- get_cat_summary(low_performers, test_cat_score, session_type)
anova_stats_low_performer <- data.frame(
  DFn = cat_lmm_low$DFn, DFd = cat_lmm_low$DFd,
  F   = cat_lmm_low$F,   p   = cat_lmm_low$p,  pes = cat_lmm_low$pes
)

# Post-hoc pairwise comparisons for high performers (exploratory)
posthoc_high_performer <- get_cat_posthoc(high_performers_adjusted, p_adjust_method = "bonferroni")

# Equivalence tests (TOST F-test) for CAT --------------------------------
# Bound: ηp² = 0.01 (Cohen's f = 0.10) — matches negligible-effect threshold
# (Cohen, 1988) and the minimum detectable effect in Porras-Salazar et al. (2024).
tost_cat_all            <- get_tost_ftest(anova_cat_all, eqbound_pes = 0.01)
tost_cat_high_performer <- list(p_value = cat_lmm_high$p_equiv, pes_bound = 0.01)
tost_cat_low_performer  <- list(p_value = cat_lmm_low$p_equiv,  pes_bound = 0.01)

# LMM sensitivity check for CAT (full sample) -----------------------------
# For a balanced complete-case design, LMM and RM-ANOVA are mathematically
# equivalent. Results should match anova_stats_all closely.
cat_lmm <- get_cat_lmm_sensitivity(
  cat %>% filter(subject_id %in% completed_subjects)
)

# A priori RM-ANOVA power using ICC from LMM --------------------------------
# Proper within-subjects power: λ = n·k·f² / (1 − ρ), where ρ = ICC.
# ICC estimated from LMM variance components (between-subject / total variance).
vc_cat    <- as.data.frame(VarCorr(cat_lmm$model))
icc_cat   <- vc_cat$vcov[1] / sum(vc_cat$vcov)   # subject intercept / total
n_power   <- 75; k_power <- 4
lambda_rm <- n_power * k_power * porras_f^2 / (1 - icc_cat)
df1_pow   <- k_power - 1
df2_pow   <- (n_power - 1) * (k_power - 1)
power_rm  <- 1 - pf(qf(0.95, df1_pow, df2_pow), df1_pow, df2_pow, ncp = lambda_rm)
# power_rm and icc_cat available for inline reporting in manuscript


# Plots
cat_all_subjects_p <- anova_cat_all$plot + labs(title = "All Subjects")
cat_high_performer_p <- anova_cat_high_performer$plot + labs(title = "High Performers (Top 25%)*")
cat_low_performer_p <- anova_cat_low_performer$plot + labs(title = "Low Performers (Bottom 25%)*")

cat_test_p <- (cat_all_subjects_p / cat_low_performer_p / cat_high_performer_p) +
  plot_layout(axis_titles = "collect_x") +
  plot_annotation(tag_levels = "a",
                  tag_suffix = ".",
                  caption = "*Scores have been adjusted to account for learning affect and to prevent impact of imbalanced session orders on subgroup scores."
                  ) &
  theme(
    plot.title = element_text(hjust = 0.005, margin = margin(b = 1, unit = "mm")),
    plot.subtitle = element_text(face = "italic", hjust = 0.01, margin = margin(b = 3, unit = "mm")),
    plot.tag = element_text(size = 7, face = "bold", ),
    axis.title = element_text(margin = margin(r = 2, unit = "mm")),
    panel.background = element_rect(fill = "transparent", colour = NA),
    plot.background = element_rect(fill = "transparent", colour = NA),
    plot.caption = element_text(hjust = 0.5, size = 6, color = "gray40", 
                                margin = margin(t = 5, unit = "mm"))
  )

ggsave(
  here::here("manuscript", "figs", "cat_test.png"),
  plot = cat_test_p,
  dpi = 500,
  width = double_col_width,
  height = 240,
  units = "mm"
)

# Vertical version for presentation
cat_all_subjects_vert_p <- anova_cat_all$plot_vert + labs(title = "All Subjects")

ggsave(
  here::here("manuscript", "figs", "cat_test_vert.png"),
  plot = cat_all_subjects_vert_p,
  dpi = 500,
  width = 120,
  height = 75,
  units = "mm"
)

# 1.2 Learning Effect

# Reference dataframe
cat_learning_d <- get_cat_summary(cat, test_cat_score, trial_number) %>% 
  mutate(
    change_from_baseline = mean_score - mean_score[trial_number == 1]
  )

learning_effect_labels <- cat_learning_d %>%
  filter(trial_number %in% c(2, 3, 4)) %>%
  mutate(
    y_position = 106,
    label = paste0("+", round(change_from_baseline, 1), "p")
  )

# Plot
cat_learning_p <- ggplot(cat, aes(x = trial_number, y = test_cat_score)) +
  geom_point(alpha = 0.15, colour = "#7692ff") +
  geom_line(aes(group = subject_id), alpha = 0.05, colour = "#7692ff") +
  geom_smooth(method = "loess", se = TRUE, color = "navy", fill = "#7692ff") +
  geom_text(
    data = learning_effect_labels,
    aes(x = trial_number, y = y_position, label = label),
    size = 2.25,
    fontface = "bold",
    color = "gray30"
  ) +
  annotate("text", x = 0.9, y = 106, label = "Mean learning effect,\nrelative to Trial 1", hjust = 0, size = 2) +
  labs(x = "Trial number", y = "CAT Test Score (0 - 100)") +
  coord_cartesian(xlim = c(0.95, 4.05), ylim = c(0, 105)) +
  theme_minimal(base_size = 7) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.ticks.x = element_line(color = "grey"),
    axis.title.x = element_text(margin = margin(t = 6)),
    axis.title.y = element_text(margin = margin(l = 6)),
    axis.ticks.length = unit(1, "mm"),
  )

ggsave(
  here::here("manuscript", "figs", "cat_learning.png"),
  plot = cat_learning_p,
  dpi = 500,
  width = single_col_width,
  height = single_col_width,
  units = "mm"
)


# 2. PEBL Battery

metric_cols <- c(
  "mean_pumps", "total_pumps","best_span_performance", "mean_span_performance", "memory_span",
  "total_correct_trials", "fms", "mean_reaction_time", "n_trials",
  "perseverance_error", "sd_reaction_time", "mean_response_time",
  "number_of_correct", "number_of_errors", "percentage_of_correct",
  "minimum_moves", "total_moves", "total_time", "trials",
  "letters_task_number_clicks","letters_task_total_time",
  "mixed_task_number_clicks","mixed_task_total_time",
  "numbers_task_number_clicks", "numbers_task_number_of_errors",
  "numbers_task_total_time"
)


# 2.1 Entire sample
pebl_tbl <- get_pebl_summary(
  df          = pebl,
  metric_cols = names(metric_domain_map),
  metric_map  = metric_domain_map
)

# Domain-level z-score composites: RM-ANOVA + TOST (excludes BART/behavior)
pebl_domain_tost <- get_pebl_domain_anova_tost(pebl_tbl, eqbound_pes = 0.01)

# Build summary table for manuscript (Table: PEBL domain composites)
.domain_order  <- c("working_memory", "executive_function", "attention", "processing_speed")
.domain_labels <- c(
  working_memory     = "Working Memory",
  executive_function = "Executive Function",
  attention          = "Attention",
  processing_speed   = "Processing Speed"
)

pebl_domain_tost_tbl <- purrr::map_dfr(.domain_order, function(d) {
  r       <- pebl_domain_tost[[d]]
  dfn_fmt <- ifelse(r$DFn == round(r$DFn),
                    as.character(as.integer(r$DFn)),
                    sprintf("%.2f", r$DFn))
  tibble::tibble(
    Domain     = .domain_labels[d],
    n          = r$n,
    Statistic  = sprintf("F(%s, %d) = %.3f", dfn_fmt, r$DFd, r$F),
    p          = r$p,
    pes        = r$pes,
    p_equiv    = r$p_equiv,
    Equivalent = ifelse(r$equivalent, "Yes", "No")
  )
})

pebl_tbl_wide <- pebl_tbl$summary_wide %>%
  rename(test = test_type) %>%
  # remove irrelevant entries for now (CAUTION: Hard coded)
  filter(!performance_change %in% c(
    "23C: +0.0%; 25C: +0.0%; 27C: +0.0%",
    "23C: nan %; 25C: nan %; 27C: nan %",
    "23C: +0.8%; 25C: +0.0%; 27C: +0.0%",
    "23C: +0.0%; 25C: +0.0%; 27C: -0.5%"
  ))
# plot_data    <- pebl_tbl$plot_data

# build flextable
ft_pebl <- make_pebl_flextable(
  df          = pebl_tbl_wide,
  save_image  = TRUE,
  saving_path = here::here("manuscript", "figs", "pebl_test_tbl.png")
)


# 2.2 High performers
pebl_high_performers <- pebl %>% 
  semi_join(high_performers_subjects, by = "subject_id")

pebl_tbl_high_performers <- get_pebl_summary(
  df          = pebl_high_performers,
  metric_cols = names(metric_domain_map),
  metric_map  = metric_domain_map
)

pebl_tbl_high_performers_wide <- pebl_tbl_high_performers$summary_wide %>%
  rename(test = test_type) %>%
  # remove irrelevant entries for now (CAUTION: Hard coded)
  filter(!performance_change %in% c(
    "23C: +0.0%; 25C: +0.0%; 27C: +0.0%",
    "23C: nan %; 25C: nan %; 27C: nan %",
    "23C: +0.8%; 25C: +0.0%; 27C: +0.0%",
    "23C: +0.0%; 25C: +0.0%; 27C: -0.5%"
  ))

ft_pebl_high_performers <- make_pebl_flextable(
  df          = pebl_tbl_high_performers_wide,
  save_image  = TRUE,
  saving_path = here::here("manuscript", "figs", "pebl_test_tbl_high_perf.png")
)


# Generate domain plots and summary data
pebl_domain_results <- get_pebl_domain_summary(
  pebl_summary_result = pebl_tbl,
  session_order = c("21C", "23C", "25C", "27C"),
  baseline_session = "21C"
)

# Extract components
domain_plots <- pebl_domain_results$plots
pebl_domain_d <- pebl_domain_results$summary_data

# Combine plots in 3x2 grid
pebl_domain_combined_p <- guide_area() /
  (domain_plots[["Overall"]] | 
     domain_plots[["Working Memory"]]) / 
  (domain_plots[["Executive Function"]] |
     domain_plots[["Attention"]]) /
  (domain_plots[["Decision Making"]] |
     domain_plots[["Processing Speed"]]) +
  plot_layout(
    axis_titles = "collect",
    guides = "collect",
    heights = c(0.3, 1, 1, 1)) +
  plot_annotation(
    tag_levels = "a", 
    tag_suffix = ".",
    caption = "Positive values indicate improved performance. Each point represents one participant's\naverage change across all metrics in that domain. Outliers beyond ±50% were excluded."
  ) &
  theme(
    plot.tag = element_text(size = 7, face = "bold"),
    plot.margin = margin(2.5, 5, 2.5, 5, "mm"),
    axis.title = element_text(margin = margin(r = 2, unit = "mm")),
    plot.caption = element_text(hjust = 0.0, size = 6, color = "gray40", 
                                margin = margin(t = 5, unit = "mm")),
    legend.direction = "horizontal",
  )

# Save combined figure
ggsave(
  here::here("manuscript", "figs", "pebl_domain_combined.png"),
  plot = pebl_domain_combined_p,
  dpi = 500,
  width = double_col_width,  # Use double column for 2 columns
  height = 180,  # Adjust as needed for 3 rows
  units = "mm"
)


# Same here: Generate vertical version for presentation
domain_plots_vert <- pebl_domain_results$plots_vert

domain_plots["Overall"]

ggsave(
  here::here("manuscript", "figs", "pebl_overall_vert.png"),
  plot = domain_plots_vert["Overall"],
  dpi = 500,
  width = 120,
  height = 75,
  units = "mm"
)

# Physiological Measurements ----------------------------------------------

# 1. Heart rate

# get hrv summary
hrv_summary <- list(
  subject_means = hrv %>%
    filter(!is.na(session_id)) %>%
    group_by(subject_id, session_id, session_type) %>%
    summarise(
      mean_hr = round(mean(hr_bpm, na.rm = TRUE), 0),
      sd_hr   = round(sd(hr_bpm, na.rm = TRUE), 0),
      mean_rmssd = round(mean(rmssd, na.rm = TRUE), 0),
      n       = sum(!is.na(hr_bpm)),
      .groups = "drop"
    ),
  
  sample_means = hrv %>%
    filter(!is.na(session_id)) %>%
    group_by(session_type) %>%
    summarise(
      mean_hr = round(mean(hr_bpm, na.rm = TRUE), 0),
      sd_hr   = round(sd(hr_bpm, na.rm = TRUE), 0),
      mean_rmssd = round(mean(rmssd, na.rm = TRUE), 0),
      .groups = "drop"
    )
)

# Use sample mean for reference dataframe
hr_d <- hrv_summary$sample_means

# And subject mean for plot
hr_p <- ggplot(hrv_summary$subject_means, aes(x = session_type, y = mean_hr, fill = session_type)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.4) +
  geom_jitter(aes(color = session_type), width = 0.2, size = 1, alpha = 0.8) +
  scale_fill_manual(values = session_type_palette) +
  scale_color_manual(values = session_type_palette) +
  labs(
    x = "Session Type",
    y = "Heart rate [bpm]",
    fill = "Session Type",
    color = "Session Type"
  ) +
  theme_minimal(base_size = 7) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.title.x = element_text(margin = margin(t = 6)),
    axis.title.y = element_text(margin = margin(r = 6)),
    legend.position = "none"
  ) +
  coord_cartesian(ylim = c(55, 102))


ggsave(
  here::here("manuscript", "figs", "hr_single.png"),
  plot = hr_p,
  dpi = 500,
  width = 100,
  height = 75,
  units = "mm"
)

# =============================================================================
# 1.1 RMSSD Analysis
# =============================================================================
# Root Mean Square of Successive Differences (RMSSD) is a time-domain measure
# of heart rate variability that reflects parasympathetic (vagal) activity.
# Higher RMSSD = stronger vagal tone, lower stress
# Lower RMSSD = reduced vagal activity, potentially higher stress
# Based on input from M. Renard and J. Lee (NUS)

# -----------------------------------------------------------------------------
# 1.1.1 Data Preparation
# -----------------------------------------------------------------------------

# Remove extreme outliers (top 1%) to avoid skewing results
rmssd_p99 <- quantile(hrv$rmssd, 0.99, na.rm = TRUE)

# Prepare subject-level aggregated data for statistical modeling
# We aggregate to subject x session level to account for repeated measures
rmssd_model_data <- hrv %>%
  filter(!is.na(session_id), rmssd <= rmssd_p99) %>%
  group_by(subject_id, session_type) %>%
  summarise(
    mean_rmssd = mean(rmssd, na.rm = TRUE),           # Average RMSSD per session
    mean_hr = mean(hr_bpm, na.rm = TRUE),             # Average HR per session
    mean_rr = mean(60000 / hr_bpm, na.rm = TRUE),     # Average RR interval (ms)
    cv_rmssd = mean((rmssd / (60000 / hr_bpm)) * 100, na.rm = TRUE), # Coefficient of variation
    .groups = "drop"
  )

# -----------------------------------------------------------------------------
# 1.1.2 Sensitivity Analysis for HR-RMSSD Relationship
# -----------------------------------------------------------------------------
# Following de Geus et al. (2019) recommendations, we examine whether the
# temperature effect on RMSSD is independent of heart rate changes or whether
# it's simply a mechanical consequence of HR increases

# Calculate correlation between HR and RMSSD by condition
# This documents the HR-RMSSD relationship in our data
cor_summary <- rmssd_model_data %>%
  group_by(session_type) %>%
  summarise(
    cor_hr_rmssd = cor(mean_hr, mean_rmssd),
    n = n()
  )

# cat("\n=== HR-RMSSD CORRELATIONS BY CONDITION ===\n")
# print(cor_summary)

# Model 1: Main analysis (unadjusted for HR)
# Tests whether temperature affects RMSSD
model_main <- lmer(
  mean_rmssd ~ session_type + (1|subject_id), 
  data = rmssd_model_data
)

# Model 2: HR-adjusted analysis (recommended by de Geus et al., 2019)
# Tests whether temperature effect remains after accounting for HR
# If effect disappears → RMSSD changes are HR-mediated
# If effect remains/strengthens → genuine autonomic effect beyond HR
model_hr_adj <- lmer(
  mean_rmssd ~ session_type + mean_hr + (1|subject_id), 
  data = rmssd_model_data
)

# Model 3: Coefficient of Variation approach (alternative adjustment)
# CV-RMSSD normalizes RMSSD by mean RR interval
# This is the adjustment method preferred by de Geus et al. (2019)
model_cv <- lmer(
  cv_rmssd ~ session_type + (1|subject_id), 
  data = rmssd_model_data
)

# -----------------------------------------------------------------------------
# 1.1.3 Extract Statistical Results for Manuscript Reporting
# -----------------------------------------------------------------------------

# Helper function to extract ANOVA statistics with proper p-values
extract_anova_stats <- function(model, effect_name = "session_type") {
  # Use lmerTest's anova which computes p-values with Satterthwaite approximation
  anova_result <- anova(model)
  effect_row <- which(rownames(anova_result) == effect_name)
  
  list(
    DFn = round(anova_result[effect_row, "NumDF"], 0),
    DFd = round(anova_result[effect_row, "DenDF"], 1),
    F = round(anova_result[effect_row, "F value"], 2),
    p = anova_result[effect_row, "Pr(>F)"],
    p_formatted = ifelse(
      anova_result[effect_row, "Pr(>F)"] < 0.001,
      "< .001",
      paste("=", format(round(anova_result[effect_row, "Pr(>F)"], 3), nsmall = 3))
    )
  )
}

# Extract statistics from each model
rmssd_stats_unadj <- extract_anova_stats(model_main, "session_type")
rmssd_stats_hr_adj <- extract_anova_stats(model_hr_adj, "session_type")
rmssd_stats_cv <- extract_anova_stats(model_cv, "session_type")
hr_effect_stats <- extract_anova_stats(model_hr_adj, "mean_hr")

# Compare all three models (for console output during analysis)
# cat("\n=== UNADJUSTED MODEL ===\n")
# cat("Temperature effect on RMSSD without HR adjustment:\n")
# print(anova(model_main))
# 
# cat("\n=== HR-ADJUSTED MODEL ===\n")
# cat("Temperature effect on RMSSD after controlling for HR:\n")
# print(anova(model_hr_adj))
# 
# cat("\n=== CV-RMSSD MODEL ===\n")
# cat("Temperature effect using coefficient of variation approach:\n")
# print(anova(model_cv))

# -----------------------------------------------------------------------------
# 1.1.4 Prepare Summary Statistics for Manuscript
# -----------------------------------------------------------------------------

# Calculate subject-level median RMSSD for visualization
rmssd_subject <- hrv %>%
  group_by(subject_id, session_type) %>%
  summarise(
    rmssd_subj = median(rmssd, na.rm = TRUE),
    .groups = "drop"
  )

# Calculate session-level descriptive statistics for manuscript reporting
# This dataframe is used in the QMD file for inline reporting
rmssd_d <- hrv %>% 
  filter(rmssd <= rmssd_p99) %>%
  group_by(session_type) %>%
  summarise(
    mean_session_rmssd = round(mean(rmssd, na.rm = TRUE), 0),
    sd_session_rmssd   = round(sd(rmssd, na.rm = TRUE), 0),
    cor_hr_rmssd = round(cor(rmssd, hr_bpm, use = "complete.obs"), 2),
    .groups = "drop"
  )

# -----------------------------------------------------------------------------
# 1.1.5 Create Visualization
# -----------------------------------------------------------------------------

rmssd_p <- hrv %>% 
  filter(rmssd <= rmssd_p99) %>%
  ggplot(aes(x = session_type, y = rmssd, color = session_type, fill = session_type)) +
  # Density distribution (half violin)
  stat_halfeye(adjust = 0.8, width = 0.5, .width = 0, alpha = 0.6) +
  # Boxplot showing quartiles
  geom_boxplot(width = 0.4, alpha = 0.3, outlier.shape = NA) +
  # Individual subject medians
  geom_jitter(
    data = rmssd_subject,
    aes(x = session_type, y = rmssd_subj),
    inherit.aes = FALSE,
    width = 0.1,
    alpha = 0.4,
    size = 1,
    color = "black"
  ) +
  scale_fill_manual(values = session_type_palette) +
  scale_color_manual(values = session_type_palette) +
  labs(
    x = "Session type",
    y = "RMSSD (ms)",
    color = "Session Type",
    fill = "Session Type"
  ) +
  theme_minimal(base_size = 7) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.title.x = element_text(margin = margin(t = 6)),
    axis.title.y = element_text(margin = margin(r = 6)),
    legend.position = "none"
  )

# Save figure for manuscript
ggsave(
  here::here("manuscript", "figs", "rmssd.png"),
  plot = rmssd_p,
  dpi = 500,
  width = single_col_width,
  height = 89,
  units = "mm"
)


# 2. Skin temperature

# get tsk by minute
tsk_by_min <- tsk %>%
  filter(!is.na(session_id)) %>%
  mutate(
    minute = running_time / 60,
    minute_bin = floor(minute)
  ) %>% # integer minute bin: 30 = 30:00-30:59
  filter(minute_bin >= 30, minute_bin <= 150) %>% # restrict window
  group_by(session_type, minute_bin) %>%
  summarise(
    mean_tsk = mean(tsk_c, na.rm = TRUE),
    sd_tsk = sd(tsk_c, na.rm = TRUE),
    n = sum(!is.na(tsk_c)),
    .groups = "drop"
  ) %>%
  mutate(
    ymin = mean_tsk - sd_tsk,
    ymax = mean_tsk + sd_tsk
  )


tsk_d <- tsk_by_min %>%
  group_by(session_type) %>%
  summarise(
    mean_session_tsk = round(mean(mean_tsk, na.rm = TRUE), 1),
    .groups = "drop"
  )


tsk_p <- ggplot(tsk_by_min, aes(x = minute_bin, y = mean_tsk, color = session_type, fill = session_type)) +
  geom_ribbon(aes(ymin = ymin, ymax = ymax), alpha = 0.20, colour = NA) + # ribbon = mean ± SD
  geom_line(size = 0.75, lineend = "round") +
  scale_color_manual(values = session_type_palette) +
  scale_fill_manual(values = session_type_palette) +
  scale_y_continuous(
    breaks = c(26, 28, 30, 32, 34, 36)
  ) +
  scale_x_continuous(
    breaks = c(30, 60, 90, 120, 150),
    limits = c(30, 150),
    expand = c(0, 2)
  ) +
  labs(
    x = "Session time (min)",
    y = "Mean skin temperature (°C)",
    color = "Session Type",
    fill = "Session Type"
  ) +
  theme_minimal(base_size = 7) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.title.x = element_text(margin = margin(t = 6)),
    axis.title.y = element_text(margin = margin(r = 6)),
    legend.position = "top"
  ) +
  coord_cartesian(ylim = c(26, 36))

tsk_p

# 3. Combine into one plot + save

physiology <- (tsk_p | free(hr_p)) +
  plot_annotation(tag_levels = "a", tag_suffix = ".") &
  theme(plot.tag = element_text(size = 7, face = "bold"))

ggsave(
  here::here("manuscript", "figs", "tsk_hr.png"),
  plot = physiology,
  dpi = 500,
  width = double_col_width,
  height = 100,
  units = "mm"
)
