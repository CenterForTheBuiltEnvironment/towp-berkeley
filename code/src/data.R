# Title: Load public dataset
# Description: Reads the anonymized CSVs in data/ and reconstructs the
#              tibbles (subjects, cat, hrv, survey, pebl, env_sessions)
#              expected by func.R / stat.R / analysis.R.
# Author: Toby Kramer

source(here::here("code", "src", "setup.R"))


# Subjects ------------------------------------------------------------------
# Already filtered to participants who completed all four sessions and
# stripped of free-text/quasi-identifying fields (see data/README.md).

subjects <- read_csv(
  here::here("data", "subjects.csv"),
  col_types = cols(subject_id = col_character())
)


# Cognitive Ability Test (CAT) -----------------------------------------------
# Already carries the manual session-type corrections and trial_number
# applied in the original pipeline.

cat <- read_csv(
  here::here("data", "cat.csv"),
  col_types = cols(session_id = col_character(), subject_id = col_character())
)


# Heart rate variability ------------------------------------------------------
# Already cleaned (invalid-RR removal, 1-minute windowing) in the original
# pipeline; `time_from_start_min` replaces the original absolute timestamp.

hrv <- read_csv(
  here::here("data", "hrv.csv"),
  col_types = cols(session_id = col_character(), subject_id = col_character())
)


# PEBL cognitive battery ------------------------------------------------------
# Exported in long format (one row per metric); pivot back to the wide
# shape (one column per metric) that get_pebl_summary() expects.

pebl <- read_csv(
  here::here("data", "pebl_long.csv"),
  col_types = cols(session_id = col_character(), subject_id = col_character())
) %>%
  pivot_wider(names_from = metric, values_from = value)


# Thermal comfort survey -------------------------------------------------------
# Exported in long format (one row per question); pivot back to wide and
# reapply the ordered factor levels defined in setup.R.

#
# A small number of session/survey_id combinations have more than one
# submission recorded for the same question (likely repeated check-ins
# during a session mapped to the same survey_id label upstream; the
# disambiguating timestamp was dropped before export). None of the
# functions that consume `survey` (stacked_pct_data, plot_stacked_pct,
# get_friedman_thermal) ever compare two of these columns within the same
# row — each only ever looks at one outcome column at a time — so a
# per-measure replicate index is sufficient to widen safely without
# dropping any observations or needing the two submissions' columns to
# line up with each other row-for-row.

survey <- read_csv(
  here::here("data", "survey_long.csv"),
  col_types = cols(session_id = col_character(), subject_id = col_character())
) %>%
  mutate(.rep = row_number(), .by = c(session_id, subject_id, session_type, survey_id, measure)) %>%
  pivot_wider(names_from = measure, values_from = response) %>%
  select(-.rep) %>%
  mutate(
    thermal_sensation         = factor(thermal_sensation, levels = thermal_sensation_levels, ordered = TRUE),
    thermal_preference        = factor(thermal_preference, levels = thermal_preference_levels, ordered = TRUE),
    air_speed_preference      = factor(air_speed_preference, levels = air_speed_preference_levels, ordered = TRUE),
    acceptability_thermal     = factor(acceptability_thermal, levels = acceptability_levels, ordered = TRUE),
    acceptability_air_speed   = factor(acceptability_air_speed, levels = acceptability_levels, ordered = TRUE),
    acceptability_humidity    = factor(acceptability_humidity, levels = acceptability_levels, ordered = TRUE),
    acceptability_air_quality = factor(acceptability_air_quality, levels = acceptability_levels, ordered = TRUE)
  )


# Environmental conditions -----------------------------------------------------
# env_sessions.csv has no session_type column (it wasn't needed for the
# session-level export); recover it via a session_id -> session_type lookup
# built from the other tables, which together cover every session.
#
# NOTE: env_sessions.csv holds one row per session (already averaged from
# the original per-reading sensor log). That per-reading log is not part of
# the public data tier, so any variability computed from env_sessions
# reflects between-session variability, not the finer within-session,
# per-reading variability reported in the manuscript's environmental
# conditions table. See analysis.R and README.md for details.

session_type_lookup <- bind_rows(
  cat %>% select(session_id, session_type),
  hrv %>% select(session_id, session_type),
  survey %>% select(session_id, session_type),
  pebl %>% select(session_id, session_type)
) %>%
  distinct(session_id, .keep_all = TRUE)

env_sessions <- read_csv(
  here::here("data", "env_sessions.csv"),
  col_types = cols(session_id = col_character())
) %>%
  left_join(session_type_lookup, by = "session_id")


# Skin temperature -------------------------------------------------------------
# Already cleaned in the original pipeline; `time_from_start_min` replaces
# the original absolute timestamp (same convention as hrv.csv).

tsk <- read_csv(
  here::here("data", "tsk.csv"),
  col_types = cols(session_id = col_character(), subject_id = col_character())
) %>%
  mutate(running_time = time_from_start_min * 60)
