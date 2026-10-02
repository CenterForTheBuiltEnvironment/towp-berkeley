# Title: Setup
# Description: Load packages, define column types, color palettes, and
#              cognitive domain mappings for PEBL test battery

pacman::p_load(
  here, dplyr, ggplot2, readr, tidyr, purrr, stringr, lubridate, arrow,
  janitor, scales, glue, yaml, data.table, rstatix, patchwork, flextable, officer, ggdist,
  lmerTest, pwr
)


# Global settings ---------------------------------------------------------

single_col_width = 89 #mm
double_col_width = 183 #mm

# Column data types and levels -------------------------------------------------------

thermal_sensation_levels = c("cold","cool","slightly cool", "neutral","slightly warm", "warm", "hot")
thermal_preference_levels = c("cooler", "no change", "warmer")
air_speed_preference_levels = c("lower", "no change", "higher")
acceptability_levels = c("very unacceptable", "somewhat unacceptable", "neither acceptable nor unacceptable", "somewhat acceptable", "very acceptable")

sex_levels = c("female", "male", "non-binary / third gender")
degree_levels = c("none", "high school", "bachelors", "masters", "ph.d.")

session_type_levels = c("21C", "23C", "25C", "27C")

col_subjects <- readr::cols(
  subject_id = readr::col_character(),
  sex = readr::col_factor(levels=sex_levels),
  degree = readr::col_factor(levels=degree_levels, ordered = TRUE)
)

col_sessions <- readr::cols(
  session_id = readr::col_character()
)

col_hrv <- readr::cols(
  session_id     = readr::col_character(),
  subject_id = readr::col_character()
)

col_tsk <- readr::cols(
  subject_id = readr::col_character()
)

col_survey <- readr::cols(
  subject_id = readr::col_character(),
  survey_id = readr::col_factor(levels = c("start", "middle", "end")),
  thermal_sensation = readr::col_factor(levels = thermal_sensation_levels, labels ,ordered = TRUE),
  thermal_preference = readr::col_factor(levels = thermal_preference_levels, ordered = TRUE),
  air_speed_preference = readr::col_factor(levels = air_speed_preference_levels, ordered = TRUE),
  acceptability_thermal = readr::col_factor(levels = acceptability_levels, ordered = TRUE),
  acceptability_air_speed = readr::col_factor(levels = acceptability_levels, ordered = TRUE),
  acceptability_humidity = readr::col_factor(levels = acceptability_levels, ordered = TRUE),
  acceptability_air_quality = readr::col_factor(levels = acceptability_levels, ordered = TRUE)
)


# Color palettes ----------------------------------------------------------

thermal_sensation_palette <- c("#6b8dd6", "#82c6ed","#b8e4f7","#c9e0b0","#f7a9b7","#eb6b58","#b44a4a")

thermal_preference_palette <- c("#82c6ed","#c9e0b0","#eb6b58")

acceptability_palette <- c('#bc3e4d','#d99fa8','#f0f0f0','#c1e0b9','#38a257')

session_type_palette <- c("#FFBE0B", "#FB5607", "#FF006E", "#8338EC")



# Parameter -> metric / domain mapping ---------------------------------------------


# Parameter -> metric / domain mapping ---------------------------------------------
# Cognitive domain classification based on:
# - Lezak, M. D., Howieson, D. B., Bigler, E. D., & Tranel, D. (2012). 
#   Neuropsychological Assessment (5th ed.). Oxford University Press.
# - Strauss, E., Sherman, E. M. S., & Spreen, O. (2006). 
#   A Compendium of Neuropsychological Tests (3rd ed.). Oxford University Press.
# - Diamond, A. (2013). Executive functions. Annual Review of Psychology, 64, 135-168.
# - PEBL test battery documentation: http://pebl.sourceforge.net/

metric_domain_map <- list(
  # BART – Decision Making / Risk Processing
  mean_pumps = list(
    label  = "Mean Pumps",
    metric = "behavior",
    domain = "decision_making"  # Changed from risk_taking
  ),
  total_pumps = list(
    label  = "Total Pumps",
    metric = "behavior",
    domain = "decision_making"
  ),
  
  # Corsi – Visuospatial Working Memory
  best_span_performance = list(
    label  = "Best Span",
    metric = "accuracy",
    domain = "working_memory"
  ),
  mean_span_performance = list(
    label  = "Mean Span",
    metric = "accuracy",
    domain = "working_memory"
  ),
  memory_span = list(
    label  = "Memory Span",
    metric = "accuracy",
    domain = "working_memory"
  ),
  total_correct_trials = list(
    label  = "Total Correct Trials",
    metric = "accuracy",
    domain = "working_memory"
  ),
  
  # BCST – Executive Function (Set-Shifting/Cognitive Flexibility)
  fms = list(
    label  = "False Memory Score",
    metric = "accuracy",
    domain = "executive_function"  # Changed from executive_control
  ),
  mean_reaction_time = list(
    label  = "Mean Reaction Time",
    metric = "speed",
    domain = "executive_function"
  ),
  n_trials = list(
    label  = "Number of Trials",
    metric = "efficiency",  # Changed from speed
    domain = "executive_function"
  ),
  perseverance_error = list(
    label  = "Perseverative Errors",  # Fixed spelling
    metric = "accuracy",
    domain = "executive_function"
  ),
  sd_reaction_time = list(
    label  = "SD Reaction Time",
    metric = "consistency",  # Changed from speed
    domain = "executive_function"
  ),
  
  # Flanker – Attention & Inhibitory Control
  mean_response_time = list(
    label  = "Mean Response Time",
    metric = "speed",
    domain = "attention"  # More specific than processing_speed
  ),
  number_of_correct = list(
    label  = "Correct Responses",
    metric = "accuracy",
    domain = "attention"
  ),
  number_of_errors = list(
    label  = "Errors",
    metric = "accuracy",
    domain = "attention"
  ),
  percentage_of_correct = list(
    label  = "Percent Correct",
    metric = "accuracy",
    domain = "attention"
  ),
  
  # Tower of London – Executive Function (Planning)
  minimum_moves = list(
    label  = "Minimum Moves",
    metric = "accuracy",
    domain = "executive_function"  # Changed from planning_problem_solving
  ),
  total_moves = list(
    label  = "Total Moves",
    metric = "efficiency",  # Better than accuracy for this
    domain = "executive_function"
  ),
  total_time = list(
    label  = "Total Time",
    metric = "speed",
    domain = "executive_function"
  ),
  trials = list(
    label  = "Trials",
    metric = "completion",
    domain = "executive_function"
  ),
  
  # PTrails – Processing Speed & Executive Function
  letters_task_number_clicks = list(
    label  = "Letters: Clicks",
    metric = "accuracy",
    domain = "processing_speed"
  ),
  letters_task_total_time = list(
    label  = "Letters: Total Time",
    metric = "speed",
    domain = "processing_speed"
  ),
  mixed_task_number_clicks = list(
    label  = "Mixed: Clicks",
    metric = "accuracy",
    domain = "executive_function"  # Mixed task requires set-shifting
  ),
  mixed_task_total_time = list(
    label  = "Mixed: Total Time",
    metric = "speed",
    domain = "executive_function"  # Mixed task is executive function
  ),
  numbers_task_number_clicks = list(
    label  = "Numbers: Clicks",
    metric = "accuracy",
    domain = "processing_speed"
  ),
  numbers_task_number_of_errors = list(
    label  = "Numbers: Errors",
    metric = "accuracy",
    domain = "processing_speed"
  ),
  numbers_task_total_time = list(
    label  = "Numbers: Total Time",
    metric = "speed",
    domain = "processing_speed"
  )
)

