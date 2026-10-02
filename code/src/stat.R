# Title: Bind scripts for main data analysis
# Description: Bind information from seperate sources to put together main analysis
# Author: Toby Kramer
# Date: 2025-10-29


# ---- Setup ----

# pacman::p_load(here, dplyr, ggplot2, readr, tibble, purrr, yaml, rstatix)

source(here::here("code", "src", "data.R"))
source(here::here("code", "src", "setup.R"))


# Cognitive Tests ---------------------------------------------------------


get_cat_anova <- function(test_data, session_type_palette = NULL) {

  # optional simple check
  if (!is.data.frame(test_data)) stop("test_data must be a data.frame/tibble")

  # if no palette provided, create a simple default palette
  if (is.null(session_type_palette)) {
    session_types <- unique(as.character(test_data$session_type))
    session_type_palette <- setNames(rainbow(length(session_types)), session_types)
  }

  anova_results <- test_data %>%
    anova_test(
      dv = test_cat_score,
      wid = subject_id,
      within = session_type,
      effect.size = "pes"  # partial eta-squared
    )

  # Extract main ANOVA stats including effect size
  stats <- anova_results$ANOVA %>%
    filter(Effect == "session_type") %>%
    select(DFn, DFd, F, p, pes) %>%
    mutate(
      F = round(F, 3),
      p = signif(p, 3),
      pes = round(pes, 3)
    )

  # Extract sphericity test (Mauchly's test)
  sphericity_test <- anova_results$`Mauchly's Test for Sphericity`
  if (!is.null(sphericity_test) && nrow(sphericity_test) > 0) {
    sphericity <- list(
      W = round(sphericity_test$W[1], 3),
      p = signif(sphericity_test$p[1], 3),
      violated = sphericity_test$p[1] < 0.05
    )
  } else {
    sphericity <- list(W = NA, p = NA, violated = FALSE)
  }

  # Extract sphericity corrections (GG and HF) if sphericity is violated
  sphericity_corr <- anova_results$`Sphericity Corrections`
  if (!is.null(sphericity_corr) && nrow(sphericity_corr) > 0) {
    corrections <- list(
      GGe = round(sphericity_corr$GGe[1], 3),  # Greenhouse-Geisser epsilon
      p_GG = signif(sphericity_corr$`p[GG]`[1], 3),
      HFe = round(sphericity_corr$HFe[1], 3),  # Huynh-Feldt epsilon
      p_HF = signif(sphericity_corr$`p[HF]`[1], 3)
    )
  } else {
    corrections <- list(GGe = NA, p_GG = NA, HFe = NA, p_HF = NA)
  }

  # Use corrected p-value if sphericity is violated
  if (sphericity$violated && !is.na(corrections$p_GG)) {
    stats$p_corrected <- corrections$p_GG
    stats$correction_used <- "GG"
  } else {
    stats$p_corrected <- stats$p
    stats$correction_used <- "none"
  }
  
  # Create formatted string for plot (use corrected p if applicable)
  p_display <- ifelse(stats$correction_used == "GG",
                      paste0(stats$p_corrected, " (GG)"),
                      stats$p)
  stats_text <- paste0(
    "Repeated-measures ANOVA: F(",
    stats$DFn, ", ", stats$DFd, ") = ",
    stats$F, ", p = ", p_display, ", ηp² = ", stats$pes
  )
  
  # Calculate maximum CAT score for text positioning
  # max_cat_score <- max(test_data$test_cat_score, na.rm = TRUE)
  
  # n per session_type, but also keep target_temp for x-position
  n_counts <- test_data %>%
    group_by(session_type, target_temp) %>%
    summarise(n_subjects = n_distinct(subject_id),
              median_score = median(test_cat_score, na.rm = TRUE),
              .groups = "drop")
  
  cat_plot <- ggplot(
    data = test_data,
    aes(x = test_cat_score, y = target_temp)
  ) +
    # boxplots at nominal temperatures
    geom_boxplot(
      aes(fill = session_type, group = session_type),
      alpha = 0.4,
      width = 1,
      outliers = FALSE,
    ) +
    scale_fill_manual(values = session_type_palette) +
    
    # scatter points at *actual* mean session temperature
    geom_point(
      aes(y = mean_session_t_air_c, color = session_type),
      alpha = 0.8,
      size = 1.5,
      position = position_jitter(width = 0.5, height = 0)
    ) +
    scale_color_manual(values = session_type_palette) +
    coord_cartesian(xlim = c(105, 0)) +
    geom_text(
      data = n_counts,
      aes(
        x = median_score,
        y = target_temp + 0.8,
        label = paste0("n = ", n_subjects),
      ),
      inherit.aes = FALSE
    ) +
    # numeric y-scale underneath, but label with original session_type
    scale_y_continuous(
      breaks = sort(unique(test_data$target_temp)),
      labels = sort(unique(test_data$target_temp))
    ) +
    theme_minimal(base_size = 7) +
    theme(
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.grid.minor.x = element_blank(),
      axis.ticks.y = element_line(color = "grey"),
      legend.position = "none",
      axis.title.x = element_text(margin = margin(t = 6)),
      axis.title.y = element_text(margin = margin(r = 6)),
      axis.ticks.length = unit(1, "mm")
    ) +
    labs(
      y = "Session Type // Mean Air Temperature (°C)",
      x = "CAT Test Score (0 - 100)",
      subtitle = stats_text
    )
  
  cat_plot_vert <- ggplot(
    data = test_data,
    aes(x = test_cat_score, y = target_temp)
  ) +
    geom_boxplot(
      aes(fill = session_type, group = session_type),
      alpha = 0.4,
      width = 1,
      outliers = FALSE
    ) +
    scale_fill_manual(values = session_type_palette) +
    
    geom_point(
      aes(y = mean_session_t_air_c, color = session_type),
      alpha = 0.6,
      size = 1.5,
      position = position_jitter(width = 0.5, height = 0)
    ) +
    scale_color_manual(values = session_type_palette) +
    
    # move xlim -> ylim because of flip
    coord_flip(xlim = c(100, 0)) +
    
    # remove this for now
    # geom_text(
    #   data = n_counts,
    #   aes(
    #     x = median_score,
    #     y = target_temp + 0.8,
    #     label = paste0("n = ", n_subjects)
    #   ),
    #   inherit.aes = FALSE
    # ) +
    # 
    scale_y_continuous(
      breaks = sort(unique(test_data$target_temp)),
      labels = sort(unique(test_data$target_temp))
    ) +
    
    theme_minimal(base_size = 7) +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      axis.ticks.y = element_blank(),
      legend.position = "none",
      axis.title.x = element_text(margin = margin(t = 6)),
      axis.title.y = element_text(margin = margin(r = 6)),
    ) +
    labs(
      y = "Session Type // Mean Air Temperature (°C)",
      x = "CAT Test Score (0 - 100)",
      subtitle = stats_text
    )
  
  return(list(
    plot  = cat_plot,
    plot_vert = cat_plot_vert,
    stats = stats,
    sphericity = sphericity,
    corrections = corrections
  ))
}


# Post-hoc pairwise comparisons for CAT -----------------------------------

get_cat_posthoc <- function(test_data, p_adjust_method = "bonferroni") {

  # Pairwise paired t-tests with correction for multiple comparisons

  posthoc <- test_data %>%
    pairwise_t_test(
      test_cat_score ~ session_type,
      paired = TRUE,
      p.adjust.method = p_adjust_method
    ) %>%
    mutate(
      # Add effect size (Cohen's d for paired samples)
      cohens_d = round(statistic / sqrt(n1), 3),
      p.adj = signif(p.adj, 3),
      p.adj.signif = case_when(
        p.adj < 0.001 ~ "***",
        p.adj < 0.01 ~ "**",
        p.adj < 0.05 ~ "*",
        TRUE ~ "ns"
      )
    ) %>%
    select(group1, group2, n1, statistic, df, p, p.adj, p.adj.signif, cohens_d)

  return(posthoc)
}


# PEBL Battery ------------------------------------------------------------

get_pebl_summary <- function(df,
                             metric_cols = NULL,
                             metric_map,
                             session_order = c("21C", "23C", "25C", "27C"),
                             baseline_session = session_order[1]) {
  ## 0) Input checks / mapping --------------------------------------------
  
  if (is.null(names(metric_map))) {
    stop("metric_map must be a *named* list: names = parameters, values = list(label=, metric=, domain=).")
  }
  
  # if metric_cols not given, use all parameters from metric_map
  if (is.null(metric_cols)) {
    metric_cols <- names(metric_map)
  }
  
  # keep only parameters that exist in df
  metric_cols <- intersect(metric_cols, names(df))
  if (length(metric_cols) == 0L) {
    stop("None of the provided metric_cols / parameters are present in df.")
  }
  
  # build a tibble with parameter -> metric + label + domain from the list-based map
  parameter_info <- tibble(
    parameter        = metric_cols,
    parameter_tested = vapply(metric_cols, function(p) metric_map[[p]]$label,  character(1)),
    metric           = vapply(metric_cols, function(p) metric_map[[p]]$metric, character(1)),
    domain           = vapply(metric_cols, function(p) metric_map[[p]]$domain, character(1))
  )
  
  if (!all(parameter_info$metric %in% c("speed", "accuracy", "efficiency", "consistency", "behavior", "completion"))) {
    bad <- parameter_info$parameter[!parameter_info$metric %in% c("speed", "accuracy", "efficiency", "consistency", "behavior", "completion")]
    stop("metric_map$metric must be in list of allowed metrics. Problem for: ",
         paste(bad, collapse = ", "))
  }
  
  ## 1) Long with all runs -------------------------------------------------
  
  long_runs <- df %>%
    pivot_longer(
      cols = all_of(metric_cols),
      names_to  = "parameter",
      values_to = "value"
    ) %>%
    drop_na(value) %>%
    left_join(parameter_info, by = "parameter") %>%
    mutate(
      metric = factor(metric, levels = c("speed", "accuracy", "efficiency", "consistency", "behavior", "completion")),
      domain = factor(domain)
    )
  
  ## 2) Average across run_id per subject × session × test × parameter ----
  
  long_mean <- long_runs %>%
    group_by(test_type, subject_id, session_type, session_id,
             parameter, parameter_tested, metric, domain) %>%
    summarise(
      value = mean(value, na.rm = TRUE),
      .groups = "drop"
    )
  
  ## 3) Summary (medians, IQR, N) -----------------------------------------
  
  summary_long <- long_mean %>%
    group_by(session_type, test_type, parameter, parameter_tested, metric, domain) %>%
    summarise(
      n   = n_distinct(subject_id[!is.na(value)]),
      med = median(value, na.rm = TRUE),
      q1  = quantile(value, 0.25, na.rm = TRUE),
      q3  = quantile(value, 0.75, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      summary = sprintf("%.2f (%.2f, %.2f, N = %d)", med, q1, q3, n)
    ) %>%
    filter(n != 0)
  
  ## 4) Performance change -------------------------------------------------
  
  perf_change <- summary_long %>%
    filter(session_type %in% session_order) %>%
    group_by(test_type, parameter, parameter_tested, metric, domain) %>%
    mutate(
      baseline_med = med[session_type == baseline_session][1],
      change = ifelse(
        is.na(baseline_med) | baseline_med == 0,
        NA_real_,
        100 * (med - baseline_med) / baseline_med
      )
    ) %>%
    ungroup() %>%
    filter(session_type != baseline_session) %>%
    mutate(
      label_change = ifelse(
        is.na(change),
        paste0(session_type, ": nan %"),
        sprintf("%s: %+.1f%%", session_type, change)
      )
    ) %>%
    group_by(test_type, parameter, parameter_tested, metric, domain) %>%
    summarise(
      performance_change = paste(label_change, collapse = "; "),
      .groups = "drop"
    )
  
  ## 5) Repeated-measures ANOVA on per-session means ----------------------
  
  # subjects with complete data across all sessions
  complete_subjects <- long_mean %>%
    filter(session_type %in% session_order) %>%
    group_by(test_type, parameter, parameter_tested, metric, domain, subject_id) %>%
    summarise(
      n_sessions_nonmiss = n_distinct(session_type[!is.na(value)]),
      .groups = "drop"
    ) %>%
    filter(n_sessions_nonmiss == length(session_order)) %>%
    select(test_type, parameter, parameter_tested, metric, domain, subject_id)
  
  # one mean per subject × session for complete subjects
  long_rm <- long_mean %>%
    inner_join(
      complete_subjects,
      by = c("test_type", "parameter", "parameter_tested", "metric", "domain", "subject_id")
    ) %>%
    filter(session_type %in% session_order) %>%
    mutate(
      session_type = factor(session_type, levels = session_order)
    )
  
  pvals <- long_rm %>%
    group_by(test_type, parameter, parameter_tested, metric, domain) %>%
    group_modify(~ {
      d <- .x
      n_subj     <- n_distinct(d$subject_id)
      n_sessions <- n_distinct(d$session_type)

      if (n_subj < 2 || n_sessions < 2) {
        return(tibble(p_value = NA_real_, pes = NA_real_, n_subjects = n_subj,
                      sphericity_p = NA_real_, p_GG = NA_real_))
      }

      res <- tryCatch(
        anova_test(
          data   = d,
          dv     = value,
          wid    = subject_id,
          within = session_type,
          effect.size = "pes"
        ),
        error = function(e) NULL
      )

      if (is.null(res)) {
        return(tibble(p_value = NA_real_, pes = NA_real_, n_subjects = n_subj,
                      sphericity_p = NA_real_, p_GG = NA_real_))
      }

      tab <- get_anova_table(res)

      # Extract sphericity test
      sph <- res$`Mauchly's Test for Sphericity`
      sphericity_p <- if (!is.null(sph) && nrow(sph) > 0) sph$p[1] else NA_real_

      # Extract GG-corrected p-value
      corr <- res$`Sphericity Corrections`
      p_GG <- if (!is.null(corr) && nrow(corr) > 0) corr$`p[GG]`[1] else NA_real_

      tibble(
        p_value      = tab$p[tab$Effect == "session_type"],
        pes          = tab$pes[tab$Effect == "session_type"],
        n_subjects   = n_subj,
        sphericity_p = sphericity_p,
        p_GG         = p_GG
      )
    }) %>%
    ungroup() %>%
    mutate(
      # Use GG-corrected p if sphericity is violated
      p_final = ifelse(!is.na(sphericity_p) & sphericity_p < 0.05 & !is.na(p_GG),
                       p_GG, p_value),
      p_value_label = ifelse(
        is.na(p_final),
        NA_character_,
        sprintf("%.2f (%d)", signif(p_final, 3), n_subjects)
      ),
      pes_label = ifelse(is.na(pes), NA_character_, sprintf("%.3f", pes))
    ) %>%
    select(test_type, parameter, parameter_tested, metric, domain,
           p_value_label, pes_label, p_value, pes, sphericity_p)
  
  ## 6) Wide summary table -------------------------------------------------
  
  summary_wide <- summary_long %>%
    filter(session_type %in% session_order) %>%
    mutate(
      session_type = factor(session_type, levels = session_order),
      session_col  = paste0("summary_", session_type)
    ) %>%
    select(test_type, parameter, parameter_tested, metric, domain, session_col, summary) %>%
    pivot_wider(
      names_from  = session_col,
      values_from = summary
    ) %>%
    left_join(
      perf_change,
      by = c("test_type", "parameter", "parameter_tested", "metric", "domain")
    ) %>%
    left_join(
      pvals %>% select(test_type, parameter, parameter_tested, metric, domain,
                       p_value_label, pes_label),
      by = c("test_type", "parameter", "parameter_tested", "metric", "domain")
    ) %>%
    rename(p_value = p_value_label, effect_size = pes_label) %>%
    arrange(test_type, domain, metric, parameter_tested) %>%
    # use the human-readable label in the final 'parameter' column
    mutate(parameter = parameter_tested) %>%
    select(test_type, domain, metric, parameter, everything(), -parameter_tested)
  
  # Return:
  # - summary_wide: final table (includes 'domain')
  # - plot_data: subject-level, per-session means for ggplot (also with 'domain')
  list(
    summary_wide = summary_wide,
    plot_data    = long_mean
  )
}


# =============================================================================
# PEBL Domain-Level Performance Change Analysis (By Domain)
# =============================================================================
# This function creates separate plots for each cognitive domain, showing
# performance changes across all temperature conditions.

get_pebl_domain_summary <- function(pebl_summary_result,
                                    session_order = c("21C", "23C", "25C", "27C"),
                                    baseline_session = session_order[1]) {
  
  # Extract the plot_data from get_pebl_summary() result
  long_mean <- pebl_summary_result$plot_data
  
  # Filter for sessions we care about
  sessions_to_compare <- session_order[session_order != baseline_session]
  
  # -----------------------------------------------------------------------------
  # Step 1: Calculate baseline values for each subject × test × parameter
  # -----------------------------------------------------------------------------
  
  baseline_values <- long_mean %>%
    filter(session_type == baseline_session) %>%
    select(test_type, subject_id, parameter, metric, domain, baseline_value = value)
  
  # -----------------------------------------------------------------------------
  # Step 2: Calculate performance change for each subject × session × parameter
  # -----------------------------------------------------------------------------
  
  performance_changes <- long_mean %>%
    filter(session_type %in% sessions_to_compare) %>%
    left_join(baseline_values, 
              by = c("test_type", "subject_id", "parameter", "metric", "domain")) %>%
    filter(!is.na(baseline_value), baseline_value != 0) %>%
    mutate(
      # Calculate percentage change
      pct_change_raw = 100 * (value - baseline_value) / baseline_value,
      
      # Normalize so positive = better performance
      # For speed metrics: decrease is better, so flip the sign
      # For accuracy/efficiency/behavior/completion: increase is better, keep sign
      pct_change_normalized = case_when(
        metric == "speed" ~ -pct_change_raw,  # Flip: decrease in time = improvement
        metric == "consistency" ~ -pct_change_raw,  # Flip: less variability = improvement
        TRUE ~ pct_change_raw  # Keep: increase = improvement
      )
    )
  
  # -----------------------------------------------------------------------------
  # Step 3: Average across all metrics within each domain for each subject
  # -----------------------------------------------------------------------------
  
  domain_changes <- performance_changes %>%
    group_by(subject_id, session_type, domain) %>%
    summarise(
      mean_change = mean(pct_change_normalized, na.rm = TRUE),
      n_metrics = n(),  # How many metrics contributed to this average
      .groups = "drop"
    ) %>%
    # Remove outliers (anything beyond ±100%)
    filter(abs(mean_change) <= 50) %>%
    mutate(
      session_type = factor(session_type, levels = sessions_to_compare),
      domain = factor(domain, 
                      levels = c("working_memory", "executive_function", 
                                 "attention", "processing_speed", "decision_making"),
                      labels = c("Working Memory", "Executive Function", 
                                 "Attention", "Processing Speed", "Decision Making"))
    )
  
  # -----------------------------------------------------------------------------
  # Step 3b: Create "Overall" domain by averaging across all domains per subject
  # -----------------------------------------------------------------------------
  
  overall_changes <- domain_changes %>%
    group_by(subject_id, session_type) %>%
    summarise(
      mean_change = mean(mean_change, na.rm = TRUE),
      n_metrics = sum(n_metrics),  # Total metrics across all domains
      .groups = "drop"
    ) %>%
    mutate(domain = "Overall")
  
  # Combine domain-specific and overall changes
  domain_changes <- bind_rows(domain_changes, overall_changes) %>%
    mutate(
      domain = factor(domain, 
                      levels = c("Overall", "Working Memory", "Executive Function", 
                                 "Attention", "Processing Speed", "Decision Making"))
    )
  
  pebl_domain_d <- domain_changes %>%
    group_by(session_type, domain) %>%
    summarise(
      n = n(),
      mean_change = round(mean(mean_change, na.rm = TRUE), 1),
      sd_change = round(sd(mean_change, na.rm = TRUE), 1),
      median_change = round(median(mean_change, na.rm = TRUE), 1),
      q1_change = round(quantile(mean_change, 0.25, na.rm = TRUE), 1),
      q3_change = round(quantile(mean_change, 0.75, na.rm = TRUE), 1),
      min_change = round(min(mean_change, na.rm = TRUE), 1),
      max_change = round(max(mean_change, na.rm = TRUE), 1),
      .groups = "drop"
    ) %>%
    arrange(domain, session_type)
  
  # Determine global symmetric x-axis limits for all plots
  max_abs_change <- max(abs(domain_changes$mean_change), na.rm = TRUE)
  x_limit <- ceiling(max_abs_change / 10) * 10  # Round up to nearest 10
  y_limit <- 20 # similar use for flipped axes
  
  # Define color palette for conditions
  condition_colors <- c("23C" = "#FB5607", "25C" = "#FF006E", "27C" = "#8338EC")
  
  # -----------------------------------------------------------------------------
  # Step 4: Create horizontal + vertical boxplots for each domain
  # -----------------------------------------------------------------------------
  
  plots_list <- list()
  
  # Get unique domains
  domains <- levels(domain_changes$domain)
  
  for (dom in domains) {
    
    plot_data <- domain_changes %>%
      filter(domain == dom)
    
    p <- ggplot(plot_data, aes(x = mean_change, y = session_type, fill = session_type)) +
      geom_boxplot(alpha = 0.4, outlier.shape = NA) +
      geom_point(
        aes(color = session_type),
        alpha = 0.2,
        size = 1.5,
        position = position_jitter(width = 0, height = 0.15)
      ) +
      # Reference line at zero (no change)
      geom_vline(xintercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.5) +
      scale_fill_manual(values = condition_colors, name = "Session type") +
      scale_color_manual(values = condition_colors, name = "Session type") +
      scale_x_continuous(limits = c(-x_limit, x_limit)) +  # Symmetric limits
      coord_cartesian(xlim = c(-50, 50)) +
      labs(
        title = dom,
        x = "Performance Change (%)\n← Worse | Better →",
        y = NULL  # Remove y-axis label
      ) +
      theme_minimal(base_size = 7) +
      theme(
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        plot.title = element_text(hjust = 0.1, margin = margin(b = 3, unit = "mm")),
        axis.title.x = element_text(margin = margin(t = 6)),
        axis.title.y = element_text(margin = margin(r = 6)),
        axis.text.y = element_blank(),  # Remove y-axis text
        axis.ticks.y = element_blank() # Remove y-axis ticks
      )
    
    plots_list[[dom]] <- p
  }
  
  plots_list_vert <- list()
  
  domains <- levels(domain_changes$domain)
  
  for (dom in domains) {
    
    plot_data <- domain_changes %>%
      filter(domain == dom)
    
    p <- ggplot(plot_data, aes(x = session_type, y = mean_change, fill = session_type)) +
      
      # use custom hlines instead of axes
      geom_hline(yintercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.5) +
      geom_hline(yintercept = 15, linetype = "dashed", color = "gray80", linewidth = 0.3) +
      geom_hline(yintercept = -15, linetype = "dashed", color = "gray80", linewidth = 0.3) +
      
      geom_boxplot(width = 0.5, alpha = 0.4, outlier.shape = NA) +
      
      geom_point(
        aes(color = session_type),
        alpha = 0.6,
        size = 1.5,
        position = position_jitter(width = 0.12, height = 0)
      ) +
      
      scale_fill_manual(values = condition_colors, name = "Session type") +
      scale_color_manual(values = condition_colors, name = "Session type") +
      
      scale_y_continuous(
        limits = c(-y_limit, y_limit),
        breaks = c(-15, 0, 15)
      ) +
      
      labs(
        title = dom,
        x = NULL,
        y = "Performance Change (%)\n← Worse | Better →"
      ) +
      
      theme_minimal(base_size = 7) +
      theme(
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.title = element_text(hjust = 0.1, margin = margin(b = 3, unit = "mm")),
        axis.title.y = element_text(margin = margin(r = 6)),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()
      )
    
    plots_list_vert[[dom]] <- p
  }
  
  return(list(
    plots = plots_list,
    plots_vert = plots_list_vert,
    summary_data = pebl_domain_d
  ))
}


# Equivalence testing (TOST F-test) ---------------------------------------

# Tests whether an observed F-ratio falls within a practical equivalence bound.
# Equivalence bound is specified in partial eta-squared (ηp²) units.
# Default bound (pes = 0.01) corresponds to Cohen's f = 0.10, matching both
# the negligible-effect threshold (Cohen, 1988) and the minimum effect size
# for which Porras-Salazar et al. (2024) was powered (d = 0.20).
#
# Implementation: base R pf() with non-central F distribution
# (Campbell & Lakens, 2021). Cross-validated against TOSTER::equ_ftest()
# using eqb = eqbound_pes directly (TOSTER's eqb takes ηp², not Cohen's f).
#
# anova_result: return value of get_cat_anova() — uses $stats for F, DFn, DFd
# eqbound_pes:  equivalence bound in ηp² units (NOT Cohen's f)
# alpha:        significance level for the equivalence test (default 0.05)

get_tost_ftest <- function(anova_result, eqbound_pes = 0.01, alpha = 0.05) {

  stats <- anova_result$stats

  # Convert ηp² bound to Cohen's f
  f_bound <- sqrt(eqbound_pes / (1 - eqbound_pes))

  # Non-centrality parameter when true effect equals the bound:
  # λ = (df1 + df2 + 1) × f²  (Campbell & Lakens, 2021)
  ncp_bound <- (stats$DFn + stats$DFd + 1) * f_bound^2

  # P(F ≤ F_obs | NCP = ncp_bound): small p means observed effect is in the
  # lower tail of the bound distribution → equivalence supported
  p_equiv <- pf(stats$F, df1 = stats$DFn, df2 = stats$DFd,
                ncp = ncp_bound, lower.tail = TRUE)

  equivalent <- p_equiv < alpha

  list(
    f_bound    = round(f_bound, 3),
    pes_bound  = eqbound_pes,
    p_value    = signif(p_equiv, 3),
    equivalent = equivalent,
    label      = sprintf(
      "Equivalence test (bound: ηp² = %.2f, f = %.3f): p = %.3f — %s",
      eqbound_pes, f_bound, p_equiv,
      ifelse(equivalent, "equivalence confirmed", "equivalence not confirmed")
    )
  )
}


# Friedman test for ordinal thermal comfort variables ---------------------

# Non-parametric repeated-measures test appropriate for ordinal survey data.
# Aggregates multiple responses per participant per session to one value before
# testing. Returns Friedman χ², Kendall's W effect size, and Bonferroni-
# corrected post-hoc pairwise comparisons.
#
# survey_data:   the survey tibble from x_data.R
# outcome_var:   column name as string: "thermal_sensation", "thermal_preference",
#                or "acceptability_thermal"
# session_order: factor levels for session_type

get_friedman_thermal <- function(survey_data, outcome_var,
                                 session_order = c("21C", "23C", "25C", "27C")) {

  df <- survey_data %>%
    filter(!is.na(.data[[outcome_var]]), session_type %in% session_order) %>%
    mutate(value_num = as.numeric(.data[[outcome_var]])) %>%
    group_by(subject_id, session_type) %>%
    summarise(value_num = mean(value_num, na.rm = TRUE), .groups = "drop") %>%
    mutate(session_type = factor(session_type, levels = session_order))

  # Keep only participants with complete data across all four conditions
  complete_subjects <- df %>%
    group_by(subject_id) %>%
    filter(n_distinct(session_type) == length(session_order)) %>%
    pull(subject_id) %>%
    unique()

  df_complete <- df %>% filter(subject_id %in% complete_subjects)
  n_subjects  <- length(complete_subjects)
  k           <- length(session_order)

  result <- df_complete %>%
    friedman_test(value_num ~ session_type | subject_id)

  # Kendall's W = χ² / (n × (k − 1))
  kendalls_w <- result$statistic / (n_subjects * (k - 1))

  posthoc <- df_complete %>%
    pairwise_wilcox_test(
      value_num ~ session_type,
      paired            = TRUE,
      p.adjust.method   = "bonferroni"
    )

  list(
    outcome    = outcome_var,
    n          = n_subjects,
    statistic  = round(result$statistic, 2),
    df         = result$df,
    p_value    = signif(result$p, 3),
    kendalls_w = round(kendalls_w, 3),
    posthoc    = posthoc
  )
}


# PEBL domain-level composite RM-ANOVA + TOST ------------------------------
#
# For each cognitive domain (excluding decision_making / BART), computes a
# z-score composite across its constituent metrics, runs RM-ANOVA, and tests
# equivalence using the same F-test approach as get_tost_ftest().
#
# Direction handling (inherited from get_pebl_domain_summary):
#   - "speed", "efficiency", "consistency": flip z-score sign (lower is better)
#   - "behavior": excluded entirely (BART — not a performance measure)
#   - "accuracy", "completion": keep sign (higher is better)
#
# pebl_summary: return value of get_pebl_summary() — uses $plot_data
# eqbound_pes:  equivalence bound in ηp² (default 0.01, same as CAT)

get_pebl_domain_anova_tost <- function(pebl_summary,
                                        eqbound_pes = 0.01,
                                        exclude_metric_types = "behavior",
                                        flip_metric_types    = c("speed", "efficiency", "consistency"),
                                        session_order        = c("21C", "23C", "25C", "27C")) {

  long_mean <- pebl_summary$plot_data

  # Remove BART and any other excluded metric types; keep target sessions
  long_filtered <- long_mean %>%
    filter(session_type %in% session_order,
           !metric %in% exclude_metric_types,
           !is.na(value))

  # Z-score each metric across all subjects × sessions, flip lower-is-better
  long_z <- long_filtered %>%
    group_by(parameter) %>%
    mutate(
      z = as.numeric(scale(value)),
      z = ifelse(metric %in% flip_metric_types, -z, z)
    ) %>%
    ungroup()

  # Average z-scores within domain for each subject × session
  domain_composites <- long_z %>%
    group_by(subject_id, session_type, domain) %>%
    summarise(composite = mean(z, na.rm = TRUE), .groups = "drop") %>%
    mutate(session_type = factor(session_type, levels = session_order))

  domains <- sort(unique(domain_composites$domain))

  purrr::map(setNames(domains, domains), function(dom) {

    df_dom <- domain_composites %>% filter(domain == dom)

    # Complete-case subjects: all 4 sessions present, no NA composite
    complete_ids <- df_dom %>%
      group_by(subject_id) %>%
      filter(n_distinct(session_type) == length(session_order),
             !any(is.na(composite))) %>%
      pull(subject_id) %>%
      unique()

    df_c <- df_dom %>% filter(subject_id %in% complete_ids)
    n    <- length(complete_ids)

    aov_res <- tryCatch(
      anova_test(data = df_c, dv = composite, wid = subject_id,
                 within = session_type, effect.size = "pes"),
      error = function(e) NULL
    )
    if (is.null(aov_res)) return(list(domain = dom, n = n, error = "ANOVA failed"))

    tab <- get_anova_table(aov_res)   # applies GG correction if sphericity violated
    row <- tab[tab$Effect == "session_type", ]

    F_val   <- row$F
    DFn     <- row$DFn
    DFd     <- row$DFd
    p_final <- row$p
    pes_val <- row$pes

    # TOST equivalence test (same non-central F approach as get_tost_ftest)
    f_bound   <- sqrt(eqbound_pes / (1 - eqbound_pes))
    ncp_bound <- (DFn + DFd + 1) * f_bound^2
    p_equiv   <- pf(F_val, df1 = DFn, df2 = DFd, ncp = ncp_bound, lower.tail = TRUE)

    list(
      domain     = dom,
      n          = n,
      F          = round(F_val, 3),
      DFn        = DFn,
      DFd        = round(DFd, 0),
      p          = signif(p_final, 3),
      pes        = round(pes_val, 3),
      p_equiv    = signif(p_equiv, 3),
      equivalent = p_equiv < 0.05
    )
  })
}


# LMM sensitivity check for CAT -------------------------------------------

# Fits a linear mixed-effects model equivalent to the repeated-measures ANOVA.
# For a balanced complete-case design, results should be numerically identical.
# Used to confirm robustness of RM-ANOVA results and address reviewer request
# to justify or replace RM-ANOVA with mixed-effects models.
#
# cat_data: CAT test data, pre-filtered to complete-case participants.

get_cat_lmm_sensitivity <- function(cat_data,
                                    session_order = c("21C", "23C", "25C", "27C")) {

  df <- cat_data %>%
    mutate(session_type = factor(session_type, levels = session_order))

  model <- lmer(
    test_cat_score ~ session_type + (1 | subject_id),
    data = df,
    REML = FALSE
  )

  # anova() from lmerTest provides Satterthwaite df and p-values
  aov_table <- anova(model)

  list(
    F     = round(aov_table[["F value"]][1], 3),
    DFn   = aov_table[["NumDF"]][1],
    DFd   = round(aov_table[["DenDF"]][1], 1),
    p     = signif(aov_table[["Pr(>F)"]][1], 3),
    model = model
  )
}


# LMM for performer subgroup analyses, controlling for individual learning -----
#
# Replaces the mean-correction RM-ANOVA approach for high/low performer
# subgroups. Including trial_number as a fixed covariate absorbs the average
# learning trend; the random slope (1 + trial_number | subject_id) allows
# each participant their own learning rate, addressing Reviewer 1's concern
# that a population-mean correction introduces bias for subgroups with
# different learning curves. Falls back to random intercept only when the
# slope model is singular (typical at n ≈ 19).
#
# data:         subgroup CAT data (high or low performers)
# eqbound_pes:  equivalence bound in ηp² units (default 0.01)
# session_order: factor levels for session_type

get_cat_lmm_subgroup <- function(data,
                                  eqbound_pes = 0.01,
                                  session_order = c("21C", "23C", "25C", "27C")) {

  df <- data %>%
    filter(session_type %in% session_order) %>%
    mutate(
      session_type = factor(session_type, levels = session_order),
      trial_number = as.numeric(trial_number)
    )

  n_subj <- length(unique(df$subject_id))

  # Attempt random intercept + slope (individual learning curves)
  model <- tryCatch(
    suppressMessages(suppressWarnings(
      lmer(test_cat_score ~ session_type + trial_number +
             (1 + trial_number | subject_id),
           data = df, REML = FALSE)
    )),
    error = function(e) NULL
  )

  # Fall back to random intercept only if model failed or is singular
  if (is.null(model) || isSingular(model)) {
    random_slopes <- FALSE
    model <- lmer(
      test_cat_score ~ session_type + trial_number + (1 | subject_id),
      data = df, REML = FALSE
    )
  } else {
    random_slopes <- TRUE
  }

  aov_tab <- anova(model)
  row     <- aov_tab["session_type", ]
  F_val   <- round(row[["F value"]], 3)
  DFn     <- row[["NumDF"]]
  DFd     <- round(row[["DenDF"]], 0)
  p_val   <- signif(row[["Pr(>F)"]], 3)
  pes     <- round(F_val * DFn / (F_val * DFn + DFd), 3)

  # TOST (same non-central F approach as get_tost_ftest)
  f_bound   <- sqrt(eqbound_pes / (1 - eqbound_pes))
  ncp_bound <- (DFn + DFd + 1) * f_bound^2
  p_equiv   <- pf(F_val, df1 = DFn, df2 = DFd, ncp = ncp_bound, lower.tail = TRUE)

  list(
    model         = model,
    random_slopes = random_slopes,
    n             = n_subj,
    F             = F_val,
    DFn           = DFn,
    DFd           = DFd,
    p             = p_val,
    pes           = pes,
    p_equiv       = signif(p_equiv, 3),
    equivalent    = p_equiv < 0.05
  )
}