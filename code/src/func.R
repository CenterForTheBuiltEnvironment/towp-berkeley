# Title: Plotting functions
# Description: Define functions for plotting different types experimental results
# Author: Toby Kramer
# Date: 2025-10-29

source(here::here("code", "src", "setup.R"))


# Tables ---------------------------------------------------------------

ft_std <- function(data) {
  
  thin_border  <- fp_border(color = "black", width = 0.4)
  thick_border <- fp_border(color = "black", width = 1.0)

  ft <- flextable(data)
  
  # ---- Global standard settings ----
  ft <- font(ft, fontname = "Arial", part = "all")
  ft <- fontsize(ft, size = 9, part = "body")
  ft <- fontsize(ft, size = 9, part = "header")
  ft <- bold(ft, part = "header")
  
  # Borders
  ft <- border_remove(ft)
  ft <- hline_top(ft, part = "header", border = thin_border)
  ft <- hline(ft, part = "header", border = thin_border)
  ft <- hline_bottom(ft, part = "body", border = thin_border)
  
  # Spacing
  ft <- padding(ft, padding = 3)
  
  
  return(ft)
}


# Subject Feedback --------------------------------------------------------

stacked_pct_data <- function(data, var) {
  
  var <- rlang::enquo(var)
  
  data %>%
    drop_na(!!var) %>%
    count(session_type, !!var, name = "n") %>%
    group_by(session_type) %>%
    mutate(
      pct = n / sum(n),
      pct_label = scales::percent(pct, accuracy = 1)
    ) %>%
    ungroup()
}

plot_stacked_pct <- function(data, var, palette, legend_title = NULL) {
  
  df <- data %>%
    drop_na({{ var }})
  
  ggplot(df, aes(x = session_type, fill = {{ var }})) +
    geom_bar(position = "fill") +
    scale_y_continuous(labels = percent, expand = expansion(mult = c(0, 0.01))) +
    scale_fill_manual(values = palette, labels = label_wrap_gen(width = 9)) +
    labs(
      x = "Session type",
      y = "Percentage",
      fill = legend_title   # <-- key addition
    ) +
    theme_minimal(base_size = 7) +
    theme(
      legend.position = "right",
      legend.direction = "vertical",
      legend.title = if (is.null(legend_title)) element_blank() else element_text(size = 6),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.title.x = element_text(margin = margin(t = 6)),
      axis.ticks.y = element_line(color = "grey", linewidth = 0.25),
      axis.ticks.x = element_blank(),
      axis.ticks.length = unit(1, "mm")
    ) +
    guides(
      fill = guide_legend(
        ncol = 1,
        byrow = TRUE,
        label.position = "right",
        keywidth  = unit(7.5, "mm"),
        keyheight = unit(5, "mm"),
        label.hjust = 0,
        label.vjust = 0.5
      )
    )
}



# Test scores -------------------------------------------------------------

get_cat_summary <- function(data, score_col, group_col) {
  score_col <- enquo(score_col)
  group_col <- enquo(group_col)
  
  data %>%
    group_by(!!group_col) %>%
    summarise(
      mean_score = round(mean(!!score_col, na.rm = TRUE), 1),
      n = n(),
      sd_score = round(sd(!!score_col, na.rm = TRUE), 1),
      .groups = "drop"
    )
}


make_pebl_flextable <- function(df,
                                save_image  = FALSE,
                                saving_path = NULL,
                                zoom        = 5) {
  # make sure flextable is available
  if (!requireNamespace("flextable", quietly = TRUE)) {
    stop("Package 'flextable' is required but not installed.")
  }
  
  # define a standard border (similar to your std_border)
  std_border <- officer::fp_border(color = "black", width = 1)
  
  # build flextable
  ft <- flextable::flextable(df)
  ft <- flextable::font(ft, fontname = "Arial")
  ft <- flextable::fontsize(ft, size = 5)
  ft <- flextable::bold(ft, part = "header")
  ft <- flextable::fontsize(ft, size = 6, part = "header")
  ft <- flextable::color(ft, color = "black", part = "header")
  ft <- flextable::bg(ft, bg = "grey90", part = "header")
  ft <- flextable::padding(ft, padding = 3)
  
  # borders
  ft <- flextable::border_remove(ft)
  ft <- flextable::hline_top(ft,    part = "header", border = std_border)
  ft <- flextable::hline(ft,        part = "header", border = std_border)
  ft <- flextable::hline_bottom(ft, part = "body",   border = std_border)
  
  # alignment
  ft <- flextable::align(ft, align = "left", part = "all")
  ft <- flextable::valign(ft, valign = "top", part = "all")
  
  # table properties
  ft <- flextable::set_table_properties(ft, width = 1)
  
  # column widths (only if those columns exist)
  if (ncol(df) >= 1) {
    ft <- flextable::width(ft, j = 1, width = 0.5)
  }
  if (ncol(df) >= 8) {
    ft <- flextable::width(ft, j = 8, width = 0.5)
  }
  
  # optionally save as image
  if (isTRUE(save_image)) {
    if (is.null(saving_path)) {
      stop("If save_image = TRUE, you must provide a 'saving_path'.")
    }
    flextable::save_as_image(
      ft,
      path = saving_path,
      zoom = zoom
    )
  }
  
  ft
}




# Physiology data ---------------------------------------------------------

clean_rr <- function(data,
                     rr_col,
                     group_vars = NULL,
                     min_rr = 300,
                     max_rr = 2000,
                     max_delta_prop = 0.20,
                     use_mad = TRUE,
                     mad_mult = 3) {
  
  rr_col <- enquo(rr_col)
  
  df <- data
  
  # Optional grouping (e.g. c("subject_id", "session_id"))
  if (!is.null(group_vars)) {
    df <- df %>% group_by(across(all_of(group_vars)))
  }
  
  df <- df %>%
    # 1) Physiological range
    filter(between(!!rr_col, min_rr, max_rr)) %>%
    
    # 2) Beat-to-beat delta filter
    mutate(
      prev_rr  = lag(!!rr_col),
      rr_diff  = abs(!!rr_col - prev_rr),
      delta_ok = is.na(prev_rr) | rr_diff <= max_delta_prop * prev_rr
    ) %>%
    filter(delta_ok) 
  
  # 3) Optional MAD filter (robust outliers)
  if (use_mad) {
    df <- df %>%
      mutate(
        rr_med = median(!!rr_col, na.rm = TRUE),
        rr_mad = mad(!!rr_col, na.rm = TRUE),
        mad_ok = ifelse(
          is.na(rr_mad) | rr_mad == 0,
          TRUE,  # if MAD cannot be computed, keep values
          abs(!!rr_col - rr_med) <= mad_mult * rr_mad
        )
      ) %>%
      filter(mad_ok)
  }
  
  df %>%
    select(-prev_rr, -rr_diff, -delta_ok,
           -matches("^rr_med$|^rr_mad$|^mad_ok$")) %>%
    ungroup()
}

