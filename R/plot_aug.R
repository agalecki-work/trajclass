#' Augment observed data with model predictions and diagnostics
#'
#' The first three columns of `aug` are treated as: 
#'   1. subject ID, 
#'   2. time variable, 
#'   3. outcome variable.
#' No specific names ("time", "y", etc.) are required.
#'
#' @param aug An augmented tibble returned by augment-style functions.
#'   Must have at least 3 columns: ID, time, outcome, followed by .fitted, .se_fit, etc.
#' @param se_fit Logical. Include standard error of fitted values? (default: TRUE)
#' @param ... Passed to predict() methods (not currently used)
#'
#' @return A ggplot object
#' @export
plot_augmented_traj <- function(aug,
                                title = "Trajectory: Observed vs Fitted",
                                ci_mult = 1.96,
                                point_size = 1.8,
                                line_size = 1.1,
                                ribbon_alpha = 0.2) {
  
  # ── Minimal validation ─────────────────────────────────────────────────────
  if (!is.data.frame(aug) || ncol(aug) < 4) {
    stop("`aug` must be a data frame/tibble with at least 4 columns", call. = FALSE)
  }
  
  # ── Identify columns dynamically ───────────────────────────────────────────
  # First 3 columns: ID, time, outcome (whatever their names are)
  id_col    <- names(aug)[1]   # subject identifier
  time_col  <- names(aug)[2]   # time / ordering variable
  y_col     <- names(aug)[3]   # outcome variable
  
  # ── Prepare data for plotting ──────────────────────────────────────────────
  plot_data <- aug |>
    dplyr::rename(
      .time  = all_of(time_col),
      .y     = all_of(y_col)
    )
  
  has_se <- ".se_fit" %in% names(plot_data)
  
  p <- ggplot(plot_data, aes(x = .time, y = .y)) +
    # Observed points
    geom_point(size = point_size, color = "grey30", alpha = 0.8) +
    
    # Fitted line
    geom_line(aes(y = .fitted), color = "firebrick", linewidth = line_size) +
    
    # CI ribbon (if .se_fit exists)
    { if (has_se) {
      geom_ribbon(
        aes(ymin = .fitted - ci_mult * .se_fit,
            ymax = .fitted + ci_mult * .se_fit),
        fill = "firebrick", alpha = ribbon_alpha
      )
    }} +
    
    # Labels & theme
    labs(
      title    = title,
      subtitle = if (has_se) "Solid line: fitted • Ribbon: ≈95% pointwise CI" 
                 else "Solid line: fitted values",
      x        = time_col,          # use actual column name
      y        = y_col              # use actual column name
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(color = "grey50")
    )
  
  p
}