# data-generation.R
# Functions for generating synthetic longitudinal trajectories
# for testing and demonstration purposes

#' Generate a single synthetic trajectory
#'
#' Internal helper function to create one realistic-looking time series
#' with specified pattern.
#'
#' @param id Identifier for the trajectory (will be coerced to character)
#' @param pattern One of: "linear", "quadratic", "hockey_stick"
#' @param years Numeric vector of time points (default: 0:26)
#' @param baseline_y Starting value of the outcome (default: 125)
#' @param noise_sd Standard deviation of Gaussian noise (default: 2)
#' @param key_vars Named list defining column names for id, time, and outcome
#'   (default: `list(id_col = "id", time_col = "time", y_col = "y")`)
#'
#' @return A tibble with columns named according to `key_vars` + `true_pattern`
#' @keywords internal
generate_one_trajectory <- function(
    id,
    pattern,
    years = 0:26,
    baseline_y = 125,
    noise_sd = 2,
    key_vars = list(id_col = "id", time_col = "time", y_col = "y")
) {
  e <- stats::rnorm(length(years), sd = noise_sd)
  
  if (pattern == "linear") {
    y <- baseline_y - 1.1 * years + e
  } else if (pattern == "quadratic") {
    y <- baseline_y - 0.8 * years - 0.08 * years^2 + e
  } else if (pattern == "hockey_stick") {
    bp <- sample(10:18, 1)
    before_idx <- years < bp
    after_idx  <- years >= bp
    
    before <- baseline_y - 0.8 * years[before_idx]
    after  <- before[length(before)] - 6.5 * (years[after_idx] - bp)
    
    y <- numeric(length(years))
    y[before_idx] <- before
    y[after_idx]  <- after
    y <- y + e
  } else {
    stop("Unknown pattern. Choose from: linear, quadratic, hockey_stick")
  }
  
  tibble::tibble(
    !!key_vars$id_col   := as.character(id),
    !!key_vars$time_col := years,
    !!key_vars$y_col    := y,
    true_pattern        = pattern
  )
}


#' Generate synthetic trajectory dataset for testing
#'
#' Creates a balanced dataset with multiple trajectories per pattern,
#' including controlled proportions of missing values.
#'
#' The built-in dataset `example_data` was created using default parameters
#' of this function (`n_sub = 1`, `pct_miss = c(0,10,50,90)`).
#'
#' @param n_sub Number of subjects (trajectories) **per pattern** and **per missingness level** (default: 1)
#' @param pct_miss Numeric vector of missingness percentages (default: c(0,10,50,90))
#' @param years Time points (default: 0:26)
#' @param baseline_y Starting value of the outcome variable (default: 125)
#' @param noise_sd Standard deviation of measurement noise (default: 2)
#' @param seed Random seed for reproducibility (default: NULL)
#' @param key_vars Named list defining column names for id, time, and outcome  
#'   Default: `list(id_col = "id", time_col = "time", y_col = "y")`
#'
#' @return A tibble with columns named according to `key_vars` + `true_pattern` (character)
#'
#' @examples
#' set.seed(20250116)
#' 
#' # Default column names
#' sim_default <- generate_synthetic_data(n_sub = 2, pct_miss = c(0, 30))
#' str(sim_default)           # columns: id, time, y, true_pattern
#' 
#' # Custom column names
#' sim_custom <- generate_synthetic_data(
#'   n_sub = 1,
#'   pct_miss = c(0, 50),
#'   key_vars = list(
#'     id_col   = "ID",
#'     time_col = "time",
#'     y_col    = "egfr"
#'   )
#' )
#' colnames(sim_custom)            # columns: ID, time, egfr, true_pattern
#' @export
generate_synthetic_data <- function(
    n_sub = 1,
    pct_miss = c(0, 10, 50, 90),
    years = 0:26,
    baseline_y = 125,
    noise_sd = 2,
    seed = NULL,
    key_vars = list(id_col = "id", time_col = "time", y_col = "y")
) {
  if (!is.null(seed)) set.seed(seed)

  patterns <- c("linear", "quadratic", "hockey_stick")
  n_patterns <- length(patterns)
  n_miss <- length(pct_miss)

  # Create all combinations
  combo <- expand.grid(
    subj_idx   = 1:n_sub,
    pattern_idx = 1:n_patterns,
    miss_idx    = 1:n_miss,
    stringsAsFactors = FALSE
  )

  # Construct ID: "<Subjectnumber>_<patternnumber><pct_miss/10>"
  combo$id <- sprintf(
    "%d_%d%d",
    combo$subj_idx,
    combo$pattern_idx,
    pct_miss[combo$miss_idx] / 10
  )

  trajectories <- purrr::pmap_dfr(
    .l = list(
      id       = combo$id,
      pattern  = patterns[combo$pattern_idx],
      pct_miss = pct_miss[combo$miss_idx]
    ),
    .f = function(id, pattern, pct_miss) {

      traj <- generate_one_trajectory(
        id          = id,
        pattern     = pattern,
        years       = years,
        baseline_y  = baseline_y,
        noise_sd    = noise_sd,
        key_vars    = key_vars
      )

      # Apply missingness to outcome column
      y_col <- key_vars$y_col
      n <- nrow(traj)
      if (pct_miss > 0 && pct_miss <= 100) {
        n_miss <- round(n * pct_miss / 100)
        if (n_miss > 0) {
          miss_idx <- sample(seq_len(n), size = n_miss, replace = FALSE)
          traj[[y_col]][miss_idx] <- NA_real_
        }
      }

      # If ALL outcome values became NA → put one realistic value back
      if (all(is.na(traj[[y_col]]))) {
        one_time <- sample(years, size = 1)
        one_idx  <- which(traj[[time_col]] == one_time)
        traj[[y_col]][one_idx] <- baseline_y - 1.5 * one_time + stats::rnorm(1, sd = noise_sd * 1.3)
      }

      traj
    }
  )

  # Final sorting
  trajectories |>
    dplyr::arrange(.data[[key_vars$id_col]], .data[[key_vars$time_col]])
}
