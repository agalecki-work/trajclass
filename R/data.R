#' Synthetic example trajectory dataset
#'
#' A small synthetic dataset generated using `generate_synthetic_data`:
#' \itemize{
#'   \item key_vars = list(id_col = "ID", time_col = "time", y_col = "egfr")
#'   \item n_sub = 1 (one subject per pattern per missingness level)
#'   \item pct_miss = c(0, 10, 50, 90)
#'   \item years = 0:26
#'   \item baseline_y = 125
#'   \item noise_sd = 2
#' }
#'
#' Total: 1 subject x 3 patterns × 4 missingness levels = 12 trajectories, 324 rows.
#'
#' @format A tibble with 324 rows and 4 columns:
#' \describe{
#'   \item{id}{Character subject identifier (e.g. "1_10", "1_13", "1_50", ...)}
#'   \item{time}{Time points (0 to 26)}
#'   \item{egfr}{Simulated outcome variable}
#'   \item{true_pattern}{One of: "linear", "quadratic", "hockey_stick"}
#' }
#' @source Generated with seed 20250116 via `generate_synthetic_data()`
#' @keywords datasets
"example_data"
