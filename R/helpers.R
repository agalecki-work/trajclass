#' Create a project setup list for trajectory analysis
#'
#' @param data A data frame or tibble in long format with columns for subject ID,
#'   time, and outcome variable.
#' @param keys A character vector of length specifying the id_col, time_col, y_colcolumn names in `data`.
#' @param nobs_persub Integer. Minimum number of observations required per subject.
#'   Default is 10.
#'
#' @return A list with class `"project_setup"` containing:
#'   - `data`: the input data (unchanged)
#'   - `keys`: the validated key variables vector
#'   - `nobs_persub`: the minimum number of observations per subject
#'
#' @export
create_project_setup <- function(data,
                                 keys,
                                 nobs_persub = NULL) {
  
  # Input checks
  if (!is.data.frame(data) && !inherits(data, "tbl_df")) {
    stop("`data` must be a data frame or tibble", call. = FALSE)
  }
  
  if (length(keys) != 3L) stop("keys has to be a vector of length 3")
  
  required_cols <- unname(keys)
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop(
      "The following columns specified in `keys` are missing from `data`: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }
  
  if (!is.numeric(nobs_persub) || length(nobs_persub) != 1L ||
      !is.finite(nobs_persub) || nobs_persub < 1) {
    stop("`nobs_persub` must be a positive integer", call. = FALSE)
  }
  
  # Create the structure
  setup <- list(
    data        = data,
    keys        = keys,
    nobs_persub = nobs_persub
  )
  
  class(setup) <- "project_setup"
  
  setup
}
