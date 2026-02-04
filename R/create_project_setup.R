#' Create a project setup object for trajectory analysis
#'
#' Creates a simple list object of class `"project_setup"` containing the input
#' data, key variable names, and minimum number of observations per subject.
#'
#' @param data A data frame or tibble in long format containing at least the
#'   three key columns.
#' @param keys Named character vector of length 3 with names `"id"`, `"time"`,
#'   `"y"`, giving the column names in `data`.
#' @param nobs_persub Positive integer. Minimum number of observations required
#'   per subject (default: 10).
#'
#' @return A list with class `"project_setup"` containing:
#'   \describe{
#'     \item{`data`}{The input data frame/tibble (unchanged)}
#'     \item{`keys`}{The supplied key variables vector}
#'     \item{`nobs_persub`}{The minimum number of observations per subject}
#'   }
#'
#' @export
create_project_setup <- function(data,
                                 keys,
                                 nobs_persub = 10L) {
  
  # Input checks
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame or tibble", call. = FALSE)
  }
  
  if (!is.character(keys) || length(keys) != 3L ||
      !identical(sort(names(keys)), sort(c("id", "time", "y")))) {
    stop(
      "`keys` must be a named character vector of length 3 with names ",
      "'id', 'time', and 'y'", call. = FALSE
    )
  }
  
  required_cols <- unname(keys)
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop(
      "Column(s) missing in `data`: ", paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }
  
  if (!is.numeric(nobs_persub) || length(nobs_persub) != 1L ||
      nobs_persub < 1 || nobs_persub != round(nobs_persub)) {
    stop("`nobs_persub` must be a positive integer", call. = FALSE)
  }
  
  # Build the object
  setup <- list(
    data        = data,
    keys        = keys,
    nobs_persub = as.integer(nobs_persub)
  )
  
  class(setup) <- "project_setup"
  
  setup
}

