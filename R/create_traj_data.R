#' Create a base trajectory data object (pre-processing step)

#'
#' @description
#' Takes original long-format data, selects the three key columns specified in `key_vars`,
#' removes any row where **any** of the three key columns is missing (`NA`)
#'
#' The result is a minimal tibble with class `"traj_data"` (inheriting from `tbl_df` / tibble).
#' One attribute is added:  
#' - `data_info` — tibble with one row with basic information on input data
#'
#' This function is intended as the very first pre-processing step before further
#' operations (filtering by nobs per subject, classification, etc.).
#'
#' @param data A data frame or tibble in long format
#' @param key_vars **Required.** Named character vector of length 3 defining the column names.  
#'   Must have names `"id_col"`, `"time_col"`, `"y_col"`.  
#'   Example: `c(id_col = "ID", time_col = "time", y_col = "egfr")`
#'
#' @return A tibble with names of key columns columns retained  (object of class `"traj_data")`  
#'   (inherits from `tbl_df`, `tbl`, `data.frame`).  
#'   All rows with missing values in any key column are removed.  
#'   
#'   Attributes:
#'   - `data_info` : tibble with one row describing input data
#'
#' @examples
#' \dontrun{
#' # Basic usage with example_data
#' orig_keys <- getOption("trajclass.key_vars", default = NULL)
#' traj_df <- create_traj_data(example_data, key_vars = orig_keys)
#' 
#' 
#' # Check attributes
#' attrx <- attributes(traj_df)
#' names(attrx)                      # "names" "row.names" "class" "info"
#' names(attrx$info)                 # "orig_key_vars" "datain" "dataout" 
#' attrx$info$orig_key_vars          # named list
#' attrx$info$datain                 # a tibble with 1 row
#' attrx$info$dataout$key_vars       # character vector with names, e.g. [1] "ID"   "time" "egfr"
#' # Check if No rows with missing key values
#' any(is.na(traj_df$ID))          # FALSE
#' any(is.na(traj_df$time))        # FALSE
#' any(is.na(traj_df$egfr))        # FALSE 
#' 
#' }
#'
#' @export
create_traj_data <- function(data, 
         key_vars = getOption("trajclass.key_vars", default = NULL) , 
         .msg = TRUE
         ) {
  datain_info <- inspect_parent_data(.msg = .msg)   # tibble with one row
 
  
  # ── Input validation ────────────────────────────────────────────────────────
  stopifnot(is.data.frame(data))

   if (is.null(key_vars)) {
    # Option has never been set
    stop(
      "Key columns are not configured yet.\n",
      "Please set them first, for example:\n",
      "  options(trajclass.key_vars = c(id_col = \"ID\", time_col = \"time\", y_col = \"egfr\"))\n",
      call. = FALSE
    )
  }

  
  if (missing(key_vars)) {
    stop("Argument 'key_vars' is required. Example: c(id_col = 'ID', time_col = 'time', y_col = 'egfr')")
  }
  
  if (!is.character(key_vars) || length(key_vars) != 3 || is.null(names(key_vars))) {
    stop("'key_vars' must be a named character vector of length 3")
  }
  
  required_names <- c("id_col", "time_col", "y_col")
  if (!all(required_names %in% names(key_vars))) {
    stop("key_vars must contain names: ", paste(required_names, collapse = ", "))
  }
  
  id_col   <- key_vars["id_col"]
  time_col <- key_vars["time_col"]
  y_col    <- key_vars["y_col"]
  
  required_cols <- c(id_col, time_col, y_col)
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("Required column(s) not found in data: ", paste(missing_cols, collapse = ", "))
  }
  
  # ── Select key columns + remove rows with any missing key value. Keep original names ─────────────
  # ── 2. Select key columns + early rename to standardized names ─────────────
  out <- data |>
    dplyr::select(dplyr::all_of(c(id_col, time_col, y_col))) |>
    dplyr::rename(
      id   = id_col,
      time = time_col,
      y    = y_col
    ) |>
    # ── Remove rows with missing values in any key column ──────────────────────
    dplyr::filter(
      !is.na(id),
      !is.na(time),
      !is.na(y)
    )
  colnames(out) <- key_vars
  
  # ── Set base class ──────────────────────────────────────────────────────────
  class(out) <- c("traj_data", class(out))
  
  # ── Attach attributes ───────────────────────────────────────────────────────
       
 
 out_info = tibble::tibble(
      key_vars  = unname(key_vars),
      class_df  = class(out)[1],
      nobs      = nrow(out)
    )
 
  info <- list(
       orig_key_vars = as.list(key_vars),
       datain = datain_info, 
       dataout  = out_info   #    tibble with one row     
  )

  attr(out, "info") <- info   # 
  out
}
