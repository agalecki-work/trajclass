#' Check structure of longitudinal data in long format
#'
#' @description
#' Verifies whether a longitudinal dataset in long format has:
#' - one row per time point per subject
#' - rows properly grouped and sorted by subject ID and time
#' - no duplicate (id, time) combinations
#' - classifies remaining variables as time-varying or time-invariant
#'
#' @details
#' The function:
#' 1. Converts the ID column to character (if it is a factor). A message is issued.
#' 2. Sorts the data by ID and time
#' 3. Checks whether rows with the same ID are consecutive and times are non-decreasing within each ID
#' 4. Detects duplicate (ID, time) pairs
#' 5. Determines which variables vary within subjects (time-varying) vs. are constant per subject (time-invariant)
#'
#' @note
#' - The function assumes the time column is numeric (or something `diff()` can handle). If time is a Date/POSIXct,
#'   convert it to numeric days beforehand.
#' - `id_col` and `time_col` are **required** (no defaults) to avoid silent assumptions about column names.
#' - Because the package depends on `segmented` (which loads `MASS`), we deliberately avoid broad preferences
#'   for `dplyr::select`. Use explicit `dplyr::select()` when needed.
#'
#' @param data A data frame in long format (one row per observation per subject)
#' @param id_col **Required.** String. Name of the subject identifier column.
#' @param time_col **Required.** String. Name of the time/visit/measurement occasion column.
#'
#' @return A named list with three components:
#'   \describe{
#'     \item{`is_properly_sorted`}{Logical. Are rows properly grouped by ID and sorted by time within groups?}
#'     \item{`has_no_duplicate_id_time`}{Logical. Are there no duplicate (id, time) combinations?}
#'     \item{`classification`}{A tibble with columns `variable`, `is_time_varying`, `status` showing
#'       whether each variable (excluding `id_col` and `time_col`) is time-varying or time-invariant.}
#'   }
#'
#' @examples
#' \dontrun{
#' # Basic usage – must supply both column names
#' result <- check_long_structure(
#'   data     = example_data,
#'   id_col   = "ID",
#'   time_col = "time"
#' )
#' names(result)
#' result$is_properly_sorted
#' result$has_no_duplicate_id_time
#' tvars <- result$classification
#' glimpse(tvars)
#' }
#' 
#' @keywords internal
check_long_structure <- function(data, id_col, time_col) {

  # ── Input validation ──────────────────────────────────────────────────────────
  stopifnot(is.data.frame(data))

  if (missing(id_col)) {
    stop("Argument 'id_col' is required. Example: id_col = 'ID'")
  }
  if (missing(time_col)) {
    stop("Argument 'time_col' is required. Example: time_col = 'time'")
  }

  required_cols <- c(id_col, time_col)
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("Required column(s) not found: ", paste(missing_cols, collapse = ", "))
  }

  # ── Convert id to character (handles factors safely) ──────────────────────────
  if (is.factor(data[[id_col]])) {
    message("Converting '", id_col, "' from factor to character")
    data[[id_col]] <- as.character(data[[id_col]])
  }

  # Sort once (safer and needed for consecutive check)
  data <- data |>
    dplyr::arrange(.data[[id_col]], .data[[time_col]])

  # ── 1. Check proper grouping & sorting ───────────────────────────────────────

  # Are same-ID rows consecutive? (no interleaving of subjects)
  consecutive_groups <- {
    id_vec <- data[[id_col]]
    same_as_prev <- id_vec == dplyr::lag(id_vec, default = id_vec[1])
    same_as_next <- id_vec == dplyr::lead(id_vec, default = id_vec[nrow(data)])
    all(same_as_prev | same_as_next)
  }

  # Within each id: time non-decreasing
  time_sorted_within <- data |>
    dplyr::group_by(.data[[id_col]]) |>
    dplyr::summarise(
      time_diff_ok = all(diff(.data[[time_col]]) >= 0 | is.na(diff(.data[[time_col]]))),
      .groups = "drop"
    ) |>
    dplyr::pull(time_diff_ok) |>
    all(na.rm = TRUE)

  is_properly_sorted <- consecutive_groups && time_sorted_within

  # ── 2. Check for duplicate (id, time) pairs ──────────────────────────────────
  dups <- data |>
    dplyr::count(.data[[id_col]], .data[[time_col]], name = "cnt") |>
    dplyr::filter(cnt > 1L)

  has_duplicates <- nrow(dups) > 0

  # ── 3. Classify time-varying vs time-invariant variables ─────────────────────
  exclude_cols <- c(id_col, time_col)

  varying <- data |>
    dplyr::group_by(.data[[id_col]]) |>
    dplyr::summarise(
      dplyr::across(
        .cols = -dplyr::any_of(exclude_cols),
        .fns  = ~ n_distinct(.) > 1L | any(is.na(.)) != all(is.na(.))
      ),
      .groups = "drop"
    ) |>
    dplyr::summarise(dplyr::across(-dplyr::any_of(id_col), any)) |>
    tidyr::pivot_longer(
      cols      = dplyr::everything(),
      names_to  = "variable",
      values_to = "is_time_varying"
    ) |>
    dplyr::mutate(status = dplyr::if_else(
      is_time_varying,
      "time-varying",
      "time-invariant"
    ))

  # ── Return ───────────────────────────────────────────────────────────────────
  list(
    is_properly_sorted       = is_properly_sorted,
    has_no_duplicate_id_time = !has_duplicates,
    classification           = varying
  )
}


#' Inspect 'data' argument of the calling function and return info as tibble
#'
#' @details Must be called as the **very first expression** inside a function
#'   that has a formal argument named `data`.
#'
#' @param .msg logical. Whether to print a short message about the data
#'   (default: `TRUE`)
#'
#' @return A one-row `tibble` with information about the `data` argument
#' @export
inspect_parent_data <- function(.msg = FALSE) {
  
  parent_call  <- sys.call(-1L)
  parent_frame <- sys.frame(-1L)
  parent_fun   <- sys.function(-1L)
  
  formal_names <- names(formals(parent_fun))
  
  if (!"data" %in% formal_names) {
    stop("inspect_parent_data() requires the parent function to have a 'data' argument")
  }
  
  data_pos <- match("data", formal_names)
  data_expr <- parent_call[[data_pos + 1L]]
  
  # Friendly name for printing / reporting
  data_name <- 
    if (is.symbol(data_expr)) {
      as.character(data_expr)
    } else if (is.call(data_expr) && identical(data_expr[[1L]], quote(`::`))) {
      sprintf("%s::%s", as.character(data_expr[[2L]]), as.character(data_expr[[3L]]))
    } else if (is.call(data_expr) && identical(data_expr[[1L]], quote(`$`))) {
      as.character(data_expr[[2L]])
    } else {
      deparse1(data_expr, width.cutoff = 40L)
    }
  
  data_value <- parent_frame$data
  
  # ── Compute summary values ───────────────────────────────────────────────────
  cls   <- if (is.null(data_value)) "NULL" else paste(class(data_value), collapse = ", ")
  typ   <- typeof(data_value)
  nr    <- if (is.data.frame(data_value) || is.matrix(data_value)) nrow(data_value) else NA_integer_
  nc    <- if (is.data.frame(data_value) || is.matrix(data_value)) ncol(data_value) else NA_integer_
  sz    <- if (is.null(data_value)) NA_real_ else as.numeric(utils::object.size(data_value))
  
  # ── Build one-row tibble ─────────────────────────────────────────────────────
  out <- tibble::tibble(
    name_as_called     = data_name,
    class              = cls,
    typeof             = typ,
    nrow               = nr,
    ncol               = nc,
    size_bytes         = sz,
    size_pretty        = format(sz, big.mark = ",", scientific = FALSE),
    expression         = deparse1(data_expr, width.cutoff = 500L)
  )
  
  # ── Optional message ─────────────────────────────────────────────────────────str

  if (.msg && !isTRUE(getOption("inspect_parent_data.quiet", FALSE))) {
    dim_part <- if (!is.na(nr) && !is.na(nc)) sprintf(" (%d × %d)", nr, nc) else ""
    msg <- sprintf(
      "Working with data: %s\nClass: %s%s\nSize: %s bytes\n",
      data_name, cls, dim_part, out$size_pretty[[1L]]
    )
    message(msg)
  }
  
  out
}

