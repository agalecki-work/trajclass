# Silence codetools / R CMD check warnings about ellipsis in tidy-eval contexts
utils::globalVariables("...")

# Silence codetools / R CMD check warnings about ellipsis in tidy-eval contexts
utils::globalVariables("...")

#' Create standardized trajectory object
#'
#' @description
#' Prepares a clean tibble with standardized columns `id`, `time`, `y` 
#' from a longitudinal dataset. Removes rows with missing key values, 
#' then applies user-specified or default filtering.
#'
#' The returned object has class `"traj"` plus one of:
#' \itemize{
#'   \item `"traj_0"` – empty (zero rows/subjects)
#'   \item `"traj_1"` – single trajectory / subject
#'   \item `"traj_m"` – multiple trajectories / subjects
#' }
#'
#' Metadata is attached as attributes `data_info` and `status`.
#'
#' @param data A data frame or tibble with longitudinal data
#' @param key_vars Named list defining column roles  
#'   Default: `list(id_col = "id", time_col = "time", y_col = "y")`
#' @param tfilter Function or `NULL` (default: >= non-NA `y` per subject)
#'
#' @return Object of class `"traj"` + one of `"traj_0"`, `"traj_1"`, `"traj_m"`
#'   with attributes `data_info` (original metadata) and `status` ("empty", "one", "multiple")
#'
#' @examples
#' \dontrun{
#' # ── Basic usage (multiple subjects) ──────────────────────────────────────────
#' traj_m <- create_traj(example_data)
#' 
#' class(traj_m)                        # "traj_m" "traj" ...
#' attr(traj_m, "status")               # "multiple"
#' attr(traj_m, "data_info")$data_name  # "example_data"
#' 
#' 
#' # ── Single-subject input → different class ────────────────────────────────
#' one_id <- unique(example_data$id)[1]
#' one_subject <- example_data |> dplyr::filter(id == one_id)
#' 
#' traj_1 <- create_traj(one_subject)
#' class(traj_1)                        # "traj_1" "traj" ...
#' attr(traj_1, "status")               # "one"
#' 
#' 
#' # ── Explicit key_vars (when column names differ) ────────────────────────────
#' traj_explicit <- create_traj(
#'   data = example_data,
#'   key_vars = list(
#'     id_col   = "id",
#'     time_col = "time",
#'     y_col    = "y"
#'   )
#' )
#' identical(traj_explicit, traj_m)     # TRUE when names match
#' 
#' 
#' # --- Custom tfilter: keep subjects with max(y) >= 100 ──────────────────────────
#' traj_high <- create_traj(
#'   example_data,
#'   tfilter = function(d) {
#'     d |>
#'       dplyr::group_by(id) |>
#'       dplyr::summarise(max_y = max(y, na.rm = TRUE)) |>
#'       dplyr::mutate(keep = max_y >= 100) |>
#'       dplyr::right_join(d, by = "id") |>
#'       dplyr::pull(keep)
#'   }
#' )
#' attr(traj_high, "status")            # "multiple" or "one"
#' 
#' 
#' # ── Invalid input (caught early) ─────────────────────────────────────────────
#' # Duplicate source column → error
#' # create_traj(
#' #   example_data,
#' #   key_vars = list(id_col = "id", time_col = "id", y_col = "y")
#' # )
#' # → Error: Duplicate column names in key_vars
#' }
#' 
#' @export
create_traj <- function(
    data,
    key_vars = list(id_col = "id", time_col = "time", y_col = "y"),
    tfilter = NULL
) {

  # ── Input validation ───────────────────────────────────────────────────────
  stopifnot(is.data.frame(data))

  id_col   <- key_vars$id_col
  time_col <- key_vars$time_col
  y_col    <- key_vars$y_col

  src_cols <- c(id_col, time_col, y_col)
  if (anyDuplicated(src_cols)) {
    stop("Duplicate column names in key_vars")
  }

  if (!all(src_cols %in% names(data))) {
    stop("Missing columns in data")
  }

  # ── Capture original data name ─────────────────────────────────────────────
  data_name <- deparse(substitute(data))

  # ── Original data summary (before filtering) ───────────────────────────────
  orig_n_rows     <- nrow(data)
  orig_n_cols     <- ncol(data)
  orig_n_subjects <- length(unique(data[[id_col]]))

  # ── Select key columns + drop rows with missing keys ───────────────────────
  valid_data <- data |>
    dplyr::select(dplyr::all_of(src_cols)) |>
    dplyr::filter(
      !is.na(.data[[id_col]]),
      !is.na(.data[[time_col]]),
      !is.na(.data[[y_col]])
    )

  # ── Default tfilter: at least 5 non-missing y per subject ───────────────────
  default_tfilter <- function(d) {
    d |>
      dplyr::group_by(.data[[id_col]]) |>
      dplyr::summarise(n_valid = sum(!is.na(.data[[y_col]]))) |>
      dplyr::mutate(keep = n_valid >= 5L) |>
      dplyr::right_join(d, by = id_col) |>
      dplyr::pull(keep)
  }

  keep_rows <- if (is.null(tfilter)) default_tfilter(valid_data) else tfilter(valid_data)

  filtered_data <- valid_data[keep_rows, , drop = FALSE]

  # ── Standardize column names ───────────────────────────────────────────────
  out <- filtered_data |>
    dplyr::rename_with(
      ~ c("id", "time", "y"),
      .cols = dplyr::all_of(c(id_col, time_col, y_col))
    )

  # ── Determine status and class suffix ──────────────────────────────────────
  n_final <- dplyr::n_distinct(out$id)

  status <- if (n_final == 0L) "empty" else if (n_final == 1L) "one" else "multiple"

  class_suffix <- switch(
    status,
    "empty"    = "traj_0",
    "one"      = "traj_1",
    "multiple" = "traj_m"
  )

  # ── Set class ──────────────────────────────────────────────────────────────
  class(out) <- c(class_suffix, "traj", class(out))

  # ── Attach attributes ──────────────────────────────────────────────────────
  attr(out, "data_info") <- list(
    data_name   = data_name,
    key_vars    = key_vars,
    n_rows      = orig_n_rows,
    n_cols      = orig_n_cols,
    n_subjects  = orig_n_subjects
  )

  attr(out, "status") <- status

  out
}
