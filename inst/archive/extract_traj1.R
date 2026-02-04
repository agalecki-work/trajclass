#' Extract single trajectory from a traj object
#'
#' @param x An object of class `"traj"` (or subclass)
#' @param id Character scalar. ID of the subject to extract.  
#'   If `NULL` (default), returns an empty `"traj"` object.
#'
#' @return A new object with class `"traj1"` (or `"traj_0"` if empty),  
#'   containing only one subject (or zero). Attributes `data_info` and `status` are updated.
#' @examples
#' \dontrun{
#' # ── Example 1: Extract one specific subject ──────────────────────────────────
#' keys <- c(id_col = "ID", time_col = "time", y_col = "egfr")
#' traj_all <- traj_create(example_data, key_vars = keys)
#' 
#' # Pick one ID from the full object
#' one_id <- unique(traj_all$id)[1]   # e.g. "1_10"
#' 
#' traj_single <- extract_traj1(traj_all, id = one_id)
#' 
#' # Output: single-subject traj1 object
#' class(traj_single)                 # "traj1" "traj" ...
#' nrow(traj_single)                  # number of rows for that ID only
#' unique(traj_single$id)             # only the requested ID
#' 
#' 
#' # ── Example 2: Extract ALL subjects into a named list of traj1 objects ───────
#' keys <- c(id_col = "ID", time_col = "time", y_col = "egfr")
#' traj_all <- traj_create(example_data, key_vars = keys)
#' 
#' ids <- traj_all |> distinct(id) |> pull(id)
#' 
#' # Create named list: each element is a single-subject traj1 object
#' traj1_list <- setNames(
#'   lapply(ids, function(one_id) {
#'     extract_traj1(traj_all, id = one_id)
#'   }),
#'   ids
#' )
#' 
#' # Output structure
#' length(traj1_list)                  # number of subjects / list elements
#' names(traj1_list)                   # the IDs (e.g. "1_10", "1_11", ...)
#' class(traj1_list[[1]])              # "traj1" "traj" ...
#' nrow(traj1_list[[1]])               # Number of rows for that specific ID only
#' 
#' # Quick access example
#' traj1_list[["1_10"]]                # traj1 object for ID "1_10"
#' }
#' @export
extract_traj1 <- function(x, id = NULL) {
  
  # ── Input validation ────────────────────────────────────────────────────────
  if (!inherits(x, "traj")) {
    stop("Input must be a traj object")
  }
  
  if (!is.null(id) && !is.character(id)) {
    stop("'id' must be a character scalar or NULL")
  }
  
  info <- attr(x, "data_info")
  if (is.null(info) || !is.list(info)) {
    stop("Input object is missing 'data_info' attribute")
  }
  
  # ── If id = NULL → return empty object ──────────────────────────────────────
  if (is.null(id)) {
    out <- x[0, , drop = FALSE]
    class(out) <- c("traj_0", "traj", class(out))
    attr(out, "data_info") <- info
    attr(out, "status") <- "empty"
    message("id = NULL → returning empty traj object")
    return(out)
  }
  
  # ── Subset to the selected ID (using standardized "id" column) ──────────────
  out <- x |>
    dplyr::filter(id == !!id)   # ← !!id injects the string value of id
  
  # ── Update status and class ────────────────────────────────────────────────
  n_final <- dplyr::n_distinct(out$id)
  
  if (n_final == 0) {
    class(out) <- c("traj_0", "traj", class(out))
    attr(out, "status") <- "empty"
    warning("No matching subject found → returning empty traj object")
  } else if (n_final == 1) {
    class(out) <- c("traj1", "traj", class(out))
    attr(out, "status") <- "one"
  } else {
    stop("More than one subject after filtering — this should not happen in extract_traj1")
  }
  
  # ── Update data_info attribute (reflect subset) ────────────────────────────
  new_info <- info
  new_info$n_rows     <- nrow(out)
  new_info$n_subjects <- n_final
  
  attr(out, "data_info") <- new_info
  
  out
}
