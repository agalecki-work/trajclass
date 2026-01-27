# Silence codetools / R CMD check warnings about ellipsis in tidy-eval contexts
utils::globalVariables("...")
# Silence R CMD check notes for non-standard-evaluation / pipeline variables
utils::globalVariables(c(".data", "id", "time", "n_valid", "keep"))


# ──────────────────────────────────────────────────────────────────────────────
# Your functions start here

#' Subset a traj object
#'
#' @description
#' Generic function for subsetting objects inheriting from `"traj"`.
#' Dispatches to specific methods based on the subclass (`traj_0`, `traj_1`, `traj_m`).
#'
#' @param x An object of class `"traj"` or subclass
#' @param ids Character vector of subject IDs to retain  
#'   `NULL` → select only the first subject (if any)
#'
#' @return Object of the same class family as `x` (or `"traj_0"` if empty)
#' @export
subset_traj <- function(x, ids = NULL) {
  UseMethod("subset_traj")
}


#' @export
#' @rdname subset_traj
subset_traj.traj_0 <- function(x, ids = NULL) {
  warning("Input is already an empty traj object → returning itself")
  x
}


#' @export
#' @rdname subset_traj
subset_traj.traj_1 <- function(x, ids = NULL) {
  info <- attr(x, "traj_info")
  current_id <- unique(x$id)

  if (is.null(ids)) {
    return(x)
  }

  if (!is.character(ids)) {
    stop("ids must be a character vector")
  }

  if (current_id %in% ids) {
    return(x)
  } else {
    out <- x[0, , drop = FALSE]
    class(out) <- c("traj_0", "traj", class(out))
    attr(out, "data_info") <- attr(x, "data_info")
    attr(out, "status") <- "empty"
    warning("Requested IDs do not match the single subject → empty object returned")
    return(out)
  }
}


#' @export
#' @rdname subset_traj
subset_traj.traj_m <- function(x, ids = NULL) {
  info <- attr(x, "data_info")
  status_old <- attr(x, "status")

  if (is.null(ids)) {
    first_id <- x$id[1]
    message("No ids provided → selecting first subject: ", first_id)
    ids <- first_id
  }

  if (!is.character(ids)) {
    stop("ids must be a character vector")
  }

  out <- x |>
    dplyr::filter(.data$id %in% ids)

  n_after <- dplyr::n_distinct(out$id)

  new_status <- if (n_after == 0L) "empty" else if (n_after == 1L) "one" else "multiple"

  class_suffix <- switch(
    new_status,
    "empty"    = "traj_0",
    "one"      = "traj_1",
    "traj_m"
  )

  class(out) <- c(class_suffix, "traj", class(out))

  attr(out, "data_info") <- info
  attr(out, "status") <- new_status

  if (n_after == 0L) {
    warning("No matching subjects → empty traj object returned")
  }

  out
}