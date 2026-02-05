#' Select one model fit from a traj1_fit object
#'
#' @param x An object of class `"traj1_fit"`
#' @param idx Integer (1, 2, 3) or `NA`:
#'   - 1 = linear
#'   - 2 = quadratic
#'   - 3 = hockey-stick
#'   - NA = return empty object
#'
#' @return A list with class vector starting with `"lin"`, `"quad"`, or `"hstick"`,
#'   followed by `"traj1_fit"` and `"list"`. Retains original attributes.
#' @export
select_traj1_fit <- function(x, idx = 1L) {
  
  if (!inherits(x, "traj1_fit")) {
    stop("`x` must be an object of class 'traj1_fit'", call. = FALSE)
  }
  
  if (length(idx) != 1L || (!is.na(idx) && !idx %in% 1:3)) {
    stop("`idx` must be 1, 2, 3, or NA", call. = FALSE)
  }
  
  if (is.na(idx)) {
    res <- list(fit = NULL, fit_info = NULL)
    class(res) <- c("traj1_fit_single", "traj1_fit", "list")
    attr(res, "message") <- "No model selected (idx = NA)"
    return(res)
  }
  
  model_names <- c("linear", "quadratic", "hockey_stick")
  selected_name <- model_names[idx]
  
  model_fit <- x[[1]][[selected_name]]
  if(is.null(model_fit)) return(NULL)
  
  attrs_all <- attributes(x)
  fit_info  <-  attrs_all$attrs_list$fit_info
  
  fit_info_row <- fit_info |>
    dplyr::filter(model == selected_name) |>
    dplyr::slice(1)
  
  if (nrow(fit_info_row) == 0) {
    fit_info_row <- tibble::tibble(
      model   = selected_name,
      status  = "not found",
      message = "No fit_info row for this model",
      valid   = FALSE
    )
  }
  
  res <- model_fit
  
  attr(res, "fit_info") <- fit_info_row
  
  
  # Preserve original attributes (except names/class/row.names)
  attrs <- attributes(x)
  attrs_keep <- attrs[!names(attrs) %in% c("names", "class", "row.names")]
  attributes(res) <- c(attributes(res), attrs_keep)
  
  # Assign class vector: child → parent → list
  
  class(res) <- c(class(model_fit), "traj1_fit")
  
  res
}

