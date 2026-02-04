#' Tidy augment tibble forsingle-model objects
#'
#' @param x a single fitted model extracted from traj_fit object
#' @param ci Confidence level for intervals (default 0.95)
#' @param ... Passed to `parameters::model_parameters()`
#'
#' @return A tibble with model coefficients (one row per term)
#' @export
my_augment <- function(x, se_fit = TRUE, ...) {
  class1 <- class(x)[1]
  if (class1 == "traj1_fit") {
    stop("my_augment method not defined for class 'traj1_fit'", call. = FALSE)
  }

  UseMethod("my_augment")
}





#' Augment observed data with model predictions and diagnostics
#'
#' @param x A fitted model object returned by `select_traj1_fit()`  
#'   (class `"traj1_fit_single"`, inheriting from `"lm"` or `"segmented"`)
#' @param se_fit Logical. Include standard error of fitted values? (default: TRUE)
#' @param ... Passed to `predict.lm()` or `predict.segmented()`
#'
#' @return A tibble with original data + `.fitted`, `.resid`, `.se_fit` (if requested),
#'   and diagnostics (`.hat`, `.sigma`, `.cooksd`, `.std.resid`) when available.
#' @export
my_augment.lm <- function(x, se_fit = TRUE, ...) {
  
  # ── Validation ───────────────────────────────────────────────────────────────

  if (!inherits(x, c("lm", "segmented"))) {
    stop("x must inherit from 'lm' or 'segmented'", call. = FALSE)
  }
  
  # ── Extract original data ────────────────────────────────────────────────────
  attrs <- attributes(x)
  raw_df <- attrs$attrs_list$df
  
  if (is.null(raw_df)) {
    stop("Original data not found in attributes$attrs_list$df", call. = FALSE)
  }
  
  raw_df <- as_tibble(raw_df)
  
  # ── Predictions ──────────────────────────────────────────────────────────────
  pred <- predict(x, se.fit = se_fit, ...)
  
  if (se_fit) {
    fitted    <- pred$fit
    se_fit_vec <- pred$se.fit
  } else {
    fitted     <- pred
    se_fit_vec <- NULL
  }
  
  resid_vec <- residuals(x)
  
  # ── Diagnostics (only for lm, skip for segmented) ────────────────────────────
  hat_vec   <- NULL
  sigma_vec <- NULL
  cooksd    <- NULL
  std_resid <- resid_vec / sigma(x)   # always computable
  
  if (inherits(x, "lm") && !inherits(x, "segmented")) {
    infl <- tryCatch(influence.measures(x), error = function(e) NULL)
    
    if (!is.null(infl)) {
      hat_vec   <- infl$hat
      sigma_vec <- infl$sigma
      cooksd    <- infl$cooksd
    }
  }
  
  # ── Build augmented tibble ───────────────────────────────────────────────────
  augmented <- raw_df |>
    dplyr::mutate(
      .fitted    = fitted,
      .resid     = resid_vec,
      .std.resid = std_resid
    )
  
  if (se_fit) {
    augmented <- augmented |> dplyr::mutate(.se_fit = se_fit_vec)
  }
  
  if (!is.null(hat_vec)) {
    augmented <- augmented |> dplyr::mutate(
      .hat    = hat_vec,
      .sigma  = sigma_vec,
      .cooksd = cooksd
    )
  }
  
  # Move augmented columns to the end
  augmented <- augmented |>
    dplyr::relocate(starts_with("."), .after = last_col())
  
  out <- augmented
  attr(out, "fit_info") <- attr(x, "fit_info")
  out
}


