#' Tidy glimpse tibble for single-model objects
#'
#' @param x a single fitted model extracted from traj_fit object
#' @param ... Passed to `parameters::model_parameters()`
#'
#' @return A tibble with one row
#' @export
my_glimpse <- function(x, ...) {
  class1 <- class(x)[1]
  if (class1 == "traj1_fit") {
    stop("my_glimpse method not defined for class 'traj1_fit'", call. = FALSE)
  }

  UseMethod("my_glimpse")
}



#' Glimpse (one-row summary) for lm / segmented models derived from traj1_fit
#'
#' @param x A fitted model object inheriting from `"lm"` (possibly with `"traj1_fit"` ancestry)
#' @param ... Passed to other extractors (not currently used)
#'
#' @return A one-row tibble with key model summary statistics
#' @export
#' 
my_glimpse.lm <- function(x, ...) {
  
  # ── Validation ───────────────────────────────────────────────────────────────
  
  if (!inherits(x, "lm")) {
    stop("x must inherit from 'lm'", call. = FALSE)
  }
  
  # ── Extract traj1_id and model name ────────────────────────────────────────
  attrs_all <- attributes(x)
  
  traj1_id_val <- if (!is.null(attrs_all$attrs_list) && 
                     !is.null(attrs_all$attrs_list$traj1_id)) {
    attrs_all$attrs_list$traj1_id
  } else {
    NA_character_
  }
  
  fit_info <- attr(x, "fit_info")
  model_name <- if (!is.null(fit_info) && nrow(fit_info) > 0) {
    fit_info$model[1]
  } else {
    if (inherits(x, "segmented")) "hockey_stick" else class(x)[1]
  }
  
  # ── Extract model performance metrics ──────────────────────────────────────
  perf <- tryCatch(performance::model_performance(x), error = function(e) NULL)
  
  # Fallback to summary.lm if performance fails
  summ <- summary(x)
  
  res <- tibble::tibble(
    .traj_id      = traj1_id_val,
    model         = model_name,
    status        = fit_info$status %||% "ok",
    valid         = fit_info$valid  %||% TRUE,
    message       = fit_info$message %||% "OK",
    n_obs         = nobs(x),
    df_resid      = df.residual(x),
    r_squared     = if (!is.null(perf)) perf$R2 %||% NA_real_ else summ$r.squared %||% NA_real_,
    adj_r_squared = if (!is.null(perf)) perf$R2_adj %||% NA_real_ else summ$adj.r.squared %||% NA_real_,
    sigma         = sigma(x),
    aic           = AIC(x),
    bic           = BIC(x),                    # ← added BIC
    breakpoint_est = if (inherits(x, "segmented")) x$psi[1, "Est."] else NA_real_,
    breakpoint_se  = if (inherits(x, "segmented")) x$psi[1, "St.Err"] else NA_real_
  )
  
  res
}



