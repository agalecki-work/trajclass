#' Tidy coefficients from traj1_fit or single-model objects
#'
#' @param x A `traj1_fit` object or a single fitted model extracted from it
#' @param ci Confidence level for intervals (default 0.95)
#' @param ... Passed to `parameters::model_parameters()`
#'
#' @return A tibble with model coefficients (one row per term)
#' @export
my_tidy <- function(x, ci = 0.95, ...) {
  UseMethod("my_tidy")
}

#' @export
my_tidy.traj1_fit <- function(x, ci = 0.95, ...) {
  
  # ── Full traj1_fit object ──────────────────────────────────────────────────
  # We extract and tidy all three models
  
  fits <- x$fit
  info <- x$fit_info
  
  # Helper to tidy one model
  tidy_one <- function(model_obj, model_n) {
    if (is.null(model_obj)) return(NULL)
    
    params <- parameters::model_parameters(model_obj, ci = ci, verbose = FALSE, ...)
    
    params <- params |>
      dplyr::rename_with(
        ~ dplyr::case_when(
          . == "Parameter"   ~ "term",
          . == "Coefficient" ~ "estimate",
          . == "SE"          ~ "std.error",
          . %in% c("t", "z", "Statistic", "statistic") ~ "statistic",
          . == "p"           ~ "p.value",
          . == "CI_low"      ~ "conf.low",
          . == "CI_high"     ~ "conf.high",
          TRUE               ~ .
        ),
        .cols = everything()
      ) |>
      dplyr::mutate(model = model_n, .before = 1) |>
      dplyr::select(model, term, estimate, std.error, statistic, p.value,
                    conf.low, conf.high, dplyr::everything())
    
    if (inherits(model_obj, "segmented")) {
      psi <- model_obj$psi
      psi_rows <- purrr::map_dfr(seq_len(nrow(psi)), ~ {
        tibble::tibble(
          model     = model_n,
          term      = if (nrow(psi) > 1) paste0("breakpoint_", .x) else "breakpoint",
          estimate  = psi[.x, "Est."],
          std.error = psi[.x, "St.Err"],
          statistic = NA_real_,
          p.value   = NA_real_,
          conf.low  = NA_real_,
          conf.high = NA_real_
        )
      })
      params <- dplyr::bind_rows(params, psi_rows)
    }
    
    params
  }
  
  # Tidy all three models
  tidy_list <- list(
    linear       = tidy_one(fits$linear,       "linear"),
    quadratic    = tidy_one(fits$quadratic,    "quadratic"),
    hockey_stick = tidy_one(fits$hockey_stick, "hockey_stick")
  )
  
  # Stack them into one tibble
  dplyr::bind_rows(tidy_list, .id = "model") |>
    dplyr::arrange(model, term)
}

#' Tidy coefficients from traj1_fit or single-model objects
#'
#' @param x A `traj1_fit` object or a single fitted model extracted from it
#' @param ci Confidence level for intervals (default 0.95)
#' @param ... Passed to `parameters::model_parameters()`
#'
#' @return A tibble with model coefficients (one row per term)
#' @export
my_tidy <- function(x, ci = 0.95, ...) {
  UseMethod("my_tidy")
}

#' @export
my_tidy.traj1_fit <- function(x, ci = 0.95, ...) {
  
  # ── Full traj1_fit object ──────────────────────────────────────────────────
  # We extract and tidy all three models
  
  fits <- x$fit
  info <- x$fit_info
  
  # Helper to tidy one model
  tidy_one <- function(model_obj, model_n) {
    if (is.null(model_obj)) return(NULL)
    
    params <- parameters::model_parameters(model_obj, ci = ci, verbose = FALSE, ...)
    
    params <- params |>
      dplyr::rename_with(
        ~ dplyr::case_when(
          . == "Parameter"   ~ "term",
          . == "Coefficient" ~ "estimate",
          . == "SE"          ~ "std.error",
          . %in% c("t", "z", "Statistic", "statistic") ~ "statistic",
          . == "p"           ~ "p.value",
          . == "CI_low"      ~ "conf.low",
          . == "CI_high"     ~ "conf.high",
          TRUE               ~ .
        ),
        .cols = everything()
      ) |>
      dplyr::mutate(model = model_n, .before = 1) |>
      dplyr::select(model, term, estimate, std.error, statistic, p.value,
                    conf.low, conf.high, dplyr::everything())
    
    if (inherits(model_obj, "segmented")) {
      psi <- model_obj$psi
      psi_rows <- purrr::map_dfr(seq_len(nrow(psi)), ~ {
        tibble::tibble(
          model     = model_n,
          term      = if (nrow(psi) > 1) paste0("breakpoint_", .x) else "breakpoint",
          estimate  = psi[.x, "Est."],
          std.error = psi[.x, "St.Err"],
          statistic = NA_real_,
          p.value   = NA_real_,
          conf.low  = NA_real_,
          conf.high = NA_real_
        )
      })
      params <- dplyr::bind_rows(params, psi_rows)
    }
    
    params
  }
  
  # Tidy all three models
  tidy_list <- list(
    linear       = tidy_one(fits$linear,       "linear"),
    quadratic    = tidy_one(fits$quadratic,    "quadratic"),
    hockey_stick = tidy_one(fits$hockey_stick, "hockey_stick")
  )
  
  # Stack them into one tibble
  dplyr::bind_rows(tidy_list, .id = "model") |>
    dplyr::arrange(model, term)
}

#' Tidy coefficients from traj1_fit or single-model objects
#'
#' @param x A `traj1_fit` object or a single fitted model extracted from it
#' @param ci Confidence level for intervals (default 0.95)
#' @param ... Passed to `parameters::model_parameters()`
#'
#' @return A tibble with model coefficients (one row per term)
#' @export
my_tidy <- function(x, ci = 0.95, ...) {
  UseMethod("my_tidy")
}

#' @export
my_tidy.traj1_fit <- function(x, ci = 0.95, ...) {
  
  # ── Full traj1_fit object ──────────────────────────────────────────────────
  # We extract and tidy all three models
  
  fits <- x$fit
  info <- x$fit_info
  
  # Helper to tidy one model
  tidy_one <- function(model_obj, model_n) {
    if (is.null(model_obj)) return(NULL)
    
    params <- parameters::model_parameters(model_obj, ci = ci, verbose = FALSE, ...)
    
    params <- params |>
      dplyr::rename_with(
        ~ dplyr::case_when(
          . == "Parameter"   ~ "term",
          . == "Coefficient" ~ "estimate",
          . == "SE"          ~ "std.error",
          . %in% c("t", "z", "Statistic", "statistic") ~ "statistic",
          . == "p"           ~ "p.value",
          . == "CI_low"      ~ "conf.low",
          . == "CI_high"     ~ "conf.high",
          TRUE               ~ .
        ),
        .cols = everything()
      ) |>
      dplyr::mutate(model = model_n, .before = 1) |>
      dplyr::select(model, term, estimate, std.error, statistic, p.value,
                    conf.low, conf.high, dplyr::everything())
    
    if (inherits(model_obj, "segmented")) {
      psi <- model_obj$psi
      psi_rows <- purrr::map_dfr(seq_len(nrow(psi)), ~ {
        tibble::tibble(
          model     = model_n,
          term      = if (nrow(psi) > 1) paste0("breakpoint_", .x) else "breakpoint",
          estimate  = psi[.x, "Est."],
          std.error = psi[.x, "St.Err"],
          statistic = NA_real_,
          p.value   = NA_real_,
          conf.low  = NA_real_,
          conf.high = NA_real_
        )
      })
      params <- dplyr::bind_rows(params, psi_rows)
    }
    
    params
  }
  
  # Tidy all three models
  tidy_list <- list(
    linear       = tidy_one(fits$linear,       "linear"),
    quadratic    = tidy_one(fits$quadratic,    "quadratic"),
    hockey_stick = tidy_one(fits$hockey_stick, "hockey_stick")
  )
  
  # Stack them into one tibble
  dplyr::bind_rows(tidy_list, .id = "model") |>
    dplyr::arrange(model, term)
}

#' @export
my_tidy.lm <- function(x, ci = 0.95, ...) {
  
  # ── Single-model case (fitx1, fitx2, fitx3) ────────────────────────────────
  
  attrs_all <- attributes(x)
  traj1_id_val <- attrs_all$attrs_list$traj1_id %||% NA_character_
  
  fit_info <- attr(x, "fit_info")
  model_name <- if (!is.null(fit_info) && nrow(fit_info) > 0) {
    fit_info$model[1]
  } else {
    if (inherits(x, "segmented")) "hockey_stick" else class(x)[1]
  }
  
  params <- parameters::model_parameters(x, ci = ci, verbose = FALSE, ...)
  
  params <- params |>
    dplyr::rename_with(
      ~ dplyr::case_when(
        . == "Parameter"   ~ "term",
        . == "Coefficient" ~ "estimate",
        . == "SE"          ~ "std.error",
        . %in% c("t", "z", "Statistic", "statistic") ~ "statistic",
        . == "p"           ~ "p.value",
        . == "CI_low"      ~ "conf.low",
        . == "CI_high"     ~ "conf.high",
        TRUE               ~ .
      ),
      .cols = everything()
    ) |>
    dplyr::mutate(
      .traj_id = traj1_id_val,
      model    = model_name,
      .before  = 1
    ) |>
    dplyr::select(.traj_id, model, term, estimate, std.error, statistic, p.value,
                  conf.low, conf.high, dplyr::everything())
  
  if (inherits(x, "segmented")) {
    psi <- x$psi
    psi_rows <- purrr::map_dfr(seq_len(nrow(psi)), ~ {
      tibble::tibble(
        .traj_id  = traj1_id_val,
        model     = model_name,
        term      = if (nrow(psi) > 1) paste0("breakpoint_", .x) else "breakpoint",
        estimate  = psi[.x, "Est."],
        std.error = psi[.x, "St.Err"],
        statistic = NA_real_,
        p.value   = NA_real_,
        conf.low  = NA_real_,
        conf.high = NA_real_
      )
    })
    params <- dplyr::bind_rows(params, psi_rows)
  }
  
  params
}

