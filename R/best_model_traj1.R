#' Select the best model from a traj1_fit object using AIC or BIC
#'
#' @param x An object of class `traj1_fit` returned by `traj1_fit()`
#' @param criterion Character: `"aic"` (default) or `"bic"`
#' @param return Character: what to return when no valid model exists  
#'   `"string"` → returns a short string (default)  
#'   `"index"` → returns index (1 = linear, 2 = quadratic, 3 = hockey_stick) or NA  
#'   `"model_name"` → returns model name or "invalid"
#'
#' @return A string describing the best model (or "invalid"/"none"/etc.)
#' @export
best_model_traj1 <- function(x,
                             criterion = c("aic", "bic"),
                             return = c("string", "index", "model_name")) {
  
  criterion <- match.arg(criterion)
  return    <- match.arg(return)
  
  # Early exit if input is invalid
  if (!inherits(x, "traj1_fit")) {
    return(switch(return,
                  string      = "invalid input",
                  index       = NA_integer_,
                  model_name  = "invalid"))
  }
  
  # Extract fit_info and models
  attrs <- attributes(x)
  info <-  attrs$attrs_list$fit_info
  fits <- x$fit
  
  # Prepare tibble with AIC/BIC for valid models only
  model_summary <- info |>
    dplyr::filter(valid) |>
    dplyr::mutate(
      model_fit = dplyr::case_when(
        model == "linear"       ~ list(fits$linear),
        model == "quadratic"    ~ list(fits$quadratic),
        model == "hockey_stick"    ~ list(fits$hockey_stick),   # adjust if you renamed
        TRUE                    ~ list(NULL)
      ),
      ic_value = purrr::map_dbl(model_fit, ~ {
        if (is.null(.x)) NA_real_ else {
          if (criterion == "aic") AIC(.x) else BIC(.x)
        }
      }),
      model_label = dplyr::case_when(
        model == "linear"    ~ "linear",
        model == "quadratic" ~ "quadratic",
        model == "hockey_stick" ~ "hockey_stick",
        TRUE                 ~ "unknown"
      )
    ) |>
    dplyr::filter(!is.na(ic_value)) |>
    dplyr::arrange(ic_value)   # smaller is better
  
  # No valid models with computable IC
  if (nrow(model_summary) == 0) {
    return(switch(return,
                  string      = "invalid - no valid model with finite AIC/BIC",
                  index       = NA_integer_,
                  model_name  = "invalid"))
  }
  
  # Best model
  best_row <- model_summary |> dplyr::slice(1)
  
  best_name <- best_row$model_label
  best_ic   <- best_row$ic_value
  
  msg <- sprintf("%s (%.1f)", best_name, best_ic)
  
  switch(return,
         string      = msg,
         index       = which(c("linear", "quadratic", "hockey_stick") == best_name),
         model_name  = best_name)
}

