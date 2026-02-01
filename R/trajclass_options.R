# ── Exported helper ────────────────────────────────────────────────────────

#' View current trajclass options (changed or all)
#'
#' @param all If `TRUE`, show all trajclass.* options. If `FALSE` (default),
#'   show only options that differ from the package defaults.
#'
#' @return Named list of current options
#' @export
trajclass_options <- function(all = FALSE) {
  
  current <- options()[grepl("^trajclass\\.", names(options()))]
   # Original defaults (hard-coded so we can compare)
  if (!all) {
     defaults <- list(
      trajclass.key_vars           = c(id_col = "", time_col = "", y_co = ""),
      trajclass.nper_sub           = 10L,
      trajclass.davies_p_value     = 0.20
      )
    
    # Only keep entries that are different or were not set by the package
    changed <- current[!mapply(identical, current, defaults[names(current)])]
    
    if (length(changed) == 0) {
      message("No trajclass options have been changed from defaults.")
      return(invisible(list()))
    }
    
    return(changed)
  } 
  # Return all current trajclass.* options
  if (length(current) == 0) {
    message("No trajclass options are currently set.")
    return(invisible(list()))
  }
  
  current
}

