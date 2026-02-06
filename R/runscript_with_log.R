#' runScript_with_log
#'
#' @export
runScript_with_log <- function(script_name,  # "step0.R" 
                            scriptsDir = NULL, # if NULL 
                            log_file = NULL, 
                            split = TRUE, 
                            max_deparse = 10000) {
  script_path <- if (is.null(scriptsDir)){
    pkgDir <- find.package("trajclass")
    scriptsDir <- system.file("scripts", package = "trajclass")
    file.path(scriptsDir, script_name)
  } else {
    file.path(scriptsDir, script_name)
  }
  
  if (is.null(log_file)) {
    # Auto-name the log file (same name as script name but .log extension)
    log_file <- sub("\\.[^.]+$", ".log", script_name)
  }
  
  message("Sourcing: ", script_name)
  message("From:", script_path)
  message("Logging to: ", log_file)
  message("   (console shows only warnings, messages & errors)")
  
  withr::local_options(max.deparse.length = max_deparse)
  
  # Normal output (echo + print + cat) → file only
  withr::local_output_sink(log_file, append = TRUE, split = FALSE)
  
  # message sink untouched → warnings/errors stay visible on console
  res <- tryCatch(
    source(script_path, echo = TRUE, max.deparse.length = max_deparse),
    error = function(e) {
      message("Script stopped with error (see console above)")
      invisible(e)
    },
    warning = function(w) {
      message("Warning occurred (see console above)")
      invokeRestart("muffleWarning")
    }
  )
}