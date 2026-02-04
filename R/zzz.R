.onLoad <- function(libname, pkgname) {
  
  op <- options()
  
  op.trajclass <- list(
    trajclass.davies_p_value = 0.20,
    trajclass.display_seg    = FALSE
  )
  
  # Only set options that the user has not already set
  toset <- !(names(op.trajclass) %in% names(op))
  if (any(toset)) {
    options(op.trajclass[toset])
  }
  invisible()
}