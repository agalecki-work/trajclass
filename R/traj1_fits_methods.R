#' @export
print.traj1_fits <- function(x, ...) {
  cat("traj1_fits object for subject:", x$id, "\n")
  cat("Data source:", x$data_name, "\n")
  cat("Models fitted:\n")
  cat(" - Linear:   ", if (!is.null(x$linear)) "Yes" else "Failed", "\n")
  cat(" - Quadratic: ", if (!is.null(x$quadratic)) "Yes" else "Failed", "\n")
  cat(" - Segmented: ", if (!is.null(x$segmented)) "Yes" else "Failed", "\n")
  if (!is.null(x$segmented)) {
    cat("  Breakpoint:", round(x$segmented$psi[2,1], 2), "\n")
  }
  invisible(x)
}