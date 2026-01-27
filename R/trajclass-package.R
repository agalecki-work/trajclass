#' trajclass: Trajectory Representation and Manipulation
#'
#' @description
#' The **trajclass** package provides a consistent class system for longitudinal 
#' trajectory / time series data in long format.
#'
#' Current features:
#' \itemize{
#'   \item Creation of standardized trajectory objects via `create_traj()`
#'   \item Classes: `"traj"` (base) + one of:
#'     \itemize{
#'       \item `"traj_0"` – empty (zero rows/subjects)
#'       \item `"traj_1"` – single trajectory / subject
#'       \item `"traj_m"` – multiple trajectories / subjects
#'     }
#'   \item Subsetting via generic `subset_traj()` with methods for each subclass
#'   \item Attached metadata in attributes `data_info` and `status`
#' }
#'
#' Future versions will add classification of trajectories into patterns 
#' (linear, quadratic, hockey-stick).
#'
#' @docType package
#' @name trajclass
"_PACKAGE"

