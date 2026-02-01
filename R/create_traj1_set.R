#' Create standardized trajectory object
#'
#' @description
#' Prepares a clean tibble with standardized columns `id`, `time`, `y` 
#' from a longitudinal dataset (stored in `traj` object. Applies the specified filter (default: ≥5 non-NA `y` values per subject).
#'
#' The returned object always has base class `"traj"`, and adds subclass `"traj1"`
#' if exactly one unique subject remains after filtering.
#'
#' Metadata is attached as attribute `"data_info"` (a tibble).
#'
#' @param data A data frame or tibble with longitudinal data
#' @param key_vars **Required.** Named character vector of length 3 defining the column names  
#'   Must have names `"id_col"`, `"time_col"`, `"y_col"`  
#'   Example: `c(id_col = "ID", time_col = "time", y_col = "egfr")`
#' @param nobs_persub Minimum number of non-missing observations per subject  
#'   (default = 5). Subjects with fewer valid rows are excluded.
#'
#' @return A tibble with standardized columns `"id"`, `"time"`, `"y"` and class:
#'   - `"traj1" "traj"` — exactly one subject
#'   - `"traj"` — multiple subjects or empty
#'   
#'   Attribute `"data_info"` is a tibble containing two rows
#' @export
#'
#' @examples
#' \dontrun{
#' orig_keys <- getOption("trajclass.key_vars", default = NULL)
#' traj_df <- create_traj_data(example_data, key_vars = orig_keys)
#' traj1_set <- create_traj1_set(traj_df)
#' class(traj1_set)
#' length(traj1_set)                  # number of selected subjects
#'  head(names(traj1_set))             # "1_10" "1_11" "1_15" ... 
     
#' traj1 <-  traj1_set[["1_15"]]                    
#' attrx <- attributes(traj1)
#' names(attrx)  #   "names"  "row.names"  "class"      "traj1_info"
#' attrx$names   #  "id"   "time" "y" 
#' head(attrx$row.names)   # [1] 1 2 3 4 5 6
# names(attrx$traj1_info) # "traj_id"       "orig_key_vars" "nobs_persub" 
#' }
#' @export
create_traj1_set <- function(
    data,
    nobs_persub = getOption("trajclass.nobs_persub",  default =  10),
    .msg = TRUE
) {
  
  datain_info <- inspect_parent_data(.msg = .msg)   # tibble with one  row
 
  # ── 1. Input validation ─────────────────────────────────────────────────────
  if (!inherits(data, "traj_data")) stop("incorrect argument for create_traj_object function")
  out <- data
  names(out) <- c("id", "time", "y")

  # ── 3. Exclude subjects with insufficient non-missing y values ─────────────
  if (nobs_persub > 0) {
    keep_subjects <- out |>
      dplyr::group_by(id) |>
      dplyr::summarise(n_valid = sum(!is.na(y))) |>
      dplyr::filter(n_valid >= nobs_persub) |>
      dplyr::pull(id)
    
    out <- out |> dplyr::filter(id %in% keep_subjects)
  }

  # Create a list of tibbles
  attrx <- attributes(out)
  orig_key_vars <- attrx$info$orig_key_vars
  
  attr(out, "info") <- NULL
  
  subjects_list <- split(out, out$id)    # plain list of tibbles
 
  out <-lapply(subjects_list, function(x){ 
  
     attr(x, "traj1_info") <- list(
       traj_id = x$id[1],
       orig_key_vars = unname(orig_key_vars),
       nobs_persub = nobs_persub
     )
     class(x) <- c("traj1", class(x))
     x
  })

     attr(out, "datain_info") <- datain_info
  class(out) <- c("traj1_set", class(data))
     out
  }


