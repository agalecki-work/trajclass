# Simple extractor helper (can be later improved / made more sophisticated)
extract_info <- function(info_list, key, default = NULL) {
  if (is.list(info_list) && key %in% names(info_list)) {
    info_list[[key]]
  } else {
    default
  }
}

