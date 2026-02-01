.onLoad <- function(libname, pkgname) {
  
  op <- options()
  
  op.trajclass <- list(
    
    # Single named vector for the three key columns
    # → starts empty → user MUST set it before most functions work
    trajclass.key_vars = c(
      id_col   = "",     # subject / patient / group identifier
      time_col = "",     # time / visit / date column
      y_col    = ""      # outcome / response / measurement column
    ),
    
    # Minimum number of observations required per subject
    trajclass.nper_sub = 10L,
    
    # Davies test p-value threshold for considering a breakpoint meaningful
    trajclass.davies_p_value = 0.20
  )
  
  # Only set options that the user has not already set
  toset <- !(names(op.trajclass) %in% names(op))
  if (any(toset)) {
    options(op.trajclass[toset])
  }
  
  # Informative startup message
  packageStartupMessage(
    "trajclass ", utils::packageVersion("trajclass"), " loaded\n",
    "  • Key columns not yet configured (trajclass.key_vars is empty):\n",
    "      id   = ''\n",
    "      time = ''\n",
    "      y    = ''\n",
   "    → Set them before analysis, e.g.:\n",
    "      options(trajclass.key_vars = c(id_col = \"ID\", time_col = \"time\", y_col = \"egfr\"))\n",
    "    → This setting matches the structure of example_data shipped with the package\n")
  
  invisible()
}