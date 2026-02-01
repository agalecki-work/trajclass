
# Make sure you have devtools (once)
setwd("C:/ATG/github/trajclass")
# Delete man/ + other generated files
unlink(c("man", "NAMESPACE"), recursive = TRUE)

# Re-generate everything
devtools::document()
# devtools::load_all()
# devtools::document()
devtools::install() # skips tests by default

library(conflicted)
# library(dplyr)
# library(tidyr)     # ← this brings pivot_longer() and pivot_wider()
library(tidyverse)
library(trajclass)
library(segmented)
#  conflicts()
prefer_tidyverbs()

sort(getNamespaceExports("trajclass"))
ls("package:trajclass")

# ?prefer_tidyverbs
# ?example_data

# ?longdata_myinfo
# ?check_long_structure
# ?generate_synthetic_data
# ?example_data
# ????create_traj_all

trajclass_options()
options(trajclass.key_vars = c(id_col = "ID", time_col = "time", y_col = "egfr"))
trajclass_options() # → only shows options the user has modified, such as key_vars
orig_keys <- getOption("trajclass.key_vars", default = NULL )


# trajclass_options(all = TRUE) 
# → shows everything currently set (even if default)

# STEP 1: df returned with class `traj_data` inheriting from
# ?create_traj_data
 
traj_df <- create_traj_data(example_data, key_vars = orig_keys)


# ?create_traj1_set
traj1_set <- create_traj1_set(traj_df)
class(traj1_set)

traj1 <-  traj1_set[["1_30"]]


fit1 <- traj1_fit(traj1) 
# ?traj1_fit
names(fit1)
fit1$fit_info




