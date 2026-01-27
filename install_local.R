
# Make sure you have devtools (once)
# install.packages("devtools")

# Then, from any working directory:
# devtools::install("C:/ATG/github/trajclass")
# or using Windows-style backslashes (both usually work)

### devtools::install_deps()
# devtools::install("C:\\ATG\\github\\trajclass") # skips tests by default
setwd("C:/ATG/github/trajclass")

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
# ?check_long_structure
# ?generate_synthetic_data
?create_traj
?subset_traj

data(example_data)
colnames(example_data)

# ?check_long_structure
# ?extract_egfrtibble
res <- extract_egfrtibble(example_egfr_data)

# Quick inspection
class(res)                                 # "egfrtibble" + tibble classes
dim(res)                                   # rows × 3 (id, time, egfr)
attr(res, "origdata_info")$final_subjects  # usually "All"

# ?subset_egfrtibble

res <- extract_egfrtibble(example_egfr_data)

# Get all unique IDs from the original data
all_ids <- unique(example_egfr_data$id)




