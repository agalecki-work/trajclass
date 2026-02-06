### Execute this script From fresh R session and check for errrs, in a log file if any

# Aim: Create traj1_set object and save it in an external file for later use
rm(list = ls())
getwd()
packageVersion("trajclass")
library(conflicted)
# library(dplyr)
# library(tidyr)   
library(tidyverse)
library(trajclass)
library(segmented)
library(parameters)
#  conflicts()
prefer_tidyverbs()
message("... Script step1.R executed")


dim(example_data) # [1] 201   4 

## Use create_project_setup() to generate prj_setup object 

prj_setup <- 
  create_project_setup (data = example_data, 
                        keys = c("ID", "time", "egfr"),
                        nobs_persub = 3   # >= 15 recommended
) 

names(traj_df <- create_traj_data(prj_setup)) # traj_df with original key var names, e.g  "ID"   "time" "egfr"
class(traj_df) #  "traj_data"  "tbl_df"     "tbl"        "data.frame"


# --- create_traj1_set

traj1_set <- create_traj1_set(prj_setup)
typeof(traj1_set)
class(traj1_set)

length(traj1_set) # Number of subjects with sufficient number of valid rows of data
names(traj1_set)

# --- OUTPUT: create_traj1_set and Prj_info objects stored in external file

traj1_set <- create_traj1_set(prj_setup)
Prj_info <- prj_setup
Prj_info$data <- NULL # remove raw data
Prj_info$label = c("example_data [201x4]") # add project specific label
names(Prj_info)
class(traj1_set)
save(Prj_info, traj1_set, file = "./step1_objects.rda")  # saved in current working directory (you may choose different directory)
message("... step1.R script execution completed ====")
message("... Prj_info and traj1_set objects were saved in external file")
message("... Use ls() command to find out what objects were created by this script")


