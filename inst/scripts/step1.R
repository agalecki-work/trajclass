### Execute this script From fresh R session and check for errors, if any

# Aim: Create traj1_set object and save it in an external file for later use
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

# STEP 1: df returned with class `traj_data` inheriting from
dim(example_data) # [1] 201   4

## Use create_project_setup to generate project setup(s) 

example_project_setup <- 
  create_project_setup (data = example_data, 
                        keys = c("ID", "time", "egfr"),
                        nobs_persub = 3   # >= 15 recommended
) 

prj_setup <- example_project_setup # Select here one project setup from those defined above and save it in `prj_setup
names(traj_df <- create_traj_data(prj_setup)) # traj_df with original key var names, e.g  "ID"   "time" "egfr"
class(traj_df) #  "traj_data"  "tbl_df"     "tbl"        "data.frame"


# --- create_traj1_set


traj1_set <- create_traj1_set(prj_setup)
typeof(traj1_set)
class(traj1_set)

length(traj1_set) # Number of subjects with sufficient number of valid rows of data
names(traj1_set)

# --- create_traj1_set

traj1_set <- create_traj1_set(prj_setup)
class(traj1_set)
save(traj1_set, file = "./traj1_set_object.rda")  # saved in current working directory (you may choose different directory)


