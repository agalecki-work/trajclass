### # Execute this script from fresh R session and check for errrs, if any


# Examine data for one subject selected from `traj1_set object created in Step 1
rm(list = ls())
getwd()

library(conflicted)
# library(dplyr)
# library(tidyr)   
library(tidyverse)
library(trajclass)
library(segmented)
library(parameters)

#  conflicts()
prefer_tidyverbs()
message("... Script step2.R is executed")

packageVersion("trajclass")
message("... Load `Prj_info and `traj1_set` objects from external file created by step1.R")
# Object traj1_set  created by step1.R")

load(file = "./step1_objects.rda", verbose = TRUE)
ls()   #  
typeof(traj1_set)
length(traj1_set) 
subject_names <- names(traj1_set) # Select one subject from this list
print(head(subject_names))

.traj_id = "1_30"   # Corresponds to subject name
message("... `.traj_d`: Contains id =", .traj_id, " for selected subject") 
message("...`traj1`: contains data for selected subject.")

# extract object `traj1` from `traj1_set` object with data for one subject and examine it
traj1 <-  traj1_set[[.traj_id]]  # one subject selected
typeof(traj1)
class(traj1)
glimpse(traj1)
plot(traj1$y ~ traj1$time)

message("... `fit1`: Contains fits of three models: linear, quadratic, hockey_stick fitted")
fit1 <- traj1_fit(traj1)
class(fit1)
# my_glimpse(fit1)   # Errr method not defined for traj1_fit class
my_tidy(fit1)
# my_augment(fit1)   # Errr method not defined for traj1_fit class

# Extract linear model fit identified by idx = 1
fit_s1 <- select_traj1_fit(fit1, idx =1)
class(fit_s1)
gg_lin <- my_glimpse(fit_s1)
glimpse(gg_lin)             # Vertical presentation
rm(gg_lin)
my_tidy(fit_s1)
my_augment(fit_s1)

# Extract quadratic model fit identified by idx =2
fit_s2 <- select_traj1_fit(fit1, idx = 2)
class(fit_s2)
gg_quad <- my_glimpse(fit_s2)
glimpse(gg_quad)
rm(gg_quad)
my_tidy(fit_s2)
my_augment(fit_s2)

# Extract hockey_stick model fit identified by idx =3
fit_s3 <- select_traj1_fit(fit1, idx = 3)
class(fit_s3)
gg_hkey_stick <- my_glimpse(fit_s3)
glimpse(gg_hkey_stick)
rm(gg_hkey_stick)
my_tidy(fit_s3)

# Plots with fitted lines for selected subject 
aug3 <- my_augment(fit_s3)
plot_augmented_traj(aug3, title = "Hockey-stick model fit") # Good fit
aug2 <- my_augment(fit_s2)
plot_augmented_traj(aug2, title = "Quadratic model fit") # Reasonable fit except time < 10
aug1 <- my_augment(fit_s1)
plot_augmented_traj(aug1, title = "Linear model fit") # Poor fit

# Find idx of the best model fit
best_model_traj1(fit1)  # msg
idx_best_s <- best_model_traj1(fit1,  return = "index")
cat("idx_best_s =", idx_best_s, "\n")
fit_best_s <- select_traj1_fit(fit1, idx = idx_best_s)
gg_best_s  <- my_glimpse(fit_best_s)
glimpse(gg_best_s)
rm(gg_best_s, aug1, aug2, aug3)  # They can be easily reconstructed, if needed

message("... step2.R script execution completed ")
message("... external files not created by this script")
message("... Use ls() command to find out what objects were created by this script")




