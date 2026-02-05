### # Execute this script From fresh R session and check for errors, if any

# Examine data for one subject selected from `traj1_set object created in Step 1
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
packageVersion("trajclass")

load(file = "./traj1_set_object.rda")  # Load `traj1_set` object from external file created by script stored in step1.R 
typeof(traj1_set)
length(traj1_set) 
subject_names <- names(traj1_set) # Select one subject from this list
print(head(subject_names))

.traj_id = "1_30"   # Corresponds to subject name

# extract object `traj1` from `traj1_set` object with data for one subject and examine it
traj1 <-  traj1_set[[.traj_id]]  # one subject selected
typeof(traj1)
class(traj1)
glimpse(traj1)
plot(traj1$y ~ traj1$time)

# fit 3 models: linear, quadratic, hockey_stick using data from one subject stored in traj1 object
fit1 <- traj1_fit(traj1)
class(fit1)
# my_glimpse(fit1)   # Error method not defined for traj1_fit class
my_tidy(fit1)
# my_augment(fit1)   # Error method not defined for traj1_fit class

# Extract linear model fit identified by idx = 1
fit_s1 <- select_traj1_fit(fit1, idx =1)
class(fit_s1)
gg <- my_glimpse(fit_s1)
glimpse(gg)             # Vertical presentation
my_tidy(fit_s1)
my_augment(fit_s1)

# Extract quadratic model fit identified by idx =2
fit_s2 <- select_traj1_fit(fit1, idx = 2)
class(fit_s2)
gg <- my_glimpse(fit_s2)
glimpse(gg)
my_tidy(fit_s2)
my_augment(fit_s2)

# Extract hockey_stick model fit identified by idx =3
fit_s3 <- select_traj1_fit(fit1, idx = 3)
class(fit_s3)
gg <- my_glimpse(fit_s3)
glimpse(gg)
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
idx_best <- best_model_traj1(fit1,  return = "index")
cat("idx_best =", idx_best, "\n")
fit_best <- select_traj1_fit(fit1, idx = idx_best)
gg <- my_glimpse(fit_best)
glimpse(gg)

