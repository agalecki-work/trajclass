# Examine  traj1_set object that data for all subjects with sufficient number of valid observations
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

load(file = "C:/temp/traj1_set_object.rda")
typeof(traj1_set)
length(traj1_set) 
subject_names <- names(traj1_set) # Select one subject from this list
print(head(subject_names))

# extract object `traj1` with data for one subject and examine it
traj1 <-  traj1_set[["1_30"]]  # one subject selected
typeof(traj1)
class(traj1)
glimpse(traj1)
plot(traj1$y ~ traj1$time)

# fit 3 models: linear, quadratic, hockey_stick using data from one subject stored in traj1 object
fit1 <- traj1_fit(traj1)
class(fit1)
my_glimpse(fit1)   # Error not defined
my_tidy(fit1)
my_augment(fit1) # Error not defined

# Extract linear model fit identified by idx = 1
fit_s1 <- select_traj1_fit(fit1, idx =1)
class(fit_s1)
gg <- my_glimpse(fit_s1)
glimpse(gg)
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
aug3 <- my_augment(fit_s3)
plot_augmented_traj(aug3, title = "Hockey-stick model fit") # Good fit
aug2 <- my_augment(fit_s2)
plot_augmented_traj(aug2, title = "Quadratic model fit") # Reasonable fit
aug1 <- my_augment(fit_s1)
plot_augmented_traj(aug1, title = "Linear model fit") # Poor fit



# Find idx of the best model
best_model_traj1(fit1)  # msg
idx_best <- best_model_traj1(fit1,  return = "index")
cat("idx_best =", idx_best, "\n")
fit_best <- select_traj1_fit(fit1, idx = idx_best)
gg <- my_glimpse(fit_best)
glimpse(gg)

#  go through all subjects in traj1_set and save best model fits  in a list

best_model_fits  <- lapply(traj1_set, function(x){
    fit1 <- traj1_fit(x)
    idx_best <- best_model_traj1(fit1,  return = "index")
    select_traj1_fit(fit1, idx = idx_best)

})

# glimpse tables appended vertically (for later use)

gfits <- lapply(best_model_fits, my_glimpse)
gfits_all <- bind_rows(gfits)
gfits_all$breakpoint


# Augment tables appended vertically (for later use)

augs <- lapply(best_model_fits, my_augment)
augs_best <- bind_rows(augs)
augs_best





