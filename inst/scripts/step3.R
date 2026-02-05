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


#===== Linear model fits for all subjects ==========


lin_all_fits <- lapply(traj1_set, function(x) {
     fitx <- traj1_fit(x)
     select_traj1_fit(fitx, idx = 1)
})

#--- Linear model fits: tibble with glimpses
g_lin <- lapply(lin_all_fits, my_glimpse) # list
g_tbl <- bind_rows(g_lin)           # tibble
(nms <- colnames(g_tbl))
selct <- c(".traj_id", "status", "valid", "n_obs", "r_squared", "sigma", "aic")
g_tbl |> select(all_of(selct))

#--- Linear model fits: append tidy tibbles with parameter estimates

td_lin <- lapply(lin_all_fits, my_tidy)   # list
td_tbl <- bind_rows(td_lin)           # tibble
(nms <- colnames(td_tbl))
selct <- c(".traj_id", "model", "term", "estimate", "std.error", "statistic", "p.value","conf.low", "conf.high")
td_tbl |> select(all_of(selct))

#----- linear model fits: Augment tables appended for all subjects

aug_lin <- lapply(lin_all_fits, my_augment)   # list
aug_tbl <- bind_rows(aug_lin)           # tibble
(nms <- colnames(aug_tbl))
selct <- c("ID", "time", "egfr", ".fitted", ".resid", ".se_fit")
aug_tbl |> select(all_of(selct))

#===== Quadratic model fits for all subjects ==========


quad_all_fits <- lapply(traj1_set, function(x) {
     fitx <- traj1_fit(x)
     select_traj1_fit(fitx, idx = 2)   # quadratic model
   })

#--- quadratic model fits: tibble with glimpses for all subjectsb appended
g_quad <- lapply(quad_all_fits, my_glimpse) # list
g_tbl <- bind_rows(g_quad)           # tibble
(nms <- colnames(g_tbl))
selct <- c(".traj_id", "status", "valid", "n_obs", "r_squared", "sigma", "aic")
g_tbl |> select(all_of(selct))

#--- quadratic models: append tidy tibbles

td_quad <- lapply(quad_all_fits, my_tidy)   # list
td_tbl <- bind_rows(td_quad)           # tibble
(nms <- colnames(td_tbl))
selct <- c(".traj_id", "model", "term", "estimate", "std.error", "statistic", "p.value","conf.low", "conf.high")
td_tbl |> select(all_of(selct))

#---quadratic  models:  Augment tables appended for all subjects

aug_quad <- lapply(quad_all_fits, my_augment)   # list
aug_tbl <- bind_rows(aug_quad)           # tibble
(nms <- colnames(aug_tbl))
selct <- c("ID", "time", "egfr", ".fitted", ".resid", ".se_fit")
aug_tbl |> select(all_of(selct))


#===== Hockey_stick model fits for all subjects ==========


hstick_all_fits <- lapply(traj1_set, function(x) {
     fitx <- traj1_fit(x)
     select_traj1_fit(fitx, idx = 3)  
})

fail <- sapply(hstick_all_fits, is.null) # logical vector: TRUE for some subjects


nms2remove <- names(hstick_all_fits)[fail] # Subject with NULL result
hstick_all_fits[nms2remove] <- NULL


g_hkey <- lapply(hstick_all_fits, my_glimpse)
g_tbl <- bind_rows(g_hkey)
(nms <- colnames(g_tbl))
selct <- c(".traj_id", "status", "valid", "n_obs", "aic", "breakpoint_est", "breakpoint_se")
g_tbl |> select(all_of(selct))



# ===== BEST model fits for all subjects. go through all subjects in traj1_set and save best model fits  in a list

best_model_fits  <- lapply(traj1_set, function(x){
    fit1 <- traj1_fit(x)
    idx_best <- best_model_traj1(fit1,  return = "index")
    select_traj1_fit(fit1, idx = idx_best)

})

# glimpse tables for best models appended vertically (for later use)

gfits <- lapply(best_model_fits, my_glimpse)
gfits_all <- bind_rows(gfits)
names(gfits_all)


# Augment tables for best models appended vertically (for later use)

augs <- lapply(best_model_fits, my_augment)
augs_best <- bind_rows(augs)
augs_best

# glimpse tables for hockey_stick models fitted to




