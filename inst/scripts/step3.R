### # Execute this script From fresh R session and check for errors, if any

# Examine data for all subjects
rm(list= ls())
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

message("=========== script step3.R is executed")

load(file = "./traj1_set_object.rda")  # Load `traj1_set` object from external file created by script stored in step1.R 
message("Object traj1_set was loaded from external file")
typeof(traj1_set)
length(traj1_set) 
subject_names <- names(traj1_set) # Select one subject from this list
print(head(subject_names))


#===== Linear model fits for all subjects ==========
message("=== Linear model fits for all subjects created ===")


lin_all_fits <- lapply(traj1_set, function(x) {
     fitx <- traj1_fit(x)
     select_traj1_fit(fitx, idx = 1)
})

#--- Linear model fits: tibble with glimpses
g_lin <- lapply(lin_all_fits, my_glimpse) # list
g_lin_bind <- bind_rows(g_lin)           # tibble
(nms <- colnames(g_lin_bind))
selct <- c(".traj_id", "status", "valid", "n_obs", "r_squared", "sigma", "aic")
g_lin_bind |> select(all_of(selct))

#--- Linear model fits: append tidy tibbles with parameter estimates

td_lin <- lapply(lin_all_fits, my_tidy)   # list
td_lin_bind <- bind_rows(td_lin)           # tibble
(nms <- colnames(td_lin_bind))
selct <- c(".traj_id", "model", "term", "estimate", "std.error", "statistic", "p.value","conf.low", "conf.high")
td_lin_bind |> select(all_of(selct))

#----- linear model fits: Augment tables appended for all subjects

aug_lin <- lapply(lin_all_fits, my_augment)   # list
aug_lin_bind <- bind_rows(aug_lin)           # tibble
(nms <- colnames(aug_lin_bind))
selct <- c("ID", "time", "egfr", ".fitted", ".resid", ".se_fit")
aug_lin_bind |> select(all_of(selct))

#===== Quadratic model fits for all subjects ==========
message("=== Quadratic model fits for all subjects created ===")


quad_all_fits <- lapply(traj1_set, function(x) {
     fitx <- traj1_fit(x)
     select_traj1_fit(fitx, idx = 2)   # quadratic model
   })

#--- quadratic model fits: tibble with glimpses for all subjectsb appended
g_quad <- lapply(quad_all_fits, my_glimpse) # list
g_quad_bind <- bind_rows(g_quad)           # tibble
(nms <- colnames(g_quad_bind))
selct <- c(".traj_id", "status", "valid", "n_obs", "r_squared", "sigma", "aic")
g_quad_bind |> select(all_of(selct))

#--- quadratic models: append tidy tibbles

td_quad <- lapply(quad_all_fits, my_tidy)   # list
td_quad_bind <- bind_rows(td_quad)           # tibble
(nms <- colnames(td_quad_bind))
selct <- c(".traj_id", "model", "term", "estimate", "std.error", "statistic", "p.value","conf.low", "conf.high")
td_quad_bind |> select(all_of(selct))

#---quadratic  models:  Augment tables appended for all subjects

aug_quad <- lapply(quad_all_fits, my_augment)   # list
aug_quad_bind <- bind_rows(aug_quad)           # tibble
(nms <- colnames(aug_quad_bind))
selct <- c("ID", "time", "egfr", ".fitted", ".resid", ".se_fit")
aug_quad_bind |> select(all_of(selct))


#===== Hockey_stick model fits for all subjects ==========
message("=== Hockey stick model fits for all subjects created ===")


hstick_all_fits <- lapply(traj1_set, function(x) {
     fitx <- traj1_fit(x)
     select_traj1_fit(fitx, idx = 3)  
})

fail <- sapply(hstick_all_fits, is.null) # logical vector: TRUE for some subjects


nms2remove <- names(hstick_all_fits)[fail] # Subject with NULL result
hstick_all_fits[nms2remove] <- NULL  # removed

#--- Hockey_sticks: glimpse
g_hkey <- lapply(hstick_all_fits, my_glimpse)
g_hkey_bind <- bind_rows(g_hkey)
(nms <- colnames(g_hkey_bind))
selct <- c(".traj_id", "status", "valid", "n_obs", "aic", "breakpoint_est", "breakpoint_se")
g_hkey_bind |> select(all_of(selct))

#--- Hockey_sticks: tidy
td_hkey <- lapply(hstick_all_fits, my_tidy)
td_hkey_bind <- bind_rows(td_hkey)
(nms <- colnames(td_hkey_bind))
selct <- c(".traj_id", "status", "valid", "n_obs", "aic", "breakpoint_est", "breakpoint_se")
td_hkey_bind |> select(all_of(selct))


# ===== BEST model fits for all subjects. go through all subjects in traj1_set and save best model fits  in a list
message("=== Best model fits for all subjects created ===")

best_model_fits  <- lapply(traj1_set, function(x){
    fit1 <- traj1_fit(x)
    idx_best <- best_model_traj1(fit1,  return = "index")
    select_traj1_fit(fit1, idx = idx_best)

})

# glimpse tables for best models appended vertically (for later use)

g_best <- lapply(best_model_fits, my_glimpse)
g_best_bind <- bind_rows(g_best)
(nms <- colnames(g_hkey_bind))
selct <- c(".traj_id", "status", "valid", "n_obs", "aic", "breakpoint_est", "breakpoint_se")
g_best_bind |> select(all_of(selct))

g_tbls <- c("g_lin_bind", "g_quad_bind", "g_hkey_bind")
td_tbls <- c("td_lin_bind", "td_quad_bind", "td_hkey_bind")
save(list = c(g_tbls, td_tbls), file = "summary_tbls.rda")


