# Execute this script From fresh R session and check for errrs, if any

rm(list = ls())
getwd()  # check working directory
message("... Script step0.R executed")

library(trajclass)
packageDescription("trajclass")

packageVersion("trajclass")

exported_objects <- sort(getNamespaceExports("trajclass")) # Exported object names

pkg_path <- system.file(package = "trajclass")
trajclass_options(all= TRUE)

print(scr_path <- paste0(pkg_path, "/", "scripts")) # path to scripts
list.files(scr_path) # list of scripts to be executed by the user

sessionInfo()
message("... step0.R script execution completed ====")
message("... Use ls() command to find out what objects were created by this script")





