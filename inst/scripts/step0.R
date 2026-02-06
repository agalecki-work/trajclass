# Execute this script From fresh R session and check for errors, if any
rm(list = ls())
getwd()  # check working directory

message("Script step0.R from trajclass package executed")

library(trajclass)
packageDescription("trajclass")

packageVersion("trajclass")

exported_objects <- sort(getNamespaceExports("trajclass")) # Exported object names
ls("package:trajclass") # All objects
typeof(prefer_tidyverbs) # [1] "closure"
trajclass_options(all =TRUE)  # Default trajclass options

pkg_path <- system.file(package = "trajclass")


print(scr_path <- paste0(pkg_path, "/", "scripts")) # path to scripts
list.files(scr_path) # list of scripts to be executed by the user

sessionInfo()




