
# Make sure you have devtools (once)
setwd("C:/ATG/github/trajclass")
# Delete man/ + other generated files
#unlink(c("man", "NAMESPACE"), recursive = TRUE)

# Re-generate everything
devtools::document()
# devtools::load_all()
# devtools::document()
devtools::install() # skips tests by default







