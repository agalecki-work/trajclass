setwd("C:/ATG/github/trajclass/R")

ff <- list.files(pattern = "\\.R$")
lapply(ff, function(f){
 print(f)
 source(f)
})

