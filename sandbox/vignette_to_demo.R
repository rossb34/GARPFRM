# Script to extract R code from the vignettes and create a demo file with the
# extracted R code.

library(knitr)

files <- list.files("vignettes/")
vignette_files <- files[grep(pattern=".Rnw", x=files)]

for(file in vignette_files){
  knit(input=paste("vignettes", file, sep="/"), 
       output=paste("demo", gsub(".Rnw", ".R", file), sep="/"), 
       tangle=TRUE)
}

