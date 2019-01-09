# Run this script when chamges are made to IRlibrary functions

library(devtools)
setwd("./IRlibrary")
document()
setwd("..")
install("IRlibrary")