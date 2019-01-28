# This script will install the IRlibrary library. It should be run failrly often, as i am still
# adding functions 


# Check to see if devtools is installed. If not, install it
devtools_pkg <- "devtools"

new.packages <- devtools_pkg[!(devtools_pkg %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# Load devtools and install library
library("devtools")

install("IRlibrary", upgrade = 'always')


