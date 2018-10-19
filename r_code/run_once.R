# First, install all packages needed to run pkgFireCARES and the associated scripts
install.packages( c( "RPostgreSQL",
                     "ranger",
                     "glmnet",
                     "devtools",
                     "roxygen2",
                     "acs",
                     "boot",
                     "magrittr"), 
                  repos="https://cloud.r-project.org",
                  dependencies=c("Depends","Imports") )
# Next compile the pkgFireCARES package. I need to know if devtools requires the  
# RTools keychain installed. Since the server is running on a Linux box, this is 
# probably not a current problem. But this needs to be tested to be sure.
library(devtools)
library(roxygen2)
document("pkgFireCARES")
install ("pkgFireCARES")
# install the Census key
library(acs)
api.key.install( "d075012434f632cf410c8b7d9441c0f56747117c" )
