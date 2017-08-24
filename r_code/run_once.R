install.packages( c( "RPostgreSQL",
                     "ranger",
                     "glmnet",
                     "devtools",
                     "roxygen2",
                     "acs",
                     "boot",
                     "utils"), 
                  repos="https://cloud.r-project.org",
                  dependencies=c("Depends","Imports") )
library(devtools)
library(roxygen2)
document("pkgFireCARES")
install ("pkgFireCARES")
