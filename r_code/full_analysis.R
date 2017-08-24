# full_analysis.R
#
# This script is called as:
# source("full_analysis.R")
#
# There are three switches that this uses to determine its behavior:
# 
# * The data.frame: models.run. This data frame determines what models are run.
#   Its format is given as an example below. If it is undefined, then a default
#   version is loaded from 'models_run.R' Note that multiple model objects per
#   risk level does not present a problem.
#
#       risk lst
#       lr   npt.final
#       lr   npt.final.L
#       mr   mr.final
#       hr   hr.final
#
# * The logical variable 'do.predictions'. If it is 
#   TRUE, then predictions are generated from the models run. If not, then no predictions
#   are generated from the estimated models. If it is undefined, then it is assumed to be TRUE.
#
# * The logical variable 'roll.up.2.dept'. If it is TRUE, then the predictions are rolled
#   up to the department level. If it is FALSE, then the predictions are left at the census
#   tract level. If it is undefined, then it is assumed to be FALSE.
#
# As currently written, it is assumed that the caller can define these variables in the 
# Global Environment before 'source'ing this script. If not, then some adjustment will 
# have to be made.
#
# Future Work:
# * I do not like the data.frame approach to models.run. My preference would be a list 
#   that looks something like this:
#
#   list( lr=c("npt.final", "npt.final.L"),
#         mr=c("mr.final"),
#         hr=c("hr.final")
#       )
#   I like it better because it more cleanly represents what is being done and the work flow.
#   If Python can handle R lists, then this becomes the preferred approach.
#
src.names <- c( lr="low.risk.fires",
                mr="med.risk.fires",
                hr="high.risk.fires")
src.tabls <- c( lr="select * from nist.low_risk_fires",
                mr="select * from nist.med_risk_fires",
                hr="select * from nist.high_risk_fires")
est.names <- c( lr="lr.mr.pred",
                mr="lr.mr.pred",
                hr="hr.pred")
est.tabls <- c( lr="select * from nist.lr_mr_pred",
                mr="select * from nist.lr_mr_pred",
                hr="select * from nist.hr_pred")
library(RPostgreSQL)
library(pkgFireCARES)
if(! exists("conn")){
  conn <- dbConnect( "PostgreSQL", 
                     host="firecares-restore.c3gxdjk57saa.us-east-1.rds.amazonaws.com", 
                     dbname="nfirs", 
                     user="username", 
                     password="pwd")
}
browser()
# The 'models.run' object contains the list of control objects
# to run and some key information this script needs to run them.
# Check to see if it already exists. If not load it off the disk.
# If it still doesn't exist, error out.
if( ! exists("models.run"))   source("models_run.R")
if( ! exists("models.run")) stop("The list of models to run (in 'models.run') does not exist!")
if(length(setdiff(unique(models.run$risk), c("lr", "mr", "hr"))) > 0) stop( "The only risk classes allowed in models.run are 'lr', 'mr', and 'hr'!")
if( ! exists("roll.up.2.dept")) roll.up.2.dept <- FALSE
if( ! exists("do.predictions")) do.predictions <- TRUE

models.run0 <- list()
for(i in unique(models.run$risk)){
  models.run0[[i]] <- models.run$lst[models.run$risk == i]
}

for(i in names(models.run0)){
  src.name <- est.names[i]
  if(! exists(est.name)){
    assign(est.name, dbGetQuery(conn, est.tabls[i]))
    assign(est.name, fcSetup(get(est.name))
  }
  models  <- mass.npt(conn, list=models.run0[[i]])
  objects <- fcMacro(models)
#
# The format of 'objects' (from the fcMacro documentation) is:
#     npt.name   res.name       tst.name       msg.name         save.name
#     npt.final  npt.final.res  npt.final.tst  messages.00.txt  npt.final.RData
#     ...
#
  if(do.predictions){
    est.name <- src.names[i]
    if(! exists(src.name)){
      assign(src.name, dbGetQuery(conn, src.tabls[i]))
      assign(src.name, fcSetup(get(src.name))
    }
# Basically what I am going to do here is call fcEstimate. Note that 
# fcEstimate can take lists of control and output objects and work on 
# them as a unit. However, since the output objects can be quite large,
# I am doing them one at a time so as not to blow up the machine.
    for(j in 1:nrow(objects)){
      e <- new.env()
      load(objects$save.name[i], e)
      with(objects, assign(npt.name[i], get(npt.name[i], e)))
      with(objects, assign(res.name[i], get(res.name[i], e)))
      rm(e)
      temp.98765 <- fcEstimate(objects$npt.name[i], 
                               objects$res.name[i], 
                               get("est.name"), 
                               subset=quote(fd_size %in% paste("size_", 3:9, sep="")))
# As written it is possible for some results to have the census tract column called
# 'geoid' in some cases and 'tr10_fid' in others. More particularly, the team wants
# the column called 'tr10_fid' to be consistent with their work. This takes care of 
# that.
      if( "geoid" %in% names(temp.98765)){
        names(temp.98765)[match("geoid", names(temp.98765))] <- "tr10_fid"
      }
# Here I get a list of the names of the prediction columns.
      dta.cols <- setdiff(names(temp.98765), c("geoid", "tr10_fid", "fd_id", "parcel_id", "year", "geoid_source"))
# Since the high-risk results are at the parcel level, and all the rest of the data
# is at the census tract level, I need to roll up the hr data to the census tract level.
      if(i == 'hr'){
        temp.98765 <- aggregate(temp.98765[, dta.cols], 
                                with(temp.98765, list(year=year, geoid=geoid, fd_id=fd_id), 
                                function(x) sum(x, na.rm=TRUE)))
      }
# Since it is highly likely that there will be multiple columns across different
# risk classes with the same names, take steps to make the names different.
      names(temp.98765)[match(dta.cols), names(temp.98765)] <- paste(i, dta.cols, sep=".")
# Combine the various predictions into a single data frame.
      if(exists("predictions")){
        predictions <- merge(predictions, temp.98765, all=TRUE)
      } else {
        predictions <- temp.98765
      }
      rm(temp.98765)
    }
# If requested, roll the data up to the department level.
    if(roll.up.2.dept){
      dta.cols <- setdiff(names(predictions), c("geoid", "tr10_fid", "fd_id", "parcel_id", "year", "geoid_source"))
      temp.98765 <- aggregate(predictions[, dta.cols], 
                              with(predictions, list(year=year, fd_id=fd_id), 
                              function(x) sum(x, na.rm=TRUE)))	  
    }
  }
}
