# full_analysis.R
#
# This script is called as:
# source("full_analysis.R")
#
# There are four switches that this uses to determine its behavior:
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
# * The logical variable 'bypass.models'. If it is TRUE, then no models are estimated. If not, 
#   then the models listed in 'models.run' above are estimated first. Note that if 'bypass.models'
#   is TRUE, then the 'objects' data frame (output of the fcMacro function) must be defined in
#   the global environment. Since this script is set up to evaluate each separate risk level 
#   separately, the objects data frame must be supplied separately for each risk level. That is 
#   accomplished by supplying each objects data frame in a named list (with the risk levels as 
#   the names). If 'bypass.models' is undefined, then it is assumed to be FALSE.
#
# * The logical variable 'do.predictions'. If it is TRUE, then predictions are generated from 
#   the models run. If not, then no predictions are generated from the estimated models. If it 
#   is undefined, then it is assumed to be TRUE.
#
# * The logical variable 'roll.up.2.dept'. If it is TRUE, then the predictions are rolled
#   up to the department level. If it is FALSE, then the predictions are left at the census
#   tract level. If it is undefined, then it is assumed to be FALSE.
#
# As currently written, it is assumed that the caller can define these variables in the 
# Global Environment before 'source'ing this script. If not, then some adjustment will 
# have to be made.
#
# Output
# This script leaves the following files in the global environment:
# 
# Run Information:
#   conn           DBI Connection supplied to the script
#   bypass.models  Logical variable described above.
#   do.predictions Logical variable described above.
#   roll.up.2.dept Logical variable described above.
#   models.run     A LIST containing the same information as models.run above (see 'future work' 
#                  below for a description of this list).
#
# Output Information
#   prediction     Predictions either by census tract or department depending on the 
#                  value of roll.up.2.dept.
#   *.predict      For each risk class contains the original predictions. This will differ from the
#                  values in 'predictions' in two cases. If roll.up.2.dept is TRUE, then this will
#                  contain tract / parcel level predictions will 'predictions' contains summaries by
#                  department. This will always differ from the values in 'predictions' for high-risk
#                  fires because the raw values are at the parcel level, not at either the tract or 
#                  department level.
#   object.list    This contains a list of data frames with the names of the message files created on
#                  the disk, the save files created on the disk (when bypass.models is FALSE), and the 
#                  objects they contain. Note that if bypass.models is TRUE, then this list must be supplied
#                  (it will still be left in the global environment).
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
                     host="firecares-restore.c3gxdjk57saa.us-east-1.rds.amazonaws.com", # Sys.getenv("host"),
                     dbname="nfirs",                                                    # Sys.getenv("dbname"),
                     user="username",                                                   # Sys.getenv("user"),
                     password="pwd")                                                    # Sys.getenv("password"))
}
# The 'models.run' object contains the list of control objects
# to run and some key information this script needs to run them.
# Check to see if it already exists. If not load it off the disk.
# If it still doesn't exist, error out.
if( ! exists("models.run"))   source("models_run.R")
if( ! exists("models.run")) stop("The list of models to run (in 'models.run') does not exist!")
if(length(setdiff(unique(models.run$risk), c("lr", "mr", "hr"))) > 0){
  stop( "The only risk classes allowed in models.run are 'lr', 'mr', and 'hr'!")
}
if( ! exists("do.predictions")) do.predictions <- TRUE
if( ! exists("roll.up.2.dept")) roll.up.2.dept <- FALSE
roll.up.2.dept <- roll.up.2.dept && do.predictions
if( ! exists("bypass.models")) bypass.models <- FALSE
if(bypass.models & (! exists("object.list"))){
  stop("If bypass.models is TRUE then the objects data frame (output of the fcMacro function) must be present!")
}
if(! bypass.models) object.list <- list()
models.run0 <- list()
for(i in unique(models.run$risk)){
  models.run0[[i]] <- models.run$lst[models.run$risk == i]
}
models.run <- models.run0
rm(models.run0)

for(i in names(models.run)){
  src.name <- src.names[i]
  if(! bypass.models){
# Download the data needed for the model estimation
    if(! exists(src.name)){
      assign(src.name, dbGetQuery(conn, src.tabls[i]))
      assign(src.name, fcSetup(get(src.name)))
    }
# Create the control objects for the models...
    models  <- mass.npt(conn, list=models.run[[i]])
# And estimate the models.
    objects <- fcMacro(models)
	object.list[[i]] <- objects
  } else {
# If bypass.models is defined, then we need to get the relevant 'objects' data frame
# If it does not exist in the object.list list then skip to the next risk level.
    if(i %in% names(object.list)){
      objects <- object.list[[i]]
	} else {
      next
	}
  }
#
# Note: The format of 'objects' (from the fcMacro documentation) is:
#       npt.name   res.name       tst.name       msg.name         save.name
#       npt.final  npt.final.res  npt.final.tst  messages.00.txt  npt.final.RData
#       ...
#
  if(do.predictions){
    pred.name <- paste(i, "predict", sep=".")
    est.name <- est.names[i]
    if(! exists(est.name)){
      assign(est.name, dbGetQuery(conn, est.tabls[i]))
      assign(est.name, fcSetup(get(est.name)))
    }
# Basically what I am going to do here is call fcEstimate. Note that 
# fcEstimate can take lists of control and output objects and work on 
# them as a unit. However, since the output objects can be quite large,
# I am doing them one at a time so as not to blow up the machine.
    for(j in 1:nrow(objects)){
      e <- new.env()
      load(objects$save.name[j], e)
      with(objects, assign(npt.name[j], get(npt.name[j], e), globalenv()))
      with(objects, assign(res.name[j], get(res.name[j], e), globalenv()))
      rm(e)
      temp.98765 <- fcEstimate(objects$npt.name[j], 
                               objects$res.name[j], 
                               get(est.name), 
                               subset=quote(fd_size %in% paste("size_", 3:9, sep="")))
# As written it is possible for some results to have the census tract column called
# 'geoid' in some cases and 'tr10_fid' in others. More particularly, the team wants
# that column called 'tr10_fid' to be consistent with their work. This takes care of 
# that.
      if( "geoid" %in% names(temp.98765)){
        names(temp.98765)[match("geoid", names(temp.98765))] <- "tr10_fid"
      }
# Here I get a list of the names of the prediction columns.
      dta.cols <- setdiff(names(temp.98765), c("geoid", "tr10_fid", "fd_id", "parcel_id", "year", "geoid_source"))
# Since it is highly likely that there will be multiple columns across different
# risk classes with the same names, take steps to make the names different.
      names(temp.98765)[match(dta.cols, names(temp.98765))] <- paste(i, dta.cols, sep=".")
# Combine the various predictions into a single data frame.
      if(exists(pred.name)){
        assign(pred.name, merge(get(pred.name), temp.98765, all=TRUE))
      } else {
        assign(pred.name, temp.98765)
      }
      rm(temp.98765)
    }
# 
# Since the high-risk results are at the parcel level, and all the rest of the data
# is at the census tract level, I need to roll up the hr data to the census tract level.
    if(i == 'hr'){
      temp.98765 <- hr.predict
      hr.predict <- aggregate(temp.98765[, paste(i, dta.cols, sep=".")], 
                              with(temp.98765, list(year=year, tr10_fid=tr10_fid, fd_id=fd_id)), 
                              function(x) sum(x, na.rm=TRUE))
    }
#
# Cleanup
  rm(list=intersect(c(src.name, objects$npt.name, objects$res.name ), ls()))
  rm(list=intersect(c("src.name", "objects", "est.name", "dta.cols" ), ls()))
#
# Merge all the predictions into a single prediction data frame.
    if(exists("predictions", inherits=FALSE)){
      predictions <- merge(predictions, get(pred.name), all=TRUE)
    } else {
      predictions <- get(pred.name)
    }
# Clean up. I want to leave the intermediate results, mainly for the high-risk data
# so they can be inspected. So we return hr.pred to its former self, and delete
# the intermediate table.
    if(i == 'hr'){
      hr.predict <- temp.98765
      rm(temp.98765)
    }
  }
}
# If requested, roll the data up to the department level.
# Note that this does not work for sz2 and sz3!!!
if(roll.up.2.dept){
  fire.col <- grep("fire", names(predictions), value=TRUE)
  sz2.col  <- grep("sz2",  names(predictions), value=TRUE)
  sz3.col  <- grep("sz3",  names(predictions), value=TRUE)
  predictions <- rollUp2Dept(predictions, fire.col, sz2.col, sz3.col)
}
#
# Do cleanup
keep <- c("conn", "bypass.models", "do.predictions", "roll.up.2.dept",
          "models.run", "object.list", 
          "predictions", paste(names(models.run), "predict", sep="."))
rm(list=setdiff(ls(), keep))
