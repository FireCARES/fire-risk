# source_data     source_table    model_s   est_data   est_table  rollup
# low.risk.fires  low_risk_fires  npt.final lr.mr.pred lr_mr_pred FALSE
# med.risk.fires  med_risk_fires  mr.final  lr.mr.pred lr_mr_pred FALSE
# high.risk.fires high_risk_fires hr.final  hr.pred    hr_pred    TRUE
#
#
#
#
library(RPostgreSQL)
library(pkgFireCARES)
conn <- dbConnect( "PostgreSQL", 
                   host="firecares-restore.c3gxdjk57saa.us-east-1.rds.amazonaws.com", 
                   dbname="nfirs", 
                   user="username", 
                   password="pwd")
# The 'models.run' object contains the list of control objects
# to run and some key information this script needs to run them.
# Check to see if it already exists. If not load it off the disk.
# If it still doesn't exist, error out.
if( ! exists("models.run"))   source("models_run.R")
if( ! exists("models.run")) stop("The list of models to run (in 'models.run') does not exist!")
if( 'lr' %in% models.run$risk ){
  low.risk.fires  <- dbGetQuery(conn, "select * from nist.low_risk_fires" )
  low.risk.fires  <- fcSetup(low.risk.fires )
  lr.mr.pred <- dbGetQuery(conn, "select * from nist.lr_mr_pred")
  lr.mr.pred <- fcSetup(lr.mr.pred)
}
if( 'mr' %in% models.run$risk ){
  med.risk.fires  <- dbGetQuery(conn, "select * from nist.med_risk_fires" )
  med.risk.fires  <- fcSetup(med.risk.fires )
  lr.mr.pred <- dbGetQuery(conn, "select * from nist.lr_mr_pred")
  lr.mr.pred <- fcSetup(lr.mr.pred)
}
if( 'hr' %in% models.run$risk ){
  high.risk.fires <- dbGetQuery(conn, "select * from nist.high_risk_fires")
  high.risk.fires <- fcSetup(high.risk.fires)
  hr.pred    <- dbGetQuery(conn, "select * from nist.hr_pred")
  hr.pred    <- fcSetup(hr.pred)
}
ctls <- models.run$lst
# mass.npt needs to be modified. Instead of working with a pattern,
# I need to work with a list. Probably the best way to handle it is
# to make both options available.
ctls <- mass.npt(list=ctls)
objs <- fcMacro(ctls)
# Note that most of these will NOT run the 'fcTest' routine because 
# the target variables will differ within the object. For this
# stage, the fact that it does not run 'fcTest' is not important.
#
# OK. This doesn't work. The problem is I need the names of the input and
# output objects as well as the associated prediction data frame.
# The solution, I think, is to supply a data.frame with all that information.
# That will result in a slight modification of the work above, where I do 
# mass.npt for each object rather than by the approach I use above.
#
# The other problem is knowing what input files go with what estimation file.
# I can either hard-code that information in or set up the routine so that the
# information is supplied. I think that the latter is the best approach. I am
# already building a data frame with the list of control objects to be run. It 
# would be a simple matter to include the .pred table to estimate with it as a 
# column.
objs <- merge(objs, models.run, by.x=c("npt.name"), by.y="npt")
for( i in 1:nrow(objs)){
  e <- new.env()
  load(objs$save.name[i], e)
  if(objs$risk=="hr"){
    uu <- fcEstimate( get(objs$npt.name[i], e), 
                      get(objs$res.name[i], e), 
                      hr.pred
    )
# Here is my planned work-plan for high-risk predictions.
# First, isolate the index fields (except for 'parcel_id'), probably
#   using intersect with the names of the fields.
# Then list the prediction fields.
# Aggregate, using the formula interface (which will require a 'formula(text)' type call)
# 
  ndx  <- intersect(c("year", "geoid", "state", "region", "fd_id", "fd_size"), names(uu))
  vars <- setdiff(names(uu), c("year", "geoid", "geoid_source", "parcel_id", "state", "region", "fd_id", "fd_size"))
  uu <- aggregate()
  } else {
    uu <- fcEstimate( get(objs$npt.name[i], e), 
                      get(objs$res.name[i], e), 
                      lr.mr.pred
    )
  }
  if(exists("results")){
    results <- merge(results, uu)
  } else {
    results <- uu
  }
}
