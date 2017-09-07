#' Runs the full analysis of a set of models.
#'
#' This function runs through the entire process of estimating models and 
#' developing predictions at either the census tract level or at the department
#' level. This function can be run with NO inputs. That is, it can be called 
#' as \code{full_analysis()} and it will work. Default values exist in some
#' form for all the parameters. Supply the parameters if you want different
#' results.
#' 
#' @param conn A DBI Connection. This is a connection to the database containing
#'             the data and model definitions. If none is entered, default connection
#'             information is obtained from the operating system environment.
#'
#' @param models.run Either a list or a data frame. This determines what models 
#'                   are run. Its format is given as an example below. If it is 
#'                   undefined, then a default set of models are run (see below).
#'                   Note that multiple model objects per risk level does not present 
#'                   a problem.
#'
#' @param bypass.models=FALSE Logical. If it is TRUE, then no models are estimated. If not, 
#'                      then the models listed in 'models.run' above are estimated first. 
#'                      Note that if 'bypass.models' is TRUE, then the 'objects' 
#'                      data frame (output of the fcMacro function) must be supplied. 
#'
#' @param do.predictions=TRUE Logical. If it is TRUE, then predictions are generated from 
#'                            the models run. If not, then no predictions are generated 
#'                            from the estimated models.
#'
#' @param roll.up.2.dept=FALSE Logical. If it is TRUE, then the predictions are rolled
#'                             up to the department level. If it is FALSE, then the 
#'                             predictions are left at the census tract level.
#' 
#' @param object.list List of data frames. The list must contain an entry for every risk
#'                    level that is run. The entry for each risk level must contain data frame 
#'                    with the output from \code{\link(fcMacro}} for that risk level. If 
#'                    \code{bypass.models} is TRUE and this parameter is undefined, the 
#'                    function will error out.
#'
#' @details
#' The \code{models.run} parameter can have one of two formats, a list format or a data 
#' frame format. The list format is preferred. The data frame format has two columns: 
#' \code{risk} and \code{lst}. Both columns have character format. Each row represents 
#' a model set to be run. The \code{risk} column specfies the risk level associated with
#' that model set (and can only be one of 'lr', 'mr', or 'hr'). The \code{lst} column
#' is the \code{lst} value from the \code{controls} database for the model set to be 
#' run. The default value of \code{models.run} (in data.frame format) is listed below.
#'
#' \tabular{lcl}{
#' risk  \tab lst\cr
#' lr    \tab npt.final\cr
#' lr    \tab npt.final.L\cr
#' mr    \tab mr.final\cr
#' hr    \tab hr.final
#' }
#'
#' The list format contains a named entry for each risk level to be run. The 
#' entry contains a character vector listing the \code{lst} values from the 
#' \code{controls} database for the model sets to be run for that risk level.
# The default value of \code{models.run} (in list format) is listed below.
#' 
#' ##  models.run <- list( lr=c("npt.final", "npt.final.L"),
#' ##                      mr=c("mr.final"),
#' ##                      hr=c("hr.final") )
#'
#' The \code{object.list} 
#'
#'
#' Note: The format of 'objects' (from the fcMacro documentation) is:
#'       npt.name   res.name       tst.name       msg.name         save.name
#'       npt.final  npt.final.res  npt.final.tst  messages.00.txt  npt.final.RData
#'       ...
#'
#' @export
#'
#' @return
#' returns a list with the following entries:
#'
#' \describe{
#'   \item{table.name.est}{Name of the table on the database in which the new estimates
#'                   are stored.} 
#'   \item{table.name.err}{Name of the table on the database in which the new error values
#'                   are stored.}
#'   \item{rows}{Number of rows added to the data set.}
#'   \item{elapsed.time}{Time it took to complete the download (in seconds?)}
#' }
#'
#' This script leaves the following files in the global environment:
#'
#' Run Information:
#'   conn           DBI Connection supplied to the script
#'   bypass.models  Logical variable described above.
#'   do.predictions Logical variable described above.
#'   roll.up.2.dept Logical variable described above.
#'   models.run     A LIST containing the same information as models.run above (see 'future work' 
#'                  below for a description of this list).
#'
#' Output Information
#'   prediction     Predictions either by census tract or department depending on the 
#'                  value of roll.up.2.dept.
#'   *.predict      For each risk class contains the original predictions. This will differ from the
#'                  values in 'predictions' in two cases. If roll.up.2.dept is TRUE, then this will
#'                  contain tract / parcel level predictions will 'predictions' contains summaries by
#'                  department. This will always differ from the values in 'predictions' for high-risk
#'                  fires because the raw values are at the parcel level, not at either the tract or 
#'                  department level.
#'   object.list    This contains a list of data frames with the names of the message files created on
#'                  the disk, the save files created on the disk (when bypass.models is FALSE), and the 
#'                  objects they contain. Note that if bypass.models is TRUE, then this list must be supplied
#'                  (it will still be left in the global environment).
#'
#' @section Future Work:
#' Convert this to a function in the pkgFireCARES package. That requires the following tasks:
#'   * Convert the switches above to parameters. As currently written, this script 
#'     can be run with no information provided--all the inputs have defaults. Keep it
#'     that way.
#'   * As a script, this left several objects in the global environment. Find a way
#'     to return those objects.
#'   * Convert this header to documentation for the function.
#'   * Update the documentation for the package (in \code{pkgFireCARES.R}) to reflect the 
#'     addition of this function.
#'   * Test everything and make sure I didn't break it.
#'
full_analysis <- function(conn=NULL, 
                          models.run=NULL,
                          bypass.models=FALSE,
                          do.predictions=TRUE,
                          roll.up.2.dept=FALSE,
                          object.list=NULL){
#
  library(RPostgreSQL)
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
# Handle default values of inputs and check for validity
  if(is.null("conn")){
    conn <- dbConnect( "PostgreSQL", 
                       host    =Sys.getenv("DATABASE_HOST"),
                       port    =Sys.getenv("DATABASE_PORT"),
                       dbname  =Sys.getenv("DATABASE_NAME"),
                       user    =Sys.getenv("DATABASE_USER"),
                       password=Sys.getenv("DATABASE_PASSWORD"))
  }
# The 'models.run' object contains the list of control objects
# to run and some key information this script needs to run them.
# Check to see if it already exists. If not, create it.
  if(is.null("models.run")){
    models.run <- list( lr=c("npt.final", "npt.final.L"),
                        mr=c("mr.final"),
                        hr=c("hr.final") )
  }
  if(length(setdiff(unique(models.run$risk), c("lr", "mr", "hr"))) > 0){
    stop( "The only risk classes allowed in models.run are 'lr', 'mr', and 'hr'!")
  }
  roll.up.2.dept <- roll.up.2.dept && do.predictions
  if(bypass.models & is.null("object.list")){
    stop("If bypass.models is TRUE then objects.list must be defined!")
  }
  if(! bypass.models) object.list <- list()
# if necessary, convert models.run to a more congenial list.
  if(is.data.frame(models.run)){
    models.run0 <- list()
    for(i in unique(models.run$risk)){
      models.run0[[i]] <- models.run$lst[models.run$risk == i]
    }
    models.run <- models.run0
    rm(models.run0)
  }
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
      risk.results <- list()
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
# 'geoid' in some cases and 'tr10_fid' in others. That would cause problems merging 
# data sets.  The team wants that column called 'tr10_fid' to be consistent with 
# their work. This takes care of that.
        if( "geoid" %in% names(temp.98765)){
          names(temp.98765)[match("geoid", names(temp.98765))] <- "tr10_fid"
        }
# Here I get a list of the names of the prediction columns.
        dta.cols <- setdiff(names(temp.98765), c("geoid", "tr10_fid", "fd_id", "parcel_id", "year", "geoid_source"))
# Since it is highly likely that there will be multiple columns across different
# risk classes with the same names, take steps to make the names different.
        names(temp.98765)[match(dta.cols, names(temp.98765))] <- paste(i, dta.cols, sep=".")
# Combine the various predictions into a single data frame.
        if(i %in% names(risk.results)){
          risk.results[[i]] <- merge(risk.results[[i]], temp.98765, all=TRUE)
        } else {
          risk.results[[i]] <- temp.98765
        }
        rm(temp.98765)
      }
# 
# Since the high-risk results are at the parcel level, and all the rest of the data
# is at the census tract level, I need to roll up the hr data to the census tract level.
      if(i == 'hr'){
        temp.98765 <- risk.results[["hr"]]
        risk.results[["hr"]] <- aggregate(temp.98765[, paste(i, dta.cols, sep=".")], 
                                          with(temp.98765, list(year=year, tr10_fid=tr10_fid, fd_id=fd_id)), 
                                          function(x) sum(x, na.rm=TRUE))
      }
#
# Cleanup
# Again, some of these objects can be quite large. Deleting them 
# may prevent the program from crashing due to an out-of-memory 
# error.
    rm(list=intersect(c(src.name, objects$npt.name, objects$res.name ), ls()))
    rm(list=intersect(c("src.name", "objects", "est.name", "dta.cols" ), ls()))
#
# Merge all the predictions into a single prediction data frame.
      if(exists("predictions", inherits=FALSE)){
        predictions <- merge(predictions, risk.results[[i]], all=TRUE)
      } else {
        predictions <- risk.results[[i]]
      }
# Clean up. I want to leave the intermediate results, mainly for the high-risk data
# so they can be inspected. So we return risk.results[['hr']] to its former self, 
# and delete the intermediate table.
      if(i == 'hr'){
        risk.results[["hr"]] <- temp.98765
        rm(temp.98765)
      }
    }
  }
# If requested, roll the data up to the department level.
  if(roll.up.2.dept){
    fire.col <- grep("fire", names(predictions), value=TRUE)
    sz2.col  <- grep("sz2",  names(predictions), value=TRUE)
    sz3.col  <- grep("sz3",  names(predictions), value=TRUE)
    predictions <- rollUp2Dept(predictions, fire.col, sz2.col, sz3.col)
  }
# Return the results
  list(models.run=models.run,
       settings=list(bypass.models=bypass.models,
                     do.predictions=do.predictions,
                     roll.up.2.dept=roll.up.2.dept),
       models.run=models.run,
       prediction=prediction,
       object.list=object.list,
       risk.results=risk.results)             
}