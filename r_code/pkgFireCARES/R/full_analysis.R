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
#' @param roll.up.2.dept=TRUE  Logical. If it is TRUE, then the predictions are rolled
#'                             up to the department level. If it is FALSE, then the
#'                             predictions are left at the census tract level.
#'
#' @param object.list List of data frames. The list must contain an entry for every risk
#'                    level that is run. The entry for each risk level must contain data frame
#'                    with the output from \code{\link{fcMacro}} for that risk level. If
#'                    \code{bypass.models} is TRUE and this parameter is undefined, the
#'                    function will error out. If \code{bypass.models} is FALSE, then
#'                    this parameter is ignored.
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
#' \tabular{cc}{
#' \strong{risk} \tab \strong{lst}\cr
#' lr            \tab npt.final   \cr
#' lr            \tab npt.final.L \cr
#' mr            \tab mr.final    \cr
#' hr            \tab hr.final
#' }
#'
#' The list format contains a named entry for each risk level to be run. The
#' entry contains a character vector listing the \code{lst} values from the
#' \code{controls} database for the model sets to be run for that risk level.
#' The default value of \code{models.run} (in list format) is listed below.
#'
#' \code{models.run <- list(lr=c("npt.final", "npt.final.L"), mr=c("mr.final"), hr=c("hr.final"))}
#'
#' The \code{object.list} list object has an entry for each risk level run. That
#' entry is a data frame with information output from \code{\link{fcMacro}}. The
#' structure of that data frame is given by the following example:
#'
#' \tabular{ccccc}{
#' \strong{npt.name} \tab \strong{res.name} \tab \strong{tst.name} \tab \strong{msg.name} \tab \strong{save.name} \cr
#'   npt.final       \tab npt.final.res     \tab npt.final.tst     \tab messages.00.txt   \tab npt.final.RData    \cr
#'   npt.final.L     \tab npt.final.L.res   \tab npt.final.L.tst   \tab messages.01.txt   \tab npt.final.L.RData
#' }
#'
#' Note that if you are supplying the \code{object.list} structure while using the
#' \code{bypass.models} option, you can safely leave out the \code{tst.name} and
#' \code{msg.name} columns.
#'
#' @export
#'
#' @return
#' returns a list with the following entries:
#'
#' \describe{
#'   \item{models.run}{The \code{models.run} input listing the models run by
#'                     risk level}
#'   \item{bypass.models}{The input \code{bypass.models} value}
#'   \item{do.predictions}{The input \code{do.predictions} value}
#'   \item{roll.up.2.dept}{The input \code{roll.up.2.dept} value}
#'   \item{object.list}{The \code{object.list} object described above. If the
#'                      \code{bypass.models} flag is set, then this is the object
#'                      supplied to the function. Otherwise it is returned by the
#'                      calls to \code{\link{fcMacro}}.}
#'   \item{prediction}{Data frame containing predictions for all variables requested
#'                     in the models.run object. The predictions are either by census
#'                     tract or by department depending on the value of the \code{roll.up.2.dept}
#'                     flag.}
#'   \item{risk.results}{This is a list, with an entry for each risk level. Each entry
#'                       contains a data frame with the raw estimates for that risk level.
#'                       For low and medium risk fires this contains the predictions at the
#'                       census tract level (which are redundant with the results in
#'                       \code{predictions} if \code{roll.up.2.dept} is FALSE). For high risk
#'                       fires, this contains predictions at the parcel level.}
#' }
#'
full_analysis <- function(conn=NULL,
                          models.run=NULL,
                          bypass.models=FALSE,
                          do.predictions=TRUE,
                          roll.up.2.dept=TRUE,
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
  old.ls <- c(ls(pos=globalenv()), "rmse.sum")
# Handle default values of inputs and check for validity
  if(is.null(conn)){
    conn <- dbConnect( "PostgreSQL",
                       host    =Sys.getenv("NFIRS_DATABASE_HOST"),
                       port    =Sys.getenv("NFIRS_DATABASE_PORT"),
                       dbname  =Sys.getenv("NFIRS_DATABASE_NAME"),
                       user    =Sys.getenv("NFIRS_DATABASE_USER"),
                       password=Sys.getenv("NFIRS_DATABASE_PASSWORD"))
  }
# The 'models.run' object contains the list of control objects
# to run and some key information this script needs to run them.
# Check to see if it already exists. If not, create it.
  if(is.null(models.run)){
    models.run <- list(lr=c("npt.final", "npt.final.L"),
                       mr=c("mr.final"),
                       hr=c("hr.final"))
  }
  if(length(setdiff(unique(models.run$risk), c("lr", "mr", "hr"))) > 0){
    stop( "The only risk classes allowed in models.run are 'lr', 'mr', and 'hr'!")
  }
  roll.up.2.dept <- roll.up.2.dept && do.predictions
  if(bypass.models & is.null(object.list)){
    stop("If bypass.models is TRUE then objects.list must be defined!")
  }
  if(! bypass.models) object.list <- list()
# if necessary, convert a models.run data.frame to a more congenial list.
  if(is.data.frame(models.run)){
    models.run0 <- list()
    for(i in unique(models.run$risk)){
      models.run0[[i]] <- models.run$lst[models.run$risk == i]
    }
    models.run <- models.run0
    rm(models.run0)
  }
#
# Here we begin the actual analysis
#
  risk.results <- list()
  for(i in names(models.run)){
    src.name <- src.names[i]
    if(bypass.models){
# If bypass.models is defined, then we need to get the relevant 'objects' data frame
# If it does not exist in the object.list list then skip to the next risk level.
      if(i %in% names(object.list)){
        objects <- object.list[[i]]
      } else {
        next
	  }
    } else {
# Download the data needed for the model estimation
      if(! exists(src.name)){
        assign(src.name, dbGetQuery(conn, src.tabls[i]), pos=globalenv())
        assign(src.name, fcSetup(get(src.name)), pos=globalenv())
      }
# Create the control objects for the models...
      models  <- mass.npt(conn, list=models.run[[i]])
# And estimate the models.
      objects <- fcMacro(models)
	  object.list[[i]] <- objects
    }
#
    if(do.predictions){
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
        with(objects, assign(npt.name[j], get(npt.name[j], e), pos=globalenv()))
        with(objects, assign(res.name[j], get(res.name[j], e), pos=globalenv()))
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
# This returns the global environment to its original state and removes
# a few variables from the functions environment.
    rm(list=setdiff(ls(pos=globalenv()), old.ls), pos=globalenv())
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
  out <- list(models.run=models.run,
              bypass.models=bypass.models,
              do.predictions=do.predictions,
              roll.up.2.dept=roll.up.2.dept,
              object.list=object.list)
  if(do.predictions){
    out[["prediction"]] <- predictions
    out[["risk.results"]] <- risk.results
  }
  out
}
