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
#' @param refresh.data=FALSE Logical. [This feature has not been implemented yet]
#'                   If this is TRUE, then the views on which the data for this
#'                   analysis are based are refreshed and the data.frames
#'                   used in this analysis are requeried.
#'
#' @param models.run Either a list or a data frame. This determines what models
#'                   are run. Its format is given as an example below. If it is
#'                   undefined, then a default set of models are run (see below).
#'                   Note that multiple model objects per risk level does not present
#'                   a problem.
#'
#' @param merg      Either a vector or a list. A value of '0' guarantees that the merg
#'                  option will not be used.
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
#' @param do.rmse=FALSE Logical. If TRUE, then the RMS Errors of all the models will be
#'                      computed.
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
#' @param incl.detail=FALSE Logical. If TRUE, the \code{risk.results} section of the
#'                    output (described below) is included. If FALSE then the \code{risk.results}
#'                    section is not returned.
#'
#' @details
#' The \code{models.run} parameter can have one of two formats, a list format or a data
#' frame format. The list format is preferred. The data frame format has two columns:
#' \code{risk} and \code{lst}. Both columns have character format. Each row represents
#' a model set to be run. The \code{risk} column specfies the risk level associated with
#' that model set (and can only be one of 'lr', 'mr', 'hr', 'ems500', and 'emscty').
#' The \code{lst} column is the \code{lst} value from the \code{controls} database for the
#' model set to be run. The default value of \code{models.run} (in data.frame format) is
#' listed below.
#'
#' \tabular{cc}{
#' \strong{risk} \tab \strong{lst}\cr
#' lr            \tab npt.final   \cr
#' mr            \tab mr.final    \cr
#' hr            \tab hr.final    \cr
#' ems500        \tab ems.5.final \cr
#' emscty        \tab ems.C.final
#' }
#'
#' The list format contains a named entry for each risk level to be run. Each
#' entry contains a character vector listing the \code{lst} values from the
#' \code{controls} database for the model sets to be run for that risk level.
#' The default value of \code{models.run} (in list format) is listed below.
#'
#' \code{models.run <- list(lr=c("npt.final"),
#'                          mr=c("mr.final"),
#'                          hr=c("hr.final"),
#'                          ems500=c("ems.5.final"),
#'                          emscty=c("ems.C.final"))}
#'
#' The \code{merg} entry is if you want to merge multiple models into a
#' single model. It can either be a data frame or a list of data frames. The
#' data frame contains two columns. The first column, labeled either 'geoid' or
#' 'tr10_fid', contains the tract/spatial identifiers used for prediction. The
#' 'group' column should refer to a specific prediction column in the preliminary
#' output. See \code{\link{fcMerge}} for more details on format. If the
#' \code{merg} entry is a list, then each entry in the list is treated as
#' as separate merger and must have the appropriate format.
#'
#' Each list entry can be (read 'should be') named. If it is named, then the
#' new column in the predictions output will have the name given the list entry.
#' If only a data frame is supplied, then the default name of "merge" will be used.
#' For that reason, the list format is preferred, even if only one merger is
#' done.
#'
#' As currently written, this will only merge numeric columns. If you want to merge
#' non-numeric columns, the merger will have to be performed outside of this function.
#'
#' This returns the output data frame with the new merged column and with the old
#' constituent columns removed. However, since the removal of the old columns is
#' performed last, it is possible to use the same source column for multiple final
#' merged columns.
#'
#' The \code{merg} entry is only processed if \code{do.predictions} is
#' \code{TRUE}.
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
#' \code{msg.name} columns. Also, the \code{msg.name} column is now deprecated, so
#' do not rely on it.
#'
#' @export
#'
#' @return
#' returns a list with the following entries:
#'
#' \describe{
#' \item{models.run}{The \code{models.run} input listing the models run by risk level.}
#'
#' \item{bypass.models}{The input \code{bypass.models} value.}
#'
#' \item{do.predictions}{The input \code{do.predictions} value.}
#'
#' \item{roll.up.2.dept}{The input \code{roll.up.2.dept} value.}
#'
#' \item{object.list}{The \code{object.list} object described above. If the
#'    \code{bypass.models} flag is set, then this is the object supplied to the
#'    function. Otherwise it is returned by the calls to \code{\link{fcMacro}}.}
#'
#' \item{msg.name}{Name of the message file to which messages are sent.}
#'
#' \item{prediction}{Data frame containing predictions for all variables requested
#'    in the models.run object. The predictions are either by census tract or by
#'    department depending on the value of the \code{roll.up.2.dept} flag.}
#'
#' \item{risk.results}{This is a list, with an entry for each risk level. Each entry
#'    contains a data frame with the raw estimates for that risk level.
#'    For low and medium risk fires this contains the predictions at the
#'    census tract level (which are redundant with the results in
#'    \code{predictions} if \code{roll.up.2.dept} is FALSE). For high risk
#'    fires, this contains predictions at the parcel level.}
#' }
#'
full_analysis <- function(conn=NULL,
                          refresh.data=FALSE,
                          models.run=NULL,
                          merg=NULL,
                          bypass.models=FALSE,
                          do.predictions=TRUE,
                          do.rmse=FALSE,
                          roll.up.2.dept=TRUE,
                          object.list=NULL,
                          incl.detail=FALSE){
#
  gc()
  src.names <- c( lr="low.risk.fires",
                  mr="med.risk.fires",
                  hr="high.risk.fires",
                  ems500="ems.500",
                  emscty="ems.cnty")
  src.tabls <- c( lr="select * from nist.low_risk_fires",
                  mr="select * from nist.med_risk_fires",
                  hr="select * from nist.high_risk_fires",
                  ems500="select * from nist.ems_table_500",
                  emscty="select * from nist.ems_table_cnty")
  est.names <- c( lr="lr.mr.pred",
                  mr="lr.mr.pred",
                  hr="hr.pred",
                  ems500="ems.pred.5",
                  emscty="ems.pred.c")
  est.tabls <- c( lr="select * from nist.lr_mr_pred",
                  mr="select * from nist.lr_mr_pred",
                  hr="select * from nist.hr_pred",
                  ems500="select * from nist.ems_500_pred",
                  emscty="select * from nist.ems_cty_pred")
  old.ls <- c(ls(pos=globalenv()), "rmse.sum")
# Handle default values of inputs and check for validity
  if(is.null(conn)){
    conn <- RPostgreSQL::dbConnect( "PostgreSQL",
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
    models.run <- list(lr=c("npt.final"),
                       mr=c("mr.final"),
                       hr=c("hr.final"),
                       ems500=c("ems.5.final"),
                       emscty=c("ems.C.final")
                       )
# If models.run is NULL AND merg is NULL, then assume the user wants
# the default values for 'merg' as well.
    if(is.null(merg)){
      merg <- list(
        ems=RPostgreSQL::dbGetQuery(conn, "select * from nist.ems_groups")
      )
    }
  }
  if(identical(merg, 0)) merg <- NULL
  if(length(setdiff(unique(models.run$risk), c("lr", "mr", "hr", "ems500", "emscty"))) > 0){
    stop( "The only classes allowed in 'models.run' are 'lr', 'mr', 'hr', 'ems500', and 'emscty'!")
#   Do I want to stop the analysis or simply skip any wrong ones with an warning?)
  }
    roll.up.2.dept <- roll.up.2.dept && do.predictions
  if(bypass.models & is.null(object.list)){
    stop("If bypass.models is TRUE then objects.list must be defined!")
  }
  openLog()
  if(! bypass.models){
    if(! is.null(object.list)) msgOut("'bypass.models' is FALSE and 'object list' is defined. 'object.list' will be overwritten.",
                                      type="warning")
    object.list <- list()
  }
# if necessary, convert a models.run data.frame to a more congenial list.
  if(is.data.frame(models.run)){
    models.run0 <- list()
    for(i in unique(models.run$risk)){
      models.run0[[i]] <- models.run$lst[models.run$risk == i]
    }
    models.run <- models.run0
    rm(models.run0)
  }
  if(refresh.data) msgOut("The 'refresh.data' feature has not been implemented yet.",
                          type="warning")
# if(refresh.data) refresh_views(conn)
#
# Here we begin the actual analysis
#
  risk.results <- list()
  for(i in names(models.run)){
    setContext(paste0("Risk Group ", i))
    msgOut("Running Risk Group", type="status")
    gc()
    src.name <- src.names[i]
    if(bypass.models){
# If bypass.models is defined, then we need to get the relevant 'objects' data frame
# If it does not exist in object.list then skip to the next risk level.
      if(i %in% names(object.list)){
        objects <- object.list[[i]]
      } else {
        msgOut(paste0("bypass.models=TRUE and no entry exists for model group '", i,
                      "' in the 'object.list' object. It will by skipped."),
               type="warning")
        next
	    }
    } else {
# Download the data needed for the model estimation
      msgOut("Getting Data", type="start")
      if(! exists(src.name) | refresh.data){
        tryCatch(
          {
            assign(src.name, RPostgreSQL::dbGetQuery(conn, src.tabls[i]), pos=globalenv())
            assign(src.name, fcSetup(get(src.name)), pos=globalenv())
          },
          error=function(x){
            msgOut(paste0("Cannot Download Data. Skipping risk group: ", x$message),
                   type="error")})
      }
      if(! exists(src.name)) next
        msgOut("Elapsed Time: ", type="stop")
# Create the control objects for the models...
      models  <- mass.npt(conn, list=models.run[[i]])
# And estimate the models.
      objects <- fcMacro(models, do.rmse=do.rmse)
	    object.list[[i]] <- objects
    }
#
    if(do.predictions){
      est.name <- est.names[i]
      if(! exists(est.name) | refresh.data){
        msgOut("Getting Prediction Data", type="start")
        assign(est.name, RPostgreSQL::dbGetQuery(conn, est.tabls[i]))
        assign(est.name, fcSetup(get(est.name)))
        msgOut("Elapsed Time: ", type="stop")
      }
# Basically what I am going to do here is call fcEstimate. Note that
# fcEstimate can take lists of control and output objects and work on
# them as a unit. However, since the output objects can be quite large,
# I am doing them one at a time so as not to blow up the machine.
      for(j in 1:nrow(objects)){
        setContext(paste0(getContext(), "; ", objects$npt.name[j]))
        msgOut("Predicting", type="start")
        tryCatch(
          {
            e <- new.env()
            load(objects$save.name[j], e)
            with(objects, assign(npt.name[j], get(npt.name[j], e), pos=globalenv()))
            with(objects, assign(res.name[j], get(res.name[j], e), pos=globalenv()))
            rm(e)
            temp.98765 <- fcEstimate(objects$npt.name[j],
                                     objects$res.name[j],
                                     get(est.name),
                                     subset=quote(fd_size %in% paste0("size_", 3:9)))
# As written it is possible for some results to have the census tract column called
# 'geoid' in some cases and 'tr10_fid' in others. That would cause problems merging
# data sets.  The team wants that column called 'tr10_fid' to be consistent with
# their work. This takes care of that.
            if( "geoid" %in% names(temp.98765)){
              names(temp.98765)[match("geoid", names(temp.98765))] <- "tr10_fid"
            }
# Similarly, some versions list department as 'fd_id' while other use 'fc_dept_id'.
# This makes them consistent.
            if( "fd_id" %in% names(temp.98765)){
              names(temp.98765)[match("fd_id", names(temp.98765))] <- "fc_dept_id"
            }
# Here I get a list of the names of the prediction columns.
            dta.cols <- setdiff(names(temp.98765), c("geoid", "tr10_fid", "fd_id", "fc_dept_id", "parcel_id", "year", "geoid_source"))
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
          },
          error=function(x) msgOut(x$message, type="error"))
        msgOut("Elapsed Time: ", type="stop")
      }
#
# Since the high-risk results are at the parcel level, and all the rest of the data
# is at the census tract level, I need to roll up the hr data to the census tract level.
      if(i == 'hr'){
        temp.98765 <- risk.results[["hr"]]
        risk.results[["hr"]] <- rollUp2Dept(temp.98765,
                                            "hr.fires",
                                            "hr.sz2",
                                            "hr.sz3",
                                            c("year", "tr10_fid", "fc_dept_id"))
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
        predictions <- merge(predictions,
                             risk.results[[i]][,-match("year", names(risk.results[[i]]))],
                             all=TRUE)
      } else {
        predictions <- risk.results[[i]][,-match("year", names(risk.results[[i]]))]
      }
# I want to leave the intermediate results, mainly for the high-risk and ems data,
# so they can be inspected. So we return risk.results[['hr']] to its former self,
# and delete the intermediate table.
      if(i == 'hr'){
        risk.results[["hr"]] <- temp.98765
        rm(temp.98765)
      }
    }
  }

# Now deal with the "merg" option.
# We wait till all the predictions are complete before running
# the merger operation(s) for a couple of reasons. First, this
# ensures that the intermediate results will be returned (if requested).
# Second, this allows us to merge across merge groups, and in particular
# across the ems500 and emscty groups, which is the entire purpose of
# this option
  if(do.predictions & ! is.null(merg)){
    setContext("Mergers")
    if(! is.list(merg)){
      merg <- list(merg)
      names(merg) <- "merge"
    }
    cols <- NULL
    new.preds <- list()
    for(i in 1:length(merg)){
      gd <- grep("tr10_fid|geoid", names(merg[[i]]))[1]
      if(is.na(gd)){
        msgOut(paste0("Merger ", names(merg)[i], " does not have a geoid column. It will be skipped"),
               type="warning")
      } else {
        tryCatch(
          {
            tmp <- fcMerge(predictions,
                           merg[[i]]$group[match(predictions$tr10_fid, merg[[i]][[gd]])])
            cols <- union(cols, tmp$cols)
            new.preds[[names(merg)[i]]] <- as.numeric(tmp$result)
          },
          error=function(x) msgOut(x$message, type="error"))
      }
    }
    if(! is.null(cols)){
      predictions <- predictions[,-cols]
      predictions <- cbind(predictions, as.data.frame(new.preds))
    }
    rm(tmp, new.preds, cols, gd)
  }
# If requested, roll the data up to the department level.
if(roll.up.2.dept){
    fire.col <- grep("fire", names(predictions), value=TRUE)
    sz2.col  <- grep("sz2",  names(predictions), value=TRUE)
    sz3.col  <- grep("sz3",  names(predictions), value=TRUE)
    predictions <- rollUp2Dept(predictions, fire.col, sz2.col, sz3.col, "fc_dept_id")
  }
# Return the results
  out <- list(models.run=models.run,
              bypass.models=bypass.models,
              do.predictions=do.predictions,
              roll.up.2.dept=roll.up.2.dept,
              object.list=object.list)
  if(! is.null(merg)) out$merg <- merg
  if(do.predictions){
    out[["prediction"]] <- predictions
    if(incl.detail) out[["risk.results"]] <- risk.results
  }
  out
}
