#' pkgFireCARES: A package for estimating community risk in FireCARES
#'
#' This package creates a series of functions that are used to
#' estimate community risk models and make community risk predictions.
#'
#' @section Functions:
#'
#'  Functions included are:
#'
#'  \code{\link{full_analysis}}: Runs through complete analysis (depending
#'  on the parameters set).
#'
#'  \code{\link{refresh_views}}: [This feature has not been implemented yet]
#'  Since the underlying data used to create the data sets for this analysis
#'  change, and the materialized views this analysis uses do not refresh
#'  automatically, this makes sure those views are current before continuing
#'  with the analysis.
#'
#'  \code{\link{fcCluster}}: Builds the cluster assignments for US Counties.
#'  So far, this is used exclusively for EMS analysis.
#'
#'  \code{\link{fcSetup}}: Takes data file (either for model estimation or
#' prediction) and prepare it for use.
#'
#'  \code{\link{npt}}: Builds a control object from the specified templates
#'  in the database.
#'
#'  \code{\link{mass.npt}}: Builds a collection of control objects. This
#'  function calls npt to do most of the work.
#'
#'  \code{\link{fcRun}}: Uses the control object to run a set of models.
#'
#'  \code{\link{fcTest}}: Calculates the out-of-sample Root-Mean-Square error
#'  on the results for the models in the supplied test object. This function
#'  works on output from \code{\link{fcRun}}.
#'
#'  \code{\link{fcMacro}}: For a supplied set of control objects, sequentially
#'  \code{\link{fcRun}}s them, runs \code{\link{fcTest}} on them, summarized the
#'  \code{\link{fcTest}} results in a single data.frame, and saves the results to
#'  disk.
#'
#'  \code{\link{naive}}: Takes a \code{\link{fcTest}} output and computes the
#'  naive estimator and the RMS Error for the naive estimator for that test
#'  object.
#'
#'  \code{\link{fcEstimate}}: Takes output from the \code{\link{fcRun}} routine
#'  and new data and computes predictions by tract or (for high-risk fires)
#'  Assessors Parcel.
#'
#'  \code{\link{fcMerge}}: Takes two or more separate predictions (from \code{\link{fcEstimate}})
#'  and combines them according to a rule specified. Primarily used with EMS.
#'
#'  \code{\link{rollUp2Dept}}: Takes output from the \code{\link{fcEstimate}} routine
#'  sums over census tracts to the department level.
#'
#'  \code{\link{lasso}}: Helper function for LASSO and ridge regression models.
#'
#'  \code{\link{ranger}}: Helper function for Random Forest models (using the
#'  \pkg{ranger} package).
#'
#'  \code{\link{c_test}}: Combines two test objects.
#'
#'  \code{\link{acs.dwnld}}: Downloads new ACS data from Census for import to
#'  the database. This should considerably simplify the process of keeping
#'  census data up to date. Note that it requires census API key installed
#'  (see the acs package documentation).
#'
#' @section Database Info:
#' This package works with data on the \code{nfirs} database on the FireCARES server.
#' In particular, it works with the information in the \code{nist} and \code{controls}
#' schemas. Most of that, however, is transparent to the functions in this
#' package. Any function (except \code{\link{full_analysis}}) that accesses the
#' database takes a DBI Connection as one of its parameters. That connection will
#'  contain all the connection parameters and must be supplied.
#'
#' Note that while I assume that the database is a PostgreSQL one (as is currently
#' the case), there is nothing in these functions (again, except for
#' \code{\link{full_analysis}}) that is specific to PostgreSQL. So any DBI connection
#' can be used. There are packages in R that create DBI connections for MySQL, SQLite,
#' Oracle, the ODBC Interface, SQLServer, and others. So these functions should
#' continue to work even if the server hosting the database is changed.
#'
#' Even \code{\link{full_analysis}} allows for a DBI connection object to be supplied.
#' So, if the database were to change, then the correct connection object could be
#' supplied without rewriting the package.
#'
#' @section Typical Workflow:
#'
#' This section takes you through the basic work flow that will typically
#' be followed in using this package. The function \code{\link{full_analysis}}
#' automates this process.
#'
#' \emph{Build county clusters} This is typically done with a call
#' to \code{\link{fcCluster}} and (so far) is only relevant to EMS risk.
#' Since the underlying data used to build county clusters only changes rarely,
#' it should only have to be done once every year or two.
#'
#' \emph{Refresh the views in the database.} This is typically done with a call
#' to \code{\link{refresh_views}} [which is not functional yet!!!!].
#'
#' \emph{Build the definitions of the models to be estimated.} That will
#' typically be done by a call to \code{\link{mass.npt}}, although it could be
#' done by calling \code{\link{npt}} directly. Either will leave one or more
#' control objects in the working environment.
#'
#' \emph{Download data for analysis.} That will need to be done separately if
#' \code{\link{full_analysis}} is not used.
#'
#' \emph{Prepare the data for analysis.} This is done by a call to
#' \code{\link{fcSetup}}.
#'
#' \emph{Estimate the models and calcuate the RMS Error for all the models
#' queued up for estimation.} That is typically done by a call to
#' \code{\link{fcMacro}}. However it can be done by sequentially calling
#' \code{\link{fcRun}} and \code{\link{fcTest}}, although that is not recommended.
#' Note that \code{\link{fcMacro}} takes all the objects created by either
#' \code{\link{fcRun}} or \code{\link{fcTest}}, saves them to file and deletes them
#' from the working enviroment. It leaves behind a summary data frame
#' summarizing the RMS Errors of the models run.
#'
#' \emph{Estimate the naive model for comparison.} To do that, you will need
#' the output of \code{\link{fcTest}} from one of the sets of models estimated,
#' and will use the \code{\link{naive}} function.
#'
#' \emph{Estimate predictions for each tract or parcel.} That occurs in three
#' steps. First, download the data to be used to make predictions. That will
#' occur outside any of these functions. Second, prepare the new data for
#' analysis. That occurs through a call to \code{\link{fcSetup}}. Finally,
#' compute the predictions based on the selected models. That occurs through
#' a call to \code{\link{fcEstimate}}.
#'
#' \emph{Optionally combine separate predictions into one.} In some cases
#' separate predictions apply to different portions of the prediction set.
#' A call to \code{\link{fcMerge}} will combine them.
#'
#' \emph{Optionally, roll the census tract predictions up to the department level.}
#' A call to \code{\link{rollUp2Dept}} completes this task.
#'
#' @section ACS Data:
#' These models and estimates rely on data from the American Community Survey
#' maintained by the Census Bureau. New data is released for the survey
#' annually. The function \code{\link{acs.dwnld}} is a utility function that
#' simplifies the process of downloading new data. In order to use it you will
#' need a Census API key installed on the server (see the \pkg{acs} package
#' documentation for more details). It leaves a set of tables on the server
#' (in the 'nist' schema) that are formatted the same as the master ACS tables
#' already on the server. Those tables will need to be appended to the existing
#' ACS tables already on the server.
#'
#' @importFrom magrittr %>% %$%
#'
#' @section IMPORTS:
#' acs,boot,glmnet,ranger,RPostgreSQL,utils,cluster,magrittr
#'
#' @section SUGGESTS:
#' doParallel
#'
#' @section Notes:
#' These functions do assume that the information they need is in the
#' 'controls' and 'nist' schemas, so if that changes, these functions will need
#' to be rewritten.
#'
#' The 'controls' schema is assumed to contain the definitions of all models
#' that are used by these functions. The format for the tables in the 'controls'
#' schema is very specific, and is hard-coded into these functions. There are
#' three tables assumed to exist in the 'controls' schema:
#' \code{models}, and \code{inputs} and \code{runs}. Their layouts are described
#' below.
#'
#' @section Table models:
#' This table specifies information about each model to be run.
#'
#' \tabular{lcl}{
#' Name    \tab Type    \tab Details\cr
#' index   \tab integer \tab primary key.\cr
#' lst     \tab text    \tab Typically the name of the control object.\cr
#' model   \tab text    \tab Name of the model to be estimated.\cr
#' library \tab text    \tab Name of the library needed to estimate the model.\cr
#' ff      \tab text    \tab Name of the function that estimates the model.\cr
#' target  \tab text    \tab Name of the dependent variable estimated.\cr
#' runs    \tab text    \tab One of '0', 'S', or 'L'. Whether the model is estimated over the whole
#'                           data set ('0' or 'S') or separately over subsets of the data ('L').
#' }
#'
#' @section Table inputs:
#' This table specifies the parameters for the model to be run.
#'
#' \tabular{lcl}{
#' Name    \tab Type    \tab Details\cr
#' index   \tab integer \tab primary key.\cr
#' lst     \tab text    \tab Typically the name of the control object.\cr
#' model   \tab text    \tab Name of the model to be estimated.\cr
#' input   \tab text    \tab Name of an input parameter for the estimation
#' function (ff above).\cr
#' class   \tab text    \tab Class of the input parameter.\cr
#' value   \tab text    \tab Value of the input parameter.
#' }
#'
#' Note that for practical purposes, the (lst, model) pair serve as keys
#' to the list of models, and they are a foreign key that the \code{inputs} table
#' uses to link to the \code{models} table.
#'
#' @section Table runs:
#' This table specifies how the data is partitioned. Each partition will
#' have a separate model built for it. Other than the partition, all other
#' inputs are identical.
#'
#' \tabular{lcl}{
#' Name    \tab Type    \tab Details\cr
#' grp     \tab text    \tab One of 'L', 'S', '0', or 'C'. This matches the \code{runs}
#' column in the \code{models} table.\cr
#' tier1   \tab text    \tab This combined with 'tier2' below serve as a name
#' for the subset to be evaluated.\cr
#' tier2   \tab text    \tab See above.\cr
#' value   \tab text    \tab Definition of the subset to be evaluated.
#' }
#'
#' @section Parallel Processing:
#' Both LASSO and Random Forest (through the \pkg{ranger} package) can use parallel
#' computation if multiple processors are available. The \pkg{ranger} package has
#' support for multiple processors built in by default. I have made no adjustment
#' to the defaults, so it will use them if they are there and the package supports
#' them. LASSO (through the \pkg{glmnet} package) can also use it, but setup is required.
#' LASSO here is set up to use the \pkg{doParallel} package if it is set up. Note that
#' for LASSO to use multiple processors, \pkg{doParallel} must be set up separately. That is, the
#' package must be installed and loaded (typically with a call to \code{\link[base]{library}})
#' in advance. It that is done (and works--doParallel only works on certain types of systems)
#' the LASSO will make use of it. If not, it will not.
#'
#' @examples
#' \dontrun{
#' conn <- dbConnect("PostgreSQL",
#'                   host="some.host.com",
#'                   dbname="nfirs",
#'                   user="user",
#'                   password="pwd")
#' low.risk.fires <- dbGetQuery(conn, "select * from nist.low_risk_fires")
#' low.risk.fires <- fcSetup(low.risk.fires)
#' med.risk.fires <- dbGetQuery(conn, "select * from nist.med_risk_fires")
#' med.risk.fires <- fcSetup(med.risk.fires)
#' high.risk.fires <- dbGetQuery(conn, "select * from nist.high_risk_fires")
#' high.risk.fires <- fcSetup(high.risk.fires)
#'
#' models <- mass.npt(conn, pattern="final")
#' tables <- fcMacro(models)
#' tables
#'
#' lr.mr.pred <- dbGetQuery(conn, "select * from nist.lr_mr_pred")
#' lr.mr.pred <- fcSetup(lr.mr.pred)
#'
#' e <- new.env()
#' npt.final <- e$npt.final
#' npt.final.res <- e$npt.final.res
#' lr.pred <- fcEstimate("npt.final",
#'                       "npt.final.res",
#'                       lr.mr.pred,
#'                       quote(fd_size %in% paste("size_", 3:9, sep="")))
#' head(lr.pred)
#' }
#'
#' @docType package
#' @name pkgFireCARES
##
## General Notes:
##  For reasons I do not understand, the doParallel package (used in
##  the LASSO models) does not load on the fly. So it needs to be loaded
##  separately before running the models.
##
## Consider at some point using the package glmnetUtils for glmnet. That may
##   considerably simplify the use of glmnet routines.
##
## Consider replacing some of the loops (and in particular the boot routines)
##   with foreach loops.
##
## Can I use the functionality in fcEstimate to replace the same functionality
## in fcTest??? I can't replace the entire function, but I may be able to replace
## the complicated part: the estimation of the out-of-sample values.
##
## Check dependencies and imports and get them properly arranged.
##
"_PACKAGE"
