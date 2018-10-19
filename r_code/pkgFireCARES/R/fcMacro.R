##  fcMacro.R
#' Run a set of control objects
#'
#' This function takes a list of control objects and goes through the steps
#' needed to run the tests for those control objects, and save the outputs.
#'
#' @param npt character vector. Lists names of the control objects to be run.
#' @param conn DBI connection. Connection to the database containing the
#' 'controls' sets. Only needed if the control objects in npt above are not
#' on the command line. [optional]
#' @param do.rmse logical. If TRUE (the default) it will calculate the out-of-sample
#' RMS errors.
#' @param save.tests environment. Where to save the test results. [optional]
#'
#' @details
#' The object listed in \code{npt} need not exist in the R environment. If it
#' does not, then this function calls the \code{\link{npt}} function to get
#' the definition of the control object out of the database. That is what the
#' \code{conn} variable is used for. If the objects already exist in the R
#' environment, then there is no need to supply the \code{conn} variable.
#'
#' This routine sequentially runs \code{\link{fcRun}} and \code{\link{fcTest}}
#' for all the test objects supplied. It then collects summary information from
#' the \code{\link{fcTest}} output for all the control objects listed and
#' returns it in the list \code{rmse.sum} which is created in the global environment.
#'
#' The function saves the control, output, and test objects to disk and
#' deletes them from the R environment. This step is necessary because some of
#' output objects are quite large.
#'
#' The function creates a message text file on disk for each control object so
#' as to contain any errors or or messages generated while running the models.
#' The name of the message file is 'message.nn.txt' where nn is a two-digit
#' number. This function finds the message file with the largest such number
#' already on the disk and names the new file with the next-largest number.
#' Note that if a large number of such files already exist on the disk or if
#' fcMacro is called with a long list of control objects, it is possible for
#' the 'nn' in the name above to extend into triple digits.
#'
#' If the save.tests environment is supplied, then the \code{\link{fcTest}}
#' results (and only the \code{\link{fcTest}} results) will be retained in the
#' environment specified. The \code{\link{fcTest}} objects are small enough that
#' they can be retained without much harm. Note again that they are saved (if
#' they are saved) in a separate environment which keeps the global environment
#' less cluttered.
#'
#' Side Effects:
#'
#' \itemize{
#'    \item Creates \code{rmse.sum} in the global environment if \code{do.rmse}
#'          is \code{TRUE}.
#'    \item For each control object, creates a .RData file on the disk
#'          containing the objects created.
#'    \item For each control object, creates a message text file on disk
#'          containing any errors or messages generated in the process.
#'    \item Optionally saves the outputs of the \code{\link{fcTest}} function
#'          in a specified environment.
#' }
#'
#' \code{rmse.sum} is a named list. Each control object that produces a fcTest
#' object (many will not) has an entry in the list. The name of the entry is the
#' name of the control object. The entry is the .$se entry in the fcTest object.
#'
#' @return
#' Data frame listing the objects and files created for each control object.
#'
#' The data.frame has the following structure:
#'
#' \describe{
#'   \item{npt.name}{Name of the control object.}
#'   \item{res.name}{Name of the results output by \code{\link{fcRun}}.}
#'   \item{tst.name}{Name of the test results output by \code{\link{fcTest}}.}
#'   \item{msg.name}{Text file in which errors and messages associated with
#'                   this object are output. This is obsolete, and will soon
#'                   be deleted.}
#'   \item{save.name}{Name of the file to which all objects are saved.}
#' }
#'
#' Note that if an object is not created (typically due to an error) then an NA
#' will appear in the appropriate cell.
#'
#' @export
#' @examples
#' \dontrun{
#'   fcMacro(c("mr.final", "npt.final", "npt.final.L"))
#'
#'   res <- new.env()
#'   fcMacro(c("mr.final", "npt.final", "npt.final.L"), save.tests=res)
#' }
fcMacro <- function(npt, conn=NULL, do.rmse=TRUE, save.tests=NULL)
{
  openLog()
  ctxt <- getContext()
  for(i in 1:length(npt))
  {
# First this creates names for output objects and files. This is based on the name of the
# control object. Note that I do not test for conflicts in the names either with existing
# objects or existing files. So, user beware.
#
    npt.name <- npt[i]
    res.name <- paste( npt.name, "res", sep=".")
    tst.name <- paste( npt.name, "tst", sep=".")
    msg.name <- paste(getDests(), collapse=", ")
    save.name <- paste(npt.name, "RData", sep=".")

    setContext(c(ctxt, npt.name))

    if(exists("test.00") && "subset" %in% names(test.00)){
      sbset <- test.00$subset
    } else {
      sbset <- quote(include & fd_size %in% paste("size_", 3:9, sep=""))
    }
    msgOut(paste0("Output to '", save.name, "'"), type="status")
# Check to see if the control object exists in the R environment. If it does not, then
# create it from the database with a call to the 'npt' function.
    if(! exists(npt.name, where=globalenv())){
      if(! is.null(conn)){
        tryCatch({
          if(grepl("L", npt.name)){
            assign(npt.name, npt(conn, npt.name, run="L"), envir=globalenv())
          } else {
            assign(npt.name, npt(conn, npt.name), envir=globalenv())
          }
        },
        error=function(e) {
                msgOut(paste0("Cannot load input object '",  npt[i], "' (fcMacro)!:", e$message),
                                 type="error")})
      } else {
        msgOut(paste0("Input object '", npt[i], "' cannot be found (fcMacro)!", type="error"))
      }
    }
    if(! exists(npt.name, where=globalenv())) next
# Run the model
    fcRun(get(npt.name))
    if(exists("offset", where=globalenv(), inherits=FALSE)) rm(offset, pos=globalenv())
# Calculate out-of-sample RMSE
    if(do.rmse){
      msgOut("Calculating out-of-sample RMSE", type="start")
      tryCatch(assign(tst.name, fcTest(get(npt.name), out, subset=sbset), pos=globalenv()),
               error  =function(e) msgOut(e$message, type="error"))
      msgOut("Elapsed time: ", type="stop")
    }
    assign(res.name, out, pos=globalenv())
    rm(out, pos=globalenv())
# Save the output...In some cases this can take upwards of an hour.
    msgOut(paste0("Saving to file '", save.name, "'"), type="start")
    save(list=intersect(ls(pos=globalenv()), c(npt.name, res.name, tst.name)), file=save.name)
    msgOut("Elapsed time: ", type="stop")
# This collects the actual RMSE values and saves them to a summary list.
# As usual, I create and modify this in the global environment so that if
# something goes wrong (or it just takes too long and gets interrupted)
# all the previous work hasn't been lost.
    if(exists(tst.name, where=globalenv())){
      if(! exists("rmse.sum")) rmse.sum <<- list()
      rmse.sum[[npt.name]] <<- get(tst.name, pos=globalenv())$se
    }
# If save.tests is defined, collect the 'test.' objects and save them to the
# specified environment.
    if(! is.null(save.tests) & exists(tst.name, where=globalenv())){
      assign(tst.name, get(tst.name, pos=globalenv()), envir=save.tests)
    }
# Delete all added objects, and clean up. I include an explicit garbage-collection
# run because I have found that it makes a big difference.
    uu <- c(ifelse(exists(npt.name, where=globalenv()),npt.name,NA),
            ifelse(exists(res.name, where=globalenv()),res.name,NA),
            ifelse(exists(tst.name, where=globalenv()),tst.name,NA),
            msg.name,
            save.name)
    if(exists("result")) result <- rbind(result, uu)
		else                 result <- matrix(uu, ncol=5)
    rm(list=intersect(ls(pos=globalenv()), c(res.name, npt.name, tst.name)), pos=globalenv())
    rm(npt.name, res.name, tst.name, msg.name, save.name)
    gc()
  }
	result <- as.data.frame(result, stringsAsFactors=FALSE)
	row.names(result) <- NULL
  names(result) <- c("npt.name", "res.name", "tst.name", "msg.name", "save.name")
	result
}
