##  FireCARES_1.R
##  This file creates a series of functions that are used to
##  estimate models, build control objects, perform some testing
##  on the models and make predictions. It also includes a few 
##  other helping functions.
##
##  Note that I am using a commenting standard for functions that
##  is intended to work with the roxygen2 package. At some point I 
##  will likely turn this into a package, and then that format will
##  ease the building of documentation for the package.
##
##  For reasons I do not understand, the doParallel package (used in
##  the LASSO models) does not load on the fly. So it needs to be loaded
##  separately before running the models.
##
##  Functions included are:
##    fcSetup:   Takes data file (either for model estimation or prediction)
##               and prepare it for use.
##    npt:       Builds a control object from the specified templates
##               in the database.
##    mass.npt:  Builds a collection of control objects. This function
##               calls npt to do most of the work.
##    fcRun:     Uses the control object to run a set of models.
##    fcTest:    Calculates the out-of-sample Root-Mean-Square error
##               on the results for the models in the supplied test object.
##               This function works on output from 'run'
##    fcMacro:   For a supplied set of control objects, sequentially 'runs'
##               them, runs 'test' on them, summarized the 'test' results 
##               in a single data.frame, and saves the results to disk.
##    naive:     Takes a 'test' output and computes the naive estimator
##               and the RMS Error for the naive estimator for that
##               test object.
##    fcEstimate:Takes output from the 'run' routine and new data and
##               computes predictions by tract or (for high-risk fires)
##               Assessors Parcel.
##    lasso:     Helper function for LASSO and ridge regression models.
##    ranger:    Helper function for Random Forest models (using the 
##               ranger package).
##    c.test:    Combines two test objects.
##    acs.dwnld: Downloads new ACS data from Census for import to the 
##               database. This should considerably simplify the process
##               of keeping census data up to date. Note that it requires
##               a census API key installed (see the acs package documentation).
##
##  IMPORTS:  acs,boot,glmnet,ranger,RPostgreSQL,utils
##  SUGGESTS: doParallel

#' Mass Building of Control Objects
#'
#' This uses a pattern to collect a set of control files, and then calls 'npt' 
#' for each of those control files.
#'
#' @param base character. Used to pattern-match the 'lst' values in the control 
#' list.
#' @param conn DBI connection. Connects to the database containing the 
#' 'controls' sets.
#' @param relocate environment. This is an [optional] environment into which
#' to move any existing test objects.
#'
#' @return Vector listing the control objects created.
#'
#' @details
#' Creates:
#'   A set of control objects in the global environment, as specified in the 
#'  'base' input.
#'
#' This is usually followed up with a call to \code{\link{macro}}
#'
#' @export
#' @examples
#' dontrun{
#'   conn <- dbConnect("PostgreSQL", host="hostname.com", dbname="nfirs", user="username", password="***")
#'   mass.npt("npt.f", conn)
#'   res <- new.env()
#'   mass.npt("final", conn, res)
#' }
mass.npt <- function(base, conn, relocate=NULL)
{
#   This section finds any 'test.' objects and moves them into a user-specified
#   backup environment. This makes room for the new objects that will shortly be 
#   created.
	if(is.environment(relocate)){
        a <- setdiff(ls(pattern="test.", envir=globalenv()), "test.00")
        for(i in a) {
            assign(i, get(i), envir=relocate)
            rm(list=i, envir=globalenv())
        }
    }
#   Find the 'lst' values, and then create the input files for each of the control lists.
    a <- dbGetQuery(conn, paste("select lst, replace(runs, '0', 'S') as runs, count(*) as n ", 
	                              "from controls.models where lst like '%", base, "%' ", 
								  "group by lst, replace(runs, '0', 'S') order by lst", sep=""))
    for(i in 1:nrow(a)) {
        assign(a$lst[i], pkgFireCARES::npt(conn, a$lst[i], run=ifelse(a$runs[i] == "L", "long", "short")), envir=globalenv())
    }
	a$lst
}




#' Create control objects
#'
#' This function creates the specified control objects from the templates
#' maintained in the database.
#'
#' @param conn  DBI Connection. Connects to the database containing the 
#' controls tables.
#' @param group character The entry in the 'lst' column. This determines which 
#' models get used.
#' @param risk character. This along with 'y' and 'mdls' (and 'run') represent an 
#' alternative way of specifying which models get used. If 'group' is 
#' specified, this is ignored. This is risk category, and is one of 'l' 
#' (for low risk properties), 'm' (for medium risk properties), or 'h' (for 
#  high risk properties).
#' @param y character. This is the target variable, and is one of 'f' 
#' (for fires), 'j' (for injuries), 'd' (for deaths), 'sz2' (for "size 2" 
#' fires), or 'sz3' (for "size 3" fires).
#' @param mdls character This is a character vector of the models to be 
#' included for that target variable.
#' @param run character This takes one of two values: 'short' and 'long'. This 
#' determines whether a single model is run for all department sizes and 
#' regions (typically with dummies for each), or whether separate models are 
#' run for each combination of department size and region.
#'
#' @return control object
#'
#' @details
#'  As written, this creates a single control object regardless of whether
#'  multiple groups are specified.
#'
#'  If multiple groups are specified, only the first is processed. If y and
#'  mdl are specified, only the first y value (and the first 'runs' value)
#'  is processed. The multiple models in the y; mdl formulation are all
#'  added to a single control object, so be careful. It is easy to build a
#'  control object that will produce an output file so large it will choke
#'  the computer.
#'
#' @examples
#' dontrun{
#'   npt(conn, "npt.base")
#'   npt(conn, "npt.base", run="long")
#'   npt(conn, y="f", mdls=c("", ""))
#'   npt(conn, y="d", mdls=c("", ""), run="long")
#' }
#'
npt <- function(conn, group=NULL, risk=NULL, y=NULL, mdls=NULL, run="short") 
{
# Initially, we check to make sure the inputs make sense.
    if(is.null(group)){
        if(is.null(risk)) stop("at least one of 'group' and 'risk' must be specified")
        if(is.null(mdls) | is.null(y)) stop("'risk', 'y' and 'mdls' must all be specified")
        if(! (is.character(risk) & is.character(y) & is.character(mdls))) stop("'risk', 'y' and 'mdls' must be character vectors")
        if(length(risk) > 1){
            simpleWarning("risk has more than one value. Only the first will be used")
            risk <- risk[1]
        }
		if(! risk %in% c('l', 'm', 'h')) stop("risk must be one of 'l', 'm', or 'h'")
        if(length(y)	> 1){
            simpleWarning("y has more than one value. Only the first will be used")
            y <- y[1]
        }
		if(! y %in% c('f', 'j', 'd', 'sz2', 'sz3')) stop("y must be one of 'f', 'j', 'd', 'sz2' or 'sz3'")
	}
    else{
        if(! class(group) == "character") stop("If you use 'group' it must be a character vector")
        if(length(group)	> 1){
            simpleWarning("group has more than one value. Only the first will be used")
            group <- group[1]
        }
    }
    if(length(run) > 1){
        simpleWarning("run has more than one value. Only the first will be used")
        run <- run[1]
    }
	if(! run %in% c("short", "long")) stop("run must be one of 'short' or 'long'")
# Now we build the 'runs' portion of the object
# Note that this has changed, and the format of the output file has changed with it.
# As a result, some earlier control objects will no longer be compatible.
    r0 <- dbGetQuery(conn, paste("select * from controls.runs where grp = '", run, "' order by tier1, tier2", sep=""))
    r <- list()
    r0$tier.names <- with(r0, ifelse(is.na(tier2), tier1, paste(tier1, tier2, sep=".")))
    for(i in 1:nrow(r0)) {
        r[[r0$tier.names[i]]] <- parse(text=r0$value[i])[[1]]
    }

# Now select the input information used for the models to be built as part of this control object
    if(! is.null(group)) {
        mdl  <- dbGetQuery(conn, paste("select * from controls.models where lst in ('", paste(group, collapse="', '"), "')", sep=""))
        npts <- dbGetQuery(conn, paste("select * from controls.inputs where lst in ('", paste(group, collapse="', '"), "')", sep=""))
    } else {
	    lst.check <- 'npt'
		if(risk != 'l') lst.check <- paste(risk, "r", sep="")
        mdl  <- dbGetQuery(conn, paste("select * from controls.models where target='", y, "' AND ",
                                         "split_part(lst, '.', 1) = '", lst.check, "' AND ",
                                         "model in ('", paste(mdls, collapse="', '"), "') AND ",
                                         "runs in (", ifelse(run=="long", "'L'", "'S', '0'"), ")", sep=""))
        npts <- dbGetQuery(conn, paste("select * from controls.models JOIN controls.inputs USING (lst, model) ",
		                                 "where target='", y, "' AND split_part(lst, '.', 1) = '", lst.check, "' AND ",
                                          "model in ('", paste(mdls, collapse="', '"), "') ",
										 "AND runs in (", ifelse(run=="long", "'L'", "'S', '0'"), ")", sep=""))
    }
# For reasons I don't yet understand, when I run a lasso or random forest model, all subsequent glm models fail to run.
# This section ensures that any random forest models are run last.
    if(nrow(mdl[mdl$library %in% c('glmnet', 'ranger'),]) > 0) {
        mdl <- rbind(rbind(mdl[! mdl$library %in% c('glmnet', 'ranger'),], mdl[mdl$library == 'glmnet',]), 
                      mdl[mdl$library == 'ranger',])
    }
# This section takes the data.frames returned from the 'controls' database and translates them into named control structures.
    models <- NULL
# Cycle through all the models to be run.
    for(i in 1:nrow(mdl)) {
# Add the basic structure for each model
        models[[mdl$model[i]]] <- list(fn=c(library=mdl$library[i], ff=mdl$ff[i]), inputs=list())
# Build the list of inputs in a form ready to use by 'do.call'
# This translates most inputs into R names rather than actual data. That makes the built call readable.
        npt0 <- subset(npts, lst==mdl$lst[i] & model==mdl$model[i])
        for(j in 1:nrow(npt0)) {
            if(npt0$class[j] == "call") 
                models[[i]]$inputs[[npt0$input[j]]] <- parse(text=npt0$value[j])[[1]]
            else if(npt0$class[j] == "formula")
                models[[i]]$inputs[[npt0$input[j]]] <- as.formula(npt0$value[j], env=.GlobalEnv)
            else
                models[[i]]$inputs[[npt0$input[j]]] <- do.call(paste("as", npt0$class[j], sep="."), list(x=npt0$value[j]))
        }
    }
    list(models=models, runs=r)
}




#' Run a set of control objects
#'
#' This function takes a list of control objects and goes through the steps 
#' needed to run the tests for those control objects, and save the outputs.
#'
#' @param npt character vector. Lists names of the control objects to be run.
#' @param conn DBI connection. Connection to the database containing the 
#' 'controls' sets. Only needed if the control objects in npt above are not
#' on the command line. [optional]
#' @param save.tests environment. Where to save the test results. [optional]
#'
#' @details
#' The object listed in \code{npt} need not exist in the R environment. If it does not, then
#' this function calls the \code{\link{npt}} function to get the definition of the control object 
#' out of the database. That is what the 'conn' variable is used for. If the objects
#' already exist in the R environment, then there is no need to supply the \code{conn} variable.
#'
#' If the save.tests environment is not supplied, then the test results will be removed 
#' from the R environment after they are saved to disk (and a summary saved to a data.frame). 
#' Some of these objects can be very large, so removing them is largely necessary. The \code{test} 
#' objects are the only ones saved. They are small enough that they can be saved without 
#' much harm. Note again that they are saved (if they are saved) in a separate environment 
#' which keeps the global environment less cluttered.
#'
#' Creates rmse.sum in the global environment.
#'
#' @return nothing
#' @export
#' @examples
#' dontrun{
#'   fcMacro(c("mr.final", "npt.final", "npt.final.L"))
#'
#'   res <- new.env()
#'   fcMacro(c("mr.final", "npt.final", "npt.final.L"), save.tests=res)
#' }
fcMacro <- function(npt, conn=NULL, save.tests=NULL)
{
    for(i in 1:length(npt))
    {
# First this creates names for output objects and files. This is based on the name of the
# control object. Note that I do not test for conflicts in the names either with existing 
# objects or existing files. So, user beware.
#
        npt.name <- npt[i]
        res.name <- sub(strsplit(npt.name, ".", fixed=TRUE)[[1]][1], "results", npt.name)
        tst.name <- sub(strsplit(npt.name, ".", fixed=TRUE)[[1]][1], "test",    npt.name)
        msg.name <- paste("messages", formatC(i, width=2, flag="0"), "txt", sep=".")
        save.name <- paste(npt.name, "RData", sep=".")

        if(exists("test.00") && "subset" %in% names(test.00)){
            sbset <- test.00$subset
        } else {
            sbset <- quote(include & fd_size %in% paste("size_", 3:9, sep=""))
        }
        if(interactive()) cat("\nRunning '", npt.name, "'; Output to '", save.name, "':\n", sep="")
# Check to see if the control object exists in the R environment. If it does not, then 
# create it from the database with a call to the 'npt' function.
        if(! exists(npt.name, where=globalenv())){
            if(! is.null(conn)){
                if(grepl("L", npt.name)){
                    assign(npt.name, pkgFireCARES::npt(conn, npt.name, run="long"), envir=globalenv())
                } else {
                    assign(npt.name, pkgFireCARES::npt(conn, npt.name), envir=globalenv())
                }
            } else {
                if(interactive()) cat("\tNo such object exists!\n")
                next
            }
        }
# Run the model
        pkgFireCARES::fcRun(get(npt.name), sink=msg.name)
        if(exists("offset", where=globalenv(), inherits=FALSE)) rm(offset, pos=globalenv())
# Calculate out-of-sample RMSE
        if(interactive()) cat("Calculating out-of-sample RMSE: ", sep="")
        tm <- tryCatch(system.time(assign(tst.name, pkgFireCARES::fcTest(get(npt.name), out, subset=sbset), pos=globalenv())),
            error  =function(e){
                cat("ERROR   Testing Object: ", res.name, ". Message: ", e$message, "\n", sep="", 
                     file=ifelse(is.null(msg.name), stderr(), msg.name))
                return(rep(0, 5))
            }
)
        if(interactive()) cat("Elapsed time: ", tm[3], " secs\n", sep="")
        assign(res.name, out, pos=globalenv())
        rm(out, pos=globalenv())
# Save the output...In some cases this can take upwards of an hour.
        if(interactive()) cat("Saving to file ", save.name, ": ", sep="")
        tm <- system.time(save(list=intersect(ls(pos=globalenv()), c(npt.name, res.name, tst.name)), file=save.name))
        if(interactive()) cat("Elapsed time: ", tm[3], " secs\n", sep="")
# This collects the actual RMSE values and saves them to a summary list.
# As usual, I create and modify this in the global environment so that if 
# something goes wrong (or it just takes too long and gets interrupted)
# all the previous work hasn't been lost.
        if(exists(tst.name, where=globalenv())){
            if(! exists("rmse.sum")) rmse.sum <<- list()
            colname <- strsplit(tst.name, ".", fixed=TRUE)[[1]]
            colname <- paste(colname[2], ifelse(substring(colname[3], 1, 1)=='0', 'S', substring(colname[3], 1, 1)), sep=".")
            rmse.sum[[colname]] <<- get(tst.name, pos=globalenv())$se
        }
# If save.tests is defined, collect the 'test.' objects and save them to the 
# specified environment.
        if(! is.null(save.tests) & exists(tst.name, where=globalenv()))
        {
            assign(tst.name, get(tst.name, pos=globalenv()), envir=save.tests)
        }
# Delete all added objects, and clean up. I include an explicit garbage-collection
# run because I have found that it makes a big difference.
        rm(list=intersect(ls(pos=globalenv()), c(res.name, npt.name, tst.name)), pos=globalenv())
        rm(npt.name, res.name, tst.name, msg.name, save.name)
        gc()
    }
}



#' Fit models described in the supplied control objects
#'
#' The function takes the control object specifying a series of regression
#' models and runs those models. 
#'
#' @param sets Control Object The control object describing the models to run.
#'  This will typically be generated by 'npt'
#' @param n    Integer. Number of bootstrap replications to run in order to 
#' estimate the confidence intervals on parameters. n=0 (the default) will not
#' run any bootstrap replicates.
#' @param sink  Character. Specifies the name of the text file to send error
#' messages to. [optional]
#'
#' @return NULL
#'
#' @details
#' Creates: an \code{out} object listing output of the models.
#'
#' The reason that \code{out} is created in the global environment rather than
#' returned as an object is that if one of the models errors out, I still 
#' get the results of the previous models.
#'
#' This will typically be followed up by a run of \code{\link{test}}
#' @export
#' @examples
#' dontrun{
#'   fcRun(mr.d.00, sink="messages.13.txt")
#'   fcRun(mr.f.S0b, n=1000, sink="messages.08.txt")
#'   fcRun(mr.j.L0a)
#' }
fcRun <- function(sets, n=0, sink=NULL) 
{
# In each model, 'library' specfies the R library that will be needed to run the model.
# Here I extract a list of all those libraries and load them.
	for(i in unique(sapply(sets$models,  function(x) x$fn['library']))) loadNamespace(i)

    if(interactive()) u <- Sys.time()
    out <<- list()
# This sets up my 'error' sink file, if 'sink' is correctly specified.
    if(! is.null(sink))
    {
        if(is.character(sink)) ff <- file(sink, "w")
        else
        {
            warning("the 'sink' term must be a character")
            ff <- NULL
        }
    }
    else ff <- NULL

# Now we iterate through the models.
    for(k in names(sets$models))
    {
# I don't know why I allow for a 'null' library, but I do. Any model with
# a null library is skipped and no analysis is run.
        if(tolower(sets$models[[k]]$fn['library']) == "null") next

# Create the output slot for model output.
        out[[k]] <<- list()
# Here I extract the name of the function to be used.
# In a rare cases, I may specify the function with a '::' (or possibly 
# '$') value. That means that the actual function used is not the one that
# R would normally pick. This really only applies to lasso and ranger
# models (and really, only ranger, since there is no 'lasso' function
# in the glmnet package). For normal usage, this is not necessary. 
# This will only be needed if a user wanted to call the 'ranger' function 
# directly rather than through the interface set up here.
        fn  <- sets$models[[k]]$fn['ff']
        if(grep("::|[$]", fn) > 0){
            env <- strsplit("::|[$]", fn)[[1]]
			fn  <- env[2]
			env <- env[1]
        }
# Pull the inputs. Subset is handled differently, so extract it. Pull the 
# (name of the) data.
        aa <- a <- sets$models[[k]]$inputs
        subset.a <- a$subset
        a$subset <- NULL
        data <- a$data
# In some versions, different models are run for different subsets of the 
# data. the sets$runs section of the control object determines this. Here 
# we iterate through each subset of the data which will have its own model.
#
# Note that in a previous version of this function, I allowed runs to be 
# nested. I have deleted the functionality that handled the nested runs.
# That means there are some control objects out there that will not work
# with this version of the run function. However, any control object produced
# by the current version of npt should work.
        for(i in names(sets$runs))
        {
            if(interactive()) u[1] <- Sys.time()
            if(interactive()) cat("Evaluating ", k, format(" model: ", width=16 - nchar(k)), i, format(":", width=16 - nchar(i)), sep="")
# Create the space where the model results will go, and build the subset name 
# for this specific (sub-)model.
            out[[k]][[i]] <<- list()
            aa$subset <- substitute(u & v & set %in% c("training", "validation"), list(u=sets$runs[[i]], v=subset.a))
# Run the (sub-)model
# Since this is set up to run multiple models in batches, catch any errors
# and route them to the error file with enough information to identify which
# model the error was for.
# This will skip any models that error out, and will continue processing the
# remaining models.
            tryCatch(
                {
                    if(exists("env")){
                        out[[k]][[i]]$model <<- do.call(fn, aa, envir=getNamespace(env), quote=TRUE)
                    } else {
                        out[[k]][[i]]$model <<- do.call(fn, aa)
                    }
                },
                error  =function(e) cat("ERROR   in Model: ", k, ", run ", i, ". Message: ", e$message, "\n", sep="", file=ifelse(is.null(ff), stderr(), ff)),
                message=function(e) cat("MESSAGE in Model: ", k, ", run ", i, ". Message: ", e$message, "\n", sep="", file=ifelse(is.null(ff), stderr(), ff))
    )
# If I have asked for bootstrapping of the results, then run the bootstrap.
# This starts by subsetting the data according to the subset defined above,
# then it runs the bootstrap.
            if(n > 0)
            {
                dta <- do.call("subset", list(x=data, subset=aa$subset))
                if(interactive()) pb <- winProgressBar(title=paste("Bootstrapping ", k, " model: ", i, " ", j, ": ", n, " iterations", sep=""), label="0", max=n)
                    out[[k]][[i]]$boot <<- boot(dta, bbb, R=n, strata=dta$fd_id, a=a, ff=ff, fn=fn, pb=pb, nme=names(fixef(out[[k]][[i]]$model)))
                if(interactive()) close(pb)
            }

            if(interactive()) u[2] <- Sys.time()
            if(interactive()) cat("Elapsed time:", format(u[2] - u[1]), "\n")
        }
    }
    if(! is.null(ff)) close(ff)
}




#' LASSO helper function 
#'
#' This is a helper function that \code{\link{run}} calls whenever a LASSO model is used.
#'
#' @param formula Formula. This describes the model that the LASSO fits.
#' @param data Data Frame. The data used for the model.
#' @param subset Name. This defines the subset of the data the model is 
#' evaluated over.
#' @param ... Additional parameters to the \code{\link[glmnet]{cv.glmnet}} function.
#'
#' @return Returns the glmnet.lasso object with the call slot altered to 
#' reflect the call to this function rather than the glmnet function.
#' 
#' @details
#' Basically it takes the standard inputs from the \code{\link{run}} routine and translates
#' them to work with the glmnet \code{\link[glmnet]{cv.glmnet}} function.
lasso <- function(formula, data, subset=NULL, ...)
{
#   Get the argument list for the function
    argg <- c(as.list(environment()), list(...))
    cll <- match.call()

#   Unlike all the other models used here, 'glmnet' does not use the formula interface. 
#   This section is intended to take formula / data input and adjust it to fit the glmnet
#   interface.
    aaa <- list(formula=argg$formula, data=argg$data)
    if(! is.null(argg$subset)) aaa <- c(aaa, list(subset=argg$subset))
    if("offset" %in% names(argg)) aaa <- c(aaa, list(offset=argg$offset))
    dta <- do.call("model.frame", aaa)

#   create the x, y and offset matrices/vectors that are needed by glmnet.
    argg$x <- model.matrix(argg$formula, dta)
    argg$y <- model.response(dta)
    if("offset" %in% names(argg)) argg$offset <- model.offset(dta)
    argg$parallel <- TRUE
#   Regardless, remove the 'formula', 'subset' and 'data' arguments from the argument list.
    argg$formula <- NULL
    argg$data    <- NULL
    argg$subset  <- NULL
#   And take care of an annoying issue with the 'family' parameter
    if("family" %in% names(argg) & ! is.character(argg$family)){
        argg$family <- deparse(argg$family)
    }
        
#   call the main 'cv.glmnet' function with the modified function list and set up a parallel cluster for it.
    if( isNamespaceLoaded( "doParallel" ) ) registerDoParallel()
    out <- do.call(glmnet::cv.glmnet, argg)
    if( isNamespaceLoaded( "doParallel" ) ) stopImplicitCluster()
    out$call <- cll
    out
}

#' ranger helper function 
#'
#' This is a helper function that \code{\link{run}} calls whenever a ranger model is used.
#'
#' @param formula Formula. This describes the model that the LASSO fits.
#' @param data Data Frame. The data used for the model.
#' @param subset Name. This defines the subset of the data the model is 
#' evaluated over.
#' @param ... Additional parameters to the \code{\link[ranger]{ranger}} function.
#'
#' @return Returns the ranger object with the call slot altered to 
#' reflect the call to this function rather than the ranger function.
#' 
#' @details
#' Basically it takes the standard inputs from the 'run' routine and translates
#' them to work with the \code{link[ranger]{ranger}} function.
#' 
#' The reason this helper function exists is because the default ranger
#' function does not have a subset argument.
ranger <- function(formula, data, subset=NULL, ...)
{
#   Get the argument list for the function
    argg <- c(as.list(environment()), list(...))
    cll <- match.call()
#   If the "subset" argument is included, subset the data to pass on to the main 'ranger' 
#   function.

    if(! is.null(argg$subset)) {
        argg$data <- do.call("subset", list(x=argg$data, subset=argg$subset))
    }
#   Regardless, remove the 'subset' argument from the argument list.
    argg$subset <- NULL
#   Make sure that the 'write.forest', 'importance' and 'verbose' arguments are included
#   unless they have been specifically excluded. Note that if write.forest has been, we won't
#   be able to generate predictions, and if importance has been, we will lose some information
#   about the model.
    if(! "write.forest" %in% names(argg)) argg$write.forest <- TRUE
    if(! "importance"   %in% names(argg)) argg$importance   <- "impurity"
    if(! "verbose"      %in% names(argg)) argg$verbose      <- FALSE
#
#   call the main 'ranger' function with the modified function list.
    out <- do.call(ranger::ranger, argg)
    out$call <- cll
    out
}




#' Compute out-of-sample RMS Errors for model output
#'
#' Compute out-of-sample RMS Errors for model output
#'
#' @param input  Control object. The input control object used by \code{\link{run}} to 
#' generate the output.
#' @param output Model Output. The model output produced by \code{\link{run}}.
#' @param subset The subset of the data over which to estimate RMS Errors. 
#' I include this because in some cases the test subset has been different from
#' the training subset in non-random ways.
#'
#' @export
#' @details
#' This function takes output from the \code{\link{run}} function and calculates the
#' out-of-sample Root-Mean-Square Error values for each model in the output
#' object.
#' 
fcTest <- function(input, output, subset=NULL) 
{
#   Test for errors:
#   Test to see if the 'data' and dependent variables are all identical. 
    x <- sapply(input$models, function(x) as.character(x$inputs$data))
    dta <- x[1]
    if(! all(dta == x)) stop("data are not all identical. Try breaking up the input and output files.")
    x <- sapply(input$models, function(x) as.character(x$inputs$formula[2]))
    y <- x[1]
    if(! all(y == x)) stop("Dependent variables are not all identical. Try breaking up the input and output files.")
    rm(x)

#   If a subset is not specified in the input to this function, use the first subset from the input file
    if(is.null(subset))
    {
        x <- sapply(input$models, function(x) as.character(x$inputs$subset))
        if(! all(x[1] == x)) simpleWarning("The subsets are not all identical. Using the first. Try specifying the subset you want.")
        rm(x)
        subset <- input$models[[1]]$inputs$subset     
    } 

#   this allows me to extend a pre-existing test output object. I never use this option.
    if(is.list(subset))
    {
        old.res <- subset
        subset <- old.res$subset
        results <- old.res$results
        if(y != old.res$lhs) stop("When 'subset' is the old results list, then the dependent variables must match.")
    }

#   Now we generate a data frame with just the new (test) data.
    new.data <- do.call("subset", list(x=get(dta), subset=substitute(a & set == "test", list(a=subset))))

    if(! exists("old.res"))
    {
        results <- new.data[, intersect(c("year", "geoid", "state", "region", "fd_id", "fd_size", "parcel_id"), names(new.data))]
        results$dept.new <- as.character(NA)
# y contains the target variable. This will be a problem when dealing with the
# fire size models, because there we work with probabilities, and the names
# will be different. So the 'else' portion of this handles that case.
        if(y %in% names(new.data))  results[[y]] <- new.data[[y]]
        else
        {
            yy <- eval(parse(text=y), new.data)
            y <- "y"
            if(ncol(yy) == 2) results[[y]] <- yy[, 1] / (yy[, 1] + yy[, 2])
            else                  results[[y]] <- yy
        }
    }

#   'vars' will be the list of names of the models analyzed.
    vars <- NULL
    for(k in names(input$models))
    {
#   OK, when will I ever have a NULL library??? What circumstance is this preparing for???
        if(tolower(input$models[[k]]$fn["library"]) == "null") next
#   Make sure the proper library is loaded.
        loadNamespace(input$models[[k]]$fn["library"])
#   Initialize the variable in the results table and update the list of models
#  LASSO (glmnet) is a special case because there I will have two outputs.
        if(input$models[[k]]$fn["library"] == "glmnet"){
            results[[paste(k, "min", sep=".")]] <- as.numeric(NA)
            results[[paste(k, "1se", sep=".")]] <- as.numeric(NA)
            new.vars <- paste(k, c("min", "1se"), sep=".")
        } else {
            if(k == y){
                new.var <- paste(k, ".est", sep="")
            } else {
                new.var <- k
            }
            results[[new.var]] <- as.numeric(NA)
            new.vars <- new.var
        }
        vars <- c(vars, new.vars)

#   This set generates the predictions. Each "run" is a separate prediction
        for(i in names(input$runs)){
#   x is a logical vector showing the subset of new.data that this run applies to.
            x <- eval(input$runs[[i]], envir=new.data)
#   In some cases that subset is empty. Plus sometimes 'run' produces 
#   a NULL model--typically when the run errors out.
#   Skip those cases.
            if(any(x) & ! is.null(output[[k]][[i]]$model)){
#   An uncomfortably large number of routines require special handling to ensure they give answers.
#   So, here I cycle through each of the libraries that need special handling and give it to them.
#
#   GLMER
#     There are two things that need attention here. First, I need to add the 'allow.new.levels' flag.
#     Second, I want to record which records are associated with new levels. This section does that.
                if(input$models[[k]]$fn["library"] == "lme4"){
                    results[[new.var]][x] <- predict(output[[k]][[i]]$model, newdata=new.data[x,], type="response", allow.new.levels=TRUE)
                    x1 <- results$fd_id %in% row.names(ranef(output[[k]][[i]]$model)$fd_id)
                    results$dept.new[x & ! x1] <- paste(results$dept.new[x & ! x1], k, sep=";")
                }
#   LASSO / RIDGE
#     The input format for glmnet is different, so I have to account for that. 
#     There are two different solutions to glmnet. This is designed to return both.
                else if(input$models[[k]]$fn["library"] == "glmnet") {
                    new.x <- model.matrix(input$models[[k]]$inputs$formula, new.data[x,], na.action=na.pass)
                    if("offset" %in% names(input$models[[k]]$inputs)) {
                        off <- eval(input$models[[k]]$inputs$offset, new.data[x,])
                        results[[new.vars[1]]][x] <- predict(output[[k]][[i]]$model, newx=new.x, type="response", s="lambda.min", offset=off)
                        results[[new.vars[2]]][x] <- predict(output[[k]][[i]]$model, newx=new.x, type="response", s="lambda.1se", offset=off)
                    } else {
                        results[[new.vars[1]]][x] <- predict(output[[k]][[i]]$model, newx=new.x, type="response", s="lambda.min")
                        results[[new.vars[2]]][x] <- predict(output[[k]][[i]]$model, newx=new.x, type="response", s="lambda.1se")
                    }
                }
#   Ranger (Random Forest implementation)
#     Right now, the only special item here is verbose=FALSE, and the name of the newdata set. 
#     It also returns a structure, and I just want the prediction
                else if(input$models[[k]]$fn["library"] == "ranger"){
                    results[[new.var]][x] <- predict(output[[k]][[i]]$model,    data=new.data[x,], verbose=FALSE)$predictions
                }
#       Everything else
                else {
                    results[[new.var]][x] <- predict(output[[k]][[i]]$model, newdata=new.data[x,], type="response")
                }
            }
        }
    }
    results$dept.new <- sub("^NA;", "", results$dept.new)

# Here we actually compute RMS Errors from the predictions computed above.
    s <- results[, vars]
    s <- (s - results[[y]]) ^ 2
    if(is.null(ncol(s))) se <- sqrt(mean(s, na.rm=TRUE))
    else                       se <- sqrt(colMeans(s, na.rm=TRUE))
	names(se) <- vars
# Finally, we compile everything into a list and return it.
    if(exists("old.res"))  list(lhs=y, subset=subset, se=c(old.res$se, se), results=results)
    else                       list(lhs=y, subset=subset, se=se, results=results)
}





#' Merge multiple \code{\link{test}} objects 
#'
#' Merges multiple \code{\link{test}} objects
#'
#' @param t1 test object.
#' @param ... Additional test objects
#'
#' @return Returns a test object that contains all the information in the  
#' separate test objects supplied.
#' 
#' @export
#' @examples
#' dontrun{
#'   c.test(test.f.L1, test.f.L2)
#' }
c.test <- function(t1, ...)
{
# First, get the arguments to this function.
# Since the first object in the match.call list is NOT an argument of the function,
# screen it out.
    x <- as.list(match.call())
    if(length(x) <  2) stop("This function must be called with at least 2 test objects.")
    if(length(x) == 2) return(t1)
    x <- x[2:length(x)]
# Test to see if the listed objects are actually test outputs and are compatible.
# If not, raise an error.
    for(i in 1:length(x))
    {
        x[[i]] <- eval(x[[i]])
        if(! identical(names(x[[i]]), c("lhs", "subset", "se", "results"))) stop(paste("The ", i, "th object is not a test object", sep=""))
    }
    if(length(unique(unlist(lapply(x, function(z) z$lhs)))) > 1) stop("The Left Hand Side of all test objects must be the same")
    if(length(unique(unlist(lapply(x, function(z) deparse(z$subset))))) > 1) stop("The subsets of all test objects must be the same")

    out <- x[[1]]
# This section does the actual merging of the objects.
    for(i in 2:length(x))
    {
        if(! identical(out$results[,c(1:6,8)], x[[i]]$results[,c(1:6,8)])) 
            stop(paste("The results frame for test object", i, "is not identical in relevant to the first test object"))
        out$results <- cbind(out$results, x[[i]]$results[, 9:ncol(x[[i]]$results)])
        out$se <- c(out$se, x[[i]]$se)
        out$results$dept.new <- paste(out$results$dept.new, x[[i]]$results$dept.new, sep=";")
        out$results$dept.new[out$results$dept.new == "NA;NA"] <- NA
        out$results$dept.new <- sub("NA;", "", out$results$dept.new)
        out$results$dept.new <- sub(";NA", "", out$results$dept.new)
    }
    out
}




#' Naive estimator
#'
#' Generates the naive estimator for any given data set.
#'
#' @param test Test object.
#'
#' @return (modified) test object.
#'
#' @export
#' @details
#' This takes an output object from the test function and computes
#' the Naive predictor. The Naive predictor says that the best prediction
#' for a tract-year is the number of outcomes (fires, injuries, etc.) 
#' that occurred for that tract the previous year. It is undefined 
#' for the first year in the data set.
#' 
naive <- function(test)
{
# First make check to see if test is actually an output of the test function.
    if(! is.list(test)) stop("this is not the output of the fn.test function")
    if(any(names(test) != c("lhs", "subset", "se", "results"))) stop("this is not the output of the fn.test function")
# This section generates the initial naive predictor.
    x <- test$results[, c("year", "geoid", test$lhs)]
    x$ndx <- paste(x$geoid, x$year, sep=".")
    x$match <- paste(x$geoid, x$year - 1, sep=".")
    x$naive <- x[[test$lhs]][match(x$match, x$ndx)]
# This section corrects for the case where f_located is different
# for the two years.
    if("f_located" %in% names(test$results))
    {
        x$f_located <- test$results$f_located
        x$naive <- x$naive * x$f_located / x$f_located[match(x$match, x$ndx)]
    }
# Finally return the results, with the RMS Error for the naive estimator.
    test$results$naive <- x$naive
    s <- (test$results[[test$lhs]] - test$results$naive) ^ 2
    test$se <- c(test$se, naive=sqrt(sum(s, na.rm=TRUE) / length(s[! is.na(s)])))
    test
}




#' Condition a data set for use in model estimation
#'
#' Condition a data set for use in model estimation
#'
#' @param dta data.frame. The data set that needs to be condition for use.
#' @param seed Intger. A random seed used to ensure consistent results 
#' for the partitioning of the data set into training and test sets.
#'
#' @return The conditioned data frame.
#'
#' @export
#' @details
#' This takes the 'low.risk.fires', 'med.risk.fires' and 'high.risk.fires'
#' data.frames as pulled from the database and makes the modification needed 
#' to use them for analysis.
#'
#' It also does much of the pre-processing on the 'lr_mr_pred' and 'hr_pred' 
#' data frames as well.
#'
#' This function carries out the following tasks:
#' 1) Set any nulls in the outcome variables to zero.
#' 2) Turn any categorical predictors into factors.
#' 3) Take the log of the income variable.
#' 4) Ensure that there is an f_located column in the table.
#' 5) Define any filters that are needed (only for training tables).
#' 6) Define training and test sets for the training tables.
#'
fcSetup <- function(dta, seed=953016876)
{
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#
# low.risk.fires fields
#  Index: year, geoid <<change to tr10_fid?>>, region, state, fdid, fd_id, fd_size
#  Info: dept_incidents, dept_fires, dept_lr, f_located
#  Outcomes: res_all, low_risk, res_1, res_2, res_3, injuries, deaths
#  Covariates: age_hh_sz, pop, black, amer_es, other, hispanic, males, age_..., hse_units, vacant,
#              renter_occ, crowded, sfr, units_10, mh, older, inc_hh<<text>>, svi, married, unemployed,
#              nilf, smoke_st, smoke_cty, fuel_...
#
# med.risk fields
#  Index: year, geoid <<change to tr10_fid?>>, region, state, fd_id, fd_size
#  Info: dept_incidents, dept_fires, f_located
#  Outcomes: med_risk, mr_1, mr_2, mr_3, injuries, deaths
#  Covariates: age_hh_sz, pop, black, amer_es, other, hispanic, males, age_..., hse_units, vacant,
#              renter_occ, crowded, sfr, units_10, mh, older, apt_parcels, mr_parcels, inc_hh<<text>>, 
#              svi, married, unemployed, nilf, smoke_st, smoke_cty
#
# high.risk fields
#  Index: year, parcel_id, geoid <<change to tr10_fid?>>, geoid_source, region, state, fd_id, fd_size
#  Preliminary: res_corelogic, res_other, bld_units, hr_floors, eff_yr, risk_class
#  Info: dept_incidents, dept_fires, f_located
#  Outcomes: fires, size_1, size_2, size_3, injuries, deaths
#  Covariates: age_hh_sz, pop, black, amer_es, other, hispanic, males, age_..., hse_units, vacant,
#              renter_occ, crowded, sfr, units_10, mh, older, inc_hh<<text>>, svi, married, unemployed,
#              nilf, smoke_st, smoke_cty
#
# lr_mr_pred fields
#  Index: year, geoid <<change to tr10_fid?>>, region, state, fd_id, fd_size
#  Info: f_located
#  Covariates: age_hh_sz, pop, black, amer_es, other, hispanic, males, age_..., hse_units, vacant,
#              renter_occ, crowded, sfr, units_10, mh, older, apt_parcels, mr_parcels, inc_hh<<text>>, 
#              fuel_..., svi, married, unemployed, nilf, smoke_st, smoke_cty
#
# hr_pred fields
#  Index: year, parcel_id, geoid <<change to tr10_fid?>>, geoid_source, region, state, fd_id, fd_size
#  Preliminary: res_corelogic, res_other, bld_units, hr_floors, eff_yr, risk_class
#  Info: 
#  Covariates: age_hh_sz, pop, black, amer_es, other, hispanic, males, age_..., hse_units, vacant,
#              renter_occ, crowded, sfr, units_10, mh, older, inc_hh<<text>>, svi, married, unemployed,
#              nilf, smoke_st, smoke_cty
#
# Identify what table this is:
    if('res_all' %in% names(dta))  lr.tbl   <- TRUE
        else                           lr.tbl   <- FALSE
    if('injuries' %in% names(dta)) pred.tbl <- FALSE
        else                           pred.tbl <- TRUE
	tr.id <- grep("tr10_fid|geoid$", names(dta), value=TRUE)
# Format the data as needed.
    v.factors <- intersect(c('region', 'state', 'fd_id', 'fd_size', 'res_corelogic', 'res_other', 'risk_class'), names(dta))
    v.zeros <- intersect(c('res_all', 'low_risk', 'res_1', 'res_2', 'res_3', 'injuries', 'deaths', 'med_risk', 
                             'mr_1', 'mr_2', 'mr_3', 'fires', 'size_1', 'size_2', 'size_3', 'hr_floors'), names(dta))
    v.na <- setdiff(names(dta), c(v.zeros, 'res_corelogic', 'eff_yr', 'geoid_source', 'bld_units'))
    for(i in v.zeros) dta[[i]][is.na(dta[[i]])] <- 0
    dta$region[dta$state == 'PR'] <- 'Puerto Rico'
    dta$region[dta$state %in% c('AK', 'HI')] <- 'West'
    dta$inc_hh <- log(as.numeric(dta$inc_hh))
    for(i in v.factors) dta[[i]] <- factor(dta[[i]])
    dta$region <- relevel(dta$region, 'West')
    if(! 'f_located' %in% names(dta)) dta$f_located <- 1
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# Not all filters are needed if this is a medium or high risk data set.  !
#                                                                        !
# Filters:                                                               !
#  low_risk_fires:  base, lcl, small, no.fire, giants                    !
#  med_risk_fires:  base, lcl, small, no.fire                            !
#  high_risk_fires: base, lcl, small, no.fire                            !
#  lr_mr_pred:      <none>                                               !
#  hr_pred:         <none>                                               !
#                                                                        !
# The filters for the '_pred' tables are not needed because the          !
# prediction function (below) automatically screens out any rows with    !
# NULL predictors.                                                       !
#                                                                        !
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# create filters
    if(! pred.tbl){
# base filter
        dta$no.fire <- dta$small <- dta$base <- dta$include <- TRUE
        lvl <- grep("[3-9]", levels(dta$fd_size), value=TRUE)
        dta$base <- dta$base & dta$fd_size %in% lvl
        dta$base <- dta$base & dta$f_located > 0
        for(i in v.na) dta$base <- dta$base & ! is.na(dta[[i]])

# small filter
        dta$small <- dta$dept_incidents > 25 & ! is.na(dta$dept_incidents)
# no.fires filter
        fire.col <- grep("low_risk|med_risk|^fires", names(dta))
		dta$no.fire[dta[[fire.col]] == 0] <- FALSE

# define outliers (lcl)
        dept <- dta[, c(tr.id, 'year', 'fd_id', 'dept_incidents')]
        ddd <- unique(dta[,c('year', 'fd_id', 'dept_incidents')])
        ddd <- aggregate(ddd$dept_incidents, list(fd_id=ddd$fd_id),
                          function(x) c(mean(x, na.rm=TRUE), sd(x, na.rm=TRUE)))
        ddd$m <- ddd$x[,1]
        ddd$sd <- ddd$x[,2]
        dept$m <- ddd$m[match(dept$fd_id, ddd$fd_id)]
        dept$sd <- ddd$sd[match(dept$fd_id, ddd$fd_id)]
        dept$lg <- ! (is.na(dept$dept_incidents) | dept$dept_incidents < dept$m - 2 * dept$sd)
        dta$lcl <- dept$lg
        rm(dept, ddd)
        if(lr.tbl) {
# giants filter
            u <- with(dta[dta$base,], list(pop      =dta[[tr.id]][  pop > quantile(pop,      .999)],
                                               hse.units=dta[[tr.id]][hse_units > quantile(hse_units, .999)],
                                               males    =dta[[tr.id]][males > quantile(males,     .999)],
                                               age_45_54=dta[[tr.id]][age_45_54 > quantile(age_45_54, .999)]))
            v <- NULL
            for(i in names(u)) v <- union(v, u[[i]])
            dta$giants <- ! dta[[tr.id]] %in% v
            rm(i, u, v)
        }
# Finish Filters
        filter <- intersect(c('base', 'small', 'lcl', 'giants'), names(dta))
        for(i in filter) dta$include <- dta$include & dta[[i]]
#
# partition data (do I use different seeds for each of the risk types?)
        set.seed(seed)
        tr10_fid <- unique(dta[[tr.id]])
        tr10_fid <- data.frame(tr10_fid=tr10_fid, v=floor(runif(length(tr10_fid)) * 3) + 1, set="",
                                stringsAsFactors=FALSE)
        tr10_fid$set <- c("training", "validation", "test")[tr10_fid$v]
        tr10_fid$set <- factor(tr10_fid$set)
        dta$set <- tr10_fid$set[match(dta[[tr.id]], tr10_fid$tr10_fid)]
    }
    dta
}



#' Predict expected values
#'
#' Predicts expected values based on fitted models and new data.
#'
#' @param input character vector. This lists the names of the control objects 
#'  used to generate the model(s) used for prediction.
#' @param output character vector. This lists the names of the output models 
#'  created from the control objects listed in 'input'.
#' @param new.data data.frame This is the data that will be used to generate 
#' the predictions. It needs to contain all the input variables used in 
#' creating the model.
#' @param subset   name. This specifies the subset of new.data for which 
#' predictions will be made. If, as I expect, you want predictions for all 
#' rows in the new.data object, then the default value (subset=TRUE) will 
#' provide that. There is no need to screen out rows with undefined input
#' variables since this routine already takes that into account.
#'
#' @return Returns a data frame with some identifier columns and the 
#' predictions from the model for each row. If any row is screened out by the
#' subset parameter, then it will be returned with an NA in the prediction 
#' column.
#'
#' @export
#' @details
#' This function produces tract-level estimates of the modeled information 
#' based on models run and output in the "output" object, and using the data 
#' in "new.data".
#'
fcEstimate <- function(input, output, new.data, subset=TRUE) 
{
    nulls <- attr(na.omit(new.data), "na.action")
    nulls <- !((1:nrow(new.data)) %in% nulls)

#   Initialize the results table.
    results <- grep("geoid|tr10_fid|fd_id|parcel_id|year", names(new.data))
    results <- new.data[,results]

    if(length(input) != length(output)) stop("The 'input' vector must be the same length as the 'output' vector.")
# We iterate through all the control objects 
    for(a in 1:length(input)){
	    nput <- get(input[a])
		oput <- get(output[a])
# for each control object, iterate through the models in it.
        for(k in names(nput$models)) {
#   OK, when will I ever have a NULL library??? What circumstance is this preparing for???
            if(tolower(nput$models[[k]]$fn["library"]) == "null" | nput$models[[k]]$fn["library"] == "" | is.na(nput$models[[k]]$fn["library"])) next
#   Make sure the proper library is loaded.
            loadNamespace(nput$models[[k]]$fn["library"])
#   Initialize the variable in the results table and update the list of models
            if(nput$models[[k]]$fn["library"] == "glmnet"){
                new.vars <- paste(k, c("min", "1se"), sep=".")
                results[[new.vars[1]]] <- as.numeric(NA)
                results[[new.vars[2]]] <- as.numeric(NA)
            } else {
                results[[k]] <- as.numeric(NA)
            }
#   This set generates the predictions. Each "run" is a separate prediction
            for(i in names(nput$runs)) {
#   fltr is a logical vector showing the subset of new.data that this run applies to.
                fltr <- eval(substitute(nulls & a & b, list(a=subset, b=nput$runs[[i]])), envir=new.data)
#   And in some cases that subset is empty, or there was no model output for that group, so skip those cases.
                if(any(fltr) & ! is.null(oput[[k]][[i]]$model)) {
#   An uncomfortably large number of routines require special handling to ensure they give answers.
#   So, here I cycle through each of the libraries that need special handling and give it to them.
#
#   GLMER
#   There are two things that need attention here. First, I need to add the 'allow.new.levels' flag.
#   In the 'test' script, I record which rows have no department id in the model. I am not interested 
#   in doing that here, because it is the final predicted results.
                    if(nput$models[[k]]$fn["library"] == "lme4") {
                        results[[k]][fltr] <- predict(oput[[k]][[i]]$model, newdata=new.data[fltr,], type="response", allow.new.levels=TRUE)
                    }
#   LASSO / RIDGE
#   The input format for glmnet is different, so I have to account for that. 
#   There are two different solutions to glmnet. This is designed to return both.
                    else if(nput$models[[k]]$fn["library"] == "glmnet") {
                        glm.fmla <- nput$models[[k]]$inputs$formula
                        glm.fmla[[2]] <- NULL
                        new.x <- model.matrix(glm.fmla, new.data[fltr,], na.action=na.pass)
                        if("offset" %in% names(nput$models[[k]]$inputs)) {
                            off <- eval(nput$models[[k]]$inputs$offset, new.data[fltr,])
                            results[[new.vars[1]]][fltr] <- predict(oput[[k]][[i]]$model, newx=new.x, type="response", s="lambda.min", offset=off)
                            results[[new.vars[2]]][fltr] <- predict(oput[[k]][[i]]$model, newx=new.x, type="response", s="lambda.1se", offset=off)
                        } else {
                            results[[new.vars[1]]][fltr] <- predict(oput[[k]][[i]]$model, newx=new.x, type="response", s="lambda.min")
                            results[[new.vars[2]]][fltr] <- predict(oput[[k]][[i]]$model, newx=new.x, type="response", s="lambda.1se")
                        }
                    }
#   Ranger (Random Forest implementation)
#   Right now, the only special item here is verbose=FALSE, and the name of the newdata set.
#   The predict function also returns a structure, and I just want the prediction
                    else if(nput$models[[k]]$fn["library"] == "ranger") {
                        results[[k]][fltr] <- predict(oput[[k]][[i]]$model, data=new.data[fltr,], verbose=FALSE)$predictions
                    }
#   Everything else
                    else {
                        results[[k]][fltr] <- predict(oput[[k]][[i]]$model, newdata=new.data[fltr,], type="response")
                    }
                }
            }
        }
	}
    results
}




#' Download ACS data 
#'
#' Downloads the ACS data needed for use in model estimation.
#'
#' @param conn   DBI Connection. The connection to the database where the data 
#' will be uploaded.
#' @param cols   Character vector. Vector of column names (from the ACS) 
#' reflecting what ACS columns to download.
#' @param year   Integer. The year for which to download ACS data
#' @param states Character vector. This allows me to specify a subset of states. 
#' If it is not specified, all nationwide data is downloaded. [optional]
#' 
#' @export
#' @details
#' This leaves a 'temporary' table on the database. That table will then need
#' to be manually inserted into the main acs_est table.
#' 
#' @return returns a list with the following entries:
#'   table.name.est: Name of the table on the database in which the new estimates
#'                   are stored. 
#'   table.name.err: Name of the table on the database in which the new error values
#'                   are stored.
#'   rows:           Number of rows added to the data set.
#'   elapsed.time:   Time it took to complete the download (in seconds?)
#' 
acs.dwnld <- function(conn, year, cols, states=NULL)
{
    time.0 <- Sys.time()
# If states is not specified above, list all the states in the US (including DC)
    if(is.null(states)){
                states <- c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", "HI", "ID", "IL", 
                             "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT", 
                             "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", 
                             "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY", "DC")
    }

# Break up the cols vector into bite-size chunks
# I do this mainly because I expect the census api will limit the number of variables I can download at a time.
    cols.l <- list()
    for(i in 0:((length(cols) - 1) %/% 25)) {
        cols.l[[i + 1]] <- cols[(1 + (i * 25)):min(length(cols), 25 + (i * 25))]
    }
    n.cols <- length(cols.l)
# I need a temporary table into which to upload the data. This section finds a temporary 
#   table name I can use to do that.
        n <- 0
        tnames <- dbGetQuery(conn, "select * from pg_tables")$tablename
        tname0 <- paste("acs", year, sep="_")
        tname.est <- paste(tname0, "est", sep="_")
        tname.err <- paste(tname0, "err", sep="_")
        while(tname.est %in% tnames | tname.err %in% tnames){
            tname.est <- paste(tname0, n, "est", sep="_")
            tname.err <- paste(tname0, n, "err", sep="_")
                n <- n + 1
        }
        rm(n, tnames, tname0)
        initial <- TRUE

        number.rows <- 0
# The acs library will only download data for one state at a time. So this routine is set up to
#    download all the data for one state and upload it to the database before moving on to the 
#    next state.
    for(i in states){
# This section downloads the data
        for(j in 1:n.cols){
            tmp <- acs.fetch(year, geography=geo.make(state=i, county="*", tract="*"), variable=cols.l[[j]])
            if(exists("out.91362")){
                out.91362 <- cbind(out.91362, tmp)
            } else {
                out.91362 <- tmp
            }
                rm(tmp)
        }
# Now we construct the data frame that will be uploaded. This includes building the geoid column
# <<<<<<<<<< change name ?geoid? to ?tr10_fid? >>>>>>>>>>>>>>>>>>
        out.91363 <- geography(out.91362)
        out.91363$geoid <- with(out.91363, paste("14000US", formatC(state, width=2, flag="0"), formatC(county, width=3, flag="0"), tract, sep=""))
        if(initial){
            dbWriteTable(conn, tname.est, cbind(out.91363, as.data.frame(estimate(out.91362))), row.names=FALSE)
            dbWriteTable(conn, tname.err, cbind(out.91363, as.data.frame(standard.error(out.91362))), row.names=FALSE)
            initial <- FALSE
        } else {
            dbWriteTable(conn, tname.est, cbind(out.91363, as.data.frame(estimate(out.91362))), row.names=FALSE, append=TRUE)
            dbWriteTable(conn, tname.err, cbind(out.91363, as.data.frame(standard.error(out.91362))), row.names=FALSE, append=TRUE)
        }
        number.rows <- number.rows + nrow(out.91362)
        rm(out.91362, out.91363)
    }
    list(table.name.est=tname.est, table.name.err=tname.err, rows=number.rows, elapsed.time=Sys.time() - time.0)
}
