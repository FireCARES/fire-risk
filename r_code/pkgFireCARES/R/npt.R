##  npt.R
#' Mass Building of Control Objects
#'
#' This uses a pattern to collect a set of control files, and then calls 'npt'
#' for each of those control files.
#'
#' @param conn DBI connection. Connects to the database containing the
#' 'controls' sets.
#' @param pattern character. Used to pattern-match the 'lst' values in the control
#' list.
#' @param list character. List of control objects to build.
#' @param relocate environment. This is an [optional] environment into which
#' to move any existing test objects.
#'
#' @return Vector listing the control objects created.
#'
#' @details
#' Creates:
#'   A set of control objects in the global environment, as specified in the
#'  'pattern' input.
#'
#' This is usually followed up with a call to \code{\link{fcMacro}}
#'
#' @export
#' @examples
#' \dontrun{
#'   conn <- dbConnect("PostgreSQL",
#'                     host="hostname.com",
#'                     dbname="nfirs",
#'                     user="username",
#'                     password="***")
#'   mass.npt(conn, "npt.f")
#'   res <- new.env()
#'   mass.npt("final", conn, res)
#' }
#'
mass.npt <- function(conn, pattern=NULL, list=NULL, relocate=NULL)
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
  if(! is.null(pattern)){
    a <- RPostgreSQL::dbGetQuery(conn, paste0("select lst, runs, count(*) as n ",
	                               "from controls.models where lst like '%", pattern, "%' ",
                                 "group by lst, runs order by lst"))
  } else {
    if(is.null(list)) stop( "At least one of 'pattern' or 'list' must be specified!")
        a <- RPostgreSQL::dbGetQuery(conn, paste0("select lst, runs, count(*) as n ",
	                                   "from controls.models where lst in ('",
                                     paste(list, collapse="','"),
                                     "') group by lst, runs order by lst"))
  }
  openLog()
  for(i in 1:nrow(a)) {
    setContext(paste0("npt('", a$lst[i], '.', a$runs[i], "')"))
    tryCatch(assign(paste0(a$lst[i], '.', a$runs[i]),
                    npt(conn, group=a$lst[i], run=a$runs[i]), envir=globalenv()),
             error=function(x) msgOut(e$message, type="error"))
  }
  paste0(a$lst, ".", a$runs)
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
#' fires), 'sz3' (for "size 3" fires), and 'ems'.
#' @param mdls character This is a character vector of the models to be
#' included for that target variable.
#' @param run character This currently takes one of four values: '0', 'S', 'L',
#' and 'C'. The values '0' and 'S' both run a single model for all department
#' sizes and regions (typically with dummies for each). The value 'L'runs separate
#' models for each combination of department size and region. The value 'C' runs
#' separate models for each cluster. Note that '0' as an option is deprecated.
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
#' Note that this function does not check for the validity of the input to
#' the \code{run} parameter. That allows me to add additional types of runs if needed
#' without rewriting the function. On the other hand, that means invalid
#' inputs are caught only if the queries fail.
#'
#' @examples
#' \dontrun{
#'   npt(conn, "npt.base")
#'   npt(conn, "npt.base", run="L")
#'   npt(conn, y="f", mdls=c("", ""))
#'   npt(conn, y="d", mdls=c("", ""), run="L")
#' }
#'
npt <- function(conn, group=NULL, risk=NULL, y=NULL, mdls=NULL, run="S")
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
		if(! y %in% c('f', 'j', 'd', 'sz2', 'sz3', 'ems')) stop("y must be one of 'f', 'j', 'd', 'sz2', 'sz3' or 'ems'")
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
# Now we build the 'runs' portion of the object
# Note that this has changed, and the format of the output file has changed with it.
# As a result, some earlier control objects will no longer be compatible.
    r0 <- RPostgreSQL::dbGetQuery(conn, paste0("select * from controls.runs where grp = '", run, "' order by tier1, tier2"))
    if(nrow(r0) == 0) stop(paste0("Input parametr run='", run, "' to the 'npt' function is invalid!"))
    r <- list()
    r0$tier.names <- with(r0, ifelse(is.na(tier2), tier1, paste(tier1, tier2, sep=".")))
    for(i in 1:nrow(r0)) {
        r[[r0$tier.names[i]]] <- parse(text=r0$value[i])[[1]]
    }

# Now select the input information used for the models to be built as part of this control object
    if(! is.null(group)) {
        mdl  <- RPostgreSQL::dbGetQuery(conn, paste0("select * from controls.models where lst = '", group,
                                        "' AND runs = '", run, "'"))
        npts <- RPostgreSQL::dbGetQuery(conn, paste0("select * from controls.inputs where lst = '", group,
                                        "' AND model in ('", paste(mdl$model, collapse="', '"), "')"))
    } else {
	    lst.check <- 'npt'
		if(risk != 'l') lst.check <- paste(risk, "r", sep="")
        mdl  <- RPostgreSQL::dbGetQuery(conn, paste0("select * from controls.models where target='", y, "' AND ",
                                         "split_part(lst, '.', 1) = '", lst.check, "' AND ",
                                         "model in ('", paste(mdls, collapse="', '"), "') AND ",
                                         "runs = '", run, "'"))
        npts <- RPostgreSQL::dbGetQuery(conn, paste("select * from controls.models JOIN controls.inputs USING (lst, model) ",
    		                               "where target='", y, "' AND split_part(lst, '.', 1) = '", lst.check, "' AND ",
		                                   "model in ('", paste(mdls, collapse="', '"), "') ",
		                                   "AND runs = '", run, "'", sep=""))
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
