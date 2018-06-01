##  fcTest.R
#' Compute out-of-sample RMS Errors for model output
#'
#' Compute out-of-sample RMS Errors for model output
#'
#' @param input  Control object. The input control object used by \code{\link{fcRun}} to
#'               generate the output.
#' @param output Model Output. The model output produced by \code{\link{fcRun}}.
#' @param subset The subset of the data over which to estimate RMS Errors.
#'               I include this because in some cases the test subset has been different from
#'               the training subset in non-random ways.
#'
#' @export
#' @details
#' This function takes output from the \code{\link{fcRun}} function and calculates the
#' out-of-sample Root-Mean-Square Error values for each model in the output
#' object.
#'
#' @return
#' This returns a list with the following members:
#'
#' \describe{
#'   \item{lhs}{Name of the left-hand side variable.}
#'   \item{subset}{The subset to which the results are applied.}
#'   \item{se}{A named vector with the root-mean-square errors on the
#'             out-of-sample data for each model in the control object.}
#'   \item{results}{A data frame with the row-by-row results.}
#' }
#'
fcTest <- function(input, output, subset=NULL)
{
#   Test for errors:
#   Test to see if the 'data' and dependent variables are all identical.
    x <- sapply(input$models, function(x) as.character(x$inputs$data))
    dta <- x[1]
    if(! all(dta == x)) stop("data are not all identical. Try breaking up the input and output files.")
    x <- sapply(input$models, function(x) as.character(x$inputs$formula[2]))
    y <- y.name <- x[1]
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

    if(! exists("old.res")) {
        results <- new.data[, intersect(c("year", "geoid", "state", "region", "fd_id", "fc_dept_id", "fd_size", "parcel_id"), names(new.data))]
        results$dept.new <- as.character(NA)
# y contains the target variable. This will be a problem when dealing with the
# fire size models, because there we work with probabilities, and the names
# will be different. So the 'else' portion of this handles that case.
        if(y %in% names(new.data)) {
            results[[y]] <- new.data[[y]]
        } else {
            yy <- eval(parse(text=y.name), new.data)
            y <- "y"
            if(is.matrix(yy)){
                if(     ncol(yy) == 1) yy <- as.vector(yy)
				else if(ncol(yy) == 2) yy <- yy[, 1] / (yy[, 1] + yy[, 2])
				else stop("The specified y variable returns 3 or more columns and I don't know what to do with that.")
			}
            results[[y]] <- yy
        }
    }

#   'vars' will be the list of names of the models analyzed.
    vars <- NULL
    for(k in names(input$models)) {
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
                new.vars <- paste(k, ".est", sep="")
            } else {
                new.vars <- k
            }
            results[[new.vars]] <- as.numeric(NA)
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
                if(input$models[[k]]$fn["library"] == "lme4"){
#   GLMER
#     There are two things that need attention here. First, I need to add the 'allow.new.levels' flag.
#     Second, I want to record which records are associated with new levels. This section does that.
                    results[[new.vars]][x] <- predict(output[[k]][[i]]$model, newdata=new.data[x,], type="response", allow.new.levels=TRUE)
                    x1 <- results$fd_id %in% row.names(ranef(output[[k]][[i]]$model)$fd_id)
                    results$dept.new[x & ! x1] <- paste(results$dept.new[x & ! x1], k, sep=";")
                } else if(input$models[[k]]$fn["library"] == "glmnet") {
#   LASSO / RIDGE
#     The input format for glmnet is different, so I have to account for that.
#     There are two different solutions to glmnet. This is designed to return both.
                    new.x <- model.matrix(input$models[[k]]$inputs$formula, new.data[x,], na.action=na.pass)
                    if("offset" %in% names(input$models[[k]]$inputs)) {
                        off <- eval(input$models[[k]]$inputs$offset, new.data[x,])
                        results[[new.vars[1]]][x] <- predict(output[[k]][[i]]$model, newx=new.x, type="response", s="lambda.min", newoffset=off, offset=off)
                        results[[new.vars[2]]][x] <- predict(output[[k]][[i]]$model, newx=new.x, type="response", s="lambda.1se", newoffset=off, offset=off)
                    } else {
                        results[[new.vars[1]]][x] <- predict(output[[k]][[i]]$model, newx=new.x, type="response", s="lambda.min")
                        results[[new.vars[2]]][x] <- predict(output[[k]][[i]]$model, newx=new.x, type="response", s="lambda.1se")
                    }
                } else if(input$models[[k]]$fn["library"] == "ranger"){
#   Ranger (Random Forest implementation)
#     Right now, the only special item here is verbose=FALSE, and the name of the newdata set.
#     It also returns a structure, and I just want the prediction
                    results[[new.vars]][x] <- predict(output[[k]][[i]]$model,    data=new.data[x,], verbose=FALSE)$predictions
                } else {
#       Everything else
                    results[[new.vars]][x] <- predict(output[[k]][[i]]$model, newdata=new.data[x,], type="response")
                }
            }
        }
    }
    results$dept.new <- sub("^NA;", "", results$dept.new)

# Here we actually compute RMS Errors from the predictions computed above.
    s <- results[, vars]
    s <- (s - results[[y]]) ^ 2
    if(is.null(ncol(s))) se <- sqrt(   mean (s, na.rm=TRUE))
    else                 se <- sqrt(colMeans(s, na.rm=TRUE))
	names(se) <- vars
# Finally, we compile everything into a list and return it.
    if(exists("old.res")) list(lhs=y.name, subset=subset, se=c(old.res$se, se), results=results)
    else                  list(lhs=y.name, subset=subset, se=se,                results=results)
}





#' Merge multiple \code{\link{fcTest}} objects
#'
#' Merges multiple \code{\link{fcTest}} objects
#'
#' @param t1 test object.
#' @param ... Additional test objects
#'
#' @return Returns a test object that contains all the information in the
#' separate test objects supplied.
#'
#' @export
#' @examples
#' \dontrun{
#'   c_test(test.f.L1, test.f.L2)
#' }
c_test <- function(t1, ...)
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
#' @param test \code{\link{fcTest}} object.
#'
#' @return (modified) \code{\link{fcTest}} object.
#'
#' @export
#' @details
#' This takes an output object from the \code{\link{fcTest}} function and computes
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
