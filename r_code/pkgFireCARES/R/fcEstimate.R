##  fcEstimate.R
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
    results <- grep("geoid|tr10_fid|fd_id|fc_dept_id|parcel_id|year$", names(new.data))
    results <- new.data[,results]

    if(length(input) != length(output)) stop("The 'input' vector must be the same length as the 'output' vector.")
# We iterate through all the control objects
    for(a in 1:length(input)){
	    nput <- get(input[a])
		  oput <- get(output[a])
# for each control object, iterate through the models in it.
        for(k in names(nput$models)) {
#   OK, when will I ever have a NULL library??? What circumstance is this
#   preparing for???
            if(tolower(nput$models[[k]]$fn["library"]) == "null" |
               nput$models[[k]]$fn["library"] == "" |
               is.na(nput$models[[k]]$fn["library"])) next
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
#   fltr is a logical vector showing the subset of new.data that this run
#   applies to.
                fltr <- eval(substitute(nulls & a & b, list(a=subset, b=nput$runs[[i]])), envir=new.data)
#   And in some cases that subset is empty, or there was no model output for
#   that group, so skip those cases.
                if(any(fltr) & ! is.null(oput[[k]][[i]]$model)) {
#   An uncomfortably large number of routines require special handling to
#   ensure they give answers. So, here I go through each of the libraries that
#   need special handling and give it to them.
                    if(nput$models[[k]]$fn["library"] == "lme4") {
#   GLMER
#   Here, I need to add the 'allow.new.levels' flag. Without it the predict
#   function will probably fail.
                        results[[k]][fltr] <- predict(oput[[k]][[i]]$model, newdata=new.data[fltr, ], type="response", allow.new.levels=TRUE)
                    } else if(nput$models[[k]]$fn["library"] == "glmnet") {
#   LASSO / RIDGE
#   The input format for glmnet is different, so I have to account for that.
#   There are two different solutions to glmnet. This is designed to return both.
                        glm.fmla <- nput$models[[k]]$inputs$formula
                        glm.fmla[[2]] <- NULL
                        new.x <- model.matrix(glm.fmla, new.data[fltr, ], na.action=na.pass)
                        if("offset" %in% names(nput$models[[k]]$inputs)) {
                            off <- eval(nput$models[[k]]$inputs$offset, new.data[fltr,])
#                           off <- model.offset(n.dta)
                            results[[new.vars[1]]][fltr] <- predict(oput[[k]][[i]]$model, newx=new.x, type="response", s="lambda.min", newoffset=off, offset=off)
                            results[[new.vars[2]]][fltr] <- predict(oput[[k]][[i]]$model, newx=new.x, type="response", s="lambda.1se", newoffset=off, offset=off)
                        } else {
                            results[[new.vars[1]]][fltr] <- predict(oput[[k]][[i]]$model, newx=new.x, type="response", s="lambda.min")
                            results[[new.vars[2]]][fltr] <- predict(oput[[k]][[i]]$model, newx=new.x, type="response", s="lambda.1se")
                        }
                    } else if(nput$models[[k]]$fn["library"] == "ranger") {
#   Ranger (Random Forest implementation)
#    Differences from other predictions:
#    * In some cases Ranger fails because it expects to see the response variable
#      in the data. The fire size responses are constructued and therefore do
#      not appear in the data, and so the predict.ranger function fails. Where
#      that problem occurs, I add a column of zeros with the response name.
#    * The parameter name for the newdata is different from the other predict functions.
#    * I need to include the verbose=FALSE term.
#    * The predict function returns a structure, and I just want the prediction.
                        rspns <- as.list(nput$models[[k]]$inputs$formula)[[2]]
                        if (! deparse(rspns) %in% names(new.data)) {
                            rdata <- cbind(tmp.y=0, new.data)
                            names(rdata)[match("tmp.y", names(rdata))] <- deparse(rspns)
                            results[[k]][fltr] <- predict(oput[[k]][[i]]$model,
                                                          data=rdata[fltr, ],
                                                          verbose=FALSE)$predictions
                        } else {
                            results[[k]][fltr] <- predict(oput[[k]][[i]]$model, data=new.data[fltr, ], verbose=FALSE)$predictions
                        }
                    } else {
#   Everything else
                        results[[k]][fltr] <- predict(oput[[k]][[i]]$model, newdata=new.data[fltr, ], type="response")
                    }
                }
            }
        }
	}
    results
}
