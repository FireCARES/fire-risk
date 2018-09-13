#' Takes two or more models and combines them.
#'
#' There are cases where different models apply to different portions of the 
#' prediction set. This takes the predictions for each model and applies the 
#' prediction only to those portions of the set to which it applies.
#'
#' @param predictions data.frame. This is a data frame of the predictions for the 
#'                    models that make up the constituent parts of the final 
#'                    prediction.
#'
#' @param merge.by vector. This vector must be the same length as the prediction
#'                 frame, and determines which column of the \code{prediction} 
#'                 data frame goes in which record. See \code{details} below for 
#'                 its format.
#'
#' @details
#' This function returns a single vector where each entry is one of the values in 
#' \code{predictions} from the corresponding row. Which row depends on the value
#' in the corresponding entry in \code{merge.by}. 
#' 
#' Each entry in the \code{merge.by} vector should refer to a specific column 
#' in the \code{predictions} data frame, but format is flexible. If 
#' \code{merge.by} is a factor or character vector, then the columns will be 
#' referenced by name. If it is an integer vector, then they will be referenced
#' by column number. If it is a numeric vector, then the vector will be coerced
#' to an integer vector.
#' 
#' If an entry in the \code{merge.by} vector does not correspond to a column
#' in the \code{predictions} data frame, then an NA will be returned for that
#' entry.
#' 
#' This is similar to the process in \code{\link{fcRun}} and \code{\link{fcEstimate}}
#' where different portions of the domain are estimated and predicted separately.
#' This is more flexible in that it allows different models to be applied to different
#' portions of the domain while in those routines, the models must be the same--they
#' are simply estimated and predicted separately.
#' 
#' @export
#'
#' @return
#' returns a list. The \code{cols} entry in the list is an integer vector
#' listing the column numbers that were merged to generate the results. 
#' The \code{result} entry is the vector containing the merged data.
#'
#' @examples 
#' y <- matrix(rnorm(30), ncol=5)
#' y <- as.data.frame(y)
#' x1 <- c(1,-1,6,2,2,4)
#' x2 <- c("V1", "VX", "V6", "V2", "V2", "V4")
#' x3 <- c(1.1, 0.9, 7.4, 2.1, 2.9, 4.4)
#' 
#' # All three of these return the same result
#' fcMerge(y,x1)
#' fcMerge(y,x2)
#' fcMerge(y,x3)
#' 
#' \dontrun{
#' # This throws an error because x4 is longer than y
#' x4 <- c(1,2,3,4,5,6,7)
#' fcMerge(y,x4)
#' }
#' 

fcMerge <- function(predictions,
                    merge.by){
  if(length(merge.by) != nrow(predictions)) stop("'merge.by' must have the same length as 'predictions'")
  if(is.factor(merge.by)){
    cols <- match(levels(merge.by), names(predictions))
    mb <- match(merge.by, names(predictions))
  } else if(is.character(merge.by)){
    cols <- match(unique(merge.by), names(predictions))
    mb <- match(merge.by, names(predictions))
  } else {
    if(! is.integer(merge.by)){
      merge.by <- as.integer(merge.by)
    }
    cols <- intersect(unique(merge.by), 1:ncol(predictions))
    mb <- merge.by
    mb[mb < 1 | mb > ncol(predictions)] <- NA
  }
  list(cols=cols[!is.na(cols)],
# This is for debugging
#       result=cbind(predictions, 
#                    mb=mb, 
#                    final=predictions[cbind(1:nrow(predictions), mb)])
       result=predictions[cbind(1:nrow(predictions), mb)])
}