#' Roll up census-tract predictions to the department level
#'
#' This function takes output from the fcEstimate function, which is typically at the census-tract
#' level, and rolls them up to the department level.
#'
#' @param predictions data frame containing predictions as output by the fcEstimate function. See details below.
#' @param fire.col character vector containing names of the columns with fire predictions.
#' @param sz2.col character vector containing names of the columns with predicted percentages of fires that
#'                go beyond the room of origin.
#' @param sz3.col character vector containing names of the columns with predicted percentages of size 2 fires
#'                that go beyond the structure of origin.
#' @param indx.cols character vector containing the names of the columns to which the data is
#'                to be rolled up.
#'
#' @details
#' This routine is intended to work with a data frame containing multiple estimates (think low- medium-
#' and high-risk estimates all contained in the same data frame). When that is the case, the first
#' entry in fire.col goes with the first entry in sz2.col and sz3.col. The second entry in in fire.col
#' goes with the second entry in sz2.col and sz3.col. And so on. See the example below.
#'
#' This function takes output from the fcEstimate function, which is typically at the census-tract
#' level, and rolls them up to the department level.
#' What makes this more complicated than a simple call to aggregate is that the sz2 and sz3
#' columns are percentages rather than estimated counts. So, summing those columns produces
#' nonsensical results.
#'
#' What this function does is create temporary columns for the estimated counts for medium and
#' large fires. It then sums over census tracts (rolling up to the department level), and
#' back-calcuates the percentages from the accumulated estimated counts.
#'
#' @return
#' A data frame with the department-level predictions.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' dept.predict <- rollUp2Dept(predictions, c("lr.fires", "mr.fires", "hr.fires"),
#'                                          c("lr.sz2",   "mr.sz2",   "hr.sz2"),
#'                                          c("lr.sz3",   "mr.sz3",   "hr.sz3"),
#'                                          c("year", "fd_id"))
#' }
rollUp2Dept <- function(predictions, fire.col, sz2.col, sz3.col, indx.cols){
  if(! is.data.frame(predictions)) stop( "'predictions' must be a data frame!")
  if(! is.character(fire.col)) stop("The fire.col input must be a character vector!")
  if(! is.character(sz2.col)) stop("The sz2.col input must be a character vector!")
  if(! is.character(sz3.col)) stop("The sz3.col input must be a character vector!")
  if(! is.character(indx.cols)) stop("The indx.cols input must be a character vector!")
  if(length(fire.col) != length(sz2.col) | length(sz2.col) != length(sz3.col)) stop( "The fire.col, sz2.col, and sz3.col vectors must be the same length!")
  if(length(fire.col) == 0) stop("The .col inputs must have at least one value in them!")
  if(! all(fire.col %in% names(predictions))) stop( "The values in fire.col must match columns in the predictions data frame!")
  if(! all(sz2.col %in% names(predictions))) stop( "The values in sz2.col must match columns in the predictions data frame!")
  if(! all(sz3.col %in% names(predictions))) stop( "The values in sz3.col must match columns in the predictions data frame!")
  if(! all(indx.cols %in% names(predictions))) stop( "The values in indx.cols must match columns in the predictions data frame!")

  n.cols <- ncol(predictions)
  n <- length(fire.col)

# Generate unique names for the size 2 and size 3 fires.
  tmp <- make.unique(c(names(predictions), sz2.col, sz3.col))
  sz2.fires <- tmp[n.cols + 1:n]
  sz3.fires <- tmp[n.cols + n + 1:n]
# What makes this more complicated is that the sz2 and sz3 columns are percentages rather than
# counts. So summing them up produces nonsensical results.
#
# What this routine does is is convert the percentages to numbers before summing them.
  for(i in 1:n){
    predictions[[sz2.fires[i]]] <- predictions[[fire.col[i]]] * predictions[[sz2.col[i]]]
    predictions[[sz3.fires[i]]] <- predictions[[sz2.fires[i]]] * predictions[[sz3.col[i]]]
  }
# Here the data variables are summed over census tracts.
  dta.cols <- setdiff(names(predictions), c("geoid", "tr10_fid", "fd_id", "parcel_id", "year", "geoid_source", "fc_dept_id"))
  indx.lst <- list()
  for(j in indx.cols){ indx.lst[[j]] <- predictions[[j]] }
  predictions <- aggregate( predictions[, dta.cols],
                            indx.lst,
                            function(x) {
                              if( length(! is.na(x)) == 0) NA
                              else sum(x, na.rm=TRUE)
                            }
                          )
#
# Now the old sz2 and sz3 columns are garbage. However, the new columns, which contain estimated
# counts of size 2 and size 3 fires, are not. Use them to back-estimate the percentages.
  for(i in 1:n){
    predictions[[sz2.col[i]]] <- predictions[[sz2.fires[i]]] / predictions[[fire.col[i]]]
    predictions[[sz3.col[i]]] <- predictions[[sz3.fires[i]]] / predictions[[sz2.fires[i]]]
  }
#
# Finally, delete the temporary columns generated as part of this routine.
  predictions <- predictions[,setdiff(names(predictions), c(sz2.fires, sz3.fires))]
  predictions
}
