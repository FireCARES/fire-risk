##  acs.dwnld.R
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
#' @return 
#' returns a list with the following entries:
#'
#' \describe{
#'   \item{table.name.est}{Name of the table on the database in which the new estimates
#'                   are stored.} 
#'   \item{table.name.err}{Name of the table on the database in which the new error values
#'                   are stored.}
#'   \item{rows}{Number of rows added to the data set.}
#'   \item{elapsed.time}{Time it took to complete the download (in seconds?)}
#' }
#'
#' @section Future Work:
#' Supply default values for \code{cols}.
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
