##  utils.R
##
## This includes the following functions:
##   acs.dwnld
##   msg.out
##
#' Download ACS data
#'
#' Downloads the ACS data needed for use in model estimation.
#'
#' @param conn   DBI Connection. The connection to the database where the data
#' will be uploaded.
#' @param year   Integer. The year for which to download ACS data
#' @param cols   Character vector. Vector of column names (from the ACS)
#' reflecting what ACS columns to download. [optional]
#' @param states Character vector. This allows me to specify a subset of states.
#' If it is not specified, all nationwide data is downloaded. [optional]
#'
#' @export
#'
#' @details
#' This leaves a 'temporary' table on the database. That table will then need
#' to be manually inserted into the main acs_est table.
#'
#' @return
#' returns a list with the following entries:
#'
#' \describe{
#' \item{table.name.est}{Name of the table on the database in which the new estimates
#'                   are stored}
#' \item{table.name.err}{Name of the table on the database in which the new error values
#'                   are stored}
#' \item{rows}{Number of rows added to the data set}
#' \item{elapsed.time}{Time it took to complete the download}
#' }
#'
#' @import acs
#'
acs.dwnld <- function(conn, year, cols=NULL, states=NULL)
{
  time.0 <- Sys.time()
# If states is not specified above, list all the states in the US (including DC)
  if(is.null(states)){
    states <- c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", "HI", "ID", "IL",
                "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT",
                "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI",
                "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY", "DC")
  }
  if(is.null(cols)){
    cols <- c("B01001_001", "B01001_002", "B01001_003", "B01001_004", "B01001_005", "B01001_006", "B01001_007",
              "B01001_008", "B01001_009", "B01001_010", "B01001_011", "B01001_012", "B01001_013", "B01001_014",
              "B01001_015", "B01001_016", "B01001_017", "B01001_018", "B01001_019", "B01001_020", "B01001_021",
              "B01001_022", "B01001_023", "B01001_024", "B01001_025", "B01001_026", "B01001_027", "B01001_028",
              "B01001_029", "B01001_030", "B01001_031", "B01001_032", "B01001_033", "B01001_034", "B01001_035",
              "B01001_036", "B01001_037", "B01001_038", "B01001_039", "B01001_040", "B01001_041", "B01001_042",
              "B01001_043", "B01001_044", "B01001_045", "B01001_046", "B01001_047", "B01001_048", "B01001_049",

              "B02001_001", "B02001_002", "B02001_003", "B02001_004", "B02001_005", "B02001_006", "B02001_007",
              "B02001_008", "B02001_009", "B02001_010",

              "B03003_001", "B03003_002", "B03003_003",

			  "B11005_002",

              "B12001_001", "B12001_002", "B12001_003", "B12001_004", "B12001_005", "B12001_006", "B12001_007",
              "B12001_008", "B12001_009", "B12001_010", "B12001_011", "B12001_012", "B12001_013", "B12001_014",
              "B12001_015", "B12001_016", "B12001_017", "B12001_018", "B12001_019",

              "B19013_001",

              "B23025_001", "B23025_002", "B23025_003", "B23025_004", "B23025_005", "B23025_006", "B23025_007",

              "B25002_001", "B25002_002", "B25002_003",

              "B25014_001", "B25014_002", "B25014_003", "B25014_004", "B25014_005", "B25014_006", "B25014_007",
              "B25014_008", "B25014_009", "B25014_010", "B25014_011", "B25014_012", "B25014_013",

              "B25024_001", "B25024_002", "B25024_003", "B25024_004", "B25024_005", "B25024_006", "B25024_007",
              "B25024_008", "B25024_009", "B25024_010", "B25024_011",

              "B25034_001", "B25034_002", "B25034_003", "B25034_004", "B25034_005", "B25034_006", "B25034_007",
              "B25034_008", "B25034_009", "B25034_010",

              "B25040_001", "B25040_002", "B25040_003", "B25040_004", "B25040_005", "B25040_006", "B25040_007",
              "B25040_008", "B25040_009", "B25040_010")
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
        tnames <- RPostgreSQL::dbGetQuery(conn, "select * from pg_tables")$tablename
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
          RPostgreSQL::dbWriteTable(conn, tname.est, cbind(out.91363, as.data.frame(estimate(out.91362))), row.names=FALSE)
          RPostgreSQL::dbWriteTable(conn, tname.err, cbind(out.91363, as.data.frame(standard.error(out.91362))), row.names=FALSE)
            initial <- FALSE
        } else {
          RPostgreSQL::dbWriteTable(conn, tname.est, cbind(out.91363, as.data.frame(estimate(out.91362))), row.names=FALSE, append=TRUE)
          RPostgreSQL::dbWriteTable(conn, tname.err, cbind(out.91363, as.data.frame(standard.error(out.91362))), row.names=FALSE, append=TRUE)
        }
        number.rows <- number.rows + nrow(out.91362)
        rm(out.91362, out.91363)
    }
    list(table.name.est=tname.est, table.name.err=tname.err, rows=number.rows, elapsed.time=Sys.time() - time.0)
}
