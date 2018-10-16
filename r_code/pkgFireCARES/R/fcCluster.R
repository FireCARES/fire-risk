#' Clusters the counties
#'
#' This function generates the county clusters for use by any estimation routine
#' that uses them.
#'
#' @param conn A DBI Connection. This is a connection to the database containing
#'             the data and model definitions. If none is entered, default connection
#'             information is obtained from the operating system environment.
#'
#' @param filter=1000 Numeric. If the number of missing fields in a column is greater
#'                    than \code{filter}, that column is excluded from the analysis. If
#'                    \code{filter} is NULL, then no columns are excluded.
#'
#' @param clusters=9 The number of clusters to use.
#'
#'
#' @details
#' The main purpose of this routine is to update the \code{nist.county_clusters} table
#' in the FireCARES database. It does the following tasks.
#'
#' \itemize{
#'   \item Load needed data from the FireCARES database.
#'   \item Filter out the offshore territories and any columns with
#'   too few records to be effectively usable.
#'   \item Cluster the data using the cluster package
#'   \item Upload the county clustering results back to FireCARES.
#' }
#'
#' @export
#'
#' @importFrom magrittr %$% %>%
#'
#' @return
#' Invisibly returns the revised cluster table used by later queries.
#'
#'
fcCluster <- function(conn,
                      filter=1000,
                      clusters=9){
  county.data <- RPostgreSQL::dbGetQuery(conn, "SELECT * FROM nist.county_clustering")
  cl.table    <- RPostgreSQL::dbGetQuery(conn, "SELECT * FROM nist.county_clusters")
  states      <- RPostgreSQL::dbGetQuery(conn, "SELECT state_fipscode, state_abbreviation, state_name, region FROM usgs_stateorterritoryhigh")

# Build column filters: only use those columns with enough data to be helpful
  fltr <- sapply(county.data, function(x) length(x[is.na(x)])) < filter
  fltr[names(county.data) %in% c("geoid")] <- FALSE

# Exclude AK, HI, and the offshore territories.
  include <- substring(county.data$geoid, 1, 2) %in%
             (states %$%
              state_fipscode[! state_abbreviation %in% c("AS",#"AK", "HI",
                                                         "GU", "MP", "PR", "VI")])

# cluster the data
  pam.out <- cluster::pam(county.data[include, fltr],  k=clusters, stand=TRUE)
  rm(include)

# Put the results back in the database where it can be used.
  cl.table$clusters <- paste0("cluster_", pam.out$clustering[match(cl.table$geoid, county.data$geoid)])
  RPostgreSQL::dbWriteTable(conn, "tmp_clusters", cl.table, row.names=FALSE, overwrite=TRUE)
  RPostgreSQL::dbSendQuery(conn, "truncate nist.county_clusters;
                   insert into nist.county_clusters
                   select * from tmp_clusters;")
  RPostgreSQL::dbSendQuery(conn, "drop table tmp_clusters")
  invisible(cl.table)
}
