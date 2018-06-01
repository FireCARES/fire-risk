#' Refreshes the Materialized Views in the database
#'
#' This function refreshes the specified materialized views in the database.
#' Since the materialized views are the source of the data used for this modeling
#' exercise, and the view are not refreshed automatically, this does that.
#'
#' @param conn A DBI Connection. This is a connection to the database containing
#'             the data and model definitions. If none is entered, default connection
#'             information is obtained from the operating system environment.
#'
#' @param tables This provides the views that will be refreshed. It can take one of
#'               two forms: either a vector of view names, or a list (or dataframe).
#'               If it is a vector of view names, then the views are assumed to be in
#'               the \code{public} schema. If they are not in the public schema, then
#'               the list form must be supplied. The first entry in the list will be
#'               a vector of schemas, while the second entry in the list will be a
#'               vector of view names. If this field is \code{Null}, then the list of
#'               views to be refreshed will be drawn from the database (see details below).
#'
#' @details
#' If \code{tables} is null, then the list of views to refresh is drawn from the database.
#' There is a table in the \code{public} schema in the database called \code{m_views}. It
#' has three columns:
#'
#' \describe{
#'   \item{order}{Numeric field listing the order in which views are to be refreshed.}
#'   \item{schema}{Lists the schema in which the view to be refreshed is located. If it is
#'                 Null, then the schema is assumed to be \code{public}.}
#'   \item{view}{Contains the name of the materialized view to be refreshed.}
#' }
#'
#' Any view with a Null entry in the \code{order} field will not be refreshed. All the others
#' will be refreshed in the order listed in the \code{order} field.
#'
#' @export
#'
#' @return
#' returns a data frame with the following entries:
#'
#' \describe{
#'   \item{schema}{schema name of the view refreshed.}
#'   \item{view}{name of the view refreshed.}
#'   \item{statement}{actual sql statement used to refresh the view.}
#'   \item{complete}{logical value stating whether the refresh completed.}
#'   \item{rows.aff}{number of rows affected--this may not actually mean anything,
#'                   I do not have enough information yet to know that.}
#' }
#'
refresh_views <- function(conn=NULL,
                          tables=NULL){
  #
  library(RPostgreSQL)
  if(is.null(conn)){
    conn <- dbConnect( "PostgreSQL",
                       host    =Sys.getenv("NFIRS_DATABASE_HOST"),
                       port    =Sys.getenv("NFIRS_DATABASE_PORT"),
                       dbname  =Sys.getenv("NFIRS_DATABASE_NAME"),
                       user    =Sys.getenv("NFIRS_DATABASE_USER"),
                       password=Sys.getenv("NFIRS_DATABASE_PASSWORD"))
  }

  if(is.null(tables)){
    tables <- dbGetQuery(conn, "select schema, view from m_views where \"order\" is not null order by \"order\"")
  }
# These take several different input possibilities and standardizes them.
# In particular, note that where there is no entry for \code{schema}, then the schema is
# assumed to be public.
  if(is.vector(tables)) tables <- data.frame(schema="public", view=tables)
  tables[[1]][is.na(tables[[1]])] <- "public"
  if(is.list(tables) & ! is.data.frame(tables)) tables <- as.data.frame(tables)

  tables$statement <- as.character(NA)
  tables$complete <- FALSE
  tables$rows.aff <- NA
  tables$timer    <- NA

# Now for the actual work
  for(i in 1:nrow(tables)){
    sql <- paste0('REFRESH MATERIALIZED VIEW "', tables$schema[i], '"."', tables$view[i], '"')
    cat(sql, "\n")
    tmr <- system.time(tmp <- dbSendQuery(conn, sql))
    tmp <- dbGetInfo(tmp)
    tables$statement[i] <- tmp$statement
    tables$rows.aff[i]  <- tmp$rowsAffected
    tables$complete[i]  <- as.logical(tmp$completed)
    tables$timer[i]     <- tmr[3]
  }
  tables
}
