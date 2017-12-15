#' Connect to klimageo database
#'
#' Connect to the klimegeo database. The connection is done via the
#' \code{RPostgres} package.
#'
#' @param dbname Database name.
#' @param host,port Host and port.
#' @param user,password User name and password. If \code{NULL}, will be
#'   retrieved from \code{PGUSER} and \code{PGPASSWORD} envvars, or from the
#'   appropriate line in \code{~/.pgpass}. See
#'   \url{http://www.postgresql.org/docs/9.6/static/libpq-pgpass.html} for more
#'   details.
#'
#' @return An S4 object that inherits from DBIConnection. This object is used to
#'   communicate with the database engine.
#' @export
#'
#' @examples
#' \dontrun{
#' con <- dbConnect_klimageo()
#' dbDisconnect(con)
#' }
dbConnect_klimageo <- function(dbname = "klimageo",
                               host = "blobdb.cms.hu-berlin.de",
                               port = 5432,
                               user = NULL, password = NULL) {
  con <- DBI::dbConnect(RPostgres::Postgres(), dbname = dbname,
                        host = host, port = port,
                        user = user, password = password
  )
  DBI::dbExecute(con,
                 "SET SESSION CHARACTERISTICS AS TRANSACTION ISOLATION LEVEL REPEATABLE READ;")
  con
}
