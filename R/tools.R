#' Connect to klimageo database
#'
#' Connect to the klimegeo database. The connection is done via the \code{odbc}
#' package. Thus, the odbc driver and connection settings have to be set-up
#' outside of R before. The correct timezone is chosen by this command.
#'
#' @param dsn The Data Source Name. This corresponds to the set-up entry in the
#'   \href{http://db.rstudio.com/best-practices/drivers/}{odbc.ini}.
#'
#' @return An S4 object that inherits from DBIConnection. This object is used to
#'   communicate with the database engine.
#' @export
#'
#' @examples
dbConnect_klimageo <- function(dsn = "klimageodb") {
  DBI::dbConnect(odbc::odbc(), dsn = dsn, timezone = "Europe/Berlin")
}


write_table <- function(name, arg_list) {
  # conn is first in list
  conn <- arg_list[[1]]
  # convert the rest to data frame ignoring NULL
  df <- arg_list[-1]
  df <- do.call(cbind.data.frame, df[!sapply(df, is.null)])
  # write the table
  DBI::dbWriteTable(conn, name = name, value = df, append = TRUE)
}

dbWriteTable_site <- function(conn, site_name,
                              site_lat = NULL, site_lon = NULL, site_altitude = NULL,
                              site_comment = NULL) {
  write_table(name = "site", as.list(environment()))
}
