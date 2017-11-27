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
#' \dontrun{
#' con <- dbConnect_klimageo()
#' dbDisconnect(con)
#' }
dbConnect_klimageo <- function(dsn = "klimageodb") {
  DBI::dbConnect(odbc::odbc(), dsn = dsn, timezone = "Europe/Berlin")
}


#' Write table to database
#'
#' @param name Name of the table.
#' @param arg_list List with first element being DBIConnection to the target
#'   database and rest being columns of same length to be put into the table.
#'
write_table <- function(name, arg_list) {
  # conn is first in list
  conn <- arg_list[[1]]
  # convert the rest to data frame ignoring NULL
  df <- arg_list[-1]
  df <- do.call(cbind.data.frame, df[!sapply(df, is.null)])
  # write the table
  DBI::dbWriteTable(conn, name = name, value = df, append = TRUE)
}

#' Insert data into \code{site} table
#'
#' @param conn Database connection.
#' @param site_name String vector of name of site or name of campaign.
#' @param site_lat Numeric vector of geographical latitude WGS84.
#' @param site_lon Numeric vector of geographical longitude WGS84.
#' @param site_altitude Numeric vector of height above sea level of surface in
#'   m.
#' @param site_comment String vector of additional comment.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' con <- dbConnect_klimageo()
#' dbWriteTable_site(con, "Adlershof", site_lon = 14., site_lat = 53.)
#' dbDisconnect(conn)
#' }
dbWriteTable_site <- function(conn, site_name,
                              site_lat = NULL, site_lon = NULL, site_altitude = NULL,
                              site_comment = NULL) {
  write_table(name = "site", as.list(environment()))
}


#' Insert data into \code{device_manufacturer} table
#'
#' @param conn Database connection.
#' @param devman_name String vector of name of device manufacturer.
#' @param devman_comment String vector of additional comment.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' con <- dbConnect_klimageo()
#' dbWriteTable_device_manufacturer(con, "TSI")
#' dbDisconnect(conn)
#' }
dbWriteTable_site <- function(conn, devman_name,
                              devman_comment = NULL) {
  write_table(name = "device_manufacturer", as.list(environment()))
}

#' Insert data into \code{device_type} table
#'
#' @param conn Database connection.
#' @param devtype_name String vector of name of device type.
#' @param devtype_comment String vector of additional comment.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' con <- dbConnect_klimageo()
#' dbWriteTable_device_type(con, "thermometer")
#' dbDisconnect(conn)
#' }
dbWriteTable_site <- function(conn, devtype_name,
                              devtype_comment = NULL) {
  write_table(name = "device_type", as.list(environment()))
}
