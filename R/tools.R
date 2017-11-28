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
#' @param site_comment String vector of additional comments.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' con <- dbConnect_klimageo()
#' dbWriteTable_site(con, "Adlershof", site_lon = 14., site_lat = 53.)
#' dbDisconnect(conn)
#' }
dbWriteTable_site <- function(conn,
                              site_name,
                              site_lat = NULL, site_lon = NULL, site_altitude = NULL,
                              site_comment = NULL) {
  write_table(name = "site", as.list(environment()))
}


#' Insert data into \code{device_manufacturer} table
#'
#' @param conn Database connection.
#' @param devman_name String vector of name of device manufacturer.
#' @param devman_comment String vector of additional comments.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' con <- dbConnect_klimageo()
#' dbWriteTable_device_manufacturer(con, "TSI")
#' dbDisconnect(conn)
#' }
dbWriteTable_device_manufacturer <- function(conn,
                                             devman_name,
                                             devman_comment = NULL) {
  write_table(name = "device_manufacturer", as.list(environment()))
}

#' Insert data into \code{device_type} table
#'
#' @param conn Database connection.
#' @param devtype_name String vector of name of device type.
#' @param devtype_comment String vector of additional comments.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' con <- dbConnect_klimageo()
#' dbWriteTable_device_type(con, "thermometer")
#' dbDisconnect(conn)
#' }
dbWriteTable_device_type <- function(conn,
                                     devtype_name,
                                     devtype_comment = NULL) {
  write_table(name = "device_type", as.list(environment()))
}


#' Insert data into \code{device_model} table
#'
#' @param conn Database connection.
#' @param devmod_name String vector of name of model.
#' @param devtype_id Integer vector of IDs from \code{device_type}.
#' @param devman_id Integer vector of IDs from \code{device_manufacturer}.
#' @param devmod_comment String vector of additional comments.
#'
#'
#' @export
#'
#' @examples
#' \dontrun{
#' con <- dbConnect_klimageo()
#' dbWriteTable_device_manufacturer(con, "TSI")
#' dbWriteTable_device_type(con, "thermometer")
#' dbWriteTable_device_model(con, "THERMO1000", 1, 1)
#' dbDisconnect(conn)
#' }
dbWriteTable_device_model <- function(conn,
                                      devmod_name,
                                      devtype_id,
                                      devman_id = NULL,
                                      devmod_comment = NULL) {
  write_table(name = "device_model", as.list(environment()))
}


#' Insert data into \code{device} table
#'
#' @param conn Database connection.
#' @param dev_name String vector of name of device.
#' @param devmod_id Integer vector of device_model ID.
#' @param dev_identifier String vector of device identifiers, e.g. serial
#'   numbers.
#' @param dev_comment String vector of additional comments.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' con <- dbConnect_klimageo()
#' dbWriteTable_device_manufacturer(con, "TSI")
#' dbWriteTable_device_type(con, "thermometer")
#' dbWriteTable_device_model(con, "THERMO1000", 1, 1)
#' dbWriteTable_device(con, "My first THERMO1000", 1, "NCC1701-T")
#' dbDisconnect(conn)
#' }
dbWriteTable_device <- function(conn,
                                dev_name,
                                devmod_id,
                                dev_identifier = NULL,
                                dev_comment = NULL) {
  write_table(name = "device", as.list(environment()))
}


#' Insert data into \code{calibrated_device} table
#'
#' @param conn Database connection.
#' @param dev_id Integer vector of device ID.
#' @param caldev_datetime POSIXct vector of date and time of calibration.
#' @param caldev_parameter String vector of values of calibration parameters.'
#' @param caldev_comment String vector of additional comments.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' con <- dbConnect_klimageo()
#' dbWriteTable_device_manufacturer(con, "TSI")
#' dbWriteTable_device_type(con, "thermometer")
#' dbWriteTable_device_model(con, "THERMO1000", 1, 1)
#' dbWriteTable_device(con, "My first THERMO1000", 1, "NCC1701-T")
#' dbWriteTable_calibrated_device(con,
#'                                1,
#'                                as.POSIXct("2012-01-01 12:15:12", tz = "GMT"),
#'                                "a=10, b=99.12")
#' dbDisconnect(conn)
#' }
dbWriteTable_calibrated_device <- function(conn,
                                           dev_id,
                                           caldev_datetime = NULL,
                                           caldev_parameter = NULL,
                                           caldev_comment = NULL) {
  # we need caldev_datetime to be POSIXct for consistent time zone storage
  if (!is.null(caldev_datetime) & !inherits(caldev_datetime, "POSIXct")) {
    stop("caldev_datetime is used but not POSIXct")
  }
  write_table(name = "calibrated_device", as.list(environment()))
}
