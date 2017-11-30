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
#' dbDisconnect(con)
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
#' dbDisconnect(con)
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
#' dbDisconnect(con)
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
#' @param devtype_id Integer vector of \code{device_type} IDs.
#' @param devman_id Integer vector of \code{device_manufacturer} IDs.
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
#' dbDisconnect(con)
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
#' @param devmod_id Integer vector of \code{device_model} ID.
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
#' dbDisconnect(con)
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
#' @param dev_id Integer vector of \code{device} ID.
#' @param caldev_datetime POSIXct vector of date and time of calibration.
#' @param caldev_parameter String vector of values of calibration parameters.
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
#' dbDisconnect(con)
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



#' Insert data into \code{device} and \code{calibrated_device} table with
#' calibration parameters \code{NULL}
#'
#' @description This function adds new devices to the database that do not
#'   require any calibration. First, it adds the new devices into the
#'   \code{device} table. It then uses the respective created
#'   \code{device.dev_id} to add new entries in \code{calibrated_device} with both
#'   \code{calibrated_device.caldev_datetime} and
#'   \code{calibrated_device.caldev_parameter} being \code{NULL}.
#'
#' @param conn Database connection.
#' @param dev_name String vector of name of device.
#' @param devmod_id Integer vector of \code{device_model} ID.
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
#' dbDisconnect(con)
#' }
dbWriteTable_uncalibrated_device <- function(conn,
                                             dev_name,
                                             devmod_id,
                                             dev_identifier = NULL,
                                             dev_comment = NULL) {
  # use transaction to ensure either both, device and calibrated_device, were
  # changed or none
  invisible(DBI::dbWithTransaction(conn, {
    # write device into "device" table
    write_table(name = "device", as.list(environment()))
    # surround dev_name with "'" and concatinate them
    dev_name_string <- paste0(paste0("'", dev_name, "'", collapse = ", "))
    # get IDs of newly created rows, CHECK IF ORDER IS OK!
    dev_id <- DBI::dbGetQuery(conn,
                              paste0("SELECT dev_id FROM device WHERE dev_name in (",
                                     dev_name_string,");")
    )
    DBI::dbWriteTable(conn, name = "calibrated_device",
                      value = data.frame(dev_id = dev_id), append = TRUE)
  }))

}




#' Insert data into \code{physical_quantity} table
#'
#' @param conn Database connection.
#' @param pq_name String vector of name of physical quantity.
#' @param pq_unit String vector of units of physical quantity. Use "1" for
#'   unitless quantities.
#' @param pq_comment String vector of additional comments.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' con <- dbConnect_klimageo()
#' dbWriteTable_physical_quantity(con, "air temperature", "degC")
#' dbDisconnect(con)
#' }
dbWriteTable_physical_quantity <- function(conn,
                                           pq_name,
                                           pq_unit,
                                           pq_comment = NULL) {
  write_table(name = "physical_quantity", as.list(environment()))
}



#' Insert data into \code{integration_type} table
#'
#' @param conn Database connection.
#' @param inttype_name String vector of name of integration type.
#' @param inttype_description String vector of description of \code{integration_type}.
#' @param inttype_comment String vector of additional comments.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' con <- dbConnect_klimageo()
#' dbWriteTable_integration_type(con, "average", "average value over a period")
#' dbDisconnect(con)
#' }
dbWriteTable_integration_type <- function(conn,
                                          inttype_name,
                                          inttype_description,
                                          inttype_comment = NULL) {
  write_table(name = "integration_type", as.list(environment()))
}



#' Insert data into \code{integration} table
#'
#' @param conn Database connection.
#' @param inttype_id Integer vector of \code{integration_type} ID.
#' @param int_measurement_interval Numeric vector of intervals between measurements in s.
#' @param int_interval Numeric vector of integration interval in s of one stored measurement.
#' @param int_comment Character vector of additional information.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' con <- dbConnect_klimageo()
#' dbWriteTable_integration_type("average", "average value over a period")
#' dbWriteTable_integration(con, 1, 60, 600)
#' dbDisconnect(con)
#' }
dbWriteTable_integration <- function(conn,
                                          inttype_id,
                                          int_measurement_interval,
                                          int_interval,
                                          int_comment = NULL) {
  write_table(name = "integration", as.list(environment()))
}




#' Insert data into \code{person} table
#'
#' @param conn Database connection.
#' @param pers_name Character vector of person name.
#' @param pers_comment Character vector of additional information.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' con <- dbConnect_klimageo()
#' dbWriteTable_person(con, "Maxi Mueller")
#' dbDisconnect(con)
#' }
dbWriteTable_person <- function(conn,
                                pers_name,
                                pers_comment = NULL) {
  write_table(name = "person", as.list(environment()))
}



#' Insert data into \code{measurand} table
#'
#' @param conn Database connection.
#' @param md_name Character vector of measurand name.
#' @param md_setup_datetime POSIXct vector of date and time of set-up of measurand.
#' @param pq_id Integer vector of \code{physical_quantity} ID.
#' @param site_id Integer vector of \code{site} ID.
#' @param caldev_id Integer vector of \code{calibrated_device} ID.
#' @param int_id Integer vector of \code{integration} ID.
#' @param md_height Numeric vector of measurement height.
#' @param pers_id Integer vector of \code{person} ID.
#' @param md_comment Character vector of additional information.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' con <- dbConnect_klimageo()
#' # add all required entries before with the respective dbWriteTable_*
#' dbWriteTable_measurand(md_name = "TA2M_1",
#'                        md_setup_datetime = as.POSIXct("2012-01-01 12:15:12", tz = "UTC"),
#'                        pq_id = 1,
#'                        site_id = 1,
#'                        caldev_id = 1,
#'                        int_id = 1,
#'                        md_height = 2.,
#'                        pers_id = 1,
#'                        md_comment = "the 2m temperature"))
#' dbDisconnect(con)
#' }
dbWriteTable_measurand <- function(conn,
                                md_name,
                                md_setup_datetime,
                                pq_id,
                                site_id,
                                caldev_id,
                                int_id,
                                md_height = NULL,
                                pers_id = NULL,
                                md_comment = NULL) {
  if (!inherits(md_setup_datetime, "POSIXct")) {
    stop("md_setup_datetime is not POSIXct")
  }
  write_table(name = "measurand", as.list(environment()))
}


#' Insert data into \code{quality_flag} table
#'
#' @param conn Database connection.
#' @param qf_id Integer vector with 1 <= qf_id <= 9 indicating value ok and
#'   qf_id >= 10 indicating value not ok;
#' @param qf_name Character vector of quality_flag name.
#' @param qf_description Character vector of quality_flag description.
#' @param qf_comment Character vector of of additional information..
#'
#' @export
#'
#' @examples
#' \dontrun{
#' con <- dbConnect_klimageo()
#' dbWriteTable_quality_flag(
#'    qf_id = c(1,11),
#'    qf_name = c("value ok, automatic qc", "corrected calculation constant"),
#'    qf_description = c("value ok, automatically checked for consistency",
#'                       "original value used from calculation constant")))
#' dbDisconnect(con)
#' }
dbWriteTable_quality_flag <- function(conn,
                                      qf_id,
                                      qf_name,
                                      qf_description,
                                      qf_comment = NULL) {
  write_table(name = "quality_flag", as.list(environment()))
}


#' Insert data into \code{station_adlershof} table
#'
#' @param conn Database connection.
#' @param stadl_datetime POSIXct vector of date and time of measurement.
#' @param md_id Integer vector of measurand ID.
#' @param stadl_value Numeric vector of measurement values.
#' @param qf_id Integer vector with 1 <= qf_id <= 9 indicating value ok and
#'   qf_id >= 10 indicating value not ok;
#' @export
#'
#' @examples
#' \dontrun{
#' con <- dbConnect_klimageo()
#' # add all required entries before with the respective dbWriteTable_*
#' dbWriteTable_station_adlershof
#' dbDisconnect(con)
#' }
dbWriteTable_station_adlershof <- function(conn,
                                           stadl_datetime,
                                           md_id,
                                           stadl_value,
                                           qf_id = NULL) {
  if (!inherits(stadl_datetime, "POSIXct")) {
    stop("stadl_datetime is not POSIXct")
  }
  write_table(name = "station_adlershof", as.list(environment()))
}
