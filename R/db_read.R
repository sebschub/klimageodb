#' Table reading
#'
#' These function retrieve the respective tables as a data frame from the
#' database.
#'
#' @param conn Database connection.
#' @name readtable
#' @seealso \code{\link[DBI]{dbReadTable}} for the general function to retrieve
#'   complete tables. \code{\link[DBI]{dbGetQuery}} and
#'   \url{http://db.rstudio.com/best-practices/run-queries-safely/} for more
#'   general SQL queries. Alternatively, you can use \code{\link[dplyr]{tbl}} of
#'   the \code{dplyr} package to only work on part of the database.
NULL

#' @rdname readtable
#' @export
dbReadTable_site <- function(conn){
  DBI::dbReadTable(conn, "site")
}

#' @rdname readtable
#' @export
dbReadTable_device_manufacturer <- function(conn){
  DBI::dbReadTable(conn, "device_manufacturer")
}

#' @rdname readtable
#' @export
dbReadTable_device_type <- function(conn){
  DBI::dbReadTable(conn, "device_type")
}

#' @rdname readtable
#' @export
dbReadTable_device_model <- function(conn){
  DBI::dbReadTable(conn, "device_model")
}

#' @rdname readtable
#' @export
dbReadTable_device <- function(conn){
  DBI::dbReadTable(conn, "device")
}

#' @rdname readtable
#' @export
dbReadTable_calibrated_device <- function(conn){
  DBI::dbReadTable(conn, "calibrated_device")
}

#' @rdname readtable
#' @export
dbReadTable_physical_quantity <- function(conn){
  DBI::dbReadTable(conn, "physical_quantity")
}

#' @rdname readtable
#' @export
dbReadTable_integration_type <- function(conn){
  DBI::dbReadTable(conn, "integration_type")
}

#' @rdname readtable
#' @export
dbReadTable_integration <- function(conn){
  DBI::dbReadTable(conn, "integration")
}

#' @rdname readtable
#' @export
dbReadTable_person <- function(conn){
  DBI::dbReadTable(conn, "person")
}

#' @rdname readtable
#' @export
dbReadTable_measurand <- function(conn){
  DBI::dbReadTable(conn, "measurand")
}

#' @rdname readtable
#' @export
dbReadTable_quality_flag <- function(conn){
  DBI::dbReadTable(conn, "quality_flag")
}

#' @rdname readtable
#' @export
dbReadTable_station_adlershof <- function(conn){
  DBI::dbReadTable(conn, "station_adlershof")
}

#' @rdname readtable
#' @export
dbReadTable_station_adlershof_correction <- function(conn){
  DBI::dbReadTable(conn, "station_adlershof_correction")
}
