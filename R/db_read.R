#' Table reading
#'
#' These function retrieve the respective tables and views as a data frame from
#' the database.
#'
#' Most of the listed function retrieve present tables in the database. For
#' convinience, function of the form \code{dbReadTable_*_detail} receive the
#' views which join different database tables together. For example,
#' \code{dbReadTable_measurand_detail} retrieves the \code{measurand} table with
#' all id fields replaced by the corresponding information in
#' \code{physical_quantity}, \code{calibration_state}, etc.
#'
#' @inheritParams database_fields
#'
#' @return The functions return a data frame that contains the complete data
#'   from the respective remote table, effectively the result of calling
#'   \code{\link[DBI]{dbGetQuery}} with \code{SELECT * FROM <name>}. An error is
#'   raised if the table does not exist. An empty table is returned as a data
#'   frame with zero rows.
#' @name readtable
#' @seealso \code{\link[DBI]{dbReadTable}} for the general function to retrieve
#'   complete tables. \code{\link[DBI]{dbGetQuery}} and
#'   \url{http://db.rstudio.com/best-practices/run-queries-safely/} for more
#'   general SQL queries. Alternatively, you can use \code{\link[dplyr]{tbl}} of
#'   the \code{dplyr} package to only work on part of the database.
NULL


# Tables ------------------------------------------------------------------

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
dbReadTable_calibration_state <- function(conn){
  DBI::dbReadTable(conn, "calibration_state")
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


# Views -------------------------------------------------------------------

#' @rdname readtable
#' @export
dbReadTable_device_model_detail <- function(conn){
  DBI::dbReadTable(conn, "device_model_detail")
}

#' @rdname readtable
#' @export
dbReadTable_device_detail <- function(conn){
  DBI::dbReadTable(conn, "device_detail")
}

#' @rdname readtable
#' @export
dbReadTable_calibration_state_detail <- function(conn){
  DBI::dbReadTable(conn, "calibration_state_detail")
}

#' @rdname readtable
#' @export
dbReadTable_integration_detail <- function(conn){
  DBI::dbReadTable(conn, "integration_detail")
}

#' @rdname readtable
#' @export
dbReadTable_measurand_detail <- function(conn){
  DBI::dbReadTable(conn, "measurand_detail")
}
