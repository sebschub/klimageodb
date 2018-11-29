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
#' \code{physical_quantity}, \code{calibration_state}, etc. Only the tables that
#' include measurement data allow a preselection of the data to retrieve only a
#' part of the data set.
#'
#' @inheritParams database_fields
#'
#' @return The functions return a data frame that contains the data from the
#'   respective remote table, effectively the result of calling
#'   \code{\link[DBI]{dbGetQuery}} with \code{SELECT * FROM <name>}. Commands
#'   that retrieve measurement data (\code{dbReadTable_station_*}) allow a
#'   preselection of the data with \code{start_datetime}, \code{end_datetime}
#'   and \code{md_id}; all other tables are retrieved completely. An error is
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


dbGetQuery_table <- function(conn, name, column_datetime,
                             start_datetime,
                             end_datetime,
                             md_id) {

  # build string for WHERE depending on which arguments are !NULL
  where_list <- list()
  if (!is.null(start_datetime)) {
    if (!inherits(start_datetime, "POSIXct")) {
      stop("start_datetime is used but it is not POSIXct.")
    }
    where_list[["start_datetime"]] <-
      paste0(column_datetime, " >= '", format.POSIXct(start_datetime, format = "%F %T%z"), "'")
  }
  if (!is.null(end_datetime)) {
    if (!inherits(end_datetime, "POSIXct")) {
      stop("end_datetime is used but it is not POSIXct.")
    }
    where_list[["end_datetime"]] <-
      paste0(column_datetime, " <= '", format.POSIXct(end_datetime, format = "%F %T%z"), "'")
  }
  if (!is.null(md_id)) {
    where_list[["md_id"]] <- paste0("md_id IN (", paste(md_id, collapse = ", "), ")")
  }

  if (length(where_list) > 0) {
    where_string <- paste("WHERE", paste(where_list, collapse = " AND "))
  } else {
    where_string <- ""
  }

  DBI::dbGetQuery(conn,
                  paste("SELECT * FROM", name,
                        where_string, ";"))
}

#' @rdname readtable
#' @export
dbReadTable_station_adlershof <- function(conn,
                                          start_datetime = NULL,
                                          end_datetime = NULL,
                                          md_id = NULL){
  dbGetQuery_table(conn, "station_adlershof", "stadl_datetime",
                   start_datetime, end_datetime, md_id)
}

#' @rdname readtable
#' @export
dbReadTable_station_adlershof_correction <- function(conn,
                                                     start_datetime = NULL,
                                                     end_datetime = NULL,
                                                     md_id = NULL){
  dbGetQuery_table(conn, "station_adlershof_correction", "stadlcor_datetime",
                   start_datetime, end_datetime, md_id)
}

#' @rdname readtable
#' @export
dbReadTable_station_patagonia <- function(conn,
                                          start_datetime = NULL,
                                          end_datetime = NULL,
                                          md_id = NULL){
  dbGetQuery_table(conn, "station_patagonia", "stapa_datetime",
                   start_datetime, end_datetime, md_id)
}

#' @rdname readtable
#' @export
dbReadTable_station_patagonia_correction <- function(conn,
                                                     start_datetime = NULL,
                                                     end_datetime = NULL,
                                                     md_id = NULL){
  dbGetQuery_table(conn, "station_patagonia_correction", "stapacor_datetime",
                   start_datetime, end_datetime, md_id)
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

#' @rdname readtable
#' @export
dbReadTable_station_adlershof_corrected <- function(conn,
                                                    start_datetime = NULL,
                                                    end_datetime = NULL,
                                                    md_id = NULL){
  dbGetQuery_table(conn, "station_adlershof_corrected", "stadl_datetime",
                   start_datetime, end_datetime, md_id)
}

#' @rdname readtable
#' @export
dbReadTable_station_patagonia_corrected <- function(conn,
                                                    start_datetime = NULL,
                                                    end_datetime = NULL,
                                                    md_id = NULL){
  dbGetQuery_table(conn, "station_patagonia_corrected", "stapa_datetime",
                   start_datetime, end_datetime, md_id)
}
