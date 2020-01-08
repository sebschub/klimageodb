#' Collection of arguments to "@inheritParams" from
#'
#'
#' @param conn Database connection.
#' @param calstate_id Integer vector of \code{calibration_state} ID.
#' @param calstate_datetime POSIXct vector of date and time of calibration.
#' @param calstate_parameter String vector of values of calibration parameters.
#' @param calstate_comment String vector of additional comments.
#' @param dev_id Integer vector of \code{device} ID.
#' @param dev_name String vector of name of device.
#' @param dev_identifier String vector of device identifiers, e.g. serial
#'   numbers.
#' @param dev_comment String vector of additional comments.
#' @param devman_id Integer vector of \code{device_manufacturer} IDs.
#' @param devman_name String vector of name of device manufacturer.
#' @param devman_comment String vector of additional comments.
#' @param devmod_id Integer vector of \code{device_model} ID.
#' @param devmod_name String vector of name of model.
#' @param devmod_comment String vector of additional comments.
#' @param devtype_id Integer vector of \code{device_type} IDs.
#' @param devtype_name String vector of name of device type.
#' @param devtype_comment String vector of additional comments.
#' @param end_datetime End date and time as a single POSIXct value.
#' @param int_id Integer vector of \code{integration} ID.
#' @param int_measurement_interval Numeric vector of intervals between
#'   measurements in s.
#' @param int_interval Numeric vector of integration interval in s of one stored
#'   measurement.
#' @param int_comment Character vector of additional information.
#' @param inttype_id Integer vector of \code{integration_type} ID.
#' @param inttype_name String vector of name of integration type.
#' @param inttype_description String vector of description of
#'   \code{integration_type}.
#' @param inttype_comment String vector of additional comments.
#' @param md_id Integer vector of measurand ID.
#' @param md_comment Character vector of additional information.
#' @param md_name Character vector of measurand name.
#' @param md_setup_datetime POSIXct vector of date and time of set-up of
#'   measurand.
#' @param md_height Numeric vector of measurement height.
#' @param md_orientation Numeric vector of north-south measurement orientation.
#' @param md_tilt Numeric vector of measurement tilt.
#' @param name Name of the table.
#' @param pers_id Integer vector of \code{person} ID.
#' @param pers_name Character vector of person name.
#' @param pers_comment Character vector of additional information.
#' @param standard_name Standard name of a physical quantity as defined by the
#'   CF convention.
#' @param pq_id Integer vector of \code{physical_quantity} ID.
#' @param pq_name String vector of name of physical quantity.
#' @param pq_unit String vector of units of physical quantity. Use "1" for
#'   unitless quantities.
#' @param pq_description String vector of description of physical quantity.
#' @param pq_comment String vector of additional comments.
#' @param qf_id Integer vector with 1 <= qf_id <= 9 indicating value ok and
#'   qf_id >= 10 indicating value not ok.
#' @param qf_id_good Integer value with 1 <= qf_id_good <= 9 that will be
#'   written to the non-selected points indicating value ok.
#' @param qf_id_bad Integer value with qf_id_bad >= 10 that will be written to
#'   the selected points indicating value not ok and incorrectable.
#' @param qf_name Character vector of quality_flag name.
#' @param qf_description Character vector of quality_flag description.
#' @param qf_comment Character vector of of additional information.
#' @param site_id Integer vector of \code{site} ID.
#' @param site_name String vector of name of site or name of campaign.
#' @param site_lat Numeric vector of geographical latitude WGS84.
#' @param site_lon Numeric vector of geographical longitude WGS84.
#' @param site_altitude Numeric vector of height above sea level of surface in
#'   m.
#' @param site_comment String vector of additional comments.
#' @param stadl_id Integer vector of \code{station_adlershof} ID.
#' @param stadl_datetime POSIXct vector of date and time of measurement.
#' @param stadl_value Numeric vector of measurement values.
#' @param stadlcor_datetime POSIXct vector of corrected date and time of
#'   measurement.
#' @param stadlcor_value Numeric vector of corrected value of measurement.
#' @param stapa_id Integer vector of \code{station_patagonia} ID.
#' @param stapa_datetime POSIXct vector of date and time of measurement.
#' @param stapa_value Numeric vector of measurement values.
#' @param stapacor_datetime POSIXct vector of corrected date and time of
#'   measurement.
#' @param stapacor_value Numeric vector of corrected value of measurement.
#' @param start_datetime Start date and time as a single POSIXct value.

#'
#' @name database_fields
#' @keywords internal
NULL

#' Get primary key name of a table
#'
#' @inheritParams database_fields
#'
#' @return String of primary key.
#' @keywords internal
get_primarykey <- function(conn, name) {
  # get name of primary key, from https://wiki.postgresql.org/wiki/Retrieve_primary_key_columns
  DBI::dbGetQuery(
    conn,
    paste0("SELECT a.attname FROM pg_index i JOIN pg_attribute a ON a.attrelid = i.indrelid AND a.attnum = ANY(i.indkey) WHERE i.indrelid = '",
           name,
           "'::regclass AND i.indisprimary;")
  )[1,1]
}



#' Get columns as a data frame from a table
#'
#' @param conn Database connection.
#' @param name Name of the table.
#' @param columns Character vector of names of the column.
#'
#' @return Data frame of columns.
#' @keywords internal
get_columns <- function(conn, name, columns) {
  columns_string <- paste(columns, collapse = ",")
  DBI::dbGetQuery(
    conn,
    paste("SELECT", columns_string, "FROM", name, ";")
  )
}


get_ids_from_unique_column <- function(conn,
                                      table, id_name,
                                      column_name, column_values) {
  # do this via factor
  column_ids <- as.factor(column_values)
  # get for each level of column_values the id

  for (cil in seq_along(levels(column_ids))) {
    query <- DBI::dbGetQuery(
      conn,
      paste0("SELECT ", id_name, " FROM ", table,
             " WHERE ", column_name, "='", levels(column_ids)[cil], "';")
    )
    if (!identical(dim(query), as.integer(c(1, 1))) ) {
      stop(paste("Query in", table, "with", id_name, "failed."))
    }
    levels(column_ids)[cil] <- query[1,1]
  }
  column_ids
}


get_ids_from_datetime_column <- function(conn,
                                      table, id_name,
                                      column_name, column_values,
                                      column_datetime) {
  # do this via factor
  column_ids <- as.factor(column_values)
  # get for each level of column_values the id

  for (cil in seq_along(levels(column_ids))) {
    levels(column_ids)[cil] <- DBI::dbGetQuery(
      conn,
      paste0("SELECT ", id_name, " FROM ", table,
             " WHERE ", column_name, "='", levels(column_ids)[cil], "'",
             " ORDER BY ", column_datetime ," DESC LIMIT 1;")
    )[1,1]
  }
  column_ids
}



#' @rdname dbWithTransaction_or_Savepoint
#' @export
#' @keywords internal
dbBegin_or_Savepoint <- function(conn, spname) {
  tryCatch({
    DBI::dbBegin(conn)
    TRUE
  }, error = function(e) {
    DBI::dbExecute(conn, paste("SAVEPOINT", spname, ";"))
    FALSE
  })
}

#' @rdname dbWithTransaction_or_Savepoint
#' @export
#' @keywords internal
dbCommit_or_Savepoint <- function(conn, transaction, spname) {
  if (transaction) {
    DBI::dbCommit(conn)
  } else {
    DBI::dbExecute(conn, paste("RELEASE SAVEPOINT", spname, ";"))
  }
}

#' @rdname dbWithTransaction_or_Savepoint
#' @export
#' @keywords internal
dbRollback_or_Savepoint <- function(conn, transaction, spname) {
  if (transaction) {
    DBI::dbRollback(conn)
  } else {
    DBI::dbExecute(conn, paste("ROLLBACK TO SAVEPOINT", spname, ";"))
    DBI::dbExecute(conn, paste("RELEASE SAVEPOINT", spname, ";"))
  }
}

#' Self-contained SQL commands within new transactions or savepoint in present
#' transaction
#'
#' The function \code{dbWithTransaction_or_Savepoint()} starts a new transaction
#' or issues a savepoint via \code{dbBegin_or_Savepoint()}. If the execution of
#' the \code{code} argument fails, the state will be committed or the savepoint
#' removed via \code{dbCommit_or_Savepoint()}. Otherwise, the transaction is
#' rollbacked completely or up until the savepoint via
#' \code{dbRollback_or_Savepoint()}. The functions work similiarly to
#' \code{\link[DBI]{dbWithTransaction}()}, \code{\link[DBI]{dbBegin}()},
#' \code{\link[DBI]{dbCommit}()} or \code{\link[DBI]{dbRollback}()},
#' respectively, but allow to be nested by using savepoints if necessary.
#'
#' @inheritParams database_fields
#' @param code An arbitrary block of R code.
#' @param spname String of the possibly required savpoint.
#' @param transaction Use standard transactions or use Savepoints?
#'
#' @return \code{dbWithTransaction_or_Savepoint()} returns the value of the
#'   executed code.
#' @export
#' @keywords internal
dbWithTransaction_or_Savepoint <- function(conn, code, spname) {
  new_transaction <- dbBegin_or_Savepoint(conn, spname)
  code_result <- tryCatch(code,
                          error = function(e){
                            dbRollback_or_Savepoint(conn, new_transaction, spname)
                            stop(e$message)
                          })
  dbCommit_or_Savepoint(conn, new_transaction, spname)
  if (!is.null(code_result)) code_result
}


#' Write table to database
#'
#' @param name Name of the table.
#' @param arg_list List with first element being DBIConnection to the target
#'   database and rest being columns of same length to be put into the table.
#' @param return_newrows Return the newly added rows?
#'
#' @return Newly added rows or NULL.
#' @keywords internal
write_table <- function(name, arg_list, return_newrows = TRUE) {
  # conn is first in list
  conn <- arg_list[[1]]

  # convert the rest to data frame ignoring NULL
  df <- arg_list[-1]
  df <- do.call(cbind.data.frame, df[!sapply(df, is.null)])

  dbWithTransaction_or_Savepoint(conn, {
    # get pk values before insertion of data
    if (return_newrows) {
      pk_string <- get_primarykey(conn, name)
      old_pk_values <- get_columns(conn, name, pk_string)[[1]]
    }

    # write the table
    DBI::dbWriteTable(conn, name = name, value = df, append = TRUE)

    if (return_newrows) {
      # get pk values after insertion of data
      new_pk_values <- get_columns(conn, name, pk_string)[[1]]
      diff_pk_values <- setdiff(new_pk_values, old_pk_values)
      if (length(diff_pk_values > 0)) {
        diff_pk_string <- paste0(paste0("'",
                                        diff_pk_values,
                                        "'",
                                        collapse = ", ")
        )
        DBI::dbGetQuery(conn,
                        paste("SELECT * FROM", name,
                              "WHERE", pk_string, "in (", diff_pk_string, ");"
                        ))
      }
    }
  }, spname = "write_table_savepoint")

}

#' Insert data into \code{site} table
#'
#' @inheritParams database_fields
#'
#' @return Data frame of newly added rows.
#' @family custom dbWriteTable functions
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
#'
#' @inheritParams database_fields
#'
#' @return Data frame of newly added rows.
#' @family custom dbWriteTable functions
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
#'
#' @inheritParams database_fields
#'
#' @return Data frame of newly added rows.
#' @family custom dbWriteTable functions
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




#' Add device models to \code{device_model} table
#'
#' \code{dbWriteTable_device_model} requires a correct device type id and,
#' optionally, a correct device manufacturer id while \code{dbAdd_device_model}
#' derives that from the device type and the device manufacturer name,
#' respectively.
#'
#' @inheritParams database_fields
#'
#' @return Data frame of newly added rows.
#' @family custom dbWriteTable functions
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
dbAdd_device_model <- function(conn,
                               devmod_name,
                               devtype_name,
                               devman_name = NULL,
                               devmod_comment = NULL) {

  dbWithTransaction_or_Savepoint(conn, {
    devtype_id <- get_ids_from_unique_column(conn,
                                             table = "device_type",
                                             id_name = "devtype_id",
                                             column_name = "devtype_name",
                                             column_values = devtype_name)
    if (!is.null(devman_name)) {
      devman_id <- get_ids_from_unique_column(conn,
                                              table = "device_manufacturer",
                                              id_name = "devman_id",
                                              column_name = "devman_name",
                                              column_values = devman_name)
    } else {
      devman_id <- NULL
    }
    dbWriteTable_device_model(conn,
                              devmod_name = devmod_name,
                              devtype_id = devtype_id,
                              devman_id = devman_id,
                              devmod_comment = devmod_comment)
  }, spname = "dbAdd_device_model")
}




#' @rdname dbAdd_device_model
#' @export
dbWriteTable_device_model <- function(conn,
                                      devmod_name,
                                      devtype_id,
                                      devman_id = NULL,
                                      devmod_comment = NULL) {
  write_table(name = "device_model", as.list(environment()))
}



#' Add devices to \code{device} table
#'
#' \code{dbWriteTable_device} requires a correct device model id while
#' \code{dbAdd_device} derives that from the device model name.
#'
#' @inheritParams database_fields
#'
#' @return Data frame of newly added rows.
#' @seealso \code{\link{dbAdd_device_uncalibrated}} for adding a device that
#'   does not require calibration to both \code{device} and
#'   \code{calibration_state}
#' @family custom dbWriteTable functions
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
dbAdd_device <- function(conn,
                         dev_name,
                         devmod_name,
                         dev_identifier = NULL,
                         dev_comment = NULL) {

  dbWithTransaction_or_Savepoint(conn, {
    devmod_id <- get_ids_from_unique_column(conn,
                                            table = "device_model",
                                            id_name = "devmod_id",
                                            column_name = "devmod_name",
                                            column_values = devmod_name)
    dbWriteTable_device(conn,
                        dev_name = dev_name,
                        devmod_id = devmod_id,
                        dev_identifier = dev_identifier,
                        dev_comment = dev_comment)
  }, spname = "dbAdd_device")
}



#' @rdname dbAdd_device
#' @export
dbWriteTable_device <- function(conn,
                                dev_name,
                                devmod_id,
                                dev_identifier = NULL,
                                dev_comment = NULL) {
  write_table(name = "device", as.list(environment()))
}


#' Add calibrated devices to \code{calibration_state} table
#'
#' \code{dbWriteTable_calibration_state} requires a correct device id while
#' \code{dbAdd_calibration_state} derives that from the device name.
#'
#' @inheritParams database_fields
#'
#' @return Data frame of newly added rows.
#' @seealso \code{\link{dbAdd_device_uncalibrated}} for adding a device that
#'   does not require calibration to both \code{device} and
#'   \code{calibration_state}
#' @family custom dbWriteTable functions
#' @export
#'
#' @examples
#' \dontrun{
#' con <- dbConnect_klimageo()
#' dbWriteTable_device_manufacturer(con, "TSI")
#' dbWriteTable_device_type(con, "thermometer")
#' dbWriteTable_device_model(con, "THERMO1000", 1, 1)
#' dbWriteTable_device(con, "My first THERMO1000", 1, "NCC1701-T")
#' dbWriteTable_calibration_state(con,
#'                                1,
#'                                as.POSIXct("2012-01-01 12:15:12", tz = "GMT"),
#'                                "a=10, b=99.12")
#' dbDisconnect(con)
#' }
dbAdd_calibration_state <- function(conn,
                                    dev_name,
                                    calstate_datetime = NULL,
                                    calstate_parameter = NULL,
                                    calstate_comment = NULL) {
  dbWithTransaction_or_Savepoint(conn, {
    dev_id <- get_ids_from_unique_column(conn,
                                         table = "device",
                                         id_name = "dev_id",
                                         column_name = "dev_name",
                                         column_values = dev_name)
    dbWriteTable_calibration_state(conn,
                                   dev_id = dev_id,
                                   calstate_datetime = calstate_datetime,
                                   calstate_parameter = calstate_parameter,
                                   calstate_comment = calstate_comment)
  }, spname = "dbAdd_calibration_state")

}





#' @rdname dbAdd_calibration_state
#' @export
dbWriteTable_calibration_state <- function(conn,
                                           dev_id,
                                           calstate_datetime = NULL,
                                           calstate_parameter = NULL,
                                           calstate_comment = NULL) {
  # we need calstate_datetime to be POSIXct for consistent time zone storage
  if (!is.null(calstate_datetime) & !inherits(calstate_datetime, "POSIXct")) {
    stop("calstate_datetime is used but not POSIXct.")
  }
  write_table(name = "calibration_state", as.list(environment()))
}





#' Insert data into \code{device} and \code{calibration_state} without
#' calibration parameters
#'
#' This function adds new devices to the database that do not require any
#' calibration. First, it adds the new devices into the \code{device} table. It
#' then uses the respective created \code{device.dev_id} to add new entries in
#' \code{calibration_state} with both \code{calibration_state.calstate_datetime}
#' and \code{calibration_state.calstate_parameter} being \code{NA}.
#'
#' @inheritParams database_fields
#'
#' @return List of data frames of newly added rows.
#' @seealso \code{\link{dbWriteTable_device}} and
#'   \code{\link{dbWriteTable_calibration_state}} for the separate functions
#'   called by this wrapper.
#' @export
#'
#' @examples
#' \dontrun{
#' con <- dbConnect_klimageo()
#' dbWriteTable_device_manufacturer(con, "TSI")
#' dbWriteTable_device_type(con, "thermometer")
#' dbWriteTable_device_model(con, "THERMO1000", 1, 1)
#' dbAdd_device_uncalibrated(con, "My first THERMO1000", 1, "NCC1701-T")
#' dbDisconnect(con)
#' }
dbAdd_device_uncalibrated <- function(conn,
                                      dev_name,
                                      devmod_name,
                                      dev_identifier = NULL,
                                      dev_comment = NULL) {
  # use transaction to ensure either both, device and calibration_state, were
  # changed or none
  dbWithTransaction_or_Savepoint(conn, {
    # write device into "device" table
    new_device <- dbAdd_device(conn,
                               dev_name = dev_name,
                               devmod_name = devmod_name,
                               dev_identifier = dev_identifier,
                               dev_comment = dev_comment)
    new_calibration_state <- dbWriteTable_calibration_state(conn, dev_id = new_device$dev_id)
  }, spname = "dbAdd_device_uncalibrated_savepoint")
  list(device = new_device, calibration_state = new_calibration_state)

}



#' Add a physical quantity into \code{physical_quantity} table
#'
#' These function add rows defining a physical quantity into
#' \code{physical_quantity}. \strong{Follow the
#' \href{http://cfconventions.org/Data/cf-standard-names/48/build/cf-standard-name-table.html}{CF
#' conventions} as far as possible.} \code{dbAdd_physical_quantity} supports
#' this by getting both the unit and the description from valid standard names
#' set with \code{pq_name}. \code{dbWriteTable_physical_quantity} allows
#' entering the strings manually, however, it does not enforce valid CF strings.
#'
#' \code{dbAdd_physical_quantity} queries
#' \url{http://cfconventions.org/Data/cf-standard-names/48/src/cf-standard-name-table.xml}
#' once per session. Thus, it requires an internet connection.
#'
#'
#' @inheritParams database_fields
#'
#' @return Data frame of newly added rows.
#' @family custom dbWriteTable functions
#' @export
#'
#' @examples
#' \dontrun{
#' con <- dbConnect_klimageo()
#' dbWriteTable_physical_quantity(con, "air temperature", "degC")
#' dbDisconnect(con)
#' }
dbAdd_physical_quantity <- function(conn,
                                    pq_name,
                                    pq_comment = NULL) {
  unit_description <- get_cf_unit_description(pq_name)
  dbWriteTable_physical_quantity(conn,
                                 pq_name = pq_name,
                                 pq_unit = unit_description$unit,
                                 pq_description = unit_description$description,
                                 pq_comment = pq_comment)
}




#' @rdname dbAdd_physical_quantity
#' @export
dbWriteTable_physical_quantity <- function(conn,
                                           pq_name,
                                           pq_unit,
                                           pq_description = NULL,
                                           pq_comment = NULL) {
  write_table(name = "physical_quantity", as.list(environment()))
}




#' Insert data into \code{integration_type} table
#'
#'
#' @inheritParams database_fields
#'
#' @return Data frame of newly added rows.
#' @family custom dbWriteTable functions
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



#' Add integration kind to \code{integration} table
#'
#' \code{dbWriteTable_integration} requires a correct device integration type id
#' while \code{dbAdd_integration} derives that from the device model name.
#'
#' @inheritParams database_fields
#'
#' @return Data frame of newly added rows.
#' @family custom dbWriteTable functions
#' @export
#'
#' @examples
#' \dontrun{
#' con <- dbConnect_klimageo()
#' dbWriteTable_integration_type("average", "average value over a period")
#' dbWriteTable_integration(con, 1, 60, 600)
#' dbDisconnect(con)
#' }
dbAdd_integration <- function(conn,
                              inttype_name,
                              int_measurement_interval,
                              int_interval,
                              int_comment = NULL) {
  dbWithTransaction_or_Savepoint(conn, {
    inttype_id <- get_ids_from_unique_column(conn,
                                             table = "integration_type",
                                             id_name = "inttype_id",
                                             column_name = "inttype_name",
                                             column_values = inttype_name)
    dbWriteTable_integration(conn,
                             inttype_id = inttype_id,
                             int_measurement_interval = int_measurement_interval,
                             int_interval = int_interval,
                             int_comment = int_comment)
  }, spname = "dbAdd_integration")

}




#' @rdname dbAdd_integration
#' @export
dbWriteTable_integration <- function(conn,
                                     inttype_id,
                                     int_measurement_interval,
                                     int_interval,
                                     int_comment = NULL) {
  write_table(name = "integration", as.list(environment()))
}



#' Insert data into \code{person} table
#'
#'
#' @inheritParams database_fields
#'
#' @return Data frame of newly added rows.
#' @family custom dbWriteTable functions
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



#' Add measurand to \code{measurand} table
#'
#' \code{dbWriteTable_measurand} requires a correct physical quantity id, site
#' id, calibrated device id, integration id and, optionally, person id.
#' \code{dbAdd_measurand} derives the physical quantity id, site id, calibrated
#' device id and person id from the respective names; integration id still have
#' to be entered numerical as id. \strong{Note} that \code{dbAdd_measurand}
#' derives the calibrated device id from the device id with the most recent
#' calibration date \code{calstate_datetime}. Use
#' \code{\link{dbReadTable_integration_detail}} to query the integration table
#' and select the correct id.
#'
#' @inheritParams database_fields
#'
#' @return Data frame of newly added rows.
#' @family custom dbWriteTable functions
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
#'                        calstate_id = 1,
#'                        int_id = 1,
#'                        md_height = 2.,
#'                        pers_id = 1,
#'                        md_comment = "the 2m temperature"))
#' dbDisconnect(con)
#' }
dbAdd_measurand <- function(conn,
                            md_name,
                            md_setup_datetime,
                            pq_name,
                            site_name,
                            dev_name,
                            int_id,
                            md_height = NULL,
                            md_orientation = NULL,
                            md_tilt = NULL,
                            pers_name = NULL,
                            md_comment = NULL) {
  dbWithTransaction_or_Savepoint(conn, {
    pq_id <- get_ids_from_unique_column(conn,
                                        table = "physical_quantity",
                                        id_name = "pq_id",
                                        column_name = "pq_name",
                                        column_values = pq_name)
    site_id <- get_ids_from_unique_column(conn,
                                          table = "site",
                                          id_name = "site_id",
                                          column_name = "site_name",
                                          column_values = site_name)
    # use view to get correct id
    calstate_id <- get_ids_from_datetime_column(conn,
                                              table = "calibration_state_detail",
                                              id_name = "calstate_id",
                                              column_name = "dev_name",
                                              column_values = dev_name,
                                              column_datetime = "calstate_datetime")
    if (!is.null(pers_name)) {
      pers_id <- get_ids_from_unique_column(conn,
                                            table = "person",
                                            id_name = "pers_id",
                                            column_name = "pers_name",
                                            column_values = pers_name)
    } else {
      pers_id <- NULL
    }
    dbWriteTable_measurand(conn,
                           md_name = md_name,
                           md_setup_datetime = md_setup_datetime,
                           pq_id = pq_id,
                           site_id = site_id,
                           calstate_id = calstate_id,
                           int_id = int_id,
                           md_height = md_height,
                           md_orientation = md_orientation,
                           md_tilt = md_tilt,
                           pers_id = pers_id,
                           md_comment = md_comment)
  }, spname = "dbAdd_measurand")

}




#' @rdname dbAdd_measurand
#' @export
dbWriteTable_measurand <- function(conn,
                                   md_name,
                                   md_setup_datetime,
                                   pq_id,
                                   site_id,
                                   calstate_id,
                                   int_id,
                                   md_height = NULL,
                                   md_orientation = NULL,
                                   md_tilt = NULL,
                                   pers_id = NULL,
                                   md_comment = NULL) {
  if (!inherits(md_setup_datetime, "POSIXct")) {
    stop("md_setup_datetime is not POSIXct.")
  }
  write_table(name = "measurand", as.list(environment()))
}





#' Insert data into \code{quality_flag} table
#'
#'
#' @inheritParams database_fields
#'
#' @return Data frame of newly added rows.
#' @family custom dbWriteTable functions
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


#' Add measurements to \code{station_adlershof} table
#'
#' \code{dbWriteTable_station_adlershof} requires a correct measurand id while
#' \code{dbAdd_station_adlershof} derives that from the measurand name.
#'
#' \code{dbWriteTable_station_adlershof} requires a correct measurand id
#' \code{md_id} to write measurements to station_adlershof.
#' \code{dbAdd_station_adlershof} finds \code{md_id} from a given measurand name
#' \code{md_name}. To this end, it queries the table \code{measurand} and
#' selects the according to \code{md_setup_datetime} the most recent
#' \code{md_id}.
#'
#' @inheritParams database_fields
#'
#' @export
#'
#' @return For performance reason, contrary to the meta data table functions,
#'   this function does not return anything.
#' @family custom dbWriteTable functions
#' @examples
#' \dontrun{
#' con <- dbConnect_klimageo()
#' # add all required entries before with the respective dbWriteTable_*
#' dbAdd_station_adlershof(con,
#'                         md_name = "TA2M_1"
#'                         stadl_datetime = as.POSIXct("2017-01-01 12:05:12", tz = "UTC"),
#'                         stadl_value = 272.15)
#' dbWriteTable_station_adlershof(con,
#'                                stadl_datetime = as.POSIXct("2017-01-01 12:15:12", tz = "UTC"),
#'                                md_id = 1,
#'                                stadl_value = 273.15)
#' dbDisconnect(con)
#' }
dbAdd_station_adlershof <- function(conn,
                                    md_name,
                                    stadl_datetime,
                                    stadl_value,
                                    qf_id = NULL) {

  dbWithTransaction_or_Savepoint(conn, {
    md_id <- get_ids_from_datetime_column(conn,
                                          table = "measurand", id_name = "md_id",
                                          column_name = "md_name",
                                          column_values = md_name,
                                          column_datetime = "md_setup_datetime")
    if (any(is.na(md_id))) stop("No md_id for some md_name.")
    # stadl_datetime checked in dbWriteTable_station_adlershof
    dbWriteTable_station_adlershof(conn,
                                   stadl_datetime = stadl_datetime,
                                   md_id = md_id,
                                   stadl_value = stadl_value,
                                   qf_id = qf_id)
  }, spname = "dbAddMeasurement_station_adlershof_savepoint")
}


#' @rdname dbAdd_station_adlershof
#' @export
dbWriteTable_station_adlershof <- function(conn,
                                           stadl_datetime,
                                           md_id,
                                           stadl_value,
                                           qf_id = NULL) {
  if (!inherits(stadl_datetime, "POSIXct")) {
    stop("stadl_datetime is not POSIXct.")
  }
  write_table(name = "station_adlershof", as.list(environment()), return_newrows = FALSE)
}


#' Add measurements to \code{station_patagonia} table
#'
#' \code{dbWriteTable_station_patagonia} requires a correct measurand id while
#' \code{dbAdd_station_patagonia} derives that from the measurand name.
#'
#' \code{dbWriteTable_station_patagonia} requires a correct measurand id
#' \code{md_id} to write measurements to station_patagonia.
#' \code{dbAdd_station_patagonia} finds \code{md_id} from a given measurand name
#' \code{md_name}. To this end, it queries the table \code{measurand} and
#' selects the according to \code{md_setup_datetime} the most recent
#' \code{md_id}.
#'
#' @inheritParams database_fields
#'
#' @export
#'
#' @return For performance reason, contrary to the meta data table functions,
#'   this function does not return anything.
#' @family custom dbWriteTable functions
#' @examples
#' \dontrun{
#' con <- dbConnect_klimageo()
#' # add all required entries before with the respective dbWriteTable_*
#' dbAdd_station_patagonia(con,
#'                         md_name = "TA2M_1"
#'                         stapa_datetime = as.POSIXct("2017-01-01 12:05:12", tz = "UTC"),
#'                         stapa_value = 272.15)
#' dbWriteTable_station_patagonia(con,
#'                                stapa_datetime = as.POSIXct("2017-01-01 12:15:12", tz = "UTC"),
#'                                md_id = 1,
#'                                stapa_value = 273.15)
#' dbDisconnect(con)
#' }
dbAdd_station_patagonia <- function(conn,
                                    md_name,
                                    stapa_datetime,
                                    stapa_value,
                                    qf_id = NULL) {

  dbWithTransaction_or_Savepoint(conn, {
    md_id <- get_ids_from_datetime_column(conn,
                                          table = "measurand", id_name = "md_id",
                                          column_name = "md_name",
                                          column_values = md_name,
                                          column_datetime = "md_setup_datetime")
    if (any(is.na(md_id))) stop("No md_id for some md_name.")
    # stapa_datetime checked in dbWriteTable_station_patagonia
    dbWriteTable_station_patagonia(conn,
                                   stapa_datetime = stapa_datetime,
                                   md_id = md_id,
                                   stapa_value = stapa_value,
                                   qf_id = qf_id)
  }, spname = "dbAddMeasurement_station_patagonia_savepoint")
}


#' @rdname dbAdd_station_patagonia
#' @export
dbWriteTable_station_patagonia <- function(conn,
                                           stapa_datetime,
                                           md_id,
                                           stapa_value,
                                           qf_id = NULL) {
  if (!inherits(stapa_datetime, "POSIXct")) {
    stop("stapa_datetime is not POSIXct.")
  }
  write_table(name = "station_patagonia", as.list(environment()), return_newrows = FALSE)
}




#' Add corrections of \code{station_adlershof} measurements
#'
#' In general, use \code{dbAddCorrection_station_adlershof} to consistently add
#' the corrected measurement values to \code{station_adlershof_correction} and
#' to modify the quality flags in \code{station_adlershof}.
#' \code{dbWriteTable_station_adlershof_correction} and
#' \code{dbUpdate_station_adlershof_qf_id} do the respective single actions.
#'
#' By design, qf_id with values >= 10 in \code{station_adlershof} indicates that
#' entries in \code{station_adlershof} are corrected in
#' \code{station_adlershof_correction}. Thus,
#' \code{dbAddCorrection_station_adlershof} imposes the constraint qf_id >= 10.
#' \code{dbUpdate_station_adlershof_qf_id}, on the other hand, allows general
#' values of qf_id (further checks are done by the database).
#'
#'
#' @inheritParams database_fields
#' @param overwrite Overwrite non-null qf_id values. If \code{FALSE}, only null
#'   qf_id values are modified.
#'
#' @return For performance reason, contrary to the meta data table functions,
#'   these functions do not return anything.
#' @family custom dbWriteTable functions
#' @export
#'
#' @examples
#' \dontrun{
#' con <- dbConnect_klimageo()
#' add all required entries before with the respective dbWriteTable_*
#' dbAddCorrection_station_adlershof(
#'   con,
#'   stadl_id = c(1,2),
#'   qf_id = c(11,11),
#'   stadlcor_datetime =
#'   as.POSIXct(c("2017-01-01 12:15:12", "2017-01-01 12:15:12"), tz = "UTC"),
#'   md_id = c(1,1),
#'   stadlcor_value = c(290.12, 289.23))
#' dbDisconnect(con)
#' }
dbAddCorrection_station_adlershof <- function(conn,
                                              stadl_id, qf_id,
                                              stadlcor_datetime,
                                              md_id,
                                              stadlcor_value) {
  if (!all(qf_id >= 10)) {
    stop("Not all qf_id values are larger or equal 10.")
  }
  dbWithTransaction_or_Savepoint(conn, {
    dbWriteTable_station_adlershof_correction(conn = conn,
                     stadl_id = stadl_id,
                     stadlcor_datetime = stadlcor_datetime,
                     md_id = md_id,
                     stadlcor_value = stadlcor_value)
    dbUpdate_station_adlershof_qf_id(conn, stadl_id, qf_id, overwrite = TRUE)
  }, spname = "dbAddCorrection_station_adlershof_savepoint")
}


#' General routine for Updating qf_id
#'
#' @keywords internal
dbUpdate_qf_id <- function(conn,
                           table_string, id_string, spname,
                           id, qf_id, overwrite = FALSE) {
  if (!overwrite) {
    # get stadl_ids out of argument list for which qf_id is null
    id_qf_id_null <-
      DBI::dbGetQuery(conn,
                      paste("SELECT", id_string, "FROM", table_string, "WHERE",
                            paste0(id_string, " IN (", paste(id, collapse = ", "), ")"),
                            "AND qf_id IS NULL;")
      )[, 1]
    # these entries can be modified, the rest not
    towrite <- id %in% id_qf_id_null
    id_notwritten <- id[!towrite]
    id <- id[towrite]
    qf_id <- qf_id[towrite]
  }
  dbWithTransaction_or_Savepoint(conn, {
    # update necessary qf_id in database
    qf_id_update <-
      DBI::dbSendStatement(conn,
                           statement = paste("UPDATE", table_string,
                           "SET \"qf_id\"=$1 WHERE", id_string, "=$2"),
                           params = list(qf_id, id))
    DBI::dbClearResult(qf_id_update)
    # output message if not all values were written
    if (!overwrite) {
      if (!all(towrite)) {
        message(paste("Did not update entries with", id_string,
                      paste(id_notwritten, collapse = ", "),
                      "because of non-null qf_id."))
      }
    }
  }, spname = spname)
}


#' @rdname dbAddCorrection_station_adlershof
#' @export
dbUpdate_station_adlershof_qf_id <- function(conn, stadl_id, qf_id,
                                             overwrite = TRUE) {
  dbUpdate_qf_id(conn,
                 table_string = "station_adlershof", id_string = "stadl_id",
                 spname = "dbUpdateQF_station_adlershof_savepoint",
                 id = stadl_id, qf_id = qf_id,
                 overwrite = overwrite)
}


#' @rdname dbAddCorrection_station_adlershof
#' @export
dbWriteTable_station_adlershof_correction <- function(conn,
                                                      stadl_id,
                                                      stadlcor_datetime,
                                                      md_id,
                                                      stadlcor_value) {
  if (!inherits(stadlcor_datetime, "POSIXct")) {
    stop("stadlcor_datetime is not POSIXct.")
  }
  write_table(name = "station_adlershof_correction",
              as.list(environment()),
              return_newrows = FALSE)
}


#' Add corrections of \code{station_patagonia} measurements
#'
#' In general, use \code{dbAddCorrection_station_patagonia} to consistently add
#' the corrected measurement values to \code{station_patagonia_correction} and
#' to modify the quality flags in \code{station_patagonia}.
#' \code{dbWriteTable_station_patagonia_correction} and
#' \code{dbUpdate_station_patagonia_qf_id} do the respective single actions.
#'
#' By design, qf_id with values >= 10 in \code{station_patagonia} indicates that
#' entries in \code{station_patagonia} are corrected in
#' \code{station_patagonia_correction}. Thus,
#' \code{dbAddCorrection_station_patagonia} imposes the constraint qf_id >= 10.
#' \code{dbUpdate_station_patagonia_qf_id}, on the other hand, allows general
#' values of qf_id (further checks are done by the database).
#'
#'
#' @inheritParams database_fields
#' @param overwrite Overwrite non-null qf_id values. If \code{FALSE}, only null
#'   qf_id values are modified.
#'
#' @return For performance reason, contrary to the meta data table functions,
#'   these functions do not return anything.
#' @family custom dbWriteTable functions
#' @export
#'
#' @examples
#' \dontrun{
#' con <- dbConnect_klimageo()
#' add all required entries before with the respective dbWriteTable_*
#' dbAddCorrection_station_patagonia(
#'   con,
#'   stapa_id = c(1,2),
#'   qf_id = c(11,11),
#'   stapacor_datetime =
#'   as.POSIXct(c("2017-01-01 12:15:12", "2017-01-01 12:15:12"), tz = "UTC"),
#'   md_id = c(1,1),
#'   stapacor_value = c(290.12, 289.23))
#' dbDisconnect(con)
#' }
dbAddCorrection_station_patagonia <- function(conn,
                                              stapa_id, qf_id,
                                              stapacor_datetime,
                                              md_id,
                                              stapacor_value) {
  if (!all(qf_id >= 10)) {
    stop("Not all qf_id values are larger or equal 10.")
  }
  dbWithTransaction_or_Savepoint(conn, {
    dbWriteTable_station_patagonia_correction(conn = conn,
                                              stapa_id = stapa_id,
                                              stapacor_datetime = stapacor_datetime,
                                              md_id = md_id,
                                              stapacor_value = stapacor_value)
    dbUpdate_station_patagonia_qf_id(conn, stapa_id, qf_id, overwrite = TRUE)
  }, spname = "dbAddCorrection_station_patagonia_savepoint")
}



#' @rdname dbAddCorrection_station_patagonia
#' @export
dbUpdate_station_patagonia_qf_id <- function(conn, stapa_id, qf_id,
                                             overwrite = TRUE) {
  dbUpdate_qf_id(conn,
                 table_string = "station_patagonia", id_string = "stapa_id",
                 spname = "dbUpdateQF_station_patagonia_savepoint",
                 id = stapa_id, qf_id = qf_id,
                 overwrite = overwrite)
}


#' @rdname dbAddCorrection_station_patagonia
#' @export
dbWriteTable_station_patagonia_correction <- function(conn,
                                                      stapa_id,
                                                      stapacor_datetime,
                                                      md_id,
                                                      stapacor_value) {
  if (!inherits(stapacor_datetime, "POSIXct")) {
    stop("stapacor_datetime is not POSIXct.")
  }
  write_table(name = "station_patagonia_correction",
              as.list(environment()),
              return_newrows = FALSE)
}

