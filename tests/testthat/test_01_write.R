context("Database write and some read functions")

con <- dbConnect_klimageo(host = "localhost", port = 5432)
test_that("dbConnect_klimageo", {
  expect_is(con, "DBIConnection")
})


factor2character <- function(df) {
  i_factor <- sapply(df, is.factor)
  df[i_factor] <- lapply(df[i_factor], as.character)
  df
}

test_transaction_completed <- function(conn) {
  expect_true(dbBegin(conn))
  expect_true(dbCommit(conn))
}

test_dbWriteTable_table <- function(conn, table, df, args_ind) {
  # database does not store factors, identical and all.equal gives false when
  # comparing factor and non-factor so convert to character
  df_compare <- factor2character(df)

  # are POSIXct argument in the call, convert them to character for failing test
  test_nonPct <- FALSE
  i_Pct <- sapply(df, function(x) inherits(x, "POSIXct"))
  if (any(i_Pct)) {
    test_nonPct <- TRUE
    df_nonPct <- df
    df_nonPct[i_Pct] <- lapply(df_nonPct[i_Pct], as.character)
  }

  # function to test
  write_string <- paste0("dbWriteTable_", table)
  write_function <- get(write_string)
  read_string <- paste0("dbReadTable_", table)
  read_function <- get(read_string)

  # argument for test function
  write_args <- c(list(conn = conn), as.list(df[args_ind]))
  read_args <- c(list(conn = conn))

  # write test
  test_that(write_string, {
    # call test function
    res <- do.call(write_function, write_args)

    if (table %in% c("station_adlershof", "station_adlershof_correction",
                     "station_patagonia", "station_patagonia_correction")) {
      # get table from database because these functions don't get new data
      # THIS ONLY WORKS WHEN APPLIED TO EMPTY DATABASE!
      df_db <- dbReadTable(conn, table)
    } else {
      df_db <- res
    }

    # compare with expected
    expect_equal(df_compare, df_db)

    if (test_nonPct) {
      # non-POSIXct should fail
      args_nonPct <- c(list(conn = con), as.list(df_nonPct[args_ind]))
      expect_error(do.call(write_function, args_nonPct), "POSIXct")
    }

    # transaction should be finished
    test_transaction_completed(conn)
  })

  # read test
  test_that(read_string, {
    # get table from database because these functions don't get new data
    # THIS ONLY WORKS WHEN APPLIED TO EMPTY DATABASE!
    df_db <- do.call(read_function, read_args)
    # compare with expected
    expect_equal(df_compare, df_db)
  })
}


# check transaction function
test_that("dbWithTransaction_or_Savepoint", {
  # correct result
  x <- dbWithTransaction_or_Savepoint(con, 1, "sptest1")
  expect_identical(x, 1)

  # correct result
  x <- dbWithTransaction_or_Savepoint(con, 2, "sptest2")
  expect_identical(x, 2)

  # works also within transaction
  expect_true(dbBegin(con))
  x <- dbWithTransaction_or_Savepoint(con, 3, "sptest3")
  expect_identical(x, 3)
  x <- dbWithTransaction_or_Savepoint(con, 4, "sptest4")
  expect_identical(x, 4)
  expect_true(dbCommit(con))

  # error message is forwarded and really rolled back
  expect_error(
    dbWithTransaction_or_Savepoint(con, {
      dbWriteTable(con, "testtable", data.frame(me = 1, you = 2))
      stop("CUSTOM ERROR MESSAGE")
    }, "sptest5"),
    "CUSTOM ERROR MESSAGE")
  expect_error(dbReadTable(con, "testtable"), 'relation "testtable" does not exist')
  expect_true(dbBegin(con))
  expect_error(
    dbWithTransaction_or_Savepoint(con, {
      dbWriteTable(con, "testtable", data.frame(me = 1, you = 2))
      stop("CUSTOM ERROR MESSAGE")
    }, "sptest6"),
    "CUSTOM ERROR MESSAGE")
  expect_error(dbReadTable(con, "testtable"), 'relation "testtable" does not exist')
  expect_true(dbCommit(con))
})


site_df <- data.frame(
  site_id = 1,
  site_name = "Adlershof",
  site_lat = 53.,
  site_lon = 14.,
  site_altitude = 30.2,
  site_comment = "im Garten")

test_dbWriteTable_table(con, "site", site_df, 2:6)



device_manufacturer_df <- data.frame(
  devman_id = c(1, 2),
  devman_name = c("TSI", "Grimm"),
  devman_comment = as.character(c(NA, NA)))

test_dbWriteTable_table(con, "device_manufacturer", device_manufacturer_df, 2)



device_type_df <- data.frame(
  devtype_id = c(1, 2),
  devtype_name = c("thermometer", "barometer"),
  devtype_comment = c("measures temperature", "measures pressure"))

test_dbWriteTable_table(con, "device_type", device_type_df, 2:3)



device_model_df <- data.frame(
  devmod_id = 1,
  devmod_name = "THERMO1000",
  devtype_id = 1,
  devman_id = 1,
  devmod_comment = "big yellow box")

test_dbWriteTable_table(con, "device_model", device_model_df, 2:5)


device_model_df_new <- data.frame(
  devmod_id = c(2, 3),
  devmod_name = c("THERMO2000", "Press0r"),
  devtype_id = c(1, 2),
  devman_id = c(2, 1),
  devmod_comment = c("big", "small")
)

device_model_df <- factor2character(rbind(device_model_df, device_model_df_new))

test_that("Add_device_model", {
  res <- dbAdd_device_model(
    con,
    devmod_name = device_model_df_new$devmod_name,
    devtype_name = device_type_df$devtype_name[device_model_df_new$devtype_id],
    devman_name = device_manufacturer_df$devman_name[device_model_df_new$devman_id],
    devmod_comment = device_model_df_new$devmod_comment
    )
  expect_equal(res, factor2character(device_model_df_new))
  res <- dbReadTable_device_model(con)
  expect_equal(res, device_model_df)
})


device_df <- data.frame(
  dev_id = 1,
  dev_name = "My first THERMO1000",
  devmod_id = 1,
  dev_identifier = "NCC1701-T",
  dev_comment = "it beeps")

test_dbWriteTable_table(con, "device", device_df, 2:5)



device_df_new <- data.frame(
  dev_id = c(2,3),
  dev_name = c("My first Press0r", "My second THERMO1000"),
  devmod_id = c(3, 1),
  dev_identifier = c("NCC1701-P", "NCC1702-T"),
  dev_comment = as.character(c(NA, NA))
)

device_df <- factor2character(rbind(device_df, device_df_new))

test_that("Add_device", {
  res <- dbAdd_device(
    con,
    dev_name = device_df_new$dev_name,
    devmod_name = device_model_df$devmod_name[device_df_new$devmod_id],
    dev_identifier = device_df_new$dev_identifier,
    dev_comment = device_df_new$dev_comment
  )
  expect_equal(res, factor2character(device_df_new))
  res <- dbReadTable_device(con)
  expect_equal(res, device_df)
})





calibration_state_df <- data.frame(
  calstate_id = 1,
  dev_id = 1,
  calstate_datetime = as.POSIXct("2012-01-01 12:15:12", tz = "UTC"),
  calstate_parameter = "a=10, b=99.12",
  calstate_comment = as.character(NA))

test_dbWriteTable_table(con, "calibration_state", calibration_state_df, 2:4)



# add new entries to device and calibration_state. CHECK IF NUMBER OF ROWS
# MODIFIES ABOVE!
device_uncalibrated_df <- data.frame(
  dev_id = c(4,5),
  dev_name = c("My other THERMO1000", "My third THERMO1000"),
  devmod_id = c(1, 1),
  dev_identifier = c("NCC1701-T1", "NCC1701-T2"),
  dev_comment = as.character(c(NA, NA)))
device_df_new <- factor2character(device_uncalibrated_df)
# columns added by database and row from device above
device_df <- rbind(factor2character(device_df),
                   device_df_new)

calibration_state_df_new <- data.frame(
  calstate_id = as.integer(c(2, 3)),
  dev_id = device_df$dev_id[length(device_df$dev_id)-c(1,0)],
  calstate_datetime = as.POSIXct(c(NA, NA)),
  calstate_parameter = as.character(c(NA, NA)),
  calstate_comment = as.character(c(NA, NA)),
  stringsAsFactors = FALSE)

calibration_state_df <-
  rbind(factor2character(calibration_state_df),
        calibration_state_df_new
  )

test_that("dbAdd_device_uncalibrated", {
  res <- dbAdd_device_uncalibrated(con,
                                   dev_name = device_uncalibrated_df$dev_name,
                                   devmod_name = device_model_df$devmod_name[device_uncalibrated_df$devmod_id],
                                   dev_identifier = device_uncalibrated_df$dev_identifier)
  # test the new rows output
  expect_equal(device_df_new, res$device)
  expect_equal(calibration_state_df_new, res$calibration_state)
  # test total tables
  df <- dbReadTable(con, "device")
  expect_equal(device_df, df)
  dfcd <- dbReadTable(con, "calibration_state")
  expect_equal(calibration_state_df, dfcd)
  test_transaction_completed(con)
})



calibration_state_df_new <- data.frame(
  calstate_id = c(4, 5),
  dev_id = c(2, 1),
  calstate_datetime = as.POSIXct(c("2013-01-01 12:15:12", "2013-06-01 12:15:12")),
  calstate_parameter = as.character(c(NA, NA)),
  calstate_comment = as.character(c(NA, NA)),
  stringsAsFactors = FALSE)

calibration_state_df <- rbind(calibration_state_df, calibration_state_df_new)

test_that("dbAdd_calibration_state", {
  res <- dbAdd_calibration_state(con,
                                 dev_name = device_df$dev_name[calibration_state_df_new$dev_id],
                                 calstate_datetime = calibration_state_df_new$calstate_datetime,
                                 calstate_parameter = calibration_state_df_new$calstate_parameter,
                                 calstate_comment = calibration_state_df_new$calstate_comment)
  expect_equal(calibration_state_df_new, res)
  df <- dbReadTable(con, "calibration_state")
  expect_equal(df, calibration_state_df)
  test_transaction_completed(con)
})


physical_quantity_df <- data.frame(
  pq_id = 1,
  pq_name = "air_temperature",
  pq_unit = "K",
  pq_description = "Air temperature is the bulk temperature of the air, not the surface (skin) temperature.",
  pq_comment = "says if air is cold or warm")

test_dbWriteTable_table(con, "physical_quantity", physical_quantity_df, 2:5)



physical_quantity_df_new <- data.frame(
  pq_id = c(2, 3),
  pq_name = c("diffuse_downwelling_shortwave_flux_in_air", "air_pressure"),
  pq_unit = c("W m-2", "Pa"),
  pq_description = c("Downwelling radiation is radiation from above. It does not mean \"net downward\".  When thought of as being incident on a surface, a radiative flux is sometimes called \"irradiance\".  In addition, it is identical with the quantity measured by a cosine-collector light-meter and sometimes called \"vector irradiance\".  In accordance with common usage in geophysical disciplines, \"flux\" implies per unit area, called \"flux density\" in physics.  \"shortwave\" means shortwave radiation. \"Diffuse\" radiation is radiation that has been scattered by particles in the atmosphere such as cloud droplets and aerosols.",
                     "Air pressure is the force per unit area which would be exerted when the moving gas molecules of which the air is composed strike a theoretical surface of any orientation."),
  pq_comment = as.character(c(NA, NA)),
  stringsAsFactors = FALSE
)
physical_quantity_df <- rbind(factor2character(physical_quantity_df),
                              physical_quantity_df_new)

test_that("dbAdd_physical_quantity", {
 res <- dbAdd_physical_quantity(con,
                                pq_name = physical_quantity_df_new$pq_name)
 expect_equal(res, physical_quantity_df_new)
 df <- dbReadTable_physical_quantity(con)
 expect_equal(df, physical_quantity_df)
})



integration_type_df <- data.frame(
  inttype_id = c(1, 2),
  inttype_name = c("single", "average"),
  inttype_description = c("single value at a specific time", "average value over a period"),
  inttype_comment = as.character(c(NA,NA)))

test_dbWriteTable_table(con, "integration_type", integration_type_df, 2:3)



integration_df <- data.frame(
  int_id = 1,
  inttype_id = 2,
  int_measurement_interval = 60,
  int_interval = 600,
  int_comment = as.character(NA))

test_dbWriteTable_table(con, "integration", integration_df, 2:4)



integration_df_new <- data.frame(
  int_id = 2,
  inttype_id = 1,
  int_measurement_interval = 100,
  int_interval = 100,
  int_comment = as.character(NA),
  stringsAsFactors = FALSE
)
integration_df <- rbind(factor2character(integration_df), integration_df_new)

test_that("dbAdd_integration", {
  res <- dbAdd_integration(con,
                           inttype_name = integration_type_df$inttype_name[integration_df_new$inttype_id],
                           int_measurement_interval = integration_df_new$int_measurement_interval,
                           int_interval = integration_df_new$int_interval
  )
  expect_equal(integration_df_new, res)
  df <- dbReadTable(con, "integration")
  expect_equal(df, integration_df)
  test_transaction_completed(con)
})




person_df <- data.frame(
  pers_id = 1,
  pers_name = "Karl Heinz",
  pers_comment = as.character(NA))

test_dbWriteTable_table(con, "person", person_df, 2)



measurand_df <- data.frame(
  md_id = c(1, 2, 3),
  md_name = c("TA2M_1", "TA2M_1", "TA2M_2"),
  md_setup_datetime = as.POSIXct(
    c("2012-01-01 12:15:12", "2013-01-01 12:15:12", "2014-01-01 12:15:12"),
    tz = "UTC"),
  pq_id = c(1, 1, 1),
  site_id = c(1, 1, 1),
  calstate_id = c(1, 2, 2),
  int_id = c(1, 1, 1),
  md_height = c(2., 2., 2.),
  md_orientation = as.numeric(c(NA, NA, NA)),
  md_tilt = as.numeric(c(NA, NA, NA)),
  pers_id = c(1, 1, 1),
  md_comment = c("the 2m temperature", "the 2m temperature", "the 2m temperature"))

test_dbWriteTable_table(con, "measurand", measurand_df, 2:12)


measurand_df_new <- data.frame(
  md_id = c(4, 5),
  md_name = c("P2M_1", "RAD"),
  md_setup_datetime = as.POSIXct(
    c("2013-01-01 12:15:12", "2014-01-01 12:15:12"),
    tz = "UTC"),
  pq_id = c(3, 2),
  site_id = c(1, 1),
  calstate_id = c(4, 5),
  int_id = c(1, 1),
  md_height = c(2, 1),
  md_orientation = as.numeric(c(NA, NA)),
  md_tilt = as.numeric(c(NA, NA)),
  pers_id = c(1, 1),
  md_comment = as.character(c(NA, NA))
)

measurand_df <- rbind(measurand_df, factor2character(measurand_df_new))

test_that("dbAdd_measurand", {
  calstate_detail <- dbReadTable_calibration_state_detail(con)
  # sort by calstate_id
  calstate_detail <- calstate_detail[order(calstate_detail$calstate_id), ]
  res <- dbAdd_measurand(con,
                         md_name = measurand_df_new$md_name,
                         md_setup_datetime = measurand_df_new$md_setup_datetime,
                         pq_name = physical_quantity_df$pq_name[measurand_df_new$pq_id],
                         site_name = site_df$site_name[measurand_df_new$site_id],
                         dev_name = calstate_detail$dev_name[measurand_df_new$calstate_id],
                         int_id = measurand_df_new$int_id,
                         md_height = measurand_df_new$md_height,
                         pers_name = person_df$pers_name[measurand_df_new$pers_id])

  expect_equal(res, factor2character(measurand_df_new))
  df <- dbReadTable_measurand(con)
  expect_equal(df, factor2character(measurand_df))
  test_transaction_completed(con)
})



quality_flag_df <- data.frame(
  qf_id = c(1,11),
  qf_name = c("value ok, automatic qc", "corrected calculation constant"),
  qf_description = c("value ok, automatically checked for consistency",
                     "original value used from calculation constant"),
  qf_comment = as.character(c(NA, NA)))

test_dbWriteTable_table(con, "quality_flag", quality_flag_df, 1:3)



station_adlershof_df <- data.frame(
  stadl_id = c(1, 2, 3),
  stadl_datetime = c(as.POSIXct("2017-01-01 12:15:12", tz = "UTC"),
                     as.POSIXct("2017-01-01 16:15:12", tz = "CET"),
                     as.POSIXct("2017-01-05 16:15:12", tz = "GMT")),
  md_id = c(1, 1, 1),
  stadl_value = c(293.15, 294.15, 270.15),
  qf_id = as.integer(NA, NA, NA))

test_dbWriteTable_table(con, "station_adlershof", station_adlershof_df, 2:4)

md_name <- c("TA2M_1", "TA2M_1", "TA2M_2")
station_adlershof_df_new <- data.frame(
  stadl_id = c(4, 5, 6),
  stadl_datetime = c(as.POSIXct("2017-02-01 12:15:12", tz = "UTC"),
                     as.POSIXct("2017-02-01 16:15:12", tz = "CET"),
                     as.POSIXct("2017-02-05 16:15:12", tz = "GMT")),
  md_id = c(2, 2, 3),
  stadl_value = c(293.15, 294.15, 270.15),
  qf_id = as.integer(NA, NA, NA))

station_adlershof_df <- rbind(station_adlershof_df, station_adlershof_df_new)

test_that("dbAdd_station_adlershof", {
  dbAdd_station_adlershof(
    con,
    md_name = md_name,
    stadl_datetime = station_adlershof_df_new$stadl_datetime,
    stadl_value = station_adlershof_df_new$stadl_value)
  df <- dbReadTable_station_adlershof(con)
  expect_equal(df, station_adlershof_df)
  test_transaction_completed(con)
})



cor_stadl_id <- 1
station_adlershof_df$qf_id[cor_stadl_id] <- 1
test_that("dbUpdate_station_adlershof_qf_id", {
  dbUpdate_station_adlershof_qf_id(con,
                               stadl_id = cor_stadl_id,
                               qf_id = station_adlershof_df$qf_id[cor_stadl_id])
  df <- dbReadTable(con, "station_adlershof")
  # reorder df because UPDATE changes original order
  df <- df[order(df$stadl_id), ]
  rownames(df) <- 1:nrow(df)
  expect_equal(station_adlershof_df, df)
  test_transaction_completed(con)
})


cor_stadl_id <- c(2, 3)
station_adlershof_correction_df <-
  data.frame(stadlcor_id = c(1, 2),
             stadl_id = cor_stadl_id,
             stadlcor_datetime = as.POSIXct(c("2017-05-01 16:15:12", "2017-05-05 16:15:12"), tz = "UTC"),
             md_id = c(1,2),
             stadlcor_value = c(290.15, 278.15)
  )
station_adlershof_df$qf_id[cor_stadl_id] <- 11

station_adlershof_corrected_df <- station_adlershof_df
station_adlershof_corrected_df$stadl_datetime[cor_stadl_id] <- station_adlershof_correction_df$stadlcor_datetime
station_adlershof_corrected_df$md_id[cor_stadl_id]          <- station_adlershof_correction_df$md_id
station_adlershof_corrected_df$stadl_value[cor_stadl_id]    <- station_adlershof_correction_df$stadlcor_value

test_that("dbAddCorrection_station_adlershof", {
  dbAddCorrection_station_adlershof(con,
                                    stadl_id = station_adlershof_correction_df$stadl_id,
                                    qf_id = station_adlershof_df$qf_id[cor_stadl_id],
                                    stadlcor_datetime = station_adlershof_correction_df$stadlcor_datetime,
                                    md_id = station_adlershof_correction_df$md_id,
                                    stadlcor_value = station_adlershof_correction_df$stadlcor_value)
  df <- dbReadTable(con, "station_adlershof_correction")
  expect_equal(station_adlershof_correction_df, df)
  # no POSIXct should give error
  expect_error(
    dbAddCorrection_station_adlershof(con,
                                      stadl_id = station_adlershof_correction_df$stadl_id,
                                      qf_id = station_adlershof_df$qf_id[cor_stadl_id],
                                      stadlcor_datetime = as.character(station_adlershof_correction_df$stadlcor_datetime),
                                      md_id = station_adlershof_correction_df$md_id,
                                      stadlcor_value = station_adlershof_correction_df$stadlcor_value),
    "POSIXct"
  )
  test_transaction_completed(con)
})

test_that("dbReadTable_station_adlershof_*", {
  # reading of only correction data
  df <- dbReadTable_station_adlershof_correction(con)
  expect_equal(station_adlershof_correction_df, df)

  # reading of total corrected data (also check of correct VIEW definition)
  df <- dbReadTable_station_adlershof_corrected(con)
  # reorder df because UPDATE changes original order
  df <- df[order(df$stadl_id), ]
  rownames(df) <- 1:nrow(df)
  expect_equal(station_adlershof_corrected_df, df)

  test_transaction_completed(con)
})



station_patagonia_df <- data.frame(
  stapa_id = c(1, 2, 3),
  stapa_datetime = c(as.POSIXct("2017-01-01 12:15:12", tz = "UTC"),
                     as.POSIXct("2017-01-01 16:15:12", tz = "CET"),
                     as.POSIXct("2017-01-05 16:15:12", tz = "GMT")),
  md_id = c(1, 1, 1),
  stapa_value = c(293.15, 294.15, 270.15),
  qf_id = as.integer(NA, NA, NA))

test_dbWriteTable_table(con, "station_patagonia", station_patagonia_df, 2:4)

md_name <- c("TA2M_1", "TA2M_1", "TA2M_2")
station_patagonia_df_new <- data.frame(
  stapa_id = c(4, 5, 6),
  stapa_datetime = c(as.POSIXct("2017-02-01 12:15:12", tz = "UTC"),
                     as.POSIXct("2017-02-01 16:15:12", tz = "CET"),
                     as.POSIXct("2017-02-05 16:15:12", tz = "GMT")),
  md_id = c(2, 2, 3),
  stapa_value = c(293.15, 294.15, 270.15),
  qf_id = as.integer(NA, NA, NA))

station_patagonia_df <- rbind(station_patagonia_df, station_patagonia_df_new)

test_that("dbAdd_station_patagonia", {
  dbAdd_station_patagonia(
    con,
    md_name = md_name,
    stapa_datetime = station_patagonia_df_new$stapa_datetime,
    stapa_value = station_patagonia_df_new$stapa_value)
  df <- dbReadTable_station_patagonia(con)
  expect_equal(df, station_patagonia_df)
  test_transaction_completed(con)
})



cor_stapa_id <- 1
station_patagonia_df$qf_id[cor_stapa_id] <- 1
test_that("dbUpdate_station_patagonia_qf_id", {
  dbUpdate_station_patagonia_qf_id(con,
                                   stapa_id = cor_stapa_id,
                                   qf_id = station_patagonia_df$qf_id[cor_stapa_id])
  df <- dbReadTable(con, "station_patagonia")
  # reorder df because UPDATE changes original order
  df <- df[order(df$stapa_id), ]
  rownames(df) <- 1:nrow(df)
  expect_equal(station_patagonia_df, df)
  test_transaction_completed(con)
})


cor_stapa_id <- c(2, 3)
station_patagonia_correction_df <-
  data.frame(stapacor_id = c(1, 2),
             stapa_id = cor_stapa_id,
             stapacor_datetime = as.POSIXct(c("2017-05-01 16:15:12", "2017-05-05 16:15:12"), tz = "UTC"),
             md_id = c(1,2),
             stapacor_value = c(290.15, 278.15)
  )
station_patagonia_df$qf_id[cor_stapa_id] <- 11

station_patagonia_corrected_df <- station_patagonia_df
station_patagonia_corrected_df$stapa_datetime[cor_stapa_id] <- station_patagonia_correction_df$stapacor_datetime
station_patagonia_corrected_df$md_id[cor_stapa_id]          <- station_patagonia_correction_df$md_id
station_patagonia_corrected_df$stapa_value[cor_stapa_id]    <- station_patagonia_correction_df$stapacor_value

test_that("dbAddCorrection_station_patagonia", {
  dbAddCorrection_station_patagonia(con,
                                    stapa_id = station_patagonia_correction_df$stapa_id,
                                    qf_id = station_patagonia_df$qf_id[cor_stapa_id],
                                    stapacor_datetime = station_patagonia_correction_df$stapacor_datetime,
                                    md_id = station_patagonia_correction_df$md_id,
                                    stapacor_value = station_patagonia_correction_df$stapacor_value)
  df <- dbReadTable(con, "station_patagonia_correction")
  expect_equal(station_patagonia_correction_df, df)
  # no POSIXct should give error
  expect_error(
    dbAddCorrection_station_patagonia(con,
                                      stapa_id = station_patagonia_correction_df$stapa_id,
                                      qf_id = station_patagonia_df$qf_id[cor_stapa_id],
                                      stapacor_datetime = as.character(station_patagonia_correction_df$stapacor_datetime),
                                      md_id = station_patagonia_correction_df$md_id,
                                      stapacor_value = station_patagonia_correction_df$stapacor_value),
    "POSIXct"
  )
  test_transaction_completed(con)
})

test_that("dbReadTable_station_patagonia_*", {
  # reading of only correction data
  df <- dbReadTable_station_patagonia_correction(con)
  expect_equal(station_patagonia_correction_df, df)

  # reading of total corrected data (also check of correct VIEW definition)
  df <- dbReadTable_station_patagonia_corrected(con)
  # reorder df because UPDATE changes original order
  df <- df[order(df$stapa_id), ]
  rownames(df) <- 1:nrow(df)
  expect_equal(station_patagonia_corrected_df, df)

  test_transaction_completed(con)
})








# all transaction should have been either committed or rollbacked
test_that("transaction complete", {
  test_transaction_completed(con)
})

dbDisconnect(con)
