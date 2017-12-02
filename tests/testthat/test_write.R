context("Database write functions")

con <- dbConnect_klimageo()
test_that("dbConnect_klimageo", {
  expect_is(con, "DBIConnection")
})


factor2character <- function(df) {
  i_factor <- sapply(df, is.factor)
  df[i_factor] <- lapply(df[i_factor], as.character)
  df
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
  test_string <- paste0("dbWriteTable_", table)
  test_function <- get(test_string)

  # argument for test function
  args <- c(list(conn = con), as.list(df[args_ind]))

  test_that(test_string, {
    # call test function
    res <- do.call(test_function, args)

    if (table %in% c("station_adlershof", "station_adlershof_correction")) {
      # get table from database because these functions don't get new data
      # THIS ONLY WORKS WHEN APPLIED TO EMPTY DATABASE!
      df_db <- dbReadTable(conn, table)
    } else {
      df_db <- res
    }

    # compare with expected
    expect_true(all.equal(df_compare, df_db))

    if (test_nonPct) {
      # non-POSIXct should fail
      args_nonPct <- c(list(conn = con), as.list(df_nonPct[args_ind]))
      expect_error(do.call(test_function, args_nonPct), "POSIXct")
    }
  })
}

site_df <- data.frame(
  site_id = 1,
  site_name = "Adlershof",
  site_lat = 53.,
  site_lon = 14.,
  site_altitude = 30.2,
  site_comment = "im Garten")

test_dbWriteTable_table(con, "site", site_df, 2:6)



device_manufacturer_df <- data.frame(
  devman_id = 1,
  devman_name = "TSI",
  devman_comment = as.character(NA))

test_dbWriteTable_table(con, "device_manufacturer", device_manufacturer_df, 2)



device_type_df <- data.frame(
  devtype_id = 1,
  devtype_name = "thermometer",
  devtype_comment = "measures temperature")

test_dbWriteTable_table(con, "device_type", device_type_df, 2:3)



device_model_df <- data.frame(
  devmod_id = 1,
  devmod_name = "THERMO1000",
  devtype_id = 1,
  devman_id = 1,
  devmod_comment = "big yellow box")

test_dbWriteTable_table(con, "device_model", device_model_df, 2:5)



device_df <- data.frame(
  dev_id = 1,
  dev_name = "My first THERMO1000",
  devmod_id = 1,
  dev_identifier = "NCC1701-T",
  dev_comment = "it beeps")

test_dbWriteTable_table(con, "device", device_df, 2:5)



calibrated_device_df <- data.frame(
  caldev_id = 1,
  dev_id = 1,
  caldev_datetime = as.POSIXct("2012-01-01 12:15:12", tz = "UTC"),
  caldev_parameter = "a=10, b=99.12",
  caldev_comment = as.character(NA))

test_dbWriteTable_table(con, "calibrated_device", calibrated_device_df, 2:4)



# add new entries to device and calibrated_device. CHECK IF NUMBER OF ROWS
# MODIFIES ABOVE!
uncalibrated_device_df <- data.frame(
  dev_id = c(2,3),
  dev_name = c("My second THERMO1000", "My third THERMO1000"),
  devmod_id = c(1, 1),
  dev_identifier = c("NCC1701-T1", "NCC1701-T2"),
  dev_comment = as.character(c(NA, NA)))
device_df_new_compare <- factor2character(uncalibrated_device_df)
# columns added by database and row from device above
device_df_compare2 <- rbind(factor2character(device_df),
                            device_df_new_compare)

calibrated_device_df_new_compare <- data.frame(
  caldev_id = as.integer(c(2, 3)),
  dev_id = device_df_compare2$dev_id[-1],
  caldev_datetime = as.POSIXct(c(NA, NA)),
  caldev_parameter = as.character(c(NA, NA)),
  caldev_comment = as.character(c(NA, NA)),
  stringsAsFactors = FALSE)

calibrated_device_df_compare2 <-
  rbind(factor2character(calibrated_device_df),
        calibrated_device_df_new_compare
  )

test_that("dbAdd_uncalibrated_device", {
  res <- dbAdd_uncalibrated_device(con,
                                   dev_name = uncalibrated_device_df$dev_name,
                                   devmod_id = uncalibrated_device_df$devmod_id,
                                   dev_identifier = uncalibrated_device_df$dev_identifier)
  # test the new rows output
  expect_true(all.equal(device_df_new_compare, res$device))
  expect_true(all.equal(calibrated_device_df_new_compare, res$calibrated_device))
  # test total tables
  df <- dbReadTable(con, "device")
  expect_true(all.equal(device_df_compare2, df))
  dfcd <- dbReadTable(con, "calibrated_device")
  expect_true(all.equal(calibrated_device_df_compare2, dfcd))
})



physical_quantity_df <- data.frame(
  pq_id = 1,
  pq_name = "air temperature",
  pq_unit = "degC",
  pq_comment = "says if air is cold or warm")

test_dbWriteTable_table(con, "physical_quantity", physical_quantity_df, 2:4)



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



person_df <- data.frame(
  pers_id = 1,
  pers_name = "Karl Heinz",
  pers_comment = as.character(NA))

test_dbWriteTable_table(con, "person", person_df, 2)



measurand_df <- data.frame(
  md_id = 1,
  md_name = "TA2M_1",
  md_setup_datetime = as.POSIXct("2012-01-01 12:15:12", tz = "UTC"),
  pq_id = 1,
  site_id = 1,
  caldev_id = 1,
  int_id = 1,
  md_height = 2.,
  pers_id = 1,
  md_comment = "the 2m temperature")

test_dbWriteTable_table(con, "measurand", measurand_df, 2:10)



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



cor_stadl_id <- 1
station_adlershof_df$qf_id[cor_stadl_id] <- 1
test_that("dbUpdateQF_station_adlershof", {
  dbUpdateQF_station_adlershof(con,
                               stadl_id = cor_stadl_id,
                               qf_id = station_adlershof_df$qf_id[cor_stadl_id])
  df <- dbReadTable(con, "station_adlershof")
  # reorder df because UPDATE changes original order
  df <- df[order(df$stadl_id), ]
  rownames(df) <- 1:3
  expect_true(all.equal(station_adlershof_df, df))
})



cor_stadl_id <- c(2, 3)
station_adlershof_correction_df <-
  data.frame(stadlcor_id = c(1, 2),
             stadl_id = cor_stadl_id,
             stadlcor_datetime = as.POSIXct(c("2017-05-01 16:15:12", "2017-05-05 16:15:12"), tz = "UTC"),
             md_id = c(1,1),
             stadlcor_value = c(290.15, 278.15)
  )
station_adlershof_df$qf_id[cor_stadl_id] <- 11

test_that("dbAddCorrection_station_adlershof", {
  dbAddCorrection_station_adlershof(con,
                                    stadl_id = station_adlershof_correction_df$stadl_id,
                                    qf_id = station_adlershof_df$qf_id[cor_stadl_id],
                                    stadlcor_datetime = station_adlershof_correction_df$stadlcor_datetime,
                                    md_id = station_adlershof_correction_df$md_id,
                                    stadlcor_value = station_adlershof_correction_df$stadlcor_value)
  df <- dbReadTable(con, "station_adlershof_correction")
  expect_true(all.equal(station_adlershof_correction_df, df))
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
})


dbDisconnect(con)
