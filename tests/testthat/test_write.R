context("Database write functions")

con <- dbConnect_klimageo()
test_that("dbConnect_klimageo", {
  expect_is(con, "DBIConnection")
})


# database does not store factors, identical and all.equal gives false when
# comparing factor and non-factor
site_df <- data.frame(site_name = "Adlershof",
                      site_lat = 53.,
                      site_lon = 14.,
                      site_altitude = 30.2,
                      site_comment = "im Garten")
# add columns that will be added by the database and convert to non-factor
site_df_full <- cbind(data.frame(site_id = as.integer(1), site_df))
site_df_full$site_name <- as.character(site_df_full$site_name)
site_df_full$site_comment <- as.character(site_df_full$site_comment)

test_that("dbWriteTable_site", {
  # writing factor because people will do that
  dbWriteTable_site(con,
                    site_name = site_df$site_name,
                    site_lon  = site_df$site_lon,
                    site_lat  = site_df$site_lat,
                    site_altitude = site_df$site_altitude,
                    site_comment = site_df$site_comment)

  df <- dbReadTable(con, "site")
  expect_true(identical(site_df_full, df))
})


device_manufacturer_df <- data.frame(devman_name = "TSI")
# add columns that will be added by the database and convert to non-factor
device_manufacturer_df_full <-
  cbind(data.frame(devman_id = as.integer(1)),
        device_manufacturer_df,
        data.frame(devman_comment = as.character(NA), stringsAsFactors = FALSE))
device_manufacturer_df_full$devman_name <-
  as.character(device_manufacturer_df_full$devman_name)

test_that("dbWriteTable_device_manufacturer", {
  dbWriteTable_device_manufacturer(con,
                                   devman_name = device_manufacturer_df$devman_name)
  df <- dbReadTable(con, "device_manufacturer")
  # comparison with non-factor
  expect_true(identical(device_manufacturer_df_full, df))
})



device_type_df <- data.frame(devtype_name = "thermometer",
                             devtype_comment = "measures temperature")
# add columns that will be added by the database and convert to non-factor
device_type_df_full <- cbind(data.frame(devtype_id = as.integer(1)),
                             device_type_df)
device_type_df_full$devtype_name <- as.character(device_type_df_full$devtype_name)
device_type_df_full$devtype_comment <- as.character(device_type_df_full$devtype_comment)

test_that("dbWriteTable_device_type", {
  dbWriteTable_device_type(con,
                           devtype_name = device_type_df$devtype_name,
                           devtype_comment = device_type_df$devtype_comment)
  df <- dbReadTable(con, "device_type")
  expect_true(identical(device_type_df_full, df))
})



device_model_df <- data.frame(devmod_name = "THERMO1000",
                              devtype_id = 1,
                              devman_id = 1,
                              devmod_comment = "big yellow box")
device_model_df_full <- cbind(data.frame(devmod_id = as.integer(1)),
                              device_model_df)
device_model_df_full$devmod_name <- as.character(device_model_df_full$devmod_name)
device_model_df_full$devmod_comment <- as.character(device_model_df_full$devmod_comment)

test_that("dbWriteTable_device_model", {
  dbWriteTable_device_model(con,
                            devmod_name = device_model_df$devmod_name,
                            devtype_id = device_model_df$devtype_id,
                            devman_id = device_model_df$devman_id,
                            devmod_comment = device_model_df$devmod_comment)
  df <- dbReadTable(con, "device_model")
  # use all.equal because all.equal(1., as.integer(1)) but not identical(1.,
  # as.integer(1))
  expect_true(all.equal(device_model_df_full, df))
})



device_df <- data.frame(dev_name = "My first THERMO1000",
                        devmod_id = 1,
                        dev_identifier = "NCC1701-T",
                        dev_comment = "it beeps")
device_df_full <- cbind(data.frame(dev_id = as.integer(1)),
                        device_df)
device_df_full$dev_name <- as.character(device_df_full$dev_name)
device_df_full$dev_identifier <- as.character(device_df_full$dev_identifier)
device_df_full$dev_comment <- as.character(device_df_full$dev_comment)

test_that("dbWriteTable_device", {
  dbWriteTable_device(con,
                      dev_name = device_df$dev_name,
                      devmod_id = device_df$devmod_id,
                      dev_identifier = device_df$dev_identifier,
                      dev_comment = device_df$dev_comment)
  df <- dbReadTable(con, "device")
  expect_true(all.equal(device_df_full, df))
})


# non-POSIXct datetime should give error
calibrated_device_df <- data.frame(dev_id = 1,
                                   caldev_datetime = "2012-01-01 12:15:12",
                                   caldev_parameter = "a=10, b=99.12")
# POSIXct datetime should work
calibrated_device_dfPct <- calibrated_device_df
calibrated_device_dfPct$caldev_datetime <-
  as.POSIXct(calibrated_device_dfPct$caldev_datetime, tz = "UTC")
# for comparison
calibrated_device_dfPct_full <-
  cbind(data.frame(caldev_id = as.integer(1)),
        calibrated_device_dfPct,
        data.frame(caldev_comment = as.character(NA), stringsAsFactors = FALSE))
calibrated_device_dfPct_full$caldev_parameter <-
  as.character(calibrated_device_dfPct_full$caldev_parameter)

test_that("dbWriteTable_calibrated_device", {
  dbWriteTable_calibrated_device(con,
                                 dev_id = calibrated_device_dfPct$dev_id,
                                 caldev_datetime = calibrated_device_dfPct$caldev_datetime,
                                 caldev_parameter = calibrated_device_dfPct$caldev_parameter)
  df <- dbReadTable(con, "calibrated_device")
  expect_true(all.equal(calibrated_device_dfPct_full, df))
  expect_error(
    dbWriteTable_calibrated_device(con,
                                   dev_id = calibrated_device_df$dev_id,
                                   caldev_datetime = calibrated_device_df$caldev_datetime,
                                   caldev_parameter = calibrated_device_df$caldev_parameter)

  )
})

# add new entries to device and calibrated_device. CHECK IF NUMBER OF ROWS
# MODIFIES ABOVE!
uncalibrated_device_df <- data.frame(
  dev_name = c("My second THERMO1000", "My third THERMO1000"),
  devmod_id = c(1, 1),
  dev_identifier = c("NCC1701-T1", "NCC1701-T2"))
# columns added by database and row from device above
device_df_full2 <-
  cbind(data.frame(dev_id = as.integer(c(2,3))),
        uncalibrated_device_df,
        data.frame(dev_comment = as.character(c(NA, NA)), stringsAsFactors = FALSE))
device_df_full2$dev_name <- as.character(device_df_full2$dev_name)
device_df_full2$dev_identifier <- as.character(device_df_full2$dev_identifier)
device_df_full2 <- rbind(device_df_full, device_df_full2)

calibrated_device_dfPct_full2 <-
  rbind(calibrated_device_dfPct_full,
        data.frame(caldev_id = as.integer(c(2, 3)),
                   dev_id = device_df_full2$dev_id[-1],
                   caldev_datetime = as.character(c(NA, NA)),
                   caldev_parameter = as.character(c(NA, NA)),
                   caldev_comment = as.character(c(NA, NA)),
                   stringsAsFactors = FALSE)
  )

test_that("dbWriteTable_uncalibrated_device", {
  dbWriteTable_uncalibrated_device(con,
                                   dev_name = uncalibrated_device_df$dev_name,
                                   devmod_id = uncalibrated_device_df$devmod_id,
                                   dev_identifier = uncalibrated_device_df$dev_identifier)
  df <- dbReadTable(con, "device")
  expect_true(all.equal(device_df_full2, df))
  dfcd <- dbReadTable(con, "calibrated_device")
  expect_true(all.equal(calibrated_device_dfPct_full2, dfcd))
})



physical_quantity_df <- data.frame(pq_name = "air temperature",
                                   pq_unit = "degC",
                                   pq_comment = "says if air is cold or warm")
physical_quantity_df_full <- cbind(data.frame(pq_id = as.integer(1)),
                                   physical_quantity_df)
physical_quantity_df_full$pq_name <- as.character(physical_quantity_df_full$pq_name)
physical_quantity_df_full$pq_unit <- as.character(physical_quantity_df_full$pq_unit)
physical_quantity_df_full$pq_comment <- as.character(physical_quantity_df_full$pq_comment)

test_that("dbWriteTable_physical_quantity", {
  dbWriteTable_physical_quantity(con,
                                 pq_name = physical_quantity_df$pq_name,
                                 pq_unit = physical_quantity_df$pq_unit,
                                 pq_comment = physical_quantity_df$pq_comment)
  df <- dbReadTable(con, "physical_quantity")
  expect_true(identical(physical_quantity_df_full, df))
})



integration_type_df <- data.frame(inttype_name = c("single", "average"),
                                  inttype_description = c("single value at a specific time", "average value over a period"))
integration_type_df_full <- cbind(data.frame(inttype_id = as.integer(c(1, 2))),
                                  integration_type_df,
                                  data.frame(inttype_comment = as.character(c(NA,NA)), stringsAsFactors = FALSE))
integration_type_df_full$inttype_name <- as.character(integration_type_df_full$inttype_name)
integration_type_df_full$inttype_description <- as.character(integration_type_df_full$inttype_description)

test_that("dbWriteTable_integration_type", {
  dbWriteTable_integration_type(con,
                                inttype_name = integration_type_df$inttype_name,
                                inttype_description = integration_type_df$inttype_description)
  df <- dbReadTable(con, "integration_type")
  expect_true(identical(integration_type_df_full, df))
})




integration_df <- data.frame(inttype_id = 2,
                             int_measurement_interval = 60,
                             int_interval = 600)
integration_df_full <- cbind(data.frame(int_id = as.integer(1)),
                                  integration_df)

test_that("dbWriteTable_integration", {
  dbWriteTable_integration(con,
                           inttype_id = integration_df$inttype_id,
                           int_measurement_interval = integration_df$int_measurement_interval,
                           int_interval = integration_df$int_interval,)
  df <- dbReadTable(con, "integration_type")
  expect_true(identical(integration_type_df_full, df))
})



person_df <- data.frame(pers_name = "Karl Heinz")
person_df_full <- cbind(data.frame(pers_id = as.integer(1)),
                        person_df,
                        data.frame(pers_comment = as.character(NA)), stringsAsFactors = FALSE)
person_df_full$pers_name <- as.character(person_df_full$pers_name)
person_df_full$pers_comment <- as.character(person_df_full$pers_comment)

test_that("dbWriteTable_person", {
  dbWriteTable_person(con,
                      pers_name = person_df$pers_name)
  df <- dbReadTable(con, "person")
  expect_true(identical(person_df_full, df))
})



measurand_df <- data.frame(md_name = "TA2M_1",
                           md_setup_datetime = "2012-01-01 12:15:12",
                           pq_id = 1,
                           site_id = 1,
                           caldev_id = 1,
                           int_id = 1,
                           md_height = 2.,
                           pers_id = 1,
                           md_comment = "the 2m temperature")
measurand_dfPct <- measurand_df
measurand_dfPct$md_setup_datetime <-
  as.POSIXct(measurand_dfPct$md_setup_datetime, tz = "UTC")

measurand_dfPct_full <- cbind(data.frame(md_id = as.integer(1)),
                        measurand_dfPct)
measurand_dfPct_full$md_name <- as.character(measurand_dfPct_full$md_name)
measurand_dfPct_full$md_comment <- as.character(measurand_dfPct_full$md_comment)

test_that("dbWriteTable_measurand", {
  dbWriteTable_measurand(con,
                         md_name = measurand_dfPct$md_name,
                         md_setup_datetime = measurand_dfPct$md_setup_datetime,
                         pq_id = measurand_dfPct$pq_id,
                         site_id = measurand_dfPct$site_id,
                         caldev_id = measurand_dfPct$caldev_id,
                         int_id = measurand_dfPct$int_id,
                         md_height = measurand_dfPct$md_height,
                         pers_id = measurand_dfPct$pers_id,
                         md_comment = measurand_dfPct$md_comment)
  df <- dbReadTable(con, "measurand")
  expect_true(all.equal(measurand_dfPct_full, df))
  expect_error(
    dbWriteTable_measurand(con,
                           md_name = measurand_df$md_name,
                           md_setup_datetime = measurand_df$md_setup_datetime,
                           pq_id = measurand_df$pq_id,
                           site_id = measurand_df$site_id,
                           caldev_id = measurand_df$caldev_id,
                           int_id = measurand_df$int_id,
                           md_height = measurand_df$md_height,
                           pers_id = measurand_df$pers_id,
                           md_comment = measurand_df$md_comment)

  )
})




quality_flag_df <- data.frame(
  qf_id = c(1,11),
  qf_name = c("value ok, automatic qc", "corrected calculation constant"),
  qf_description = c("value ok, automatically checked for consistency",
                     "original value used from calculation constant"))

quality_flag_df_full <- cbind(quality_flag_df,
                                 data.frame(qf_comment = as.character(c(NA, NA)), stringsAsFactors = FALSE))
quality_flag_df_full$qf_name <- as.character(quality_flag_df_full$qf_name)
quality_flag_df_full$qf_description <- as.character(quality_flag_df_full$qf_description)

test_that("dbWriteTable_quality_flag", {
  dbWriteTable_quality_flag(con,
                         qf_id = quality_flag_df$qf_id,
                         qf_name = quality_flag_df$qf_name,
                         qf_description = quality_flag_df$qf_description)
  df <- dbReadTable(con, "quality_flag")
  expect_true(all.equal(quality_flag_df_full, df))
})



station_adlershof_df <- data.frame(
  stadl_datetime = c("2017-01-01 12:15:12", "2017-01-01 16:15:12", "2017-01-05 16:15:12"),
  md_id = c(1, 1, 1),
  stadl_value = c(293.15, 294.15, 270.15))

# test different time zones
station_adlershof_dfPct <- station_adlershof_df
station_adlershof_dfPct$stadl_datetime <-
  c(as.POSIXct(station_adlershof_dfPct$stadl_datetime[1], tz = "UTC"),
    as.POSIXct(station_adlershof_dfPct$stadl_datetime[2], tz = "CET"),
    as.POSIXct(station_adlershof_dfPct$stadl_datetime[2], tz = "GMT"))
station_adlershof_dfPct_full <- cbind(data.frame(stadl_id = c(1, 2, 3)),
                                      station_adlershof_dfPct,
                                      data.frame(qf_id = as.integer(NA, NA, NA)))

test_that("dbWriteTable_station_adlershof", {
  dbWriteTable_station_adlershof(con,
                            stadl_datetime = station_adlershof_dfPct$stadl_datetime,
                            md_id = station_adlershof_dfPct$md_id,
                            stadl_value = station_adlershof_dfPct$stadl_value)
  df <- dbReadTable(con, "station_adlershof")
  expect_true(all.equal(station_adlershof_dfPct_full, df))
  # no POSIXct should give error
  expect_error(
    dbWriteTable_station_adlershof(con,
                                   stadl_datetime = station_adlershof_df$stadl_datetime,
                                   md_id = station_adlershof_df$md_id,
                                   stadl_value = station_adlershof_df$stadl_value)
  )
})



cor_stadl_id <- 1
station_adlershof_dfPct_full$qf_id[cor_stadl_id] <- 1
test_that("dbUpdateQF_station_adlershof", {
  dbUpdateQF_station_adlershof(con,
                               stadl_id = cor_stadl_id,
                               qf_id = station_adlershof_dfPct_full$qf_id[cor_stadl_id])
  df <- dbReadTable(con, "station_adlershof")
  # reorder df because UPDATE changes original order
  df <- df[order(df$stadl_id), ]
  rownames(df) <- 1:3
  expect_true(all.equal(station_adlershof_dfPct_full, df))
})


cor_stadl_id <- c(2, 3)
station_adlershof_correction_df <-
  data.frame(stadlcor_id = c(1, 2),
             stadl_id = cor_stadl_id,
             stadlcor_datetime = c("2017-05-01 16:15:12", "2017-05-05 16:15:12"),
             md_id = c(1,1),
             stadlcor_value = c(290.15, 278.15)
  )
station_adlershof_correction_dfPct <- station_adlershof_correction_df
station_adlershof_correction_dfPct$stadlcor_datetime <-
  as.POSIXct(station_adlershof_correction_dfPct$stadlcor_datetime, tz = "UTC")

station_adlershof_dfPct_full$qf_id[cor_stadl_id] <- 11
test_that("dbAddCorrection_station_adlershof", {
  dbAddCorrection_station_adlershof(con,
                                    stadl_id = station_adlershof_correction_dfPct$stadl_id,
                                    qf_id = station_adlershof_dfPct_full$qf_id[cor_stadl_id],
                                    stadlcor_datetime = station_adlershof_correction_dfPct$stadlcor_datetime,
                                    md_id = station_adlershof_correction_dfPct$md_id,
                                    stadlcor_value = station_adlershof_correction_dfPct$stadlcor_value)
  df <- dbReadTable(con, "station_adlershof_correction")
  expect_true(all.equal(station_adlershof_correction_dfPct, df))
  # no POSIXct should give error
  expect_error(
    dbAddCorrection_station_adlershof(con,
                                      stadl_id = station_adlershof_correction_df$stadl_id,
                                      qf_id = station_adlershof_dfPct_full$qf_id[cor_stadl_id],
                                      stadlcor_datetime = station_adlershof_correction_df$stadlcor_datetime,
                                      md_id = station_adlershof_correction_df$md_id,
                                      stadlcor_value = station_adlershof_correction_df$stadlcor_value)
  )
})


dbDisconnect(con)
