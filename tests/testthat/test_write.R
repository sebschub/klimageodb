context("Database write functions")

con <- dbConnect_klimageo()
test_that("dbConnect_klimageo", {
  expect_is(con, "DBIConnection")
})



site_df <- data.frame(site_name = "Adlershof",
                         site_lon = 14.,
                         site_lat = 53.,
                         stringsAsFactors = FALSE)
dbWriteTable_site(con,
                  site_name = site_df$site_name,
                  site_lon  = site_df$site_lon,
                  site_lat  = site_df$site_lat)

test_that("dbWriteTable_site", {
  df <- dbReadTable(con, "site")
  expect_true(identical(site_df, df[c("site_name", "site_lon", "site_lat")]))
})


# database does not store factors, identical and all.equal gives false when
# comparing factor and non-factor
device_manufacturer_df <- data.frame(devman_name = "TSI")
device_manufacturer_dfnf <- device_manufacturer_df
device_manufacturer_dfnf$devman_name <- as.character(device_manufacturer_dfnf$devman_name)
# writing factor because people will do that
dbWriteTable_device_manufacturer(con,
                                 devman_name = device_manufacturer_df$devman_name)

test_that("dbWriteTable_device_manufacturer", {
  df <- dbReadTable(con, "device_manufacturer")
  # comparison with non-factor
  expect_true(identical(device_manufacturer_dfnf, df[c("devman_name")]))
})



device_type_df <- data.frame(devtype_name = "thermometer")
device_type_dfnf <- device_type_df
device_type_dfnf$devtype_name <- as.character(device_type_dfnf$devtype_name)
dbWriteTable_device_type(con,
                         devtype_name = device_type_df$devtype_name)

test_that("dbWriteTable_device_type", {
  df <- dbReadTable(con, "device_type")
  expect_true(identical(device_type_dfnf, df[c("devtype_name")]))
})



device_model_df <- data.frame(devmod_name = "THERMO1000",
                              devtype_id = 1,
                              devman_id = 1)
device_model_dfnf <- device_model_df
device_model_dfnf$devmod_name <- as.character(device_model_dfnf$devmod_name)
dbWriteTable_device_model(con,
                          devmod_name = device_model_df$devmod_name,
                          devtype_id = device_model_df$devtype_id,
                          devman_id = device_model_df$devman_id)

test_that("dbWriteTable_device_model", {
  df <- dbReadTable(con, "device_model")
  # use all.equal because all.equal(1., as.integer(1)) but not identical(1.,
  # as.integer(1))
  expect_true(all.equal(device_model_dfnf, df[c("devmod_name", "devtype_id", "devman_id")]))
})



device_df <- data.frame(dev_name = "My first THERMO1000",
                        devmod_id = 1,
                        dev_identifier = "NCC1701-T")
device_dfnf <- device_df
device_dfnf$dev_name <- as.character(device_dfnf$dev_name)
device_dfnf$dev_identifier <- as.character(device_dfnf$dev_identifier)
dbWriteTable_device(con,
                    dev_name = device_df$dev_name,
                    devmod_id = device_df$devmod_id,
                    dev_identifier = device_df$dev_identifier)

test_that("dbWriteTable_device", {
  df <- dbReadTable(con, "device")
  expect_true(all.equal(device_dfnf, df[c("dev_name", "devmod_id", "dev_identifier")]))
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
calibrated_device_dfPctnf <- calibrated_device_dfPct
calibrated_device_dfPctnf$caldev_parameter <-
  as.character(calibrated_device_dfPctnf$caldev_parameter)

dbWriteTable_calibrated_device(con,
                               dev_id = calibrated_device_dfPct$dev_id,
                               caldev_datetime = calibrated_device_dfPct$caldev_datetime,
                               caldev_parameter = calibrated_device_dfPct$caldev_parameter)

test_that("dbWriteTable_calibrated_device", {
  df <- dbReadTable(con, "calibrated_device")
  expect_true(all.equal(calibrated_device_dfPctnf, df[c("dev_id", "caldev_datetime", "caldev_parameter")]))
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
uncalibrated_device_dfnf <- uncalibrated_device_df
uncalibrated_device_dfnf$dev_name <- as.character(uncalibrated_device_dfnf$dev_name)
uncalibrated_device_dfnf$dev_identifier <- as.character(uncalibrated_device_dfnf$dev_identifier)
dbWriteTable_uncalibrated_device(con,
                                 dev_name = uncalibrated_device_df$dev_name,
                                 devmod_id = uncalibrated_device_df$devmod_id,
                                 dev_identifier = uncalibrated_device_df$dev_identifier)

test_that("dbWriteTable_uncalibrated_device", {
  df <- dbReadTable(con, "device")
  expect_true(all.equal(rbind(device_dfnf, uncalibrated_device_dfnf),
                        df[c("dev_name", "devmod_id", "dev_identifier")]))
  dfcd <- dbReadTable(con, "calibrated_device")
  expect_true(all.equal(
    rbind(calibrated_device_dfPctnf,
          data.frame(dev_id = df$dev_id[-1],
                     caldev_datetime = c(NA, NA),
                     caldev_parameter = c(NA, NA))
          ),
    dfcd[c("dev_id", "caldev_datetime", "caldev_parameter")]))
})




physical_quantity_df <- data.frame(pq_name = "air temperature",
                                   pq_unit = "degC")
physical_quantity_dfnf <- physical_quantity_df
physical_quantity_dfnf$pq_name <- as.character(physical_quantity_dfnf$pq_name)
physical_quantity_dfnf$pq_unit <- as.character(physical_quantity_dfnf$pq_unit)
dbWriteTable_physical_quantity(con,
                               pq_name = physical_quantity_df$pq_name,
                               pq_unit = physical_quantity_df$pq_unit)

test_that("dbWriteTable_physical_quantity", {
  df <- dbReadTable(con, "physical_quantity")
  expect_true(identical(physical_quantity_dfnf, df[c("pq_name", "pq_unit")]))
})




dbDisconnect(con)
