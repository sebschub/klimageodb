context("Database write functions")

con <- dbConnect_klimageo()
test_that("dbConnect_klimageo", {
  expect_is(con, "DBIConnection")
})


site_df <- data.frame(site_name = "Adlershof",
                         site_lon = 14.,
                         site_lat = 53.,
                         stringsAsFactors = FALSE)
test_that("dbWriteTable_site", {
  dbWriteTable_site(con,
                    site_name = site_df$site_name,
                    site_lon = site_df$site_lon,
                    site_lat = site_df$site_lat)
  df <- dbReadTable(con, "site")
  expect_true(identical(site_df, df[c("site_name", "site_lon", "site_lat")]))
})

device_manufacturer_df <- data.frame(devman_name = "TSI")
test_that("dbWriteTable_device_manufacturer", {
  dbWriteTable_device_manufacturer(con,
                                   devman_name = device_manufacturer_df$devman_name)
  df <- dbReadTable(con, "device_manufacturer")
  expect_true(identical(device_manufacturer_df, df[c("devman_name")]))
})


device_type_df <- data.frame(devtype_name = "thermometer")
test_that("dbWriteTable_device_type", {
  dbWriteTable_device_type(con,
                           devtype_name = device_type_df$devtype_name)
  df <- dbReadTable(con, "device_type")
  expect_true(identical(device_type_df, df[c("devtype_name")]))
})


dbDisconnect(con)
