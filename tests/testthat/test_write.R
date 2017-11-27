context("Database write functions")

con <- dbConnect_klimageo()
test_that("dbConnect_klimageo", {
  expect_is(con, "DBIConnection")
})


site_array <- data.frame(site_name = "Adlershof",
                         site_lon = 14.,
                         site_lat = 53.,
                         stringsAsFactors = FALSE)
test_that("dbWriteTable_site", {
  dbWriteTable_site(con,
                    site_name = site_array$site_name,
                    site_lon = site_array$site_lon,
                    site_lat = site_array$site_lat)
  df <- dbReadTable(con, "site")
  expect_true(identical(site_array, df[c("site_name", "site_lon", "site_lat")]))
})

dbDisconnect(con)
