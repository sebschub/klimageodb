context("Database simple read functions")

con <- dbConnect_klimageo(host = "localhost")
test_that("dbConnect_klimageo2", {
  expect_is(con, "DBIConnection")
})


test_transaction_completed <- function(conn) {
  expect_true(dbBegin(conn))
  expect_true(dbCommit(conn))
}

simple_read_test <- function(conn, table) {

  read_string <- paste0("dbReadTable_", table)
  read_function <- get(read_string)

  # argument for test function
  read_args <- c(list(conn = conn))

  test_that(read_string, {
    df <- do.call(read_function, read_args)
    df_dbi <- dbReadTable(conn, table)
    expect_identical(df, df_dbi)
    test_transaction_completed(conn)
  })

}

test_tables <- c("site",
                 "device_manufacturer", "device_type", "device_model", "device",
                 "calibrated_device",
                 "physical_quantity",
                 "integration_type", "integration",
                 "person",
                 "measurand",
                 "quality_flag",
                 "station_adlershof", "station_adlershof_correction")

test_views <- c("device_model_detail", "device_detail", "calibrated_device_detail",
                 "integration_detail",
                 "measurand_detail")

for (table in c(test_tables, test_views)) {
  simple_read_test(con, table)
}


dbDisconnect(con)
