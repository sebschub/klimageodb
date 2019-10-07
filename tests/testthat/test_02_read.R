context("Database simple read functions")

con <- dbConnect_klimageo(host = "localhost", port = 5432)
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
                 "calibration_state",
                 "physical_quantity",
                 "integration_type", "integration",
                 "person",
                 "measurand",
                 "quality_flag",
                 "station_adlershof", "station_adlershof_correction",
                 "station_patagonia", "station_patagonia_correction")

test_views <- c("device_model_detail", "device_detail", "calibration_state_detail",
                "integration_detail",
                "measurand_detail",
                "station_adlershof_corrected",
                "station_patagonia_corrected")

for (table in c(test_tables, test_views)) {
  simple_read_test(con, table)
}

# test selection of data in read function
where_read_test <- function(conn, table, id_column, date_column) {

  read_string <- paste0("dbReadTable_", table)
  read_function <- get(read_string)

  # argument for test function
  read_args_simple <- c(list(conn = conn))
  df_full <- do.call(read_function, read_args_simple)

  # create all kinds of md_id combinations as list elements
  md_id_list <- list()
  for (l in 1:5) {
    md_id_list <- c(md_id_list, combn(1:5, l, simplify = FALSE))
  }

  start_datetime <- as.POSIXct("2017-01-03 17:00:00", tz = "UTC")
  end_datetime <- as.POSIXct("2017-02-05 09:00:00", tz = "UTC")

  test_that(read_string, {
    # test start date
    read_args <- c(list(conn = conn, start_datetime = start_datetime))
    df_selection <- do.call(read_function, read_args)
    df_selection <- df_selection[order(df_selection[[id_column]]), ]
    df_full_selection <- df_full[which(df_full[[date_column]] >= start_datetime), ]
    df_full_selection <- df_full_selection[order(df_full_selection[[id_column]]), ]
    expect_equivalent(df_selection, df_full_selection)
    test_transaction_completed(conn)

    # test end date
    read_args <- c(list(conn = conn, end_datetime = end_datetime))
    df_selection <- do.call(read_function, read_args)
    df_selection <- df_selection[order(df_selection[[id_column]]), ]
    df_full_selection <- df_full[which(df_full[[date_column]] <= end_datetime), ]
    df_full_selection <- df_full_selection[order(df_full_selection[[id_column]]), ]
    expect_equivalent(df_selection, df_full_selection)
    test_transaction_completed(conn)

    # test both dates
    read_args <- c(list(conn = conn, start_datetime = start_datetime, end_datetime = end_datetime))
    df_selection <- do.call(read_function, read_args)
    df_selection <- df_selection[order(df_selection[[id_column]]), ]
    df_full_selection <- df_full[which(df_full[[date_column]] >= start_datetime &
                                         df_full[[date_column]] <= end_datetime), ]
    df_full_selection <- df_full_selection[order(df_full_selection[[id_column]]), ]
    expect_equivalent(df_selection, df_full_selection)
    test_transaction_completed(conn)

    # check md_ids
    for (md_id in md_id_list) {
      read_args <- c(list(conn = conn, md_id = md_id))
      df_selection <- do.call(read_function, read_args)
      df_selection <- df_selection[order(df_selection[[id_column]]), ]
      df_full_selection <- df_full[which(df_full$md_id %in% md_id), ]
      df_full_selection <- df_full_selection[order(df_full_selection[[id_column]]), ]
      expect_equivalent(df_selection, df_full_selection)
      test_transaction_completed(conn)
    }
  })

}

where_read_test(con, "station_adlershof", "stadl_id", "stadl_datetime")
where_read_test(con, "station_adlershof_corrected", "stadl_id", "stadl_datetime")
where_read_test(con, "station_adlershof_correction", "stadlcor_id", "stadlcor_datetime")

where_read_test(con, "station_patagonia", "stapa_id", "stapa_datetime")
where_read_test(con, "station_patagonia_corrected", "stapa_id", "stapa_datetime")
where_read_test(con, "station_patagonia_correction", "stapacor_id", "stapacor_datetime")


dbDisconnect(con)
