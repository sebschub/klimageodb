## Tests for interactive function not working yet
# context("Correction of data")
#
# con <- dbConnect_klimageo(host = "localhost", port = 5432)
# test_that("dbConnect_klimageo2", {
#   expect_is(con, "DBIConnection")
# })
#
#
# test_transaction_completed <- function(conn) {
#   expect_true(dbBegin(conn))
#   expect_true(dbCommit(conn))
# }
#
# dbReadTable_station_adlershof(con)
# dbReadTable_station_adlershof_corrected(con)
# dbReadTable_station_adlershof_correction(con)
# dbReadTable_quality_flag(con)
# dbCheckTable_station_adlershof(con, md_id = 1, qf_id_bad = 11)
# dbCheckTable_station_adlershof(con, md_id = 1, qf_id_good = 20, qf_id_bad = 11)
# dbCheckTable_station_adlershof(con, md_id = 2, qf_id_bad = 11)
#
#
# dbDisconnect(con)
