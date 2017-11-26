context("Database write functions")

con <- dbConnect_klimageo()

test_that("dbConnect_klimageo", {
  expect_is(con, "DBIConnection")
})
