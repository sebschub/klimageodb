dbConnect_klimageo <- function(dsn) {
  dbConnect(odbc::odbc(), dsn = dsn, timezone = "Europe/Berlin")
}
