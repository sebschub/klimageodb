% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/db_write.R
\name{dbAddCorrection_station_patagonia}
\alias{dbAddCorrection_station_patagonia}
\alias{dbUpdate_station_patagonia_qf_id}
\alias{dbWriteTable_station_patagonia_correction}
\title{Add corrections of \code{station_patagonia} measurements}
\usage{
dbAddCorrection_station_patagonia(
  conn,
  stapa_id,
  qf_id,
  stapacor_datetime,
  md_id,
  stapacor_value
)

dbUpdate_station_patagonia_qf_id(conn, stapa_id, qf_id, overwrite = TRUE)

dbWriteTable_station_patagonia_correction(
  conn,
  stapa_id,
  stapacor_datetime,
  md_id,
  stapacor_value
)
}
\arguments{
\item{conn}{Database connection.}

\item{stapa_id}{Integer vector of \code{station_patagonia} ID.}

\item{qf_id}{Integer vector with 1 <= qf_id <= 9 indicating value ok and
qf_id >= 10 indicating value not ok.}

\item{stapacor_datetime}{POSIXct vector of corrected date and time of
measurement.}

\item{md_id}{Integer vector of measurand ID.}

\item{stapacor_value}{Numeric vector of corrected value of measurement.}

\item{overwrite}{Overwrite non-null qf_id values. If \code{FALSE}, only null
qf_id values are modified.}
}
\value{
For performance reason, contrary to the meta data table functions,
  these functions do not return anything.
}
\description{
In general, use \code{dbAddCorrection_station_patagonia} to consistently add
the corrected measurement values to \code{station_patagonia_correction} and
to modify the quality flags in \code{station_patagonia}.
\code{dbWriteTable_station_patagonia_correction} and
\code{dbUpdate_station_patagonia_qf_id} do the respective single actions.
}
\details{
By design, qf_id with values >= 10 in \code{station_patagonia} indicates that
entries in \code{station_patagonia} are corrected in
\code{station_patagonia_correction}. Thus,
\code{dbAddCorrection_station_patagonia} imposes the constraint qf_id >= 10.
\code{dbUpdate_station_patagonia_qf_id}, on the other hand, allows general
values of qf_id (further checks are done by the database).
}
\examples{
\dontrun{
con <- dbConnect_klimageo()
add all required entries before with the respective dbWriteTable_*
dbAddCorrection_station_patagonia(
  con,
  stapa_id = c(1,2),
  qf_id = c(11,11),
  stapacor_datetime =
  as.POSIXct(c("2017-01-01 12:15:12", "2017-01-01 12:15:12"), tz = "UTC"),
  md_id = c(1,1),
  stapacor_value = c(290.12, 289.23))
dbDisconnect(con)
}
}
\seealso{
Other custom dbWriteTable functions: 
\code{\link{dbAddCorrection_station_adlershof}()},
\code{\link{dbAdd_calibration_state}()},
\code{\link{dbAdd_device_model}()},
\code{\link{dbAdd_device}()},
\code{\link{dbAdd_integration}()},
\code{\link{dbAdd_measurand}()},
\code{\link{dbAdd_physical_quantity}()},
\code{\link{dbAdd_station_adlershof}()},
\code{\link{dbAdd_station_patagonia}()},
\code{\link{dbWriteTable_device_manufacturer}()},
\code{\link{dbWriteTable_device_type}()},
\code{\link{dbWriteTable_integration_type}()},
\code{\link{dbWriteTable_person}()},
\code{\link{dbWriteTable_quality_flag}()},
\code{\link{dbWriteTable_site}()}
}
\concept{custom dbWriteTable functions}
