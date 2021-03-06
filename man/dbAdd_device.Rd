% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/db_write.R
\name{dbAdd_device}
\alias{dbAdd_device}
\alias{dbWriteTable_device}
\title{Add devices to \code{device} table}
\usage{
dbAdd_device(
  conn,
  dev_name,
  devmod_name,
  dev_identifier = NULL,
  dev_comment = NULL
)

dbWriteTable_device(
  conn,
  dev_name,
  devmod_id,
  dev_identifier = NULL,
  dev_comment = NULL
)
}
\arguments{
\item{conn}{Database connection.}

\item{dev_name}{String vector of name of device.}

\item{devmod_name}{String vector of name of model.}

\item{dev_identifier}{String vector of device identifiers, e.g. serial
numbers.}

\item{dev_comment}{String vector of additional comments.}

\item{devmod_id}{Integer vector of \code{device_model} ID.}
}
\value{
Data frame of newly added rows.
}
\description{
\code{dbWriteTable_device} requires a correct device model id while
\code{dbAdd_device} derives that from the device model name.
}
\examples{
\dontrun{
con <- dbConnect_klimageo()
dbWriteTable_device_manufacturer(con, "TSI")
dbWriteTable_device_type(con, "thermometer")
dbWriteTable_device_model(con, "THERMO1000", 1, 1)
dbWriteTable_device(con, "My first THERMO1000", 1, "NCC1701-T")
dbDisconnect(con)
}
}
\seealso{
\code{\link{dbAdd_device_uncalibrated}} for adding a device that
  does not require calibration to both \code{device} and
  \code{calibration_state}

Other custom dbWriteTable functions: 
\code{\link{dbAddCorrection_station_adlershof}()},
\code{\link{dbAddCorrection_station_patagonia}()},
\code{\link{dbAdd_calibration_state}()},
\code{\link{dbAdd_device_model}()},
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
