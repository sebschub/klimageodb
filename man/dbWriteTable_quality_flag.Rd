% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/db_write.R
\name{dbWriteTable_quality_flag}
\alias{dbWriteTable_quality_flag}
\title{Insert data into \code{quality_flag} table}
\usage{
dbWriteTable_quality_flag(
  conn,
  qf_id,
  qf_name,
  qf_description,
  qf_comment = NULL
)
}
\arguments{
\item{conn}{Database connection.}

\item{qf_id}{Integer vector with 1 <= qf_id <= 9 indicating value ok and
qf_id >= 10 indicating value not ok.}

\item{qf_name}{Character vector of quality_flag name.}

\item{qf_description}{Character vector of quality_flag description.}

\item{qf_comment}{Character vector of of additional information.}
}
\value{
Data frame of newly added rows.
}
\description{
Insert data into \code{quality_flag} table
}
\examples{
\dontrun{
con <- dbConnect_klimageo()
dbWriteTable_quality_flag(
   qf_id = c(1,11),
   qf_name = c("value ok, automatic qc", "corrected calculation constant"),
   qf_description = c("value ok, automatically checked for consistency",
                      "original value used from calculation constant")))
dbDisconnect(con)
}
}
\seealso{
Other custom dbWriteTable functions: 
\code{\link{dbAddCorrection_station_adlershof}()},
\code{\link{dbAddCorrection_station_patagonia}()},
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
\code{\link{dbWriteTable_site}()}
}
\concept{custom dbWriteTable functions}
