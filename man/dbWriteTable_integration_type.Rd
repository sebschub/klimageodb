% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/db_write.R
\name{dbWriteTable_integration_type}
\alias{dbWriteTable_integration_type}
\title{Insert data into \code{integration_type} table}
\usage{
dbWriteTable_integration_type(
  conn,
  inttype_name,
  inttype_description,
  inttype_comment = NULL
)
}
\arguments{
\item{conn}{Database connection.}

\item{inttype_name}{String vector of name of integration type.}

\item{inttype_description}{String vector of description of
\code{integration_type}.}

\item{inttype_comment}{String vector of additional comments.}
}
\value{
Data frame of newly added rows.
}
\description{
Insert data into \code{integration_type} table
}
\examples{
\dontrun{
con <- dbConnect_klimageo()
dbWriteTable_integration_type(con, "average", "average value over a period")
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
\code{\link{dbWriteTable_person}()},
\code{\link{dbWriteTable_quality_flag}()},
\code{\link{dbWriteTable_site}()}
}
\concept{custom dbWriteTable functions}
