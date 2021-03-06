% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cf_data.R
\name{get_cf_standard_names_table}
\alias{get_cf_standard_names_table}
\title{Get CF standard name table}
\usage{
get_cf_standard_names_table(version = 48)
}
\arguments{
\item{version}{The version of the CF standard name table.}
}
\value{
An \code{xml2} nodeset of the entries in the CF standard name table.
}
\description{
This function downloads the CF standard name table and caches it for further
use.
}
\details{
This function downloads the xml file of the CF standard name table, extracts
its entries with the \code{\link[xml2]{read_xml}} and
\code{\link[xml2]{xml_find_all}}, and stores the result as well as the
\code{version} in the \code{cf_cache} environment. If called again, the
function uses the cached result unless the \code{version} argument changes.
}
\examples{
get_cf_standard_names_table()
# use older standard
get_cf_standard_names_table(42)
}
\keyword{internal}
