cf_cache <- new.env()

#' Get CF standard name table
#'
#' This function downloads the CF standard name table and caches it for further
#' use.
#'
#' This function downloads the xml file of the CF standard name table, extracts
#' its entries with the \code{\link[xml2]{read_xml}} and
#' \code{\link[xml2]{xml_find_all}}, and stores the result as well as the
#' \code{version} in the \code{cf_cache} environment. If called again, the
#' function uses the cached result unless the \code{version} argument changes.
#'
#'
#' @param version The version of the CF standard name table.
#'
#' @return An \code{xml2} nodeset of the entries in the CF standard name table.
#' @export
#' @keywords internal
#'
#' @examples
#' get_cf_standard_names_table()
#' # use older standard
#' get_cf_standard_names_table(42)
get_cf_standard_names_table <- function(version = 48) {

  # check if data is not already present
  if ( is.null(entries <- get0("entries", envir = cf_cache)) |
      !identical(version, get0("version", envir = cf_cache)) ) {
    url <- paste0("http://cfconventions.org/Data/cf-standard-names/", version,
                  "/src/cf-standard-name-table.xml")
    data <- xml2::read_xml(url)
    entries <- xml2::xml_find_all(data, '//entry')
    assign("entries", entries, envir = cf_cache)
    assign("version", version, envir = cf_cache)
  }
  entries
}

#' Get unit and description of a variable from CF standard name table
#'
#' This function extracts for a \code{standard_name} the unit and description
#' form the CF standard name table.
#'
#' @inheritParams get_cf_standard_names_table
#' @param standard_name String vector of standard names in the CF standard name
#'   table.
#'
#' @return A data frame with columns \code{unit} and \code{description}.
#' @export
#' @keywords internal
#'
#' @examples
#' get_cf_unit_description("air_temperature")
#' # use older standard
#' get_cf_unit_description("air_temperature", version = 42)
get_cf_unit_description <- function(standard_name, version = 48) {

  tryCatch({
    entries <- get_cf_standard_names_table(version)[
    match(standard_name,
          xml2::xml_attr(get_cf_standard_names_table(version), "id")
    )]}, error = function(e) {
      stop("Error fetching the CF convention file or wrong standard name.")
    })
  data.frame(
    unit =        xml2::xml_text(xml2::xml_find_all(entries, "canonical_units")),
    description = xml2::xml_text(xml2::xml_find_all(entries, "description"))
  )
}
