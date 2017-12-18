cf_cache <- new.env()

get_cf_standard_names_table <- function(
  url = "http://cfconventions.org/Data/cf-standard-names/48/src/cf-standard-name-table.xml") {

  # check if data is not already present
  if (is.null(entries <- get0("entries", envir = cf_cache))) {
    data <- xml2::read_xml(url)
    entries <- xml2::xml_find_all(data, '//entry')
    assign("entries", entries, envir = cf_cache)
  }
  entries
}


get_cf_unit_description <- function(standard_name) {

  tryCatch({
    entries <- get_cf_standard_names_table()[
    match(standard_name,
          xml2::xml_attr(get_cf_standard_names_table(), "id")
    )]}, error = function(e) {
      stop("Error fetching the CF convention file or wrong standard name.")
    })
  data.frame(
    unit = as.character(xml2::xml_contents(xml2::xml_find_all(entries, "canonical_units"))),
    description = as.character(xml2::xml_contents(xml2::xml_find_all(entries, "description")))
  )
}
