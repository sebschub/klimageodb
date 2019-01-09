#' Check data in the \code{station_adlershof} table and flag bad data points
#'
#' This function allows to interactively select bad data points of a selected
#' \code{md_id} and a period defined by \code{start_datetime} and
#' \code{end_datetime}. This points will be flagged with the quality flag
#' \code{qf_id_bad} and \code{NA} values in \code{station_adlershof_correcting}
#' while the rest of the points will be flagged with \code{qf_id_good}.
#' Currently, this function does not offer a way to write other values than
#' \code{NA} into \code{station_adlershof_correcting}; it is only meant for the
#' case when the data is not correctable.
#'
#' The data of the selected \code{md_id} and period is plotted unless some
#' correcting entries in \code{station_adlershof_correcting} already refer to
#' the selected data; in this case the function is aborted. Click on the bad
#' points in the plot. These points will be marked with \code{qf_id_bad} and
#' corresponding \code{NA} values are written to
#' \code{station_adlershof_correcting}, while the rest is marked with
#' \code{qf_id_good}. The bad points will be written to the terminal; you will
#' have to confirm that the respective quality flags will be written. If there
#' are already some quality flags set in the data, you will have to confirm to
#' overwrite them.
#'
#' @inheritParams database_fields
#'
#' @export
#'
#' @examples
#' \dontrun{
#' con <- dbConnect_klimageo()
#' # add all required entries before with the respective dbWriteTable_*
#' dbWriteTable_measurand(md_name = "TA2M_1",
#'                        md_setup_datetime = as.POSIXct("2012-01-01 12:15:12", tz = "UTC"),
#'                        pq_id = 1,
#'                        site_id = 1,
#'                        calstate_id = 1,
#'                        int_id = 1,
#'                        md_height = 2.,
#'                        pers_id = 1,
#'                        md_comment = "the 2m temperature"))
#' dbDisconnect(con)
#' }
dbCheckTable_station_adlershof <- function(conn,
                                           start_datetime = NULL,
                                           end_datetime = NULL,
                                           md_id,
                                           qf_id_good = 1,
                                           qf_id_bad = 10){

  # check conventions of qf_id
  stopifnot(qf_id_good >= 1 & qf_id_good <= 9)
  stopifnot(qf_id_bad  >= 10)

  # get metadata for information on the plot
  metadata <- DBI::dbGetQuery(conn,
                              paste("SELECT * FROM measurand_detail",
                                    "WHERE md_id =", md_id, ";"))
  # data to check
  dbdata <- dbReadTable_station_adlershof(conn,
                                          start_datetime = start_datetime,
                                          end_datetime = end_datetime,
                                          md_id = md_id)

  if (nrow(dbdata) == 0) {
    message("No data present for the selected md_id and period.")
    # stop returning the correcting data
    return(invisible())
  }

  # get corresponding data from correcting table
  dbdata_correcting <-
    DBI::dbGetQuery(conn,
                    paste("SELECT * FROM station_adlershof_correction",
                          "WHERE stadl_id in (",
                          paste0("'", dbdata$stadl_id, "'", collapse = ", "),
                          ");")
                    )
  if (nrow(dbdata_correcting) > 0) {
    message("Correcting data already present in the selected period! Handle this case manually.")
    message("Returning the respective data in station_adlershof_correction.")
    # stop returning the correcting data
    return(dbdata_correcting)
  }

  # plot time series with metadata
  graphics::plot(dbdata$stadl_datetime, dbdata$stadl_value,
                 xlab = "date and time",
                 ylab = paste0(metadata$pq_name, " (", metadata$pq_unit, ")"),
                 main = paste("ORIGINAL",
                              metadata$site_name,
                              ifelse(is.na(metadata$md_height),
                                     "",
                                     paste0("height = ", metadata$md_height, "m")),
                              ifelse(is.na(metadata$md_orientation),
                                     "",
                                     paste0("orientation = ", metadata$md_orientation, "\u00B0")),
                              ifelse(is.na(metadata$md_tilt),
                                     "",
                                     paste0("tilt = ", metadata$md_tilt, "\u00B0"))
                 )
  )
  message("Showing the raw data from the database:")
  message(paste0("Select points that will be flagged with qf_id ", qf_id_bad,
      "."))
  message(paste0("The remaining points will be flagged with qf_id ",
      qf_id_good,"."))
  # let the user select bad points
  bad_index <- graphics::identify(dbdata$stadl_datetime, dbdata$stadl_value)
  bad_data <- dbdata[bad_index, ]

  if (length(bad_index) > 0) {
    good_data <- dbdata[-bad_index, ]
    message("Selected bad values:")
    message(paste0(utils::capture.output(bad_data), collapse = "\n"))
    question_text <- paste0("Do you want to flag these lines with qf_id ",
                            qf_id_bad, " and the rest with qf_id ", qf_id_good, "?")
  } else {
    # no bad values identified
    good_data <- dbdata
    message("No bad values selected.")
    question_text <- paste0("Do you want to flag the complete data with qf_id ",
                            qf_id_good, "?")
  }

  answer <- utils::askYesNo(question_text, default = FALSE)

  # answer is na if cancel was chosen. Stop function
  if (is.na(answer)) {
    message("Canceled.")
    return(invisible())
  }

  if (answer) {
    # apply modifications

    # good points?
    if (nrow(good_data) > 0) {

      # ask if there are already some non-na qf_id in the data
      if (any(!is.na(good_data$qf_id))) {
        message("Some qf_id of the good points are already defined.")
        answer_check <- utils::askYesNo("Do you want to override these values?",
                                        default = TRUE)
      } else {
        answer_check <- TRUE
      }

      # answer_check is na if cancel was chosen. Stop function
      if (is.na(answer_check)) {
        message("Canceled.")
        return(invisible())
      }
      if (answer_check) {
        dbUpdate_station_adlershof_qf_id(conn, good_data$stadl_id,
                                         qf_id = rep(qf_id_good, nrow(good_data)))
      } else {
        message("Modifing qf_id of good points skipped.")
      }

    } else {
      message(paste("No good points left. qf_id", qf_id_good, "not written to data."))
    }

    # bad points?
    if (nrow(bad_data) > 0) {

      # ask if there are already some non-na qf_id in the data
      if (any(!is.na(bad_data$qf_id))) {
        message("Some qf_id of the bad points are already defined.")
        answer_check <- utils::askYesNo("Do you want to override these values?",
                                        default = TRUE)
      } else {
        answer_check <- TRUE
      }

      # answer_check is na if cancel was chosen. Stop function
      if (is.na(answer_check)) {
        message("Canceled.")
        return(invisible())
      }
      if (answer_check) {
        dbAddCorrection_station_adlershof(conn,
                                          stadl_id = bad_data$stadl_id,
                                          qf_id = rep(qf_id_bad, nrow(bad_data)),
                                          stadlcor_datetime = bad_data$stadl_datetime,
                                          md_id = md_id,
                                          stadlcor_value = rep(NA_real_, nrow(bad_data)))
      } else {
        message("Modifing qf_id of bad points skipped.")
      }

    } else {
      # already informed the user that there are no bad points
    }

    message("Showing corrected data for reference.")
    # corrected data
    dbdata_corrected <-
      dbReadTable_station_adlershof_corrected(conn,
                                              start_datetime = start_datetime,
                                              end_datetime = end_datetime,
                                              md_id = md_id)

    # plot time series with metadata
    graphics::plot(dbdata_corrected$stadl_datetime, dbdata_corrected$stadl_value,
                   xlab = "date and time",
                   ylab = paste0(metadata$pq_name, " (", metadata$pq_unit, ")"),
                   main = paste("CORRECTED",
                                metadata$site_name,
                                ifelse(is.na(metadata$md_height),
                                       "",
                                       paste0("height = ", metadata$md_height, "m")),
                                ifelse(is.na(metadata$md_orientation),
                                       "",
                                       paste0("orientation = ", metadata$md_orientation, "\u00B0")),
                                ifelse(is.na(metadata$md_tilt),
                                       "",
                                       paste0("tilt = ", metadata$md_tilt, "\u00B0"))
                   )
    )

  } else {
    message("No modifications done.")
  }

}
