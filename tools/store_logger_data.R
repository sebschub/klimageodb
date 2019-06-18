args <- commandArgs(trailingOnly=TRUE)
if (length(args) != 3) stop(paste("Exactly three arguments required but got", length(args)))

md_name_exclude <- c(
  "Datetime",    # time is required for every other measurement
  "RecNbr",      # we don't need the logger measurement number
  "RRain_mm_Tot" # roof rain sensor not working
  )

library(klimageodb)
con <- dbConnect_klimageo(user = "klimageo_1")

logger_data <- list()
display_line <- list()

# read all stations
station_names <- c("garden", "roof")
for (station_index in seq_along(station_names)) {
  
  station_name <- station_names[station_index]
  logger_data[[station_name]] <- read.csv(file = args[station_index])

  logger_data[[station_name]]$Datetime <- as.POSIXct(logger_data[[station_name]]$Datetime, tz = "GMT")

  for (icol in seq_along(logger_data[[station_name]])) {

    md_name <- names(logger_data[[station_name]])[icol]
    
    # skip some columns
    if (md_name %in% md_name_exclude) next
    
    # try to add the data; errors are expected for not defined measurands
    tryCatch({
      dbAdd_station_adlershof(conn = con,
                              md_name = md_name,
                              stadl_datetime = logger_data[[station_name]]$Datetime,
                              stadl_value = logger_data[[station_name]][[icol]])
    }, error = function(e) {
      message(paste0(md_name, ": ", e$message))
    })
  }

  # sort (probably not required) and get latest data
  logger_data_sorted <- logger_data[[station_name]][order(logger_data[[station_name]]$Datetime), ]
  display_line[[station_name]] <- logger_data_sorted[nrow(logger_data_sorted), ]
}

# get md_id of precipitation
md_id_precip <- dbGetQuery(con, paste("SELECT md_id FROM measurand_detail",
                                      "WHERE pq_name = 'rainfall_amount' AND site_name = 'Adlershof_Garden'",
                                      "ORDER BY md_setup_datetime DESC LIMIT 1;"))[1,1]

# time 12 hours before latest entry here
time_start <- display_line[["garden"]]$Datetime - 60*60*12

# sum up values in database
precip_sum <- dbGetQuery(con, paste0("SELECT sum(stadl_value) FROM station_adlershof ",
                                     "WHERE md_id = ", md_id_precip, " AND stadl_datetime > '",
                                     strftime(time_start, tz = "GMT"), " +00:00';"))[1,1]

# UV index
tryCatch({
  uv_df <- read.csv2(Sys.getenv("URL_UVINDEX"), header=FALSE, stringsAsFactors = FALSE)
  dbAdd_station_adlershof(conn = con,
                          md_name = "UV_sglux_A",
                          stadl_datetime = as.POSIXct(paste(uv_df$V1, uv_df$V2), tz = "Europe/Berlin"),
                          stadl_value = as.numeric(uv_df$V3))
}, error = function(e) {
  message(paste0("UV_sglux_A: ", e$message))
  uv_df <<- data.frame(V3 = NA)
})
dbDisconnect(con)


# data for board

# height corrected pressure with measurement height of 56m
lapse_rate <- 0.0065
height_pressure <- 56
pressure_corrected <- display_line[["roof"]]$RBP_mbar_Avg * 
  (1. - lapse_rate * height_pressure / (display_line[["roof"]]$RAirTK_Avg + lapse_rate * height_pressure))^-5.257

display_df <- data.frame(
  Date = strftime(display_line[["garden"]]$Datetime, format="%d.%m.%Y", tz = "Europe/Berlin"),
  Time = strftime(display_line[["garden"]]$Datetime, format="%H:%M", tz = "Europe/Berlin"),
  uSec = NA,
  "GLOBAL 8135 (W/m2 (Ave))"  = max(0., round(display_line[["roof"]]$RSR01Up1_Avg, 0)), # incoming shortwave roof
  "HFP01 00640 (W/m2 (Ave))"  = as.numeric(uv_df$V3), # UV index
  "HFP01 00639 (W/m2 (Ave))"  = round(display_line[["roof"]]$RIR01UpCo1_Avg, 0), # incoming longwave roof
  "ML2X-1 (% (Ave))"          = round(display_line[["roof"]]$RWS_ms_S_WVT, 1), # wind speed roof
  "ML2X-2 (% (Ave))"          = round(pressure_corrected, 0), # pressure roof
  "ML2X-3 (% (Ave))"          = NA,
  "ML2X-4 (% (Ave))"          = NA,
  "TH2-1 (degC (Ave))"        = NA,
  "TH2-2 (degC (Ave))"        = NA,
  "TH2-3 (degC (Ave))"        = NA,
  "TH2-4 (degC (Ave))"        = NA,
  "TH2-6 (degC (Ave))"        = NA,
  "Bilanz oben (W/m2 (Ave))"  = NA,
  "Bilanz unten (W/m2 (Ave))" = NA,
  "Bilanz Temp (degC (Ave))"  = NA,
  "RM826-02 (mm)"             = round(precip_sum, 1), # precipitation roof
  "RFT2 Feuchte (% (Ave))"    = round(display_line[["garden"]]$GRH_2*100, 0), # relative humidity garden
  "RFT2 Temp ( C (Ave))"      = round(display_line[["garden"]]$GAirTC_2_Avg, 1), # 2m temperature garden
  stringsAsFactors = FALSE
)

filecon <- file(args[3], open = "wt")
writeLines(
  "Date,Time,uSec,GLOBAL 8135 (W/m2 (Ave)),HFP01 00640 (W/m2 (Ave)),HFP01 00639 (W/m2 (Ave)),ML2X-1 (% (Ave)),ML2X-2 (% (Ave)),ML2X-3 (% (Ave)),ML2X-4 (% (Ave)),TH2-1 (degC (Ave)),TH2-2 (degC (Ave)),TH2-3 (degC (Ave)),TH2-4 (degC (Ave)),TH2-6 (degC (Ave)),Bilanz oben (W/m2 (Ave)),Bilanz unten (W/m2 (Ave)),Bilanz Temp (degC (Ave)),RM826-02 (mm),RFT2 Feuchte (% (Ave)),RFT2 Temp ( C (Ave))",
  con = filecon)
write.table(display_df, file = filecon, quote = FALSE, sep = ",", dec = ".", row.names = FALSE, col.names = FALSE)
close(filecon)


# data for webpage as small html
library(xtable)

# webpage parts
names_lst <- list(german = c("Lufttemperatur", 
                             "Windgeschwindigkeit", 
                             "Relative Feuchte", 
                             "Luftdruck", 
                             "Einfallende Solarstrahlung", 
                             "Atmosphärische Gegenstrahlung",
                             "UV-Index"), 
                  english = c("Air temperature", 
                              "Wind velocity", 
                              "Relative Humidity", 
                              "Air pressure", 
                              "Incoming solar radiation", 
                              "Incoming longwave radiation",
                              "UV index")
)

webpage_df <- data.frame(
  value = c(display_line[["garden"]]$GAirTC_2_Avg,
            display_line[["roof"]]$RWS_ms_S_WVT,
            display_line[["garden"]]$GRH_2*100,
            pressure_corrected,
            max(0., display_line[["roof"]]$RSR01Up1_Avg),
            display_line[["roof"]]$RIR01UpCo1_Avg,
            as.numeric(uv_df$V3)
  ), 
  unit = c("°C", "m/s", "%", "hPa", "W/m²", "W/m²", "")
)

html_head_lst <- list(german = paste("<!DOCTYPE html>", 
                                     "<html lang=\"de\">", 
                                     "<head>", 
                                     "<meta charset=\"utf-8\" />",
                                     "<title>Wetter Adlershof</title>",
                                     "</head>",
                                     "<body style=\"font-family: Verdana,Helvetica,Arial; font-size: 0.8em;\">", 
                                     sep="\n"),
                      english = paste("<!DOCTYPE html>", 
                                      "<html lang=\"de\">", 
                                      "<head>", 
                                      "<meta charset=\"utf-8\" />",
                                      "<title>Weather Adlershof</title>",
                                      "</head>",
                                      "<body style=\"font-family: Verdana,Helvetica,Arial; font-size: 0.8em;\">", 
                                      sep="\n")
)
                      
html_intro_lst <- list(german = paste("Messstation Adlershof</br>Stand:", 
                                      strftime(display_line[["garden"]]$Datetime, format="%d.%m.%Y %H:%M", tz = "Europe/Berlin")),
                       english = paste("Station Adlershof</br>Stand:", 
                                       strftime(display_line[["garden"]]$Datetime, format="%d/%m/%Y %H:%M", tz = "Europe/Berlin"))
)

html_tail <- paste("</body>", "</html>", sep="\n")

## the html files
for (lang in c("german", "english")) {
  html_table <- paste(
    print(
      xtable(cbind(names_lst[[lang]], webpage_df), 
             digits = matrix(c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 1, 1, 0, 0, 0, 0, 1, NA, NA, NA, NA, NA, NA, NA), ncol = 4)
      ), 
      "html", include.rownames=FALSE, include.colnames = FALSE,
      html.table.attributes=""), 
    collapse = "\n")
  
  write(paste(html_head_lst[[lang]], html_intro_lst[[lang]], html_table, html_tail, sep="\n"), 
        paste0("/var/www/html/aws/aws_", lang, ".html")
  )
}
