args <- commandArgs(trailingOnly=TRUE)
if (length(args) != 3) stop(paste("Exactly three arguments required but got", length(args)))

md_name_exclude <- c(
  "Datetime",    # time is required for every other measurement
  "RecNbr"       # we don't need the logger measurement number
  #"GRain_m_Tot"  # rain sensor not installed yet
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

# time one hour before latest entry here
time_1h_before <- display_line[["garden"]]$Datetime - 60*60

# sum up values in database
precip_sum <- dbGetQuery(con, paste0("SELECT sum(stadl_value) FROM station_adlershof ",
                                     "WHERE md_id = ", md_id_precip, " AND stadl_datetime > '",
                                     strftime(time_1h_before, tz = "GMT"), " +00:00';"))[1,1]

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



display_df <- data.frame(
  Date = strftime(display_line[["garden"]]$Datetime, format="%d.%m.%Y", tz = "Europe/Berlin"),
  Time = strftime(display_line[["garden"]]$Datetime, format="%H:%M", tz = "Europe/Berlin"),
  uSec = NA,
  "GLOBAL 8135 (W/m2 (Ave))"  = max(0., round(display_line[["garden"]]$GCM3Up_Avg, 0)), # incoming shortwave
  "HFP01 00640 (W/m2 (Ave))"  = as.numeric(uv_df$V3), # UV index
  "HFP01 00639 (W/m2 (Ave))"  = round(display_line[["garden"]]$GCG3UpCo_Avg, 0), # incoming longwave
  "ML2X-1 (% (Ave))"          = round(display_line[["garden"]]$GWS_ms_S_WVT, 1), # wind speed
  "ML2X-2 (% (Ave))"          = NA,
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
  "RM826-02 (mm)"             = round(precip_sum, 1), # precipitation
  "RFT2 Feuchte (% (Ave))"    = round(display_line[["garden"]]$GRH_2*100, 0), # relative humidity
  "RFT2 Temp ( C (Ave))"      = round(display_line[["garden"]]$GAirTC_2_Avg, 1), # 2m temperature
  stringsAsFactors = FALSE
)

filecon <- file(args[3], open = "wt")
writeLines(
  "Date,Time,uSec,GLOBAL 8135 (W/m2 (Ave)),HFP01 00640 (W/m2 (Ave)),HFP01 00639 (W/m2 (Ave)),ML2X-1 (% (Ave)),ML2X-2 (% (Ave)),ML2X-3 (% (Ave)),ML2X-4 (% (Ave)),TH2-1 (degC (Ave)),TH2-2 (degC (Ave)),TH2-3 (degC (Ave)),TH2-4 (degC (Ave)),TH2-6 (degC (Ave)),Bilanz oben (W/m2 (Ave)),Bilanz unten (W/m2 (Ave)),Bilanz Temp (degC (Ave)),RM826-02 (mm),RFT2 Feuchte (% (Ave)),RFT2 Temp ( C (Ave))",
  con = filecon)
write.table(display_df, file = filecon, quote = FALSE, sep = ",", dec = ".", row.names = FALSE, col.names = FALSE)
close(filecon)

