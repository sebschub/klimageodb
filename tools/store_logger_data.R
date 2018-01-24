args <- commandArgs(trailingOnly=TRUE)
if (length(args)!=2) stop(paste("Exactly two arguments required but got", length(args)))

md_name_exclude <- c(
  "Datetime",    # time is required for every other measurement
  "RecNbr"       # we don't need the logger measurement number
  #"GRain_m_Tot"  # rain sensor not installed yet
  )

library(klimageodb)
 
logger_data <- read.csv(file = args[1])
# logger_data <- read.csv(file = "test3.csv")
logger_data$Datetime <- as.POSIXct(logger_data$Datetime, tz = "GMT")


con <- dbConnect_klimageo(user = "klimageo_1")

for (icol in seq_along(logger_data)) {

  md_name <- names(logger_data)[icol]

  # skip some columns
  if (md_name %in% md_name_exclude) next

  # try to add the data; errors are expected for not defined measurands
  tryCatch({
    dbAdd_station_adlershof(conn = con,
                            md_name = md_name,
                            stadl_datetime = logger_data$Datetime,
                            stadl_value = logger_data[[icol]])
  }, error = function(e) {
    message(paste0(md_name, ": ", e$message))
  })
}

dbDisconnect(con)


# sort (probably not required) and get latest data
logger_data_sorted <- logger_data[order(logger_data$Datetime), ]
display_line <- logger_data_sorted[nrow(logger_data_sorted), ]
display_df <- data.frame(
  Date = strftime(display_line$Datetime, format="%d.%m.%Y", tz = "Europe/Berlin"),
  Time = strftime(display_line$Datetime, format="%H:%M", tz = "Europe/Berlin"),
  uSec = NA,
  "GLOBAL 8135 (W/m2 (Ave))"  = max(0., round(display_line$GCM3Up_Avg, 0)), # incoming shortwave
  "HFP01 00640 (W/m2 (Ave))"  = NA,
  "HFP01 00639 (W/m2 (Ave))"  = NA,
  "ML2X-1 (% (Ave))"          = round(display_line$GWS_ms_S_WVT, 1), # wind speed
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
  "RM826-02 (mm)"             = round(display_line$GRain_mm_Tot, 1), # precipitation
  "RFT2 Feuchte (% (Ave))"    = round(display_line$GRH_2*100, 0), # relative humidity
  "RFT2 Temp ( C (Ave))"      = round(display_line$GAirTC_2_Avg, 1), # 2m temperature
  stringsAsFactors = FALSE
)

filecon <- file(args[2], open = "wt")
writeLines(
  "Date,Time,uSec,GLOBAL 8135 (W/m2 (Ave)),HFP01 00640 (W/m2 (Ave)),HFP01 00639 (W/m2 (Ave)),ML2X-1 (% (Ave)),ML2X-2 (% (Ave)),ML2X-3 (% (Ave)),ML2X-4 (% (Ave)),TH2-1 (degC (Ave)),TH2-2 (degC (Ave)),TH2-3 (degC (Ave)),TH2-4 (degC (Ave)),TH2-6 (degC (Ave)),Bilanz oben (W/m2 (Ave)),Bilanz unten (W/m2 (Ave)),Bilanz Temp (degC (Ave)),RM826-02 (mm),RFT2 Feuchte (% (Ave)),RFT2 Temp ( C (Ave))",
  con = filecon)
write.table(display_df, file = filecon, quote = FALSE, sep = ",", dec = ".", row.names = FALSE, col.names = FALSE)
close(filecon)

