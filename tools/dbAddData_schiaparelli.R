args <- commandArgs(trailingOnly=TRUE)
if (length(args) != 1) stop(paste("Exactly one argument required but got", length(args)))

library(klimageodb)

######### Add Data of Schiaparelli Glacier Station to Data base ###########



# Load data ---------------------------------------------------------------

# Get timestamp of last transferred observation (timestamp written in 'timestamp.txt')

tryCatch({
  timestamp_in <- as.POSIXct(
    read.table('/home/aws/schiaparelli/timestamp.txt', sep = ',')[1,1],
    tz = "UTC")
}, error = function(e) {
  message(e$message)
  timestamp_in <<- as.POSIXct("2000-01-01 00:00:00", tz = "UTC")
})

# Read in datafile

data <- read.csv(args[1], dec = ".", header = F, skip = 4)

header <- read.table(args[1], 
                     skip = 1, nrows = 1, 
                     header = FALSE, sep = ",",
                     stringsAsFactors = FALSE)

names(data) <- as.character(header)

data$TIMESTAMP <- as.POSIXct(data$TIMESTAMP, tz = "Etc/GMT+3")

# select only new observations

data <- data[data$TIMESTAMP > timestamp_in, ]

if (nrow(data) == 0) {
  stop("No new data found!")
}
 

# Add data table for Schiaparelli (Patagonia) station ---------------------

# Connect to Data base

con <- dbConnect_klimageo(user = "klimageo_2")

# Add Data

# Battery Voltage

dbAdd_station_patagonia(con, md_name = "BattV_Schiaparelli", 
                        stapa_datetime = data$TIMESTAMP,
                        stapa_value = data$BattV_Avg)

# Panel Temperature
# Calculate Kelvin from Deg Celsius

dbAdd_station_patagonia(con, md_name = "PTemp_Schiaparelli", 
                        stapa_datetime = data$TIMESTAMP,
                        stapa_value = data$PTemp_C + 273.15)

# Air Temperature
# Calculate Kelvin from Deg Celsius

dbAdd_station_patagonia(con, md_name = "AirT_Schiaparelli", 
                        stapa_datetime = data$TIMESTAMP,
                        stapa_value = data$AirTC + 273.15)

# Relative Humidity
# Calculate Value from percentage

dbAdd_station_patagonia(con, md_name = "RH_Schiaparelli", 
                        stapa_datetime = data$TIMESTAMP,
                        stapa_value = data$RH / 100)

# Solar Radiation

dbAdd_station_patagonia(con, md_name = "Slr_Schiaparelli", 
                        stapa_datetime = data$TIMESTAMP,
                        stapa_value = data$SlrW_Avg)

# Wind Vector

dbAdd_station_patagonia(con, md_name = "WDir_Schiaparelli", 
                        stapa_datetime = data$TIMESTAMP,
                        stapa_value = data$WDir)

# Wind Speed

dbAdd_station_patagonia(con, md_name = "WS_Schiaparelli", 
                        stapa_datetime = data$TIMESTAMP,
                        stapa_value = data$WS_ms)

# Precipitation

dbAdd_station_patagonia(con, md_name = "Rain_Schiaparelli", 
                        stapa_datetime = data$TIMESTAMP,
                        stapa_value = data$Rain_mm_Tot)

# Snow Height alias 
# Difference between base (ground) and temperature corrected distance to target (DBTCDT)

dbAdd_station_patagonia(con, md_name = "DBTCDT_Schiaparelli", 
                        stapa_datetime = data$TIMESTAMP,
                        stapa_value = data$DBTCDT_Avg)

# Disconnect from Data base

dbDisconnect(con)

# Write Timestamp of last transferred observation in separate file
timestamp_last <- as.character(data$TIMESTAMP[length(data$TIMESTAMP)], tz = "UTC")
write(timestamp_last, "/home/aws/schiaparelli/timestamp.txt", append = F)
