measurand_label <- list("2m Lufttemperatur (T2m)" = 1,
                        "Relative Feuchte (RF)" = 2,
                        "Windgeschwindigkeit (WG)" = 3,
                        "Windrichtung (WR)" = 4,
                        "Niederschlag (PP)" = 5,
                        "Luftdruck (LD)" = 6,
                        "Globalstrahlung (KW↓)" = 7,
                        "Reflektierte Solarstrahlung (KW↑)" = 8,
                        "Atmosphärische Gegenstrahlung (LW↓)" = 9,
                        "Emittierte und refl. langwellige Str. (LW↑)" = 10,
                        "UV-Index (UVI)" = 11)

titlePanel_label <- "Messstation der Klimageographie in Adlershof"

dateRange_label <- "Darstellungszeitraum"
dateRange_language <- "de"
dateRange_separator <- " bis "

kind_label <- "Darstellungsart"
kind_choices <- list("Zeitreihe" = 1, "mittlerer Tagesgang" = 2)

averaging_choices <- list("Original" = "original",
                     "Stunden" = "hours",
                     "Tage" = "days",
                     "Wochen" = "weeks"
)
averaging_label <- "Mittelung der Zeitreihe"

garden_label <- "Garten"
roof_label <- "Dach"

plot_y_label <- list("air_temperature" = "T2m (°C)",
                     "relative_humidity" = "RF (%)",
                     "wind_speed" = "WG (m/s)",
                     "wind_from_direction" = "WR (°)",
                     "rainfall_amount" = "PP (mm)",
                     "air_pressure" = "LD (hPa)",
                     "surface_downwelling_shortwave_flux_in_air" = "KW↓ (W/m²)",
                     "surface_upwelling_shortwave_flux_in_air" = "KW↑ (W/m²)",
                     "surface_downwelling_longwave_flux_in_air" = "LW↓ (W/m²)",
                     "surface_upwelling_longwave_flux_in_air" = "LW↑ (W/m²)",
                     "ultraviolet_index" = "UVI")

source("../app_template.R", local = TRUE)

# needs to be in this file
shinyApp(ui = ui, server = server)
