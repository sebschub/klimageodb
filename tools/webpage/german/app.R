measurand_label <- list("2m Lufttemperatur (T2m)" = 1,
                        "Relative Feuchte (RF)" = 2,
                        "Windgeschwindigkeit (WG)" = 3,
                        "Windrichtung (WR)" = 4,
                        "Globalstrahlung (KW↓)" = 5,
                        "Reflektierte Solarstrahlung (KW↑)" = 6,
                        "Atmosphärische Gegenstrahlung (LW↓)" = 7,
                        "Refl. und emittierte langwellige Str. (LW↑)" = 8)

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

garden_label <- "Messgarten"
roof_label <- "Institutsdach"

plot_y_label <- list("air_temperature" = "T2m (°C)",
                     "relative_humidity" = "RF (%)",
                     "wind_speed" = "WG (m/s)",
                     "wind_from_direction" = "WR (°)",
                     "surface_downwelling_shortwave_flux_in_air" = "KW↓ (W/m²)",
                     "surface_upwelling_shortwave_flux_in_air" = "KW↑ (W/m²)",
                     "surface_downwelling_longwave_flux_in_air" = "LW↓ (W/m²)",
                     "surface_upwelling_longwave_flux_in_air" = "LW↑ (W/m²)")

source("../app_template.R", local = TRUE)

# needs to be in this file
shinyApp(ui = ui, server = server)
