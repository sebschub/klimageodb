measurand_label <- list("2m air temperatur (T2m)" = 1,
                        "Relative humidity (RH)" = 2,
                        "Wind speed (WS)" = 3,
                        "Wind direction (WD)" = 4,
                        "Incoming shortwave radiation (KW↓)" = 5,
                        "Reflected shortwave radiation (KW↑)" = 6,
                        "Incoming longwave radiation (LW↓)" = 7,
                        "Refl. and emitted longwave rad. (LW↑)" = 8)

titlePanel_label <- "Climate station of the Geography department in Adlershof"

dateRange_label <- "Period"
dateRange_language <- "en"
dateRange_separator <- " til "

kind_label <- "Type"
kind_choices <- list("Time series" = 1, "Average diurnal cycle" = 2)

averaging_choices <- list("Original" = "original",
                     "Hours" = "hours",
                     "Days" = "days",
                     "Weeks" = "weeks"
)
averaging_label <- "Averaging"

garden_label <- "Measurement garden"
roof_label <- "Institute's roof"

plot_y_label <- list("air_temperature" = "T2m (°C)",
                     "relative_humidity" = "RH (%)",
                     "wind_speed" = "WS (m/s)",
                     "wind_from_direction" = "WD (°)",
                     "surface_downwelling_shortwave_flux_in_air" = "KW↓ (W/m²)",
                     "surface_upwelling_shortwave_flux_in_air" = "KW↑ (W/m²)",
                     "surface_downwelling_longwave_flux_in_air" = "LW↓ (W/m²)",
                     "surface_upwelling_longwave_flux_in_air" = "LW↑ (W/m²)")

source("../app_template.R", local = TRUE)

# needs to be in this file
shinyApp(ui = ui, server = server)
