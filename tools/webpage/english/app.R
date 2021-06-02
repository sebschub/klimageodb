measurand_label <- list("2m air temperatur (T2m)" = 1,
                        "Relative humidity (RH)" = 2,
                        "Wind speed (WS)" = 3,
                        "Wind direction (WD)" = 4,
                        "Precipitation (PP)" = 5,
                        "Air pressure 56m a.s.l. (AP)" = 6,
                        "Incoming shortwave radiation (KW↓)" = 7,
                        "Reflected shortwave radiation (KW↑)" = 8,
                        "Incoming longwave radiation (LW↓)" = 9,
                        "Emitted and refl. longwave rad. (LW↑)" = 10,
                        "UV index (UVI)" = 11)

titlePanel_label <- "Climate station of the Geography department in Adlershof"

dateRange_label <- "Period"
dateRange_language <- "en"
dateRange_separator <- " til "

kind_label <- "Type"
kind_choices <- list("Time series" = 1, "Average diurnal cycle" = 2)

averaging_choices <- list("10min values" = "original",
                     "Hours" = "hours",
                     "Days" = "days",
                     "Weeks" = "weeks"
)
averaging_label <- "Averaging"

garden_label <- "Garden"
roof_label <- "Roof"

plot_y_label <- list("air_temperature" = "T2m (°C)",
                     "relative_humidity" = "RH (%)",
                     "wind_speed" = "WS (m/s)",
                     "wind_from_direction" = "WD (°)",
                     "rainfall_amount" = "PP (mm)",
                     "air_pressure" = "AP (hPa)",
                     "surface_downwelling_shortwave_flux_in_air" = "KW↓ (W/m²)",
                     "surface_upwelling_shortwave_flux_in_air" = "KW↑ (W/m²)",
                     "surface_downwelling_longwave_flux_in_air" = "LW↓ (W/m²)",
                     "surface_upwelling_longwave_flux_in_air" = "LW↑ (W/m²)",
                     "ultraviolet_index" = "UVI")

source("../app_template.R", local = TRUE)

# needs to be in this file
shinyApp(ui = ui, server = server)
