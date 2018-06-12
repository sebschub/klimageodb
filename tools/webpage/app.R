library(shiny)
#library(pool)
library(dplyr)
library(ggplot2)
library(grid)
library(lubridate)
library(circular)
library(klimageodb)

#pool <- dbPool(
#  RPostgres::Postgres(),
#  dbname = "klimageo",
#  host = "blobdb.cms.hu-berlin.de",
#  port = 5432,
#  user = "klimageo_1"
#)
#onStop(function() {
#  poolClose(pool)
#})

con <- dbConnect_klimageo(user = "klimageo_1")

# get md_id
measurand <- tbl(con, "measurand_detail") %>%
  filter((pq_name == "air_temperature" &
          site_name %in% c("Adlershof_Garden", "Adlershof_Roof") &
          md_height == 2.) |
         (pq_name == "wind_speed" &
          site_name %in% c("Adlershof_Garden", "Adlershof_Roof") ) |
         (pq_name == "wind_from_direction" &
          site_name %in% c("Adlershof_Garden", "Adlershof_Roof") ) |
         (pq_name == "relative_humidity" &
          site_name %in% c("Adlershof_Garden", "Adlershof_Roof") &
          md_height == 2.) |
         (pq_name == "surface_downwelling_shortwave_flux_in_air" &
          site_name %in% c("Adlershof_Garden", "Adlershof_Roof") &
          md_height == 2.) |
         (pq_name == "surface_upwelling_shortwave_flux_in_air" &
          site_name %in% c("Adlershof_Garden", "Adlershof_Roof") &
          md_height == 2.) |
         (pq_name == "surface_downwelling_longwave_flux_in_air" &
          site_name %in% c("Adlershof_Garden", "Adlershof_Roof") &
          md_height == 2.) |
         (pq_name == "surface_upwelling_longwave_flux_in_air" &
          site_name %in% c("Adlershof_Garden", "Adlershof_Roof") &
          md_height == 2.)
  ) %>% collect()

# get whole data (CHANGE THIS LATER IF IT IS TOO MUCH)
# and apply some modifications for display
data_complete <- tbl(con, "station_adlershof_corrected") %>%
  filter(md_id %in% measurand$md_id) %>% collect() %>%
  left_join(measurand, by = "md_id") %>%
  mutate(stadl_value = if_else(pq_name == "air_temperature", stadl_value - 273.15, stadl_value)) %>%
  mutate(stadl_value = if_else(pq_name == "relative_humidity", stadl_value * 100,  stadl_value)) %>%
  mutate(site_name   = factor(if_else(site_name == "Adlershof_Garden", "Messgarten",  "Institutsdach"),
                              levels = c("Messgarten",  "Institutsdach")))


dbDisconnect(con)


measurand_label <- list("2m Lufttemperatur (T2m)" = 1,
                        "Relative Feuchte (RF)" = 2,
                        "Windgeschwindigkeit (WG)" = 3,
                        "Windrichtung (WR)" = 4,
                        "Globalstrahlung (KW↓)" = 5,
                        "Reflektierte Solarstrahlung (KW↑)" = 6,
                        "Atmosphärische Gegenstrahlung (LW↓)" = 7,
                        "Refl. und emittierte langwellige Str. (LW↑)" = 8)

round_unit <- c("hours", "days", "weeks")

ui <- fluidPage(

  # Application title
  titlePanel("Messstation der Klimageographie in Adlershof"),

  fluidRow(
    # shinyjs::useShinyjs used for greying out in case of diurnal cycle
    shinyjs::useShinyjs(),
    column(6,
           # have the entries distributed into 2 columns
           tags$head(tags$style(HTML(
".multicol{font-size:12px;
height:auto;
-webkit-column-count: 2;
-moz-column-count: 2;
column-count: 2;
}
div.checkbox {margin-top: 0px;}"))),
           strong(p("Messgrößen")),
           tags$div(align = "left", 
                    class = "multicol",
                    checkboxGroupInput("measurands", label = NULL,
                                       choices = measurand_label,
                                       selected = c(1, 2, 3))
           )),
    column(3,
           dateRangeInput("dateRange", label = "Darstellungszeitraum",
                          start = Sys.Date()-7, end = Sys.Date(),
                          min = "2017-12-23", max = Sys.Date(),
                          language = 'de', weekstart = 1,
                          separator = " bis "),
           radioButtons("kind", label = "Darstellungsart",
                        choices = list("Zeitreihe" = 1, "mittlerer Tagesgang" = 2),
                        selected = 1)
    ),
    column(3,
           radioButtons("averaging", label = "Mittelung der Zeitreihe",
                        choices = list("Original" = "original",
                                       "Stunden" = "hours",
                                       "Tage" = "days",
                                       "Wochen" = "weeks"
                                       ),
                        selected = "original")
    )
  ),

  #uiOutput("plot.ui")
  plotOutput("plot", height = 800)
)


server <- function(input, output) {

  plot_pq <- function(pq, ylab, line = TRUE) {
    p <- ggplot(filter(data_statistics(), pq_name == pq),
                aes(x = stadl_datetime, y = stadl_value, color = site_name)) +
      #geom_point() +
      labs(x = "Datum/Uhrzeit", y = ylab, color = "Ort") +
      theme_light() +
      theme(axis.title.x = element_text(size = 18),
            axis.title.y = element_text(size = 18, margin = margin(t = 0, r = 12, b = 0, l = 0)),
            axis.text = element_text(size = 16),
            legend.title = element_text(size = 18),
            legend.text = element_text(size = 16))
    if (line) p <- p + geom_line()
    p
  }
  
  data_range <- reactive({
    data_complete %>%
      filter(stadl_datetime >= input$dateRange[1],
             stadl_datetime <= (input$dateRange[2] + 1))
  })

  data_statistics <- reactive({
    if (input$kind == 2) {
      shinyjs::disable("averaging")
      data_range() %>%
        mutate(stadl_datetime = hour(stadl_datetime)) %>%
        group_by(pq_name, site_name, stadl_datetime) %>%
        summarise(stadl_value = ifelse(
          any(pq_name == "wind_from_direction"),
          mean(circular(stadl_value, units = "degrees", modulo = "2pi"), na.rm = TRUE),
          mean(stadl_value, na.rm = TRUE))
        )
    } else {
      shinyjs::enable("averaging")
      if (input$averaging != "original") {
        data_range() %>%
          mutate(stadl_datetime = floor_date(stadl_datetime, unit = input$averaging)) %>%
          group_by(pq_name, site_name, stadl_datetime) %>%
          summarise(stadl_value = ifelse(
            any(pq_name == "wind_from_direction"),
            mean(circular(stadl_value, units = "degrees", modulo = "2pi"), na.rm = TRUE),
            mean(stadl_value, na.rm = TRUE))
          )
      } else {
        data_range()
      }
    }
  })

  plots <- reactive({
    list(temperature =
           plot_pq("air_temperature", "T2m (°C)")
         ,
         relativehumidity =
           plot_pq("relative_humidity", "RF (%)")
         ,
         windspeed =
           plot_pq("wind_speed", "WG (m/s)")
         ,
         winddirection =
           plot_pq("wind_from_direction", "WR (°)", line = FALSE)
         ,
         shortwaveincoming =
           plot_pq("surface_downwelling_shortwave_flux_in_air", "KW↓ (W/m²)")
         ,
         shortwaveoutgoing =
           plot_pq("surface_upwelling_shortwave_flux_in_air", "KW↑ (W/m²)")
         ,
         longwaveincoming =
           plot_pq("surface_downwelling_longwave_flux_in_air", "LW↓ (W/m²)")
         ,
         longwaveoutgoing =
           plot_pq("surface_upwelling_longwave_flux_in_air", "LW↑ (W/m²)")
    )
  })

  output$plot <- renderPlot({

    # show plots sith alignes x axis following https://gist.github.com/tomhopper/faa24797bb44addeba79
    grobs <- NULL
    for (m in seq_along(measurand_label)) {
      if (m %in% input$measurands) {
        if (is.null(grobs)) {
          grobs <- ggplotGrob(plots()[[m]])
        } else {
          grobs <- rbind(grobs, ggplotGrob(plots()[[m]]), size = "last")
        }
      }
    }
    grid.newpage()
    grid.draw(grobs)
  })

  #output$plot.ui <- renderUI({
  #  plotOutput("plot", height = 300*length(input$measurands))
  #})
}

shinyApp(ui = ui, server = server)

