library(shiny)
#library(pool)
library(dplyr)
library(ggplot2)
library(grid)
library(lubridate)
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
          site_name == "Adlershof_Garden" &
          md_height == 2.) |
         (pq_name == "wind_speed" &
          site_name == "Adlershof_Garden" &
          md_height == 2.) |
         (pq_name == "relative_humidity" &
          site_name == "Adlershof_Garden" &
          md_height == 2.) |
         (pq_name == "surface_downwelling_shortwave_flux_in_air" &
          site_name == "Adlershof_Garden" &
          md_height == 2.) |
         (pq_name == "surface_downwelling_longwave_flux_in_air" &
          site_name == "Adlershof_Garden" &
          md_height == 2.)
           ) %>% collect()

# get whole data (CHANGE THIS LATER IF IT IS TOO MUCH)
# and apply some modifications for display
data_complete <- tbl(con, "station_adlershof_corrected") %>%
  filter(md_id %in% measurand$md_id) %>% collect() %>%
  left_join(measurand, by = "md_id") %>%
  mutate(stadl_value = if_else(pq_name == "air_temperature", stadl_value - 273.15, stadl_value)) %>%
  mutate(stadl_value = if_else(pq_name == "relative_humidity", stadl_value * 100,  stadl_value))


dbDisconnect(con)


measurand_label <- list("Lufttemperatur" = 1,
                        "Relative Feuchte" = 2,
                        "Windgeschwindigkeit" = 3,
                        "Globalstrahlung" = 4,
                        "Atmosphärische Gegenstrahlung" = 5)

round_unit <- c("hours", "days", "weeks")

ui <- fluidPage(

  # Application title
  titlePanel("Messstation der Klimageographie in Adlershof"),

  fluidRow(
    shinyjs::useShinyjs(),
    column(3,
           checkboxGroupInput("measurands", label = "Messgrößen",
                              choices = measurand_label,
                              selected = 1)
    ),
    column(4,
           dateRangeInput("dateRange", label = "Darstellungszeitraum",
                          start = "2018-01-16", end = Sys.Date(),
                          min = "2018-01-16", max = Sys.Date(),
                          language = 'de', weekstart = 1,
                          separator = " bis ")
    ),
    column(3,
      radioButtons("kind", label = "Darstellungsart",
                   choices = list("Zeitreihe" = 1, "mittlerer Tagesgang" = 2),
                   selected = 1)
    ),
    column(2,
           radioButtons("averaging", label = "Mittlung der Zeitreihe",
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
        group_by(pq_name, stadl_datetime) %>%
        summarise(stadl_value = mean(stadl_value, na.rm = TRUE))
    } else {
      shinyjs::enable("averaging")
      if (input$averaging != "original") {
        data_range() %>%
          mutate(stadl_datetime = floor_date(stadl_datetime, unit = input$averaging)) %>%
          group_by(pq_name, stadl_datetime) %>%
          summarise(stadl_value = mean(stadl_value, na.rm = TRUE))
      } else {
        data_range()
      }
    }
  })

  plots <- reactive({
    list(temperature =
           ggplot(filter(data_statistics(), pq_name == "air_temperature"),
                  aes(x = stadl_datetime, y = stadl_value)) +
           geom_point() + geom_line() +
           labs(x = "Datum/Uhrzeit", y = "Lufttemperatur (°C)") +
           theme_light() +
           theme(axis.title = element_text(size = 18),
                 axis.text = element_text(size = 16)),
         relativehumidity =
           ggplot(filter(data_statistics(), pq_name == "relative_humidity"),
                  aes(x = stadl_datetime, y = stadl_value)) +
           geom_point() + geom_line() +
           labs(x = "Datum/Uhrzeit", y = "Relative Feuchte (%)") +
           theme_light() +
           theme(axis.title = element_text(size = 18),
                 axis.text = element_text(size = 16)),
         windspeed =
           ggplot(filter(data_statistics(), pq_name == "wind_speed"),
                  aes(x = stadl_datetime, y = stadl_value)) +
           geom_point() + geom_line() +
           labs(x = "Datum/Uhrzeit", y = "Windgeschwindigkeit (m/s)") +
           theme_light() +
           theme(axis.title = element_text(size = 18),
                 axis.text = element_text(size = 16))
         ,
         shortwaveincoming =
           ggplot(filter(data_statistics(), pq_name == "surface_downwelling_shortwave_flux_in_air"),
                  aes(x = stadl_datetime, y = stadl_value)) +
           geom_point() + geom_line() +
           labs(x = "Datum/Uhrzeit", y = "Globalstrahlung (W/m²)") +
           theme_light() +
           theme(axis.title = element_text(size = 18),
                 axis.text = element_text(size = 16))
         ,
         longwaveincoming =
           ggplot(filter(data_statistics(), pq_name == "surface_downwelling_longwave_flux_in_air"),
                  aes(x = stadl_datetime, y = stadl_value)) +
           geom_point() + geom_line() +
           labs(x = "Datum/Uhrzeit", y = "Atmos. Gegenstrahlung (W/m²)") +
           theme_light() +
           theme(axis.title = element_text(size = 18),
                 axis.text = element_text(size = 16))

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

