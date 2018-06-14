library(shiny)
#library(pool)
library(dplyr)
library(ggplot2)
library(cowplot)
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

con <- dbConnect_klimageo(user = "klimageo_6")

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
  mutate(site_name   = factor(if_else(site_name == "Adlershof_Garden", garden_label, roof_label),
                              levels = c(garden_label, roof_label)))


dbDisconnect(con)

round_unit <- c("hours", "days", "weeks")

ui <- fluidPage(

  # Application title
  titlePanel(titlePanel_label),

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
           dateRangeInput("dateRange", label = dateRange_label,
                          start = Sys.Date()-7, end = Sys.Date(),
                          min = "2017-12-23", max = Sys.Date(),
                          language = dateRange_language, weekstart = 1,
                          separator = dateRange_separator),
           radioButtons("kind", label = kind_label,
                        choices = kind_choices,
                        selected = 1)
    ),
    column(3,
           radioButtons("averaging", label = averaging_label,
                        choices = averaging_choices,
                        selected = "original")
    )
  ),

  #uiOutput("plot.ui")
  plotOutput("plot", height = 800)
)


server <- function(input, output) {

  plot_pq <- function(pq, line = TRUE) {
    p <- ggplot(filter(data_statistics(), pq_name == pq),
                aes(x = stadl_datetime, y = stadl_value, color = site_name)) +
      labs(x = NULL, y = plot_y_label[[pq]], color = NULL) +
      theme_light() +
      theme(axis.title.x = element_text(size = 18),
            axis.title.y = element_text(size = 18, margin = margin(t = 0, r = 12, b = 0, l = 0)),
            axis.text = element_text(size = 16),
            #legend.title = element_text(size = 20, margin = margin(r = 30, unit = "pt")),
            # margin is only supported with ggplot2 >= 2.3
            legend.text = element_text(size = 18, margin = margin(r = 20, unit = "pt")),
            legend.position = "bottom")
    if (line) {
      p <- p + geom_line()
    } else {
      p <- p + geom_point(size = 0.8)
    }
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
    list(temperature = plot_pq("air_temperature"),
         relativehumidity = plot_pq("relative_humidity"),
         windspeed = plot_pq("wind_speed"),
         winddirection = plot_pq("wind_from_direction", line = FALSE),
         shortwaveincoming = plot_pq("surface_downwelling_shortwave_flux_in_air"),
         shortwaveoutgoing = plot_pq("surface_upwelling_shortwave_flux_in_air"),
         longwaveincoming = plot_pq("surface_downwelling_longwave_flux_in_air"),
         longwaveoutgoing = plot_pq("surface_upwelling_longwave_flux_in_air")
    )
  })

  output$plot <- renderPlot({

    if (length(input$measurands > 0)) {
      # list of plots to show
      plot_grid_args <- plots()[as.integer(input$measurands)]
      # remove legend
      plot_grid_args <- lapply(plot_grid_args, function(p) {p + theme(legend.position = "none")})
      # more arguments for plot_grid
      plot_grid_args[["ncol"]] <- 1
      plot_grid_args[["align"]] <- "v"
      # all plots without legend
      grid_plots <- do.call(plot_grid, plot_grid_args)

      # get legend from first plot (has both sites), not important if it is
      # shown or not
      legend_plots <- get_legend(plots()[[1]])
      # add legend to plot
      plot_grid(legend_plots, grid_plots, ncol = 1, align = "v", rel_heights = c(.5, 10))
    }
  })

  #output$plot.ui <- renderUI({
  #  plotOutput("plot", height = 300*length(input$measurands))
  #})
}
