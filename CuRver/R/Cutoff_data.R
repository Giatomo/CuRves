library(shiny)
library(sortable)
library(tidyverse)
library(spsComps)
library(shinythemes)
library(keys)

hotkeys <- c("up", "down")


`%||%` <- function(a, b) if (is.null(a)) b else a

# UI ----------------------------------------------------------------------

ui <- fluidPage(theme= shinytheme("yeti"),

                tags$link(rel = "stylesheet", type="text/css", href="www/style.css"),
                useKeys(),
                keysInput("keys", hotkeys),

                fluidRow(column(12,
                              uiOutput("well_selector")

                           ),

                         fluidRow(
                           column(12,
                           plotOutput("plot",
                                      dblclick = "cutoff_click",
                                      brush = brushOpts("cutoff_brush",
                                                        direction = "x",
                                                        resetOnNew = TRUE,
                                                        fill = "#3DB2FF",
                                                        opacity = .15)),
                           actionButton("close", "Validate & Close")
                           )
                         )

                )
)


# Server  -----------------------------------------------------------------

server <- function(input, output, session) {

  all_wells <- unique(dataframe$well)
  max_time <- max(dataframe$Time)

  rvs <- reactiveValues(wells = all_wells |>
                          purrr::set_names() |>
                          purrr::map(\(x) list(min = 0, max =  max_time)))

  selected <- reactiveValues(current = 1)

  output$well_selector <- renderUI({

    selectizeInput("whichwell",
                   label = "Choose" ,
                   choices = all_wells,
                   selected = all_wells[selected$current])

  })

  observeEvent(input$keys, {

    if (input$keys == "down") {

      if (selected$current == length(all_wells)) {
        selected$current <- 1
      }

      else {
        selected$current <- selected$current + 1
      }

    }
    else if (input$keys == "up") {
      if (selected$current == 1) {
        selected$current = length(all_wells)
      }
      else {
        selected$current   <- selected$current - 1
      }
    }
  })

  observeEvent(input$close, {stopApp(rvs$wells)})


  current_data <- reactive({

    dataframe |>
      filter(well == input$whichwell)

  })


  observeEvent(input$which_well,{
    if (is.null(rvs$wells[[input$whichwell]][["min"]])){

      rvs$wells[[input$whichwell]][["min"]] <- 0
      rvs$wells[[input$whichwell]][["min"]] <- max_time
    }

  })

  observeEvent(input$cutoff_brush,{

    rvs$wells[[input$whichwell]][["min"]] <- input$cutoff_brush[["xmin"]]
    rvs$wells[[input$whichwell]][["max"]] <- input$cutoff_brush[["xmax"]]

    })

  observeEvent(input$cutoff_click,{

    rvs$wells[[input$whichwell]][["min"]] <- 0
    rvs$wells[[input$whichwell]][["max"]] <- input$cutoff_click[["x"]]

  })

  output$plot <- renderPlot({

    req(input$whichwell)

    max_value <- max(current_data()$value)

    current_data() |>
      ggplot(aes(x = Time, y = value)) +
      annotate("rect",
        ymin = 0,
        ymax = max_value + 0.2 * max_value,
        xmin = (rvs$wells[[input$whichwell]][["min"]] %||% 0),
        xmax = (rvs$wells[[input$whichwell]][["max"]] %||% 0),
        alpha = 0.15,
        fill = "#3DB2FF") +
      geom_line(color = "#F9B208", size = 1) +
      geom_point(color = "white",fill = "#F98404",  size = 3.5, shape = 23) +
      geom_vline(xintercept = rvs$wells[[input$whichwell]][["min"]] %||% 0,
                 size = 1, linetype = "dashed") +
      geom_vline(xintercept = rvs$wells[[input$whichwell]][["max"]] %||% 0,
                 size = 1, linetype = "dashed") +
      expand_limits(x = 0, y = 0) +
      scale_x_continuous(expand = c(0, 0),
                         breaks = seq(0, max_time, by = 2)) +
      scale_y_continuous(expand = c(0, 0)) +
      theme_minimal() +
      theme(text = element_text(size=20))



})

}

# Run App -----------------------------------------------------------------

time_cutoff <- list(ui = ui, server = server)


run_time_cutoff <- function(dataframe) {
  server_env <- environment(server)
  server_env$dataframe <- dataframe

  return(runApp(time_cutoff))
}

