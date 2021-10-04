library(shiny)
library(sortable)
library(tidyverse)
library(spsComps)
library(shinythemes)

`%||%` <- function(a, b) if (is.null(a)) b else a

# UI ----------------------------------------------------------------------

ui <- fluidPage(theme= shinytheme("yeti"),

    tags$link(rel = "stylesheet", type="text/css", href="www/style.css"),

    fluidRow(

        column(4,
            h3("Wells"),
            div(style = 'width:100%;overflow-x: scroll;height:90vh;overflow-y: scroll;',
            uiOutput("well_list"))
         ),

        column(8,
            h3("Conditions"),

            fluidRow(
                column(3,
                    selectizeInput("whichcondition",
                                   label = NULL,
                                   choices = NULL,
                                   )
                    ),
                column(5,
                    textButton(
                      textId = "condition_name",
                      label = "",
                      btn_label = "Submit",
                      placeholder = "New name",
                      class = "btn-primary")
                  ),

                column(2,
                    actionButton("add_condition", "+", class = "btn-primary")),

                column(2,
                    actionButton("del_condition", "Del"))



            ),

            fluidRow(
              column(2,
                     actionButton("save_condition", "Save")),
              column(2,
                     actionButton("load_condition", "Load"))
            ),

            fluidRow(
              column(6, uiOutput("replicates")),
              column(6, uiOutput("blanks")),
            ),
            fluidRow(actionButton("close", "Validate & Close"))
        )
    )
  )


# Server  -----------------------------------------------------------------

server <- function(input, output, session) {

    rvs <- reactiveValues(conditions = list())

    observeEvent(input$add_condition, {

      new_condition <- ids::adjective_animal(max_len = 15)
      rvs$conditions[[new_condition]] <- list(replicates = character(), blanks = character())

      updateSelectizeInput(session, 'whichcondition',
                           label    = NULL,
                           choices  = names(rvs$conditions),
                           server   = TRUE,
                           selected = new_condition)
    })

    observeEvent(input$save_condition, {

      downloadHandler(
        filename = function() {
          paste("Test.Rds", sep = "")
        },
        content = function(file) {
          saveRDS(rvs$conditions, file)
        })

      #saveRDS(rvs$conditions, 'C:/Users/Thomas/your_list.Rds')
    })

    observeEvent(input$load_condition, {

      rvs$conditions <<-readRDS('C:/Users/Thomas/your_list.Rds')

      updateSelectizeInput(session, 'whichcondition',
                           label    = NULL,
                           choices  = names(rvs$conditions),
                           server   = TRUE,
                           selected = names(rvs$conditions)[1])
    })



    observeEvent(input$del_condition, {

        current_condition <- input$whichcondition

        rvs$conditions[[current_condition]] <- NULL

        selection <- names(rvs$conditions)[length(rvs$conditions)]

        updateSelectizeInput(session, 'whichcondition',
                             label    = NULL,
                             choices  = names(rvs$conditions),
                             server   = TRUE,
                             selected = selection)
      })

    observeEvent(input$condition_name_btn, {

      current_condition <- input$whichcondition
      new_name <- input$condition_name


      rvs$conditions[[new_name]] <<- rvs$conditions[[current_condition]]
      rvs$conditions[[current_condition]] <- NULL

      updateSelectizeInput(session, 'whichcondition',
                           label    = NULL,
                           choices  = names(rvs$conditions),
                           server   = TRUE,
                           selected = new_name)

    })


    observeEvent(input$close, {stopApp(rvs$conditions)})


    output$well_list <- renderUI({

      rank_list(text     = NULL,
                labels   = wells,
                input_id = "well_var",
                options  = sortable_options(group = list(name = "sort_conditions_group",
                                                         put   = FALSE,
                                                         pull  = "clone"),
                                            multiDrag = TRUE))
    })



    output$replicates <- renderUI({

      req(input$whichcondition)

        rank_list(text     = "Replicates",
                  labels   = rvs$conditions[[input$whichcondition]][["replicates"]],
                  input_id = input$whichcondition,
                  options  = sortable_options(group = list(name = "sort_conditions_group",
                                                           put  = TRUE,
                                                           pull = TRUE),
                                              multiDrag = TRUE
                                              ))

    })

    if (allow_blanks == TRUE) {


    output$blanks <- renderUI({

      req(input$whichcondition)

        id <- paste(input$whichcondition, "blank", sep = "_")

        rank_list(text     = "Blanks",
                  labels   = rvs$conditions[[input$whichcondition]][["blanks"]],
                  input_id = id,
                  options  = sortable_options(group = list(name = "sort_conditions_group",
                                                           put  = TRUE,
                                                           pull = TRUE),
                                              multiDrag = TRUE))
    })

    }

    observe({

      names <- names(rvs$conditions)

      purrr::walk2(names, paste(names, "blank", sep = "_"),
                   \(x, y) {rvs$conditions[[x]]$replicates <<- unique(input[[x]]) %||% rvs$conditions[[x]]$replicates
                            rvs$conditions[[x]]$blanks     <<- unique(input[[y]]) %||% rvs$conditions[[x]]$blanks })

    }, priority = 2)

}

# Run App -----------------------------------------------------------------

conditions_sorter <- list(ui = ui, server = server)


run_conditions_sorter <- function(wells = c(outer(LETTERS[1:8], 1:12, "paste0")),
                                  allow_blanks = TRUE) {
  server_env <- environment(server)
  server_env$wells <- wells
  server_env$allow_blanks <- allow_blanks
  return(runApp(conditions_sorter))
}

