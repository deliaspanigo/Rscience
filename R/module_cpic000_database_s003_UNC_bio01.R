
# # # 01) UI - Selection for 'database'
module_cpiC000_database_s03_UNC_bio01_ui <- function(id){
  ns <- shiny::NS(id)

  vector_opt <- c("Select one..." = "",
                  "01 - Ejer_13_03" = "Rscience_Ejer_13_03",
                  "02 - iris" = "iris")

  div(shinyjs::useShinyjs(), id = ns("input-panel"),

      # h2("Initial user election - database"),
      fluidRow(
        column(6,

               # # # R examples selector
               div(shinyjs::useShinyjs(), id = ns("input-example"),
                   selectInput(inputId = ns("file_UNC"),
                               label = "UNC - Bio01 - Excersice",
                               choices = vector_opt)

               ) # Div
        ),
        # # # Action buttons
        column(4, br(), br(), uiOutput(ns("action_buttons")))

      ), # End fluidRow
      br(), br(), br(),
      textOutput(ns("calling_help"))#,
      # # # Visualization for database
      #uiOutput(ns("show_all_database"))

  ) # End div
}


module_cpiC000_database_s03_UNC_bio01_ui2 <- function(id){
  ns <- shiny::NS(id)



  div(#shinyjs::useShinyjs(), id = ns("input-panel"),


      uiOutput(ns("show_all_database"))

  ) # End div
}


# # # 01) SERVER - Selection for 'database'
module_cpiC000_database_s03_UNC_bio01_server <- function(id, input_file_source){
  moduleServer(
    id,
    function(input, output, session) {


      # # # External information
      original_file_source <- reactive({
        input_file_source
      })

      # # # Initial objects and default values -------------------------------------------------
      # # Action buttons - default values
      action_button_load <- reactiveVal(FALSE)
      action_button_show <- reactiveVal(FALSE)

      # # Colours for actions buttons
      hardcorded_initial_color <- "orange" #"#F4A020"
      color_button_load <- reactiveVal(hardcorded_initial_color)


      database <- reactiveVal(NULL)



      # # # renderUI for action buttons ----------------------------------------
      output$action_buttons <- renderUI({

        ns <- shiny::NS(id)
        req(color_button_load())

        # # # Style for load button
        standard_style_button_load <- "color: white; background-color: _color_;width: 150px; height: 50px; font-size: 20px;"
        output_style_button_load <- gsub(pattern = "_color_",
                                         replacement = color_button_load(),
                                         x = standard_style_button_load)

        # # # Style for reset button
        output_style_button_reset <- "color: white; background-color: #F4A020;width: 150px; height: 50px; font-size: 20px;"

        # # # UI content
        div(
          fluidRow(
            column(2, actionButton(ns("action_load"), label = "LOAD", style = output_style_button_load)),
            column(5),
            column(2, actionButton(ns("action_reset_all"), "RESET ALL", style = output_style_button_reset))
          )
        )
      })



      control_user_01 <- reactive({

        # # # Initial text
        validate(
          need(!is.null(input$file_UNC), "Error 01: input$file_UNC can not be NULL.")
        )

        # # # File name control
        validate(
          need(input$file_UNC != '', 'Select an R example.')
        )


        return(TRUE)

      })


      observeEvent(input$file_UNC, {

        req(control_user_01())
        # # # Reset for action buttons
        action_button_load(FALSE)
        action_button_show(FALSE)
        color_button_load(hardcorded_initial_color)

        # # # Reset database
        database(NULL)

      })


      control_user_99 <- reactive({

        req(control_user_01())

        dt_control <- control_user_01()

        return(dt_control)

      })



      # # # Reset actions
      # There are differents reset actions.
      #
      # # 01) Reset All with reset button---------------------------------------
      observeEvent(input$action_reset_all, {

        # # # Reset for div()
        shinyjs::reset("input-panel")

        # # # Reset for action buttons
        action_button_load(FALSE)
        action_button_show(FALSE)
        color_button_load(hardcorded_initial_color)

        # # # Reset database
        database(NULL)
      })



      # # # Activate 'load' ---------------------------------------------
      # 1) If the load button is pressed...
      # If all the previous control for database are OK we change the state of
      # the action "load" to TRUE.

      observeEvent(input$action_load, {


        req(control_user_99(), input$action_load)
        action_button_load(TRUE)



      })





      # Import database --------------------------------------------------------
      observeEvent(action_button_load(),{

        req(control_user_99(), action_button_load())

        database(
          eval(parse(text = input$file_UNC))
        )






      })




      # # # database post control
      control_database <- reactive({

        req(control_user_99(), action_button_load())

        validate(
          need(!is.null(database()), "Error Database 01: 'database' is a NULL object."),
          need(is.data.frame(database()), "Error Database 02: The 'database' object must be a dataframe."),
          need(ncol(database()) > 0, "Error Database 03: 'database' must have at least one column."),
          need(ncol(database()) > 0, "Error Database 04: 'database' must have at least one row."),


        )


        return(TRUE)

      })



      observeEvent(control_database(), {

        req(control_database())
        action_button_show(TRUE)

      })



      observeEvent(action_button_show(), {

        if(is.null(control_database())) color_button_load(hardcorded_initial_color) else
          if(action_button_show()) color_button_load("green") else
            if(!control_database()) color_button_load("red")


      })

      output$df_database <- renderTable({


        req(control_database(), database(), action_button_show())

        database()
        #mtcars
      }, rownames = TRUE, align = "c")



#
#       output$df_database2 <- renderDT({
#
#         req(control_database(), database(), action_button_show())
#
#
#
#         mi_tabla <- database()
#         #https://rstudio.github.io/DT/functions.html
#         vector_pos <- 1:nrow(mi_tabla)
#         vector_color <- rep(NA, length(vector_pos))
#         vector_color[c(T, F)] <- "lightblue"#'red'#
#         vector_color[c(F, T)] <- "lightgreen"#'blue'#
#         vector_color <- vector_color[vector_pos]
#
#         datatable(
#           mi_tabla,
#           rownames = TRUE,
#           options = list(
#             pageLength = -1,
#
#             headerCallback = DT::JS(
#               "function(thead) {",
#               "  $(thead).css('font-size', '2em');",
#               "}"
#             ),
#             columnDefs = list(list(className = 'dt-center', targets = "_all")),
#             #pageLength = 5,
#             dom = "t",
#             scrollX = TRUE,
#             searching = FALSE,
#             scrollCollapse = TRUE,  # Permitir colapsar el scroll
#             fixedColumns = list(leftColumns = 3),  # Fijar las primeras 3 columnas
#             #lengthMenu = list(c(-1), c("All")), # Todas las filas
#             style = list(
#               'font-size' = '20px'  # Tamaño de letra para el nombre de las columnas
#             )
#           )
#
#         ) %>%formatStyle(
#           colnames(mi_tabla),
#           backgroundColor = styleRow(vector_pos, vector_color),#,
#           target = 'row',
#           fontSize = "26px"
#         )
#
#       })
#



      vector_var_names_database <- reactive({

        req(database(), action_button_show())
        colnames(database())
      })




      file_name_database <- reactive({
        input$file_UNC
      })


      file_size_database <- reactive({

        req(database(), action_button_show())
        info_size <- object.size(database())
        info_size <- as.character(info_size)
        info_size
      })



      # # Intro for source data (its information)
      intro_source_database <- reactive({

        req(action_button_show(), database())

        text_list <- list(
          "file_source" = original_file_source(),
          "file_name" = file_name_database(),
          # "Database size" = file_size_database(),
          "ncol" = ncol(database()),
          "nrow"= nrow(database())
        )

        text_list

      })


      output$intro_source_database <- renderTable({

        req(action_button_show(), intro_source_database(), database())


        df_output <- as.data.frame(intro_source_database())
        colnames(df_output) <- names(intro_source_database())
        df_output




      }, rownames = FALSE, align = 'c', border = "all")




      output$show_all_database <- renderUI({

        ns <- shiny::NS(id)

        req(action_button_show(), intro_source_database(), database())

        div(
          fluidRow( column(12,
                           h2("Database details"),
                           tags$div(
                             style = "font-size: 30px;",
                             tableOutput(ns("intro_source_database"))))),
          br(), br(), br(),
          fluidRow(column(12,
                          h2("Database"),
                          shinycssloaders::withSpinner(tableOutput(ns("df_database")))
                          #shinycssloaders::withSpinner(DTOutput(ns("df_database2")))

                          )
          )
        )

      })



      output$calling_help <- renderText({

        req(control_user_01(), control_user_99(), control_database())
        ""

      })





      output_list <- reactive({

        req(database(), action_button_show(),
            vector_var_names_database(),
            file_name_database(), file_size_database(),
            intro_source_database(), original_file_source())



        the_list <- list(database(), vector_var_names_database(),
                         file_name_database(), file_size_database(),
                         intro_source_database(), original_file_source())

        names(the_list) <- c("database", "vector_var_names_database",
                             "file_name_database", "file_size_database",
                             "intro_source_database", "original_file_source")

        return(the_list)
      })









      return(output_list)



    }
  )
}

