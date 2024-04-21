# Space database


# # # 01) UI - Selection for 'database'
module_cpiC000_database_s01_excel_ui <- function(id){
  ns <- shiny::NS(id)


  div(shinyjs::useShinyjs(), id = ns("input-panel"),

      #h2("Initial user election - database"),
      # div(shinyjs::useShinyjs(), id = ns("input-xlsx"),

      div(shinyjs::useShinyjs(), id = ns("input-xlsx"),
          fluidRow(
            column(3,
                   fileInput(inputId = ns("info_xlsx"),
                             label = "File '.xlsx' selection...")
            ),
            column(3, uiOutput(ns("sheet_selection"))),
            #column(3),

            # # # Action buttons
            column(6, br(), br(), uiOutput(ns("action_buttons")))
          )
      ),
      br(), br(), br(),
      # End fluidRow
      textOutput(ns("calling_help"))#,
      #shinycssloaders::withSpinner(uiOutput(ns("show_all_database")))

  ) # End div
}

# # # 01) UI - Selection for 'database'
module_cpiC000_database_s01_excel_ui2 <- function(id){
  ns <- shiny::NS(id)


  div(#shinyjs::useShinyjs(), id = ns("input-panel"),

      #h2("Initial user election - database"),
      # div(shinyjs::useShinyjs(), id = ns("input-xlsx"),


      shinycssloaders::withSpinner(uiOutput(ns("show_all_database")))

  ) # End div
}


# # # 01) SERVER - Selection for 'database'
module_cpiC000_database_s01_excel_server <- function(id, input_file_source){
  moduleServer(
    id,
    function(input, output, session) {

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

      # # Default - 'database' object
      selected_info_xlsx <- reactiveVal(NULL)
      sheets_xlsx <- reactiveVal(NULL)

      # # Default - Control for source of information
      control_user_99 <- reactiveVal(FALSE)

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
            column(6, actionButton(ns("action_load"), label = "LOAD", style = output_style_button_load)),
            #column(8),
            column(6, actionButton(ns("action_reset_all"), "RESET ALL", style = output_style_button_reset))
          )
        )
      })







      # # # Excel --------------------------------------------------------------
      # # # Excel control 01 of 04
      # input$file_source is "xlsx"


      # # # Excel control 02 of 04
      # input$info_xlsx control
      control_user_01 <- reactive({

        # # # Initial text
        validate(
          need(!is.null(input$info_xlsx), "Select a '.xlsx' file.")
        )

        # # # File name control
        validate(
          need(!is.null(input$info_xlsx$name), 'Error 05: File name problems. Change the file name.'),
          need(input$info_xlsx$name != '', 'Error 06: File name problems. Change the file name.')
        )

        # # # File path control
        validate(
          need(!is.null(input$info_xlsx$datapath), 'Error 08: problems regarding the path of the xlsx file. Change the file name or folder name.'),
          need(input$info_xlsx$datapath != '', 'Error 09: File path problems. Change the file name or folder names.')
        )

        # # # Specific control for file extension
        validate(
          need(grepl(".xlsx$", input$info_xlsx$name), "Error 07: Wrong file! You must select a '.xlsx'' file."),
        )

        return(TRUE)

      })


      observeEvent(input$info_xlsx, {

        req(control_user_01())
        # # # Reset for action buttons
        action_button_load(FALSE)
        action_button_show(FALSE)
        color_button_load(hardcorded_initial_color)

        selected_info_xlsx(input$info_xlsx)
        shinyjs::reset("sheet_xlsx")
        # # # Reset database
        database(NULL)

      })



      # # # Excel control 03 of 04
      # input$info_xlsx control
      control_user_02 <- reactive({

        req(control_user_01())

        # # # Initial text
        validate(
          need(!is.null(selected_info_xlsx()), "Select a '.xlsx' file.")
        )

        # # # File name control
        validate(
          need(!is.null(selected_info_xlsx()$name), 'Error 05: File name problems. Change the file name.'),
          need(selected_info_xlsx()$name != '', 'Error 06: File name problems. Change the file name.')
        )

        # # # File path control
        validate(
          need(!is.null(selected_info_xlsx()$datapath), 'Error 08: problems regarding the path of the xlsx file. Change the file name or folder name.'),
          need(selected_info_xlsx()$datapath != '', 'Error 09: File path problems. Change the file name or folder names.')
        )

        # # # Specific control for file extension
        validate(
          need(grepl(".xlsx$", selected_info_xlsx()$name), "Error 07: Wrong file! You must select a '.xlsx'' file."),
        )

        return(TRUE)

      })


      observeEvent(selected_info_xlsx(), {

        req(control_user_02())
        database(NULL)
        sheets_xlsx(NULL)

        # # # # Reset for action buttons
        # action_button_load(FALSE)
        # action_button_show(FALSE)
        # color_button_load(hardcorded_initial_color)
        #
        # shinyjs::reset("input-xlsx")
        # shinyjs::reset("sheet_xlsx")
        # # # # Reset database

        name_sheets <- openxlsx::getSheetNames(selected_info_xlsx()$datapath)
        set_options <- super_sheet_info(name_sheets = name_sheets)
        set_options <- c("Select one... " = "", set_options)
        sheets_xlsx(set_options)
      })



      # # # Sheet selection for xlsx file
      output$sheet_selection <- renderUI({

        ns <- shiny::NS(id)

        # Previuos controls
        req(sheets_xlsx())

        pos_selected <- 1

        # # # SelectInput for sheet selection
        selectInput(inputId = ns("sheet_xlsx"),
                    label = "Sheet selector:",
                    choices = sheets_xlsx(),
                    selected = sheets_xlsx()[pos_selected])

      })



      # # # Excel control 04 of 04
      control_user_03 <- reactive({

        req(control_user_02())

        validate(


          need(!is.null(input$sheet_xlsx), 'Error 06: Sheet problems. Change the sheet names.'),
          need(input$sheet_xlsx != '', "Select a sheet from the 'xlsx' file."),



        )

        return(TRUE)

      })


      selected_sheet <- reactive({
        req(control_user_03())

        output_value <- input$sheet_xlsx
        output_value <- as.integer(as.numeric(as.character(output_value)))
        output_value
      })


      observeEvent(input$sheet_xlsx, {

        req(control_user_03())



        # # # # Reset for action buttons
        action_button_load(FALSE)
        action_button_show(FALSE)
        color_button_load(hardcorded_initial_color)
        database(NULL)


      })





      # # # Reset actions
      # There are differents reset actions.
      #
      # # 01) Reset All with reset button---------------------------------------
      observeEvent(input$action_reset_all, {

        # # # Reset for div()
        shinyjs::reset("input-panel")
        shinyjs::reset("input-xlsx")
        shinyjs::reset("info_xlsx")


        # # # Reset for action buttons
        action_button_load(FALSE)
        action_button_show(FALSE)
        color_button_load(hardcorded_initial_color)

        # # # Reset internal info_xlsx (path and file name)
        selected_info_xlsx(NULL)
        sheets_xlsx(NULL)
        # # # Reset database
        database(NULL)
      })







      # # # Previous control for database --------------------------------------
      # For each source of information there is a specific control.
      # Each control return only a TRUE or FALSE.
      # Depending on the user's choice, the control_file_source_99 object pipes the corresponding control.
      control_user_99 <- reactive({

        req(control_user_03())

        dt_control <- control_user_03()

        return(dt_control)

      })




      # # # Activate 'load' ---------------------------------------------
      # 1) If the load button is pressed...
      # If all the previous control for database are OK we change the state of
      # the action "load" to TRUE.
      # For xlsx files we take the info_xlsx information
      # internally to try import the selected xlsx file.
      observeEvent(input$action_load, {


        req(input$action_load, control_user_99())
        action_button_load(TRUE)
        database(
          read.xlsx(xlsxFile = selected_info_xlsx()$datapath,
                    sheet =  selected_sheet())
        )
      })








      # # # database post control
      control02_post_database <- reactive({

        req(control_user_99(), action_button_load())

        validate(
          need(!is.null(database()), "Error Database 01: 'database' is a NULL object."),
          need(is.data.frame(database()), "Error Database 02: The 'database' object must be a dataframe."),
          need(ncol(database()) > 0, "Error Database 03: 'database' must have at least one column."),
          need(ncol(database()) > 0, "Error Database 04: 'database' must have at least one row.")


        )


        return(TRUE)

      })


      observeEvent(action_button_load(),{


        req(control_user_99(), action_button_load(), control02_post_database())

        action_button_show(TRUE)
      })



      # # # Activate 'show'
      # If database has beed imported succesfully, and all the controls
      # post upload are OK, we put the status for 'show' as TRUE.
      observeEvent(input$action_load, {
        #req(control02_post_database())
        if(!action_button_show() && is.null(database())){

          color_button_load("red")
        } else color_button_load("green")



      })



      output$df_database <- renderTable({


        req(action_button_show(), intro_source_database(), database())

        database()
        #mtcars
      }, rownames = TRUE, align = "c")

      # # # More objects
      # # Vector with var names from database
      vector_var_names_database <- reactive({

        req(database(), action_button_show())
        colnames(database())
      })

      # # File name form database
      file_name_database <- reactive({

        req(database(), action_button_show())
        selected_info_xlsx()$name

      })


      file_size_database <- reactive({

        req(database(), action_button_show())
        info_size <- object.size(database())
        info_size <- as.character(info_size)
        info_size
      })






      intro_source_database <- reactive({

        req(action_button_show(), database())

        text_list <- list(
          "file_source" = original_file_source(),
          "file_name" = file_name_database(),
          #"Total sheets", length(sheets_xlsx()),
          #"Full file size", selected_info_xlsx()$size,
          "selected_sheet" =  selected_sheet(),
          #"Selected sheet size", file_size_database(),
          "ncol" = ncol(database()),
          "nrow" =  nrow(database())
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
                             shinycssloaders::withSpinner(tableOutput(ns("intro_source_database")))))),

          br(), br(), br(),
          fluidRow(column(12,
                          h2("Database"),
                          shinycssloaders::withSpinner(tableOutput(ns("df_database"))))
          )
        )

      })



      output$calling_help <- renderText({

        req(control_user_01(), control_user_02(), control_user_03,
            control_user_99(), control02_post_database())
        ""

      })





      # # # Output!
      output_list <- reactive({

        # Requerimets...
        req(database(),
            action_button_show(),
            vector_var_names_database(), file_name_database(),
            file_size_database(), intro_source_database(), original_file_source()
        )


        # Final list
        the_list <- list(database(), vector_var_names_database(), file_name_database(),
                         file_size_database(), intro_source_database(), original_file_source())

        # Names for final list objects
        names(the_list) <- c("database", "vector_var_names_database", "file_name_database",
                             "file_size_database", "intro_source_database", "original_file_source")

        # Return...
        return(the_list)
      })



      # Module - Final Return
      return(output_list)



    }
  )
}

