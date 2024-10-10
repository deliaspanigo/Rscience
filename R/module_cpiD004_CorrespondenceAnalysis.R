
# # # Module 01 - ANOVA
# UI and SERVER for all modules respect to ANOVA


module_cpiD004_s02B_varselection_ui <- function(id){

  ns <- shiny::NS(id)

  div(
    uiOutput(ns("intro_source_database"))

  )
}




module_cpiD004_s01_varselection_ui <- function(id){

  ns <- shiny::NS(id)

  div(
    uiOutput(ns("vars_selection"))

  )
}




module_cpiD004_s01_varselection_server <- function(id, input_general){
  moduleServer(
    id,
    function(input, output, session) {

      # # # Very importan objects from input_general
      # # Vector var names from database
      vector_var_names_database <- reactive({
        req(input_general())

        input_general()$vector_var_names_database
      })


      # # Info about source database
      intro_source_database <- reactive({
        req(input_general())

        input_general()$intro_source_database
      })


      # # # Control user 01
      control_user_01 <- reactive({


        validate(
          need(!is.null(input_general), "Error 01: Module t test s01 - input_general can not be NULL."),
          need(!is.null(vector_var_names_database()), "Error 10: Module t test s01 - vector_var_names_database() can not be NULL."),
          need(!is.null(intro_source_database()), "Error 11: Module t test s01 - intro_source_database() can not be NULL.")
        )


        return(TRUE)
      })



      # # # Initial values
      action_button_load <- reactiveVal(FALSE)
      action_button_show <- reactiveVal(FALSE)

      # # Colours for actions buttons
      hardcorded_initial_color <- "orange" #"#F4A020"
      color_button_load <- reactiveVal(hardcorded_initial_color)
      color_button_show <- reactiveVal(hardcorded_initial_color)



      # # # Action buttons
      output$action_buttons <- renderUI({

        ns <- shiny::NS(id)


        # # # Style for load button
        standard_style_button_load <- "color: white; background-color: _color_;width: 150px; height: 100px; font-size: 30px;"
        output_style_button_load <- gsub(pattern = "_color_",
                                         replacement = color_button_load(),
                                         x = standard_style_button_load)

        # # # Style for reset button
        output_style_button_reset <- "color: white; background-color: orange;width: 150px; height: 100px; font-size: 30px;"

        # # # UI content
        div(
          fluidRow(
            actionButton(ns("action_load"), label = "RUN", style = output_style_button_load),
            actionButton(ns("action_reset_all"), "RESET ALL", style = output_style_button_reset)
          )
        )
      })



      # # # Intro source database
      output$intro_source_database <- shiny::renderTable({



        df_output <- as.data.frame(intro_source_database())
        colnames(df_output) <- names(intro_source_database())
        df_output



      })



      # output$df_lala2 <-  shiny::renderTable({
      #   #vector_vars <- selected_var_name()
      #   #print(vector_vars)
      #   #print(input_general()$database)
      #   df_full <- input_general()$database
      #   selected_vars <- input$selected_var_name
      #   df_selected_vars <- df_full[selected_vars]
      #   #input_general()$database
      #   #df_selected_vars <- mtcars
      #   #print(df_selected_vars)
      #   df_selected_vars
      # })



      # # # Var selection forCorrespondence Analysisy
      output$vars_selection <- renderUI({

        ns <- shiny::NS(id)



        set_options <- setup_var_info(all_var_names = vector_var_names_database())
        set_options2 <- c("Var selection..." = "", set_options)

        pos01 <- c(1)
        pos02 <- 2:length(set_options)


        div(shinyjs::useShinyjs(), id = ns("input-var-selection"),

            fluidRow(
              # column(4,
              #        fluidRow(
              #         column(12,
              #         selectInput(inputId = ns("selected_var_labels"),
              #                                    label = "Col var labels",
              #                                    choices = set_options2,
              #                                    selected = set_options2[1])
              #          )
              #        )
              #        ),
              column(4,
                     fluidRow(

                       column(12,
                              radioButtons(inputId = ns("selected_var_labels"),
                                           label = "Labels/id for units.",
                                           choices = set_options,
                                           selected = set_options[pos01])
                       ))),
              column(4,
                     fluidRow(

                       column(12,
                              checkboxGroupInput(inputId = ns("selected_var_name"),
                                                 label = "Selected Vars",
                                                 choices = set_options,
                                                 inline = FALSE,
                                                 selected = set_options[pos02])
                       ))),
              column(4,
                     fluidRow(
                       column(4,
                              selectInput(inputId = ns("alpha_value"), label = "Alpha value",
                                          choices = c(0.10, 0.05, 0.01),
                                          #choices = c(0.05),
                                          selected = 0.05)
                       ),

                       column(8, uiOutput(ns("action_buttons"))))),
            ),
            fluidRow(
              column(4),
              column(4,
                     shiny::selectInput(inputId = ns("amount_dim"),
                                        label ="Number of dimentions",
                                        choices = 2:100,#length(set_options),
                                        selected = 100)#length(set_options))
              )),
            fluidRow(
              column(12, textOutput(ns("calling_help")))
            )
        )

      })



      # # # Control user 02
      control_user_02 <- reactive({

        req(control_user_01())

        # validate(
        #   need(!is.null(input$vr_var_name), "Error 09: Module t test s01 - input$vr_var_name can not be NULL."),
        #   need(!is.null(input$factor_var_name), "Error 10: Module t test s01 - input$factor_var_name can not be NULL."),
        #   need(!is.null(input$alpha_value), "Error 11: Module t test s01 - input$alpha_value can not be NULL.")
        # )
        #
        # validate(
        #   need(is.vector(input$vr_var_name), "Error 12: Module t test s01 - input$vr_var_name must be a vector."),
        #   need(is.vector(input$factor_var_name), "Error 13: Module t test s01 - input$vr_var_name must be a vector."),
        #   need(is.vector(input$alpha_value), "Error 14: Module t test s01 - input$alpha_value must be a vector.")
        # )
        #
        # validate(
        #   need(length(input$vr_var_name) == 1, "Error 15: Module t test s01 - input$vr_var_name has length 1."),
        #   need(length(input$factor_var_name) == 1, "Error 16: Module t test s01 - input$factor_var_name has length 1."),
        #   need(length(input$alpha_value) == 1, "Error 17: Module t test s01 - input$alpha_value has length 1.")
        # )
        #
        #
        # validate(
        #   need(input$vr_var_name != "", "Select a response variable."),
        #   need(input$factor_var_name != "", "Select a factor."),
        # )
        #
        # validate(
        #   need(input$vr_var_name != input$factor_var_name, "Selected variables can not be equal.")
        # )



        return(TRUE)
      })


      # # # General control user
      control_user_99 <- reactive({

        req(control_user_02())
        control_user_02()

      })


      # #
      # Reset All --------------------------------------------------------------
      observeEvent(input$action_reset_all, {

        shinyjs::reset("input-var-selection")
        color_button_load(hardcorded_initial_color)
        color_button_show(hardcorded_initial_color)
        action_button_load(FALSE)
        action_button_show(FALSE)


      })


      observeEvent(input$selected_var_labels, {

        # Not show yet
        color_button_load(hardcorded_initial_color)
        color_button_show(hardcorded_initial_color)
        action_button_load(FALSE)
        action_button_show(FALSE)

      })


      # # # Reset if change file source or any options
      # # VR selection
      observeEvent(input$selected_var_name, {

        # Not show yet
        color_button_load(hardcorded_initial_color)
        color_button_show(hardcorded_initial_color)
        action_button_load(FALSE)
        action_button_show(FALSE)

        shiny::updateSelectInput(inputId = "amount_dim",
                                 label ="Number of axis (max: p-1)",
                                 choices = 1:(length(input$selected_var_name)-1),
                                 selected = (length(input$selected_var_name)-1))
      })

      observeEvent(input$amount_dim, {

        # Not show yet
        color_button_load(hardcorded_initial_color)
        color_button_show(hardcorded_initial_color)
        action_button_load(FALSE)
        action_button_show(FALSE)

      })

      observeEvent(input$alpha_value, {

        # Not show yet
        color_button_load(hardcorded_initial_color)
        color_button_show(hardcorded_initial_color)
        action_button_load(FALSE)
        action_button_show(FALSE)

      })

      observeEvent(input$action_load, {


        req(control_user_99())
        action_button_load(TRUE)
        color_button_load("green")
      })



      observeEvent(input$action_load, {

        req(!action_button_load())

        color_button_load("red")
        action_button_load(FALSE)

      })




      observeEvent(input$action_load, {

        req(action_button_load(), control_user_99())
        action_button_show(TRUE)

      })



      # # # Final objects
      selected_var_labels <- reactive({

        req(action_button_show())

        output_value <- input$selected_var_labels
        return(output_value)
      })

      selected_var_name <- reactive({

        req(action_button_show())

        output_value <- input$selected_var_name
        return(output_value)
      })

      selected_amount_dim <- reactive({

        #req(action_button_show())

        output_value <- as.numeric(as.character(input$amount_dim))
        return(output_value)
      })


      alpha_value <- reactive({
        req(action_button_show())
        output_value <- as.numeric(as.character(input$alpha_value))
        output_value
      })




      output$calling_help <- renderText({

        req(control_user_99())
        ""

      })


      # # # Output list
      output_list <- reactive({

        req(action_button_show())


        the_list <- list(selected_var_name(), selected_var_labels(),
                         alpha_value(), intro_source_database(), selected_amount_dim())

        names(the_list) <- c("selected_var_name", "selected_var_labels",
                             "alpha_value", "intro_source_database", "selected_amount_dim")
        the_list
      })


      return(output_list)
    })
}






module_cpiD004_s02_rscience_ui <- function(id){

  ns <- shiny::NS(id)

  div(
    uiOutput(ns("page_test")),
    br(), br(), br(),br(), br(), br(),br(), br(), br(),br(),br(), br(), br(),
    br(), br(), br(),br(), br(), br(),br(), br(), br(),br(),br(), br(), br(),
    br(), br(), br(),br(), br(), br(),br(), br(), br(),br(),br(), br(), br(),
    br(), br(), br(),br(), br(), br(),br(), br(), br(),br(),br(), br(), br(),
    br(), br(), br(),br(), br(), br(),br(), br(), br(),br(),br(), br(), br(),
    br(), br(), br(),br(), br(), br(),br(), br(), br(),br(),br(), br(), br(),
    br(), br(), br(),br(), br(), br(),br(), br(), br(),br(),br(), br(), br(),
    br(), br(), br(),br(), br(), br(),br(), br(), br(),br(),br(), br(), br(),
    br(), br(), br(),br(), br(), br(),br(), br(), br(),br(),br(), br(), br()
  )

}




module_cpiD004_s02_rscience_server <- function(id, input_general, input_01_anova){
  moduleServer(
    id,
    function(input, output, session) {


      # # # Control for initial inputs
      control_user_01 <- reactive({

        check_previous <- FALSE

        validate(
          need(!is.null(input_general()), "Error 01: Module anova s02 - input_general can not be NULL."),
          need(!is.null(input_01_anova()), "Error 02: Module anova s02 - input_01_anova can not be NULL.")
        )

        check_previous <- fn_cpiD004_control_previous(database = input_general()$database,
                                                      selected_var_name = input_01_anova()$selected_var_name,
                                                      alpha_value = input_01_anova()$alpha_value)

        validate(
          need(check_previous$dt_ok, check_previous$text_output)
        )
        return(TRUE)
      })


      ####################################################################################

      output$intro_source_database <- renderTable({


        list_intro <- input_general()$intro_source_database
        df_output <- as.data.frame(list_intro)
        colnames(df_output) <- names(list_intro)
        df_output



      })

      # # # All anova results
      RR_general <- reactive({

        req(control_user_01())

        the_output <- fn_cpiD004_results(database = input_general()$database,
                                         selected_var_name = input_01_anova()$selected_var_name,
                                         selected_var_labels = input_01_anova()$selected_var_labels,
                                         alpha_value = input_01_anova()$alpha_value,
                                         selected_amount_dim = input_01_anova()$selected_amount_dim)


        the_output

      })



      # # # Control for initial inputs
      control_user_02 <- reactive({
        #
        req(control_user_01())
        #
        validate(
          need(!is.null(RR_general()), "Error 03: Module anova s02 - RR_general() can not be NULL.")
        )
        #
        #
        #
        #
        #         # Control post
        # check_post <- fn_cpiD004_control_post(list_results_from_cpiA001_anova1way = RR_general())
        # #
        # validate(
        #   need(check_post$check_ok, check_post$text_output)
        # )

        return(TRUE)
      })





      RR_code <- reactive({

        req(control_user_02())
        the_code <- fn_cpiD004_code_sectionALL(intro_source_database = input_01_anova()$intro_source_database,
                                                                vr_var_name = input_01_anova()$vr_var_name,
                                                                factor_var_name = input_01_anova()$factor_var_name,
                                                                alpha_value = input_01_anova()$alpha_value)
        the_code
      })

      ####################################################################################


      # # # Tab01 - Minibase
      output$tab01_minibase <- DT::renderDT({

        req(control_user_02())

        mi_lista <- RR_general()




        # Vector con nombres de elementos a ver
        selected_objs <- c("minibase")


        # Usar lapply para mostrar los elementos deseados
        #mi_lista[selected_objs]

        mi_tabla <- mi_lista[[selected_objs]]
        #https://rstudio.github.io/DT/functions.html
        vector_pos <- 1:nrow(mi_tabla)
        vector_color <- rep(NA, length(vector_pos))
        vector_color[c(T, F)] <- "lightblue"#'red'#
        vector_color[c(F, T)] <- "lightgreen"#'blue'#
        vector_color <- vector_color[vector_pos]

        datatable(
          mi_tabla,
          rownames = TRUE,
          options = list(

            headerCallback = DT::JS(
              "function(thead) {",
              "  $(thead).css('font-size', '2em');",
              "}"
            ),
            columnDefs = list(list(className = 'dt-center', targets = "_all")),
            #pageLength = 5,
            #dom = "t",
            scrollX = TRUE,
            searching = FALSE,
            scrollCollapse = TRUE,  # Permitir colapsar el scroll
            #fixedColumns = list(leftColumns = 3),  # Fijar las primeras 3 columnas
            #lengthMenu = list(c(-1), c("All")), # Todas las filas
            style = list(
              'font-size' = '20px'  # Tamaño de letra para el nombre de las columnas
            )
          )

        ) %>%formatStyle(
          colnames(mi_tabla),
          backgroundColor = styleRow(vector_pos, vector_color),#,
          target = 'row',
          fontSize = "26px"
        )

      })

      output$tab01_FULL <- renderUI({

        ns <- shiny::NS(id)

        div(
          h2("1) minibase"),
          DT::DTOutput(ns("tab01_minibase")),
          br(), br(), br()
        )

      })

      ######################################################
      output$tab02_analysisB_list <- renderPrint({

        req(control_user_02())


        mi_lista <- RR_general()




        # Vector con nombres de elementos a ver
        nombres_a_ver <- c("list_pca", "autovalores","df_autovalores")

        # Usar lapply para mostrar los elementos deseados
        mi_lista[nombres_a_ver]
      })

      output$tab02_analysisB_FULL <- renderUI({

        ns <- shiny::NS(id)

        div(
          h2("1) R Outputs"),
          verbatimTextOutput(ns("tab02_analysisB_list")),
          br(), br(), br(),
        )

      })

      ####################################################################################



      # # # Tab 05 - Analysis resume...
      output$tab02_analysis_df01 <- DT::renderDT({

        req(control_user_02())

        mi_lista <- RR_general()




        # Vector con nombres de elementos a ver
        selected_objs <- c("df_inertia")


        # Usar lapply para mostrar los elementos deseados
        #mi_lista[selected_objs]

        mi_tabla <- mi_lista[[selected_objs]]
        #https://rstudio.github.io/DT/functions.html
        vector_pos <- 1:nrow(mi_tabla)
        vector_color <- rep(NA, length(vector_pos))
        vector_color[c(T, F)] <- "lightblue"#'red'#
        vector_color[c(F, T)] <- "lightgreen"#'blue'#
        vector_color <- vector_color[vector_pos]

        datatable(
          mi_tabla,
          rownames = TRUE,
          options = list(

            headerCallback = DT::JS(
              "function(thead) {",
              "  $(thead).css('font-size', '2em');",
              "}"
            ),
            columnDefs = list(list(className = 'dt-center', targets = "_all")),
            #pageLength = 5,
            dom = "t",
            scrollX = TRUE,
            searching = FALSE,
            scrollCollapse = TRUE,  # Permitir colapsar el scroll
            fixedColumns = list(leftColumns = 3),  # Fijar las primeras 3 columnas
            #lengthMenu = list(c(-1), c("All")), # Todas las filas
            style = list(
              'font-size' = '20px'  # Tamaño de letra para el nombre de las columnas
            )
          )

        ) %>%formatStyle(
          colnames(mi_tabla),
          backgroundColor = styleRow(vector_pos, vector_color),#,
          target = 'row',
          fontSize = "26px"
        )

      })

      # # # Tab 05 - Analysis resume...
      output$tab02_analysis_info <- DT::renderDT({

        req(control_user_02())

        mi_lista <- RR_general()




        # Vector con nombres de elementos a ver
        selected_objs <- c("df_info")


        # Usar lapply para mostrar los elementos deseados
        #mi_lista[selected_objs]

        mi_tabla <- mi_lista[[selected_objs]]
        #https://rstudio.github.io/DT/functions.html
        vector_pos <- 1:nrow(mi_tabla)
        vector_color <- rep(NA, length(vector_pos))
        vector_color[c(T, F)] <- "lightblue"#'red'#
        vector_color[c(F, T)] <- "lightgreen"#'blue'#
        vector_color <- vector_color[vector_pos]

        datatable(
          mi_tabla,
          rownames = TRUE,
          options = list(

            headerCallback = DT::JS(
              "function(thead) {",
              "  $(thead).css('font-size', '2em');",
              "}"
            ),
            columnDefs = list(list(className = 'dt-center', targets = "_all")),
            #pageLength = 5,
            dom = "t",
            scrollX = TRUE,
            searching = FALSE,
            scrollCollapse = TRUE,  # Permitir colapsar el scroll
            fixedColumns = list(leftColumns = 3),  # Fijar las primeras 3 columnas
            #lengthMenu = list(c(-1), c("All")), # Todas las filas
            style = list(
              'font-size' = '20px'  # Tamaño de letra para el nombre de las columnas
            )
          )

        ) %>%formatStyle(
          colnames(mi_tabla),
          backgroundColor = styleRow(vector_pos, vector_color),#,
          target = 'row',
          fontSize = "26px"
        )

      })

      output$tab02_analysis_df02 <- DT::renderDT({

        req(control_user_02())

        mi_lista <- RR_general()




        # Vector con nombres de elementos a ver
        selected_objs <- c("df_chi_squared")


        # Usar lapply para mostrar los elementos deseados
        #mi_lista[selected_objs]

        mi_tabla <- mi_lista[[selected_objs]]
        mi_tabla <- mi_tabla[,-1]
        #https://rstudio.github.io/DT/functions.html
        vector_pos <- 1:nrow(mi_tabla)
        vector_color <- rep(NA, length(vector_pos))
        vector_color[c(T, F)] <- "lightblue"#'red'#
        vector_color[c(F, T)] <- "lightgreen"#'blue'#
        vector_color <- vector_color[vector_pos]
        vector_pos_numeric <- apply(mi_tabla, 2, is.numeric)

        datatable(
          mi_tabla,
          rownames = TRUE,
          options = list(

            headerCallback = DT::JS(
              "function(thead) {",
              "  $(thead).css('font-size', '2em');",
              "}"
            ),
            columnDefs = list(list(className = 'dt-center', targets = "_all")),
            #pageLength = 5,
            dom = "t",
            scrollX = TRUE,
            searching = FALSE,
            scrollCollapse = TRUE,  # Permitir colapsar el scroll
            fixedColumns = list(leftColumns = 3),  # Fijar las primeras 3 columnas
            #lengthMenu = list(c(-1), c("All")), # Todas las filas
            style = list(
              'font-size' = '20px'  # Tamaño de letra para el nombre de las columnas
            )
          )

        ) %>%formatStyle(
          colnames(mi_tabla),
          backgroundColor = styleRow(vector_pos, vector_color),#,
          target = 'row',
          fontSize = "26px"
        )%>%
          formatRound(columns = names(mi_tabla)[vector_pos_numeric], digits = 4)

      })

      output$tab02_analysis_df03 <- DT::renderDT({

        req(control_user_02())

        mi_lista <- RR_general()




        # Vector con nombres de elementos a ver
        selected_objs <- c("df_table01_row_coord")


        # Usar lapply para mostrar los elementos deseados
        #mi_lista[selected_objs]

        mi_tabla <- mi_lista[[selected_objs]]
        #https://rstudio.github.io/DT/functions.html
        vector_pos <- 1:nrow(mi_tabla)
        vector_color <- rep(NA, length(vector_pos))
        vector_color[c(T, F)] <- "lightblue"#'red'#
        vector_color[c(F, T)] <- "lightgreen"#'blue'#
        vector_color <- vector_color[vector_pos]
        vector_pos_numeric <- apply(mi_tabla, 2, is.numeric)

        datatable(
          mi_tabla,
          rownames = TRUE,
          options = list(

            headerCallback = DT::JS(
              "function(thead) {",
              "  $(thead).css('font-size', '2em');",
              "}"
            ),
            columnDefs = list(list(className = 'dt-center', targets = "_all")),
            #pageLength = 5,
            dom = "t",
            scrollX = TRUE,
            searching = FALSE,
            scrollCollapse = TRUE,  # Permitir colapsar el scroll
            fixedColumns = list(leftColumns = 3),  # Fijar las primeras 3 columnas
            #lengthMenu = list(c(-1), c("All")), # Todas las filas
            style = list(
              'font-size' = '20px'  # Tamaño de letra para el nombre de las columnas
            )
          )

        ) %>%formatStyle(
          colnames(mi_tabla),
          backgroundColor = styleRow(vector_pos, vector_color),#,
          target = 'row',
          fontSize = "26px"
        )%>%
          formatRound(columns = names(mi_tabla)[vector_pos_numeric], digits = 4)

      })

      output$tab02_analysis_df04 <- DT::renderDT({

        req(control_user_02())

        mi_lista <- RR_general()




        # Vector con nombres de elementos a ver
        selected_objs <- c("df_table02_row_cos2")


        # Usar lapply para mostrar los elementos deseados
        #mi_lista[selected_objs]

        mi_tabla <- mi_lista[[selected_objs]]
        #https://rstudio.github.io/DT/functions.html
        vector_pos <- 1:nrow(mi_tabla)
        vector_color <- rep(NA, length(vector_pos))
        vector_color[c(T, F)] <- "lightblue"#'red'#
        vector_color[c(F, T)] <- "lightgreen"#'blue'#
        vector_color <- vector_color[vector_pos]
        vector_pos_numeric <- apply(mi_tabla, 2, is.numeric)

        datatable(
          mi_tabla,
          rownames = TRUE,
          options = list(

            headerCallback = DT::JS(
              "function(thead) {",
              "  $(thead).css('font-size', '2em');",
              "}"
            ),
            columnDefs = list(list(className = 'dt-center', targets = "_all")),
            #pageLength = 5,
            dom = "t",
            scrollX = TRUE,
            searching = FALSE,
            scrollCollapse = TRUE,  # Permitir colapsar el scroll
            fixedColumns = list(leftColumns = 3),  # Fijar las primeras 3 columnas
            #lengthMenu = list(c(-1), c("All")), # Todas las filas
            style = list(
              'font-size' = '20px'  # Tamaño de letra para el nombre de las columnas
            )
          )

        ) %>%formatStyle(
          colnames(mi_tabla),
          backgroundColor = styleRow(vector_pos, vector_color),#,
          target = 'row',
          fontSize = "26px"
        )%>%
          formatRound(columns = names(mi_tabla)[vector_pos_numeric], digits = 4)

      })

      output$tab02_analysis_df05 <- DT::renderDT({

        req(control_user_02())

        mi_lista <- RR_general()




        # Vector con nombres de elementos a ver
        selected_objs <- c("df_table03_row_contrib")


        # Usar lapply para mostrar los elementos deseados
        #mi_lista[selected_objs]

        mi_tabla <- mi_lista[[selected_objs]]
        #https://rstudio.github.io/DT/functions.html
        vector_pos <- 1:nrow(mi_tabla)
        vector_color <- rep(NA, length(vector_pos))
        vector_color[c(T, F)] <- "lightblue"#'red'#
        vector_color[c(F, T)] <- "lightgreen"#'blue'#
        vector_color <- vector_color[vector_pos]
        vector_pos_numeric <- apply(mi_tabla, 2, is.numeric)

        datatable(
          mi_tabla,
          rownames = TRUE,
          options = list(

            headerCallback = DT::JS(
              "function(thead) {",
              "  $(thead).css('font-size', '2em');",
              "}"
            ),
            columnDefs = list(list(className = 'dt-center', targets = "_all")),
            #pageLength = 5,
            dom = "t",
            scrollX = TRUE,
            searching = FALSE,
            scrollCollapse = TRUE,  # Permitir colapsar el scroll
            fixedColumns = list(leftColumns = 3),  # Fijar las primeras 3 columnas
            #lengthMenu = list(c(-1), c("All")), # Todas las filas
            style = list(
              'font-size' = '20px'  # Tamaño de letra para el nombre de las columnas
            )
          )

        ) %>%formatStyle(
          colnames(mi_tabla),
          backgroundColor = styleRow(vector_pos, vector_color),#,
          target = 'row',
          fontSize = "26px"
        )%>%
          formatRound(columns = names(mi_tabla)[vector_pos_numeric], digits = 4)

      })


      output$tab02_analysis_df06 <- DT::renderDT({

        req(control_user_02())

        mi_lista <- RR_general()




        # Vector con nombres de elementos a ver
        selected_objs <- c("df_table04_col_coord")


        # Usar lapply para mostrar los elementos deseados
        #mi_lista[selected_objs]

        mi_tabla <- mi_lista[[selected_objs]]
        #https://rstudio.github.io/DT/functions.html
        vector_pos <- 1:nrow(mi_tabla)
        vector_color <- rep(NA, length(vector_pos))
        vector_color[c(T, F)] <- "lightblue"#'red'#
        vector_color[c(F, T)] <- "lightgreen"#'blue'#
        vector_color <- vector_color[vector_pos]
        vector_pos_numeric <- apply(mi_tabla, 2, is.numeric)

        datatable(
          mi_tabla,
          rownames = TRUE,
          options = list(

            headerCallback = DT::JS(
              "function(thead) {",
              "  $(thead).css('font-size', '2em');",
              "}"
            ),
            columnDefs = list(list(className = 'dt-center', targets = "_all")),
            #pageLength = 5,
            dom = "t",
            scrollX = TRUE,
            searching = FALSE,
            scrollCollapse = TRUE,  # Permitir colapsar el scroll
            fixedColumns = list(leftColumns = 3),  # Fijar las primeras 3 columnas
            #lengthMenu = list(c(-1), c("All")), # Todas las filas
            style = list(
              'font-size' = '20px'  # Tamaño de letra para el nombre de las columnas
            )
          )

        ) %>%formatStyle(
          colnames(mi_tabla),
          backgroundColor = styleRow(vector_pos, vector_color),#,
          target = 'row',
          fontSize = "26px"
        )%>%
          formatRound(columns = names(mi_tabla)[vector_pos_numeric], digits = 4)

      })



      output$tab02_analysis_df07 <- DT::renderDT({

        req(control_user_02())

        mi_lista <- RR_general()




        # Vector con nombres de elementos a ver
        selected_objs <- c("df_table05_col_cos2")


        # Usar lapply para mostrar los elementos deseados
        #mi_lista[selected_objs]

        mi_tabla <- mi_lista[[selected_objs]]
        #https://rstudio.github.io/DT/functions.html
        vector_pos <- 1:nrow(mi_tabla)
        vector_color <- rep(NA, length(vector_pos))
        vector_color[c(T, F)] <- "lightblue"#'red'#
        vector_color[c(F, T)] <- "lightgreen"#'blue'#
        vector_color <- vector_color[vector_pos]
        vector_pos_numeric <- apply(mi_tabla, 2, is.numeric)


        datatable(
          mi_tabla,
          rownames = TRUE,
          options = list(

            headerCallback = DT::JS(
              "function(thead) {",
              "  $(thead).css('font-size', '2em');",
              "}"
            ),
            columnDefs = list(list(className = 'dt-center', targets = "_all")),
            #pageLength = 5,
            dom = "t",
            scrollX = TRUE,
            searching = FALSE,
            scrollCollapse = TRUE,  # Permitir colapsar el scroll
            fixedColumns = list(leftColumns = 3),  # Fijar las primeras 3 columnas
            #lengthMenu = list(c(-1), c("All")), # Todas las filas
            style = list(
              'font-size' = '20px'  # Tamaño de letra para el nombre de las columnas
            )
          )

        ) %>%formatStyle(
          colnames(mi_tabla),
          backgroundColor = styleRow(vector_pos, vector_color),#,
          target = 'row',
          fontSize = "26px"
        )%>%
          formatRound(columns = names(mi_tabla)[vector_pos_numeric], digits = 4)

      })



      output$tab02_analysis_df08 <- DT::renderDT({

        req(control_user_02())

        mi_lista <- RR_general()




        # Vector con nombres de elementos a ver
        selected_objs <- c("df_table06_col_contrib")


        # Usar lapply para mostrar los elementos deseados
        #mi_lista[selected_objs]

        mi_tabla <- mi_lista[[selected_objs]]
        #https://rstudio.github.io/DT/functions.html
        vector_pos <- 1:nrow(mi_tabla)
        vector_color <- rep(NA, length(vector_pos))
        vector_color[c(T, F)] <- "lightblue"#'red'#
        vector_color[c(F, T)] <- "lightgreen"#'blue'#
        vector_color <- vector_color[vector_pos]
        vector_pos_numeric <- apply(mi_tabla, 2, is.numeric)

        datatable(
          mi_tabla,
          rownames = TRUE,
          options = list(

            headerCallback = DT::JS(
              "function(thead) {",
              "  $(thead).css('font-size', '2em');",
              "}"
            ),
            columnDefs = list(list(className = 'dt-center', targets = "_all")),
            #pageLength = 5,
            dom = "t",
            scrollX = TRUE,
            searching = FALSE,
            scrollCollapse = TRUE,  # Permitir colapsar el scroll
            fixedColumns = list(leftColumns = 3),  # Fijar las primeras 3 columnas
            #lengthMenu = list(c(-1), c("All")), # Todas las filas
            style = list(
              'font-size' = '20px'  # Tamaño de letra para el nombre de las columnas
            )
          )

        ) %>%formatStyle(
          colnames(mi_tabla),
          backgroundColor = styleRow(vector_pos, vector_color),#,
          target = 'row',
          fontSize = "26px"
        )%>%
          formatRound(columns = names(mi_tabla)[vector_pos_numeric], digits = 4)

      })




      output$tab02_analysis_FULL <- renderUI({

        ns <- shiny::NS(id)

        div(
          # h2("1) Requeriment - Normality test - Residuals"),
          # verbatimTextOutput(ns("tab02_analysis_df01")),
          # br(), br(), br(),
          #
          # h2("1) Requeriment - Normality test - Residuals"),
          # verbatimTextOutput(ns("tab02_analysis_df02")),
          # br(), br(), br(),

          h2("1) General Information"),
          h3("R object: df_info"),
          DTOutput(ns("tab02_analysis_info")),
          br(), br(), br(),

          h2("2) Inertia"),
          h3("R object: df_inertia"),
          DTOutput(ns("tab02_analysis_df01")),
          br(), br(), br(),


          h2("3) Chi Squared Test"),
          h3("R object: df_chi_squared"),
          DTOutput(ns("tab02_analysis_df02")),
          br(), br(), br(),

          h2("4) Row coordinates"),
          h3("R object: df_table01_row_coord"),
          DTOutput(ns("tab02_analysis_df03")),
          br(), br(), br(),

          #h2("4) Quality representation by rows"),
          #h3("R df_table02_row_cos2"),
          #DTOutput(ns("tab02_analysis_df04")),
          #br(), br(), br(),

          #h2("5) Contribution by rows"),
          #h3("R object: df_table03_row_contrib"),
          #DTOutput(ns("tab02_analysis_df05")),
          #br(), br(), br(),

          h2("5) Column coordinates"),
          h3("R object: df_table04_col_coord"),
          DTOutput(ns("tab02_analysis_df06")),
          br(), br(), br(),

          #h2("7) Quality representation by cols"),
          #h3("R object: df_table05_col_cos2"),
          #DTOutput(ns("tab02_analysis_df07")),
          #br(), br(), br(),

          #h2("8) Contribution by cols"),
          #h3("R object: df_table06_col_contrib"),
          #DTOutput(ns("tab02_analysis_df08")),
          #br(), br(), br()
        )

      })
      ###################################################################

      # # # Tab01 - Anova Results
      output$tab01_all_anova_results <- renderPrint({

        req(control_user_02())

        RR_general()
      })

      ##########################################################################





      #############################################################
      output$el_plot1 <- renderPlot({


        mi_lista <- RR_general()


        new_plot <-  with(mi_lista,{
          # # # Create a new plot...
          # # # Create a new plot...
          # # # Create a new plot...

          # # # Plot 001 - eigenvalues
          set_width01 <- 1
          set_space01 <- 0.2

          barplot(height = df_inertia$porc_inertia,
                  names.arg = df_inertia$axis,
                  xlab = "Axis",
                  ylab = "Inertia",
                  main = "Plot 001 - Inertia",
                  ylim = c(0, 120),
                  las = 1,
                  axes = F,
                  width = set_width01,
                  space = set_space01, col = "blue")


          axis(side = 2, col = "black", las = 1,
               col.ticks = "black",
               at = seq(0, 100, by = 10),
               labels = seq(0, total_inertia_value, length.out = 11))

          pos_x_text01 <- (set_width01 + set_space01) * (c(1:nrow(df_inertia))) - (set_width01/2)
          pos_y_text01 <- df_inertia$porc_inertia
          text_label01 <- df_inertia$inertia
          text(x = pos_x_text01,
               y = pos_y_text01,
               labels = text_label01,
               pos = 3,
               font = 2)

        })

        new_plot
      })



      output$el_plot2 <- renderPlot({


        mi_lista <- RR_general()


        new_plot <-  with(mi_lista,{
          # # # Create a new plot...
          # # # Create a new plot...
          # # # Create a new plot...
          set_width02 <- 1
          set_space02 <- 0.2

          barplot(height = df_inertia$acum_porc_inertia,
                  names.arg = df_inertia$axis,
                  xlab = "Axis",
                  ylab = "Inertia",
                  main = "Plot 002 - Acumulative Inertia",
                  ylim = c(0, 120),
                  las = 1,
                  axes = F,
                  width = set_width02,
                  space = set_space02, col = "blue")

          # Cambiar el color del eje Y a blanco
          axis(side = 2, col = "black", las = 1,
               col.ticks = "black",
               at = seq(0, 100, by = 10),
               labels = seq(0, total_inertia_value, length.out = 11))

          pos_x_text02 <- (set_width02 + set_space02) * (c(1:nrow(df_inertia))) - (set_width02/2)
          pos_y_text02 <- df_inertia$acum_porc_inertia
          text_label02 <- df_inertia$acum_inertia
          text(x = pos_x_text02,
               y = pos_y_text02,
               labels = text_label02,
               pos = 3,
               font = 2)


        })

        new_plot
      })

      output$el_plot3 <- renderPlot({


        mi_lista <- RR_general()


        new_plot <-  with(mi_lista,{
          # # # Create a new plot...
          # # # Create a new plot...
          # # # Create a new plot...
          # # # Plot 003 - Porcentaje eigenvalues
          set_width03 <- 1
          set_space03 <- 0.2

          barplot(height = df_inertia$porc_inertia,
                  names.arg = df_inertia$axis,
                  xlab = "Axis",
                  ylab = "Percentage",
                  main = "Plot 003 - Percentage Inertia",
                  ylim = c(0, 120),
                  las = 1,
                  axes = F,
                  width = set_width03,
                  space = set_space03, col = "blue")

          # Cambiar el color del eje Y a blanco
          axis(side = 2, col = "black", las = 1,
               col.ticks = "black", at = seq(0, 100, by = 10),
               labels = T)

          pos_x_text03 <- (set_width03 + set_space03) * (c(1:nrow(df_inertia))) - (set_width03/2)
          pos_y_text03 <- df_inertia$porc_inertia
          text_label03 <- paste0(df_inertia$porc_inertia, "%")
          text(x = pos_x_text03,
               y = pos_y_text03,
               labels = text_label03,
               pos = 3,
               font = 2)

        })

        new_plot
      })

      output$el_plot4 <- renderPlot({


        mi_lista <- RR_general()


        new_plot <-  with(mi_lista,{
          # # # Plot 004 - Acumulative Porcentaje eigenvalues
          set_width04 <- 1
          set_space04 <- 0.2

          barplot(height = df_inertia$acum_porc_inertia,
                  names.arg = df_inertia$axis,
                  xlab = "Axis",
                  ylab = "Percentage",
                  main = "Plot 004 - Acumulative Percentage Inertia",
                  ylim = c(0, 120),
                  las = 1,
                  axes = F,
                  width = set_width04,
                  space = set_space04, col = "blue")

          # Cambiar el color del eje Y a blanco
          axis(side = 2, col = "black", las = 1,
               col.ticks = "black", at = seq(0, 100, by = 10),
               labels = T)

          pos_x_text04 <- (set_width04 + set_space04) * (c(1:nrow(df_inertia))) - (set_width04/2)
          pos_y_text04 <- df_inertia$acum_porc_inertia
          text_label04 <- paste0(df_inertia$acum_porc_inertia, "%")
          text(x = pos_x_text04,
               y = pos_y_text04,
               labels = text_label04,
               pos = 3,
               font = 2)

        })

        new_plot
      })




      output$el_plot5 <- renderPlot({


        mi_lista <- RR_general()


        new_plot <-  with(mi_lista,{
          # factoextra::fviz_pca_var(X = list_ca,
          #                          title = "Plot 005 CA - Variables",
          #                          col.var = "red")

          factoextra::fviz_ca_biplot(list_ca, map = "symbiplot",
                                     title = "Plot 005 - CA - Simetric Biplot",
                                     col.row = "blue",
                                     col.col = "red",
                                     repel = TRUE)

        })

        new_plot
      })


      output$el_plot6 <- renderPlot({


        mi_lista <- RR_general()


        new_plot <-  with(mi_lista,{
          factoextra::fviz_ca_biplot(list_ca, map = "rowprincipal",
                                     title = "Plot 006 - CA - Biplot Asimétrico con
                                     vectores de columnas en el espacio de las filas.",
                                     col.row = "blue",
                                     col.col = "red",
                                     repel = TRUE, arrow = c(TRUE, TRUE))


        })

        new_plot
      })

      output$el_plot7 <- renderPlot({


        mi_lista <- RR_general()


        new_plot <-  with(mi_lista,{
          factoextra::fviz_ca_biplot(list_ca, map = "colprincipal",
                                     title = "Plot 007 - CA - Biplot Asimétrico con
                                     vectores de filas en el espacio de las columnas.",
                                     col.row = "blue",
                                     col.col = "red",
                                     repel = TRUE, arrow = c(TRUE, TRUE))


        })

        new_plot
      })


      output$el_plot8 <- renderPlotly({


        mi_lista <- RR_general()


        new_plot <-  with(mi_lista,{
          pos_x_graph <- "Dim1"
          pos_y_graph <- "Dim2"
          pos_z_graph <- "Dim3"


          seleceted_x_coord_ind <- df_table04_coord_ind[, pos_x_graph ]
          seleceted_y_coord_ind <- df_table04_coord_ind[, pos_y_graph ]
          seleceted_z_coord_ind <- df_table04_coord_ind[, pos_z_graph ]

          seleceted_x_coord_var <- df_table01_coord_var[, pos_x_graph ]
          seleceted_y_coord_var <- df_table01_coord_var[, pos_y_graph ]
          seleceted_z_coord_var <- df_table01_coord_var[, pos_z_graph ]

          plot005 <- plotly::plot_ly()

          plot005 <- add_trace(p = plot005,
                               x = seleceted_x_coord_ind,
                               y = seleceted_y_coord_ind,
                               z = seleceted_z_coord_ind,
                               type = 'scatter3d',
                               mode = 'markers',
                               name = "coord_ind",
                               marker = list(size = 10,
                                             color = 'blue',
                                             opacity = 0.7))


          plot005 <- plotly::layout(p = plot005,
                                    scene = list(xaxis = list(title = pos_x_graph , zeroline = TRUE),
                                                 yaxis = list(title = pos_y_graph , zeroline = TRUE),
                                                 zaxis = list(title = pos_z_graph ,  zeroline = TRUE)),
                                    title = "Plot 009 - Biplot 3D",
                                    font = list(size = 20),
                                    margin = list(t = 100))


          plot005 <- add_trace(p = plot005,
                               x = 0,
                               y = 0,
                               z = 0,
                               type = 'scatter3d',
                               mode = 'markers',
                               name = "center",
                               marker = list(size = 10,
                                             color = 'black',
                                             opacity = 0.7))




          plot005 <- add_trace(p = plot005,
                               x = seleceted_x_coord_var,
                               y = seleceted_y_coord_var,
                               z = seleceted_z_coord_var,
                               type = 'scatter3d',
                               mode = 'markers',
                               name = "coord_var",
                               marker = list(size = 10,
                                             color = 'red',
                                             opacity = 0.7))

          #
          # plot005 <- add_trace(p = plot005,
          #                      x = c(0, seleceted_x_coord_var),
          #                      y = c(0, seleceted_y_coord_var),
          #                      z = c(0, seleceted_z_coord_var),
          #                      type = 'scatter3d',
          #                      mode = 'segments',
          #                      name = "vector_var",
          #                      marker = list(size = 10,
          #                                    color = 'green',
          #                                    opacity = 0.7))
          for(k in 1:length(seleceted_x_coord_var)){
            plot005 <- add_trace(p = plot005,
                                 x = c(0, seleceted_x_coord_var[k]),
                                 y = c(0, seleceted_y_coord_var[k]),
                                 z = c(0, seleceted_z_coord_var[k]),
                                 type = "scatter3d",
                                 mode = "lines",
                                 name = df_table01_coord_var$variables[k])



          }

          plot005 <- add_trace(p = plot005,
                               x = seleceted_x_coord_var,
                               y = seleceted_y_coord_var,
                               z = seleceted_z_coord_var,
                               #marker = m,
                               type = "scatter3d",
                               mode = "text+markers",
                               name = "var_labels", linetypes = NULL,
                               text = df_table01_coord_var$variables)

          #
          # plot005 <- add_segments(p = plot005,
          #              x = 0, xend = seleceted_x_coord_var,
          #              y = 0, yend = seleceted_y_coord_var,
          #              z = 0, zend = seleceted_z_coord_var)

          plot005

        })

        new_plot
      })
      #
      # output$el_plot4 <- renderPlotly({
      #
      #
      #   mi_lista <- RR_general()
      #
      #
      #   new_plot <-  with(mi_lista,{
      #     # # # Create a new plot...
      #     # # # Create a new plot...
      #     # # # New plotly...
      #     plot004_factor <- plotly::plot_ly()
      #
      #     # # # Boxplot and info...
      #     plot004_factor <- plotly::add_trace(p = plot004_factor,
      #                                         type = "box",
      #                                         x = df_table_plot004$level ,
      #                                         color = df_table_plot004$level,
      #                                         colors = df_table_plot004$color,
      #                                         lowerfence = df_table_plot004$min,
      #                                         q1 = df_table_plot004$Q1,
      #                                         median = df_table_plot004$median,
      #                                         q3 = df_table_plot004$Q3,
      #                                         upperfence = df_table_plot004$max,
      #                                         boxmean = TRUE,
      #                                         boxpoints = FALSE,
      #                                         line = list(color = "black", width = 3)
      #     )
      #
      #     # # # Title and settings...
      #     plot004_factor <- plotly::layout(p = plot004_factor,
      #                                      title = "Plot 004 - Boxplot and means",
      #                                      font = list(size = 20),
      #                                      margin = list(t = 100))
      #
      #
      #     # # # Without zerolines...
      #     plot004_factor <- plotly::layout(p = plot004_factor,
      #                                      xaxis = list(zeroline = FALSE),
      #                                      yaxis = list(zeroline = FALSE))
      #
      #     # # # Output plot004_anova...
      #     plot004_factor
      #
      #   })
      #
      #   new_plot
      # })
      ###############################################

      output$tabla01 <- renderPrint({

        req(control_user_02())

        mi_lista <- RR_general()




        # Vector con nombres de elementos a ver
        selected_objs <- c("df_inertia")


        # Usar lapply para mostrar los elementos deseados
        mi_lista[selected_objs]

      })

      output$tabla02 <- renderPrint({

        req(control_user_02())

        mi_lista <- RR_general()




        # Vector con nombres de elementos a ver
        selected_objs <- c("df_inertia")


        # Usar lapply para mostrar los elementos deseados
        mi_lista[selected_objs]

      })

      output$tabla03 <- renderPrint({

        req(control_user_02())

        mi_lista <- RR_general()




        # Vector con nombres de elementos a ver
        selected_objs <- c("df_inertia")


        # Usar lapply para mostrar los elementos deseados
        mi_lista[selected_objs]

      })

      output$tabla04 <- renderPrint({

        req(control_user_02())

        mi_lista <- RR_general()




        # Vector con nombres de elementos a ver
        selected_objs <- c("df_inertia")


        # Usar lapply para mostrar los elementos deseados
        mi_lista[selected_objs]

      })

      output$tabla05 <- renderPrint({

        req(control_user_02())

        mi_lista <- RR_general()




        # Vector con nombres de elementos a ver
        selected_objs <- c("df_table01_row_coord")


        # Usar lapply para mostrar los elementos deseados
        mi_lista[selected_objs]

      })


      output$tabla06 <- renderPrint({

        req(control_user_02())

        mi_lista <- RR_general()




        # Vector con nombres de elementos a ver
        selected_objs <- c("df_table04_col_coord")


        # Usar lapply para mostrar los elementos deseados
        mi_lista[selected_objs]

      })

      # output$tabla03 <- renderPrint({
      #
      #   req(control_user_02())
      #
      #   mi_lista <- RR_general()
      #
      #
      #
      #
      #   # Vector con nombres de elementos a ver
      #   selected_objs <- c("df_table_plot003")
      #
      #
      #   # Usar lapply para mostrar los elementos deseados
      #   mi_lista[selected_objs]
      #
      # })
      #
      # output$tabla04 <- renderPrint({
      #
      #   req(control_user_02())
      #
      #   mi_lista <- RR_general()
      #
      #
      #
      #
      #   # Vector con nombres de elementos a ver
      #   selected_objs <- c("df_table_plot004")
      #
      #
      #   # Usar lapply para mostrar los elementos deseados
      #   mi_lista[selected_objs]
      #
      # })
      ##########################################################################









      ##########################################################################




      #observe(print(input$clipbtn))

      color_button_copy <- reactiveVal("orange")


      output$clip <- renderUI({

        ns <- shiny::NS(id)

        req(RR_code())

        standard_style_button_clip <- "color: white; background-color: _color_;width: 150px; height: 50px; font-size: 20px;"
        output_style_button_clip <- gsub(pattern = "_color_",
                                         replacement = color_button_copy(),
                                         x = standard_style_button_clip)


        rclipButton(
          inputId = ns("clipbtn"),
          label = "Copy Code",
          clipText = RR_code(),
          icon = icon("clipboard"),
          tooltip = "Click me to copy the content of the text field to the clipboard!",
          options = list(delay = list(show = 800, hide = 100), trigger = "hover"),
          style = output_style_button_clip
        )
      })


      observeEvent(input$clipbtn,{

        if(input$clipbtn == 0) color_button_copy("orange") else color_button_copy("green")
      })


      observeEvent(RR_general(),{
        color_button_copy("orange")
      })


      # # # DOWNLOAD file
      color_button_download <- reactiveVal("orange")


      observeEvent(input$downloadBtn,{

        if(input$downloadBtn == 0) color_button_download("orange") else color_button_download("green")
      })



      output$downloadBtn <- downloadHandler(
        filename = function() {
          # Nombre del archivo a descargar
          paste0("script_anova_", Sys.Date(), "_descargado.R")
        },
        content = function(file) {
          # Escribir el contenido de texto en el archivo
          writeLines(RR_code(), file)
        }
      )




      output$tab05_code <- renderText({

        req(control_user_02())


        RR_code()

      })

      ##########################################################################


      # # # Calling help
      output$calling_help <- renderText({

        req(control_user_01(), control_user_02())
        ""

      })



      output$page_test <- renderUI({

        req(control_user_02())

        ns <- shiny::NS(id)

        div(
          box(
            title = "Analysis",
            status = "primary",
            id = "my_box03",
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = FALSE,
            #closable = TRUE,# Colapsado por defecto
            width = 12,
            rclipboardSetup(),
            textOutput(ns("calling_help")),

            shiny::tabsetPanel(id = ns("super_tabset_panel"),
                               selected = "Analysis",
                               tabPanel("minibase",
                                        fluidRow(
                                          column(12,
                                                 h1("Correspondence Analysis"))),
                                        fluidRow(
                                          column(12,
                                                 h3("R Object: minibase"),
                                                 h3("Comment: Only selected cols - Only rows without NA values."),
                                                 uiOutput(ns("tab01_FULL"))
                                          )
                                        )),
                               # tabPanel("AnalysisB",  # 05
                               #          fluidRow(
                               #            column(12,
                               #                   h1("Correspondence Analysis"),
                               #                   uiOutput(ns("tab02_analysisB_FULL"))
                               #            )
                               #          )
                               # ),
                               tabPanel("Analysis",  # 05
                                        fluidRow(
                                          column(12,
                                                 h1("Correspondence Analysis"),
                                                 uiOutput(ns("tab02_analysis_FULL"))
                                          )
                                        )
                               ),

                               tabPanel("Plots",  # 05,
                                        fluidRow(column(12, h1("Correspondence Analysis"))),
                                        fluidRow(
                                          #column(1),
                                          column(6, plotOutput(ns("el_plot1"), height = "40vh", width = "70vh")),
                                          column(6, verbatimTextOutput(ns("tabla01"))),
                                        ),
                                        br(),br(),br(),

                                        fluidRow(
                                          #column(1),
                                          column(6, plotOutput(ns("el_plot2"), height = "40vh", width = "70vh")),
                                          column(6, verbatimTextOutput(ns("tabla02"))),
                                        ),
                                        br(),br(),br(),

                                        fluidRow(
                                          #column(1),
                                          column(6, plotOutput(ns("el_plot3"), height = "40vh", width = "70vh")),
                                          column(6, verbatimTextOutput(ns("tabla03"))),
                                        ),
                                        br(),br(),br(),

                                        fluidRow(
                                          #column(1),
                                          column(6, plotOutput(ns("el_plot4"), height = "40vh", width = "70vh")),
                                          column(6, verbatimTextOutput(ns("tabla04"))),
                                        ),
                                        br(),br(),br(),

                                        fluidRow(
                                          #fluidRow(column(12, h2("5) Biplot simétrico"))),
                                          column(6, plotOutput(ns("el_plot5"), height = "40vh", width = "70vh")),
                                          column(6, verbatimTextOutput(ns("tabla05")), br(),
                                                    verbatimTextOutput(ns("tabla06"))),
                                        ),
                                        br(),br(),br(),

                                        #fluidRow(column(12, h2("6) Biplot Asimétrico con vectores de columnas en el espacio de las filas"))),
                                        #fluidRow(
                                        #  column(6, plotOutput(ns("el_plot6"), height = "40vh", width = "70vh")),
                                        #  column(6, verbatimTextOutput(ns("tabla06"))),
                                        #),
                                        #br(),br(),br(),


                                        #fluidRow(column(12, h2("7) Biplot Asimétrico con vectores de filas en el espacio de las columnas"))),
                                        #fluidRow(
                                          #column(1),

                                         # column(6, plotOutput(ns("el_plot7"), height = "40vh", width = "70vh")),
                                         # column(6, verbatimTextOutput(ns("tabla07"))),
                                        #),
                                        br(),br(),br()

                                        # fluidRow(column(12, h2("8) Biplot Asimétrico con vectores de filas en el espacio de las columnas"))),
                                        #
                                        # fluidRow(
                                        #   #column(1),
                                        #   column(12, plotlyOutput(ns("el_plot8"), height = "100vh", width = "140vh"))#,
                                        #   #column(6, verbatimTextOutput(ns("tabla07"))),
                                        # )
                                        # br(),br(),br(),
                                        # fluidRow(
                                        #   #column(1),
                                        #   column(6, plotlyOutput(ns("el_plot4"), height = "40vh", width = "70vh")),
                                        #   column(6, verbatimTextOutput(ns("tabla04"))),
                                        # )

                                        #shinycssloaders::withSpinner(uiOutput(ns("plot_outputs33"))),
                               ),
                               # tabPanel("Plots - Residuals",  # 05,
                               #          fluidRow(column(12, h1("Correspondence Analysisy"))),
                               #          fluidRow(
                               #            #column(1),
                               #            column(12,
                               #                   #plotOutput(ns("tab04_plots")),
                               #                   br(),
                               #                   #br()
                               #
                               #                   shinycssloaders::withSpinner(uiOutput(ns("plot_outputs66"))),
                               #            )
                               #          )
                               # ),
                               # tabPanel("Plots - Factor",  # 05,
                               #          fluidRow(h1("Correspondence Analysisy")),
                               #          fluidRow(
                               #            #column(1),
                               #            column(12,
                               #                   #plotOutput(ns("tab04_plots")),
                               #                   br(),
                               #                   #br()
                               #
                               #                   shinycssloaders::withSpinner(uiOutput(ns("tab_04_all_plotly_v01"))),
                               #            )
                               #          )
                               # ),
                               # tabPanel("Plots - Residuals",  # 05,
                               #          fluidRow(h1("Correspondence Analysisy")),
                               #          fluidRow(
                               #            #column(1),
                               #            column(12,
                               #                   #plotOutput(ns("tab04_plots")),
                               #                   br(),
                               #                   #br()
                               #
                               #                   shinycssloaders::withSpinner(uiOutput(ns("tab_05_all_plotly_residuals_v01"))),
                               #            )
                               #          )
                               # ),
                               # tabPanel("Plots2",  # 05,
                               #          fluidRow(h1("Correspondence Analysisy")),
                               #          fluidRow(
                               #            #column(1),
                               #            column(12,
                               #                   #plotOutput(ns("tab04_plots")),
                               #                   br(),
                               #                   shinycssloaders::withSpinner(uiOutput(ns("tab_04_all_plotly_v02"))),
                               #            )
                               #          )
                               # ),
                               tabPanel("Full Results",  # 05
                                        fluidRow(
                                          column(12,
                                                 h1("Correspondence Analysis"),
                                                 verbatimTextOutput(ns("tab01_all_anova_results"))
                                          )
                                        )
                               ),



                               tabPanel("R code",  # 05
                                        fluidRow(
                                          column(10,
                                                 h1("Correspondence Analysisy"),
                                                 verbatimTextOutput(ns("tab05_code"))
                                          ),
                                          br(), br(),
                                          column(2, uiOutput(ns("clip")),
                                                 br(),
                                                 downloadButton(ns("downloadBtn"), "Download Code")
                                          )
                                        )
                               )

            ), br(), br(), br(), br(), br(), br()
          )
        )
      })


    }
  )
}

