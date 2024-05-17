
# # # Module 01 - ANOVA
# UI and SERVER for all modules respect to ANOVA



module_cpiC002_s01_varselection_ui <- function(id){

  ns <- shiny::NS(id)

  div(
    uiOutput(ns("vars_selection"))
  )
}




module_cpiC002_s01_varselection_server <- function(id, input_general){
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
      output$intro_source_database <- renderTable({



        df_output <- as.data.frame(intro_source_database())
        colnames(df_output) <- names(intro_source_database())
        df_output



      })



      # # # Var selection for Resumeny
      output$vars_selection <- renderUI({

        ns <- shiny::NS(id)



        set_options <- setup_var_info(all_var_names = vector_var_names_database())
        set_options <- c("Var selection..." = "", set_options)




        div(shinyjs::useShinyjs(), id = ns("input-var-selection"),
            # fluidRow(
            #   column(12, h1("t Test  for 2 independent samples"))
            # ),
            # fluidRow(
            #   column(6,
            #          tableOutput(ns("intro_source_database")))
            # ),
            # br(), br(), br(),


            fluidRow(
              column(4,
                     fluidRow(
                       column(12,
                              selectInput(inputId = ns("vr_var_name"), label = "Response Variable (RV)",
                                          choices = set_options ,
                                          selected = set_options[1])
                       )),
                     fluidRow(
                       column(12,
                              selectInput(inputId = ns("factor_var_name"), label = "Factor",
                                          choices = set_options,
                                          selected = set_options[1])
                       ))),
              column(4,
                     fluidRow(
                       column(12,
                              selectInput(inputId = ns("alpha_value"), label = "Alpha value",
                                          choices = c(0.10, 0.05, 0.01),
                                          #choices = c(0.05),
                                          selected = 0.05)
                       ))),
              column(4, uiOutput(ns("action_buttons"))),
            ),
            fluidRow(
              column(12, textOutput(ns("calling_help")))
            )
        )

        #     column(8,
        #             fluidRow(
        #       selectInput(inputId = ns("vr_var_name"), label = "Response Variable",
        #                   choices = set_options ,
        #                   selected = set_options[1])
        #             ),
        #             fluidRow(
        #       selectInput(inputId = ns("factor_var_name"), label = "Factor",
        #                   choices = set_options,
        #                   selected = set_options[1])
        #             )
        #       ),
        #       column(4,
        #             fluidRow(
        #       selectInput(inputId = ns("alpha_value"), label = "Alpha value",
        #                   choices = c(0.10, 0.05, 0.01),
        #                   selected = 0.05)
        #          )
        #       )
        #       ,
        #   column(4, br(), br(), uiOutput(ns("action_buttons"))
        #          )
        #   ),
        # br(),
        # textOutput(ns("calling_help"))
        # )



      })



      # # # Control user 02
      control_user_02 <- reactive({

        req(control_user_01())

        validate(
          need(!is.null(input$vr_var_name), "Error 09: Module t test s01 - input$vr_var_name can not be NULL."),
          need(!is.null(input$factor_var_name), "Error 10: Module t test s01 - input$factor_var_name can not be NULL."),
          need(!is.null(input$alpha_value), "Error 11: Module t test s01 - input$alpha_value can not be NULL.")
        )

        validate(
          need(is.vector(input$vr_var_name), "Error 12: Module t test s01 - input$vr_var_name must be a vector."),
          need(is.vector(input$factor_var_name), "Error 13: Module t test s01 - input$vr_var_name must be a vector."),
          need(is.vector(input$alpha_value), "Error 14: Module t test s01 - input$alpha_value must be a vector.")
        )

        validate(
          need(length(input$vr_var_name) == 1, "Error 15: Module t test s01 - input$vr_var_name has length 1."),
          need(length(input$factor_var_name) == 1, "Error 16: Module t test s01 - input$factor_var_name has length 1."),
          need(length(input$alpha_value) == 1, "Error 17: Module t test s01 - input$alpha_value has length 1.")
        )


        validate(
          need(input$vr_var_name != "", "Select a response variable."),
          need(input$factor_var_name != "", "Select a factor."),
        )

        validate(
          need(input$vr_var_name != input$factor_var_name, "Selected variables can not be equal.")
        )



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


      # # # Reset if change file source or any options
      # # VR selection
      observeEvent(input$vr_var_name, {

        # Not show yet
        color_button_load(hardcorded_initial_color)
        color_button_show(hardcorded_initial_color)
        action_button_load(FALSE)
        action_button_show(FALSE)

      })


      # # Factor selection
      observeEvent(input$factor_var_name, {

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
      vr_var_name <- reactive({

        req(action_button_show())

        output_value <- input$vr_var_name
        return(output_value)
      })


      factor_var_name <- reactive({
        req(action_button_show())

        output_value <- input$factor_var_name
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


        the_list <- list(vr_var_name(), factor_var_name(), alpha_value(), intro_source_database())

        names(the_list) <- c("vr_var_name", "factor_var_name", "alpha_value", "intro_source_database")
        the_list
      })


      return(output_list)
    })
}






module_cpiC002_s02_rscience_ui <- function(id){

  ns <- shiny::NS(id)

  div(
    uiOutput(ns("page_test")),
    br(), br(), br(),br(), br(), br(),br(), br(), br(),br(),br(), br(), br(),
    br(), br(), br(),br(), br(), br(),br(), br(), br(),br(),br(), br(), br(),
    br(), br(), br(),br(), br(), br(),br(), br(), br(),br(),br(), br(), br()
  )

}




module_cpiC002_s02_rscience_server <- function(id, input_general, input_01_anova){
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

        check_previous <- fn_cpiC002_control_previous(database = input_general()$database,
                                                                       vr_var_name = input_01_anova()$vr_var_name,
                                                                       factor_var_name = input_01_anova()$factor_var_name,
                                                                       alpha_value = input_01_anova()$alpha_value)

        validate(
          need(check_previous$dt_ok, check_previous$text_output)
        )
        return(TRUE)
      })


      ####################################################################################


      # # # All anova results
      RR_general <- reactive({

        req(control_user_01())

        the_output <- fn_cpiC002_results(database = input_general()$database,
                                                          vr_var_name = input_01_anova()$vr_var_name,
                                                          factor_var_name = input_01_anova()$factor_var_name,
                                                          alpha_value = input_01_anova()$alpha_value)


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
        # check_post <- fn_cpiC002_control_post(list_results_from_cpiA001_anova1way = RR_general())
        # #
        # validate(
        #   need(check_post$check_ok, check_post$text_output)
        # )

        return(TRUE)
      })






      RR_code <- reactive({

        req(control_user_02())
        the_code <- fn_cpiC002_code_sectionALL(intro_source_database = input_01_anova()$intro_source_database,
                                                                vr_var_name = input_01_anova()$vr_var_name,
                                                                factor_var_name = input_01_anova()$factor_var_name,
                                                                alpha_value = input_01_anova()$alpha_value)
        the_code
      })

      ####################################################################################



      # # # Tab01 - Anova Results
      output$tab01_all_anova_results <- renderPrint({

        req(control_user_02())

        RR_general()
      })

      ##########################################################################

      # # #Tab02 - Residuals Requeriments
      output$tab02_requeriments_obj01 <- renderPrint({

        req(control_user_02())


        mi_lista <- RR_general()




        # Vector con nombres de elementos a ver
        nombres_a_ver <- c("test_residuals_normality")

        # Usar lapply para mostrar los elementos deseados
        mi_lista[nombres_a_ver]

      })
      output$tab02_requeriments_obj02 <- renderPrint({

        req(control_user_02())


        mi_lista <- RR_general()




        # Vector con nombres de elementos a ver
        nombres_a_ver <- c("test_residuals_homogeneity")

        # Usar lapply para mostrar los elementos deseados
        mi_lista[nombres_a_ver]

      })
      output$tab02_requeriments_obj03 <- renderPrint({

        req(control_user_02())


        mi_lista <- RR_general()




        # Vector con nombres de elementos a ver
        nombres_a_ver <- c("df_residuals_variance_levels")

        # Usar lapply para mostrar los elementos deseados
        mi_lista[nombres_a_ver]

      })

      output$tab02_analysis_anova_FULL <- renderUI({

        ns <- shiny::NS(id)

        div(
          h2("1) Requeriment - Normality test - Residuals"),
          verbatimTextOutput(ns("tab02_requeriments_obj01")),
          br(), br(), br(),

          h2("2) Requeriment - Homogeneity test - Residuals"),
          verbatimTextOutput(ns("tab02_requeriments_obj02")),
          br(), br(), br(),

          h2("3) Estimated variances - Residuals"),
          verbatimTextOutput(ns("tab02_requeriments_obj03")),
          br(), br(), br()
        )

      })

      output$tab02_requeriments <- renderPrint({

        req(control_user_02())


        mi_lista <- RR_general()




        # Vector con nombres de elementos a ver
        nombres_a_ver <- c("test_residuals_normality",
                           "test_residuals_homogeneity",
                           "df_residuals_variance_levels")

        # Usar lapply para mostrar los elementos deseados
        elementos_a_ver <- lapply(nombres_a_ver, function(nombre) mi_lista[[nombre]])
        names(elementos_a_ver) <- nombres_a_ver
        elementos_a_ver

      })

      ##########################################################################


      #################################################################
      # # # Tab 05 - Analysis resume...
         output$tab03_analysis_anova_obj01_B <- renderDT({

        req(control_user_02())

        mi_lista <- RR_general()




        # Vector con nombres de elementos a ver
        selected_objs <- c("df_selected_vars")


        # Usar lapply para mostrar los elementos deseados

        mi_tabla <- mi_lista[[selected_objs]]
        #https://rstudio.github.io/DT/functions.html
        vector_pos <- 1:nrow(mi_tabla)
        vector_color <- rep(NA, length(vector_pos))
        vector_color[c(T, F)] <- "lightblue"#'red'#
        vector_color[c(F, T)] <- "lightgreen"#'blue'#
        vector_color <- vector_color[vector_pos]

        datatable(
          mi_tabla,
          rownames = FALSE,
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

      output$tab03_analysis_anova_FULL_B <- renderUI({

        ns <- shiny::NS(id)

        div(
          h2("1) References"),
          h3("R Object: df_selected_vars"),
          DTOutput(ns("tab03_analysis_anova_obj01_B")),
          br(), br(), br()#,#

          # h2("2) Factor resumen"),
          # verbatimTextOutput(ns("tab03_analysis_anova_obj02_B")),
          # br(), br(), br()

          # h2("3) Requeriment - Normality from each group"),
          # verbatimTextOutput(ns("tab03_analysis_anova_obj03")),
          # br(), br(), br(),
          # h2("3) Normality"),
          # verbatimTextOutput(ns("tab03_analysis_anova_obj03_B")),
          # br(), br(), br(),

          # h2("4) Homogeneity"),
          # verbatimTextOutput(ns("tab03_analysis_anova_obj04_B")),
          # br(), br(), br(),


        )

      })


      ##################################################

      output$tab33_position_levels <- renderDT({

        req(control_user_02())

        mi_lista <- RR_general()




        # Vector con nombres de elementos a ver
        selected_objs <- c("df_vr_position_levels")


        # Usar lapply para mostrar los elementos deseados

        mi_tabla <- mi_lista[[selected_objs]]
        #https://rstudio.github.io/DT/functions.html
        vector_pos <- 1:nrow(mi_tabla)
        vector_color <- rep(NA, length(vector_pos))
        vector_color[c(T, F)] <- "lightblue"#'red'#
        vector_color[c(F, T)] <- "lightgreen"#'blue'#
        vector_color <- vector_color[vector_pos]

        datatable(
          mi_tabla,
          rownames = FALSE,
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

        ) %>% formatRound(columns= c("min", "max", "mean", "Q1", "median.Q2", "Q3", "max"), digits=4) %>%
        formatStyle(
          colnames(mi_tabla),
          backgroundColor = styleRow(vector_pos, vector_color),#,
          target = 'row',
          fontSize = "26px"
        )
        # %>% formatStyle(
        #   4,
        #              decimal.mark = ".", # Cambiar el separador decimal
        #              digits = 2)
      })

      output$tab33_dispersion_levels <- renderDT({

        req(control_user_02())

        mi_lista <- RR_general()




        # Vector con nombres de elementos a ver
        selected_objs <- c("df_vr_dispersion_levels")


        # Usar lapply para mostrar los elementos deseados

        mi_tabla <- mi_lista[[selected_objs]]
        #https://rstudio.github.io/DT/functions.html
        vector_pos <- 1:nrow(mi_tabla)
        vector_color <- rep(NA, length(vector_pos))
        vector_color[c(T, F)] <- "lightblue"#'red'#
        vector_color[c(F, T)] <- "lightgreen"#'blue'#
        vector_color <- vector_color[vector_pos]

        datatable(
          mi_tabla,
          rownames = FALSE,
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

        ) %>% formatRound(columns= c("range", "variance", "standard_deviation", "standard_error", "percentage_coefficient_variation"), digits=4) %>%
          formatStyle(
          colnames(mi_tabla),
          backgroundColor = styleRow(vector_pos, vector_color),#,
          target = 'row',
          fontSize = "26px"
        )
      })
      #############################################################
      output$el_plot1 <- renderPlotly({


        mi_lista <- RR_general()


        new_plot <-  with(mi_lista,{
          # # # Create a new plot...
          # # # Create a new plot...
          # # # Create a new plot...
          plot001_factor <- plotly::plot_ly()

          # # # Plot001 - Scatter plot for VR and FACTOR on minibase_mod *****************
          plot001_factor <- plotly::add_trace(p = plot001_factor,
                                              type = "scatter",
                                              mode = "markers",
                                              x = minibase_mod$FACTOR,
                                              y = minibase_mod$RV,
                                              color = minibase_mod$FACTOR,
                                              colors = df_factor_info$color,
                                              marker = list(size = 15, opacity = 0.7))

          # # # Title and settings...
          plot001_factor <-   plotly::layout(p = plot001_factor,
                                             xaxis = list(title = "FACTOR"),
                                             yaxis = list(title = "RV"),
                                             title = "Plot 001 - Scatterplot",
                                             font = list(size = 20),
                                             margin = list(t = 100))



          # # # Without zerolines
          plot001_factor <-   plotly::layout(p = plot001_factor,
                                             xaxis = list(zeroline = FALSE),
                                             yaxis = list(zeroline = FALSE))


          # # # Plot output
          plot001_factor

        })

        new_plot
      })

      output$el_plot2 <- renderPlotly({


        mi_lista <- RR_general()


        new_plot <-  with(mi_lista,{
          # # # Create a new plot...
          # # # Create a new plot...
          plot002_factor <- plot_ly()


          # # # Adding errors...
          plot002_factor <-   add_trace(p = plot002_factor,
                                        type = "scatter",
                                        mode = "markers",
                                        x = df_table_plot002$level,
                                        y = df_table_plot002$mean,
                                        color = df_table_plot002$level,
                                        colors = df_table_plot002$color,
                                        marker = list(symbol = "line-ew-open",
                                                      size = 50,
                                                      opacity = 1,
                                                      line = list(width = 5)),
                                        error_y = list(type = "data", array = df_table_plot002$standard_deviation)
          )


          #plot002_factor

          plot002_factor <- plotly::layout(p = plot002_factor,
                                           xaxis = list(title = "FACTOR"),
                                           yaxis = list(title = "RV"),
                                           title = "Plot 002 - Mean and Standard Deviation",
                                           font = list(size = 20),
                                           margin = list(t = 100))

          # # # Without zerolines
          plot002_factor <-plotly::layout(p = plot002_factor,
                                          xaxis = list(zeroline = FALSE),
                                          yaxis = list(zeroline = FALSE))


          # # # Plot output
          plot002_factor

        })

        new_plot
      })

      output$el_plot3 <- renderPlotly({


        mi_lista <- RR_general()


        new_plot <-  with(mi_lista,{
          # # # Create a new plot...
          # # # Create a new plot...
          plot003_factor <- plot_ly()


          # # # Adding errors...
          plot003_factor <-   add_trace(p = plot003_factor,
                                        type = "scatter",
                                        mode = "markers",
                                        x = df_table_plot003$level,
                                        y = df_table_plot003$mean,
                                        color = df_table_plot003$level,
                                        colors = df_table_plot003$color,
                                        marker = list(symbol = "line-ew-open",
                                                      size = 50,
                                                      opacity = 1,
                                                      line = list(width = 5)),
                                        error_y = list(type = "data", array = df_table_plot003$standard_error)
          )


          #plot002_factor

          plot003_factor <- plotly::layout(p = plot003_factor,
                                           xaxis = list(title = "FACTOR"),
                                           yaxis = list(title = "RV"),
                                           title = "Plot 003 - Mean and Standard Error",
                                           font = list(size = 20),
                                           margin = list(t = 100))

          # # # Without zerolines
          plot003_factor <-plotly::layout(p = plot003_factor,
                                          xaxis = list(zeroline = FALSE),
                                          yaxis = list(zeroline = FALSE))


          # # # Plot output
          plot003_factor

        })

        new_plot
      })

      output$el_plot4 <- renderPlotly({


        mi_lista <- RR_general()


        new_plot <-  with(mi_lista,{
          # # # Create a new plot...
          # # # Create a new plot...
          # # # New plotly...
          plot004_factor <- plotly::plot_ly()

          # # # Boxplot and info...
          plot004_factor <- plotly::add_trace(p = plot004_factor,
                                              type = "box",
                                              x = df_table_plot004$level ,
                                              color = df_table_plot004$level,
                                              colors = df_table_plot004$color,
                                              lowerfence = df_table_plot004$min,
                                              q1 = df_table_plot004$Q1,
                                              median = df_table_plot004$median,
                                              q3 = df_table_plot004$Q3,
                                              upperfence = df_table_plot004$max,
                                              boxmean = TRUE,
                                              boxpoints = FALSE,
                                              line = list(color = "black", width = 3)
          )

          # Title and labels for axis
          plot004_factor <- plotly::layout(p = plot004_factor,
                                           title = "Plot 004 - Boxplot and means",
                                           xaxis = list(title = "FACTOR"),
                                           yaxis = list(title = "RV"),
                                           font = list(size = 20),
                                           margin = list(t = 100))


          # # # Without zerolines...
          plot004_factor <- plotly::layout(p = plot004_factor,
                                           xaxis = list(zeroline = FALSE),
                                           yaxis = list(zeroline = FALSE))

          # # # Output plot004_anova...
          plot004_factor

        })

        new_plot
      })

      output$el_plot5 <- renderPlotly({


        mi_lista <- RR_general()


        new_plot <-  with(mi_lista,{
          # # # Create a new plot...
          # # # Create a new plot...
          # # # New plotly...
          plot003_residuals <- plotly::plot_ly()

          # Add traces
          plot003_residuals <- plotly::add_trace(p = plot003_residuals,
                                                 type = "violin",
                                                 x = minibase_mod$RV,
                                                 showlegend = TRUE,
                                                 side = "positive",
                                                 points = FALSE,
                                                 #name = levels(minibase_mod$FACTOR)[minibase_mod$lvl_order_number],
                                                 color = minibase_mod$FACTOR,
                                                 colors = df_table_plot005$color)



          # # # Title and settings...
          plot003_residuals <- plotly::layout(p = plot003_residuals,
                                              title = "Plot 005 - Smoothed",
                                              xaxis = list(title = "RV"),
                                              yaxis = list(title = "FACTOR"),
                                              font = list(size = 20),
                                              margin = list(t = 100))


          # # # Without zerolines...
          plot003residuals <- plotly::layout(p = plot003_residuals,
                                             xaxis = list(zeroline = FALSE),
                                             yaxis = list(zeroline = FALSE))

          # # # Output plot003_anova...
          plot003_residuals

        })

        new_plot
      })

      output$el_plot6 <- renderPlotly({


        mi_lista <- RR_general()


        new_plot <-  with(mi_lista,{
          # # # Create a new plot...
          # # # Create a new plot...
          # # # New plotly...

          # Convertir cyl a factor
          #mtcars$cyl <- as.factor(mtcars$cyl)

          # Dividir los datos por los niveles del factor
          data_split <- split(minibase, minibase$FACTOR)

          # Crear un gráfico vacío
          el_plot6 <- plot_ly()

          # Usar lapply para agregar un histograma para cada nivel del factor
          lapply(names(data_split), function(i) {
            # Calcular los bins y las frecuencias relativas
            hist_data <- hist(data_split[[i]]$RV, plot = FALSE)
            relative_freq <- hist_data$counts / sum(hist_data$counts)

            # Calcular el ancho de las barras
            bar_width <- diff(range(hist_data$breaks)) / length(hist_data$counts)

            # Agregar el gráfico de barras al gráfico
            el_plot6 <<- add_trace(el_plot6, x = hist_data$mids,
                                   y = relative_freq, name = i,
                                   type = "bar", opacity = 0.6, width = bar_width)
          })

          # Cambiar el modo de superposición para que los histogramas se puedan ver claramente
          el_plot6 <- layout(el_plot6, barmode = "overlay")

          # # # Title and settings...
          el_plot6 <- plotly::layout(p = el_plot6,
                                              title = "Plot 006 - Histogram",
                                              xaxis = list(title = "RV"),
                                              yaxis = list(title = "Relative Frecuency"),
                                              font = list(size = 20),
                                              margin = list(t = 100))


          # # # Without zerolines...
          el_plot6 <- plotly::layout(p = el_plot6,
                                             xaxis = list(zeroline = FALSE),
                                             yaxis = list(zeroline = FALSE))

          # Imprimir el gráfico
          el_plot6
          #print(p)

        })

        new_plot
      })
      ###############################################

      output$tabla01 <- renderPrint({

        req(control_user_02())

        mi_lista <- RR_general()




        # Vector con nombres de elementos a ver
        selected_objs <- c("df_table_plot001")


        # Usar lapply para mostrar los elementos deseados
        mi_lista[selected_objs]

      })

      output$tabla02 <- renderPrint({

        req(control_user_02())

        mi_lista <- RR_general()




        # Vector con nombres de elementos a ver
        selected_objs <- c("df_table_plot002")


        # Usar lapply para mostrar los elementos deseados
        mi_lista[selected_objs]

      })

      output$tabla03 <- renderPrint({

        req(control_user_02())

        mi_lista <- RR_general()




        # Vector con nombres de elementos a ver
        selected_objs <- c("df_table_plot003")


        # Usar lapply para mostrar los elementos deseados
        mi_lista[selected_objs]

      })

      output$tabla04 <- renderPrint({

        req(control_user_02())

        mi_lista <- RR_general()




        # Vector con nombres de elementos a ver
        selected_objs <- c("df_table_plot004")


        # Usar lapply para mostrar los elementos deseados
        mi_lista[selected_objs]

      })

      output$tabla05 <- renderPrint({

        req(control_user_02())

        mi_lista <- RR_general()




        # Vector con nombres de elementos a ver
        selected_objs <- c("df_table_plot005")


        # Usar lapply para mostrar los elementos deseados
        mi_lista[selected_objs]

      })

      output$tabla06 <- renderPrint({

        req(control_user_02())

        mi_lista <- RR_general()




        # Vector con nombres de elementos a ver
        selected_objs <- c("df_table_plot006")


        # Usar lapply para mostrar los elementos deseados
        mi_lista[selected_objs]

      })
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
          rclipboardSetup(),
          box(
            title = "Summary - 2 Var (Cuantitative and Qualitative)",
            status = "primary",
            id = ns("my_box03C"),
            solidHeader = TRUE,
            collapsible = TRUE,
            closable = FALSE,# Colapsado por defecto
            collapsed = FALSE,
            width = 12,
          textOutput(ns("calling_help")),

          shiny::tabsetPanel(id = ns("super_tabset_panel"),
                             # tabPanel("Analysis",  # 05
                             #          fluidRow(
                             #            column(12,
                             #                   h1("Resumen"),
                             #                   uiOutput(ns("tab03_analysis_anova_FULL"))
                             #            )
                             #          )
                             # ),
                             tabPanel("Reference",  # 05
                                      fluidRow(
                                        column(12,
                                               h1("Summary - 2 Var (Cuantitative and Qualitative)"),
                                               uiOutput(ns("tab03_analysis_anova_FULL_B"))
                                        )
                                      )
                             ),

                             tabPanel("Summary",
                                      fluidRow(
                                        column(12,
                                               h1("Summary - 2 Var (Cuantitative and Qualitative)")
                                               )
                                        ),
                                      fluidRow(
                                        column(12,
                                               h2("1) Position"),
                                               h3("R Object: df_vr_position_levels"),
                                               DTOutput(ns("tab33_position_levels"))
                                        )
                                      ),
                                      br(), br(), br(),

                                      fluidRow(
                                        column(12,
                                               h2("2) Dispersion"),
                                               h3("R Object: df_vr_dispersion_levels"),
                                               DTOutput(ns("tab33_dispersion_levels"))
                                        )
                                      ),
                                      br(), br(), br(),

                             ),
                             tabPanel("Plots - Raw Data",  # 05,
                                      fluidRow(column(12, h1("Summary - 2 Var (Cuantitative and Qualitative)"))),
                                      fluidRow(
                                        #column(1),
                                        column(6, plotlyOutput(ns("el_plot1"), height = "40vh", width = "70vh")),
                                        column(6, verbatimTextOutput(ns("tabla01"))),
                                      ),
                                      br(),br(),br(),

                                      fluidRow(
                                        #column(1),
                                        column(6, plotlyOutput(ns("el_plot2"), height = "40vh", width = "70vh")),
                                        column(6, verbatimTextOutput(ns("tabla02"))),
                                      ),
                                      br(),br(),br(),

                                      fluidRow(
                                        #column(1),
                                        column(6, plotlyOutput(ns("el_plot3"), height = "40vh", width = "70vh")),
                                        column(6, verbatimTextOutput(ns("tabla03"))),
                                      ),
                                      br(),br(),br(),
                                      fluidRow(
                                        #column(1),
                                        column(6, plotlyOutput(ns("el_plot4"), height = "40vh", width = "70vh")),
                                        column(6, verbatimTextOutput(ns("tabla04"))),
                                      ),
                                      br(),br(),br(),
                                      fluidRow(
                                        #column(1),
                                        column(6, plotlyOutput(ns("el_plot5"), height = "40vh", width = "70vh")),
                                        column(6, verbatimTextOutput(ns("tabla05"))),
                                      ),
                                      br(),br(),br(),
                                      fluidRow(
                                        #column(1),
                                        column(6, plotlyOutput(ns("el_plot6"), height = "40vh", width = "70vh")),
                                        column(6, verbatimTextOutput(ns("tabla06"))),
                                      )

                                      #shinycssloaders::withSpinner(uiOutput(ns("plot_outputs33"))),
                             ),
                             # tabPanel("Plots - Residuals",  # 05,
                             #          fluidRow(column(12, h1("Resumeny"))),
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
                             #          fluidRow(h1("Resumeny")),
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
                             #          fluidRow(h1("Resumeny")),
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
                             #          fluidRow(h1("Resumeny")),
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
                                               h1("Summary - 2 Var (Cuantitative and Qualitative)"),
                                               verbatimTextOutput(ns("tab01_all_anova_results"))
                                        )
                                      )
                             )#,



                             # tabPanel("R code",  # 05
                             #          fluidRow(
                             #            column(10,
                             #                   h1("Summary - 2 Var (Cuantitative and Qualitative)"),
                             #                   verbatimTextOutput(ns("tab05_code"))
                             #            ),
                             #            br(), br(),
                             #            column(2, uiOutput(ns("clip")),
                             #                   br(),
                             #                   downloadButton(ns("downloadBtn"), "Download Code")
                             #            )
                             #          )
                             # )

          ), br(), br(), br(), br(), br(), br()
          ), br(), br(), br(), br(), br(), br()
        )
      })


    }
  )
}

