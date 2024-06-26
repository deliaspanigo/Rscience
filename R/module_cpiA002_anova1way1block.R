
# # # Module 01 - ANOVA
# UI and SERVER for all modules respect to ANOVA



module_cpiA002_s01_varselection_ui <- function(id){

  ns <- shiny::NS(id)

  div(
    uiOutput(ns("vars_selection"))
  )
}




module_cpiA002_s01_varselection_server <- function(id, input_general){
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
          need(!is.null(input_general), "Error 01: Module anova s01 - input_general can not be NULL."),
          need(!is.null(vector_var_names_database()), "Error 10: Module anova s01 - vector_var_names_database() can not be NULL."),
          need(!is.null(intro_source_database()), "Error 11: Module anova s01 - intro_source_database() can not be NULL.")
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



      # # # Var selection for anova 1 way
      output$vars_selection <- renderUI({

        ns <- shiny::NS(id)



        set_options <- setup_var_info(all_var_names = vector_var_names_database())
        set_options <- c("Var selection..." = "", set_options)




        div(shinyjs::useShinyjs(), id = ns("input-var-selection"),
            fluidRow(
              column(6, h1("ANOVA 1 Way with 1 Block"))
            ),
            fluidRow(
              column(6,
                     tableOutput(ns("intro_source_database")))
            ),
            br(), br(), br(),


            fluidRow(
              column(2,
                     selectInput(inputId = ns("vr_var_name"), label = "Response Variable",
                                 choices = set_options ,
                                 selected = set_options[1])
              ),
              column(2,
                     selectInput(inputId = ns("factor_var_name"), label = "Factor",
                                 choices = set_options,
                                 selected = set_options[1])
              ),
              column(2,
                     selectInput(inputId = ns("block_var_name"), label = "Block",
                                 choices = set_options,
                                 selected = set_options[1])
              ),

              column(2,
                     selectInput(inputId = ns("alpha_value"), label = "Alpha value",
                                 choices = c(0.10, 0.05, 0.01),
                                 selected = 0.05)
              ),
              column(4, br(), br(), uiOutput(ns("action_buttons"))
              )
            ),
            br(),
            textOutput(ns("calling_help"))
        )



      })



      # # # Control user 02
      control_user_02 <- reactive({

        req(control_user_01())

        validate(
          need(!is.null(input$vr_var_name), "Error 09: Module anova s01 - input$vr_var_name can not be NULL."),
          need(!is.null(input$factor_var_name), "Error 10: Module anova s01 - input$factor_var_name can not be NULL."),
          need(!is.null(input$block_var_name), "Error 10: Module anova s01 - input$block_var_name can not be NULL."),
          need(!is.null(input$alpha_value), "Error 11: Module anova s01 - input$alpha_value can not be NULL.")
        )

        validate(
          need(is.vector(input$vr_var_name), "Error 12: Module anova s01 - input$vr_var_name must be a vector."),
          need(is.vector(input$factor_var_name), "Error 13: Module anova s01 - input$factor_var_name must be a vector."),
          need(is.vector(input$block_var_name), "Error 13: Module anova s01 - input$block_var_name must be a vector."),
          need(is.vector(input$alpha_value), "Error 14: Module anova s01 - input$alpha_value must be a vector.")
        )

        validate(
          need(length(input$vr_var_name) == 1, "Error 15: Module anova s01 - input$vr_var_name has length 1."),
          need(length(input$factor_var_name) == 1, "Error 16: Module anova s01 - input$factor_var_name has length 1."),
          need(length(input$block_var_name) == 1, "Error 16: Module anova s01 - input$block_var_name has length 1."),
          need(length(input$alpha_value) == 1, "Error 17: Module anova s01 - input$alpha_value has length 1.")
        )


        validate(
          need(input$vr_var_name != "", "Select a response variable."),
          need(input$factor_var_name != "", "Select a factor."),
          need(input$block_var_name != "", "Select a block.")
        )

        validate(
          need(input$vr_var_name != input$factor_var_name, "Selected variables can not be equal."),
          need(input$vr_var_name != input$block_var_name, "Selected variables can not be equal."),
          need(input$factor_var_name != input$block_var_name, "Selected variables can not be equal.")
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


      block_var_name <- reactive({
        req(action_button_show())

        output_value <- input$block_var_name
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


        the_list <- list(vr_var_name(), factor_var_name(), block_var_name(), alpha_value(), intro_source_database())

        names(the_list) <- c("vr_var_name", "factor_var_name", "block_var_name", "alpha_value", "intro_source_database")
        the_list
      })


      return(output_list)
    })
}






module_cpiA002_s02_rscience_ui <- function(id){

  ns <- shiny::NS(id)

  uiOutput(ns("page_test"))

}




module_cpiA002_s02_rscience_server <- function(id, input_general, input_01_anova){
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

        check_previous <- fn_cpiA002_control_previous(database = input_general()$database,
                                                             vr_var_name = input_01_anova()$vr_var_name,
                                                             factor_var_name = input_01_anova()$factor_var_name,
                                                             block_var_name = input_01_anova()$block_var_name,
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

        the_output <- fn_cpiA002_gen02(database = input_general()$database,
                                       vr_var_name = input_01_anova()$vr_var_name,
                                       factor_var_name = input_01_anova()$factor_var_name,
                                       block_var_name = input_01_anova()$block_var_name,
                                       alpha_value = input_01_anova()$alpha_value)


        the_output

      })



      # # # Control for initial inputs
      control_user_02 <- reactive({
        #
        req(control_user_01())
        #
        # validate(
        #   need(!is.null(RR_general()$"R_results"$"p01_test"), "Error 03: Module anova s02 - RR_general() can not be NULL.")
        # )
        # #
        # #
        # #
        # #
        # #         # Control post
        # check_post <- fn_cpiA002_control_p01_test(all_results = RR_general()$"R_results"$"p01_test")
        # #
        # validate(
        #   need(check_post$check_ok, check_post$text_output)
        # )

        return(TRUE)
      })



if(FALSE){
      # RR - Tables
      RR_g01_tables <- reactive({

        req(control_user_02())
        all_tables_g01 <- cpiA002_anova1way1block_recruit_g01_Tables(list_results_from_cpiA001_anova1way = RR_general())
        all_tables_g01
      })


      # RR - Plots
      RR_g01_plots <- reactive({

        req(control_user_02())
        all_plots_g01 <- cpiA001_anova1way_recruit_g01_FactorPlots(list_results_from_cpiA001_anova1way = RR_general())
        all_plots_g01
      })




      # RR - Tables
      RR_g02_tables <- reactive({

        req(control_user_02())
        all_tables_g02 <- cpiA001_anova1way_recruit_g02_Tables(list_results_from_cpiA001_anova1way = RR_general())
        all_tables_g02
      })


      # RR - Plots
      RR_g02_plots <- reactive({

        req(control_user_02())
        all_plots_g02 <- cpiA001_anova1way_recruit_g02_ResidualsPlots(list_results_from_cpiA001_anova1way = RR_general())
        all_plots_g02
      })

      RR_code <- reactive({

        req(control_user_02())
        the_code <- cpiA001_anova1way_code_sectionALL(intro_source_database = input_01_anova()$intro_source_database,
                                                      vr_var_name = input_01_anova()$vr_var_name,
                                                      factor_var_name = input_01_anova()$factor_var_name,
                                                      alpha_value = input_01_anova()$alpha_value)
        the_code
      })

      ####################################################################################


      observe({
        for (i in 1:length(RR_g01_plots())) {
          # Need local so that each item gets its own number. Without it, the value
          # of i in the renderPlot() will be the same across all instances, because
          # of when the expression is evaluated.
          local({
            my_i <- i
            plot_name <- paste("plotA", my_i, sep="")
            table_name <- paste("tableA", my_i, sep="")

            output[[plot_name]] <- plotly::renderPlotly({
              RR_g01_plots()[[my_i]]
            })

            output[[table_name]] <- renderPrint({

              req(control_user_02())

              my_lista <-RR_g01_tables()
              vector_search <- names(my_lista)[my_i]

              # Filtered list
              filtered_list <- my_lista[vector_search]

              filtered_list

            })

          })
        }
      })

      output$plot_outputs33 <- renderUI({
        ns <- shiny::NS(id)
        plot_output_list <- lapply(1:length(RR_g01_plots()), function(i) {
          plot_name <- paste("plotA", i, sep="")
          table_name <- paste("tableA", i, sep="")


          div(
            fluidRow(
              column(6, plotlyOutput(ns(plot_name), height = "100%", width = "100%")),
              column(6, verbatimTextOutput(ns(table_name)))
            ), br(), br(), br()
          )
        })
      })

      ######################################################################


      observe({
        for (i in 1:length(RR_g02_plots())) {
          # Need local so that each item gets its own number. Without it, the value
          # of i in the renderPlot() will be the same across all instances, because
          # of when the expression is evaluated.
          local({
            my_i <- i
            plot_name <- paste("plotB", my_i, sep="")
            table_name <- paste("tableB", my_i, sep="")

            output[[plot_name]] <- plotly::renderPlotly({
              RR_g02_plots()[[my_i]]
            })

            output[[table_name]] <- renderPrint({

              req(control_user_02())

              my_lista <-RR_g02_tables()
              vector_search <- names(my_lista)[my_i]

              # Filtered list
              filtered_list <- my_lista[vector_search]

              filtered_list

            })

          })
        }
      })

      output$plot_outputs66 <- renderUI({
        ns <- shiny::NS(id)
        plot_output_list <- lapply(1:length(RR_g02_plots()), function(i) {
          plot_name <- paste("plotB", i, sep="")
          table_name <- paste("tableB", i, sep="")


          div(
            fluidRow(
              column(6, plotlyOutput(ns(plot_name), height = "100%", width = "100%")),
              column(6, verbatimTextOutput(ns(table_name)))
            ), br(), br(), br()
          )
        })
      })

      ######################################################################


      # # # #Tab02 - Residuals Requeriments
      # output$tab02_requeriments <- renderPrint({
      #
      #   req(control_user_02())
      #
      #
      #   mi_lista <- RR_general()
      #
      #
      #
      #
      #   # Vector con nombres de elementos a ver
      #   nombres_a_ver <- c("test_residuals_normality",
      #                      "test_residuals_homogeneity",
      #                      "df_residuals_variance_levels")
      #
      #   # Usar lapply para mostrar los elementos deseados
      #   elementos_a_ver <- lapply(nombres_a_ver, function(nombre) mi_lista[[nombre]])
      #   names(elementos_a_ver) <- nombres_a_ver
      #   elementos_a_ver
      #
      # })

      ##########################################################################

      # # # Tab 05 - Analysis resume...
      output$tab03_analysis_anova <- renderPrint({

        req(control_user_02())

        mi_lista <- RR_general()




        # Vector con nombres de elementos a ver
        nombres_a_ver <- c("df_selected_vars",
                           "df_table_anova", "df_factor_info", "check_unbalanced_reps",
                           "df_tukey_table",
                           "df_model_error")
        # nombres_a_ver <- c("df_selected_vars", "df_factor", "dt_unbalanced_reps", "lm_ancova_with",
        #                    "test_normality_residuals", "test_homogeneity_residuals",
        #                    "sum_residuos", "table_ancova_with",
        #                    "df_resumen_ancova_with_large", "df_resumen_ancova_with_short",
        #                    "df_tukey", "tukey01_full_groups", "tukey02_full_pairs")

        # Usar lapply para mostrar los elementos deseados
        elementos_a_ver <- lapply(nombres_a_ver, function(nombre) mi_lista[[nombre]])
        names(elementos_a_ver) <- nombres_a_ver
        elementos_a_ver

      })

      ##########################################################################




      output$tab05_code <- renderText({

        req(control_user_02())


        RR_code()

      })



}



############################################################################
      observe({
        for (i in 1:length(RR_general()$"out03A_plots_factor")) {
          # Need local so that each item gets its own number. Without it, the value
          # of i in the renderPlot() will be the same across all instances, because
          # of when the expression is evaluated.
          local({
            my_i <- i
            plot_name <- paste("plotA", my_i, sep="")
            table_name <- paste("tableA", my_i, sep="")

            output[[plot_name]] <- plotly::renderPlotly({
              RR_general()$"out03A_plots_factor"[[my_i]]
            })

            output[[table_name]] <- renderPrint({

              req(control_user_02())

              my_lista <- RR_general()$"out03B_tables_factor"
              vector_search <- names(my_lista)[my_i]

              # Filtered list
              filtered_list <- my_lista[vector_search]

              filtered_list

            })

          })
        }
      })

      output$plot_outputs33 <- renderUI({
        ns <- shiny::NS(id)
        plot_output_list <- lapply(1:length(RR_general()$"out03A_plots_factor"), function(i) {
          plot_name <- paste("plotA", i, sep="")
          table_name <- paste("tableA", i, sep="")


          div(
            fluidRow(
              column(6, plotlyOutput(ns(plot_name), height = "100%", width = "100%")),
              column(6, verbatimTextOutput(ns(table_name)))
            ), br(), br(), br()
          )
        })
      })

##############################################################
      # # # Tab01 - Analysis
      output$tab01_analysis <- renderPrint({

        req(control_user_02())

        RR_general()$"out01_analysis"
      })


      # # # Tab01 - Analysis
      output$tab01_analysis_v02_obj01 <- renderPrint({

        req(control_user_02())

        all_resutls <- RR_general()$"out01_analysis"
        selected_results <- "df_selected_vars"

        all_resutls[selected_results]
      })
      output$tab01_analysis_v02_obj02 <- renderPrint({

        req(control_user_02())

        all_resutls <- RR_general()$"out01_analysis"
        selected_results <- "df_table_anova"

        all_resutls[selected_results]
      })
      output$tab01_analysis_v02_obj03 <- renderPrint({

        req(control_user_02())

        all_resutls <- RR_general()$"out01_analysis"
        selected_results <- c("df_factor_info", "check_unbalanced_reps")

        all_resutls[selected_results]
      })
      output$tab01_analysis_v02_obj04 <- renderPrint({

        req(control_user_02())

        all_resutls <- RR_general()$"out01_analysis"
        selected_results <- "df_tukey_table"

        all_resutls[selected_results]
      })
      output$tab01_analysis_v02_obj05 <- renderPrint({

        req(control_user_02())

        all_resutls <- RR_general()$"out01_analysis"
        selected_results <- "df_model_error"

        all_resutls[selected_results]
      })


      output$tab01_analysis_FULL <- renderUI({

        ns <- shiny::NS(id)

        div(
          h2("1) References"),
          verbatimTextOutput(ns("tab01_analysis_v02_obj01")),
          br(), br(), br(),

          h2("2) Anova 1 way and 1 block - Table"),
          verbatimTextOutput(ns("tab01_analysis_v02_obj02")),
          br(), br(), br(),

          h2("3)Factor resumen"),
          verbatimTextOutput(ns("tab01_analysis_v02_obj03")),
          br(), br(), br(),

          h2("4) Multiple comparation test (Tukey)"),
          verbatimTextOutput(ns("tab01_analysis_v02_obj04")),
          br(), br(), br(),

          h2("5) Model Error"),
          verbatimTextOutput(ns("tab01_analysis_v02_obj05")),
          br(), br(), br()
        )

      })
####################################################################
      # # # Tab02 - Requeriments
      output$tab02_table_req <- renderPrint({

        req(control_user_02())

        RR_general()$"out03B_tables_factor"[[1]]

      })
      output$tab02_plot_req <-  plotly::renderPlotly({
        RR_general()$"out03A_plots_factor"[[1]]
      })

      #########################################################
      output$tab02_requeriments <- renderPrint({

        req(control_user_02())

        RR_general()$"out02_requeriments"
      })

      output$tab02_requeriments_obj01 <- renderPrint({

        req(control_user_02())

        RR_general()$"out02_requeriments"[[1]]
      })
      output$tab02_requeriments_obj02 <- renderPrint({

        req(control_user_02())

        RR_general()$"out02_requeriments"[[2]]
      })

      output$tab02_requeriments_obj03 <- renderPrint({

        req(control_user_02())

        RR_general()$"out02_requeriments"[[2]]
      })


      output$tab02_requeriments_FULL <- renderUI({

        ns <- shiny::NS(id)

        div(
          h2("2) Requeriment - Normal distribution from residuals"),
          verbatimTextOutput(ns("tab02_requeriments_obj01")),
          br(), br(), br(),

          h2("3) Requeriment - Homogeinity from residuals"),
          verbatimTextOutput(ns("tab02_requeriments_obj02")),
          br(), br(), br(),

          h2("4) Estimated variance from residuals"),
          verbatimTextOutput(ns("tab02_requeriments_obj03")),
          br(), br(), br()
        )

      })
      ############################################################

      output$tab03_plot_factor <- renderUI({
        ns <- shiny::NS(id)
        plot_output_list <- lapply(1:length(RR_general()$"out03A_plots_factor"), function(i) {
          plot_name <- paste("plotA", i, sep="")
          table_name <- paste("tableA", i, sep="")


          div(
            fluidRow(
              column(6, plotlyOutput(ns(plot_name), height = "100%", width = "100%")),
              column(6, verbatimTextOutput(ns(table_name)))
            ), br(), br(), br()
          )
        })
      })


      # # # Tab05 - Full Results
      output$tab05_full_results <- renderPrint({

        req(control_user_02())

        RR_general()$"out05_full_results"
      })


      # # # Tab06 - R Code
      output$tab06_R_code <- renderText({

        req(control_user_02())

        RR_general()$"out06_R_code"
      })

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
          textOutput(ns("calling_help")),

          shiny::tabsetPanel(id = ns("super_tabset_panel"),
                             tabPanel("Analysis",
                                      fluidRow(
                                        column(12,
                                               h1("Anova 1 way with block"),
                                               uiOutput(ns("tab01_analysis_FULL"))
                                        )
                                      )
                             ),
                             tabPanel("Requeriment",
                                      h1("Anova 1 way with block"), br(),
                                      fluidRow(
                                        column(12, h2("1) Requeriments - No interaction Factor-Block")),
                                        column(6,
                                          plotlyOutput(ns("tab02_plot_req"), height = "100%", width = "100%")
                                          ),
                                        column(6,
                                               verbatimTextOutput(ns("tab02_table_req"))
                                        )
                                        )
                                      ,
                                      br(), br(),
                                      fluidRow(
                                        column(12,
                                                uiOutput(ns("tab02_requeriments_FULL"))
                                               #verbatimTextOutput(ns("tab02_requeriments"))
                                        )
                                      )
                             ),
                             tabPanel("Plots - Factor",
                                      fluidRow(
                                        column(12,
                                               h1("Anova 1 way with block"),
                                               uiOutput(ns("tab03_plot_factor"))
                                        )
                                      )
                             ),
                             tabPanel("Full Results",
                                      fluidRow(
                                        column(12,
                                               h1("Anova 1 way with block"),
                                               verbatimTextOutput(ns("tab05_full_results"))
                                        )
                                      )
                             ),
                             tabPanel("R Code",
                                      fluidRow(
                                        column(12,
                                               h1("Anova 1 way with block"),
                                               verbatimTextOutput(ns("tab06_R_code"))
                                        )
                                      )
                             )
                             # tabPanel("Analysis",  # 05
                             #          fluidRow(
                             #            column(12,
                             #                   h2("Anova 1 way"),
                             #                   verbatimTextOutput(ns("tab03_analysis_anova"))
                             #            )
                             #          )
                             # ),
                             # tabPanel("Requeriments",  # 05
                             #          fluidRow(
                             #            column(12,
                             #                   h2("Anova 1 way"),
                             #                   verbatimTextOutput(ns("tab02_requeriments"))
                             #            )
                             #          )
                             # ),
                             # tabPanel("Plots - Raw Data",  # 05,
                             #          fluidRow(h2("Anova 1 way")),
                             #          fluidRow(
                             #            #column(1),
                             #            column(12,
                             #                   #plotOutput(ns("tab04_plots")),
                             #                   br(),
                             #                   #br()
                             #
                             #                   shinycssloaders::withSpinner(uiOutput(ns("plot_outputs33"))),
                             #            )
                             #          )
                             # ),
                             # tabPanel("Plots - Residuals",  # 05,
                             #          fluidRow(h2("Anova 1 way")),
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
                             # tabPanel("Full Results",  # 05
                             #          fluidRow(
                             #            column(12,
                             #                   h2("Anova 1 way"),
                             #                   verbatimTextOutput(ns("tab05_full_results"))
                             #            )
                             #          )
                             # ),
                             #
                             #
                             #
                             # tabPanel("R code",  # 05
                             #          fluidRow(
                             #            column(10,
                             #                   h2("Anova 1 way"),
                             #                   verbatimTextOutput(ns("tab05_code"))
                             #            ),
                             #            column(2, uiOutput(ns("clip")),
                             #                   br(),
                             #                   downloadButton(ns("downloadBtn"), "Download Code")
                             #            )
                             #          )
                             # )

          ), br(), br(), br(), br(), br(), br()
        )
      })


    }
  )
}

