
# # # Module 01 - ANOVA
# UI and SERVER for all modules respect to ANOVA



module_cpiA020_s01_varselection_ui <- function(id){

  ns <- shiny::NS(id)

  div(
    uiOutput(ns("vars_selection"))
  )
}




module_cpiA020_s01_varselection_server <- function(id, input_general){
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
              column(6, h1("Multiple Linear Regresion"))
            ),
            fluidRow(
              column(6,
                     tableOutput(ns("intro_source_database")))
            ),
            br(), br(), br(),


            fluidRow(
              column(4,
                     selectInput(inputId = ns("vr_var_name"), label = "Response Variable",
                                 choices = set_options ,
                                 selected = set_options[1])
              ),
              column(2,
                     selectInput(inputId = ns("alpha_value"), label = "Alpha value",
                                 choices = c(0.10, 0.05, 0.01),
                                 selected = 0.05)
              ),
              column(2),
              column(4, br(), br(), uiOutput(ns("action_buttons"))
              )
            ),
            fluidRow(
              column(12,
                                selectInput(inputId = ns("x_var_name"), label = "X (Regresor)",
                                            choices = set_options,
                                            selected = set_options[1], multiple = TRUE, width = "200%")
              )
            ),
            br(),
            textOutput(ns("calling_help"))
        )



      })



      # # # Control user 02
      control_user_02 <- reactive({

        req(control_user_01())



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
      observeEvent(input$factor01_var_name, {

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


      x_var_name <- reactive({
        req(action_button_show())

        output_value <- input$x_var_name
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


        the_list <- list(vr_var_name(), x_var_name(), alpha_value(), intro_source_database())

        names(the_list) <- c("vr_var_name", "x_var_name", "alpha_value", "intro_source_database")
        the_list
      })


      return(output_list)
    })
}






module_cpiA020_s02_rscience_ui <- function(id){

  ns <- shiny::NS(id)

  uiOutput(ns("page_test"))

}




module_cpiA020_s02_rscience_server <- function(id, input_general, input_01_anova){
  moduleServer(
    id,
    function(input, output, session) {


      # # # Control for initial inputs
      control_user_01 <- reactive({

        # check_previous <- FALSE
        #
        # validate(
        #   need(!is.null(input_general()), "Error 01: Module anova s02 - input_general can not be NULL."),
        #   need(!is.null(input_01_anova()), "Error 02: Module anova s02 - input_01_anova can not be NULL.")
        # )
        #
        # check_previous <- fn_cpiA008_control_previous(database = input_general()$database,
        #                                                      vr_var_name = input_01_anova()$vr_var_name,
        #                                                      factor01_var_name = input_01_anova()$factor01_var_name,
        #                                                      factor02_var_name = input_01_anova()$factor02_var_name,
        #                                                      alpha_value = input_01_anova()$alpha_value)
        #
        # validate(
        #   need(check_previous$dt_ok, check_previous$text_output)
        # )
        return(TRUE)
      })


      ####################################################################################


      # # # All anova results
      RR_general <- reactive({

        req(control_user_01())

        the_output <- fn_cpiA020_gen02(database = input_general()$database,
                                       vr_var_name = input_01_anova()$vr_var_name,
                                       x_var_name = input_01_anova()$x_var_name,
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
        # check_post <- fn_cpiA008_control_p01_test(all_results = RR_general()$"R_results"$"p01_test")
        # #
        # validate(
        #   need(check_post$check_ok, check_post$text_output)
        # )

        return(TRUE)
      })



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

      # # # Tab01 - Analysis
      output$tab01_analysis_obj01 <- renderPrint({

        req(control_user_02())


        mi_lista <- RR_general()$"out01_analysis"




        # Vector con nombres de elementos a ver
        nombres_a_ver <- c("df_selected_vars")

        # Usar lapply para mostrar los elementos deseados
        mi_lista[nombres_a_ver]

      })
      output$tab01_analysis_obj02 <- renderPrint({

        req(control_user_02())


        mi_lista <- RR_general()$"out01_analysis"




        # Vector con nombres de elementos a ver
        nombres_a_ver <- c("df_table_reg")

        # Usar lapply para mostrar los elementos deseados
        mi_lista[nombres_a_ver]

      })
      output$tab01_analysis_obj03 <- renderPrint({

        req(control_user_02())


        mi_lista <- RR_general()$"out01_analysis"




        # Vector con nombres de elementos a ver
        nombres_a_ver <- c("df_table_det_coef")

        # Usar lapply para mostrar los elementos deseados
        mi_lista[nombres_a_ver]

      })
      output$tab01_analysis_obj04 <- renderPrint({

        req(control_user_02())


        mi_lista <- RR_general()$"out01_analysis"




        # Vector con nombres de elementos a ver
        nombres_a_ver <- c("df_position")

        # Usar lapply para mostrar los elementos deseados
        mi_lista[nombres_a_ver]

      })
      output$tab01_analysis_obj05 <- renderPrint({

        req(control_user_02())


        mi_lista <- RR_general()$"out01_analysis"




        # Vector con nombres de elementos a ver
        nombres_a_ver <- c("df_dispersion")

        # Usar lapply para mostrar los elementos deseados
        mi_lista[nombres_a_ver]

      })



      output$tab01_analysis_FULL <- renderUI({

        ns <- shiny::NS(id)

        div(
          h2("1) References"),
          verbatimTextOutput(ns("tab01_analysis_obj01")),
          br(), br(), br(),

          h2("2) Linear Regresion - Table"),
          verbatimTextOutput(ns("tab01_analysis_obj02")),
          br(), br(), br(),

          h2("3) R^2 and Ajusted R^2"),
          verbatimTextOutput(ns("tab01_analysis_obj03")),
          br(), br(), br(),

          h2("4) Position"),
          verbatimTextOutput(ns("tab01_analysis_obj04")),
          br(), br(), br(),

          h2("5) Dispersion"),
          verbatimTextOutput(ns("tab01_analysis_obj05")),
          br(), br(), br()
        )

      })


      output$tab01_analysis <- renderPrint({

        req(control_user_02())

        RR_general()$"out01_analysis"
      })



      output$tab02_requeriments <- renderPrint({

        req(control_user_02())

        RR_general()$"out02_requeriments"
      })


      output$tab02_homogeneityplot <- plotly::renderPlotly({
        RR_general()$"out04A_plots_residuals"[[2]]
      })


      output$tab02_cor <- renderPrint({

        req(control_user_02())
        #phrase02_model_output
        mi_lista <- RR_general()$"out05_full_results"
        selected_obj <- c("phrase02_model_output","check_all_cor", "df_cor_resumen")
        #selected_obj <- c("check_all_cor", "df_cor_resumen")

        sublista <- mi_lista[selected_obj]
        sublista[[1]] <- capture.output(write(sublista[[1]], file = ""))
        names(sublista) <- selected_obj
        sublista
      })

##############################################################

      output$tab22_cor_01_01 <- renderPrint({

        req(control_user_02())
        #phrase02_model_output
        mi_lista <- RR_general()$"out05_full_results"
        selected_obj <- c("df_normality")
        #selected_obj <- c("check_all_cor", "df_cor_resumen")

        mi_lista[selected_obj]
      })

      output$tab22_cor_01_02 <- renderPrint({

        req(control_user_02())
        #phrase02_model_output
        mi_lista <- RR_general()$"out05_full_results"
        selected_obj <- c("list_normality_test")
        #selected_obj <- c("check_all_cor", "df_cor_resumen")

        mi_lista[selected_obj]
      })

      output$tab22_cor_02_01 <- renderPrint({

        req(control_user_02())
        #phrase02_model_output
        mi_lista <- RR_general()$"out05_full_results"
        selected_obj <- c("df_homogeneity")
        #selected_obj <- c("check_all_cor", "df_cor_resumen")

        mi_lista[selected_obj]
      })

      output$tab22_cor_02_02 <- renderPrint({

        req(control_user_02())
        #phrase02_model_output
        mi_lista <- RR_general()$"out05_full_results"
        selected_obj <- c("list_homogeneity")
        #selected_obj <- c("check_all_cor", "df_cor_resumen")

        mi_lista[selected_obj]
      })


      output$tab22_cor_03_01 <- renderPrint({

        req(control_user_02())
        #phrase02_model_output
        mi_lista <- RR_general()$"out05_full_results"
        selected_obj <- c("df_cor_pearson")
        #selected_obj <- c("check_all_cor", "df_cor_resumen")

        mi_lista[selected_obj]
      })

      output$tab22_cor_03_02 <- renderPrint({

        req(control_user_02())
        #phrase02_model_output
        mi_lista <- RR_general()$"out05_full_results"
        selected_obj <- c("df_matrix_cor_pearson")
        #selected_obj <- c("check_all_cor", "df_cor_resumen")

        mi_lista[selected_obj]
      })

      output$tab22_cor_03_03 <- renderPrint({

        req(control_user_02())
        #phrase02_model_output
        mi_lista <- RR_general()$"out05_full_results"
        selected_obj <- c("list_cor_pearson")
        #selected_obj <- c("check_all_cor", "df_cor_resumen")

        mi_lista[selected_obj]
      })


      output$tab22_cor_04_01 <- renderPrint({

        req(control_user_02())
        #phrase02_model_output
        mi_lista <- RR_general()$"out05_full_results"
        selected_obj <- c("df_cor_spearman")
        #selected_obj <- c("check_all_cor", "df_cor_resumen")

        mi_lista[selected_obj]
      })

      output$tab22_cor_04_02 <- renderPrint({

        req(control_user_02())
        #phrase02_model_output
        mi_lista <- RR_general()$"out05_full_results"
        selected_obj <- c("df_matrix_cor_spearman")
        #selected_obj <- c("check_all_cor", "df_cor_resumen")

        mi_lista[selected_obj]
      })

      output$tab22_cor_04_03 <- renderPrint({

        req(control_user_02())
        #phrase02_model_output
        mi_lista <- RR_general()$"out05_full_results"
        selected_obj <- c("list_cor_spearman")
        #selected_obj <- c("check_all_cor", "df_cor_resumen")

        mi_lista[selected_obj]
      })

      output$tab22_cor_05_01 <- renderPrint({

        req(control_user_02())
        #phrase02_model_output
        mi_lista <- RR_general()$"out05_full_results"
        selected_obj <- c("df_cor_mix")
        #selected_obj <- c("check_all_cor", "df_cor_resumen")

        mi_lista[selected_obj]
      })


      output$tab22_cor_05_02 <- renderDT({

        req(control_user_02())
        #phrase02_model_output
        mi_lista <- RR_general()$"out05_full_results"
        selected_objs <- c("df_cor_mix")
        #selected_obj <- c("check_all_cor", "df_cor_resumen")

        #mi_lista[selected_obj]

        mi_tabla <- mi_lista[[selected_objs]]
        #mi_tabla
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
###############################################################
      output$tab03_plot_factor <- renderUI({
        ns <- shiny::NS(id)
        #vector_for <- 1:length(RR_general()$"out03A_plots_factor")
        # vector_for <- 1:2
        vector_for <- 2
        plot_output_list <- lapply(vector_for, function(i) {
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
                                               h1("Multiple Linear Regresion"),
                                               uiOutput(ns("tab01_analysis_FULL"))

                                               #verbatimTextOutput(ns("tab01_analysis"))
                                        )
                                      )
                             ),
                             tabPanel("Requeriments",
                                      h1("Multiple Linear Regresion"),
                                      fluidRow(
                                        column(12,
                                               h2("1) Requeriment - Normality test - Residuals"),
                                               verbatimTextOutput(ns("tab02_requeriments"))
                                        )
                                      ),
                                      br(), br(), br(),
                                      fluidRow(
                                        column(12,
                                               h2("2) Requeriment - Homogeneity visual evaluation - Residuals"),
                                               plotlyOutput(ns("tab02_homogeneityplot"), height = "100%", width = "100%")),
                                      ),
                                      br(), br(), br(),
                                      fluidRow(
                                        column(12,
                                               h2("3) Requeriment - No correlation between regresors"),
                                               verbatimTextOutput(ns("tab02_cor"))),
                                      )
                             ),
                             tabPanel("Correlation",
                                      h1("Multiple Linear Regresion"),
                                      h2("Algunas explicaciones..."),
                                      shiny::tabsetPanel(id = ns("super_cor_panel"),
                                                         tabPanel("Mix",
                                                                  # fluidRow(
                                                                  #   column(12,
                                                                  #          h2("1) Requeriment - No correlation between regresors"),
                                                                  #          verbatimTextOutput(ns("tab22_cor_05_01"))),
                                                                  # ), br(), br(), br(),
                                                                  fluidRow(
                                                                    column(12,
                                                                           h2("1) Requeriment - No correlation between regresors"),
                                                                           h3("R object: df_cor_mix"),
                                                                           DTOutput(ns("tab22_cor_05_02"))),
                                                                  ), br(), br(), br(),
                                                         ),
                                                         tabPanel("Normality",
                                                                  fluidRow(
                                                                    column(12,
                                                                           h2("1) Normality test for regresors"),
                                                                           verbatimTextOutput(ns("tab22_cor_01_01"))),
                                                                  ), br(),br(),br(),
                                                                  fluidRow(
                                                                    column(12,
                                                                           h2("2) List - Normality"),
                                                                           verbatimTextOutput(ns("tab22_cor_01_02"))),
                                                                  )
                                                                  ),
                                                         tabPanel("Homogeneity",
                                                                  fluidRow(
                                                                    column(12,
                                                                           h2("1) Homogeneity between par of regresors"),
                                                                           verbatimTextOutput(ns("tab22_cor_02_01"))),
                                                                  ), br(),br(),br(),
                                                                  fluidRow(
                                                                    column(12,
                                                                           h2("2) List - Homogeneity"),
                                                                           verbatimTextOutput(ns("tab22_cor_02_02"))),
                                                                  )
                                                                  ),
                                                         tabPanel("Cor Pearson",
                                                                  fluidRow(
                                                                    column(12,
                                                                           h2("1) Requeriment - No correlation between regresors"),
                                                                           verbatimTextOutput(ns("tab22_cor_03_01"))),
                                                                  ), br(),br(),br(),
                                                                  fluidRow(
                                                                    column(12,
                                                                           h2("1) Matrix Correlation - Pearson"),
                                                                           verbatimTextOutput(ns("tab22_cor_03_02"))),
                                                                  ), br(),br(),br(),
                                                                  fluidRow(
                                                                    column(12,
                                                                           h2("2) List - Cor Pearson"),
                                                                           verbatimTextOutput(ns("tab22_cor_03_03"))),
                                                                  )
                                                                  ),
                                                         tabPanel("Cor Spearman",
                                                                  fluidRow(
                                                                    column(12,
                                                                           h2("1) Requeriment - No correlation between regresors"),
                                                                           verbatimTextOutput(ns("tab22_cor_04_01"))),
                                                                  ), br(),br(),br(),
                                                                  fluidRow(
                                                                    column(12,
                                                                           h2("2) Matrix Correlation - Spearman"),
                                                                           verbatimTextOutput(ns("tab22_cor_04_02"))),
                                                                  ), br(),br(),br(),
                                                                  fluidRow(
                                                                    column(12,
                                                                           h2("2) List - Cor Spearman"),
                                                                           verbatimTextOutput(ns("tab22_cor_04_03"))),
                                                                  )
                                                                  )


                                )
                             ),
                             tabPanel("Plots",
                                      fluidRow(
                                        column(12,
                                               h1("Multiple Linear Regresion"),
                                               uiOutput(ns("tab03_plot_factor"))
                                        )
                                      )
                             ),
                             tabPanel("Full Results",
                                      fluidRow(
                                        column(12,
                                               h1("Multiple Linear Regresion"),
                                               verbatimTextOutput(ns("tab05_full_results"))
                                        )
                                      )
                             ),
                             tabPanel("R Code",
                                      fluidRow(
                                        column(12,
                                               h1("Multiple Linear Regresion"),
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

