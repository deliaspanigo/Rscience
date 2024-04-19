
# # # Module 01 - ANOVA
# UI and SERVER for all modules respect to ANOVA



module_cpiB005_s01_varselection_ui <- function(id){

  ns <- shiny::NS(id)

  div(
    uiOutput(ns("vars_selection"))
  )
}




module_cpiB005_s01_varselection_server <- function(id, input_general){
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
              column(12, h1("Generalized Linear Modelo - Simple Linear Regresion - Binomial"))
            ),
            fluidRow(
              column(6,
                     tableOutput(ns("intro_source_database")))
            ),
            br(), br(), br(),


            fluidRow(
              column(4,
                     fluidRow(
                       column(12,
                              selectInput(inputId = ns("vr_var_name"), label = "Response Variable (Y)",
                                          choices = set_options ,
                                          selected = set_options[1])
                       )),
                     fluidRow(
                       column(12,
                              selectInput(inputId = ns("x_var_name"), label = "Regresor (X)",
                                          choices = set_options,
                                          selected = set_options[1])
                       ))),
              column(4,
                     fluidRow(
                       column(12,
                              selectInput(inputId = ns("alpha_value"), label = "Alpha value",
                                          # choices = c(0.10, 0.05, 0.01),
                                          choices = c(0.05),
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
        #       selectInput(inputId = ns("x_var_name"), label = "Factor",
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
          need(!is.null(input$vr_var_name), "Error 09: Module anova s01 - input$vr_var_name can not be NULL."),
          need(!is.null(input$x_var_name), "Error 10: Module anova s01 - input$x_var_name can not be NULL."),
          need(!is.null(input$alpha_value), "Error 11: Module anova s01 - input$alpha_value can not be NULL.")
        )

        validate(
          need(is.vector(input$vr_var_name), "Error 12: Module anova s01 - input$vr_var_name must be a vector."),
          need(is.vector(input$x_var_name), "Error 13: Module anova s01 - input$vr_var_name must be a vector."),
          need(is.vector(input$alpha_value), "Error 14: Module anova s01 - input$alpha_value must be a vector.")
        )

        validate(
          need(length(input$vr_var_name) == 1, "Error 15: Module anova s01 - input$vr_var_name has length 1."),
          need(length(input$x_var_name) == 1, "Error 16: Module anova s01 - input$x_var_name has length 1."),
          need(length(input$alpha_value) == 1, "Error 17: Module anova s01 - input$alpha_value has length 1.")
        )


        validate(
          need(input$vr_var_name != "", "Select a response variable."),
          need(input$x_var_name != "", "Select a regresor variable."),
        )

        validate(
          need(input$vr_var_name != input$x_var_name, "Selected variables can not be equal.")
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
      observeEvent(input$x_var_name, {

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






module_cpiB005_s02_rscience_ui <- function(id){

  ns <- shiny::NS(id)

  uiOutput(ns("page_test"))

}




module_cpiB005_s02_rscience_server <- function(id, input_general, input_01_anova){
  moduleServer(
    id,
    function(input, output, session) {


      # # # Control for initial inputs
      control_user_01 <- reactive({

        check_previous <- FALSE

        # validate(
        #   need(!is.null(input_general()), "Error 01: Module anova s02 - input_general can not be NULL."),
        #   need(!is.null(input_01_anova()), "Error 02: Module anova s02 - input_01_anova can not be NULL.")
        # )
        #
        # check_previous <- cpiB005_slr_binomial_control_previous(database = input_general()$database,
        #                                                               vr_var_name = input_01_anova()$vr_var_name,
        #                                                               x_var_name = input_01_anova()$x_var_name,
        #                                                               alpha_value = input_01_anova()$alpha_value)
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

        the_output <- cpiB005_slr_binomial_results(database = input_general()$database,
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
        validate(
          need(!is.null(RR_general()), "Error 03: Module anova s02 - RR_general() can not be NULL.")
        )
        #
        #
        #
        #
        #         # Control post
        # check_post <- cpiB005_slr_binomial_control_post(list_results_from_cpiA001_anova1way = RR_general())
        # #
        # validate(
        #   need(check_post$check_ok, check_post$text_output)
        # )

        return(TRUE)
      })




      # # RR - Tables
      # RR_g01_tables <- reactive({
      #
      #   req(control_user_02())
      #   all_tables_g01 <- cpiB005_slr_binomial_recruit_g01_Tables(list_results_from_cpiA001_anova1way = RR_general())
      #   all_tables_g01
      # })
      #
      #
      # # RR - Plots
      # RR_g01_plots <- reactive({
      #
      #   req(control_user_02())
      #   all_plots_g01 <- cpiB005_slr_binomial_recruit_g01_FactorPlots(list_results_from_cpiA001_anova1way = RR_general())
      #   all_plots_g01
      # })
      #
      #
      #
      #
      # # RR - Tables
      # RR_g02_tables <- reactive({
      #
      #   req(control_user_02())
      #   all_tables_g02 <- cpiB005_slr_binomial_recruit_g02_Tables(list_results_from_cpiA001_anova1way = RR_general())
      #   all_tables_g02
      # })


      # RR - Plots
      # RR_g02_plots <- reactive({
      #
      #   req(control_user_02())
      #   all_plots_g02 <- cpiB005_slr_binomial_recruit_g02_ResidualsPlots(list_results_from_cpiA001_anova1way = RR_general())
      #   all_plots_g02
      # })

      RR_code <- reactive({

        req(control_user_02())
        the_code <- cpiB005_slr_binomial_code_sectionALL(intro_source_database = input_01_anova()$intro_source_database,
                                                               vr_var_name = input_01_anova()$vr_var_name,
                                                               x_var_name = input_01_anova()$x_var_name,
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
          h2("1) Requeriment - Normaility test - Residuals"),
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

      # # # Tab 05 - Analysis resume...
      output$tab03_analysis_anova_obj01 <- renderPrint({

        req(control_user_02())

        mi_lista <- RR_general()




        # Vector con nombres de elementos a ver
        selected_objs <- c("df_selected_vars")


        # Usar lapply para mostrar los elementos deseados
        mi_lista[selected_objs]

      })
      output$tab03_analysis_anova_obj02 <- renderPrint({

        req(control_user_02())

        mi_lista <- RR_general()




        # Vector con nombres de elementos a ver
        selected_objs <- c("df_show_n")


        # Usar lapply para mostrar los elementos deseados
        mi_lista[selected_objs]

      })
      output$tab03_analysis_anova_obj03 <- renderPrint({

        req(control_user_02())

        mi_lista <- RR_general()




        # Vector con nombres de elementos a ver
        selected_objs <- c("model_glm")


        # Usar lapply para mostrar los elementos deseados
        mi_lista[selected_objs]

      })



      output$tab03_analysis_anova_obj04 <- renderPrint({

        req(control_user_02())

        mi_lista <- RR_general()




        # Vector con nombres de elementos a ver
        selected_objs <- c("summary_glm")


        # Usar lapply para mostrar los elementos deseados
        mi_lista[selected_objs]

      })
      output$tab03_analysis_anova_obj05 <- renderPrint({

        req(control_user_02())

        mi_lista <- RR_general()




        # Vector con nombres de elementos a ver
        selected_objs <- c("df_table_slr")


        # Usar lapply para mostrar los elementos deseados
        mi_lista[selected_objs]

      })





      output$tab03_analysis_anova_FULL <- renderUI({

        ns <- shiny::NS(id)

        div(
          h2("1) References"),
          verbatimTextOutput(ns("tab03_analysis_anova_obj01")),
          br(), br(), br(),

          h2("2) Factor resumen"),
          verbatimTextOutput(ns("tab03_analysis_anova_obj02")),
          br(), br(), br(),

          h2("3) Anova 1 way - Table"),
          verbatimTextOutput(ns("tab03_analysis_anova_obj03")),
          br(), br(), br(),

          h2("4) Multiple comparation test (Tukey)"),
          verbatimTextOutput(ns("tab03_analysis_anova_obj04")),
          br(), br(), br(),

          h2("5) Model Error"),
          verbatimTextOutput(ns("tab03_analysis_anova_obj05")),
          br(), br(), br()
        )

      })



      output$el_plot <- renderPlot({

        #plot(mtcars)
        req(control_user_02())

        mi_lista <- RR_general()
        new_plot <-  with( mi_lista,{


          #Ploteamos las medias de los niveles del factor
          plot(x = minibase_mod$"X",
               y = minibase_mod$"VR",
               col= "red",
               xlab = "X",
               ylab = "VR")
          points(x =  minibase_mod$"X",
                 y= minibase_mod$fitted.values, col= "black")


        })
      })


      output$el_plot2 <- renderPlotly({


        mi_lista <- RR_general()


        new_plot <-  with(mi_lista,{
          # # # Create a new plot...
          plot002 <- plot_ly()


          # # # Adding errors...
          plot002 <-   add_trace(p = plot002,
                                        x = minibase$X,
                                        y = minibase$VR,
                                        type = 'scatter',
                                        mode = 'markers',
                                        name = "data",
                                        marker = list(size = 15, color = 'blue'))

          plot002 <- add_trace(p = plot002,
                               x = minibase_mod$"X",
                               y = minibase_mod$"fitted.values",
                               type = 'scatter',
                               mode = 'markers',
                               name = "fitted.values",
                               marker = list(size = 15, color = 'black'))


          #plot002_factor

          plot002 <- plotly::layout(p = plot002,
                                           xaxis = list(title = "X"),
                                           yaxis = list(title = "VR"),
                                           title = "Plot 001 - Scatterplot",
                                           font = list(size = 20),
                                           margin = list(t = 100))

          # # # Without zerolines
          plot002 <-plotly::layout(p = plot002,
                                          xaxis = list(zeroline = FALSE),
                                          yaxis = list(zeroline = FALSE))

          # # # Plot output
          plot002

        })

        new_plot
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
          textOutput(ns("calling_help")),

          shiny::tabsetPanel(id = ns("super_tabset_panel"),
                             tabPanel("Analysis",  # 05
                                      fluidRow(
                                        column(12,
                                               h1("Anova 1 way"),
                                               uiOutput(ns("tab03_analysis_anova_FULL"))
                                        )
                                      )
                             ),
                             # tabPanel("Requeriments",  # 05
                             #          fluidRow(
                             #            column(12,
                             #                   h1("Anova 1 way"),
                             #                   uiOutput(ns("tab02_analysis_anova_FULL"))
                             #            )
                             #          )
                             # ),
                             tabPanel("Plots - Raw Data",  # 05,
                                      fluidRow(column(12, h1("Anova 1 way"))),
                                      fluidRow(
                                        #column(1),
                                        column(12,
                                               plotOutput(ns("el_plot")),
                                               br(),
                                               plotlyOutput(ns("el_plot2")),
                                               #plotOutput(ns("tab04_plots")),
                                               br(),
                                               #br()

                                               #shinycssloaders::withSpinner(uiOutput(ns("plot_outputs33"))),
                                        )
                                      )
                             ),
                             # tabPanel("Plots - Residuals",  # 05,
                             #          fluidRow(column(12, h1("Anova 1 way"))),
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
                             #          fluidRow(h1("Anova 1 way")),
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
                             #          fluidRow(h1("Anova 1 way")),
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
                             #          fluidRow(h1("Anova 1 way")),
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
                                               h1("Anova 1 way"),
                                               verbatimTextOutput(ns("tab01_all_anova_results"))
                                        )
                                      )
                             ),



                             tabPanel("R code",  # 05
                                      fluidRow(
                                        column(10,
                                               h1("Anova 1 way"),
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
      })


    }
  )
}

