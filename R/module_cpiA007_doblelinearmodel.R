# https://plotly-r.com/linking-views-with-shiny.html

# # # Module 01 - ANOVA
# UI and SERVER for all modules respect to ANOVA



module_cpiA007_s01_varselection_ui <- function(id){

  ns <- shiny::NS(id)

  div(
    uiOutput(ns("vars_selection"))
  )
}




module_cpiA007_s01_varselection_server <- function(id, input_general){
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
              column(6, h1("Doble Linear Regresion"))
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
                     selectInput(inputId = ns("x01_var_name"), label = "X01 (Regresor)",
                                 choices = set_options,
                                 selected = set_options[1])
              ),
              column(2,
                     selectInput(inputId = ns("x02_var_name"), label = "X02 (Regresor)",
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


      x01_var_name <- reactive({
        req(action_button_show())

        output_value <- input$x01_var_name
        return(output_value)
      })


      x02_var_name <- reactive({
        req(action_button_show())

        output_value <- input$x02_var_name
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


        the_list <- list(vr_var_name(), x01_var_name(), x02_var_name(), alpha_value(), intro_source_database())

        names(the_list) <- c("vr_var_name", "x01_var_name", "x02_var_name", "alpha_value", "intro_source_database")
        the_list
      })


      return(output_list)
    })
}






module_cpiA007_s02_rscience_ui <- function(id){

  ns <- shiny::NS(id)

  uiOutput(ns("page_test"))

}




module_cpiA007_s02_rscience_server <- function(id, input_general, input_01_anova){
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
        # check_previous <- fn_cpiA006_control_previous(database = input_general()$database,
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

        the_output <- fn_cpiA007_gen02(database = input_general()$database,
                                       vr_var_name = input_01_anova()$vr_var_name,
                                       x01_var_name = input_01_anova()$x01_var_name,
                                       x02_var_name = input_01_anova()$x02_var_name,
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
        # check_post <- fn_cpiA006_control_p01_test(all_results = RR_general()$"R_results"$"p01_test")
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
      output$tab01_analysis <- renderPrint({

        req(control_user_02())

        RR_general()$"out01_analysis"
      })



      output$tab02_requerimentsA <- renderPrint({

        #req(control_user_02())

        #RR_general()$"out02_requeriments"
        req(control_user_02())
        selected_objects <- c("df_check_cor_test")

        all_resutls <- RR_general()$"out05_full_results"
        all_resutls[selected_objects]

      })


      output$tab02_requerimentsB_01 <- renderPrint({

        req(control_user_02())

        selected_objects <- c("pearson_cor_results")

        all_resutls <- RR_general()$"out02_requeriments"
        all_resutls[selected_objects]

      })

      output$tab02_requerimentsB_02 <- renderPrint({

        req(control_user_02())

        selected_objects <- c("spearman_cor_results")

        all_resutls <- RR_general()$"out02_requeriments"
        all_resutls[selected_objects]

      })

      output$tab02_requerimentsB_03 <- renderUI({

        ns <- shiny::NS(id)

        the_table <- RR_general()$"out05_full_results"$"df_check_cor_test"
        the_test <- the_table$"selected_cor_test"

        if(the_test == "Pearson") verbatimTextOutput(ns("tab02_requerimentsB_01")) else
          if(the_test == "Spearman") verbatimTextOutput(ns("tab02_requerimentsB_02")) else NULL



      })

      output$tab02_requerimentsC <- renderPrint({

        #req(control_user_02())

        #RR_general()$"out02_requeriments"
        req(control_user_02())

        selected_objects <- c("test_residuals_normality")

        all_resutls <- RR_general()$"out05_full_results"

        all_resutls[selected_objects]


      })

      output$tab02_requerimentsD <- renderPrint({

        req(control_user_02())

        selected_objects <- c("df_check_cor_test", "normality_x01_results", "normality_x02_results",
                              "homogeneity_results")

        all_resutls <- RR_general()$"out05_full_results"

        all_resutls[selected_objects]
      })

      output$tab02_homogeneityplot <- plotly::renderPlotly({
        RR_general()$"out04A_plots_residuals"[[2]]
      })



      output$tab03_specialplot <- plotly::renderPlotly({
        RR_general()$"out04A_plots_residuals"[[1]]
      })
#
#       output$tab03_specialplot2 <- plotly::renderPlotly({
#         RR_general()$"out04A_plots_residuals"[[1]]
#       })

      output$tab03_plot_factor2 <- renderUI({

        ns <- shiny::NS(id)

        div(
        fluidRow(
          column(12, plotlyOutput(ns("tab03_specialplot"),
                                  height = "80vh", width = "80vh")),

        )
        )
      })

      output$tab03_special <- renderPrint({

        req(control_user_02())

        selected_objects <- c("df_table_reg",
                              "df_table_det_coef",
                              "df_position")

        all_resutls <- RR_general()$"out05_full_results"
        all_resutls[selected_objects]

      })
      # output$tab03_plot_factor <- renderUI({
      #   ns <- shiny::NS(id)
      #   plot_output_list <- lapply(1:length(RR_general()$"out03A_plots_factor"), function(i) {
      #     plot_name <- paste("plotA", i, sep="")
      #     table_name <- paste("tableA", i, sep="")
      #
      #
      #     div(
      #       fluidRow(
      #         column(6, plotlyOutput(ns(plot_name), height = "100%", width = "100%")),
      #         column(6, verbatimTextOutput(ns(table_name)))
      #       ), br(), br(), br()
      #     )
      #   })
      # })
      #

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
                                               h2("Doble Linear Regresion"),
                                               verbatimTextOutput(ns("tab01_analysis"))
                                        )#,
                                        #column(6, plotlyOutput(ns("tab03_specialplot2"), height = "100%", width = "100%")),

                                      )
                             ),
                             tabPanel("Requeriments",
                                      h2("Doble Linear Regresion"),
                                      # fluidRow(
                                      #   column(12,
                                      #          h2("Resumen Correlation test selection"),
                                      #          verbatimTextOutput(ns("tab02_requerimentsA"))
                                      #         )
                                      #   ),
                                      #br(), br(), br(),
                                     #br(), br(), br(),

                                      fluidRow(
                                        column(12,
                                               h2("Requeriment 01 - Residuals normality"),
                                               verbatimTextOutput(ns("tab02_requerimentsC"))

                                        )
                                      ),
                                      br(), br(), br(),
                                      br(), br(), br(),

                                      fluidRow(
                                        column(6,
                                               h2("Requeriment 02 - Residuals homogeneity"),
                                               plotlyOutput(ns("tab02_homogeneityplot"), height = "100%", width = "100%")),

                                      ),
                                      br(), br(), br(),
                                      br(), br(), br(),
                                      fluidRow(
                                        column(12,
                                               h2("Requeriment 03 - Non correlation X01 and X02 (Selected correlation test)"),

                                               uiOutput(ns("tab02_requerimentsB_03"))
                                        )
                                      ),
                                      br(), br(), br(),
                                      br(), br(), br(),

                                      fluidRow(
                                        column(12,
                                              h2("Extra - Details about requeriments for Person Correlation test"),
                                              verbatimTextOutput(ns("tab02_requerimentsD"))
                                              )
                                        ),


                             ),
                             tabPanel("Plots",
                                      fluidRow(
                                        column(12,
                                               h2("Doble Linear Regresion"),
                                               uiOutput(ns("tab03_plot_factor2")),
                                               br(), br(),br(),
                                               verbatimTextOutput(ns("tab03_special"))

                                        )
                                      )
                             ),
                             tabPanel("Full Results",
                                      fluidRow(
                                        column(12,
                                               h2("Doble Linear Regresion"),
                                               verbatimTextOutput(ns("tab05_full_results"))
                                        )
                                      )
                             ),
                             tabPanel("R Code",
                                      fluidRow(
                                        column(12,
                                               h2("Doble Linear Regresion"),
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

