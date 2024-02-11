
# # # Module 01 - ANOVA
# UI and SERVER for all modules respect to ANOVA



module02_anova_s01_varselection_ui <- function(id){

  ns <- shiny::NS(id)

  div(
    uiOutput(ns("vars_selection"))
    )
}




module02_anova_s01_varselection_server <- function(id, input_general){
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
        standard_style_button_load <- "color: white; background-color: _color_;width: 150px; height: 50px; font-size: 20px;"
        output_style_button_load <- gsub(pattern = "_color_",
                                         replacement = color_button_load(),
                                         x = standard_style_button_load)

        # # # Style for reset button
        output_style_button_reset <- "color: white; background-color: #F4A020;width: 150px; height: 50px; font-size: 20px;"

        # # # UI content
        div(
          fluidRow(
            actionButton(ns("action_load"), label = "LOAD", style = output_style_button_load),
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
              column(2, h1("ANOVA 1 Way"))
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
        need(!is.null(input$alpha_value), "Error 11: Module anova s01 - input$alpha_value can not be NULL.")
      )

      validate(
        need(is.vector(input$vr_var_name), "Error 12: Module anova s01 - input$vr_var_name must be a vector."),
        need(is.vector(input$factor_var_name), "Error 13: Module anova s01 - input$vr_var_name must be a vector."),
        need(is.vector(input$alpha_value), "Error 14: Module anova s01 - input$alpha_value must be a vector.")
      )

      validate(
        need(length(input$vr_var_name) == 1, "Error 15: Module anova s01 - input$vr_var_name has length 1."),
        need(length(input$factor_var_name) == 1, "Error 16: Module anova s01 - input$factor_var_name has length 1."),
        need(length(input$alpha_value) == 1, "Error 17: Module anova s01 - input$alpha_value has length 1.")
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






module02_anova_s02_rscience_ui <- function(id){

  ns <- shiny::NS(id)

  uiOutput(ns("page_test"))

}




module02_anova_s02_rscience_server <- function(id, input_general, input_01_anova){
  moduleServer(
    id,
    function(input, output, session) {


      # # # Control for initial inputs
      control_user_01 <- reactive({

        validate(
          need(!is.null(input_general()), "Error 01: Module anova s02 - input_general can not be NULL."),
          need(!is.null(input_01_anova()), "Error 02: Module anova s02 - iinput_01_anova can not be NULL.")
        )

        return(TRUE)
      })


      # # # All anova results
      results_test001_anova <- reactive({

        req(control_user_01())

        the_output <- test001_anova_full_gen01(database = input_general()$database,
                                       vr_var_name = input_01_anova()$vr_var_name,
                                       factor_var_name = input_01_anova()$factor_var_name,
                                       alpha_value = input_01_anova()$alpha_value)




        the_output

      })


      # # # Control for anova results object
      control_user_02 <- reactive({

        req(control_user_01())

        validate(
          need(!is.null(results_test001_anova()), "Error 03: Module anova s02 - results_test001_anova() can not be NULL.")        )

        return(TRUE)

      })


      # # # Tab01 - Anova Results
      output$tab01_all_anova_results <- renderPrint({

        req(control_user_02())

        results_test001_anova()
      })

      ##########################################################################

      # # #Tab02 - Residuals Requeriments
      output$tab02_requeriments <- renderPrint({

        req(control_user_02())


        mi_lista <- results_test001_anova()




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
      output$tab03_analysis_anova <- renderPrint({

        req(control_user_02())

        mi_lista <- results_test001_anova()




        # Vector con nombres de elementos a ver
        nombres_a_ver <- c("df_selected_vars",
                           "df_table_anova", "df_factor_info", "check_unbalanced_reps",
                           "df_tukey_original_table",
                           "df_tukey_new_table")
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





      # Boxplot
      output$plot001 <- plotly::renderPlotly({

        req(control_user_02())

        anova_plot001 <- test001_anova_plot001(minibase_mod = results_test001_anova()$minibase_mod,
                               df_factor_info = results_test001_anova()$df_factor_info)

        anova_plot001

      })


      # Boxplot
      output$plot002 <- plotly::renderPlotly({

        req(control_user_02())

        anova_plot002 <- test001_anova_plot002(df_plot002_table = results_test001_anova()$df_plot002_table)
        anova_plot002
      })





      # Boxplot
      output$plot003 <- renderPlotly({

        req(control_user_02())


        anova_plot003 <- test001_anova_plot003(minibase_mod = results_test001_anova()$minibase_mod,
                              df_factor_info = results_test001_anova()$df_factor_info)

        anova_plot003


      })



      # Boxplot con medias
      output$plot004 <- renderPlotly({

        req(control_user_02())



        anova_plot004 <- test001_anova_plot004(minibase_mod   = results_test001_anova()$minibase_mod,
                                               df_factor_info = results_test001_anova()$df_factor_info)


        anova_plot004

      })


      # Boxplot
      output$plot005 <- renderPlotly({

        req(control_user_02())


        anova_plot005 <- test001_anova_plot005(minibase_mod   = results_test001_anova()$minibase_mod,
                                               df_factor_info = results_test001_anova()$df_factor_info)
        anova_plot005




      })



      output$tab_04_all_plotly <- renderUI({

        ns <- shiny::NS(id)

        div(
          plotlyOutput(ns("plot001")),
          br(),
          br(),
          plotlyOutput(ns("plot002")),
          br(),
          br(),
          plotlyOutput(ns("plot003")),
          br(),
          br(),
          plotlyOutput(ns("plot004")),
          br(),
          br(),
          plotlyOutput(ns("plot005"))
        )

      })
      ##########################################################################



      # # # Tab 05 - Analysis...
      THE_CODE <- reactive({
        req(control_user_02())

        the_code <- showme_your_code_test001_anova(intro_source_database = input_01_anova()$intro_source_database,
                                                   vr_var_name = input_01_anova()$vr_var_name,
                                                   factor_var_name = input_01_anova()$factor_var_name,
                                                   alpha_value = input_01_anova()$alpha_value)

        the_code

      })


      #observe(print(input$clipbtn))

      color_button_copy <- reactiveVal("orange")


      output$clip <- renderUI({

        ns <- shiny::NS(id)

        req(THE_CODE())

        standard_style_button_clip <- "color: white; background-color: _color_;width: 150px; height: 50px; font-size: 20px;"
        output_style_button_clip <- gsub(pattern = "_color_",
                                         replacement = color_button_copy(),
                                         x = standard_style_button_clip)


        rclipButton(
          inputId = ns("clipbtn"),
          label = "Copy Code",
          clipText = THE_CODE(),
          icon = icon("clipboard"),
          tooltip = "Click me to copy the content of the text field to the clipboard!",
          options = list(delay = list(show = 800, hide = 100), trigger = "hover"),
          style = output_style_button_clip
        )
      })


      observeEvent(input$clipbtn,{

        if(input$clipbtn == 0) color_button_copy("orange") else color_button_copy("green")
      })


      observeEvent(results_test001_anova(),{
        color_button_copy("orange")
      })


      output$tab05_code <- renderText({

        req(control_user_02())



        THE_CODE()
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
                                               h2("Anova 1 way"),
                                               verbatimTextOutput(ns("tab03_analysis_anova"))
                                        )
                                      )
                             ),
                             tabPanel("Requeriments",  # 05
                                      fluidRow(
                                        column(12,
                                               h2("Anova 1 way"),
                                               verbatimTextOutput(ns("tab02_requeriments"))
                                        )
                                      )
                             ),
                             tabPanel("Plots",  # 05,
                                      fluidRow(h2("Anova 1 way")),
                                      fluidRow(
                                        column(1),
                                        column(6,
                                               #plotOutput(ns("tab04_plots")),
                                               br(),
                                               shinycssloaders::withSpinner(uiOutput(ns("tab_04_all_plotly"))),
                                        )
                                      )
                             ),
                             tabPanel("Full Results",  # 05
                                      fluidRow(
                                        column(12,
                                               h2("Anova 1 way"),
                                               verbatimTextOutput(ns("tab01_all_anova_results"))
                                        )
                                      )
                             ),



                             tabPanel("R code",  # 05
                                      fluidRow(
                                        column(10,
                                               h2("Anova 1 way"),
                                               verbatimTextOutput(ns("tab05_code"))
                                        ),
                                        column(2, uiOutput(ns("clip")))
                                      )
                             )

          ), br(), br(), br(), br(), br(), br()
        )
      })


    }
  )
}

