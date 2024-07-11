
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
              column(12, h1("ANOVA 1 Way"))
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
                      selectInput(inputId = ns("vr_var_name"), label = "Response Variable",
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

        check_previous <- FALSE

        validate(
          need(!is.null(input_general()), "Error 01: Module anova s02 - input_general can not be NULL."),
          need(!is.null(input_01_anova()), "Error 02: Module anova s02 - input_01_anova can not be NULL.")
        )

        check_previous <- cpiA001_anova1way_control_previous(database = input_general()$database,
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

        the_output <- cpiA001_anova1way_results(database = input_general()$database,
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
        check_post <- cpiA001_anova1way_control_post(list_results_from_cpiA001_anova1way = RR_general())
#
        validate(
          need(check_post$check_ok, check_post$text_output)
        )

        return(TRUE)
      })




      # RR - Tables
      RR_g01_tables <- reactive({

        req(control_user_02())
        all_tables_g01 <- cpiA001_anova1way_recruit_g01_Tables(list_results_from_cpiA001_anova1way = RR_general())
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

      RR_g02_contrastes_A <- reactive({

        minibase <- RR_general()$minibase
        selected_k <- nlevels(minibase$"FACTOR")

        df_info_acum_contrastes <- info_acum_general_contrastes(k = selected_k)
        df_info_contrastes <- info_general_contrastes(k = selected_k)
        texto_contrastes <- "
        For k=_total00_ there are _total01_ different possible contrasts.
        From them you can take up to (k-1)=_total02_ orthogonal contrasts simultaneously."


        #Tomando _total02_ contrastes de _total01_ posibles hay _total03_ combinaciones posibles,
        #de las cuales _total04_ son combinaciones ortogonales y _total05_ son combinaciones no ortogonales.

        texto_contrastes <- gsub(pattern = "_total00_", replacement = df_info_contrastes[[1]], x = texto_contrastes)
        texto_contrastes <- gsub(pattern = "_total01_", replacement = df_info_contrastes[[2]], x = texto_contrastes)
        texto_contrastes <- gsub(pattern = "_total02_", replacement = df_info_contrastes[[3]], x = texto_contrastes)
        texto_contrastes <- gsub(pattern = "_total03_", replacement = df_info_contrastes[[4]], x = texto_contrastes)
        texto_contrastes <- gsub(pattern = "_total04_", replacement = df_info_contrastes[[5]], x = texto_contrastes)
        texto_contrastes <- gsub(pattern = "_total05_", replacement = df_info_contrastes[[6]], x = texto_contrastes)


        list_output <- Hmisc::llist(texto_contrastes, df_info_contrastes)
        list_output
      })

      RR_g02_contrastes_B <- reactive({

        req(check_matrix_contraste_mod())

        minibase <- RR_general()$minibase

        mat.contrast   <- matrix_contrasts_mod()
        mat_info_contrastes <- mat.contrast
        colnames(mat_info_contrastes) <- paste0("C", 1:ncol(mat.contrast))
        rownames(mat_info_contrastes) <- levels(minibase$"FACTOR")

        contrasts(minibase$"FACTOR") <- mat.contrast

        model_contrastes <- aov(VR ~ FACTOR,
                     contrasts = list(FACTOR = mat.contrast),
                     data = minibase)


        vector_armado <- 1:ncol(mat.contrast)
        names(vector_armado) <- colnames(mat.contrast)

        selected_k <- nlevels(minibase$"FACTOR")
        df_info_contrastes <- info_acum_general_contrastes(k = selected_k)

        list_contrastes <- summary.aov(model_contrastes, split = list(FACTOR = as.list(vector_armado)))

        list_output <- Hmisc::llist(df_info_contrastes, mat_info_contrastes, list_contrastes)
        list_output
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
      output$contrasts_dynamic_selects <- renderUI({
        ns <- shiny::NS(id)
        mis_niveles <- levels(RR_general()$minibase$"FACTOR")
        cantidad_niveles <- length(mis_niveles)
        cantidad_contrastes <- cantidad_niveles-1

        armado_panel <-  lapply(1:cantidad_contrastes, function(x) {

           armado_niveles <-  lapply(1:cantidad_niveles, function(i) {
              selectInput(ns(paste0("select_", x, "_", i)),
                          label = mis_niveles[i],#paste("Select Input", i),
                          choices = c("Select group..." = "", "g1", "g2", "---"))
            })

           intro_contrast <- paste0("Orthogonal Contrast ", x, " of ", cantidad_contrastes)
           tabPanel(paste0("C",x),
                    br(),
                    h3(intro_contrast),
                    armado_niveles)

            })

        do.call(tabsetPanel, armado_panel)

    })

      list_vector_contrasts_original <- reactive({

        ns <- shiny::NS(id)
        mis_niveles <- levels(RR_general()$minibase$"FACTOR")
        cantidad_niveles <- length(mis_niveles)
        cantidad_contrastes <- cantidad_niveles-1

      aver_rejunte <-   lapply(1:cantidad_contrastes, function(x) {

        vector_contraste <-   lapply(1:cantidad_niveles, function(i) {

          input[[paste0("select_", x, "_", i)]]
        })
        vector_contraste
      })

      #print(aver_rejunte)
       aver_rejunte

      })

      matrix_contrasts_original <- reactive({

        new_matrix <- do.call(cbind, list_vector_contrasts_original())
        colnames(new_matrix) <- paste0("C", 1:ncol(new_matrix))
        new_matrix
      })

      matrix_contrasts_mod <- reactive({

        selected_matrix <- matrix_contrasts_original()
        new_matrix <- apply(selected_matrix, 2, function(vector_grupos){

          #vector_grupos <- unlist(contrasts_selected_groups())
          new_vector01 <- rep(NA, length(vector_grupos))
          dt_g1_pos <- vector_grupos == "g1"
          dt_g2_neg <- vector_grupos == "g2"
          dt_sin_grupo <- vector_grupos == "---"

          new_vector01[dt_g1_pos] <-    1
          new_vector01[dt_g2_neg] <-   -1
          new_vector01[dt_sin_grupo] <- 0
          cantidad_pos <- sum(new_vector01 > 0)
          cantidad_neg <- sum(new_vector01 < 0)

          new_vector02 <- rep(NA, length(new_vector01))
          new_vector02[dt_g2_neg] <- -cantidad_pos
          new_vector02[dt_g1_pos] <-  cantidad_neg
          new_vector02[dt_sin_grupo] <- 0

          if(sum(is.na(new_vector02)) == 0){
            if(cantidad_pos > cantidad_neg) new_vector02 <- -new_vector02
          }
          #print( new_vector02)
          new_vector02
        })


        colnames(new_matrix) <- paste0("C", 1:ncol(new_matrix))
        new_matrix
      })

      output$output_matrix_contrasts_final <- renderPrint({

        #matrix_contrasts_original()
        matrix_contrasts_mod()
      })

      output$output_matrix_contrasts_original <- renderPrint({

        #matrix_contrasts_original()
        matrix_contrasts_original()
      })


      check_matrix_contraste_mod <- reactive({

        req(matrix_contrasts_mod())

        #print(vector_contraste())

        check_cantidad_vacios <- sum(matrix_contrasts_original() == "") == 0
        validate(
          need(check_cantidad_vacios, "Assign a category for each factor level in each orthogonal contrast.")
        )

        check_cantidad_positivos <- sum(matrix_contrasts_mod() > 0) > 0
        validate(
          need(check_cantidad_positivos, "You must put together two groups in each contrast.")
        )

        check_cantidad_negativos <- sum(matrix_contrasts_mod() < 0) > 0
        validate(
          need(check_cantidad_negativos, "You must put together two groups in each contrast.")
        )

        check_ortogonalidad <- check_mat_full_orthogonality(matrix_contrasts_mod())
        validate(
          need(check_ortogonalidad, "Change your choice made. There are contrasts that are not orthogonal to each other.")
        )

        only_col_cero <- apply(matrix_contrasts_mod(), 2, function(x){
          sum(x == 0) == length(x)
        })

        check_no_cero <- sum(only_col_cero) == 0

        validate(
          need(check_no_cero, "Debe armar dos grupos en cada contraste.")
        )

        return(TRUE)

      })
      # output$tab22_contrasts_menu <- renderUI({
      #   ns <- shiny::NS(id)
      #
      #   print( vector_contraste())
      #   div(
      #     fluidRow(
      #       column(12,
      #              #"hola"
      #       #uiOutput(ns("contrasts_dynamic_selects"))
      #       )
      #     )
      #   )
      #
      # })

      output$tab22_contrastsA <- renderPrint({

        req(control_user_02())

        mi_lista <-  RR_g02_contrastes_A()




        #Vector con nombres de elementos a ver
        #selected_objs <- c("df_info_contrastes", "texto_contrastes")
        selected_objs <- c("texto_contrastes")

        # Usar lapply para mostrar los elementos deseados
        cat(mi_lista[selected_objs][[1]])

      })
      output$tab22_contrastsB <- renderPrint({

        req(control_user_02())

        mi_lista <- RR_g02_contrastes_B()




        # Vector con nombres de elementos a ver
        selected_objs <- c("mat_info_contrastes", "list_contrastes")


        # Usar lapply para mostrar los elementos deseados
        mi_lista[selected_objs]

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
        selected_objs <- c("df_factor_info", "check_unbalanced_reps")


        # Usar lapply para mostrar los elementos deseados
        mi_lista[selected_objs]

      })

      output$tab03_analysis_anova_obj03 <- renderPrint({

        req(control_user_02())

        mi_lista <- RR_general()




        # Vector con nombres de elementos a ver
        selected_objs <- c("df_table_anova")


        # Usar lapply para mostrar los elementos deseados
        mi_lista[selected_objs]

      })
      output$tab03_analysis_anova_obj04 <- renderPrint({

        req(control_user_02())

        mi_lista <- RR_general()




        # Vector con nombres de elementos a ver
        selected_objs <- c("df_tukey_table")


        # Usar lapply para mostrar los elementos deseados
        mi_lista[selected_objs]

      })
      output$tab03_analysis_anova_obj05 <- renderPrint({

        req(control_user_02())

        mi_lista <- RR_general()




        # Vector con nombres de elementos a ver
        selected_objs <- c("df_model_error")


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
                             tabPanel("Requeriments",  # 05
                                      fluidRow(
                                        column(12,
                                               h1("Anova 1 way"),
                                               uiOutput(ns("tab02_analysis_anova_FULL"))
                                        )
                                      )
                             ),
                             tabPanel("Plots - Raw Data",  # 05,
                                      fluidRow(column(12, h1("Anova 1 way"))),
                                      fluidRow(
                                        #column(1),
                                        column(12,
                                               #plotOutput(ns("tab04_plots")),
                                               br(),
                                               #br()

                                               shinycssloaders::withSpinner(uiOutput(ns("plot_outputs33"))),
                                        )
                                      )
                             ),
                             tabPanel("Plots - Residuals",  # 05,
                                      fluidRow(column(12, h1("Anova 1 way"))),
                                      fluidRow(
                                        #column(1),
                                        column(12,
                                               #plotOutput(ns("tab04_plots")),
                                               br(),
                                               #br()

                                               shinycssloaders::withSpinner(uiOutput(ns("plot_outputs66"))),
                                        )
                                      )
                             ),
                             tabPanel("Contrasts",  # 05,
                                      fluidRow(column(12, h1("Anova 1 way"))),

                                      h2("1) Information"),
                                      shinycssloaders::withSpinner(verbatimTextOutput(ns("tab22_contrastsA"))),
                                      br(), br(),

                                      h2("2) Level selection to contrast"),
                                      fluidRow(
                                        column(6,
                                               h2("Contrast Matrix"),
                                               verbatimTextOutput(ns("output_matrix_contrasts_original")),
                                               verbatimTextOutput(ns("output_matrix_contrasts_final"))),

                                        column(6,
                                               h2("Contrast Selection"),
                                               uiOutput(ns("contrasts_dynamic_selects")))
                                      ),

                                      br(), br(),

                                      h2("3) Contrast"),
                                      #uiOutput(ns("tab22_contrasts_menu")),

                                        #column(1),
                                        column(12,
                                               #plotOutput(ns("tab04_plots")),
                                               br(),
                                               #br()

                                          shinycssloaders::withSpinner(verbatimTextOutput(ns("tab22_contrastsB"))),
                                          br(), br(), br(),br(),br()
                                        )

                             ),
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

