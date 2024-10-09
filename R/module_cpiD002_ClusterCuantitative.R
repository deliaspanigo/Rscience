
# # # Module 01 - ANOVA
# UI and SERVER for all modules respect to ANOVA


module_cpiD002_s02B_varselection_ui <- function(id){

  ns <- shiny::NS(id)

  div(
    uiOutput(ns("intro_source_database"))

  )
}




module_cpiD002_s01_varselection_ui <- function(id){

  ns <- shiny::NS(id)

  div(
    uiOutput(ns("vars_selection"))

  )
}




module_cpiD002_s01_varselection_server <- function(id, input_general){
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



      # # # Var selection for Cluster - Cuantitative vars
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
                                           label = "Col var labels",
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
              column(4,
                     radioButtons(inputId = ns("method_cluster"), label = "Method",
                                  choices = c("Variance/Covariance Matrix" = F,
                                              "Correlation Matrix" = T)
                     )
              )
            ),
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

      })

      observeEvent(input$method_cluster, {

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

      selected_method <- reactive({

        req(action_button_show())

        output_value <- as.logical(as.character(input$method_cluster))
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
                         alpha_value(), intro_source_database(),
                         selected_method())

        names(the_list) <- c("selected_var_name", "selected_var_labels",
                             "alpha_value", "intro_source_database",
                             "selected_method")
        the_list
      })


      return(output_list)
    })
}






module_cpiD002_s02_rscience_ui <- function(id){

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




module_cpiD002_s02_rscience_server <- function(id, input_general, input_01_anova){
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

        check_previous <- fn_cpiD002_control_previous(database = input_general()$database,
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

      k_groups_01 <- reactive({ 6 })

      # output$k_selection2 <- renderUI({
      #
      #   req(k_groups_01())
      #   ns <- shiny::NS(id)
      #
      #   vector_g <- 2:k_groups_01()
      #   vector_g <- c("Select one..." = "", vector_g)
      #   vector_g <- 4
      #   div(
      #   selectInput(inputId = ns("k_groups_02"),
      #               label = "Estudio previo de grupos",
      #               choices = vector_g)
      #   )
      # })
      #
      # k_groups_02 <- reactive({
      #   req(input$k_groups_02)
      #
      #   as.numeric(as.character(input$k_groups_02))
      # })


      RR01 <- reactive({

        req(control_user_01(), k_groups_01())

        #if(input$k_groups_01 == "") return(NULL)

        database = input_general()$database
        selected_var_name = input_01_anova()$selected_var_name
        selected_var_labels = input_01_anova()$selected_var_labels
        alpha_value = input_01_anova()$alpha_value
        k_groups_01 <- k_groups_01()
        selected_method <- input_01_anova()$selected_method
        ##############################################

        # Rscience code
        vector_all_vars <- c(selected_var_labels, selected_var_name)
        minibase <- database[vector_all_vars]

        vector_labels <- as.character(minibase[,selected_var_labels])

        minibase2 <- minibase[selected_var_name]


        k_colors_01 <- rainbow(k_groups_01)
        #################################################
        # Realiza la estandarizacion:

        z <- minibase2[, -1]
        z <- as.matrix(z)

       if(selected_method) z <- scale(z)

        N <- ncol(z)
        S <- dist(z, method = "euclidean")/sqrt(N)
        #clusterS <- hclust(S, method = "single")
        #clusterC <- hclust(S, method = "complete")
        clusterA <- hclust(d = S, method = "average")
        #clusterW <- hclust(S, method = "ward.D2")


        # correlación cofenética:
        matrix_cophenetic <- cophenetic(clusterA)
        cophenetic_correlation_value <- cor(matrix_cophenetic, S, method = "pearson")


        ############ Agrupamiento jerarquico  (MODO R_ Variables) ############
        t_z <- t(z)
        t_N <- ncol(t_z)
        t_S <- dist(t_z, method = "euclidean")/sqrt(t_N)
        t_clusterA <- hclust(t_S, method = "average")



        output_list <- Hmisc::llist(vector_all_vars, minibase, minibase2,
                                    k_groups_01,  k_colors_01, z, N,
                                    S, clusterA,
                                    matrix_cophenetic, cophenetic_correlation_value,
                                    t_z, t_N, t_S, t_clusterA
                                    )

        #return(output_list)

        output_list


      })

      RR02 <- reactive({

        req(control_user_01())
        #req(control_user_01(), input$k_groups_02)

        #if(input$k_groups_01 == "") return(NULL)

        database = input_general()$database
        selected_var_name = input_01_anova()$selected_var_name
        selected_var_labels = input_01_anova()$selected_var_labels
        alpha_value = input_01_anova()$alpha_value
        selected_method <- input_01_anova()$selected_method
        #k_groups_02 <- as.numeric(as.character(input$k_groups_02)) #k_groups_02()

        ##############################################

        # Rscience code
        vector_all_vars <- c(selected_var_labels, selected_var_name)
        minibase <- database[vector_all_vars]

        vector_labels <- as.character(minibase[,selected_var_labels])

        minibase2 <- minibase[selected_var_name]

        #k_colors_02 <- rainbow(k_groups_02)
        #################################################
        # Realiza la estandarizacion:
        #z <- minibase2[, -1]
        z <- as.matrix(minibase2)

        if(selected_method) z <- scale(z)

        #z <- scale(minibase2[, -1])
        N <- ncol(z)
        S <- dist(z, method = "euclidean")/sqrt(N)

        df_z <- as.data.frame(z)


        matrix_distances01 <- as.matrix(S)
        colnames(matrix_distances01) <- 1:ncol(matrix_distances01)
        rownames(matrix_distances01) <- 1:nrow(matrix_distances01)

        dt_pos <- lower.tri(matrix_distances01, diag = TRUE)
        matrix_distances02 <- round(matrix_distances01,4)
        matrix_distances02[!dt_pos] <- ""
        matrix_distances02 <- as.data.frame(matrix_distances02)
        colnames(matrix_distances02) <- 1:ncol(matrix_distances02)
        rownames(matrix_distances02) <- 1:nrow(matrix_distances02)

        matrix_distances03 <- as.data.frame(matrix_distances01)
        colnames(matrix_distances03) <- 1:ncol(matrix_distances03)
        rownames(matrix_distances03) <- 1:nrow(matrix_distances03)



        #clusterS <- hclust(S, method = "single")
        #clusterC <- hclust(S, method = "complete")
        clusterA <- hclust(d = S, method = "average")
        #clusterW <- hclust(S, method = "ward.D2")


        # correlación cofenética:
        matrix_cophenetic <- cophenetic(clusterA)
        cophenetic_correlation_value <- cor(matrix_cophenetic, S, method = "pearson")


        ############ Agrupamiento jerarquico  (MODO R_ Variables) ############
        t_z <- t(z)
        t_N <- ncol(t_z)
        t_S <- dist(t_z, method = "euclidean")/sqrt(t_N)
        t_clusterA <- hclust(t_S, method = "average")


        k_groups_02 <- NA
        k_colors_02 <- NA
        output_list <- Hmisc::llist(vector_all_vars, minibase, minibase2,
                                    k_groups_02,  k_colors_02, z, N,
                                    S, matrix_distances01, matrix_distances02,
                                    matrix_distances03, df_z,
                                    clusterA,
                                    matrix_cophenetic, cophenetic_correlation_value,
                                    t_z, t_N, t_S, t_clusterA
        )

        #return(output_list)

        output_list


      })


      # # # All anova results
      # RR_general <- reactive({
      #
      #   req(control_user_01())
      #
      #   the_output <- fn_cpiD002_results(database = input_general()$database,
      #                                    selected_var_name = input_01_anova()$selected_var_name,
      #                                    selected_var_labels = input_01_anova()$selected_var_labels,
      #                                    alpha_value = input_01_anova()$alpha_value)
      #
      #
      #   the_output
      #
      # })
      #
      #

      # # # Control for initial inputs
      control_user_02 <- reactive({
        #
        req(control_user_01())
        #
        validate(
          #need(!is.null(RR_general()), "Error 03: Module anova s02 - RR_general() can not be NULL.")
          need(!is.null(RR01()), "Error 03: Module anova s02 - RR01() can not be NULL.")

        )
        #
        #
        #
        #
        #         # Control post
        # check_post <- fn_cpiD002_control_post(list_results_from_cpiA001_anova1way = RR_general())
        # #
        # validate(
        #   need(check_post$check_ok, check_post$text_output)
        # )

        return(TRUE)
      })





      RR_code <- reactive({

        req(control_user_02())
        the_code <- fn_cpiD002_code_sectionALL(intro_source_database = input_01_anova()$intro_source_database,
                                                                vr_var_name = input_01_anova()$vr_var_name,
                                                                factor_var_name = input_01_anova()$factor_var_name,
                                                                alpha_value = input_01_anova()$alpha_value)
        the_code
      })

      ####################################################################################


      # # # Tab01 - Minibase
      output$tab01_minibase <- DT::renderDT({

        req(control_user_02())

        #mi_lista <- RR_general()
        mi_lista <- RR02()



        # Vector con nombres de elementos a ver
        nombres_a_ver <- c("minibase")


        # Usar lapply para mostrar los elementos deseados
        #mi_lista[nombres_a_ver]

        mi_tabla <- mi_lista[[nombres_a_ver]]
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


        #mi_lista <- RR_general()

        mi_lista <- RR02()

        nombres_a_ver <- names(mi_lista)
#
#         # Vector con nombres de elementos a ver
#         nombres_a_ver <- c("matrix_distances", "matrix_distances02",
#                            "matrix_distances03", "amount_tde",
#                            "list_cluster", "matrix_cor_cophenetic",
#                            "cor_pearson_value")

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
      output$tab02_analysis_df01 <-  shiny::renderPrint({

        req(control_user_02())


        #mi_lista <- RR_general()
        mi_lista <- RR02()


        nombres_a_ver <- c("matrix_distances01")
        # Vector con nombres de elementos a ver
        #nombres_a_ver <- c("matrix_distances")

        # Usar lapply para mostrar los elementos deseados
        mi_lista[nombres_a_ver]

      })

      output$tab02_analysis_df02 <-  shiny::renderPrint({

        req(control_user_02())


        #mi_lista <- RR_general()


        mi_lista <- RR02()
        nombres_a_ver <- c("matrix_distances02")

        # Vector con nombres de elementos a ver
        #nombres_a_ver <- c("matrix_distances02")

        # Usar lapply para mostrar los elementos deseados
        mi_lista[nombres_a_ver]

      })

      output$tab02_analysis_df03 <-  shiny::renderPrint({

        req(control_user_02())


        #mi_lista <- RR_general()

        mi_lista <- RR02()


        nombres_a_ver <- c("matrix_distances03")


        # Vector con nombres de elementos a ver
        #nombres_a_ver <- c("matrix_distances03")

        # Usar lapply para mostrar los elementos deseados
        mi_lista[nombres_a_ver]

      })

      output$tab02_analysis_df04 <- DT::renderDT({

        req(control_user_02())

        #mi_lista <- RR_general()

        mi_lista <- RR02()


        #nombres_a_ver <- c("S")
        nombres_a_ver <- c("matrix_distances02")

        # Vector con nombres de elementos a ver
        #nombres_a_ver <- c("matrix_distances03")


        # Usar lapply para mostrar los elementos deseados
        #mi_lista[nombres_a_ver]

        mi_tabla <- mi_lista[[nombres_a_ver]]
        #mi_tabla <- as.data.frame(as.matrix(mi_tabla))
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
        )%>%
          formatRound(columns = names(mi_tabla), digits = 4)

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
          #
          # h2("1) Requeriment - Normality test - Residuals"),
          # verbatimTextOutput(ns("tab02_analysis_df03")),
          # br(), br(), br(),

          h2("1) Lower triangular and diagonal matrix - Euclidean Distance"),
          h3("R object: matrix_distances02"),
          DTOutput(ns("tab02_analysis_df04")),
          br(), br(), br()

        )

      })
      ###################################################################

      # # # Tab01 - Anova Results
      output$tab01_all_anova_results <- renderPrint({

        req(control_user_02())

        #RR_general()
        RR02()

        #nombres_a_ver <- names(mi_lista)
      })

      ##########################################################################





      #############################################################

      # output$el_plot1 <- renderPlot({
      #
      #   req(RR01())
      #
      #   mi_lista <- RR01()
      #   new_plot <-  with(mi_lista,{
      #     # # # Create a new plot...
      #     # # # Create a new plot...
      #     # # # Create a new plot...
      #     factoextra::fviz_nbclust(x = z, FUN = hcut,
      #                              diss = S, method = "wss",
      #                              #nboot = TRUE,
      #                              k.max = k_groups_01,
      #                              linecolor = "black")
      #
      #   })
      #
      #   new_plot
      # })
      #


      output$el_plot2 <- renderPlot({

        req(RR02())

        mi_lista <- RR02()
        new_plot <-  with(mi_lista,{
          # # # Plot 002
          plot(clusterA, hang = -1,
               main = "Plot 001 - Cluster",
               xlab="Label",
               ylab="Distance")

        })

        new_plot
      })


      output$el_plot3 <- renderPlot({

        req(RR02())

        mi_lista <- RR02()
        new_plot <-  with(mi_lista,{
          # # # Plot 003
          factoextra::fviz_dend(clusterA, k = k_groups_02,
                    k_colors = k_colors_02,
                    rect = TRUE, rect_fill = TRUE, horiz = TRUE,
                    color_labels_by_k = FALSE)

        })

        new_plot
      })


      output$el_plot4 <- renderPlot({

        req(RR02())

        mi_lista <- RR02()
        new_plot <-  with(mi_lista,{
          # # # Plot 003
          plot(t_clusterA, hang = -1, main = "Plot 002 - Hierarchical Grouping")


        })

        new_plot
      })


      output$el_plot5 <- renderPlot({

        req(RR02())

        mi_lista <- RR02()
        new_plot <-  with(mi_lista,{
          # # # Plot 004

          stats::heatmap(z, col = rev(heat.colors(256)),
                         main = "Plot 003 - Heatmap")
          leyenda <- as.raster(matrix(heat.colors(256), ncol = 1))
          graphics::rasterImage(leyenda, xleft = 0.85, xright = 0.9, ybottom = 0.85,
                      ytop = 0.95)
          text(x = 0.92, y = c(0.85, 0.9, 0.95), labels = c(-2, 0, 2))
          text(x = 0.875, y = 1, label = "z", cex = 2)



        })

        new_plot
      })
      # output$el_plot3 <- renderPlotly({
      #
      #
      #   mi_lista <- RR_general()
      #
      #
      #   new_plot <-  with(mi_lista,{
      #     # # # Create a new plot...
      #     # # # Create a new plot...
      #     plot003_factor <- plot_ly()
      #
      #
      #     # # # Adding errors...
      #     plot003_factor <-   add_trace(p = plot003_factor,
      #                                   type = "scatter",
      #                                   mode = "markers",
      #                                   x = df_table_plot003$level,
      #                                   y = df_table_plot003$mean,
      #                                   color = df_table_plot003$level,
      #                                   colors = df_table_plot003$color,
      #                                   marker = list(symbol = "line-ew-open",
      #                                                 size = 50,
      #                                                 opacity = 1,
      #                                                 line = list(width = 5)),
      #                                   error_y = list(type = "data", array = df_table_plot003$standard_error)
      #     )
      #
      #
      #     #plot002_factor
      #
      #     plot003_factor <- plotly::layout(p = plot003_factor,
      #                                      xaxis = list(title = "FACTOR"),
      #                                      yaxis = list(title = "VR"),
      #                                      title = "Plot 003 - Mean and Standard Error",
      #                                      font = list(size = 20),
      #                                      margin = list(t = 100))
      #
      #     # # # Without zerolines
      #     plot003_factor <-plotly::layout(p = plot003_factor,
      #                                     xaxis = list(zeroline = FALSE),
      #                                     yaxis = list(zeroline = FALSE))
      #
      #
      #     # # # Plot output
      #     plot003_factor
      #
      #   })
      #
      #   new_plot
      # })
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

        #mi_lista <- RR_general()


        mi_lista <- RR02()


        nombres_a_ver <- c("S")

        # Vector con nombres de elementos a ver
        #nombres_a_ver <- c("matrix_distances03")


        # Usar lapply para mostrar los elementos deseados
        mi_lista[nombres_a_ver]

      })

      output$tabla02 <- renderPrint({

        req(control_user_02())

        #mi_lista <- RR_general()

        mi_lista <- RR02()


        nombres_a_ver <- c("matrix_distances03")


        # Vector con nombres de elementos a ver
        #nombres_a_ver <- c("matrix_distances02")


        # Usar lapply para mostrar los elementos deseados
        mi_lista[nombres_a_ver]

      })

      output$tabla03 <- renderPrint({

        req(control_user_02())

        #mi_lista <- RR_general()


        mi_lista <- RR02()


        nombres_a_ver <- c("S")

        # Vector con nombres de elementos a ver
        #nombres_a_ver <- c("matrix_distances02")


        # Usar lapply para mostrar los elementos deseados
        mi_lista[nombres_a_ver]

      })

      output$tabla04 <- renderPrint({

        req(control_user_02())

        #mi_lista <- RR_general()

        mi_lista <- RR02()


        nombres_a_ver <- c("matrix_cophenetic", "cophenetic_correlation_value")


        # Vector con nombres de elementos a ver
        #nombres_a_ver <- c("matrix_distances02")


        # Usar lapply para mostrar los elementos deseados
        mi_lista[nombres_a_ver]

      })

      output$tabla05 <- renderPrint({

        req(control_user_02())

        #mi_lista <- RR_general()


        mi_lista <- RR02()


        nombres_a_ver <- c("df_z")

        # Vector con nombres de elementos a ver
        #nombres_a_ver <- c("matrix_distances02")


        # Usar lapply para mostrar los elementos deseados
        mi_lista[nombres_a_ver]

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
      #   nombres_a_ver <- c("df_table_plot003")
      #
      #
      #   # Usar lapply para mostrar los elementos deseados
      #   mi_lista[nombres_a_ver]
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
      #   nombres_a_ver <- c("df_table_plot004")
      #
      #
      #   # Usar lapply para mostrar los elementos deseados
      #   mi_lista[nombres_a_ver]
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


      # observeEvent(RR_general(),{
      observeEvent(RR02(),{
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

            #fluidRow(column(12,plotOutput(ns("el_plot1")))),

            fluidRow(column(12, uiOutput(ns("k_selection2")))),

            br(), br(), br(), br(),
            shiny::tabsetPanel(id = ns("super_tabset_panel"),
                               tabPanel("Minibase",
                                        fluidRow(
                                          column(12,
                                                 h1("Cluster - Cuantitative vars"),
                                                 uiOutput(ns("tab01_FULL"))
                                          )
                                        )),
                               # tabPanel("AnalysisB",  # 05
                               #          fluidRow(
                               #            column(12,
                               #                   h1("Cluster - Cuantitative vars"),
                               #                   uiOutput(ns("tab02_analysisB_FULL"))
                               #            )
                               #          )
                               # ),
                               tabPanel("Analysis",  # 05
                                        fluidRow(
                                          column(12,
                                                 h1("Cluster - Cuantitative vars"),
                                                 uiOutput(ns("tab02_analysis_FULL"))
                                          )
                                        )
                               ),

                               tabPanel("Plots",  # 05,
                                        # fluidRow(column(12, h1("Cluster - Cuantitative vars"))),
                                        # fluidRow(
                                        #   #column(1),
                                        #   column(6, plotOutput(ns("el_plot1"), height = "40vh", width = "70vh")),
                                        #   column(6, verbatimTextOutput(ns("tabla01"))),
                                        # ),
                                        # br(),br(),br(),

                                        fluidRow(
                                          #column(1),
                                          column(6, plotOutput(ns("el_plot2"), height = "40vh", width = "70vh")),
                                          column(6, verbatimTextOutput(ns("tabla02"))),
                                        ),
                                        br(),br(),br(),


                                        # fluidRow(
                                        #   #column(1),
                                        #   column(6, plotOutput(ns("el_plot3"), height = "40vh", width = "70vh")),
                                        #   column(6, verbatimTextOutput(ns("tabla03"))),
                                        # ),
                                        # br(),br(),br(),

                                        fluidRow(
                                          #column(1),
                                          column(6, plotOutput(ns("el_plot4"), height = "40vh", width = "70vh")),
                                          column(6, verbatimTextOutput(ns("tabla04"))),
                                        ),
                                        br(),br(),br(),

                                        fluidRow(
                                          #column(1),
                                          column(6, plotOutput(ns("el_plot5"), height = "40vh", width = "70vh")),
                                          column(6, verbatimTextOutput(ns("tabla05"))),
                                        ),
                                        br(),br(),br()
                                        # fluidRow(
                                        #   #column(1),
                                        #   column(6, plotlyOutput(ns("el_plot3"), height = "40vh", width = "70vh")),
                                        #   column(6, verbatimTextOutput(ns("tabla03"))),
                                        # ),
                                        # br(),br(),br(),
                                        # fluidRow(
                                        #   #column(1),
                                        #   column(6, plotlyOutput(ns("el_plot4"), height = "40vh", width = "70vh")),
                                        #   column(6, verbatimTextOutput(ns("tabla04"))),
                                        # )

                                        #shinycssloaders::withSpinner(uiOutput(ns("plot_outputs33"))),
                               ),
                               # tabPanel("Plots - Residuals",  # 05,
                               #          fluidRow(column(12, h1("Cluster - Cuantitative vars"))),
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
                               #          fluidRow(h1("Cluster - Cuantitative vars")),
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
                               #          fluidRow(h1("Cluster - Cuantitative vars")),
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
                               #          fluidRow(h1("Cluster - Cuantitative vars")),
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
                                                 h1("Cluster - Cuantitative vars"),
                                                 verbatimTextOutput(ns("tab01_all_anova_results"))
                                          )
                                        )
                               ),



                               tabPanel("R code",  # 05
                                        fluidRow(
                                          column(10,
                                                 h1("Cluster - Cuantitative vars"),
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

