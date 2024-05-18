
# library("Rscience")
# rm(list = ls())
# source("global.R")


app_05_Rscience <- function(){


  library("bslib")
  library("dplyr")
  library("DT")
  library("EnvStats")
  library("Hmisc")
  library("openxlsx")  # Para archivos XLSX
  library("plotly")
  library("rclipboard")
  library("rmarkdown")
  library("shiny")
  library("shinycssloaders")
  library("shinydashboard")
  library("shinyjs")
  library("shinyWidgets")
  library("stringr")
  library("tools")
  library("emmeans")
  library("Rscience")

  ui <- shinydashboard::dashboardPage(

    # # # Dashboard title
    shinydashboard::dashboardHeader(title = "R-Science"),

    # # # Sidebar content
    shinydashboard::dashboardSidebar(
      width = "340px",
      shinydashboard::sidebarMenu(
        "Free Distribution - 0.0.1",br(),br(),
        shinydashboard::menuItem(text = "database", tabName = "tab01_database", icon = shiny::icon("th")),
        shinydashboard::menuItem(text = "Summary", tabName = "tab03_anova", icon = shiny::icon("th")),

        shinydashboard::menuItem(text = "Kruskal-Wallis", tabName = "tab02_anova", icon = shiny::icon("th")),
        shinydashboard::menuItem(text = "Friedman", tabName = "tab04_anova", icon = shiny::icon("th"))


      )
    ),

    # # # Body content
    shinydashboard::dashboardBody(

      # # # Selected tab on sidebar
      tags$style(
        shiny::HTML('/* active selected tab in the sidebarmenu */  /*La elegida del menu lateral*/
            .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
            background-color: green;
            }'),
        shiny::HTML('.sidebar-menu { font-size: 28px; }'),
        shiny::HTML('.treeview-menu > li > a { font-size: 28px; }'),
        # shiny::HTML("
        #   /* Cambiar el ancho del encabezado */
        #   .main-header {
        #     width: 250px; /* Puedes ajustar el valor según tus necesidades */
        #   }
        # # "),
        # shiny::HTML("
        #   /* Ajustar la posición del botón que colapsa el menú lateral */
        #   .sidebar-toggle {
        #     top: 10px; /* Puedes ajustar el valor según tus necesidades */
        #     right: 50px; /* Puedes ajustar el valor según tus necesidades */
        #   }
        # ")
      ),




      # tags$head(tags$style(HTML('
      #                             /* logo - Donde va R-science*/
      #                             .skin-blue .main-header .logo {
      #                             background-color: orange;
      #                             }
      #
      #                             /* logo when hovered - Cuando le pone el mouse arriba al R-science*/
      #                             .skin-blue .main-header .logo:hover {
      #                             background-color: #f4b943;
      #                             }
      #
      #                             /* navbar (rest of the header) */
      #                             .skin-blue .main-header .navbar {
      #                             background-color: #000000;
      #                             }
      #
      #                             /* main sidebar */ /* Color de las opciones laterales no elegidas */
      #                             .skin-blue .main-sidebar {
      #                             background-color: #f4b943;
      #                             }
      #
      #                             /* active selected tab in the sidebarmenu */  /*La elegida del menu lateral*/
      #                             .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
      #                             background-color: #ffffff;
      #                             }
      #
      #                             /* other links in the sidebarmenu */
      #                             .skin-blue .main-sidebar .sidebar .sidebar-menu a{
      #                             background-color: #00ff00;
      #                             color: #000000;
      #                             }
      #
      #                             /* other links in the sidebarmenu when hovered */
      #                             .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
      #                             background-color: #ff69b4;
      #                             }
      #                             /* toggle button when hovered  */
      #                             .skin-blue .main-header .navbar .sidebar-toggle:hover{
      #                             background-color: #ff69b4;
      #                             }
      #
      #                             /* body */
      #                             .content-wrapper, .right-side {
      #                             background-color: #7da2d1;
      #                             }
      #
      #                             '))),
      tags$style(HTML('
                                /* logo - Donde va R-science*/
                                .skin-blue .main-header .logo {
                                font-size: 32px
                                }'
      )
      ),
      # # # Super h2()
      tags$style(
        #HTML('.custom-h2 { font-weight: bold; }') # Estilos CSS para aplicar negrita solo a la clase custom-h2
        #HTML('.custom-h2 { font-weight: bold; }') # Estilos CSS para aplicar negrita solo a la clase custom-h2
        shiny::HTML(" h2 { font-weight: bold; }")
      ),


      # # # SelectInput settings...
      tags$style(shiny::HTML('.selectize-input { font-size: 32px; line-height: 32px;}')), # Estilos CSS para aumentar el tamaño de letra
      tags$style(shiny::HTML('.selectize-dropdown { font-size: 28px; line-height: 28px; }')), # Estilos CSS para aumentar el tamaño de letra
      tags$style(shiny::HTML('.control-label { font-size: 28px; }')), # Estilos CSS para cambiar el tamaño de letra del label

      # # # fileInput settings...
      tags$style(shiny::HTML('.btn-file { font-size: 28px; }')), # Estilos CSS para cambiar el tamaño de letra del botón adjunto


      # # # TabPanel settings...
      tags$style(
        shiny::HTML(".tabbable > .nav > li > a    {background-color: orange;  color:black; font-size: 30px;}
            .tabbable > .nav > li[class=active]    > a {background-color: green; color:white}")
      ),

      # # # Tabpanel style
      # tags$style(
      #   "li a {
      #     font-size: 30px;
      #     font-weight: bold;
      #   }
      # "
      # ),

      # tags$style(HTML(".verbatim-text-output pre {
      #         color:black;
      #         line-height: 3;
      #         background: white;
      #         font-size: 20px;
      #         font-weight: bold;}")), # Ajusta el interlineado entre elementos

      # # # verbatimTextOutput style
      tags$style(
        "pre {
        color:black;
        background: white;
        font-size: 20px;
        font-weight: bold;
        line-height: 2;
      }
    "
      ),

      tags$style(HTML("
      .shiny-table {
        font-size: 28px; /* Cambiar el tamaño de letra aquí */
      }
    ")),

      tags$style(shiny::HTML("
    .shiny-output-error-validation {
      font-size: 28px; /* Cambia el tamaño de letra de los mensajes de validación */
    }
  ")),

      # # # Tab items
      shinydashboard::tabItems(
        # 1) Data base selection
        shinydashboard::tabItem(tabName = "tab01_database",
                                h1("Import database"),
                                fluidRow(
                                  column(3,
                                         shiny::selectInput(inputId = "file_source",
                                                            label = "File source...",
                                                            choices = c("Select one..." = "",
                                                                        "01 - UNC - Bio01" = "UNC_bio01",
                                                                        "02 - R examples" = "R_example",
                                                                        "03 - xlsx" = "xlsx"
                                                            ))
                                  ),#),
                                  #br(),
                                  #fluidRow(
                                  column(9,
                                         shiny::conditionalPanel(condition = 'input.file_source == "xlsx"',
                                                                 module_cpiE000_database_s01_excel_ui(id = "data_excel")
                                         ),

                                         shiny::conditionalPanel(condition = 'input.file_source == "R_example"',
                                                                 module_cpiE000_database_s02_example_ui(id = "data_example")
                                         ),
                                         shiny::conditionalPanel(condition = 'input.file_source == "UNC_bio01"',
                                                                 module_cpiE000_database_s03_UNC_bio01_ui(id = "data_UNC_bio01")
                                         )
                                  )
                                ),


                                shiny::conditionalPanel(condition = 'input.file_source == "xlsx"',
                                                        module_cpiE000_database_s01_excel_ui2(id = "data_excel")
                                ),

                                shiny::conditionalPanel(condition = 'input.file_source == "R_example"',
                                                        module_cpiE000_database_s02_example_ui2(id = "data_example")
                                ),
                                shiny::conditionalPanel(condition = 'input.file_source == "UNC_bio01"',
                                                        module_cpiE000_database_s03_UNC_bio01_ui2(id = "data_UNC_bio01")
                                )
        ),

        # 2) ANOVA
        shinydashboard::tabItem(tabName = "tab02_anova",
                                h1("Kruskal-Wallis test"),
                                fluidRow(
                                  column(12,
                                         box(
                                           title = "Database info",
                                           status = "primary",
                                           id = "my_box01",
                                           solidHeader = TRUE,
                                           collapsible = TRUE,
                                           collapsed = TRUE,
                                           #closable = TRUE,# Colapsado por defecto
                                           width = 12,
                                           tableOutput("intro_source_database")
                                         )
                                  )
                                ),
                                # https://cran.r-project.org/web/packages/shinydashboardPlus/vignettes/improved-boxes.html
                                box(


                                  title = "Var selection",
                                  status = "primary",
                                  id = "my_box02",
                                  solidHeader = TRUE,
                                  collapsible = TRUE,
                                  closable = FALSE,# Colapsado por defecto
                                  collapsed = FALSE,
                                  width = 12,
                                  module_cpiE001_s01_varselection_ui(id = "anova01_A"),
                                  #actionButton("toggle_box", "Toggle Box"),
                                  br(), br(), br()),
                                fluidRow(
                                  column(12, module_cpiE001_s02_rscience_ui(id = "anova01_B")
                                  )),
                                br(), br(), br()
        ),

        # 2) ANOVA
        shinydashboard::tabItem(tabName = "tab03_anova",
                                h1("Resumen"),
                                fluidRow(
                                  column(12,
                                         box(
                                           title = "Database info",
                                           status = "primary",
                                           id = "my_box03A",
                                           solidHeader = TRUE,
                                           collapsible = TRUE,
                                           collapsed = TRUE,
                                           #closable = TRUE,# Colapsado por defecto
                                           width = 12,
                                           tableOutput("intro_source_database3")
                                         )
                                  )
                                ),
                                # https://cran.r-project.org/web/packages/shinydashboardPlus/vignettes/improved-boxes.html
                                box(
                                  title = "Var selection",
                                  status = "primary",
                                  id = "my_box03B",
                                  solidHeader = TRUE,
                                  collapsible = TRUE,
                                  closable = FALSE,# Colapsado por defecto
                                  collapsed = FALSE,
                                  width = 12,

                                  fluidRow(
                                    column(6,

                                           selectInput(inputId = "amount_vars", label = "Amount of vars",
                                                       choices = c("Select one..." = "",
                                                                   "1 var  - Response Variable" = 1,
                                                                   "2 vars - Response Variable +  Factor" = 2)),
                                    )),br(), br(),


                                  conditionalPanel(condition = "input.amount_vars == 2",
                                                   module_cpiE002_s01_varselection_ui(id = "anova03_A")),
                                  conditionalPanel(condition = "input.amount_vars == 1",
                                                   module_cpiE003_s01_varselection_ui(id = "anova04_A")),
                                  #actionButton("toggle_box", "Toggle Box"),
                                  br(), br(), br()),

                                fluidRow(

                                  column(12, conditionalPanel(condition = "input.amount_vars == 2",
                                                              module_cpiE002_s02_rscience_ui(id = "anova03_B")),
                                         conditionalPanel(condition = "input.amount_vars == 1",
                                                          module_cpiE003_s02_rscience_ui(id = "anova04_B"))

                                  )
                                ),
                                br(), br(), br()
        ),

        # 2) ANOVA
        shinydashboard::tabItem(tabName = "tab04_anova",
                                h1("Friedman test"),
                                fluidRow(
                                  column(12,
                                         box(
                                           title = "Database info",
                                           status = "primary",
                                           id = "my_box04A",
                                           solidHeader = TRUE,
                                           collapsible = TRUE,
                                           collapsed = TRUE,
                                           #closable = TRUE,# Colapsado por defecto
                                           width = 12,
                                           tableOutput("intro_source_database4")
                                         )
                                  )
                                ),
                                # https://cran.r-project.org/web/packages/shinydashboardPlus/vignettes/improved-boxes.html
                                box(


                                  title = "Var selection",
                                  status = "primary",
                                  id = "my_box04B",
                                  solidHeader = TRUE,
                                  collapsible = TRUE,
                                  closable = FALSE,# Colapsado por defecto
                                  collapsed = FALSE,
                                  width = 12,
                                  module_cpiE004_s01_varselection_ui(id = "anova05_A"),
                                  #actionButton("toggle_box", "Toggle Box"),
                                  br(), br(), br()),
                                fluidRow(
                                  column(12, module_cpiE004_s02_rscience_ui(id = "anova05_B")
                                  )),
                                br(), br(), br()
        )

      )
    )

  )







  server <- function(input, output, session) {

    # observeEvent(input$toggle_box, {
    #   updateBox("my_box", action = "toggle")
    # })

    # # # Initial inputs
    # - all_var_names()
    # - database

    # # # Intro source database


    input_general_01_xlsx <- module_cpiE000_database_s01_excel_server(id = "data_excel",
                                                                      input_file_source = input$file_source)

    input_general_02_example <- module_cpiE000_database_s02_example_server(id = "data_example",
                                                                           input_file_source = input$file_source)

    input_general_03_UNC_bio01 <- module_cpiE000_database_s03_UNC_bio01_server(id = "data_UNC_bio01",
                                                                               input_file_source = input$file_source)

    #input_general <- reactiveVal(NULL)


    input_general <- reactive({

      if (input$file_source == "xlsx")  input_general_01_xlsx() else
        if (input$file_source == "R_example") input_general_02_example() else
          if (input$file_source == "UNC_bio01") input_general_03_UNC_bio01() else NULL
    })


    ################################################
    output$intro_source_database <- renderTable({


      list_intro <- input_general()$intro_source_database
      df_output <- as.data.frame(list_intro)
      colnames(df_output) <- names(list_intro)
      df_output



    })

    output$intro_source_database3 <- renderTable({


      list_intro <- input_general()$intro_source_database
      df_output <- as.data.frame(list_intro)
      colnames(df_output) <- names(list_intro)
      df_output



    })

    output$intro_source_database4 <- renderTable({


      list_intro <- input_general()$intro_source_database
      df_output <- as.data.frame(list_intro)
      colnames(df_output) <- names(list_intro)
      df_output



    })
    ##################################################################################
    input_01_anova <- module_cpiE001_s01_varselection_server(id = "anova01_A",
                                                             input_general = input_general)



    module_cpiE001_s02_rscience_server(id = "anova01_B",
                                       input_general = input_general,
                                       input_01_anova = input_01_anova)

    ##################################################################################
    input_03_anova <- module_cpiE002_s01_varselection_server(id = "anova03_A",
                                                             input_general = input_general)



    module_cpiE002_s02_rscience_server(id = "anova03_B",
                                       input_general = input_general,
                                       input_01_anova = input_03_anova)
    ##################################################################################
    input_04_anova <- module_cpiE003_s01_varselection_server(id = "anova04_A",
                                                             input_general = input_general)



    module_cpiE003_s02_rscience_server(id = "anova04_B",
                                       input_general = input_general,
                                       input_01_anova = input_04_anova)
    ##################################################################################
    input_05_anova <- module_cpiE004_s01_varselection_server(id = "anova05_A",
                                                             input_general = input_general)



    module_cpiE004_s02_rscience_server(id = "anova05_B",
                                       input_general = input_general,
                                       input_01_anova = input_05_anova)

    ##################################################################################
  }





  shinyApp(ui = ui, server = server, options = list(launch.browser = TRUE))

}



