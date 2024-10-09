
# library("Rscience")
# rm(list = ls())
# source("global.R")


app_02_Rscience <- function(){


  library("Rscience")
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

  ui <- shinydashboard::dashboardPage(

    # # # Dashboard title
    shinydashboard::dashboardHeader(title = "R-Science 0.0.3"),

    # # # Sidebar content
    shinydashboard::dashboardSidebar(
      width = "340px",
      shinydashboard::sidebarMenu(
        " Generalized Linear Models - 0.0.3",br(),br(),
        shinydashboard::menuItem(text = "database", tabName = "tab01_database", icon = shiny::icon("th")),
        shinydashboard::menuItem(text = "Anova 1 way", tabName = "tab02_anova", icon = shiny::icon("th"),
                                 menuSubItem("Binomial", tabName = "tab02_anova_01"),
                                 menuSubItem("Poisson",  tabName = "tab02_anova_02"),
                                 menuSubItem("Gamma",    tabName = "tab02_anova_03"),
                                 menuSubItem("Gaussian", tabName = "tab02_anova_04")
                                 ),
        shinydashboard::menuItem(text = "Simple Linear Regresion", tabName = "tab03_slr", icon = shiny::icon("th"),
                                 menuSubItem("Binomial", tabName = "tab03_slr_01"),
                                 menuSubItem("Poisson",  tabName = "tab03_slr_02"),
                                 menuSubItem("Gamma",    tabName = "tab03_slr_03"),
                                 menuSubItem("Gaussian", tabName = "tab03_slr_04")
        )




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
                                shiny::selectInput(inputId = "file_source",
                                                   label = "File source...",
                                                   choices = c("Select one..." = "",
                                                               "R examples" = "R_example",
                                                               "xlsx" = "xlsx",
                                                               "UNC" = "UNC")),
                                br(),br(),br(),

                                shiny::conditionalPanel(condition = 'input.file_source == "xlsx"',
                                                        module01_database_s01_excel_ui(id = "data_excel")
                                ),

                                shiny::conditionalPanel(condition = 'input.file_source == "R_example"',
                                                        module01_database_s02_example_ui(id = "data_example")
                                ),
                                shiny::conditionalPanel(condition = 'input.file_source == "UNC"',
                                                        module01_database_s03_UNC_ui(id = "data_UNC")
                                )
        ),

        # 2) ANOVA
        shinydashboard::tabItem(tabName = "tab02_anova_01",
                                module_cpiB001_s01_varselection_ui(id = "anova01_A"),
                                br(), br(), br(),
                                module_cpiB001_s02_rscience_ui(id = "anova01_B")
        ),
        shinydashboard::tabItem(tabName = "tab02_anova_02",
                                module_cpiB002_s01_varselection_ui(id = "anova02_A"),
                                br(), br(), br(),
                                module_cpiB002_s02_rscience_ui(id = "anova02_B")
        ),
        shinydashboard::tabItem(tabName = "tab02_anova_03",
                                module_cpiB003_s01_varselection_ui(id = "anova03_A"),
                                br(), br(), br(),
                                module_cpiB003_s02_rscience_ui(id = "anova03_B")
        ),
        shinydashboard::tabItem(tabName = "tab02_anova_04",
                                module_cpiB004_s01_varselection_ui(id = "anova04_A"),
                                br(), br(), br(),
                                module_cpiB004_s02_rscience_ui(id = "anova04_B")
        ),


        # SLR
        shinydashboard::tabItem(tabName = "tab03_slr_01",
                                module_cpiB005_s01_varselection_ui(id = "slr01_A"),
                                br(), br(), br(),
                                module_cpiB005_s02_rscience_ui(id = "slr01_B")
        ),
        shinydashboard::tabItem(tabName = "tab03_slr_02",
                                module_cpiB006_s01_varselection_ui(id = "slr02_A"),
                                br(), br(), br(),
                                module_cpiB006_s02_rscience_ui(id = "slr02_B")
        ),
        shinydashboard::tabItem(tabName = "tab03_slr_03",
                                module_cpiB007_s01_varselection_ui(id = "slr03_A"),
                                br(), br(), br(),
                                module_cpiB007_s02_rscience_ui(id = "slr03_B")
        ),
        shinydashboard::tabItem(tabName = "tab03_slr_04",
                                module_cpiB008_s01_varselection_ui(id = "slr04_A"),
                                br(), br(), br(),
                                module_cpiB008_s02_rscience_ui(id = "slr04_B")
        )

      )
    )

  )







  server <- function(input, output, session) {

    # # # Initial inputs
    # - all_var_names()
    # - database

    input_general_01_xlsx <- module01_database_s01_excel_server(id = "data_excel",
                                                                input_file_source = input$file_source)

    input_general_02_example <- module01_database_s02_example_server(id = "data_example",
                                                                     input_file_source = input$file_source)

    input_general_03_UNC <- module01_database_s03_UNC_server(id = "data_UNC",
                                                                     input_file_source = input$file_source)

    #input_general <- reactiveVal(NULL)


    input_general <- reactive({

      if (input$file_source == "xlsx")  input_general_01_xlsx() else
        if (input$file_source == "R_example") input_general_02_example() else
          if (input$file_source == "UNC") input_general_03_UNC() else NULL
    })


    ##################################################################################
    input_01_anova <- module_cpiB001_s01_varselection_server(id = "anova01_A",
                                                             input_general = input_general)



    module_cpiB001_s02_rscience_server(id = "anova01_B",
                                       input_general = input_general,
                                       input_01_anova = input_01_anova)
    ##################################################################################

    input_02_anova <- module_cpiB002_s01_varselection_server(id = "anova02_A",
                                                             input_general = input_general)



    module_cpiB002_s02_rscience_server(id = "anova02_B",
                                       input_general = input_general,
                                       input_01_anova = input_02_anova)
    ##################################################################################


    input_03_anova <- module_cpiB003_s01_varselection_server(id = "anova03_A",
                                                             input_general = input_general)



    module_cpiB003_s02_rscience_server(id = "anova03_B",
                                       input_general = input_general,
                                       input_01_anova = input_03_anova)
    ##################################################################################

    input_04_anova <- module_cpiB004_s01_varselection_server(id = "anova04_A",
                                                             input_general = input_general)



    module_cpiB004_s02_rscience_server(id = "anova04_B",
                                       input_general = input_general,
                                       input_01_anova = input_04_anova)
    ##################################################################################





    input_01_slr <- module_cpiB005_s01_varselection_server(id = "slr01_A",
                                                             input_general = input_general)



    module_cpiB005_s02_rscience_server(id = "slr01_B",
                                       input_general = input_general,
                                       input_01_anova = input_01_slr)
    ##################################################################################


    input_02_slr <- module_cpiB006_s01_varselection_server(id = "slr02_A",
                                                           input_general = input_general)



    module_cpiB006_s02_rscience_server(id = "slr02_B",
                                       input_general = input_general,
                                       input_01_anova = input_02_slr)
    ##################################################################################



    input_03_slr <- module_cpiB007_s01_varselection_server(id = "slr03_A",
                                                           input_general = input_general)



    module_cpiB007_s02_rscience_server(id = "slr03_B",
                                       input_general = input_general,
                                       input_01_anova = input_03_slr)
    ##################################################################################


    input_04_slr <- module_cpiB008_s01_varselection_server(id = "slr04_A",
                                                           input_general = input_general)



    module_cpiB008_s02_rscience_server(id = "slr04_B",
                                       input_general = input_general,
                                       input_01_anova = input_04_slr)
    ##################################################################################


  }





  shinyApp(ui = ui, server = server, options = list(launch.browser = TRUE))

}



