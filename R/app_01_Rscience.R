
# library("Rscience")
# rm(list = ls())
# source("global.R")


app_01_Rscience <- function(){



  library("bslib")
  library("dplyr")
  library("DT")
  library("EnvStats")
  library("Hmisc")
  library("openxlsx")  # Para archivos XLSX
  library("plotly")
  library("rclipboard")
  library("rmarkdown")
  library("reshape2")
  library("shiny")
  library("shinycssloaders")
  library("shinydashboard")
  library("shinyjs")
  library("shinyWidgets")
  library("stringr")
  library("tools")
  library("gmodels")
  library("Rscience")

ui <- shinydashboard::dashboardPage(

  # # # Dashboard title
  shinydashboard::dashboardHeader(title = "R-Science 0.0.3"),

  # # # Sidebar content
  shinydashboard::dashboardSidebar(
    width = "340px",
    shinydashboard::sidebarMenu(
      " General Linear Models - 0.0.3",br(),br(),
      shinydashboard::menuItem(text = "database", tabName = "tab01_database", icon = shiny::icon("th")),
      shinydashboard::menuItem(text = "Anova 1 way", tabName = "tab02_anova", icon = shiny::icon("th")),
      shinydashboard::menuItem(text = "Anova 1 way with block", tabName = "tab03_anova", icon = shiny::icon("th")),
      shinydashboard::menuItem(text = "Anova 2 ways", tabName = "tab04_anova", icon = shiny::icon("th")),
      shinydashboard::menuItem(text = "Simple Linear Regresion", tabName = "tab06_anova", icon = shiny::icon("th")),
      shinydashboard::menuItem(text = "Doble Linear Regresion", tabName = "tab07_anova", icon = shiny::icon("th")),
      shinydashboard::menuItem(text = "Multiple Linear Regresion", tabName = "tab20_anova", icon = shiny::icon("th")),
      shinydashboard::menuItem(text = "MLR Selection", tabName = "tab21_anova", icon = shiny::icon("th")),
      #shinydashboard::menuItem(text = "Ancova with", tabName = "tab09_anova", icon = shiny::icon("th")),
      #shinydashboard::menuItem(text = "Ancova without", tabName = "tab10_anova", icon = shiny::icon("th")),
      shinydashboard::menuItem(text = "Ancova", tabName = "tab11_anova", icon = shiny::icon("th")),
      shinydashboard::menuItem(text = "Dummy", tabName = "tab14_anova", icon = shiny::icon("th"))




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
    tags$style(HTML("
    .custom-checkbox-group input[type='checkbox'] {
      font-size: 20px; /* Cambia el tamaño de la fuente */
      transform: scale(2);
    }
  ")),
    # tags$style("input[type=checkbox] {
    #             transform: scale(2);
    #             font-size: 20px;
    #             padding: 0.1rem;
    #             border: 0.25rem solid green;
    #             border-radius: 0.5rem;
    #        }"),
  #   tags$style(HTML("
  #   .custom-checkbox-group label {
  #     font-size: 20px; /* Cambia el tamaño de la fuente */
  #        /* Agrega espacio alrededor de los checkboxes */
  #   }
  #   .custom-checkbox-group input[type='checkbox'] {
  #     font-size: 20px; /* Cambia el tamaño de la fuente */
  #     width: 20px; /* Cambia el tamaño del checkbox */
  #     height: 20px;
  #     margin-right: 10px; /* Espacio entre el checkbox y el texto */
  #   }
  # ")),
    # # # Tab items
    shinydashboard::tabItems(
      # 1) Data base selection
      shinydashboard::tabItem(tabName = "tab01_database",
              h1("Import database"),
              shiny::selectInput(inputId = "file_source",
                          label = "File source...",
                          choices = c("Select one..." = "",
                                      "R examples" = "R_example",
                                      "xlsx" = "xlsx")),
              br(),br(),br(),

              shiny::conditionalPanel(condition = 'input.file_source == "xlsx"',
                module01_database_s01_excel_ui(id = "data_excel")
                ),

              shiny::conditionalPanel(condition = 'input.file_source == "R_example"',
                         module01_database_s02_example_ui(id = "data_example")
                         )
        ),

      # 2) ANOVA
      shinydashboard::tabItem(tabName = "tab02_anova",
              module02_anova_s01_varselection_ui(id = "anova01"),
              br(), br(), br(),
              module02_anova_s02_rscience_ui(id = "anova02")
      ),
      shinydashboard::tabItem(tabName = "tab03_anova",
                              module_cpiA002_s01_varselection_ui(id = "anova03_A"),
                              br(), br(), br(),
                              module_cpiA002_s02_rscience_ui(id = "anova03_B")
      ),
      shinydashboard::tabItem(tabName = "tab04_anova",
                              module_cpiA003_s01_varselection_ui(id = "cpiA003_A"),
                              br(), br(), br(),
                              module_cpiA003_s02_rscience_ui(id = "cpiA003_B")
      ),
      shinydashboard::tabItem(tabName = "tab06_anova",
                              module_cpiA006_s01_varselection_ui(id = "cpiA006_A"),
                              br(), br(), br(),
                              module_cpiA006_s02_rscience_ui(id = "cpiA006_B")
      ),
      shinydashboard::tabItem(tabName = "tab07_anova",
                              module_cpiA007_s01_varselection_ui(id = "cpiA007_A"),
                              br(), br(), br(),
                              module_cpiA007_s02_rscience_ui(id = "cpiA007_B")
      ),
      shinydashboard::tabItem(tabName = "tab20_anova",
                              module_cpiA020_s01_varselection_ui(id = "cpiA020_A"),
                              br(), br(), br(),
                              module_cpiA020_s02_rscience_ui(id = "cpiA020_B")
      ),
      shinydashboard::tabItem(tabName = "tab21_anova",
                              module_cpiA021_s01_varselection_ui(id = "cpiA021_A"),
                              br(), br(), br(),
                              module_cpiA021_s02_rscience_ui(id = "cpiA021_B")
      ),
      # shinydashboard::tabItem(tabName = "tab09_anova",
      #                         module_cpiA009_s01_varselection_ui(id = "cpiA009_A"),
      #                         br(), br(), br(),
      #                         module_cpiA009_s02_rscience_ui(id = "cpiA009_B")
      # ),
      # shinydashboard::tabItem(tabName = "tab10_anova",
      #                         module_cpiA010_s01_varselection_ui(id = "cpiA010_A"),
      #                         br(), br(), br(),
      #                         module_cpiA010_s02_rscience_ui(id = "cpiA010_B")
      #),
      shinydashboard::tabItem(tabName = "tab11_anova",
                              module_cpiA011_s01_varselection_ui(id = "cpiA011_A"),
                              br(), br(), br(),
                              module_cpiA011_s02_rscience_ui(id = "cpiA011_B"),

      ),
      shinydashboard::tabItem(tabName = "tab14_anova",
                              module_cpiA014_s01_varselection_ui(id = "cpiA014_A"),
                              br(), br(), br(),
                              module_cpiA014_s02_rscience_ui(id = "cpiA014_B"),

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

  #input_general <- reactiveVal(NULL)


  input_general <- reactive({

    if (input$file_source == "xlsx")  input_general_01_xlsx() else
      if (input$file_source == "R_example") input_general_02_example() else NULL

  })


##################################################################################
  input_01_anova <- module02_anova_s01_varselection_server(id = "anova01",
                                          input_general = input_general)



  module02_anova_s02_rscience_server(id = "anova02",
                                     input_general = input_general,
                                     input_01_anova = input_01_anova)
##################################################################################



input_03_anova <- module_cpiA002_s01_varselection_server(id = "anova03_A",
                                          input_general = input_general)



  module_cpiA002_s02_rscience_server(id = "anova03_B",
                                     input_general = input_general,
                                     input_01_anova = input_03_anova)
##################################################################################



  input_cpiA003 <- module_cpiA003_s01_varselection_server(id = "cpiA003_A",
                                                  input_general = input_general)



  module_cpiA003_s02_rscience_server(id = "cpiA003_B",
                                     input_general = input_general,
                                     input_01_anova = input_cpiA003)
  ##################################################################################


  input_cpiA006 <- module_cpiA006_s01_varselection_server(id = "cpiA006_A",
                                                          input_general = input_general)



  module_cpiA006_s02_rscience_server(id = "cpiA006_B",
                                     input_general = input_general,
                                     input_01_anova = input_cpiA006)
  ##################################################################################
  input_cpiA007 <- module_cpiA007_s01_varselection_server(id = "cpiA007_A",
                                                          input_general = input_general)



  module_cpiA007_s02_rscience_server(id = "cpiA007_B",
                                     input_general = input_general,
                                     input_01_anova = input_cpiA007)
  ##################################################################################

  input_cpiA020 <- module_cpiA020_s01_varselection_server(id = "cpiA020_A",
                                                          input_general = input_general)



  module_cpiA020_s02_rscience_server(id = "cpiA020_B",
                                     input_general = input_general,
                                     input_01_anova = input_cpiA020)
  ##################################################################################

  input_cpiA021 <- module_cpiA021_s01_varselection_server(id = "cpiA021_A",
                                                          input_general = input_general)



  module_cpiA021_s02_rscience_server(id = "cpiA021_B",
                                     input_general = input_general,
                                     input_01_anova = input_cpiA021)
  ##################################################################################


  input_cpiA009 <- module_cpiA009_s01_varselection_server(id = "cpiA009_A",
                                                          input_general = input_general)



  module_cpiA009_s02_rscience_server(id = "cpiA009_B",
                                     input_general = input_general,
                                     input_01_anova = input_cpiA009)
  ##################################################################################


  input_cpiA010 <- module_cpiA010_s01_varselection_server(id = "cpiA010_A",
                                                          input_general = input_general)



  module_cpiA010_s02_rscience_server(id = "cpiA010_B",
                                     input_general = input_general,
                                     input_01_anova = input_cpiA010)
  ##################################################################################
  input_cpiA011 <- module_cpiA011_s01_varselection_server(id = "cpiA011_A",
                                                          input_general = input_general)

  #observe(print(input_cpiA011()))
  module_cpiA011_s02_rscience_server(id = "cpiA011_B",
                                     input_general = input_general,
                                     input_01_anova = input_cpiA011)


  ##################################################################################
  ##################################################################################
  input_cpiA014 <- module_cpiA014_s01_varselection_server(id = "cpiA014_A",
                                                          input_general = input_general)

  #observe(print(input_cpiA011()))
  module_cpiA014_s02_rscience_server(id = "cpiA014_B",
                                     input_general = input_general,
                                     input_01_anova = input_cpiA014)
  # output$"mix" <- renderUI({
  #   div(
  #   module_cpiA011_s01_varselection_ui(id = "cpiA011_A"),
  #   br(), br(), br(),
  #   module_cpiA011_s02_rscience_ui(id = "cpiA011_B")
  #   )
  # })
  # module_ancova_rscience_server(id = space_ancova)
  #
  # module_dummy_rscience_server(id = space_dummy)
  #
  # module_slr_rscience_server(id = space_slr)

}





shinyApp(ui = ui, server = server, options = list(launch.browser = TRUE))

}



