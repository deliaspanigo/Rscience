
# library("Rscience")
# rm(list = ls())
# source("global.R")


app_04_Rscience <- function(){

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
        " Multivariate- 0.0.3",br(),br(),
        shinydashboard::menuItem(text = "database", tabName = "tab00_database", icon = shiny::icon("th")),
        br(), br(),
        shinydashboard::menuItem(text = "01 - Cluster - Binomial Vars", tabName = "tab01_anova", icon = shiny::icon("th")),
        shinydashboard::menuItem(text = "02 - Cluster - Cuanti Vars", tabName = "tab02_anova", icon = shiny::icon("th")),
        shinydashboard::menuItem(text = "03 - PCA", tabName = "tab03_anova", icon = shiny::icon("th")),
        shinydashboard::menuItem(text = "04 - Correspondence Analysis", tabName = "tab04_anova", icon = shiny::icon("th"))


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
        shiny::HTML('.treeview-menu > li > a { font-size: 28px; }')
      ),
      tags$style(HTML('
                                /* logo - Donde va R-science*/
                                .skin-blue .main-header .logo {
                                font-size: 32px
                                }'
      )
      ),
      # # # Super h2()
      tags$style(
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
        shinydashboard::tabItem(tabName = "tab00_database",
                                h1("Import database"),
                                fluidRow(
                                  column(3,
                                         shiny::selectInput(inputId = "file_source",
                                                            label = "File source...",
                                                            choices = c("Select one..." = "",
                                                                        "01 - R examples" = "R_example",
                                                                        "02 - xlsx" = "xlsx",
                                                                        "03 - UNC - Bio01" = "UNC_bio01"))
                                  )
                                  ),
                                fluidRow(
                                  column(12,
                                         shiny::conditionalPanel(condition = 'input.file_source == "xlsx"',
                                                                 module_cpiD000_database_s01_excel_ui(id = "data_excel")
                                         ),

                                         shiny::conditionalPanel(condition = 'input.file_source == "R_example"',
                                                                 module_cpiD000_database_s02_example_ui(id = "data_example")
                                         ),
                                         shiny::conditionalPanel(condition = 'input.file_source == "UNC_bio01"',
                                                                 module_cpiD000_database_s03_UNC_bio01_ui(id = "data_UNC_bio01")
                                         )
                                  )
                                ),


                                shiny::conditionalPanel(condition = 'input.file_source == "xlsx"',
                                                        module_cpiD000_database_s01_excel_ui2(id = "data_excel")
                                ),

                                shiny::conditionalPanel(condition = 'input.file_source == "R_example"',
                                                        module_cpiD000_database_s02_example_ui2(id = "data_example")
                                ),
                                shiny::conditionalPanel(condition = 'input.file_source == "UNC_bio01"',
                                                        module_cpiD000_database_s03_UNC_bio01_ui2(id = "data_UNC_bio01")
                                )
        ),

        # 2) ANOVA
        shinydashboard::tabItem(tabName = "tab01_anova",
                                h1("01 -Cluster - Binomial Vars"),
                                fluidRow(
                                  column(12,
                                         box(
                                           title = "Database info",
                                           status = "primary",
                                           id = "my_box01A",
                                           solidHeader = TRUE,
                                           collapsible = TRUE,
                                           collapsed = TRUE,
                                           #closable = TRUE,# Colapsado por defecto
                                           width = 12,
                                           module_cpiD001_s02B_varselection_ui(id = "anova01_B")
                                           #tableOutput("intro_source_database")
                                         )
                                  )
                                ),
                                # https://cran.r-project.org/web/packages/shinydashboardPlus/vignettes/improved-boxes.html
                                box(
                                  title = "Var selection",
                                  status = "primary",
                                  id = "my_box01B",
                                  solidHeader = TRUE,
                                  collapsible = TRUE,
                                  closable = FALSE,# Colapsado por defecto
                                  collapsed = FALSE,
                                  width = 12,
                                  module_cpiD001_s01_varselection_ui(id = "anova01_A"),
                                  #actionButton("toggle_box", "Toggle Box"),
                                  br(), br(), br()),
                                br(), br(),br(), br(),br(), br(),

                                module_cpiD001_s02_rscience_ui(id = "anova01_B")
        ),
        shinydashboard::tabItem(tabName = "tab02_anova",
                                h1("02 -Cluster - Cuanti Vars"),
                                fluidRow(
                                  column(12,
                                         box(
                                           title = "Database info",
                                           status = "primary",
                                           id = "my_box02A",
                                           solidHeader = TRUE,
                                           collapsible = TRUE,
                                           collapsed = TRUE,
                                           #closable = TRUE,# Colapsado por defecto
                                           width = 12,
                                           module_cpiD002_s02B_varselection_ui(id = "anova02_B")
                                           #tableOutput("intro_source_database")
                                         )
                                  )
                                ),
                                # https://cran.r-project.org/web/packages/shinydashboardPlus/vignettes/improved-boxes.html
                                box(
                                  title = "Var selection",
                                  status = "primary",
                                  id = "my_box02B",
                                  solidHeader = TRUE,
                                  collapsible = TRUE,
                                  closable = FALSE,# Colapsado por defecto
                                  collapsed = FALSE,
                                  width = 12,
                                  module_cpiD002_s01_varselection_ui(id = "anova02_A"),
                                  #actionButton("toggle_box", "Toggle Box"),
                                  br(), br(), br()),
                                br(), br(),br(), br(),br(), br(),

                                module_cpiD002_s02_rscience_ui(id = "anova02_B")
        ),
        shinydashboard::tabItem(tabName = "tab03_anova",
                                h1("03 -PCA"),
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
                                           module_cpiD003_s02B_varselection_ui(id = "anova03_B")
                                           #tableOutput("intro_source_database")
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
                                  module_cpiD003_s01_varselection_ui(id = "anova03_A"),
                                  #actionButton("toggle_box", "Toggle Box"),
                                  br(), br(), br()),
                                br(), br(),br(), br(),br(), br(),

                                module_cpiD003_s02_rscience_ui(id = "anova03_B")
        ),
        shinydashboard::tabItem(tabName = "tab04_anova",
                                h1("04 - Correspondence Analysis"),
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
                                           module_cpiD004_s02B_varselection_ui(id = "anova04_B")
                                           #tableOutput("intro_source_database")
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
                                  module_cpiD004_s01_varselection_ui(id = "anova04_A"),
                                  #actionButton("toggle_box", "Toggle Box"),
                                  br(), br(), br()),
                                br(), br(),br(), br(),br(), br(),

                                module_cpiD004_s02_rscience_ui(id = "anova04_B")
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


    input_general_01_xlsx <- module_cpiD000_database_s01_excel_server(id = "data_excel",
                                                                      input_file_source = input$file_source)

    input_general_02_example <- module_cpiD000_database_s02_example_server(id = "data_example",
                                                                           input_file_source = input$file_source)

    input_general_03_UNC_bio01 <- module_cpiD000_database_s03_UNC_bio01_server(id = "data_UNC_bio01",
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
    ##################################################################################
    input_01_anova <- module_cpiD001_s01_varselection_server(id = "anova01_A",
                                                             input_general = input_general)



    module_cpiD001_s02_rscience_server(id = "anova01_B",
                                       input_general = input_general,
                                       input_01_anova = input_01_anova)


    ##################################################################################
    input_02_anova <- module_cpiD002_s01_varselection_server(id = "anova02_A",
                                                             input_general = input_general)



    module_cpiD002_s02_rscience_server(id = "anova02_B",
                                       input_general = input_general,
                                       input_01_anova = input_02_anova)

    ##################################################################################
    input_03_anova <- module_cpiD003_s01_varselection_server(id = "anova03_A",
                                                             input_general = input_general)



    module_cpiD003_s02_rscience_server(id = "anova03_B",
                                       input_general = input_general,
                                       input_01_anova = input_03_anova)
    ##################################################################################
    input_04_anova <- module_cpiD004_s01_varselection_server(id = "anova04_A",
                                                             input_general = input_general)



    module_cpiD004_s02_rscience_server(id = "anova04_B",
                                       input_general = input_general,
                                       input_01_anova = input_04_anova)
  }





  shinyApp(ui = ui, server = server, options = list(launch.browser = TRUE))

}



