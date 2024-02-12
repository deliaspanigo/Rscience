
library("Rscience")
#rm(list = ls())
#source("global.R")


app_01_anova_v02 <- function(){
ui <- dashboardPage(

  # # # Dashboard title
  dashboardHeader(title = "R-Science"),

  # # # Sidebar content
  dashboardSidebar(
    width = "300px",
    sidebarMenu(
      menuItem(text = "database", tabName = "tab01_database", icon = icon("th")),
      menuItem(text = "Anova 1 way", tabName = "tab02_anova", icon = icon("th"))
    )
  ),

  # # # Body content
  dashboardBody(

    # # # Selected tab on sidebar
    tags$style(
      HTML('/* active selected tab in the sidebarmenu */  /*La elegida del menu lateral*/
            .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
            background-color: green;
            }'),
      HTML('.sidebar-menu { font-size: 28px; }'),
      HTML('.treeview-menu > li > a { font-size: 28px; }')
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

    # # # Super h2()
    tags$style(
      #HTML('.custom-h2 { font-weight: bold; }') # Estilos CSS para aplicar negrita solo a la clase custom-h2
      #HTML('.custom-h2 { font-weight: bold; }') # Estilos CSS para aplicar negrita solo a la clase custom-h2
    HTML(" h2 { font-weight: bold; }")
    ),


    # # # SelectInput settings...
    tags$style(HTML('.selectize-input { font-size: 32px; line-height: 32px;}')), # Estilos CSS para aumentar el tamaño de letra
    tags$style(HTML('.selectize-dropdown { font-size: 28px; line-height: 28px; }')), # Estilos CSS para aumentar el tamaño de letra
    tags$style(HTML('.control-label { font-size: 28px; }')), # Estilos CSS para cambiar el tamaño de letra del label

    # # # fileInput settings...
    tags$style(HTML('.btn-file { font-size: 28px; }')), # Estilos CSS para cambiar el tamaño de letra del botón adjunto


    # # # TabPanel settings...
    tags$style(
      HTML(".tabbable > .nav > li > a    {background-color: orange;  color:black; font-size: 30px;}
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

    tags$style(HTML("
    .shiny-output-error-validation {
      font-size: 28px; /* Cambia el tamaño de letra de los mensajes de validación */
    }
  ")),

    # # # Tab items
    tabItems(
      # 1) Data base selection
      tabItem(tabName = "tab01_database",
              h1("Import database"),
              selectInput(inputId = "file_source",
                          label = "File source...",
                          choices = c("Select one..." = "",
                                      "R examples" = "R_example",
                                      "xlsx" = "xlsx")),
              br(),br(),br(),

              conditionalPanel(condition = 'input.file_source == "xlsx"',
                module01_database_s01_excel_ui(id = "data_excel")
                ),

        conditionalPanel(condition = 'input.file_source == "R_example"',
                         module01_database_s02_example_ui(id = "data_example")
                         )
        ),

      # 2) ANOVA
      tabItem(tabName = "tab02_anova",
              module02_anova_s01_varselection_ui(id = "anova01"),
              br(), br(), br(),
              module02_anova_s02_rscience_ui(id = "anova02")
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



  input_01_anova <- module02_anova_s01_varselection_server(id = "anova01",
                                          input_general = input_general)



  module02_anova_s02_rscience_server(id = "anova02",
                                     input_general = input_general,
                                     input_01_anova = input_01_anova)





  # module_ancova_rscience_server(id = space_ancova)
  #
  # module_dummy_rscience_server(id = space_dummy)
  #
  # module_slr_rscience_server(id = space_slr)

}





shinyApp(ui, server)

}



