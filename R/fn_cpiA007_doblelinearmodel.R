

# # # # Special Functions
# Take the original code from a function
fn_cpiA007_TakeCode <- function(selected_fn){


  test_code <- capture.output(selected_fn)

  # Fist "{" - Its the function beggining
  pos_first_key <- grep("\\{", test_code)[1]

  # Last "{" - Its the function end
  pos_last_key <- tail(grep("\\}", test_code), 1)

  # Seleccion
  vector_output_code <- test_code[(pos_first_key + 1):(pos_last_key - 1)]

  # Eliminamos los return y los "hide" que hemos colocado.
  vector_output_code <- grep("return\\(", vector_output_code, value = TRUE, invert = TRUE)
  vector_output_code <- grep("hide_", vector_output_code, value = TRUE, invert = TRUE)
  vector_output_code <- grep("# hide_", vector_output_code, value = TRUE, invert = TRUE)

  text_output_code <- paste(vector_output_code, collapse = "\n")
  # test_code <- test_code[-1]
  # test_code <- test_code[-length(test_code)]
  # test_code <- grep("bytecode:", test_code, value = TRUE, invert = TRUE)
  # test_code <- grep("function", test_code, value = TRUE, invert = TRUE)
  # test_code <- test_code[-length(test_code)]
  # test_code <- grep("hide_", test_code, value = TRUE, invert = TRUE)
  # test_code <- grep("# hide", test_code, value = TRUE, invert = TRUE)
  text_output_code

}

# List the cronologic order for objects in a function
fn_cpiA007_ObjNamesInOrder <- function(selected_fn){

  selected_code <- deparse(body(selected_fn))
  selected_code <- grep("<-", selected_code, value = TRUE)
  selected_code <- trimws(selected_code)
  selected_code <- gsub("\\s", "", selected_code)
  selected_code <- sub("<-.*", "", selected_code)
  selected_code <- grep("^[a-zA-Z0-9._]*$", selected_code, value = TRUE)
  selected_code <- grep("^hide", selected_code, value = TRUE, invert = TRUE)

  # # # # # #
  selected_code <- grep("^detail_name", selected_code, value = TRUE, invert = TRUE)
  selected_code <- grep("^selected_role", selected_code, value = TRUE, invert = TRUE)

  # # # # # #
  selected_code <- unique(selected_code)

  return(selected_code)

}


# # # # Control funcitons
# Control previous
fn_cpiA007_control_previous <- function(database, vr_var_name, x01_var_name, x02_var_name,alpha_value){

  dt_ok <- FALSE

  # # # # # # # Database
  # # 1) Database can not be NULL
  # if(is.null(database)){
  #   text_output <- "Control pre test 001: Object 'database' can not be a NULL."
  #   return(Hmisc::llist(dt_ok, text_output))
  # }
  #
  # # 2) Database must be a dataframe
  # if(!is.data.frame(database)){
  #   text_output <- "Control pre test 002: Object 'database' must be a dataframe."
  #   return(Hmisc::llist(dt_ok, text_output))
  # }
  #
  # # 3) Database must has at least 2 columns
  # if(!(ncol(database) >= 2)){
  #   text_output <- "Control pre test 003: Object 'database' must has al least 2 columns."
  #   return(Hmisc::llist(dt_ok, text_output))
  # }
  #
  #
  # # 4) Database must has at least 2 rows
  # if(!(nrow(database) >= 2)){
  #   text_output <- "Control pre test 004: Object 'database' must has al least 2 rows."
  #   return(Hmisc::llist(dt_ok, text_output))
  # }
  #
  #
  #
  #
  # # # # # # # # # vr_var_name
  # # 5) vr_var_name is not NULL
  # if(is.null(vr_var_name)){
  #   text_output <- "Control pre test 005: Object 'vr_var_name' can not be NULL."
  #   return(Hmisc::llist(dt_ok, text_output))
  # }
  #
  # # 6) vr_var_name is a vector
  # if(!is.vector(vr_var_name)){
  #   text_output <- "Control pre test 006: Object 'vr_var_name' must be vector."
  #   return(Hmisc::llist(dt_ok, text_output))
  # }
  #
  # # 7) vr_var_name is a vector
  # if(!(length(vr_var_name) == 1)){
  #   text_output <- "Control pre test 007: Object 'vr_var_name' must be vector of length 1."
  #   return(Hmisc::llist(dt_ok, text_output))
  # }
  #
  # # 8) vr_var_name is not NA
  # if(is.na(vr_var_name)){
  #   text_output <- "Control pre test 008: Object 'vr_var_name' can not be NA."
  #   return(Hmisc::llist(dt_ok, text_output))
  # }
  #
  # # 9) vr_var_name is character
  # if(!is.character(vr_var_name)){
  #   text_output <- "Control pre test 009: Object 'vr_var_name' must be character."
  #   return(Hmisc::llist(dt_ok, text_output))
  # }
  #
  # # 15) x_var_name is a colname from database
  # if(!(vr_var_name %in% colnames(database))){
  #   text_output <- "Control pre test 010: Object 'vr_var_name' must be a colname from database."
  #   return(Hmisc::llist(dt_ok, text_output))
  # }
  #
  #
  # # # # # # # # # # x_var_name
  # # # 10) x_var_name is not NULL
  # # if(is.null(x_var_name)){
  # #   text_output <- "Control pre test 011: Object 'x_var_name' can not be NULL."
  # #   return(Hmisc::llist(dt_ok, text_output))
  # # }
  # #
  # # # 11) x_var_name is a vector
  # # if(!is.vector(x_var_name)){
  # #   text_output <- "Control pre test 012: Object 'x_var_name' must be vector."
  # #   return(Hmisc::llist(dt_ok, text_output))
  # # }
  # #
  # # # 12) x_var_name is a vector
  # # if(!(length(x_var_name) == 1)){
  # #   text_output <- "Control pre test 013: Object 'x_var_name' must be vector of length 1."
  # #   return(Hmisc::llist(dt_ok, text_output))
  # # }
  # #
  # # # 13) x_var_name is not NA
  # # if(is.na(x_var_name)){
  # #   text_output <- "Control pre test 014: Object 'x_var_name' can not be NA."
  # #   return(Hmisc::llist(dt_ok, text_output))
  # # }
  # #
  # # # 14) x_var_name is character
  # # if(!is.character(x_var_name)){
  # #   text_output <- "Control pre test 015: Object 'x_var_name' must be character."
  # #   return(Hmisc::llist(dt_ok, text_output))
  # # }
  # #
  # #
  # # # 15) x_var_name is a colname from database
  # # if(!(x_var_name %in% colnames(database))){
  # #   text_output <- "Control pre test 016: Object 'x_var_name' must be a colname from database."
  # #   return(Hmisc::llist(dt_ok, text_output))
  # # }
  #
  #
  #
  # # # # # # # # # alpha_value
  # # 16) alpha_value is not NULL
  # if(is.null(alpha_value)){
  #   text_output <- "Control pre test 017: Object 'alpha_value' can not be NULL."
  #   return(Hmisc::llist(dt_ok, text_output))
  # }
  #
  # # 17) alpha_value is a vector
  # if(!is.vector(alpha_value)){
  #   text_output <- "Control pre test 018: Object 'alpha_value' must be vector."
  #   return(Hmisc::llist(dt_ok, text_output))
  # }
  #
  # # 18) alpha_value is a vector
  # if(!(length(alpha_value) == 1)){
  #   text_output <- "Control pre test 019: Object 'alpha_value' must be vector of length 1."
  #   return(Hmisc::llist(dt_ok, text_output))
  # }
  #
  # # 19) alpha_value is not NA
  # if(is.na(alpha_value)){
  #   text_output <- "Control pre test 020: Object 'alpha_value' can not be NA."
  #   return(Hmisc::llist(dt_ok, text_output))
  # }
  #
  # # 20) alpha_value is numeric
  # if(!is.numeric(alpha_value)){
  #   text_output <- "Control pre test 021: Object 'alpha_value' must be numeric."
  #   return(Hmisc::llist(dt_ok, text_output))
  # }
  #
  #
  # # 20) alpha_value is between 0 and 1
  # if(!(alpha_value >= 0 && alpha_value <= 1)){
  #   text_output <- "Control pre test 022: Object 'alpha_value' must be a number between 0 and 1."
  #   return(Hmisc::llist(dt_ok, text_output))
  # }
  #
  #
  #
  # # # # # # # # # x_var_name and vr_var_name
  # # 15) x_var_name is not NULL
  # if(vr_var_name == x_var_name){
  #   text_output <- "Control pre test 023: Objects 'vr_var_name' and 'x_var_name' can not be equal."
  #   return(Hmisc::llist(dt_ok, text_output))
  # }
  #
  #
  #
  # # New object
  # vector_var_names <- c(vr_var_name, x_var_name)
  #
  # if(sum(vector_var_names %in% colnames(database)) != 2){
  #   text_output <- "Control pre test 024: Objects 'vr_var_name', and 'x_var_name' must be colnames from database."
  #   return(Hmisc::llist(dt_ok, text_output))
  # }
  #
  #
  #
  # # # # # # # # # # # # minibase
  # minibase <- na.omit(database[vector_var_names])
  # colnames(minibase) <- c("VR", "X")
  #
  #
  #
  # # # # # # # minibase
  # # 1) minibase can not be NULL
  # if(is.null(minibase)){
  #   text_output <- "Control pre test 025: Object 'minibase' can not be a NULL."
  #   return(Hmisc::llist(dt_ok, text_output))
  # }
  #
  # # 2) minibase must be a dataframe
  # if(!is.data.frame(minibase)){
  #   text_output <- "Control pre test 026: Object 'minibase' must be a dataframe."
  #   return(Hmisc::llist(dt_ok, text_output))
  # }
  #
  # # 3) minibase must has at exactly 2 columns
  # if(!(ncol(minibase) == 2)){
  #   text_output <- "Control pre test 027: Object 'minibase' must has exactly 3 columns."
  #   return(Hmisc::llist(dt_ok, text_output))
  # }
  #
  #
  # # 4) minibase must has at least 2 rows
  # if(!(nrow(minibase) >= 2)){
  #   text_output <- "Control pre test 028: Object 'database' must has al least 2 rows."
  #   return(Hmisc::llist(dt_ok, text_output))
  # }
  #
  #
  # # 4) minibase$VR can not be constant
  # if(var(minibase$VR) == 0){
  #   text_output <- "Control pre test 029: Object 'minibase$VR' can not be constant."
  #   return(Hmisc::llist(dt_ok, text_output))
  # }
  #
  # # 4) minibase$VR can not be constant
  # if(length(unique(as.character(minibase$VR))) == 1){
  #   text_output <- "Control pre test 030: Object 'minibase$VR' can not be constant."
  #   return(Hmisc::llist(dt_ok, text_output))
  # }
  #
  #
  # # 4) minibase$VR can not be constant
  # if(length(unique(as.character(minibase$VR))) == 1){
  #   text_output <- "Control pre test 031: Object 'minibase$VR' can not be constant."
  #   return(Hmisc::llist(dt_ok, text_output))
  # }



  # Final!
  dt_ok <- TRUE
  text_output <- ""
  return(Hmisc::llist(dt_ok, text_output))



}



fn_cpiA007_code_p01_test <- function(database, vr_var_name, x01_var_name, x02_var_name, alpha_value){





  # # # # # Section 04 - Var rols and minibase -----------------------------------
  # # # Selected vars
  vector_all_var_names <- colnames(database)
  vector_name_selected_vars <- c(vr_var_name, x01_var_name, x02_var_name)
  vector_rol_vars <- c("VR", "X01", "X02")




  # # # # # Section 05 - minibase ------------------------------------------------
  # Only selected vars. Only completed rows. Factor columns as factor object in R.
  minibase <- na.omit(database[vector_name_selected_vars])
  colnames(minibase) <- vector_rol_vars



  # # # # # Section 06 - Selected vars info as dataframe
  df_selected_vars <- data.frame(
    "order" = 1:length(vector_name_selected_vars),
    "var_name" = vector_name_selected_vars,
    "var_number" = match(vector_name_selected_vars, vector_all_var_names),
    "var_letter" = openxlsx::int2col(match(vector_name_selected_vars, vector_all_var_names)),
    "var_role" = vector_rol_vars,
    "doble_reference" = paste0(vector_rol_vars, "(", vector_name_selected_vars, ")")
  )
  df_selected_vars





  # # # Anova control
  # 'VR' must be numeric and 'FACTOR must be factor.
  df_control_minibase <- data.frame(
    "order" = 1:nrow(df_selected_vars),
    "var_name" = df_selected_vars$var_name,
    "var_role" = df_selected_vars$var_role,
    "control" = c("is.numeric()", "is.numeric()", "is.numeric()"),
    "verify" = c(is.numeric(minibase[,1]), is.numeric(minibase[,2]), is.numeric(minibase[,3]))
  )
  df_control_minibase



  # # # database and minibase reps
  # Our 'n' is from minibase
  df_show_n <- data.frame(
    "object" = c("database", "minibase"),
    "n_col" = c(ncol(database), ncol(minibase)),
    "n_row" = c(nrow(database), nrow(minibase))
  )
  df_show_n




  # # # TEst de correlacion
  pearson_cor_results <- cor.test(x = minibase$X01, y = minibase$X02,
                                  method = "pearson", exact = F)

  spearman_cor_results <- cor.test(x = minibase$X01, y = minibase$X02,
                                   method = "spearman", exact = F, continuity = F)

  normality_x01_results <- shapiro.test(x = minibase$X01)
  normality_x02_results <- shapiro.test(x = minibase$X02)
  homogeneity_results <- bartlett.test(list(minibase$X01, minibase$X02))


  # Asignar el mensaje usando una expresión condicional
  df_check_cor_test <- data.frame(
    "normality_x01" = NA,
    "normality_x02" = NA,
    "homogeneity" = NA,
    "req_pearson_ok" = NA,
    "selected_cor_test" = NA)

  df_check_cor_test$"normality_x01" <- ifelse(normality_x01_results$"p.value" < alpha_value, FALSE, TRUE)
  df_check_cor_test$"normality_x02" <- ifelse(normality_x02_results$"p.value" < alpha_value, FALSE, TRUE)
  df_check_cor_test$"homogeneity"   <- ifelse(homogeneity_results$"p.value" < alpha_value, FALSE, TRUE)
  df_check_cor_test$"req_pearson_ok"   <- ifelse(sum(df_check_cor_test[1,c(1:3)]) == 3, TRUE, FALSE)
  df_check_cor_test$"selected_cor_test"   <- ifelse(df_check_cor_test$"req_pearson_ok", "Pearson", "Spearman")

  df_check_cor_test


  # # # # # Section 06 - Anova Test ----------------------------------------------
  # # # Anova test
  lm_full <- lm(VR ~ X01 + X02, data = minibase)
  summary_full <- summary(lm_full)
  df_table_reg <- as.data.frame(summary_full $coefficients) # Common anova table


  df_table_det_coef <- data.frame(
    "r.squared" =  summary_full$r.squared,
    "adj.r.squared" = summary_full$adj.r.squared,
    "f.obs" = summary_full$fstatistic[1],
    "df_num" = summary_full$fstatistic[2],
    "df_den" = summary_full$fstatistic[3]
  )

  df_table_det_coef$"p.value" <- pf(q = df_table_det_coef$"f.obs",
                                    df1 = df_table_det_coef$"df_num",
                                    df2 = df_table_det_coef$"df_den")
  rownames(df_table_det_coef) <- rep("", nrow(df_table_det_coef))


  # # # # # Section 07 - minibase_mod --------------------------------------------
  # # # Detect rows on database there are on minibase
  dt_rows_database_ok <- rowSums(!is.na(database[vector_name_selected_vars])) == length(vector_name_selected_vars)



  minibase_mod <- minibase
  minibase_mod$"fitted.values" <- lm_full$fitted.values
  minibase_mod$"residuals" <- lm_full$residuals
  minibase_mod$"studres" <- minibase_mod$"residuals"/sd(minibase_mod$"residuals")
  minibase_mod$"id_database" <- c(1:nrow(database))[dt_rows_database_ok]
  minibase_mod$"id_minibase" <- 1:nrow(minibase)






  # # # # # Section 08 - Requeriments for residuals-------------------------------
  # # # Normality test (Shapiro-Wilk)
  test_residuals_normality <- shapiro.test(minibase_mod$residuals)
  test_residuals_normality




  # # # Sum for residuals
  sum_residuals <- sum(minibase_mod$residuals)
  sum_residuals



  # # # Mean for residuals
  mean_residuals <- mean(minibase_mod$residuals)
  mean_residuals

  detail_role <- c("VR", "X01", "X02", "residuals")
  detail_name <- c(vector_name_selected_vars, "---")


  list_position <- lapply(1:length(detail_role), function(x){

    selected_role <- detail_role[x]

    data.frame(
      "rol_var" = selected_role,
      "var_name" = detail_name[x],
      "n" = length(minibase_mod[,selected_role]),
      "min" = min(minibase_mod[,selected_role]),
      "mean" = mean(minibase_mod[,selected_role]),
      "median" = median(minibase_mod[,selected_role]),
      "max" = max(minibase_mod[,selected_role])
    )
  })
  df_position <- do.call(rbind.data.frame, list_position)
  df_position




  list_dispersion <- lapply(1:length(detail_role), function(x){

    selected_role <- detail_role[x]

    data.frame(
      "rol_var" = detail_role[x],
      "var_name" = detail_name[x],
      "n" = length(minibase_mod[,selected_role]),
      "range" = max(minibase_mod[,selected_role]) - min(minibase_mod[,selected_role]),
      "variance" = var(minibase_mod[,selected_role]),
      "sd" = sd(minibase_mod[,selected_role])
    )
  })
  df_dispersion <- do.call(rbind.data.frame, list_dispersion)
  df_dispersion





  # Tabla plot 001
  df_table_plot001 <- df_position


  # Tabla plot 002
  df_table_plot002 <- df_position



  df_table_plot003 <- df_position


  df_table_plot004 <- df_position


  # --- # hide_: Proccesing objects order
  hide_correct_order <- fn_cpiA007_ObjNamesInOrder(selected_fn = fn_cpiA007_code_p01_test)
  hide_output_list_objects <- mget(hide_correct_order)

  # --- # hide_: return!
  return(hide_output_list_objects)

}



fn_cpiA007_code_p02_plot001 <- function(results_p01_test){



  new_plot <-  with(results_p01_test,{


      # Crear el gráfico interactivo con Plotly

    # Crear el gráfico interactivo con Plotly

    plot001 <- plotly::plot_ly()

    plot001 <- add_trace(p = plot001,
                         x = minibase$X02,
                         y = minibase$X01,
                         z = minibase$VR,
                         type = 'scatter3d',
                         mode = 'markers',
                         name = "data",
                         marker = list(size = 10,
                                       color = 'blue',
                                       opacity = 0.7))


    plot001 <- plotly::layout(p = plot001,
                              scene = list(xaxis = list(title = "X02", zeroline = FALSE),
                                           yaxis = list(title = "X01", zeroline = FALSE),
                                           zaxis = list(title = "VR",  zeroline = FALSE)),
                              title = "Plot 001 - Scatterplot XYZ",
                              font = list(size = 20),
                              margin = list(t = 100))




    # Ajustar el modelo de regresión lineal
    modelo <- lm_full

    # Rango de valores para Sepal.Length y Sepal.Width
    x_range <- seq(min(minibase$X02), max(minibase$X02), length.out = 20)
    y_range <- seq(min(minibase$X01), max(minibase$X01), length.out = 20)

    # Crear una malla de puntos en el espacio bidimensional
    the_mesh <- expand.grid("X02" = x_range, "X01" = y_range)

    # Calcular los valores predichos para Petal.Length en cada punto de la malla
    the_mesh$"VR" <- predict(modelo, newdata = the_mesh)


    # Agregar el plano de la regresión lineal al gráfico con add_surface
    plot001 <- add_surface(plot001,
                           type = "mesh3d",
                           x = x_range,
                           y = y_range,
                           z = matrix(the_mesh$"VR", 20, 20),
                           showscale = FALSE,
                           colorscale = list(c(0,1), c('red', 'red')),
                           opacity = 0.5,
                           name = 'Plano')

    plot001
    })



  return(new_plot)

}



fn_cpiA007_code_p02_plot002 <- function(results_p01_test){



  new_plot <-  with(results_p01_test,{


    # Crear el gráfico interactivo con Plotly

    plot002 <- plotly::plot_ly()

    plot002 <- add_trace(p = plot002,
                         x = minibase_mod$fitted.values,
                         y = minibase_mod$residuals,
                         type = 'scatter',
                         mode = 'markers',
                         name = "data",
                         marker = list(size = 15, color = 'blue'))


    # # Agregar la recta
    # selected_slop <- df_table_reg[2,1]# Pendiente
    # selected_constant <- df_table_reg[1,1]  # Ordenada al origen
    #
    # x_recta <- c(min(minibase$X), max(minibase$X))
    # y_recta <- selected_slop * x_recta + selected_constant
    # plot002 <- add_trace(p = plot002,
    #                      x = x_recta, y = y_recta,
    #                      type = 'scatter',
    #                      mode = 'lines',
    #                      name = 'slop',
    #                      line = list(width = 5, color = 'orange'))


    plot002 <- plotly::layout(p = plot002,
                              xaxis = list(title = "Fitted values"),
                              yaxis = list(title = "Residuals"),
                              title = "Plot 002 - Residuals vs. Fitted values",
                              font = list(size = 20),
                              margin = list(t = 100))



    plot002 <- plotly::layout(p = plot002,
                              xaxis = list(zeroline = FALSE),
                              yaxis = list(zeroline = TRUE))

    # Mostrar el gráfico interactivo
    plot002


  })



  return(new_plot)

}




fn_cpiA007_code_p02_plot003 <- function(results_p01_test){



  new_plot <-  with(results_p01_test,{


    # Crear el gráfico interactivo con Plotly

    # Crear el gráfico interactivo con Plotly

    plot001 <- plotly::plot_ly()

    plot001 <- add_trace(p = plot001,
                         x = minibase$X01,
                         y = minibase$VR,
                         type = 'scatter',
                         mode = 'markers',
                         name = "data",
                         marker = list(size = 15, color = 'blue'))


    # Agregar la recta
    selected_slop <- df_table_reg[2,1]# Pendiente
    selected_constant <- df_table_reg[1,1]  # Ordenada al origen

    x_recta <- c(min(minibase$X01), max(minibase$X01))
    y_recta <- selected_slop * x_recta + selected_constant
    plot001 <- add_trace(p = plot001,
                         x = x_recta, y = y_recta,
                         type = 'scatter',
                         mode = 'lines',
                         name = 'slop',
                         line = list(width = 5, color = 'orange'))


    plot001 <- plotly::layout(p = plot001,
                              xaxis = list(title = "BLOCK"),
                              yaxis = list(title = "VR"),
                              title = "Plot 003 - Interacción Factor-Bloque",
                              font = list(size = 20),
                              margin = list(t = 100))



    plot001 <- plotly::layout(p = plot001,
                              xaxis = list(zeroline = FALSE),
                              yaxis = list(zeroline = FALSE))

    # Mostrar el gráfico interactivo
    plot001


  })



  return(new_plot)

}




fn_cpiA007_code_p02_plot004 <- function(results_p01_test){



  new_plot <-  with(results_p01_test,{


    # Crear el gráfico interactivo con Plotly

    # Crear el gráfico interactivo con Plotly

    plot001 <- plotly::plot_ly()

    plot001 <- add_trace(p = plot001,
                         x = minibase$X01,
                         y = minibase$VR,
                         type = 'scatter',
                         mode = 'markers',
                         name = "data",
                         marker = list(size = 15, color = 'blue'))


    # Agregar la recta
    selected_slop <- df_table_reg[2,1]# Pendiente
    selected_constant <- df_table_reg[1,1]  # Ordenada al origen

    x_recta <- c(min(minibase$X01), max(minibase$X01))
    y_recta <- selected_slop * x_recta + selected_constant
    plot001 <- add_trace(p = plot001,
                         x = x_recta, y = y_recta,
                         type = 'scatter',
                         mode = 'lines',
                         name = 'slop',
                         line = list(width = 5, color = 'orange'))


    plot001 <- plotly::layout(p = plot001,
                              xaxis = list(title = "BLOCK"),
                              yaxis = list(title = "VR"),
                              title = "Plot 004 - Interacción Factor-Bloque",
                              font = list(size = 20),
                              margin = list(t = 100))



    plot001 <- plotly::layout(p = plot001,
                              xaxis = list(zeroline = FALSE),
                              yaxis = list(zeroline = FALSE))

    # Mostrar el gráfico interactivo
    plot001


  })



  return(new_plot)

}


# Control post
fn_cpiA007_control_p01_test <- function(all_results){

  if(is.null(all_results)){
    text_output <- "Control post test 001: Object 'all_results' can not be NULL."
    check_ok <- FALSE
    return(Hmisc::llist(check_ok, text_output))
  }



  obj_name01 <- "df_table_anova"
  spected_col_names <- c("Df", "Sum Sq", "Mean Sq", "F value", "Pr(>F)")


  if(!(obj_name01 %in% names(all_results))){
    text_output <- "Control post test 002: Object 'df_table_anova' doesn't exist in 'all_results'."
    check_ok <- FALSE
    return(Hmisc::llist(check_ok, text_output))
  }


  # # # 1) About the table
  if(is.null(all_results[obj_name01])){
    text_output <- "Control post test 003: Object '_obj_name01_' can not be NULL."
    text_output <- gsub("_obj_name01_", "obj_name01", text_output)
    check_ok <- FALSE
    return(Hmisc::llist(check_ok, text_output))
  }


  selected_obj01 <- all_results[[obj_name01]]


  if(!identical(spected_col_names, colnames(selected_obj01))){
    text_output <- "Control post test 004: Object '_obj_name01_' has unexpected column names."
    text_output <- gsub("_obj_name01_", "obj_name01", text_output)
    check_ok <- FALSE
    return(Hmisc::llist(check_ok, text_output))
  }


  if(!is.data.frame(selected_obj01)){
    text_output <- "Control post test 005: Object '_obj_name01_' must be a data.frame."
    text_output <- gsub("_obj_name01_", "obj_name01", text_output)
    check_ok <- FALSE
    return(Hmisc::llist(check_ok, text_output))
  }


  if(nrow(selected_obj01) != 3){
    text_output <- "Control post test 006: Object '_obj_name01_' must has 3 rows."
    text_output <- gsub("_obj_name01_", "obj_name01", text_output)
    check_ok <- FALSE
    return(Hmisc::llist(check_ok, text_output))
  }


  if(ncol(selected_obj01) != 5){
    text_output <- "Control post test 007: Object '_obj_name01_' must have 5 cols."
    text_output <- gsub("_obj_name01_", "obj_name01", text_output)
    check_ok <- FALSE
    return(Hmisc::llist(check_ok, text_output))
  }





  # All OK!
  check_ok <- TRUE
  text_output <- ""
  return(Hmisc::llist(check_ok, text_output))

}


fn_cpiA007_control_p02_plots <- function(all_results){

  if(is.null(all_results)){
    text_output <- "Control post test 001: Object 'all_results' can not be NULL."
    check_ok <- FALSE
    return(Hmisc::llist(check_ok, text_output))
  }




  # All OK!
  check_ok <- TRUE
  text_output <- ""
  return(Hmisc::llist(check_ok, text_output))

}





fn_cpiA007_gen01 <- function(database,  vr_var_name, x01_var_name, x02_var_name,alpha_value){

  output_list <- list()
  output_list$"R_code" <- list()
  output_list$"R_code"$"p01_test" <- NULL
  output_list$"R_code"$"p02_plots" <- list()
  output_list$"R_code"$"p02_plots"$"plot001" <- NULL
  output_list$"R_code"$"p02_plots"$"plot002" <- NULL
  output_list$"R_code"$"p02_plots"$"plot003" <- NULL
  output_list$"R_code"$"p02_plots"$"plot004" <- NULL

  output_list$"check_control_previous" <- NULL

  output_list$"check_control_post" <- list()
  output_list$"check_control_post"$"p01_test" <- NULL


  output_list$"R_results" <- list()
  output_list$"R_results"$"p01_test" <- NULL
  output_list$"R_results"$"p02_plots" <- list()
  output_list$"R_results"$"p02_plots"$"plot001" <- NULL
  output_list$"R_results"$"p02_plots"$"plot002" <- NULL
  output_list$"R_results"$"p02_plots"$"plot003" <- NULL
  output_list$"R_results"$"p02_plots"$"plot004" <- NULL

  # Step 01 - R_code
  output_list$"R_code"$"p01_test"    <- fn_cpiA007_TakeCode(selected_fn = fn_cpiA007_code_p01_test)
  output_list$"R_code"$"p02_plots"$"plot001"   <- fn_cpiA007_TakeCode(selected_fn = fn_cpiA007_code_p02_plot001)
  output_list$"R_code"$"p02_plots"$"plot002"   <- fn_cpiA007_TakeCode(selected_fn = fn_cpiA007_code_p02_plot002)
  output_list$"R_code"$"p02_plots"$"plot003"   <- fn_cpiA007_TakeCode(selected_fn = fn_cpiA007_code_p02_plot003)
  output_list$"R_code"$"p02_plots"$"plot004"   <- fn_cpiA007_TakeCode(selected_fn = fn_cpiA007_code_p02_plot004)


  # Step 02 - Pre Control ------------------------------------------------------
  output_list$"check_control"$"previous" <- fn_cpiA007_control_previous(database, vr_var_name,
                                                          x01_var_name,
                                                          x02_var_name,
                                                          alpha_value)





  # Step 03 - Results ----------------------------------------------------------
  output_list$"R_results"$"p01_test" <- fn_cpiA007_code_p01_test(database,  vr_var_name, x01_var_name, x02_var_name, alpha_value)


  output_list$"R_results"$"p02_plots"$"plot001" <- fn_cpiA007_code_p02_plot001(results_p01_test = output_list$"R_results"$"p01_test")
  output_list$"R_results"$"p02_plots"$"plot002" <- fn_cpiA007_code_p02_plot002(results_p01_test = output_list$"R_results"$"p01_test")
  output_list$"R_results"$"p02_plots"$"plot003" <- fn_cpiA007_code_p02_plot003(results_p01_test = output_list$"R_results"$"p01_test")
  output_list$"R_results"$"p02_plots"$"plot004" <- fn_cpiA007_code_p02_plot004(results_p01_test = output_list$"R_results"$"p01_test")





  return(output_list)

}


fn_cpiA007_gen02 <- function(database,  vr_var_name, x01_var_name, x02_var_name, alpha_value){


  all_results <- fn_cpiA007_gen01(database,  vr_var_name, x01_var_name, x02_var_name, alpha_value)



  output_list <- list()


  # Out01 - Analysis -------------------------------------------------------------
  selection01 <- c("df_selected_vars",
                   "df_table_reg",
                   "df_table_det_coef",
                   "df_position",
                   "df_dispersion")

  output_list$"out01_analysis" <- all_results$R_results$p01_test[selection01]




  # Out02 - Requeriments ---------------------------------------------------------
  selection02 <- c("df_check_cor_test", "normality_x01_results",
  "normality_x02_results", "homogeneity_results", "pearson_cor_results",
  "spearman_cor_results", "test_residuals_normality")

  output_list$"out02_requeriments" <- all_results$R_results$p01_test[selection02]



  # Out03 - Plots and Tables - Factor --------------------------------------------
  output_list$"out03A_plots_factor" <- all_results$R_results$p02_plots


  selection03B <- c("df_table_plot001", "df_table_plot002",
                    "df_table_plot003", "df_table_plot004")
  output_list$"out03B_tables_factor" <- all_results$R_results$p01_test[selection03B]




  # Out04 - Plots and Tables - Residuals -----------------------------------------
  output_list$"out04A_plots_residuals" <- all_results$R_results$p02_plots


  selection04B <- c("df_table_plot001", "df_table_plot002",
                    "df_table_plot003", "df_table_plot004")
  output_list$"out04B_tables_residuals" <- all_results$R_results$p01_test[selection04B]



  # Out05 - Full Results ---------------------------------------------------------
  output_list$"out05_full_results" <- all_results$R_results$p01_test



  # Out06 - R Code ---------------------------------------------------------------
  output_list$"out06_R_code" <- paste0(unlist(all_results$R_code), collapse = "\n\n\n")


  return(output_list)
}


