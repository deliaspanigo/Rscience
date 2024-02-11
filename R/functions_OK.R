



# Ancova

ancova_general_section01_to_03 <- function(file_source, alpha_value,
                                           selected_path, name_database,
                                           selected_pos_vars, all_colnames){

  if(is.null(file_source)) return(NULL)
  if(file_source == "") return(NULL)
  if(is.null(alpha_value)) return(NULL)
  if(is.null(selected_pos_vars)) return(NULL)
  if(is.null(all_colnames)) return(NULL)

  section01_general_libreries <- '
# # # Section 01 - Libraries --------------------------------------------------------
  library(stats)     # Statistics and graphical functions
  library(openxlsx)  # Import files from xlsx
  library(agricolae) # Tukey test
  library(plotly)    # Advanced graphical functions
  '

  if(file_source == "xlsx"){

    if(is.null(selected_path)) return(NULL)

    list_code_xlsx <- list()
    list_code_xlsx[[1]] <- section01_general_libreries

    list_code_xlsx[[2]] <- '
# # # Section 02 - Operator specifications ------------------------------------------
  # Alpha value
  alpha_value <- _alpha_value_

  # Full file path for Excel
  xlsx_path <- _selected_path_
  xlsx_file_name <- tail(strsplit(xlsx_path, "/")[[1]], n = -1)
  xlsx_file_name
'

    list_code_xlsx[[3]] <- '
# # # Seccion 03 - Inport database from Excel file ----------------------------------
  # Import database
  database <- openxlsx::read.xlsx(xlsxFile =  xlsx_file_name, sheet = 1)
'

    the_code_xlsx <- paste0(unlist(list_code_xlsx), "\n\n\n")


    the_code_xlsx <- gsub(pattern = "_alpha_value_",   replacement = alpha_value,   x = the_code_xlsx)
    the_code_xlsx <- gsub(pattern = "_selected_path_", replacement = selected_path, x = the_code_xlsx)
    return(the_code_xlsx)
  }


  if(file_source == "example"){

    if(is.null(name_database)) return(NULL)

    list_code_example <- list()
    list_code_example[[1]] <- section01_general_libreries

    list_code_example[[2]] <- '
# # # Section 02 - Operator specifications ------------------------------------------
  # Alpha value
  alpha_value <- _alpha_value_
'

    list_code_example[[3]] <- '
# # # Seccion 03 - Inport database from Excel file ----------------------------------
  # Import database
  database <- _name_database_
'

    list_code_example[[4]] <- '
# # # Seccion 04 - Role and var selection -------------------------------------------
  # Import database
    _text_var_vr_
    _text_var_factor_
    _text_var_cov_

  # All selected pos vars on specific order (VR, FACTOR, COV)
    selected_pos_vars <- c(pos_var_vr, pos_var_factor, pos_var_cov)
'
    new_objects <- c("pos_var_vr <- ", "pos_var_factor <- ", "pos_var_cov <- ")
    selected_name_vars <- all_colnames[selected_pos_vars]
    selected_pos_vars <- match(selected_name_vars, all_colnames)
    selected_letter_vars <- openxlsx::int2col(selected_pos_vars)
    selected_role_vars <- c("VR", "FACTOR", "COV")

    step01_1 <- paste0(new_objects, selected_pos_vars)
    n_len1 <- max(nchar(step01_1)) + 2
    step01_2 <- stringr::str_pad(step01_1, width = n_len1, side = "right", pad = " ")


    step02_1 <- paste0("  #'", selected_name_vars, "'")
    n_len2 <- max(nchar(step02_1)) + 2
    step02_2 <- stringr::str_pad(step02_1, width = n_len2, side = "right", pad = " ")

    step03_1 <- paste0(" - Column ", selected_letter_vars)

    all_text <- paste0(step01_2,  step02_2, step03_1)
    names(all_text) <- selected_role_vars

    ###

    the_code_example <- paste0(unlist(list_code_example), "\n\n\n")
    the_code_example <- gsub(pattern = "_alpha_value_",   replacement = alpha_value,   x = the_code_example)
    the_code_example <- gsub(pattern = "_name_database_", replacement = name_database, x = the_code_example)
    the_code_example <- gsub(pattern = "_text_var_vr_", replacement = all_text["VR"], x = the_code_example)
    the_code_example <- gsub(pattern = "_text_var_factor_", replacement = all_text["FACTOR"], x = the_code_example)
    the_code_example <- gsub(pattern = "_text_var_cov_", replacement = all_text["COV"], x = the_code_example)
    the_code_example <- gsub(pattern = "_name_database_", replacement = name_database, x = the_code_example)
    the_code_example <- gsub(pattern = "_name_database_", replacement = name_database, x = the_code_example)
    return(the_code_example)
  }



  #################################################################################

  the_code_02_example <- '
# # # Section 02 - Operator specifications ------------------------------------------
  # Alpha value
  alpha_value <- _alpha_value_



# # # Seccion 03 - Importar la base de datos Excel ----------------------------------
  # Importar base
  database <- _name_database_
'

  if(file_source == "xlsx")
    the_code <- c(the_code_01_libreries, the_code_02_xlsx)
  the_code <- paste0(the_code, collapse = "\n\n\n")

  the_code <- gsub(pattern = "_alpha_value_", replacement = alpha_value, x = the_code)
  the_code <- gsub(pattern = "_selected_path_", replacement = selected_path, x = the_code)

}



ancova_with_full_gen01 <- function(database, selected_pos_vars, alpha_value){


  selected_role_vars <- c("VR", "FACTOR", "COV")
  selected_name_vars <- colnames(database)[selected_pos_vars]
  selected_doble_reference_var <- paste0(selected_role_vars, "(", selected_name_vars, ")")

  df_selected_vars <- data.frame(
    "order" = 1:length(selected_pos_vars),
    "var_name" = colnames(database)[selected_pos_vars],
    "var_number" = selected_pos_vars,
    "var_letter" = openxlsx::int2col(selected_pos_vars),
    "var_role" = selected_role_vars,
    "doble_reference" = selected_doble_reference_var
  )

  # Minibase
  minibase <- na.omit(database[selected_pos_vars])
  colnames(minibase) <- selected_role_vars
  minibase[,2] <- as.factor(minibase[,2])
  colnames(minibase) <- selected_role_vars

  df_control_minibase <- data.frame(
    "order" = 1:length(selected_role_vars),
    "var_name" = df_selected_vars$var_name,
    "var_role" = df_selected_vars$var_role,
    "control" = c("is.numeric()", "is.factor()", "is.numeric()"),
    "verify" = c(is.numeric(minibase[,1]), is.factor(minibase[,2]), is.numeric(minibase[,3]))
  )


  df_show_n <- data.frame(
    "object" = c("database", "minibase"),
    "n_col" = c(ncol(database), ncol(minibase)),
    "n_row" = c(nrow(database), nrow(minibase))
  )

  #
  df_factor <- data.frame(
    "order" = 1:nlevels(minibase[,2]),
    "level" = levels(minibase[,2]),
    "n" = as.vector(table(minibase[,2])),
    "color" = rainbow(nlevels(minibase[,2]))
  )

  dt_unbalanced_reps <- length(unique(df_factor$n)) > 1

  #####################################################################################

  # Medidas de posicion particionadas (VR)
  df_position_vr_levels <- data.frame(
    "order" = 1:nlevels(minibase[,2]),
    "level" = levels(minibase[,2]),
    "min" = tapply(minibase[,1], minibase[,2], min),
    "mean" = tapply(minibase[,1], minibase[,2], mean),
    "median" = tapply(minibase[,1], minibase[,2], median),
    "max" = tapply(minibase[,1], minibase[,2], max),
    "n" = tapply(minibase[,1], minibase[,2], length)
  )



  # Medidas de dispersion particionadas (VR)
  df_dispersion_vr_levels <- data.frame(
    "order" = 1:nlevels(minibase[,2]),
    "level" = levels(minibase[,2]),
    "range" = tapply(minibase[,1], minibase[,2], function(x){max(x) - min(x)}),
    "variance" = tapply(minibase[,1], minibase[,2], var),
    "standard_deviation" = tapply(minibase[,1], minibase[,2], sd),
    "standard_error" = tapply(minibase[,1], minibase[,2], function(x){sd(x)/sqrt(length(x))}),
    "n" = tapply(minibase[,1], minibase[,2], length)
  )


  # Medidas de posicion particionadas (VR)
  df_position_vr_general <- data.frame(
    "min" = min(minibase[,1]),
    "mean" = mean(minibase[,1]),
    "median" = median(minibase[,1]),
    "max" = max(minibase[,1]),
    "n" = length(minibase[,1])
  )



  # Medidas de dispersion particionadas (VR)
  df_dispersion_vr_general <- data.frame(
    "range" = max(minibase[,1]) - min(minibase[,1]),
    "variance" = var(minibase[,1]),
    "standard_deviation" = sd(minibase[,1]),
    "standard_error" = sd(minibase[,1])/(sqrt(length(minibase[,1]))),
    "n" = length(minibase[,1])
  )


  #########################################################################################

  # Medidas de posicion particionadas (COV)
  df_position_cov_levels <- data.frame(
    "order" = 1:nlevels(minibase[,2]),
    "level" = levels(minibase[,2]),
    "min" = tapply(minibase[,3], minibase[,2], min),
    "mean" = tapply(minibase[,3], minibase[,2], mean),
    "median" = tapply(minibase[,3], minibase[,2], median),
    "max" = tapply(minibase[,3], minibase[,2], max),
    "n" = tapply(minibase[,3], minibase[,2], length)
  )



  # Medidas de dispersion particionadas  (COV)
  df_dispersion_cov_levels <- data.frame(
    "order" = 1:nlevels(minibase[,2]),
    "level" = levels(minibase[,2]),
    "range" = tapply(minibase[,3], minibase[,2], function(x){max(x) - min(x)}),
    "variance" = tapply(minibase[,3], minibase[,2], var),
    "standard_deviation" = tapply(minibase[,3], minibase[,2], sd),
    "standard_error" = tapply(minibase[,3], minibase[,2], function(x){sd(x)/sqrt(length(x))}),
    "n" = tapply(minibase[,3], minibase[,2], length)
  )

  # Medidas de posicion particionadas (VR)
  df_position_cov_general <- data.frame(
    "min" = min(minibase[,3]),
    "mean" = mean(minibase[,3]),
    "median" = median(minibase[,3]),
    "max" = max(minibase[,3]),
    "n" = length(minibase[,3])
  )



  # Medidas de dispersion particionadas (VR)
  df_dispersion_cov_general <- data.frame(
    "range" = max(minibase[,3]) - min(minibase[,3]),
    "variance" = var(minibase[,3]),
    "standard_deviation" = sd(minibase[,3]),
    "standard_error" = sd(minibase[,3])/(sqrt(length(minibase[,3]))),
    "n" = length(minibase[,3])
  )



  ################################################################################

  # Analisis
  lm_ancova_with <- lm(VR ~ COV + FACTOR + FACTOR:COV, data = minibase)
  aov_ancova_with <- aov(lm_ancova_with)
  coefficients_ancova_with <- coefficients(aov_ancova_with)
  table_ancova_with <- as.data.frame(summary(aov_ancova_with)[[1]])




  ######################################################################################

  dt_rows_database_ok <- rowSums(is.na(database[selected_pos_vars])) == 0


  minibase_mod <- minibase
  minibase_mod$"fitted.values" <- lm_ancova_with$fitted.values
  minibase_mod$"residuals" <- lm_ancova_with$residuals
  minibase_mod$"id_database" <- c(1:nrow(database))[dt_rows_database_ok]
  minibase_mod$"id_minibase" <- 1:nrow(minibase)
  minibase_mod$"lvl_order_number" <- as.numeric(minibase[,2])
  minibase_mod$"lvl_color" <- df_factor$color[minibase_mod$"lvl_order_number"]



  ######################################################################################

  # Medidas de posicion particionadas (residuals)
  df_position_residuals_levels <- data.frame(
    "order" = 1:nlevels(minibase_mod[,2]),
    "level" = levels(minibase_mod[,2]),
    "min" = tapply(minibase_mod$residuals, minibase_mod[,2], min),
    "mean" = tapply(minibase_mod$residuals, minibase_mod[,2], mean),
    "median" = tapply(minibase_mod$residuals, minibase_mod[,2], median),
    "max" = tapply(minibase_mod$residuals, minibase_mod[,2], max),
    "n" = tapply(minibase_mod$residuals, minibase_mod[,2], length)
  )



  # Medidas de dispersion particionadas  (residuals)
  df_dispersion_residuals_levels <- data.frame(
    "order" = 1:nlevels(minibase_mod[,2]),
    "level" = levels(minibase_mod[,2]),
    "range" = tapply(minibase_mod$residuals, minibase_mod[,2], function(x){max(x) - min(x)}),
    "variance" = tapply(minibase_mod$residuals, minibase_mod[,2], var),
    "standard_deviation" = tapply(minibase_mod$residuals, minibase_mod[,2], sd),
    "standard_error" = tapply(minibase_mod$residuals, minibase_mod[,2], function(x){sd(x)/sqrt(length(x))}),
    "n" = tapply(minibase_mod$residuals, minibase_mod[,2], length)
  )



  # Medidas de posicion particionadas (residuals)
  df_position_residuals_general <- data.frame(
    "min" = min(minibase_mod$residuals),
    "mean" = mean(minibase_mod$residuals),
    "median" = median(minibase_mod$residuals),
    "max" = max(minibase_mod$residuals),
    "n" = length(minibase_mod$residuals)
  )



  # Medidas de dispersion particionadas (residuals)
  df_dispersion_residuals_general <- data.frame(
    "range" = max(minibase_mod$residuals) - min(minibase_mod$residuals),
    "variance" = var(minibase_mod$residuals),
    "standard_deviation" = sd(minibase_mod$residuals),
    "standard_error" = sd(minibase_mod$residuals)/(sqrt(length(minibase_mod$residuals))),
    "n" = length(minibase_mod$residuals)
  )





  # # # Seccion 09 - Requisitos del modelo de Ancova con interacción ------------------
  # Test de Normalidad de Shapiro-Wilk
  test_normality_residuals <- shapiro.test(minibase_mod$residuals)
  test_normality_residuals


  test_homogeneity_residuals <- bartlett.test(residuals ~ FACTOR, data = minibase_mod)
  test_homogeneity_residuals


  sum_residuos <- sum(minibase_mod$residuals)
  sum_residuos

  # # Sección 09-2) Tabla Tukey --------------------------------------------------
  # Tukey completo
  tukey01_full_groups <- agricolae::HSD.test(y = lm_ancova_with,
                                             trt = colnames(minibase)[2],
                                             alpha = alpha_value,
                                             group = TRUE,
                                             console = FALSE,
                                             unbalanced = dt_unbalanced_reps)

  tukey02_full_pairs <- agricolae::HSD.test(y = lm_ancova_with,
                                            trt = colnames(minibase)[2],
                                            alpha = alpha_value,
                                            group = FALSE,
                                            console = FALSE,
                                            unbalanced = dt_unbalanced_reps)


  ########################################################

  vector_mean_vr_levels <- tukey01_full_groups$means[,1]
  names(vector_mean_vr_levels) <- rownames(tukey01_full_groups$means)

  mean_of_means_vr <- mean(vector_mean_vr_levels)
  vector_mean_of_means_vr <- rep(mean_of_means_vr, length(vector_mean_vr_levels))
  names(vector_mean_of_means_vr) <- names(vector_mean_vr_levels)

  vector_tau_vr_levels <- vector_mean_vr_levels - mean_of_means_vr
  names(vector_tau_vr_levels) <- names(vector_mean_vr_levels)

  df_factorial_effects <- data.frame(
    "order" = 1:length(vector_mean_vr_levels),
    "level" = names(vector_mean_vr_levels),
    "mu" = vector_mean_of_means_vr,
    "tau_i" = vector_tau_vr_levels,
    "mu_i" = vector_mean_vr_levels,
    "groups" = tukey01_full_groups$groups$groups
  )


  df_tukey_means <- data.frame(
    "order" = 1:length(vector_mean_vr_levels),
    "level" = names(vector_mean_vr_levels),
    "mean" = vector_mean_vr_levels,
    "groups" = tukey01_full_groups$groups$groups
  )

  df_tukey_effects <- data.frame(
    "order" = 1:length(vector_mean_vr_levels),
    "level" = names(vector_mean_vr_levels),
    "tau_i" = vector_tau_vr_levels,
    "groups" = tukey01_full_groups$groups$groups
  )



  # Suma de los efectos tau
  sum_tau <- sum(vector_tau_vr_levels)
  sum_tau






  # # Sección 09-3) Tabla con las pendientes y ordenadas de cada factor ----------
  # Ecuaciones de cada recta
  # Ordenadas de los niveles del factor
  pos_mod_intercept <- c((1:(nlevels(minibase[,2])-1)) + 2)
  vector_mod_intercept_levels <- c(0, coefficients_ancova_with[pos_mod_intercept])
  names(vector_mod_intercept_levels) <- levels(minibase[,2])
  vector_intercept_levels <- vector_mod_intercept_levels + coefficients_ancova_with[1]
  names(vector_intercept_levels) <- levels(minibase[,2])


  # Pendientes de los niveles dle factor
  pos_mod_slope <- c((nlevels(minibase[,2]) + 2):length(coefficients_ancova_with))
  vector_mod_slope_levels <- c(0, coefficients_ancova_with[pos_mod_slope])
  names(vector_mod_slope_levels) <- levels(minibase[,2])

  vector_slope_levels <- vector_mod_slope_levels + coefficients_ancova_with[2]
  names(vector_slope_levels) <- levels(minibase[,2])


  df_lines <- data.frame(
    "order" = 1:length(vector_slope_levels),
    "level" = names(vector_slope_levels),
    "slope" = vector_slope_levels,
    "intercept" = vector_intercept_levels
  )




  # # Sección 09-4) Tabla con las estimaciones del modelo ------------------------
  # Estimacion de la interaccion en las combinaciones COV:FACTOR
  slope_general <- mean(vector_slope_levels)
  vector_slope_general <- rep(slope_general, length(vector_slope_levels))
  names(vector_slope_general) <- names(vector_slope_levels)


  # Efectos de interaccion
  vector_interaction <- vector_slope_levels - vector_slope_general
  names(vector_interaction) <- names(vector_interaction)

  df_interaction <- data.frame(
    "order" = 1:length(vector_interaction),
    "level" = names(vector_interaction),
    "beta" = vector_slope_general,
    "gamma_i" = vector_interaction,
    "beta_i" = vector_slope_levels
  )


  # Suma de las interacciones
  sum_interaction <- sum(vector_interaction)
  sum_interaction






  # # Sección 09-6) Tabla con estimaciones del modelo de Ancova (modelo largo) ---

  df_resumen_ancova_with_large <- data.frame(
    "order" = df_factorial_effects$order,
    "level" = df_factorial_effects$level,
    "mu" = df_factorial_effects$mu,
    "tau_i" = df_factorial_effects$tau_i,
    "beta" = df_interaction$beta,
    "gamma_i" = df_interaction$gamma_i,
    "mean_cov_i" = df_position_cov_levels$mean,
    "min_cov_i" = df_position_cov_levels$min,
    "max_cov_i" = df_position_cov_levels$max
  )



  df_resumen_ancova_with_short <- data.frame(
    "order" = df_factorial_effects$order,
    "level" = df_factorial_effects$level,
    "mu_i" = df_factorial_effects$mu_i,
    "beta_i" = df_interaction$beta_i,
    "mean_cov_i" = df_position_cov_levels$mean,
    "min_cov_i" = df_position_cov_levels$min,
    "max_cov_i" = df_position_cov_levels$max
  )


  vector_dif_min_cov_levels <- df_position_cov_levels$min - df_position_cov_levels$mean
  names(vector_dif_min_cov_levels) <- df_position_cov_levels$level

  vector_dif_max_cov_levels <- df_position_cov_levels$max - df_position_cov_levels$mean
  names(vector_dif_max_cov_levels) <- df_position_cov_levels$level


  vector_initial_point_y_levels <- vector_mean_vr_levels + (vector_slope_levels*(vector_dif_min_cov_levels))
  vector_end_point_y_levels     <- vector_mean_vr_levels + (vector_slope_levels*(vector_dif_max_cov_levels))


  df_segments <- data.frame(
    "order" = df_position_cov_levels$order,
    "level" = df_position_cov_levels$level,
    "min_cov_i_x" = df_position_cov_levels$min,
    "max_cov_i_x" = df_position_cov_levels$max,
    "initial_point_i_y" = vector_initial_point_y_levels,
    "end_point_i_x" = vector_end_point_y_levels
  )


  # hide_: all objects in the internal envirorment
  hide_all_objets <- ls()

  # hide_: All in a output list
  hide_output_list_objects <- sapply(hide_all_objets, function(list_name) { get(list_name)  }, simplify = FALSE)                                                    # HIDE

  # hide_: return!
  return(hide_output_list_objects)
}



ancova_with_full_gen02 <- function(database, selected_pos_vars, alpha_value){



  results_gen01 <- ancova_with_full_gen01(database, selected_pos_vars, alpha_value)

  # Los objetos estan ordenados alfabeticamente,y no por el orden de aparicion
  # para el usuario.
  # Vamos a tomar retomar el orden de aparicion y de paso
  # ocultamos algunos objetos:
  # - Los que empiezan con 'hide_'
  # - Los que terminan el "_ok".


  # Obtiene el código fuente de la función
  codigo_fuente <- deparse(body(ancova_with_full_gen01))

  # Busca y selecciona las líneas que contienen "<- data.frame("
  lineas_seleccionadas <- codigo_fuente
  lineas_seleccionadas <- grep("<-", lineas_seleccionadas, value = TRUE)
  lineas_seleccionadas <- grep("\\$.*<-", lineas_seleccionadas, value = TRUE, invert = TRUE)


  #  lineas_seleccionadas <- gsub(" ", "", lineas_seleccionadas)
  lineas_seleccionadas <- trimws(lineas_seleccionadas)

  lineas_seleccionadas <- grep("\\).*<-", lineas_seleccionadas, value = TRUE, invert = TRUE)
  lineas_seleccionadas <- grep("\\].*<-", lineas_seleccionadas, value = TRUE, invert = TRUE)

  #  lineas_seleccionadas <- grep("\\$", lineas_seleccionadas, value = TRUE, invert = TRUE)
  lineas_seleccionadas <- sub(" <-.*", "", lineas_seleccionadas)
  lineas_seleccionadas <- grep("^hide_", lineas_seleccionadas, value = TRUE, invert = TRUE)



  #lineas_seleccionadas <- grep("dt_rows_database_ok", lineas_seleccionadas, value = TRUE, invert = TRUE)
  #lineas_seleccionadas <- grep("minibase_mod", lineas_seleccionadas, value = TRUE, invert = TRUE)
  # lineas_seleccionadas <- grep("_ok$", lineas_seleccionadas, value = TRUE, invert = TRUE)
  # lineas_seleccionadas <- grep("_dif_", lineas_seleccionadas, value = TRUE, invert = TRUE)
  # lineas_seleccionadas <- grep("_point_", lineas_seleccionadas, value = TRUE, invert = TRUE)
  # lineas_seleccionadas <- grep("_intercept_", lineas_seleccionadas, value = TRUE, invert = TRUE)
  # lineas_seleccionadas <- grep("slope_", lineas_seleccionadas, value = TRUE, invert = TRUE)
  # lineas_seleccionadas <- grep("vector_interaction", lineas_seleccionadas, value = TRUE, invert = TRUE)

  results_gen02 <- results_gen01[lineas_seleccionadas]

  return(results_gen02)

}





ancova_without_full_gen01 <- function(database, selected_pos_vars, alpha_value){


  selected_role_vars <- c("VR", "FACTOR", "COV")
  selected_name_vars <- colnames(database)[selected_pos_vars]
  selected_doble_reference_var <- paste0(selected_role_vars, "(", selected_name_vars, ")")

  df_selected_vars <- data.frame(
    "order" = 1:length(selected_pos_vars),
    "var_name" = colnames(database)[selected_pos_vars],
    "var_number" = selected_pos_vars,
    "var_letter" = openxlsx::int2col(selected_pos_vars),
    "var_role" = selected_role_vars,
    "doble_reference" = selected_doble_reference_var
  )

  # Minibase
  minibase <- na.omit(database[selected_pos_vars])
  colnames(minibase) <- selected_role_vars
  minibase[,2] <- as.factor(minibase[,2])
  colnames(minibase) <- selected_role_vars

  df_control_minibase <- data.frame(
    "order" = 1:length(selected_role_vars),
    "var_name" = df_selected_vars$var_name,
    "var_role" = df_selected_vars$var_role,
    "control" = c("is.numeric()", "is.factor()", "is.numeric()"),
    "verify" = c(is.numeric(minibase[,1]), is.factor(minibase[,2]), is.numeric(minibase[,3]))
  )

  df_show_n <- data.frame(
    "object" = c("database", "minibase"),
    "n_col" = c(ncol(database), ncol(minibase)),
    "n_row" = c(nrow(database), nrow(minibase))
  )


  #
  df_factor <- data.frame(
    "order" = 1:nlevels(minibase[,2]),
    "level" = levels(minibase[,2]),
    "n" = as.vector(table(minibase[,2])),
    "color" = rainbow(nlevels(minibase[,2]))
  )

  dt_unbalanced_reps <- length(unique(df_factor$n)) > 1

  #####################################################################################

  # Medidas de posicion particionadas (VR)
  df_position_vr_levels <- data.frame(
    "order" = 1:nlevels(minibase[,2]),
    "level" = levels(minibase[,2]),
    "min" = tapply(minibase[,1], minibase[,2], min),
    "mean" = tapply(minibase[,1], minibase[,2], mean),
    "median" = tapply(minibase[,1], minibase[,2], median),
    "max" = tapply(minibase[,1], minibase[,2], max),
    "n" = tapply(minibase[,1], minibase[,2], length)
  )



  # Medidas de dispersion particionadas (VR)
  df_dispersion_vr_levels <- data.frame(
    "order" = 1:nlevels(minibase[,2]),
    "level" = levels(minibase[,2]),
    "range" = tapply(minibase[,1], minibase[,2], function(x){max(x) - min(x)}),
    "variance" = tapply(minibase[,1], minibase[,2], var),
    "standard_deviation" = tapply(minibase[,1], minibase[,2], sd),
    "standard_error" = tapply(minibase[,1], minibase[,2], function(x){sd(x)/sqrt(length(x))}),
    "n" = tapply(minibase[,1], minibase[,2], length)
  )


  # Medidas de posicion particionadas (VR)
  df_position_vr_general <- data.frame(
    "min" = min(minibase[,1]),
    "mean" = mean(minibase[,1]),
    "median" = median(minibase[,1]),
    "max" = max(minibase[,1]),
    "n" = length(minibase[,1])
  )



  # Medidas de dispersion particionadas (VR)
  df_dispersion_vr_general <- data.frame(
    "range" = max(minibase[,1]) - min(minibase[,1]),
    "variance" = var(minibase[,1]),
    "standard_deviation" = sd(minibase[,1]),
    "standard_error" = sd(minibase[,1])/(sqrt(length(minibase[,1]))),
    "n" = length(minibase[,1])
  )


  #########################################################################################

  # Medidas de posicion particionadas (COV)
  df_position_cov_levels <- data.frame(
    "order" = 1:nlevels(minibase[,2]),
    "level" = levels(minibase[,2]),
    "min" = tapply(minibase[,3], minibase[,2], min),
    "mean" = tapply(minibase[,3], minibase[,2], mean),
    "median" = tapply(minibase[,3], minibase[,2], median),
    "max" = tapply(minibase[,3], minibase[,2], max),
    "n" = tapply(minibase[,3], minibase[,2], length)
  )



  # Medidas de dispersion particionadas  (COV)
  df_dispersion_cov_levels <- data.frame(
    "order" = 1:nlevels(minibase[,2]),
    "level" = levels(minibase[,2]),
    "range" = tapply(minibase[,3], minibase[,2], function(x){max(x) - min(x)}),
    "variance" = tapply(minibase[,3], minibase[,2], var),
    "standard_deviation" = tapply(minibase[,3], minibase[,2], sd),
    "standard_error" = tapply(minibase[,3], minibase[,2], function(x){sd(x)/sqrt(length(x))}),
    "n" = tapply(minibase[,3], minibase[,2], length)
  )

  # Medidas de posicion particionadas (VR)
  df_position_cov_general <- data.frame(
    "min" = min(minibase[,3]),
    "mean" = mean(minibase[,3]),
    "median" = median(minibase[,3]),
    "max" = max(minibase[,3]),
    "n" = length(minibase[,3])
  )



  # Medidas de dispersion particionadas (VR)
  df_dispersion_cov_general <- data.frame(
    "range" = max(minibase[,3]) - min(minibase[,3]),
    "variance" = var(minibase[,3]),
    "standard_deviation" = sd(minibase[,3]),
    "standard_error" = sd(minibase[,3])/(sqrt(length(minibase[,3]))),
    "n" = length(minibase[,3])
  )



  ################################################################################

  # Analisis
  lm_ancova_without <- lm(VR ~ COV + FACTOR, data = minibase)
  aov_ancova_without <- aov(lm_ancova_without)
  coefficients_ancova_without <- coefficients(aov_ancova_without)
  table_ancova_without <- as.data.frame(summary(aov_ancova_without)[[1]])




  ######################################################################################

  dt_rows_database_ok <- rowSums(is.na(database[selected_pos_vars])) == 0


  minibase_mod <- minibase
  minibase_mod$"fitted.values" <- lm_ancova_without$fitted.values
  minibase_mod$"residuals" <- lm_ancova_without$residuals
  minibase_mod$"id_database" <- c(1:nrow(database))[dt_rows_database_ok]
  minibase_mod$"id_minibase" <- 1:nrow(minibase)
  minibase_mod$"lvl_order_number" <- as.numeric(minibase[,2])
  minibase_mod$"lvl_color" <- df_factor$color[minibase_mod$"lvl_order_number"]



  ######################################################################################

  # Medidas de posicion particionadas (residuals)
  df_position_residuals_levels <- data.frame(
    "order" = 1:nlevels(minibase_mod[,2]),
    "level" = levels(minibase_mod[,2]),
    "min" = tapply(minibase_mod$residuals, minibase_mod[,2], min),
    "mean" = tapply(minibase_mod$residuals, minibase_mod[,2], mean),
    "median" = tapply(minibase_mod$residuals, minibase_mod[,2], median),
    "max" = tapply(minibase_mod$residuals, minibase_mod[,2], max),
    "n" = tapply(minibase_mod$residuals, minibase_mod[,2], length)
  )



  # Medidas de dispersion particionadas  (residuals)
  df_dispersion_residuals_levels <- data.frame(
    "order" = 1:nlevels(minibase_mod[,2]),
    "level" = levels(minibase_mod[,2]),
    "range" = tapply(minibase_mod$residuals, minibase_mod[,2], function(x){max(x) - min(x)}),
    "variance" = tapply(minibase_mod$residuals, minibase_mod[,2], var),
    "standard_deviation" = tapply(minibase_mod$residuals, minibase_mod[,2], sd),
    "standard_error" = tapply(minibase_mod$residuals, minibase_mod[,2], function(x){sd(x)/sqrt(length(x))}),
    "n" = tapply(minibase_mod$residuals, minibase_mod[,2], length)
  )



  # Medidas de posicion particionadas (residuals)
  df_position_residuals_general <- data.frame(
    "min" = min(minibase_mod$residuals),
    "mean" = mean(minibase_mod$residuals),
    "median" = median(minibase_mod$residuals),
    "max" = max(minibase_mod$residuals),
    "n" = length(minibase_mod$residuals)
  )



  # Medidas de dispersion particionadas (residuals)
  df_dispersion_residuals_general <- data.frame(
    "range" = max(minibase_mod$residuals) - min(minibase_mod$residuals),
    "variance" = var(minibase_mod$residuals),
    "standard_deviation" = sd(minibase_mod$residuals),
    "standard_error" = sd(minibase_mod$residuals)/(sqrt(length(minibase_mod$residuals))),
    "n" = length(minibase_mod$residuals)
  )





  # # # Seccion 09 - Requisitos del modelo de Ancova con interacción ------------------
  # Test de Normalidad de Shapiro-Wilk
  test_normality_residuals <- shapiro.test(minibase_mod$residuals)
  test_normality_residuals


  test_homogeneity_residuals <- bartlett.test(residuals ~ FACTOR, data = minibase_mod)
  test_homogeneity_residuals


  sum_residuos <- sum(minibase_mod$residuals)
  sum_residuos

  # # Sección 09-2) Tabla Tukey --------------------------------------------------
  # Tukey completo
  tukey01_full_groups <- agricolae::HSD.test(y = lm_ancova_without,
                                             trt = colnames(minibase)[2],
                                             alpha = alpha_value,
                                             group = TRUE,
                                             console = FALSE,
                                             unbalanced = dt_unbalanced_reps)

  tukey02_full_pairs <- agricolae::HSD.test(y = lm_ancova_without,
                                            trt = colnames(minibase)[2],
                                            alpha = alpha_value,
                                            group = FALSE,
                                            console = FALSE,
                                            unbalanced = dt_unbalanced_reps)


  ########################################################

  vector_mean_vr_levels <- tukey01_full_groups$means[,1]
  names(vector_mean_vr_levels) <- rownames(tukey01_full_groups$means)

  mean_of_means_vr <- mean(vector_mean_vr_levels)
  vector_mean_of_means_vr <- rep(mean_of_means_vr, length(vector_mean_vr_levels))
  names(vector_mean_of_means_vr) <- names(vector_mean_vr_levels)

  vector_tau_vr_levels <- vector_mean_vr_levels - mean_of_means_vr
  names(vector_tau_vr_levels) <- names(vector_mean_vr_levels)


  df_factorial_effects <- data.frame(
    "order" = 1:length(vector_mean_vr_levels),
    "level" = names(vector_mean_vr_levels),
    "mu" = vector_mean_of_means_vr,
    "tau_i" = vector_tau_vr_levels,
    "mu_i" = vector_mean_vr_levels,
    "groups" = tukey01_full_groups$groups$groups
  )


  df_tukey_means <- data.frame(
    "order" = 1:length(vector_mean_vr_levels),
    "level" = names(vector_mean_vr_levels),
    "mean" = vector_mean_vr_levels,
    "groups" = tukey01_full_groups$groups$groups
  )

  df_tukey_effects <- data.frame(
    "order" = 1:length(vector_mean_vr_levels),
    "level" = names(vector_mean_vr_levels),
    "tau_i" = vector_tau_vr_levels,
    "groups" = tukey01_full_groups$groups$groups
  )



  # Suma de los efectos tau
  sum_tau <- sum(vector_tau_vr_levels)
  sum_tau







  # # Sección 09-3) Tabla con las pendientes y ordenadas de cada factor ----------
  # Ecuaciones de cada recta
  # Ordenadas de los niveles del factor
  pos_mod_intercept <- c((1:(nlevels(minibase[,2])-1)) + 2)
  vector_mod_intercept_levels <- c(0, coefficients_ancova_without[pos_mod_intercept])
  names(vector_mod_intercept_levels) <- levels(minibase[,2])
  vector_intercept_levels <- vector_mod_intercept_levels + coefficients_ancova_without[1]
  names(vector_intercept_levels) <- levels(minibase[,2])


  # Pendientes de los niveles dle factor
  pos_slope_levels <- rep(2, nlevels(minibase[,2]))
  vector_slope_levels <- coefficients_ancova_without[pos_slope_levels]
  names(vector_slope_levels) <- levels(minibase[,2])


  df_lines <- data.frame(
    "order" = 1:length(vector_slope_levels),
    "level" = names(vector_slope_levels),
    "slope" = vector_slope_levels,
    "intercept" = vector_intercept_levels
  )




  # # Sección 09-4) Tabla con las estimaciones del modelo ------------------------
  # Pendiente general
  slope_general <- vector_slope_levels[1]
  vector_slope_general <- rep(slope_general, length(vector_slope_levels))
  names(vector_slope_general) <- names(vector_slope_levels)


  # # Efectos de interaccion
  # vector_interaction <- rep(0, length(vector_slope_levels))
  # names(vector_interaction) <- names(vector_slope_general)
  #
  # df_interaction <- data.frame(
  #   "order" = 1:length(vector_interaction),
  #   "level" = names(vector_interaction),
  #   "beta" = vector_slope_general,
  #   "gamma_i" = vector_interaction,
  #   "beta_i" = vector_slope_levels
  # )
  #
  #
  # # Suma de las interacciones
  # sum_interaction <- sum(vector_interaction)
  # sum_interaction






  # # Sección 09-6) Tabla con estimaciones del modelo de Ancova (modelo largo) ---


  # # Sección 09-6) Tabla con estimaciones del modelo de Ancova sin interaccion (modelo largo) ---

  df_resumen_ancova_without_large <- data.frame(
    "order" = df_factorial_effects$order,
    "level" = df_factorial_effects$level,
    "mu" = df_factorial_effects$mu,
    "tau_i" = df_factorial_effects$tau_i,
    "beta" = df_lines$slope,
    "mean_cov_i" = df_position_cov_levels$mean,
    "min_cov_i" = df_position_cov_levels$min,
    "max_cov_i" = df_position_cov_levels$max
  )



  df_resumen_ancova_without_short <- data.frame(
    "order" = df_factorial_effects$order,
    "level" = df_factorial_effects$level,
    "mu_i" = df_factorial_effects$mu_i,
    "beta" = df_lines$slope,
    "mean_cov_i" = df_position_cov_levels$mean,
    "min_cov_i" = df_position_cov_levels$min,
    "max_cov_i" = df_position_cov_levels$max
  )


  vector_dif_min_cov_levels <- df_position_cov_levels$min - df_position_cov_levels$mean
  names(vector_dif_min_cov_levels) <- df_position_cov_levels$level

  vector_dif_max_cov_levels <- df_position_cov_levels$max - df_position_cov_levels$mean
  names(vector_dif_max_cov_levels) <- df_position_cov_levels$level


  vector_initial_point_y_levels <- vector_mean_vr_levels + (vector_slope_levels*(vector_dif_min_cov_levels))
  vector_end_point_y_levels     <- vector_mean_vr_levels + (vector_slope_levels*(vector_dif_max_cov_levels))


  df_segments <- data.frame(
    "order" = df_position_cov_levels$order,
    "level" = df_position_cov_levels$level,
    "min_cov_i_x" = df_position_cov_levels$min,
    "max_cov_i_x" = df_position_cov_levels$max,
    "initial_point_i_y" = vector_initial_point_y_levels,
    "end_point_i_x" = vector_end_point_y_levels
  )



  # hide_: all objects in the internal envirorment
  hide_all_objets <- ls()

  # hide_: All in a output list
  hide_output_list_objects <- sapply(hide_all_objets, function(list_name) { get(list_name)  }, simplify = FALSE)                                                    # HIDE

  # hide_: return!
  return(hide_output_list_objects)
}



ancova_without_full_gen02 <- function(database, selected_pos_vars, alpha_value){



  results_gen01 <- ancova_without_full_gen01(database, selected_pos_vars, alpha_value)

  # Los objetos estan ordenados alfabeticamente,y no por el orden de aparicion
  # para el usuario.
  # Vamos a tomar retomar el orden de aparicion y de paso
  # ocultamos algunos objetos:
  # - Los que empiezan con 'hide_'
  # - Los que terminan el "_ok".


  # Obtiene el código fuente de la función
  codigo_fuente <- deparse(body(ancova_without_full_gen01))

  # Busca y selecciona las líneas que contienen "<- data.frame("
  lineas_seleccionadas <- codigo_fuente
  lineas_seleccionadas <- grep("<-", lineas_seleccionadas, value = TRUE)
  lineas_seleccionadas <- grep("\\$.*<-", lineas_seleccionadas, value = TRUE, invert = TRUE)


  #  lineas_seleccionadas <- gsub(" ", "", lineas_seleccionadas)
  lineas_seleccionadas <- trimws(lineas_seleccionadas)

  lineas_seleccionadas <- grep("\\).*<-", lineas_seleccionadas, value = TRUE, invert = TRUE)
  lineas_seleccionadas <- grep("\\].*<-", lineas_seleccionadas, value = TRUE, invert = TRUE)

  #  lineas_seleccionadas <- grep("\\$", lineas_seleccionadas, value = TRUE, invert = TRUE)
  lineas_seleccionadas <- sub(" <-.*", "", lineas_seleccionadas)
  lineas_seleccionadas <- grep("^hide_", lineas_seleccionadas, value = TRUE, invert = TRUE)



  #lineas_seleccionadas <- grep("dt_rows_database_ok", lineas_seleccionadas, value = TRUE, invert = TRUE)
  #lineas_seleccionadas <- grep("minibase_mod", lineas_seleccionadas, value = TRUE, invert = TRUE)
  # lineas_seleccionadas <- grep("_ok$", lineas_seleccionadas, value = TRUE, invert = TRUE)
  # lineas_seleccionadas <- grep("_dif_", lineas_seleccionadas, value = TRUE, invert = TRUE)
  # lineas_seleccionadas <- grep("_point_", lineas_seleccionadas, value = TRUE, invert = TRUE)
  # lineas_seleccionadas <- grep("_intercept_", lineas_seleccionadas, value = TRUE, invert = TRUE)
  # lineas_seleccionadas <- grep("slope_", lineas_seleccionadas, value = TRUE, invert = TRUE)
  # lineas_seleccionadas <- grep("vector_interaction", lineas_seleccionadas, value = TRUE, invert = TRUE)

  results_gen02 <- results_gen01[lineas_seleccionadas]

  return(results_gen02)

}






#############################################################################################################



slr_general_section01_to_03 <- function(file_source, alpha_value,
                                        selected_path, name_database,
                                        selected_pos_vars, all_colnames){

  if(is.null(file_source)) return(NULL)
  if(file_source == "") return(NULL)
  if(is.null(alpha_value)) return(NULL)
  if(is.null(selected_pos_vars)) return(NULL)
  if(is.null(all_colnames)) return(NULL)

  section01_general_libreries <- '
# # # Section 01 - Libraries --------------------------------------------------------
  library(stats)     # Statistics and graphical functions
  library(openxlsx)  # Import files from xlsx
  library(agricolae) # Tukey test
  library(plotly)    # Advanced graphical functions
  '

  if(file_source == "xlsx"){

    if(is.null(selected_path)) return(NULL)

    list_code_xlsx <- list()
    list_code_xlsx[[1]] <- section01_general_libreries

    list_code_xlsx[[2]] <- '
# # # Section 02 - Operator specifications ------------------------------------------
  # Alpha value
  alpha_value <- _alpha_value_

  # Full file path for Excel
  xlsx_path <- _selected_path_
  xlsx_file_name <- tail(strsplit(xlsx_path, "/")[[1]], n = -1)
  xlsx_file_name
'

    list_code_xlsx[[3]] <- '
# # # Seccion 03 - Inport database from Excel file ----------------------------------
  # Import database
  database <- openxlsx::read.xlsx(xlsxFile =  xlsx_file_name, sheet = 1)
'

    the_code_xlsx <- paste0(unlist(list_code_xlsx), "\n\n\n")


    the_code_xlsx <- gsub(pattern = "_alpha_value_",   replacement = alpha_value,   x = the_code_xlsx)
    the_code_xlsx <- gsub(pattern = "_selected_path_", replacement = selected_path, x = the_code_xlsx)
    return(the_code_xlsx)
  }


  if(file_source == "example"){

    if(is.null(name_database)) return(NULL)

    list_code_example <- list()
    list_code_example[[1]] <- section01_general_libreries

    list_code_example[[2]] <- '
# # # Section 02 - Operator specifications ------------------------------------------
  # Alpha value
  alpha_value <- _alpha_value_
'

    list_code_example[[3]] <- '
# # # Seccion 03 - Inport database from Excel file ----------------------------------
  # Import database
  database <- _name_database_
'

    list_code_example[[4]] <- '
# # # Seccion 04 - Role and var selection -------------------------------------------
  # Import database
    _text_var_vr_
    _text_var_factor_
    _text_var_cov_

  # All selected pos vars on specific order (VR, FACTOR, COV)
    selected_pos_vars <- c(pos_var_vr, pos_var_factor, pos_var_cov)
'
    new_objects <- c("pos_var_vr <- ", "pos_var_factor <- ", "pos_var_cov <- ")
    selected_name_vars <- all_colnames[selected_pos_vars]
    selected_pos_vars <- match(selected_name_vars, all_colnames)
    selected_letter_vars <- openxlsx::int2col(selected_pos_vars)
    selected_role_vars <- c("VR", "FACTOR", "COV")

    step01_1 <- paste0(new_objects, selected_pos_vars)
    n_len1 <- max(nchar(step01_1)) + 2
    step01_2 <- stringr::str_pad(step01_1, width = n_len1, side = "right", pad = " ")


    step02_1 <- paste0("  #'", selected_name_vars, "'")
    n_len2 <- max(nchar(step02_1)) + 2
    step02_2 <- stringr::str_pad(step02_1, width = n_len2, side = "right", pad = " ")

    step03_1 <- paste0(" - Column ", selected_letter_vars)

    all_text <- paste0(step01_2,  step02_2, step03_1)
    names(all_text) <- selected_role_vars

    ###

    the_code_example <- paste0(unlist(list_code_example), "\n\n\n")
    the_code_example <- gsub(pattern = "_alpha_value_",   replacement = alpha_value,   x = the_code_example)
    the_code_example <- gsub(pattern = "_name_database_", replacement = name_database, x = the_code_example)
    the_code_example <- gsub(pattern = "_text_var_vr_", replacement = all_text["VR"], x = the_code_example)
    the_code_example <- gsub(pattern = "_text_var_factor_", replacement = all_text["FACTOR"], x = the_code_example)
    the_code_example <- gsub(pattern = "_text_var_cov_", replacement = all_text["COV"], x = the_code_example)
    the_code_example <- gsub(pattern = "_name_database_", replacement = name_database, x = the_code_example)
    the_code_example <- gsub(pattern = "_name_database_", replacement = name_database, x = the_code_example)
    return(the_code_example)
  }



  #################################################################################

  the_code_02_example <- '
# # # Section 02 - Operator specifications ------------------------------------------
  # Alpha value
  alpha_value <- _alpha_value_



# # # Seccion 03 - Importar la base de datos Excel ----------------------------------
  # Importar base
  database <- _name_database_
'

  if(file_source == "xlsx")
    the_code <- c(the_code_01_libreries, the_code_02_xlsx)
  the_code <- paste0(the_code, collapse = "\n\n\n")

  the_code <- gsub(pattern = "_alpha_value_", replacement = alpha_value, x = the_code)
  the_code <- gsub(pattern = "_selected_path_", replacement = selected_path, x = the_code)

}



slr_full_gen01 <- function(database, pos_var_vr, pos_var_reg, alpha_value){



  selected_pos_vars <- c(pos_var_vr, pos_var_reg)
  selected_role_vars <- c("VR", "X")
  selected_name_vars <- colnames(database)[selected_pos_vars]
  selected_doble_reference_var <- paste0(selected_role_vars, "(", selected_name_vars, ")")

  df_selected_vars <- data.frame(
    "order" = 1:length(selected_pos_vars),
    "var_name" = colnames(database)[selected_pos_vars],
    "var_number" = selected_pos_vars,
    "var_letter" = openxlsx::int2col(selected_pos_vars),
    "var_role" = selected_role_vars,
    "doble_reference" = selected_doble_reference_var
  )

  # Minibase
  minibase <- na.omit(database[selected_pos_vars])
  colnames(minibase) <- selected_role_vars

  df_control_minibase <- data.frame(
    "order" = 1:length(selected_role_vars),
    "var_name" = df_selected_vars$var_name,
    "var_role" = df_selected_vars$var_role,
    "control" = c("is.numeric()", "is.numeric()"),
    "verify" = c(is.numeric(minibase[,1]), is.numeric(minibase[,2]))
  )


  df_show_n <- data.frame(
    "object" = c("database", "minibase"),
    "n_col" = c(ncol(database), ncol(minibase)),
    "n_row" = c(nrow(database), nrow(minibase))
  )



  #####################################################################################

  # Medidas de posicion particionadas (VR)
  df_position_vr_general <- data.frame(
    "min" = min(minibase[,1]),
    "mean" = mean(minibase[,1]),
    "median" = median(minibase[,1]),
    "max" = max(minibase[,1]),
    "n" = length(minibase[,1])
  )



  # Medidas de dispersion particionadas (VR)
  df_dispersion_vr_general <- data.frame(
    "range" = max(minibase[,1]) - min(minibase[,1]),
    "variance" = var(minibase[,1]),
    "standard_deviation" = sd(minibase[,1]),
    "standard_error" = sd(minibase[,1])/(sqrt(length(minibase[,1]))),
    "n" = length(minibase[,1])
  )


  # Medidas de posicion particionadas (VR)
  df_position_X_general <- data.frame(
    "min" = min(minibase[,2]),
    "mean" = mean(minibase[,2]),
    "median" = median(minibase[,2]),
    "max" = max(minibase[,2]),
    "n" = length(minibase[,2])
  )



  # Medidas de dispersion particionadas (VR)
  df_dispersion_X_general <- data.frame(
    "range" = max(minibase[,2]) - min(minibase[,2]),
    "variance" = var(minibase[,2]),
    "standard_deviation" = sd(minibase[,2]),
    "standard_error" = sd(minibase[,2])/(sqrt(length(minibase[,2]))),
    "n" = length(minibase[,2])
  )

  #########################################################################################


  # Analisis
  lm_slr <- lm(VR ~ X, data = minibase)
  summary_slr <- summary(lm_slr)
  table_slr <- as.data.frame(summary_slr$coefficients)

  df_line <- data.frame(
    "order" = 1:length(lm_slr$coefficients),
    "coefficient" = c("intercep", "slope"),
    "model_term" = c("beta_0", "beta_1"),
    "estimate" = lm_slr$coefficients
  )
  rownames(df_line) <- 1:nrow(df_line)


  vector_points_x <- c(min(minibase$X), max(minibase$X))
  vector_points_y <- df_line$estimate[1] + df_line$estimate[2]*vector_points_x

  df_graph01_segment <- data.frame(
    "order" = 1:length(lm_slr$coefficients),
    "details" = c("initial", "end"),
    "x" = vector_points_x,
    "y" = vector_points_y
  )


  df_info_error_slr <- data.frame(
    "residual_standard_error" = summary_slr$sigma,
    "df_error" = summary_slr$df[2])



  p_value_det_coef <- pf(q = summary_slr$fstatistic["value"],
                         df1 = summary_slr$fstatistic["numdf"],
                         df2 = summary_slr$fstatistic["dendf"], lower.tail = F)



  df_determination_coef <- data.frame(
    "r_squared" = summary_slr$r.squared,
    "adj_r_squared" = summary_slr$adj.r.squared,
    "f_statistic" = summary_slr$fstatistic["value"],
    "df_numerator"  = summary_slr$fstatistic["numdf"],
    "df_denominator"  = summary_slr$fstatistic["dendf"],
    "p_value_det_coef" = p_value_det_coef
  )





  ######################################################################################

  dt_rows_database_ok <- rowSums(is.na(database[selected_pos_vars])) == 0


  minibase_mod <- minibase
  minibase_mod$"fitted.values" <- lm_slr$fitted.values
  minibase_mod$"residuals" <- lm_slr$residuals
  minibase_mod$"rstandard" <- rstandard(lm_slr)
  minibase_mod$"id_database" <- c(1:nrow(database))[dt_rows_database_ok]
  minibase_mod$"id_minibase" <- 1:nrow(minibase)




  ######################################################################################




  # Medidas de posicion particionadas (residuals)
  df_position_residuals_general <- data.frame(
    "min" = min(minibase_mod$residuals),
    "mean" = mean(minibase_mod$residuals),
    "median" = median(minibase_mod$residuals),
    "max" = max(minibase_mod$residuals),
    "n" = length(minibase_mod$residuals)
  )



  # Medidas de dispersion particionadas (residuals)
  df_dispersion_residuals_general <- data.frame(
    "range" = max(minibase_mod$residuals) - min(minibase_mod$residuals),
    "variance" = var(minibase_mod$residuals),
    "standard_deviation" = sd(minibase_mod$residuals),
    "standard_error" = sd(minibase_mod$residuals)/(sqrt(length(minibase_mod$residuals))),
    "n" = length(minibase_mod$residuals)
  )





  # # # Seccion 09 - Requisitos del modelo de Ancova con interacción ------------------
  # Test de Normalidad de Shapiro-Wilk
  test_normality_residuals <- shapiro.test(minibase_mod$residuals)
  test_normality_residuals


  sum_residuals <- sum(minibase_mod$residuals)
  sum_residuals

  mean_residuals <- mean(minibase_mod$residuals)
  mean_residuals





  # # Sección 09-6) Tabla con estimaciones del modelo de Ancova (modelo largo) ---



  # hide_: all objects in the internal envirorment
  hide_all_objets <- ls()

  # hide_: All in a output list
  hide_output_list_objects <- sapply(hide_all_objets, function(list_name) { get(list_name)  }, simplify = FALSE)                                                    # HIDE

  # hide_: return!
  return(hide_output_list_objects)
}



slr_full_gen02 <- function(database, pos_var_vr, pos_var_reg, alpha_value){



  results_gen01 <- slr_full_gen01(database, pos_var_vr, pos_var_reg, alpha_value)

  # Los objetos estan ordenados alfabeticamente,y no por el orden de aparicion
  # para el usuario.
  # Vamos a tomar retomar el orden de aparicion y de paso
  # ocultamos algunos objetos:
  # - Los que empiezan con 'hide_'
  # - Los que terminan el "_ok".


  # Obtiene el código fuente de la función
  codigo_fuente <- deparse(body(slr_full_gen01))

  # Busca y selecciona las líneas que contienen "<- data.frame("
  lineas_seleccionadas <- codigo_fuente
  lineas_seleccionadas <- grep("<-", lineas_seleccionadas, value = TRUE)
  lineas_seleccionadas <- grep("\\$.*<-", lineas_seleccionadas, value = TRUE, invert = TRUE)


  #  lineas_seleccionadas <- gsub(" ", "", lineas_seleccionadas)
  lineas_seleccionadas <- trimws(lineas_seleccionadas)

  lineas_seleccionadas <- grep("\\).*<-", lineas_seleccionadas, value = TRUE, invert = TRUE)
  lineas_seleccionadas <- grep("\\].*<-", lineas_seleccionadas, value = TRUE, invert = TRUE)

  #  lineas_seleccionadas <- grep("\\$", lineas_seleccionadas, value = TRUE, invert = TRUE)
  lineas_seleccionadas <- sub(" <-.*", "", lineas_seleccionadas)
  lineas_seleccionadas <- grep("^hide_", lineas_seleccionadas, value = TRUE, invert = TRUE)



  #lineas_seleccionadas <- grep("dt_rows_database_ok", lineas_seleccionadas, value = TRUE, invert = TRUE)
  #lineas_seleccionadas <- grep("minibase_mod", lineas_seleccionadas, value = TRUE, invert = TRUE)
  # lineas_seleccionadas <- grep("_ok$", lineas_seleccionadas, value = TRUE, invert = TRUE)
  # lineas_seleccionadas <- grep("_dif_", lineas_seleccionadas, value = TRUE, invert = TRUE)
  # lineas_seleccionadas <- grep("_point_", lineas_seleccionadas, value = TRUE, invert = TRUE)
  # lineas_seleccionadas <- grep("_intercept_", lineas_seleccionadas, value = TRUE, invert = TRUE)
  # lineas_seleccionadas <- grep("slope_", lineas_seleccionadas, value = TRUE, invert = TRUE)
  # lineas_seleccionadas <- grep("vector_interaction", lineas_seleccionadas, value = TRUE, invert = TRUE)

  results_gen02 <- results_gen01[lineas_seleccionadas]

  return(results_gen02)

}

