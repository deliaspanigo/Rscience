

# # # # Special Functions
# Take the original code from a function
fn_cpiA009_TakeCode <- function(selected_fn){


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
fn_cpiA009_ObjNamesInOrder <- function(selected_fn){

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
fn_cpiA009_control_previous <- function(database, vr_var_name, x01_var_name, x02_var_name,alpha_value){

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



fn_cpiA009_code_p01_test_with <- function(database, vr_var_name, factor_var_name, cov_var_name, alpha_value){





  # # # Selected vars
  vector_all_var_names <- colnames(database)
  vector_name_selected_vars <- c(vr_var_name, factor_var_name, cov_var_name)
  vector_rol_vars <- c("VR", "FACTOR", "COV")

  # Minibase
  minibase <- na.omit(database[,vector_name_selected_vars])
  colnames(minibase) <- vector_rol_vars
  minibase[,2] <- as.factor(minibase[,2])
  #colnames(minibase) <- selected_role_vars


  # # # Selected vars info as dataframe
  df_selected_vars <- data.frame(
    "order" = 1:length(vector_name_selected_vars),
    "var_name" = vector_name_selected_vars,
    "var_number" = match(vector_name_selected_vars, vector_all_var_names),
    "var_letter" = openxlsx::int2col(match(vector_name_selected_vars, vector_all_var_names)),
    "var_role" = vector_rol_vars,
    "doble_reference" = paste0(vector_rol_vars, "(", vector_name_selected_vars, ")")
  )
  df_selected_vars

  df_control_minibase <- data.frame(
    "order" = 1:length(vector_rol_vars),
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
  df_factor_info <- data.frame(
    "order" = 1:nlevels(minibase[,2]),
    "level" = levels(minibase[,2]),
    "n" = as.vector(table(minibase[,2])),
    "color" = rainbow(nlevels(minibase[,2]))
  )

  check_unbalanced_reps <- length(unique(df_factor_info$n)) > 1

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
  df_table_ancova_with <- as.data.frame(summary(aov_ancova_with)[[1]])


  # # # Standard error from model for each level
  model_error_var <- df_table_ancova_with$`Mean Sq`[4]
  model_error_sd <- sqrt(model_error_var)

  df_model_error <- data.frame(
    "order" = df_factor_info$order,
    "level" = df_factor_info$level,
    "n" = df_factor_info$n,
    "model_error_var" = model_error_var,
    "model_error_sd" = model_error_sd
  )
  df_model_error["model_error_se"] <- df_model_error["model_error_sd"]/sqrt(df_model_error$n)
  df_model_error


  ######################################################################################

  dt_rows_database_ok <- rowSums(is.na(database[vector_name_selected_vars])) == 0


  minibase_mod <- minibase
  minibase_mod$"fitted.values" <- lm_ancova_with$fitted.values
  minibase_mod$"residuals" <- lm_ancova_with$residuals
  minibase_mod$"id_database" <- c(1:nrow(database))[dt_rows_database_ok]
  minibase_mod$"id_minibase" <- 1:nrow(minibase)
  minibase_mod$"lvl_order_number" <- as.numeric(minibase[,2])
  minibase_mod$"lvl_color" <- df_factor_info$color[minibase_mod$"lvl_order_number"]

  minibase_mod$"VR_mod"  <- minibase$"VR"  - df_position_vr_levels$"mean"[minibase_mod$"lvl_order_number"]
  minibase_mod$"COV_mod" <- minibase$"COV" - df_position_cov_levels$"mean"[minibase_mod$"lvl_order_number"]


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
  test_residuals_normality <- shapiro.test(minibase_mod$residuals)
  test_residuals_normality


  test_residuals_homogeneity <- bartlett.test(residuals ~ FACTOR, data = minibase_mod)
  test_residuals_homogeneity


  sum_residuos <- sum(minibase_mod$residuals)
  sum_residuos

  # # Sección 09-2) Tabla Tukey --------------------------------------------------
  # Tukey completo
  tukey01_full_groups <- agricolae::HSD.test(y = lm_ancova_with,
                                             trt = colnames(minibase)[2],
                                             alpha = alpha_value,
                                             group = TRUE,
                                             console = FALSE,
                                             unbalanced = check_unbalanced_reps)

  tukey02_full_pairs <- agricolae::HSD.test(y = lm_ancova_with,
                                            trt = colnames(minibase)[2],
                                            alpha = alpha_value,
                                            group = FALSE,
                                            console = FALSE,
                                            unbalanced = check_unbalanced_reps)


  ########################################################

  # # Original table from R about Tukey
  df_tukey_original_table <- tukey01_full_groups$groups
  df_tukey_original_table



  # # # New table about Tukey
  df_tukey_table <- data.frame(
    "level" = rownames(tukey01_full_groups$groups),
    "mean" = tukey01_full_groups$groups[,1],
    "group" = tukey01_full_groups$groups[,2]
  )
  df_tukey_table

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

  df_slop <- data.frame(
    "orden" = df_interaction$"order",
    "level" = names(vector_interaction),
    "slop" = df_interaction$"beta_i"
  )
  df_slop

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
    "end_point_i_x" = vector_end_point_y_levels,
    "color" = df_factor_info$color
  )



  df_table_factor_plot002 <- data.frame(
    "order" = df_factor_info$order,
    "level" = df_factor_info$level,
    "n" = df_factor_info$n,
    "mean" = tapply(minibase[,1], minibase[,2], mean),
    "model_error_sd" = df_model_error$model_error_sd
  )
  df_table_factor_plot002["lower_limit"] <- df_table_factor_plot002$mean - df_table_factor_plot002$model_error_sd
  df_table_factor_plot002["upper_limmit"] <- df_table_factor_plot002$mean + df_table_factor_plot002$model_error_sd
  df_table_factor_plot002["color"] <- df_factor_info$color
  df_table_factor_plot002




  df_table_factor_plot003 <- data.frame(
    "order" = df_factor_info$order,
    "level" = df_factor_info$level,
    "n" = df_factor_info$n,
    "mean" = tapply(minibase[,1], minibase[,2], mean),
    "model_error_se" = df_model_error$model_error_se
  )
  df_table_factor_plot003["lower_limit"] <- df_table_factor_plot003$mean - df_table_factor_plot003$model_error_se
  df_table_factor_plot003["upper_limmit"] <- df_table_factor_plot003$mean + df_table_factor_plot003$model_error_se
  df_table_factor_plot003["color"] <- df_factor_info$color
  correct_pos_letters <- order(df_tukey_table$level)
  vector_letters <- df_tukey_table$group[correct_pos_letters]
  df_table_factor_plot003["group"] <- vector_letters


  # # # # # Section 13 - Special table to plots ----------------------------------






  # --- # hide_: Proccesing objects order
  hide_correct_order <- fn_cpiA009_ObjNamesInOrder(selected_fn = fn_cpiA009_code_p01_test_with)
  hide_output_list_objects <- mget(hide_correct_order)

  # --- # hide_: return!
  return(hide_output_list_objects)

}


fn_cpiA009_code_p01_test_without <- function(database, vr_var_name, factor_var_name, cov_var_name, alpha_value){





  # # # # # Section 04 - Var rols and minibase -----------------------------------
  # # # Selected vars
  vector_all_var_names <- colnames(database)
  vector_name_selected_vars <- c(vr_var_name, factor_var_name, cov_var_name)
  vector_rol_vars <- c("VR", "FACTOR", "COV")



  # # # Selected vars info as dataframe
  df_selected_vars <- data.frame(
    "order" = 1:length(vector_name_selected_vars),
    "var_name" = vector_name_selected_vars,
    "var_number" = match(vector_name_selected_vars, vector_all_var_names),
    "var_letter" = openxlsx::int2col(match(vector_name_selected_vars, vector_all_var_names)),
    "var_role" = vector_rol_vars,
    "doble_reference" = paste0(vector_rol_vars, "(", vector_name_selected_vars, ")")
  )
  df_selected_vars





  # # # # # Section 05 - minibase ------------------------------------------------
  # Only selected vars. Only completed rows. Factor columns as factor object in R.
  minibase <- na.omit(database[vector_name_selected_vars])
  colnames(minibase) <- vector_rol_vars
  minibase$"FACTOR" <- as.factor(minibase$"FACTOR")



  # # # Anova control
  # 'VR' must be numeric and 'FACTOR must be factor.
  df_control_minibase <- data.frame(
    "order" = 1:nrow(df_selected_vars),
    "var_name" = df_selected_vars$var_name,
    "var_role" = df_selected_vars$var_role,
    "control" = c("is.numeric()", "is.factor()", "is.numeric()"),
    "verify" = c(is.numeric(minibase[,1]), is.factor(minibase[,2]), is.numeric(minibase[,3]))
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



  # # # Factor info
  # Default order for levels its alphabetic order.
  df_factor_info <- data.frame(
    "order" = 1:nlevels(minibase[,2]),
    "level" = levels(minibase[,2]),
    "n" = as.vector(table(minibase[,2])),
    "mean" = tapply(minibase[,1], minibase[,2], mean),
    "color" = rainbow(nlevels(minibase[,2]))
  )
  df_factor_info



  # # # Unbalanced reps for levels?
  # Important information for Tukey.
  # If reps its equal or not equal in all levels must be detailled
  # on Tukey.
  check_unbalanced_reps <- length(unique(df_factor_info$n)) > 1
  check_unbalanced_reps





  # # # # # Section 06 - Anova Test ----------------------------------------------
  # # # Anova test
  lm_ancova_without <- lm(VR ~ COV + FACTOR, data = minibase)   # Linear model
  aov_ancova_without <- aov(lm_ancova_without)                                 # R results for ancova
  df_table_ancova_without <- as.data.frame(summary(aov_ancova_without)[[1]])   # Common ancova table
  df_table_ancova_without



  # # # Standard error from model for each level
  model_error_var <- df_table_ancova_without$`Mean Sq`[3]
  model_error_sd <- sqrt(model_error_var)

  df_model_error <- data.frame(
    "order" = df_factor_info$order,
    "level" = df_factor_info$level,
    "n" = df_factor_info$n,
    "model_error_var" = model_error_var,
    "model_error_sd" = model_error_sd
  )
  df_model_error["model_error_se"] <- df_model_error["model_error_sd"]/sqrt(df_model_error$n)
  df_model_error





  # # # # # Section 07 - minibase_mod --------------------------------------------
  # # # Detect rows on database there are on minibase
  dt_rows_database_ok <- rowSums(!is.na(database[vector_name_selected_vars])) == ncol(minibase)



  # # # Object minibase_mod and new cols
  minibase_mod <- minibase
  minibase_mod$"lvl_order_number" <- as.numeric(minibase_mod[,2])
  minibase_mod$"lvl_color" <- df_factor_info$color[minibase_mod$"lvl_order_number"]
  minibase_mod$"fitted.values" <- df_factor_info$"mean"[minibase_mod$"lvl_order_number"]
  minibase_mod$"residuals" <- lm_ancova_without$residuals
  minibase_mod$"id_database" <- c(1:nrow(database))[dt_rows_database_ok]
  minibase_mod$"id_minibase" <- 1:nrow(minibase)
  minibase_mod$"studres" <- minibase_mod$"residuals"/model_error_sd





  # # # # # Section 08 - Requeriments for residuals-------------------------------
  # # # Normality test (Shapiro-Wilk)
  test_residuals_normality <- shapiro.test(minibase_mod$residuals)
  test_residuals_normality




  # # # Homogeinidy test (Bartlett)
  test_residuals_homogeneity <- bartlett.test(residuals ~ FACTOR, data = minibase_mod)
  test_residuals_homogeneity



  # # # Residuals variance from levels from original residuals
  df_residuals_variance_levels <- data.frame(
    "order" = 1:nlevels(minibase_mod[,2]),
    "level" = levels(minibase_mod[,2]),
    "variance" = tapply(minibase_mod$residuals, minibase_mod[,2], var),
    "n" = tapply(minibase_mod$residuals, minibase_mod[,2], length)
  )
  df_residuals_variance_levels



  # # # Sum for residuals
  sum_residuals <- sum(minibase_mod$residuals)
  sum_residuals



  # # # Mean for residuals
  mean_residuals <- mean(minibase_mod$residuals)
  mean_residuals





  # # # # # Section 09 - Tukey --------------------------------------------------
  # # # Tukey test - Tukey with groups - Full version
  tukey01_full_groups <- agricolae::HSD.test(y = lm_ancova_without,
                                             trt = "FACTOR",
                                             alpha = alpha_value,
                                             group = TRUE,
                                             console = FALSE,
                                             unbalanced = check_unbalanced_reps)



  # # # Tukey test - Tukey pairs comparation - Full version
  tukey02_full_pairs <- agricolae::HSD.test(y = lm_ancova_without,
                                            trt = "FACTOR",
                                            alpha = alpha_value,
                                            group = FALSE,
                                            console = FALSE,
                                            unbalanced = check_unbalanced_reps)



  # # Original table from R about Tukey
  df_tukey_original_table <- tukey01_full_groups$groups
  df_tukey_original_table



  # # # New table about Tukey
  df_tukey_table <- data.frame(
    "level" = rownames(tukey01_full_groups$groups),
    "mean" = tukey01_full_groups$groups[,1],
    "group" = tukey01_full_groups$groups[,2]
  )
  df_tukey_table





  # # # # # Section 10 - Partitioned Measures (VR)--------------------------------
  # # # Partitioned Measures of Position (VR)
  df_vr_position_levels <- data.frame(
    "order" = 1:nlevels(minibase[,2]),
    "level" = levels(minibase[,2]),
    "min" = tapply(minibase[,1], minibase[,2], min),
    "mean" = tapply(minibase[,1], minibase[,2], mean),
    "Q1" = tapply(minibase[,1], minibase[,2], quantile, 0.25),
    "median" = tapply(minibase[,1], minibase[,2], median),
    "Q3" = tapply(minibase[,1], minibase[,2], quantile, 0.75),
    "max" = tapply(minibase[,1], minibase[,2], max),
    "n" = tapply(minibase[,1], minibase[,2], length)
  )



  # # # Partitioned Measures of Dispersion (VR)
  df_vr_dispersion_levels <- data.frame(
    "order" = 1:nlevels(minibase[,2]),
    "level" = levels(minibase[,2]),
    "range" = tapply(minibase[,1], minibase[,2], function(x){max(x) - min(x)}),
    "variance" = tapply(minibase[,1], minibase[,2], var),
    "standard_deviation" = tapply(minibase[,1], minibase[,2], sd),
    "standard_error" = tapply(minibase[,1], minibase[,2], function(x){sd(x)/sqrt(length(x))}),
    "n" = tapply(minibase[,1], minibase[,2], length)
  )
  df_vr_dispersion_levels



  # # # General Measures of Position (VR)
  df_vr_position_general <- data.frame(
    "min" = min(minibase[,1]),
    "mean" = mean(minibase[,1]),
    "median" = median(minibase[,1]),
    "max" = max(minibase[,1]),
    "n" = length(minibase[,1])
  )
  df_vr_position_general



  # # # General Measures of Dispersion (VR)
  df_vr_dispersion_general <- data.frame(
    "range" = max(minibase[,1]) - min(minibase[,1]),
    "variance" = var(minibase[,1]),
    "standard_deviation" = sd(minibase[,1]),
    "standard_error" = sd(minibase[,1])/(sqrt(length(minibase[,1]))),
    "n" = length(minibase[,1])
  )
  df_vr_dispersion_general





  # # # # # Section 11 - Partitioned Measures (Residuals)-------------------------
  # # # Partitioned Measures of Position (residuals)
  df_residuals_position_levels <- data.frame(
    "order" = 1:nlevels(minibase_mod[,2]),
    "level" = levels(minibase_mod[,2]),
    "min" = tapply(minibase_mod$residuals, minibase_mod[,2], min),
    "mean" = tapply(minibase_mod$residuals, minibase_mod[,2], mean),
    "median" = tapply(minibase_mod$residuals, minibase_mod[,2], median),
    "max" = tapply(minibase_mod$residuals, minibase_mod[,2], max),
    "n" = tapply(minibase_mod$residuals, minibase_mod[,2], length)
  )
  df_residuals_position_levels



  # # # Partitioned Measures of Dispersion (residuals)
  df_residual_dispersion_levels <- data.frame(
    "order" = 1:nlevels(minibase_mod[,2]),
    "level" = levels(minibase_mod[,2]),
    "range" = tapply(minibase_mod$residuals, minibase_mod[,2], function(x){max(x) - min(x)}),
    "variance" = tapply(minibase_mod$residuals, minibase_mod[,2], var),
    "standard_deviation" = tapply(minibase_mod$residuals, minibase_mod[,2], sd),
    "standard_error" = tapply(minibase_mod$residuals, minibase_mod[,2], function(x){sd(x)/sqrt(length(x))}),
    "n" = tapply(minibase_mod$residuals, minibase_mod[,2], length)
  )
  df_residual_dispersion_levels



  # # # General Measures of Position (residuals)
  df_residuals_position_general <- data.frame(
    "min" = min(minibase_mod$residuals),
    "mean" = mean(minibase_mod$residuals),
    "median" = median(minibase_mod$residuals),
    "max" = max(minibase_mod$residuals),
    "n" = length(minibase_mod$residuals)
  )
  df_residuals_position_general



  # # # General Measures of Dispersion (residuals)
  df_residuals_dispersion_general <- data.frame(
    "range" = max(minibase_mod$residuals) - min(minibase_mod$residuals),
    "variance" = var(minibase_mod$residuals),
    "standard_deviation" = sd(minibase_mod$residuals),
    "standard_error" = sd(minibase_mod$residuals)/(sqrt(length(minibase_mod$residuals))),
    "n" = length(minibase_mod$residuals)
  )
  df_residuals_dispersion_general





  # # # # # Section 12 - Model estimators ----------------------------------------
  # # # Means for each level
  vector_est_mu_i <- df_vr_position_levels$mean
  vector_est_mu_i



  # # # Mean of means
  est_mu <- mean(vector_est_mu_i)
  vector_est_mu <- rep(est_mu, length(vector_est_mu_i))
  vector_est_mu



  # # # Tau efects
  vector_est_tau_i <- vector_est_mu_i - vector_est_mu
  vector_est_tau_i



  # # # Sum of tau efects
  sum_est_tau_i <- sum(vector_est_tau_i)
  sum_est_tau_i



  # # # Long model information on dataframe
  df_anova1way_model_long <- data.frame(
    "order" = df_factor_info$order,
    "level" = df_factor_info$level,
    "n" = df_factor_info$n,
    "est_mu" = vector_est_mu,
    "est_tau_i" = vector_est_tau_i
  )
  df_anova1way_model_long



  # # # Short model information on dataframe
  df_anova1way_model_short <- data.frame(
    "order" = df_factor_info$order,
    "level" = df_factor_info$level,
    "n" = df_factor_info$n,
    "est_mu_i" = vector_est_mu_i
  )
  df_anova1way_model_short





  # # # # # Section 13 - Special table to plots ----------------------------------

  # # # Table for plot001
  df_table_factor_plot001 <- data.frame(
    "order" = df_factor_info$order,
    "level" = df_factor_info$level,
    "n" = df_factor_info$n,
    "mean" = tapply(minibase[,1], minibase[,2], mean),
    "min" = tapply(minibase[,1], minibase[,2], min),
    "max" = tapply(minibase[,1], minibase[,2], max),
    "sd" = tapply(minibase[,1], minibase[,2], sd),
    "var" = tapply(minibase[,1], minibase[,2], var)
  )

  df_table_factor_plot002 <- data.frame(
    "order" = df_factor_info$order,
    "level" = df_factor_info$level,
    "n" = df_factor_info$n,
    "mean" = tapply(minibase[,1], minibase[,2], mean),
    "model_error_sd" = df_model_error$model_error_sd
  )
  df_table_factor_plot002["lower_limit"] <- df_table_factor_plot002$mean - df_table_factor_plot002$model_error_sd
  df_table_factor_plot002["upper_limmit"] <- df_table_factor_plot002$mean + df_table_factor_plot002$model_error_sd
  df_table_factor_plot002["color"] <- df_factor_info$color
  df_table_factor_plot002



  df_table_factor_plot003 <- data.frame(
    "order" = df_factor_info$order,
    "level" = df_factor_info$level,
    "n" = df_factor_info$n,
    "mean" = tapply(minibase[,1], minibase[,2], mean),
    "model_error_se" = df_model_error$model_error_se
  )
  df_table_factor_plot003["lower_limit"] <- df_table_factor_plot003$mean - df_table_factor_plot003$model_error_se
  df_table_factor_plot003["upper_limmit"] <- df_table_factor_plot003$mean + df_table_factor_plot003$model_error_se
  df_table_factor_plot003["color"] <- df_factor_info$color
  df_table_factor_plot003



  # # # Table for plot004
  df_table_factor_plot004 <- df_vr_position_levels
  df_table_factor_plot004["color"] <- df_factor_info$color

  # # # Table for plot005
  df_table_factor_plot005 <- df_table_factor_plot004

  # # # Table for plot006
  df_table_factor_plot006 <- df_table_factor_plot004


  df_table_factor_plot007 <- df_table_factor_plot003
  correct_pos_letters <- order(df_tukey_table$level)
  vector_letters <- df_tukey_table$group[correct_pos_letters]
  df_table_factor_plot007["group"] <- vector_letters

  # # # Table for plot006
  df_table_residuals_plot001 <- data.frame(
    "order" = 1:nlevels(minibase_mod[,2]),
    "level" = levels(minibase_mod[,2]),
    "n" = tapply(minibase_mod$residuals, minibase_mod[,2], length),
    "min" = tapply(minibase_mod$residuals, minibase_mod[,2], min),
    "mean" = tapply(minibase_mod$residuals, minibase_mod[,2], mean),
    "max" = tapply(minibase_mod$residuals, minibase_mod[,2], max),
    "var" = tapply(minibase_mod$residuals, minibase_mod[,2], var),
    "sd" = tapply(minibase_mod$residuals, minibase_mod[,2], sd),
    "color" = df_factor_info$color
  )
  df_table_residuals_plot001

  # # # Table for plot006
  df_table_residuals_plot002 <- df_table_residuals_plot001

  # # # Table for plot006
  df_table_residuals_plot003 <- df_table_residuals_plot001

  # # # Table for plot006
  df_table_residuals_plot004 <- data.frame(
    "variable" = "residuals",
    "n" = length(minibase_mod$residuals),
    "min" = min(minibase_mod$residuals),
    "mean" = mean(minibase_mod$residuals),
    "max" = max(minibase_mod$residuals),
    "var" = var(minibase_mod$residuals),
    "sd" = sd(minibase_mod$residuals),
    "model_error_var" = model_error_var,
    "model_error_sd" = model_error_sd
  )

  # # # Table for plot006
  df_table_residuals_plot005  <- df_table_residuals_plot004

  # # # Table for plot006
  df_table_residuals_plot006 <- data.frame(
    "order" = 1:nlevels(minibase_mod[,2]),
    "level" = levels(minibase_mod[,2]),
    "n" = tapply(minibase_mod$studres, minibase_mod[,2], length),
    "min" = tapply(minibase_mod$studres, minibase_mod[,2], min),
    "mean" = tapply(minibase_mod$studres, minibase_mod[,2], mean),
    "max" = tapply(minibase_mod$studres, minibase_mod[,2], max),
    "var" = tapply(minibase_mod$studres, minibase_mod[,2], var),
    "sd" = tapply(minibase_mod$studres, minibase_mod[,2], sd),
    "color" = df_factor_info$color
  )


  # # # Table for plot006
  df_table_residuals_plot007 <- df_table_residuals_plot006


  df_table_residuals_plot008 <- data.frame(
    "variable" = "studres",
    "n" = length(minibase_mod$studres),
    "min" = min(minibase_mod$studres),
    "mean" = mean(minibase_mod$studres),
    "max" = max(minibase_mod$studres),
    "var" = var(minibase_mod$studres),
    "sd" = sd(minibase_mod$studres)
  )


  df_table_residuals_plot009 <- df_table_residuals_plot008

  df_table_residuals_plot010 <- df_table_residuals_plot008



  # --- # hide_: Proccesing objects order
  hide_correct_order <- fn_cpiA009_ObjNamesInOrder(selected_fn = fn_cpiA009_code_p01_test_with)
  hide_output_list_objects <- mget(hide_correct_order)

  # --- # hide_: return!
  return(hide_output_list_objects)

}



fn_cpiA009_code_p02_plot001 <- function(results_p01_test){



  new_plot <-  with(results_p01_test,{


    # # # Create a new plot...
    plot001 <- plotly::plot_ly()


    # # # Adding errors...
    plot001 <- plotly::add_trace(p = plot001,
                                 type = "scatter",
                                 mode = "markers",
                                 x = minibase_mod$COV,
                                 y = minibase_mod$VR,
                                 color = minibase_mod$FACTOR,
                                 colors = df_factor_info$color,
                                 marker = list(size = 15, opacity = 0.7))


    # plot001<-  add_text(p = plot001,
    #                      x = df_table_plot002$level,
    #                      y = df_table_plot002$mean,
    #                      text = df_table_plot002$group, name = "Tukey Group",
    #                      size = 20)

    # # # Title and settings...
    plot001 <- plotly::layout(p = plot001,
                              xaxis = list(title = "COV"),
                              yaxis = list(title = "VR"),
                              title = "Plot 001 - Scatterplot",
                              font = list(size = 20),
                              margin = list(t = 100))



    # # # Without zerolines
    plot001 <-plotly::layout(p = plot001,
                             xaxis = list(zeroline = FALSE),
                             yaxis = list(zeroline = FALSE))


    for (x in 1:nrow(df_segments)){
      plot001 <- add_segments(p = plot001,
                              x = df_segments$min_cov_i_x[x],
                              y = df_segments$initial_point_i_y[x],
                              xend = df_segments$max_cov_i_x[x],
                              yend = df_segments$end_point_i_x[x],
                              line = list(color = df_segments$color[x],
                                          width = 4),
                              name = df_segments$level[x])
    }
    # # # Plot output
    plot001
  })



  return(new_plot)

}



fn_cpiA009_code_p02_plot002 <- function(results_p01_test){



  new_plot <-  with(results_p01_test,{


    # # # Create a new plot...
    plot002 <- plotly::plot_ly()


    # # # Adding errors...
    plot002 <-   plotly::add_trace(p = plot002,
                                   type = "scatter",
                                   mode = "markers",
                                   x = df_table_factor_plot002$level,
                                   y = df_table_factor_plot002$mean,
                                   color = df_table_factor_plot002$level,
                                   colors = df_table_factor_plot002$color,
                                   marker = list(symbol = "line-ew-open",
                                                 size = 50,
                                                 opacity = 1,
                                                 line = list(width = 5)),
                                   error_y = list(type = "data", array = df_table_factor_plot002$model_error_sd)
    )


    # # # Title and settings...
    plot002 <- plotly::layout(p = plot002,
                              xaxis = list(title = "FACTOR"),
                              yaxis = list(title = "VR"),
                              title = "Plot 002 - Mean y model standard deviation",
                              font = list(size = 20),
                              margin = list(t = 100))

    # # # Without zerolines
    plot002 <-plotly::layout(p = plot002,
                             xaxis = list(zeroline = FALSE),
                             yaxis = list(zeroline = FALSE))

    # # # Plot output
    plot002


  })



  return(new_plot)

}




fn_cpiA009_code_p02_plot003 <- function(results_p01_test){



  new_plot <-  with(results_p01_test,{


    # # # Create a new plot...
    plot003 <- plotly::plot_ly()


    # # # Adding errors...
    plot003 <-   plotly::add_trace(p = plot003,
                                   type = "scatter",
                                   mode = "markers",
                                   x = df_table_factor_plot003$level,
                                   y = df_table_factor_plot003$mean,
                                   color = df_table_factor_plot003$level,
                                   colors = df_table_factor_plot003$color,
                                   marker = list(symbol = "line-ew-open",
                                                 size = 50,
                                                 opacity = 1,
                                                 line = list(width = 5)),
                                   error_y = list(type = "data", array = df_table_factor_plot003$model_error_se)
    )


    plot003 <-  add_text(p = plot003,
                                x = df_table_factor_plot003$level,
                                y = df_table_factor_plot003$mean,
                                text = df_table_factor_plot003$group, name = "Tukey Group",
                                size = 20)

    # # # Title and settings...
    plot003 <- plotly::layout(p = plot003,
                              xaxis = list(title = "FACTOR"),
                              yaxis = list(title = "VR"),
                              title = "Plot 003 - Mean y model standard error",
                              font = list(size = 20),
                              margin = list(t = 100))

    # # # Without zerolines
    plot003 <-plotly::layout(p = plot003,
                             xaxis = list(zeroline = FALSE),
                             yaxis = list(zeroline = FALSE))

    # # # Plot output
    plot003


  })



  return(new_plot)

}




fn_cpiA009_code_p02_plot004 <- function(results_p01_test){




  new_plot <-  with(results_p01_test,{


    # # # Create a new plot...
    plot004 <- plotly::plot_ly()


    # # # Adding errors...
    plot004 <- plotly::add_trace(p = plot004,
                                 type = "scatter",
                                 mode = "markers",
                                 x = minibase_mod$COV_mod,
                                 y = minibase_mod$VR_mod,
                                 color = minibase_mod$FACTOR,
                                 colors = df_factor_info$color,
                                 marker = list(size = 15, opacity = 0.7))


    # plot001<-  add_text(p = plot001,
    #                      x = df_table_plot002$level,
    #                      y = df_table_plot002$mean,
    #                      text = df_table_plot002$group, name = "Tukey Group",
    #                      size = 20)

    # # # Title and settings...
    plot004 <- plotly::layout(p = plot004,
                              xaxis = list(title = "COV_mod"),
                              yaxis = list(title = "VR_mod"),
                              title = "Plot 001 - Scatterplot",
                              font = list(size = 20),
                              margin = list(t = 100))



    # # # Without zerolines
    plot004 <-plotly::layout(p = plot004,
                             xaxis = list(zeroline = FALSE),
                             yaxis = list(zeroline = FALSE))


    # for (x in 1:nrow(df_segments)){
    #   plot001 <- add_segments(p = plot001,
    #                           x = df_segments$min_cov_i_x[x],
    #                           y = df_segments$initial_point_i_y[x],
    #                           xend = df_segments$max_cov_i_x[x],
    #                           yend = df_segments$end_point_i_x[x],
    #                           line = list(color = df_segments$color[x],
    #                                       width = 4),
    #                           name = df_segments$level[x])
    # }
    # # # Plot output
    plot004
  })



  return(new_plot)

}


# Control post
fn_cpiA009_control_p01_test <- function(all_results){

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


fn_cpiA009_control_p02_plots <- function(all_results){

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





fn_cpiA009_gen01 <- function(database,  vr_var_name, factor_var_name, cov_var_name,alpha_value){

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
  output_list$"R_code"$"p01_test"    <- fn_cpiA009_TakeCode(selected_fn = fn_cpiA009_code_p01_test_with)
  output_list$"R_code"$"p02_plots"$"plot001"   <- fn_cpiA009_TakeCode(selected_fn = fn_cpiA009_code_p02_plot001)
  output_list$"R_code"$"p02_plots"$"plot002"   <- fn_cpiA009_TakeCode(selected_fn = fn_cpiA009_code_p02_plot002)
  output_list$"R_code"$"p02_plots"$"plot003"   <- fn_cpiA009_TakeCode(selected_fn = fn_cpiA009_code_p02_plot003)
  output_list$"R_code"$"p02_plots"$"plot004"   <- fn_cpiA009_TakeCode(selected_fn = fn_cpiA009_code_p02_plot004)


  # Step 02 - Pre Control ------------------------------------------------------
  output_list$"check_control"$"previous" <- fn_cpiA009_control_previous(database, vr_var_name,
                                                                        factor_var_name,
                                                                        cov_var_name,
                                                                        alpha_value)





  # Step 03 - Results ----------------------------------------------------------
  output_list$"R_results"$"p01_test" <- fn_cpiA009_code_p01_test_with(database,  vr_var_name, factor_var_name, cov_var_name, alpha_value)


  output_list$"R_results"$"p02_plots"$"plot001" <- fn_cpiA009_code_p02_plot001(results_p01_test = output_list$"R_results"$"p01_test")
  output_list$"R_results"$"p02_plots"$"plot002" <- fn_cpiA009_code_p02_plot002(results_p01_test = output_list$"R_results"$"p01_test")
  output_list$"R_results"$"p02_plots"$"plot003" <- fn_cpiA009_code_p02_plot003(results_p01_test = output_list$"R_results"$"p01_test")
  output_list$"R_results"$"p02_plots"$"plot004" <- fn_cpiA009_code_p02_plot004(results_p01_test = output_list$"R_results"$"p01_test")





  return(output_list)

}


fn_cpiA009_gen02 <- function(database,  vr_var_name, factor_var_name, cov_var_name, alpha_value){


  all_results <- fn_cpiA009_gen01(database,  vr_var_name, factor_var_name, cov_var_name, alpha_value)



  output_list <- list()


  # Out01 - Analysis -------------------------------------------------------------
  selection01 <- c("df_selected_vars",
                   "df_factor_info",
                   "check_unbalanced_reps",
                   "df_table_ancova_with",
                   "df_tukey_table",
                   "df_slop")

  output_list$"out01_analysis" <- all_results$R_results$p01_test[selection01]




  # Out02 - Requeriments ---------------------------------------------------------
  selection02 <- c("test_residuals_normality", "test_residuals_homogeneity")

  output_list$"out02_requeriments" <- all_results$R_results$p01_test[selection02]



  # Out03 - Plots and Tables - Factor --------------------------------------------
  output_list$"out03A_plots" <- all_results$R_results$p02_plots


  selection03B <- c("df_table_plot001", "df_table_plot002",
                    "df_table_plot003", "df_table_plot004")
  output_list$"out03B_tableplots" <- all_results$R_results$p01_test[selection03B]




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


