

# # # # Special Functions
# Take the original code from a function
fn_cpiA020_TakeCode <- function(selected_fn){


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
fn_cpiA020_ObjNamesInOrder <- function(selected_fn){


  selected_fn = fn_cpiA020_code_p01_test
  selected_code <- deparse(body(selected_fn))
  selected_code <- grep("<-", selected_code, value = TRUE)
  selected_code <- selected_code[grep("^ {0,4}[^ ]", selected_code)]
  #selected_code <- selected_code[grep("^(\t{0,1}[^\t])", selected_code)]
  #selected_code <- grep("^(\t{0,1}[^\t])", selected_code)
  selected_code <- trimws(selected_code)
  selected_code <- gsub("\\s", "", selected_code)
  selected_code <- sub("<-.*", "", selected_code)
  selected_code <- grep("^[a-zA-Z0-9._]*$", selected_code, value = TRUE)
  selected_code <- grep("^hide", selected_code, value = TRUE, invert = TRUE)

  # # # # # #
  selected_code <- grep("^detail_name", selected_code, value = TRUE, invert = TRUE)
  selected_code <- grep("^selected_role", selected_code, value = TRUE, invert = TRUE)
  selected_code

  return(selected_code)

}


# # # # Control funcitons
# Control previous
fn_cpiA020_control_previous <- function(database, vr_var_name, x_var_name, alpha_value){

  dt_ok <- FALSE

  # # # # # # Database
  # 1) Database can not be NULL
  if(is.null(database)){
    text_output <- "Control pre test 001: Object 'database' can not be a NULL."
    return(Hmisc::llist(dt_ok, text_output))
  }

  # 2) Database must be a dataframe
  if(!is.data.frame(database)){
    text_output <- "Control pre test 002: Object 'database' must be a dataframe."
    return(Hmisc::llist(dt_ok, text_output))
  }

  # 3) Database must has at least 2 columns
  if(!(ncol(database) >= 2)){
    text_output <- "Control pre test 003: Object 'database' must has al least 2 columns."
    return(Hmisc::llist(dt_ok, text_output))
  }


  # 4) Database must has at least 2 rows
  if(!(nrow(database) >= 2)){
    text_output <- "Control pre test 004: Object 'database' must has al least 2 rows."
    return(Hmisc::llist(dt_ok, text_output))
  }




  # # # # # # # # vr_var_name
  # 5) vr_var_name is not NULL
  if(is.null(vr_var_name)){
    text_output <- "Control pre test 005: Object 'vr_var_name' can not be NULL."
    return(Hmisc::llist(dt_ok, text_output))
  }

  # 6) vr_var_name is a vector
  if(!is.vector(vr_var_name)){
    text_output <- "Control pre test 006: Object 'vr_var_name' must be vector."
    return(Hmisc::llist(dt_ok, text_output))
  }

  # 7) vr_var_name is a vector
  if(!(length(vr_var_name) == 1)){
    text_output <- "Control pre test 007: Object 'vr_var_name' must be vector of length 1."
    return(Hmisc::llist(dt_ok, text_output))
  }

  # 8) vr_var_name is not NA
  if(is.na(vr_var_name)){
    text_output <- "Control pre test 008: Object 'vr_var_name' can not be NA."
    return(Hmisc::llist(dt_ok, text_output))
  }

  # 9) vr_var_name is character
  if(!is.character(vr_var_name)){
    text_output <- "Control pre test 009: Object 'vr_var_name' must be character."
    return(Hmisc::llist(dt_ok, text_output))
  }

  # 15) x_var_name is a colname from database
  if(!(vr_var_name %in% colnames(database))){
    text_output <- "Control pre test 010: Object 'vr_var_name' must be a colname from database."
    return(Hmisc::llist(dt_ok, text_output))
  }


  # # # # # # # # x_var_name
  # 10) x_var_name is not NULL
  if(is.null(x_var_name)){
    text_output <- "Control pre test 011: Object 'x_var_name' can not be NULL."
    return(Hmisc::llist(dt_ok, text_output))
  }

  # 11) x_var_name is a vector
  if(!is.vector(x_var_name)){
    text_output <- "Control pre test 012: Object 'x_var_name' must be vector."
    return(Hmisc::llist(dt_ok, text_output))
  }

  # 12) x_var_name is a vector
  if(!(length(x_var_name) == 1)){
    text_output <- "Control pre test 013: Object 'x_var_name' must be vector of length 1."
    return(Hmisc::llist(dt_ok, text_output))
  }

  # 13) x_var_name is not NA
  if(is.na(x_var_name)){
    text_output <- "Control pre test 014: Object 'x_var_name' can not be NA."
    return(Hmisc::llist(dt_ok, text_output))
  }

  # 14) x_var_name is character
  if(!is.character(x_var_name)){
    text_output <- "Control pre test 015: Object 'x_var_name' must be character."
    return(Hmisc::llist(dt_ok, text_output))
  }


  # 15) x_var_name is a colname from database
  if(!(x_var_name %in% colnames(database))){
    text_output <- "Control pre test 016: Object 'x_var_name' must be a colname from database."
    return(Hmisc::llist(dt_ok, text_output))
  }



  # # # # # # # # alpha_value
  # 16) alpha_value is not NULL
  if(is.null(alpha_value)){
    text_output <- "Control pre test 017: Object 'alpha_value' can not be NULL."
    return(Hmisc::llist(dt_ok, text_output))
  }

  # 17) alpha_value is a vector
  if(!is.vector(alpha_value)){
    text_output <- "Control pre test 018: Object 'alpha_value' must be vector."
    return(Hmisc::llist(dt_ok, text_output))
  }

  # 18) alpha_value is a vector
  if(!(length(alpha_value) == 1)){
    text_output <- "Control pre test 019: Object 'alpha_value' must be vector of length 1."
    return(Hmisc::llist(dt_ok, text_output))
  }

  # 19) alpha_value is not NA
  if(is.na(alpha_value)){
    text_output <- "Control pre test 020: Object 'alpha_value' can not be NA."
    return(Hmisc::llist(dt_ok, text_output))
  }

  # 20) alpha_value is numeric
  if(!is.numeric(alpha_value)){
    text_output <- "Control pre test 021: Object 'alpha_value' must be numeric."
    return(Hmisc::llist(dt_ok, text_output))
  }


  # 20) alpha_value is between 0 and 1
  if(!(alpha_value >= 0 && alpha_value <= 1)){
    text_output <- "Control pre test 022: Object 'alpha_value' must be a number between 0 and 1."
    return(Hmisc::llist(dt_ok, text_output))
  }



  # # # # # # # # x_var_name and vr_var_name
  # 15) x_var_name is not NULL
  if(vr_var_name == x_var_name){
    text_output <- "Control pre test 023: Objects 'vr_var_name' and 'x_var_name' can not be equal."
    return(Hmisc::llist(dt_ok, text_output))
  }



  # New object
  vector_var_names <- c(vr_var_name, x_var_name)

  if(sum(vector_var_names %in% colnames(database)) != 2){
    text_output <- "Control pre test 024: Objects 'vr_var_name', and 'x_var_name' must be colnames from database."
    return(Hmisc::llist(dt_ok, text_output))
  }



  # # # # # # # # # # # minibase
  minibase <- na.omit(database[vector_var_names])
  colnames(minibase) <- c("VR", "X")



  # # # # # # minibase
  # 1) minibase can not be NULL
  if(is.null(minibase)){
    text_output <- "Control pre test 025: Object 'minibase' can not be a NULL."
    return(Hmisc::llist(dt_ok, text_output))
  }

  # 2) minibase must be a dataframe
  if(!is.data.frame(minibase)){
    text_output <- "Control pre test 026: Object 'minibase' must be a dataframe."
    return(Hmisc::llist(dt_ok, text_output))
  }

  # 3) minibase must has at exactly 2 columns
  if(!(ncol(minibase) == 2)){
    text_output <- "Control pre test 027: Object 'minibase' must has exactly 3 columns."
    return(Hmisc::llist(dt_ok, text_output))
  }


  # 4) minibase must has at least 2 rows
  if(!(nrow(minibase) >= 2)){
    text_output <- "Control pre test 028: Object 'database' must has al least 2 rows."
    return(Hmisc::llist(dt_ok, text_output))
  }


  # 4) minibase$VR can not be constant
  if(var(minibase$VR) == 0){
    text_output <- "Control pre test 029: Object 'minibase$VR' can not be constant."
    return(Hmisc::llist(dt_ok, text_output))
  }

  # 4) minibase$VR can not be constant
  if(length(unique(as.character(minibase$VR))) == 1){
    text_output <- "Control pre test 030: Object 'minibase$VR' can not be constant."
    return(Hmisc::llist(dt_ok, text_output))
  }


  # 4) minibase$VR can not be constant
  if(length(unique(as.character(minibase$VR))) == 1){
    text_output <- "Control pre test 031: Object 'minibase$VR' can not be constant."
    return(Hmisc::llist(dt_ok, text_output))
  }



  # Final!
  dt_ok <- TRUE
  text_output <- ""
  return(Hmisc::llist(dt_ok, text_output))



}



fn_cpiA020_code_p01_test <- function(database, vr_var_name, x_var_name, alpha_value){





  # # # # # Section 04 - Var rols and minibase -----------------------------------
  # # # Selected vars
  vector_all_var_names <- colnames(database)
  vector_name_selected_vars <- c(vr_var_name, x_var_name)

  num_pos_x <- 1:length(x_var_name)
  count_digits <- ceiling(log10(length(x_var_name)))
  if(count_digits < 2) count_digits <- 2
  vector_pos_x <- sprintf(paste0("%0", count_digits, "d"), num_pos_x)
  vector_new_name_x <- paste0("X", vector_pos_x)
  vector_rol_vars <- c("VR", vector_new_name_x)




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








  # # # database and minibase reps
  # Our 'n' is from minibase
  df_show_n <- data.frame(
    "object" = c("database", "minibase"),
    "n_col" = c(ncol(database), ncol(minibase)),
    "n_row" = c(nrow(database), nrow(minibase))
  )
  df_show_n




  # # # # # Section 06 - Anova Test ----------------------------------------------
  # # # Anova test
  lm_full <- lm(VR ~ ., data = minibase)
  summary_full <- summary(lm_full)
  df_table_reg <- as.data.frame(summary_full $coefficients) # Common anova table
  df_table_reg


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

  detail_role <- c(colnames(minibase), "residuals")
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

###################################################################
  confidence_value <- 1 - alpha_value
  ####################################
  # Normality

  list_normality_test <- sapply(vector_new_name_x, function(x){

    shapiro.test(minibase[,x])
  }, simplify = F)


  all_pars_matrix <- combn(vector_new_name_x, 2)


  list_homogeneity <- sapply(1:ncol(all_pars_matrix), function(x){

    var01 <- all_pars_matrix[1,x]
    var02 <- all_pars_matrix[2,x]
    selected_vars <- c(var01, var02)
    test_homogeneity <- bartlett.test(x = list(minibase[,var01], minibase[,var02]))

    output_list <- Hmisc::llist(selected_vars, test_homogeneity)
  }, simplify = F)

  list_cor_pearson <- sapply(1:ncol(all_pars_matrix), function(x){

    var01 <- all_pars_matrix[1,x]
    var02 <- all_pars_matrix[2,x]
    selected_vars <- c(var01, var02)
    test_cor_pearson <- cor.test(x = minibase[,var01],
                                 y = minibase[,var02],
                                 alternative = "two.sided",
                                 method = "pearson",
                                 conf.level = confidence_value,
                                 exact = FALSE,
                                 continuity = FALSE)

    output_list <- Hmisc::llist(selected_vars, test_cor_pearson)
  }, simplify = F)


  list_cor_spearman <- sapply(1:ncol(all_pars_matrix), function(x){

    var01 <- all_pars_matrix[1,x]
    var02 <- all_pars_matrix[2,x]
    selected_vars <- c(var01, var02)
    test_cor_spearman <- cor.test(x = minibase[,var01],
                                  y = minibase[,var02],
                                  alternative = "two.sided",
                                  method = "spearman",
                                  conf.level = confidence_value,
                                  exact = FALSE,
                                  continuity = FALSE)

    output_list <- Hmisc::llist(selected_vars, test_cor_spearman)
  }, simplify = F)


  df_normality <- data.frame(
    "orden" = 1:length(list_normality_test),
    "variables" = names(list_normality_test),
    "test" = rep("Normality - Shapiro-Wilk", length(list_normality_test)),
    "p.value" = unlist(purrr::map(list_normality_test, "p.value")),
    "alpha.value" = rep(alpha_value, length(list_normality_test))
  )
  df_normality$"h0_normality" <- df_normality$p.value < df_normality$alpha.value
  df_normality$"h0_normality" <- ifelse(test = df_normality$"h0_normality",
                                    yes = "Rejected H0",
                                     no = "No rejected H0")
  df_normality


  df_homogeneity <- data.frame(
    "orden" = 1:length(list_homogeneity),
    "var01" = unlist(lapply(list_homogeneity, function(x){x$selected_vars[1]})),
    "var02" = unlist(lapply(list_homogeneity, function(x){x$selected_vars[2]})),
    "test" = "Homogeneity Test",
    "p.value" = unlist(purrr::map(list_homogeneity, ~ .x$"test_homogeneity"$"p.value")),
    "alpha.value" = alpha_value
  )
  df_homogeneity$"h0_homogeneity" <- df_homogeneity$"p.value" < df_homogeneity$"alpha.value"
  df_homogeneity$"h0_homogeneity" <- ifelse(test = df_homogeneity$"h0_homogeneity",
                                   yes = "Rejected H0",
                                   no = "No rejected H0")
  df_homogeneity

  ########################################################
  df_cor_pearson <- data.frame(
    "orden" = 1:length(list_cor_pearson),
    "var01" = unlist(lapply(list_cor_pearson, function(x){x$selected_vars[1]})),
    "var02" = unlist(lapply(list_cor_pearson, function(x){x$selected_vars[2]})),
    "cor_test" = "Pearson",
    "cor_est" = unlist(purrr::map(list_cor_pearson, ~ .x$"test_cor_pearson"$"estimate")),
    "p.value" = unlist(purrr::map(list_cor_pearson, ~ .x$"test_cor_pearson"$"p.value")),
    "alpha.value" = alpha_value
  )
  df_cor_pearson$"h0_cor_pearson" <- df_cor_pearson$"p.value" < df_cor_pearson$"alpha.value"
  df_cor_pearson$"h0_cor_pearson" <- ifelse(test = df_cor_pearson$"h0_cor_pearson",
                                             yes = "Rejected H0",
                                              no = "No rejected H0")


  ########################################################
  df_cor_spearman <- data.frame(
    "orden" = 1:length(list_cor_spearman),
    "var01" = unlist(lapply(list_cor_spearman, function(x){x$selected_vars[1]})),
    "var02" = unlist(lapply(list_cor_spearman, function(x){x$selected_vars[2]})),
    "cor_test" = "Spearman",
    "cor_est" = unlist(purrr::map(list_cor_spearman, ~ .x$"test_cor_spearman"$"estimate")),
    "p.value" = unlist(purrr::map(list_cor_spearman, ~ .x$"test_cor_spearman"$"p.value")),
    "alpha.value" = alpha_value
  )
  df_cor_spearman$"h0_cor_spearman" <- df_cor_spearman$"p.value" < df_cor_spearman$"alpha.value"
  df_cor_spearman$"h0_cor_spearman" <- ifelse(test = df_cor_spearman$"h0_cor_spearman",
                                               yes = "Rejected H0",
                                                no = "No rejected H0")
  df_cor_spearman


  df_cor_mix <- data.frame(
    "orden" = df_homogeneity$"orden",
    "var01" = df_homogeneity$"var01",
    "var02" = df_homogeneity$"var02",
    "h0_normality_var01" = df_normality[df_homogeneity$"var01", "h0_normality"],
    "h0_normality_var02" = df_normality[df_homogeneity$"var02", "h0_normality"],
    "h0_homogeneity" = df_homogeneity$"h0_homogeneity"
  )
  selected_cols <- c("h0_normality_var01", "h0_normality_var02", "h0_homogeneity")
  #df_cor_mix$"check_req_pearson" <- rowSums(df_cor_mix[selected_cols]) == 3
  df_cor_mix$"check_req_pearson" <- apply(df_cor_mix[selected_cols], 1, function(x){

    vector_category <- unique(x)
    output_value <- FALSE
    if(length(vector_category) == 1) {
      vector_category == "No rejected H0"
    }

    output_value

  })
  df_cor_mix$"cor_test" <- ifelse(test = df_cor_mix$"check_req_pearson",
                                  yes = df_cor_pearson$"cor_test",
                                  no = df_cor_spearman$"cor_test")

  df_cor_mix$"cor_est" <- ifelse(test = df_cor_mix$"check_req_pearson",
                                 yes = df_cor_pearson$"cor_est",
                                 no = df_cor_spearman$"cor_est")

  df_cor_mix$"p.value" <- ifelse(test = df_cor_mix$"check_req_pearson",
                                 yes = df_cor_pearson$"p.value",
                                 no = df_cor_spearman$"p.value")

  df_cor_mix$"alpha.value" <- alpha_value

  df_cor_mix$"h0_cor_selected" <- ifelse(test = df_cor_mix$"check_req_pearson",
                                   yes = df_cor_pearson$"h0_cor_pearson",
                                   no = df_cor_spearman$"h0_cor_spearman")


  check_all_cor <- sum(df_cor_mix$"h0_cor_selected" == "No rejected H0") == length(df_cor_mix)

  phrase02_model_A <- "The selected model meets the requirement of no correlation between the regressor variables."
  phrase02_model_B <- "The selected model does NOT meet the requirement of no correlation between the regressor variables."
  phrase02_model_output <- ifelse(check_all_cor, phrase02_model_A, phrase02_model_B)


  df_cor_resumen <- df_cor_mix[c("orden", "var01", "var02", "cor_test", "cor_est", "p.value", "h0_cor_selected")]

  ################################################################

  df_matrix_cor_pearson <- reshape2::dcast(data = df_cor_pearson,
                                           formula = var02 ~ var01,
                                           value.var = "cor_est")

  df_matrix_cor_spearman <- reshape2::dcast(data = df_cor_spearman,
                                           formula = var02 ~ var01,
                                           value.var = "cor_est")
###################################################################
  # Tabla plot 001
  df_table_plot001 <- df_position


  # Tabla plot 002
  df_table_plot002 <- df_position



  df_table_plot003 <- df_position


  df_table_plot004 <- df_position




  # --- # hide_: Proccesing objects order
  hide_correct_order <- fn_cpiA020_ObjNamesInOrder(selected_fn = fn_cpiA020_code_p01_test)
  hide_output_list_objects <- mget(hide_correct_order)

  # --- # hide_: return!
  return(hide_output_list_objects)

}



fn_cpiA020_code_p02_plot001 <- function(results_p01_test){



  new_plot <-  with(results_p01_test,{


    # Crear el gráfico interactivo con Plotly

    # Crear el gráfico interactivo con Plotly

    plot001 <- plotly::plot_ly()

    # plot001 <- add_trace(p = plot001,
    #                      x = minibase$X,
    #                      y = minibase$VR,
    #                      type = 'scatter',
    #                      mode = 'markers',
    #                      name = "data",
    #                      marker = list(size = 15, color = 'blue'))
    #
    #
    # # Agregar la recta
    # selected_slop <- df_table_reg[2,1]# Pendiente
    # selected_constant <- df_table_reg[1,1]  # Ordenada al origen
    #
    # x_recta <- c(min(minibase$X), max(minibase$X))
    # y_recta <- selected_slop * x_recta + selected_constant
    # plot001 <- add_trace(p = plot001,
    #                      x = x_recta, y = y_recta,
    #                      type = 'scatter',
    #                      mode = 'lines',
    #                      name = 'slop',
    #                      line = list(width = 5, color = 'orange'))
    #
    #
    # plot001 <- plotly::layout(p = plot001,
    #                           xaxis = list(title = "X"),
    #                           yaxis = list(title = "VR"),
    #                           title = "Plot 001 - Scatterplot XY",
    #                           font = list(size = 20),
    #                           margin = list(t = 100))
    #
    #
    #
    # plot001 <- plotly::layout(p = plot001,
    #                           xaxis = list(zeroline = FALSE),
    #                           yaxis = list(zeroline = FALSE))

    # Mostrar el gráfico interactivo
    plot001


  })



  return(new_plot)

}



fn_cpiA020_code_p02_plot002 <- function(results_p01_test){



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
                              title = "Plot 001 - Residuals vs. Fitted values",
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




fn_cpiA020_code_p02_plot003 <- function(results_p01_test){



  new_plot <-  with(results_p01_test,{


    # Crear el gráfico interactivo con Plotly

    # Crear el gráfico interactivo con Plotly

    plot001 <- plotly::plot_ly()
    #
    # plot001 <- add_trace(p = plot001,
    #                      x = minibase$X,
    #                      y = minibase$VR,
    #                      type = 'scatter',
    #                      mode = 'markers',
    #                      name = "data",
    #                      marker = list(size = 15, color = 'blue'))
    #
    #
    # # Agregar la recta
    # selected_slop <- df_table_reg[2,1]# Pendiente
    # selected_constant <- df_table_reg[1,1]  # Ordenada al origen
    #
    # x_recta <- c(min(minibase$X), max(minibase$X))
    # y_recta <- selected_slop * x_recta + selected_constant
    # plot001 <- add_trace(p = plot001,
    #                      x = x_recta, y = y_recta,
    #                      type = 'scatter',
    #                      mode = 'lines',
    #                      name = 'slop',
    #                      line = list(width = 5, color = 'orange'))
    #
    #
    # plot001 <- plotly::layout(p = plot001,
    #                           xaxis = list(title = "BLOCK"),
    #                           yaxis = list(title = "VR"),
    #                           title = "Plot 001 - Interacción Factor-Bloque",
    #                           font = list(size = 20),
    #                           margin = list(t = 100))
    #
    #
    #
    # plot001 <- plotly::layout(p = plot001,
    #                           xaxis = list(zeroline = FALSE),
    #                           yaxis = list(zeroline = FALSE))

    # Mostrar el gráfico interactivo
    plot001


  })



  return(new_plot)

}




fn_cpiA020_code_p02_plot004 <- function(results_p01_test){



  new_plot <-  with(results_p01_test,{


    # Crear el gráfico interactivo con Plotly

    # Crear el gráfico interactivo con Plotly

    plot001 <- plotly::plot_ly()

    # plot001 <- add_trace(p = plot001,
    #                      x = minibase$X,
    #                      y = minibase$VR,
    #                      type = 'scatter',
    #                      mode = 'markers',
    #                      name = "data",
    #                      marker = list(size = 15, color = 'blue'))
    #
    #
    # # Agregar la recta
    # selected_slop <- df_table_reg[2,1]# Pendiente
    # selected_constant <- df_table_reg[1,1]  # Ordenada al origen
    #
    # x_recta <- c(min(minibase$X), max(minibase$X))
    # y_recta <- selected_slop * x_recta + selected_constant
    # plot001 <- add_trace(p = plot001,
    #                      x = x_recta, y = y_recta,
    #                      type = 'scatter',
    #                      mode = 'lines',
    #                      name = 'slop',
    #                      line = list(width = 5, color = 'orange'))
    #
    #
    # plot001 <- plotly::layout(p = plot001,
    #                           xaxis = list(title = "BLOCK"),
    #                           yaxis = list(title = "VR"),
    #                           title = "Plot 001 - Interacción Factor-Bloque",
    #                           font = list(size = 20),
    #                           margin = list(t = 100))
    #
    #
    #
    # plot001 <- plotly::layout(p = plot001,
    #                           xaxis = list(zeroline = FALSE),
    #                           yaxis = list(zeroline = FALSE))

    # Mostrar el gráfico interactivo
    plot001


  })



  return(new_plot)

}


# Control post
fn_cpiA020_control_p01_test <- function(all_results){

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


fn_cpiA020_control_p02_plots <- function(all_results){

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





fn_cpiA020_gen01 <- function(database,  vr_var_name, x_var_name, alpha_value){

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
  output_list$"R_code"$"p01_test"    <- fn_cpiA020_TakeCode(selected_fn = fn_cpiA020_code_p01_test)
  output_list$"R_code"$"p02_plots"$"plot001"   <- fn_cpiA020_TakeCode(selected_fn = fn_cpiA020_code_p02_plot001)
  output_list$"R_code"$"p02_plots"$"plot002"   <- fn_cpiA020_TakeCode(selected_fn = fn_cpiA020_code_p02_plot002)
  output_list$"R_code"$"p02_plots"$"plot003"   <- fn_cpiA020_TakeCode(selected_fn = fn_cpiA020_code_p02_plot003)
  output_list$"R_code"$"p02_plots"$"plot004"   <- fn_cpiA020_TakeCode(selected_fn = fn_cpiA020_code_p02_plot004)


  # Step 02 - Pre Control ------------------------------------------------------
  output_list$"check_control"$"previous" <- fn_cpiA020_control_previous(database, vr_var_name,
                                                                        x_var_name,
                                                                        alpha_value)





  # Step 03 - Results ----------------------------------------------------------
  output_list$"R_results"$"p01_test" <- fn_cpiA020_code_p01_test(database,  vr_var_name, x_var_name, alpha_value)


  output_list$"R_results"$"p02_plots"$"plot001" <- fn_cpiA020_code_p02_plot001(results_p01_test = output_list$"R_results"$"p01_test")
  output_list$"R_results"$"p02_plots"$"plot002" <- fn_cpiA020_code_p02_plot002(results_p01_test = output_list$"R_results"$"p01_test")
  output_list$"R_results"$"p02_plots"$"plot003" <- fn_cpiA020_code_p02_plot003(results_p01_test = output_list$"R_results"$"p01_test")
  output_list$"R_results"$"p02_plots"$"plot004" <- fn_cpiA020_code_p02_plot004(results_p01_test = output_list$"R_results"$"p01_test")





  return(output_list)

}


fn_cpiA020_gen02 <- function(database,  vr_var_name, x_var_name, alpha_value){


  all_results <- fn_cpiA020_gen01(database,  vr_var_name, x_var_name, alpha_value)



  output_list <- list()


  # Out01 - Analysis -------------------------------------------------------------
  selection01 <- c("df_selected_vars",
                   "df_table_reg",
                   "df_table_det_coef",
                   "df_position",
                   "df_dispersion")

  output_list$"out01_analysis" <- all_results$R_results$p01_test[selection01]




  # Out02 - Requeriments ---------------------------------------------------------
  selection02 <- c("test_residuals_normality")

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
