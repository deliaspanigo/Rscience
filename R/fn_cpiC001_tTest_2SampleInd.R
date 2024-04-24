

# # # # Special Functions
# Take the original code from a function
fn_cpiC001_tTest_2SampleInd_TakeCode <- function(selected_fn){


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
fn_cpiC001_tTest_2SampleInd_ObjNamesInOrder <- function(selected_fn){

  selected_code <- deparse(body(selected_fn))
  selected_code <- grep("<-", selected_code, value = TRUE)
  selected_code <- trimws(selected_code)
  selected_code <- gsub("\\s", "", selected_code)
  selected_code <- sub("<-.*", "", selected_code)
  selected_code <- grep("^[a-zA-Z0-9._]*$", selected_code, value = TRUE)
  selected_code <- grep("^hide", selected_code, value = TRUE, invert = TRUE)
  selected_code <- unique(selected_code)

  return(selected_code)

}


# # # # Control funcitons
# Control previous
fn_cpiC001_tTest_2SampleInd_control_previous <- function(database, vr_var_name, factor_var_name, alpha_value){

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


  # 4) Database must has at least 4 rows
  if(!(nrow(database) >= 4)){
    text_output <- "Control pre test 004: Object 'database' must has al least 4 rows."
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

  # 15) factor_var_name is a colname from database
  if(!(vr_var_name %in% colnames(database))){
    text_output <- "Control pre test 010: Object 'vr_var_name' must be a colname from database."
    return(Hmisc::llist(dt_ok, text_output))
  }


  # # # # # # # # factor_var_name
  # 10) factor_var_name is not NULL
  if(is.null(factor_var_name)){
    text_output <- "Control pre test 011: Object 'factor_var_name' can not be NULL."
    return(Hmisc::llist(dt_ok, text_output))
  }

  # 11) factor_var_name is a vector
  if(!is.vector(factor_var_name)){
    text_output <- "Control pre test 012: Object 'factor_var_name' must be vector."
    return(Hmisc::llist(dt_ok, text_output))
  }

  # 12) factor_var_name is a vector
  if(!(length(factor_var_name) == 1)){
    text_output <- "Control pre test 013: Object 'factor_var_name' must be vector of length 1."
    return(Hmisc::llist(dt_ok, text_output))
  }

  # 13) factor_var_name is not NA
  if(is.na(factor_var_name)){
    text_output <- "Control pre test 014: Object 'factor_var_name' can not be NA."
    return(Hmisc::llist(dt_ok, text_output))
  }

  # 14) factor_var_name is character
  if(!is.character(factor_var_name)){
    text_output <- "Control pre test 015: Object 'factor_var_name' must be character."
    return(Hmisc::llist(dt_ok, text_output))
  }


  # 15) factor_var_name is a colname from database
  if(!(factor_var_name %in% colnames(database))){
    text_output <- "Control pre test 016: Object 'factor_var_name' must be a colname from database."
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



  # # # # # # # # factor_var_name and vr_var_name
  # 15) factor_var_name is not NULL
  if(vr_var_name == factor_var_name){
    text_output <- "Control pre test 023: Objects 'vr_var_name' and 'factor_var_name' can not be equal."
    return(Hmisc::llist(dt_ok, text_output))
  }


  # New object
  vector_var_names <- c(vr_var_name, factor_var_name)

  if(sum(vector_var_names %in% colnames(database)) != 2){
    text_output <- "Control pre test 024: Objects 'vr_var_name' and 'factor_var_name' must be colnames from database."
    return(Hmisc::llist(dt_ok, text_output))
  }



  # # # # # # # # # # # minibase
  minibase <- na.omit(database[vector_var_names])
  minibase[,2] <- as.factor(minibase[,2])
  colnames(minibase) <- c("VR", "FACTOR")



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
    text_output <- "Control pre test 027: Object 'minibase' must has exactly 2 columns."
    return(Hmisc::llist(dt_ok, text_output))
  }


  # 4) minibase must has at least 4 rows
  if(!(nrow(minibase) >= 4)){
    text_output <- "Control pre test 028: Object 'database' must has al least 4 rows."
    return(Hmisc::llist(dt_ok, text_output))
  }

  # 4) minibase$VR can not be constant
  if(!is.numeric(minibase$VR)){
    text_output <- "Control pre test 029: Object 'minibase$VR' must be numeric."
    return(Hmisc::llist(dt_ok, text_output))
  }
  # 4) minibase$VR can not be constant
  if(var(minibase$VR) == 0){
    text_output <- "Control pre test 030: Object 'minibase$VR' can not be constant."
    return(Hmisc::llist(dt_ok, text_output))
  }

  # 4) minibase$VR can not be constant
  if(length(unique(as.character(minibase$VR))) == 1){
    text_output <- "Control pre test 031: Object 'minibase$VR' can not be constant."
    return(Hmisc::llist(dt_ok, text_output))
  }


  # 4) minibase$VR can not be constant
  if(length(unique(as.character(minibase$VR))) == 1){
    text_output <- "Control pre test 032: Object 'minibase$VR' can not be constant."
    return(Hmisc::llist(dt_ok, text_output))
  }


  # Al least 2 levels in minibase$FACTOR
  if(!(nlevels(minibase$FACTOR) == 2)){
    text_output <- "Control pre test 033: for t Test FACTOR must has exactly 2 levels."
    return(Hmisc::llist(dt_ok, text_output))
  }



  # Al least 2 reps in each level
  reps_level <- tapply(minibase$VR, minibase$FACTOR, length)
  dt_reps_level <- reps_level >= 2
  check_01 <- sum(dt_reps_level) == length(dt_reps_level)
  if(!check_01){
    text_output <- "Control pre test 034: On minibase from FACTOR each level must has al least 2 reps."
    return(Hmisc::llist(dt_ok, text_output))
  }


  # var greater than zero from each level
  vars_level <- tapply(minibase$VR, minibase$FACTOR, var)
  dt_vars_level <- reps_level > 0
  check_02 <- sum(dt_vars_level) ==  length(dt_vars_level)
  if(!check_02){
    text_output <- "Control pre test 035: On minibase from FACTOR each level can not be constant.
On each level variance must be greater than zero."
    return(Hmisc::llist(dt_ok, text_output))
  }


  # At least 2 differents values from each level
  ndata_level <- tapply(minibase$VR, minibase$FACTOR, function(x){
    length(unique(as.character(x)))
  })
  dt_ndata_level <- ndata_level >= 2
  check_03 <- sum(dt_ndata_level) == length(dt_ndata_level)
  if(!check_03){
    text_output <- "Control pre test 036: On minibase from FACTOR each level must not be constant."
    return(Hmisc::llist(dt_ok, text_output))
  }


  # Final!
  dt_ok <- TRUE
  text_output <- ""
  return(Hmisc::llist(dt_ok, text_output))



}




# Control post
fn_cpiC001_tTest_2SampleInd_control_post <- function(list_results_from_fn_cpiC001_tTest_2SampleInd){

  # if(is.null(list_results_from_fn_cpiC001_tTest_2SampleInd)){
  #   text_output <- "Control post test 001: Object 'list_results_from_fn_cpiC001_tTest_2SampleInd' can not be NULL."
  #   check_ok <- FALSE
  #   return(Hmisc::llist(check_ok, text_output))
  # }
  #
  #
  #
  # obj_name01 <- "df_table_anova"
  # spected_col_names <- c("Df", "Sum Sq", "Mean Sq", "F value", "Pr(>F)")
  #
  #
  # if(!(obj_name01 %in% names(list_results_from_fn_cpiC001_tTest_2SampleInd))){
  #   text_output <- "Control post test 002: Object 'df_table_anova' doesn't exist in 'list_results_from_fn_cpiC001_tTest_2SampleInd'."
  #   check_ok <- FALSE
  #   return(Hmisc::llist(check_ok, text_output))
  # }
  #
  #
  # # # # 1) About the table
  # if(is.null(list_results_from_fn_cpiC001_tTest_2SampleInd[obj_name01])){
  #   text_output <- "Control post test 003: Object '_obj_name01_' can not be NULL."
  #   text_output <- gsub("_obj_name01_", "obj_name01", text_output)
  #   check_ok <- FALSE
  #   return(Hmisc::llist(check_ok, text_output))
  # }
  #
  #
  # selected_obj01 <- list_results_from_fn_cpiC001_tTest_2SampleInd[[obj_name01]]
  #
  #
  # if(!identical(spected_col_names, colnames(selected_obj01))){
  #   text_output <- "Control post test 004: Object '_obj_name01_' has unexpected column names."
  #   text_output <- gsub("_obj_name01_", "obj_name01", text_output)
  #   check_ok <- FALSE
  #   return(Hmisc::llist(check_ok, text_output))
  # }
  #
  #
  # if(!is.data.frame(selected_obj01)){
  #   text_output <- "Control post test 005: Object '_obj_name01_' must be a data.frame."
  #   text_output <- gsub("_obj_name01_", "obj_name01", text_output)
  #   check_ok <- FALSE
  #   return(Hmisc::llist(check_ok, text_output))
  # }
  #
  #
  # if(nrow(selected_obj01) != 2){
  #   text_output <- "Control post test 006: Object '_obj_name01_' must has 2 rows."
  #   text_output <- gsub("_obj_name01_", "obj_name01", text_output)
  #   check_ok <- FALSE
  #   return(Hmisc::llist(check_ok, text_output))
  # }
  #
  #
  # if(ncol(selected_obj01) != 5){
  #   text_output <- "Control post test 007: Object '_obj_name01_' must has 5 cols."
  #   text_output <- gsub("_obj_name01_", "obj_name01", text_output)
  #   check_ok <- FALSE
  #   return(Hmisc::llist(check_ok, text_output))
  # }
  #
  #
  #
  #
  # # # # 2) About Degree of Fredom
  # vector_df <- selected_obj01$Df
  # if(!is.vector(vector_df)){
  #   text_output <- "Control post test 008: internal object 'vector_df' must be vector."
  #   check_ok <- FALSE
  #   return(Hmisc::llist(check_ok, text_output))
  # }
  #
  #
  # if(!is.numeric(vector_df)){
  #   text_output <- "Control post test 009: Column 'Df' must contain only numbers."
  #   check_ok <- FALSE
  #   return(Hmisc::llist(check_ok, text_output))
  # }
  #
  #
  # if(sum(is.na(vector_df)) != 0){
  #   text_output <- "Control post test 010: Column 'Df' cannot contain NA values."
  #   check_ok <- FALSE
  #   return(Hmisc::llist(check_ok, text_output))
  # }
  #
  #
  # if(sum(vector_df == 0) != 0){
  #   text_output <- "Control post test 011: Column 'Df' cannot contain 0 as value."
  #   check_ok <- FALSE
  #   return(Hmisc::llist(check_ok, text_output))
  # }
  #
  #
  #
  #
  #
  # # # # 3) Sum Sq
  # vector_sum_sq <- selected_obj01$`Sum Sq`
  #
  # if(!is.vector(vector_sum_sq)){
  #   text_output <- "Control post test 012: internal object 'vector_sum_sq' must be vector."
  #   check_ok <- FALSE
  #   return(Hmisc::llist(check_ok, text_output))
  # }
  #
  #
  # if(!is.numeric(vector_sum_sq)){
  #   text_output <- "Control post test 013: Column 'Sum Sq' must contain only numbers."
  #   check_ok <- FALSE
  #   return(Hmisc::llist(check_ok, text_output))
  # }
  #
  #
  # if(sum(is.na(vector_sum_sq)) != 0){
  #   text_output <- "Control post test 014: Column 'Sum Sq' cannot contain NA values."
  #   check_ok <- FALSE
  #   return(Hmisc::llist(check_ok, text_output))
  # }
  #
  #
  # if(sum(vector_sum_sq == 0) != 0){
  #   text_output <- "Control post test 015: Column 'Sum Sq' cannot contain 0 as value."
  #   check_ok <- FALSE
  #   return(Hmisc::llist(check_ok, text_output))
  # }
  #
  #
  #
  # # # # 4) Mean Sq
  # vector_mean_sq <- selected_obj01$`Mean Sq`
  #
  # if(!is.vector(vector_mean_sq)){
  #   text_output <- "Control post test 016: internal object 'vector_mean_sq' must be vector."
  #   check_ok <- FALSE
  #   return(Hmisc::llist(check_ok, text_output))
  # }
  #
  # if(!is.numeric(vector_mean_sq)){
  #   text_output <- "Control post test 017: Column 'Mean Sq' must contain only numbers."
  #   check_ok <- FALSE
  #   return(Hmisc::llist(check_ok, text_output))
  # }
  #
  # if(sum(is.na(vector_mean_sq)) != 0){
  #   text_output <- "Control post test 018: Column 'Mean Sq' cannot contain NA values."
  #   check_ok <- FALSE
  #   return(Hmisc::llist(check_ok, text_output))
  # }
  #
  #
  # if(sum(vector_mean_sq == 0) != 0){
  #   text_output <- "Control post test 019: Column 'Mean Sq' cannot contain 0 as value."
  #   check_ok <- FALSE
  #   return(Hmisc::llist(check_ok, text_output))
  # }
  #
  #
  #
  # # # # 4) F value
  # vector_f_value <- selected_obj01$`F value`
  #
  # if(!is.vector(vector_f_value)){
  #   text_output <- "Control post test 020: internal object 'vector_f_value' must has vector."
  #   check_ok <- FALSE
  #   return(Hmisc::llist(check_ok, text_output))
  # }
  #
  # if(!is.numeric(vector_f_value)){
  #   text_output <- "Control post test 021: Column 'F value' must be numeric."
  #   check_ok <- FALSE
  #   return(Hmisc::llist(check_ok, text_output))
  # }
  #
  # if(sum(!is.na(vector_f_value)) != 1){
  #   text_output <- "Control post test 022: Column 'F value' must contain only 1 pvalue."
  #   check_ok <- FALSE
  #   return(Hmisc::llist(check_ok, text_output))
  # }
  #
  # if(is.na(vector_f_value[1])){
  #   text_output <- "Control post test 023: Column 'F value', for FACTOR cannot be NA."
  #   check_ok <- FALSE
  #   return(Hmisc::llist(check_ok, text_output))
  # }
  #
  # if(!(vector_f_value[1] > 0)){
  #   text_output <- "Control post test 024: Column 'F value', for FACTOR f value must be a number greater than zero."
  #   check_ok <- FALSE
  #   return(Hmisc::llist(check_ok, text_output))
  # }
  #
  #
  #
  # if(!is.na(vector_f_value[2])){
  #   text_output <- "Control post test 025: Column 'F value', for Residuals f value must be NA."
  #   check_ok <- FALSE
  #   return(Hmisc::llist(check_ok, text_output))
  # }
  #
  #
  #
  # # # # 5) p values
  # vector_p_value <- selected_obj01$`Pr(>F)`
  #
  # if(!is.vector(vector_p_value)){
  #   text_output <- "Control post test 026: internal object 'vector_p_value' must has vector."
  #   check_ok <- FALSE
  #   return(Hmisc::llist(check_ok, text_output))
  # }
  #
  # if(!is.numeric(vector_p_value)){
  #   text_output <- "Control post test 027: Column 'Pr(>F)' must be numeric."
  #   check_ok <- FALSE
  #   return(Hmisc::llist(check_ok, text_output))
  # }
  #
  # if(sum(!is.na(vector_p_value)) != 1){
  #   text_output <- "Control post test 028: Column 'Pr(>F)' must contain only 1 pvalue."
  #   text_output <- gsub("_obj_name01_", "obj_name01", text_output)
  #   check_ok <- FALSE
  #   return(Hmisc::llist(check_ok, text_output))
  # }
  #
  # if(is.na(vector_p_value[1])){
  #   text_output <- "Control post test 029: Column 'Pr(>F)', for FACTOR cannot be NA."
  #   check_ok <- FALSE
  #   return(Hmisc::llist(check_ok, text_output))
  # }
  #
  # if(!(vector_p_value[1] >= 0 && vector_p_value[1] <= 1)){
  #   text_output <- "Control post test 030: Column 'Pr(>F)', for FACTOR pvalue must be a number between 0 and 1."
  #   check_ok <- FALSE
  #   return(Hmisc::llist(check_ok, text_output))
  # }
  #
  #
  #
  # if(!is.na(vector_p_value[2])){
  #   text_output <- "Control post test 031: Column 'Pr(>F)', for Residuals pvalue must be NA."
  #   text_output <- gsub("_obj_name01_", "obj_name01", text_output)
  #   check_ok <- FALSE
  #   return(Hmisc::llist(check_ok, text_output))
  # }


  # All OK!
  check_ok <- TRUE
  text_output <- ""
  return(Hmisc::llist(check_ok, text_output))

}


# # # # Actions for sections
# # For sections 01 to 03 there are no action in R. The actions its on Shiny.
# # We need the input objects from shiny to.

fn_cpiC001_tTest_2SampleInd_results <- function(database, vr_var_name, factor_var_name, alpha_value){

  # # # # # Section 04 - Var rols and minibase -----------------------------------
  # # # Selected vars


  vector_all_var_names <- colnames(database)
  vector_name_selected_vars <- c(vr_var_name, factor_var_name)
  vector_rol_vars <- c("VR", "FACTOR")



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
  minibase[,2] <- as.factor(minibase[,2])



  # # # Anova control
  # 'VR' must be numeric and 'FACTOR must be factor.
  df_control_minibase <- data.frame(
    "order" = 1:nrow(df_selected_vars),
    "var_name" = df_selected_vars$var_name,
    "var_role" = df_selected_vars$var_role,
    "control" = c("is.numeric()", "is.factor()"),
    "verify" = c(is.numeric(minibase[,1]), is.factor(minibase[,2]))
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



  # Normality
  list_normality_test <- tapply(minibase$VR, minibase$FACTOR, shapiro.test)
  df_normality <- data.frame(
    "orden" = 1:length(list_normality_test),
    "level" = names(list_normality_test),
    "test" = "Normality",
    "p.value" = sapply(list_normality_test, function(x){x$p.value}),
    "alpha.value" = alpha_value
  )
  df_normality$"h0_normality" <- df_normality$"p.value" < df_normality$"alpha.value"
  df_normality$"h0_normality" <- ifelse(test = df_normality$"h0_normality",
                                        yes = "Rejected H0",
                                        no = "No rejected H0")

  check_normality <- sum(df_normality$"h0_normality" == "No rejected H0") == 2

  phrase01_A <- "Normality on both group verifed."
  phrase01_B <- "At least one group is has NOT normality."
  phrase01_output <- ifelse(test = check_normality,
                            yes = phrase01_A,
                            no = phrase01_B)

  phrase02_A <- "It is valid to perform the t test."
  phrase02_B <- "It is NOT valid to perform the t test."
  phrase02_output <- ifelse(test = check_normality,
                            yes = phrase02_A,
                             no = phrase02_B)

  homogeneity_test <- bartlett.test(VR ~ FACTOR, data = minibase)

  check_homogeneity <- homogeneity_test$p.value >= alpha_value

  df_homogeneity <- data.frame(
    "orden" = 1,
    "test" = "Homogeneity",
    "p.value" = homogeneity_test$"p.value",
    "alpha.value" = alpha_value
  )
  df_homogeneity$"h0_homogeneity" <- df_homogeneity$"p.value" < df_homogeneity$"alpha.value"
  df_homogeneity$"h0_homogeneity" <- ifelse(test = df_homogeneity$"h0_homogeneity",
                                             yes = "Rejected H0",
                                              no = "No rejected H0")


  phrase03_A <- "Groups are homogenious. Classic t Test will be apply."
  phrase03_B <- "Groups are NOT homogenious. t Test with Welch approximation."
  phrase03_output <- ifelse(test = check_homogeneity,
                             yes = phrase03_A,
                              no = phrase03_B)

  phrase03_output <- ifelse(test = check_normality,
                            yes = phrase03_output,
                             no = phrase02_B)
  # phrase02_A <- "Performed: Classic t Test."
  # phrase02_B <- "Performed: t Test with Welch approximation."
  # phrase02_output <- ifelse(test = check_homogeneity,
  #                           yes = phrase01_A,
  #                           no = phrase01_B)

  confidence_value <- 1 - alpha_value


  results_t_test <- t.test(formula = VR ~ FACTOR, data = minibase,
                           alternative = "two.sided",
                           conf.level = confidence_value,
                           var.equal = check_homogeneity)

  results_t_test


  df_t_test_2sample_ind <- data.frame(
    "orden" = 1,
    "test" = "t test",
    #"details" = "2 independent sample",
    "p.value" = results_t_test$"p.value",
    "alpha.value" = alpha_value
  )
  df_t_test_2sample_ind$"h0_t_test" <- df_t_test_2sample_ind$"p.value" < df_t_test_2sample_ind$"alpha.value"
  df_t_test_2sample_ind$"h0_t_test" <- ifelse(test = df_t_test_2sample_ind$"h0_t_test",
                                            yes = "Rejected H0",
                                            no = "No rejected H0")

  # # # Detect rows on database there are on minibase
  dt_rows_database_ok <- rowSums(!is.na(database[vector_name_selected_vars])) == ncol(minibase)

  minibase_mod <- minibase
  minibase_mod$"lvl_order_number" <- as.numeric(minibase_mod[,2])
  minibase_mod$"lvl_color" <- df_factor_info$color[minibase_mod$"lvl_order_number"]
  minibase_mod$"id_database" <- c(1:nrow(database))[dt_rows_database_ok]
  minibase_mod$"id_minibase" <- 1:nrow(minibase)



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
    "n" = tapply(minibase[,1], minibase[,2], length),
    "color" = df_factor_info$"color"
  )



  # # # Partitioned Measures of Dispersion (VR)
  df_vr_dispersion_levels <- data.frame(
    "order" = 1:nlevels(minibase[,2]),
    "level" = levels(minibase[,2]),
    "range" = tapply(minibase[,1], minibase[,2], function(x){max(x) - min(x)}),
    "variance" = tapply(minibase[,1], minibase[,2], var),
    "standard_deviation" = tapply(minibase[,1], minibase[,2], sd),
    "standard_error" = tapply(minibase[,1], minibase[,2], function(x){sd(x)/sqrt(length(x))}),
    "n" = tapply(minibase[,1], minibase[,2], length),
    "color" = df_factor_info$"color"
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

  df_table_plot001 <- data.frame(
    "orden" = df_vr_position_levels$"order",
    "level" = df_vr_position_levels$"level",
    "min" = df_vr_position_levels$"min",
    "max" = df_vr_position_levels$"max",
    "mean" = df_vr_position_levels$"mean",
    "standard_deviation" = df_vr_dispersion_levels$"standard_deviation",
    "standard_error" = df_vr_dispersion_levels$"standard_error",
    "color" = df_factor_info$"color"
  )

  df_table_plot002 <- data.frame(
    "orden" = df_vr_position_levels$"order",
    "level" = df_vr_position_levels$"level",
    "mean" = df_vr_position_levels$"mean",
    "standard_deviation" = df_vr_dispersion_levels$"standard_deviation",
    "standard_error" = df_vr_dispersion_levels$"standard_error",
    "color" = df_factor_info$"color"
  )

  df_table_plot003 <- data.frame(
    "orden" = df_vr_position_levels$"order",
    "level" = df_vr_position_levels$"level",
    "mean" = df_vr_position_levels$"mean",
    "standard_deviation" = df_vr_dispersion_levels$"standard_deviation",
    "standard_error" = df_vr_dispersion_levels$"standard_error",
    "color" = df_factor_info$"color"
  )


  df_table_plot004 <-  df_vr_position_levels
  # --- # hide_: Proccesing objects order
  hide_correct_order <- fn_cpiC001_tTest_2SampleInd_ObjNamesInOrder(selected_fn = fn_cpiC001_tTest_2SampleInd_results)
  hide_output_list_objects <- mget(hide_correct_order)

  # --- # hide_: return!
  return(hide_output_list_objects)


}



# # # # Code
# # To put all the code together, we need the items that Shiny provides.
fn_cpiC001_tTest_2SampleInd_code_section01_Libreries <- function(){

  #--- Librerias
  section01_general_libreries <- '
  # # # # # Section 01 - Libraries -----------------------------------------------
  library("agricolae") # Tukey test
  library("dplyr")     # Developing with %>%
  library("openxlsx")  # Import files from xlsx
  library("plotly")    # Advanced graphical functions
'

  return(section01_general_libreries)

}


fn_cpiC001_tTest_2SampleInd_code_section02_FileSource <- function(intro_source_database){


  file_source <- intro_source_database$file_source

  #--- Import database from excel
  section02_database_s01_import_excel_files <- '
  # # # # # Section 02 - Import database from excel file -------------------------
  selected_xlsx_file <- "_selected_xlsx_file_"
  selected_sheet <- _selected_sheet_
  database <- openxlsx::read.xlsx(xlsxFile = selected_xlsx_file, sheet = selected_sheet)
  database
  '


  #--- Import database from R example
  section02_database_s02_R_example <- '
  # # # # # Section 02 - R example as database------------------------------------
  database <- _selected_R_database_
  database
  '

  ##############################################################################
  section02_SELECTED <- ""
  if(file_source == "xlsx"){
    section02_SELECTED <- section02_database_s01_import_excel_files

    section02_SELECTED <- gsub(pattern = "_selected_xlsx_file_" ,
                               replacement =  intro_source_database$file_name,
                               x = section02_SELECTED)

    section02_SELECTED <- gsub(pattern = "_selected_sheet_" ,
                               replacement =  intro_source_database$selected_sheet,
                               x = section02_SELECTED)

  }

  if(file_source == "R_example"){

    section02_SELECTED <- section02_database_s02_R_example

    section02_SELECTED <- gsub(pattern = "_selected_R_database_" ,
                               replacement =  intro_source_database$file_name,
                               x = section02_SELECTED)

  }


  return(section02_SELECTED)
}


fn_cpiC001_tTest_2SampleInd_code_section03_VarSelection <- function(vr_var_name, factor_var_name, alpha_value){


  #--- Var selection
  section03_varselection <- '
  # # # # # Section 03 - Var selection, alpha value and confidence value -------------------------
  vr_var_name <- "_selected_vr_var_name_"
  factor_var_name <- "_selected_factor_var_name_"
  alpha_value <- _selected_alpha_value_
  confidence_value <- 1 - alpha_value
  '

  section03_varselection <- gsub(pattern = "_selected_vr_var_name_" ,
                                 replacement =  vr_var_name,
                                 x = section03_varselection)

  section03_varselection <- gsub(pattern = "_selected_factor_var_name_" ,
                                 replacement =  factor_var_name,
                                 x = section03_varselection)

  section03_varselection <- gsub(pattern = "_selected_alpha_value_" ,
                                 replacement =  alpha_value,
                                 x = section03_varselection)

  return(section03_varselection)

}


fn_cpiC001_tTest_2SampleInd_code_section04_UntilTheEnd <- function(){


  the_code <- fn_cpiC001_tTest_2SampleInd_TakeCode(selected_fn = fn_cpiC001_tTest_2SampleInd_results)

  return(the_code)
}


fn_cpiC001_tTest_2SampleInd_code_sectionXX_g01_Plots <- function(){



  list_code <- list()

  list_code[[1]] <- fn_cpiC001_tTest_2SampleInd_TakeCode(selected_fn = fn_cpiC001_tTest_2SampleInd_factor_plot001)
  list_code[[2]] <- fn_cpiC001_tTest_2SampleInd_TakeCode(selected_fn = fn_cpiC001_tTest_2SampleInd_factor_plot002)
  list_code[[3]] <- fn_cpiC001_tTest_2SampleInd_TakeCode(selected_fn = fn_cpiC001_tTest_2SampleInd_factor_plot003)
  list_code[[4]] <- fn_cpiC001_tTest_2SampleInd_TakeCode(selected_fn = fn_cpiC001_tTest_2SampleInd_factor_plot004)
  list_code[[5]] <- fn_cpiC001_tTest_2SampleInd_TakeCode(selected_fn = fn_cpiC001_tTest_2SampleInd_factor_plot005)
  list_code[[6]] <- fn_cpiC001_tTest_2SampleInd_TakeCode(selected_fn = fn_cpiC001_tTest_2SampleInd_factor_plot006)
  list_code[[7]] <- fn_cpiC001_tTest_2SampleInd_TakeCode(selected_fn = fn_cpiC001_tTest_2SampleInd_factor_plot007)

  vector_code <- unlist(list_code)
  all_code <- paste0(vector_code, collapse = "\n\n\n\n")

  return(all_code)

}



fn_cpiC001_tTest_2SampleInd_code_sectionXX_g02_Plots <- function(){


  list_code <- list()

  list_code[[1]]  <- fn_cpiC001_tTest_2SampleInd_TakeCode(selected_fn = fn_cpiC001_tTest_2SampleInd_residuals_plot001)
  list_code[[2]]  <- fn_cpiC001_tTest_2SampleInd_TakeCode(selected_fn = fn_cpiC001_tTest_2SampleInd_residuals_plot002)
  list_code[[3]]  <- fn_cpiC001_tTest_2SampleInd_TakeCode(selected_fn = fn_cpiC001_tTest_2SampleInd_residuals_plot003)
  list_code[[4]]  <- fn_cpiC001_tTest_2SampleInd_TakeCode(selected_fn = fn_cpiC001_tTest_2SampleInd_residuals_plot004)
  list_code[[5]]  <- fn_cpiC001_tTest_2SampleInd_TakeCode(selected_fn = fn_cpiC001_tTest_2SampleInd_residuals_plot005)
  list_code[[6]]  <- fn_cpiC001_tTest_2SampleInd_TakeCode(selected_fn = fn_cpiC001_tTest_2SampleInd_residuals_plot006)
  list_code[[7]]  <- fn_cpiC001_tTest_2SampleInd_TakeCode(selected_fn = fn_cpiC001_tTest_2SampleInd_residuals_plot007)
  list_code[[8]]  <- fn_cpiC001_tTest_2SampleInd_TakeCode(selected_fn = fn_cpiC001_tTest_2SampleInd_residuals_plot008)
  list_code[[9]]  <- fn_cpiC001_tTest_2SampleInd_TakeCode(selected_fn = fn_cpiC001_tTest_2SampleInd_residuals_plot009)
  list_code[[10]] <- fn_cpiC001_tTest_2SampleInd_TakeCode(selected_fn = fn_cpiC001_tTest_2SampleInd_residuals_plot010)


  vector_code <- unlist(list_code)
  all_code <- paste0(vector_code, collapse = "\n\n\n\n")

  return(all_code)


  # objetos <- ls("package:Rscience", pattern = "^fn_cpiC001_tTest_2SampleInd_residuals_plot+[0-9]+")
  #
  #
  #
  # # Filtrar solo las funciones que coinciden con el patrón
  # #list_plot <- objetos[grep("^fn_cpiC001_tTest_2SampleInd_plot[0-9]+", objetos)]
  # list_plot <- objetos
  # list_plot <- sort(list_plot)
  #
  # all_code <- sapply(list_plot, function(x){
  #
  #
  #
  #   selected_code <- paste0("fn_cpiC001_tTest_2SampleInd_TakeCode(", x, ")")
  #   code_new_plot <- eval(parse(text = selected_code))
  #   code_new_plot
  #
  # }, simplify = F)
  #
  # all_code <- unlist(all_code)
  # all_code <- paste0(all_code, collapse = "\n\n\n")
  #
  #
  # return(all_code)

}




fn_cpiC001_tTest_2SampleInd_code_sectionALL <- function(intro_source_database, vr_var_name, factor_var_name, alpha_value){

  # objetos <- ls()
  #
  # # Filtrar solo las funciones que coinciden con el patrón
  # selected_fun <- objetos[grep("^fn_cpiC001_tTest_2SampleInd_code_section[0-9]+_[a-zA-Z]", objetos)]
  # selected_fun <- sort(selected_fun)

  vector_code <- list()

  vector_code[1] <- fn_cpiC001_tTest_2SampleInd_code_section01_Libreries()
  vector_code[2] <- fn_cpiC001_tTest_2SampleInd_code_section02_FileSource(intro_source_database)
  vector_code[3] <- fn_cpiC001_tTest_2SampleInd_code_section03_VarSelection(vr_var_name, factor_var_name, alpha_value)
  vector_code[4] <- fn_cpiC001_tTest_2SampleInd_code_section04_UntilTheEnd()
  vector_code[5] <- fn_cpiC001_tTest_2SampleInd_code_sectionXX_g01_Plots()
  vector_code[6] <- fn_cpiC001_tTest_2SampleInd_code_sectionXX_g02_Plots()

  vector_code <- paste0(vector_code, collapse = "\n\n\n\n\n")
  return(vector_code)
}


# # # Tables

fn_cpiC001_tTest_2SampleInd_recruit_g01_Tables <- function(list_results_from_fn_cpiC001_tTest_2SampleInd){


  all_tables <- with(list_results_from_fn_cpiC001_tTest_2SampleInd, {

    objetos <- ls()
    vector_obj_name <- objetos[grep("^df_table_factor_plot+[0-9]", objetos)]
    vector_obj_name <- sort(vector_obj_name)
    list_tables <- lapply(vector_obj_name, function(x) get(x))
    names(list_tables) <- vector_obj_name
    list_tables
  })

  return(all_tables)

}


fn_cpiC001_tTest_2SampleInd_recruit_g02_Tables <- function(list_results_from_fn_cpiC001_tTest_2SampleInd){


  all_tables <- with(list_results_from_fn_cpiC001_tTest_2SampleInd, {

    objetos <- ls()
    vector_obj_name <- objetos[grep("^df_table_residuals_plot+[0-9]", objetos)]
    vector_obj_name <- sort(vector_obj_name)
    list_tables <- lapply(vector_obj_name, function(x) get(x))
    names(list_tables) <- vector_obj_name
    list_tables
  })

  return(all_tables)

}


# # # Plots FACTOR
fn_cpiC001_tTest_2SampleInd_factor_plot001 <- function(minibase_mod, df_factor_info){

  # # # Create a new plot...
  plot001_factor <- plotly::plot_ly()

  # # # Plot001 - Scatter plot for VR and FACTOR on minibase_mod *****************
  plot001_factor <- plotly::add_trace(p = plot001_factor,
                                      type = "scatter",
                                      mode = "markers",
                                      x = minibase_mod$FACTOR,
                                      y = minibase_mod$VR,
                                      color = minibase_mod$FACTOR,
                                      colors = df_factor_info$color,
                                      marker = list(size = 15, opacity = 0.7))

  # # # Title and settings...
  plot001_factor <-   plotly::layout(p = plot001_factor,
                                     title = "Plot 001 - Scatterplot",
                                     font = list(size = 20),
                                     margin = list(t = 100))


  # # # Without zerolines
  plot001_factor <-   plotly::layout(p = plot001_factor,
                                     xaxis = list(zeroline = FALSE),
                                     yaxis = list(zeroline = FALSE))


  # # # Plot output
  plot001_factor

}





fn_cpiC001_tTest_2SampleInd_factor_plot002 <- function(df_table_factor_plot002){


  # # # Create a new plot...
  plot002_factor <- plot_ly()


  # # # Adding errors...
  plot002_factor <-   add_trace(p = plot002_factor,
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
  plot002_factor <- plotly::layout(p = plot002_factor,
                                   title = "Plot 002 - Mean and model standard deviation",
                                   font = list(size = 20),
                                   margin = list(t = 100))

  # # # Without zerolines
  plot002_factor <-plotly::layout(p = plot002_factor,
                                  xaxis = list(zeroline = FALSE),
                                  yaxis = list(zeroline = FALSE))

  # # # Plot output
  plot002_factor
}






fn_cpiC001_tTest_2SampleInd_factor_plot003 <- function(df_table_factor_plot003){


  # # # Create a new plot...
  plot003_factor <- plotly::plot_ly()


  # # # Adding errors...
  plot003_factor <-   plotly::add_trace(p = plot003_factor,
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


  # # # Title and settings...
  plot003_factor <- plotly::layout(p = plot003_factor,
                                   title = "Plot 003 - Mean y model standard error",
                                   font = list(size = 20),
                                   margin = list(t = 100))

  # # # Without zerolines
  plot003_factor <-plotly::layout(p = plot003_factor,
                                  xaxis = list(zeroline = FALSE),
                                  yaxis = list(zeroline = FALSE))

  # # # Plot output
  plot003_factor
}










fn_cpiC001_tTest_2SampleInd_factor_plot004 <- function(df_table_factor_plot004){


  # # # New plotly...
  plot004_factor <- plotly::plot_ly()

  # # # Boxplot and info...
  plot004_factor <- plotly::add_trace(p = plot004_factor,
                                      type = "box",
                                      x = df_table_factor_plot004$level ,
                                      color = df_table_factor_plot004$level,
                                      colors = df_table_factor_plot004$color,
                                      lowerfence = df_table_factor_plot004$min,
                                      q1 = df_table_factor_plot004$Q1,
                                      median = df_table_factor_plot004$median,
                                      q3 = df_table_factor_plot004$Q3,
                                      upperfence = df_table_factor_plot004$max,
                                      boxmean = TRUE,
                                      boxpoints = FALSE,
                                      line = list(color = "black", width = 3)
  )

  # # # Title and settings...
  plot004_factor <- plotly::layout(p = plot004_factor,
                                   title = "Plot 004 - Boxplot and means",
                                   font = list(size = 20),
                                   margin = list(t = 100))


  # # # Without zerolines...
  plot004_factor <- plotly::layout(p = plot004_factor,
                                   xaxis = list(zeroline = FALSE),
                                   yaxis = list(zeroline = FALSE))

  # # # Output plot004_anova...
  plot004_factor
}



fn_cpiC001_tTest_2SampleInd_factor_plot005 <- function(minibase_mod, df_table_factor_plot005){



  all_levels <- levels(minibase_mod[,2])
  n_levels <- length(all_levels)
  all_color <- rainbow(length(all_levels))



  plot005_factor <- plot_ly()

  # Violinplot
  for (k in 1:n_levels){

    # Selected values
    selected_level <- all_levels[k]
    selected_color <- all_color[k]
    dt_filas <- minibase_mod[,2] == selected_level

    # Plotting selected violinplot
    plot005_factor <- plot005_factor %>%
      add_trace(x = minibase_mod[,2][dt_filas],
                y = minibase_mod[,1][dt_filas],
                type = "violin",
                name = paste0("violin", k),
                points = "all",
                marker = list(color = selected_color),
                line = list(color = selected_color),
                fillcolor = I(selected_color)

      )


  }




  # Boxplot
  plot005_factor <- plotly::add_trace(p = plot005_factor,
                                      type = "box",
                                      name = "boxplot",
                                      x = df_table_factor_plot005$level ,
                                      color = df_table_factor_plot005$level ,
                                      lowerfence = df_table_factor_plot005$min,
                                      q1 = df_table_factor_plot005$Q1,
                                      median = df_table_factor_plot005$median,
                                      q3 = df_table_factor_plot005$Q3,
                                      upperfence = df_table_factor_plot005$max,
                                      boxmean = TRUE,
                                      boxpoints = TRUE,
                                      fillcolor = df_table_factor_plot005$color,
                                      line = list(color = "black", width = 3),
                                      opacity = 0.5,
                                      width = 0.2)


  # # # Title and settings...
  plot005_factor <- plotly::layout(p = plot005_factor,
                                   title = "Plot 005 - Violinplot",
                                   font = list(size = 20),
                                   margin = list(t = 100))


  # # # Without zerolines...
  plot005_factor <- plotly::layout(p = plot005_factor,
                                   xaxis = list(zeroline = FALSE),
                                   yaxis = list(zeroline = FALSE))

  # # # Output plot003_anova...
  plot005_factor


}


fn_cpiC001_tTest_2SampleInd_factor_plot006 <- function(minibase_mod, df_table_factor_plot006){




  #library(plotly)
  plot006_factor <- plotly::plot_ly()

  # Add traces
  plot006_factor <- plotly::add_trace(p = plot006_factor,
                                      type = "violin",
                                      y = minibase_mod$VR,
                                      x = minibase_mod$FACTOR,
                                      showlegend = TRUE,
                                      side = "positive",
                                      points = "all",
                                      name = "Violinplot",
                                      color = minibase_mod$FACTOR,
                                      colors = df_table_factor_plot006$color)



  # # # Title and settings...
  plot006_factor <- plotly::layout(p = plot006_factor,
                                   title = "Plot 006 - Scatterplot + Jitter +  Smoothed",
                                   font = list(size = 20),
                                   margin = list(t = 100))


  # # # Without zerolines...
  plot006_factor <- plotly::layout(p = plot006_factor,
                                   xaxis = list(zeroline = FALSE),
                                   yaxis = list(zeroline = FALSE))

  # # # Output plot003_anova...
  plot006_factor


}



fn_cpiC001_tTest_2SampleInd_factor_plot007 <- function(df_table_factor_plot007){


  # # # Create a new plot...
  plot007_factor <- plotly::plot_ly()


  # # # Adding errors...
  plot007_factor <-   plotly::add_trace(p = plot007_factor,
                                        type = "scatter",
                                        mode = "markers",
                                        x = df_table_factor_plot007$level,
                                        y = df_table_factor_plot007$mean,
                                        color = df_table_factor_plot007$level,
                                        colors = df_table_factor_plot007$color,
                                        marker = list(symbol = "line-ew-open",
                                                      size = 50,
                                                      opacity = 1,
                                                      line = list(width = 5)),
                                        error_y = list(type = "data", array = df_table_factor_plot007$model_error_se)
  )



  plot007_factor <-  add_text(p = plot007_factor,
                              x = df_table_factor_plot007$level,
                              y = df_table_factor_plot007$mean,
                              text = df_table_factor_plot007$group, name = "Tukey Group",
                              size = 20)

  # # # Title and settings...
  plot007_factor <- plotly::layout(p = plot007_factor,
                                   title = "Plot 007 - Mean y model standard error",
                                   font = list(size = 20),
                                   margin = list(t = 100))

  # # # Without zerolines
  plot007_factor <-plotly::layout(p = plot007_factor,
                                  xaxis = list(zeroline = FALSE),
                                  yaxis = list(zeroline = FALSE))


  # # # Plot output
  plot007_factor
}








fn_cpiC001_tTest_2SampleInd_recruit_g01_FactorPlots <- function(list_results_from_fn_cpiC001_tTest_2SampleInd){



  all_plots <-  with(list_results_from_fn_cpiC001_tTest_2SampleInd, {

    list_plots <- list()

    list_plots[[1]] <- fn_cpiC001_tTest_2SampleInd_factor_plot001(minibase_mod, df_factor_info)
    list_plots[[2]] <- fn_cpiC001_tTest_2SampleInd_factor_plot002(df_table_factor_plot002)
    list_plots[[3]] <- fn_cpiC001_tTest_2SampleInd_factor_plot003(df_table_factor_plot003)
    list_plots[[4]] <- fn_cpiC001_tTest_2SampleInd_factor_plot004(df_table_factor_plot004)
    list_plots[[5]] <- fn_cpiC001_tTest_2SampleInd_factor_plot005(minibase_mod, df_table_factor_plot005)
    list_plots[[6]] <- fn_cpiC001_tTest_2SampleInd_factor_plot006(minibase_mod, df_table_factor_plot006)
    list_plots[[7]] <- fn_cpiC001_tTest_2SampleInd_factor_plot007(df_table_factor_plot007)

    list_plots
  })


  return(all_plots)


}




##############################################################################################



# # # Plots Residuals



fn_cpiC001_tTest_2SampleInd_residuals_plot001 <- function(minibase_mod, df_factor_info){

  # # # Create a new plot...
  plot001_residuals <- plotly::plot_ly()

  # # # Plot001 - Scatter plot for VR and FACTOR on minibase_mod *****************
  plot001_residuals <- plotly::add_trace(p = plot001_residuals,
                                         type = "scatter",
                                         mode = "markers",
                                         x = minibase_mod$FACTOR,
                                         y = minibase_mod$residuals,
                                         color = minibase_mod$FACTOR,
                                         colors = df_factor_info$color,
                                         marker = list(size = 15, opacity = 0.7))

  # # # Title and settings...
  plot001_residuals <-   plotly::layout(p = plot001_residuals,
                                        title = "Plot 001 - Scatterplot - Residuals",
                                        font = list(size = 20),
                                        margin = list(t = 100))


  # # # Without zerolines
  plot001_residuals <-   plotly::layout(p = plot001_residuals,
                                        xaxis = list(zeroline = FALSE),
                                        yaxis = list(zeroline = TRUE))


  # # # Plot output
  plot001_residuals

}


fn_cpiC001_tTest_2SampleInd_residuals_plot002 <- function(minibase_mod, df_table_residuals_plot002){




  #library(plotly)
  plot002_residuals <- plotly::plot_ly()

  # Add traces
  plot002_residuals <- plotly::add_trace(p = plot002_residuals,
                                         type = "violin",
                                         y = minibase_mod$residuals,
                                         x = minibase_mod$FACTOR,
                                         showlegend = TRUE,
                                         side = "positive",
                                         points = "all",
                                         name = "Violinplot",
                                         color = minibase_mod$FACTOR,
                                         colors = df_table_residuals_plot002$color)



  # # # Title and settings...
  plot002_residuals <- plotly::layout(p = plot002_residuals,
                                      title = "Plot 002 - Residuals - Scatterplot + Jitter +  Smoothed",
                                      font = list(size = 20),
                                      margin = list(t = 100))


  # # # Without zerolines...
  plot002_residuals <- plotly::layout(p = plot002_residuals,
                                      xaxis = list(zeroline = FALSE),
                                      yaxis = list(zeroline = FALSE))

  # # # Output plot003_anova...
  plot002_residuals


}




fn_cpiC001_tTest_2SampleInd_residuals_plot003 <- function(minibase_mod, df_table_residuals_plot003){



  plot003_residuals <- plotly::plot_ly()

  # Add traces
  plot003_residuals <- plotly::add_trace(p = plot003_residuals,
                                         type = "violin",
                                         x = minibase_mod$residuals,
                                         showlegend = TRUE,
                                         side = "positive",
                                         points = FALSE,
                                         #name = levels(minibase_mod$FACTOR)[minibase_mod$lvl_order_number],
                                         color = minibase_mod$FACTOR,
                                         colors = df_table_residuals_plot003$color)



  # # # Title and settings...
  plot003_residuals <- plotly::layout(p = plot003_residuals,
                                      title = "Plot 003 - Residuals - Scatterplot + Jitter +  Smoothed",
                                      font = list(size = 20),
                                      margin = list(t = 100))


  # # # Without zerolines...
  plot003residuals <- plotly::layout(p = plot003_residuals,
                                     xaxis = list(zeroline = FALSE),
                                     yaxis = list(zeroline = FALSE))

  # # # Output plot003_anova...
  plot003_residuals


}



fn_cpiC001_tTest_2SampleInd_residuals_plot004 <- function(minibase_mod){




  #library(plotly)
  plot005_residuals <- plotly::plot_ly()

  # Add traces
  plot005_residuals <- plotly::add_trace(p = plot005_residuals,
                                         type = "violin",
                                         x = minibase_mod$residuals,
                                         #x = minibase_mod$FACTOR,
                                         showlegend = TRUE,
                                         side = "positive",
                                         points = "all",
                                         name = " ")#
  #color = minibase_mod$FACTOR,
  #colors = df_table_factor_plot006$color)



  # # # Title and settings...
  plot005_residuals <- plotly::layout(p = plot005_residuals,
                                      title = "Plot 004 - Residuals",
                                      font = list(size = 20),
                                      margin = list(t = 100))


  # # # Without zerolines...
  plot005_residuals <- plotly::layout(p = plot005_residuals,
                                      xaxis = list(zeroline = TRUE),
                                      yaxis = list(zeroline = FALSE))

  # # # Output plot003_anova...
  plot005_residuals


}



fn_cpiC001_tTest_2SampleInd_residuals_plot005 <- function(minibase_mod){



  qq_info <- EnvStats::qqPlot(x = minibase_mod$residuals, plot.it = F,
                              param.list = list(mean = mean(minibase_mod$residuals),
                                                sd = sd(minibase_mod$residuals)))

  cuantiles_teoricos <- qq_info$x
  cuantiles_observados <- qq_info$y

  #library(plotly)
  plot007_residuals <- plotly::plot_ly()

  # Crear el gráfico QQ plot
  plot007_residuals <-add_trace(p = plot007_residuals,
                                x = cuantiles_teoricos,
                                y = cuantiles_observados,
                                type = 'scatter', mode = 'markers',
                                marker = list(color = 'blue'),
                                name = "points")

  # Agregar la línea de identidad
  pendiente <- 1
  intercepto <- 0

  # Calcular las coordenadas de los extremos de la línea de identidad
  x_extremos <- range(cuantiles_teoricos)
  y_extremos <- pendiente * x_extremos + intercepto

  # Agregar la recta de identidad
  plot007_residuals <- add_trace(p = plot007_residuals,
                                 x = x_extremos,
                                 y = y_extremos,
                                 type = 'scatter',
                                 mode = 'lines',
                                 line = list(color = 'red'),
                                 name = "identity")


  # Establecer etiquetas de los ejes
  # plot007_residuals <- layout(p = plot007_residuals,
  #                             xaxis = list(title = 'Expected quantiles'),
  #                             yaxis = list(title = 'Observed quantiles'))

  plot007_residuals <- plotly::layout(p = plot007_residuals,
                                      title = "Plot 005 - QQ Plot Residuals",
                                      font = list(size = 20),
                                      margin = list(t = 100))

  # Mostrar el gráfico
  plot007_residuals


}



fn_cpiC001_tTest_2SampleInd_residuals_plot006 <- function(minibase_mod, df_factor_info){

  # # # Create a new plot...
  plot003_residuals <- plotly::plot_ly()

  # # # Plot001 - Scatter plot for VR and FACTOR on minibase_mod *****************
  plot003_residuals <- plotly::add_trace(p = plot003_residuals,
                                         type = "scatter",
                                         mode = "markers",
                                         x = minibase_mod$fitted.values,
                                         y = minibase_mod$residuals,
                                         color = minibase_mod$FACTOR,
                                         colors = df_factor_info$color,
                                         marker = list(size = 15, opacity = 0.7))

  # # # Title and settings...
  plot003_residuals <-   plotly::layout(p = plot003_residuals,
                                        title = "Plot 006 - Scatterplot - Residuals vs Fitted.values",
                                        font = list(size = 20),
                                        margin = list(t = 100))


  # # # Without zerolines
  plot003_residuals <-   plotly::layout(p = plot003_residuals,
                                        xaxis = list(zeroline = FALSE),
                                        yaxis = list(zeroline = TRUE))


  # # # Plot output
  plot003_residuals

}




fn_cpiC001_tTest_2SampleInd_residuals_plot007 <- function(minibase_mod, df_factor_info){

  # # # Create a new plot...
  plot004_residuals <- plotly::plot_ly()

  # # # Plot001 - Scatter plot for VR and FACTOR on minibase_mod *****************
  plot004_residuals <- plotly::add_trace(p = plot004_residuals,
                                         type = "scatter",
                                         mode = "markers",
                                         x = minibase_mod$FACTOR,
                                         y = minibase_mod$studres,
                                         color = minibase_mod$FACTOR,
                                         colors = df_factor_info$color,
                                         marker = list(size = 15, opacity = 0.7))

  # # # Title and settings...
  plot004_residuals <-   plotly::layout(p = plot004_residuals,
                                        title = "Plot 007 - Scatterplot - Studentized Residuals",
                                        font = list(size = 20),
                                        margin = list(t = 100))


  # # # Without zerolines
  plot004_residuals <-   plotly::layout(p = plot004_residuals,
                                        xaxis = list(zeroline = FALSE),
                                        yaxis = list(zeroline = TRUE))


  # # # Plot output
  plot004_residuals

}





fn_cpiC001_tTest_2SampleInd_residuals_plot008 <- function(minibase_mod){




  #library(plotly)
  plot006_residuals <- plotly::plot_ly()

  # Add traces
  plot006_residuals <- plotly::add_trace(p = plot006_residuals,
                                         type = "violin",
                                         x = minibase_mod$studres,
                                         #x = minibase_mod$FACTOR,
                                         showlegend = TRUE,
                                         side = "positive",
                                         points = "all",
                                         name = " ")#
  #color = minibase_mod$FACTOR,
  #colors = df_table_factor_plot006$color)



  # # # Title and settings...
  plot006_residuals <- plotly::layout(p = plot006_residuals,
                                      title = "Plot 008 - Studentized Residuals",
                                      font = list(size = 20),
                                      margin = list(t = 100))


  # # # Without zerolines...
  plot006_residuals <- plotly::layout(p = plot006_residuals,
                                      xaxis = list(zeroline = TRUE),
                                      yaxis = list(zeroline = FALSE))

  # # # Output plot003_anova...
  plot006_residuals


}




fn_cpiC001_tTest_2SampleInd_residuals_plot009 <- function(minibase_mod){




  x <- seq(-4, 4, length.out = 100)
  y <- dnorm(x, mean = 0, 1)
  #  x <- x*model_error_sd
  densidad_suavizada <- density(x, kernel = "gaussian", adjust = 0.5)
  hist_data_studres <- hist(minibase_mod$studres, plot = FALSE)
  hist_data_studres$"rel_frec" <- hist_data_studres$counts/sum(hist_data_studres$counts)

  densidad_studres <-  density(x = minibase_mod$studres, kernel = "gaussian", adjust =0.5)

  #library(plotly)
  plot005_residuals <- plotly::plot_ly()


  # plot005_residuals <- add_trace(p = plot005_residuals,
  #                                x = densidad_studres$x,
  #                                y = densidad_studres$y,
  #                                type = 'scatter',
  #                                mode = 'lines',
  #                                name = 'densidad_studres')

  plot005_residuals <- add_trace(p = plot005_residuals,
                                 x = x,
                                 y = y,
                                 type = 'scatter',
                                 mode = 'lines',
                                 name = 'Normal Standard')





  # # Add traces
  # plot005_residuals <- plotly::add_trace(p = plot005_residuals,
  #                                        type = "violin",
  #                                        x = minibase_mod$residuals,
  #                                        #x = minibase_mod$FACTOR,
  #                                        showlegend = TRUE,
  #                                        side = "positive",
  #                                        points = FALSE,
  #                                        name = "violinplot")#

  plot005_residuals <- plotly::add_trace(p = plot005_residuals,
                                         type = "bar",
                                         x = hist_data_studres$"mids",
                                         y = hist_data_studres$"density",
                                         name = "hist - studres")

  plot005_residuals <- plotly::layout(p = plot005_residuals,
                                      bargap = 0)

  plot005_residuals <- plotly::layout(p = plot005_residuals,
                                      title = "Plot 009 - Studres Distribution",
                                      font = list(size = 20),
                                      margin = list(t = 100))

  plot005_residuals


}



fn_cpiC001_tTest_2SampleInd_residuals_plot010 <- function(minibase_mod){



  qq_info <- EnvStats::qqPlot(x = minibase_mod$studres, plot.it = F,
                              param.list = list(mean = 0,
                                                sd = 1))

  cuantiles_teoricos <- qq_info$x
  cuantiles_observados <- qq_info$y

  #library(plotly)
  plot007_residuals <- plotly::plot_ly()

  # Crear el gráfico QQ plot
  plot007_residuals <-add_trace(p = plot007_residuals,
                                x = cuantiles_teoricos,
                                y = cuantiles_observados,
                                type = 'scatter', mode = 'markers',
                                marker = list(color = 'blue'),
                                name = "points")

  # Agregar la línea de identidad
  pendiente <- 1
  intercepto <- 0

  # Calcular las coordenadas de los extremos de la línea de identidad
  x_extremos <- range(cuantiles_teoricos)
  y_extremos <- pendiente * x_extremos + intercepto

  # Agregar la recta de identidad
  plot007_residuals <- add_trace(p = plot007_residuals,
                                 x = x_extremos,
                                 y = y_extremos,
                                 type = 'scatter',
                                 mode = 'lines',
                                 line = list(color = 'red'),
                                 name = "identity")


  # Establecer etiquetas de los ejes
  # plot007_residuals <- layout(p = plot007_residuals,
  #                             xaxis = list(title = 'Expected quantiles'),
  #                             yaxis = list(title = 'Observed quantiles'))

  plot007_residuals <- plotly::layout(p = plot007_residuals,
                                      title = "Plot 010 - QQ Plot - studres",
                                      font = list(size = 20),
                                      margin = list(t = 100))

  # Mostrar el gráfico
  plot007_residuals


}




fn_cpiC001_tTest_2SampleInd_recruit_g02_ResidualsPlots <- function(list_results_from_fn_cpiC001_tTest_2SampleInd){


  all_plots <-  with(list_results_from_fn_cpiC001_tTest_2SampleInd, {

    list_plots <- list()

    list_plots[[1]]  <- fn_cpiC001_tTest_2SampleInd_residuals_plot001(minibase_mod, df_factor_info)
    list_plots[[2]]  <- fn_cpiC001_tTest_2SampleInd_residuals_plot002(minibase_mod, df_table_residuals_plot002)
    list_plots[[3]]  <- fn_cpiC001_tTest_2SampleInd_residuals_plot003(minibase_mod, df_table_residuals_plot003)
    list_plots[[4]]  <- fn_cpiC001_tTest_2SampleInd_residuals_plot004(minibase_mod)
    list_plots[[5]]  <- fn_cpiC001_tTest_2SampleInd_residuals_plot005(minibase_mod)
    list_plots[[6]]  <- fn_cpiC001_tTest_2SampleInd_residuals_plot006(minibase_mod, df_factor_info)
    list_plots[[7]]  <- fn_cpiC001_tTest_2SampleInd_residuals_plot007(minibase_mod, df_factor_info)
    list_plots[[8]]  <- fn_cpiC001_tTest_2SampleInd_residuals_plot008(minibase_mod)
    list_plots[[9]]  <- fn_cpiC001_tTest_2SampleInd_residuals_plot009(minibase_mod)
    list_plots[[10]] <- fn_cpiC001_tTest_2SampleInd_residuals_plot010(minibase_mod)

    list_plots
  })


  return(all_plots)


}













