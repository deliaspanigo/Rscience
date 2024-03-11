

# # # # Special Functions
# Take the original code from a function
fn_cpiA002_TakeCode <- function(selected_fn){


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
fn_cpiA002_ObjNamesInOrder <- function(selected_fn){

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
fn_cpiA002_control_previous <- function(database, vr_var_name, factor_var_name, block_var_name, alpha_value){

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

#############
  # # # # # # # # block_var_name
  # 10) block_var_name is not NULL
  if(is.null(block_var_name)){
    text_output <- "Control pre test 011: Object 'block_var_name' can not be NULL."
    return(Hmisc::llist(dt_ok, text_output))
  }

  # 11) factor_var_name is a vector
  if(!is.vector(block_var_name)){
    text_output <- "Control pre test 012: Object 'block_var_name' must be vector."
    return(Hmisc::llist(dt_ok, text_output))
  }

  # 12) factor_var_name is a vector
  if(!(length(block_var_name) == 1)){
    text_output <- "Control pre test 013: Object 'block_var_name' must be vector of length 1."
    return(Hmisc::llist(dt_ok, text_output))
  }

  # 13) factor_var_name is not NA
  if(is.na(block_var_name)){
    text_output <- "Control pre test 014: Object 'block_var_name' can not be NA."
    return(Hmisc::llist(dt_ok, text_output))
  }

  # 14) factor_var_name is character
  if(!is.character(block_var_name)){
    text_output <- "Control pre test 015: Object 'block_var_name' must be character."
    return(Hmisc::llist(dt_ok, text_output))
  }


  # 15) factor_var_name is a colname from database
  if(!(block_var_name %in% colnames(database))){
    text_output <- "Control pre test 016: Object 'block_var_name' must be a colname from database."
    return(Hmisc::llist(dt_ok, text_output))
  }

###############


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

  if(vr_var_name == block_var_name){
    text_output <- "Control pre test 023: Objects 'vr_var_name' and 'factor_var_name' can not be equal."
    return(Hmisc::llist(dt_ok, text_output))
  }


  if(block_var_name == factor_var_name){
    text_output <- "Control pre test 023: Objects 'block_var_name' and 'factor_var_name' can not be equal."
    return(Hmisc::llist(dt_ok, text_output))
  }


  # New object
  vector_var_names <- c(vr_var_name, factor_var_name, block_var_name)

  if(sum(vector_var_names %in% colnames(database)) != 3){
    text_output <- "Control pre test 024: Objects 'vr_var_name', 'block_var_name' and 'factor_var_name' must be colnames from database."
    return(Hmisc::llist(dt_ok, text_output))
  }



  # # # # # # # # # # # minibase
  minibase <- na.omit(database[vector_var_names])
  minibase[,2] <- as.factor(minibase[,2])
  minibase[,3] <- as.factor(minibase[,3])
  minibase[,4] <- as.factor(paste0(minibase[,2], "_", minibase[,3]))
  colnames(minibase) <- c("VR", "FACTOR", "BLOCK", "COMBINATION")



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
  if(!(ncol(minibase) == 4)){
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


  # Al least 2 levels in minibase$FACTOR
  if(!(nlevels(minibase$FACTOR) >= 2)){
    text_output <- "Control pre test 032: FACTOR must has al least 2 levels."
    return(Hmisc::llist(dt_ok, text_output))
  }

  # Al least 2 levels in minibase$FACTOR
  if(!(nlevels(minibase$BLOCK) >= 2)){
    text_output <- "Control pre test 032: BLOCK must has al least 2 levels."
    return(Hmisc::llist(dt_ok, text_output))
  }


  # Al least 2 reps in each level
  reps_level <- tapply(minibase$VR, minibase$FACTOR, length)
  dt_reps_level <- reps_level >= 2
  check_01 <- sum(dt_reps_level) == length(dt_reps_level)
  if(!check_01){
    text_output <- "Control pre test 033: On minibase from FACTOR each level must has al least 2 reps."
    return(Hmisc::llist(dt_ok, text_output))
  }


  # var greater than zero from each level
  vars_level <- tapply(minibase$VR, minibase$FACTOR, var)
  dt_vars_level <- reps_level > 0
  check_02 <- sum(dt_vars_level) ==  length(dt_vars_level)
  if(!check_02){
    text_output <- "Control pre test 034: On minibase from FACTOR each level can not be constant.
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
    text_output <- "Control pre test 035: On minibase from FACTOR each level must not be constant."
    return(Hmisc::llist(dt_ok, text_output))
  }


  # Final!
  dt_ok <- TRUE
  text_output <- ""
  return(Hmisc::llist(dt_ok, text_output))



}



fn_cpiA002_code_p01_test <- function(database, vr_var_name, factor_var_name,
                                  block_var_name, alpha_value){





  # # # # # Section 04 - Var rols and minibase -----------------------------------
  # # # Selected vars
  vector_all_var_names <- colnames(database)
  vector_name_selected_vars <- c(vr_var_name, factor_var_name, block_var_name)
  vector_rol_vars <- c("VR", "FACTOR", "BLOCK")




  # # # # # Section 05 - minibase ------------------------------------------------
  # Only selected vars. Only completed rows. Factor columns as factor object in R.
  minibase <- na.omit(database[vector_name_selected_vars])
  colnames(minibase) <- vector_rol_vars
  minibase[,2] <- as.factor(as.character(minibase[,2]))
  minibase[,3] <- as.factor(as.character(minibase[,3]))
  minibase$"COMBINATION" <- paste0(minibase[,2], "_", minibase[,3])
  minibase$"COMBINATION" <- as.factor(as.character(minibase$COMBINATION))



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
    "control" = c("is.numeric()", "is.factor()", "is.factor()"),
    "verify" = c(is.numeric(minibase[,1]), is.factor(minibase[,2]), is.factor(minibase[,3]))
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


  df_block_info <- data.frame(
    "order" = 1:nlevels(minibase[,3]),
    "level" = levels(minibase[,3]),
    "n" = as.vector(table(minibase[,3])),
    "mean" = tapply(minibase[,1], minibase[,3], mean),
    "color" = rainbow(nlevels(minibase[,3]))
  )
  df_block_info



  df_combination_info <- data.frame(
    "order" = 1:nlevels(minibase[,4]),
    "level" = levels(minibase[,4]),
    "n" = as.vector(table(minibase[,4])),
    "mean" = tapply(minibase[,1], minibase[,4], mean),
    "color" = rainbow(nlevels(minibase[,4]))
  )




  # # # Unbalanced reps for levels for FACTOR?
  # Important information for Tukey.
  # If reps its equal or not equal in all levels must be detailled
  # on Tukey.
  check_unbalanced_reps <- length(unique(df_factor_info$n)) > 1
  check_unbalanced_reps





  # # # # # Section 06 - Anova Test ----------------------------------------------
  # # # Anova test
  lm_anova <- lm(VR ~ FACTOR + BLOCK, data = minibase)               # Linear model
  aov_anova <- aov(lm_anova)                                 # R results for anova
  df_table_anova <- as.data.frame(summary(aov_anova)[[1]])   # Common anova table
  df_table_anova



  # # # Standard error from model for each level
  model_error_var <- df_table_anova$`Mean Sq`[3]
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





  # # # # # Section 09 - Tukey --------------------------------------------------
  # # # Tukey test - Tukey with groups - Full version
  tukey01_full_groups <- agricolae::HSD.test(y = lm_anova,
                                             trt = colnames(minibase)[2],
                                             alpha = alpha_value,
                                             group = TRUE,
                                             console = FALSE,
                                             unbalanced = check_unbalanced_reps)



  # # # Tukey test - Tukey pairs comparation - Full version
  tukey02_full_pairs <- agricolae::HSD.test(y = lm_anova,
                                            trt = colnames(minibase)[2],
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




  # # # # # Section 07 - minibase_mod --------------------------------------------
  # # # Detect rows on database there are on minibase
  dt_rows_database_ok <- rowSums(!is.na(database[vector_name_selected_vars])) == length(vector_name_selected_vars)



  # # # Object minibase_mod and new cols
  minibase_mod <- minibase
  minibase_mod$"order_number_lvl_factor" <- as.numeric(minibase_mod[,2])
  minibase_mod$"color_lvl_factor" <- df_factor_info$color[minibase_mod$"order_number_lvl_"]
  minibase_mod$"order_number_lvl_block" <- as.numeric(minibase_mod[,3])
  minibase_mod$"color_lvl_block" <- df_block_info$color[minibase_mod$"order_number_lvl_block"]
  minibase_mod$"order_number_lvl_combination" <- as.numeric(minibase_mod[,4])
  minibase_mod$"color_lvl_combination" <- df_combination_info$color[minibase_mod$"order_number_lvl_combination"]


  minibase_mod$"fitted.values" <- df_combination_info$"mean"[minibase_mod$"order_number_lvl_combination"]
  minibase_mod$"residuals" <- lm_anova$residuals
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
  df_residuals_variance_factor_levels <- data.frame(
    "order" = 1:nlevels(minibase_mod[,2]),
    "factor" = levels(minibase_mod[,2]),
    "variance" = tapply(minibase_mod$residuals, minibase_mod[,2], var),
    "n" = tapply(minibase_mod$residuals, minibase_mod[,2], length)
  )
  df_residuals_variance_factor_levels



  # # # Sum for residuals
  sum_residuals <- sum(minibase_mod$residuals)
  sum_residuals



  # # # Mean for residuals
  mean_residuals <- mean(minibase_mod$residuals)
  mean_residuals


  # # # # # Section 10 - Partitioned Measures (VR) for FACTOR --------------------------------
  # # # Partitioned Measures Position (VR) for FACTOR
  df_vr_position_levels_factor <- data.frame(
    "order" = 1:nlevels(minibase[,2]),
    "level" = levels(minibase[,2]),
    "n" = tapply(minibase[,1], minibase[,2], length),
    "min" = tapply(minibase[,1], minibase[,2], min),
    "mean" = tapply(minibase[,1], minibase[,2], mean),
    "Q1" = tapply(minibase[,1], minibase[,2], quantile, 0.25),
    "median" = tapply(minibase[,1], minibase[,2], median),
    "Q3" = tapply(minibase[,1], minibase[,2], quantile, 0.75),
    "max" = tapply(minibase[,1], minibase[,2], max)
  )



  # # # Partitioned Measures of Dispersion (VR) for FACTOR
  df_vr_dispersion_levels_factor <- data.frame(
    "order" = 1:nlevels(minibase[,2]),
    "level" = levels(minibase[,2]),
    "n" = tapply(minibase[,1], minibase[,2], length),
    "range" = tapply(minibase[,1], minibase[,2], function(x){max(x) - min(x)}),
    "variance" = tapply(minibase[,1], minibase[,2], var),
    "standard_deviation" = tapply(minibase[,1], minibase[,2], sd),
    "standard_error" = tapply(minibase[,1], minibase[,2], function(x){sd(x)/sqrt(length(x))})
  )
  df_vr_dispersion_levels_factor




  # # # # # Section 11 - Partitioned Measures (VR) for BLOCK --------------------------------
  # # # Partitioned Measures Position (VR) for BLOCK
  df_vr_position_levels_block <- data.frame(
    "order" = 1:nlevels(minibase[,3]),
    "level" = levels(minibase[,3]),
    "min" = tapply(minibase[,1], minibase[,3], min),
    "mean" = tapply(minibase[,1], minibase[,3], mean),
    "Q1" = tapply(minibase[,1], minibase[,3], quantile, 0.25),
    "median" = tapply(minibase[,1], minibase[,3], median),
    "Q3" = tapply(minibase[,1], minibase[,3], quantile, 0.75),
    "max" = tapply(minibase[,1], minibase[,3], max),
    "n" = tapply(minibase[,1], minibase[,3], length)
  )



  # # # Partitioned Measures of Dispersion (VR) for FACTOR
  df_vr_dispersion_levels_block <- data.frame(
    "order" = 1:nlevels(minibase[,3]),
    "level" = levels(minibase[,3]),
    "range" = tapply(minibase[,1], minibase[,3], function(x){max(x) - min(x)}),
    "variance" = tapply(minibase[,1], minibase[,3], var),
    "standard_deviation" = tapply(minibase[,1], minibase[,3], sd),
    "standard_error" = tapply(minibase[,1], minibase[,3], function(x){sd(x)/sqrt(length(x))}),
    "n" = tapply(minibase[,1], minibase[,3], length)
  )
  df_vr_dispersion_levels_block




  # # # # # Section 11 - Partitioned Measures (VR) for BLOCK --------------------------------
  # # # Partitioned Measures Position (VR) for BLOCK
  df_vr_position_levels_combination <- data.frame(
    "order" = 1:nlevels(minibase[,4]),
    "level" = levels(minibase[,4]),
    "n" = tapply(minibase[,1], minibase[,4], length),
    "min" = tapply(minibase[,1], minibase[,4], min),
    "mean" = tapply(minibase[,1], minibase[,4], mean),
    "Q1" = tapply(minibase[,1], minibase[,4], quantile, 0.25),
    "median" = tapply(minibase[,1], minibase[,4], median),
    "Q3" = tapply(minibase[,1], minibase[,4], quantile, 0.75),
    "max" = tapply(minibase[,1], minibase[,4], max)
  )



  # # # Partitioned Measures of Dispersion (VR) for FACTOR
  df_vr_dispersion_levels_combiantion <- data.frame(
    "order" = 1:nlevels(minibase[,4]),
    "level" = levels(minibase[,4]),
    "n" = tapply(minibase[,1], minibase[,4], length),
    "range" = tapply(minibase[,1], minibase[,4], function(x){max(x) - min(x)}),
    "variance" = tapply(minibase[,1], minibase[,4], var),
    "standard_deviation" = tapply(minibase[,1], minibase[,4], sd),
    "standard_error" = tapply(minibase[,1], minibase[,4], function(x){sd(x)/sqrt(length(x))})
  )
  df_vr_dispersion_levels_combiantion



  # # # # # Section 12 - General Measures (VR) -----------------------------------
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





  # # # # # Section 12 - Partitioned Measures (Residuals) for FACTOR -------------------------
  # # # Partitioned Measures of Position (residuals) for FACTOR
  df_residuals_position_levels_factor <- data.frame(
    "order" = 1:nlevels(minibase_mod[,2]),
    "level" = levels(minibase_mod[,2]),
    "min" = tapply(minibase_mod$residuals, minibase_mod[,2], min),
    "mean" = tapply(minibase_mod$residuals, minibase_mod[,2], mean),
    "median" = tapply(minibase_mod$residuals, minibase_mod[,2], median),
    "max" = tapply(minibase_mod$residuals, minibase_mod[,2], max),
    "n" = tapply(minibase_mod$residuals, minibase_mod[,2], length)
  )
  df_residuals_position_levels_factor



  # # # Partitioned Measures of Dispersion (residuals) for FACTOR ----------------
  df_residual_dispersion_levels_factor <- data.frame(
    "order" = 1:nlevels(minibase_mod[,2]),
    "level" = levels(minibase_mod[,2]),
    "range" = tapply(minibase_mod$residuals, minibase_mod[,2], function(x){max(x) - min(x)}),
    "variance" = tapply(minibase_mod$residuals, minibase_mod[,2], var),
    "standard_deviation" = tapply(minibase_mod$residuals, minibase_mod[,2], sd),
    "standard_error" = tapply(minibase_mod$residuals, minibase_mod[,2], function(x){sd(x)/sqrt(length(x))}),
    "n" = tapply(minibase_mod$residuals, minibase_mod[,2], length)
  )
  df_residual_dispersion_levels_factor



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


  # Tabla plot 001
  df_table_plot001 <- aggregate(VR ~ FACTOR + BLOCK, data = minibase, FUN = mean)
  colnames(df_table_plot001)[3] <- "mean"
  df_table_plot001$"color_factor" <- rep(df_factor_info$color, nlevels(minibase$BLOCK))


  # Tabla plot 002

  df_table_plot002 <- data.frame(
    "order" = df_factor_info$order,
    "level" = df_factor_info$level,
    "n" = df_factor_info$n,
    "mean" = tapply(minibase[,1], minibase[,2], mean),
    "model_error_se" = df_model_error$model_error_se
  )

  df_table_plot002["lower_limit"]  <- df_table_plot002$mean - df_table_plot002$model_error_se
  df_table_plot002["upper_limmit"] <- df_table_plot002$mean + df_table_plot002$model_error_se
  df_table_plot002["color"] <- df_factor_info$color

  correct_pos_letters <- order(df_tukey_table$level)
  vector_letters <- df_tukey_table$group[correct_pos_letters]
  df_table_plot002["group"] <- vector_letters


  # --- # hide_: Proccesing objects order
  hide_correct_order <- fn_cpiA002_ObjNamesInOrder(selected_fn = fn_cpiA002_code_p01_test)
  hide_output_list_objects <- mget(hide_correct_order)

  # --- # hide_: return!
  return(hide_output_list_objects)

}



fn_cpiA002_code_p02_plot001 <- function(results_p01_test){



  new_plot <-  with(results_p01_test,{


      # Crear el gráfico interactivo con Plotly
      plot001 <- df_table_plot001 %>%
        group_by(FACTOR)  %>%
        plot_ly(x = ~BLOCK,
                y = ~mean,
                type = 'scatter',
                mode = 'lines+markers',
                color = ~FACTOR,
                colors =~color_factor,
                line = list(width = 4),
                marker = list(size = 20, opacity = 0.7))

      plot001 <- plotly::layout(p = plot001,
                                xaxis = list(title = "BLOCK"),
                                yaxis = list(title = "VR"),
                                title = "Plot 001 - Interacción Factor-Bloque",
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



fn_cpiA002_code_p02_plot002 <- function(results_p01_test){



  new_plot <-  with(results_p01_test,{


    # # # Create a new plot...
    plot002 <- plotly::plot_ly()


    # # # Adding errors...
    plot002 <-   plotly::add_trace(p = plot002,
                                   type = "scatter",
                                   mode = "markers",
                                   x = df_table_plot002$level,
                                   y = df_table_plot002$mean,
                                   color = df_table_plot002$level,
                                   colors = df_table_plot002$color,
                                   marker = list(symbol = "line-ew-open",
                                                 size = 50,
                                                 opacity = 1,
                                                 line = list(width = 5)),
                                   error_y = list(type = "data", array = df_table_plot002$model_error_se)
    )



    plot002 <-  add_text(p = plot002,
                         x = df_table_plot002$level,
                         y = df_table_plot002$mean,
                         text = df_table_plot002$group, name = "Tukey Group",
                         size = 20)

    # # # Title and settings...
    plot002 <- plotly::layout(p = plot002,
                              xaxis = list(title = "FACTOR"),
                              yaxis = list(title = "VR"),
                              title = "Plot 002 - Mean y model standard error",
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



# Control post
fn_cpiA002_control_p01_test <- function(all_results){

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


fn_cpiA002_control_p02_plots <- function(all_results){

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





fn_cpiA002_gen01 <- function(database, vr_var_name, factor_var_name,
                             block_var_name, alpha_value){

  output_list <- list()
  output_list$"R_code" <- list()
  output_list$"R_code"$"p01_test" <- NULL
  output_list$"R_code"$"p02_plots" <- list()
  output_list$"R_code"$"p02_plots"$"plot001" <- NULL
  output_list$"R_code"$"p02_plots"$"plot002" <- NULL


  output_list$"check_control_previous" <- NULL

  output_list$"check_control_post" <- list()
  output_list$"check_control_post"$"p01_test" <- NULL


  output_list$"R_results" <- list()
  output_list$"R_results"$"p01_test" <- NULL
  output_list$"R_results"$"p02_plots" <- list()
  output_list$"R_results"$"p02_plots"$"plot001" <- NULL
  output_list$"R_results"$"p02_plots"$"plot002" <- NULL

  # Step 01 - R_code
  output_list$"R_code"$"p01_test"    <- fn_cpiA002_TakeCode(selected_fn = fn_cpiA002_code_p01_test)
  output_list$"R_code"$"p02_plots"$"plot001"   <- fn_cpiA002_TakeCode(selected_fn = fn_cpiA002_code_p02_plot001)
  output_list$"R_code"$"p02_plots"$"plot002"   <- fn_cpiA002_TakeCode(selected_fn = fn_cpiA002_code_p02_plot002)


  # Step 02 - Pre Control ------------------------------------------------------
  output_list$"check_control"$"previous" <- fn_cpiA002_control_previous(database, vr_var_name,
                                                          factor_var_name, block_var_name,
                                                          alpha_value)





  # Step 03 - Results ----------------------------------------------------------
  output_list$"R_results"$"p01_test" <- fn_cpiA002_code_p01_test(database, vr_var_name,
                                                                 factor_var_name,
                                                          block_var_name, alpha_value)


  output_list$"R_results"$"p02_plots"$"plot001" <- fn_cpiA002_code_p02_plot001(results_p01_test = output_list$"R_results"$"p01_test")
  output_list$"R_results"$"p02_plots"$"plot002" <- fn_cpiA002_code_p02_plot002(results_p01_test = output_list$"R_results"$"p01_test")





  return(output_list)

}


fn_cpiA002_gen02 <- function(database, vr_var_name, factor_var_name,
                             block_var_name, alpha_value){


  all_results <- fn_cpiA002_gen01(database, vr_var_name, factor_var_name,
                                  block_var_name, alpha_value)



  output_list <- list()


  # Out01 - Analysis -------------------------------------------------------------
  selection01 <- c("df_selected_vars",
                   "df_table_anova", "df_factor_info", "check_unbalanced_reps",
                   "df_tukey_table",
                   "df_model_error")

  output_list$"out01_analysis" <- all_results$R_results$p01_test[selection01]




  # Out02 - Requeriments ---------------------------------------------------------
  selection02 <- c("test_residuals_normality",
                   "test_residuals_homogeneity",
                   "df_residuals_variance_factor_levels")

  output_list$"out02_requeriments" <- all_results$R_results$p01_test[selection02]



  # Out03 - Plots and Tables - Factor --------------------------------------------
  output_list$"out03A_plots_factor" <- all_results$R_results$p02_plots


  selection03B <- c("df_table_plot001", "df_table_plot002")
  output_list$"out03B_tables_factor" <- all_results$R_results$p01_test[selection03B]




  # Out04 - Plots and Tables - Residuals -----------------------------------------
  output_list$"out04A_plots_residuals" <- all_results$R_results$p02_plots


  selection04B <- c("df_table_plot001", "df_table_plot002")
  output_list$"out04B_tables_residuals" <- all_results$R_results$p01_test[selection04B]



  # Out05 - Full Results ---------------------------------------------------------
  output_list$"out05_full_results" <- all_results$R_results$p01_test



  # Out06 - R Code ---------------------------------------------------------------
  output_list$"out06_R_code" <- paste0(unlist(all_results$R_code), collapse = "\n\n\n")


  return(output_list)
}

