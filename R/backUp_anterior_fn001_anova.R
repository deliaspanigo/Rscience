
# # # Test 001 anova - gen01
test001_anova_full_gen01 <- function(database, vr_var_name, factor_var_name, alpha_value){

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





  # # # # # Section 06 - Anova Test ----------------------------------------------
  # # # Anova test
  lm_anova <- lm(VR ~ FACTOR, data = minibase)               # Linear model
  aov_anova <- aov(lm_anova)                                 # R results for anova
  df_table_anova <- as.data.frame(summary(aov_anova)[[1]])   # Common anova table
  df_table_anova



  # # # Standard error from model for each level
  model_error_var <- df_table_anova$`Mean Sq`[2]
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
  df_anova_model_long <- data.frame(
    "order" = df_factor_info$order,
    "level" = df_factor_info$level,
    "n" = df_factor_info$n,
    "est_mu" = vector_est_mu,
    "est_tau_i" = vector_est_tau_i
  )
  df_anova_model_long



  # # # Short model information on dataframe
  df_anova_model_short <- data.frame(
    "order" = df_factor_info$order,
    "level" = df_factor_info$level,
    "n" = df_factor_info$n,
    "est_mu_i" = vector_est_mu_i
  )
  df_anova_model_short





  # # # # # Section 13 - Special table to plots ----------------------------------

  # # # Table for plot001
  df_plot001_table <- data.frame(
    "order" = df_factor_info$order,
    "level" = df_factor_info$level,
    "n" = df_factor_info$n,
    "mean" = tapply(minibase[,1], minibase[,2], mean),
    "min" = tapply(minibase[,1], minibase[,2], min),
    "max" = tapply(minibase[,1], minibase[,2], max),
    "sd" = tapply(minibase[,1], minibase[,2], sd),
    "var" = tapply(minibase[,1], minibase[,2], var)
  )

  df_plot002_table <- data.frame(
    "order" = df_factor_info$order,
    "level" = df_factor_info$level,
    "n" = df_factor_info$n,
    "mean" = tapply(minibase[,1], minibase[,2], mean),
    "model_error_sd" = df_model_error$model_error_sd
  )
  df_plot002_table["lower_limit"] <- df_plot002_table$mean - df_plot002_table$model_error_sd
  df_plot002_table["upper_limmit"] <- df_plot002_table$mean + df_plot002_table$model_error_sd
  df_plot002_table["color"] <- df_factor_info$color
  df_plot002_table


  df_plot003_table <- data.frame(
    "order" = df_factor_info$order,
    "level" = df_factor_info$level,
    "n" = df_factor_info$n,
    "mean" = tapply(minibase[,1], minibase[,2], mean),
    "model_error_se" = df_model_error$model_error_se
  )
  df_plot003_table["lower_limit"] <- df_plot003_table$mean - df_plot003_table$model_error_se
  df_plot003_table["upper_limmit"] <- df_plot003_table$mean + df_plot003_table$model_error_se
  df_plot003_table["color"] <- df_factor_info$color
  df_plot003_table



  # # # Table for plot004
  df_plot004_table <- df_vr_position_levels
  df_plot004_table["color"] <- df_factor_info$color

  # # # Table for plot005
  df_plot005_table <- df_plot004_table

  # # # Table for plot006
  df_plot006_table <- df_plot004_table

  # --- # hide_: Proccesing objects order
  hide_correct_order <- ObjNames_ProcOrder_test001_anova()
  hide_output_list_objects <- mget(hide_correct_order)

  # --- # hide_: return!
  return(hide_output_list_objects)


}


# # # Object names exactly in proccesing order
ObjNames_ProcOrder_test001_anova <- function(selected_fn = test001_anova_full_gen01){

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



take_code_test001_anova <- function(selected_fn){


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

# # # Show me your code
showme_your_code_test001_anova <- function(intro_source_database, vr_var_name, factor_var_name, alpha_value){

  original_file_source <- intro_source_database$file_source

  #--- Librerias
  section01_general_libreries <- '
  # # # # # Section 01 - Libraries -----------------------------------------------
  library("agricolae") # Tukey test
  library("dplyr")     # Developing with %>%
  library("openxlsx")  # Import files from xlsx
  library("plotly")    # Advanced graphical functions
'


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
  if(original_file_source == "xlsx"){
    section02_SELECTED <- section02_database_s01_import_excel_files

    section02_SELECTED <- gsub(pattern = "_selected_xlsx_file_" ,
                               replacement =  intro_source_database$file_name,
                               x = section02_SELECTED)

    section02_SELECTED <- gsub(pattern = "_selected_sheet_" ,
                               replacement =  intro_source_database$selected_sheet,
                               x = section02_SELECTED)

  }

  if(original_file_source == "R_example"){

    section02_SELECTED <- section02_database_s02_R_example

    section02_SELECTED <- gsub(pattern = "_selected_R_database_" ,
                               replacement =  intro_source_database$file_name,
                               x = section02_SELECTED)

  }
  ##############################################################################




  #--- Var selection
  section03_varselection <- '
  # # # # # Section 03 - Import database from excel file -------------------------
  vr_var_name <- "_selected_vr_var_name_"
  factor_var_name <- "_selected_factor_var_name_"
  alpha_value <- _selected_alpha_value_
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


  #--- Original test code
  test_code <- take_code_test001_anova(selected_fn = test001_anova_full_gen01)




  code_plot001 <- take_code_test001_anova(selected_fn = test001_anova_plot001)
  code_plot002 <- take_code_test001_anova(selected_fn = test001_anova_plot002)
  code_plot003 <- take_code_test001_anova(selected_fn = test001_anova_plot003)
  code_plot004 <- take_code_test001_anova(selected_fn = test001_anova_plot004)
  code_plot005 <- take_code_test001_anova(selected_fn = test001_anova_plot005)
  code_plot006 <- take_code_test001_anova(selected_fn = test001_anova_plot006)


  # # # # # Section 14 - Plots ---------------------------------------------------

  output_code <- c(section01_general_libreries,
                   section02_SELECTED,
                   section03_varselection,
                   test_code,
                   "\n\n\n\n\n",
                   " # # # # # Section 14 - Plots ---------------------------------------------------\n",
                   "",
                   code_plot001,
                   "\n\n\n\n\n",
                   code_plot002,
                   "\n\n\n\n\n",
                   code_plot003,
                   "\n\n\n\n\n",
                   code_plot004,
                   "\n\n\n\n\n",
                   code_plot005,
                   "\n\n\n\n\n",
                   code_plot006,
                   "\n\n\n\n\n")

  output_code <- paste0(output_code)

  return(output_code)

}





test001_anova_plot001 <- function(minibase_mod, df_factor_info){

  # # # Create a new plot...
  plot001_anova <- plotly::plot_ly()

  # # # Plot001 - Scatter plot for VR and FACTOR on minibase_mod *****************
  plot001_anova <- plotly::add_trace(p = plot001_anova,
                                     type = "scatter",
                                     mode = "markers",
                                     x = minibase_mod$FACTOR,
                                     y = minibase_mod$VR,
                                     color = minibase_mod$FACTOR,
                                     colors = df_factor_info$color,
                                     marker = list(size = 15, opacity = 0.7))

  # # # Title and settings...
  plot001_anova <-   plotly::layout(p = plot001_anova,
                                    title = "Plot 001 - Scatterplot",
                                    font = list(size = 20),
                                    margin = list(t = 100))


  # # # Without zerolines
  plot001_anova <-   plotly::layout(p = plot001_anova,
                                    xaxis = list(zeroline = FALSE),
                                    yaxis = list(zeroline = FALSE))


  # # # Plot output
  plot001_anova

}





test001_anova_plot002 <- function(df_plot002_table){


  # # # Create a new plot...
  plot002_anova <- plot_ly()


  # # # Adding errors...
  plot002_anova <-   add_trace(p = plot002_anova,
                               type = "scatter",
                               mode = "markers",
                               x = df_plot002_table$level,
                               y = df_plot002_table$mean,
                               color = df_plot002_table$level,
                               colors = df_plot002_table$color,
                               marker = list(symbol = "line-ew-open",
                                             size = 50,
                                             opacity = 1,
                                             line = list(width = 5)),
                               error_y = list(value = df_plot002_table$model_error_sd)
  )


  # # # Title and settings...
  plot002_anova <- plotly::layout(p = plot002_anova,
                                  title = "Plot 002 - Mean and model standard deviation",
                                  font = list(size = 20),
                                  margin = list(t = 100))

  # # # Without zerolines
  plot002_anova <-plotly::layout(p = plot002_anova,
                                 xaxis = list(zeroline = FALSE),
                                 yaxis = list(zeroline = FALSE))

  # # # Plot output
  plot002_anova
}






test001_anova_plot003 <- function(df_plot003_table){


  # # # Create a new plot...
  plot003_anova <- plotly::plot_ly()


  # # # Adding errors...
  plot003_anova <-   plotly::add_trace(p = plot003_anova,
                                       type = "scatter",
                                       mode = "markers",
                                       x = df_plot003_table$level,
                                       y = df_plot003_table$mean,
                                       color = df_plot003_table$level,
                                       colors = df_plot003_table$color,
                                       marker = list(symbol = "line-ew-open",
                                                     size = 50,
                                                     opacity = 1,
                                                     line = list(width = 5)),
                                       error_y = list(value = df_plot003_table$model_error_se)
  )


  # # # Title and settings...
  plot003_anova <- plotly::layout(p = plot003_anova,
                                  title = "Plot 003 - Mean y model standard error",
                                  font = list(size = 20),
                                  margin = list(t = 100))

  # # # Without zerolines
  plot003_anova <-plotly::layout(p = plot003_anova,
                                 xaxis = list(zeroline = FALSE),
                                 yaxis = list(zeroline = FALSE))

  # # # Plot output
  plot003_anova
}










test001_anova_plot004 <- function(df_plot004_table){


  # # # New plotly...
  plot004_anova <- plotly::plot_ly()

  # # # Boxplot and info...
  plot004_anova <- plotly::add_trace(p = plot004_anova,
                                     type = "box",
                                     x = df_plot004_table$level ,
                                     color = df_plot004_table$level,
                                     colors = df_plot004_table$color,
                                     lowerfence = df_plot004_table$min,
                                     q1 = df_plot004_table$Q1,
                                     median = df_plot004_table$median,
                                     q3 = df_plot004_table$Q3,
                                     upperfence = df_plot004_table$max,
                                     boxmean = TRUE,
                                     boxpoints = FALSE,
                                     line = list(color = "black", width = 3)
  )

  # # # Title and settings...
  plot004_anova <- plotly::layout(p = plot004_anova,
                                  title = "Plot 004 - Boxplot and means",
                                  font = list(size = 20),
                                  margin = list(t = 100))


  # # # Without zerolines...
  plot004_anova <- plotly::layout(p = plot004_anova,
                                  xaxis = list(zeroline = FALSE),
                                  yaxis = list(zeroline = FALSE))

  # # # Output plot003_anova...
  plot004_anova
}



test001_anova_plot005 <- function(minibase_mod, df_plot005_table){



  all_levels <- levels(minibase_mod[,2])
  n_levels <- length(all_levels)
  all_color <- rainbow(length(all_levels))



  plot005_anova <- plot_ly()

  # Violinplot
  for (k in 1:n_levels){

    # Selected values
    selected_level <- all_levels[k]
    selected_color <- all_color[k]
    dt_filas <- minibase_mod[,2] == selected_level

    # Plotting selected violinplot
    plot005_anova <- plot005_anova %>%
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
  plot005_anova <- plotly::add_trace(p = plot005_anova,
                                     type = "box",
                                     name = "boxplot",
                                     x = df_plot005_table$level ,
                                     color = df_plot005_table$level ,
                                     lowerfence = df_plot005_table$min,
                                     q1 = df_plot005_table$Q1,
                                     median = df_plot005_table$median,
                                     q3 = df_plot005_table$Q3,
                                     upperfence = df_plot005_table$max,
                                     boxmean = TRUE,
                                     boxpoints = TRUE,
                                     fillcolor = df_plot005_table$color,
                                     line = list(color = "black", width = 3),
                                     opacity = 0.5,
                                     width = 0.2)


  # # # Title and settings...
  plot005_anova <- plotly::layout(p = plot005_anova,
                                  title = "Plot 005 - Violinplot",
                                  font = list(size = 20),
                                  margin = list(t = 100))


  # # # Without zerolines...
  plot005_anova <- plotly::layout(p = plot005_anova,
                                  xaxis = list(zeroline = FALSE),
                                  yaxis = list(zeroline = FALSE))

  # # # Output plot003_anova...
  plot005_anova


}


test001_anova_plot006 <- function(minibase_mod, df_plot006_table){




  #library(plotly)
  plot006_anova <- plotly::plot_ly()

  # Add traces
  plot006_anova <- plotly::add_trace(p = plot006_anova,
                                     type = "violin",
                                     y = minibase_mod$VR,
                                     x = minibase_mod$FACTOR,
                                     showlegend = TRUE,
                                     side = "positive",
                                     points = "all",
                                     name = "Violinplot",
                                     color = minibase_mod$FACTOR,
                                     colors = df_plot006_table$color)



  # # # Title and settings...
  plot006_anova <- plotly::layout(p = plot006_anova,
                                  title = "Plot 006 - Scatterplot + Jitter +  Smoothed",
                                  font = list(size = 20),
                                  margin = list(t = 100))


  # # # Without zerolines...
  plot006_anova <- plotly::layout(p = plot006_anova,
                                  xaxis = list(zeroline = FALSE),
                                  yaxis = list(zeroline = FALSE))

  # # # Output plot003_anova...
  plot006_anova


}





test001_anova_plot007 <- function(minibase_mod, df_factor_info){

  # # # Create a new plot...
  plot007_anova <- plotly::plot_ly()

  # # # Plot001 - Scatter plot for VR and FACTOR on minibase_mod *****************
  plot007_anova <- plotly::add_trace(p = plot007_anova,
                                     type = "scatter",
                                     mode = "markers",
                                     x = minibase_mod$FACTOR,
                                     y = minibase_mod$residuals,
                                     color = minibase_mod$FACTOR,
                                     colors = df_factor_info$color,
                                     marker = list(size = 15, opacity = 0.7))

  # # # Title and settings...
  plot007_anova <-   plotly::layout(p = plot007_anova,
                                    title = "Plot 007 - Scatterplot - Residuals",
                                    font = list(size = 20),
                                    margin = list(t = 100))


  # # # Without zerolines
  plot007_anova <-   plotly::layout(p = plot007_anova,
                                    xaxis = list(zeroline = FALSE),
                                    yaxis = list(zeroline = TRUE))


  # # # Plot output
  plot007_anova

}


test001_anova_plot008 <- function(minibase_mod, df_plot006_table){




  #library(plotly)
  plot006_anova <- plotly::plot_ly()

  # Add traces
  plot006_anova <- plotly::add_trace(p = plot006_anova,
                                     type = "violin",
                                     y = minibase_mod$residuals,
                                     x = minibase_mod$FACTOR,
                                     showlegend = TRUE,
                                     side = "positive",
                                     points = "all",
                                     name = "Violinplot",
                                     color = minibase_mod$FACTOR,
                                     colors = df_plot006_table$color)



  # # # Title and settings...
  plot006_anova <- plotly::layout(p = plot006_anova,
                                  title = "Plot 008 - Resisualds - Scatterplot + Jitter +  Smoothed",
                                  font = list(size = 20),
                                  margin = list(t = 100))


  # # # Without zerolines...
  plot006_anova <- plotly::layout(p = plot006_anova,
                                  xaxis = list(zeroline = FALSE),
                                  yaxis = list(zeroline = FALSE))

  # # # Output plot003_anova...
  plot006_anova


}


test001_anova_plot009 <- function(minibase_mod, df_factor_info){

  # # # Create a new plot...
  plot007_anova <- plotly::plot_ly()

  # # # Plot001 - Scatter plot for VR and FACTOR on minibase_mod *****************
  plot007_anova <- plotly::add_trace(p = plot007_anova,
                                     type = "scatter",
                                     mode = "markers",
                                     x = minibase_mod$fitted.values,
                                     y = minibase_mod$residuals,
                                     color = minibase_mod$FACTOR,
                                     colors = df_factor_info$color,
                                     marker = list(size = 15, opacity = 0.7))

  # # # Title and settings...
  plot007_anova <-   plotly::layout(p = plot007_anova,
                                    title = "Plot 009 - Scatterplot - Residuals vs Fitted.values",
                                    font = list(size = 20),
                                    margin = list(t = 100))


  # # # Without zerolines
  plot007_anova <-   plotly::layout(p = plot007_anova,
                                    xaxis = list(zeroline = FALSE),
                                    yaxis = list(zeroline = TRUE))


  # # # Plot output
  plot007_anova

}



test001_anova_plot010 <- function(minibase_mod, df_factor_info){

  # # # Create a new plot...
  plot007_anova <- plotly::plot_ly()

  # # # Plot001 - Scatter plot for VR and FACTOR on minibase_mod *****************
  plot007_anova <- plotly::add_trace(p = plot007_anova,
                                     type = "scatter",
                                     mode = "markers",
                                     x = minibase_mod$FACTOR,
                                     y = minibase_mod$studres,
                                     color = minibase_mod$FACTOR,
                                     colors = df_factor_info$color,
                                     marker = list(size = 15, opacity = 0.7))

  # # # Title and settings...
  plot007_anova <-   plotly::layout(p = plot007_anova,
                                    title = "Plot 010 - Scatterplot - Studentized Residuals",
                                    font = list(size = 20),
                                    margin = list(t = 100))


  # # # Without zerolines
  plot007_anova <-   plotly::layout(p = plot007_anova,
                                    xaxis = list(zeroline = FALSE),
                                    yaxis = list(zeroline = TRUE))


  # # # Plot output
  plot007_anova

}



test001_anova_plot011 <- function(minibase_mod, df_plot006_table){




  #library(plotly)
  plot006_anova <- plotly::plot_ly()

  # Add traces
  plot006_anova <- plotly::add_trace(p = plot006_anova,
                                     type = "violin",
                                     x = minibase_mod$residuals,
                                     #x = minibase_mod$FACTOR,
                                     showlegend = TRUE,
                                     side = "positive",
                                     points = "all",
                                     name = "Violinplot")#
  #color = minibase_mod$FACTOR,
  #colors = df_plot006_table$color)



  # # # Title and settings...
  plot006_anova <- plotly::layout(p = plot006_anova,
                                  title = "Plot 011 - Residuals",
                                  font = list(size = 20),
                                  margin = list(t = 100))


  # # # Without zerolines...
  plot006_anova <- plotly::layout(p = plot006_anova,
                                  xaxis = list(zeroline = TRUE),
                                  yaxis = list(zeroline = FALSE))

  # # # Output plot003_anova...
  plot006_anova


}



test001_anova_plot012 <- function(minibase_mod, df_plot006_table){




  #library(plotly)
  plot006_anova <- plotly::plot_ly()

  # Add traces
  plot006_anova <- plotly::add_trace(p = plot006_anova,
                                     type = "violin",
                                     x = minibase_mod$studres,
                                     #x = minibase_mod$FACTOR,
                                     showlegend = TRUE,
                                     side = "positive",
                                     points = "all",
                                     name = "Violinplot")#
  #color = minibase_mod$FACTOR,
  #colors = df_plot006_table$color)



  # # # Title and settings...
  plot006_anova <- plotly::layout(p = plot006_anova,
                                  title = "Plot 012 - Studentized Residuals",
                                  font = list(size = 20),
                                  margin = list(t = 100))


  # # # Without zerolines...
  plot006_anova <- plotly::layout(p = plot006_anova,
                                  xaxis = list(zeroline = TRUE),
                                  yaxis = list(zeroline = FALSE))

  # # # Output plot003_anova...
  plot006_anova


}



