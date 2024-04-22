# Todas las regresiones posibles


amount_x_vars <- length(vector_new_name_x)

status_var <- lapply(1:amount_reg_vars, function(x){ c(F,T) })
names(status_var) <- vector_new_name_x

# All combinations
df_logic_model <- expand.grid(status_var)
total_vars <- rowSums(df_logic_model)

# Ordering combinations from NULL model to FULL model
df_logic_model <- df_logic_model[order(total_vars, decreasing = FALSE), ]
total_vars <- rowSums(df_logic_model)

# Total models
total_models <- nrow(df_logic_model)
vector_pos_model <- 1:total_models

#x <- 1
list_row <- sapply(vector_pos_model, function(x){


  print(paste0("Model ", x, " of ", total_models))

  dt_logic_x_vars <- as.vector(as.matrix(df_logic_model[x,]))
  selected_x_vars <- colnames(df_logic_model)[dt_logic_x_vars]
  amount_x_vars <- length(selected_x_vars)

  dt_model_vars <- c(T, dt_logic_x_vars)

  new_minibase <- database[vector_name_selected_vars[dt_model_vars]]
  colnames(new_minibase) <- vector_rol_vars[dt_model_vars]
  vector_logic_row <- as.logical(rowSums(is.na(new_minibase)))

  vector_logic_row
}, simplify = F)

vector_grupo_minibase <- rep(NA, length(list_row))

contador_letra <- 0
for(k1 in 1:(length(vector_grupo_minibase)-1)){
  for(k2 in (k1+1):length(vector_grupo_minibase)){

    if(is.na(vector_grupo_minibase[k1])){
      contador_letra <- contador_letra + 1
      new_letter <- int2col(contador_letra)
      vector_grupo_minibase[k1] <- new_letter

    }

    if(is.na(vector_grupo_minibase[k2])){
      dt_par <- identical(list_row[[k1]], list_row[[k2]])
      if(dt_par) vector_grupo_minibase[k2] <- vector_grupo_minibase[k1]

    }
  }
}

vector_n_minibase <- sapply(vector_pos_model, function(x){


  print(paste0("Model ", x, " of ", total_models))

  dt_logic_x_vars <- as.vector(as.matrix(df_logic_model[x,]))
  selected_x_vars <- colnames(df_logic_model)[dt_logic_x_vars]
  amount_x_vars <- length(selected_x_vars)

  dt_model_vars <- c(T, dt_logic_x_vars)

  new_minibase <- na.omit(database[vector_name_selected_vars[dt_model_vars]])
  colnames(new_minibase) <- vector_rol_vars[dt_model_vars]
  nrow(new_minibase)
})

vector_fusion_minibase <- paste0(vector_grupo_minibase, "-", vector_n_minibase)

list_lm_results <- sapply(vector_pos_model, function(x){

  print(paste0("Model ", x, " of ", total_models))

  dt_logic_x_vars <- as.vector(as.matrix(df_logic_model[x,]))
  selected_x_vars <- colnames(df_logic_model)[dt_logic_x_vars]
  amount_x_vars <- length(selected_x_vars)

  dt_model_vars <- c(T, dt_logic_x_vars)

  new_minibase <- na.omit(database[vector_name_selected_vars[dt_model_vars]])
  colnames(new_minibase) <- vector_rol_vars[dt_model_vars]


  lm_model <- lm(VR ~., data = new_minibase)



  tryCatch({
    # Ajustar el modelo lineal
    normality_test <- shapiro.test(lm_model$residuals)

    # Calcular el AIC del modelo
    #aic <- AIC(modelo)

    # Mostrar el valor de AIC
    #print(aic)
    normality_test
  }, error = function(e) {
    normality_test <- shapiro.test(mtcars[,1])
    for(k in 1:length(normality_test)) normality_test[[k]] <- NA
    # En caso de error, devolver NA
    normality_test
  })

  the_summary <- summary(lm_model)
  df_coefficients <- as.data.frame(the_summary$coefficients)

  value_AIC <- AIC(lm_model)
  value_ajusted_r2 <- the_summary$adj.r.squared
  value_BIC <- BIC(lm_model)
  n_rows_model <- nrow(new_minibase)


  #########################
  {
    plot001 <- plotly::plot_ly()

    plot001 <- add_trace(p = plot001,
                         x = lm_model$fitted.values,
                         y = lm_model$residuals,
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


    plot001 <- plotly::layout(p = plot001,
                              xaxis = list(title = "Fitted values"),
                              yaxis = list(title = "Residuals"),
                              title = "Plot 001 - Residuals vs. Fitted values",
                              font = list(size = 20),
                              margin = list(t = 100))



    plot001 <- plotly::layout(p = plot001,
                              xaxis = list(zeroline = FALSE),
                              yaxis = list(zeroline = TRUE))

    # Mostrar el gráfico interactivo
    #plot002
  }
  output_list <- Hmisc::llist(dt_logic_x_vars, selected_x_vars, amount_x_vars,
                              df_coefficients,
                              value_AIC, value_ajusted_r2, value_BIC,
                              n_rows_model, normality_test, plot001, lm_model)

  output_list
}, simplify = F)



vector_AIC <- lapply(list_lm_results, function(x){ x$value_AIC })
vector_AIC <- unlist(value_AIC)
vector_AIC


vector_ajusted_r2 <- lapply(list_lm_results, function(x){ x$value_ajusted_r2 })
vector_ajusted_r2 <- unlist(vector_ajusted_r2)
vector_ajusted_r2

vector_BIC <- lapply(list_lm_results, function(x){ x$value_BIC })
vector_BIC <- unlist(vector_BIC)
vector_BIC

vector_check_normality <- lapply(list_lm_results, function(x){
  selected_p <- x$normality_test$p.value
  check_normality <- FALSE
  if(selected_p >= alpha_value) check_normality <- TRUE
  check_normality
})

vector_check_normality <- unlist(vector_check_normality)
vector_check_normality

vector_amount_x_vars <- lapply(list_lm_results, function(x){ x$amount_x_vars })
vector_amount_x_vars <- unlist(vector_amount_x_vars)
vector_amount_x_vars

vector_n <- lapply(list_lm_results, function(x){ x$n_rows_model })
vector_n <- unlist(vector_n)
vector_n

vector_x_sig <- lapply(list_lm_results, function(x){
  vector_p_slops <- x$df_coefficients$`Pr(>|t|)`
  vector_p_slops <- vector_p_slops[-1]
  dt_dif <- vector_p_slops < alpha_value
  amount_dif <- sum(dt_dif)
  amount_dif
})

vector_x_sig <- unlist(vector_x_sig)
vector_x_sig

vector_const_sig <- lapply(list_lm_results, function(x){
  vector_p_const <- x$df_coefficients$`Pr(>|t|)`[1]
  dt_const_sig <- vector_p_const < alpha_value
  dt_const_sig
})

vector_const_sig <- unlist(vector_const_sig)
vector_const_sig


df_model_x_standard <- sapply(1:length(vector_new_name_x), function(x){

  selected_var_name <- vector_new_name_x[x]
  new_col <- df_logic_model[,x]
  new_col[new_col == FALSE] <- NA
  new_col[new_col == TRUE] <- selected_var_name
  new_col
})
df_model_x_standard <- as.data.frame(df_model_x_standard)
colnames(df_model_x_standard) <- vector_new_name_x
df_model_x_standard$"id_model" <- vector_pos_model
df_model_x_standard$"AIC" <- vector_AIC
df_model_x_standard$"ajusted_r2" <- vector_ajusted_r2
df_model_x_standard$"BIC" <- vector_BIC
df_model_x_standard$"norm_res" <- vector_check_normality
df_model_x_standard$"amount_x_vars" <- vector_amount_x_vars
df_model_x_standard$"amount_x_sig" <- vector_x_sig
df_model_x_standard$"check_x_ok" <- (vector_amount_x_vars - vector_x_sig) == 0
df_model_x_standard$"check_const_ok" <- vector_const_sig
df_model_x_standard$"n_rows" <- vector_n
df_model_x_standard$"group_minibase" <- vector_fusion_minibase

df_sel01 <- df_model_x_standard[df_model_x_standard$norm_res,]
df_sel01 <- df_sel01[order(df_sel01$AIC, decreasing = FALSE), ]
df_sel01

df_sel01_B <- df_sel01[df_sel01$check_x_ok,]
#df_sel01_B <- df_sel01[order(df_sel01$AIC, decreasing = FALSE), ]
df_sel01_B

df_sel02 <- df_model_x_standard[df_model_x_standard$check_normality,]
df_sel02 <- df_sel01[order(df_sel01$ajusted_r2, decreasing = TRUE), ]
df_sel02
