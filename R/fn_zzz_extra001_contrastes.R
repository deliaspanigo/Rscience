
# 01) OK!
amount_contrasts_theoretical <- function(k){

  # Esta funcion lo que hace es calcular la cantidad total de
  # contrastes que se puede generar (sean o no ortogonales).
  # Esta no es la cantidad maximizada, sino la cantidad total total de
  # grupos que se pueden armar.
  # Basicamnete es la combinatoria completa para 3 elementos, menos
  # el vector nulo, menos todas las combinaciones que solo tienen 0 y 1
  #
  part_A <- 3^k        # Total de combinaciones para 3
  part_B <- 1          # El vector nulo
  part_C <- (2^k) -1   # Total de vectores que solo tienen 0 y/o 1 (excluyendo al vector nulo que ya lo sacamos)
  part_D <- (2^k) -1   # Total de vectores que solo tienen 0 y/o -1 (excluyendo al vector nulo que ya lo sacamos)
  el_divisor <- 2      # Lo que queda va dividido por 2 por los vectores "opuestos"
  # que llevan a la misma comparación. Ej: c(1, -1) y c(-1, 1)

  total_comb <- (part_A - part_B  - part_C - part_D)/el_divisor
  total_comb

}

max_amount_orthogonal_contrasts_theoretical <- function(k){

  # Detalla la cantidad maxima de vectores ortogonales
  # que pueden tomarse de manera simultanea.

  k-1

}

total_amount_maximized_subsets_theoretical <- function(k){

  # Teniendo el total de contrastes posibles, y sabiendo que
  # puedo tomar k-1 vectores (ortogonales o no) de ese total, ¿de cuantas formas
  # puedo tomar k-1 vectores del total?

  total_contrasts <- amount_contrasts_theoretical(k)
  max_theoretical <- max_amount_orthogonal_contrasts_theoretical(k)

  choose(total_contrasts, max_theoretical)

}

#################################################################

# 02) OK!
gen_list_peso_cantidad <- function(k){

  # Esta funcion genera una lista de "peso" y "cantidad"
  # para luego con eso componer los vectores principales de k.

  # Determinamos la cantidad de vectores principales
  cantidad_vectores_principales <- floor(k/2)
  vector_orden_vectores_principales <- 1:cantidad_vectores_principales

  # Estimamos los coeficientes minimo y maximo posibles para
  # el grupo positivo.
  coef_max_teorico <- k-1
  coef_min_teorico <- floor(k/2)
  vector_coef_teorico <- coef_max_teorico:coef_min_teorico

  selected_coef <- vector_coef_teorico[vector_orden_vectores_principales]
  coef_min_real <- selected_coef[1]
  coef_max_real <- tail(selected_coef, 1)

  # Peso del grupo 1 en cada vector
  vector_peso_g01     <- coef_min_real:coef_max_real
  vector_cantidad_g01 <- vector_orden_vectores_principales

  # Peso del grupo 2 en cada vector
  vector_peso_g02     <- vector_peso_g01 - k
  vector_cantidad_g02 <- k - vector_cantidad_g01

  # Check general
  sum_g01 <- vector_peso_g01*vector_cantidad_g01
  sum_g02 <- vector_peso_g02*vector_cantidad_g02
  sum_vector <-  sum_g01 + sum_g02
  check_general01 <- sum(sum_vector == 0) == length(sum_vector)

  list_info_pv <- lapply(vector_orden_vectores_principales, function(x){

    vector_peso <- c(vector_peso_g01[x], vector_peso_g02[x])

    vector_cantidad <- c(vector_cantidad_g01[x], vector_cantidad_g02[x])

    output_list <- list(vector_peso, vector_cantidad)
    names(output_list) <- c("vector_peso", "vector_cantidad")

    output_list

  })
  names(list_info_pv) <- paste0("vp_", vector_orden_vectores_principales)

  output_list <- list(cantidad_vectores_principales,
                      list_info_pv)
  names(output_list) <- c("cantidad_vectores_principales", "list_info_pv")

  return(output_list)
}

#################################################################
# 03) OK!
gen_mat_principal_vectors <- function(k){

  # Esta funcion genera la matriz que posee los vectores principales
  # de "k".

  # Determinamos la cantidad de vectores principales

  list_info <- gen_list_peso_cantidad(k)

  list_info_pv <- list_info[["list_info_pv"]]

  matrix_vectores_principales <- sapply(list_info_pv, function(x){

    vector_peso     <- x[["vector_peso"]]
    vector_cantidad <- x[["vector_cantidad"]]


    output_vector <- rep(vector_peso, vector_cantidad)

    output_vector

  })


  return(matrix_vectores_principales)

}

###################################################################

# 04) OK!
vector_amount_maximized_orthogonal_subsets_theoretical <- function(k){

  # Esta funcion otorga la cantidad de subcojuntos ortogonales
  # que aporta cada vector principal de "k".

  list_full <- operation_it_amount(k)

  list_full$"list_op_vector"[[k]]

}

total_amount_maximized_orthogonal_subsets_theoretical <- function(k){

  # Esta funcion otorga la cantidad de subcojuntos ortogonales
  # que aportan en total todos los vectores principales de "k".

  list_full <- operation_it_amount(k)

  list_full$"list_op_suma"[[k]]

}

total_amount_maximized_non_orthogonal_subsets_theoretical <- function(k){

  # Esta funcion otorga la cantidad de subcojuntos NO ortogonales
  # que se pueden calcular.
  total_general <-    total_amount_maximized_subsets_theoretical(k)
  total_orthogonal <- total_amount_maximized_orthogonal_subsets_theoretical(k)
  total_non_orthogonal <- total_general - total_orthogonal

  return(total_non_orthogonal)

}
#################################################################

# 05) OK!
operation_it_tree <- function(k){

  # Esta funcion lo que hace es crear todos los vectores principales
  # desde 1 hasta k.

  list_op_tree <- as.list(rep(NA, k))
  list_op_tree[[1]] <- NA
  if(k == 1) return(list_op_tree)

  all_k <- 2:k

  for(new_k in all_k){

    list_op_tree[[new_k]] <- gen_mat_principal_vectors(new_k)
    vector_old_names <- colnames(list_op_tree[[new_k]])
    vector_new_names <- paste0("k", new_k, "_", vector_old_names)
    colnames(list_op_tree[[new_k]]) <- vector_new_names
  }

  return(list_op_tree)
}

operation_it_tree_mod <- function(k){

  # Esta funcion lo que hace es completar el arbol de vectores
  # principales con ceros para "k".


  list_principal_vectors <- operation_it_tree(k)
  vector_pos <- 1:length(list_principal_vectors)

  list_principal_vectors_mod <- sapply(vector_pos, function(x){

    if(x == 1) return(matrix(NA, 1, 1))

    selected_mat <- list_principal_vectors[[x]]
    amount_zero <- k - nrow(selected_mat)
    new_mat_rows <- matrix(0, amount_zero, ncol(selected_mat))
    new_output <- rbind(new_mat_rows, selected_mat)
    new_output
  })

  list_principal_vectors_mod
}

operation_it_pos_perm <- function(k){

  # Esta funcion lo que hace es determinar la posicion
  # de cada elemento de cada vector principal en cada permutacion posible
  # de cada vector respecto al vector principal inicial.

  list_principal_vectors_orig <- operation_it_tree(k)
  vector_pos <- 1:length(list_principal_vectors_orig)

  list_perm <- list()
  list_perm[[1]] <- list()
  list_perm[[1]][[1]] <- NA

  vector_pos01 <- 2:length(list_principal_vectors_orig)
  for(key01 in vector_pos01){

    list_perm[[key01]] <- list()
    for(key02 in 1:ncol(list_principal_vectors_orig[[key01]])){

      list_perm[[key01]][[key02]] <- list()
      selected_vector <- list_principal_vectors_orig[[key01]][,key02]
      mat_perm <- gen_mat_unique_permutations_without_opposites_fv(selected_vector)

      vector_pos_col <- 1:ncol(mat_perm)
      mat_pos_perm <- sapply(vector_pos_col, function(x){
        new_pos <- c()
        vector_ref <- selected_vector
        vector_new <- mat_perm[,x]

        for(j in 1:length(vector_new)){
          pos_j <- match(vector_new[j], vector_ref)
          vector_ref[pos_j] <- NA
          new_pos <- c(new_pos, pos_j)
        }
        new_pos
      })

      list_perm[[key01]][[key02]] <- mat_pos_perm
    }
  }

  return(list_perm)

}

operation_it_amount <- function(selected_k){

  # Esta funcion lo que hace es calcular la cantidad de permutaciones
  # posibles de cada vector principal desde 1 hasta k.

  list_op_vector <- as.list(rep(NA, selected_k))
  list_op_suma   <- as.list(rep(NA, selected_k))
  list_op_vector[[1]] <- 1
  list_op_suma[[1]]   <- 1
  list_op_vector[[2]] <- 1
  list_op_suma[[2]]   <- 1

  ##########################################################
  output_list <- list(list_op_vector, list_op_suma)
  names(output_list) <- c("list_op_vector", "list_op_suma")
  ###########################################################
  if(selected_k == 1) return(output_list)
  if(selected_k == 2) return(output_list)
  #if(selected_k == 1) return(list_op_suma[[1]])
  #if(selected_k == 2) return(list_op_suma[[2]])

  all_k <- 3:selected_k

  for(new_k in all_k){

    full_list <- gen_list_peso_cantidad(new_k)

    cantidad_vectores_principales <- full_list$"cantidad_vectores_principales"
    vector_orden_vectores_principales <- 1:cantidad_vectores_principales
    list_peso_cantidad <- full_list$list_info_pv

    aver <- sapply(1:length(list_peso_cantidad), function(y){
      x <- list_peso_cantidad[[y]]
      vector_peso     <- x$"vector_peso"
      vector_cantidad <- x$"vector_cantidad"

      g01_cantidad <- vector_cantidad[1]
      g02_cantidad <- vector_cantidad[2]
      longitud_total <- g01_cantidad + g02_cantidad
      check_especial <- g01_cantidad == g02_cantidad

      #####################################################
      factorial_total <- factorial(longitud_total)
      factorial_g01   <- factorial(g01_cantidad)
      factorial_g02   <- factorial(g02_cantidad)
      numero_comb <- factorial_total/(factorial_g01*factorial_g02)
      if(check_especial) numero_comb <- numero_comb/2
      ###############################################################################
      if(is.na(list_op_suma[[g01_cantidad]])){
        list_op_vector[[g01_cantidad]] <- operation_it(g01_cantidad)
        list_op_suma[[g01_cantidad]]   <- sum(list_op_vector[[g01_cantidad]])
      }

      if(is.na(list_op_suma[[g02_cantidad]])){
        list_op_vector[[g02_cantidad]] <- operation_it(g02_cantidad)
        list_op_suma[[g02_cantidad]]   <- sum(list_op_vector[[g02_cantidad]])
      }
      ################################################################################
      part01 <- numero_comb
      part02 <- list_op_suma[[g01_cantidad]]
      part03 <- list_op_suma[[g02_cantidad]]

      sub_total <- part01 * part02 * part03
      names(sub_total) <- NULL

      sub_total

    })

    aver <- unlist(aver)
    #names(aver) <- names(list_peso_cantidad)
    list_op_vector[[new_k]] <- aver
    list_op_suma[[new_k]]   <- sum(aver)
  }

  output_list <- list(list_op_vector, list_op_suma)
  names(output_list) <- c("list_op_vector", "list_op_suma")

  return(output_list)
}

operation_it_ngroups <- function(k){

  # Esta funcion determina la cantidad de grupos que tiene cada vector principal.
  list_principal_vectors <- operation_it_tree(k)

  list_ngroups <- list()

  output_list <-   lapply(list_principal_vectors, function(x){
    if(!is.matrix(x)) return(NA)

    matrix_cantidad <- apply(x, 2, table)
    rownames(matrix_cantidad) <- NULL
    matrix_cantidad
  })

  return(output_list)
}

#####################################################


#################################################################
# 06) OK!

gen_vec_dt_opposite_cols_fm <- function(selected_mat) {

  # Esta funcion determina la posicion de los vectores que son
  # opuestos a un vector seleccionado. La idea es que si tengo c(1, -1) y c(-1, 1)
  # me quede solo con el c(1, -1).
  # Aclaro que no dije "me quedo con el primero"... Sino que se va a fijar
  # que son opuestos, y de los opuestos se queda con el que tiene es valor
  # positivo en una posicion mas baja.


  # Número de columnas
  num_cols <- ncol(selected_mat)

  # Inicializar un vector para almacenar los índices de las columnas a conservar
  columnas_a_eliminar <- rep(FALSE, ncol(selected_mat))

  for(key01 in 1:(ncol(selected_mat)-1)){
    for(key02 in (key01+1):ncol(selected_mat)){

      vector01 <- selected_mat[,key01]
      vector02 <- selected_mat[,key02]
      if(identical(vector01, -vector02)){

        pos_max01 <- which.max(vector01)
        pos_max02 <- which.max(vector02)

        if(pos_max01 < pos_max02) columnas_a_eliminar[key02] <- TRUE else
          if(pos_max01 >= pos_max02) columnas_a_eliminar[key01] <- TRUE
      }

    }
  }



  columnas_a_conservar <- !columnas_a_eliminar

  return(columnas_a_conservar)
}

gen_mat_unique_permutations_without_opposites_fv <- function(selected_vector) {

  # Esta funcion genera todas las permutaciones posibles de un vector
  # eliminando los opuestos que llevarian al mismo contraste.
  # Si hubiera dos contrastes opuestos, se queda con el que tiene un valor
  # positivo en posiciones de filas mas bajas.

  # Obtener todas las permutaciones
  permutations <- combinat::permn(selected_vector)

  # Filtrar permutaciones únicas
  unique_permutations <- unique(permutations)

  # Convertir lista a matriz para fácil manipulación
  selected_mat <- do.call(cbind, unique_permutations)

  dt_ok <- gen_vec_dt_opposite_cols_fm(selected_mat)

  # Filtrar combinaciones opuestas
  output_matrix <- selected_mat[,dt_ok]

  if(is.vector(output_matrix)) dim(output_matrix) <- c(length(output_matrix), 1)
  # Devolver el resultado
  return(output_matrix)
}


###################################################################

# 07) OK!
amount_unique_permutations_without_opposites_fv2 <- function(selected_vector) {

  # Esta funcion calcula la cantidad de permutaciones posibles
  # de un vector sin tener en cuenta a los opuestos.
  # La idea de esto es que calcula el numero sin tener que obtener efectivamente
  # la matriz con toooodos los vectores.

  check_special_vector <- length(unique(abs(selected_vector))) == 1

  total_length <- length(selected_vector)
  factorial_total <- factorial(total_length)

  n_groups <- table(selected_vector)
  factorial_n_groups <- factorial(n_groups)
  prod_factorials <- prod(factorial_n_groups)

  total_perm <- factorial_total/prod_factorials

  if(check_special_vector) total_perm <- total_perm/2

  total_perm
  # Devolver el resultado
  return(total_perm)
}

##################################################################################

# 07) OK!
amount_maximized_orthogonal_subsets_fv2 <- function(selected_vector){

  # Esta funcion calcul la cantidad de subconjuntos maximizados ortogonales
  # que se pueden obtener para "k". La idea justamente es que hace el calculo
  # sin tener que tener en la mano los vectores.

  selected_k <- length(selected_vector)
  check_k_par <- (selected_k %% 2) == 0
  check_special_vector <- length(unique(abs(selected_vector))) == 1

  ref_vector <- c(1, -1)
  check_end01 <- length(ref_vector) == length(selected_vector)
  check_end02 <- FALSE
  check_end03 <- c()

  if(check_end01) check_end02 <- sum(selected_vector == ref_vector) == 2
  check_end03 <- sum(check_end01 + check_end02) == 2

  if(check_end03) return(1)

  #new_k <- selected_k - 1

  # selected_col <- 1
  # x <- matrix_vectores_principales[, selected_col]
  frecuencia_grupos <- table(selected_vector)
  df_detalles <- as.data.frame(frecuencia_grupos)
  colnames(df_detalles) <- c("peso", "cantidad")
  vector_cantidades <- df_detalles$cantidad

  # Combinaciones del vector seleccionado
  combinacion01 <- amount_unique_permutations_without_opposites_fv2(selected_vector)


  ################################################################

  # Combinaciones anidadas
  aver <- sapply(1:length(vector_cantidades), function(y){


    new_k <- vector_cantidades[y]
    if(new_k == 1) return(0)

    new_matrix <- gen_mat_principal_vectors(k = new_k)

    conteo_interno <- lapply(1:ncol(new_matrix), function(h){

      # h <- 1
      selected_new_vector <- new_matrix[,h]
      amount_maximized_orthogonal_subsets_fv2(selected_vector = selected_new_vector)
    })

    conteo_interno <- unlist(conteo_interno)
    conteo_interno
  })

  # vector_aver <- do.call(sum, aver)
  if(is.list(aver)) aver <- unlist(aver)
  combinacion02 <- sum(aver)
  ###############################################################

  #if(!check_special_vector){
  combinaciones_totales <- combinacion01*combinacion02
  #} else
  #  if(check_special_vector) combinaciones_totales <- combinacion02 + 1

  return(combinaciones_totales)

}


###################################################################
# 08) OK!
amount_maximized_orthogonal_subsets_fm2 <- function(selected_mat){

  # Esta funcion determina la cantidad total de subconjuntos
  # maximizados de contrastes ortogonales que se pueden generar a partir
  # de una matriz. La idea es ingresar la matriz que posee los vectores
  # principales de "k".

  apply(selected_mat, 2, amount_maximized_orthogonal_subsets_fv2)

}


total_amount_maximized_orthogonal_subsets_fm2 <- function(selected_mat){

  # Esta funcion calcula la cantidad total de subconjuntos maximizados de
  # contrastes ortogonales dado una matriz de vectores principales.

  vector_amount <- amount_maximized_orthogonal_subsets_fm2(selected_mat)
  total_sum <- sum(vector_amount)
  return(total_sum)
}

###################################################################

# 09) OK!
gen_list_all_contrasts <- function(k){

  # Esta funcion genera todos los vectores, siendo cada vector uno
  # de los posibles contrastes que se pueden generar.

  all_k <- k:2

  list_principal_vectors01 <- sapply(all_k, gen_mat_principal_vectors)
  if(is.matrix(list_principal_vectors01)) list_principal_vectors01 <- list(list_principal_vectors01)

  list_principal_vectors02 <- list()
  contador_externo <- 0
  for(key01 in 1:length(list_principal_vectors01)){
    for(key02 in 1:ncol(list_principal_vectors01[[key01]])){
      contador_externo <- contador_externo + 1
      list_principal_vectors02[[contador_externo]] <- list_principal_vectors01[[key01]][,key02]
      dim(list_principal_vectors02[[contador_externo]]) <- c(length(list_principal_vectors02[[contador_externo]]), 1)
    }
  }

  list_principal_vectors03 <- lapply(list_principal_vectors02, function(x){

    dif_rows <- k - nrow(x)

    if(dif_rows > 0){
      vector_0 <- rep(0, ncol(x))
      matrix_0 <- matrix(rep(vector_0, dif_rows), nrow = dif_rows, byrow = TRUE)

      x <- rbind(matrix_0, x)

    }

    return(x)

  })

  list_principal_vectors04 <- lapply(list_principal_vectors03, function(x){

    # x <- list_principal_vectors03[[1]]
    new_matrix <- apply(x, 2, gen_mat_unique_permutations_without_opposites_fv, simplify = F)
    new_matrix <- new_matrix[[1]]


    new_matrix
  })



  total_spected  <- amount_contrasts_theoretical(k)
  total_observed <- sum(unlist(lapply(list_principal_vectors04, ncol)))

  check_ok <- total_spected == total_observed
  if(!check_ok) print("Error in gen_list_all_contrasts()...")

  return(list_principal_vectors04)
}


gen_mat_all_contrasts <- function(k){

  # Esta funcion arma una matriz con todos los contrastes posibles.

  selected_list <- gen_list_all_contrasts(k)

  mat_principal_vertors_mod <- do.call(cbind, selected_list)

  total_spected  <- amount_contrasts_theoretical(k = k)
  total_observed <- ncol(mat_principal_vertors_mod)

  check_ok <- total_spected == total_observed
  if(!check_ok) print("Error in gen_mat_all_contrasts()...")

  return(mat_principal_vertors_mod)
}

###################################################################

# 10) OK!
check_mat_full_orthogonality <- function(selected_mat) {

  # Cada una matriz, lo que hace esta funcion es verificar
  # que toda las columnas de la matriz sean ortogonales todas entre si.
  # Devuelve un true o false.

  num_columnas <- ncol(selected_mat)

  # Inicializar matriz de productos punto
  productos_punto <- matrix(0, nrow = num_columnas, ncol = num_columnas)

  # Calcular productos punto entre todas las combinaciones de columnas
  for (i in 1:(num_columnas - 1)) {
    for (j in (i + 1):num_columnas) {
      productos_punto[i, j] <- sum(selected_mat[, i] * selected_mat[, j])
      productos_punto[j, i] <- productos_punto[i, j]  # La matriz es simétrica
    }
  }

  # Verificar orthogonalidad
  check_orthogonal <- all(productos_punto == 0)

  return(check_orthogonal)
}


###################################################################



# 11) OK!
operation_it_orthogonal_comb_master <- function(k){

  # Esta funcion lo que hace es devolver las "combinaciones maestras"
  # de vectores principales ortogonales modificados.
  # Las permutaciones ordenadas de estas matrices son las que permitiran
  # luego ir generando cada una de las combinaciones ortogonales posibles.

  # Vectores principales de cada nivel hasta k
  list_principal_vectors_orig <- operation_it_tree(k)

  list_principal_vectors_mod <- operation_it_tree_mod(k)

  list_principal_perm_orig <- operation_it_pos_perm(k)
  ##########################################(k)###########

  all_k <- 3:k

  list_comb_orto <- list()
  list_comb_orto[[1]] <- list()
  list_comb_orto[[1]][[1]] <- matrix(0, 1, 1)
  list_comb_orto[[2]] <- list()
  list_comb_orto[[2]][[1]] <- list_principal_vectors_orig[[2]]

  ##########################################(k)###########

  fn_cantidad_por_grupo <- function(selected_vector){

    cantidad_grupo <- table(selected_vector)
    new_order <- order(as.numeric(names(cantidad_grupo)), decreasing = T)
    cantidad_grupo <- cantidad_grupo[new_order]
    return(cantidad_grupo)
  }

  # k
  # pos_Combinacion orthogonal general

  for(key01 in all_k){
    list_comb_orto[[key01]] <- list()
    contador_externo <- 0
    for(key02 in 1:ncol(list_principal_vectors_orig[[key01]])) {

      selected_vector <- list_principal_vectors_orig[[key01]][,key02]
      vector_cantidad <- fn_cantidad_por_grupo(selected_vector)
      sub_k <- length(selected_vector)
      new_rows <- sub_k - vector_cantidad
      check_cantidad <- vector_cantidad == 1

      list_sub_pv <- list_comb_orto[vector_cantidad]

      list_sub_pv_mod <- list()
      vector_pos <- 1:length(list_sub_pv)

      for(key01_A in vector_pos){
        list_sub_pv_mod[[key01_A]] <- list()

        for(key01_B in 1:length(list_sub_pv[[key01_A]])){

          selected_mat <- list_sub_pv[[key01_A]][[key01_B, drop = FALSE]]
          mat_zero_row <- matrix(0, new_rows[key01_A], ncol(selected_mat))
          #mat_zero_row <- matrix(0, vector_cantidad[key01_A])
          if(key01_A == 1) mat_new <- rbind(selected_mat, mat_zero_row) else
            if(key01_A == 2) mat_new <- rbind(mat_zero_row, selected_mat)

          list_sub_pv_mod[[key01_A]][[key01_B]] <- mat_new
        }
      }


      list_fusion <- list()


      for(key02_A in 1:length(list_sub_pv_mod[[1]])){
        for(key02_B in 1:length(list_sub_pv_mod[[2]])){

          contador_externo <- contador_externo + 1

          mat_01 <- list_sub_pv_mod[[1]][[key02_A, drop = FALSE]]
          mat_02 <- list_sub_pv_mod[[2]][[key02_B, drop = FALSE]]

          check_mat_01 <- sum(unique(mat_01[,1]) != 0) != 0

          mat_fusion <- selected_vector
          if(check_mat_01) mat_fusion <- cbind(mat_fusion, mat_01)
          mat_fusion <- cbind(mat_fusion, mat_02)
          colnames(mat_fusion) <- NULL

          #list_fusion[[contador_externo]] <- mat_fusion
          list_comb_orto[[key01]][[contador_externo]]  <- mat_fusion
        }
      }

      # list_comb_orto[[key01]][[key02]] <- list_fusion
    }


  }



  return(list_comb_orto[[length(list_comb_orto)]])
}

###################################################################

# 12) OK!
operation_it_orthogonal_comb_full <- function(k){

  # Esta funcion lo que hace es generar todas las combinaciones maximizadas
  # ortogonales posibles que se pueden armar para "k".

  list_principal_vectors_orig <- operation_it_tree(k)
  list_principal_perm_orig <- operation_it_pos_perm(k)

  list_master <- operation_it_orthogonal_comb_master(k)


  list_pos_perm <- lapply(list_master, function(x){
    mat_pgs <-  apply(x, 2, function(y){

      new_vector <- y
      new_vector[new_vector == 0] <- NA
      new_vector <- na.omit(new_vector)
      sub_k <- length(new_vector)

      mat_pv <- list_principal_vectors_orig[[sub_k]]
      coincidencias <- apply(mat_pv, 2, function(z){
        all(z == new_vector)
      })
      pos_pv <- which(coincidencias)
      names(pos_pv) <- NULL

      n_comb <- ncol(list_principal_perm_orig[[sub_k]][[pos_pv]])
      vector_salida <- c(sub_k, pos_pv, n_comb)
      vector_salida
    })
    rownames(mat_pgs) <- c("sub_k", "pos_pv", "n_comb")
    mat_pgs
  })



  list_full_perm_master <- lapply(list_pos_perm, function(x){
    vector_n_comb <- x[3,]
    secuencias <- lapply(vector_n_comb, function(y) 1:y)
    combinaciones <- expand.grid(secuencias)
    vector_order <- order(combinaciones[,1])
    combinaciones <- combinaciones[vector_order, ]
    colnames(combinaciones) <- names(x)
    rownames(combinaciones) <- NULL
    combinaciones
  })




  list_full_orthogonal <- list()
  contador_externo <- 0

  for(key01 in 1:length(list_full_perm_master)){

    selected_master <- list_master[[key01]]
    initial_rows <- apply(selected_master, 2, function(x){
      vector_original <- x
      vector_pos01 <- 1:length(vector_original)
      dt_no_cero <- vector_original != 0

      vector_pos02 <- vector_pos01[dt_no_cero]
      vector_pos02
    })

    selected_pos_perm <- list_pos_perm[[key01]]
    selected_full_pos <- list_full_perm_master[[key01]]

    #mat_cero <- matrix(0, nrow(selected_master), ncol(selected_master))

    for(key02 in 1:nrow(list_full_perm_master[[key01]])){

      contador_externo <- contador_externo + 1



      vector_selected_k <- selected_pos_perm[1,]
      vector_pos_pv     <- selected_pos_perm[2,]
      vector_pos_comb   <- unlist(list_full_perm_master[[key01]][key02, ])

      mat_info <- rbind(vector_selected_k, vector_pos_pv, vector_pos_comb)
      rownames(mat_info) <- c("sub_k", "pos_pv", "pos_comb")

      list_new_rows01 <- apply(mat_info, 2, function(x){

        list_principal_perm_orig[[x[1]]][[x[2]]][,x[3]]
      })

      list_new_rows02 <- sapply(1:length(list_new_rows01), function(x){
        new_vector_pos <- initial_rows[[x]][list_new_rows01[[x]]]
        new_vector_pos
      })

      # list_new_rows03 <- sapply(1:length(list_new_rows02), function(x){
      #   #if(x == 1) return(list_new_rows02[[x]])
      #   new_vector_pos <- list_new_rows01[[1]][list_new_rows02[[x]]]
      #   new_vector_pos
      # })

      mat_mod <- selected_master
      vector_retro <- ncol(mat_mod):1

      for(key03 in vector_retro){

        vector_col <- max(vector_retro):key03
        selected_orig_rows <- initial_rows[[key03]]
        selected_mod_rows  <- list_new_rows02[[key03]]
        mat_mod[selected_orig_rows,vector_col] <- mat_mod[selected_mod_rows,vector_col]

      }

      list_full_orthogonal[[contador_externo]] <- mat_mod

    }

  }

  total_ok <- sum(unlist(lapply(list_full_orthogonal, check_mat_full_orthogonality)))
  check_all_ok <- total_ok == length(list_full_orthogonal)

  if(!check_all_ok) {
    stop("Error un list_full_orthogonal")
  }

  return(list_full_orthogonal)
}

###################################################################


info_general_contrastes <- function(k){

  # Esta funcion es lo que hace es calcular y reunir la informacion
  # util de colocar respecto a cantidad de contrastes y subconjuntos
  # maximizados ortogonales.

  output_list <- list()

  # 1) k seleccionado
  output_list[[1]] <- k
  names(output_list)[1] <- "k"

  # 2) Cantidad teorica de contrastes (TOTAL)
  output_list[[2]] <-   amount_contrasts_theoretical(k)
  names(output_list)[2] <- "amount_contrasts_theoretical"



  # 3) Cantidad maxima de contrastes ortogonales simultaneos teoricos
  output_list[[3]] <- max_amount_orthogonal_contrasts_theoretical(k)
  names(output_list)[3] <- "max_amount_orthogonal_contrasts_theoretical"

  # 4) Total de subconjuntos maximizados teoricos posibles
  output_list[[4]] <- total_amount_maximized_subsets_theoretical(k)
  names(output_list)[4] <- "total_amount_maximized_subsets_theoretical"

  # 5) Total de subconjuntos maximizados ortogonales teoricos posibles
  output_list[[5]] <- total_amount_maximized_orthogonal_subsets_theoretical(k)
  names(output_list)[5] <- "total_amount_maximized_orthogonal_subsets_theoretical"

  # 6) Total de subconjuntos maximizados no ortogonales teoricos posibles
  output_list[[6]] <- total_amount_maximized_non_orthogonal_subsets_theoretical(k)
  names(output_list)[6] <- "total_amount_maximized_non_orthogonal_subsets_theoretical"

  return(output_list)
}


info_acum_general_contrastes <- function(k){

  vector_k <- 2:k
  la_lista <- lapply(vector_k, function(x){
    as.data.frame(info_general_contrastes(x))
  })

  output_df <- do.call(rbind.data.frame, la_lista)
  output_df
}
