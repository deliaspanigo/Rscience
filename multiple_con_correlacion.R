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

check_residuals_normality <- test_residuals_normality$p.value >= alpha_value
if(check_residuals_normality) phrase01_model <- "The selected model meets the requirement of normality of the residuals." else
  phrase01_model <- "The selected model does NOT meet the requirement of normality of the residuals."


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
df_normality$"check_normality" <- df_normality$p.value >= df_normality$alpha.value
df_normality


df_homogeneity <- data.frame(
  "orden" = 1:length(list_homogeneity),
  "var01" = unlist(lapply(list_homogeneity, function(x){x$selected_vars[1]})),
  "var02" = unlist(lapply(list_homogeneity, function(x){x$selected_vars[2]})),
  "test" = "Homogeneity Test",
  "p.value" = unlist(purrr::map(list_homogeneity, ~ .x$"test_homogeneity"$"p.value")),
  "alpha.value" = alpha_value
)
df_homogeneity$"check_homogeneity" <- df_homogeneity$"p.value" >= df_homogeneity$"alpha.value"
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
df_cor_pearson$"check_cor" <- df_cor_pearson$"p.value" >= df_cor_pearson$"alpha.value"
df_cor_pearson


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
df_cor_spearman$"check_cor" <- df_cor_spearman$"p.value" >= df_cor_spearman$"alpha.value"
df_cor_spearman


df_cor_mix <- data.frame(
  "orden" = df_homogeneity$"orden",
  "var01" = df_homogeneity$"var01",
  "var02" = df_homogeneity$"var02",
  "check_normality_01" = df_normality[df_homogeneity$"var01", "check_normality"],
  "check_normality_02" = df_normality[df_homogeneity$"var02", "check_normality"],
  "check_homogeneity" = df_homogeneity$"check_homogeneity"
)
df_cor_mix$"check_req_pearson" <- rowSums(df_cor_mix[c(4,5,6)]) == 3
df_cor_mix$"cor_test" <- ifelse(test = df_cor_mix$"check_req_pearson",
                                 yes = df_cor_pearson$"cor_test",
                                  no = df_cor_spearman$"cor_test")

df_cor_mix$"cor_est" <- ifelse(test = df_cor_mix$"check_req_pearson",
                                yes = df_cor_pearson$"cor_est",
                                no = df_cor_spearman$"cor_est")

df_cor_mix$"p.value" <- ifelse(test = df_cor_mix$"check_req_pearson",
                                yes = df_cor_pearson$"p.value",
                                no = df_cor_spearman$"p.value")

df_cor_mix$"check_cor" <- ifelse(test = df_cor_mix$"check_req_pearson",
                               yes = df_cor_pearson$"check_cor",
                               no = df_cor_spearman$"check_cor")


check_all_cor <- sum(df_cor_mix$check_cor) == length(df_cor_mix)

if(check_all_cor ) phrase02_model <- "The selected model meets the requirement of no correlation between the regressor variables." else
  phrase02_model <- "The selected model does NOT meet the requirement of no correlation between the regressor variables."


