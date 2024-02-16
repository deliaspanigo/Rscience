
library(Rscience)

database <- mtcars
vr_var_name <- "mpg"
factor_var_name <- "cyl"
alpha_value <- 0.05

aver <- Rscience:::test001_anova_full_gen01(database, vr_var_name,factor_var_name, alpha_value)

minibase_mod <- aver$minibase_mod
df_plot003_table <- aver$df_plot003_table
