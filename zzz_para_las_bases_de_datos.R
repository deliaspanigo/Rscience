# PAra las bases de datos
# https://r-pkgs.org/data.html

library("devtools")
library("usethis")
library("openxlsx")

selected_file <- "Ejer11_06_ClusterContinuo.xlsx"
new_name <- "Rscience_df_bio02_Eje11_06_ClusterContinuo.rda"
Rscience_df_bio02_Eje11_06_ClusterContinuo <-  openxlsx::read.xlsx(xlsxFile = selected_file, sheet = 1)


usethis::use_data(Rscience_df_bio02_Eje11_06_ClusterContinuo)
