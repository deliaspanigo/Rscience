# PAra las bases de datos
# https://r-pkgs.org/data.html

library("devtools")
library("usethis")
library("openxlsx")

selected_file <- "Ejer116_PCA2.xlsx"
new_name <- "Rscience_df_bio02_PCA03.rda"
Rscience_df_bio02_PCA03 <-  openxlsx::read.xlsx(xlsxFile = selected_file, sheet = 1)


usethis::use_data(Rscience_df_bio02_PCA03)
