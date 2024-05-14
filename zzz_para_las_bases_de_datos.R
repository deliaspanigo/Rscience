# PAra las bases de datos
# https://r-pkgs.org/data.html

library("devtools")
library("usethis")
library("openxlsx")

selected_folder <- "./inst/extdata/"
selected_file <- "Rscience_bio01_base07_espermograma.xlsx"
full_path <- paste0(selected_folder, selected_file)

new_name <- "Rscience_bio01_base07_espermograma.rda"

Rscience_bio01_base07_espermograma <-  openxlsx::read.xlsx(xlsxFile = full_path, sheet = 1)


usethis::use_data(Rscience_bio01_base07_espermograma)
