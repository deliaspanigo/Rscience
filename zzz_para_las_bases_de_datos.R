# PAra las bases de datos
# https://r-pkgs.org/data.html

library("devtools")
library("usethis")
library("openxlsx")

selected_file <- "Rscience_bio01_base08_espermograma.xlsx"
new_name <- "Rscience_bio01_base08_espermograma.rda"
Rscience_bio01_base08_espermograma <-  openxlsx::read.xlsx(xlsxFile = selected_file, sheet = 1)


usethis::use_data(Rscience_bio01_base08_espermograma)
