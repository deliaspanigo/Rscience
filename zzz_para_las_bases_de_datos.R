# PAra las bases de datos
# https://r-pkgs.org/data.html

library("devtools")
library("usethis")
library("openxlsx")

book.url <- "https://stat.ethz.ch/~meier/teaching/book-anova"
quality <- readRDS(url(file.path(book.url, "data/quality.rds")))
str(quality)

selected_folder <- "./inst/extdata/"
selected_file <- "Rscience_bio01_base07_espermograma.xlsx"
full_path <- paste0(selected_folder, selected_file)

new_name <- "Rscience_bio02_anova2way_mixed.rda"

Rscience_bio02_anova2way_mixed <-  Machines #openxlsx::read.xlsx(xlsxFile = full_path, sheet = 1)


usethis::use_data(Rscience_bio02_anova2way_mixed)
