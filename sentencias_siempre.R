


library("devtools")

# openxlsx::read.xlsx()

devtools::load_all()

devtools::document()

devtools::check()

devtools::install()

###############################################

remove.packages("Rscience")
# Libreria
library(remotes)

# Instalar la librería desde GitHub
remotes::install_github("deliaspanigo/Rscience", force = T)

