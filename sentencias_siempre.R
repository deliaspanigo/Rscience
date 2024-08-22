


library("devtools")

# openxlsx::read.xlsx()

devtools::load_all()

devtools::document()

devtools::check()

devtools::install()

devtools::release()
###############################################

# Desinstalar Rscience
remove.packages("Rscience")

# Instalar el paquete para instalar
install.packages("remotes")

# Activar la libreria para instalar
library("remotes")

# Instalar Rscience desde la librería desde GitHub
remotes::install_github("deliaspanigo/Rscience", force = T)

Rscience::app_01_Rscience()

