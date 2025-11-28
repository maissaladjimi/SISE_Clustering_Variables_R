# =============================================================================
# inst/shiny/app.R
# Point d'entrée de l'application Shiny
# =============================================================================

library(shiny)
library(DT)
library(ggplot2)

# =============================================================================
# CHARGER LE PACKAGE (mode développement)
# =============================================================================

if (!require("devtools")) install.packages("devtools")
devtools::load_all("../..")  # Charge toutes les classes R6 depuis R/

# =============================================================================
# CHARGER UI ET SERVER
# =============================================================================

source("ui.R")      # Charge l'objet "ui"
source("server.R")  # Charge la fonction "server"

# =============================================================================
# LANCER L'APPLICATION
# =============================================================================

shinyApp(ui = ui, server = server)
