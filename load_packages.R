# Package names
packages <- c("markdown","here","ggplot2", "tidyverse", "shiny", "shinydashboard", "shinyBS", "magrittr", "rmarkdown", "plotly", "DT", "shinyWidgets",
              "shinycssloaders", "deSolve","docstring", "fontawesome", "knitr")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
suppressMessages(lapply(packages, library, character.only = T))


