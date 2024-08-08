#' Check and install missing packages
#'
#' @description Function to check and install missing packages
#' @param packagkes list of packages names to check
check_and_install_packages <- function(packages) {
  installed_packages <- rownames(installed.packages())
  for (pkg in packages) {
    if (!pkg %in% installed_packages) {
      # install.packages(pkg, dependencies = TRUE)
    }
    library(pkg, character.only = TRUE)
  }
}

required_packages <- c(
  "shiny", "bslib", "bsicons", "magrittr", "shinyWidgets", "shinydashboard",
  "shinyjs", "plotly", "DESeq2", "dplyr", "tidyverse", "heatmaply", "reshape2",
  "S4Vectors", "jsonlite", "pheatmap", "RColorBrewer", "shinydlplot", "survival",
  "readxl", "ggpubr", "shinyalert", "DT", "googledrive", "shinybusy","shinyjqui",
  "data.table","Matrix","shinycssloaders","shinyFeedback","future","promises","shinyhelper","hdf5r"
)


check_and_install_packages(required_packages)
plan(multisession)
# Setting up the auth token location and email
options(
  gargle_oauth_email = "ruggleslab.shinyseq.backend@gmail.com",
  gargle_oauth_cache = "authentication"
)

# List all of the files available to download
drive_find()

# Improve resolution of the plot
options(shiny.plot.res = 96)

