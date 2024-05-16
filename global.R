# Function to check and install missing packages
check_and_install_packages <- function(packages) {
  installed_packages <- rownames(installed.packages())
  for (pkg in packages) {
    if (!pkg %in% installed_packages) {
      install.packages(pkg, dependencies = TRUE)
    }
    library(pkg, character.only = TRUE)
  }
}

# List of required packages
required_packages <- c(
  "shiny", "bslib", "bsicons", "magrittr", "shinyWidgets", "shinydashboard",
  "shinyjs", "plotly", "DESeq2", "dplyr", "tidyverse", "heatmaply", "reshape2",
  "S4Vectors", "jsonlite", "pheatmap", "RColorBrewer", "shinydlplot", "survival",
  "readxl", "ggpubr", "shinyalert", "Glimma", "DT", "googledrive", "shinybusy"
)

# Check and install missing packages
check_and_install_packages(required_packages)

# Setting up the auth token location and email
options(
  gargle_oauth_email = "ruggleslab.shinyseq.backend@gmail.com",
  gargle_oauth_cache = "authentication"
)

# Getting the auth token (only needs to be done once, so if you have the file you don't need to do this)
# drive_auth()

# List all of the files that you have available to download
drive_find()
options(shiny.plot.res = 96)
