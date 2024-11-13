#' Check and install missing packages
#'
#' @description Function to check and install missing packages
#' @param packages list of package names to check
check_and_install_packages <- function(packages) {
  # Ensure BiocManager is available for Bioconductor packages
  if (!"BiocManager" %in% rownames(installed.packages())) {
    install.packages("BiocManager", dependencies = TRUE)
  }
  
  installed_packages <- rownames(installed.packages())
  for (pkg in packages) {
    if (!pkg %in% installed_packages) {
      if (pkg %in% BiocManager::available()) {
        BiocManager::install(pkg)
      } else {
        install.packages(pkg, dependencies = TRUE)
      }
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