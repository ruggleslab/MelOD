# Source module functions
source("modules/bulk_rna_modules/violin_server.R", local = TRUE)
source("modules/bulk_rna_modules/volcano_server.R", local = TRUE)
source("modules/bulk_rna_modules/heatmap_server.R", local = TRUE)
source("modules/bulk_rna_modules/pca_metadata_server.R", local = TRUE)
source("modules/bulk_rna_modules/input_server.R", local = TRUE)
source("modules/bulk_rna_modules/correlation_server.R", local = TRUE)

source("modules/single_cell_modules/input_server.R", local = TRUE)
source("modules/single_cell_modules/comparison_server.R", local = TRUE)
source("modules/single_cell_modules/gene_coexpression_server.R", local = TRUE)
source("modules/single_cell_modules/sc_violin_server.R", local = TRUE)
source("modules/single_cell_modules/proportion_server.R", local = TRUE)
source("modules/single_cell_modules/bubheat_server.R", local = TRUE)


# Source dashboard template
source("templates/dashboard_template.R", local = TRUE)
source("templates/home_template.R", local = TRUE)
source("templates/bulk_rna_templates/gide_template.R", local = TRUE)
source("templates/bulk_rna_templates/badal_template.R", local = TRUE)
source("templates/bulk_rna_templates/kunz_template.R", local = TRUE)
source("templates/bulk_rna_templates/fischer_template.R", local = TRUE)
source("templates/bulk_rna_templates/hugo_template.R", local = TRUE)
source("templates/bulk_rna_templates/riaz_template.R", local = TRUE)
source("templates/bulk_rna_templates/tsoi_template.R", local = TRUE)
source("templates/bulk_rna_templates/bulk_rna_template.R", local = TRUE)

source("templates/in_development_template.R", local = TRUE)
source("templates/single_cell_templates/single_cell_template.R", local = TRUE)
source("templates/single_cell_templates/qisun_template.R", local = TRUE)
source("templates/single_cell_templates/KBPT_template.R", local = TRUE)
source("templates/single_cell_templates/biermann_template.R", local = TRUE)
source("templates/single_cell_templates/jerby_template.R", local = TRUE)
source("templates/single_cell_templates/pozniak_template.R", local = TRUE)
source("templates/single_cell_templates/rambow_template.R", local = TRUE)

source("templates/proteomics_templates/kleffman_template.R", local = TRUE)
source("templates/proteomics_templates/MM500_template.R", local = TRUE)


source("templates/gene_search_template.R", local = TRUE)

# Source scripts files
source("scripts/selection.R", local = TRUE)
source("scripts/utils.R", local = TRUE)
source("scripts/plotting.R", local = TRUE) 
source("scripts/plotting_single_cell.R", local = TRUE)
source("scripts/data_processing.R", local = TRUE)
source("scripts/data_processing_single_cell.R", local = TRUE)


library(shiny)
library(shinydashboard)

# Define UI using the sourced template 
ui <- dashboardTemplate()
drive_auth(cache = "authentication", 
           email = TRUE) 

# Define server logic
server <- function(input, output, session) {
  
  # Clear all variables and run garbage collection
  rm(list = ls(envir = .GlobalEnv), envir = .GlobalEnv)  # Clear global variables
  gc()  # Force garbage collection to free memory

  observeEvent(input$tabs, {
    if (input$tabs == "kunz") {
      kunz_server()
    } else if (input$tabs == "badal") {
      badal_server()
    } else if (input$tabs == "gide") {
      gide_server()
    } else if (input$tabs == "fischer") {
      fischer_server()
    } else if (input$tabs == "hugo"){
      hugo_server()
    } else if (input$tabs == "tsoi"){
      tsoi_server()
    } else if (input$tabs == "riaz"){
      riaz_server()
    } else if (input$tabs == "qisun") {
      qisun_server()
    } else if (input$tabs == "KBPT") {
      KBPT_server()
    } else if (input$tabs == "biermann") {
      biermann_server()
    } else if (input$tabs == "jerby") {
      jerby_server()
    } else if (input$tabs == "pozniak") {
      pozniak_server()
    } else if (input$tabs == "rambow") {
      rambow_server()
    } else if (input$tabs == "kleffman"){
      kleffman_server()
    } else if (input$tabs == "MM500"){
      MM500_server()
    } else if (input$tabs == "gene_search"){
      gene_search_server("gene_search_template")
    }
  })
}

shinyApp(ui, server)

