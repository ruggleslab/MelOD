# Source module functions
source("modules/bulk_rna_modules/violin_server.R", local = TRUE)
source("modules/bulk_rna_modules/volcano_server.R", local = TRUE)
source("modules/bulk_rna_modules/heatmap_server.R", local = TRUE)
source("modules/bulk_rna_modules/pca_metadata_server.R", local = TRUE)
source("modules/bulk_rna_modules/input_server.R", local = TRUE)
source("modules/bulk_rna_modules/correlation_server.R", local = TRUE)

source("modules/single_cell_modules/input_server.R", local = TRUE)
source("modules/single_cell_modules/cell_info_server.R", local = TRUE)
source("modules/single_cell_modules/gene_expression_server.R", local = TRUE)

# Source dashboard template
source("templates/dashboard_template.R", local = TRUE)
source("templates/home_template.R", local = TRUE)

source("templates/bulk_rna_templates/gide_template.R", local = TRUE)
source("templates/bulk_rna_templates/badal_template.R", local = TRUE)
source("templates/bulk_rna_templates/kunz_template.R", local = TRUE)
source("templates/bulk_rna_templates/fischer_template.R", local = TRUE)
source("templates/bulk_rna_templates/bulk_rna_template.R", local = TRUE)

source("templates/in_development_template.R", local = TRUE)
source("templates/single_cell_templates/single_cell_template.R", local = TRUE)
source("templates/single_cell_templates/shiny_cell_template.R", local = TRUE)



# Source scripts files
source("scripts/selection.R", local = TRUE)
source("scripts/utils.R", local = TRUE)
source("scripts/plotting.R", local = TRUE)
source("scripts/plotting_single_cell.R", local = TRUE)
source("scripts/data_processing.R", local = TRUE)



library(shiny)
library(shinydashboard)

# Define UI using the sourced template
ui <- dashboardTemplate()


# Define server logic
server <- function(input, output, session) {
  observeEvent(input$tabs, {
    if (input$tabs == "kunz") {
      kunz_server()
    } else if (input$tabs == "badal") {
      badal_server()
    } else if (input$tabs == "gide") {
      gide_server()
    } else if (input$tabs == "fischer") {
      fischer_server()
    } else if (input$tabs == "seurat_test") {
      seurat_server()
    }
  })
}

# Run the application
shinyApp(ui, server)
