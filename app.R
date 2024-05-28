# Source module functions
source("modules/violin_server.R", local = TRUE)
source("modules/volcano_server.R", local = TRUE)
source("modules/heatmap_server.R", local = TRUE)
source("modules/pca_metadata_server.R", local = TRUE)
source("modules/input_server.R", local = TRUE)
source("modules/correlation_server.R", local = TRUE)

# Source dashboard template
source("templates/dashboard_template.R", local = TRUE)
source("templates/home_template.R", local = TRUE)
source("templates/gide_template.R", local = TRUE)
source("templates/badal_template.R", local = TRUE)
source("templates/kunz_template.R", local = TRUE)
source("templates/fischer_template.R", local = TRUE)
source("templates/bulk_rna_template.R", local = TRUE)

source("templates/in_development_template.R", local = TRUE)

# Source scripts files 
source("scripts/selection.R", local = TRUE)
source("scripts/utils.R", local = TRUE)
source("scripts/plotting.R", local = TRUE)
source("scripts/data_processing.R", local = TRUE)


library(shiny)
library(shinydashboard)

# Define UI using the sourced template
ui <- dashboardTemplate()

# Define server logic
server <- function(input, output, session) {
  # Observer to detect tab changes
  observeEvent(input$tabs, {
    if (input$tabs == "kunz") {
      kunz_server()
    } else if (input$tabs == "badal") {
      badal_server()
    } else if (input$tabs == "gide") {
      gide_server()
    } else if (input$tabs == "fischer") {
      fischer_server()
    }
  })
}

# Run the application
shinyApp(ui, server)
