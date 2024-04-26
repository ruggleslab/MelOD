library(shiny)
library(shinydashboard)
library(shinyjs)
# Source module functions
source("modules/home_module.R", local = TRUE)
source("modules/gide_module.R", local = TRUE)
source("modules/badal_module.R", local = TRUE)
# Source dashboard template
source("templates/dashboard_template.R", local = TRUE)

# Define UI using the sourced template
ui <- dashboardTemplate()

# Define server logic
server <- function(input, output, session) {
 home_server("home_module")
 gide_server("gide_module")
 badal_server("badal_module")
}

# Run the application
shinyApp(ui, server)
