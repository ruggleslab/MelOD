# Source module functions
source("modules/home_module.R", local = TRUE)
source("modules/gide_module.R", local = TRUE)
source("modules/badal_module.R", local = TRUE)
source("modules/kunz_module.R", local = TRUE)
source("modules/fischer_module.R", local = TRUE)
# Source dashboard template
source("templates/dashboard_template.R", local = TRUE)
# Source scripts files 
source("scripts/utils.R", local = TRUE)
source("scripts/plotting.R", local = TRUE)
source("scripts/data_processing.R", local = TRUE)

library(shiny)
library(shinydashboard)

# Define UI using the sourced template
ui <- dashboardTemplate()

# Define server logic
server <- function(input, output, session) {
  # Initialize reactive values for tracking loaded modules and the current tab
  modules_loaded <- reactiveValues(home = FALSE, gide = FALSE, badal = FALSE, kunz = FALSE, fischer = FALSE, single=FALSE, proteomic=FALSE, genes=FALSE,about=FALSE)
  
  current_tab <- reactiveVal()
  
  # Function to reset module-specific data
  reset_module <- function(module_name) {
    if (exists(paste0("reset_", module_name), envir = globalenv())) {
      do.call(paste0("reset_", module_name), list())
    }
  }
  
  # Load or reset modules based on tab changes
  observeEvent(input$tabs, {
    new_tab <- input$tabs
    
    # Reset previous module if switching tabs
    old_tab <- current_tab()
    if (!is.null(old_tab) && old_tab != new_tab) {
      reset_module(old_tab)
      modules_loaded[[old_tab]] <- FALSE  # Mark as unloaded
    }
    
    # Load module if not already loaded
    if (!modules_loaded[[new_tab]]) {
      do.call(paste0(new_tab, "_server"), list(id = paste0(new_tab, "_module"), session = session))
      modules_loaded[[new_tab]] <- TRUE
    }
    
    # Update the current active tab
    current_tab(new_tab)
  }, ignoreInit = TRUE)
}

# Run the application
shinyApp(ui, server)
