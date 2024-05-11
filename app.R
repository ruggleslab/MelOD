
# Source module functions
source("modules/home_module.R", local = TRUE)
source("modules/gide_module.R", local = TRUE)
source("modules/badal_module.R", local = TRUE)
source("modules/kunz_module.R", local = TRUE)
source("modules/fischer_module.R", local = TRUE)
# Source dashboard template
source("templates/dashboard_template.R", local = TRUE)
# Source scripts files 
source("scripts/plot.R", local=TRUE)
source("scripts/utils.R", local=TRUE)


# Define UI using the sourced template
ui <- dashboardTemplate()

# Define server logic
server <- function(input, output, session) {
  
  modules_loaded <- reactiveValues(home = FALSE, gide = FALSE, badal = FALSE, kunz = FALSE, fischer = FALSE)
  
  observeEvent(input$tabs, {
    if (!modules_loaded$home && input$tabs == "home") {
      home_server("home_module")
      modules_loaded$home <- TRUE
    }
    if (!modules_loaded$gide && input$tabs == "gide") {
      gide_server("gide_module", session = session)
      modules_loaded$gide <- TRUE
    }
    if (!modules_loaded$badal && input$tabs == "badal") {
      badal_server("badal_module", session = session)
      modules_loaded$badal <- TRUE
    }
    if (!modules_loaded$kunz && input$tabs == "kunz") {
      kunz_server("kunz_module", session = session)
      modules_loaded$kunz <- TRUE
    }
    if (!modules_loaded$fischer && input$tabs == "fischer") {
      fischer_server("fischer_module", session = session)
      modules_loaded$fischer <- TRUE
    }
  }, ignoreInit = TRUE)
}

# Run the application
shinyApp(ui, server)
