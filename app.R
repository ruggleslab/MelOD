
# Source module functions
source("modules/home_module.R", local = TRUE)
source("modules/gide_module.R", local = TRUE)
source("modules/badal_module.R", local = TRUE)
source("modules/kunz_module.R", local = TRUE)

# Source dashboard template
source("templates/dashboard_template.R", local = TRUE)
source("scripts/plot.R", local=TRUE)


# Define UI using the sourced template
ui <- dashboardTemplate()

# Define server logic
server <- function(input, output, session) {
  
 home_server("home_module")
 kunz_server("kunz_module", session=session)
 gide_server("gide_module", session=session)
 badal_server("badal_module", session=session)
 
}

# Run the application
shinyApp(ui, server)
