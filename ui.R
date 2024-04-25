source("./R/home_module.R", local = TRUE)
source("./R/bulkrna_module.R", local = TRUE)

ui <- dashboardPage(
  dashboardHeader(title = "Shiny-Seq"),
  dashboardSidebar(
    width = 250,
    sidebarMenu(id = "tabs",
                menuItem("Home", tabName = "home", icon = icon("home")),
                menuItem("Dataset", tabName = "dataset", icon = icon("database"),
                         menuItem("BulkRNA", tabName = "bulkrna_tab",
                                  menuSubItem("Gide et al 2019", tabName = "Gide")
                         )
                ),
                menuItem("About", tabName = "about", icon = icon("info-circle"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
    ),
    tabItems(
      tabItem(tabName = "home",
              home_ui("home_module")
      ),
      tabItem(tabName = "Gide",
              bulkrna_ui("bulkrna_tab")
      ),
      tabItem(tabName = "about",
              h1("About Page")
      )
    )
  )
)


server <- function(input, output, session) {
  bulkrna_server("bulkrna_tab")
  home_server("home_module")
}

shinyApp(ui, server)
