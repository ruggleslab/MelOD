library(shinydashboard)

dashboardTemplate <- function() {
  dashboardPage(
    dashboardHeader(title = "Shiny-Seq"),
    dashboardSidebar(
      width = 250,
      sidebarMenu(id = "tabs",
                  menuItem("Home", tabName = "home", icon = icon("home")),
                  menuItem("Query by studies", tabName = "dataset", icon = icon("database"),
                           menuItem("BulkRNA", tabName = "bulkrna_tab",
                                    menuSubItem("Gide et al. 2019", tabName = "gide"),
                                    menuSubItem("Badal et al. 2018", tabName = "badal"),
                                    menuSubItem("Kunz, Schartl 2018", tabName = "kunz")
                           )
                  ),
                  menuItem("Query by genes", tabName = "genes", icon = icon("search")),
                  menuItem("About", tabName = "about", icon = icon("info-circle"))
      )
    ),
    dashboardBody(
      useShinyjs(),
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
      ),
      tabItems(
        tabItem(tabName = "home", home_ui("home_module")),
        tabItem(tabName = "gide", gide_ui("gide_module")),
        tabItem(tabName = "badal", badal_ui("badal_module")),
        tabItem(tabName = "kunz", kunz_ui("kunz_module")),
        tabItem(tabName = "about", h1("About Page"))
      ),
      tags$div(class = "footer", tags$p("Rshiny-Seq Ruggles Lab"))
      
    )
  )
}
