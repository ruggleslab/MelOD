#' Dashboard Template
#'
#' @description Creates the dashboard layout for the Shiny-Seq application
#' @return A Shiny dashboard page layout
dashboardTemplate <- function() {
  dashboardPage(
    dashboardHeader(title = "Shiny-Seq"),
    dashboardSidebar(
      width = 250,
      sidebarMenu(
        id = "tabs",
        menuItem("Home", tabName = "home", icon = icon("home")),
        menuItem("Studies", tabName = "dataset", icon = icon("database"),
                 menuItem("BulkRNA", tabName = "bulkrna_tab",
                          menuSubItem("Gide et al. 2019", tabName = "gide"),
                          menuSubItem("Badal et al. 2018", tabName = "badal"),
                          menuSubItem("Kunz, Schartl 2018", tabName = "kunz"),
                          menuSubItem("Fischer, Davies 2019", tabName = "fischer")
                 ),
                 menuItem("Single Cell", tabName = "single",
                          menuSubItem("Seurat Test", tabName = "seurat_test")
                 ),
                 menuItem("Proteomic", tabName = "proteomic")
        ),
        menuItem("About", tabName = "about", icon = icon("info-circle"))
      )
    ),
    dashboardBody(
      useShinyjs(),
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
      ),
      tabItems(
        tabItem(tabName = "home", home_ui("home_template")),
        tabItem(tabName = "gide", gide_ui("gide_template")),
        tabItem(tabName = "badal", badal_ui("badal_template")),
        tabItem(tabName = "kunz", kunz_ui("kunz_template")),
        tabItem(tabName = "fischer", fischer_ui("fischer_template")),
        tabItem(tabName = "seurat_test", seurat_test_ui("seurat_test_template")),
        tabItem(tabName = "proteomic", in_development_ui("in_development_template")),
        tabItem(tabName = "about", in_development_ui("in_development_template"))
      ),
      tags$div(class = "footer",
               tags$p("Shiny-Seq Application"),
               tags$p("Developed by Ruggles Lab"),
               tags$p("Contact: support@ruggleslab.org"),
               tags$p(tags$a(href = "https://ruggleslab.org/shiny-seq/help", "Help & Documentation"))
      )
    )
  )
}
