dashboardTemplate <- function() {
#' Dashboard Template
#'
#' @description Creates the dashboard layout for the Shiny-Seq application
#' 
#' @return A Shiny dashboard page layout

  dashboardPage(
    dashboardHeader(title = tags$div(
      tags$img(src = "./images/header_logo_final4.png", height = "40px"),
    )),
    dashboardSidebar(
      width = 275,
      sidebarMenu(
        id = "tabs",
        menuItem("Home", tabName = "home", icon = icon("home")),
        menuItem("Studies", tabName = "dataset", icon = icon("database"),
                 menuItem("Bulk RNA", tabName = "bulkrna_tab",
                          menuSubItem("Gide et al. 2019", tabName = "gide"),
                          menuSubItem("Badal et al. 2018", tabName = "badal"),
                          menuSubItem("Kunz, Schartl 2018", tabName = "kunz"),
                          menuSubItem("Fischer, Davies 2019", tabName = "fischer")
                 ),
                 menuItem("Single Cell", tabName = "single",
                          menuSubItem("Qi Sun et al. 2019", tabName = "qisun")
                          # menuSubItem("Internal Data (KBPT)", tabName = "KBPT")
                 ),
                 menuItem("Proteomic", tabName = "proteomic",
                          menuSubItem("Kleffman et al. 2022", tabName = "kleffman")
                          )
        ),
        menuItem("About", tabName = "about", icon = icon("info-circle"))
      )
    ),
    dashboardBody(
      useShinyjs(),
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "styles_general.css"),
        tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
        tags$link(rel = "stylesheet", type = "text/css", href = "styles_single_cell.css")
      ),
      tabItems(
        tabItem(tabName = "home", home_ui("home_template")),
        tabItem(tabName = "gide", gide_ui("gide_template")),
        tabItem(tabName = "badal", badal_ui("badal_template")),
        tabItem(tabName = "kunz", kunz_ui("kunz_template")),
        tabItem(tabName = "fischer", fischer_ui("fischer_template")),
        tabItem(tabName = "qisun", qisun_ui("qisun_template")),
        tabItem(tabName = "KBPT", KBPT_ui("KBPT_template")),
        tabItem(tabName = "kleffman", kleffman_ui("kleffman_template")),
        
        tabItem(tabName = "about", in_development_ui("in_development_template"))
      ),
      tags$div(class = "footer",
               tags$p("Melanoma Omics Dashboard (MelOD)"),
               tags$p("Developed by Ruggles Lab"),
               tags$p(tags$a(href = "https://github.com/ruggleslab/shiny-seq", "Help & Documentation"))
      )
    )
  )
}