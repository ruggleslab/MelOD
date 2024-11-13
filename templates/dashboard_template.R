dashboardTemplate <- function() {
#' Dashboard Template
#'
#' @description Creates the dashboard layout for the Shiny-Seq application
#' 
#' @return A Shiny dashboard page layout

  dashboardPage(
    title = "MelOD", 
    dashboardHeader(title = tags$div(
      tags$img(src = "./images/header_logo_final4.png", height = "40px"),
    )),
    dashboardSidebar(
      width = 275,
      sidebarMenu(
        id = "tabs",
        menuItem("Home", tabName = "home", icon = icon("home")),
        menuItem("Studies", tabName = "dataset", icon = icon("database"),
                 menuItem("Transcriptomic", tabName = "transcriptomic_tab",
                          menuSubItem("Gide et al. 2019", tabName = "gide"),
                          menuSubItem("Fischer et al. 2019", tabName = "fischer"),
                          menuSubItem("Kunz et al. 2018", tabName = "kunz"),
                          menuSubItem("Tsoi et al. 2018", tabName = "tsoi"),
                          menuSubItem("Badal et al. 2017", tabName = "badal"),
                          menuSubItem("Riaz et al. 2017", tabName = "riaz"),
                          menuSubItem("Hugo et al. 2016", tabName = "hugo")
                 ),
                 menuItem("Single Cell", tabName = "single",
                          menuSubItem("Qi Sun et al. 2019", tabName = "qisun"),
                          menuSubItem("Biermann et al. 2022", tabName = "biermann"),
                          menuSubItem("Jerby-Arnon et al. 2018", tabName = "jerby"),
                          menuSubItem("Pozniak et al. 2022", tabName = "pozniak"),
                          menuSubItem("Rambow et al. 2018", tabName = "rambow")
                          
                 ),
                 menuItem("Proteomic", tabName = "proteomic",
                          menuSubItem("Kleffman et al. 2022", tabName = "kleffman")
                          )
        ),
        menuItem("Gene search", tabName = "gene_search", icon = icon("magnifying-glass")),
        menuItem("About", tabName = "about", icon = icon("info-circle"))
      )
    ),
    dashboardBody(
      useShinyjs(),
      tags$head(
        tags$link(rel = "icon", type = "image/x-icon", href = "images/favicon_io/favicon.ico"),
        tags$link(rel = "icon", type = "image/png", sizes = "16x16", href = "images/favicon_io/favicon-16x16.png"),
        tags$link(rel = "icon", type = "image/png", sizes = "32x32", href = "images/favicon_io/favicon-32x32.png"),
        tags$link(rel = "icon", type = "image/png", sizes = "512x512", href = "images/favicon_io/android-chrome-512x512.png"),
        tags$link(rel = "icon", type = "image/png", href = "images/favicon_io/apple-touch-icon.png"),
        tags$link(rel = "icon", type = "image/png", sizes = "192x192", href = "images/favicon_io/android-chrome-192x192.png"),
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
        tabItem(tabName = "hugo", hugo_ui("hugo_template")),
        tabItem(tabName = "tsoi", tsoi_ui("tsoi_template")),
        tabItem(tabName = "riaz", riaz_ui("riaz_template")),
        
        tabItem(tabName = "qisun", qisun_ui("qisun_template")),
        tabItem(tabName = "biermann", biermann_ui("biermann_template")),
        tabItem(tabName = "jerby", jerby_ui("jerby_template")),
        tabItem(tabName = "pozniak", pozniak_ui("pozniak_template")),
        tabItem(tabName = "rambow", rambow_ui("rambow_template")),
        tabItem(tabName = "KBPT", KBPT_ui("KBPT_template")),
        
        tabItem(tabName = "kleffman", kleffman_ui("kleffman_template")),
        
        tabItem(tabName = "gene_search", gene_search_ui("gene_search_template")),
        
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