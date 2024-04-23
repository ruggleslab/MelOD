ui <- dashboardPage(
  dashboardHeader(title = "Shiny-Seq"),
  dashboardSidebar(
    width = 350,
    
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Dataset", tabName = "dataset", icon = icon("chart-bar"),
               menuItem("BulkRNA", tabName = "bulkrna_tab",
                           menuSubItem("Gide et al 2019", tabName = "Gide", 
                                       )),
               menuItem("Single Cell", tabName = "singlecell_tab",
                        menuSubItem("dataset2", tabName = "dataset2"))
               ),
    
    
    menuItem("About", tabName = "about", icon = icon("info"))
  )),
  
  dashboardBody(tags$head(
    # Include your CSS file
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  tabItems(
    tabItem("home",
            home_ui("home_tab")),
    tabItem(
      "Gide",
      bulkrna_ui("bulkrna_tab")),
    
    tabItem(
      "dataset2",
      singlecell_ui("singlecell_tab")),
    
    tabItem("about")
  ))
)
