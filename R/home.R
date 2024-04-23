home_ui <- function(id) {
  fluidPage(tags$div(
    tags$h3("Explanation of the Project"),
    fluidRow(column(
      width = 9,
      tags$p(
        "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."
      )
    ),
    column(
      width = 3,
      tags$img(src = "./melanoma.png", width = "150px")
    )),
    hr(),
    
    fluidRow(
      column(
        width = 3,
        actionButton(NS(id, "load_dataset1"), "Gide et al. 2019", class = "btn btn-primary btn-block")
      ),
      
      column(
        width = 3,
        actionButton(NS(id, "load_dataset2"), "Load Dataset 2", class = "btn btn-primary btn-block")
      ),

      
      column(
        width = 3,
        actionButton(NS(id, "load_dataset3"), "Load Dataset 3", class = "btn btn-primary btn-block")
      )
    )
  ))
}


home_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$clicked_tab, {
      if(input$clicked_tab == "load_dataset1") {
        updateTabItems(session, "dataset", selected = "bulkrna_tab")
      }
    })
  })
}
