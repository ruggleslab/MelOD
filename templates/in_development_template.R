#' Home UI
#'
#' @description Creates the UI layout for the Home tab
#' @param id Module ID
#' @return A Shiny UI element for the Home tab
in_development_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    tags$div(
      style = "text-align: center;",
      tags$img(src = "./images/under_construction.gif", width = "400px")
    )
  )
}

#' Home Server
#'
#' @description Sets up the server logic for the Home analysis tab
in_development_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
  })
}
