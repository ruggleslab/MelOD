#' Home UI
#'
#' @description Creates the UI layout for the Home tab
#' @param id Module ID
#' @return A Shiny UI element for the Home tab
in_development_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    tags$h3("In development.... :)"),
    fluidRow(
      column(
        width = 9,
        tags$p(
          ""
        ),
      ),
      column(
        width = 3,
        tags$img(src = "./images/melanoma.png", width = "150px")  # Updated path to reference the www directory
      )
    ),
    hr()
  )
}

#' Home Server
#'
#' @description Sets up the server logic for the Home analysis tab
in_development_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
  })
}
