# Home UI Module
home_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    tags$h3("Explanation of the Project"),
    fluidRow(column(
      width = 9,
      tags$p(
        "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."
      ),
   
    ),
    column(
      width = 3,
      tags$img(src = "./images/melanoma.png", width = "150px")  # Updated path to reference the www directory
    )),
    hr()
  )
}

home_server <- function(id) {
  moduleServer(id, function(input, output, session) {

  })
}

