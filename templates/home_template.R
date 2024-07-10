home_ui <- function(id) {
#' Home UI
#'
#' @description Creates the UI layout for the Home tab
#' @param id Module ID
#' 
#' @return A Shiny UI element for the Home tab
  
  ns <- NS(id)
  fluidPage(
    tags$h3("Project Overview"),
    fluidRow(
      column(
        width = 9,
        tags$p(
          "Welcome to MelOD, the Melanoma Omics Dashboard. 
          This RShiny application is dedicated to the analysis and visualization of melanoma cancer datasets. 
          Our platform currently supports bulk RNA analysis, providing a suite of tools for differential gene expression,
          PCA, heatmaps, correlation analysis, and more. We aim to offer robust and interactive visualizations to aid researchers in exploring data effectively."
        ),
        tags$p(
          "In the near future, we plan to expand our capabilities to include single-cell RNA analysis, proteomics, and array-based datasets. 
           Each of these additions will bring new features and functionalities, enabling a more holistic view of melanoma cancer at various 
           molecular levels."
        ),
        br(),
        br(),
        br(),
        br(),
        tags$h3(
          "We value your feedback! Please take a moment to complete our survey: ",
          tags$a(href = "https://forms.gle/m3o8FMRt3NLPFMrC8", "Take the Survey")
        )
      ),
      column(
        width = 3,
        tags$img(src = "./images/melanoma.png", width = "150px")  # Updated path to reference the www directory
      )
    ),
    hr()
  )
}

home_server <- function(id) {
#' Home Server
#'
#' @description Sets up the server logic for the Home analysis tab
  
  moduleServer(id, function(input, output, session) {
  })
}