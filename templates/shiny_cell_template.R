#' Shiny Cell UI
#'
#' @description 
#' @param id Module ID
#' @return 
shiny_cell_ui <- function(id) {
  fluidPage(
    # titlePanel("shinyCell Example"),
    # sidebarLayout(
    #   sidebarPanel(
    #     selectInput("feature", "Feature:", choices = rownames(pbmc_small), selected = "CD3D")
    #   ),
    #   mainPanel(
    #     shinyCell::shinyCellOutput("shinyCellPlot")
    #   )
    # )
  )
}





#' Shiny Cell Server
#'
#' @description
shiny_cell_server <- function() {
  data("pbmc_small")
  output$shinyCellPlot <- shinyCell::renderShinyCell({
    shinyCell::shinyCellPlot(seuratObject = pbmc_small, feature = input$feature)
  })
}

