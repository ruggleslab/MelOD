home_ui <- function(id) {
  #' Home UI
  #'
  #' @description Creates the UI layout for the Home tab
  #' @param id Module ID
  #'
  #' @return A Shiny UI element for the Home tab
  
  ns <- NS(id)
  fluidPage(
    # Increase the base font size for better readability
    tags$style(HTML("
      .content_home {
        margin-top: 20px;
      }
      .section-header {
        font-size: 20px;
        font-weight: bold;
        margin-top: 40px;
      }
      .image-inline {
        vertical-align: middle;
        margin-right: 10px;
      }
      p { font-size: 16px;}
    ")),
    # Project Overview
    fluidRow(
      width = 12,
      div(class = ".content_home",
          tags$img(src = "./images/melod_large_logo.png", width = "500px"),
          div(class = "section-header", "Project Overview"),
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
          )
      )
    ),
    # App Utilization
    div(class = "section-header", "App Utilization"),
    fluidRow(
      width = 12,
      div(class = ".content_home",
          tags$p(tags$img(src = "./images/arrow_home.png", width = "50px", class = "image-inline"),
                 "Each study is accessible via the dropdown menu on the sidebar. Studies are classified by the type of data they contain."),
          tags$p(
            "On a study page, once the data is loaded from the cloud, you will find a brief explanation at the top and several boxes corresponding to different types of visualizations. Each plot is interactive, thanks to the Plotly library. 
            You can download the data used for each plot by clicking on this button ", tags$img(src = "./images/download-file.png", width = "20px", class = "image-inline"), 
            "For more information on a particular visualization, click on this button", tags$img(src = "./images/information-button.png", width = "20px", class = "image-inline")
          )
      )
    ),
    br(),
    br(),
    # Survey and Citation
    div(class = "section-header", "We value your feedback!"),
    fluidRow(
      width = 12,
      div(class = ".content_home",
          tags$p(
            "Please take a moment to complete our survey: ",
            tags$a(href = "https://forms.gle/m3o8FMRt3NLPFMrC8", "Take the Survey")
          ),
          tags$p(
            "For additional information on using MelOD! or to cite MelOD! in your work, please refer to the following paper: Coming soon!!"
          )
      )
    )
    )
}


home_server <- function(id) {
#' Home Server
#'
#' @description Sets up the server logic for the Home analysis tab
  
  moduleServer(id, function(input, output, session) {
  })
}