home_ui <- function(id) {
  #' Home UI
  #'
  #' @description Creates the UI layout for the Home tab with a sprinkle of blabla and a few well-timed burps for good measure.
  #' @param id Module ID
  #'
  #' @return A Shiny UI element for the Home tab with some extra flair and unexpected surprises.
  
  ns <- NS(id)
  fluidPage(
    # Link to the external CSS file
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")),
    
    # Project Overview Box
    column(7, box(
      title = HTML(paste("Project Overview")),
      status = "warning", solidHeader = TRUE,
      width = 12,
      div(class = ".content_home",
          tags$p(
            "Welcome to MelOD, the Melanoma Omics Dashboard. This RShiny application is your gateway to the analysis and visualization of melanoma cancer datasets. 
            Designed with researchers in mind, MelOD provides a suite of powerful tools to explore, visualize, and make sense of complex data. Whether you're a seasoned bioinformatician or just starting your journey in omics, MelOD offers a user-friendly environment tailored to your needs."
          ),
          tags$p(
            "The main objective of MelOD is to democratize access to high-quality, interactive visualizations that can help accelerate discoveries in melanoma research. By integrating data from multiple studies, including bulk RNA-Seq, single-cell RNA-Seq, and proteomics (with more to come), MelOD allows for a comprehensive examination of the molecular landscape of melanoma."
          ),
          tags$p(
            "In addition to offering a range of pre-built visualizations, MelOD empowers users to dive deep into their data, customize plots, and even download datasets for offline analysis. With an intuitive interface, the application streamlines the process of data exploration, making it easier than ever to generate insights and hypotheses that can drive further research."
          ),
          tags$p(
            "So go ahead, explore the different tabs, load your favorite datasets, and discover the stories hidden within the data. MelOD is here to make your research experience not only productive but also enjoyable!"
          )
      )
    )),
    
    column(4, box(
      title = HTML(paste("Logo")),
      status = "info", solidHeader = TRUE,
      width = 12, 
      tags$img(src = "./images/melod_large_logo.png", class = "responsive-logo"))          
    ),
    
    # App Utilization Box
    box(
      title = HTML(paste("App Utilization")),
      status = "primary", solidHeader = TRUE,
      width = 12,
      div(class = ".content_home",
          tags$p(tags$img(src = "./images/arrow_home.png", width = "50px", class = "image-inline"),
                 "Each study is accessible via the dropdown menu on the sidebar. Studies are classified by the type of data they contain."),
          tags$p(
            "On a study page, once the data is loaded from the cloud, you will find a brief explanation at the top and several boxes corresponding to different types of visualizations.
            You can download the data used for each plot by clicking on this button ", tags$img(src = "./images/download-file.png", width = "20px", class = "image-inline"), 
            "For more information on a particular visualization, click on this button", tags$img(src = "./images/information-button.png", width = "20px", class = "image-inline"),
          ),
          tags$p(
            "Each plotting box is made using plotly allowing several option for the plot and making it interative. Hovering the plot make appear this box to the top right where you can screenshot the plot or resize the plot for instance.", 
            tags$img(src = "./images/plotly.png", width = "230px", class = "image-inline"),
          ),
          tags$div(class = "dropdown-header", "Specific functionality"),
          tags$div(
            class = "clickable collapsed", 
            `data-toggle` = "collapse", `data-target` = "#collapseInstructionsbulkrna", 
            "Bulk RNA Tab"
          ),
          tags$div(id = "collapseInstructionsbulkrna", class = "collapse dropdown-content",
                   tags$ul(
                     tags$li("You can select genes directly by clicking dots on the volcano plot.")
                   )
          ),
          tags$div(
            class = "clickable collapsed", 
            `data-toggle` = "collapse", `data-target` = "#collapseInstructionssinglecell", 
            "Single Cell tab"
          ),
          tags$div(id = "collapseInstructionssinglecell", class = "collapse dropdown-content",
                   tags$ul(
                     tags$li("This section is inspired by ShinyCell"),
                     tags$li("Cell datable is linked to the cell information axis."),
                     tags$li("Same for the gene datable.")
                   )
          )
      )
    ),
    
    # Dropdown Bullet Points Box
    box(
      title = HTML(paste("Supplemental information")),
      status = "primary", solidHeader = TRUE,
      width = 12,
      div(class = ".content_home",
          tags$div(class = "dropdown-header", "Currently Working on:"),
          tags$ul(
            tags$li("Adding the possibility to download Differential expressed gene for Single cell study.")
          ),
          
          tags$div(class = "dropdown-header", "What data formats are supported?"),
          tags$ul(
            tags$li(
              class = "clickable collapsed", 
              `data-toggle` = "collapse", `data-target` = "#collapsedatabulk", 
              "Bulk RNA-Seq data (currently supported)."
            ),
            tags$div(id = "collapsedatabulk", class = "collapse dropdown-content",
                     tags$ul(
                       tags$li("Kunz"),
                       tags$li("Gide"),
                       tags$li("Badal"),
                       tags$li("Fisher"),
                     )
            ),
            tags$li(
              class = "clickable collapsed", 
              `data-toggle` = "collapse", `data-target` = "#collapsedatasingle", 
              "Single-cell RNA-Seq data (currently supported).",
            ),
            tags$div(id = "collapsedatasingle", class = "collapse dropdown-content",
                     tags$ul(
                       tags$li("Qi Sun et. al : A novel mouse model demonstrates that oncogenic melanocyte stem cells engender melanoma resembling human disease"),
                     )
            ),
            tags$li("Proteomics data (coming soon)."),
            tags$li("Array-based data (coming soon).")
          )
      )
    ),
    
    # Survey and Citation Box
    box(
      title = HTML(paste("We value your feedback!")),
      status = "primary", solidHeader = TRUE,
      width = 12,
      div(class = ".content_home",
          tags$p(
            "Please take a moment to complete our survey: ",
            tags$a(href = "https://forms.gle/m3o8FMRt3NLPFMrC8", "Take the Survey"),
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
  #' @description Sets up the server logic for the Home analysis tab. Nothing too complicated, we promise!
  
  moduleServer(id, function(input, output, session) {
  })
}
