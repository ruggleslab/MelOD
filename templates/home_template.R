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
    fluidRow(
    column(8, box(
      title = HTML(paste("Project Overview")),
      status = "warning", solidHeader = TRUE,collapsible = TRUE,
      width = 12,
      div(class = ".content_home",
          tags$p(HTML(
            "<b>Welcome to MelOD</b>, the Melanoma Omics Dashboard. This RShiny application is your gateway to the analysis and visualization of melanoma cancer datasets. 
            Designed with researchers in mind, MelOD provides a suite of powerful tools to explore, visualize, and make sense of complex data. Whether you're a seasoned bioinformatician or just starting your journey in omics, MelOD offers a user-friendly environment tailored to your needs."
          )),
          tags$p(HTML(
            "The main objective of MelOD is to <b>democratize access</b> to high-quality, interactive visualizations that can help accelerate discoveries in melanoma research. By integrating data from multiple studies, including bulk RNA-Seq, single-cell RNA-Seq, and proteomics (with more to come), MelOD allows for a comprehensive examination of the molecular landscape of melanoma."
          )),
          tags$p(
            "In addition to offering a range of pre-built visualizations, MelOD empowers users to dive deep into their data, customize plots, and even download datasets for offline analysis. With an intuitive interface, the application streamlines the process of data exploration, making it easier than ever to generate insights and hypotheses that can drive further research."
          ),
          tags$p(
            "So go ahead, explore the different tabs, load your favorite datasets, and discover the stories hidden within the data. MelOD is here to make your research experience not only productive but also enjoyable!"
          )
      )
    )),
    column(4, box(
      title = HTML(paste('')),
      status = "info", solidHeader = FALSE, collapsible = FALSE,
      width = 9, 
      tags$img(src = "./images/melod_large_logo.png", class = "responsive-logo"))          
    )),

    # App Utilization Box
   box(
      title = HTML(paste("App Utilization")),
      status = "primary", solidHeader = TRUE,collapsible = TRUE,
      width = 12,
      div(class = ".content_home",
          tags$p(tags$img(src = "./images/arrow_home.png", width = "50px", class = "image-inline"),
                 "Each study is accessible via the dropdown menu on the sidebar. Studies are classified by the type of data they contain."),
          tags$img(src = "./images/home_page_utilization.png", class = "responsive-logo"),
          tags$div(class = "dropdown-header", "Specific functionality"),
          tags$ul(
          tags$div(
            class = "clickable collapsed", 
            `data-toggle` = "collapse", `data-target` = "#collapseInstructionsbulkrna", 
            "Bulk RNA Tab"
          ),
          tags$div(
            id = "collapseInstructionsbulkrna", 
            class = "collapse dropdown-content",
            tags$ul(
              tags$li("The PCA box provides key dataset information about the study sample. Additionally, if available, a metadata box offers insights into patient mortality."),
              tags$li("You can select genes directly by clicking on the dots within the volcano plot.")
            )
          ),
          tags$div(
            class = "clickable collapsed", 
            `data-toggle` = "collapse", 
            `data-target` = "#collapseInstructionssinglecell", 
            "Single Cell Tab"
          ),
          tags$div(
            id = "collapseInstructionssinglecell", 
            class = "collapse dropdown-content",
            tags$ul(
              tags$li(
                "This section draws inspiration from the ShinyCell R package.", 
                tags$a(href = 'https://github.com/SGDDNB/ShinyCell', "Explore the ShinyCell GitHub Repository")
              ),
              tags$li("The cell datatable is linked to the cell information plot (axes)."),
              tags$li("Similarly, the co-expressed gene datatable is controlled by the co-expressed plot box."),
              tags$li("For the heatmap/bubble plot, two methods are available for gene selection: individual selection or submitting a list of genes.")
            )
          )
          ) 
      )
    ),
   fluidRow(
     column(7,box(
     title = HTML(paste("Method workflow (Temporary Version)")),
     status = "info", solidHeader = TRUE,collapsible = TRUE,
     width = 12, 
     tags$img(src = "./images/flowcharts.png", class = "responsive-logo"))),        
     
     column(5,   # Dropdown Bullet Points Box
            box(
              title = HTML(paste("Supplemental information")),
              status = "primary", solidHeader = TRUE,collapsible = TRUE,
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
                      "Bulk RNA-Seq data."
                    ),
                    tags$div(id = "collapsedatabulk", class = "collapse dropdown-content",
                             tags$ul(
                               tags$li("Kunz et al. 2018: RNA-seq analysis identifies different transcriptomic types and developmental trajectories of primary melanomas"),
                               tags$li("Gide et al. 2019: Distinct Immune Signatures Define Response to Anti-PD-1 Monotherapy and Anti-PD-1/Anti-CTLA-4 Combined Therapy"),
                               tags$li("Badal et al. 2018: Transcriptional dissection of melanoma identifies a high-risk subtype underlying TP53 family genes and epigenome deregulation"),
                               tags$li("Fisher et al. 2019: Molecular Profiling Reveals Unique Immune and Metabolic Features of Melanoma Brain Metastases"),
                             )
                    ),
                    tags$li(
                      class = "clickable collapsed", 
                      `data-toggle` = "collapse", `data-target` = "#collapsedatasingle", 
                      "Single-cell RNA-Seq data.",
                    ),
                    tags$div(id = "collapsedatasingle", class = "collapse dropdown-content",
                             tags$ul(
                               tags$li("Qi Sun et al. 2019: A novel mouse model demonstrates that oncogenic melanocyte stem cells engender melanoma resembling human disease"),
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
              status = "primary", solidHeader = FALSE,collapsible = TRUE,
              width = 12,
              div(class = ".content_home",
                  tags$p(
                    "Please take a moment to complete our survey: ",
                    tags$a(href = "https://forms.gle/m3o8FMRt3NLPFMrC8", "Take the Survey")
                  )
              )
            ),
            box(
              title = HTML(paste("Paper")),
              status = "primary", solidHeader = TRUE,collapsible = TRUE,
              width = 12,
              tags$p(
                "For additional information on using MelOD! or to cite MelOD! in your work, please refer to the following paper: Coming soon!!"
              )
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
