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
            "<b>Welcome to MelOD</b>, the Melanoma Omics Dashboard. This RShiny application serves as your gateway to analyzing and visualizing melanoma cancer datasets. 
            Designed with researchers in mind, MelOD provides a suite of powerful tools to explore, visualize, and make sense of complex data. Whether you're a seasoned bioinformatician or just starting your journey in omics, MelOD offers a user-friendly environment tailored to your needs."
          )),
          tags$p(HTML(
            "The main objective of MelOD is to <b>provide accessible, high-quality, and interactive visualizations</b> that can help accelerate discoveries in melanoma research. By integrating data from multiple studies, including transcriptiomics, single-cell RNA-Seq, and proteomics (with more to come), MelOD allows for a comprehensive examination of the molecular landscape of melanoma."
          )),
          tags$p(
            "In addition to offering a range of pre-built visualizations, MelOD empowers users to dive deep into the data, customize plots, and even download datasets for offline analysis. With an intuitive interface, the application streamlines the process of data exploration, making it easier than ever to generate insights and hypotheses that can drive further research."
          ),
          tags$p(
            "So go ahead, explore the different tabs, load your favorite datasets, and discover the stories hidden within the data. MelOD is here to make your research experience not only productive but also enjoyable!"
          )
      )
    )),
    column(4, 
      tags$img(src = "./images/melod_large_logo_old.png", class = "responsive-logo")
    )),
    # App Utilization Box
   box(
      title = HTML(paste("App Utilization")),
      status = "primary", solidHeader = TRUE,collapsible = TRUE,
      width = 12,
      div(class = ".content_home",
          tags$p(tags$img(src = "./images/arrow_home.png", width = "50px", class = "image-inline"),
                 "Each study is accessible via the dropdown menu on the sidebar. Studies are classified by the type of data they contain."),
          tags$img(src = "./images/home_page_utilization.png", class = "responsive-workflow"),
          tags$div(class = "dropdown-header", "Specific functionality"),
          tags$ul(
          tags$div(
            class = "clickable collapsed", 
            `data-toggle` = "collapse", `data-target` = "#collapseInstructionsbulkrna", 
            "Transcriptomic/Proteomic Tab"
          ),
          tags$div(
            id = "collapseInstructionsbulkrna", 
            class = "collapse dropdown-content",
            tags$ul(
              tags$li("The PCA box provides key dataset information about the study sample. Additionally, if available, a metadata box offers insights into patient mortality."),
              tags$li("You can select genes directly by clicking on the dots within the volcano plot."),
              tags$li("The threshold of the padjust value significance on the violin plot is defined by your input cutoff.")
              
            )
          ),
          tags$div(
            class = "clickable collapsed", 
            `data-toggle` = "collapse", 
            `data-target` = "#collapseInstructionssinglecell", 
            "Single Cell RNAseq Tab"
          ),
          tags$div(
            id = "collapseInstructionssinglecell", 
            class = "collapse dropdown-content",
            tags$ul(
              tags$li(
                "This section draws inspiration from the ShinyCell R package.", 
                tags$a(href = 'https://github.com/SGDDNB/ShinyCell', "Explore the ShinyCell GitHub Repository")
              ),
              tags$li("The cell datatable is linked to the cell information plot (axes) input."),
              tags$li("You can subset cells for the downstream analysis using the input box."),
              tags$li("Similarly, the co-expressed gene datatable is controlled by the co-expressed plot box."),
              tags$li("For the heatmap/bubble plot, you can search for a gene using the search bar or submit a list of genes (currently, the list is case sensitive, so watch out!")
            )
          ),
          tags$div(
            class = "clickable collapsed", 
            `data-toggle` = "collapse", 
            `data-target` = "#collapseInstructionsgenesearch", 
            "Gene search Tab"
          ),
          tags$div(
            id = "collapseInstructionsgenesearch", 
            class = "collapse dropdown-content",
            tags$ul(
              tags$li("This page is designed specifically for transcriptomic studies."),
              tags$li("It allows you to search and filter genes based on specific parameters. 
                        You can adjust the p-value and log2 fold change cutoffs to filter genes on the volcano plot, 
                        select specific genes, and choose studies of interest.")
            )
          )
          ) 
      )
    ),
   fluidRow(
     column(6,box(
     title = HTML(paste("Method workflow")),
     status = "info", solidHeader = TRUE,collapsible = TRUE,
     width = 12, 
     tags$img(src = "./images/flowcharts.png", class = "responsive-workflow"))),        
     
     column(
       6,
       box(
         title = HTML(paste("Supplemental Information")),
         status = "primary",
         solidHeader = TRUE,
         collapsible = TRUE,
         width = 12,
         div(class = "content_home",
             
             # Section: News about Updates
             tags$div(class = "dropdown-header", "News and Updates"),
             tags$ul(
               tags$li("MelOD Version 0.4 released with new features and enhanced data analysis capabilities."),
               tags$li("Integration of Biermann & Izar, 2022. | Jerby-Arnon & Regev, 2018. | Pozniak et al., 2022. | Rambow et al., 2018."),
               tags$li("Added a gene search feature for transcriptomics datasets."),
               tags$li("Other new features include survival curve statistics, a box explanation of the methods used, and a download button for processed and raw data."),
               tags$br()
             )
             ),
             
             # Section: Currently Working on
         tags$div(class = "dropdown-header", "Currently Working On:"),
         tags$ul(
           tags$li("Processing new datasets for future integration (Tirosh et al., 2016 | Sade-Feldman et al., 2018 | Li et al., 2022 | Zhang et al., 2018.)"),
           tags$li("Trying to fetch clinical information from the current dataset for survival curves."),
           tags$li("Enhancing features and optimizing the app for a better user experience.")
         ),
             # Section: What Data Formats Are Supported?
             tags$div(class = "dropdown-header", "What Dataset are available?"),
             tags$p(class = "dropdown-header-notice","Please be aware that the data has undergone reanalysis, and the results shown here may differ from the published data."),         
             tags$ul(
               
               ## Bulk RNA-Seq Data
               tags$li(
                 class = "clickable collapsed", 
                 `data-toggle` = "collapse", 
                 `data-target` = "#collapsedatabulk", 
                 "Transcriptomics data."
               ),
               tags$div(
                 id = "collapsedatabulk", 
                 class = "collapse dropdown-content",
                 tags$ul(
                   tags$li(
                     "Kunz et al. 2018: RNA-seq analysis identifies different transcriptomic types and developmental trajectories of primary melanomas.",
                     tags$br(),
                     tags$span(class = "comparison",  tags$b("Comparison Groups:"), " Naevi vs. Melanoma.")
                   ),
                   tags$li(
                     "Gide et al. 2019: Distinct Immune Signatures Define Response to Anti-PD-1 Monotherapy and Anti-PD-1/Anti-CTLA-4 Combined Therapy.",
                     tags$br(),
                     tags$span(class = "comparison", tags$b("Comparison Groups:"), " Combination Therapy vs. Monotherapy.")
                   ),
                   tags$li(
                     "Badal et al. 2017: Transcriptional dissection of melanoma identifies a high-risk subtype underlying TP53 family genes and epigenome deregulation.",
                     tags$br(),
                     tags$span(class = "comparison", tags$b("Comparison Groups:"), " Melanoma vs. Benign.")
                   ),
                   tags$li(
                     "Fisher et al. 2019: Molecular Profiling Reveals Unique Immune and Metabolic Features of Melanoma Brain Metastases.",
                     tags$br(),
                     tags$span(class = "comparison", tags$b("Comparison Groups:"), " Brain Metastases vs. Extracranial Metastases.")
                   ),
                   tags$li(
                     "Tsoi et al. 2018: Multi-stage differentiation defines melanoma subtypes with differential vulnerability to drug-induced iron-dependent oxidative stress.",
                     tags$br(),
                     tags$span(class = "comparison", tags$b("Comparison Groups:"), " Neural crest like vs Undifferentiated, Transitory vs Undifferentiated, Melanocytic vs Undifferentiated, Neural crest like vs Transitory, Neural crest like vs Melanocytic, Transitory vs Melanocytic.")
                   ),
                   tags$li(
                     "Riaz et al. 2017: Tumor and Microenvironment Evolution during Immunotherapy with Nivolumab.",
                     tags$br(),
                     tags$span(class = "comparison", tags$b("Comparison Groups:"), "Responding vs. Non-responding")
                   ),
                   tags$li(
                     "Hugo et al. 2016: Genomic and Transcriptomic Features of Response to Anti-PD-1 Therapy in Metastatic Melanoma.",
                     tags$br(),
                     tags$span(class = "comparison", tags$b("Comparison Groups:"), "Progressive disease vs. Complete or Partial Response.")
                   )
                 )
               ),
               
               ## Single-cell RNA-Seq Data
               tags$li(
                 class = "clickable collapsed", 
                 `data-toggle` = "collapse", 
                 `data-target` = "#collapsedatasingle", 
                 "Single-cell RNA-Seq data."
               ),
               tags$div(
                 id = "collapsedatasingle", 
                 class = "collapse dropdown-content",
                 tags$ul(
                   tags$li(
                     "Qi Sun et al. 2019: A novel mouse model demonstrates that oncogenic melanocyte stem cells engender melanoma resembling human disease.",
                     tags$br(),
                   tags$span(class = "comparison", tags$b("Comparison Groups:"), " TBPT.6wk.mel (tdTom+ melanoma) vs. TT (tdTom+ melanocyte stem cells - untransformed).")
                 ),
                 tags$li(
                   "Biermann et Izar, 2022: Dissecting the treatment-naive ecosystem of human melanoma brain metastasis.",
                   tags$br(),
                   tags$span(class = "comparison", tags$b("Comparison Groups:"), "TBD.")
                 ),
                 tags$li(
                   "Jerby-Arnon & Regev, 2018: A Cancer Cell Program Promotes T Cell Exclusion and Resistance to Checkpoint Blockade.",
                   tags$br(),
                   tags$span(class = "comparison", tags$b("Comparison Groups:"), "TBD.")
                 ),
                 tags$li(
                   "Pozniak et al., 2022: A TCF4/BRD4-dependent regulatory network confers cross-resistance to targeted and immune checkpoint therapy in melanoma.",
                   tags$br(),
                   tags$span(class = "comparison", tags$b("Comparison Groups:"), "TBD.")
                 ),tags$li(
                   "Rambow et al., 2018: Toward Minimal Residual Disease-Directed Therapy in Melanoma.",
                   tags$br(),
                   tags$span(class = "comparison", tags$b("Comparison Groups:"), "TBD.")
                 )
               )),
               
               tags$div(
                    tags$li(
                      class = "clickable collapsed", 
                      `data-toggle` = "collapse", `data-target` = "#collapsedataproteomic", 
                      "Proteomics data.",
                    ),
                    tags$div(id = "collapsedataproteomic", class = "collapse dropdown-content",
                             tags$ul(
                               tags$li("Kleffman et al. 2022: Melanoma-secreted Amyloid Beta Suppresses Neuroinflammation and Promotes Brain Metastasis",
                                       tags$br(),
                                       
                               tags$span( class = "comparison", tags$b("Comparison Groups:"), " Brain Metastases (BM) vs. Extracranial Metastases (NBM)."
                               )
                             )
                    ),
                    
                  )
              ),
              tags$li("Array-based data (coming soon).")),
    
            
        
      
     
      
       ),
       # Survey and Citation Box
       box(
         title = HTML(paste("We value your feedback!")),
         status = "primary", solidHeader = FALSE,collapsible = TRUE,
         width = 12,
         div(class = ".content_home",
             tags$p(
               "Please take a moment to complete our survey: ",
               tags$a(href = "https://forms.gle/CgxYffiZ7NQA5VpM6", "Take the Survey")
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
       
       
       )))  

}

home_server <- function(id) {
  #' Home Server
  #'
  #' @description Sets up the server logic for the Home analysis tab. Nothing too complicated, we promise!
  
  moduleServer(id, function(input, output, session) {
  })
}
