source("global.R", local = TRUE)

blurbs <- "./www/data_blurbs.json" 
blurbs_info <- fromJSON(blurbs)



#' Blurb Study UI
#' 
#' @description Creates the UI component for displaying the study overview
#' @param id Module ID
#' @return A Shiny UI element
blurb_study_ui <- function(id) {
  Id_info <- blurbs_info[[paste(id, "info", sep = "_")]]
  box(
    title = "Study Overview", status = "info", solidHeader = TRUE, width = 12, collapsible = TRUE,
    tags$h2(Id_info$title),
    tags$h3("Lead Author: ", Id_info$lead_author),
    tags$p(Id_info$abstract),
    tags$p("Read the full paper: ", tags$a(href = Id_info$paper_link, "PubMed")),
    tags$p("DOI: ", tags$a(href = paste("https://doi.org/", Id_info$doi, sep = ""), Id_info$doi)),
    tags$p("Data Access: ", tags$a(href = Id_info$data_link, "ENA Dataset"))
  )
}

#' Blurb Data UI
#' 
#' @description Creates the UI component for explaining the data used in the analysis
#' @param id Module ID
#' @return A Shiny UI element
blurb_data_ui <- function(id) {
  Id_info <- blurbs_info[[paste(id, "info", sep = "_")]]
  box(
    title = "Data used in analysis", status = "info",
    collapsible = TRUE,
    solidHeader = TRUE, width = 12,
    tags$p(Id_info$data_explanation)
  )
}

#' Blurb Comparison UI
#' 
#' @description Creates the UI component for explaining the comparison done in the analysis
#' @param id Module ID
#' @return A Shiny UI element
blurb_comparison_ui <- function(id) {
  Id_info <- blurbs_info[[paste(id, "info", sep = "_")]]
  box(
    status = "warning",
    background = "orange", width = 12,
    tags$p(Id_info$data_comparison)
  )
}

#' PCA UI
#' 
#' @description Creates the UI component for displaying PCA plot
#' @param id Module ID
#' @return A Shiny UI element (box)
pca_ui <- function(id) {
  ns <- NS(id)
  
  tabBox(
    title = HTML(paste("PCA", actionLink(ns("info_pca_plot"), label = "", icon = icon("info-circle")), downloadButton(ns('pca_data'), label = "", icon = icon("save-file", lib = "glyphicon")))),
    width = 12,
    tabPanel("PCA",
             withSpinner(plotlyOutput(ns('pca_plot')),type = 6, color = "#FFA812", size = 0.5),
             fluidRow(
               column(6,
                      selectInput(
                        inputId = ns("size_by"),
                        label = "Size by:",
                        choices = c("Constant" = "constant", "Size Factor" = "size_factor"),
                        selected = "constant"
                      )
               ),
               column(6,
                      selectInput(
                        inputId = ns("color_by"),
                        label = "Color by:",
                        choices = c("Condition" = "condition", "Sample" = "sample"),
                        selected = "condition"
                      )
               )
             )
    ),
    tabPanel("Variance",
             withSpinner(plotlyOutput(ns('variance_plot')),type = 6, color = "#FFA812", size = 0.5),
    )
  )
}

metadata_ui <- function(id) {
  #' Metadata UI
  #' 
  #' @description Creates the UI component for displaying metadata plots
  #' @param id Module ID
  #' @return A Shiny UI element (tab box)
  
  ns <- NS(id)
  tabBox(
    title = HTML(paste("Metadata", actionLink(ns("info_metadata_plot"), label = "", icon = icon("info-circle")), downloadButton(ns('metadata_data'), label = "", icon = icon("save-file", lib = "glyphicon")))),    
    id = "tabset1",
    width = 12,
    tabPanel("Mortality by condition", withSpinner(uiOutput(ns("mortality_by_condition")),type = 6, color = "#FFA812", size = 0.5)),
    tabPanel("Mortality by gene", withSpinner(uiOutput(ns("mortality_by_gene") ,type = 6, color = "#FFA812", size = 0.5)),
                      selectizeInput(ns("gene_mortality"), "Gene", choices = NULL, selected = NULL, multiple = FALSE, options = list(maxItems = 1))),
  )
}


#' Input UI
#' 
#' @description Creates the UI component for input parameters
#' @param id Module ID
#' @return A Shiny UI element
input_ui <- function(id) {
  ns <- NS(id)
  box(
    title = "Inputs", status = "warning",
    collapsible = TRUE,
    solidHeader = TRUE, width = 12,
    tags$h3("Parameters", style = "margin-top: 0;"),
             numericInput(ns("slider_padj"), "padj Cutoff", 0.05, min = 0, max = 1, step = 0.01),
             numericInput(ns("slider_log2"), "log2foldchange Cutoff", 2, step = 0.1),
             selectizeInput(ns("selected_gene"), "Gene(s) selection (up to 10)", choices = NULL, selected = NULL, multiple = TRUE, options = list(maxItems = 10)),
             numericInput(ns("number"), "Number of genes for the heatmap (min. 2 if no genes selected)", 10, min = 0, step = 1),
             actionButton(ns("update_plot"), "Generate plots", class = "btn-primary"),
             actionButton(ns("reset_selection"), "Reset Selection", class = "btn-primary")
    
             
    
  )
}


#' Volcano UI
#' 
#' @description Creates the UI component for displaying volcano plot
#' @param id Module ID
#' @return A Shiny UI element
volcano_ui <- function(id) {
  ns <- NS(id)
  fluidRow(
    box(
      title = HTML(paste("Volcano plot", actionLink(ns("info_volcano_plot"), label = "", icon = icon("info-circle")), downloadButton(ns('volcano_data'), label = "", icon = icon("save-file", lib = "glyphicon")))),
      status = "primary",
      width = 12,
      solidHeader = TRUE,
      collapsible = TRUE,
      plotlyOutput(ns("volcano_plot"))
     
    )
  )
}


#' Violin UI
#' 
#' @description Creates the UI component for displaying violin plot
#' @param id Module ID
#' @return A Shiny UI element
violin_ui <- function(id) {
  ns <- NS(id)
  fluidRow(
    box(
      title = HTML(paste("Violin plot", actionLink(ns("info_violin_plot"), label = "", icon = icon("info-circle")), downloadButton(ns('violin_data'), label = "", icon = icon("save-file", lib = "glyphicon")))),
      status = "primary",
      width = 12,
      solidHeader = TRUE,
      collapsible = TRUE,
      withSpinner(plotlyOutput(ns(id = 'violin_plot')),type = 6, color = "#FFA812", size = 0.5),
    )
  )
}



#' Heatmap UI
#' 
#' @description Creates the UI component for displaying heatmaps
#' @param id Module ID
#' @return A Shiny UI element
heatmap_ui <- function(id) {
  ns <- NS(id)
  box(
      title = HTML(paste("Heatmap", actionLink(ns("info_heatmap_plot"), label = "", icon = icon("info-circle")), downloadButton(ns('heatmap_data'), label = "", icon = icon("save-file", lib = "glyphicon")))),
      status = "primary",
      width = 12,
      solidHeader = TRUE,
      collapsible = TRUE,

      withSpinner(uiOutput(ns("heatmap_plot")),type = 6, color = "#FFA812", size = 0.5),
      br(),
      br(),
      br(),
      br(),
      
  )
}
#' Correlation UI
#'
#' @description Creates the UI layout for the correlation plot
#' @param id Module ID
#' @return A Shiny UI element for the correlation plot
correlation_ui <- function(id) {
  ns <- NS(id)
  box(
    title = HTML(paste("Correlation", actionLink(ns("info_correlation_plot"), label = "", icon = icon("info-circle")), downloadButton(ns('correlation_data'), label = "", icon = icon("save-file", lib = "glyphicon")))),
    width = 12,
    solidHeader = TRUE,
    status = "primary",
    
    withSpinner(uiOutput(ns('correlation_plot')),type = 6, color = "#FFA812", size = 0.5),
    fluidRow(useShinyjs(),
             column(6,selectizeInput(ns("gene_of_interest"), "Gene of interest for correlation", choices = NULL, selected = NULL, multiple = FALSE, options = list(maxItems = 1))),
             column(6,numericInput(ns("correlation_threshold"), "Correlation threshold", value = 0.2, min = 0, max = 1, step = 0.1))
    )
    )
}

#' DESeq2 Table UI
#' 
#' @description Creates the UI component for displaying DESeq2 results in a table
#' @param id Module ID
#' @return A Shiny UI element
deseq2_table_ui <- function(id) {
  ns <- NS(id)
  fluidRow(
    box(
      width = 12,
      solidHeader = TRUE,
      title = "DESeq2 Results", status = "info", collapsible = TRUE,
      DT::dataTableOutput(ns("filtered_results"))
    )
  )
}
