source("global.R", local = TRUE)

blurbs <- "./www/data_blurbs.json" 
blurbs_info <- fromJSON(blurbs)

#' Blurb Explanation UI
#' 
#' @description Creates the UI component for explaining the data used in the analysis
#' @param id Module ID
#' @return A Shiny UI element
blurb_explanation_ui <- function(id) {
  Id_info <- blurbs_info[[paste(id, "info", sep = "_")]]
  box(
    title = "Explanation of data used in analysis:", status = "info",
    collapsible = TRUE,
    solidHeader = TRUE, width = 12,
    tags$p(Id_info$data_explanation)
  )
}

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
    tags$p("Read the full paper: ", tags$a(href = Id_info$paper_link, "PubMed", target = "_blank")),
    tags$p("DOI: ", tags$a(href = paste("https://doi.org/", Id_info$doi, sep = ""), Id_info$doi, target = "_blank")),
    tags$p("Data Access: ", tags$a(href = Id_info$data_link, "ENA Dataset", target = "_blank"))
  )
}


#' Gide Selector UI
#' 
#' @description Creates the UI component for selecting comparisons in the Gide study
#' @param id Module ID
#' @return A Shiny UI element
gide_selector_ui <- function(id) {
  ns <- NS(id)
  box(
    background = 'orange', width = 12,
    radioButtons(
      label = "Select comparison",
      inputId = ns("selection"),
      choices = c("Combotherapy", "Monotherapy"),
      selected = "Combotherapy",
      inline = TRUE
    ),
    uiOutput(ns("debug_selection"))
  )
}

#' Badal Selector UI
#' 
#' @description Creates the UI component for selecting comparisons in the Badal study
#' @param id Module ID
#' @return A Shiny UI element
badal_selector_ui <- function(id) {
  ns <- NS(id)
  box(
    background = 'orange', width = 12,
    radioButtons(
      label = "Select comparison",
      inputId = ns("selection_badal"),
      choices = c("Gene", "Tumor Stage"),
      selected = "Gene",
      inline = TRUE
    ),
    uiOutput(ns("debug_selection"))
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
    numericInput(ns("number"), "Number of genes for the heatmap (min. 2 if no genes selected)", 10, min = 0, step = 1),
    selectizeInput(ns("selected_gene"), "Gene(s) selection (up to 10)", choices = NULL, selected = NULL, multiple = TRUE, options = list(maxItems = 10)),
    actionButton(ns("update_plot"), "Generate plots", class = "btn-primary")
  )
}

#' PCA Metadata UI
#' 
#' @description Creates the UI component for displaying PCA plots and metadata
#' @param id Module ID
#' @return A Shiny UI element
pca_metadata_ui <- function(id) {
  ns <- NS(id)
  fluidRow(
    box(
      title = HTML(paste("PCA", actionLink(ns("info_pca_plot"), label = "", icon = icon("info-circle")), downloadButton(ns('pca_data'), label = "", icon = icon("save-file", lib = "glyphicon")))),
      status = "primary",
      width = 8,
      solidHeader = TRUE,
      collapsible = TRUE,
      plotlyOutput(ns(id = 'pca_plot'))
    ),
    tabBox(
      title = "Metadata",
      id = "tabset1",
      width = 4,
      tabPanel("Mortality", plotlyOutput(ns("mortality"))),
      tabPanel("Gender", plotlyOutput(ns("gender_data")))
    )
  )
}

#' Differential Gene UI
#' 
#' @description Creates the UI component for displaying differential gene expression plots
#' @param id Module ID
#' @return A Shiny UI element
differential_gene_ui <- function(id) {
  ns <- NS(id)
  fluidRow(
    box(
      title = HTML(paste("Differential gene expression", actionLink(ns("info_violin_plot"), label = "", icon = icon("info-circle")), downloadButton(ns('violin_data'), label = "", icon = icon("save-file", lib = "glyphicon")))),
      status = "primary",
      width = 12,
      solidHeader = TRUE,
      collapsible = TRUE,
      column(6, plotlyOutput(ns("volcano_plot"))),
      column(6, plotlyOutput(ns(id = 'violin_plot')))
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
  fluidRow(
    box(
      title = HTML(paste("Heatmap", actionLink(ns("info_heatmap_plot"), label = "", icon = icon("info-circle")), downloadButton(ns('heatmap_data'), label = "", icon = icon("save-file", lib = "glyphicon")))),
      status = "primary",
      width = 12,
      solidHeader = TRUE,
      collapsible = TRUE,
      plotlyOutput(ns("heatmap_plot"))
    )
  )
}

#' Correlation UI
#' 
#' @description Creates the UI component for displaying correlation plots
#' @param id Module ID
#' @return A Shiny UI element
correlation_ui <- function(id) {
  ns <- NS(id)
  fluidRow(
    box(
      title = "Correlation",
      status = "primary",
      width = 12,
      solidHeader = TRUE,
      collapsible = TRUE,
      plotlyOutput(ns("correlation"))
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
      title = "DESeq2 Results", status = "info", collapsible = TRUE,
      DT::dataTableOutput(ns("filtered_results"))
    )
  )
}
