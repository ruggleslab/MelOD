#' Kunz UI
#'
#' @description Creates the UI layout for the Kunz analysis tab
#' @param id Module ID
#' @return A Shiny UI element for the Kunz analysis tab
kunz_ui <- function(id) {
  fluidPage(
    add_busy_spinner(spin = "fading-circle", color = "#FFA812"),
    fluidRow(
      blurb_study_ui("kunz")),
    fluidRow(
      column(6, blurb_data_ui("kunz")),
      column(6, blurb_comparison_ui("kunz"))),
    fluidRow(
      column(6,pca_ui("kunz")),
      column(6,metadata_ui("kunz"))),
    fluidRow(
      column(6,input_ui("kunz")),
      column(6,volcano_ui("kunz"))),
    fluidRow(
      column(6,deseq2_table_ui("kunz")),
      column(6,violin_ui("kunz"))),
    fluidRow(
      column(8,heatmap_ui("kunz")),
      column(4, correlation_ui("kunz")))
   )
}

#' Kunz Server
#'
#' @description Sets up the server logic for the Kunz analysis tab
kunz_server <- function() {
  
  # Load data
  dds <- list(readRDS(file.path("./data/kunz", "Kunz_Deseq2.rds")))
  clinical_data <- list(read.csv(file = "./data/badal/clinical_data.csv", sep = ";"))
  
  # Initialize servers
  selection_server(dds, clinical_data, "kunz")
  input_server("kunz")
  differential_gene_server("kunz")

  pca_metadata_server("kunz")
  heatmap_server("kunz")
  correlation_server("kunz")  
}


