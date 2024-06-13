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
      column(4,input_ui("kunz")),
      column(8,deseq2_table_ui("kunz"))),
    fluidRow(
      column(6,volcano_ui("kunz")),
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
  # Initialize the progress bar
  progress <- shiny::Progress$new()
  on.exit(progress$close())
  progress$set(message = "Initializing", value = 0)
  
  ## Download the file in current wd and save the object
  ## Wrap in tryCatch to handle errors
  tryCatch({
    progress$set(message = "Downloading data...", value = 0.3)
    kunz <- drive_download("Kunz_Deseq2.rds", overwrite = TRUE)
    
    progress$set(message = "Loading data...", value = 0.6)
    kunz_dds <- readRDS(kunz$local_path)
    
    ## Delete the file once it's loaded to save on storage space
    if (file.exists(kunz$local_path)) {
      file.remove(kunz$local_path)
    }
    
    dds <- list(kunz_dds)
    
    # Load additional data
    progress$set(message = "Loading clinical data...", value = 0.8)
    clinical_data <- list(read.csv(file = "./data/bulk_rna/badal/clinical_data.csv", sep = ";"))
    
    progress$set(message = "Initializing servers...", value = 0.9)
    
    # Initialize servers
    selection_result <- selection_server(dds, clinical_data, "kunz")
    input_server("kunz", selection_result)
    volcano_server("kunz", selection_result)
    violin_server("kunz", selection_result)
    correlation_server("kunz", selection_result)
    heatmap_server("kunz", selection_result)
    pca_metadata_server("kunz", selection_result)
    
    # Finalize progress
    progress$set(message = "Finalizing", value = 1)
  }, error = function(e) {
    ## Error handling
    progress$close()
    showModal(modalDialog(
      title = "Error",
      paste("An error occurred:", e$message),
      easyClose = TRUE
    ))
  })
}
