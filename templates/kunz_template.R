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
  ## Show a modal dialog indicating that loading is in progress
  showModal(modalDialog(
    title = "Please Wait",
    "Downloading and loading data...",
    easyClose = FALSE,
    footer = NULL
  ))
  
  ## Download the file in current wd and save the object
  ## Wrap in tryCatch to handle errors
  tryCatch({
    kunz <- drive_download("Kunz_Deseq2.rds", overwrite = TRUE)
    kunz_dds <- readRDS(kunz$local_path)
    
    ## Delete the file once it's loaded to save on storage space
    if (file.exists(kunz$local_path)) {
      file.remove(kunz$local_path)
    }
    
    dds <- list(kunz_dds)
    # Load data
    clinical_data <- list(read.csv(file = "./data/badal/clinical_data.csv", sep = ";"))
  

  # Initialize servers
  selection_result <- selection_server(dds, clinical_data, "kunz")
  input_server("kunz", selection_result)
  volcano_server("kunz", selection_result)
  violin_server("kunz", selection_result)
  correlation_server("kunz", selection_result)
  heatmap_server("kunz", selection_result)
  pca_metadata_server("kunz", selection_result)
  
  
  ## Close the modal after the loading is complete
  removeModal()
  }, error = function(e) {
    ## Error handling
    removeModal()
    showModal(modalDialog(
      title = "Error",
      paste("An error occurred:", e$message),
      easyClose = TRUE
    ))
  })
}
