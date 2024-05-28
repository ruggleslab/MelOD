#' Fischer UI
#'
#' @description Creates the UI layout for the Fischer analysis tab
#' @param id Module ID
#' @return A Shiny UI element for the Fischer analysis tab
fischer_ui <- function(id) {
    fluidPage(
      add_busy_spinner(spin = "fading-circle", color = "#FFA812"),
      fluidRow(
        blurb_study_ui("fischer")),
      fluidRow(
        column(6, blurb_data_ui("fischer")),
        column(6, blurb_comparison_ui("fischer"))),
      fluidRow(
        column(6,pca_ui("fischer")),
        column(6,metadata_ui("fischer"))),
      fluidRow(
        column(4,input_ui("fischer")),
        column(8,deseq2_table_ui("fischer"))),
      fluidRow(
        column(6,volcano_ui("fischer")),
        column(6,violin_ui("fischer"))),
      fluidRow(
        column(8,heatmap_ui("fischer")),
        column(4, correlation_ui("fischer")))
    )
}



#' Fischer Server
#'
#' @description Sets up the server logic for the Fischer analysis tab
fischer_server <- function() {
  
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
    fischer <- drive_download("Fischer_Deseq2.rds", overwrite = TRUE)
    fischer_dds <- readRDS(fischer$local_path)
    
    ## Delete the file once it's loaded to save on storage space
    if (file.exists(fischer$local_path)) {
      file.remove(fischer$local_path)
    }
    
  dds <- list(fischer_dds)
  clinical_data <- list(read.csv("./data/fischer/Fischer_demographics_information_Final.csv"))
  
  # Initialize servers
  selection_result <- selection_server(dds, clinical_data, "fischer")
  input_server("fischer", selection_result)
  volcano_server("fischer", selection_result)
  violin_server("fischer", selection_result)
  correlation_server("fischer", selection_result)
  heatmap_server("fischer", selection_result)
  pca_metadata_server("fischer", selection_result) 
  
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

