fischer_ui <- function(id) {
#' Fischer UI
#'
#' @description Creates the UI layout for the Fischer analysis tab
#' @param id Module ID
#' 
#' @return A Shiny UI element for the Fischer analysis tab
    
  fluidPage(
      fluidRow(
        blurb_study_ui("fischer")),
      fluidRow(
        column(6, blurb_data_ui("fischer")),
        column(6, blurb_method_ui("fischer",TRUE))),
      fluidRow( 
        column(6,blurb_comparison_ui("fischer"))),
      fluidRow(
        column(6,pca_ui("fischer")),
        column(6,metadata_ui("fischer"))),
      fluidRow(
        column(4,input_ui("fischer", 0.05, 2)),
        column(8,deseq2_table_ui("fischer"))),
      fluidRow(
        column(6,volcano_ui("fischer")),
        column(6,violin_ui("fischer"))),
      fluidRow(
        column(8,heatmap_ui("fischer")),
        column(4, correlation_ui("fischer")))
    )
}

fischer_server <- function() {

#' Fischer Server
#'
#' @description Sets up the server logic for the Fischer analysis tab
  
  show_modal_progress_line(color = "#FFA812",text = "Initialization") # show the modal window
  Sys.sleep(0.5)
  
  tryCatch({
  update_modal_progress(0.3, text="Downloading data...") 
  Sys.sleep(0.5)
  
  fischer <- drive_download("Fischer_Deseq2_2.rds", overwrite = TRUE)
  fischer_dds <- readRDS(fischer$local_path)

  update_modal_progress(0.6, text="Loading data...") 
  Sys.sleep(0.5)
  
  dds <- list(fischer_dds)
  
  update_modal_progress(0.8, text="Loading clinical data...")
  Sys.sleep(0.5)
  
  clinical_data_drive <- drive_download("Fischer_demographics_information_Final.csv", overwrite = TRUE)
  clinical_data <- list(read.csv(clinical_data_drive$local_path))
  
  
  
  # List of all downloaded file paths
  downloaded_files <- list(
    fischer$local_path,
    clinical_data_drive$local_path
  )
  
  # Remove downloaded files if they exist
  for (file_path in downloaded_files) {
    if (file.exists(file_path)) {
      file.remove(file_path)
    }
  }
  
  update_modal_progress(0.9, text="Initializing servers...") 
  Sys.sleep(0.5)
  
  observe_helpers()
  
  selection_result <- selection_server(dds, "fischer", clinical_data)
  input_server("fischer", selection_result)
  volcano_server("fischer", selection_result)
  violin_server("fischer", selection_result)
  correlation_server("fischer", selection_result)
  heatmap_server("fischer", selection_result)
  pca_metadata_server("fischer", selection_result) 
  
  update_modal_progress(1, text="Finalizing")
  Sys.sleep(0.5)
  
  }, error = function(e) {
    showNotification("An error occurred during the process.", type = "error")
    print(e)    
    
  })
  remove_modal_progress() 
}