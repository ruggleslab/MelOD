#' Fischer UI
#'
#' @description Creates the UI layout for the Fischer analysis tab
#' @param id Module ID
#' @return A Shiny UI element for the Fischer analysis tab
fischer_ui <- function(id) {
    fluidPage(
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
  

  show_modal_progress_line(color = "#FFA812",text = "Initialization") # show the modal window
  Sys.sleep(0.5)

  tryCatch({
    
    update_modal_progress(0.3, text="Downloading data...") 
    Sys.sleep(0.5)
    
    fischer <- drive_download("Fischer_Deseq2.rds", overwrite = TRUE)
    fischer_dds <- readRDS(fischer$local_path)
    
    if (file.exists(fischer$local_path)) {
      file.remove(fischer$local_path)
    }
  
  update_modal_progress(0.6, text="Loading data...") 
  Sys.sleep(0.5)
  
  dds <- list(fischer_dds)
  
  update_modal_progress(0.8, text="Loading clinical data...")
  Sys.sleep(0.5)
  
  clinical_data <- list(read.csv("./data/bulk_rna/fischer/Fischer_demographics_information_Final.csv"))
  
  update_modal_progress(0.9, text="Initializing servers...") 
  Sys.sleep(0.5)
  
  selection_result <- selection_server(dds, clinical_data, "fischer")
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