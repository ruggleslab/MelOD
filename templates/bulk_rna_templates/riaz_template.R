riaz_ui <- function(id) {
  #' riaz UI
  #'
  #' @description Creates the UI layout for the riaz analysis tab
  #' @param id Module ID
  #' 
  #' @return A Shiny UI element for the riaz analysis tab
  
  fluidPage(
    fluidRow(
      blurb_study_ui("riaz")),
    fluidRow(
      column(6, blurb_data_ui("riaz")),
      column(6, blurb_method_ui("riaz"))),
    fluidRow( 
      column(6,blurb_comparison_ui("riaz"))),
    fluidRow(
      column(6,pca_ui("riaz")),
      column(6,metadata_ui("riaz"))),
    fluidRow(
      column(4,input_ui("riaz", 0.05, 2)),
      column(8,deseq2_table_ui("riaz"))),
    fluidRow(
      column(6,volcano_ui("riaz")),
      column(6,violin_ui("riaz"))),
    fluidRow(
      column(8,heatmap_ui("riaz")),
      column(4, correlation_ui("riaz"))),
  )
}


riaz_server <- function() {
  #' riaz Server
  #'
  #' @description Sets up the server logic for the riaz analysis tab
  
  show_modal_progress_line(color = "#FFA812",text = "Initialization")
  Sys.sleep(0.5)
  
  tryCatch({
    update_modal_progress(0.3, text="Downloading data...")
    Sys.sleep(0.5)
    
    riaz <- drive_download("Riaz_Non Responders_vs_Responders_Deseq2.rds", overwrite = TRUE)
    
    update_modal_progress(0.6, text="Loading data...")
    Sys.sleep(0.5)
    
    riaz_dds <- readRDS(riaz$local_path)
    
    if (file.exists(riaz$local_path)) {
      file.remove(riaz$local_path)
    }
    
    dds <- list(riaz_dds)
    update_modal_progress(0.8, text="Loading clinical data...")
    Sys.sleep(0.5)
    
    update_modal_progress(0.9, text="Initializing servers...")
    Sys.sleep(0.5)
    
    observe_helpers()
    
    selection_result <- selection_server(dds, "riaz")
    input_server("riaz", selection_result)
    volcano_server("riaz", selection_result)
    violin_server("riaz", selection_result)
    correlation_server("riaz", selection_result)
    heatmap_server("riaz", selection_result)
    pca_metadata_server("riaz", selection_result)
    update_modal_progress(1, text="Finalizing")
    Sys.sleep(0.5)
    
  }, error = function(e) {
    showNotification("An error occurred during the process.", type = "error")
    print(e)
    
  })
  remove_modal_progress()
}