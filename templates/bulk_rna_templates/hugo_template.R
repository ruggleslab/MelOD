hugo_ui <- function(id) {
  #' hugo UI
  #'
  #' @description Creates the UI layout for the hugo analysis tab
  #' @param id Module ID
  #' 
  #' @return A Shiny UI element for the hugo analysis tab
  
  fluidPage(
    fluidRow(
      blurb_study_ui("hugo")),
    fluidRow(
      column(6, blurb_data_ui("hugo")),
      column(6, blurb_method_ui("hugo"))),
    fluidRow( 
      column(6,blurb_comparison_ui("hugo"))),
    fluidRow(
      column(6,pca_ui("hugo")),
      column(6,metadata_ui("hugo"))),
    fluidRow(
      column(4,input_ui("hugo", 0.05, 2)),
      column(8,deseq2_table_ui("hugo"))),
    fluidRow(
      column(6,volcano_ui("hugo")),
      column(6,violin_ui("hugo"))),
    fluidRow(
      column(8,heatmap_ui("hugo")),
      column(4, correlation_ui("hugo"))),
  )
}


hugo_server <- function() {
  #' hugo Server
  #'
  #' @description Sets up the server logic for the hugo analysis tab
  
  show_modal_progress_line(color = "#FFA812",text = "Initialization")
  Sys.sleep(0.5)
  
  tryCatch({
    update_modal_progress(0.3, text="Downloading data...")
    Sys.sleep(0.5)
    
    hugo <- drive_download("Hugo_Responder_vs_Non_Responder.dds.rds", overwrite = TRUE)
    
    update_modal_progress(0.6, text="Loading data...")
    Sys.sleep(0.5)
    
    hugo_dds <- readRDS(hugo$local_path)
    
    if (file.exists(hugo$local_path)) {
      file.remove(hugo$local_path)
    }
    
    dds <- list(hugo_dds)
    update_modal_progress(0.8, text="Loading clinical data...")
    Sys.sleep(0.5)
    update_modal_progress(0.9, text="Initializing servers...")
    Sys.sleep(0.5)
    
    observe_helpers()
    
    selection_result <- selection_server(dds, "hugo")
    input_server("hugo", selection_result)
    volcano_server("hugo", selection_result)
    violin_server("hugo", selection_result)
    correlation_server("hugo", selection_result)
    heatmap_server("hugo", selection_result)
    pca_metadata_server("hugo", selection_result)
    update_modal_progress(1, text="Finalizing")
    Sys.sleep(0.5)
    
  }, error = function(e) {
    showNotification("An error occurred during the process.", type = "error")
    print(e)
    
  })
  remove_modal_progress()
}