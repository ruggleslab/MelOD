kunz_ui <- function(id) {
#' Kunz UI
#'
#' @description Creates the UI layout for the Kunz analysis tab
#' @param id Module ID
#' 
#' @return A Shiny UI element for the Kunz analysis tab
  
  fluidPage(
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
      column(4, correlation_ui("kunz"))),
   )
}


kunz_server <- function() {
#' Kunz Server
#'
#' @description Sets up the server logic for the Kunz analysis tab
  
  show_modal_progress_line(color = "#FFA812",text = "Initialization")
  Sys.sleep(0.5)
  
  tryCatch({
    update_modal_progress(0.3, text="Downloading data...")
    Sys.sleep(0.5)
    
    kunz <- drive_download("Kunz_Deseq2.rds", overwrite = TRUE)
    
    update_modal_progress(0.6, text="Loading data...")
    Sys.sleep(0.5)
    
    kunz_dds <- readRDS(kunz$local_path)
    
    if (file.exists(kunz$local_path)) {
      file.remove(kunz$local_path)
    }
    
    dds <- list(kunz_dds)
    update_modal_progress(0.8, text="Loading clinical data...")
    Sys.sleep(0.5)
    
    clinical_data <- list(read.csv(file = "./data/bulk_rna/badal/clinical_data.csv", sep = ";"))
    update_modal_progress(0.9, text="Initializing servers...")
    Sys.sleep(0.5)

    selection_result <- selection_server(dds, clinical_data, "kunz")
    input_server("kunz", selection_result)
    volcano_server("kunz", selection_result)
    violin_server("kunz", selection_result)
    correlation_server("kunz", selection_result)
    heatmap_server("kunz", selection_result)
    pca_metadata_server("kunz", selection_result)
    
    update_modal_progress(1, text="Finalizing")
    Sys.sleep(0.5)
    
  }, error = function(e) {
    showNotification("An error occurred during the process.", type = "error")
    print(e)
    
  })
  remove_modal_progress()
}