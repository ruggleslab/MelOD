library(DESeq2)
setMethod("rowData", "DESeqResults", function(x) x)
setMethod("sizeFactors", "DESeqResults", function(object) rep(1, 10))
setMethod("normalizationFactors", "DESeqResults", function(object) NULL)
setMethod("assay", "DESeqResults", function(x, i) {
  if(i == "counts") {
    mat <- matrix(1, nrow(x), 10)
    rownames(mat) <- rownames(x)
    colnames(mat) <- paste0("sample", 1:10)
    mat
  } else NULL
})

ibrahim_ui <- function(id) {
  #' Ibrahim UI
  #'
  #' @description Creates the UI layout for the Ibrahim analysis tab
  #' @param id Module ID
  #' 
  #' @return A Shiny UI element for the Ibrahim analysis tab
  
  fluidPage(
    fluidRow(
      blurb_study_ui("ibrahim")),
    fluidRow(
      column(6, blurb_data_ui("ibrahim")),
      column(6, blurb_method_ui("ibrahim"))),
    fluidRow( 
      column(6,blurb_comparison_ui("ibrahim"))),
    fluidRow(
      column(6,pca_ui("ibrahim")),
      column(6,metadata_ui("ibrahim"))),
    fluidRow(
      column(4,input_ui("ibrahim", 0.05, 2)),
      column(8,deseq2_table_ui("ibrahim"))),
    fluidRow(
      column(6,volcano_ui("ibrahim")),
      column(6,violin_ui("ibrahim"))),
    fluidRow(
      column(8,heatmap_ui("ibrahim")),
      column(4, correlation_ui("ibrahim"))),
  )
}


ibrahim_server <- function() {
  #' Ibrahim Server
  #'
  #' @description Sets up the server logic for the Ibrahim analysis tab
  
  show_modal_progress_line(color = "#FFA812",text = "Initialization")
  Sys.sleep(0.5)
  
  tryCatch({
    update_modal_progress(0.3, text="Downloading data...")
    Sys.sleep(0.5)
    
    ibrahim <- drive_download("Ibrahim_NF1_Mutant_vs_Wild_Type.dds.rds", overwrite = TRUE)
    
    update_modal_progress(0.6, text="Loading data...")
    Sys.sleep(0.5)
    
    ibrahim_dds <- readRDS(ibrahim$local_path)
    
    if (file.exists(ibrahim$local_path)) {
      file.remove(ibrahim$local_path)
    }
    
    dds <- list(ibrahim_dds)
    update_modal_progress(0.8, text="Loading clinical data...")
    Sys.sleep(0.5)
    update_modal_progress(0.9, text="Initializing servers...")
    Sys.sleep(0.5)
    
    observe_helpers()
    
    selection_result <- selection_server(dds, "ibrahim")
    input_server("ibrahim", selection_result)
    volcano_server("ibrahim", selection_result)
    violin_server("ibrahim", selection_result)
    correlation_server("ibrahim", selection_result)
    heatmap_server("ibrahim", selection_result)
    pca_metadata_server("ibrahim", selection_result)
    update_modal_progress(1, text="Finalizing")
    Sys.sleep(0.5)
    
  }, error = function(e) {
    showNotification("An error occurred during the process.", type = "error")
    print(e)
    
  })
  remove_modal_progress()
}