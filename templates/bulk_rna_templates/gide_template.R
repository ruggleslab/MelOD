gide_ui <- function(id) {
#' Gide UI
#'
#' @description Creates the UI layout for the Gide analysis tab
#' @param id Module ID
#' 
#' @return A Shiny UI element for the Gide analysis tab
  
  fluidPage(
    fluidRow(blurb_study_ui("gide")),
    fluidRow(
      column(6,blurb_data_ui("gide")),
      column(6,blurb_method_ui("gide"))),
    fluidRow( 
      column(6,blurb_comparison_ui("gide")) ,
      column(6,gide_selector_ui("gide"))),
    fluidRow(
      column(6,pca_ui("gide")),
      column(6,metadata_ui("gide"))),
    fluidRow(
      column(4,input_ui("gide", 0.05, 2)),
      column(8,deseq2_table_ui("gide"))),
    fluidRow(
      column(6,volcano_ui("gide")),
      column(6,violin_ui("gide"))),
    fluidRow(
      column(8,heatmap_ui("gide")),
      column(4, correlation_ui("gide")))
  )
}


gide_selector_ui <- function(id) {
#' Gide Selector UI
#' 
#' @description Creates the UI component for selecting comparisons in the Gide study
#' @param id Module ID
#' 
#' @return A Shiny UI element
  
  ns <- NS(id)
  box(
    background = 'orange', width = 12,
    radioButtons(
      label = "Selection",
      inputId = ns("selection"),
      choices = c("Combotherapy" = 1, "Monotherapy" =2),
      selected = 1,
      inline = TRUE
    ),
    uiOutput(ns("debug_selection"))
  )
}


gide_server <- function() {
#' Gide Server
#'
#' @description Sets up the server logic for the Gide analysis tab
  
  show_modal_progress_line(color = "#FFA812",text = "Initialization")
  Sys.sleep(0.5)
  
  tryCatch({
    update_modal_progress(0.3, text="Downloading combotherapy data...")
    Sys.sleep(0.5)
    
    gide_combo <- drive_download("Gide_Mono_Non responder_vs_Responder_Deseq2.rds", overwrite = TRUE)
    gide_combo_dds <- readRDS(gide_combo$local_path)
    
    update_modal_progress(0.5, text="Downloading monotherapy data...")
    Sys.sleep(0.5)
    
    gide_mono <- drive_download("Gide_Combo_Non responder_vs_Responder_Deseq2.rds", overwrite = TRUE)
    gide_mono_dds <- readRDS(gide_mono$local_path)
    
   
    
    update_modal_progress(0.6, text="Loading data...")
    Sys.sleep(0.5)
    
    dds <- list(gide_combo_dds, gide_mono_dds)
    update_modal_progress(0.8, text="Loading clinical data...")
    Sys.sleep(0.5)

    
    gide_mono_metadata <- drive_download("Gide_demographics_combotherapy.csv", overwrite = TRUE)
    gide_combo_metadata <- drive_download("Gide_demographics_monotherapy.csv", overwrite = TRUE)
    
    clinical_data <- list(
    read.csv(gide_combo_metadata$local_path, sep = ','),
    read.csv(gide_mono_metadata$local_path, sep = ',')
    )
    # List of all downloaded file paths
    downloaded_files <- list(
      gide_combo_metadata$local_path,
      gide_combo_metadata$local_path,
      gide_combo$local_path,
      gide_mono$local_path
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
    
    selection_result <- selection_server(dds, "gide", clinical_data)
    selection_list_server(dds, "gide", selection_result, clinical_data)
    input_server("gide", selection_result)
    volcano_server("gide", selection_result)
    violin_server("gide", selection_result)
    correlation_server("gide", selection_result)
    heatmap_server("gide", selection_result)
    pca_metadata_server("gide", selection_result)
    
    update_modal_progress(1, text="Finalizing") 
    Sys.sleep(0.5)
    
  }, error = function(e) {
    showNotification("An error occurred during the process.", type = "error")
    print(e)    
  })
  remove_modal_progress() 
}