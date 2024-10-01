badal_ui <- function(id) {
#' Badal UI
#'
#' @description Creates the UI layout for the Badal analysis tab
#' @param id Module ID
#' 
#' @return A Shiny UI element for the Badal analysis tab
  
  fluidPage(
    fluidRow(blurb_study_ui("badal")),
    fluidRow(column(6, blurb_data_ui("badal")),
             column(6, fluidRow(blurb_comparison_ui("badal")))),
    fluidRow(
      column(6,pca_ui("badal")),
      column(6,metadata_ui("badal"))),
    fluidRow(
      column(4,input_ui("badal")),
      column(8,deseq2_table_ui("badal"))),
    fluidRow(
      column(6,volcano_ui("badal")),
      column(6,violin_ui("badal"))),
    fluidRow(
      column(8,heatmap_ui("badal")),
      column(4, correlation_ui("badal")))
  )
}


#' #' Badal Selector UI
#' #' 
#' #' @description Creates the UI component for selecting comparisons in the Badal study
#' #' @param id Module ID
#' #' @return A Shiny UI element
#' badal_selector_ui <- function(id) {
#'   ns <- NS(id)
#'   box(
#'     background = 'orange', width = 12,
#'     radioButtons(
#'       label = "Select comparison",
#'       inputId = ns("selection_badal"),
#'       choices = c("Gene", "Tumor Stage"),
#'       selected = "Gene",
#'       inline = TRUE
#'     ),
#'     uiOutput(ns("debug_selection"))
#'   )
#' }


badal_server <- function() {
#' Badal Server
#'
#' @description Sets up the server logic for the Badal analysis tab
  
  show_modal_progress_line(color = "#FFA812",text = "Initialization")
  Sys.sleep(0.5)
  tryCatch({
    update_modal_progress(0.3, text="Downloading data...")
    Sys.sleep(0.5)
    
    badal <- drive_download("Badal_common acquired nevus_vs_primary melanoma_Deseq2.rds", overwrite = TRUE)
    badal_dds <- readRDS(badal$local_path)
    
    update_modal_progress(0.6, text="Loading data...")
    Sys.sleep(0.5)
    
    if (file.exists(badal$local_path)) {
      file.remove(badal$local_path)
    }
    
    dds <- list(badal_dds)
    
    update_modal_progress(0.8, text="Loading clinical data...")
    Sys.sleep(0.5)
    
    clinical_data <- list(read.csv(file = "./data/bulk_rna/badal/clinical_data.csv"))
    update_modal_progress(0.9, text="Initializing servers...")
    Sys.sleep(0.5)
    
    observe_helpers()
    
    selection_result <- selection_server(dds, clinical_data, "badal")
    input_server("badal", selection_result)
    volcano_server("badal", selection_result)
    violin_server("badal", selection_result)
    correlation_server("badal", selection_result)
    heatmap_server("badal", selection_result)
    pca_metadata_server("badal", selection_result)
    
    update_modal_progress(1, text="Finalizing")
    Sys.sleep(0.5)
    
  }, error = function(e) {
    showNotification("An error occurred during the process.", type = "error")
    print(e)    
    
  })
  remove_modal_progress()
}