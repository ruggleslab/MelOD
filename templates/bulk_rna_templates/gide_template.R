#' Gide UI
#'
#' @description Creates the UI layout for the Gide analysis tab
#' @param id Module ID
#' @return A Shiny UI element for the Gide analysis tab
gide_ui <- function(id) {
  
  fluidPage(
    fluidRow(blurb_study_ui("gide")),
    fluidRow(column(6, blurb_data_ui("gide")),
             column(6, fluidRow(blurb_comparison_ui("gide"),
                                gide_selector_ui("gide")))),
    fluidRow(
      column(6,pca_ui("gide")),
      column(6,metadata_ui("gide"))),
    fluidRow(
      column(4,input_ui("gide")),
      column(8,deseq2_table_ui("gide"))),
    fluidRow(
      column(6,volcano_ui("gide")),
      column(6,violin_ui("gide"))),
    fluidRow(
      column(8,heatmap_ui("gide")),
      column(4, correlation_ui("gide")))
  )
}



#' Gide Selector UI
#' 
#' @description Creates the UI component for selecting comparisons in the Gide study
#' @param id Module ID
#' @return A Shiny UI element
gide_selector_ui <- function(id) {
  ns <- NS(id)
  box(
    background = 'orange', width = 12,
    radioButtons(
      label = "Selection",
      inputId = ns("selection"),
      choices = c("Combotherapy", "Monotherapy"),
      selected = "Combotherapy",
      inline = TRUE
    ),
    uiOutput(ns("debug_selection"))
  )
}



#' Gide Server
#'
#' @description Sets up the server logic for the Gide analysis tab
gide_server <- function() {
  
  show_modal_progress_line(color = "#FFA812",text = "Initialization")
  Sys.sleep(0.5)
  
  tryCatch({
    
    update_modal_progress(0.3, text="Downloading combotherapy data...")
    Sys.sleep(0.5)
    
    gide_combo <- drive_download("Gide_PreCombo_Deseq2.rds", overwrite = TRUE)
    gide_combo_dds <- readRDS(gide_combo$local_path)
    
    update_modal_progress(0.5, text="Downloading monotherapy data...")
    Sys.sleep(0.5)
    
    gide_mono <- drive_download("Gide_PreMono_Deseq2.rds", overwrite = TRUE)
    gide_mono_dds <- readRDS(gide_mono$local_path)
    
    if (file.exists(gide_combo$local_path)) {
      file.remove(gide_combo$local_path)
    }
    
    if (file.exists(gide_mono$local_path)) {
      file.remove(gide_mono$local_path)
    }
    
    update_modal_progress(0.6, text="Loading data...")
    Sys.sleep(0.5)
    
    dds <- list(gide_combo_dds, gide_mono_dds)
    
    update_modal_progress(0.8, text="Loading clinical data...")
    Sys.sleep(0.5)

    clinical_data <- list(
    read.csv(file.path("./data/bulk_rna/gide/combo", "Gide_demographics_combotherapy.csv"), sep = ','),
    read.csv(file.path("./data/bulk_rna/gide/mono", "Gide_demographics_monotherapy.csv"), sep = ',')
    )
    
    update_modal_progress(0.9, text="Initializing servers...")
    Sys.sleep(0.5)    
    
    selection_result <- selection_server(dds, clinical_data, "gide")
    selection_list_server(dds, clinical_data, "gide", selection_result)
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