kleffman_ui <- function(id) {
  #' kleffman UI
  #'
  #' @description Creates the UI layout for the kleffman analysis tab
  #' @param id Module ID
  #' 
  #' @return A Shiny UI element for the kleffman analysis tab
  
  fluidPage(
    fluidRow(blurb_study_ui("kleffman")),
    fluidRow(column(6, blurb_data_ui("kleffman")),
             column(6, fluidRow(blurb_comparison_ui("kleffman"),
                                kleffman_selector_ui("kleffman")))),
    fluidRow(
      column(4,input_ui("kleffman", 2, 0.5)),
      column(8,deseq2_table_ui("kleffman"))),
    fluidRow(
      column(6,volcano_ui("kleffman")),
      column(6,violin_ui("kleffman"))),
    fluidRow(
      column(8,heatmap_ui("kleffman")),
      column(4, correlation_ui("kleffman")))
  )
}


kleffman_selector_ui <- function(id) {
  #' kleffman Selector UI
  #' 
  #' @description Creates the UI component for selecting comparisons in the kleffman study
  #' @param id Module ID
  #' 
  #' @return A Shiny UI element
  
  ns <- NS(id)
  box(
    background = 'orange', width = 12,
    radioButtons(
      label = "Selection",
      inputId = ns("selection"),
      choices = c("Paired samples" = 1, "Unpaired samples" = 2 ),
      selected = 2,
      inline = TRUE
    ),
    uiOutput(ns("debug_selection"))
  )
}


kleffman_server <- function() {
  #' kleffman Server
  #'
  #' @description Sets up the server logic for the kleffman analysis tab
  
  show_modal_progress_line(color = "#FFA812",text = "Initialization")
  Sys.sleep(0.5)
  
  tryCatch({
    update_modal_progress(0.3, text="Downloading unpaired data...")
    Sys.sleep(0.5)

    kleffman_unpaired <- drive_download("Kleffman_unpaired.rds", overwrite = TRUE)
    kleffman_unpaired_dds <- readRDS(kleffman_unpaired$local_path)

    update_modal_progress(0.5, text="Downloading paired data...")
    Sys.sleep(0.5)

    kleffman_paired <- drive_download("Kleffman_paired.rds", overwrite = TRUE)
    kleffman_paired_dds <- readRDS(kleffman_paired$local_path)

    
    # List of all downloaded file paths
    downloaded_files <- list(
      kleffman_unpaired$local_path,
      kleffman_paired$local_path
    )
    
    # Remove downloaded files if they exist
    for (file_path in downloaded_files) {
      if (file.exists(file_path)) {
        file.remove(file_path)
      }
    }
    

    update_modal_progress(0.6, text="Loading data...")
    Sys.sleep(0.5)
    
    dds <- list( kleffman_paired_dds,kleffman_unpaired_dds)
    update_modal_progress(0.8, text="Loading clinical data...")
    Sys.sleep(0.5)
    
    update_modal_progress(0.9, text="Initializing servers...")
    Sys.sleep(0.5)    
    
    observe_helpers()
    
    selection_result <- selection_server(dds, "kleffman")
    selection_list_server(dds, "kleffman", selection_result)
    input_server("kleffman", selection_result)
    volcano_server("kleffman", selection_result)
    violin_server("kleffman", selection_result)
    correlation_server("kleffman", selection_result)
    heatmap_server("kleffman", selection_result)

    update_modal_progress(1, text="Finalizing") 
    Sys.sleep(0.5)
    
  }, error = function(e) {
    showNotification("An error occurred during the process.", type = "error")
    print(e)    
  })
  remove_modal_progress() 
}