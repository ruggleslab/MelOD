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
      column(4,input_ui("kleffman")),
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
      choices = c("Paired" = 1, "Unpaired" = 2 ),
      selected = 1,
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
    # update_modal_progress(0.3, text="Downloading unpaired data...")
    # Sys.sleep(0.5)
    # 
    # kleffman_unpaired <- drive_download("kleffman_unpaired.rds", overwrite = TRUE)
    # kleffman_unpaired_dds <- readRDS(kleffman_unpaired$local_path)
    # 
    # update_modal_progress(0.5, text="Downloading paired data...")
    # Sys.sleep(0.5)
    # 
    # kleffman_paired <- drive_download("kleffman_paired.rds", overwrite = TRUE)
    # kleffman_paired_dds <- readRDS(kleffman_paired$local_path)
    # 
    # if (file.exists(kleffman_unpaired$local_path)) {
    #   file.remove(kleffman_unpaired$local_path)
    # }
    # 
    # if (file.exists(kleffman_paired$local_path)) {
    #   file.remove(kleffman_paired$local_path)
    # }
    # 
    # update_modal_progress(0.6, text="Loading data...")
    # Sys.sleep(0.5)


    kleffman_unpaired_dds <- readRDS("/Users/paul/Documents/pro/proteomics/kleffman/kleffman_unpaired/proteomics_unpaired.rds")
    kleffman_paired_dds <- readRDS("/Users/paul/Documents/pro/proteomics/kleffman/kleffman_paired/kleffman_paired.rds")
    
    
    dds <- list( kleffman_paired_dds,kleffman_unpaired_dds)
    update_modal_progress(0.8, text="Loading clinical data...")
    Sys.sleep(0.5)
    
    clinical_data <- list(read.csv(file = "./data/bulk_rna/badal/clinical_data.csv", sep = ";"),read.csv(file = "./data/bulk_rna/badal/clinical_data.csv", sep = ";"))
    
    update_modal_progress(0.9, text="Initializing servers...")
    Sys.sleep(0.5)    
    
    observe_helpers()
    
    selection_result <- selection_server(dds, clinical_data, "kleffman")
    selection_list_server(dds, clinical_data, "kleffman", selection_result)
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