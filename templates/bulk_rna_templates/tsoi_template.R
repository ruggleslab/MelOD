tsoi_ui <- function(id) {
  #' tsoi UI
  #'
  #' @description Creates the UI layout for the tsoi analysis tab
  #' @param id Module ID
  #' 
  #' @return A Shiny UI element for the tsoi analysis tab
  
  fluidPage(
    fluidRow(
      blurb_study_ui("tsoi")),
    fluidRow(
      column(6, blurb_data_ui("tsoi")),
      column(6, fluidRow(blurb_comparison_ui("tsoi"),
                tsoi_selector_ui("tsoi")))),
    fluidRow(
      column(6,pca_ui("tsoi")),
      column(6,metadata_ui("tsoi"))),
    fluidRow(
      column(4,input_ui("tsoi")),
      column(8,deseq2_table_ui("tsoi"))),
    fluidRow(
      column(6,volcano_ui("tsoi")),
      column(6,violin_ui("tsoi"))),
    fluidRow(
      column(8,heatmap_ui("tsoi")),
      column(4, correlation_ui("tsoi"))),
  )
}


tsoi_selector_ui <- function(id) {
  #' Gide Selector UI
  #' 
  #' @description Creates the UI component for selecting comparisons in the Tsoi study
  #' @param id Module ID
  #' 
  #' @return A Shiny UI element
  
  ns <- NS(id)
  box(
    background = 'orange', width = 12,
    radioButtons(
      label = "Selection",
      inputId = ns("selection"),
      choices = c(
        "Neural crest like vs Undifferentiated" = 1,
        "Transitory vs Undifferentiated" =2,
        "Melanocytic vs Undifferentiated" =3,
        "Neural crest like vs Transitory" =4,
        "Neural crest like vs Melanocytic" =5,
        "Transitory vs Melanocytic" = 6),
      selected = 1,
      inline = TRUE
    ),
    uiOutput(ns("debug_selection"))
  )
}



tsoi_server <- function() {
  #' tsoi Server
  #'
  #' @description Sets up the server logic for the tsoi analysis tab
  
  show_modal_progress_line(color = "#FFA812",text = "Initialization")
  Sys.sleep(0.5)
  
  tryCatch({
    update_modal_progress(0.3, text = "Downloading the 6 different comparison datasets...")
    Sys.sleep(0.5)
    
    # Download and read "Undifferentiated vs Neural crest like" dataset
    undiff_vs_neural <- drive_download("Tsoi_Undifferentiated_vs_Neural crest like_Deseq2.rds", overwrite = TRUE)
    undiff_vs_neural_dds <- readRDS(undiff_vs_neural$local_path)
    
    # Download and read "Undifferentiated vs Transitory" dataset
    undiff_vs_transitory <- drive_download("Tsoi_Undifferentiated_vs_Transitory_Deseq2.rds", overwrite = TRUE)
    undiff_vs_transitory_dds <- readRDS(undiff_vs_transitory$local_path)

    # Download and read "Undifferentiated vs Melanocytic" dataset
    undiff_vs_melanocytic <- drive_download("Tsoi_Undifferentiated_vs_Melanocytic_Deseq2.rds", overwrite = TRUE)
    undiff_vs_melanocytic_dds <- readRDS(undiff_vs_melanocytic$local_path)

    # Download and read "Neural crest like vs Transitory" dataset
    neural_vs_transitory <- drive_download("Tsoi_Neural crest like_vs_Transitory_Deseq2.rds", overwrite = TRUE)
    neural_vs_transitory_dds <- readRDS(neural_vs_transitory$local_path)

    # Download and read "Neural crest like vs Melanocytic" dataset
    neural_vs_melanocytic <- drive_download("Tsoi_Neural crest like_vs_Melanocytic_Deseq2.rds", overwrite = TRUE)
    neural_vs_melanocytic_dds <- readRDS(neural_vs_melanocytic$local_path)

    # Download and read "Transitory vs Melanocytic" dataset
    transitory_vs_melanocytic <- drive_download("Tsoi_Transitory_vs_Melanocytic_Deseq2.rds", overwrite = TRUE)
    transitory_vs_melanocytic_dds <- readRDS(transitory_vs_melanocytic$local_path)
    
    # List of all downloaded file paths
    downloaded_files <- list(
      undiff_vs_neural$local_path,
      undiff_vs_transitory$local_path,
      undiff_vs_melanocytic$local_path,
      neural_vs_transitory$local_path,
      neural_vs_melanocytic$local_path,
      transitory_vs_melanocytic$local_path
    )
    
    # Remove downloaded files if they exist
    for (file_path in downloaded_files) {
      if (file.exists(file_path)) {
        file.remove(file_path)
      }
    }
    
    
    update_modal_progress(0.6, text="Loading data...")
    Sys.sleep(0.5)
    
    dds <- list(undiff_vs_neural_dds, undiff_vs_transitory_dds,undiff_vs_melanocytic_dds,neural_vs_transitory_dds,
                neural_vs_melanocytic_dds,transitory_vs_melanocytic_dds)
    update_modal_progress(0.8, text="Loading clinical data...")
    Sys.sleep(0.5)
    
    clinical_data <- list(read.csv(file = "./data/bulk_rna/badal/clinical_data.csv", sep = ";"),read.csv(file = "./data/bulk_rna/badal/clinical_data.csv", sep = ";"))
    
    
    update_modal_progress(0.9, text="Initializing servers...")
    Sys.sleep(0.5)
    
    observe_helpers()
    
    selection_result <- selection_server(dds, clinical_data, "tsoi")
    selection_list_server(dds, clinical_data, "tsoi", selection_result)
    input_server("tsoi", selection_result)
    volcano_server("tsoi", selection_result)
    violin_server("tsoi", selection_result)
    correlation_server("tsoi", selection_result)
    heatmap_server("tsoi", selection_result)
    pca_metadata_server("tsoi", selection_result)
    
    update_modal_progress(1, text="Finalizing")
    Sys.sleep(0.5)
    
  }, error = function(e) {
    showNotification("An error occurred during the process.", type = "error")
    print(e)
    
  })
  remove_modal_progress()
}