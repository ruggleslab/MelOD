MM500_ui <- function(id) {
  #' MM500 UI
  #'
  #' @description Creates the UI layout for the MM500 analysis tab
  #' @param id Module ID
  #' 
  #' @return A Shiny UI element for the MM500 analysis tab
  
  fluidPage(
    fluidRow(blurb_study_ui("MM500")),
    fluidRow(
      column(6,blurb_data_ui("MM500")),
      column(6,blurb_method_ui("MM500"))),
    fluidRow( 
      column(6,blurb_comparison_ui("MM500")) ,
      column(6,MM500_selector_ui("MM500"))),
    fluidRow(metadata_ui("MM500")),
    fluidRow(
      column(4,input_ui("MM500", 0.5, 0.5)),
      column(8,deseq2_table_ui("MM500"))),
    fluidRow(
      column(6,volcano_ui("MM500")),
      column(6,violin_ui("MM500"))),
    fluidRow(
      column(8,heatmap_ui("MM500")),
      column(4, correlation_ui("MM500")))
  )
}


MM500_selector_ui <- function(id) {
  #' MM500 Selector UI
  #' 
  #' @description Creates the UI component for selecting comparisons in the MM500 study
  #' @param id Module ID
  #' 
  #' @return A Shiny UI element
  
  ns <- NS(id)
  box(
    background = 'orange', width = 12,
    radioButtons(
      label = "Selection",
      inputId = ns("selection"),
      choices = c("Segundo" = 1, "Cuarto" = 3, "Cero"= 2),
      selected = 2,
      inline = TRUE
    ),
    uiOutput(ns("debug_selection"))
  )
}


MM500_server <- function() {
  #' MM500 Server
  #'
  #' @description Sets up the server logic for the MM500 analysis tab
  
  show_modal_progress_line(color = "#FFA812",text = "Initialization")
  Sys.sleep(0.5)
  
  tryCatch({
    update_modal_progress(0.3, text="Downloading segundo data...")
    Sys.sleep(0.5)
    
    MM500_segundo <- drive_download("segundo_ttest_summarized_experiment.rds", overwrite = TRUE)
    MM500_segundo_dds <- readRDS(MM500_segundo$local_path)
    
    update_modal_progress(0.5, text="Downloading cuarto data...")
    Sys.sleep(0.5)
    
    MM500_cuarto <- drive_download("cuarto_ttest_summarized_experiment.rds", overwrite = TRUE)
    MM500_cuarto_dds <- readRDS(MM500_cuarto$local_path)
    
    update_modal_progress(0.6, text="Downloading cero data...")
    Sys.sleep(0.5)
    
    MM500_cero <- drive_download("cero_ttest_summarized_experiment.rds", overwrite = TRUE)
    MM500_cero_dds <- readRDS(MM500_cero$local_path)
    
    update_modal_progress(0.8, text="Loading clinical data...")
    Sys.sleep(0.5)
    
    
    MM500_segundo_metadata <- drive_download("segundo_clinical_data.csv", overwrite = TRUE)
    MM500_cero_metadata <- drive_download("cero_clinical_data.csv", overwrite = TRUE)
    
    clinical_data <- list(
      read.csv(MM500_segundo_metadata$local_path, sep = ','),
      read.csv(MM500_cero_metadata$local_path, sep = ',')
    )
  
    # List of all downloaded file paths
    downloaded_files <- list(
      MM500_segundo$local_path,
      MM500_cuarto$local_path,
      MM500_cero$local_path,
      MM500_segundo_metadata$local_path,
      MM500_cero_metadata$local_path
    )
    
    # Remove downloaded files if they exist
    for (file_path in downloaded_files) {
      if (file.exists(file_path)) {
        file.remove(file_path)
      }
    }
    

    
    dds <- list(MM500_segundo_dds,MM500_cero_dds,MM500_cuarto_dds)
 
    update_modal_progress(0.9, text="Initializing servers...")
    Sys.sleep(0.5)    
    
    observe_helpers()
    
    selection_result <- selection_server(dds, "MM500",clinical_data)
    selection_list_server(dds, "MM500", selection_result, clinical_data)
    input_server("MM500", selection_result)
    volcano_server("MM500", selection_result)
    violin_server("MM500", selection_result)
    correlation_server("MM500", selection_result)
    heatmap_server("MM500", selection_result)
    pca_metadata_server("MM500", selection_result)
    
    
    update_modal_progress(1, text="Finalizing") 
    Sys.sleep(0.5)
    
  }, error = function(e) {
    showNotification("An error occurred during the process.", type = "error")
    print(e)    
  })
  remove_modal_progress() 
}