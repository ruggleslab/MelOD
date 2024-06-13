#' Badal UI
#'
#' @description Creates the UI layout for the Badal analysis tab
#' @param id Module ID
#' @return A Shiny UI element for the Badal analysis tab
badal_ui <- function(id) {
  fluidPage(
    add_busy_spinner(spin = "fading-circle", color = "#FFA812"),
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



#' Badal Server
#'
#' @description Sets up the server logic for the Badal analysis tab
badal_server <- function() {
  # Initialize the progress bar
  progress <- shiny::Progress$new()
  on.exit(progress$close())
  progress$set(message = "Initializing", value = 0)
  
  ## Download the file in current wd and save the object
  ## Wrap in tryCatch to handle errors
  tryCatch({
    progress$set(message = "Downloading data...", value = 0.3)
    badal <- drive_download("Badal_Deseq2.rds", overwrite = TRUE)
    badal_dds <- readRDS(badal$local_path)
    
    ## Delete the file once it's loaded to save on storage space
    if (file.exists(badal$local_path)) {
      file.remove(badal$local_path)
    }
    
    dds <- list(badal_dds)
    
    progress$set(message = "Loading clinical data...", value = 0.6)
    clinical_data <- list(read.csv(file = "./data/bulk_rna/badal/clinical_data.csv"))
    
    progress$set(message = "Initializing servers...", value = 0.8)
    
    # Initialize servers
    selection_result <- selection_server(dds, clinical_data, "badal")
    input_server("badal", selection_result)
    volcano_server("badal", selection_result)
    violin_server("badal", selection_result)
    correlation_server("badal", selection_result)
    heatmap_server("badal", selection_result)
    pca_metadata_server("badal", selection_result)
    
    # Finalize progress
    progress$set(message = "Finalizing", value = 1)
  }, error = function(e) {
    ## Error handling
    progress$close()
    showModal(modalDialog(
      title = "Error",
      paste("An error occurred:", e$message),
      easyClose = TRUE
    ))
  })
}


