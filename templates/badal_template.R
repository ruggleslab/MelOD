#' Badal UI
#'
#' @description Creates the UI layout for the Badal analysis tab
#' @param id Module ID
#' @return A Shiny UI element for the Badal analysis tab
badal_ui <- function(id) {
  fluidPage(
    add_busy_spinner(spin = "fading-circle", color = "#FFA812"),
    
    
    fluidRow(blurb_study_ui("badal")),
    
    fluidRow(
      column(5, pca_ui("badal")),
      column(7, fluidRow(blurb_data_ui("badal")), fluidRow(metadata_ui("badal")))),
    # fluidRow(
    #   column(6,input_ui("badal")),
    #   column(6,volcano_ui("badal"))),
    # fluidRow(
    #   column(6,violin_ui("badal")),
    #   column(6,deseq2_table_ui("badal"))),
    # heatmap_ui("badal")
    # 
    # 
    # 
    # 
    # 
    # 
    # 
    # 
    # 
    # 
    # 
    # 
    # fluidRow(
    #   blurb_explanation_ui("badal")
    # ),
    # column(6,
    #        column(12, input_ui("badal")),
    #        column(12, badal_selector_ui('badal'))),
    # column(6,
    #        blurb_study_ui("badal")),
    # pca_metadata_ui("badal"),
    # differential_gene_ui("badal"),
    deseq2_table_ui("badal"),
    heatmap_ui("badal")
  )
}

#' Badal Selector UI
#' 
#' @description Creates the UI component for selecting comparisons in the Badal study
#' @param id Module ID
#' @return A Shiny UI element
badal_selector_ui <- function(id) {
  ns <- NS(id)
  box(
    background = 'orange', width = 12,
    radioButtons(
      label = "Select comparison",
      inputId = ns("selection_badal"),
      choices = c("Gene", "Tumor Stage"),
      selected = "Gene",
      inline = TRUE
    ),
    uiOutput(ns("debug_selection"))
  )
}


#' Badal Server
#'
#' @description Sets up the server logic for the Badal analysis tab
badal_server <- function() {

  ## Show a modal dialog indicating that loading is in progress
  showModal(modalDialog(
    title = "Please Wait",
    "Downloading and loading data...",
    easyClose = FALSE,
    footer = NULL
  ))
  
  ## Download the file in current wd and save the object
  ## Wrap in tryCatch to handle errors
  tryCatch({
    badal <- drive_download("Badal_Deseq2.rds", overwrite = TRUE)
    badal_dds <- readRDS(badal$local_path)
    
    ## Delete the file once it's loaded to save on storage space
    if (file.exists(badal$local_path)) {
      file.remove(badal$local_path)
    }
    
    dds <- list(badal_dds)
    clinical_data <- list(read.csv(file = "./data/badal/clinical_data.csv"))
    
    ## Load other server modules
    selection_server(dds, clinical_data, "badal")
    input_server(dds = dds, clinical_data = clinical_data, "badal")
    pca_metadata_server(dds = dds, clinical_data = clinical_data, "badal")
    differential_gene_server(dds = dds, clinical_data = clinical_data, "badal")
    heatmap_server(dds = dds, clinical_data = clinical_data, "badal")
    
    ## Close the modal after the loading is complete
    removeModal()
  }, error = function(e) {
    ## Error handling
    removeModal()
    showModal(modalDialog(
      title = "Error",
      paste("An error occurred:", e$message),
      easyClose = TRUE
    ))
  })
}