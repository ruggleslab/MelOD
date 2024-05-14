source("./templates/template_test.R", local = TRUE)
# 
# 
# badal_ui <- function(id) {
#   shared_ui("badal")
# }
# 
# badal_server <- function(id, session) {
# 
#   ns <- session$ns
#   # Load DESeq2 results and CPM values
#   dds <- readRDS(file.path("./data/badal", "Badal_Deseq2.rds"))
#   clinical_data <- read.csv(file = "./data/badal/clinical_data.csv", sep=";")
#   server_shared(dds = dds,clinical_data = clinical_data, "badal")
# }

# 






badal_ui <- function(id) {
  fluidPage(
    add_busy_spinner(spin = "fading-circle", color = "#6699CC"),
    # Your existing UI elements
    fluidRow(
      blurb_explanation_ui("badal")
    ),
    column(6,
           column(12, input_ui("badal")),
           column(12, badal_selector_ui('badal'))),
    column(6,
           blurb_study_ui("badal")),
    pca_metadata_ui("badal"),
    differential_gene_ui("badal"),
    deseq2_table_ui("badal"),
    heatmap_ui("badal")
  )
}

badal_server <- function(id, session) {
  ns <- session$ns
  
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