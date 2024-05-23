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
             column(6, fluidRow(blurb_comparison_ui("badal"),
                                gide_selector_ui("badal")))),
    fluidRow(
      column(6,pca_ui("badal")),
      column(6,metadata_ui("badal"))),
    fluidRow(
      column(6,input_ui("badal")),
      column(6,volcano_ui("badal"))),
    fluidRow(
      column(6,deseq2_table_ui("badal")),
      column(6,violin_ui("badal"))),
    fluidRow(
      column(8,heatmap_ui("badal")),
      column(4, correlation_ui("badal")))
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
    input_server("badal")
    pca_metadata_server("badal")
    differential_gene_server("badal")
    heatmap_server("badal")
    correlation_server("badal")  
    
    
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





# 
# 
# 
# rds <-  readRDS(file.path("/Users/paul/Downloads", "Badal_Deseq2.rds"))
# 
# 
# metadata_csv_from_rds <- as.data.frame(colData(rds))
# badal <- read.csv(file = "./data/badal/clinical_data.csv")
# 
# 
# 
# 
# name = badal
# 
# 
# 
# 
# 
# name <- name %>%
#   mutate(numeric_patient_id = as.numeric(str_extract(Patient.ID, "\\d+")))
# 
# metadata_csv_from_rds <- metadata_csv_from_rds %>%
#   mutate(numeric_id_1 = as.numeric(str_extract(Sample, "\\d+")))
# 
# metadata_csv_from_rds$ID <- rownames(metadata_csv_from_rds)
# 
# merged_df <- merge(name, metadata_csv_from_rds, by.x = "numeric_patient_id", by.y = "numeric_id_1", all.x = TRUE)
# merged_df <- merged_df %>%
#   distinct(numeric_patient_id, .keep_all = TRUE)
# 
# 
# 
# # List of columns to drop
# columns_to_drop <- c("X", "Sample","RNA.seq.ID")  # Replace with the actual column names you want to drop
# 
# # Drop the specified columns
# merged_df <- merged_df %>%
#   select(-all_of(columns_to_drop))
# 
# # Remove rows with NA values
# merged_df <- na.omit(merged_df)
# # Ensure no row names exist
# rownames(merged_df) <- NULL
# merged_df <- column_to_rownames(merged_df, var = "ID")
# 
# # Rename the column (for example, renaming "old_name" to "new_name")
# merged_df <- merged_df %>%
#   rename("OS(days)" = "Overall.Survival..Days")
# 
# 
# # Optionally, save the modified dataframe back to a CSV file
# write.csv(merged_df, file.path("./data/fischer/Fischer_demographics_information_Final.csv"), row.names = TRUE)
# 
# 
# 
