#' Fischer UI
#'
#' @description Creates the UI layout for the Fischer analysis tab
#' @param id Module ID
#' @return A Shiny UI element for the Fischer analysis tab
fischer_ui <- function(id) {
    fluidPage(
      add_busy_spinner(spin = "fading-circle", color = "#FFA812"),
      fluidRow(
        blurb_study_ui("fischer")),
      fluidRow(
        column(6, blurb_data_ui("fischer")),
        column(6, blurb_comparison_ui("fischer"))),
      fluidRow(
        column(6,pca_ui("fischer")),
        column(6,metadata_ui("fischer"))),
      fluidRow(
        column(6,input_ui("fischer")),
        column(6,volcano_ui("fischer"))),
      fluidRow(
        column(6,deseq2_table_ui("fischer")),
        column(6,violin_ui("fischer"))),
      fluidRow(
        column(8,heatmap_ui("fischer")),
        column(4, correlation_ui("fischer")))
    )
  }


#' Fischer Server
#'
#' @description Sets up the server logic for the Fischer analysis tab
fischer_server <- function() {
  
  dds <- list(readRDS(file.path("./data/fischer", "Fischer_Deseq2.rds")))
  clinical_data <- list(read.csv("./data/fischer/Fischer_demographics_information_Final.csv"))
  
  selection_server(dds,clinical_data,"fischer")
  input_server("fischer")
  pca_metadata_server("fischer")

  differential_gene_server("fischer")

  heatmap_server("fischer")
  correlation_server("fischer")  
  
}


# 
# rds <-  readRDS(file.path("./data/fischer", "Fischer_Deseq2.rds"))
# 
# 
# metadata_csv_from_rds <- as.data.frame(colData(rds))
# fisher <- read.csv("./data/fischer/Fischer_demographics_information_Final.csv")
# 
# 
# 
# 
# name = fisher
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
