source("./templates/template_test.R", local = TRUE)


# 
# fischer_ui <- function(id) {
#   shared_ui("fischer")
# }
# 
# fischer_server <- function(id, session) {
# 
#   ns <- session$ns
#   # Load DESeq2 results and CPM values
# 
#   server_shared(dds = dds,clinical_data = clinical_data, "fischer")
# }
# 




fischer_ui <- function(id) {

  fluidPage(
    add_busy_spinner(spin = "fading-circle", color = "#6699CC"),
    
    fluidRow(
      blurb_explanation_ui("fischer")),
    fluidRow(
      column(5, input_ui("fischer")),
      column(7,  blurb_study_ui("fischer"))),
    pca_metadata_ui("fischer"),
    differential_gene_ui("fischer"),
    deseq2_table_ui("fischer"),
    heatmap_ui("fischer"),

  )
}

fischer_server <- function(id, session) {

  ns <- session$ns
  
  dds <- list(readRDS(file.path("./data/fischer", "Fischer_Deseq2.rds")))
  clinical_data <- list(read.csv("./data/fischer/Fischer_demographics_information_Final.csv"))
  selection_server(dds,clinical_data,"fischer")
  input_server(dds = dds,clinical_data = clinical_data, "fischer")
  pca_metadata_server(dds = dds,clinical_data = clinical_data, "fischer")
  differential_gene_server(dds = dds,clinical_data = clinical_data, "fischer")
  heatmap_server(dds = dds,clinical_data = clinical_data, "fischer")


}

