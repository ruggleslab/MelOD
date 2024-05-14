source("./templates/template_test.R", local = TRUE)



# 
# kunz_ui <- function(id) {
#   shared_ui("kunz")
# }
# 
# kunz_server <- function(id, session) {
# 
#   ns <- session$ns
#   # Load DESeq2 results and CPM values
# 
#   dds <- readRDS(file.path("./data/kunz", "Kunz_Deseq2.rds"))
#   clinical_data <- read.csv(file = "./data/badal/clinical_data.csv", sep=";")
#   server_shared(dds = dds,clinical_data = clinical_data, "kunz")
# }
# 
# 
# #


kunz_ui <- function(id) {

  fluidPage(
    add_busy_spinner(spin = "fading-circle", color = "#6699CC"),
    
    fluidRow(
    blurb_explanation_ui("kunz")),
    fluidRow(
      column(5, input_ui("kunz")),
      column(7,  blurb_study_ui("kunz"))),
    pca_metadata_ui("kunz"),
    differential_gene_ui("kunz"),
    deseq2_table_ui("kunz"),
    heatmap_ui("kunz")


  )
}

kunz_server <- function(id, session) {

  ns <- session$ns
  dds <- list(readRDS(file.path("./data/kunz", "Kunz_Deseq2.rds")))
  clinical_data <- list(read.csv(file = "./data/badal/clinical_data.csv", sep=";"))
  selection_server(dds,clinical_data,"kunz")
  input_server(dds = dds,clinical_data = clinical_data, "kunz")
  pca_metadata_server(dds = dds,clinical_data = clinical_data, "kunz")
  differential_gene_server(dds = dds,clinical_data = clinical_data, "kunz")
  heatmap_server(dds = dds,clinical_data = clinical_data, "kunz")

}


