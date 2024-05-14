source("./templates/template_test.R", local = TRUE)



gide_ui <- function(id) {
  
  fluidPage(
    add_busy_spinner(spin = "fading-circle", color = "#FFA812"),
    
    fluidRow(
      blurb_explanation_ui("gide")),
      column(6,
      column(12,
      input_ui("gide")),
      column(12,gide_selector_ui('gide'))),
    column(6,
      blurb_study_ui("gide")),
    
    pca_metadata_ui('gide'),
    differential_gene_ui('gide'),
    deseq2_table_ui('gide'),
    heatmap_ui('gide')
    
  
  )
}

gide_server <- function(id, session) {
  
  ns <- session$ns
  dds <- list(readRDS(file.path("./data/gide/mono", "ddsPreMono.rds")), readRDS(file.path("./data/gide/combo", "ddsPreCombo.rds")))
  clinical_data <- list(read.csv(file.path("./data/gide/mono", "Gide_demographics_monotherapy.csv"), sep=','), read.csv(file.path("./data/gide/combo", "Gide_demographics_combotherapy.csv"), sep=','))
  selection_server(dds,clinical_data,"gide")
  selection_list_server(dds,clinical_data,"gide")
  input_server(dds = dds,clinical_data = clinical_data, "gide")
  pca_metadata_server(dds = dds,clinical_data = clinical_data, "gide")
  differential_gene_server(dds = dds,clinical_data = clinical_data, "gide")
  heatmap_server(dds = dds,clinical_data = clinical_data, "gide")
  
  
  }


