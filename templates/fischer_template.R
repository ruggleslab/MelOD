#' Fischer UI
#'
#' @description Creates the UI layout for the Fischer analysis tab
#' @param id Module ID
#' @return A Shiny UI element for the Fischer analysis tab
fischer_ui <- function(id) {

  fluidPage(
    add_busy_spinner(spin = "fading-circle", color = "#FFA812"),
    
    
    
    
    fluidRow(blurb_study_ui("fischer")),
    
    fluidRow(
      column(5, pca_ui("fischer")),
      column(7, fluidRow(blurb_data_ui("fischer")), fluidRow(metadata_ui("fischer")))),
    # fluidRow(
    #   column(6,input_ui("fischer")),
    #   column(6,volcano_ui("fischer"))),
    # fluidRow(
    #   column(6,violin_ui("fischer")),
    #   column(6,deseq2_table_ui("fischer"))),
    # heatmap_ui("fischer")
    # 
    
    
    
    
    # 
    # 
    # fluidRow(
    #   blurb_explanation_ui("fischer")),
    # fluidRow(
    #   column(5, input_ui("fischer")),
    #   column(7,  blurb_study_ui("fischer"))),
    # pca_metadata_ui("fischer"),
    # differential_gene_ui("fischer"),
    deseq2_table_ui("fischer"),
    heatmap_ui("fischer"),

  )
}


#' Fischer Server
#'
#' @description Sets up the server logic for the Fischer analysis tab
fischer_server <- function() {
  
  dds <- list(readRDS(file.path("./data/fischer", "Fischer_Deseq2.rds")))
  clinical_data <- list(read.csv("./data/fischer/Fischer_demographics_information_Final.csv"))
  
  selection_server(dds,clinical_data,"fischer")
  input_server(dds = dds,clinical_data = clinical_data, "fischer")
  pca_metadata_server(dds = dds,clinical_data = clinical_data, "fischer")
  differential_gene_server(dds = dds,clinical_data = clinical_data, "fischer")
  heatmap_server(dds = dds,clinical_data = clinical_data, "fischer")

}

