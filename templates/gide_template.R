#' Gide UI
#'
#' @description Creates the UI layout for the Gide analysis tab
#' @param id Module ID
#' @return A Shiny UI element for the Gide analysis tab
gide_ui <- function(id) {
  
  fluidPage(
    add_busy_spinner(spin = "fading-circle", color = "#FFA812"),
    
    
    

    
    fluidRow(blurb_study_ui("gide")),
    fluidRow(column(6, blurb_data_ui("gide")),
             column(6, fluidRow(blurb_comparison_ui("gide"),
                                gide_selector_ui("gide")))),
    fluidRow(
      column(6,pca_ui("gide")),
      column(6,metadata_ui("gide"))),
    # fluidRow(
    #   column(6,input_ui("gide")),
    #   column(6,volcano_ui("gide"))),
    # fluidRow(
    #   column(6,violin_ui("gide")),
    #   column(6,deseq2_table_ui("gide"))),
    # heatmap_ui("gide")
    # 
    # 
    # 
    # 
    # 
    # fluidRow(
    #   blurb_explanation_ui("gide")),
    #   column(6,
    #   column(12,
    #   input_ui("gide")),
    #   column(12,gide_selector_ui('gide'))),
    # column(6,
    #   blurb_study_ui("gide")),
    # 
    # pca_metadata_ui('gide'),
    # differential_gene_ui('gide'),
    deseq2_table_ui('gide'),
    heatmap_ui('gide')
    
  
  )
}

#' Gide Selector UI
#' 
#' @description Creates the UI component for selecting comparisons in the Gide study
#' @param id Module ID
#' @return A Shiny UI element
gide_selector_ui <- function(id) {
  ns <- NS(id)
  box(
    background = 'orange', width = 12,
    radioButtons(
      label = "Selection",
      inputId = ns("selection"),
      choices = c("Combotherapy", "Monotherapy"),
      selected = "Combotherapy",
      inline = TRUE
    ),
    uiOutput(ns("debug_selection"))
  )
}

#' Gide Server
#'
#' @description Sets up the server logic for the Gide analysis tab
gide_server <- function() {
  

  dds <- list(readRDS(file.path("./data/gide/mono", "ddsPreMono.rds")), readRDS(file.path("./data/gide/combo", "ddsPreCombo.rds")))
  clinical_data <- list(read.csv(file.path("./data/gide/mono", "Gide_demographics_monotherapy.csv"), sep=','), read.csv(file.path("./data/gide/combo", "Gide_demographics_combotherapy.csv"), sep=','))
  
  selection_server(dds,clinical_data,"gide")
  selection_list_server(dds,clinical_data,"gide")
  input_server(dds ,clinical_data = clinical_data, "gide")
  pca_metadata_server(dds,clinical_data = clinical_data, "gide")
  differential_gene_server(dds = dds,clinical_data = clinical_data, "gide")
  heatmap_server(dds = dds,clinical_data = clinical_data, "gide")
  
}


