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
    fluidRow(
      column(6,input_ui("gide")),
      column(6,volcano_ui("gide"))),
    fluidRow(
      column(6,deseq2_table_ui("gide")),
      column(6,violin_ui("gide"))),
    fluidRow(
      column(8,heatmap_ui("gide")),
      column(4, correlation_ui("gide")))
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
  

  # Initialize servers
  selection_result <- selection_server(dds, clinical_data, "gide")
  selection_list_server(dds,clinical_data,"gide",selection_result)
  input_server("gide", selection_result)
  volcano_server("gide", selection_result)
  violin_server("gide", selection_result)
  correlation_server("gide", selection_result)
  heatmap_server("gide", selection_result)
  pca_metadata_server("gide", selection_result) 
  
}
