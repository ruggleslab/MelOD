#' Selection Badal Server
#' 
#' @description Handles selection for badal input
#' @param dds DESeq2 dataset
#' @param clinical_data Clinical data
#' @param id Module ID
selection_badal_server <- function(dds, clinical_data, id) {
  moduleServer(id, function(input, output, session) {
    observe({
      req(input$selection_badal)  # Ensure that input selection is available before proceeding
    })
  })
}

#' Selection List Server
#' 
#' @description Handles selection for list input
#' @param dds DESeq2 dataset
#' @param clinical_data Clinical data
#' @param id Module ID
selection_list_server <- function(dds, clinical_data, id) {
  moduleServer(id, function(input, output, session) {
    observe({
      req(input$selection)  # Ensure that input selection is available before proceeding
      selected_dds <- NULL
      selected_clinical_data <- NULL
      
      if ("Combotherapy" %in% input$selection) {
        selected_dds <- dds[[1]]
        selected_clinical_data <- clinical_data[[1]]
      } else if ("Monotherapy" %in% input$selection) {
        selected_dds <- dds[[2]]
        selected_clinical_data <- clinical_data[[2]]
      }
      
      if (!is.null(selected_dds)) {
        global_selected_dds(selected_dds)  # Update the global reactive value for dds
        global_selected_clinical_data(selected_clinical_data)  # Update the global reactive value for clinical data
      }
    })
  })
}

#' Selection Server
#' 
#' @description Handles overall selection logic
#' @param dds DESeq2 dataset
#' @param clinical_data Clinical data
#' @param id Module ID
selection_server <- function(dds, clinical_data, id) {
  moduleServer(id, function(input, output, session) {
    observe({
      if (length(dds) > 1) {
        selection_list_server(dds, clinical_data, id)  # Now also passing clinical_data
      } else {
        global_selected_dds(dds[[1]])  # Default to the first dataset if only one is available
        global_selected_clinical_data(clinical_data[[1]])
      }
    })
  })
}
