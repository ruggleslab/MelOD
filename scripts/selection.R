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
#' @param shared_reactives Shared reactive variables
selection_list_server <- function(dds, clinical_data, id, shared_reactives) {
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
        shared_reactives$selected_dds(selected_dds)  # Update the shared reactive value for dds
        shared_reactives$selected_clinical_data(selected_clinical_data)  # Update the shared reactive value for clinical data
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
    selected_dds <- reactiveVal()
    selected_clinical_data <- reactiveVal()
    utilities <- reactive({
      req(selected_dds())
      shared_server_utilities(selected_dds())
    })
    filtered_res <- reactive({
      req(utilities())
      utilities()$filtered_genes
    })
    dds_processed <- reactive({
      req(utilities())
      utilities()$dds
    })
    display_genes <- reactive({
      req(filtered_res())
      get_display_genes(filtered_res(), input$selected_gene)
    })
    
    observe({
      if (length(dds) > 1) {
        selection_list_server(dds, clinical_data, id, shared_reactives)  # Pass shared_reactives
      } else {
        selected_dds(dds[[1]])  # Default to the first dataset if only one is available
        selected_clinical_data(clinical_data[[1]])
      }
    })
    return(list(
      selected_dds = selected_dds,
      selected_clinical_data = selected_clinical_data,
      utilities = utilities,
      filtered_res = filtered_res,
      dds_processed = dds_processed,
      display_genes = display_genes
    ))
  })
}
