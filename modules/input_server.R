#' Input Server
#' 
#' @description Handles input for the application
#' @param dds DESeq2 dataset
#' @param clinical_data Clinical data
#' @param id Module ID
input_server <- function(dds, clinical_data, id) {
  moduleServer(id, function(input, output, session) {
    # Check the number of items in `dds` and adjust accordingly
    
    # Use the selected `dds` within a reactive context
    utilities <- reactive({
      dds <- global_selected_dds()
      shared_server_utilities(dds)
    })
    
    # Observe for changes in the filtered genes and update the select input
    observe({
      filtered_res <- utilities()$filtered_genes
      updateSelectizeInput(session, "selected_gene", choices = rownames(filtered_res), server = TRUE)
    })
    
    # Create a reactive expression for displaying genes
    display_genes <- reactive({
      utilities()$display_genes(input$selected_gene)
    })
    
    # Example output to show selected genes (if needed)
    output$gene_display <- renderTable({
      display_genes()
    })
  })
}
