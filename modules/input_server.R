#' Input Server
#' 
#' @description Handles input for the application, updating the gene selection and displaying selected genes.
#' @param id A unique module ID used to identify the module in the Shiny application.
#' @param shared_reactives A reactiveValues object for sharing reactive variables across modules.
input_server <- function(id, shared_reactives) {
  moduleServer(id, function(input, output, session) {
    
    selected_dds <- shared_reactives$selected_dds
    utilities <- shared_reactives$utilities
    dds_processed <- shared_reactives$dds_processed
    
    # Observe for changes in the filtered genes and update the select input
    observe({
      filtered_res <- utilities()$filtered_genes
      updateSelectizeInput(session, "selected_gene", choices = rownames(filtered_res), server = TRUE, selected = NULL)
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
