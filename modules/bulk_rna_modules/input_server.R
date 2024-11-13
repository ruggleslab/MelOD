input_server <- function(id, shared_reactives) {
#' Input Server
#' 
#' @description Handles input for the application, updating the gene selection and displaying selected genes.
#' @param id A unique module ID used to identify the module in the Shiny application.
#' @param shared_reactives A reactiveValues object for sharing reactive variables across modules.

  moduleServer(id, function(input, output, session) {
    observe({
      updateMultiInput(
        session = session,
        inputId = "selected_gene",
        choices = rownames(shared_reactives$utilities()$filtered_genes)
      )
    })
    
    observeEvent(input$reset_selection, {
      updateMultiInput(
        session = session,
        inputId = "selected_gene",
        choices = rownames(shared_reactives$utilities()$filtered_genes),
        selected = NULL
      )
    })
    
    display_genes <- reactive({
      shared_reactives$utilities()$display_genes(input$selected_gene)
    })
    
    output$gene_display <- renderTable({
      display_genes()
    })
    
    output[[paste0(id, "_download")]] <- downloadHandler(
      filename = function() {
        paste0("processed_data_", id, ".rds")
      },
      content = function(file) {
        saveRDS(shared_reactives$selected_dds(), file)
      }
    )
    
  })
}
