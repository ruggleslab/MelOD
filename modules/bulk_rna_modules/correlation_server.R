correlation_server <- function(id,shared_reactives) {
  moduleServer(id, function(input, output, session) {
    
    blurbs <- fromJSON("./www/info_blurbs.json")

    
    filtered_res <- shared_reactives$filtered_res
    dds_processed <- shared_reactives$dds_processed
    display_genes <- shared_reactives$display_genes
    
    # Debounce the inputs to avoid frequent reloading
    gene_of_interest <- debounce(reactive(input$gene_of_interest), 500)
    correlation_threshold <- debounce(reactive(input$correlation_threshold), 500)
    
    
    observe( {
      updateSelectizeInput(session, "gene_of_interest", choices = rownames(filtered_res()), server = TRUE)
    })

    #' Process Data
    #'
    #' @description Processes the correlation data
    processed_data <- reactive( {
      process_gene_correlations(dds_processed(), display_genes(), gene_of_interest(), correlation_threshold())
    })

    #' Plot Data
    #'
    #' @description Generates the correlation plot
    plot_data <- reactive( {
      plot_gene_correlations(processed_data(),gene_of_interest() )
    })


    
    output$correlation_plot <- renderUI({
      result <- plot_data()
      if (is.character(result)) {
        div(class = "error-message", result)
      } else {
        result
      }
    })
    


    setup_download_handler(id, output, "correlation_data", processed_data, "correlation")

    observeEvent(input$info_correlation_plot, {
      shinyalert(title = blurbs$info$correlation$title, html = TRUE,
                 text = blurbs$info$correlation$text)
    })

  })
}
