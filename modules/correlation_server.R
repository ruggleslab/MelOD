correlation_server <- function(id,shared_reactives) {
  moduleServer(id, function(input, output, session) {
    blurbs <- fromJSON("./www/info_blurbs.json")


    filtered_res <- shared_reactives$filtered_res
    dds_processed <- shared_reactives$dds_processed
    display_genes <- shared_reactives$display_genes
    
    
    observeEvent(c(input$update_plot, input$selection), {
      updateSelectizeInput(session, "gene_of_interest", choices = rownames(filtered_res()), server = TRUE)
    })

    #' Process Data
    #'
    #' @description Processes the correlation data
    processed_data <- eventReactive(c(input$update_plot, input$selection), {
      process_gene_correlations(dds_processed(), display_genes(), input$gene_of_interest, input$correlation_threshold)
    })

    #' Plot Data
    #'
    #' @description Generates the correlation plot
    plot_data <- eventReactive(c(input$update_plot, input$selection), {
      plot_gene_correlations(processed_data()$filtered_results, processed_data()$gene_of_interest)
    })


    
    output$correlation_plot <- renderUI({
      result <- plot_data()
      if (is.character(result)) {
        div(class = "error-message", result)
      } else {
        result
      }
    })
    


    setup_download_handler(output, "correlation_data", processed_data, "correlation")

    observeEvent(input$info_correlation_plot, {
      shinyalert(title = blurbs$info$correlation$title, html = TRUE,
                 text = blurbs$info$correlation$text)
    })

  })
}
