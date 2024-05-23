correlation_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    blurbs <- fromJSON("./www/info_blurbs.json")
    
    selected_dds <- reactive({ global_selected_dds() })
    utilities <- reactive({ shared_server_utilities(selected_dds()) })
    filtered_res <- reactive({ utilities()$filtered_genes })
    dds_processed <- reactive({ utilities()$dds })
    display_genes <- reactive({ get_display_genes(filtered_res(), input$selected_gene) })
    
    observe({
      updateSelectizeInput(session, "gene_of_interest", choices = isolate(rownames(filtered_res())))
    })
    
    #' Process Data
    #' 
    #' @description Processes the correlation data
    processed_data <- eventReactive(c(input$update_plot, input$reset_selection), {
      process_gene_correlations(dds_processed(), display_genes(), input$gene_of_interest, input$correlation_threshold)
    })
    
    #' Plot Data
    #' 
    #' @description Generates the correlation plot
    plot_data <- eventReactive(c(input$update_plot, input$reset_selection), {
      plot_gene_correlations(processed_data()$filtered_results, processed_data()$gene_of_interest)
    })
    
    render_plots <- function(output, plot_data) {
      output$correlation_plot <- renderPlotly({
        plot_data()
      })
    }
    
    render_plots(output, plot_data)
    
    setup_download_handler(output, "correlation_data", processed_data, "correlation")
    
    observeEvent(input$info_correlation_plot, {
      shinyalert(title = blurbs$info$correlation$title, html = TRUE,
                 text = blurbs$info$correlation$text)
    })
  
  })
}
