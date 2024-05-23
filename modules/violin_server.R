violin_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    blurbs <- fromJSON("./www/info_blurbs.json")
    selected_dds <- reactive({ global_selected_dds() })
    utilities <- reactive({ shared_server_utilities(selected_dds()) })
    filtered_res <- reactive({ utilities()$filtered_genes })
    dds_processed <- reactive({ utilities()$dds })
    display_genes <- reactive({ get_display_genes(filtered_res(), input$selected_gene) })

    #' Process Data
    #' @description Processes the data for the violin plot
    processed_data <- eventReactive(c(input$update_plot, input$reset_selection), {
      
      process_violin_data(dds_processed(), display_genes())
    })
    
    #' Plot Data
    #' @description Generates the violin plot
    plot_data <- reactive({
      plot_violin(processed_data()$merged_data, processed_data()$gene_of_interest, input$slider_padj)
    })
    

    #' Render Plot
    output$violin_plot <- renderPlotly({ plot_data() })

    #' Download Handler
    setup_download_handler(output, "violin_data", processed_data, "violin")

    observeEvent(input$info_violin_plot, {
      shinyalert(title = blurbs$info$violin$title, html = TRUE,
                 text = blurbs$info$violin$text)
    })

    #' Filtered Results Table
    #'
    #' @description Renders the filtered results table
    output$filtered_results <- render_filtered_results_table(dds_processed, input)

  })
}

