volcano_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    blurbs <- fromJSON("./www/info_blurbs.json")
    
    selected_dds <- reactive({ global_selected_dds() })
    utilities <- reactive({ shared_server_utilities(selected_dds()) })
    dds_processed <- reactive({ utilities()$dds })
    
    #' Process Data
    #' @description Processes the data for the volcano plot
    processed_data <- eventReactive(c(input$update_plot, input$reset_selection), {
      process_volcano_data(dds_processed(), input$slider_padj, input$slider_log2)
    })
    
    #' Plot Data
    #' @description Generates the volcano plot
    plot_data <- reactive({
      plot_volcano(processed_data()$res, processed_data()$dds, input$selected_gene)
    })
    
    #' Render Plot
    output$volcano_plot <- renderPlotly({ plot_data() })
    
    observeEvent(input$info_volcano_plot, {
      shinyalert(title = blurbs$info$volcano$title, html = TRUE,
                 text = blurbs$info$volcano$text)
    })
    #' Download Handler
    setup_download_handler(output, "volcano_data", reactive({ processed_data()$res }), "volcano")
  })
}