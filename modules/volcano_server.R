volcano_server <- function(id, shared_reactives) {
  moduleServer(id, function(input, output, session) {
    blurbs <- fromJSON("./www/info_blurbs.json")
  
 
    dds_processed <- shared_reactives$dds_processed
  

    #' Process Data
    #' @description Processes the data for the volcano plot
    processed_data <- eventReactive(c(input$update_plot, input$selection), {
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

#' #' Event Observers
#' #' 
#' #' @description Sets up observers for plot interactions and gene selection
#' #' 
#' event_observers(input, session, display_genes, filtered_res, selected_genes_plotly, new_genes)
#' 



