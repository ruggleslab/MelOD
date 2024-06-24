#' Violin Server
#'
#' @description Sets up the server logic for the violin analysis and related plots
#' @param id Module ID
#' @param shared_reactives A reactiveValues object for sharing reactive variables across modules.
violin_server <- function(id, shared_reactives) {
  moduleServer(id, function(input, output, session) {
    blurbs <- fromJSON("./www/info_blurbs.json")
    
    filtered_res <- shared_reactives$filtered_res
    dds_processed <- shared_reactives$dds_processed
    display_genes <- shared_reactives$display_genes
    
    # Debounce the display_genes reactive to wait for user input
    debounced_display_genes <- debounce(reactive({ display_genes() }), millis = 1000)
    
    processed_data <- reactive({
      process_violin_data(dds_processed(), debounced_display_genes())
    })
    
    plot_data <- reactive({
      plot_violin(processed_data()$merged_data, processed_data()$gene_of_interest, input$slider_padj, input$box_or_violin, input$violon_color, input$violon_dot)
    })
    
    output$violin_plot <- renderPlotly({ plot_data() })
    
    observeEvent(input$info_violin_plot, {
      shinyalert(title = blurbs$info$violin$title, html = TRUE,
                 text = blurbs$info$violin$text)
    })
    
    output$filtered_results <- render_filtered_results_table(dds_processed, input)
    
    setup_download_handler(id, output, "violin_data", processed_data, "violin")
    
  })
}
