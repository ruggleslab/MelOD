#' Heatmap Server
#'
#' @description Sets up the server logic for the heatmap analysis and related plots
#' @param id Module ID
#' @param shared_reactives A reactiveValues object for sharing reactive variables across modules.
heatmap_server <- function(id, shared_reactives) {
  moduleServer(id, function(input, output, session) {
    blurbs <- fromJSON("./www/info_blurbs.json")
    
    filtered_res <- shared_reactives$filtered_res
    dds_processed <- shared_reactives$dds_processed
    
    # Debounce inputs
    debounced_slider_padj <- debounce(reactive({ input$slider_padj }), millis = 1000)
    debounced_slider_log2 <- debounce(reactive({ input$slider_log2 }), millis = 1000)
    debounced_number <- debounce(reactive({ input$number }), millis = 1000)
    debounced_selected_gene <- debounce(reactive({ input$selected_gene }), millis = 1000)
    
    processed_data <- reactive({
      process_heatmap_data(dds_processed(), debounced_slider_padj(), debounced_slider_log2(), debounced_number(), debounced_selected_gene())
    })
    
    plot_data <- reactive({
      plot_heatmap(processed_data()$matrix, dds_processed(), debounced_selected_gene())
    })
    
    output$heatmap_plot <- renderUI({
      result <- plot_data()
      if (is.character(result)) {
        div(class = "error-message", result)
      } else {
        result
      }
    })
    
    observeEvent(input$info_heatmap_plot, {
      shinyalert(title = blurbs$info$heatmap$title, html = TRUE,
                 text = blurbs$info$heatmap$text)
    })
    
    observe({
      filtered_res <- filtered_res()
      updateSelectizeInput(session, "selected_gene", choices = rownames(filtered_res), server = TRUE, selected = NULL)
    })
    
    setup_download_handler(id, output, "heatmap_data", reactive({ processed_data()$matrix }), "heatmap")
    
  })
}
