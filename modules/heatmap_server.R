#' Heatmap Server
#'
#' @description Sets up the server logic for the heatmap analysis and related plots
#' @param id Module ID
heatmap_server <- function(id,shared_reactives) {
  moduleServer(id, function(input, output, session) {
    # Load the JSON file
    blurbs <- fromJSON("./www/info_blurbs.json")
    

    
    filtered_res <- shared_reactives$filtered_res
    dds_processed <- shared_reactives$dds_processed
    #' Process Data
    #' 
    #' @description Processes the heatmap data
    processed_data <- eventReactive(c(input$update_plot, input$selection), {
      process_heatmap_data(dds_processed(), isolate(input$slider_padj), isolate(input$slider_log2), isolate(input$number), isolate(input$selected_gene))
    })
    
    
    #' Plot Data
    #' 
    #' @description Generates the heatmap plot
    plot_data <- eventReactive(c(input$update_plot, input$selection),{
      plot_heatmap(processed_data()$matrix, dds_processed(), input$selected_gene)
    })
    

    output$heatmap_plot <- renderUI({
      result <- plot_data()
      if (is.character(result)) {
        div(class = "error-message", result)
      } else {
        result
      }
    })
    
    setup_download_handler(output, "heatmap_data", reactive({ processed_data()$matrix }), "heatmap")
    
 

    observeEvent(input$info_heatmap_plot, {
      shinyalert(title = blurbs$info$heatmap$title, html = TRUE,
                 text = blurbs$info$heatmap$text)
    })
    
    
    
    observe({
      filtered_res <- filtered_res()
      updateSelectizeInput(session, "selected_gene", choices = rownames(filtered_res), server = TRUE, selected = NULL)
    })
    
  })
}
