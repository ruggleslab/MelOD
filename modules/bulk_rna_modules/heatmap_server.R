#' Heatmap Server
#'
#' @description Sets up the server logic for the heatmap analysis and related plots
#' @param id Module ID
#' @param shared_reactives A reactiveValues object for sharing reactive variables across modules.
heatmap_server <- function(id,shared_reactives) {
  moduleServer(id, function(input, output, session) {
    blurbs <- fromJSON("./www/info_blurbs.json")
  
    filtered_res <- shared_reactives$filtered_res
    dds_processed <- shared_reactives$dds_processed

    processed_data <- eventReactive(c(input$update_plot, input$selection), {
      process_heatmap_data(dds_processed(), isolate(input$slider_padj), isolate(input$slider_log2), isolate(input$number), isolate(input$selected_gene))
    })
    
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
