heatmap_server <- function(id, shared_reactives) {
#' Heatmap Server
#'
#' @description Sets up the server logic for the heatmap analysis and related plots.
#' @param id Module ID
#' @param shared_reactives A reactiveValues object for sharing reactive variables across modules.

  moduleServer(id, function(input, output, session) {
    blurbs <- fromJSON("./www/info_blurbs.json")
    
    # Debounce inputs
    debounced_slider_padj <- debounce(reactive(input$slider_padj), millis = 1000)
    debounced_slider_log2 <- debounce(reactive(input$slider_log2), millis = 1000)
    debounced_number <- debounce(reactive(input$number), millis = 1000)
    debounced_selected_gene <- debounce(reactive(input$selected_gene), millis = 1000)
    debounced_z_score_range <- debounce(reactive(input$z_score_range), millis = 1000)
    debounced_font_size <- debounce(reactive(input$font_size), millis = 1000)
    
    processed_data <- reactive({
      if (input$number < 2 && length(input$selected_gene) < 2) {
        showFeedbackWarning(inputId = "number", text = "The number must be at least 2 if no genes are selected.")
      } else {
        hideFeedback("number")
      }
      process_heatmap_data(
        shared_reactives$dds_processed(), 
        debounced_slider_padj(), 
        debounced_slider_log2(), 
        debounced_number(), 
        debounced_selected_gene()
      )
    })
    
    plot_data <- reactive({
      plot_heatmap(
        processed_data()$matrix, 
        shared_reactives$dds_processed(), 
        debounced_selected_gene(), 
        heatmap_palette = input$heatmap_palette,
        z_score_range = debounced_z_score_range(),
        font_size = debounced_font_size()
      )
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
      shinyalert(
        title = blurbs$info$heatmap$title, 
        html = TRUE,
        text = blurbs$info$heatmap$text
      )
    })
    
    observe({
      updateSelectizeInput(session, "selected_gene", choices = rownames(shared_reactives$filtered_res()), server = TRUE, selected = NULL)
    })
    
    setup_download_handler(id, output, "heatmap_data", reactive({ processed_data()$matrix }), "heatmap")
  })
}


