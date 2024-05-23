

#' Heatmap Server
#'
#' @description Sets up the server logic for the heatmap analysis and related plots
#' @param id Module ID
heatmap_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Load the JSON file
    blurbs <- fromJSON("./www/info_blurbs.json")
    
    
    #' Initialize Reactives
    #' 
    #' @description Initializes the reactive expressions for the module
    selected_dds <- reactive({ global_selected_dds() })
    utilities <- reactive({ shared_server_utilities(selected_dds()) })
    filtered_res <- reactive({ utilities()$filtered_genes })
    dds_processed <- reactive({ utilities()$dds })
    
    
    #' Process Data
    #' 
    #' @description Processes the heatmap data
    processed_data <- eventReactive(c(input$update_plot, input$reset_selection), {
      process_heatmap_data(dds_processed(), isolate(input$slider_padj), isolate(input$slider_log2), isolate(input$number), isolate(input$selected_gene))
    })
    
    
    #' Plot Data
    #' 
    #' @description Generates the heatmap plot
    plot_data <- eventReactive(c(input$update_plot, input$reset_selection),{
      plot_heatmap(processed_data()$matrix, dds_processed(), input$selected_gene)
    })
    
    render_plots <- function(output, plot_data) {
      output$heatmap_plot <- renderPlotly({
        plot_data()
      })
    }
    
    render_plots(output, plot_data)
    
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
