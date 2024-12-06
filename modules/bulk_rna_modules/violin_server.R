violin_server <- function(id, shared_reactives) {
  #' Violin Server
  #'
  #' @description Sets up the server logic for the violin analysis and related plots.
  #' @param id Module ID
  #' @param shared_reactives A reactiveValues object for sharing reactive variables across modules.
  
  moduleServer(id, function(input, output, session) {
    blurbs <- fromJSON("./www/info_blurbs.json")
    
    debounced_display_genes <- debounce(reactive({ shared_reactives$display_genes() }), millis = 800)
    
    processed_data <- reactive({
      process_violin_data(shared_reactives$dds_processed(), debounced_display_genes())
    })
    
    plot_data <- reactive({
      req(debounced_display_genes())
      plot_violin(
        processed_data()$merged_data, 
        processed_data()$gene_of_interest, 
        input$slider_padj, 
        input$box_or_violin, 
        input$violon_color, 
        input$violon_dot
      )
    })
    
    output$violin_plot <- renderPlotly({ plot_data() })
    
    observeEvent(input$info_violin_plot, {
      shinyalert(
        title = blurbs$info$violin$title, 
        html = TRUE, 
        text = blurbs$info$violin$text
      )
    })
    
    
    setup_download_handler(
      id, 
      output, 
      "violin_data", 
      reactive(processed_data()$merged_data), 
      "violin"
    )
    })
}
