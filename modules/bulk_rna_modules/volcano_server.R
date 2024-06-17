volcano_server <- function(id, shared_reactives) {
  moduleServer(id, function(input, output, session) {
    blurbs <- fromJSON("./www/info_blurbs.json")
  
 
    filtered_res <- shared_reactives$filtered_res
    dds_processed <- shared_reactives$dds_processed
    current_selection <- reactiveVal(character())
    
    
    #' Process Data
    #' @description Processes the data for the volcano plot
    processed_data <- reactive({
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
    setup_download_handler(id, output, "volcano_data", reactive({ processed_data()$res }), "volcano")

    observe({
      runjs("Shiny.setInputValue('plotly_selected-A', null);
            Shiny.setInputValue('plotly_click-A', null);")
      new_selection <- event_data("plotly_selected")$customdata
      new_click <- event_data("plotly_click")$customdata
      current_genes <- current_selection()
      if (!is.null(new_selection)) {
        current_genes <- unique(c(current_genes, new_selection))
      }
      if (!is.null(new_click)) {
        current_genes <- unique(c(current_genes, new_click))
      }
      current_selection(current_genes)
      updateSelectizeInput(session, "selected_gene", choices = rownames(filtered_res()), server = TRUE, selected = current_genes)
    })
    
    # Reset button to clear the selection
    observeEvent(input$reset_selection, {
      current_selection(character())  
      runjs("Shiny.setInputValue('plotly_selected-A', null);
            Shiny.setInputValue('plotly_click-A', null);")
      updateSelectizeInput(session, "selected_gene", choices = rownames(filtered_res()), server = TRUE, selected = NULL)
      
    })
    
    
  })
}





