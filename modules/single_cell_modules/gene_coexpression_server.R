gene_coexpression_server <- function(id, shared_reactives) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    blurbs <- fromJSON("./www/info_blurbs.json")
    
    
    debounced_marker_size <- debounce(reactive({ input$marker_size }), millis = 0)
    

    observe({ 
      
      updateSelectizeInput(session, "gene_plot_coexpression_selection", choices = names(shared_reactives$sc1gene_data()), server = TRUE)
      updateSelectizeInput(session, "gene_plot_coexpression_selection_2", choices = names(shared_reactives$sc1gene_data()), selected = names(shared_reactives$sc1gene_data())[3], server = TRUE)
    })
    
    observeEvent(input$info_coexpression_plot, {
      shinyalert(
        title = blurbs$info$coexpression$title, 
        html = TRUE,
        text = blurbs$info$coexpression$text
      )
    })

    processed_data <- reactive({

      req(input$cell_plot_clustered_X_axis, 
          input$cell_plot_clustered_Y_axis, 
          input$gene_plot_coexpression_selection,
          input$gene_plot_coexpression_selection_2,
          input$cell_subset, 
          input$cell_subset_choices_box,
          input$gene_plot_coexpression_color)
      process_coexpression_data(shared_reactives$sc1conf_data(), shared_reactives$sc1meta_data(), input$cell_plot_clustered_X_axis, 
                                input$cell_plot_clustered_Y_axis, input$gene_plot_coexpression_selection, 
                                input$gene_plot_coexpression_selection_2, input$cell_subset, 
                                input$cell_subset_choices_box, shared_reactives$h5_data(), shared_reactives$sc1gene_data())
    })
    
    output$gene_plot_coexpression <- renderPlotly({
      req(processed_data())
      
      coexpression_plotly(processed_data(),input$cell_plot_clustered_X_axis, 
                          input$cell_plot_clustered_Y_axis, input$gene_plot_coexpression_color, debounced_marker_size())
    })
    
    setup_download_handler(id, output, "coexpressed_gene_plot_clustered_data", reactive({processed_data()$ggData}), "coexpressed_gene_plot_data")
    
    
    output$gene_datatable_coexpression <- renderDataTable({
      req(input$gene_plot_coexpression_selection, 
          input$gene_plot_coexpression_selection_2,
          input$cell_subset, 
          input$cell_subset_choices_box)
      
      ggData <- scDRcoexNum(shared_reactives$sc1conf_data(), shared_reactives$sc1meta_data(), input$gene_plot_coexpression_selection, 
                            input$gene_plot_coexpression_selection_2, input$cell_subset, 
                            input$cell_subset_choices_box, shared_reactives$h5_data(), shared_reactives$sc1gene_data()) 
      
      datatable(ggData, rownames = FALSE, extensions = "Buttons", 
                options = list(pageLength = -1, dom = "Bt", buttons = c("copy", "csv", "excel"))) %>% 
        formatRound(columns = c("percent"), digits = 2)
    })
  })
}







