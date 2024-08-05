gene_expression_server <- function(id, shared_reactives) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    
    debounced_marker_size <- debounce(reactive({ input$marker_size }), millis = 0)
    
    
    observe({ 
      
      updateSelectizeInput(session, "gene_plot_culstered_selection", choices = names(shared_reactives$sc1gene_data()), server = TRUE)
      updateSelectizeInput(session, "gene_plot_culstered_selection_2", choices = names(shared_reactives$sc1gene_data()), server = TRUE)
    })
   
    processed_data <- reactive({
      req(input$cell_plot_culstered_X_axis, 
          input$cell_plot_culstered_Y_axis, 
          input$gene_plot_culstered_selection,
          input$cell_subset, 
          input$cell_subset_choices_box,
          input$gene_plot_culstered_color, 
          input$gene_plot_culstered_selection_2)
      
      list(
        data1 = process_gene_data(shared_reactives$sc1conf_data(), shared_reactives$sc1meta_data(), input$cell_plot_culstered_X_axis, 
                                  input$cell_plot_culstered_Y_axis, input$gene_plot_culstered_selection,
                                  input$cell_subset, input$cell_subset_choices_box, shared_reactives$h5_data(), shared_reactives$sc1gene_data()),
        data2 = if (input$split_view) process_gene_data(shared_reactives$sc1conf_data(), shared_reactives$sc1meta_data(), input$cell_plot_culstered_X_axis, 
                                                        input$cell_plot_culstered_Y_axis, input$gene_plot_culstered_selection_2,
                                                        input$cell_subset, input$cell_subset_choices_box, shared_reactives$h5_data(), shared_reactives$sc1gene_data()) else NULL
      )
    })
    output$gene_plot_culstered <- renderPlotly({
      req(processed_data())
      
      p1 <- gene_plotly(processed_data()$data1, input$cell_plot_culstered_X_axis, 
                           input$cell_plot_culstered_Y_axis, input$gene_plot_culstered_color, debounced_marker_size())
      
      if (input$split_view && !is.null(processed_data()$data2)) {
        p2 <- gene_plotly(processed_data()$data2, input$cell_plot_culstered_X_axis, 
                             input$cell_plot_culstered_Y_axis, input$gene_plot_culstered_color, debounced_marker_size())
        
        # Extract layout properties from the first plot
        layout_p1 <- layout(p1)$xaxis
        layout_p1$scaleanchor <- 'x'
        layout_p1$scaleratio <- layout(p1)$xaxis$scaleratio
        
        # Apply the same properties to the second plot
        p2 <- p2 %>%
          layout(
            xaxis = list(scaleanchor = 'x', scaleratio = layout_p1$scaleratio),
            yaxis = list(scaleanchor = 'x', scaleratio = layout_p1$scaleratio)
          )
        
        # Combine the two plots
        p <- subplot(p1, p2, nrows = 1, shareX = TRUE, shareY = TRUE)
      } else {
        p <- p1
      }
      
      p
    })
    
    setup_download_handler(id, output, "gene_plot_culstered_data", reactive({processed_data()$data1$ggData}), "gene_plot_data")
    
    output$cell_datatable <- renderDataTable({
      req(input$cell_plot_culstered_info, 
          input$gene_plot_culstered_selection,
          input$cell_subset, 
          input$cell_subset_choices_box,
          input$inpsplt)
      
      ggData <- scDRnum(shared_reactives$sc1conf_data(), shared_reactives$sc1meta_data(), input$cell_plot_culstered_info, input$gene_plot_culstered_selection,
                        input$cell_subset, input$cell_subset_choices_box,
                        shared_reactives$h5_data(), shared_reactives$sc1gene_data(), input$inpsplt)
      
      datatable(ggData, rownames = FALSE, extensions = "Buttons",
                options = list(pageLength = -1, dom = "Bt", buttons = c("copy", "csv", "excel"))) %>%
        formatRound(columns = c("pctExpress"), digits = 2)
    })
  })
}
