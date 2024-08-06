comparison_server <- function(id, shared_reactives) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    
    debounced_marker_size <- debounce(reactive({ input$marker_size }), millis = 0)
    
    
    observe({ 
      
      updateSelectizeInput(session, "gene_plot_clustered_selection", choices = names(shared_reactives$sc1gene_data()), server = TRUE)
      updateSelectizeInput(session, "gene_plot_clustered_selection_2", choices = names(shared_reactives$sc1gene_data()), server = TRUE)
  
      
      updateSelectInput(session, "cell_plot_clustered_info",
                        choices = shared_reactives$sc1conf_data()$UI,
                        selected =  shared_reactives$sc1def_data()$meta1)
      
      updateSelectInput(session, "cell_plot_clustered_info_2",
                        choices = shared_reactives$sc1conf_data()$UI,
                        selected =  shared_reactives$sc1def_data()$meta1)
    })
   
    processed_data <- reactive({
      req(input$cell_plot_clustered_X_axis, 
          input$cell_plot_clustered_Y_axis, 
          input$gene_plot_clustered_selection,
          input$cell_subset, 
          input$cell_subset_choices_box,
          input$gene_plot_clustered_color, 
          input$gene_plot_clustered_selection_2,
          input$cell_plot_clustered_info, 
          input$cell_plot_clustered_color, 
          input$cell_plot_clustered_label, 
          input$cell_plot_clustered_info_2)
      
      
      list(
        data_cell_1 = if (input$choice_comparison_cell_gene == "Cell Vs Cell" | input$choice_comparison_cell_gene == "Cell Vs Gene") {
          process_cell_data(shared_reactives$sc1conf_data(), shared_reactives$sc1meta_data(), input$cell_plot_clustered_X_axis, 
                            input$cell_plot_clustered_Y_axis, input$cell_plot_clustered_info,
                            input$cell_subset, input$cell_subset_choices_box)
        } else {
          NULL
        },
        
        data_cell_2 = if (input$choice_comparison_cell_gene == "Cell Vs Cell") {
          process_cell_data(shared_reactives$sc1conf_data(), shared_reactives$sc1meta_data(), input$cell_plot_clustered_X_axis, 
                            input$cell_plot_clustered_Y_axis, input$cell_plot_clustered_info_2,
                            input$cell_subset, input$cell_subset_choices_box)
        } else {
          NULL
        },
        
        data_gene_1 = if (input$choice_comparison_cell_gene == "Gene Vs Gene" | input$choice_comparison_cell_gene == "Cell Vs Gene") {
          process_gene_data(shared_reactives$sc1conf_data(), shared_reactives$sc1meta_data(), input$cell_plot_clustered_X_axis, 
                            input$cell_plot_clustered_Y_axis, input$gene_plot_clustered_selection,
                            input$cell_subset, input$cell_subset_choices_box, shared_reactives$h5_data(), shared_reactives$sc1gene_data())
        } else {
          NULL
        },
        
        data_gene_2 = if (input$choice_comparison_cell_gene == "Gene Vs Gene") {
          process_gene_data(shared_reactives$sc1conf_data(), shared_reactives$sc1meta_data(), input$cell_plot_clustered_X_axis, 
                            input$cell_plot_clustered_Y_axis, input$gene_plot_clustered_selection_2,
                            input$cell_subset, input$cell_subset_choices_box, shared_reactives$h5_data(), shared_reactives$sc1gene_data())
        } else {
          NULL
        }
      )
      
      
    })

    
    output$gene_plot_clustered <- renderPlotly({
      req(processed_data())
      
      
      if (input$choice_comparison_cell_gene == "Cell Vs Cell") {
        p1 <- cell_plotly(processed_data()$data_cell_1, debounced_marker_size(),input$cell_plot_clustered_X_axis, 
                          input$cell_plot_clustered_Y_axis, input$cell_plot_clustered_color, input$cell_plot_clustered_label)
        p2 <- cell_plotly(processed_data()$data_cell_2, debounced_marker_size(),input$cell_plot_clustered_X_axis, 
                          input$cell_plot_clustered_Y_axis, input$cell_plot_clustered_color, input$cell_plot_clustered_label)
      }
      
      if (input$choice_comparison_cell_gene == "Cell Vs Gene") {
        p1 <- cell_plotly(processed_data()$data_cell_1, debounced_marker_size(),input$cell_plot_clustered_X_axis, 
                          input$cell_plot_clustered_Y_axis, input$cell_plot_clustered_color, input$cell_plot_clustered_label)
        p2 <- gene_plotly(processed_data()$data_gene_1, input$cell_plot_clustered_X_axis,
                         input$cell_plot_clustered_Y_axis, input$gene_plot_clustered_color, debounced_marker_size())
      }
      if (input$choice_comparison_cell_gene == "Gene Vs Gene") {
        p1 <- gene_plotly(processed_data()$data_gene_1, input$cell_plot_clustered_X_axis,
                          input$cell_plot_clustered_Y_axis, input$gene_plot_clustered_color, debounced_marker_size())
        p2 <- gene_plotly(processed_data()$data_gene_2, input$cell_plot_clustered_X_axis,
                          input$cell_plot_clustered_Y_axis, input$gene_plot_clustered_color, debounced_marker_size())
      }
      
      

        # Extract layout properties from the first plot
        layout_p1 <- layout(p1)$xaxis
        layout_p1$scaleanchor <- 'x'
        layout_p1$scaleratio <- layout(p1)$xaxis$scaleratio
        
        # Apply the same properties to the second plot
        p2 <- p2 %>%
          layout(
            xaxis = list(scaleanchor = 'x', scaleratio = layout_p1$scaleratio),
            yaxis = list(scaleanchor = 'x', scaleratio = layout_p1$scaleratio),
            showlegend = TRUE
            
          )
        
        # Combine the two plots
        p <- subplot(p1, p2, nrows = 1, shareX = TRUE, shareY = TRUE)
      
      p
    })
    
    
    
    setup_download_handler(id, output, "gene_plot_clustered_data", reactive({processed_data()$data1$ggData}), "gene_plot_data")
    
    output$cell_datatable <- renderDataTable({
      req(input$cell_plot_clustered_info, 
          input$gene_plot_clustered_selection,
          input$cell_subset, 
          input$cell_subset_choices_box,
          input$inpsplt)
      
      ggData <- scDRnum(shared_reactives$sc1conf_data(), shared_reactives$sc1meta_data(), input$cell_plot_clustered_info, input$gene_plot_clustered_selection,
                        input$cell_subset, input$cell_subset_choices_box,
                        shared_reactives$h5_data(), shared_reactives$sc1gene_data(), input$inpsplt)
      
      datatable(ggData, rownames = FALSE, extensions = "Buttons",
                options = list(pageLength = -1, dom = "Bt", buttons = c("copy", "csv", "excel"))) %>%
        formatRound(columns = c("pctExpress"), digits = 2)
    })
  })
}
