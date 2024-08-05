cell_info_server <- function(id, shared_reactives) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    debounced_marker_size <- debounce(reactive({ input$marker_size }), millis = 0)
    
    
    observe({ 
      updateSelectInput(session, "cell_plot_culstered_X_axis",
                        choices = shared_reactives$sc1conf_data()[dimred == TRUE]$UI,
                        selected =  shared_reactives$sc1def_data()$dimred[1])
      
      updateSelectInput(session, "cell_plot_culstered_Y_axis",
                        choices = shared_reactives$sc1conf_data()[dimred == TRUE]$UI,
                        selected =  shared_reactives$sc1def_data()$dimred[2])
      
      updateSelectInput(session, "cell_plot_culstered_info",
                        choices = shared_reactives$sc1conf_data()$UI,
                        selected =  shared_reactives$sc1def_data()$meta1)
      
      updateSelectInput(session, "cell_plot_culstered_info_2",
                        choices = shared_reactives$sc1conf_data()$UI,
                        selected =  shared_reactives$sc1def_data()$meta1)})

   
    
    processed_data <- reactive({
      req(input$cell_plot_culstered_X_axis, 
          input$cell_plot_culstered_Y_axis, 
          input$cell_plot_culstered_info, 
          input$cell_subset, 
          input$cell_subset_choices_box,
          input$cell_plot_culstered_color, 
          input$cell_plot_culstered_label, 
          input$cell_plot_culstered_info_2)
      
      
      
      list(
        data1 = process_cell_data(shared_reactives$sc1conf_data(), shared_reactives$sc1meta_data(), input$cell_plot_culstered_X_axis, 
                                 input$cell_plot_culstered_Y_axis, input$cell_plot_culstered_info,
                                 input$cell_subset, input$cell_subset_choices_box),
        data2 = if (input$split_view) process_cell_data(shared_reactives$sc1conf_data(), shared_reactives$sc1meta_data(), input$cell_plot_culstered_X_axis, 
                                                        input$cell_plot_culstered_Y_axis, input$cell_plot_culstered_info_2,
                                                        input$cell_subset, input$cell_subset_choices_box) else NULL
      )
      
      
      
      
    })
    
    output$cell_plot_culstered <- renderPlotly({
      req(processed_data())
      
      p1 <- cell_plotly(processed_data()$data1, debounced_marker_size(),input$cell_plot_culstered_X_axis, 
                        input$cell_plot_culstered_Y_axis, input$cell_plot_culstered_color, input$cell_plot_culstered_label)
      
      if (input$split_view) {
        p2 <- cell_plotly(processed_data()$data2, debounced_marker_size(),input$cell_plot_culstered_X_axis, 
                          input$cell_plot_culstered_Y_axis, input$cell_plot_culstered_color_2, input$cell_plot_culstered_label)
        
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
    
    setup_download_handler(id, output, "cell_plot_culstered_data", reactive({processed_data()$ggData}), "cell_plot_data")
  })
}

  
