cell_info_server <- function(id, sc1conf, sc1meta, sc1def) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    
    debounced_marker_size <- debounce(reactive({ input$marker_size }), millis = 0)
    
    
    updateSelectInput(session, "cell_plot_culstered_X_axis",
                      choices = sc1conf[dimred == TRUE]$UI,
                      selected = sc1def$dimred[1])
    
    updateSelectInput(session, "cell_plot_culstered_Y_axis",
                      choices = sc1conf[dimred == TRUE]$UI,
                      selected = sc1def$dimred[2])
    
    updateSelectInput(session, "cell_plot_culstered_info",
                      choices = sc1conf$UI,
                      selected = sc1def$meta1)
    
 
    updateSelectInput(session, "cell_plot_culstered_info_2",
                          choices = sc1conf$UI,
                          selected = sc1def$meta1)
  
    
   
    output$cell_plot_culstered <- renderPlotly({

      req(input$cell_plot_culstered_X_axis, 
          input$cell_plot_culstered_Y_axis, 
          input$cell_plot_culstered_info, 
          input$cell_subset, 
          input$cell_subset_choices_box,
          input$cell_plot_culstered_color, 
          input$cell_plot_culstered_label, 
          input$cell_plot_culstered_info_2)
    
    p1 <- cell_plotly(sc1conf, sc1meta, input$cell_plot_culstered_X_axis, input$cell_plot_culstered_Y_axis, input$cell_plot_culstered_info,
                      input$cell_subset, input$cell_subset_choices_box,
                      debounced_marker_size(), input$cell_plot_culstered_color, input$cell_plot_culstered_label)
    
    if (input$split_view) {
      p2 <- cell_plotly(sc1conf, sc1meta, input$cell_plot_culstered_X_axis, input$cell_plot_culstered_Y_axis, input$cell_plot_culstered_info_2,
                        input$cell_subset, input$cell_subset_choices_box,
                        debounced_marker_size(), input$cell_plot_culstered_color_2, input$cell_plot_culstered_label)
    
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
    
    
    
    
    
    
    
  })
}
    # 
    # output$cell_plot_culstered_pdf <- downloadHandler(
    #   filename = function() {
    #     paste0("sc1", input$cell_plot_culstered_X_axis, "_", input$cell_plot_culstered_Y_axis, "_",
    #            input$cell_plot_culstered_info, ".pdf")
    #   },
    #   content = function(file) {
    #     ggsave(file, device = "pdf", height = input$sc1a1oup1.h, width = input$sc1a1oup1.w, useDingbats = FALSE,
    #            plot = scDRcell(sc1conf, sc1meta, input$cell_plot_culstered_X_axis, input$cell_plot_culstered_Y_axis, input$cell_plot_culstered_info,
    #                            input$cell_subset, input$cell_subset_choices_box,
    #                            debounced_marker_size(), input$cell_plot_culstered_color, input$cell_plot_culstered_order,
    #                            input$sc1a1fsz, input$sc1a1asp, input$sc1a1txt, input$cell_plot_culstered_label))
    #   })
    # 
    # output$cell_plot_culstered_png <- downloadHandler(
    #   filename = function() {
    #     paste0("sc1", input$cell_plot_culstered_X_axis, "_", input$cell_plot_culstered_Y_axis, "_",
    #            input$cell_plot_culstered_info, ".png")
    #   },
    #   content = function(file) {
    #     ggsave(file, device = "png", height = input$sc1a1oup1.h, width = input$sc1a1oup1.w,
    #            plot = scDRcell(sc1conf, sc1meta, input$cell_plot_culstered_X_axis, input$cell_plot_culstered_Y_axis, input$cell_plot_culstered_info,
    #                            input$cell_subset, input$cell_subset_choices_box,
    #                            debounced_marker_size(), input$cell_plot_culstered_color, input$cell_plot_culstered_order,
    #                            input$sc1a1fsz, input$sc1a1asp, input$sc1a1txt, input$cell_plot_culstered_label))
    #   })
