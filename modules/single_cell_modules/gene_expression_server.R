gene_expression_server <- function(id, sc1conf, sc1meta, sc1gene, sc1def, h5_file_path) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    debounced_marker_size <- debounce(reactive({ input$marker_size }), millis = 0)
    
    updateSelectizeInput(session, "gene_plot_culstered_selection", choices = names(sc1gene), server = TRUE)
    updateSelectizeInput(session, "gene_plot_culstered_selection_2", choices = names(sc1gene), server = TRUE)

 
      output$gene_plot_culstered <- renderPlotly({
        req(input$cell_plot_culstered_X_axis, 
            input$cell_plot_culstered_Y_axis, 
            input$gene_plot_culstered_selection,
            input$cell_subset, 
            input$cell_subset_choices_box,
            input$gene_plot_culstered_color, 
            input$gene_plot_culstered_selection_2)
       
          print("gene expression")
          gene_plotly(sc1conf, sc1meta, input$cell_plot_culstered_X_axis, input$cell_plot_culstered_Y_axis, input$gene_plot_culstered_selection,
                      input$cell_subset, input$cell_subset_choices_box,
                      h5_file_path, sc1gene,
                      input$marker_size, input$gene_plot_culstered_color, input$split_view, input$gene_plot_culstered_selection_2)
      })

    
      output$cell_datatable <- renderDataTable({
        req(input$cell_plot_culstered_info, 
            input$gene_plot_culstered_selection,
            input$cell_subset, 
            input$cell_subset_choices_box,
            input$inpsplt)
        ggData <- scDRnum(sc1conf, sc1meta, input$cell_plot_culstered_info, input$gene_plot_culstered_selection,
                               input$cell_subset, input$cell_subset_choices_box,
                               h5_file_path, sc1gene, input$inpsplt)
        datatable(ggData, rownames = FALSE, extensions = "Buttons",
                  options = list(pageLength = -1, dom = "tB", buttons = c("copy", "csv", "excel"))) %>%
          formatRound(columns = c("pctExpress"), digits = 2)
      })
      
    })
}

    # # Download handlers
    # output$gene_plot_culstered_pdf <- downloadHandler(
    #   filename = function() {
    #     paste0("sc1", input$cell_plot_culstered_X_axis, "_", input$cell_plot_culstered_Y_axis, "_",
    #            input$gene_plot_culstered_selection, ".pdf")
    #   },
    #   content = function(file) {
    #     data <- plot_data()
    #     ggsave(file, device = "pdf", height = data$sc1a1oup2_h, width = data$sc1a1oup2_w, useDingbats = FALSE,
    #            plot = scDRgene(sc1conf, sc1meta, data$cell_plot_culstered_X_axis, data$cell_plot_culstered_Y_axis, data$gene_plot_culstered_selection,
    #                            data$cell_subset, data$cell_subset_choices_box,
    #                            h5_file_path, sc1gene,
    #                            data$marker_size, data$gene_plot_culstered_color, data$gene_plot_culstered_order,
    #                            data$sc1a1fsz, data$sc1a1asp, data$sc1a1txt))
    #   })
    # 
    # output$gene_plot_culstered_png <- downloadHandler(
    #   filename = function() {
    #     paste0("sc1", input$cell_plot_culstered_X_axis, "_", input$cell_plot_culstered_Y_axis, "_",
    #            input$gene_plot_culstered_selection, ".png")
    #   },
    #   content = function(file) {
    #     data <- plot_data()
    #     ggsave(file, device = "png", height = data$sc1a1oup2_h, width = data$sc1a1oup2_w,
    #            plot = scDRgene(sc1conf, sc1meta, data$cell_plot_culstered_X_axis, data$cell_plot_culstered_Y_axis, data$gene_plot_culstered_selection,
    #                            data$cell_subset, data$cell_subset_choices_box,
    #                            h5_file_path, sc1gene,
    #                            data$marker_size, data$gene_plot_culstered_color, data$gene_plot_culstered_order,
    #                            data$sc1a1fsz, data$sc1a1asp, data$sc1a1txt))
    #   })


