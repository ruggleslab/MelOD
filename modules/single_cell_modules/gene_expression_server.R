gene_expression_server <- function(id, sc1conf, sc1meta, sc1gene, sc1def, h5_file_path) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    
    debounced_marker_size <- debounce(reactive({ input$marker_size }), millis = 500)
    
    
    optCrt <- "{ option_create: function(data, escape) { return('<div class=\"create\"><strong>' + data.value + '</strong></div>'); } }"
    updateSelectizeInput(session, "gene_plot_culstered_selection", choices = names(sc1gene), server = TRUE,
                         selected = sc1def$gene1, options = list(
                           maxOptions = 7, create = TRUE, persist = TRUE, render = I(optCrt)))
    
    output$gene_plot_culstered <- renderPlotly({
      scDRgene_plotly(sc1conf, sc1meta, input$cell_plot_culstered_X_axis, input$cell_plot_culstered_Y_axis, input$gene_plot_culstered_selection,
                      input$cell_subset, input$cell_subset_choices_box,
                      h5_file_path, sc1gene,
                      debounced_marker_size(), input$gene_plot_culstered_color, input$gene_plot_culstered_order)
    })
    
    output$gene_plot_culstered_pdf <- downloadHandler(
      filename = function() {
        paste0("sc1", input$cell_plot_culstered_X_axis, "_", input$cell_plot_culstered_Y_axis, "_",
               input$gene_plot_culstered_selection, ".pdf")
      },
      content = function(file) {
        ggsave(file, device = "pdf", height = input$sc1a1oup2.h, width = input$sc1a1oup2.w, useDingbats = FALSE,
               plot = scDRgene(sc1conf, sc1meta, input$cell_plot_culstered_X_axis, input$cell_plot_culstered_Y_axis, input$gene_plot_culstered_selection,
                               input$cell_subset, input$cell_subset_choices_box,
                               h5_file_path, sc1gene,
                               debounced_marker_size(), input$gene_plot_culstered_color, input$gene_plot_culstered_order,
                               input$sc1a1fsz, input$sc1a1asp, input$sc1a1txt))
      })
    
    output$gene_plot_culstered_png <- downloadHandler(
      filename = function() {
        paste0("sc1", input$cell_plot_culstered_X_axis, "_", input$cell_plot_culstered_Y_axis, "_",
               input$gene_plot_culstered_selection, ".png")
      },
      content = function(file) {
        ggsave(file, device = "png", height = input$sc1a1oup2.h, width = input$sc1a1oup2.w,
               plot = scDRgene(sc1conf, sc1meta, input$cell_plot_culstered_X_axis, input$cell_plot_culstered_Y_axis, input$gene_plot_culstered_selection,
                               input$cell_subset, input$cell_subset_choices_box,
                               h5_file_path, sc1gene,
                               debounced_marker_size(), input$gene_plot_culstered_color, input$gene_plot_culstered_order,
                               input$sc1a1fsz, input$sc1a1asp, input$sc1a1txt))
      })
  })
}
