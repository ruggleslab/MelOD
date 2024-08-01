sc_violin_server <- function(id, shared_reactives) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    updateSelectizeInput(session, "violin_plot_X",
                         choices = shared_reactives$sc1conf_data()[grp == TRUE]$UI,
                         selected = shared_reactives$sc1def_data()$grp1)
    
    updateSelectizeInput(session, "violin_plot_Y", server = TRUE, 
                         choices = c(shared_reactives$sc1conf_data()[is.na(fID)]$UI, names(shared_reactives$sc1gene_data())), 
                         selected = shared_reactives$sc1conf_data()[is.na(fID)]$UI[1], options = list(
                           maxOptions = length(shared_reactives$sc1conf_data()[is.na(fID)]$UI) + 3))
    
    processed_data <- reactive({
      req(input$violin_plot_X, input$violin_plot_Y, input$cell_subset, input$cell_subset_choices_box)
      
      process_violin_data(shared_reactives$sc1conf_data(), shared_reactives$sc1meta_data(), input$violin_plot_X, input$violin_plot_Y, 
                          input$cell_subset, input$cell_subset_choices_box, 
                          shared_reactives$h5_data(), shared_reactives$sc1gene_data())
    })
    
    output$sc_violin_plot <- renderPlotly({ 
      req(processed_data())
      
      sc_violin_plotly(processed_data(), input$sc_box_or_violin, input$sc_violon_dot)
    }) 
    setup_download_handler(id, output, "sc_violin_boxplot_data", reactive({processed_data()$ggData}), "gene_plot_data")
    
    
  })
}
