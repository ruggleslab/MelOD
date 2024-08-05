proportion_server <- function(id,shared_reactives) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    
    observe({updateSelectizeInput(session, "proportion_plot_X",
                                  choices = shared_reactives$sc1conf_data()[grp == TRUE]$UI,
                                  selected = shared_reactives$sc1def_data()$grp1)
      
      updateSelectizeInput(session, "proportion_group_by",
                           choices = shared_reactives$sc1conf_data()[grp == TRUE]$UI,
                           selected = shared_reactives$sc1def_data()$grp2)
    })
    
    
    processed_data <- reactive({
      req(input$proportion_plot_X, input$proportion_group_by, input$cell_subset, input$cell_subset_choices_box)
      
      process_proportion_data(shared_reactives$sc1conf_data(), shared_reactives$sc1meta_data(), input$proportion_plot_X, input$proportion_group_by,
                              input$cell_subset, input$cell_subset_choices_box)
    })
    
    output$proportion_plot <- renderPlotly({
      req(processed_data())
      
      proportion_plotly(processed_data(), input$proportion_type, input$proportion_flip_axis)
    })
    
    setup_download_handler(id, output, "proportion_data", reactive({processed_data()$ggData}), "proportion_data")
    
    
  })
}


