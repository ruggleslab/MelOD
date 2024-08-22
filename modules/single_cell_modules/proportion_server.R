proportion_server <- function(id, shared_reactives) {
  #' Proportion Server Module
  #'
  #' This function defines a Shiny module server for visualizing the proportion of cell subsets
  #' across different groups in a single-cell dataset. It manages input selections for X-axis 
  #' and grouping variables, processes the data for proportion calculations, and generates 
  #' Plotly visualizations. The module also includes event handling for displaying additional
  #' information through alerts.
  #'
  #' @param id The module ID.
  #' @param shared_reactives A list of reactive objects shared across the application, including 
  #' single-cell data (`sc1conf_data`, `sc1meta_data`).
  #'
  #' @return This module does not return a value; it is used for its side effects within a Shiny application.
  
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    blurbs <- fromJSON("./www/info_blurbs.json")
    
    observe({
      updateSelectizeInput(session, "proportion_plot_X",
                           choices = shared_reactives$sc1conf_data()[grp == TRUE]$UI,
                           selected = shared_reactives$sc1def_data()$grp1)
      
      updateSelectizeInput(session, "proportion_group_by",
                           choices = shared_reactives$sc1conf_data()[grp == TRUE]$UI,
                           selected = shared_reactives$sc1def_data()$grp2)
    })
    
    observeEvent(input$info_proportion_plot, {
      shinyalert(
        title = blurbs$info$proportion$title, 
        html = TRUE,
        text = blurbs$info$proportion$text
      )
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
