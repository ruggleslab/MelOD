sc_violin_server <- function(id, shared_reactives) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    blurbs <- fromJSON("./www/info_blurbs.json")
    
    observe({  updateSelectizeInput(session, "violin_plot_X",
                                    choices = shared_reactives$sc1conf_data()[grp == TRUE]$UI,
                                    selected = shared_reactives$sc1def_data()$grp1)
      
      updateSelectizeInput(session, "violin_plot_Y_cell", server = TRUE, 
                           choices = c(shared_reactives$sc1conf_data()[is.na(fID)]$UI), 
                           selected = shared_reactives$sc1conf_data()[is.na(fID)]$UI[1])
      
      
      updateSelectizeInput(session, "violin_plot_Y_gene", server = TRUE, 
                           choices =  names(shared_reactives$sc1gene_data()))
      
      })
    
    
    observeEvent(input$info_sc_violin_plot, {
      shinyalert(
        title = blurbs$info$sc_violin$title, 
        html = TRUE,
        text = blurbs$info$sc_violin$text
      )
    })
    
    processed_data <- reactive({
      
      if (input$choice_comparison_violin == "Cell info"){
        violin_y_axis <- input$violin_plot_Y_cell
      } else violin_y_axis <- input$violin_plot_Y_gene
       
      req(input$violin_plot_X, input$cell_subset,violin_y_axis, input$cell_subset_choices_box)
      
      
      process_violin_single_cell_data(shared_reactives$sc1conf_data(), shared_reactives$sc1meta_data(), input$violin_plot_X, violin_y_axis, 
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
