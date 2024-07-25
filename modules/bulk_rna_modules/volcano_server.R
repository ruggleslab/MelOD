volcano_server <- function(id, shared_reactives) {
#' Volcano Server
#'
#' @description Sets up the server logic for the volcano analysis and related plots.
#' @param id Module ID
#' @param shared_reactives A reactiveValues object for sharing reactive variables across modules.
  moduleServer(id, function(input, output, session) {
    blurbs <- fromJSON("./www/info_blurbs.json")
    current_selection <- reactiveVal(character())
    
    processed_data <- reactive({
      process_volcano_data(shared_reactives$dds_processed(), input$slider_padj, input$slider_log2)
    })
    
    plot_data <- reactive({
      plot_volcano(processed_data()$res, processed_data()$dds, input$selected_gene)
    })
    
    output$volcano_plot <- renderPlotly({ plot_data() })
    
    observeEvent(input$info_volcano_plot, {
      shinyalert(
        title = blurbs$info$volcano$title, 
        html = TRUE,
        text = blurbs$info$volcano$text
      )
    })
    
    observe({
      runjs("Shiny.setInputValue('plotly_selected-A', null);
            Shiny.setInputValue('plotly_click-A', null);")
      new_selection <- event_data("plotly_selected")$customdata
      new_click <- event_data("plotly_click")$customdata
      current_genes <- current_selection()
      if (!is.null(new_selection)) {
        current_genes <- unique(c(current_genes, new_selection))
      }
      if (!is.null(new_click)) {
        current_genes <- unique(c(current_genes, new_click))
      }
      current_selection(current_genes)
      updateSelectizeInput(session, "selected_gene", choices = rownames(shared_reactives$filtered_res()), server = TRUE, selected = current_genes)
    })
    
    observeEvent(input$reset_selection, {
      current_selection(character())  
      runjs("Shiny.setInputValue('plotly_selected-A', null);
            Shiny.setInputValue('plotly_click-A', null);")
    })
    
    setup_download_handler(id, output, "volcano_data", reactive({ processed_data()$res }), "volcano")
  })
}



