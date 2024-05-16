heatmap_server <- function(dds, clinical_data, id) {
  moduleServer(id, function(input, output, session) {
    #' Initialize Reactives
    #' 
    #' @description Initializes the reactive expressions for the module
    selected_dds <- reactive({ global_selected_dds() })
    utilities <- reactive({ shared_server_utilities(selected_dds()) })
    filtered_res <- reactive({ utilities()$filtered_genes })
    dds_processed <- reactive({ utilities()$dds })
    
    #' Plot Data with Reset Functionality
    #' 
    #' @description Generates the heatmap plot data and resets selected genes
    #' @return A list containing the heatmap plot
    plot_data <- eventReactive(c(input$update_plot, input$reset_selection), {
      generate_heatmap_plot_data(dds_processed(), isolate(input$slider_padj), isolate(input$slider_log2), isolate(input$number), isolate(input$selected_gene))
    })
    
    #' Render Plots
    #' 
    #' @description Renders the heatmap plot
    render_heatmap_plots(output, plot_data)
    
    #' Event Observers
    #' 
    #' @description Sets up observers for plot interactions
    event_observers_heatmap(input)
  })
}


