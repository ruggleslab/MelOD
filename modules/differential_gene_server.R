differential_gene_server <- function(dds, clinical_data, id) {
  moduleServer(id, function(input, output, session) {
    #' Initialize Reactives
    #' 
    #' @description Initializes the reactive expressions for the module
    selected_dds <- reactive({ global_selected_dds() })
    utilities <- reactive({ shared_server_utilities(selected_dds()) })
    filtered_res <- reactive({ utilities()$filtered_genes })
    dds_processed <- reactive({ utilities()$dds })
    display_genes <- reactive({ get_display_genes(filtered_res(), input$selected_gene) })
    selected_genes_plotly <- reactiveVal(character(0))
    
    #' Plot Data with Reset Functionality
    #' 
    #' @description Generates the plot data and resets selected genes
    #' @return A list containing the violin and volcano plots
    plot_data <- eventReactive(c(input$update_plot,input$reset_selection ),{
      generate_plot_data(dds_processed(), display_genes(), input$slider_padj, input$slider_log2)
    })
    
    #' Render Plots
    #' 
    #' @description Renders the violin and volcano plots
    render_plots(output, plot_data)
    
    #' Download Handlers
    #' 
    #' @description Sets up the download handlers for exporting data
    download_handlers(output, display_genes)
    
    #' Event Observers
    #' 
    #' @description Sets up observers for plot interactions and gene selection
    event_observers(input, session, display_genes, filtered_res, selected_genes_plotly)
    
    #' Filtered Results Table
    #' 
    #' @description Renders the filtered results table
    output$filtered_results <- render_filtered_results_table(dds_processed, input, selected_genes_plotly)
  })
}

