differential_gene_server <- function(id) {
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
    new_genes <- reactiveVal(NULL)
    #' Generate and Plot Data with Reset Functionality
    #' 
    #' @description Generates the plot data and resets selected genes, then renders plots
    plot_data <- eventReactive(c(input$update_plot, input$reset_selection), {
      list(
        violin = create_boxplot(dds = dds_processed(), display_genes = display_genes(), padj_cut = input$slider_padj, log2_cut = input$slider_log2),
        volcano = create_volcanoplot(dds = dds_processed(), display_genes = display_genes(), padj_cut = input$slider_padj, log2_cut = input$slider_log2)
      )
    })
    
    #' Render Plots
    #' 
    #' @description Renders the violin and volcano plots
    #' @param output Shiny output object
    #' @param plot_data Reactive expression containing the plot data
    render_plots <- function(output, plot_data) {
      output$violin_plot <- renderPlotly({ plot_data()$violin })
      output$volcano_plot <- renderPlotly({ plot_data()$volcano })
    }
    
    render_plots(output, plot_data)
    
    #' Download Handlers
    #' 
    #' @description Sets up the download handlers for exporting data
    download_handlers(output,name="Deseq2", display_genes)
    
    #' Event Observers
    #' 
    #' @description Sets up observers for plot interactions and gene selection
    #' 
    event_observers(input, session, display_genes, filtered_res, selected_genes_plotly, new_genes)
    
    #' Filtered Results Table
    #' 
    #' @description Renders the filtered results table
    output$filtered_results <- render_filtered_results_table(dds_processed, input)
    
   
    
  })
}

