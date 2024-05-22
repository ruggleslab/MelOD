#' Correlation Server
#'
#' @description Sets up the server logic for the correlation analysis
#' @param dds DESeq2 dataset
#' @param id Module ID
correlation_server <- function(id) {
  moduleServer(id, function(input, output, session) {
  
    
    #' Initialize Reactives
    #' 
    #' @description Initializes the reactive expressions for the module
    selected_dds <- reactive({ global_selected_dds() })
    utilities <- reactive({ shared_server_utilities(selected_dds()) })
    filtered_res <- reactive({ utilities()$filtered_genes })
    dds_processed <- reactive({ utilities()$dds })
    display_genes <- reactive({ get_display_genes(filtered_res(), input$selected_gene) })
    
    update_correlation_genes_choices(session, filtered_res)
    
    #' Generate and Plot Data with Reset Functionality
    #' 
    #' @description Generates the plot data and resets selected genes, then renders plots
    plot_data <- eventReactive(c(input$update_plot, input$reset_selection), {
      list(
        correlation = analyze_gene_correlations(dds_processed(), display_genes=display_genes(), gene_of_interest = input$gene_of_interest, threshold = input$correlation_threshold)
      )
    })
    
    #' Render Plots
    #' 
    #' @description Renders the correlation plot
    #' @param output Shiny output object
    #' @param plot_data Reactive expression containing the plot data
    render_plots <- function(output, plot_data) {
      output$correlation_plot <- renderPlotly({ plot_data()$correlation })
    }
    
    render_plots(output, plot_data)
    
    #' Event Observers
    #' 
    #' @description Sets up observers for correlation plot interactions
    observeEvent(input$info_correlation_plot, {
      shinyalert(title = "Correlation Plot Information", html = TRUE,
                 text = 'The correlation plot displays the correlation coefficients and their significance for each gene relative to the gene of interest.')
    })
  })
}
