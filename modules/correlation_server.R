#' Correlation Server
#'
#' @description Sets up the server logic for the correlation analysis
#' @param dds DESeq2 dataset
#' @param id Module ID
correlation_server <- function(dds, id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    #' Initialize Reactives
    #' 
    #' @description Initializes the reactive expressions for the module
    selected_dds <- reactive({ global_selected_dds() })
    utilities <- reactive({ shared_server_utilities(selected_dds()) })
    filtered_res <- reactive({ utilities()$filtered_genes })
    dds_processed <- reactive({ utilities()$dds })
    display_genes <- reactive({ get_display_genes(filtered_res(), input$selected_gene) })
    selected_genes_plotly <- reactiveVal(character(0))
    
    update_correlation_genes_choices(session, filtered_res)
    
    #' Reactive expression for correlation data
    #' 
    #' @description Generates the correlation data from the selected DESeq2 dataset
    #' @return Correlation data
    correlation_data_reactive <- reactive({
      req(input$gene_of_interest)
      req(input$correlation_threshold)
      req(dds_processed())
      analyze_gene_correlations(dds_processed(), gene_of_interest = input$gene_of_interest, threshold = input$correlation_threshold)
    })
    
    #' Render Correlation Plot
    #' 
    #' @description Renders the correlation plot
    output$correlation_plot <- renderPlotly({
      req(correlation_data_reactive())
      correlation_data_reactive()
    })
    
    #' Event Observers
    #' 
    #' @description Sets up observers for correlation plot interactions
    observeEvent(input$info_correlation_plot, {
      shinyalert(title = "Correlation Plot Information", html = TRUE,
                 text = 'The correlation plot displays the correlation coefficients and their significance for each gene relative to the gene of interest.')
    })
    
  })
}
