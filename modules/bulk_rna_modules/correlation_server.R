correlation_server <- function(id, shared_reactives) {
#' Correlation Server
#'
#' @description Sets up the server logic for the correlation analysis and related plots.
#' @param id Module ID
#' @param shared_reactives A reactiveValues object for sharing reactive variables across modules.
  moduleServer(id, function(input, output, session) {
    
    blurbs <- fromJSON("./www/info_blurbs.json")
    
    gene_of_interest <- debounce(reactive(input$cor_gene_of_interest), 500)
    correlation_threshold <- debounce(reactive(input$correlation_threshold), 500)
    
    
    
    observe({
      updateSelectizeInput(session, "cor_gene_of_interest", choices = rownames(shared_reactives$filtered_res()), server = TRUE)
    })

    processed_data <- reactive({
      process_gene_correlations(
        shared_reactives$dds_processed(), 
        shared_reactives$display_genes(), 
        gene_of_interest(), 
        correlation_threshold()
      )
    })
    
    plot_data <- reactive({
      plot_gene_correlations(processed_data(), gene_of_interest())
    })
    
    output$correlation_plot <- renderUI({
      result <- plot_data()
      if (is.character(result)) {
        div(class = "error-message", result)
      } else {
        result
      }
    })
    
    setup_download_handler(id, output, "correlation_data", processed_data, "correlation")
    
    observeEvent(input$info_correlation_plot, {
      shinyalert(
        title = blurbs$info$correlation$title, 
        html = TRUE,
        text = blurbs$info$correlation$text
      )
    })
  })
}
