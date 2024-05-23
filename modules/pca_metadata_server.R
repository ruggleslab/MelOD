#' PCA Metadata Server
#'
#' @description Sets up the server logic for the PCA analysis and related plots
#' @param id Module ID
pca_metadata_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    blurbs <- fromJSON("./www/info_blurbs.json")
    selected_dds <- reactive({ global_selected_dds() })
    utilities <- reactive({ shared_server_utilities(selected_dds()) })
    filtered_res <- reactive({ utilities()$filtered_genes })
    dds_processed <- reactive({ utilities()$dds })
    
    #' Generate and Render PCA Data
    #' 
    #' @description Generates the PCA data and renders the PCA and variance explained plots
    pca_data_reactive <- reactive({
      dds <- selected_dds()
      pca_data(dds)
    })
    
    #' Render PCA and Variance Plots
    #' 
    #' @description Renders the PCA and variance explained plots
    #' @param output Shiny output object
    #' @param pca_data_reactive Reactive expression containing the PCA data
    render_plots <- function(output, pca_data_reactive) {
      output$pca_plot <- renderPlotly({
        req(pca_data_reactive())
        pca_data <- pca_data_reactive()$pca_data
        creation_pca(pca_data, size_by = input$size_by, color_by = input$color_by)
      })
      
      output$variance_plot <- renderPlotly({
        req(pca_data_reactive())
        vsdata <- pca_data_reactive()$vsdata
        variance_explained_plot(vsdata)
      })
    }
    
    render_plots(output, pca_data_reactive)
    
    setup_download_handler(output, "pca_data", reactive({ pca_data_reactive()$pca_data }), "pca")
    
   
    # Repeat for other plots
    observeEvent(input$info_pca_plot, {
      shinyalert(title = blurbs$info$pca$title, html = TRUE,
                 text = blurbs$info$pca$text)
    })
    
    
    output$mortality_by_condition <- renderUI({
      result <- plot_mortality_curve_by_condition(global_selected_clinical_data())
      if (is.character(result)) {
        div(class = "error-message", result)
      } else {
        result
      }
    })
    
    output$mortality_by_gene <- renderUI({
      result <- plot_mortality_curve_by_gene(global_selected_clinical_data(), dds_processed(), input$gene_mortality)
      if (is.character(result)) {
        div(class = "error-message", result)
      } else {
        result
      }
    })
    
    observe({
      filtered_res <- filtered_res()
      updateSelectizeInput(session, "gene_mortality", choices = rownames(filtered_res), server = TRUE, selected = NULL)
    })
    
    setup_download_handler(output, "metadata_data", reactive({ global_selected_clinical_data()}), "metadata")
    
    observeEvent(input$info_metadata_plot, {
      shinyalert(title = blurbs$info$pca$title, html = TRUE,
                 text = blurbs$info$pca$text)
    })
    
  })
}
