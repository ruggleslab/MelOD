pca_metadata_server <- function(id, shared_reactives) {
  #' PCA Metadata Server
  #'
  #' @description Sets up the server logic for the PCA analysis and related plots.
  #' @param id Module ID
  #' @param shared_reactives  A reactiveValues object for sharing reactive variables across modules.
  moduleServer(id, function(input, output, session) {
    blurbs <- fromJSON("./www/info_blurbs.json")
  
    
    pca_data_reactive <- reactive({
      req(shared_reactives$selected_dds())
      process_pca_data(shared_reactives$selected_dds())
    })
    
    render_plots <- function(output, pca_data_reactive) {
      output$pca_plot <- renderPlotly({
        req(pca_data_reactive())
        plot_pca(pca_data_reactive()$pca_data, size_by = input$size_by, color_by = input$color_by)
      })
      
      output$variance_plot <- renderPlotly({
        req(pca_data_reactive())
        plot_variance(pca_data_reactive()$vsdata)
      })
    }
    
    render_plots(output, pca_data_reactive)
    
    setup_download_handler(id, output, "pca_data", reactive({ pca_data_reactive()$pca_data }), "pca")
    
    observeEvent(input$info_pca_plot, {
      shinyalert(title = blurbs$info$pca$title, html = TRUE, text = blurbs$info$pca$text)
    })
    
    plot_mortality_curve_generic <- function(clinical_data, group_by, deseq2_data = NULL, gene = NULL) {
      tryCatch({
        process_clinical_data(clinical_data, group_by = group_by, deseq2_data = deseq2_data, gene = gene)
      }, error = function(e) {
        print(e)
        return("No metadata available")
      })
    }
    
    output$mortality_by_condition <- renderUI({
      result <- plot_mortality_curve_generic(shared_reactives$selected_clinical_data(), "condition")
      if (is.character(result)) {
        div(class = "error-message", result)
      } else {
        plot_mortality_curve(result, group_col = "group")
      }
    })
    
    output$mortality_by_gene <- renderUI({
      result <- plot_mortality_curve_generic(shared_reactives$selected_clinical_data(), "group", shared_reactives$dds_processed(), input$gene_mortality)
      if (is.character(result)) {
        div(class = "error-message", result)
      } else {
        plot_mortality_curve(result, group_col = "group")
      }
    })
    
    output$risk_table <- renderUI({
      result <- plot_mortality_curve_generic(shared_reactives$selected_clinical_data(), "condition")
      if (is.character(result)) {
        div(class = "error-message", result)
      } else {
        plot_mortality_curve(result, group_col = "risk_table")
      }
    })
    
    observe({
      updateSelectizeInput(session, "gene_mortality", choices = rownames(shared_reactives$filtered_res()), server = TRUE, selected = NULL)
    })
    
    setup_download_handler(
      id = id,
      output = output,
      name = "metadata_data",
      data_reactive = list(
        mortality_by_condition.csv = reactive({ plot_mortality_curve_generic(shared_reactives$selected_clinical_data(), "condition") }),
        mortality_by_gene.csv = reactive({ plot_mortality_curve_generic(shared_reactives$selected_clinical_data(), "group", shared_reactives$dds_processed(), input$gene_mortality) })
      ),
      filename_prefix = "mortality"
    )
    observeEvent(input$info_metadata_plot, {
      shinyalert(title = blurbs$info$mortality_curve$title, html = TRUE, text = blurbs$info$mortality_curve$text)
    })
    
  })
}
