pca_metadata_server <- function(dds, clinical_data, id) {
  moduleServer(id, function(input, output, session) {
    #' Reactive expression for PCA data
    #' 
    #' @description Generates the PCA data from the selected DESeq2 dataset
    #' @return PCA data
    pca_data_reactive <- reactive({
      generate_pca_data()
    })
    
    #' Render PCA Plots
    #' 
    #' @description Renders the PCA plot
    render_pca_plots(output, pca_data_reactive)
    
    #' Download PCA Data
    #' 
    #' @description Sets up the download handler for PCA data
    download_pca_data(output, pca_data_reactive)
    
    #' Event Observers
    #' 
    #' @description Sets up observers for PCA plot interactions
    event_observers_pca(input)
  
    #' Render Mortality and Gender Plots
    output$gender <- renderPlotly({
      colnames(global_selected_clinical_data()) <- as.character(unlist(global_selected_clinical_data()[2,]))
      clinical_data <- global_selected_clinical_data()[-c(1, 2), ]
      plot_gender(clinical_data)
    })
    output$mortality <- renderPlotly({
      plot_mortality_curve(global_selected_clinical_data())
    })
  
  })
}
