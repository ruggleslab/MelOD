library(dplyr)
library(tidyverse)
library(plotly)
library(shinyWidgets)
library(heatmaply)

badal_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    fluidRow(
      box(title="Inputs", status="warning",
          collapsible = TRUE,
          solidHeader = TRUE, 
          sliderInput(ns("slider"), "Slider input:", 0, 1, 0.05),
          pickerInput(ns("gene"), "Select gene: (Not Implemented Yed)", choices = NULL, multiple = TRUE, options = list(`live-search` = TRUE, size = 5)),
          pickerInput(ns("Comparison"), "Select comparison: (Not Implemented Yed)", choices = "Navy vs Cancer", multiple = TRUE, options = list("TO DO")),
          selectInput("selectedGenes", "Select genes to highlight: (Not Implemented Yed)",
                      choices = rownames(res),  # Ensure this is accessible here or move to server
                      selected = rownames(res)[1],  # Default selection
                      multiple = TRUE,
                      selectize = TRUE)
      ),
      tabBox(
        title = "Metadata",
        # The id lets us use input$tabset1 on the server to find the current tab
        id = "tabset1", height = "250px",
        tabPanel("Patient", "First tab content"),
        tabPanel("Mortality", "Tab content 2")
      )
    ),
    fluidRow(
      box(
        title = "Boxplot R vs",  # Title of the box
        status = "primary",            # Color theme
        solidHeader = TRUE,            # Gives the box a solid header
        collapsible = TRUE,            # Allows the box to be collapsed
        plotlyOutput(ns("badal_test"))   # Placeholder for the plot
      ),
      box(
        title = "Volcano Plot",  # Title of the box
        status = "primary",            # Color theme
        solidHeader = TRUE,            # Gives the box a solid header
        collapsible = TRUE,            # Allows the box to be collapsed
        plotlyOutput(ns("volcano_plot"))  # Placeholder for the plot
      ),
      box(
        title = "Heatmap",  # Title of the box
        status = "primary",      # Color theme
        solidHeader = TRUE,      # Gives the box a solid header
        collapsible = TRUE,      # Allows the box to be collapsed
        plotlyOutput(ns("badal_heatmap_test"))  # Placeholder for the volcano plot
      )
    )
  )
}












badal_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Load DESeq2 results and CPM values
    dds <- readRDS(file.path("./data/badal", "dds.Rds"))
    res = results(dds)

   
  
    
    # Define reactive expression for filtered data
    filtered_data <- reactive({
      padj_threshold <- input$slider  # Ensure this input is defined in the UI as a slider for padjust
      
      
      if (is.numeric(padj_threshold) && !is.na(padj_threshold)) {
        
        filtered_genes <- res[res$padj < padj_threshold,]
      
        
          return(df)
        
      }
      NULL  # Return NULL if conditions aren't met
    })
    
    
    

    # Creating a Plotly boxplot reactively
    output$badal_test <- renderPlotly({
      req(filtered_data())  # Ensure that the data is available
      
      
      filtered_genes <- filtered_data()
      
      if (length(filtered_genes) > 0) {
        
        
        # First, extract normalized counts for all genes
        counts <- counts(dds, normalized=FALSE)
        
        # Subset counts to include only filtered genes
        
        counts_filtered <- norm_counts[rownames(norm_counts) %in% rownames(filtered_genes),]
        df <- as.data.frame(counts_filtered)
      }
        
      # Proceed with log transformation
      log_norm_counts_filtered <- log(df + 0.01)
      
      # Create a data frame for plotting
      df <- as.data.frame(log_norm_counts_filtered)
      
      col_data_df <- as.data.frame(colData(dds))
      
      df_long <- df %>%
        tibble::rownames_to_column("gene_id") %>%
        tidyr::pivot_longer(
          cols = starts_with("Patient_"),
          names_to = "patient_id",
          values_to = "expression"
        )
      
      col_data_df <- col_data_df %>% 
        rownames_to_column(var = "patient_id")
      
      merged_data <- dplyr::left_join(df_long, col_data_df, by = c("patient_id" = "patient_id"))
      
      

      plot_ly(data = merged_data, x = ~responder, y = ~expression, type = 'box',
              color = ~responder, boxpoints = 'all',
              jitter = 0.3, pointpos = 0,  # Points directly over the box
              text = ~paste("Gene ID:", gene_id)) %>%
        layout(title = "Log CPM by Patient Response",
               yaxis = list(title = "Log CPM"),
               xaxis = list(title = "Response"))
    })
    
    
    # Generate a volcano plot
    output$volcano_plot <- renderPlotly({
      # Assuming 'res' is your dataset containing the results
      res$neg_log10_padj <- -log10(res$padj)  # Transform p-values for plotting
      threshold <- input$slider  # Get the threshold from user input
      res$sig <- ifelse(res$padj < threshold & abs(res$log2FoldChange) > 1, "Significant", "Not Significant")
      
      # Create the plot
      plot_ly(data = as.data.frame(res), x = ~log2FoldChange, y = ~neg_log10_padj, type = 'scatter', mode = 'markers',
              color = ~sig, colors = c("#E2D4B7", "#AB82C5"),
              text = ~paste("Gene:", rownames(res), "<br>log2 Fold Change:", log2FoldChange, "<br>Adjusted p-value:", padj),
              marker = list(size = 10)) %>%
        layout(title = "Volcano Plot of DESeq2 Results",
               xaxis = list(title = "Log2 Fold Change"),
               yaxis = list(title = "-log10 Adjusted p-value"))
    })

  })
}
