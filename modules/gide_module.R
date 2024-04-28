# library(dplyr)
# library(tidyverse)
# library(plotly)
# library(shinyWidgets)
# library(heatmaply)
# 
# gide_ui <- function(id) {
#   ns <- NS(id)
#   fluidPage(
#     fluidRow(
#       box(title="Inputs", status="warning",
#           collapsible = TRUE,
#           solidHeader = TRUE, 
#           sliderInput(ns("slider"), "Slider input:", 0, 1, 0.05),
#           pickerInput(ns("gene"), "Select gene:", choices = NULL, multiple = TRUE, options = list(`live-search` = TRUE, size = 5)),
#           pickerInput(ns("Comparison"), "Select comparison:", choices = "NR vs R mono PRE", multiple = TRUE, options = list("TO DO"))
#       ),
#       tabBox(
#         title = "Metadata",
#         # The id lets us use input$tabset1 on the server to find the current tab
#         id = "tabset1", height = "250px",
#         tabPanel("Patient", "First tab content"),
#         tabPanel("Mortality", "Tab content 2")
#       )
#     ),
#     fluidRow(
#       box(
#         title = "Boxplot R vs NR mono PRE",  # Title of the box
#         status = "primary",            # Color theme
#         solidHeader = TRUE,            # Gives the box a solid header
#         collapsible = TRUE,            # Allows the box to be collapsed
#         plotlyOutput(ns("gide_nr_vs_r_boxplot"))   # Placeholder for the plot
#       ),
#       box(
#         title = "Heatmap R vs NR mono PRE",  # Title of the box
#         status = "primary",            # Color theme
#         solidHeader = TRUE,            # Gives the box a solid header
#         collapsible = TRUE,            # Allows the box to be collapsed
#         plotlyOutput(ns("gide_nr_vs_r_heatmap"))  # Placeholder for the plot
#       )
#     )
#   )
# }
# 
# 
# 
# 
# gide_server <- function(id) {
#   moduleServer(id, function(input, output, session) {
#     ns <- session$ns
#     
#     # Load DESeq2 results and CPM values
#     deseq2_results <- read.csv(file.path("./data/gide", "DESeq2_resPreMono.csv"), row.names = 1)
#     cpm_values <- read.csv(file.path("./data/gide", "Gide_cpm.csv"), row.names = 1)
#     patient_data <- read.csv(file.path("./data/gide", "samples.txt"), sep = "\t", stringsAsFactors = FALSE)
#     
#     
#     # Define reactive expression for filtered data
#     filtered_data <- reactive({
#       padj_threshold <- input$slider  # Ensure this input is defined in the UI as a slider for padjust
# 
# 
#       if (is.numeric(padj_threshold) && !is.na(padj_threshold)) {
#         filtered_genes <- deseq2_results %>%
#           dplyr::filter(padj < padj_threshold) %>%
#           dplyr::pull(ensembl_id)  # Extract the Ensembl IDs
#         # Log to console to debug
#         print(paste("Number of filtered genes:", length(filtered_genes)))
#         
#         if (length(filtered_genes) > 0) {
#           filtered_cpm_values <- cpm_values[rownames(cpm_values) %in% filtered_genes, ]
#           
#           selected_columns <- grep("_PD1_PRE", names(cpm_values), value = TRUE)
#           
#           # Subset the cpm_values data frame to include only the selected columns
#           cpm_values_pre <- filtered_cpm_values[, selected_columns]
#           
#           # Reshape the CPM data from wide to long format
#           cpm_long <- cpm_values_pre %>%
#             tibble::rownames_to_column("gene_id") %>%
#             tidyr::pivot_longer(
#               cols = starts_with("Patient_"),
#               names_to = "patient_id",
#               values_to = "expression"
#             )
#           
#           # Join with patient data
#           merged_data <- dplyr::left_join(cpm_long, patient_data, by = c("patient_id" = "X"))
#           gene_choices <- unique(merged_data$gene_id)
#           updateSelectInput(session, ns("gene"), choices = gene_choices)
#           return(merged_data)
#         }
#       }
#       NULL  # Return NULL if conditions aren't met
#     })
#     
# 
#     # Creating a Plotly boxplot reactively
#     output$gide_nr_vs_r_boxplot <- renderPlotly({
#       req(filtered_data())  # Ensure that the data is available
#       plot_ly(data = filtered_data(), x = ~condition, y = ~log(expression + 0.01), type = 'box',
#               color = ~condition, boxpoints = 'all',
#               jitter = 0.3, pointpos = 0,  # Points directly over the box
#               text = ~paste("Gene ID:", gene_id)) %>%
#         layout(title = "Log CPM by Patient Response",
#                yaxis = list(title = "Log CPM"),
#                xaxis = list(title = "Response"))
#     })
#     
#     
#     # output$badal_heatmap_test <- renderPlotly({
#     #   req(filtered_data())  # Ensure that the data is available
#     # 
#     #   # Create heatmap
#     #   heatmaply(filtered_data(),
#     #             xlab = "Patient",
#     #             ylab = "Gene ID",
#     #             # labels_col = colData(dds),
#     #             colors = colorRampPalette(viridis::viridis(10))(255),
#     #             showticklabels = c(TRUE, FALSE))
#     # })
#   })
# }

source("global.R", local = TRUE)

# Load DESeq2 results and CPM values
dds <- readRDS(file.path("./data/badal", "Badal_Deseq2.rds"))
res = results(dds)




gide_ui <- function(id) {
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












gide_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    
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
