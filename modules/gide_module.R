library(dplyr)
library(tidyverse)
library(plotly)
library(shinyWidgets)
library(heatmaply)

gide_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    fluidRow(
      box(title="Inputs", status="warning",
          collapsible = TRUE,
          solidHeader = TRUE, 
          sliderInput(ns("slider"), "Slider input:", 0, 1, 0.05),
          pickerInput(ns("gene"), "Select gene:", choices = NULL, multiple = TRUE, options = list(`live-search` = TRUE, size = 5)),
          pickerInput(ns("Comparison"), "Select comparison:", choices = "NR vs R mono PRE", multiple = TRUE, options = list("TO DO"))
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
        title = "Boxplot R vs NR mono PRE",  # Title of the box
        status = "primary",            # Color theme
        solidHeader = TRUE,            # Gives the box a solid header
        collapsible = TRUE,            # Allows the box to be collapsed
        plotlyOutput(ns("gide_nr_vs_r_boxplot"))   # Placeholder for the plot
      ),
      box(
        title = "Heatmap R vs NR mono PRE",  # Title of the box
        status = "primary",            # Color theme
        solidHeader = TRUE,            # Gives the box a solid header
        collapsible = TRUE,            # Allows the box to be collapsed
        plotlyOutput(ns("gide_nr_vs_r_heatmap"))  # Placeholder for the plot
      )
    )
  )
}




gide_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Load DESeq2 results and CPM values
    deseq2_results <- read.csv(file.path("./data/gide", "DESeq2_resPreMono.csv"), row.names = 1)
    cpm_values <- read.csv(file.path("./data/gide", "Gide_cpm.csv"), row.names = 1)
    patient_data <- read.csv(file.path("./data/gide", "samples.txt"), sep = "\t", stringsAsFactors = FALSE)
    
    
    # Define reactive expression for filtered data
    filtered_data <- reactive({
      padj_threshold <- input$slider  # Ensure this input is defined in the UI as a slider for padjust


      if (is.numeric(padj_threshold) && !is.na(padj_threshold)) {
        filtered_genes <- deseq2_results %>%
          dplyr::filter(padj < padj_threshold) %>%
          dplyr::pull(ensembl_id)  # Extract the Ensembl IDs
        # Log to console to debug
        print(paste("Number of filtered genes:", length(filtered_genes)))
        
        if (length(filtered_genes) > 0) {
          filtered_cpm_values <- cpm_values[rownames(cpm_values) %in% filtered_genes, ]
          
          selected_columns <- grep("_PD1_PRE", names(cpm_values), value = TRUE)
          
          # Subset the cpm_values data frame to include only the selected columns
          cpm_values_pre <- filtered_cpm_values[, selected_columns]
          
          # Reshape the CPM data from wide to long format
          cpm_long <- cpm_values_pre %>%
            tibble::rownames_to_column("gene_id") %>%
            tidyr::pivot_longer(
              cols = starts_with("Patient_"),
              names_to = "patient_id",
              values_to = "expression"
            )
          
          # Join with patient data
          merged_data <- dplyr::left_join(cpm_long, patient_data, by = c("patient_id" = "X"))
          gene_choices <- unique(merged_data$gene_id)
          updateSelectInput(session, ns("gene"), choices = gene_choices)
          return(merged_data)
        }
      }
      NULL  # Return NULL if conditions aren't met
    })
    

    # Creating a Plotly boxplot reactively
    output$gide_nr_vs_r_boxplot <- renderPlotly({
      req(filtered_data())  # Ensure that the data is available
      plot_ly(data = filtered_data(), x = ~condition, y = ~log(expression + 0.01), type = 'box',
              color = ~condition, boxpoints = 'all',
              jitter = 0.3, pointpos = 0,  # Points directly over the box
              text = ~paste("Gene ID:", gene_id)) %>%
        layout(title = "Log CPM by Patient Response",
               yaxis = list(title = "Log CPM"),
               xaxis = list(title = "Response"))
    })
    
    
    # output$badal_heatmap_test <- renderPlotly({
    #   req(filtered_data())  # Ensure that the data is available
    # 
    #   # Create heatmap
    #   heatmaply(filtered_data(),
    #             xlab = "Patient",
    #             ylab = "Gene ID",
    #             # labels_col = colData(dds),
    #             colors = colorRampPalette(viridis::viridis(10))(255),
    #             showticklabels = c(TRUE, FALSE))
    # })
  })
}
