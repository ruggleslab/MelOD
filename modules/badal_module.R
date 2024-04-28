source("global.R", local = TRUE)

badal_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    fluidRow(
      box(title="Inputs", status="warning",
          collapsible = TRUE,
          solidHeader = TRUE, 
          sliderInput(ns("slider_padj"), "padj Cutoff", 0, 1, 0.05),
          sliderInput(ns("slider_log2"), "log2foldchange Cutoff", -2, 2, c(-1, 1)),
          selectizeInput(ns("selected_gene"), "Gene(s) selection",
                      choices = NULL,  # Ensure this is accessible here or move to server
                      selected = NULL,  # Default selection
                      multiple = TRUE)
      ),
      tabBox(
        title = "Metadata (To DO)",
        # The id lets us use input$tabset1 on the server to find the current tab
        id = "tabset1", height = "250px",
        tabPanel("Patient", "First tab content"),
        tabPanel("Mortality", "Tab content 2")
      )
    ),
    fluidRow(
      box(
        title = "Boxplot",  # Title of the box
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
      )),
      box(
        title = "Heatmap",  # Title of the box
        status = "primary",
        width = 12,# Color theme
        solidHeader = TRUE,      # Gives the box a solid header
        collapsible = TRUE,      # Allows the box to be collapsed
        plotlyOutput(ns("badal_heatmap_test"))  # Placeholder for the volcano plot
      )
    
  )
}



badal_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Load DESeq2 results and CPM values
    dds <- readRDS(file.path("./data/badal", "Badal_Deseq2.rds"))
    summary(dds)
    colData(dds)
    res = results(dds)
    # Define reactive expression for filtered data
    
    
    
    filtered_data <- reactive({
      padj_threshold <- input$slider_padj
      log2_thresholds <- input$slider_log2
      
      if (is.numeric(padj_threshold) && !is.na(padj_threshold)) {
        # Filter genes based on significance and log2fold change and sort by p-value
        filtered_genes <- res[
          !is.na(res$padj) & res$padj < padj_threshold &
            res$log2FoldChange >= log2_thresholds[1] & res$log2FoldChange <= log2_thresholds[2],
        ]
        return(filtered_genes[order(filtered_genes$padj), ])
      }
      NULL  # Return NULL if conditions aren't met
    })
    
  
    observe({ gene_data <- filtered_data()
    gene_choices <- rownames(gene_data)
    updateSelectizeInput(session, "selected_gene", choices = gene_choices,server = TRUE)
    })
   
    
    
    display_genes <- reactive({
      # Get all filtered genes from the reactive
      all_genes <- filtered_data()
      
      # Check if specific genes are selected
      selected_genes <- input$selected_gene
      if (!is.null(selected_genes) && all(selected_genes %in% rownames(all_genes))) {
        # Return only the selected genes
        return(all_genes[rownames(all_genes) %in% selected_genes, , drop = FALSE])
        print(all_genes[rownames(all_genes) %in% selected_genes, , drop = FALSE])
      } else {
        # Return top 5 genes as default
        return(head(all_genes, 10))
      }
    })
    
    
  
    
#############################################
 
    
    # Creating a Plotly boxplot reactively
    output$badal_test <- renderPlotly({
      req(filtered_data())  # Ensure that the data is available
      
      
     
      filtered_genes <- display_genes()

      # Determine the plot title based on the selection
      if (!is.null(input$selected_gene) && all(input$selected_gene %in% rownames(filtered_genes))) {
        if (length(input$selected_gene) == 1) {
          plot_title <- paste("Expression for", input$selected_gene)  # Title for single selected gene
        } else {
          plot_title <- paste("Expression for selected genes")  # Title for multiple selected genes
        }
      } else {
        plot_title <- "Top 10 most significant genes"  # Default title
      }
      
      if (nrow(filtered_genes) > 0) {
        # Extract and prepare data for plotting
        counts <- counts(dds, normalized = TRUE)
        counts_filtered <- counts[rownames(counts) %in% rownames(filtered_genes), ]
        df <- as.data.frame(counts_filtered)
        log_norm_counts_filtered <- log(df + 0.01)
        df <- as.data.frame(log_norm_counts_filtered)
        col_data_df <- as.data.frame(colData(dds))

        if (ncol(df) > 1) {
          df_long <- df %>%
            tibble::rownames_to_column("gene_id") %>%
            tidyr::pivot_longer(
              cols = starts_with("JC"),
              names_to = "Sample",
              values_to = "expression"
              
            )
          
        } else {
          # Handle the case with only one column (the gene itself)
          # Assume df has only one column of actual data besides the rownames
          gene_id <- input$selected_gene
          sample_names <- rownames(df)
          expression_values <- as.vector(df[[1]])
          df_long <- data.frame(gene_id = rep(gene_id, length(expression_values)),
                                Sample = sample_names,
                                expression = expression_values,
                                stringsAsFactors = FALSE)
 
        }
        col_data_df <- col_data_df %>%
          rownames_to_column(var = "patient_id")
   
        merged_data <- dplyr::left_join(df_long, col_data_df, by = c("Sample" = "patient_id"))
        
        # Generate the plot with dynamic title
        plot_ly(data = merged_data, x = ~condition, y = ~expression, type = 'box',
                color = ~condition, boxpoints = 'outliers',
                jitter = 0.3, pointpos = 0,
                text = ~paste("Gene ID:", gene_id)) %>%
          layout(title = plot_title,
                 yaxis = list(title = "Log CPM"),
                 xaxis = list(title = "Comparison"))
      }
    })

    
    
  
   
    output$volcano_plot <- renderPlotly({
      create_volcanoplot(res = res, input= input, gene = input$selected_gene)
    })
    
    output$badal_heatmap_test <- renderPlotly({
      create_heatmap(dds = dds, input= input, gene = input$selected_gene)
    })
    

  })
}
