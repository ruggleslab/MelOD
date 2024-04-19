bulkrna_ui <- function(id){
  sidebarLayout(
    sidebarPanel(
      textAreaInput(NS(id, "gene_search2"), "Enter Genes (one per line):", rows = 5),
      # Output to display the gene names
    ),
    mainPanel(
      plotOutput(NS(id, "plot1")),
      plotOutput(NS(id, "plot2")),
    )
  )
}


bulkrna_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    data_folder <- "./data/"
    shared.plt.mtx.pace <- read.csv(paste0(data_folder, "24-04-19-shared_genes_plt.csv"),row.names = 1)
    output$plot1 <- renderPlot({
      genes_of_interest <- c("AARS", "AAGAB", "ABALON")
            # Subset the gene expression data for the selected genes
      selected_genes <- shared.plt.mtx.pace[match(genes_of_interest, rownames(shared.plt.mtx.pace)), ]
      
      # Convert the data to a format suitable for boxplot()
      selected_genes <- as.data.frame(t(selected_genes))
      

      # Create box plots for the selected genes
      boxplot(selected_genes,
              main = "Boxplot of Gene Expression",
              xlab = "Patient IDs",
              ylab = "Expression Level",
              col = "skyblue",      # Color of the boxes
              border = "black",     # Border color of the boxes
              notch = FALSE,        # Whether to draw a notch
              outline = TRUE)      # Whether to draw outliers
  })
  
  output$plot2 <- renderPlot({
    # Your code to generate plot 2
    heatmap(x = shared.plt.mtx.pace,
            xlab = "Patient IDs",
            ylab = "Genes",
            main = "Gene Expression Heatmap",
            labRow = rownames(shared.plt.mtx.pace),   
            labCol = colnames(shared.plt.mtx.pace))
  })
  
  })
}


