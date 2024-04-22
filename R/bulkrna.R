bulkrna_ui <- function(id){
  sidebarLayout(
    sidebarPanel(
      textAreaInput(NS(id, "gene_search2"), "Enter Genes (one per line):", 
                    value = "AARS\nAAGAB\nABALON", rows = 5),
      # Output to display the gene names
    ),
    mainPanel(
      fluidRow(
        column(width = 6, 
               dropdownButton(
                 
                 tags$h3("List of Inputs"),
                 
                 selectInput(inputId = 'xcol',
                             label = 'X Variable',
                             choices = names(iris)),
                 
                 selectInput(inputId = 'ycol',
                             label = 'Y Variable',
                             choices = names(iris),
                             selected = names(iris)[[2]]),
                 
                 sliderInput(inputId = 'clusters',
                             label = 'Cluster count',
                             value = 3,
                             min = 1,
                             max = 9),
                 
                 circle = TRUE, status = "danger",
                 icon = icon("gear"), width = "300px",
                 
                 tooltip = tooltipOptions(title = "Click to see inputs !")
               ),
               
               plotOutput(NS(id, "plot1"))),
        column(width = 6,
               dropdownButton(
                 
                 tags$h3("List of Inputs"),
                 
                 selectInput(inputId = 'xcol',
                             label = 'X Variable',
                             choices = names(iris)),
                 
                 selectInput(inputId = 'ycol',
                             label = 'Y Variable',
                             choices = names(iris),
                             selected = names(iris)[[2]]),
                 
                 sliderInput(inputId = 'clusters',
                             label = 'Cluster count',
                             value = 3,
                             min = 1,
                             max = 9),
                 
                 circle = TRUE, status = "danger",
                 icon = icon("gear"), width = "300px",
                 
                 tooltip = tooltipOptions(title = "Click to see inputs !")
               ),
               
               plotOutput(NS(id, "plot2")))
      )
    )
  )
}


bulkrna_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    data_folder <- "./data/"
    shared.plt.mtx.pace <- read.csv(paste0(data_folder, "24-04-19-shared_genes_plt.csv"), row.names = 1)
    
    observe({
      # Get the genes entered by the user from the textAreaInput
      genes_of_interest <- strsplit(input$gene_search2, "\n")[[1]]
      
      # Subset the gene expression data for the selected genes
      selected_genes <- shared.plt.mtx.pace[genes_of_interest, ]
      
    
      
      # Update plot1 with the boxplot of selected genes
      output$plot1 <- renderPlot({
        # Convert the data to a format suitable for boxplot()
        selected_genes_boxplot <- as.data.frame(t(selected_genes))
        # Create box plots for the selected genes
        boxplot(selected_genes_boxplot,
                main = "Boxplot of Gene Expression",
                xlab = "Genes",
                ylab = "Expression Level",
                col = "skyblue",      # Color of the boxes
                border = "black",     # Border color of the boxes
                notch = FALSE,        # Whether to draw a notch
                outline = TRUE)      # Whether to draw outliers
      })
   
    output$plot2 <- renderPlot({
      selected_genes_matrix <- data.matrix(selected_genes, rownames.force = NA)
      # Your code to generate plot 2
      heatmap(x = selected_genes_matrix,
              xlab = "Patient IDs",
              ylab = "Genes",
              main = "Gene Expression Heatmap",
              labRow = rownames(selected_genes_matrix),   
              labCol = colnames(selected_genes_matrix))
    })
    })
    
  })
}
