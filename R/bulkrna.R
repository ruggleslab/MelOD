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
                 
                 selectInput(inputId =NS(id,'notch'),
                             label = 'Notch',
                             choices = c(TRUE,FALSE),
                             selected = FALSE),
                 
                 selectInput(inputId = NS(id,'outline'),
                             label = 'Outline',
                             choices = c(TRUE,FALSE),
                             selected = TRUE),
                 
                 selectInput(inputId = NS(id,'color_boxplot'),
                             label = 'Select Color',
                            choices = c("skyblue", "red", "green"),
                             selected = "skyblue"),
                 
                 
                 
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

data_folder <- "./data/"
shared.plt.mtx.pace <- read.csv(paste0(data_folder, "24-04-19-shared_genes_plt.csv"), row.names = 1)


bulkrna_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Define a reactive expression to handle gene selection
    selected_genes <- reactive({
      # Get the genes entered by the user from the textAreaInput
      genes_of_interest <- strsplit(input$gene_search2, "\n")[[1]]
      # Subset the gene expression data for the selected genes
      shared.plt.mtx.pace[genes_of_interest, ]
    })
    
    # Update plot1 with the boxplot of selected genes
    output$plot1 <- renderPlot({
      selected_genes_boxplot <- as.data.frame(t(selected_genes()))
      boxplot(selected_genes_boxplot,
              main = "Boxplot of Gene Expression",
              xlab = "Genes",
              ylab = "Expression Level",
              col = input$color_boxplot,      
              border = "black",     
              notch = as.logical(input$notch),      
              outline = as.logical(input$outline))
    })
    
    # Update plot2 with the heatmap of selected genes
    output$plot2 <- renderPlot({
      selected_genes_matrix <- data.matrix(selected_genes(), rownames.force = NA)
      heatmap(x = selected_genes_matrix,
              main = "Gene Expression Heatmap",
              labRow = rownames(selected_genes_matrix),   
              labCol = colnames(selected_genes_matrix))
    })
  })
}
