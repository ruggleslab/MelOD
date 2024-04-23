singlecell_ui <- function(id){


  fluidRow(
    tabBox(
      title = "BoxPlot",
      # The id lets us use input$tabset1 on the server to find the current tab
      id = NS(id,"plot1"), height = "250px",
      plotOutput(NS(id, "plot1")),
    ),
    tabBox(
      side = "right", height = "250px",
      selected = "Tab3",
      tabPanel("HeatMap", plotOutput(NS(id, "plot2"))),
      tabPanel("BoxPlot", plotOutput(NS(id, "plot1")),),
      tabPanel("Tab3", "Note that when side=right, the tab order is reversed.")
    )
  )


}


data_folder <- "./data/"
shared.plt.mtx.pace <- read.csv(paste0(data_folder, "24-04-19-shared_genes_plt.csv"), row.names = 1)


singlecell_server <- function(id) {
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
