source("global.R", local = TRUE)
source("./templates/shared_functions.R", local = TRUE)



fischer_ui <- function(id) {
  shared_ui("fischer")
}

fischer_server <- function(id, session) {
  
  ns <- session$ns
  # Load DESeq2 results and CPM values
  dds <- readRDS(file.path("./data/fischer", "Fischer_Deseq2.rds"))
  clinical_data <- read_excel("./data/fischer/Fischer_demographics_information_Final.xlsx")
  server_shared(dds = dds,clinical_data = clinical_data, "fischer")
}

























# 
# blurbs <- "./www/data_blurbs.json" # Update this path to where your JSON file is stored
# blurbs_info <- fromJSON(blurbs)
# badal_info <- blurbs_info$badal_info
# 
# badal_ui <- function(id) {
#   ns <- NS(id)
#   fluidPage(
#     
#     fluidRow(
#       box(title = "Explanation of data used in analysis:", status = "info",
#           collapsible = TRUE,
#           solidHeader = TRUE, width = 12,
#           tags$p(badal_info$data_explanation),
#           
#           
#           
#       )),
#   fluidRow(
#   
#     
#   box(title = "Inputs", status = "warning",
#       collapsible = TRUE,
#       solidHeader = TRUE, width = 6,
#       
#       tags$h3("Explanation of data used in analysis: "),
#       tags$p(badal_info$data_explanation),
#       tags$h3("Parameters", style = "margin-top: 0;"),  # Title for the parameters section
#       numericInput(ns("slider_padj"), "padj Cutoff", 0.05, min = 0, max = 1, step = 0.01),
#       numericInput(ns("slider_log2"), "log2foldchange Cutoff", 2, step = 0.1),
#       selectizeInput(ns("selected_gene"), "Gene(s) selection",
#                      choices = NULL,  # Ensure this is accessible here or move to server
#                      selected = NULL,  # Default selection
#                      multiple = TRUE),
#       actionButton(ns("update_plot"), "Generate plots", class = "btn-primary")
#       
#       
#       
#   ),
#   box(
#     title = "Study Overview", status = "info", solidHeader = TRUE, width = 6, collapsible = TRUE,
#     tags$h2(badal_info$title),
#     tags$h3("Lead Author: ", badal_info$lead_author),
#     tags$h3("Abstract: "),
#     tags$p(badal_info$abstract),
#     tags$p("Read the full paper: ", tags$a(href = badal_info$paper_link, "PubMed", target = "_blank")),
#     tags$p("DOI: ", tags$a(href = paste("https://doi.org/", badal_info$doi, sep = ""), badal_info$doi, target = "_blank")),
#     tags$p("Data Access: ", tags$a(href = badal_info$data_link, "ENA Dataset", target = "_blank")),
#     
#   )),
#     fluidRow(
#       tabBox(
#         title = "Metadata",
#         # The id lets us use input$tabset1 on the server to find the current tab
#         id = "tabset1", 
#         width = 6,
#         tabPanel("Mortality", plotlyOutput(ns("mortality"))),
#         tabPanel("Gender", plotlyOutput(ns("gender")))
#       ),
#       box(
#         title = "PCA",  # Title of the box
#         status = "primary",  
#         width = 6,# Color theme
#         solidHeader = TRUE,            # Gives the box a solid header
#         collapsible = TRUE,            # Allows the box to be collapsed
#         plotlyOutput(ns("pca"))   # Placeholder for the plot
#       )),
#     
# 
#       fluidRow(
#         
#         
#         box(
#           title = "Boxplot",  # Title of the box
#           status = "primary",            # Color theme
#           solidHeader = TRUE,            # Gives the box a solid header
#           collapsible = TRUE,            # Allows the box to be collapsed
#           plotlyOutput(ns("badal_test"))   # Placeholder for the plot
#         ),
#         box(
#           title = "Volcano Plot",  # Title of the box
#           status = "primary",            # Color theme
#           solidHeader = TRUE,            # Gives the box a solid header
#           collapsible = TRUE,            # Allows the box to be collapsed
#           plotlyOutput(ns("volcano_plot"))  # Placeholder for the plot
#         )),
#       fluidRow(
#         box(
#           title = "Heatmap",  # Title of the box
#           status = "primary",
#           width = 12,# Color theme
#           solidHeader = TRUE,      # Gives the box a solid header
#           collapsible = TRUE,      # Allows the box to be collapsed
#           plotlyOutput(ns("badal_heatmap_test"))  # Placeholder for the volcano plot
#         )),
#     fluidRow(
#       box(width = 12,title = "DESeq2 Results", status = "info", solidHeader = TRUE, collapsible = TRUE, DT::dataTableOutput(ns("filtered_results"))),
#       
#     )
#   )
# }
# 
# 
# badal_server <- function(id) {
#   moduleServer(id, function(input, output, session) {
#     ns <- session$ns
#     
#     # Load DESeq2 results and CPM values
#     dds <- readRDS(file.path("./data/badal", "Badal_Deseq2.rds"))
# 
#     # Check for NA in symbols and filter them out
#     if ("symbol" %in% names(mcols(dds))) {
#       na_filter <- !is.na(mcols(dds)$symbol)
#       dds <- dds[na_filter,]  # Keep only rows without NA in 'symbol'
#       gene_symbols <- mcols(dds)$symbol
#     } else {
#       stop("Gene symbols not found in the dataset metadata.")
#     }
#     
#     makeUniqueRowNames <- function(names) {
#       counts <- table(names)
#       duplicates <- names[counts[names] > 1]
#       for (d in unique(duplicates)) {
#         idx <- which(names == d)
#         names[idx] <- paste(d, seq_along(idx), sep = "_")
#       }
#       names
#     }
#     
#     # Apply this function to gene_symbols
#     unique_gene_symbols <- makeUniqueRowNames(gene_symbols)
#     
#     # Apply the unique row names to the DDS object
#     if (length(unique_gene_symbols) == nrow(dds)) {
#       rownames(dds) <- unique_gene_symbols
#     } else {
#       stop("The number of unique gene symbols does not match the number of rows in the dataset.")
#     }
#     
#     res <- results(dds)
#     # Define reactive expression for filtered data
#     
#     
#     
#     
#     
#     
#     
#     filtered_data <- eventReactive(input$update_plot,{
#       padj_threshold <- input$slider_padj
#       log2_thresholds <- input$slider_log2
#       if (is.numeric(padj_threshold) && !is.na(padj_threshold)) {
#         filtered_genes <- res[
#           !is.na(res$padj) & res$padj < padj_threshold &
#             (res$log2FoldChange <= -log2_thresholds | res$log2FoldChange >= log2_thresholds),
#           
#           , drop = FALSE
#         ]
#         filtered_genes[order(filtered_genes$padj), ]
#       }
#     }, ignoreNULL = FALSE)
#     
#     filter_and_order_by_padj <- function(results_data) {
#       # Ensure the padj column exists
#       if (!"padj" %in% names(results_data)) {
#         stop("The provided data does not have a 'padj' column.")
#       }
#       
#       # Filter rows where padj is not NA and order by padj
#       filtered_data <- results_data[!is.na(results_data$padj), ]
#       ordered_data <- filtered_data[order(filtered_data$padj), ]
#       
#       return(ordered_data)
#     }
#     
#     filtered_res <- filter_and_order_by_padj(res)
#     
#     ####################################################### see if I do choices = rownames(filtered_data())
#     observe({
#       updateSelectizeInput(session, "selected_gene", choices = rownames(filtered_res), server = TRUE)
#     })
#     
#     # display_genes <- reactive({
#     #   # Get all filtered genes from the reactive
#     #   all_genes <- filtered_data()
#     # 
#     #   # Check if specific genes are selected
#     #   selected_genes <- input$selected_gene
#     #   if (!is.null(selected_genes) && all(selected_genes %in% rownames(all_genes))) {
#     #     # Return only the selected genes
#     #     return(all_genes[rownames(all_genes) %in% selected_genes, , drop = FALSE])
#     #     print(all_genes[rownames(all_genes) %in% selected_genes, , drop = FALSE])
#     #   } else {
#     #     # Return top 5 genes as default
#     #     return(head(all_genes, 10))
#     #   }
#     # })
#     # 
#     
#     display_genes <- reactive({
#       # Get all filtered genes from the reactive
#       all_genes <- filtered_res
#       
#       # Check if specific genes are selected
#       selected_genes <- input$selected_gene
#       if (!is.null(selected_genes) && all(selected_genes %in% rownames(all_genes))) {
#         # Return only the selected genes
#         return(all_genes[rownames(all_genes) %in% selected_genes, , drop = FALSE])
#         print(all_genes[rownames(all_genes) %in% selected_genes, , drop = FALSE])
#       } else {
#         # Return top 5 genes as default
#         return(head(all_genes, 10))
#       }
#     })
#     
#     
#     
#   
#     
# #############################################
#     
#     # Event reactive for updating the plots only when the button is clicked
#     plot_data <- eventReactive(input$update_plot, {
#       # Isolate ensures changes in these inputs do not trigger this reactive block
#       gene <- isolate(input$selected_gene)
#       padj_cut <-isolate(input$slider_padj)
#       log2_cut <-isolate(input$slider_log2)
#       # Generate plots
#       list(
#         boxplot = create_boxplot(dds = dds,  gene = gene, display_genes = display_genes()),
#         volcano = create_volcanoplot(res = res, gene = gene,padj_cut=padj_cut,log2_cut=log2_cut),
#         heatmap = create_heatmap(dds = dds, gene = gene)
# 
#       )
#     })
#     
#     # Creating a Plotly boxplot reactively
#     output$badal_test <- renderPlotly({
#       
#       
#       
#       shiny::req(filtered_data())  # Ensure that the data is available
#       plot_data()$boxplot
#     })
# 
#    
#     # Volcano Plot
#     output$volcano_plot <- renderPlotly({
#       
#       req(plot_data())  # Make sure plot_data is available before rendering
#       plot_data()$volcano
#     })
#     
#     # Heatmap Plot
#     output$badal_heatmap_test <- renderPlotly({
#       req(plot_data())  # Make sure plot_data is available before rendering
#       plot_data()$heatmap
#     })
#     
#     ############################### if want just filtered result display filtered_data()
#     output$filtered_results <- DT::renderDataTable({
#       req(filtered_data())  # Ensure that the data is available
#       
#       # Convert the result to a data frame and include DataTables options with export buttons
#       DT::datatable(
#         as.data.frame(res),
#         extensions = 'Buttons',  # Enable buttons extension for DataTables
#         options = list(
#           dom = 'Blfrtip',  # Define the table control elements to appear on the page: B-buttons, f-filtering input, r-processing display element, t-the table, i-table information summary, p-pagination control
#           buttons = c('copy', 'csv', 'excel'),  # Include buttons for copying to clipboard, exporting to CSV, and Excel
#           pageLength = 10,
#           scrollX = TRUE
#         )
#       )
#     })
#     
#     
#     
#     
#     clinical_data <- read.csv(file = "./data/badal/clinical_data.csv", sep=";")
#     colnames(clinical_data) <- as.character(unlist(clinical_data[2,]))
#     clinical_data <- clinical_data[-c(1, 2), ]
#     colors<- c("#1E88E5", "#D81B60","#117733")  # Blue and Orange
#     
#     output$mortality <- renderPlotly({
#       
#       status_summary <- table(clinical_data$`Patient Status`)
#       status_df <- as.data.frame(status_summary)
#       names(status_df) <- c("Status", "Count")
#     
#       plot_ly(status_df, labels = ~Status, values = ~Count, type = 'pie',
#               textposition = 'inside',
#               textinfo = 'label+percent',
#               insidetextorientation = 'radial',showlegend = FALSE,marker = list(colors = colors)) %>%
#         layout(title = 'Patient Survival Status',
#                xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
#                yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
#                margin = list(t = 100))
#       
#     })
#     output$gender <- renderPlotly({
#       
#       status_summary_gender <- table(clinical_data$`Gender`)
#       status_df_gender <- as.data.frame(status_summary_gender)
#       
#       
#       names(status_df_gender) <- c("Status", "Count")
#       
#       
#       plot_ly(status_df_gender, labels = ~Status, values = ~Count, type = 'pie',
#               textposition = 'inside',
#               textinfo = 'label+percent',
#               insidetextorientation = 'radial',showlegend = FALSE,marker = list(colors = colors)) %>%
#         layout(title = 'Patient Gender Status',
#                xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
#                yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
#                margin = list(t = 100))
#       
#     })
#     output$pca <- renderPlotly({
#       creation_pca(dds)
#     })
#   })
# }
# 
# 





