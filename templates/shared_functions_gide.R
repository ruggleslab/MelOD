source("global.R", local = TRUE)



blurbs <- "./www/data_blurbs.json" 
blurbs_info <- fromJSON(blurbs)




shared_ui_gide <- function(id) { 
  Id_info <- blurbs_info[[paste(id, "info", sep = "_")]]  
  ns <- NS(id)
  
  fluidPage(
    fluidRow(
      box(title = "Explanation of data used in analysis:", status = "info",
          collapsible = TRUE,
          solidHeader = TRUE, width = 12,
          tags$p(Id_info$data_explanation),
          
      )),
    fluidRow(
      box(title = "Inputs", status = "warning",
          collapsible = TRUE,
          solidHeader = TRUE, width = 5,
          
          tags$h3("Parameters", style = "margin-top: 0;"),  # Title for the parameters section
          numericInput(ns("slider_padj"), "padj Cutoff", 0.05, min = 0, max = 1, step = 0.01),
          numericInput(ns("slider_log2"), "log2foldchange Cutoff", 2, step = 0.1),
          numericInput(ns("number"), "Number of genes for the heatmap (min. 3)", 10, min = 3, step = 1),
          checkboxGroupInput(
            inputId = ns("selection"),
            label = "Comparison",
            choices = c("Combotherapy", "Monotherapy"),
            selected = "Combotherapy",
            inline = TRUE),          
          selectizeInput(ns("selected_gene"), "Gene(s) selection (up ot 10)",
                         choices = NULL,  # Ensure this is accessible here or move to server
                         selected = NULL,  # Default selection
                         multiple = TRUE,
                         options = list(maxItems = 10)),
          actionButton(ns("update_plot"), "Generate plots", class = "btn-primary")
          
          
          
      ),
      box(
        title = "Study Overview", status = "info", solidHeader = TRUE, width = 6, collapsible = TRUE,
        tags$h2(Id_info$title),
        tags$h3("Lead Author: ", Id_info$lead_author),
        tags$p(Id_info$abstract),
        tags$p("Read the full paper: ", tags$a(href = Id_info$paper_link, "PubMed", target = "_blank")),
        tags$p("DOI: ", tags$a(href = paste("https://doi.org/", Id_info$doi, sep = ""), Id_info$doi, target = "_blank")),
        tags$p("Data Access: ", tags$a(href = Id_info$data_link, "ENA Dataset", target = "_blank")),
        tags$p(Id_info$data_explanation)
      )),
    
    fluidRow(
      
      box(
        title = "PCA",  # Title of the box
        status = "primary",  
        width = 8,# Color theme
        solidHeader = TRUE,            # Gives the box a solid header
        collapsible = TRUE,            # Allows the box to be collapsed
        plotlyOutput(ns("pca")),
        # downloadablePlotlyUI(ns(id = 'pca_data'))
      ),
      tabBox(
        title = "Metadata",
        # The id lets us use input$tabset1 on the server to find the current tab
        id = "tabset1", 
        width = 4,
        tabPanel("Mortality Curve", plotlyOutput(ns("mortality_curve"))),
        tabPanel("Gender", plotlyOutput(ns("gender")))
      )),
    
    fluidRow(
      
      box(
        title = "Boxplot",  # Title of the box
        status = "primary", 
        width = 7,
        solidHeader = TRUE,            # Gives the box a solid header
        collapsible = TRUE,            # Allows the box to be collapsed
        plotlyOutput(ns("badal_test"))   # Placeholder for the plot
      ),
      box(
        title = "Volcano Plot",  # Title of the box
        status = "primary",  
        width = 5,
        solidHeader = TRUE,            # Gives the box a solid header
        collapsible = TRUE,            # Allows the box to be collapsed
        plotlyOutput(ns("volcano_plot"))  # Placeholder for the plot
      )),
    fluidRow(
      box(
        title = "Heatmap",  # Title of the box
        status = "primary",
        width = 12,# Color theme
        solidHeader = TRUE,      # Gives the box a solid header
        collapsible = TRUE,      # Allows the box to be collapsed
        plotlyOutput(ns("badal_heatmap_test"))  # Placeholder for the volcano plot
      )),
    fluidRow(
      box(
        title = "Correlation",  # Title of the box
        status = "primary",
        width = 12,# Color theme
        solidHeader = TRUE,      # Gives the box a solid header
        collapsible = TRUE,      # Allows the box to be collapsed
        plotlyOutput(ns("correlation"))  # Placeholder for the volcano plot
      )
    ),
    fluidRow(
      box(width = 12,title = "DESeq2 Results", status = "info", solidHeader = TRUE, collapsible = TRUE, DT::dataTableOutput(ns("filtered_results"))),
      
    )
    
  )
}


server_shared_gide <- function(id) {
  moduleServer(id, function(input, output, session) {
  
    
    dds <- readRDS(file.path("./data/gide/combo", "ddsPreCombo.rds"))
    
    # Check for NA in symbols and filter them out
    if ("symbol" %in% names(mcols(dds))) {
      na_filter <- !is.na(mcols(dds)$symbol)
      dds <- dds[na_filter,]  # Keep only rows without NA in 'symbol'
      gene_symbols <- mcols(dds)$symbol
    } else {
      stop("Gene symbols not found in the dataset metadata.")
    }
    
    makeUniqueRowNames <- function(names) {
      counts <- table(names)
      duplicates <- names[counts[names] > 1]
      for (d in unique(duplicates)) {
        idx <- which(names == d)
        names[idx] <- paste(d, seq_along(idx), sep = "_")
      }
      names
    }
    
    # Apply this function to gene_symbols
    unique_gene_symbols <- makeUniqueRowNames(gene_symbols)
    
    # Apply the unique row names to the DDS object
    if (length(unique_gene_symbols) == nrow(dds)) {
      rownames(dds) <- unique_gene_symbols
    } else {
      stop("The number of unique gene symbols does not match the number of rows in the dataset.")
    }
    
    res <- results(dds)
    
    
    

    filtered_data <- eventReactive(input$update_plot,{
      padj_threshold <- input$slider_padj
      log2_thresholds <- input$slider_log2
      if (is.numeric(padj_threshold) && !is.na(padj_threshold)) {
        filtered_genes <- res[
          !is.na(res$padj) & res$padj < padj_threshold &
            (res$log2FoldChange <= -log2_thresholds | res$log2FoldChange >= log2_thresholds),
          
          , drop = FALSE
        ]
        filtered_genes[order(filtered_genes$padj), ]
      }
    }, ignoreNULL = FALSE)
    
    
    filter_and_order_by_padj <- function(results_data) {
      # Ensure the padj column exists
      if (!"padj" %in% names(results_data)) {
        stop("The provided data does not have a 'padj' column.")
      }
      
      # Filter rows where padj is not NA and order by padj
      filtered_data <- results_data[!is.na(results_data$padj), ]
      ordered_data <- filtered_data[order(filtered_data$padj), ]
      
      return(ordered_data)
    }
    
    filtered_res <- filter_and_order_by_padj(res)
    
    ####################################################### see if I do choices = rownames(filtered_data())
    observe({
      updateSelectizeInput(session, "selected_gene", choices = rownames(filtered_res), server = TRUE)
    })
    
    
    display_genes <- reactive({
      # Get all filtered genes from the reactive
      all_genes <- filtered_res
      
      # Check if specific genes are selected
      selected_genes <- input$selected_gene
      if (!is.null(selected_genes) && all(selected_genes %in% rownames(all_genes))) {
        # Return only the selected genes
        return(all_genes[rownames(all_genes) %in% selected_genes, , drop = FALSE])
        print(all_genes[rownames(all_genes) %in% selected_genes, , drop = FALSE])
      } else {
        # Return top 5 genes as default
        return(head(all_genes, 5))
      }
    })
    
    
    #############################################
    
    
    
    
    
    
    # Reactive event to update plots based on the selected treatment type
    plot_data <- eventReactive(input$update_plot, {
      # Isolate inputs
      selected_treatments <- isolate(input$selection)
      
      
      gene <- isolate(input$selected_gene)
      padj_cut <-isolate(input$slider_padj)
      log2_cut <-isolate(input$slider_log2)
      number <-isolate(input$number)
      
      # Conditional plotting based on the selected treatments
      plots <- lapply(selected_treatments, function(treatment) {
        switch(treatment,
               
               Combotherapy = list(
                 
                 dds <- readRDS(file.path("./data/gide/combo", "ddsPreCombo.rds")),
                 

                 # Check for NA in symbols and filter them out
                 if ("symbol" %in% names(mcols(dds))) {
                   na_filter <- !is.na(mcols(dds)$symbol)
                   dds <- dds[na_filter,]  # Keep only rows without NA in 'symbol'
                   gene_symbols <- mcols(dds)$symbol
                 } else {
                   stop("Gene symbols not found in the dataset metadata.")
                 },
                 
                 makeUniqueRowNames <- function(names) {
                   counts <- table(names)
                   duplicates <- names[counts[names] > 1]
                   for (d in unique(duplicates)) {
                     idx <- which(names == d)
                     names[idx] <- paste(d, seq_along(idx), sep = "_")
                   }
                   names
                 },
                 
                 # Apply this function to gene_symbols
                 unique_gene_symbols <- makeUniqueRowNames(gene_symbols),
                 
                 # Apply the unique row names to the DDS object
                 if (length(unique_gene_symbols) == nrow(dds)) {
                   rownames(dds) <- unique_gene_symbols
                 } else {
                   stop("The number of unique gene symbols does not match the number of rows in the dataset.")
                 },
                 
                 res <- results(dds),
                 
                 
                 
                 
                 filtered_data <- eventReactive(input$update_plot,{
                   padj_threshold <- input$slider_padj
                   log2_thresholds <- input$slider_log2
                   if (is.numeric(padj_threshold) && !is.na(padj_threshold)) {
                     filtered_genes <- res[
                       !is.na(res$padj) & res$padj < padj_threshold &
                         (res$log2FoldChange <= -log2_thresholds | res$log2FoldChange >= log2_thresholds),
                       
                       , drop = FALSE
                     ]
                     filtered_genes[order(filtered_genes$padj), ]
                   }
                 }, ignoreNULL = FALSE),
                 
                 
                 filter_and_order_by_padj <- function(results_data) {
                   # Ensure the padj column exists
                   if (!"padj" %in% names(results_data)) {
                     stop("The provided data does not have a 'padj' column.")
                   }
                   
                   # Filter rows where padj is not NA and order by padj
                   filtered_data <- results_data[!is.na(results_data$padj), ]
                   ordered_data <- filtered_data[order(filtered_data$padj), ]
                   
                   return(ordered_data)
                 },
                 
                 filtered_res <- filter_and_order_by_padj(res),
                 
                 ####################################################### see if I do choices = rownames(filtered_data())
                 observe({
                   updateSelectizeInput(session, "selected_gene", choices = rownames(filtered_res), server = TRUE)
                 }),
                 
                 
                 display_genes <- reactive({
                   # Get all filtered genes from the reactive
                   all_genes <- filtered_res
                   
                   # Check if specific genes are selected
                   selected_genes <- input$selected_gene
                   if (!is.null(selected_genes) && all(selected_genes %in% rownames(all_genes))) {
                     # Return only the selected genes
                     return(all_genes[rownames(all_genes) %in% selected_genes, , drop = FALSE])
                     print(all_genes[rownames(all_genes) %in% selected_genes, , drop = FALSE])
                   } else {
                     # Return top 5 genes as default
                     return(head(all_genes, 3))
                   }
                 }),
                 
                 
                 
                 
                 
                 
                 clinical_data <- read.csv(file = "./data/gide/Gide_demographics_combotherapy.csv", sep=","),
                 
                 boxplot = create_boxplot(dds = dds,  gene = gene, display_genes = display_genes()),
                 volcano = create_volcanoplot(dds = dds, gene = gene,padj_cut=padj_cut,log2_cut=log2_cut),
                 heatmap = create_heatmap(dds=dds,padj_cut=padj_cut,log2_cut=log2_cut,number=number, gene=gene),
                 correlation = create_correlation(dds=dds,padj_cut=padj_cut,log2_cut=log2_cut,number=number, gene=gene),
                 mortality = plot_mortality_curve(clinical_data),
                 pca =creation_pca(dds= dds)
                 
               ),
               Monotherapy = list(
                 clinical_data <- read.csv(file = "./data/gide/Gide_demographics_monotherapy.csv", sep=","),
                 
                 dds <- readRDS(file.path("./data/gide/mono", "ddsPreMono.rds")),
                 
                 
                 # Check for NA in symbols and filter them out
                 if ("symbol" %in% names(mcols(dds))) {
                   na_filter <- !is.na(mcols(dds)$symbol)
                   dds <- dds[na_filter,]  # Keep only rows without NA in 'symbol'
                   gene_symbols <- mcols(dds)$symbol
                 } else {
                   stop("Gene symbols not found in the dataset metadata.")
                 },
                 
                 makeUniqueRowNames <- function(names) {
                   counts <- table(names)
                   duplicates <- names[counts[names] > 1]
                   for (d in unique(duplicates)) {
                     idx <- which(names == d)
                     names[idx] <- paste(d, seq_along(idx), sep = "_")
                   }
                   names
                 },
                 
                 # Apply this function to gene_symbols
                 unique_gene_symbols <- makeUniqueRowNames(gene_symbols),
                 
                 # Apply the unique row names to the DDS object
                 if (length(unique_gene_symbols) == nrow(dds)) {
                   rownames(dds) <- unique_gene_symbols
                 } else {
                   stop("The number of unique gene symbols does not match the number of rows in the dataset.")
                 },
                 
                 res <- results(dds),
                 
                 
                 
                 
                 filtered_data <- eventReactive(input$update_plot,{
                   padj_threshold <- input$slider_padj
                   log2_thresholds <- input$slider_log2
                   if (is.numeric(padj_threshold) && !is.na(padj_threshold)) {
                     filtered_genes <- res[
                       !is.na(res$padj) & res$padj < padj_threshold &
                         (res$log2FoldChange <= -log2_thresholds | res$log2FoldChange >= log2_thresholds),
                       
                       , drop = FALSE
                     ]
                     filtered_genes[order(filtered_genes$padj), ]
                   }
                 }, ignoreNULL = FALSE),
                 
                 
                 filter_and_order_by_padj <- function(results_data) {
                   # Ensure the padj column exists
                   if (!"padj" %in% names(results_data)) {
                     stop("The provided data does not have a 'padj' column.")
                   }
                   
                   # Filter rows where padj is not NA and order by padj
                   filtered_data <- results_data[!is.na(results_data$padj), ]
                   ordered_data <- filtered_data[order(filtered_data$padj), ]
                   
                   return(ordered_data)
                 },
                 
                 filtered_res <- filter_and_order_by_padj(res),
                 
                 ####################################################### see if I do choices = rownames(filtered_data())
                 observe({
                   updateSelectizeInput(session, "selected_gene", choices = rownames(filtered_res), server = TRUE)
                 }),
                 
                 
                 display_genes <- reactive({
                   # Get all filtered genes from the reactive
                   all_genes <- filtered_res
                   
                   # Check if specific genes are selected
                   selected_genes <- input$selected_gene
                   if (!is.null(selected_genes) && all(selected_genes %in% rownames(all_genes))) {
                     # Return only the selected genes
                     return(all_genes[rownames(all_genes) %in% selected_genes, , drop = FALSE])
                     print(all_genes[rownames(all_genes) %in% selected_genes, , drop = FALSE])
                   } else {
                     # Return top 5 genes as default
                     return(head(all_genes, 3))
                   }
                 }),
                 
                 
                 
                 boxplot = create_boxplot(dds = dds,  gene = gene, display_genes = display_genes()),
                 volcano = create_volcanoplot(dds = dds, gene = gene,padj_cut=padj_cut,log2_cut=log2_cut),
                 heatmap = create_heatmap(dds=dds,padj_cut=padj_cut,log2_cut=log2_cut,number=number, gene=gene),
                 correlation = create_correlation(dds=dds,padj_cut=padj_cut,log2_cut=log2_cut,number=number, gene=gene),
                 mortality = plot_mortality_curve(clinical_data),
                 pca =creation_pca(dds= dds)
                 
               )
        )
      })

      # Combine all the list elements into a single list if multiple treatments are selected
      do.call(c, plots)
    })
    
    
    
    
   
    # Creating a Plotly boxplot reactively
    output$badal_test <- renderPlotly({
      shiny::req(filtered_data())  # Ensure that the data is available
      plot_data()$boxplot
    })
    
    
    # Volcano Plot
    output$volcano_plot <- renderPlotly({
      
      req(plot_data())  # Make sure plot_data is available before rendering
      plot_data()$volcano
    })
    
    # Heatmap Plot
    output$badal_heatmap_test <- renderPlotly({
      req(plot_data())  # Make sure plot_data is available before rendering
      plot_data()$heatmap
    })
    
    # Correlation Plot
    
    output$correlation <- renderPlotly({
      req(plot_data())  # Make sure plot_data is available before rendering
      plot_data()$correlation
    })
    
    
    
    ############################### if want just filtered result display filtered_data()
    output$filtered_results <- DT::renderDataTable({
      req(filtered_data())  # Ensure that the data is available
      
      # Convert the result to a data frame and include DataTables options with export buttons
      DT::datatable(
        as.data.frame(res),
        extensions = 'Buttons',  # Enable buttons extension for DataTables
        options = list(
          dom = 'Blfrtip',  # Define the table control elements to appear on the page: B-buttons, f-filtering input, r-processing display element, t-the table, i-table information summary, p-pagination control
          buttons = c('copy', 'csv', 'excel'),  # Include buttons for copying to clipboard, exporting to CSV, and Excel
          pageLength = 10,
          scrollX = TRUE
        )
      )
    })
 
    
    
    
    
    output$mortality_curve <- renderPlotly({
      plot_data()$mortality
      
    })
    
    
    output$mortality <- renderPlotly({
      plot_mortality(clinical_data_all)
    })
    
    
  
    
    output$pca <- renderPlotly({
      plot_data()$pca
    })
    
  })
}







