source("global.R", local = TRUE)



blurbs <- "./www/data_blurbs.json" 
blurbs_info <- fromJSON(blurbs)




shared_ui <- function(id) { 
  
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
          solidHeader = TRUE, width = 3,
          
          tags$h3("Parameters", style = "margin-top: 0;"),  # Title for the parameters section
          numericInput(ns("slider_padj"), "padj Cutoff", 0.05, min = 0, max = 1, step = 0.01),
          numericInput(ns("slider_log2"), "log2foldchange Cutoff", 2, step = 0.1),
          numericInput(ns("number"), "Number of genes for the heatmap (min. 2 if no genes selected)", 10, min = 0, step = 1),
          selectizeInput(ns("selected_gene"), "Gene(s) selection (up ot 10)",
                         choices = NULL,  # Ensure this is accessible here or move to server
                         selected = NULL,  # Default selection
                         multiple = TRUE,
                         options = list(maxItems = 10)),
          actionButton(ns("update_plot"), "Generate plots", class = "btn-primary")
          
          
          
      ),
      box(
        title = "Study Overview", status = "info", solidHeader = TRUE, width = 9, collapsible = TRUE,
        tags$h2(Id_info$title),
        tags$h3("Lead Author: ", Id_info$lead_author),
        tags$p(Id_info$abstract),
        tags$p("Read the full paper: ", tags$a(href = Id_info$paper_link, "PubMed", target = "_blank")),
        tags$p("DOI: ", tags$a(href = paste("https://doi.org/", Id_info$doi, sep = ""), Id_info$doi, target = "_blank")),
        tags$p("Data Access: ", tags$a(href = Id_info$data_link, "ENA Dataset", target = "_blank"))
      )),
    
    fluidRow(
      
      box(
        title = HTML(paste("PCA", 
                           actionLink(ns("info_pca_plot"), label = "", icon = icon("info-circle")),downloadButton(ns('pca_data'),  label = "", icon = icon("save-file", lib = "glyphicon")))),
        status = "primary", 
        width = 8,# Color theme
        solidHeader = TRUE,            # Gives the box a solid header
        collapsible = TRUE,    
        plotlyOutput(ns(id = 'pca_plot'))
        
      ),
      tabBox(
        title = "Metadata",
        # The id lets us use input$tabset1 on the server to find the current tab
        id = "tabset1", 
        width = 4,
        tabPanel("Mortality", plotlyOutput(ns("mortality"))),
        tabPanel("Gender", plotlyOutput(ns("gender_data")))
      )),
    
    fluidRow(
      
      box(
        title = HTML(paste("Differential gene expression", 
                           actionLink(ns("info_violin_plot"), label = "", icon = icon("info-circle")),downloadButton(ns('violin_data'),  label = "", icon = icon("save-file", lib = "glyphicon")))),
        status = "primary", 
        width = 12,
        solidHeader = TRUE,
        collapsible = TRUE, 
        column(6,
               plotlyOutput(ns("volcano_plot"))
        ),
        column(6,
               plotlyOutput(ns(id = 'violin_plot')))        
          # Placeholder for the plot
      ) ),
    fluidRow(
      box(
        title = HTML(paste("Heatmap", 
                           actionLink(ns("info_heatmap_plot"), label = "", icon = icon("info-circle")),downloadButton(ns('heatmap_data'),  label = "", icon = icon("save-file", lib = "glyphicon")))),
        status = "primary",
        width = 12,# Color theme
        solidHeader = TRUE,      # Gives the box a solid header
        collapsible = TRUE,      # Allows the box to be collapsed
        plotlyOutput(ns("heatmap_plot"))
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




server_shared <- function(dds ,clinical_data, id) {
  moduleServer(id, function(input, output, session) {
    
    
    #################### GENES NAMES FUNCTIONS ###################
    
    dds <- gene_names_dds(dds)
    res <- results(dds)
    
    #################### FILTERED GENES FUNCTION VIOlIN OLD ###################
    
    # filtered_data_combined <- eventReactive(input$update_plot, {
    #   # Retrieve threshold inputs
    #   padj_threshold <- input$slider_padj
    #   log2_thresholds <- input$slider_log2
    #   
    #   # Initialize a list to store the results
    #   results_list <- list()
    #   
    #   # Filtering based on p-adjusted value
    #   if (is.numeric(padj_threshold) && !is.na(padj_threshold)) {
    #     filtered_genes_padj <- res[
    #       !is.na(res$padj) & res$padj < padj_threshold, 
    #       , drop = FALSE
    #     ]
    #     filtered_genes_padj <- filtered_genes_padj[order(filtered_genes_padj$log2FoldChange, filtered_genes_padj$padj), ]
    #     results_list$padj_filtered <- filtered_genes_padj
    #   } else {
    #     results_list$padj_filtered <- NULL
    #   }
    #   
    #   # Filtering based on log2 fold change thresholds
    #   if (is.numeric(log2_thresholds) && !is.na(log2_thresholds)) {
    #     filtered_genes_foldchange <- res[
    #       !is.na(res$padj) & (res$log2FoldChange <= -log2_thresholds | res$log2FoldChange >= log2_thresholds),
    #       , drop = FALSE
    #     ]
    #     filtered_genes_foldchange <- filtered_genes_foldchange[order(filtered_genes_foldchange$log2FoldChange, filtered_genes_foldchange$padj), ]
    #     results_list$foldchange_filtered <- filtered_genes_foldchange
    #   } else {
    #     results_list$foldchange_filtered <- NULL
    #   }
    #   
    #   # Return the list of filtered data
    #   return(results_list)
    # }, ignoreNULL = FALSE)
    # 
    #################### FILTERED GENES FUNCTION ###################
    
    filter_and_order_by_padj <- function(results_data) {
      # Ensure the padj column exists
      if (!"padj" %in% names(results_data)) {
        stop("The provided data does not have a 'padj' column.")
      }

      # Filter rows where padj is not NA and order by padj
      filtered_data <- results_data[!is.na(results_data$padj), ]
      ordered_data <- filtered_data[order(filtered_data$log2FoldChange, filtered_data$padj), ]

      return(ordered_data)
    }

    filtered_res <- filter_and_order_by_padj(res)

    observe({
      updateSelectizeInput(session, "selected_gene", choices = rownames(filtered_res), server = TRUE)
    })

    #################### SELECTED GENES FUNCTION ###################
    
    
    display_genes <- reactive({
      # Get all filtered genes from the reactive
      all_genes <- filtered_res
      
      # Check if specific genes are selected
      selected_genes <- input$selected_gene
      if (!is.null(selected_genes) && all(selected_genes %in% rownames(all_genes))) {
        # Return only the selected genes
        return(all_genes[rownames(all_genes) %in% selected_genes, , drop = FALSE])
      } else {
        # Return top 5 genes as default
        return(head(all_genes, 3))
      }
    })
    
    
    #################### PLOT DATA FUNCTION ###################
    
    # Event reactive for updating the plots only when the button is clicked
    plot_data <- eventReactive(input$update_plot, {
    
      # Isolate ensures changes in these inputs do not trigger this reactive block
      # filtered_data_padj <-  filtered_data_combined()$padj_filtered
      # filtered_data_foldchange <- filtered_data_combined()$foldchange_filtered
      gene <- isolate(input$selected_gene)
      padj_cut <-isolate(input$slider_padj)
      log2_cut <-isolate(input$slider_log2)
      number <-isolate(input$number)
      
      list(
      violin= create_boxplot(dds = dds,  gene = gene, display_genes = display_genes(), padj_cut=padj_cut,log2_cut=log2_cut),
      volcano =create_volcanoplot(dds = dds, gene = gene,padj_cut=padj_cut,log2_cut=log2_cut),
      heatmap = create_heatmap(dds=dds,padj_cut=padj_cut,log2_cut=log2_cut,number=number, gene=gene))
      # correlation = create_correlation(dds=dds,padj_cut=padj_cut,log2_cut=log2_cut,number=number, gene=gene)
    })
    
    # colnames(clinical_data) <- as.character(unlist(clinical_data[2,]))
    # clinical_data <- clinical_data[-c(1, 2), ]
    # plot_mortality(clinical_data)
    # plot_gender(clinical_data)
    
    #################### PCA PLOT ###################
    
    pca_data_reactive <- reactive({
      pca_data <- pca_data(dds)  # Assuming pca_data() is a function fetching or calculating PCA data
      pca_data
    })
    
    # Generate PCA plot with Plotly
    output$pca_plot <- renderPlotly({
      req(pca_data_reactive())  # Ensure pca_data is available and only render the plot when it is
      creation_pca(pca_data_reactive())  # Assuming creation_pca() generates a Plotly plot
    })
    
    # Download handler for PCA data
    output$pca_data <- downloadHandler(
      filename = function() {
        paste("pca", "_", Sys.Date(), '.csv', sep = '')
      },
      content = function(file) {
        req(pca_data_reactive())  # Ensure data is available before attempting to write
        write.csv(pca_data_reactive(), file)
      }
    )
    
    # Observer for displaying PCA plot information using shinyalert
    observeEvent(input$info_pca_plot, {
      shinyalert(title = "PCA Plot Information", html = TRUE,
                 text = 'This is a test<br><img src="./images/violin_example.png" alt="ViolinPlot" style="width:80%;">')
    })
    
   
    #################### VIOLIN PLOT ###################
    
    output$violin_plot <- renderPlotly({
      plot_data()$violin
    })
    
    observeEvent(input$info_violin_plot, {
      shinyalert(title = "Violin Plot Information", html = TRUE,  # Enable HTML content
        text = 'This is a test<br><img src="./images/violin_example.png" alt="ViolinPlot" style="width:80%;">'
      )})
  
    
    #################### VOLCANO PLOT ###################

  
    
    output$volcano_plot <- renderPlotly({
      
      plot_data()$volcano
    })
    
    observeEvent(input$info_volcano_plot, {
      shinyalert(title = "Volcano Plot Information", html = TRUE,  # Enable HTML content
                 text = 'This is a test<br><img src="./images/violin_example.png" alt="ViolinPlot" style="width:80%;">'
      )})
    
    
    
    

    selected_genes <- reactiveVal(vector("list", 0))
    
    # Observer to handle plot clicks and update selected genes list
    observe({
      brush_data <- event_data("plotly_click")
      if (!is.null(brush_data)) {
        # Retrieve current list of selected genes
        current_genes <- selected_genes()
        new_gene <- brush_data$customdata
        
        # Add the new gene if it's not already in the list
        if (!(new_gene %in% current_genes)) {
          current_genes <- c(current_genes, new_gene)
          selected_genes(current_genes)
          
          # Update the selectize input with the new list of genes
          updateSelectizeInput(session, "selected_gene", choices = rownames(filtered_res), selected = current_genes)
        }
      } else {
        print("No points selected.")
      }
    })
    
    
    observe({
      brush_data <- event_data("plotly_selected")
      if (!is.null(brush_data)) {
        current_genes <- selected_genes()
        new_genes <- brush_data$customdata  # Array of custom data from brushed points
        print(new_genes)
        # Filter new genes to add only those not already selected
        new_genes_to_add <- new_genes[!new_genes %in% current_genes]
        if (length(new_genes_to_add) > 0) {
          current_genes <- c(current_genes, new_genes_to_add)
          selected_genes(current_genes)
          
          # Update the selectize input with the new list of genes
          updateSelectizeInput(session, "selected_gene", choices = rownames(filtered_res), selected = current_genes)
        }
      } else {
        print("No points selected.")
      }
    })
    
    
    

    #################### HEATMAP PLOT ###################
    
    output$heatmap_plot <- renderPlotly({
      plot_data()$heatmap
    })
    
    observeEvent(input$info_heatmap_plot, {
      shinyalert(title = "Volcano Plot Information", html = TRUE,  # Enable HTML content
                 text = 'This is a test<br><img src="./images/violin_example.png" alt="ViolinPlot" style="width:80%;">'
      )})
    
    
    #################### HEATMAP PLOT ###################

    

    # Filter results based on selected genes, if any
    filtered_res_table <- reactive({
      res <- as.data.frame(res)
      if (is.null(selected_genes()) || length(selected_genes()) == 0) {
        res
      } else {
        res[rownames(res) %in% selected_genes(), ]  # Filter to show only selected genes
      }
    })

   

    if (id == 'gide'){
    output$mortality <- renderPlotly({
      plot_mortality_curve(clinical_data)
      
    })
    } else {
      output$mortality <- renderPlotly({
        plot_mortality(clinical_data)
      
    })}
    
    
    
    output$filtered_results <- DT::renderDataTable({
      DT::datatable(
        filtered_res_table(),
        extensions = 'Buttons',  # Enable buttons extension for DataTables
        options = list(
          dom = 'Blfrtip',  # Define the table control elements to appear on the page: B-buttons, f-filtering input, r-processing display element, t-the table, i-table information summary, p-pagination control
          buttons = c('copy', 'csv', 'excel'), 
          pageLength = 10,
          scrollX = TRUE
        )
      )
    })
  })
}





