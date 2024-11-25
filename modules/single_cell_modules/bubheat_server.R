bubheat_server <- function(id, shared_reactives) {
  #' BubHeat Server Module
  #'
  #' This function defines a Shiny module server for creating a bubble heatmap visualization.
  #' It manages input selections for grouping, gene selection, and plot settings, and processes
  #' the data accordingly. The module also handles user interactions, such as gene selection
  #' from text input and reset options, and provides the processed data for rendering the plot.
  #'
  #' @param id The module ID.
  #' @param shared_reactives A list of reactive objects shared across the application, including 
  #' single-cell data (`sc1conf_data`, `sc1meta_data`, `sc1gene_data`, `h5_data`).
  #'
  #' @return This module does not return a value; it is used for its side effects within a Shiny application.
  
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    blurbs <- fromJSON("./www/info_blurbs.json")
    
    observe({
      updateSelectizeInput(session, "bubheat_group_by",
                           choices = shared_reactives$sc1conf_data()[grp == TRUE]$UI,
                           selected = shared_reactives$sc1def_data()$grp1)
      
      updatePickerInput(
        session = session,
        inputId = "bubheat_selected_gene",
        choices = names(shared_reactives$sc1gene_data()),
        selected = NULL,
        options = list(
          title = "Search for a gene...",
          `live-search` = TRUE,
          size = 5,
          style = "btn-primary")
      )
      
      
      
      initial_genes <- names(shared_reactives$sc1gene_data())[1:5]
      updateTextAreaInput(session, "bubheat_selected_gene_text", value = paste(initial_genes, collapse = "\n"))
      
    })
    
    observeEvent(input$info_bubheat_plot, {
      shinyalert(
        title = blurbs$info$bubheat$title,
        html = TRUE,
        text = blurbs$info$bubheat$text
      )
    })
    
    
    observeEvent(input$bubheat_selected_gene, {
      if(nzchar(input$bubheat_selected_gene)) {
        current_value <- input$bubheat_selected_gene_text
        new_gene <- input$bubheat_selected_gene
        updated_value <- paste(current_value, new_gene, sep = "\n")
        updateTextAreaInput(session, "bubheat_selected_gene_text", value = updated_value)
      }
      updateTextInput(session, "bubheat_selected_gene", value = "")
    }, ignoreInit = TRUE)
    
    
    selected_genes <- reactive({
      gene_text <- input$bubheat_selected_gene_text
      genes <- unlist(strsplit(gene_text, "[,;\n]"))
      genes <- trimws(genes)
      genes <- genes[genes != ""]
      
      # Initialize vectors for valid and invalid genes
      valid_genes <- c()
      invalid_genes <- c()
      
      # Clear previous feedback
      hideFeedback("bubheat_selected_gene_text")
      
      # Validate genes and accumulate invalid genes
      for (gene in genes) {
        if (is_valid_gene(gene, shared_reactives$sc1gene_data())) {
          valid_genes <- c(valid_genes, gene)  # Append valid gene
        } else {
          invalid_genes <- c(invalid_genes, gene)  # Collect invalid genes
        }
      }
      
      # Show feedback warning if there are invalid genes
      if (length(invalid_genes) > 0) {
        invalid_message <- paste("Gene(s) not found:", paste(invalid_genes, collapse = ", "))
        showFeedbackWarning("bubheat_selected_gene_text", invalid_message)
      } else {
        hideFeedback("bubheat_selected_gene_text")  # Clear feedback if all genes are valid
      }
      
      # Limit the number of valid genes to 50
      if (length(valid_genes) > 50) {
        valid_genes <- valid_genes[1:50]
      }
      
    #   # Format the valid genes for output
    #   formatted_genes <- sapply(valid_genes, function(gene) {
    #     if (nchar(gene) == 1) {
    #       formatted_gene <- tolower(gene)
    #     } else if (nchar(gene) > 0) {
    #       first_char <- substr(gene, 1, 1)
    #       rest_chars <- substr(gene, 2, nchar(gene))
    #       formatted_gene <- paste0(toupper(first_char), tolower(rest_chars))
    #     } else {
    #       formatted_gene <- gene
    #     }
    #     formatted_gene
    #   })
    #   formatted_genes
    #   print(formatted_genes)
    # })
    
    # Format the valid genes for output
    formatted_genes <- sapply(valid_genes, function(gene) {
        formatted_gene <- gene
    })
    formatted_genes
  })
  
  
    
    
    observeEvent(input$reset_selection_single_cell, {
      initial_genes <- names(shared_reactives$sc1gene_data())[1:5]
      updateTextAreaInput(session, "bubheat_selected_gene_text", value = paste(initial_genes, collapse = "\n"))
    })
    
    processed_data <- reactive({
      req(input$bubheat_group_by, input$cell_subset, input$cell_subset_choices_box)
      
      process_bubheat_data(shared_reactives$sc1conf_data(), shared_reactives$sc1meta_data(), selected_genes(), input$bubheat_group_by,
                           input$cell_subset, input$cell_subset_choices_box, shared_reactives$h5_data(), shared_reactives$sc1gene_data(), input$bubheat_scale)
    })
    
    output$bubheat_plot <- renderUI({
      result <- processed_data()
      if (is.character(result)) {
        div(class = "error-message", result)
      } else {
        bubheat_plotly(result, input$bubheat_type, input$bubheat_cluster_rows, input$bubheat_cluster_columns, input$bubheat_scale, input$bubheat_color, 800)
      }
    })
    
    setup_download_handler(id, output, "bubheat_data", reactive({ processed_data()$ggMat }), "proportion_data")
  })
}