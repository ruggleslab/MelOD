bubheat_server <- function(id, shared_reactives) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    observe({updateSelectizeInput(session, "bubheat_group_by",
                                  choices = shared_reactives$sc1conf_data()[grp == TRUE]$UI,
                                  selected = shared_reactives$sc1def_data()$grp1)
    
    updateSelectizeInput(session, "bubheat_group_by",
                                   choices = shared_reactives$sc1conf_data()[grp == TRUE]$UI,
                                   selected = shared_reactives$sc1def_data()$grp1)
    
      updateMultiInput(
        session = session,
        inputId = "bubheat_selected_gene",
        choices = names(shared_reactives$sc1gene_data()),
        selected = names(shared_reactives$sc1gene_data())[1:5])
      
})

selected_genes <- eventReactive(input$select_genes_single_cell, {
      gene_text <- input$bubheat_selected_gene_text
      genes <- unlist(strsplit(gene_text, "[,;\n]"))
      genes <- trimws(genes)
      genes <- genes[genes != ""]

      if (length(genes) > 50) {
        genes <- genes[1:50]
      }
      # Format each gene name: first letter uppercase, rest lowercase
      genes <- sapply(genes, function(gene) {
        if (nchar(gene) ==1){
          formatted_gene <- tolower(gene)
        }
        else if (nchar(gene) > 0) {
          first_char <- substr(gene, 1, 1)
          rest_chars <- substr(gene, 2, nchar(gene))
          formatted_gene <- paste0(toupper(first_char), tolower(rest_chars))
        } else {
          print(gene)
          formatted_gene <- gene
        }
        formatted_gene
      })
      genes
    })
    
  observeEvent(selected_genes(), {
    genes <- selected_genes()
    if (length(genes) > 0) {
      updateMultiInput(
        session = session,
        inputId = "bubheat_selected_gene",
        choices = names(shared_reactives$sc1gene_data()),  # Adjust this according to your data
        selected = genes
      )
    }
  })
      
  observeEvent(input$reset_selection_single_cell, {
    updateMultiInput(
      session = session,
      inputId = "bubheat_selected_gene",
      choices = names(shared_reactives$sc1gene_data()),
      selected = names(shared_reactives$sc1gene_data())[1:5])
    
    updateTextAreaInput(session, "bubheat_selected_gene_text",value = "")
  })  
  
  
    processed_data <- reactive({
      req(input$bubheat_group_by, input$bubheat_selected_gene, input$bubheat_group_by, input$cell_subset, input$cell_subset_choices_box)
      
      process_bubheat_data(shared_reactives$sc1conf_data(), shared_reactives$sc1meta_data(), input$bubheat_selected_gene, input$bubheat_group_by,
                           input$cell_subset, input$cell_subset_choices_box, shared_reactives$h5_data(), shared_reactives$sc1gene_data(), input$bubheat_scale)
    })
    
    
    
    output$bubheat_plot <- renderUI({
      result <- processed_data()
      if (is.character(result)) {
        div(class = "error-message", result)
      } else {
        bubheat_plotly(result, input$bubheat_type, input$bubheat_cluster_rows, input$bubheat_cluster_columns,input$bubheat_scale, input$bubheat_color, 800)
      }
    })
    
    setup_download_handler(id, output, "bubheat_data", reactive({processed_data()$ggMat}), "proportion_data")
    
  })
}


