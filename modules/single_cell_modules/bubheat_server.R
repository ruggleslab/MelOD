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
      
      updateSelectizeInput(session, "bubheat_group_by",
                           choices = shared_reactives$sc1conf_data()[grp == TRUE]$UI,
                           selected = shared_reactives$sc1def_data()$grp1)
      
      updateMultiInput(
        session = session,
        inputId = "bubheat_selected_gene",
        choices = names(shared_reactives$sc1gene_data()),
        selected = names(shared_reactives$sc1gene_data())[1:5])
    })
    
    observeEvent(input$info_bubheat_plot, {
      shinyalert(
        title = blurbs$info$bubheat$title, 
        html = TRUE,
        text = blurbs$info$bubheat$text
      )
    })
    
    selected_genes <- eventReactive(input$select_genes_single_cell, {
      gene_text <- input$bubheat_selected_gene_text
      genes <- unlist(strsplit(gene_text, "[,;\n]"))
      genes <- trimws(genes)
      genes <- genes[genes != ""]
      
      if (length(genes) > 50) {
        genes <- genes[1:50]
      }
      
      genes <- sapply(genes, function(gene) {
        if (nchar(gene) == 1) {
          formatted_gene <- tolower(gene)
        } else if (nchar(gene) > 0) {
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
      
      updateTextAreaInput(session, "bubheat_selected_gene_text", value = "")
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
        bubheat_plotly(result, input$bubheat_type, input$bubheat_cluster_rows, input$bubheat_cluster_columns, input$bubheat_scale, input$bubheat_color, 800)
      }
    })
    
    setup_download_handler(id, output, "bubheat_data", reactive({processed_data()$ggMat}), "proportion_data")
    
  })
}
