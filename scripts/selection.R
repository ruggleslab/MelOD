selection_badal_server <- function(dds, clinical_data, id) {
  #' Selection Badal Server
  #' 
  #' @description Handles selection for the Badal input module, ensuring that the required input selection is available before proceeding.
  #' @param dds A DESeq2 dataset containing the data to be processed.
  #' @param clinical_data A data frame containing clinical data associated with the DESeq2 dataset.
  #' @param id A unique module ID used to identify the module in the Shiny application.
  
  moduleServer(id, function(input, output, session) {
    observe({
      req(input$selection_badal)  # Ensure that input selection is available before proceeding
    })
  })
}

selection_list_server <- function(dds, clinical_data, id, shared_reactives) {
  #' Selection List Server
  #' 
  #' @description Handles selection for list input, updating the shared reactive variables based on the user's selection.
  #' @param dds A list of DESeq2 datasets for different treatment types.
  #' @param clinical_data A list of clinical data frames corresponding to each DESeq2 dataset.
  #' @param id A unique module ID used to identify the module in the Shiny application.
  #' @param shared_reactives A reactiveValues object for sharing reactive variables across modules.
  
  moduleServer(id, function(input, output, session) {
    observe({
      req(input$selection)  # Ensure that input selection is available before proceeding
      selected_dds <- NULL
      selected_clinical_data <- NULL
      
      if (input$selection == 1) {
        selected_dds <- dds[[1]]
        selected_clinical_data <- clinical_data[[1]]
      } else if (input$selection == 2) {
        selected_dds <- dds[[2]]
        selected_clinical_data <- clinical_data[[2]]
      }
      
      if (!is.null(selected_dds)) {
        shared_reactives$selected_dds(selected_dds)  # Update the shared reactive value for dds
        shared_reactives$selected_clinical_data(selected_clinical_data)  # Update the shared reactive value for clinical data
      }
    })
  })
}

selection_server <- function(dds, clinical_data, id) {
  #' Selection Server
  #' 
  #' @description Handles the overall selection logic for the application, including processing and filtering of the DESeq2 dataset based on the user's selection.
  #' @param dds A list of DESeq2 datasets for different treatment types.
  #' @param clinical_data A list of clinical data frames corresponding to each DESeq2 dataset.
  #' @param id A unique module ID used to identify the module in the Shiny application.
  #' 
  #' @return A list of reactive values and utilities including selected datasets, filtered results, processed datasets, and display genes.
  
  moduleServer(id, function(input, output, session) {
    selected_dds <- reactiveVal()
    selected_clinical_data <- reactiveVal()
    utilities <- reactive({
      req(selected_dds())
      shared_server_utilities(selected_dds())
    })
    filtered_res <- reactive({
      req(utilities())
      utilities()$filtered_genes
    })
    dds_processed <- reactive({
      req(utilities())
      utilities()$dds
    })
    display_genes <- reactive({
      req(filtered_res())
      get_display_genes(filtered_res(), input$selected_gene)
    })
    
    observe({
      if (length(dds) > 1) {
        selection_list_server(dds, clinical_data, id, shared_reactives)  # Pass shared_reactives
      } else {
        selected_dds(dds[[1]])  # Default to the first dataset if only one is available
        selected_clinical_data(clinical_data[[1]])
      }
    })
    return(list(
      selected_dds = selected_dds,
      selected_clinical_data = selected_clinical_data,
      utilities = utilities,
      filtered_res = filtered_res,
      dds_processed = dds_processed,
      display_genes = display_genes
    ))
  })
}

shared_server_utilities <- function(dds) {
  #' Shared Server Utilities
  #' 
  #' @description Provides shared utilities for server modules, including processing of the DESeq2 dataset and filtering of genes.
  #' @param dds A DESeq2 dataset to be processed and analyzed.
  #' 
  #' @return A list of utilities including the processed DESeq2 dataset and filtered genes.
  
  dds_processed <- tryCatch({
    gene_names_dds(dds)
  }, error = function(e) {
    dds
  })

  
  res <- tryCatch({
    results(dds_processed)  # Try to get results if it's a DESeq2 object
  }, error = function(e) {
    # If results() fails, use the dds object directly (assuming it is already in a results-like format)
    message("Warning: 'results()' function failed. Using dds directly.")
    dds_processed
  })  
  
  filtered_genes <- filter_and_order_by_padj(res)
  
  list(
    dds = dds_processed,
    filtered_genes = filtered_genes,
    display_genes = function(selected_genes) get_display_genes(filtered_genes, selected_genes)
  )
}

selection_list_single_cell_server <- function(sc1conf, sc1def, h5_file, sc1gene, sc1meta, id, shared_reactives) {
  #' Selection List Single Cell Server
  #' 
  #' @description Handles selection for single-cell data, updating the shared reactive variables based on the user's selection.
  #' @param sc1conf A list of configuration data for single-cell datasets.
  #' @param sc1def A list of default settings for single-cell datasets.
  #' @param h5_file A list of HDF5 files corresponding to the single-cell datasets.
  #' @param sc1gene A list of gene expression data for the single-cell datasets.
  #' @param sc1meta A list of metadata for the single-cell datasets.
  #' @param id A unique module ID used to identify the module in the Shiny application.
  #' @param shared_reactives A reactiveValues object for sharing reactive variables across modules.
  
  moduleServer(id, function(input, output, session) {
    
    observe({
      req(input$selection)
      sc1conf_data = NULL
      sc1def_data = NULL
      sc1gene_data = NULL
      sc1meta_data = NULL
      h5_data = NULL
      
      if ("TBPT" %in% input$selection) {
        sc1conf_data <- sc1conf[[1]]
        sc1def_data <- sc1def[[1]]
        sc1gene_data <- sc1gene[[1]]
        sc1meta_data <- sc1meta[[1]]
        h5_data <- h5_file[[1]]
      } else if ("KBPT" %in% input$selection) {
        sc1conf_data <- sc1conf[[2]]
        sc1def_data <- sc1def[[2]]
        sc1gene_data <- sc1gene[[2]]
        sc1meta_data <- sc1meta[[2]]
        h5_data <- h5_file[[2]]
      }
      if (!is.null(sc1conf_data)) {
        shared_reactives$sc1conf_data(sc1conf_data)
        shared_reactives$sc1def_data(sc1def_data)
        shared_reactives$sc1gene_data(sc1gene_data)
        shared_reactives$sc1meta_data(sc1meta_data)
        shared_reactives$h5_data(h5_data)
      }
    })
  })
}

selection_server_single_cell <- function(sc1conf, sc1def, h5_file, sc1gene, sc1meta, id) {
  #' Selection Server Single Cell
  #' 
  #' @description Handles the overall selection logic for single-cell data, including processing and filtering of the datasets based on the user's selection.
  #' @param sc1conf A list of configuration data for single-cell datasets.
  #' @param sc1def A list of default settings for single-cell datasets.
  #' @param h5_file A list of HDF5 files corresponding to the single-cell datasets.
  #' @param sc1gene A list of gene expression data for the single-cell datasets.
  #' @param sc1meta A list of metadata for the single-cell datasets.
  #' @param id A unique module ID used to identify the module in the Shiny application.
  #'
  #' @return A list of reactive values including the selected configuration data, default settings, gene expression data, metadata, and HDF5 file.
  
  moduleServer(id, function(input, output, session) {
    
    sc1conf_data <- reactiveVal()
    sc1def_data <- reactiveVal()
    sc1gene_data <- reactiveVal()
    sc1meta_data <- reactiveVal()
    h5_data <- reactiveVal()
    
    observe({
      if (length(sc1conf) > 1) {
        selection_list_single_cell_server(sc1conf, sc1def, h5_file, sc1gene, sc1meta, id, shared_reactives)
      } else {
        sc1conf_data(sc1conf[[1]])
        sc1def_data(sc1def[[1]])
        sc1gene_data(sc1gene[[1]])
        sc1meta_data(sc1meta[[1]])
        h5_data(h5_file[[1]])
      }
    })
    
    return(list(
      sc1conf_data = sc1conf_data,
      sc1def_data = sc1def_data,
      sc1gene_data = sc1gene_data,
      sc1meta_data = sc1meta_data,
      h5_data = h5_data
    ))
  })
}


