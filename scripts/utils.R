#' Add Significance Annotations
#' 
#' @description Adds significance annotations to a plot
#' @param merged_data Data containing gene expressions and conditions
#' @param plot The plot object to which annotations will be added
#' @param padj_cut Adjusted p-value cutoff
#' @return The plot with added significance annotations
add_significance_annotations <- function(merged_data, plot, padj_cut) {
  results <- merged_data %>%
    split(.$gene_id) %>%
    lapply(function(df) {
      if (nlevels(factor(df$condition)) == 2) {
        test <- t.test(expression ~ condition, data = df)
        p.value <- test$p.value
      } else {
        p.value <- NA
      }
      return(p.value)
    })
  
  p_values <- data.frame(
    gene_id = names(results),
    p_value = unlist(results)
  )
  
  # Calculate midpoints for annotations
  calculate_midpoints <- function(data) {
    unique_genes <- unique(data$gene_id)
    midpoints <- sapply(unique_genes, function(gene) {
      conditions <- unique(data$condition[data$gene_id == gene])
      if (length(conditions) == 2) {
        return(mean(c(which(unique_genes == gene) - 0.2, which(unique_genes == gene) + 0.2) - 1))
      } else {
        return(NA)
      }
    })
    return(midpoints)
  }
  
  midpoints <- calculate_midpoints(merged_data)
  shape_list <- list()
  annotation_list <- list()
  
  # Add annotations and shapes for significant p-values
  for (i in seq_along(p_values$gene_id)) {
    gene_id <- p_values$gene_id[i]
    p_value <- p_values$p_value[i]
    if (!is.na(p_value) && !is.na(midpoints[gene_id])) {
      text_value <- if (p_value < padj_cut) {
        paste("p =", signif(p_value, 3))
      } else {
        "N.S."
      }
      annotation_list[[length(annotation_list) + 1]] <- list(
        x = midpoints[gene_id],
        y = 1.1,
        text = text_value,
        xref = "x", yref = "paper",
        showarrow = FALSE,
        font = list(family = "Arial", size = 10)
      )
      
      shape_list[[length(shape_list) + 1]] <- list(
        type = "line",
        line = list(color = "black", width = 1),
        x0 = midpoints[gene_id] - 0.2,
        y0 = 1,
        x1 = midpoints[gene_id] + 0.2,
        y1 = 1,
        xref = "x", yref = "paper"
      )
      shape_list[[length(shape_list) + 1]] <- list(
        type = "line",
        line = list(color = "black", width = 1),
        x0 = midpoints[gene_id] - 0.2,
        y0 = 1,
        x1 = midpoints[gene_id] - 0.2,
        y1 = 0.99,
        xref = "x", yref = "paper"
      )
      shape_list[[length(shape_list) + 1]] <- list(
        type = "line",
        line = list(color = "black", width = 1),
        x0 = midpoints[gene_id] + 0.2,
        y0 = 1,
        x1 = midpoints[gene_id] + 0.2,
        y1 = 0.99,
        xref = "x", yref = "paper"
      )
    }
  }
  
  plot <- plot %>%
    layout(
      annotations = annotation_list,
      shapes = shape_list
    )
  
  return(plot)
}


#' Filter and Order by padj
#' 
#' @description Filters and orders genes by adjusted p-value
#' @param results_data Data containing results of DESeq2 analysis
#' @return Filtered and ordered data
filter_and_order_by_padj <- function(results_data) {
  if (!"padj" %in% names(results_data)) {
    stop("The provided data does not have a 'padj' column.")
  }
  filtered_data <- results_data[!is.na(results_data$padj), ]
  ordered_data <- filtered_data[order(filtered_data$log2FoldChange, filtered_data$padj), ]
  return(ordered_data)
}

#' Get Display Genes
#' 
#' @description Retrieves the genes to display based on the selected genes
#' @param all_genes All available genes
#' @param selected_genes Selected genes to display
#' @return A dataframe of genes to display
get_display_genes <- function(all_genes, selected_genes) {
  if (!is.null(selected_genes) && all(selected_genes %in% rownames(all_genes))) {
    return(all_genes[rownames(all_genes) %in% selected_genes, , drop = FALSE])
  } else {
    return(head(all_genes, 3))
  }
}



#' Global Selected DDS
#' 
#' @description Creates a reactive value to store the globally selected DDS
global_selected_dds <- reactiveVal()

#' Global Selected Clinical Data
#' 
#' @description Creates a reactive value to store the globally selected clinical data
global_selected_clinical_data <- reactiveVal()

#' Shared Server Utilities
#' 
#' @description Provides shared utilities for server modules
#' @param dds DESeq2 dataset
#' @return A list of utilities including processed DDS and filtered genes
shared_server_utilities <- function(dds) {
  dds_processed <- gene_names_dds(dds)
  res <- results(dds_processed)
  filtered_genes <- filter_and_order_by_padj(res)
  
  list(
    dds = dds_processed,
    filtered_genes = filtered_genes,
    display_genes = function(selected_genes) get_display_genes(filtered_genes, selected_genes)
  )
}



#' Generate PCA Data
#' 
#' @description Generates the PCA data from pca_data function(data_processing.R) from the selected DESeq2 dataset
#' @return PCA data (size factor and vsdata)
generate_pca_data <- function() {
  dds <- global_selected_dds()
  pca_data(dds)
}

#' Render PCA Plots
#' 
#' @description Renders the PCA plot
#' @param output Shiny output object
#' @param pca_data_reactive Reactive expression containing the PCA data
render_pca_plots <- function(input, output, pca_data_reactive) {
  output$pca_plot <- renderPlotly({
    req(pca_data_reactive())
    pca_data <- pca_data_reactive()$pca_data
    size_by = input$size_by
    color_by = input$color_by
    creation_pca(pca_data, size_by = size_by, color_by = color_by)
  })
}

#' Render PCA Plots
#' 
#' @description Renders the PCA plot
#' @param output Shiny output object
#' @param pca_data_reactive Reactive expression containing the PCA data
render_variance_plots <- function(output, pca_data_reactive) {
  output$variance_plot <- renderPlotly({
    req(pca_data_reactive())
    vsa_data <- pca_data_reactive()$vsdata
    variance_explained_plot(vsa_data)
  })
}


#' Download PCA Data
#' 
#' @description Sets up the download handler for PCA data
#' @param output Shiny output object
#' @param pca_data_reactive Reactive expression containing the PCA data
download_pca_data <- function(output, pca_data_reactive) {
  output$pca_data <- downloadHandler(
    filename = function() {
      paste("pca", "_", Sys.Date(), '.csv', sep = '')
    },
    content = function(file) {
      req(pca_data_reactive())
      pca_data <- pca_data_reactive()$pca_data
      write.csv(pca_data, file)
    }
  )
}


#' Event Observers for PCA
#' 
#' @description Sets up observers for PCA plot interactions
#' @param input Shiny input object
event_observers_pca <- function(input) {
  observeEvent(input$info_pca_plot, {
    shinyalert(title = "PCA Plot Information", html = TRUE,
               text = 'What Size Factors Mean:
Size factors are used in DESeq2 to normalize the count data, accounting for differences in sequencing depth and other technical biases between samples. They ensure that the comparison of gene expression levels across samples is fair and not influenced by these technical variations.')
  })
}



#' Generate Plot Data
#' 
#' @description Generates the data for the violin and volcano plots
#' @param dds Processed DESeq2 dataset
#' @param display_genes Genes to display
#' @param padj_cut Adjusted p-value cutoff
#' @param log2_cut Log2 fold change cutoff
#' @return A list containing the violin and volcano plots
generate_plot_data <- function(dds, display_genes, padj_cut, log2_cut) {
  list(
    violin = create_boxplot(dds = dds, display_genes = display_genes, padj_cut = padj_cut, log2_cut = log2_cut),
    volcano = create_volcanoplot(dds = dds, display_genes = display_genes, padj_cut = padj_cut, log2_cut = log2_cut)
  )
}

#' Render Plots
#' 
#' @description Renders the violin and volcano plots
#' @param output Shiny output object
#' @param plot_data Reactive expression containing the plot data
render_plots <- function(output, plot_data) {
  output$violin_plot <- renderPlotly({ plot_data()$violin })
  output$volcano_plot <- renderPlotly({ plot_data()$volcano })
}

#' Download Handlers
#' 
#' @description Sets up the download handlers for exporting data
#' @param output Shiny output object
#' @param display_genes Reactive expression containing the genes to display
download_handlers <- function(output, display_genes) {
  output$violin_data <- downloadHandler(
    filename = function() {
      paste("violin", "_", Sys.Date(), '.csv', sep = '')
    },
    content = function(file) {
      req(display_genes())
      write.csv(display_genes(), file)
    }
  )
}

#' Event Observers
#' 
#' @description Sets up observers for plot interactions and gene selection
#' @param input Shiny input object
#' @param session Shiny session object
#' @param display_genes Reactive expression containing the genes to display
#' @param filtered_res Reactive expression containing the filtered results
#' @param selected_genes_plotly Reactive value for selected genes
event_observers <- function(input, session, display_genes, filtered_res, selected_genes_plotly) {
  observeEvent(input$info_violin_plot, {
    shinyalert(title = "Violin Plot Information", html = TRUE,
               text = 'This is a test<br><img src="./images/violin_example.png" alt="ViolinPlot" style="width:80%;">')
  })
  
  observeEvent(event_data("plotly_click"), {
    update_selected_genes(event_data("plotly_click")$customdata, selected_genes_plotly, session, filtered_res)
  })
  
  observeEvent(event_data("plotly_selected"), {
    update_selected_genes(event_data("plotly_selected")$customdata, selected_genes_plotly, session, filtered_res)
  })
  
  observeEvent(input$selected_gene, {
    if (length(input$selected_gene) == 0) {
      selected_genes_plotly(character(0))
    }
  })
}

#' Update Selected Genes
#' 
#' @description Updates the selected genes based on plot interactions
#' @param new_genes New genes selected from the plot
#' @param selected_genes_plotly Reactive value for selected genes
#' @param session Shiny session object
#' @param filtered_res Reactive expression containing the filtered results
update_selected_genes <- function(new_genes, selected_genes_plotly, session, filtered_res) {
  if (is.null(new_genes)) return
  current_genes <- selected_genes_plotly()
  selected_genes_plotly(unique(c(current_genes, new_genes)))
  updateSelectizeInput(session, "selected_gene", choices = isolate(rownames(filtered_res())), selected = selected_genes_plotly())
}


#' Render Filtered Results Table
#' 
#' @description Renders the filtered results table based on the selected genes
#' @param dds_processed Reactive expression containing the processed DESeq2 dataset
#' @param input Shiny input object
#' @param selected_genes_plotly Reactive value for selected genes
#' @return A datatable containing the filtered results
render_filtered_results_table <- function(dds_processed, input, selected_genes_plotly) {
  DT::renderDataTable({
    res <- results(dds_processed())
    res <- as.data.frame(res)
    if (!is.null(input$selected_gene) && length(input$selected_gene) > 0)
      res <- res[rownames(res) %in% input$selected_gene, ]
    DT::datatable(res, extensions = 'Buttons', options = list(
      dom = 'Blrtip',
      buttons = c('copy', 'csv', 'excel'),
      pageLength = 10,
      scrollX = TRUE
    ))
  })
}



#' Generate Heatmap Plot Data
#' 
#' @description Generates the data for the heatmap plot
#' @param dds Processed DESeq2 dataset
#' @param padj_cut Adjusted p-value cutoff
#' @param log2_cut Log2 fold change cutoff
#' @param number Number of top genes to display
#' @param gene Specific gene to display
#' @return A list containing the heatmap plot
generate_heatmap_plot_data <- function(dds, padj_cut, log2_cut, number, gene) {
  list(
    heatmap = create_heatmap(dds = dds, padj_cut = padj_cut, log2_cut = log2_cut, number = number, gene = gene)
  )
}

#' Render Heatmap Plots
#' 
#' @description Renders the heatmap plot
#' @param output Shiny output object
#' @param plot_data Reactive expression containing the plot data
render_heatmap_plots <- function(output, plot_data) {
  output$heatmap_plot <- renderPlotly({ plot_data()$heatmap })
}

#' Event Observers for Heatmap
#' 
#' @description Sets up observers for plot interactions
#' @param input Shiny input object
event_observers_heatmap <- function(input) {
  observeEvent(input$info_heatmap_plot, {
    shinyalert(title = "Heatmap Plot Information", html = TRUE,
               text = 'This is a test<br><img src="./images/violin_example.png" alt="ViolinPlot" style="width:80%;">')
  })
}
