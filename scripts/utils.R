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


#' Download Handlers
#' 
#' @description Sets up the download handlers for exporting data
setup_download_handler <- function(id, output, name, data_reactive, filename_prefix) {
  output[[name]] <- downloadHandler(
    filename = function() {
      paste(id,"_", filename_prefix, "_", Sys.Date(), '.csv', sep = '')
    },
    content = function(file) {
      req(data_reactive())
      data <- data_reactive()
      write.csv(data, file)
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
#' @param tab_id The ID of the tab to observe
event_observers <- function(input, session, display_genes, filtered_res, selected_genes_plotly, new_genes) {
 

  observeEvent(input$info_violin_plot, {
    shinyalert(title = "Violin Plot Information", html = TRUE,
               text = 'This is a test<br><img src="./images/violin_example.png" alt="ViolinPlot" style="width:80%;">')
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
  current_genes <- selected_genes_plotly()
  selected_genes_plotly(unique(c(current_genes, new_genes)))
  updateSelectizeInput(session, "selected_gene", choices = isolate(rownames(filtered_res())), server = TRUE, selected = selected_genes_plotly())
  
}





