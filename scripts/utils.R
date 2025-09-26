add_significance_annotations <- function(merged_data, plot, padj_cut) {
  #' Add Significance Annotations
  #' 
  #' @description Adds significance annotations to a plot
  #' @param merged_data Data containing gene expressions, conditions, and padj values
  #' @param plot The plot object to which annotations will be added
  #' @param padj_cut Adjusted p-value cutoff
  #' 
  #' @return The plot with added significance annotations
  
  unique_genes <- unique(merged_data$gene_id)
  gene_indices <- setNames(seq_along(unique_genes), unique_genes)
  
  shape_list <- list()
  annotation_list <- list()
  
  for (gene_id in unique_genes) {
    df <- merged_data %>% filter(gene_id == !!gene_id)
    padj_value <- unique(df$padjust)
    gene_index <- gene_indices[gene_id] -1
  
    
    if (!is.na(padj_value)) {
      text_value <- if (padj_value < padj_cut) {
        paste("padj =", signif(padj_value, 3))
      } else {
        "N.S."
      }
      
      annotation_list <- append(annotation_list, list(
        list(
          x = gene_id,
          y = 1.1,
          text = text_value,
          xref = "x", yref = "paper",
          showarrow = FALSE,
          font = list(family = "Arial", size = 11)
        )
      ))
      
      shape_list <- append(shape_list, list(
        list(
          type = "line",
          line = list(color = "black", width = 1),
          x0 = gene_index - 0.2,
          y0 = 1,
          x1 = gene_index + 0.2,
          y1 = 1,
          xref = "x", yref = "paper"
        ),
        list(
          type = "line",
          line = list(color = "black", width = 1),
          x0 = gene_index - 0.2,
          y0 = 1,
          x1 = gene_index - 0.2,
          y1 = 0.99,
          xref = "x", yref = "paper"
        ),
        list(
          type = "line",
          line = list(color = "black", width = 1),
          x0 = gene_index + 0.2,
          y0 = 1,
          x1 = gene_index + 0.2,
          y1 = 0.99,
          xref = "x", yref = "paper"
        )
      ))
    }
  }
  
  plot <- plot %>%
    layout(
      annotations = annotation_list,
      shapes = shape_list
    )
  
  return(plot)
}


gene_names_dds <- function(dds) {
  #' Gene Names DDS
  #' 
  #' @description Processes DESeq2 dataset to ensure unique gene names
  #' @param dds DESeq2 dataset
  #' 
  #' @return Processed DESeq2 dataset with unique gene names
  
  if ("symbol" %in% names(mcols(dds))) {
    na_filter <- !is.na(mcols(dds)$symbol)
    dds <- dds[na_filter, ]
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
  unique_gene_symbols <- makeUniqueRowNames(gene_symbols)
  
  if (length(unique_gene_symbols) == nrow(dds)) {
    rownames(dds) <- unique_gene_symbols
  } else {
    stop("The number of unique gene symbols does not match the number of rows in the dataset.")
  }
  
  return(dds)
}


filter_and_order_by_padj <- function(results_data) {
#' Filter and Order by padj
#' 
#' @description Filters and orders genes by adjusted p-value used in selection.server
#' @param results_data Data containing results of DESeq2 analysis
#' 
#' @return Filtered and ordered data

    if (!"padj" %in% names(results_data)) {
    stop("The provided data does not have a 'padj' column.")
  }
  filtered_data <- results_data[!is.na(results_data$padj), ]
  ordered_data <- filtered_data[order(filtered_data$log2FoldChange, filtered_data$padj), ]
  return(ordered_data)
}

get_display_genes <- function(all_genes, selected_genes) {
#' Get Display Genes
#' 
#' @description Retrieves the genes to display based on the selected genes used in selection.server
#' @param all_genes All available genes
#' @param selected_genes Selected genes to display
#' 
#' @return A dataframe of genes to display
  
  if (!is.null(selected_genes) && all(selected_genes %in% rownames(all_genes))) {
    return(all_genes[rownames(all_genes) %in% selected_genes, , drop = FALSE])
  } else {
    return(head(all_genes, 3))
  }
}

setup_download_handler <- function(id, output, name, data_reactive, filename_prefix) {
  #' Download Handlers
  #' 
  #' @description Sets up the download handlers for exporting data in the server part, 
  #'              supporting lists of CSV files (zipped) or a single CSV/RDS file.
  
  output[[name]] <- downloadHandler(
    filename = function() {
      if (is.list(data_reactive)) {
        paste0(id, "_", filename_prefix, "_", Sys.Date(), ".zip")
      } else {
        paste0(id, "_", filename_prefix, "_", Sys.Date(), ifelse(is.data.frame(data_reactive()), ".csv", ".rds"))
      }
    },
    content = function(file) {
      req(data_reactive)
      
      if (is.list(data_reactive)) {
        # Create a unique temporary subdirectory to store files for this download
        temp_dir <- file.path(tempdir(), paste0("download_", id, "_", Sys.Date()))
        dir.create(temp_dir, showWarnings = FALSE)
        
        # Loop over each reactive item in the list and save as CSV
        for (name in names(data_reactive)) {
          data <- data_reactive[[name]]()
          filename <- paste0(filename_prefix, "_", name, ".csv")
          write.csv(data, file.path(temp_dir, filename))
        }
        
        # Create a zip file with all CSVs in the temporary directory
        zip::zipr(zipfile = file, files = dir(temp_dir, full.names = TRUE))
        
        # Clean up: Remove the temporary directory
        unlink(temp_dir, recursive = TRUE)
        
      } else {
        # Single file download: Save directly as CSV or RDS
        data <- data_reactive()
        print(data)
        if (is.data.frame(data)) {
          write.csv(data, file)
        } else {
          saveRDS(data, file)
        }
      }
    }
  )
}

  
  
setup_download_handler_plot <- function(id, output, name, data_reactive, filename_prefix) {
  output[[name]] <- downloadHandler(
    filename = function() {
      paste0(id, "_", filename_prefix, "_", Sys.Date(), ".svg")
    },
    content = function(file) {
      plot_object <- data_reactive()
      if (inherits(plot_object, "ggplot")) {
        ggsave(file, plot = plot_object, device = "svg")
      } else if (inherits(plot_object, "plotly")) {
        # Ensure that the necessary backend (orca or kaleido) is installed
        plotly::export(plot_object, file = file, engine = "kaleido", selenium = NULL)
      } else {
        stop("Unsupported plot object type. The plot must be a ggplot or plotly object.")
      }
    }
  )
}
