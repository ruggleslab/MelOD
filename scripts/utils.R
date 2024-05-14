source("global.R", local = TRUE)




# Main plotting function
add_significance_annotations <- function(merged_data, plot,padj_cut) {
 
  
  results <- merged_data %>%
    split(.$gene_id) %>%
    lapply(function(df) {
      if (nlevels(factor(df$condition)) == 2) {
        test <- wilcox.test(expression ~ condition, data = df)
        p.value <- test$p.value
      } else {
        p.value <- NA  # More than two conditions or other issues
      }
      return(p.value)
    })
  
  p_values <- data.frame(
    gene_id = names(results),
    p_value = unlist(results)
  )
  
  
  # Function to calculate midpoints between groups
  calculate_midpoints <- function(data) {
    unique_genes <- unique(data$gene_id)
    midpoints <- setNames(numeric(length(unique_genes)), unique_genes)
    
    for (gene in unique_genes) {
      conditions <- unique(data$condition[data$gene_id == gene])
      if (length(conditions) == 2) {
        
        midpoints[gene] <- mean(c(which(unique_genes == gene) - 0.2, which(unique_genes == gene) + 0.2)-1)
      } else {
        midpoints[gene] <- NA  # Handle other cases or skip
      }
    }
    return(midpoints)
  }
  
  # Calculate midpoints based on your merged_data
  midpoints <- calculate_midpoints(merged_data)
  shape_list <- list()
  annotation_list <- list()
  # Assuming 'plot' is already initialized and setup as you described
  for (i in 1:nrow(p_values)) {
    if (!is.na(p_values$p_value[i]) && !is.na(midpoints[p_values$gene_id[i]])) {
      text_value <- if (p_values$p_value[i] < padj_cut) {
        paste("p =", signif(p_values$p_value[i], 3))
      } else {
        "N.S."  # Not significant
      }
      annotation_list[[length(annotation_list)+1]] <-  list(
        x = midpoints[p_values$gene_id[i]],
        y = 1.1,
        text = text_value,
        xref = "x", yref = "paper",
        showarrow = FALSE,
        font = list(family = "Arial", size = 10)
      )
            
      shape_list[[length(shape_list) + 1]] <- list(
        type = "line",
        line = list(color = "black", width = 1),
        x0 = midpoints[p_values$gene_id[i]] - 0.2,
        y0 = 1,
        x1 = midpoints[p_values$gene_id[i]] + 0.2,
        y1 = 1,
        xref = "x", yref = "paper"
      )
      shape_list[[length(shape_list) + 1]] <- list(
        type = "line",
        line = list(color = "black", width = 1),
        x0 = midpoints[p_values$gene_id[i]] - 0.2,
        y0 = 1,
        x1 = midpoints[p_values$gene_id[i]] - 0.2,
        y1 = 0.99,
        xref = "x", yref = "paper"
      )
      shape_list[[length(shape_list) + 1]] <- list(
        type = "line",
        line = list(color = "black", width = 1),
        x0 = midpoints[p_values$gene_id[i]] + 0.2,
        y0 = 1,
        x1 = midpoints[p_values$gene_id[i]] + 0.2,
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



#################### PCA DATA ###################


pca_data <- function(dds){
vsdata <- vst(dds, blind=FALSE)
# Create PCA plot using Plotly
pca_data <- plotPCA(vsdata, intgroup = "condition", returnData = TRUE)
return(pca_data)
}


gene_names_dds <- function(dds){
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
  dds
  
}




# Helper function to filter and order genes
filter_and_order_by_padj <- function(results_data) {
  if (!"padj" %in% names(results_data)) {
    stop("The provided data does not have a 'padj' column.")
  }
  filtered_data <- results_data[!is.na(results_data$padj), ]
  ordered_data <- filtered_data[order(filtered_data$log2FoldChange, filtered_data$padj), ]
  return(ordered_data)
}

# Function to display selected genes
get_display_genes <- function(all_genes, selected_genes) {
  if (!is.null(selected_genes) && all(selected_genes %in% rownames(all_genes))) {
    return(all_genes[rownames(all_genes) %in% selected_genes, , drop = FALSE])
  } else {
    return(head(all_genes, 3))  # Or adjust the default count
  }
}




calculate_correlations <- function(dds, filepath) {
  
  dds <- gene_names_dds(dds)
  res <- results(dds)
  res.df <- as.data.frame(res)
  
  # Get normalized counts
  vst_transformed <- vst(dds)
  mat <- assay(vst_transformed)[rownames(res.df),]
  mat <- t(mat)  # Transpose matrix so genes are rows
  
  # Calculate the Spearman correlation matrix and p-values
  corr_results <- rcorr(as.matrix(mat), type = "spearman")
  correlation_matrix <- corr_results$r
  p_values_matrix <- corr_results$P
  
  # Convert the matrices to long format suitable for exporting
  correlation_df <- as.data.frame(as.table(correlation_matrix))
  names(correlation_df) <- c("RowGene", "ColGene", "Correlation")
  p_values_df <- as.data.frame(as.table(p_values_matrix))
  names(p_values_df) <- c("RowGene", "ColGene", "PValue")
  
  # Merge correlation and p-values data
  result_df <- merge(correlation_df, p_values_df, by = c("RowGene", "ColGene"))
  
  # Save to CSV
  write.csv(result_df, file = filepath, row.names = FALSE)
  
  return(paste("Correlations and p-values calculated and saved to", filepath))
}

