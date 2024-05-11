source("global.R", local = TRUE)

# calculate_correlations <- function(dds, filepath) {
#   # Obtain results and convert to dataframe
# 
#     # Filter data to keep only significant genes according to cutoffs
#   
#   res <- results(dds)
#   res.df <- as.data.frame(res)
#   
#   # Get normalized counts
#   vst_transformed <- vst(dds)
#   mat <- assay(vst_transformed)[rownames(res.df),]
#   mat <- t(mat)  # Transpose matrix so genes are rows
#   
#   # Calculate the Spearman correlation matrix and p-values
#   corr_results <- rcorr(as.matrix(mat), type = "spearman")
#   correlation_matrix <- corr_results$r
#   p_values_matrix <- corr_results$P
#   
#   # Convert the matrices to long format suitable for exporting
#   correlation_df <- as.data.frame(as.table(correlation_matrix))
#   names(correlation_df) <- c("RowGene", "ColGene", "Correlation")
#   p_values_df <- as.data.frame(as.table(p_values_matrix))
#   names(p_values_df) <- c("RowGene", "ColGene", "PValue")
#   
#   # Merge correlation and p-values data
#   result_df <- merge(correlation_df, p_values_df, by = c("RowGene", "ColGene"))
#   
#   # Save to CSV
#   write.csv(result_df, file = filepath, row.names = FALSE)
#   
#   return(paste("Correlations and p-values calculated and saved to", filepath))
# }


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



volcano_data <- function(dds){
  res <- results(dds)
  res$neg_log10_padj <- -log10(res$padj)
  # Categorize significant and non-significant genes
  res$sig <- ifelse(res$padj < padj_cut & abs(res$log2FoldChange) >= log2_cut, 
                    "Significant", "Not Significant")
  
  return(res)
}





