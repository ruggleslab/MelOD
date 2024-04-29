source("global.R", local = TRUE)



create_boxplot <- function(dds,gene , display_genes ){
  
  
  
  filtered_genes <- display_genes
  # Determine the plot title based on the selection
  if (!is.null(gene) && all(gene %in% rownames(filtered_genes))) {
    if (length(gene) == 1) {
      plot_title <- paste("Expression for", gene)  # Title for single selected gene
    } else {
      plot_title <- paste("Expression for selected genes")  # Title for multiple selected genes
    }
  } else {
    plot_title <- "Top 10 most significant genes"  # Default title
  }
  
  if (nrow(filtered_genes) > 0) {
    # Extract and prepare data for plotting
    counts <- counts(dds, normalized = TRUE)
    counts_filtered <- counts[rownames(counts) %in% rownames(filtered_genes), ]
    df <- as.data.frame(counts_filtered)
    log_norm_counts_filtered <- log(df + 0.01)
    df <- as.data.frame(log_norm_counts_filtered)
    col_data_df <- as.data.frame(colData(dds))
  
    if (ncol(df) > 1) {
      
      df_long <- df %>%
        tibble::rownames_to_column("gene_id") %>%
        tidyr::pivot_longer(
          cols = -gene_id,
          names_to = "Sample",
          values_to = "expression"
          
        )
      
    } else {
      # Handle the case with only one column (the gene itself)
      # Assume df has only one column of actual data besides the rownames
      gene_id <- gene
      sample_names <- rownames(df)
      expression_values <- as.vector(df[[1]])
      df_long <- data.frame(gene_id = rep(gene_id, length(expression_values)),
                            Sample = sample_names,
                            expression = expression_values,
                            stringsAsFactors = FALSE)
      
    }
    col_data_df <- col_data_df %>%
      rownames_to_column(var = "patient_id")
    
    merged_data <- dplyr::left_join(df_long, col_data_df, by = c("Sample" = "patient_id"))
    
    # Generate the plot with dynamic title
    plot<-plot_ly(data = merged_data, x = ~condition, y = ~expression, type = 'box',
            color = ~condition, boxpoints = 'outliers',
            jitter = 0.3, pointpos = 0,
            text = ~paste("Gene ID:", gene_id)) %>%
      layout(title = plot_title,
             yaxis = list(title = "Log CPM"),
             xaxis = list(title = "Comparison"),
             margin = list(t = 100))
  }
  return(plot)
}













create_volcanoplot <- function(res, input, gene, padj_cut, log2_cut) {
  # Create the plot
  res$neg_log10_padj <- -log10(res$padj)  # Transform p-values for plotting
 
  
  res$sig <- ifelse(res$padj < padj_cut & (abs(res$log2FoldChange) >= log2_cut), "Significant", "Not Significant")
  plot <-plot_ly(data = as.data.frame(res), x = ~log2FoldChange, y = ~neg_log10_padj, type = 'scatter', mode = 'markers',
                 color = ~sig, colors = c("#E2D4B7", "#AB82C5"),
                 text = ~paste("Gene:", rownames(res), "<br>log2 Fold Change:", log2FoldChange, "<br>Adjusted p-value:", padj),
                 marker = list(size = 7,line = list(
                   color = 'rgb(0, 0, 0)',
                   width = 1
                 ))) %>%
    layout(title = "Volcano Plot of DESeq2 Results",
           xaxis = list(title = "Log2 Fold Change"),
           yaxis = list(title = "-log10 Adjusted p-value"),
           margin = list(t = 100))
  # Check for specific genes to highlight
  if (!is.null(gene) && length(gene) > 0) {
    gene_data <- res[rownames(res) %in% gene, ]
    if (nrow(gene_data) > 0) {
      # Add highlighted genes to the plot
      plot <- add_trace(plot,
                        x = gene_data$log2FoldChange,
                        y = gene_data$neg_log10_padj,
                        type = "scatter",
                        mode = "markers", color = I("red"),                         
                        text = ~paste("Gene:", rownames(gene_data), "<br>Log2 Fold Change:", log2FoldChange, "<br>Adjusted p-value:", padj),
                        name = "Highlighted Genes")
    } else {
      # Handle case where no matching genes are found
      message("No genes found matching the specified list. Please check gene names.")
    }
  } else {
    # Handle case where no genes are specified
    message("No genes specified for highlighting.")
  }
  
  return(plot)
}
# 
# create_volcanoplot <- function(res, padj_cut, log2_cut, gene = NULL) {
#   # Categorize significant and non-significant genes
#   res$sig <- ifelse(res$padj < padj_cut & abs(res$log2FoldChange) >= log2_cut, 
#                     "Significant", "Not Significant")
#   
#   # Create the base plot with two traces for significant and non-significant genes
#   plot <- plot_ly() %>%
#     add_trace(data = subset(res, sig == "Significant"), 
#               x = ~log2FoldChange, y = ~neg_log10_padj, 
#               type = 'scatter', mode = 'markers', color = I("#AB82C5"),
#               text = ~paste("Gene:", rownames(res), "<br>Log2 Fold Change:", log2FoldChange, 
#                             "<br>Adjusted p-value:", padj),
#               marker = list(size = 7, line = list(color = 'rgb(0, 0, 0)', width = 1)),
#               name = "Significant") %>%
#     add_trace(data = subset(res, sig == "Not Significant"),
#               x = ~log2FoldChange, y = ~neg_log10_padj,
#               type = 'scatter', mode = 'markers', color = I("#E2D4B7"),
#               text = ~paste("Gene:", rownames(res), "<br>Log2 Fold Change:", log2FoldChange, 
#                             "<br>Adjusted p-value:", padj),
#               marker = list(size = 7, line = list(color = 'rgb(0, 0, 0)', width = 1)),
#               name = "Not Significant",
#               visible = "legendonly") %>%
#     layout(title = "Volcano Plot of DESeq2 Results",
#            xaxis = list(title = "Log2 Fold Change"),
#            yaxis = list(title = "-log10 Adjusted p-value"),
#            margin = list(t = 100))
#   
#   # Highlight specific genes if provided
#   if (!is.null(gene) && length(gene) > 0) {
#     gene_data <- res[rownames(res) %in% gene, ]
#     if (nrow(gene_data) > 0) {
#       plot <- add_trace(plot,
#                         data = gene_data,
#                         x = ~log2FoldChange,
#                         y = ~neg_log10_padj,
#                         type = "scatter",
#                         mode = "markers", 
#                         color = I("red"),
#                         text = ~paste("Gene:", rownames(gene_data), "<br>Log2 Fold Change:", log2FoldChange, 
#                                       "<br>Adjusted p-value:", padj),
#                         name = "Highlighted Genes")
#     } else {
#       message("No genes found matching the specified list. Please check gene names.")
#     }
#   } else {
#     message("No genes specified for highlighting.")
#   }
#   
#   return(plot)
# }







create_heatmap <- function(dds, input, gene) {
  # Set cutoffs
  log2FC_cutoff <- 0.05
  padj_cutoff <- 0.05
  
  # Obtain results and convert to dataframe
  res <- results(dds)
  res.df <- as.data.frame(res)
  
  # Filter data to keep only significant genes according to cutoffs
  res.df <- res.df[(abs(res.df$log2FoldChange) > log2FC_cutoff) & (!is.na(res.df$padj) & res.df$padj < padj_cutoff),]
  
  # Sort by padj (lowest first) and select the top 50 genes
  res.df <- res.df[order(res.df$padj), ]
  if (nrow(res.df) > 50) {
    res.df <- res.df[1:50, ]
  }
  
  # Get normalized counts for the top 50 significant genes
  mat <- assay(vst(dds))[rownames(res.df),]
  
  # Standardize the matrix (z-score)
  mat.z <- t(scale(t(mat)))
  
  # Clustering rows and columns
  dist_rows <- dist(mat.z)  # Calculating the distance between rows
  dist_cols <- dist(t(mat.z))  # Calculating the distance between columns
  hc_rows <- hclust(dist_rows)  # Clustering rows
  hc_cols <- hclust(dist_cols)  # Clustering columns
  
  # Ordering the matrix according to the clustering
  mat.z <- mat.z[hc_rows$order, hc_cols$order]
  
  # Annotations for conditions
  conditions <- colData(dds)$condition  # Assuming condition info is in colData of DESeqDataSet
  condition_colors <- scales::hue_pal()(length(unique(conditions)))  # Generate colors for conditions
  condition_mapping <- setNames(condition_colors, unique(conditions))
  annotation_colors <- list(conditions = condition_mapping)
  
  
  
  
  custom_scale <- list(
    list(0.0, "rgb(255, 245, 240)"),  # Very light pink
    list(0.2, "rgb(254, 224, 210)"),  # Light salmon
    list(0.4, "rgb(252, 187, 161)"),  # Soft pink
    list(0.6, "rgb(252, 146, 114)"),  # Warm pink
    list(0.8, "rgb(251, 106, 74)"),   # Strong pink
    list(1.0, "rgb(222, 45, 38)")     # Deep pinkish red
  )
  
  
  c <- colData(dds)[1]
  # Create an interactive heatmap with plotly
  fig <- plot_ly(x = colnames(mat.z)[hc_cols$order], y = rownames(mat.z)[hc_rows$order], z = mat.z, type = "heatmap", colorscale = "Viridis", showscale = TRUE) %>%
    layout(title = "Interactive Gene Expression Heatmap with Clustering",
           xaxis = list(title = "Samples"),
           yaxis = list(title = "Genes", autorange = "reversed"),
           margin = list(l = 100, b = 100))
  

  # Extract the condition from the DataFrame and map it to the colors
  condition_vector <- sapply(c$condition, function(x) condition_mapping[[x]])
  # Loop through each condition to create separate traces
  unique_conditions <- unique(c$condition)
  for(cond in unique_conditions) {
    # Indices for current condition
    indices <- which(c$condition == cond)
    
    fig <- add_trace(fig, x = rownames(c)[indices], y = rep("Categories", length(indices)),
                     mode = 'markers', type = 'scatter',
                     marker = list(color = condition_mapping[[cond]], size = 5, symbol = "square"),
                     
                     name = cond,  # Assign the name for the legend
                     showlegend = TRUE)
  }
  
  # Adjust the layout to ensure the legend is properly visible
  fig <- layout(fig, legend = list(x = 1.05, y = 0.5))
  
  return(fig)
}


creation_pca <- function(dds) {
  
  vsdata <- vst(dds, blind=FALSE)
  custom_colors <- c("#1E88E5", "#D81B60")  # Example colors, adjust as needed
  
  # Create PCA plot using Plotly
  pca_data <- plotPCA(vsdata, intgroup = "condition", returnData = TRUE)
  
  pca_plot <- plot_ly(
  data = pca_data,
  x = ~PC1,
  y = ~PC2,
  color = ~condition,
  text = ~condition,
  type = "scatter",
  mode = "markers",
  colors=custom_colors
) %>%
  layout(
    title = "PCA Plot",
    xaxis = list(title = "PC1",zeroline = FALSE),
    yaxis = list(title = "PC2",zeroline = FALSE)
  )

  
  return(pca_plot)  # Return the plot object
}
