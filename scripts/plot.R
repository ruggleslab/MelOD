source("global.R", local = TRUE)

# Function to create a boxplot
create_volcanoplot <- function(res, input, gene) {
  # Create the plot
  res$neg_log10_padj <- -log10(res$padj)  # Transform p-values for plotting
  threshold <- input$slider_padj  # Get the threshold from user input
  log2_cutoffs <- input$slider_log2  # Get log2fold change cutoffs
  
  
  res$sig <- ifelse(res$padj < threshold & (res$log2FoldChange <= log2_cutoffs[1] | res$log2FoldChange >= log2_cutoffs[2]), "Significant", "Not Significant")
  plot <-plot_ly(data = as.data.frame(res), x = ~log2FoldChange, y = ~neg_log10_padj, type = 'scatter', mode = 'markers',
          color = ~sig, colors = c("#E2D4B7", "#AB82C5"),
          text = ~paste("Gene:", rownames(res), "<br>log2 Fold Change:", log2FoldChange, "<br>Adjusted p-value:", padj),
          marker = list(size = 7,line = list(
            color = 'rgb(0, 0, 0)',
            width = 1
          ))) %>%
    layout(title = "Volcano Plot of DESeq2 Results",
           xaxis = list(title = "Log2 Fold Change"),
           yaxis = list(title = "-log10 Adjusted p-value"))
 gene_data <- res[rownames(res) %in% gene, ]
 plot <- add_trace(plot, 
                    x = gene_data$log2FoldChange, 
                    y = gene_data$neg_log10_padj, 
                    type = "scatter", 
                    mode = "markers", color = I("red"), 
                    inherit = FALSE,
                    name = "Highlited genes")

 
 return(plot)
}










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
    
    fig <- add_trace(fig, x = rownames(c)[indices], y = rep(-0.5, length(indices)),
                     mode = 'markers', type = 'scatter',
                     marker = list(color = condition_mapping[[cond]], size = 5, symbol = "square"),
                     name = cond,  # Assign the name for the legend
                     showlegend = TRUE)
  }
  
  # Adjust the layout to ensure the legend is properly visible
  fig <- layout(fig, legend = list(x = 1.05, y = 0.5))
  
  return(fig)
}
