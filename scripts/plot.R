source("global.R", local = TRUE)



create_boxplot <- function(dds,gene , display_genes ){


  filtered_genes <- display_genes
  
  if (nrow(filtered_genes) > 0) {
    # Extract and prepare data for plotting
    counts <- counts(dds, normalized = TRUE)
    counts_filtered <- counts[rownames(counts) %in% rownames(filtered_genes), ]
    df <- as.data.frame(counts_filtered)
    log_norm_counts_filtered <- log(df + 0.01)
    df <- as.data.frame(log_norm_counts_filtered)
    col_data_df <- as.data.frame(colData(dds))
    
    # Adjust col_data_df to only include patient_id and condition columns
    
    col_data_df <- col_data_df %>%
      rownames_to_column(var = "patient_id") %>%
      select(patient_id, condition)  # Remove that if I want other columns 
    
    
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
      gene_id <- gene
      sample_names <- rownames(df)
      expression_values <- as.vector(df[[1]])
      df_long <- data.frame(gene_id = rep(gene_id, length(expression_values)),
                            Sample = sample_names,
                            expression = expression_values,
                            stringsAsFactors = FALSE)
      
    }
    

    
    merged_data <- left_join(df_long, col_data_df, by = c("Sample" = "patient_id"))
    
    # Define a list of colors for the conditions found
    conditions_found <- unique(merged_data$condition)
    colors_chosen <- c("#D81B60", "#1E88E5")  # Example colors
    custom_colors <- setNames(colors_chosen[1:length(conditions_found)], conditions_found)
    
    
    # 
    # # Create combined variable for x-axis grouping
    # merged_data$gene_condition <- with(merged_data, paste(gene_id, condition, sep = " - "))
    # 
    # # Ordering for the plot to group by genes primarily
    # merged_data <- merged_data %>%
    #   mutate(gene_condition = factor(gene_condition, levels = unique(gene_condition[order(gene_id, condition)])))
    # 
    # 

    condition_title <- paste(" in ", paste(conditions_found, collapse=" vs "))
    
    # Determine the plot title based on the selection
    # Determine the plot title based on the selection
    if (!is.null(gene)) {
      if (length(gene) == 1) {
        plot_title <- paste("Expressions for", gene, condition_title)  # Title for single selected gene
      } else {
        plot_title <- paste("Expressions for multiple genes:", paste(gene, collapse = ", "), condition_title)
      }
    } else {
      plot_title <- "Top 3 most significant genes"  # Default title
    }
    
  
      plot <- plot_ly(merged_data,x=~gene_id, y = ~expression, color = ~factor(condition), 
                      type = "violin",colors = custom_colors, 
                      points = 'outliers',
                      box = list(visible = T),
                      meanline = list(visible = T),
                      bandwidth = 0.9,
                      text = ~paste("Gene ID:", gene_id)) %>%
        layout(title = plot_title,
               yaxis = list(title = "Log-normalized Expression"),
               xaxis = list(title = "Gene - Condition"),
               margin = list(t = 100),
               zeroline = F,
               violinmode = "group")  
  }
  
  
  return(plot)
}








create_volcanoplot <- function(dds, padj_cut, log2_cut, gene = NULL) {
  
  res <- results(dds)
  res$neg_log10_padj <- -log10(res$padj)
  # Categorize significant and non-significant genes
  res$sig <- ifelse(res$padj < padj_cut & abs(res$log2FoldChange) >= log2_cut, 
                    "Significant", "Not Significant")
  
  
  # Initialize the plotly object
  plot <- plot_ly()
  
  # Add trace for significant genes
  sig_data <- subset(res, sig == "Significant")
  plot <- add_trace(plot, data = sig_data, x = sig_data$log2FoldChange, y = sig_data$neg_log10_padj, 
                    type = 'scattergl', mode = 'markers', color = I("#1E88E5"),
                    text = paste("Gene:", rownames(sig_data), "<br>Log2 Fold Change:", sig_data$log2FoldChange, 
                                 "<br>Adjusted p-value:", sig_data$padj),
                    marker = list(size = 7, line = list(color = 'rgb(0, 0, 0)', width = 1)),
                    name = "Significant")
  
  # Add trace for non-significant genes
  non_sig_data <- subset(res, sig == "Not Significant")
  plot <- add_trace(plot, data = non_sig_data, x = non_sig_data$log2FoldChange, y = non_sig_data$neg_log10_padj, 
                    type = 'scattergl', mode = 'markers', color = I("#888888"),
                    text = paste("Gene:", rownames(non_sig_data), "<br>Log2 Fold Change:", non_sig_data$log2FoldChange, 
                                 "<br>Adjusted p-value:", non_sig_data$padj),
                    marker = list(size = 7, line = list(color = 'rgb(0, 0, 0)', width = 1)),
                    name = "Not Significant", 
                    visible='legendonly')
  
  # Check for specific genes to highlight
  if (!is.null(gene) && length(gene) > 0) {
    highlighted_genes <- subset(res, rownames(res) %in% gene)
    if (nrow(highlighted_genes) > 0) {

      plot <- add_trace(plot, data = highlighted_genes, x = highlighted_genes$log2FoldChange, y = highlighted_genes$neg_log10_padj,
                        type = "scattergl", mode = "markers", color = I("#D81B60"),
                        text = paste("Gene:", rownames(highlighted_genes), "<br>Log2 Fold Change:", highlighted_genes$log2FoldChange, 
                                     "<br>Adjusted p-value:", highlighted_genes$padj),
                        marker = list(size = 7, line = list(color = 'rgb(0, 0, 0)', width = 1)),
                        name = "Highlighted Genes")
    } else {
      message("No genes found matching the specified list. Please check gene names.")
    }
  } else {
    message("No genes specified for highlighting.")
  }
  


  unique_conditions <- unique(dds$condition)
  condition_title <- paste("Volcano plot of DESeq2 results of", paste(unique_conditions, collapse=" vs "))
  
  # Final layout adjustments
  plot <- layout(plot, title = condition_title,
                 xaxis = list(title = "Log2 Fold Change"),
                 yaxis = list(title = "-log10 Adjusted p-value"),
                 margin = list(t = 100))
  

  
  return(plot)
}




create_heatmap <- function(dds, input, padj_cut, log2_cut, number, gene = NULL) {


  # Obtain results and convert to dataframe
  res <- results(dds)
  res.df <- as.data.frame(res)
  
  # Filter data to keep only significant genes according to cutoffs
  res.df.filter <- res.df[(abs(res.df$log2FoldChange) > log2_cut) & (!is.na(res.df$padj) & res.df$padj < padj_cut),]
  

  # Sort by padj (lowest first) and select the top 50 genes
  res.df.filter <- res.df.filter[order(res.df.filter$padj), ]
  if (nrow(res.df.filter) > number) {
    res.df.filter.sub <- res.df.filter[0:number, ]
  }else{
    message("Not enought genes. Try lower number")
  }
  
  # If specific genes are provided, filter to include only these genes as well
 if (!is.null(gene)) {
   res.df.filter.sub.gene <- subset(res.df, rownames(res.df) %in% gene)
   
   
  # Merge filtered subsets (assuming you want to show genes from both criteria: significant and listed in 'gene')
  if (nrow(res.df.filter.sub.gene) > 0) {
    res.df.final <- rbind(res.df.filter.sub, res.df.filter.sub.gene)
    res.df.final <- unique(res.df.final)  # Removing potential duplicates
  } else {
    res.df.final <- res.df.filter.sub  # No genes from the specific list are in the filtered subset
  }
} else {
  res.df.final <- res.df.filter.sub  # No specific genes provided, use the top filtered genes
}


  # Get normalized counts 
  mat <- assay(vst(dds))[rownames(res.df.final),]
  
  # Standardize the matrix (z-score)
  mat.z <- t(scale(t(mat)))
  
  # Annotations for conditions
  conditions <- colData(dds)$condition 
  
  condition_colors <- scales::hue_pal()(length(unique(conditions)))  # Generate colors for conditions

  preferred_colors <- c("#D81B60","#1E88E5")
  
  # Check the number of unique conditions and match colors accordingly
  if (length(unique(conditions)) == length(preferred_colors)) {
    # Assign colors to each unique condition
    condition_mapping <- setNames(preferred_colors, unique(conditions))
  } else {
    stop("The number of conditions does not match the number of provided colors.")
  }

  conditions <- data.frame("Conditions" = conditions, check.names = F)
  
    
  # Create a vector of colors
  selection <- rep("None", nrow(mat.z))
  # Highlight selected genes
  selection[rownames(mat.z) %in% gene] <- "Selected genes"
  selection_mapping <- setNames(c("#ffffff00", "orange"), c("None", "Selected genes"))
  

  
   
  fig <- heatmaply(mat.z,
            plot_method="plotly",
            branches_lwd = 0.01,
            subplot_widths = c(0.94,0.01,0.05),
            grid_gap = 0.5,
            fontsize_row = 8,
            fontsize_col = 6,
            key.title = "Z-score",
            label_names = c("Gene", "Sample", "Z-score"),
            colors = rev(colorRampPalette(brewer.pal(3, "RdBu"))(256)),
            col_side_colors = conditions,
            col_side_palette = condition_mapping,
            row_side_colors = selection,
            row_side_palette = selection_mapping,
            colorbar_thickness = 20,


  )

  
  return(fig)
}




create_correlation <- function(dds, input, padj_cut, log2_cut, number, gene = NULL) {
  
  
  # Obtain results and convert to dataframe
  res <- results(dds)
  res.df <- as.data.frame(res)
  
  # Filter data to keep only significant genes according to cutoffs
  res.df.filter <- res.df[(abs(res.df$log2FoldChange) > log2_cut) & (!is.na(res.df$padj) & res.df$padj < padj_cut),]
  
  
  # Sort by padj (lowest first) and select the top 50 genes
  res.df.filter <- res.df.filter[order(res.df.filter$padj), ]
  if (nrow(res.df.filter) > number) {
    res.df.filter.sub <- res.df.filter[1:number, ]
  }else{
    message("Not enought genes. Try lower number")
  }
  
  # If specific genes are provided, filter to include only these genes as well
  if (!is.null(gene)) {
    res.df.filter.sub.gene <- subset(res.df, rownames(res.df) %in% gene)
    
    # Merge filtered subsets (assuming you want to show genes from both criteria: significant and listed in 'gene')
    if (nrow(res.df.filter.sub.gene) > 0) {
      res.df.final <- rbind(res.df.filter.sub, res.df.filter.sub.gene)
      res.df.final <- unique(res.df.final)  # Removing potential duplicates
    } else {
      res.df.final <- res.df.filter.sub  # No genes from the specific list are in the filtered subset
    }
  } else {
    res.df.final <- res.df.filter.sub  # No specific genes provided, use the top filtered genes
  }
  
  
  # Get normalized counts 
  mat <- assay(vst(dds))[rownames(res.df.final),]
  # Calculate the correlation matrix of the selected genes

  
  correlation_matrix <- cor(t(mat))
  
  # Calculate the matrix of p-values for correlations
  p_matrix <- outer(1:ncol(mat), 1:ncol(mat), Vectorize(function(i, j) {
    cor.test(mat[, i], mat[, j], method = "pearson")$p.value
  }))
  # Adjust p-values slightly to avoid -Inf from log10(0)
  p_matrix[p_matrix == 0] <- .Machine$double.eps
  point_sizes <- -log10(p_matrix)
  point_sizes[is.infinite(point_sizes)] <- max(point_sizes[!is.infinite(point_sizes)], na.rm = TRUE)
  
  
  
  # Create a vector of colors
  selection <- rep("None", nrow(correlation_matrix))
  # Highlight selected genes
  selection[rownames(correlation_matrix) %in% gene] <- "Selected genes"
  selection_mapping <- setNames(c("#ffffff00", "orange"), c("None", "Selected genes"))
  
  
  
  
  
  
  
  
  
  fig <- heatmaply_cor(correlation_matrix,
                   plot_method="plotly",
                   node_type = "scatter",
                   point_size_mat = point_sizes,  # use the adjusted point sizes
                   point_size_name = "-log10(p-value)",
                   label_names = c("x", "y", "Correlation"),
                   subplot_widths = c(0.94,0.01,0.05),
                   fontsize_row = 8,
                   fontsize_col = 6,
                   colors = rev(colorRampPalette(brewer.pal(3, "RdBu"))(256)),
                   colorbar_thickness = 20,
                   row_side_colors = selection,
                   row_side_palette = selection_mapping,

  )
  
  
  return(fig)
}







creation_pca <- function(dds) {

  vsdata <- vst(dds, blind=FALSE)
  custom_colors <- c("#D81B60","#1E88E5")  # Example colors, adjust as needed
  
  # Create PCA plot using Plotly
  pca_data <- plotPCA(vsdata, intgroup = "condition", returnData = TRUE)
  
  # Extract unique conditions to include in the plot title
  unique_conditions <- unique(pca_data$condition)
  condition_title <- paste("PCA plot of", paste(unique_conditions, collapse=" vs "))
  
  
  text_labels <- paste("Name:", pca_data$name, ", Condition:", pca_data$condition)
  
  pca_plot <- plot_ly(
    data = pca_data,
    x = ~PC1,
    y = ~PC2,
    color = ~condition,
    text = text_labels,
    type = "scatter",
    mode = "markers",
    colors = custom_colors
  ) %>%
    layout(
      title = condition_title,  # Use the dynamic title
      xaxis = list(title = "PC1", zeroline = FALSE),
      yaxis = list(title = "PC2", zeroline = FALSE),
      margin = list(t = 100)
    )
  

  
  callModule(downloadablePlotly, 
             id = 'pca_data', 
             plot = pca_plot, 
             filename = 'pca.csv', 
             content = function(file) {write.csv(pca_data, file)})
  
 return (pca_plot)

}




plot_mortality <- function(clinical_data){
  
  
    status_summary <- table(clinical_data$`Patient Status`)
    status_df <- as.data.frame(status_summary)
    names(status_df) <- c("Status", "Count")
    
    plot <- plot_ly(status_df, labels = ~Status, values = ~Count, type = 'pie',
            textposition = 'inside',
            textinfo = 'label+percent',
            insidetextorientation = 'radial',showlegend = FALSE) %>%
      layout(title = 'Patient Survival Status',
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             margin = list(t = 100))
return(plot)
}



plot_mortality_curve <- function(clinical_data){
  

  clinical_data$Status <- ifelse(grepl("Alive", clinical_data$Last.Followup.Status), 0, 1)
  
  # Create a survival object
  surv_obj <- Surv(time = clinical_data$Overall.Survival..Days, event = clinical_data$Status)
  
  # Fit a survival model
  fit <- survfit(surv_obj ~ 1)
  
  # Plot using plotly
  plot <- plot_ly() %>%
    add_trace(
      type = 'scatter',
      mode = 'lines+markers',
      x = fit$time, 
      y = fit$surv,
      line = list(color = 'blue')
    ) %>%
    layout(
      title = "Survival Curve",
      xaxis = list(title = "Days"),
      yaxis = list(title = "Survival Probability")
    )
  return(plot)
}




plot_gender <- function(clinical_data){
  
  
  status_summary_gender <- table(clinical_data$`Gender`)
  status_df_gender <- as.data.frame(status_summary_gender)
  
  
  names(status_df_gender) <- c("Status", "Count")
  
  
  plot <- plot_ly(status_df_gender, labels = ~Status, values = ~Count, type = 'pie',
          textposition = 'inside',
          textinfo = 'label+percent',
          insidetextorientation = 'radial',showlegend = FALSE) %>%
    layout(title = 'Patient Gender Status',
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           margin = list(t = 100))
  
  return(plot)
}

