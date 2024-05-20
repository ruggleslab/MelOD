source("global.R", local = TRUE)

#' Create PCA Plot
#' 
#' @description Creates a PCA plot for the given data
#' @param pca_data Data for PCA plot, including size factors
#' @param size_by Optional parameter to set dot size by size factor ("size_factor") or constant size ("constant")
#' @param color_by Optional parameter to color dots by "condition" or "name"
#' @return A plotly object representing the PCA plot
creation_pca <- function(pca_data, size_by = "constant", color_by = "condition") {
  unique_conditions <- unique(pca_data$condition)
  condition_title <- paste("PCA plot of", paste(unique_conditions, collapse = " vs "))
  
  # Update text labels to include size factor information
  text_labels <- paste("Name:", pca_data$name, 
                       "<br>Condition:", pca_data$condition, 
                       "<br>Size Factor:", round(pca_data$size_factor, 3))
  
  custom_colors <- c("#D81B60", "#1E88E5")
  
  # Set dot size based on size factor or constant
  dot_size <- if (size_by == "constant") {
    7  # Default size
  } else {
    pca_data$size_factor * 5  # Adjust multiplier as needed for appropriate sizing
  
  }
  
  # Set color by condition or sample
  if (color_by == "condition") {
    color_column <- pca_data$condition
    custom_colors <- c("#D81B60", "#1E88E5")
  } else {
    color_column <- pca_data$name
    num_name <- length(unique(pca_data$name))
    custom_colors <- colorRampPalette(brewer.pal(9, "Set1"))(num_name)
  }
  
  
  pca_plot <- plot_ly(
    data = pca_data,
    x = ~PC1,
    y = ~PC2,
    color = color_column,
    text = text_labels,
    colors = custom_colors,
    marker = list(size = dot_size,
                  line = list(color = "black", width = 1))    
  ) %>%
    layout(
      title = condition_title,  # Use the dynamic title
      xaxis = list(title = "PC1", zeroline = FALSE),
      yaxis = list(title = "PC2", zeroline = FALSE),
      margin = list(t = 100)
    )
  
  return(pca_plot)
}


#' Variance Explained Plot
#' 
#' @description Creates a bar plot of variance explained by the principal components
#' @param vsdata Variance stabilized transformed data from DESeq2
#' @return A plotly object representing the variance explained by each principal component
variance_explained_plot <- function(vsdata) {
  # Perform PCA on the variance stabilized data
  pca_result <- prcomp(t(assay(vsdata)))
  
  # Calculate the proportion of variance explained
  explained_variance <- pca_result$sdev^2 / sum(pca_result$sdev^2)
  
  # Create a data frame for plotting
  variance_df <- data.frame(
    PC = factor(paste0("PC", 1:length(explained_variance)), levels = paste0("PC", 1:length(explained_variance))),
    VarianceExplained = explained_variance
  )
  
  # Create the plotly plot
  plot <- plot_ly(
    data = variance_df,
    x = ~PC,
    y = ~VarianceExplained,
    type = 'bar',
    text = ~paste(round(VarianceExplained * 100, 2), "%"),
    textposition = 'auto',
    marker = list(color = 'rgba(50, 171, 96, 0.7)',
                  line = list(color = 'rgba(50, 171, 96, 1.0)', width = 1.5))
  ) %>%
    layout(
      title = "Variance Explained by Principal Components",
      xaxis = list(title = "Principal Components"),
      yaxis = list(title = "Variance Explained", tickformat = ".2%"),
      margin = list(t = 100)
    )
  
  return(plot)
}



#' Plot Gender
#' 
#' @description Plots the gender data
#' @param clinical_data Clinical data for the plot
#' @return A plotly object representing the gender plot
plot_gender <- function(clinical_data) {
  
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
}


#' Plot Mortality Curve
#' 
#' @description Plots the mortality curve
#' @param clinical_data Clinical data for the plot
#' @return A plotly object representing the mortality curve
plot_mortality_curve <- function(clinical_data) {
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
      line = list(color = "#10528b")
    ) %>%
    layout(
      title = "Survival Curve",
      xaxis = list(title = "Days"),
      yaxis = list(title = "Survival Probability",range = c(0, 1))
    )
  return(plot)
}


#' Create Boxplot
#' 
#' @description Creates a boxplot for the given data
#' @param dds Processed DESeq2 dataset
#' @param display_genes Genes to display
#' @param padj_cut Adjusted p-value cutoff
#' @param log2_cut Log2 fold change cutoff
#' @return A plotly object representing the boxplot
create_boxplot <- function(dds, display_genes, padj_cut, log2_cut) {
  if (length(display_genes) > 0) {
    # Ensure display_genes is a data frame
    if (!is.data.frame(display_genes)) {
      display_genes <- data.frame(gene_id = display_genes, stringsAsFactors = FALSE)
    }
    # Process and filter data
    gene <- rownames(display_genes)
    counts <- counts(dds, normalized = TRUE)
    counts_filtered <- counts[rownames(counts) %in% gene, , drop = FALSE]
    df <- as.data.frame(log(counts_filtered + 0.01))
    
    # Prepare sample metadata
    col_data_df <- as.data.frame(colData(dds))
    col_data_df <- col_data_df %>%
      rownames_to_column(var = "patient_id") %>%
      select(patient_id, condition)
    
    # Reshape data for plotting
    df_long <- df %>%
      tibble::rownames_to_column("gene_id") %>%
      tidyr::pivot_longer(cols = -gene_id, names_to = "Sample", values_to = "expression")
    
    merged_data <- left_join(df_long, col_data_df, by = c("Sample" = "patient_id"))
    
    # Define colors
    conditions_found <- unique(merged_data$condition)
    colors_chosen <- c("#D81B60", "#1E88E5")
    colors_chosen_darker <- c("#82103a", "#10528b")
    custom_colors <- setNames(colors_chosen[1:length(conditions_found)], conditions_found)
    
    # Generate plot title
    condition_title <- paste(" in ", paste(conditions_found, collapse=" vs "))
    plot_title <- if (length(gene) == 1) {
      paste("Expressions for", gene, condition_title)
    } else {
      paste("Expressions for multiple genes:", paste(gene, collapse = ", "), condition_title)
    }
    
    # Create plot
    plot <- plot_ly(merged_data, x = ~gene_id, y = ~expression, color = ~factor(condition), 
                    type = "violin", colors = custom_colors, points = 'outliers',
                    marker = list(color = colors_chosen_darker, size = 5, line = list(width = 0)),
                    line = list(width = 1),
                    box = list(visible = TRUE, fillcolor = colors_chosen_darker, line = list(width = 1, color = colors_chosen_darker)),
                    meanline = list(visible = TRUE),
                    bandwidth = 0.9,
                    text = ~paste("Gene ID:", gene_id)) %>%
      layout(title = plot_title,
             yaxis = list(title = "Log-normalized Expression"),
             xaxis = list(title = "Gene - Condition",
                          titlefont = list(size = 14),
                          tickfont = list(family = "Arial", size = 10, color = "black")),
             margin = list(t = 100),
             zeroline = FALSE,
             violingap = 0.2,
             violingroupgap = 0.1,
             violinmode = "group")
    
    plot <- add_significance_annotations(merged_data, plot, padj_cut)
    
    return(plot)
  } else {
    message("No genes found for display.")
  }
}


#' Create Volcanoplot
#'
#' @description Creates a volcanoplot for the given data
#' @param dds Processed DESeq2 dataset
#' @param display_genes Genes to display
#' @param padj_cut Adjusted p-value cutoff
#' @param log2_cut Log2 fold change cutoff
#' @return A plotly object representing the volcanoplot
create_volcanoplot <- function(dds, padj_cut, log2_cut, display_genes) {

  res <- results(dds)
  res$neg_log10_padj <- -log10(res$padj)

  gene <- rownames(display_genes)

  # Categorize genes
  res$sig <- ifelse(res$padj < padj_cut & abs(res$log2FoldChange) >= log2_cut,
                    ifelse(res$log2FoldChange > 0, "Upregulated", "Downregulated"),
                    "Not Significant")


  plot <- plot_ly()
  

   # Add trace for upregulated genes
  upregulated_data <- subset(res, sig == "Upregulated")
  plot <- add_trace(plot, data = upregulated_data, x = upregulated_data$log2FoldChange, y = upregulated_data$neg_log10_padj,
                    type = 'scattergl', mode = 'markers', color = I("#D81B60"),
                    text = paste("Gene:", rownames(upregulated_data), "<br>Log2 Fold Change:", upregulated_data$log2FoldChange,
                                 "<br>Adjusted p-value:", upregulated_data$padj),
                    marker = list(size = 7, line = list(color = "#82103a", width = 1)),
                    customdata = rownames(upregulated_data),
                    name = "Upregulated")

   # Add trace for downregulated genes
  downregulated_data <- subset(res, sig == "Downregulated")
  plot <- add_trace(plot, data = downregulated_data, x = downregulated_data$log2FoldChange, y = downregulated_data$neg_log10_padj,
                    type = 'scattergl', mode = 'markers', color = I("#1E88E5"),
                    text = paste("Gene:", rownames(downregulated_data), "<br>Log2 Fold Change:", downregulated_data$log2FoldChange,
                                 "<br>Adjusted p-value:", downregulated_data$padj),
                    marker = list(size = 7, line = list(color = "#10528b", width = 1)),
                    customdata = rownames(downregulated_data),
                    name = "Downregulated")
#
  # Add trace for non-significant genes
  non_sig_data <- subset(res, sig == "Not Significant")
  plot <- add_trace(plot, data = non_sig_data, x = non_sig_data$log2FoldChange, y = non_sig_data$neg_log10_padj,
                    type = 'scattergl', mode = 'markers', color = I("gray"),
                    text = paste("Gene:", rownames(non_sig_data), "<br>Log2 Fold Change:", non_sig_data$log2FoldChange,
                                 "<br>Adjusted p-value:", non_sig_data$padj),
                    marker = list(size = 7, line = list(color = "gray", width = 1)),
                    name = "Not Significant",
                    customdata = rownames(non_sig_data),
                    visible='legendonly')




  if (!is.null(gene) && length(gene) > 0) {
    highlighted_genes <- subset(res, rownames(res) %in% gene)
    if (nrow(highlighted_genes) > 0) {

      # Add gene names above the dots for highlighted genes
      plot <- add_trace(plot, data = highlighted_genes, x = highlighted_genes$log2FoldChange, y = highlighted_genes$neg_log10_padj,
                        type = "scattergl", mode = "markers", color = I("orange"),
                        text = paste("Gene:", rownames(highlighted_genes), "<br>Log2 Fold Change:", highlighted_genes$log2FoldChange,
                                     "<br>Adjusted p-value:", highlighted_genes$padj),
                        hoverinfo = 'text',
                        marker = list(size = 7, line = list(color = "darkorange", width = 1)),
                        name = "Highlighted Genes")

      # Add gene names above the dots for highlighted genes with reduced font size
      plot <- add_trace(plot, data = highlighted_genes, x = highlighted_genes$log2FoldChange, y = highlighted_genes$neg_log10_padj,
                        type = "scattergl", mode = "text", text = rownames(highlighted_genes),
                        textposition = "top center", textfont = list(size = 10, color = "black"),
                        hoverinfo = 'none',
                        showlegend = FALSE)
    } else {
      message("No genes found matching the specified list. Please check gene names.")
    }
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


#' Create Heatmap
#' 
#' @description Creates a heatmap for the given data
#' @param dds Processed DESeq2 dataset
#' @param padj_cut Adjusted p-value cutoff
#' @param log2_cut Log2 fold change cutoff
#' @param number Number of top genes to display
#' @param gene Specific gene to display
#' @return A plotly object representing the heatmap
create_heatmap <- function(dds, padj_cut, log2_cut, number, gene) {
  # Obtain results and convert to dataframe
  res <- results(dds)
  res.df <- as.data.frame(res)
  
  # Filter data to keep only significant genes according to cutoffs
  res.df.filter <- res.df[(abs(res.df$log2FoldChange) > log2_cut) & (!is.na(res.df$padj) & res.df$padj < padj_cut),]
  
  # Sort by padj (lowest first) and select the top genes
  res.df.filter <- res.df.filter[order(res.df.filter$padj), ]
  if (nrow(res.df.filter) > number) {
    res.df.filter <- res.df.filter[0:number, ]
  } else {
    message("Not enough genes. Try a lower number.")
  }
  
  # If specific genes are provided, filter to include only these genes as well
  if (!is.null(gene)) {
    res.df.gene <- subset(res.df, rownames(res.df) %in% gene)
    res.df.final <- unique(rbind(res.df.filter, res.df.gene))
  } else {
    res.df.final <- res.df.filter
  }
  
  # Get normalized counts and standardize the matrix (z-score)
  mat <- assay(vst(dds))[rownames(res.df.final),]
  mat.z <- t(scale(t(mat)))
  
  # Annotations for conditions
  conditions <- colData(dds)$condition
  condition_levels <- unique(conditions)
  preferred_colors <- c("#D81B60", "#1E88E5")
  
  if (length(condition_levels) <= length(preferred_colors)) {
    condition_colors <- setNames(preferred_colors[1:length(condition_levels)], condition_levels)
  } else {
    condition_colors <- scales::hue_pal()(length(condition_levels))
    condition_colors <- setNames(condition_colors, condition_levels)
  }
  
  conditions_df <- data.frame("Conditions" = conditions, check.names = FALSE)
  
  # Create a vector of colors for selected genes
  selection <- ifelse(rownames(mat.z) %in% gene, "Selected genes", "None")
  selection_mapping <- setNames(c("#ffffff00", "orange"), c("None", "Selected genes"))
  
  # Create the heatmap
  plot <- heatmaply(
    mat.z,
    plot_method = "plotly",
    limits = c(-2, 2),
    branches_lwd = 0.01,
    subplot_widths = c(0.95, 0.005, 0.045),
    grid_gap = 0.5,
    fontsize_row = 8,
    fontsize_col = 6,
    key.title = "Z-score",
    label_names = c("Gene", "Sample", "Z-score"),
    colors = rev(colorRampPalette(brewer.pal(3, "RdBu"))(256)),
    col_side_colors = conditions_df,
    col_side_palette = condition_colors,
    row_side_colors = selection,
    row_side_palette = selection_mapping,
    colorbar_thickness = 20
  )
  
  return(plot)
}

  
# Function to calculate and plot correlations with p-values using cor.test in a loop
analyze_gene_correlations <- function(dds, gene_of_interest, threshold = 0.4) {
  
  # Extract the counts matrix from the DESeq2 result
  counts_matrix <- counts(dds, normalized = TRUE)
  
  # Transpose the counts matrix so that genes are columns
  counts_matrix <- t(counts_matrix)
  
  # Convert the counts matrix to a data.table
  data_dt <- as.data.table(counts_matrix)
  
  # Check if the gene of interest exists in the data
  if (!(gene_of_interest %in% colnames(data_dt))) {
    stop(paste("Gene", gene_of_interest, "not found in the data."))
  }
  
  # Subset the data to get the expression levels of the gene of interest
  gene_expr <- data_dt[[gene_of_interest]]
  
  # Initialize vectors to store correlations and p-values
  correlations <- numeric(ncol(data_dt))
  p_values <- numeric(ncol(data_dt))
  
  # Calculate the correlation and p-value for each gene
  for (i in seq_along(data_dt)) {
    test_result <- cor.test(gene_expr, data_dt[[i]], use = "complete.obs")
    correlations[i] <- test_result$estimate
    p_values[i] <- test_result$p.value
  }
  
  # Convert the results to a data.table
  results_dt <- data.table(gene = colnames(data_dt), correlation = correlations, p_value = p_values)
  
  # Filter correlations based on the threshold
  filtered_results <- results_dt[abs(correlation) >= threshold]
  
  # Calculate -log10(p_value)
  filtered_results[, log_p_value := -log10(p_value)]
  
  # Create a custom color palette
  color_palette <- rev(colorRampPalette(brewer.pal(3, "RdBu"))(256))
  
  # Create a U-plot with plotly
  plot <- plot_ly(filtered_results, x = ~correlation, y = ~log_p_value, type = 'scatter', mode = 'markers',
                  marker = list(size = 10, color = ~correlation, colorscale = color_palette, showscale = TRUE, line = list(width = 1, color = 'black')),
                  text = ~paste("Gene: ", gene, "<br>Correlation: ", round(correlation, 2), "<br>-log10(p-value): ", round(log_p_value, 2)))
  
  # Customize the plot layout
  plot <- plot %>%
    layout(title = paste("Correlation and P-values of", gene_of_interest, "with other genes"),
           xaxis = list(title = "Correlation Coefficient"),
           yaxis = list(title = "-log10(p-value)"),
           showlegend = FALSE)
  
  return(plot)
}