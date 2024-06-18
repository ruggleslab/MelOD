source("global.R", local = TRUE)


creation_pca <- function(pca_data, size_by = "constant", color_by = "condition") {
  #' Create PCA Plot
  #' 
  #' @description Creates a PCA plot for the given data
  #' @param pca_data Data for PCA plot, including size factors
  #' @param size_by Optional parameter to set dot size by size factor ("size_factor") or constant size ("constant")
  #' @param color_by Optional parameter to color dots by "condition" or "name"
  #' 
  #' @return A plotly object representing the PCA plot
  
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



variance_explained_plot <- function(vsdata) {
  #' Variance Explained Plot
  #' 
  #' @description Creates a bar plot of variance explained by the principal components
  #' @param vsdata Variance stabilized transformed data from DESeq2
  #' 
  #' @return A plotly object representing the variance explained by each principal component
  #' 
  pca_result <- prcomp(t(assay(vsdata)))
  explained_variance <- pca_result$sdev^2 / sum(pca_result$sdev^2)
  
  variance_df <- data.frame(
    PC = factor(paste0("PC", 1:length(explained_variance)), levels = paste0("PC", 1:length(explained_variance))),
    VarianceExplained = explained_variance
  )
  

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


plot_mortality_curve_by_condition <- function(clinical_data) {
  #' Plot Mortality Curve
  #'
  #' @description Plots the mortality curve and calculates median survival times
  #' @param clinical_data Clinical data for the plot
  #'
  #' @return A plotly object representing the mortality curve with median survival times

  tryCatch({
    clinical_data$Status <- ifelse(grepl("Alive", clinical_data$Last.Followup.Status), 0, 1)

    surv_obj <- Surv(time = clinical_data$OS.days., event = clinical_data$Status)

    fit <- survfit(surv_obj ~ condition, data = clinical_data)

    plot <- plot_ly()

    conditions <- unique(clinical_data$condition)
    colors <- c("#1E88E5", "#D81B60", "#33a02c")  # Add more colors if necessary
    median_survivals <- list()

    for (i in seq_along(conditions)) {
      condition_name <- conditions[i]
      condition_data <- clinical_data[clinical_data$condition == condition_name, ]
      condition_surv_obj <- Surv(time = condition_data$OS.days., event = condition_data$Status)
      condition_fit <- survfit(condition_surv_obj ~ 1)

      plot <- plot %>%
        add_trace(
          type = 'scatter',
          mode = 'lines+markers',
          x = condition_fit$time,
          y = condition_fit$surv,
          name = condition_name,
          line = list(color = colors[i])
        )

      median_survival <- summary(condition_fit)$table["median"]
      if (is.na(median_survival)) {
        median_survivals[[condition_name]] <- "Not Reached"
      } else {
        median_survivals[[condition_name]] <- paste(median_survival, "days")
      }
    }

    median_text <- paste0(conditions, ": ", unlist(median_survivals), collapse = "<br>")

    plot <- plot %>%
      layout(
        title = list(
          text = paste("Survival Curve<br><sup>Median Survival: ", median_text, "</sup>")
        ),
        xaxis = list(title = "Days"),
        yaxis = list(title = "Survival Probability", range = c(0, 1))
      )

    return(plot)
  }, error = function(e) {
    return("No metadata available")
  })
}

plot_mortality_curve_by_gene <- function(clinical_data, deseq2_rds, gene) {
  #' Plot Mortality Curve
  #' 
  #' @description Plots the mortality curve and calculates median survival times
  #' @param clinical_data Clinical data for the plot
  #' @param deseq2_rds DESeq2 RDS object
  #' @param gene Selected gene for expression analysis
  #' 
  #' @return A plotly object representing the mortality curve with median survival times
  
  tryCatch({
    # Load the DESeq2 RDS object
    dds <- deseq2_rds
    gene_expression <- as.numeric(assay(dds)[gene, ])    
    # Ensure patient IDs match
    
    if (!all(clinical_data$X %in% colnames(dds))) {
      return("Some patient IDs in the clinical data do not match the DESeq2 data")
    }
    
    gene_expression <- gene_expression[match(clinical_data$X, colnames(dds))]
    
    # Jitter the gene expression values slightly to avoid ties
    gene_expression <- jitter(gene_expression, factor = 0.1)
    
    # Classify patients into tertiles based on gene expression
    tertiles <- cut(gene_expression, breaks = quantile(gene_expression, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE), include.lowest = TRUE, labels = c("Q1", "Q2", "Q3"))
    clinical_data$tertile <- tertiles
    
    clinical_data$Status <- ifelse(grepl("Alive", clinical_data$Last.Followup.Status), 0, 1)
    surv_obj <- Surv(time = clinical_data$OS.days., event = clinical_data$Status)
    
    fit <- survfit(surv_obj ~ tertile, data = clinical_data)
    
    plot <- plot_ly()
    
    tertiles <- unique(clinical_data$tertile)
    colors <- c("#1E88E5", "#D81B60", "#33a02c")  
    median_survivals <- list()
    
    for (i in seq_along(tertiles)) {
      tertile_name <- tertiles[i]
      condition_data <- clinical_data[clinical_data$tertile == tertile_name, ]
      condition_surv_obj <- Surv(time = condition_data$OS.days., event = condition_data$Status)
      condition_fit <- survfit(condition_surv_obj ~ 1)
      
      plot <- plot %>%
        add_trace(
          type = 'scatter',
          mode = 'lines+markers',
          x = condition_fit$time, 
          y = condition_fit$surv,
          name = tertile_name,
          line = list(color = colors[i])
        )
      
      median_survival <- summary(condition_fit)$table["median"]
      if (is.na(median_survival)) {
        median_survivals[[tertile_name]] <- "Not Reached"
      } else {
        median_survivals[[tertile_name]] <- paste(median_survival, "days")
      }
    }
    
    median_text <- paste0(tertiles, ": ", unlist(median_survivals), collapse = "<br>")
    
    plot <- plot %>%
      layout(
        title = list(
          text = paste("Survival Curve<br><sup>Median Survival: ", median_text, "</sup>")
        ),
        xaxis = list(title = "Days"),
        yaxis = list(title = "Survival Probability", range = c(0, 1))
      )
    
    return(plot)
  }, error = function(e) {
    print(e)
    return("No metadata available")
  })
}


plot_volcano <- function(res, dds, gene = NULL) {
  #' Create Volcanoplot
  #' 
  #' @param res Processed data for volcano plot
  #' @param dds Processed DESeq2 dataset
  #' @param gene Specific genes to highlight
  #' 
  #' @return A plotly object representing the volcanoplot
  
  plot <- plot_ly(source = "A")
  
  upregulated_data <- subset(res, sig == "Upregulated")
  plot <- add_trace(plot, data = upregulated_data, x = upregulated_data$log2FoldChange, y = upregulated_data$neg_log10_padj,
                    type = 'scattergl', mode = 'markers', color = I("#D81B60"),
                    text = paste("Gene:", rownames(upregulated_data), "<br>Log2 Fold Change:", upregulated_data$log2FoldChange,
                                 "<br>Adjusted p-value:", upregulated_data$padj),
                    marker = list(size = 7, line = list(color = "#82103a", width = 1)),
                    customdata = rownames(upregulated_data),
                    name = "Upregulated")
  
  downregulated_data <- subset(res, sig == "Downregulated")
  plot <- add_trace(plot, data = downregulated_data, x = downregulated_data$log2FoldChange, y = downregulated_data$neg_log10_padj,
                    type = 'scattergl', mode = 'markers', color = I("#1E88E5"),
                    text = paste("Gene:", rownames(downregulated_data), "<br>Log2 Fold Change:", downregulated_data$log2FoldChange,
                                 "<br>Adjusted p-value:", downregulated_data$padj),
                    marker = list(size = 7, line = list(color = "#10528b", width = 1)),
                    customdata = rownames(downregulated_data),
                    name = "Downregulated")
  
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
      plot <- add_trace(plot, data = highlighted_genes, x = highlighted_genes$log2FoldChange, y = highlighted_genes$neg_log10_padj,
                        type = "scattergl", mode = "markers", color = I("orange"),
                        text = paste("Gene:", rownames(highlighted_genes), "<br>Log2 Fold Change:", highlighted_genes$log2FoldChange,
                                     "<br>Adjusted p-value:", highlighted_genes$padj),
                        hoverinfo = 'text',
                        marker = list(size = 7, line = list(color = "darkorange", width = 1)),
                        name = "Highlighted Genes")
      
      plot <- add_trace(plot, data = highlighted_genes, x = highlighted_genes$log2FoldChange, y = highlighted_genes$neg_log10_padj,
                        type = "scattergl", mode = "text", text = rownames(highlighted_genes),
                        textposition = "top center", textfont = list(size = 10, color = "black"),
                        hoverinfo = 'none',
                        showlegend = FALSE)%>%
        plotly::event_register('plotly_click')    
    } else {
      message("No genes found matching the specified list. Please check gene names.")
    }
  }
  
  unique_conditions <- unique(dds$condition)
  condition_title <- paste("Volcano plot of DESeq2 results of", paste(unique_conditions, collapse=" vs "))
  
  plot <- layout(plot, title = condition_title,
                 xaxis = list(title = "Log2 Fold Change"),
                 yaxis = list(title = "-log10 Adjusted p-value"),
                 margin = list(t = 100))
  plot <- plot %>%  
    config(modeBarButtonsToAdd = c('drawline', 
                                   'drawopenpath', 
                                   'drawclosedpath', 
                                   'drawcircle', 
                                   'drawrect', 
                                   'eraseshape'))
  
  return(plot)
}


plot_violin <- function(merged_data, gene_of_interest, padj_cut) {
  #' Create Violin Plot
  #' 
  #' @param merged_data Processed data for violin plot
  #' @param gene_of_interest Specific gene to analyze
  #' @param padj_cut Adjusted p-value cutoff
  #' 
  #' @return A plotly object representing the violin plot
  
  conditions_found <- unique(merged_data$condition)
  colors_chosen <- c("#D81B60", "#1E88E5")
  colors_chosen_darker <- c("#82103a", "#10528b")
  custom_colors <- setNames(colors_chosen[1:length(conditions_found)], conditions_found)
  
  condition_title <- paste(" in ", paste(conditions_found, collapse=" vs "))
  plot_title <- if (length(gene_of_interest) == 1) {
    paste("Expressions for", gene_of_interest, condition_title)
  } else {
    paste("Expressions for :", paste(gene_of_interest, collapse = ", "), condition_title)
  }
  
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
  plot <- plot %>%  
    config(modeBarButtonsToAdd = c('drawline', 
                                   'drawopenpath', 
                                   'drawclosedpath', 
                                   'drawcircle', 
                                   'drawrect', 
                                   'eraseshape'))
  
  return(plot)
}


plot_heatmap <- function(mat.z, dds, gene) {
  #' Plot Heatmap
  #'
  #' @param mat.z Z-scored matrix.
  #' @param dds Processed DESeq2 dataset.
  #' @param gene Specific gene to display.
  #'
  #' @return A plotly object representing the heatmap.
  
  tryCatch({
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
  
  selection <- ifelse(rownames(mat.z) %in% gene, "Selected genes", "None")
  selection_mapping <- setNames(c("#ffffff00", "orange"), c("None", "Selected genes"))
  
  plot <- heatmaply(
    mat.z,
    plot_method = "plotly",
    limits = c(-2, 2),
    branches_lwd = 0.01,
    subplot_widths = c(0.95, 0.005, 0.045),
    grid_gap = 0.5,
    fontsize_row = 8,
    fontsize_col = 8,
    key.title = "Z-score",
    label_names = c("Gene", "Sample", "Z-score"),
    colors = rev(colorRampPalette(brewer.pal(3, "RdBu"))(256)),
    col_side_colors = conditions_df,
    col_side_palette = condition_colors,
    row_side_colors = selection,
    row_side_palette = selection_mapping,
    colorbar_thickness = 20
  )
  
  # Access and modify the second colorbar (Z-score)
  plot$x$data[[7]]$colorbar$x <- 1.05
  plot$x$data[[7]]$colorbar$y <- 0.2
  plot$x$data[[7]]$colorbar$len <- 0.3
  plot$x$data[[7]]$colorbar$thickness <- 15
  
  # Access and modify the first colorbar (Conditions)
  plot$x$data[[4]]$colorbar$x <- 1.05
  plot$x$data[[4]]$colorbar$y <- 0.2
  plot$x$data[[4]]$colorbar$len <- 0.3
  plot$x$data[[4]]$colorbar$thickness <- 15
  
  # Access and modify the third colorbar (row_side_colors)
  plot$x$data[[8]]$colorbar$title <- "Selection"
  plot$x$data[[8]]$colorbar$x <- 1.05
  plot$x$data[[8]]$colorbar$y <- 0.8
  plot$x$data[[8]]$colorbar$len <- 0.3
  plot$x$data[[8]]$colorbar$thickness <- 15
  
  plot <- plot %>%
    layout(title = "Heatmap of Gene Expression", margin = list(t = 100), height = 470 
)
  plot <- plot %>%  
    config(modeBarButtonsToAdd = c('drawline', 
                                   'drawopenpath', 
                                   'drawclosedpath', 
                                   'drawcircle', 
                                   'drawrect', 
                                   'eraseshape'))
  return(plot)
  }, error = function(e) {
    return("Please set or select at least 2 genes")
  })
}


render_filtered_results_table <- function(dds_processed, input) {
  #'Result Table
  #'
  #' @description Renders the filtered results table based on the selected genes
  #' @param dds_processed Reactive expression containing the processed DESeq2 dataset
  #' @param input Shiny input object
  #' @param selected_genes_plotly Reactive value for selected genes
  #' 
  #' @return A datatable containing the filtered results

  DT::renderDataTable({
    res <- results(dds_processed())
    res <- as.data.frame(res)
    
    # Round numeric columns to 4 decimal places
    numeric_columns <- sapply(res, is.numeric)
    res[numeric_columns] <- lapply(res[numeric_columns], function(x) round(x, 4))
    
    if (!is.null(input$selected_gene) && length(input$selected_gene) > 0)
      res <- res[rownames(res) %in% input$selected_gene, ]
    
    DT::datatable(res, extensions = 'Buttons', options = list(
      dom = 'lrBtip',
      buttons = c('copy', 'csv', 'excel'),
      pageLength = 10,
      scrollX = TRUE
    ))
  })
}

plot_gene_correlations <- function(filtered_results, gene_of_interest) {
  #' Plot gene correlations
  #'
  #' This function generates a scatter plot and histogram to visualize the correlations.
  #'
  #' @param filtered_results A data table containing the gene correlations and p-values.
  #' @param gene_of_interest A specific gene to analyze.
  #'
  #' @return A plotly plot with scatter plot and histogram of gene correlations.
  
  tryCatch({
  color_palette <- rev(colorRampPalette(brewer.pal(3, "RdBu"))(256))
  
  scatter_plot <- plot_ly(
    filtered_results, 
    x = ~correlation, 
    y = ~log_p_value, 
    type = 'scatter', 
    mode = 'markers',
    marker = list(
      size = 10, 
      color = ~correlation, 
      colorscale = color_palette, 
      showscale = TRUE, 
      cmin = -1, 
      cmax = 1, 
      line = list(width = 0.5, color = 'darkgrey'),
      colorbar = list(len = 0.3, thickness= 10) 
    ),
    text = ~paste("Gene: ", gene, "<br>Correlation: ", round(correlation, 2), "<br>-log10(p-value): ", round(log_p_value, 2))
  )
  
  
  histogram <- plot_ly(filtered_results, x = ~correlation, type = 'histogram', 
                       marker = list(color = '#75a3d1', line = list(color = 'rgba(100, 100, 100, 1)', width = 1)))
  
  plot <- subplot(scatter_plot, histogram, nrows = 2, shareX = TRUE, titleX = TRUE, titleY = TRUE, heights = c(0.75, 0.25))
  plot <- plot %>%
    layout(title = paste("Correlation of ", gene_of_interest),
           xaxis = list(title = "Correlation Coefficient"),
           yaxis = list(title = "-log10(p-value)"),
           yaxis2 = list(title = "Count"),
           colorbar= list(size = 0.2),
           margin = list(t = 100),
           showlegend = FALSE)
  plot <- plot %>%  
    config(modeBarButtonsToAdd = c('drawline', 
                                   'drawopenpath', 
                                   'drawclosedpath', 
                                   'drawcircle', 
                                   'drawrect', 
                                   'eraseshape'))
  return(plot)
}, error = function(e) {
  return("Wait...")
})
}

