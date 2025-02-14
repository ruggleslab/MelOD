source("global.R", local = TRUE)


plot_pca <- function(pca_data_frame, size_by = "constant", color_by = "group") {
  #' Create PCA Plot
  #' 
  #' @description Creates a PCA plot for the given data
  #' @param pca_data_frame Data for PCA plot, including size factors
  #' @param size_by Optional parameter to set dot size by size factor ("size_factor") or constant size ("constant")
  #' @param color_by Optional parameter to color dots by "condition" or "name"
  #' 
  #' @return A plotly object representing the PCA plot
 
  text_labels <- paste("Name:", pca_data_frame$name, 
                       "<br>Group:", pca_data_frame$group, 
                       "<br>Size Factor:", round(pca_data_frame$size_factor, 3))
  
  custom_colors <- c("#D81B60", "#1E88E5")
  
  dot_size <- if (size_by == "constant") {
    7 
  } else {
    pca_data_frame$size_factor * 5  
  }
  
  if (color_by == "group") {
    color_column <- pca_data_frame$group
  } else {
    color_column <- pca_data_frame$name
    num_name <- length(unique(pca_data_frame$name))
    custom_colors <- colorRampPalette(brewer.pal(9, "Set1"))(num_name)
  }
  
  pca_plot <- plot_ly(
    data = pca_data_frame,
    x = ~PC1,
    y = ~PC2,
    color = color_column,
    text = text_labels,
    colors = custom_colors,
    marker = list(size = dot_size,
                  line = list(color = "black", width = 1))    
  ) %>%
    layout(
      xaxis = list(title = "PC1", zeroline = FALSE),
      yaxis = list(title = "PC2", zeroline = FALSE))
  
  return(pca_plot)
}


plot_variance <- function(vs_data) {
  #' Variance Explained Plot
  #' 
  #' @description Creates a bar plot of variance explained by the principal components
  #' @param vs_data Variance stabilized transformed data from DESeq2
  #' 
  #' @return A plotly object representing the variance explained by each principal component
  #' 
  pca_result <- prcomp(t(assay(vs_data)))
  explained_variance <- pca_result$sdev^2 / sum(pca_result$sdev^2)
  
  explained_variance_df <- data.frame(
    PC = factor(paste0("PC", 1:length(explained_variance)), levels = paste0("PC", 1:length(explained_variance))),
    VarianceExplained = explained_variance
  )
  
  variance_plot <- plot_ly(
    data = explained_variance_df,
    x = ~PC,
    y = ~VarianceExplained,
    type = 'bar',
    text = ~paste(round(VarianceExplained * 100, 2), "%"),
    textposition = 'auto',
    marker = list(color = 'rgba(50, 171, 96, 0.7)',
                  line = list(color = 'rgba(50, 171, 96, 1.0)', width = 1.5))
  ) %>%
    layout(
      xaxis = list(title = "Principal Components"),
      yaxis = list(title = "Variance Explained", tickformat = ".2%")
    )
  
  return(variance_plot)
}


plot_mortality_curve <- function(clinical_data, group_col = "group") {
  #' Plot Mortality Curve
  #'
  #' @description Generates a mortality curve plot based on clinical data, showing survival probabilities over time
  #'              and statistical summaries.
  #' @param clinical_data DataFrame containing clinical data with survival information.
  #' @param group_col Column name by which to group the data for survival analysis, defaults to "group".
  #'
  #' @return A Plotly object representing the survival curves for different groups, or a string message if an error occurs.
  
  tryCatch({
    # Create the overall survival object.
    survival_object <- Surv(time = clinical_data$OS.days., event = clinical_data$status)
    survival_fit <- survfit(as.formula(paste("survival_object ~", group_col)), data = clinical_data)
    
    # Perform log-rank test to compare groups.
    logrank_test <- survdiff(as.formula(paste("survival_object ~", group_col)), data = clinical_data)
    p_value <- 1 - pchisq(logrank_test$chisq, df = length(logrank_test$n) - 1)
    
    # Determine the groups and assign a fixed color mapping.
    # If the groups are "Low" and "High", force that order and use a dedicated mapping.
    groups_raw <- unique(clinical_data[[group_col]])
    if (all(groups_raw %in% c("Low", "High"))) {
      clinical_data[[group_col]] <- factor(clinical_data[[group_col]], levels = c("Low", "High"))
      groups <- levels(clinical_data[[group_col]])
      group_color_map <- c("Low" = "#1E88E5", "High" = "#D81B60")
    } else {
      # For any other groups, sort them alphabetically so that the mapping is consistent.
      groups <- sort(unique(clinical_data[[group_col]]))
      # Define a default palette (expand or adjust colors as needed).
      default_palette <- c("#1E88E5", "#D81B60")
      # Map each group to a color. (If there are more groups than colors, colors will recycle.)
      group_color_map <- setNames(default_palette[seq_along(groups)], groups)
    }
    
    mortality_plot <- plot_ly()
    median_survivals <- list()
    
    # Loop over each group to add the survival curve.
    for (i in seq_along(groups)) {
      group_name <- groups[i]
      group_data <- clinical_data[clinical_data[[group_col]] == group_name, ]
      group_survival_object <- Surv(time = group_data$OS.days., event = group_data$status)
      group_survival_fit <- survfit(group_survival_object ~ 1)
      
      # Retrieve fixed color for this group.
      group_color <- group_color_map[group_name]
      
      # Create custom hover text.
      custom_text <- c(
        paste(
          "Time: 0",
          "<br>Survival Probability: 1.000",
          "<br>Number at risk: ", group_survival_fit$n.risk[1],
          "<br>Number of events: 0"
        ),
        paste(
          "Time: ", group_survival_fit$time,
          "<br>Survival Probability: ", round(group_survival_fit$surv, 3),
          "<br>Number at risk: ", group_survival_fit$n.risk,
          "<br>Number of events: ", group_survival_fit$n.event
        )
      )
      
      # Add the survival curve (with markers).
      mortality_plot <- mortality_plot %>%
        add_trace(
          type = 'scatter',
          mode = 'lines+markers',
          x = c(0, group_survival_fit$time), 
          y = c(1, group_survival_fit$surv),
          name = group_name,
          line = list(color = group_color),
          marker = list(color = group_color),
          text = custom_text,
          hoverinfo = 'text'
        )
      
      # Retrieve the median survival for this group.
      median_survival <- summary(group_survival_fit)$table["median"]
      if (is.na(median_survival)) {
        median_survivals[[group_name]] <- "Not Reached"
      } else {
        median_survivals[[group_name]] <- paste(median_survival, "days")
      }
    }
    
    median_text <- paste0(groups, ": ", unlist(median_survivals), collapse = "<br>")
    
    # Update the layout with titles and p-value information.
    mortality_plot <- mortality_plot %>%
      layout(
        title = list(
          text = paste("<br><sup>Median Survival: ", median_text,
                       "<br>P-value (Log-Rank Test): ", sprintf("%.3f", p_value), "</sup>")
        ),
        xaxis = list(title = "Days"),
        yaxis = list(title = "Survival Probability", range = c(0, 1.2))
      )
    
    return(mortality_plot)
  }, error = function(e) {
    print(e)
    return("No metadata available")
  })
}

plot_volcano <- function(result_data, deseq2_data, gene = NULL) {
  #' Create Volcanoplot
  #' 
  #' @param result_data Processed data for volcano plot
  #' @param deseq2_data Processed DESeq2 dataset
  #' @param gene Specific genes to highlight
  #' 
  #' @return A plotly object representing the volcanoplot
  
  volcano_plot <- plot_ly(source = "A")
  
  upregulated_data <- subset(result_data, sig == "Upregulated")
  volcano_plot <- add_trace(
    volcano_plot,
    data = upregulated_data,
    x = upregulated_data$log2FoldChange,
    y = upregulated_data$neg_log10_padj,
    type = 'scattergl',
    mode = 'markers',
    color = I("#D81B60"),
    text = paste(
      "Gene:", rownames(upregulated_data),
      "<br>Log2 Fold Change:", round(upregulated_data$log2FoldChange, 2),
      "<br>Adjusted p-value:", formatC(upregulated_data$padj, format = "e", digits = 2)
    ),
    marker = list(size = 7, line = list(color = "#82103a", width = 1)),
    customdata = rownames(upregulated_data),
    name = "Upregulated"
  )
  
  downregulated_data <- subset(result_data, sig == "Downregulated")
  volcano_plot <- add_trace(
    volcano_plot,
    data = downregulated_data,
    x = downregulated_data$log2FoldChange,
    y = downregulated_data$neg_log10_padj,
    type = 'scattergl',
    mode = 'markers',
    color = I("#1E88E5"),
    text = paste(
      "Gene:", rownames(downregulated_data),
      "<br>Log2 Fold Change:", round(downregulated_data$log2FoldChange, 2),
      "<br>Adjusted p-value:", formatC(downregulated_data$padj, format = "e", digits = 2)
    ),
    marker = list(size = 7, line = list(color = "#10528b", width = 1)),
    customdata = rownames(downregulated_data),
    name = "Downregulated"
  )
  
  non_sig_data <- subset(result_data, sig == "Not Significant")
  volcano_plot <- add_trace(
    volcano_plot,
    data = non_sig_data,
    x = non_sig_data$log2FoldChange,
    y = non_sig_data$neg_log10_padj,
    type = 'scattergl',
    mode = 'markers',
    color = I("gray"),
    text = paste(
      "Gene:", rownames(non_sig_data),
      "<br>Log2 Fold Change:", round(non_sig_data$log2FoldChange, 2),
      "<br>Adjusted p-value:", formatC(non_sig_data$padj, format = "e", digits = 2)
    ),
    marker = list(size = 7, line = list(color = "gray", width = 1)),
    name = "Not Significant",
    customdata = rownames(non_sig_data),
    visible = 'legendonly'
  )
  
  if (!is.null(gene) && length(gene) > 0) {
    highlighted_genes <- subset(result_data, rownames(result_data) %in% gene)
    if (nrow(highlighted_genes) > 0) {
      volcano_plot <- add_trace(
        volcano_plot,
        data = highlighted_genes,
        x = highlighted_genes$log2FoldChange,
        y = highlighted_genes$neg_log10_padj,
        type = "scattergl",
        mode = "markers",
        color = I("orange"),
        text = paste(
          "Gene:", rownames(highlighted_genes),
          "<br>Log2 Fold Change:", round(highlighted_genes$log2FoldChange, 2),
          "<br>Adjusted p-value:", formatC(highlighted_genes$padj, format = "e", digits = 2)
        ),
        hoverinfo = 'text',
        marker = list(size = 7, line = list(color = "darkorange", width = 1)),
        name = "Highlighted Genes"
      )
      
      volcano_plot <- add_trace(
        volcano_plot,
        data = highlighted_genes,
        x = highlighted_genes$log2FoldChange,
        y = highlighted_genes$neg_log10_padj,
        type = "scattergl",
        mode = "text",
        text = rownames(highlighted_genes),
        textposition = "top center",
        textfont = list(size = 10, color = "black"),
        hoverinfo = 'none',
        showlegend = FALSE
      ) %>%
        plotly::event_register('plotly_click')    
    } else {
      message("No genes found matching the specified list. Please check gene names.")
    }
  }
  
  unique_conditions <- unique(deseq2_data$group)
  condition_title <- paste(unique_conditions, collapse = " vs ")
  
  volcano_plot <- layout(
    volcano_plot,
    title = condition_title,
    xaxis = list(title = "Log2 Fold Change"),
    yaxis = list(title = "-log10 Adjusted p-value"),
    margin = list(t = 50)
  )
  
  volcano_plot <- volcano_plot %>%  
    config(modeBarButtonsToAdd = c(
      'drawline', 
      'drawopenpath', 
      'drawclosedpath', 
      'drawcircle', 
      'drawrect', 
      'eraseshape'
    ))
  
  return(volcano_plot)
}


plot_violin <- function(merged_data, gene_of_interest, padj_cutoff, choice_shape, choice_color, choice_dot) {
  #' Create Violin or Box Plot
  #'
  #' @param merged_data Processed data for plot
  #' @param gene_of_interest Specific gene to analyze
  #' @param padj_cutoff Adjusted p-value cutoff
  #' @param choice_shape Shape of the plot ("violin" or "boxplot")
  #' @param choice_color Color palette for the plot
  #' @param choice_dot Dot mode for the plot ("outliers", "all", etc.)
  #'
  #' @return A plotly object representing the plot

  color_palettes <- list("Red & Blue" = c("#D81B60", "#1E88E5"),
                         "Green & Purple" = c("#66BB6A", "#8E24AA")
  )

  color_palettes_darker <- list("Red & Blue" = c("#82103a", "#10528b"),
                                "Green & Purple" = c("#58b55d", "#803c92")
  )
  colors_chosen <- unlist(color_palettes[choice_color])
  colors_chosen_darker <- unlist(color_palettes_darker[choice_color])

  conditions_found <- sort(unique(merged_data$group), method = "radix", na.last = NA)
  custom_colors <- setNames(colors_chosen[1:length(conditions_found)], conditions_found)

  if (choice_shape == "violin") {
    plot_type <- "violin"
    plot <- plot_ly(merged_data, x = ~gene_id, y = ~expression, color = ~factor(group),
                    type = plot_type, colors = custom_colors, points = choice_dot,
                    jitter = 0.1, pointpos = 0,
                    marker = list(size = 5, line = list(width = 0)),
                    line = list(width = 1),
                    box = list(visible = TRUE, fillcolor = colors_chosen_darker, line = list(width = 1, color = colors_chosen_darker)),
                    meanline = list(visible = TRUE),
                    bandwidth = 0.9,
                    text = ~paste("Gene ID:", gene_id)) %>%
      layout(
             yaxis = list(title = "Log-normalized Expression"),
             xaxis = list(title = "Gene - Condition",
                          titlefont = list(size = 14),
                          tickfont = list(family = "Arial", size = 12, color = "black")),
             margin = list(t = 50),
             zeroline = FALSE,
             violingap = 0.2,
             violingroupgap = 0.1,
             violinmode = "group")
  } else {
    plot_type <- "box"
    plot <- plot_ly(merged_data, x = ~gene_id, y = ~expression, color = ~factor(group),
                    type = plot_type, colors = custom_colors, points = choice_dot,
                    jitter = 0.3, pointpos = 0,
                    marker = list(size = 5, line = list(width = 0)),
                    boxpoints = choice_dot,
                    box = list(visible = TRUE, fillcolor = colors_chosen_darker, line = list(width = 1, color = colors_chosen_darker)),
                    meanline = list(visible = TRUE),
                    text = ~paste("Gene ID:", gene_id)) %>%
      layout(
             yaxis = list(title = "Log-normalized Expression"),
             xaxis = list(title = "Gene - Condition",
                          titlefont = list(size = 14),
                          tickfont = list(family = "Arial", size = 12, color = "black")),
             margin = list(t = 50),
             zeroline = FALSE,
             boxmode = "group")
  }

  plot <- add_significance_annotations(merged_data, plot, padj_cutoff)
  plot <- plot %>%
    config(modeBarButtonsToAdd = c('drawline',
                                   'drawopenpath',
                                   'drawclosedpath',
                                   'drawcircle',
                                   'drawrect',
                                   'eraseshape'))

  return(plot)
}


plot_heatmap <- function(z_score_matrix, deseq2_data, gene, heatmap_palette, z_score_range, font_size) {
  #' Plot Heatmap
  #'
  #' @param z_score_matrix Z-scored matrix.
  #' @param deseq2_data Processed DESeq2 dataset.
  #' @param gene Specific gene to display.
  #' @param heatmap_palette Color palette for the heatmap.
  #' @param z_score_range Range for the z-scores.
  #' @param font_size Font size for row and column labels.
  #'
  #' @return A plotly object representing the heatmap.
  
  tryCatch({
    # Clip the z-score matrix to the specified range
    clip_matrix <- function(matrix, z_score_range) {
      matrix[matrix > z_score_range] <- z_score_range
      matrix[matrix < -z_score_range] <- -z_score_range
      return(matrix)
    }
    
    # Clip the matrix values before plotting
    z_score_matrix <- clip_matrix(z_score_matrix, z_score_range)
    
    # Create custom hover text with Z-score rounded to two decimal places
    genes <- rownames(z_score_matrix)
    samples <- colnames(z_score_matrix)
    hover_text_matrix <- outer(
      genes,
      samples,
      Vectorize(function(gene, sample) {
        z_value <- z_score_matrix[gene, sample]
        paste(
          "Gene:", gene,
          "<br>Sample:", sample,
          "<br>Z-score:", sprintf("%.2f", z_value)
        )
      })
    )
    
    conditions <- colData(deseq2_data)$group
    condition_levels <- sort(unique(conditions), method = "radix")
    preferred_colors <- c("#D81B60", "#1E88E5")
    
    if (length(condition_levels) <= length(preferred_colors)) {
      condition_colors <- setNames(preferred_colors[1:length(condition_levels)], condition_levels)
    } else {
      condition_colors <- scales::hue_pal()(length(condition_levels))
      condition_colors <- setNames(condition_colors, condition_levels)
    }
    
    conditions_df <- data.frame("Conditions" = conditions, check.names = FALSE)
    selection <- ifelse(rownames(z_score_matrix) %in% gene, "Selected genes", "None")
    selection_mapping <- setNames(c("#ffffff00", "orange"), c("None", "Selected genes"))
    
    # Create heatmap plot with adjusted z-score range and custom hover text
    heatmap_plot <- heatmaply(
      z_score_matrix,
      plot_method = "plotly",
      midpoint = 0,
      limits = c(-z_score_range, z_score_range),
      branches_lwd = 0.01,
      subplot_widths = c(0.95, 0.005, 0.045),
      grid_gap = 0.5,
      fontsize_row = font_size,
      fontsize_col = font_size,
      key.title = "Z-score",
      label_names = c("Gene", "Sample", "Z-score"),
      colors = rev(colorRampPalette(brewer.pal(3, heatmap_palette))(256)),
      col_side_colors = conditions_df,
      col_side_palette = condition_colors,
      row_side_colors = selection,
      row_side_palette = selection_mapping,
      colorbar_thickness = 20,
      custom_hovertext = hover_text_matrix
    )
    
    # Modify colorbars
    heatmap_plot$x$data[[7]]$colorbar$x <- 1.05
    heatmap_plot$x$data[[7]]$colorbar$y <- 0.2
    heatmap_plot$x$data[[7]]$colorbar$len <- 0.3
    heatmap_plot$x$data[[7]]$colorbar$thickness <- 15
    
    heatmap_plot$x$data[[4]]$colorbar$x <- 1.05
    heatmap_plot$x$data[[4]]$colorbar$y <- 0.2
    heatmap_plot$x$data[[4]]$colorbar$len <- 0.3
    heatmap_plot$x$data[[4]]$colorbar$thickness <- 15
    
    heatmap_plot$x$data[[8]]$colorbar$title <- "Selection"
    heatmap_plot$x$data[[8]]$colorbar$x <- 1.05
    heatmap_plot$x$data[[8]]$colorbar$y <- 0.8
    heatmap_plot$x$data[[8]]$colorbar$len <- 0.3
    heatmap_plot$x$data[[8]]$colorbar$thickness <- 15
    
    # Add configuration for drawing tools
    heatmap_plot <- heatmap_plot %>%
      config(modeBarButtonsToAdd = c(
        'drawline',
        'drawopenpath',
        'drawclosedpath',
        'drawcircle',
        'drawrect',
        'eraseshape'
      ))
    
    return(heatmap_plot)
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
  #' 
  #' @return A datatable containing the filtered results
  
  DT::renderDataTable({
    
    res <- tryCatch({
      results(dds_processed())  # Try to get results if it's a DESeq2 object
    }, error = function(e) {
      # If results() fails, use the dds object directly (assuming it is already in a results-like format)
      rowData(dds_processed())
    })    
    res <- as.data.frame(res)
    
    custom_round <- function(x, digits) {
      ifelse(abs(x) < 10^(-digits), format(x, scientific = TRUE, digits = digits), round(x, digits))
    }
    
    numeric_columns <- sapply(res, is.numeric)
    res[numeric_columns] <- lapply(res[numeric_columns], function(x) custom_round(x, 4))
    
    if (!is.null(input$selected_gene) && length(input$selected_gene) > 0)
      res <- res[rownames(res) %in% input$selected_gene, ]
    
    if ("log2FoldChange" %in% colnames(res)) {
      res$log2FoldChange <- as.numeric(res$log2FoldChange)
    }
    
    if ("log2FoldChange" %in% colnames(res) && is.numeric(res$log2FoldChange)) {
      res$log2FoldChange[is.na(res$log2FoldChange)] <- 0
      res <- res[order(-abs(res$log2FoldChange)), ]
    }
    
    # Apply dynamic cutoffs
    padj_cutoff <- input$slider_padj
    log2_cutoff <- input$slider_log2
    
    # Render the datatable with conditional formatting
    DT::datatable(res, extensions = 'Buttons', options = list(
      dom = 'lrBtip',
      buttons = c('copy', 'csv', 'excel'),
      pageLength = 10,
      scrollX = TRUE
    )) %>%
      DT::formatStyle(
        'padj', # Column to format
        backgroundColor = DT::styleInterval(
          c(padj_cutoff), # Single cutoff for two colors
          c('lightgreen', 'lightpink') # Colors based on threshold
        ),
        fontWeight = 'bold' # Bold the values
      ) %>%
      DT::formatStyle(
        'log2FoldChange',
        color = DT::styleInterval(
          c(-log2_cutoff, log2_cutoff), # Two cutoffs for three colors
          c('#D81B60', 'black', '#1E88E5') # Red for low, black for middle, blue for high
        ),
        fontWeight = 'bold'
      )
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
        line = list(width = 0.5, color = 'black'),
        colorbar = list(len = 0.3, thickness = 10) 
      ),
      text = ~paste("Gene: ", gene, "<br>Correlation: ", round(correlation, 2), "<br>-log10(p-value): ", round(log_p_value, 2))
    )
    
    histogram <- plot_ly(filtered_results, x = ~correlation, type = 'histogram', 
                         marker = list(color = '#75a3d1', line = list(color = 'rgba(100, 100, 100, 1)', width = 1)))
    
    correlation_plot <- subplot(scatter_plot, histogram, nrows = 2, shareX = TRUE, titleX = TRUE, titleY = TRUE, heights = c(0.65, 0.35))
    correlation_plot <- correlation_plot %>%
      layout(
             xaxis = list(title = "Correlation Coefficient"),
             yaxis = list(title = "-log10(p-value)"),
             yaxis2 = list(title = "Count"),
             colorbar = list(size = 0.2),
             showlegend = FALSE)
    correlation_plot <- correlation_plot %>%  
      config(modeBarButtonsToAdd = c('drawline', 
                                     'drawopenpath', 
                                     'drawclosedpath', 
                                     'drawcircle', 
                                     'drawrect', 
                                     'eraseshape'))
    return(correlation_plot)
  }, error = function(e) {
    return("Wait...")
  })
}