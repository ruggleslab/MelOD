process_pca_data <- function(dds) {
#' PCA Data
#' 
#' @description Processes DESeq2 dataset for PCA analysis
#' @param dds DESeq2 dataset
#' 
#' @return A list containing PCA data and the variance stabilized transformed data
  
  vsdata <- vst(dds, blind = FALSE)
  pca_data <- plotPCA(vsdata, intgroup = "condition", returnData = TRUE)
  
  # Extract size factors and add them to the PCA data
  size_factors <- sizeFactors(dds)
  pca_data$size_factor <- size_factors[rownames(pca_data)]
  
  return(list(pca_data = pca_data, vsdata = vsdata))
}


process_clinical_data <- function(clinical_data, group_by = "condition", deseq2_data = NULL, gene = NULL) {
  #' Process Clinical Data
  #'
  #' @description Processes clinical data from a study and returns a dataframe for plotting survival curves.
  #' @param clinical_data DataFrame containing normalized clinical data.
  #' @param group_by Column name by which to group the data, defaults to "condition".
  #' @param deseq2_data Gene expression matrix from DESeq2 results (only for the "by gene" view).
  #' @param gene Gene name to filter the gene expression matrix (only for the "by gene" view).
  #' 
  #' @return A DataFrame of clinical data based on the selected view. If both `deseq2_data` and `gene` are provided, it returns the quartiles of patient expression for the specified gene; otherwise, it returns the default DataFrame grouped by the `group_by` parameter.
  
  if (!is.null(gene) && !is.null(deseq2_data)) {
    gene_expression <- as.numeric(assay(deseq2_data)[gene, ])
    if (!all(clinical_data$X %in% colnames(deseq2_data))) {
      stop("Some patient IDs in the clinical data do not match the DESeq2 data")
    }
    gene_expression <- gene_expression[match(clinical_data$X, colnames(deseq2_data))]
    gene_expression <- jitter(gene_expression, factor = 0.1)
    clinical_data$group <- cut(gene_expression, breaks = quantile(gene_expression, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE), include.lowest = TRUE, labels = c("Q1", "Q2", "Q3"))
  } else {
    clinical_data$group <- clinical_data[[group_by]]
  }
  
  clinical_data$status <- ifelse(grepl("Alive", clinical_data$Last.Followup.Status), 0, 1)
  return(clinical_data)
}


process_volcano_data <- function(dds, padj_cut, log2_cut) {
  #' Process data for volcano plot
  #' @param dds Processed DESeq2 dataset
  #' @param padj_cut Adjusted p-value cutoff
  #' @param log2_cut Log2 fold change cutoff
  #' 
  #' @return A list containing processed data for volcano plot and dds
  
  res <- results(dds)
  res$neg_log10_padj <- -log10(res$padj)
  res$sig <- ifelse(res$padj < padj_cut & abs(res$log2FoldChange) >= log2_cut,
                    ifelse(res$log2FoldChange > 0, "Upregulated", "Downregulated"),
                    "Not Significant")
  
  return(list(res = res, dds = dds))
}


process_violin_data <- function(dds, display_genes) {
  #' Process data for violin plot
  #' @param dds Processed DESeq2 dataset
  #' @param display_genes Genes to display
  #' 
  #' @return Processed data for violin plot
  
  gene_of_interest <- rownames(display_genes)
  counts <- counts(dds, normalized = TRUE)
  counts_filtered <- counts[rownames(counts) %in% gene_of_interest, , drop = FALSE]
  df <- as.data.frame(log(counts_filtered + 1))
  
  col_data_df <- as.data.frame(colData(dds))
  col_data_df <- col_data_df %>%
    rownames_to_column(var = "patient_id") %>%
    select(patient_id, condition)
  
  df_long <- df %>%
    rownames_to_column("gene_id") %>%
    pivot_longer(cols = -gene_id, names_to = "Sample", values_to = "expression")
  
  merged_data <- left_join(df_long, col_data_df, by = c("Sample" = "patient_id"))
  
  # Extract padjust values
  padjust_values <- data.frame(
    gene_id = rownames(display_genes),
    padjust = display_genes$padj
  )
  
  # Merge padjust values with the merged_data
  merged_data <- left_join(merged_data, padjust_values, by = "gene_id")
  
  return(list(merged_data = merged_data, gene_of_interest = gene_of_interest))
}


process_heatmap_data <- function(dds, padj_cut, log2_cut, number, gene) {
  #' Process Heatmap Data
  #'
  #' This function processes the data required for creating a heatmap.
  #'
  #' @param dds Processed DESeq2 dataset.
  #' @param padj_cut Adjusted p-value cutoff.
  #' @param log2_cut Log2 fold change cutoff.
  #' @param number Number of top genes to display.
  #' @param gene Specific gene to display.
  #'
  #' @return A list containing the processed data and matrix.
  
  res <- results(dds)
  res.df <- as.data.frame(res)
  
  res.df.filter <- res.df[(abs(res.df$log2FoldChange) > log2_cut) & (!is.na(res.df$padj) & res.df$padj < padj_cut),]
  res.df.filter <- res.df.filter[order(res.df.filter$padj), ]
  
  if (nrow(res.df.filter) > number) {
    res.df.filter <- res.df.filter[0:number, ]
  } else {
    message("Not enough genes. Try a lower number.")
  }
  
  if (!is.null(gene)) {
    res.df.gene <- subset(res.df, rownames(res.df) %in% gene)
    res.df.final <- unique(rbind(res.df.filter, res.df.gene))
  } else {
    res.df.final <- res.df.filter
  }
  
  mat <- assay(vst(dds))[rownames(res.df.final),]
  mat.z <- t(scale(t(mat)))
  
  return(list(data = res.df.final, matrix = mat.z))
}


process_gene_correlations <- function(dds, display_genes, gene_of_interest, threshold = 0.4) {
  #' Process gene correlations
  #'
  #' This function calculates the correlation and p-values of a specified gene of interest with other genes.
  #'
  #' @param dds DESeq2 dataset object.
  #' @param display_genes A data table of genes to display.
  #' @param gene_of_interest A specific gene to analyze.
  #' @param threshold A numeric value to filter correlations.
  #'
  #' @return A data table containing the gene correlations and p-values.
  
  if (gene_of_interest == "") {
    gene_of_interest <- rownames(display_genes)[1]
  }
  
  counts_matrix <- counts(dds, normalized = TRUE)
  counts_matrix <- t(counts_matrix)
  data_dt <- as.data.table(counts_matrix)
  
  if (!(gene_of_interest %in% colnames(data_dt))) {
    stop(paste("Gene", gene_of_interest, "not found in the data."))
  }
  
  # Filter out genes with zero variance
  zero_variance_genes <- apply(data_dt, 2, function(x) sd(x) == 0)
  data_dt <- data_dt[, !zero_variance_genes, with = FALSE]
  gene_expr <- data_dt[[gene_of_interest]]
  correlations <- numeric(ncol(data_dt))
  p_values <- numeric(ncol(data_dt))
  
  for (i in seq_along(data_dt)) {
    if (colnames(data_dt)[i] != gene_of_interest) {
      test_result <- cor.test(gene_expr, data_dt[[i]], use = "complete.obs")
      correlations[i] <- test_result$estimate
      p_values[i] <- test_result$p.value
    } else {
      correlations[i] <- NA
      p_values[i] <- NA
    }
  }
  
  results_dt <- data.table(gene = colnames(data_dt), correlation = correlations, p_value = p_values)
  results_dt <- results_dt[gene != gene_of_interest]
  filtered_results <- results_dt[abs(correlation) >= threshold]
  filtered_results[, log_p_value := -log10(p_value)]
  filtered_results <- filtered_results[is.finite(correlation) & is.finite(log_p_value)]
  
  return(filtered_results)
}