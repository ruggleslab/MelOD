
#' PCA Data
#' 
#' @description Processes DESeq2 dataset for PCA analysis
#' @param dds DESeq2 dataset
#' @return A list containing PCA data and the variance stabilized transformed data
pca_data <- function(dds) {
  vsdata <- vst(dds, blind = FALSE)
  pca_data <- plotPCA(vsdata, intgroup = "condition", returnData = TRUE)
  
  # Extract size factors and add them to the PCA data
  size_factors <- sizeFactors(dds)
  pca_data$size_factor <- size_factors[rownames(pca_data)]
  
  return(list(pca_data = pca_data, vsdata = vsdata))
}


#' Gene Names DDS
#' 
#' @description Processes DESeq2 dataset to ensure unique gene names
#' @param dds DESeq2 dataset
#' @return Processed DESeq2 dataset with unique gene names
gene_names_dds <- function(dds) {
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
  
  dds
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
    test_result <- cor.test(gene_expr, data_dt[[i]], use = "complete.obs")
    correlations[i] <- test_result$estimate
    p_values[i] <- test_result$p.value
  }
  
  results_dt <- data.table(gene = colnames(data_dt), correlation = correlations, p_value = p_values)
  filtered_results <- results_dt[abs(correlation) >= threshold]
  filtered_results[, log_p_value := -log10(p_value)]
  filtered_results <- filtered_results[is.finite(correlation) & is.finite(log_p_value)]
  
  return(list(filtered_results = filtered_results, gene_of_interest = gene_of_interest))
}


process_volcano_data <- function(dds, padj_cut, log2_cut) {
  #' Process data for volcano plot
  #' @param dds Processed DESeq2 dataset
  #' @param padj_cut Adjusted p-value cutoff
  #' @param log2_cut Log2 fold change cutoff
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
  #' @param gene_of_interest Specific gene to analyze
  #' @return Processed data for violin plot
  
  


  gene_of_interest <- rownames(display_genes)
  
  
  gene <- rownames(display_genes)
  counts <- counts(dds, normalized = TRUE)
  counts_filtered <- counts[rownames(counts) %in% gene, , drop = FALSE]
  df <- as.data.frame(log(counts_filtered + 0.01))
  
  col_data_df <- as.data.frame(colData(dds))
  col_data_df <- col_data_df %>%
    rownames_to_column(var = "patient_id") %>%
    select(patient_id, condition)
  
  df_long <- df %>%
    tibble::rownames_to_column("gene_id") %>%
    tidyr::pivot_longer(cols = -gene_id, names_to = "Sample", values_to = "expression")
  
  merged_data <- left_join(df_long, col_data_df, by = c("Sample" = "patient_id"))
  
  return(list(merged_data = merged_data, gene_of_interest = gene_of_interest))
}



