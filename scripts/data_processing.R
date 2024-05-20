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

