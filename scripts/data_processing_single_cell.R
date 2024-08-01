process_cell_data <- function(inpConf, inpMeta, inpdrX, inpdrY, inp1, inpsub1 = NULL, inpsub2 = NULL) {
  #' Process cell data for visualization
  #'
  #' @param inpConf Data frame with configuration settings.
  #' @param inpMeta Data frame with metadata.
  #' @param inpdrX Character string for the X dimension.
  #' @param inpdrY Character string for the Y dimension.
  #' @param inp1 Character string for the main variable.
  #' @param inpsub1 Character string for the first subgroup. Default is NULL.
  #' @param inpsub2 Character string for the second subgroup. Default is NULL.
  #'
  #' @return A list containing ggData, ggData2, ggCol, rat, and bgCells.
  
  if (is.null(inpsub1)) {
    inpsub1 <- inpConf$UI[1]
  }
  
  ggData <- inpMeta[, .(
    X = get(inpConf[UI == inpdrX]$ID),
    Y = get(inpConf[UI == inpdrY]$ID),
    val = get(inpConf[UI == inp1]$ID),
    sub = get(inpConf[UI == inpsub1]$ID)
  )]
  
  rat <- (max(ggData$X) - min(ggData$X)) / (max(ggData$Y) - min(ggData$Y))
  bgCells <- FALSE
  
  if (!is.null(inpsub2) && length(inpsub2) != nlevels(factor(ggData$sub))) {
    bgCells <- TRUE
    ggData2 <- ggData[!sub %in% inpsub2]
    ggData <- ggData[sub %in% inpsub2]
  } else {
    ggData2 <- NULL
  }
  
  ggCol <- NULL
  if (!is.na(inpConf[UI == inp1]$fCL)) {
    ggCol <- strsplit(inpConf[UI == inp1]$fCL, "\\|")[[1]]
    names(ggCol) <- levels(factor(ggData$val))
    ggLvl <- levels(factor(ggData$val))[levels(factor(ggData$val)) %in% unique(ggData$val)]
    ggData[, val := factor(val, levels = ggLvl)]
    ggCol <- ggCol[ggLvl]
  }
  
  list(ggData = ggData, ggData2 = ggData2, ggCol = ggCol, rat = rat, bgCells = bgCells)
}


process_gene_data <- function(inpConf, inpMeta, inpdrX, inpdrY, inp1, inpsub1 = NULL, inpsub2 = NULL, h5file, inpGene) {
  #' Process gene data for visualization
  #'
  #' @param inpConf Data frame with configuration settings.
  #' @param inpMeta Data frame with metadata.
  #' @param inpdrX Character string for the X dimension.
  #' @param inpdrY Character string for the Y dimension.
  #' @param inp1 Character string for the main variable.
  #' @param inpsub1 Character string for the first subgroup. Default is NULL.
  #' @param inpsub2 Character string for the second subgroup. Default is NULL.
  #' @param h5file HDF5 file containing gene data.
  #' @param inpGene Data frame with gene information.
  #'
  #' @return A list containing ggData, rat, inp1, and inpsub2.
  
  if (is.null(inpsub1)) {
    inpsub1 <- inpConf$UI[1]
  }
  
  ggData <- inpMeta[, .(X = get(inpConf[UI == inpdrX]$ID),
                        Y = get(inpConf[UI == inpdrY]$ID),
                        sub = get(inpConf[UI == inpsub1]$ID))]
  rat <- (max(ggData$X) - min(ggData$X)) / (max(ggData$Y) - min(ggData$Y))
  
  read_h5_data <- function(h5file, gene) {
    h5data <- h5file[["grp"]][["data"]]
    val <- h5data$read(args = list(gene, quote(expr = )))
    val[val < 0] <- 0
    return(val)
  }
  
  ggData[, val := read_h5_data(h5file, inpGene[inp1])]
  
  ggData_no_val <- ggData[val == 0]
  ggData_with_val <- ggData[val > 0]
  ggData <- rbind(ggData_no_val, ggData_with_val)
  
  list(ggData = ggData, rat = rat, inp1 = inp1, inpsub2 = inpsub2)
}


process_coexpression_data <- function(inpConf, inpMeta, inpdrX, inpdrY, inp1, inp2, inpsub1 = NULL, inpsub2 = NULL, h5file, inpGene) {
  #' Process co-expression data for visualization
  #'
  #' @param inpConf Data frame with configuration settings.
  #' @param inpMeta Data frame with metadata.
  #' @param inpdrX Character string for the X dimension.
  #' @param inpdrY Character string for the Y dimension.
  #' @param inp1 Character string for the first gene.
  #' @param inp2 Character string for the second gene.
  #' @param inpsub1 Character string for the first subgroup. Default is NULL.
  #' @param inpsub2 Character string for the second subgroup. Default is NULL.
  #' @param h5file HDF5 file containing gene data.
  #' @param inpGene Data frame with gene information.
  #'
  #' @return A list containing ggData, rat, inp1, inp2, and inpsub2.
  
  if (is.null(inpsub1)) {
    inpsub1 <- inpConf$UI[1]
  }
  
  ggData <- inpMeta[, c(inpConf[UI == inpdrX]$ID, inpConf[UI == inpdrY]$ID, inpConf[UI == inpsub1]$ID), with = FALSE]
  colnames(ggData) <- c("X", "Y", "sub")
  rat <- (max(ggData$X) - min(ggData$X)) / (max(ggData$Y) - min(ggData$Y))
  
  h5data <- h5file[["grp"]][["data"]]
  ggData$val1 <- h5data$read(args = list(inpGene[inp1], quote(expr = )))
  ggData[val1 < 0]$val1 <- 0
  ggData$val2 <- h5data$read(args = list(inpGene[inp2], quote(expr = )))
  ggData[val2 < 0]$val2 <- 0
  
  list(ggData = ggData, rat = rat, inp1 = inp1, inp2 = inp2, inpsub2 = inpsub2)
}


process_violin_data <- function(inpConf, inpMeta, inp1, inp2, inpsub1 = NULL, inpsub2 = NULL, h5file, inpGene) {
  #' Process violin plot data for visualization
  #'
  #' @param inpConf Data frame with configuration settings.
  #' @param inpMeta Data frame with metadata.
  #' @param inp1 Character string for the main variable.
  #' @param inp2 Character string for the second variable.
  #' @param inpsub1 Character string for the first subgroup. Default is NULL.
  #' @param inpsub2 Character string for the second subgroup. Default is NULL.
  #' @param h5file HDF5 file containing gene data.
  #' @param inpGene Data frame with gene information.
  #'
  #' @return A list containing ggData, ggCol, inp1, and inp2.
  
  if (is.null(inpsub1)) {
    inpsub1 <- inpConf$UI[1]
  } 
  
  ggData <- inpMeta[, c(inpConf[UI == inp1]$ID, inpConf[UI == inpsub1]$ID), with = FALSE]
  colnames(ggData) <- c("X", "sub")
  
  if (inp2 %in% inpConf$UI) { 
    ggData$val <- inpMeta[[inpConf[UI == inp2]$ID]] 
  } else { 
    h5data <- h5file[["grp"]][["data"]] 
    ggData$val <- h5data$read(args = list(inpGene[inp2], quote(expr = ))) 
    ggData[val < 0]$val <- 0 
    set.seed(42) 
    tmpNoise <- rnorm(length(ggData$val)) * diff(range(ggData$val)) / 1000 
    ggData$val <- ggData$val + tmpNoise 
    # h5file$close_all() 
  } 
  
  if (length(inpsub2) != 0 & length(inpsub2) != nlevels(ggData$sub)) { 
    ggData <- ggData[sub %in% inpsub2] 
  } 
  
  ggCol <- strsplit(inpConf[UI == inp1]$fCL, "\\|")[[1]] 
  names(ggCol) <- levels(ggData$X) 
  ggLvl <- levels(ggData$X)[levels(ggData$X) %in% unique(ggData$X)] 
  ggData$X <- factor(ggData$X, levels = ggLvl) 
  ggCol <- ggCol[ggLvl] 
  
  list(ggData = ggData, ggCol = ggCol, inp1 = inp1, inp2 = inp2)
}


process_proportion_data <- function(inpConf, inpMeta, inp1, inp2, inpsub1 = NULL, inpsub2 = NULL) {
  #' Process proportion data for visualization
  #'
  #' @param inpConf Data frame with configuration settings.
  #' @param inpMeta Data frame with metadata.
  #' @param inp1 Character string for the main variable.
  #' @param inp2 Character string for the second variable.
  #' @param inpsub1 Character string for the first subgroup. Default is NULL.
  #' @param inpsub2 Character string for the second subgroup. Default is NULL.
  #'
  #' @return A list containing ggData, ggCol, inp1, and inp2.
  
  if (is.null(inpsub1)) {
    inpsub1 <- inpConf$UI[1]
  }
  
  ggData <- inpMeta[, c(inpConf[UI == inp1]$ID, inpConf[UI == inp2]$ID, inpConf[UI == inpsub1]$ID), with = FALSE]
  colnames(ggData) <- c("X", "grp", "sub")
  
  if (length(inpsub2) != 0 & length(inpsub2) != nlevels(factor(ggData$sub))) {
    ggData <- ggData[sub %in% inpsub2]
  }
  
  ggData <- ggData[, .(nCells = .N), by = c("X", "grp")]
  ggData <- ggData[, {tot <- sum(nCells)
  .SD[, .(pctCells = 100 * sum(nCells) / tot, nCells = nCells), by = "grp"]}, by = "X"]
  
  ggCol <- strsplit(inpConf[UI == inp2]$fCL, "\\|")[[1]]
  names(ggCol) <- levels(factor(ggData$grp))
  ggLvl <- levels(factor(ggData$grp))[levels(factor(ggData$grp)) %in% unique(ggData$grp)]
  ggData$grp <- factor(ggData$grp, levels = ggLvl)
  ggCol <- ggCol[ggLvl]
  
  list(ggData = ggData, ggCol = ggCol, inp1 = inp1, inp2 = inp2)
}


process_bubheat_data <- function(inpConf, inpMeta, inp, inpGrp, inpsub1 = NULL, inpsub2 = NULL, h5file, inpGene, inpScl) {
  #' Process bubble heatmap data for visualization
  #'
  #' @param inpConf Data frame with configuration settings.
  #' @param inpMeta Data frame with metadata.
  #' @param inp Character vector of input genes.
  #' @param inpGrp Character string for the group variable.
  #' @param inpsub1 Character string for the first subgroup. Default is NULL.
  #' @param inpsub2 Character string for the second subgroup. Default is NULL.
  #' @param h5file HDF5 file containing gene data.
  #' @param inpGene Data frame with gene information.
  #' @param inpScl Logical indicating if scaling should be applied.
  #'
  #' @return A list containing ggMat, point_size_mat, and colRange.
  
  if (is.null(inpsub1)) {
    inpsub1 <- inpConf$UI[1]
  }
  
  h5data <- h5file[["grp"]][["data"]]
  ggData <- data.table()
  
  for (iGene in inp) {
    tmp <- inpMeta[, c("sampleID", inpConf[UI == inpsub1]$ID), with = FALSE]
    colnames(tmp) <- c("sampleID", "sub")
    tmp$grpBy <- inpMeta[[inpConf[UI == inpGrp]$ID]]
    tmp$geneName <- iGene
    tmp$val <- h5data$read(args = list(inpGene[iGene], quote(expr = )))
    ggData <- rbindlist(list(ggData, tmp))
  }
  
  if (length(inpsub2) != 0 & length(inpsub2) != nlevels(factor(ggData$sub))) {
    ggData <- ggData[sub %in% inpsub2]
  }
  
  if (uniqueN(ggData$grpBy) <= 1) {
    return("Only 1 group present, unable to plot!")
  }
  if (uniqueN(ggData$geneName) < 2) {
    return("Fewer than 2 genes present, unable to plot!")
  }
  
  ggData$val <- expm1(ggData$val)
  ggData <- ggData[, .(val = mean(val), prop = sum(val > 0) / length(sampleID)), by = c("geneName", "grpBy")]
  ggData$val <- log1p(ggData$val)
  
  colRange <- range(ggData$val)
  if (inpScl) {
    ggData[, val := scale(val), by = "geneName"]
    colRange <- c(-max(abs(range(ggData$val))), max(abs(range(ggData$val))))
  }
  
  ggMat <- dcast.data.table(ggData, geneName ~ grpBy, value.var = "val")
  tmp <- ggMat$geneName
  ggMat <- as.matrix(ggMat[, -1])
  rownames(ggMat) <- tmp
  
  point_size_mat <- dcast.data.table(ggData, geneName ~ grpBy, value.var = "prop")
  point_size_mat <- as.matrix(point_size_mat[, -1])
  rownames(point_size_mat) <- tmp
  
  list(ggMat = ggMat, point_size_mat = point_size_mat, colRange = colRange)
}
