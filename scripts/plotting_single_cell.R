source("global.R", local = TRUE)

cList = list(c("#d9d9d9","#FFF7EC","#FEE8C8","#FDD49E","#FDBB84",
               "#FC8D59","#EF6548","#D7301F","#B30000","#7F0000"),
             c("#4575B4","#74ADD1","#ABD9E9","#E0F3F8","#FFFFBF",
               "#FEE090","#FDAE61","#F46D43","#D73027")[c(1,1:9,9)],
             c("#FDE725","#AADC32","#5DC863","#27AD81","#21908C",
               "#2C728E","#3B528B","#472D7B","#440154"))
names(cList) = c("White-Red", "Blue-Yellow-Red", "Yellow-Green-Purple")


# Plot theme
sctheme <- function(base_size = 24, XYval = TRUE, Xang = 0, XjusH = 0.5){
  #' Generate a custom ggplot2 theme
  #'
  #' @param base_size Numeric value for base font size
  #' @param XYval Logical indicating whether to show axis values
  #' @param Xang Numeric value for x-axis text angle
  #' @param XjusH Numeric value for x-axis text horizontal justification
  #'
  #' @return A ggplot2 theme object
  
  oupTheme = theme(
    text =             element_text(size = base_size, family = "Helvetica"),
    panel.background = element_rect(fill = "white", colour = NA),
    axis.line =   element_line(colour = "black"),
    axis.ticks =  element_line(colour = "black", size = base_size / 20),
    axis.title =  element_text(face = "bold"),
    axis.text =   element_text(size = base_size),
    axis.text.x = element_text(angle = Xang, hjust = XjusH),
    legend.position = "bottom",
    legend.key =      element_rect(colour = NA, fill = NA)
  )
  if(!XYval){
    oupTheme = oupTheme + theme(
      axis.text.x = element_blank(), axis.ticks.x = element_blank(),
      axis.text.y = element_blank(), axis.ticks.y = element_blank())
  }
  return(oupTheme)
}


# Function to extract legend
g_legend <- function(a.gplot){
  #' Extract legend from a ggplot2 plot
  #'
  #' @param a.gplot A ggplot2 plot object
  #'
  #' @return A gtable object representing the legend
  
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  legend
}


cell_plotly <- function(processed_data, inpsiz, inpdrX, inpdrY, inpcol, inplab) {
  #' Create a Plotly scatter plot for cell data
  #'
  #' @param processed_data List containing processed data for plotting
  #' @param inpsiz Numeric value for marker size
  #' @param inpdrX Character string for the X dimension
  #' @param inpdrY Character string for the Y dimension
  #' @param inpcol Numeric index for color palette
  #' @param inplab Logical indicating whether to add labels
  #'
  #' @return A Plotly scatter plot object
  
  ggData <- processed_data$ggData
  ggData2 <- processed_data$ggData2
  ggCol <- processed_data$ggCol
  rat <- processed_data$rat
  bgCells <- processed_data$bgCells
  
  p <- plot_ly(
    ggData,
    x = ~X,
    y = ~Y,
    color = ~val,
    colors = cList[[inpcol]],
    type = 'scatter',
    mode = 'markers',
    marker = list(size = inpsiz, opacity = 0.8),
    text = ~paste(
      'Value:', val,
      '<br>X:', sprintf('%.2f', X),
      '<br>Y:', sprintf('%.2f', Y),
      '<br>Sub:', sub
    ),
    hoverinfo = 'text'
  )
  
  if (bgCells) {
    p <- p %>% add_trace(
      data = ggData2,
      x = ~X,
      y = ~Y,
      mode = 'markers',
      marker = list(size = inpsiz, opacity = 0.8),
      visible = FALSE
    )
  }
  
  if (!is.null(ggCol)) {
    p <- p %>% layout(colorway = ggCol)
  } else {
    p <- p %>% colorbar(title = "")
  }
  
  if (inplab && !is.numeric(ggData$val)) {
    ggData3 <- ggData[, .(X = mean(X), Y = mean(Y)), by = val]
    p <- p %>% add_annotations(
      data = ggData3, 
      x = ~X, 
      y = ~Y, 
      text = ~val,
      showarrow = FALSE, 
      font = list(
        color = "black", 
        size = 14,  # Increase font size for better readability
        family = "Arial"
      ),
      bgcolor = "rgba(255, 255, 255, 0.5)"  # Add a semi-transparent white background for better contrast
    )
  }
  
  p <- p %>% layout(
    xaxis = list(title = inpdrX, zeroline = TRUE, showline = FALSE, showgrid = TRUE),
    yaxis = list(
      title = inpdrY,
      zeroline = TRUE,
      showline = FALSE,
      showgrid = TRUE,
      scaleanchor = "x",
      scaleratio = rat
    ),
    showlegend = TRUE
  )
  
  return(p)
}


gene_plotly <- function(processed_data, inpdrX, inpdrY, inpcol, inpsiz) {
  #' Create a Plotly scatter plot for gene data
  #'
  #' @param processed_data List containing processed data for plotting
  #' @param inpdrX Character string for the X dimension
  #' @param inpdrY Character string for the Y dimension
  #' @param inpcol Numeric index for color palette
  #' @param inpsiz Numeric value for marker size
  #'
  #' @return A Plotly scatter plot object
  
  ggData <- processed_data$ggData
  rat <- processed_data$rat
  inp1 <- processed_data$inp1
  inpsub2 <- processed_data$inpsub2
  
  create_plot <- function(data, gene, inpcol, show_legend = FALSE) {
    plot_ly(
      data,
      x = ~X,
      y = ~Y,
      color = ~val,
      colors = cList[[inpcol]],
      type = 'scatter',
      mode = 'markers',
      marker = list(size = inpsiz),
      text = ~paste(
        'Value:', sprintf('%.2f', val),
        '<br>Gene:', gene,
        '<br>Sub:', sub
      ),
      hoverinfo = 'text'
    )
  }
  
  if (!is.null(inpsub2) && length(inpsub2) != nlevels(factor(ggData$sub))) {
    ggData2 <- ggData[sub %in% inpsub2]
    ggData <- ggData[!sub %in% inpsub2]
    p <- create_plot(ggData2, inp1, inpcol)
    p <- p %>% add_trace(
      data = ggData,
      x = ~X,
      y = ~Y,
      mode = 'markers',
      marker = list(size = inpsiz),
      visible = FALSE
    )
  } else {
    p <- create_plot(ggData, inp1, inpcol) 
  }
  
  p <- p %>% layout(
    xaxis = list(
      title = inpdrX,
      zeroline = TRUE,
      showline = FALSE,
      showgrid = TRUE
    ),
    yaxis = list(
      title = inpdrY,
      zeroline = TRUE,
      showline = FALSE,
      showgrid = TRUE,
      scaleanchor = "x",
      scaleratio = rat
    ),
    showlegend = FALSE
  ) %>% colorbar(title = inp1, len = 0.3, thickness = 15)
  
  return(p)
}


scDRnum <- function(inpConf, inpMeta, inp1, inp2, inpsub1, inpsub2 = NULL, h5file, inpGene, inpsplt = NULL){
  #' Compute summary statistics for single-cell RNA-seq data
  #'
  #' @param inpConf Data frame with configuration settings
  #' @param inpMeta Data frame with metadata
  #' @param inp1 Character string for the first variable
  #' @param inp2 Character string for the second variable
  #' @param inpsub1 Character string for the first subgroup
  #' @param inpsub2 Character string for the second subgroup (optional)
  #' @param h5file HDF5 file containing gene data
  #' @param inpGene Data frame with gene information
  #' @param inpsplt Character string indicating how to split inp1 (optional)
  #'
  #' @return A data table with summary statistics
  
  if(is.null(inpsub1)){inpsub1 = inpConf$UI[1]}
  # Prepare ggData
  ggData = inpMeta[, c(inpConf[UI == inp1]$ID, inpConf[UI == inpsub1]$ID),
                   with = FALSE]
  colnames(ggData) = c("group", "sub")
  h5data <- h5file[["grp"]][["data"]]
  ggData$val2 = h5data$read(args = list(inpGene[inp2], quote(expr=)))
  ggData[val2 < 0]$val2 = 0
  # h5file$close_all()
  if(length(inpsub2) != 0 & length(inpsub2) != nlevels(ggData$sub)){
    ggData = ggData[sub %in% inpsub2]
  }
  
  # Split inp1 if necessary
  if(is.na(inpConf[UI == inp1]$fCL)){
    if(inpsplt == "Quartile"){nBk = 4}
    if(inpsplt == "Decile"){nBk = 10}
    ggData$group = cut(ggData$group, breaks = nBk)
  }
  
  # Actual data.table
  ggData$express = FALSE
  ggData[val2 > 0]$express = TRUE
  ggData1 = ggData[express == TRUE, .(nExpress = .N), by = "group"]
  ggData = ggData[, .(nCells = .N), by = "group"]
  ggData = ggData1[ggData, on = "group"]
  ggData = ggData[, c("group", "nCells", "nExpress"), with = FALSE]
  ggData[is.na(nExpress)]$nExpress = 0
  ggData$pctExpress = 100 * ggData$nExpress / ggData$nCells
  ggData = ggData[order(group)]
  colnames(ggData)[3] = paste0(colnames(ggData)[3], "_", inp2)
  return(ggData)
}


bilinear <- function(x, y, xy, Q11, Q21, Q12, Q22) {
  #' bilinear
  #'
  #' Performs bilinear interpolation based on surrounding values in a grid.
  #'
  #' @param x Numeric value for x-coordinate.
  #' @param y Numeric value for y-coordinate.
  #' @param xy Numeric value representing grid size.
  #' @param Q11 Numeric value at (0, 0).
  #' @param Q21 Numeric value at (1, 0).
  #' @param Q12 Numeric value at (0, 1).
  #' @param Q22 Numeric value at (1, 1).
  #'
  #' @return Interpolated value based on bilinear interpolation.
  
  oup <- (xy - x) * (xy - y) * Q11 + x * (xy - y) * Q21 + (xy - x) * y * Q12 + x * y * Q22
  oup <- oup / (xy * xy)
  return(oup)
}


generate_legend_data <- function(inp1, inp2, inpcol) {
  #' generate_legend_data
  #'
  #' Generates a coexpression legend based on input gene colors and grid properties.
  #'
  #' @param inp1 Input gene 1 identifier for generating the legend.
  #' @param inp2 Input gene 2 identifier for generating the legend.
  #' @param inpcol Color codes for genes, formatted as "Gene1 Color; Gene2 Color".
  #'
  #' @return A data table with color mappings for the legend.
  
  cInp <- strsplit(inpcol, "; ")[[1]]
  c10 <- if (cInp[1] == "Red (Gene1)") c(255, 0, 0) else if (cInp[1] == "Orange (Gene1)") c(255, 140, 0) else c(0, 255, 0)
  c01 <- if (cInp[2] == "Green (Gene2)") c(0, 255, 0) else c(0, 0, 255)
  c00 <- c(217, 217, 217)
  c11 <- c10 + c01
  
  nGrid <- 16
  nPad <- 2
  nTot <- nGrid + nPad * 2
  gg <- data.table(v1 = rep(0:nTot, nTot + 1), v2 = sort(rep(0:nTot, nTot + 1)))
  gg$vv1 <- gg$v1 - nPad
  gg[vv1 < 0]$vv1 <- 0
  gg[vv1 > nGrid]$vv1 <- nGrid
  gg$vv2 <- gg$v2 - nPad
  gg[vv2 < 0]$vv2 <- 0
  gg[vv2 > nGrid]$vv2 <- nGrid
  gg$cR <- bilinear(gg$vv1, gg$vv2, nGrid, c00[1], c10[1], c01[1], c11[1])
  gg$cG <- bilinear(gg$vv1, gg$vv2, nGrid, c00[2], c10[2], c01[2], c11[2])
  gg$cB <- bilinear(gg$vv1, gg$vv2, nGrid, c00[3], c10[3], c01[3], c11[3])
  gg$cMix <- rgb(gg$cR, gg$cG, gg$cB, maxColorValue = 255)
  gg <- gg[, .(v1, v2, cMix)]
  
  return(gg)
}


coexpression_plotly <- function(processed_data, inpdrX, inpdrY, inpcol, inpsiz) {
  #' coexpression_plotly
  #'
  #' Generates a coexpression UMAP plot using Plotly for visualizing gene expression levels.
  #'
  #' @param processed_data A list containing processed gene expression data, including ggData, ratio, and gene identifiers.
  #' @param inpdrX The X-axis label for the UMAP plot.
  #' @param inpdrY The Y-axis label for the UMAP plot.
  #' @param inpcol The color scheme used for coexpression in the legend.
  #' @param inpsiz Numeric value defining marker size.
  #'
  #' @return A Plotly object representing the coexpression UMAP plot with a custom legend.
  
  ggData <- processed_data$ggData
  rat <- processed_data$rat
  inp1 <- processed_data$inp1
  inp2 <- processed_data$inp2
  inpsub2 <- processed_data$inpsub2
  
  # Generate coex color palette and legend data
  legend_data <- generate_legend_data(inp1, inp2, inpcol)
  
  # Map colours
  nGrid <- 16
  nPad <- 2
  nTot <- nGrid + nPad * 2
  ggData$v1 <- round(nTot * ggData$val1 / max(ggData$val1))
  ggData$v2 <- round(nTot * ggData$val2 / max(ggData$val2))
  
  # Ensure merging doesn't fail silently
  ggData <- merge(ggData, legend_data, by.x = c("v1", "v2"), by.y = c("v1", "v2"), all.x = TRUE)
  if (!"cMix" %in% colnames(ggData)) {
    stop("Merge failed: 'cMix' not found in ggData")
  }
  
  # Format val1 and val2 to two decimal places in hover text
  ggData$text <- paste(
    inp1, ":", sprintf("%.2f", ggData$val1),
    "<br>", inp2, ":", sprintf("%.2f", ggData$val2)
  )
  
  # Filter out the subset of cells if inpsub2 is specified
  if (!is.null(inpsub2) && length(inpsub2) != 0 && length(inpsub2) != nlevels(ggData$sub)) {
    ggData2 <- ggData[!sub %in% inpsub2]
    ggData <- ggData[sub %in% inpsub2]
  } else {
    ggData2 <- NULL
  }
  
  p <- plot_ly()
  
  p <- add_trace(
    p,
    data = ggData[val1 == 0 & val2 == 0],
    x = ~X,
    y = ~Y,
    type = 'scatter',
    mode = 'markers',
    marker = list(size = inpsiz, opacity = 0.8, color = ~cMix),
    showlegend = FALSE
  )
  
  p <- add_trace(
    p,
    data = ggData[val1 > 0 & val2 == 0],
    x = ~X,
    y = ~Y,
    type = 'scatter',
    mode = 'markers',
    marker = list(size = inpsiz, opacity = 0.8, color = ~cMix),
    name = paste(inp1, "Expression"),
    text = ~text,
    hoverinfo = 'text',
    showlegend = FALSE
  )
  
  p <- add_trace(
    p,
    data = ggData[val2 > 0 & val1 == 0],
    x = ~X,
    y = ~Y,
    type = 'scatter',
    mode = 'markers',
    marker = list(size = inpsiz, opacity = 0.8, color = ~cMix),
    name = paste(inp2, "Expression"),
    text = ~text,
    hoverinfo = 'text',
    showlegend = FALSE
  )
  
  p <- add_trace(
    p,
    data = ggData[val1 > 0 & val2 > 0],
    x = ~X,
    y = ~Y,
    type = 'scatter',
    mode = 'markers',
    marker = list(size = inpsiz, color = ~cMix),
    text = ~text,
    hoverinfo = 'text',
    showlegend = FALSE
  )
  
  if (!is.null(ggData2)) {
    p <- add_trace(
      p,
      data = ggData2,
      x = ~X,
      y = ~Y,
      type = 'scatter',
      mode = 'markers',
      marker = list(size = inpsiz, color = 'snow2'),
      showlegend = FALSE,
      visible = FALSE
    )
  }
  
  p <- p %>% layout(
    xaxis = list(title = inpdrX),
    yaxis = list(title = inpdrY, scaleanchor = "x", scaleratio = rat),
    showlegend = FALSE
  )
  
  legend_plot <- plot_ly(
    data = legend_data,
    x = ~v1,
    y = ~v2,
    type = 'scatter',
    mode = 'markers',
    marker = list(symbol = 'square', color = ~cMix, opacity = 0.8, size = 7),
    showlegend = FALSE
  ) %>%
    layout(
      xaxis = list(title = inp1, tickvals = c(0, nTot), ticktext = c("low", "high")),
      yaxis = list(title = inp2, tickvals = c(0, nTot), ticktext = c("low", "high")),
      margin = list(l = 10, r = 10, b = 10, t = 10),
      showlegend = FALSE
    )
  
  invisible_plot <- plot_ly() %>% layout(
    xaxis = list(showline = FALSE, showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
    yaxis = list(showline = FALSE, showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
    showlegend = FALSE
  )
  
  s1 <- subplot(
    legend_plot,
    invisible_plot,
    nrows = 2,
    heights = c(0.3, 0.7),
    shareX = FALSE,
    shareY = FALSE,
    titleX = TRUE,
    titleY = TRUE
  )
  
  p <- subplot(
    p,
    s1,
    nrows = 1,
    widths = c(0.9, 0.1),
    shareX = FALSE,
    shareY = FALSE,
    titleX = TRUE,
    titleY = TRUE
  )
  
  return(p)
}

scDRcoexNum <- function(inpConf, inpMeta, inp1, inp2, inpsub1, inpsub2, h5file, inpGene){
  #' Compute summary statistics for coexpression data
  #'
  #' @param inpConf Data frame with configuration settings
  #' @param inpMeta Data frame with metadata
  #' @param inp1 Character string for the first gene
  #' @param inp2 Character string for the second gene
  #' @param inpsub1 Character string for the first subgroup
  #' @param inpsub2 Character string for the second subgroup
  #' @param h5file HDF5 file containing gene data
  #' @param inpGene Data frame with gene information
  #'
  #' @return A data table with summary statistics
  
  if(is.null(inpsub1)){inpsub1 = inpConf$UI[1]} 
  # Prepare ggData 
  ggData = inpMeta[, c(inpConf[UI == inpsub1]$ID), with = FALSE] 
  colnames(ggData) = c("sub") 
  h5data <- h5file[["grp"]][["data"]] 
  ggData$val1 = h5data$read(args = list(inpGene[inp1], quote(expr=))) 
  ggData[val1 < 0]$val1 = 0 
  ggData$val2 = h5data$read(args = list(inpGene[inp2], quote(expr=))) 
  ggData[val2 < 0]$val2 = 0 
  # h5file$close_all() 
  if(length(inpsub2) != 0 & length(inpsub2) != nlevels(ggData$sub)){ 
    ggData = ggData[sub %in% inpsub2] 
  } 
  
  # Actual data.table 
  ggData$express = "none" 
  ggData[val1 > 0]$express = inp1 
  ggData[val2 > 0]$express = inp2 
  ggData[val1 > 0 & val2 > 0]$express = "both" 
  ggData$express = factor(ggData$express, levels = unique(c("both", inp1, inp2, "none"))) 
  ggData = ggData[, .(nCells = .N), by = "express"] 
  ggData$percent = 100 * ggData$nCells / sum(ggData$nCells) 
  ggData = ggData[order(express)] 
  colnames(ggData)[1] = "expression > 0" 
  return(ggData) 
} 


sc_violin_plotly <- function(processed_data, inptyp, inppts) {
  #' Create a Plotly violin or box plot
  #'
  #' @param processed_data List containing processed data for plotting
  #' @param inptyp Character string indicating plot type ("violin" or "box")
  #' @param inppts Character string indicating points display type
  #'
  #' @return A Plotly violin or box plot object
  
  ggData <- processed_data$ggData
  ggCol <- processed_data$ggCol
  inp1 <- processed_data$inp1
  inp2 <- processed_data$inp2
  
  if (inptyp == "violin") {
    plot <- plot_ly(ggData, x = ~X, y = ~val, color = ~X, colors = ggCol,
                    type = "violin", points = inppts,
                    jitter = 0.1, pointpos = 0,
                    marker = list(size = 5, line = list(width = 0)),
                    line = list(width = 1),
                    box = list(visible = TRUE, fillcolor = ggCol, line = list(width = 1, color = ggCol)),
                    meanline = list(visible = TRUE),
                    text = ~paste("Condition:", sub, "<br>Value:", val)) %>%
      layout(
        yaxis = list(title = inp2),
        xaxis = list(title = inp1, tickangle = 45),
        margin = list(t = 100),
        zeroline = FALSE)
  } else {
    plot <- plot_ly(ggData, x = ~X, y = ~val, color = ~X, colors = ggCol,
                    type = "box", points = inppts,
                    jitter = 0.3, pointpos = 0,
                    marker = list(size = 5, line = list(width = 0)),
                    boxpoints = inppts,
                    box = list(visible = TRUE, fillcolor = ggCol, line = list(width = 1, color = ggCol)),
                    meanline = list(visible = TRUE),
                    text = ~paste("Condition:", sub, "<br>Value:", val)) %>%
      layout(
        yaxis = list(title = inp2),
        xaxis = list(title = inp1, tickangle = 45),
        margin = list(t = 100),
        zeroline = FALSE)
  }
  
  plot <- plot %>%  
    config(modeBarButtonsToAdd = c('drawline', 
                                   'drawopenpath', 
                                   'drawclosedpath', 
                                   'drawcircle', 
                                   'drawrect', 
                                   'eraseshape'))
  
  return(plot)
}

proportion_plotly <- function(processed_data, inptyp, inpflp) {
  #' Create a Plotly bar plot for proportions
  #'
  #' @param processed_data List containing processed data for plotting
  #' @param inptyp Character string indicating plot type ("Proportion" or "Count")
  #' @param inpflp Logical indicating whether to flip the axes
  #'
  #' @return A Plotly bar plot object
  
  ggData <- processed_data$ggData
  ggCol <- processed_data$ggCol
  inp1 <- processed_data$inp1
  inp2 <- processed_data$inp2
  
  if (inptyp == "Proportion") {
    plot <- plot_ly(ggData, x = if (inpflp) ~pctCells else ~X,
                    y = if (inpflp) ~X else ~pctCells,
                    type = 'bar', color = ~grp, colors = ggCol, orientation = if (inpflp) 'h' else 'v') %>%
      layout(xaxis = list(title = if (inpflp) "Cell Proportion (%)" else inp1, tickangle = if (inpflp) 0 else 45),
             yaxis = list(title = if (inpflp) inp1 else "Cell Proportion (%)"),
             barmode = 'stack')
  } else {
    plot <- plot_ly(ggData, x = if (inpflp) ~nCells else ~X,
                    y = if (inpflp) ~X else ~nCells,
                    type = 'bar', color = ~grp, colors = ggCol, orientation = if (inpflp) 'h' else 'v') %>%
      layout(xaxis = list(title = if (inpflp) "Number of Cells" else inp1, tickangle = if (inpflp) 0 else 45),
             yaxis = list(title = if (inpflp) inp1 else "Number of Cells"),
             barmode = 'stack')
  }
  
  plot <- plot %>% layout(font = list(size = 12), legend = list(orientation = "v"))
  
  return(plot)
}


bubheat_plotly <- function(processed_data, inpPlt, inpRow, inpCol, inpScl, inpcols, plot_height = 700) {
  #' Create a Plotly bubble heatmap or heatmap
  #'
  #' @param processed_data List containing processed data for plotting
  #' @param inpPlt Character string indicating plot type ("Bubbleplot" or "Heatmap")
  #' @param inpRow Logical indicating whether to cluster rows
  #' @param inpCol Logical indicating whether to cluster columns
  #' @param inpScl Logical indicating whether to scale data by rows
  #' @param inpcols Numeric index for color palette
  #' @param plot_height Numeric value for plot height
  #'
  #' @return A Plotly bubble heatmap or heatmap object

  ggMat <- processed_data$ggMat
  print(ggMat)
  point_size_mat <- processed_data$point_size_mat
  colRange <- processed_data$colRange

  # Create custom hover text with numerical values formatted to two decimal places
  rows <- rownames(ggMat)
  cols <- colnames(ggMat)
  hover_text_matrix <- outer(
    rows,
    cols,
    Vectorize(function(row, col) {
      value <- ggMat[row, col]
      if (!is.null(point_size_mat)) {
        size_value <- point_size_mat[row, col]
        paste(
          "Row:", row,
          "<br>Column:", col,
          "<br>Value:", sprintf("%.2f", value),
          "<br>Proportion:", sprintf("%.2f", size_value)
        )
      } else {
        paste(
          "Row:", row,
          "<br>Column:", col,
          "<br>Value:", sprintf("%.2f", value)
        )
      }
    })
  )

  plot <- heatmaply(
    ggMat,
    plot_method = "plotly",
    Rowv = if (inpRow) TRUE else FALSE,
    subplot_widths = if (inpRow) c(0.95, 0.05) else NULL,
    branches_lwd = 0.01,
    Colv = if (inpCol) TRUE else FALSE,
    scale = if (inpScl) "row" else "none",
    colors = cList[[inpcols]],
    fontsize_row = 12,
    fontsize_col = 12,
    plot_height = plot_height,
    point_size_mat = if (inpPlt == "Bubbleplot") point_size_mat else NULL,
    point_size_name = if (inpPlt == "Bubbleplot") "Proportion" else NULL,
    custom_hovertext = hover_text_matrix
  )

  return(plot)
}

