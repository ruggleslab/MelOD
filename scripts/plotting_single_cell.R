source("global.R", local = TRUE)




cList = list(c("grey85","#FFF7EC","#FEE8C8","#FDD49E","#FDBB84",
               "#FC8D59","#EF6548","#D7301F","#B30000","#7F0000"),
             c("#4575B4","#74ADD1","#ABD9E9","#E0F3F8","#FFFFBF",
               "#FEE090","#FDAE61","#F46D43","#D73027")[c(1,1:9,9)],
             c("#FDE725","#AADC32","#5DC863","#27AD81","#21908C",
               "#2C728E","#3B528B","#472D7B","#440154"))
names(cList) = c("White-Red", "Blue-Yellow-Red", "Yellow-Green-Purple")


# Plot theme
sctheme <- function(base_size = 24, XYval = TRUE, Xang = 0, XjusH = 0.5){
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
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  legend
}



cell_plotly <- function(inpConf, inpMeta, inpdrX, inpdrY, inp1, inpsub1 = NULL, inpsub2 = NULL,
                        inpsiz, inpcol, inplab, inpsplit = FALSE, inp2=NULL) {
  
  if (is.null(inpsub1)) {
    inpsub1 <- inpConf$UI[1]
  }
  
  ggData <- inpMeta[, .(X = get(inpConf[UI == inpdrX]$ID),
                        Y = get(inpConf[UI == inpdrY]$ID),
                        val = get(inpConf[UI == inp1]$ID),
                        sub = get(inpConf[UI == inpsub1]$ID))]
  rat <- (max(ggData$X) - min(ggData$X)) / (max(ggData$Y) - min(ggData$Y))
  bgCells <- FALSE
  
  if (!is.null(inpsub2) && length(inpsub2) != nlevels(factor(ggData$sub))) {
    bgCells <- TRUE
    ggData2 <- ggData[!sub %in% inpsub2]
    ggData <- ggData[sub %in% inpsub2]
  }
  
  if (!is.na(inpConf[UI == inp1]$fCL)) {
    ggCol <- strsplit(inpConf[UI == inp1]$fCL, "\\|")[[1]]
    names(ggCol) <- levels(factor(ggData$val))
    ggLvl <- levels(factor(ggData$val))[levels(factor(ggData$val)) %in% unique(ggData$val)]
    ggData[, val := factor(val, levels = ggLvl)]
    ggCol <- ggCol[ggLvl]
  }
  
  p <- plot_ly(ggData, x = ~X, y = ~Y, color = ~val, colors = cList[[inpcol]],
               type = 'scatter', mode = 'markers', marker = list(size = inpsiz, opacity = 0.8),
               text = ~paste('Value:', val, '<br>X:', X, '<br>Y:', Y, '<br>Sub:', sub),
               hoverinfo = 'text')
  
  if (bgCells) {
    p <- p %>% add_trace(data = ggData2, x = ~X, y = ~Y, mode = 'markers',
                         marker = list(size = inpsiz, opacity = 0.8), visible = FALSE)
  }
  
  if (!is.na(inpConf[UI == inp1]$fCL)) {
    p <- p %>% layout(colorway = ggCol)
  } else {
    p <- p %>% colorbar(title = "")
  }
  
  if (inplab) {
    if (!is.numeric(ggData$val)) {
      ggData3 <- ggData[, .(X = mean(X), Y = mean(Y)), by = val]
      p <- p %>% add_annotations(data = ggData3, x = ~X, y = ~Y, text = ~val,
                                 showarrow = TRUE, arrowcolor = "grey10",
                                 font = list(color = "grey10"))
    }
  }
  
  p <- p %>% layout(
    xaxis = list(title = inpdrX, zeroline = FALSE, showline = FALSE, showgrid = TRUE),
    yaxis = list(title = inpdrY, zeroline = FALSE, showline = FALSE, showgrid = TRUE, scaleanchor = "x", scaleratio = rat),
    showlegend = TRUE
  )
  
  if (inpsplit) {
    ggData <- inpMeta[, .(X = get(inpConf[UI == inpdrX]$ID),
                          Y = get(inpConf[UI == inpdrY]$ID),
                          val = get(inpConf[UI == inp2]$ID),
                          sub = get(inpConf[UI == inpsub1]$ID))]
    rat <- (max(ggData$X) - min(ggData$X)) / (max(ggData$Y) - min(ggData$Y))
    
    if (!is.na(inpConf[UI == inp2]$fCL)) {
      ggCol <- strsplit(inpConf[UI == inp2]$fCL, "\\|")[[1]]
      names(ggCol) <- levels(factor(ggData$val))
      ggLvl <- levels(factor(ggData$val))[levels(factor(ggData$val)) %in% unique(ggData$val)]
      ggData[, val := factor(val, levels = ggLvl)]
      ggCol <- ggCol[ggLvl]
    }
    
    p2 <- plot_ly(ggData, x = ~X, y = ~Y, color = ~val, colors = cList[[inpcol]],
                  type = 'scatter', mode = 'markers', marker = list(size = inpsiz, opacity = 0.8),
                  text = ~paste('Value:', val, '<br>X:', X, '<br>Y:', Y, '<br>Sub:', sub),
                  hoverinfo = 'text')
    
    if (bgCells) {
      p2 <- p2 %>% add_trace(data = ggData2, x = ~X, y = ~Y, mode = 'markers',
                             marker = list(size = inpsiz, opacity = 0.8), visible = FALSE)
    }
    if (!is.na(inpConf[UI == inp2]$fCL)) {
      p2 <- p2 %>% layout(colorway = ggCol)
    } else {
      p2 <- p2 %>% colorbar(title = "")
    }
    
    if (inplab) {
      if (!is.numeric(ggData$val)) {
        ggData3 <- ggData[, .(X = mean(X), Y = mean(Y)), by = val]
        p2 <- p2 %>% add_annotations(data = ggData3, x = ~X, y = ~Y, text = ~val,
                                     showarrow = TRUE, arrowcolor = "grey10",
                                     font = list(color = "grey10"))
      }
    }
    
    p2 <- p2 %>% layout(
      xaxis = list(title = inpdrX, zeroline = FALSE, showline = FALSE, showgrid = TRUE, matches = "x"),
      yaxis = list(title = inpdrY, zeroline = FALSE, showline = FALSE, showgrid = TRUE, scaleanchor = "x", scaleratio = rat),
      showlegend = TRUE
    )
    
    p <- subplot(p, p2, nrows = 1, shareX = TRUE, shareY = TRUE, titleX = TRUE, titleY = TRUE)
  }
  return(p)
}



scDRnum <- function(inpConf, inpMeta, inp1, inp2, inpsub1, inpsub2 = NULL,
                    inpH5 = h5_file_path, inpGene, inpsplt = NULL){
  h5file <- H5File$new(inpH5, mode = "r")
  
  if(is.null(inpsub1)){inpsub1 = inpConf$UI[1]}
  # Prepare ggData
  ggData = inpMeta[, c(inpConf[UI == inp1]$ID, inpConf[UI == inpsub1]$ID),
                   with = FALSE]
  colnames(ggData) = c("group", "sub")
  h5data <- h5file[["grp"]][["data"]]
  ggData$val2 = h5data$read(args = list(inpGene[inp2], quote(expr=)))
  ggData[val2 < 0]$val2 = 0
  h5file$close_all()
  if(length(inpsub2) != 0 & length(inpsub2) != nlevels(ggData$sub)){
    ggData = ggData[sub %in% inpsub2]
  }
  
  # Split inp1 if necessary
  if(is.na(inpConf[UI == inp1]$fCL)){
    print ('yes')
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


gene_plotly <- function(inpConf, inpMeta, inpdrX, inpdrY, inp1, inpsub1 = NULL, inpsub2 = NULL,
                            inpH5 = h5_file_path, inpGene, inpsiz, inpcol, inpsplit, inp2) {
  # Default subset if not provided
  if (is.null(inpsub1)) {
    inpsub1 <- inpConf$UI[1]
  }
  
  # Prepare data
  ggData <- inpMeta[, .(X = get(inpConf[UI == inpdrX]$ID),
                        Y = get(inpConf[UI == inpdrY]$ID),
                        sub = get(inpConf[UI == inpsub1]$ID))]
  rat <- (max(ggData$X) - min(ggData$X)) / (max(ggData$Y) - min(ggData$Y))
  
  # Read H5 data
  h5file <- H5File$new(inpH5, mode = "r")
  h5data <- h5file[["grp"]][["data"]]
  ggData[, val := h5data$read(args = list(inpGene[inp1], quote(expr = )))]
  ggData[val < 0, val := 0]
  h5file$close_all()
  
  # Handle subset2 if specified
  bgCells <- FALSE
  if (!is.null(inpsub2) && length(inpsub2) != nlevels(factor(ggData$sub))) {
    bgCells <- TRUE
    ggData2 <- ggData[!sub %in% inpsub2]
    ggData <- ggData[sub %in% inpsub2]
  }
  
  # Create plot
  p <- plot_ly(ggData, x = ~X, y = ~Y, color = ~val, colors = cList[[inpcol]],
               type = 'scatter', mode = 'markers', marker = list(size = inpsiz),
               text = ~paste('Value:', val, '<br>X:', X, '<br>Y:', Y, '<br>Sub:', sub),
               hoverinfo = 'text')
  
  # Add background cells if applicable
  if (bgCells) {
    p <- p %>% add_trace(data = ggData2, x = ~X, y = ~Y, mode = 'markers',
                         marker = list(size = inpsiz, opacity = 0.8), visible = FALSE)
  }
  
  # Add colorbar
  p <- p %>% colorbar(title = inp1, len = 0.5, thickness = 15, x = 0.7, xanchor = 'center',
                      y = -0.3, yanchor = 'bottom', orientation = 'h')
  
  # Final layout adjustments
  p <- p %>% layout(
    xaxis = list(title = inpdrX, zeroline = FALSE, showline = FALSE, showgrid = TRUE),
    yaxis = list(title = inpdrY, zeroline = FALSE, showline = FALSE, showgrid = TRUE, scaleanchor = "x", scaleratio = rat),
    showlegend = TRUE
  )
  
  if (inpsplit) {
    h5file <- H5File$new(inpH5, mode = "r")
    h5data <- h5file[["grp"]][["data"]]
    ggData[, val := h5data$read(args = list(inpGene[inp2], quote(expr = )))]
    ggData[val < 0, val := 0]
    h5file$close_all()
    p2 <- plot_ly(ggData, x = ~X, y = ~Y, color = ~val, colors = cList[[inpcol]],
                 type = 'scatter', mode = 'markers', marker = list(size = inpsiz),
                 text = ~paste('Value:', val, '<br>X:', X, '<br>Y:', Y, '<br>Sub:', sub),
                 hoverinfo = 'text')
    
    if (bgCells) {
      p2 <- p2 %>% add_trace(data = ggData2, x = ~X, y = ~Y, mode = 'markers',
                           marker = list(size = inpsiz, opacity = 0.8), visible = FALSE)
    }
  
    p2 <- p2 %>% colorbar(title = inp1, len = 0.5, thickness = 15, x = 0.7, xanchor = 'center',
                        y = -0.3, yanchor = 'bottom', orientation = 'h')
    
    p2 <- p2 %>% layout(
      xaxis = list(title = inpdrX, zeroline = FALSE, showline = FALSE, showgrid = TRUE, matches = "x"),
      yaxis = list(title = inpdrY, zeroline = FALSE, showline = FALSE, showgrid = TRUE, scaleanchor = "x", scaleratio = rat),
      showlegend = TRUE
    )
    
    p <- subplot(p, p2)
    }
  return(p)
}



bilinear <- function(x,y,xy,Q11,Q21,Q12,Q22){ 
  oup = (xy-x)*(xy-y)*Q11 + x*(xy-y)*Q21 + (xy-x)*y*Q12 + x*y*Q22 
  oup = oup / (xy*xy) 
  return(oup) 
} 


# Helper function to generate the coexpression legend
generate_legend_data <- function(inp1, inp2, inpcol) {
  # Generate coex color palette
  cInp = strsplit(inpcol, "; ")[[1]]
  c10 = if (cInp[1] == "Red (Gene1)") c(255, 0, 0) else if (cInp[1] == "Orange (Gene1)") c(255, 140, 0) else c(0, 255, 0)
  c01 = if (cInp[2] == "Green (Gene2)") c(0, 255, 0) else c(0, 0, 255)
  c00 = c(217, 217, 217)
  c11 = c10 + c01
  
  nGrid = 16
  nPad = 2
  nTot = nGrid + nPad * 2
  gg = data.table(v1 = rep(0:nTot, nTot + 1), v2 = sort(rep(0:nTot, nTot + 1)))
  gg$vv1 = gg$v1 - nPad
  gg[vv1 < 0]$vv1 = 0
  gg[vv1 > nGrid]$vv1 = nGrid
  gg$vv2 = gg$v2 - nPad
  gg[vv2 < 0]$vv2 = 0
  gg[vv2 > nGrid]$vv2 = nGrid
  gg$cR = bilinear(gg$vv1, gg$vv2, nGrid, c00[1], c10[1], c01[1], c11[1])
  gg$cG = bilinear(gg$vv1, gg$vv2, nGrid, c00[2], c10[2], c01[2], c11[2])
  gg$cB = bilinear(gg$vv1, gg$vv2, nGrid, c00[3], c10[3], c01[3], c11[3])
  gg$cMix = rgb(gg$cR, gg$cG, gg$cB, maxColorValue = 255)
  gg = gg[, .(v1, v2, cMix)]
  
  return(gg)
}

# Main function to create the coexpression plot
coexpression_plotly <- function(inpConf, inpMeta, inpdrX, inpdrY, inp1, inp2, 
                                inpsub1, inpsub2, inpH5, inpGene, 
                                inpsiz, inpcol) {
  if (is.null(inpsub1)) {
    inpsub1 <- inpConf$UI[1]
  }
  
  # Prepare data
  ggData <- inpMeta[, c(inpConf[UI == inpdrX]$ID, inpConf[UI == inpdrY]$ID, inpConf[UI == inpsub1]$ID), with = FALSE]
  colnames(ggData) <- c("X", "Y", "sub")
  rat <- (max(ggData$X) - min(ggData$X)) / (max(ggData$Y) - min(ggData$Y))
  
  h5file <- H5File$new(inpH5, mode = "r")
  h5data <- h5file[["grp"]][["data"]]
  ggData$val1 <- h5data$read(args = list(inpGene[inp1], quote(expr = )))
  ggData[val1 < 0]$val1 <- 0
  ggData$val2 <- h5data$read(args = list(inpGene[inp2], quote(expr = )))
  ggData[val2 < 0]$val2 <- 0
  h5file$close_all()
  
  bgCells <- FALSE
  if (length(inpsub2) != 0 & length(inpsub2) != nlevels(ggData$sub)) {
    bgCells <- TRUE
    ggData2 <- ggData[!sub %in% inpsub2]
    ggData <- ggData[sub %in% inpsub2]
  }
  
  # Generate coex color palette and legend data
  legend_data <- generate_legend_data(inp1, inp2, inpcol)
  cInp = strsplit(inpcol, "; ")[[1]]
  c10 = if (cInp[1] == "Red (Gene1)") c(255, 0, 0) else if (cInp[1] == "Orange (Gene1)") c(255, 140, 0) else c(0, 255, 0)
  c01 = if (cInp[2] == "Green (Gene2)") c(0, 255, 0) else c(0, 0, 255)
  c00 = c(217, 217, 217)
  c11 = c10 + c01
  
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
  
  # Map colours
  ggData$v1 <- round(nTot * ggData$val1 / max(ggData$val1))
  ggData$v2 <- round(nTot * ggData$val2 / max(ggData$val2))
  ggData$v0 <- ggData$v1 + ggData$v2
  ggData <- gg[ggData, on = .(v1, v2)]
  
  # Plotly plot
  p <- plot_ly(data = ggData, x = ~X, y = ~Y, color = ~cMix, colors = ggData$cMix,
               type = 'scatter', mode = 'markers', marker = list(size = inpsiz))
  
  if (bgCells) {
    p <- add_trace(p, data = ggData2, x = ~X, y = ~Y, type = 'scatter', mode = 'markers', 
                   marker = list(size = inpsiz, color = 'snow2'))
  }
  
  p <- p %>% layout(xaxis = list(title = inpdrX), yaxis = list(title = inpdrY))
  
  p <- layout(p, yaxis = list(scaleanchor = "x", scaleratio = rat))
  
  # Add legend
  legend_plot <- plot_ly(data = legend_data, x = ~v1, y = ~v2, type = 'scatter', mode = 'markers',
                         marker = list(symbol = 'square', color = ~cMix, size = 10), showlegend = FALSE) %>%
    layout(xaxis = list(title = inp1, tickvals = c(0, nTot), ticktext = c("low", "high")),
           yaxis = list(title = inp2, tickvals = c(0, nTot), ticktext = c("low", "high")),
           margin = list(l = 50, r = 50, b = 50, t = 50))
  # Combine main plot and legend
  combined_plot <- subplot(p, legend_plot, widths = c(0.75, 0.25), shareY = TRUE)
  
  return(combined_plot)
}


scDRcoexNum <- function(inpConf, inpMeta, inp1, inp2, 
                        inpsub1, inpsub2, inpH5 = h5_file_path, inpGene){ 
  if(is.null(inpsub1)){inpsub1 = inpConf$UI[1]} 
  # Prepare ggData 
  ggData = inpMeta[, c(inpConf[UI == inpsub1]$ID), with = FALSE] 
  colnames(ggData) = c("sub") 
  h5file <- H5File$new(inpH5, mode = "r") 
  h5data <- h5file[["grp"]][["data"]] 
  ggData$val1 = h5data$read(args = list(inpGene[inp1], quote(expr=))) 
  ggData[val1 < 0]$val1 = 0 
  ggData$val2 = h5data$read(args = list(inpGene[inp2], quote(expr=))) 
  ggData[val2 < 0]$val2 = 0 
  h5file$close_all() 
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


sc_violin_plotly <- function(inpConf, inpMeta, inp1, inp2, 
                           inpsub1, inpsub2, inpH5, inpGene, 
                           inptyp, inppts){ 
  
  if(is.null(inpsub1)){inpsub1 = inpConf$UI[1]} 
  
  # Prepare ggData 
  ggData <- inpMeta[, c(inpConf[UI == inp1]$ID, inpConf[UI == inpsub1]$ID), with = FALSE] 
  colnames(ggData) <- c("X", "sub") 
  
  # Load in either cell meta or gene expr
  if(inp2 %in% inpConf$UI){ 
    ggData$val <- inpMeta[[inpConf[UI == inp2]$ID]] 
  } else { 
    h5file <- H5File$new(inpH5, mode = "r") 
    h5data <- h5file[["grp"]][["data"]] 
    ggData$val <- h5data$read(args = list(inpGene[inp2], quote(expr=))) 
    ggData[val < 0]$val <- 0 
    set.seed(42) 
    tmpNoise <- rnorm(length(ggData$val)) * diff(range(ggData$val)) / 1000 
    ggData$val <- ggData$val + tmpNoise 
    h5file$close_all() 
  } 
  if(length(inpsub2) != 0 & length(inpsub2) != nlevels(ggData$sub)){ 
    ggData <- ggData[sub %in% inpsub2] 
  } 
  
  # Do factoring 
  ggCol <- strsplit(inpConf[UI == inp1]$fCL, "\\|")[[1]] 
  names(ggCol) <- levels(ggData$X) 
  ggLvl <- levels(ggData$X)[levels(ggData$X) %in% unique(ggData$X)] 
  ggData$X <- factor(ggData$X, levels = ggLvl) 
  ggCol <- ggCol[ggLvl] 
  
  # Actual Plotly plot
  plot <- plot_ly(ggData, x = ~X, y = ~val, type = ifelse(inptyp == "violin", "violin", "box"), 
                  color = ~X, colors = ggCol) %>%
    layout(
      xaxis = list(title = inp1, tickangle = 45),
      yaxis = list(title = inp2),
      showlegend = FALSE,
      font = list(size = 12)
    )
  
  if(inppts){
    plot <- plot %>% add_trace(type = 'scatter', mode = 'markers', 
                               marker = list(size = 8), 
                               x = ~X, y = ~val)
  }
  
  return(plot)
}



proportion_plotly <- function(inpConf, inpMeta, inp1, inp2, inpsub1, inpsub2, 
                         inptyp, inpflp){ 
  if(is.null(inpsub1)){inpsub1 = inpConf$UI[1]} 
  
  # Prepare ggData 
  ggData <- inpMeta[, c(inpConf[UI == inp1]$ID, inpConf[UI == inp2]$ID, 
                        inpConf[UI == inpsub1]$ID), with = FALSE] 
  colnames(ggData) <- c("X", "grp", "sub") 
  if(length(inpsub2) != 0 & length(inpsub2) != nlevels(ggData$sub)){ 
    ggData <- ggData[sub %in% inpsub2] 
  } 
  ggData <- ggData[, .(nCells = .N), by = c("X", "grp")] 
  ggData <- ggData[, {tot <- sum(nCells) 
  .SD[,.(pctCells = 100 * sum(nCells) / tot, 
         nCells = nCells), by = "grp"]}, by = "X"] 
  
  # Do factoring 
  ggCol <- strsplit(inpConf[UI == inp2]$fCL, "\\|")[[1]] 
  names(ggCol) <- levels(ggData$grp) 
  ggLvl <- levels(ggData$grp)[levels(ggData$grp) %in% unique(ggData$grp)] 
  ggData$grp <- factor(ggData$grp, levels = ggLvl) 
  ggCol <- ggCol[ggLvl] 
  
  # Actual Plotly plot
  if(inptyp == "Proportion"){ 
    plot <- plot_ly(ggData, x = if(inpflp) ~pctCells else ~X, 
                    y = if(inpflp) ~X else ~pctCells, 
                    type = 'bar', color = ~grp, colors = ggCol, orientation = if(inpflp) 'h' else 'v') %>%
      layout(xaxis = list(title = if(inpflp) "Cell Proportion (%)" else inp1, tickangle = if(inpflp) 0 else 45),
             yaxis = list(title = if(inpflp) inp1 else "Cell Proportion (%)"),
             barmode = 'stack')
  } else { 
    plot <- plot_ly(ggData, x = if(inpflp) ~nCells else ~X, 
                    y = if(inpflp) ~X else ~nCells, 
                    type = 'bar', color = ~grp, colors = ggCol, orientation = if(inpflp) 'h' else 'v') %>%
      layout(xaxis = list(title = if(inpflp) "Number of Cells" else inp1, tickangle = if(inpflp) 0 else 45),
             yaxis = list(title = if(inpflp) inp1 else "Number of Cells"),
             barmode = 'stack')
  }
  
  plot <- plot %>% layout(font = list(size = 12), legend = list(orientation = "v"))
  
  return(plot)
}


scGeneList <- function(inp, inpGene){ 
  geneList <- data.table(gene = unique(trimws(inp)), present = TRUE) 
  geneList[!gene %in% names(inpGene)]$present = FALSE 
  return(geneList) 
}



bubheat_plotly <- function(inpConf, inpMeta, inp, inpGrp, inpPlt, 
                             inpsub1, inpsub2, inpH5, inpGene, inpScl, inpRow, inpCol, 
                             inpcols){ 
  if(is.null(inpsub1)){inpsub1 = inpConf$UI[1]} 
  
  # Identify genes that are in our dataset 
  geneList = scGeneList(inp, inpGene) 
  geneList = geneList[present == TRUE] 
  shiny::validate(need(nrow(geneList) <= 50, "More than 50 genes to plot! Please reduce the gene list!")) 
  shiny::validate(need(nrow(geneList) > 1, "Please input at least 2 genes to plot!")) 
  
  # Prepare data
  h5file <- H5File$new(inpH5, mode = "r") 
  h5data <- h5file[["grp"]][["data"]] 
  ggData = data.table() 
  for(iGene in geneList$gene){ 
    tmp = inpMeta[, c("sampleID", inpConf[UI == inpsub1]$ID), with = FALSE] 
    colnames(tmp) = c("sampleID", "sub") 
    tmp$grpBy = inpMeta[[inpConf[UI == inpGrp]$ID]] 
    tmp$geneName = iGene 
    tmp$val = h5data$read(args = list(inpGene[iGene], quote(expr=))) 
    ggData = rbindlist(list(ggData, tmp)) 
  } 
  h5file$close_all() 
  if(length(inpsub2) != 0 & length(inpsub2) != nlevels(ggData$sub)){ 
    ggData = ggData[sub %in% inpsub2] 
  } 
  shiny::validate(need(uniqueN(ggData$grpBy) > 1, "Only 1 group present, unable to plot!")) 
  
  # Aggregate data
  ggData$val = expm1(ggData$val) 
  ggData = ggData[, .(val = mean(val), prop = sum(val > 0) / length(sampleID)), 
                  by = c("geneName", "grpBy")] 
  ggData$val = log1p(ggData$val) 
  

  
  # Scale if required 
  colRange = range(ggData$val) 
  if(inpScl){ 
    ggData[, val := scale(val), by = "geneName"] 
    colRange = c(-max(abs(range(ggData$val))), max(abs(range(ggData$val)))) 
  } 
  
  # hclust row/col if necessary 
  ggMat = dcast.data.table(ggData, geneName ~ grpBy, value.var = "val") 
  tmp = ggMat$geneName 
  ggMat = as.matrix(ggMat[, -1]) 
  rownames(ggMat) = tmp 
  if(inpRow){ 
    row_order = hclust(dist(ggMat))$order
    ggData$geneName = factor(ggData$geneName, levels = rownames(ggMat)[row_order])
  } else { 
    ggData$geneName = factor(ggData$geneName, levels = rev(geneList$gene)) 
  } 
  if(inpCol){ 
    col_order = hclust(dist(t(ggMat)))$order
    ggData$grpBy = factor(ggData$grpBy, levels = colnames(ggMat)[col_order]) 
  } 
  # Convert factors to characters
  ggData$geneName <- as.character(ggData$geneName)
  ggData$grpBy <- as.character(ggData$grpBy)
  # Actual plot according to plot type 
  
  print(ggData)
  if(inpPlt == "Bubbleplot"){ 
    plot <- plot_ly(ggData, x = ~grpBy, y = ~geneName, 
                    color = ~val, size = ~prop, 
                    type = 'scatter', mode = 'markers', 
                    marker = list(sizemode = 'diameter')) %>%
      layout(title = "Bubble Plot", 
             xaxis = list(title = inpGrp, tickangle = 45), 
             yaxis = list(title = "Genes"), 
             coloraxis = list(colorbar = list(title = "Expression"))) 
  } else { 
    plot <- heatmaply(ggMat, 
                      Rowv = if(inpRow) TRUE else NULL, 
                      Colv = if(inpCol) TRUE else NULL, 
                      scale = if(inpScl) "row" else "none", 
                      colors = cList[[inpcols]], 
                      fontsize_row = 12, 
                      fontsize_col = 12, 
                      main = "Heatmap")
  } 
  
  return(plot)
}
