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


