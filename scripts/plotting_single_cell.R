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





scDRcell_plotly <- function(inpConf, inpMeta, inpdrX, inpdrY, inp1, inpsub1, inpsub2,
                            inpsiz, inpcol, inpord, inplab) {
  if (is.null(inpsub1)) {
    inpsub1 <- inpConf$UI[1]
  }
  
  # Prepare data
  ggData <- inpMeta[, c(inpConf[UI == inpdrX]$ID, inpConf[UI == inpdrY]$ID,
                        inpConf[UI == inp1]$ID, inpConf[UI == inpsub1]$ID), with = FALSE]
  colnames(ggData) <- c("X", "Y", "val", "sub")
  rat <- (max(ggData$X) - min(ggData$X)) / (max(ggData$Y) - min(ggData$Y))
  bgCells <- FALSE
  
  if (length(inpsub2) != 0 & length(inpsub2) != nlevels(ggData$sub)) {
    bgCells <- TRUE
    ggData2 <- ggData[!sub %in% inpsub2]
    ggData <- ggData[sub %in% inpsub2]
  }
  
  
  if (inpord == "Max-1st") {
    ggData <- ggData[order(val)]
  } else if (inpord == "Min-1st") {
    ggData <- ggData[order(-val)]
  } else if (inpord == "Random") {
    ggData <- ggData[sample(nrow(ggData))]
  }
  
  
  # Do factoring if required
  if (!is.na(inpConf[UI == inp1]$fCL)) {
    ggCol <- strsplit(inpConf[UI == inp1]$fCL, "\\|")[[1]]
    names(ggCol) <- levels(ggData$val)
    ggLvl <- levels(ggData$val)[levels(ggData$val) %in% unique(ggData$val)]
    ggData$val <- factor(ggData$val, levels = ggLvl)
    ggCol <- ggCol[ggLvl]
  }
  
  
  # Plotly plot
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
    ggData3 <- ggData[, .(X = mean(X), Y = mean(Y)), by = "val"]
    p <- p %>% add_annotations(data = ggData3, x = ~X, y = ~Y, text = ~val,
                               showarrow = TRUE, arrowcolor = "grey10",
                               font = list(color = "grey10"))
  }
  
  p <- p %>% layout(
    xaxis = list(title = inpdrX, zeroline = FALSE, showline = FALSE, showgrid = TRUE),
    yaxis = list(title = inpdrY, zeroline = FALSE, showline = FALSE, showgrid = TRUE, scaleanchor = "x", scaleratio = rat),
    showlegend = TRUE
  )
  
  
  return(p)
}





scDRnum <- function(inpConf, inpMeta, inp1, inp2, inpsub1, inpsub2,
                    inpH5 = h5_file_path, inpGene, inpsplt){
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



scDRgene_plotly <- function(inpConf, inpMeta, inpdrX, inpdrY, inp1, inpsub1, inpsub2,
                            inpH5 = h5_file_path, inpGene,
                            inpsiz, inpcol, inpord) {
  if (is.null(inpsub1)) {
    inpsub1 <- inpConf$UI[1]
  }
  
  # Prepare data
  ggData <- inpMeta[, c(inpConf[UI == inpdrX]$ID, inpConf[UI == inpdrY]$ID, inpConf[UI == inpsub1]$ID), with = FALSE]
  colnames(ggData) <- c("X", "Y", "sub")
  rat <- (max(ggData$X) - min(ggData$X)) / (max(ggData$Y) - min(ggData$Y))
  
  h5file <- H5File$new(inpH5, mode = "r")
  h5data <- h5file[["grp"]][["data"]]
  ggData$val = h5data$read(args = list(inpGene[inp1], quote(expr=)))
  ggData[val < 0]$val = 0
  h5file$close_all()
  
  bgCells <- FALSE
  if (length(inpsub2) != 0 & length(inpsub2) != nlevels(ggData$sub)) {
    bgCells <- TRUE
    ggData2 <- ggData[!sub %in% inpsub2]
    ggData <- ggData[sub %in% inpsub2]
  }
  if (inpord == "Max-1st") {
    ggData <- ggData[order(val)]
  } else if (inpord == "Min-1st") {
    ggData <- ggData[order(-val)]
  } else if (inpord == "Random") {
    ggData <- ggData[sample(nrow(ggData))]
  }
  
  
  # Plotly plot
  p <- plot_ly(ggData, x = ~X, y = ~Y, color = ~val, colors = cList[[inpcol]],
               type = 'scatter', mode = 'markers', marker = list(size = inpsiz),
               text = ~paste('Value:', val, '<br>X:', X, '<br>Y:', Y, '<br>Sub:', sub),
               hoverinfo = 'text')
  
  if (bgCells) {
    p <- p %>% add_trace(data = ggData2, x = ~X, y = ~Y, mode = 'markers',
                         marker = list(size = inpsiz, opacity = 0.8), visible = FALSE)
  }
  
  p <- p %>% colorbar(title = inp1, len = 0.5, thickness = 15, x = 0.7, xanchor = 'center',
                      y = -0.3, yanchor = 'bottom', orientation = 'h') 
  
  
  
  p <- p %>% layout(
    xaxis = list(title = inpdrX, zeroline = FALSE, showline = FALSE, showgrid = TRUE),
    yaxis = list(title = inpdrY, zeroline = FALSE, showline = FALSE, showgrid = TRUE, scaleanchor = "x", scaleratio = rat),
    showlegend = TRUE
  )
  
  
  
  return(p)
}



