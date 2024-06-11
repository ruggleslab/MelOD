library(shinyhelper)
library(data.table)
library(Matrix)
library(DT)
library(magrittr)
library(ggplot2)
library(ggrepel)
library(hdf5r)
library(ggdendro)
library(gridExtra)



seurat_test_ui <- function(id) {
  
  sc1conf = readRDS("./data/single_cell/sc1conf.rds")
  sc1def  = readRDS("./data/single_cell/sc1def.rds")
  
  
  
fluidPage(
        fluidRow(
            box(
              title = "Inputs", status = "warning", solidHeader = TRUE,
              collapsible = TRUE, collapsed = FALSE,
              width = 12,
              selectInput("sc1a1sub1", "Cell information to subset:",
                          choices = sc1conf[grp == TRUE]$UI,
                          selected = sc1def$grp1),
              uiOutput("sc1a1sub1.ui"),
              actionButton("sc1a1sub1all", "Select all groups", class = "btn btn-primary"),
              actionButton("sc1a1sub1non", "Deselect all groups", class = "btn btn-primary"),
              fluidRow(
                column(
                  6, sliderInput("sc1a1siz", "Point size:", min = 0, max = 4, value = 1.25, step = 0.25),
                  radioButtons("sc1a1psz", "Plot size:", choices = c("Small", "Medium", "Large"),
                               selected = "Medium", inline = TRUE),
                  radioButtons("sc1a1fsz", "Font size:", choices = c("Small", "Medium", "Large"),
                               selected = "Medium", inline = TRUE)
                ),
                column(
                  6, radioButtons("sc1a1asp", "Aspect ratio:", choices = c("Square", "Fixed", "Free"),
                                  selected = "Square", inline = TRUE),
                  checkboxInput("sc1a1txt", "Show axis text", value = FALSE)
                )
              )
          )         
        ),   
        
        fluidRow(
            box(
              title = HTML(paste("Cell Information", downloadButton(("sc1a1oup1.pdf"),icon = icon("save-file", lib = "glyphicon")), downloadButton(("sc1a1oup1.png"), icon = icon("save-file", lib = "glyphicon")))),
              status = "primary", solidHeader = TRUE,
              uiOutput("sc1a1oup1.ui"),
              width = 12,
              fluidRow(
                column(
                  4, selectInput("sc1a1drX", "X-axis:", choices = sc1conf[dimred == TRUE]$UI,
                                  selected = sc1def$dimred[1]),
                  selectInput("sc1a1drY", "Y-axis:", choices = sc1conf[dimred == TRUE]$UI,
                              selected = sc1def$dimred[2])
                ),
                column(
                  4, selectInput("sc1a1inp1", "Cell information:", choices = sc1conf$UI,
                                 selected = sc1def$meta1) %>%
                    helper(type = "inline", size = "m", fade = TRUE,
                           title = "Cell information to colour cells by",
                           content = c("Select cell information to colour cells",
                                       "- Categorical covariates have a fixed colour palette",
                                       paste0("- Continuous covariates are coloured in a ",
                                              "Blue-Yellow-Red colour scheme, which can be ",
                                              "changed in the plot controls")))
                ),
                column(
                  4, 
                    radioButtons("sc1a1col1", "Colour (Continuous data):",
                                 choices = c("White-Red","Blue-Yellow-Red","Yellow-Green-Purple"),
                                 selected = "Blue-Yellow-Red"),
                    radioButtons("sc1a1ord1", "Plot order:",
                                 choices = c("Max-1st", "Min-1st", "Original", "Random"),
                                 selected = "Original", inline = TRUE),
                    checkboxInput("sc1a1lab1", "Show cell info labels", value = TRUE)
                  
                )
              ),
            
              # downloadButton("sc1a1oup1.pdf", "Download PDF"),
              # downloadButton("sc1a1oup1.png", "Download PNG"), br(),
              # div(style="display:inline-block",
              #     numericInput("sc1a1oup1.h", "PDF / PNG height:", width = "138px",
              #                  min = 4, max = 20, value = 6, step = 0.5)),
              # div(style="display:inline-block",
              #     numericInput("sc1a1oup1.w", "PDF / PNG width:", width = "138px",
              #                  min = 4, max = 20, value = 8, step = 0.5)), br(),
              
          )),

          fluidRow(
            
          box(
            title = HTML(paste("Gene Expression", downloadButton(("sc1a1oup2.pdf"),icon = icon("save-file", lib = "glyphicon")), downloadButton(("sc1a1oup2.png"), icon = icon("save-file", lib = "glyphicon")))),
            status = "primary", solidHeader = TRUE,
            width = 12,
            uiOutput("sc1a1oup2.ui"),
            fluidRow(
              column(
                6, selectInput("sc1a1inp2", "Gene name:", choices=NULL) %>%
                  helper(type = "inline", size = "m", fade = TRUE,
                         title = "Gene expression to colour cells by",
                         content = c("Select gene to colour cells by gene expression",
                                     paste0("- Gene expression are coloured in a ",
                                            "White-Red colour scheme which can be ",
                                            "changed in the plot controls")))
              ),
              column(
                6, 
                  radioButtons("sc1a1col2", "Colour:", choices = c("White-Red","Blue-Yellow-Red","Yellow-Green-Purple"),
                               selected = "White-Red"),
                  radioButtons("sc1a1ord2", "Plot order:", choices = c("Max-1st", "Min-1st", "Original", "Random"),
                               selected = "Max-1st", inline = TRUE)
                
              )
            ),
              # downloadButton("sc1a1oup2.pdf", "Download PDF"),
              # downloadButton("sc1a1oup2.png", "Download PNG")
              # div(style="display:inline-block",
              #     numericInput("sc1a1oup2.h", "PDF / PNG height:", width = "138px",
              #                  min = 4, max = 20, value = 6, step = 0.5)),
              # div(style="display:inline-block",
              #     numericInput("sc1a1oup2.w", "PDF / PNG width:", width = "138px",
              #                  min = 4, max = 20, value = 8, step = 0.5))
        )),
        
        fluidRow(
        box(
          title = "Cell numbers / statistics", status = "info", solidHeader = TRUE,
          fluidRow(
        radioButtons("sc1a1splt", "Split continuous cell info into:",
                     choices = c("Quartile", "Decile"),
                     selected = "Decile", inline = TRUE),
        dataTableOutput("sc1a1.dt")
          )
        )
      ),
      
      
    tags$h2("Credit. ShinyCell", tags$style(HTML(".shiny-output-error-validation {color: red; font-weight: bold;}")))
    
  )     
  
}

seurat_test_server <- function(input, output, session) {

  sc1conf = readRDS("./data/single_cell/sc1conf.rds")
  sc1def  = readRDS("./data/single_cell/sc1def.rds")
  sc1gene = readRDS("./data/single_cell/sc1gene.rds")
  sc1meta = readRDS("./data/single_cell/sc1meta.rds")
  h5_file_path <- "./data/single_cell/sc1gexpr.h5"


    ### For all tags and Server-side selectize
    observe_helpers()
    optCrt="{ option_create: function(data,escape) {return('<div class=\"create\"><strong>' + '</strong></div>');} }"
    updateSelectizeInput(session, "sc1a1inp2", choices = names(sc1gene), server = TRUE,
                         selected = sc1def$gene1, options = list(
                           maxOptions = 7, create = TRUE, persist = TRUE, render = I(optCrt)))
    updateSelectizeInput(session, "sc1a3inp1", choices = names(sc1gene), server = TRUE,
                         selected = sc1def$gene1, options = list(
                           maxOptions = 7, create = TRUE, persist = TRUE, render = I(optCrt)))
    updateSelectizeInput(session, "sc1a3inp2", choices = names(sc1gene), server = TRUE,
                         selected = sc1def$gene2, options = list(
                           maxOptions = 7, create = TRUE, persist = TRUE, render = I(optCrt)))
    updateSelectizeInput(session, "sc1b2inp1", choices = names(sc1gene), server = TRUE,
                         selected = sc1def$gene1, options = list(
                           maxOptions = 7, create = TRUE, persist = TRUE, render = I(optCrt)))
    updateSelectizeInput(session, "sc1b2inp2", choices = names(sc1gene), server = TRUE,
                         selected = sc1def$gene2, options = list(
                           maxOptions = 7, create = TRUE, persist = TRUE, render = I(optCrt)))
    updateSelectizeInput(session, "sc1c1inp2", server = TRUE,
                         choices = c(sc1conf[is.na(fID)]$UI,names(sc1gene)),
                         selected = sc1conf[is.na(fID)]$UI[1], options = list(
                           maxOptions = length(sc1conf[is.na(fID)]$UI) + 3,
                           create = TRUE, persist = TRUE, render = I(optCrt)))


    ### Plots for tab a1
    output$sc1a1sub1.ui <- renderUI({
      sub = strsplit(sc1conf[UI == input$sc1a1sub1]$fID, "\\|")[[1]]
      checkboxGroupInput("sc1a1sub2", "Select which cells to show", inline = TRUE,
                         choices = sub, selected = sub)
    })
    observeEvent(input$sc1a1sub1non, {
      sub = strsplit(sc1conf[UI == input$sc1a1sub1]$fID, "\\|")[[1]]
      updateCheckboxGroupInput(session, inputId = "sc1a1sub2", label = "Select which cells to show",
                               choices = sub, selected = NULL, inline = TRUE)
    })
    observeEvent(input$sc1a1sub1all, {
      sub = strsplit(sc1conf[UI == input$sc1a1sub1]$fID, "\\|")[[1]]
      updateCheckboxGroupInput(session, inputId = "sc1a1sub2", label = "Select which cells to show",
                               choices = sub, selected = sub, inline = TRUE)
    })
    output$sc1a1oup1 <- renderPlotly({
      scDRcell_plotly(sc1conf, sc1meta, input$sc1a1drX, input$sc1a1drY, input$sc1a1inp1,
                      input$sc1a1sub1, input$sc1a1sub2,
                      input$sc1a1siz, input$sc1a1col1, input$sc1a1ord1,
                      input$sc1a1fsz, input$sc1a1asp, input$sc1a1txt, input$sc1a1lab1)

      # scDRcell(sc1conf, sc1meta, input$sc1a1drX, input$sc1a1drY, input$sc1a1inp1,
      #          input$sc1a1sub1, input$sc1a1sub2,
      #          input$sc1a1siz, input$sc1a1col1, input$sc1a1ord1,
      #          input$sc1a1fsz, input$sc1a1asp, input$sc1a1txt, input$sc1a1lab1)
      

    })
    output$sc1a1oup1.ui <- renderUI({

      plotlyOutput("sc1a1oup1", height = pList[input$sc1a1psz])
    })
    
    
    output$sc1a1oup1.pdf <- downloadHandler(
      filename = function() { paste0("sc1",input$sc1a1drX,"_",input$sc1a1drY,"_",
                                     input$sc1a1inp1,".pdf") },
      content = function(file) { ggsave(
        file, device = "pdf", height = input$sc1a1oup1.h, width = input$sc1a1oup1.w, useDingbats = FALSE,
        plot = scDRcell(sc1conf, sc1meta, input$sc1a1drX, input$sc1a1drY, input$sc1a1inp1,
                        input$sc1a1sub1, input$sc1a1sub2,
                        input$sc1a1siz, input$sc1a1col1, input$sc1a1ord1,
                        input$sc1a1fsz, input$sc1a1asp, input$sc1a1txt, input$sc1a1lab1) )
      })
    output$sc1a1oup1.png <- downloadHandler(
      filename = function() { paste0("sc1",input$sc1a1drX,"_",input$sc1a1drY,"_",
                                     input$sc1a1inp1,".png") },
      content = function(file) { ggsave(
        file, device = "png", height = input$sc1a1oup1.h, width = input$sc1a1oup1.w,
        plot = scDRcell(sc1conf, sc1meta, input$sc1a1drX, input$sc1a1drY, input$sc1a1inp1,
                        input$sc1a1sub1, input$sc1a1sub2,
                        input$sc1a1siz, input$sc1a1col1, input$sc1a1ord1,
                        input$sc1a1fsz, input$sc1a1asp, input$sc1a1txt, input$sc1a1lab1) )
      })
    output$sc1a1.dt <- renderDataTable({
      ggData = scDRnum(sc1conf, sc1meta, input$sc1a1inp1, input$sc1a1inp2,
                       input$sc1a1sub1, input$sc1a1sub2,
                       h5_file_path, sc1gene, input$sc1a1splt)

      datatable(ggData, rownames = FALSE, extensions = "Buttons",
                options = list(pageLength = -1, dom = "tB", buttons = c("copy", "csv", "excel"))) %>%
        formatRound(columns = c("pctExpress"), digits = 2)
    })

    output$sc1a1oup2 <- renderPlotly({
      scDRgene_plotly(sc1conf, sc1meta, input$sc1a1drX, input$sc1a1drY, input$sc1a1inp2,
               input$sc1a1sub1, input$sc1a1sub2,
               h5_file_path, sc1gene,
               input$sc1a1siz, input$sc1a1col2, input$sc1a1ord2,
               input$sc1a1fsz, input$sc1a1asp, input$sc1a1txt)
      
      
      

      # scDRgene(sc1conf, sc1meta, input$sc1a1drX, input$sc1a1drY, input$sc1a1inp2,
      #          input$sc1a1sub1, input$sc1a1sub2,
      #          h5_file_path, sc1gene,
      #          input$sc1a1siz, input$sc1a1col2, input$sc1a1ord2,
      #          input$sc1a1fsz, input$sc1a1asp, input$sc1a1txt)
    })


    output$sc1a1oup2.ui <- renderUI({

      plotlyOutput("sc1a1oup2", height = pList[input$sc1a1psz])
    })
    
    
    
    output$sc1a1oup2.pdf <- downloadHandler(
      filename = function() { paste0("sc1",input$sc1a1drX,"_",input$sc1a1drY,"_",
                                     input$sc1a1inp2,".pdf") },
      content = function(file) { ggsave(
        file, device = "pdf", height = input$sc1a1oup2.h, width = input$sc1a1oup2.w, useDingbats = FALSE,
        plot = scDRgene(sc1conf, sc1meta, input$sc1a1drX, input$sc1a1drY, input$sc1a1inp2,
                        input$sc1a1sub1, input$sc1a1sub2,
                        h5_file_path, sc1gene,
                        input$sc1a1siz, input$sc1a1col2, input$sc1a1ord2,
                        input$sc1a1fsz, input$sc1a1asp, input$sc1a1txt) )
      })
    output$sc1a1oup2.png <- downloadHandler(
      filename = function() { paste0("sc1",input$sc1a1drX,"_",input$sc1a1drY,"_",
                                     input$sc1a1inp2,".png") },
      content = function(file) { ggsave(
        file, device = "png", height = input$sc1a1oup2.h, width = input$sc1a1oup2.w,
        plot = scDRgene(sc1conf, sc1meta, input$sc1a1drX, input$sc1a1drY, input$sc1a1inp2,
                        input$sc1a1sub1, input$sc1a1sub2,
                        h5_file_path, sc1gene,
                        input$sc1a1siz, input$sc1a1col2, input$sc1a1ord2,
                        input$sc1a1fsz, input$sc1a1asp, input$sc1a1txt) )
      })










### Useful stuff
# Colour palette
cList = list(c("grey85","#FFF7EC","#FEE8C8","#FDD49E","#FDBB84",
               "#FC8D59","#EF6548","#D7301F","#B30000","#7F0000"),
             c("#4575B4","#74ADD1","#ABD9E9","#E0F3F8","#FFFFBF",
               "#FEE090","#FDAE61","#F46D43","#D73027")[c(1,1:9,9)],
             c("#FDE725","#AADC32","#5DC863","#27AD81","#21908C",
               "#2C728E","#3B528B","#472D7B","#440154"))
names(cList) = c("White-Red", "Blue-Yellow-Red", "Yellow-Green-Purple")

# Panel sizes
pList = c("400px", "600px", "800px")
names(pList) = c("Small", "Medium", "Large")
pList2 = c("500px", "700px", "900px")
names(pList2) = c("Small", "Medium", "Large")
pList3 = c("600px", "800px", "1000px")
names(pList3) = c("Small", "Medium", "Large")
sList = c(18,24,30)
names(sList) = c("Small", "Medium", "Large")
lList = c(5,6,7)
names(lList) = c("Small", "Medium", "Large")

# Function to extract legend
g_legend <- function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  legend
}

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

### Common plotting functions
# Plot cell information on dimred
# scDRcell <- function(inpConf, inpMeta, inpdrX, inpdrY, inp1, inpsub1, inpsub2,
#                      inpsiz, inpcol, inpord, inpfsz, inpasp, inptxt, inplab){
#   if(is.null(inpsub1)){inpsub1 = inpConf$UI[1]}
#   # Prepare ggData
#   ggData = inpMeta[, c(inpConf[UI == inpdrX]$ID, inpConf[UI == inpdrY]$ID,
#                        inpConf[UI == inp1]$ID, inpConf[UI == inpsub1]$ID),
#                    with = FALSE]
#   colnames(ggData) = c("X", "Y", "val", "sub")
#   rat = (max(ggData$X) - min(ggData$X)) / (max(ggData$Y) - min(ggData$Y))
#   bgCells = FALSE
#   if(length(inpsub2) != 0 & length(inpsub2) != nlevels(ggData$sub)){
#     bgCells = TRUE
#     ggData2 = ggData[!sub %in% inpsub2]
#     ggData = ggData[sub %in% inpsub2]
#   }
#   if(inpord == "Max-1st"){
#     ggData = ggData[order(val)]
#   } else if(inpord == "Min-1st"){
#     ggData = ggData[order(-val)]
#   } else if(inpord == "Random"){
#     ggData = ggData[sample(nrow(ggData))]
#   }
# 
#   # Do factoring if required
#   if(!is.na(inpConf[UI == inp1]$fCL)){
#     ggCol = strsplit(inpConf[UI == inp1]$fCL, "\\|")[[1]]
#     names(ggCol) = levels(ggData$val)
#     ggLvl = levels(ggData$val)[levels(ggData$val) %in% unique(ggData$val)]
#     ggData$val = factor(ggData$val, levels = ggLvl)
#     ggCol = ggCol[ggLvl]
#   }
# 
#   # Actual ggplot
#   ggOut = ggplot(ggData, aes(X, Y, color = val))
#   if(bgCells){
#     ggOut = ggOut +
#       geom_point(data = ggData2, color = "snow2", size = inpsiz, shape = 16)
#   }
#   ggOut = ggOut +
#     geom_point(size = inpsiz, shape = 16) + xlab(inpdrX) + ylab(inpdrY) +
#     sctheme(base_size = sList[inpfsz], XYval = inptxt)
#   if(is.na(inpConf[UI == inp1]$fCL)){
#     ggOut = ggOut + scale_color_gradientn("", colours = cList[[inpcol]]) +
#       guides(color = guide_colorbar(barwidth = 15))
#   } else {
#     sListX = min(nchar(paste0(levels(ggData$val), collapse = "")), 200)
#     sListX = 0.75 * (sList - (1.5 * floor(sListX/50)))
#     ggOut = ggOut + scale_color_manual("", values = ggCol) +
#       guides(color = guide_legend(override.aes = list(size = 5),
#                                   nrow = inpConf[UI == inp1]$fRow)) +
#       theme(legend.text = element_text(size = sListX[inpfsz]))
#     if(inplab){
#       ggData3 = ggData[, .(X = mean(X), Y = mean(Y)), by = "val"]
#       lListX = min(nchar(paste0(ggData3$val, collapse = "")), 200)
#       lListX = lList - (0.25 * floor(lListX/50))
#       ggOut = ggOut +
#         geom_text_repel(data = ggData3, aes(X, Y, label = val),
#                         color = "grey10", bg.color = "grey95", bg.r = 0.15,
#                         size = lListX[inpfsz], seed = 42)
#     }
#   }
#   if(inpasp == "Square") {
#     ggOut = ggOut + coord_fixed(ratio = rat)
#   } else if(inpasp == "Fixed") {
#     ggOut = ggOut + coord_fixed()
#   }
#   return(ggOut)
# }
# 




scDRcell_plotly <- function(inpConf, inpMeta, inpdrX, inpdrY, inp1, inpsub1, inpsub2,
                            inpsiz, inpcol, inpord, inpfsz, inpasp, inptxt, inplab) {
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
               type = 'scatter', mode = 'markers', marker = list(size = inpsiz*3, opacity = 0.8),
               text = ~paste('Value:', val, '<br>X:', X, '<br>Y:', Y, '<br>Sub:', sub),
               hoverinfo = 'text')
  
  if (bgCells) {
    p <- p %>% add_trace(data = ggData2, x = ~X, y = ~Y, mode = 'markers',
                         marker = list(color = 'snow2', size = inpsiz * 12, opacity = 0.5))
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
                               font = list(color = "grey10", size = inpfsz))
  }
  
  p <- p %>% layout(
    xaxis = list(title = inpdrX, zeroline = FALSE, showline = FALSE, showgrid = TRUE),
    yaxis = list(title = inpdrY, zeroline = FALSE, showline = FALSE, showgrid = TRUE),
    showlegend = FALSE
  )
  
  if (inpasp == "Square") {
    p <- p %>% layout(yaxis = list(scaleanchor = "x", scaleratio = rat))
  } else if (inpasp == "Fixed") {
    p <- p %>% layout(yaxis = list(scaleanchor = "x"))
  }
  
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
# Plot gene expression on dimred
# scDRgene <- function(inpConf, inpMeta, inpdrX, inpdrY, inp1, inpsub1, inpsub2,
#                      inpH5 = h5_file_path, inpGene,
#                      inpsiz, inpcol, inpord, inpfsz, inpasp, inptxt){
#   if(is.null(inpsub1)){inpsub1 = inpConf$UI[1]}
#   # Prepare ggData
#   ggData = inpMeta[, c(inpConf[UI == inpdrX]$ID, inpConf[UI == inpdrY]$ID,
#                        inpConf[UI == inpsub1]$ID),
#                    with = FALSE]
#   colnames(ggData) = c("X", "Y", "sub")
#   rat = (max(ggData$X) - min(ggData$X)) / (max(ggData$Y) - min(ggData$Y))
# 
#   h5file <- H5File$new(inpH5, mode = "r")
#   h5data <- h5file[["grp"]][["data"]]
#   ggData$val = h5data$read(args = list(inpGene[inp1], quote(expr=)))
#   ggData[val < 0]$val = 0
#   h5file$close_all()
#   bgCells = FALSE
#   if(length(inpsub2) != 0 & length(inpsub2) != nlevels(ggData$sub)){
#     bgCells = TRUE
#     ggData2 = ggData[!sub %in% inpsub2]
#     ggData = ggData[sub %in% inpsub2]
#   }
#   if(inpord == "Max-1st"){
#     ggData = ggData[order(val)]
#   } else if(inpord == "Min-1st"){
#     ggData = ggData[order(-val)]
#   } else if(inpord == "Random"){
#     ggData = ggData[sample(nrow(ggData))]
#   }
# 
#   # Actual ggplot
#   ggOut = ggplot(ggData, aes(X, Y, color = val))
#   if(bgCells){
#     ggOut = ggOut +
#       geom_point(data = ggData2, color = "snow2", size = inpsiz, shape = 16)
#   }
#   ggOut = ggOut +
#     geom_point(size = inpsiz, shape = 16) + xlab(inpdrX) + ylab(inpdrY) +
#     sctheme(base_size = sList[inpfsz], XYval = inptxt) +
#     scale_color_gradientn(inp1, colours = cList[[inpcol]]) +
#     guides(color = guide_colorbar(barwidth = 15))
#   if(inpasp == "Square") {
#     ggOut = ggOut + coord_fixed(ratio = rat)
#   } else if(inpasp == "Fixed") {
#     ggOut = ggOut + coord_fixed()
#   }
#   return(ggOut)
# }

scDRgene_plotly <- function(inpConf, inpMeta, inpdrX, inpdrY, inp1, inpsub1, inpsub2,
                            inpH5 = h5_file_path, inpGene,
                            inpsiz, inpcol, inpord, inpfsz, inpasp, inptxt) {
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
               type = 'scatter', mode = 'markers', marker = list(size = inpsiz*3),
               text = ~paste('Value:', val, '<br>X:', X, '<br>Y:', Y, '<br>Sub:', sub),
               hoverinfo = 'text')
  
  if (bgCells) {
    p <- p %>% add_trace(data = ggData2, x = ~X, y = ~Y, mode = 'markers',
                         marker = list(color = 'snow2', size = inpsiz*12))
  }
  
  p <- p %>% colorbar(title = inp1, len = 0.5, thickness = 15, x = 0.7, xanchor = 'center',
                      y = -0.3, yanchor = 'bottom', orientation = 'h') %>%layout(
    xaxis = list(title = inpdrX, zeroline = FALSE, showline = FALSE, showgrid = TRUE),
    yaxis = list(title = inpdrY, zeroline = FALSE, showline = FALSE, showgrid = TRUE),
    showlegend = FALSE
  )
  
  if (inpasp == "Square") {
    p <- p %>% layout(yaxis = list(scaleanchor = "x", scaleratio = rat))
  } else if (inpasp == "Fixed") {
    p <- p %>% layout(yaxis = list(scaleanchor = "x"))
  }
  
  return(p)
}


}



