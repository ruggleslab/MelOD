seurat_test_server <- function(id) {
  moduleServer(id, function(input, output, session) {


  sc1conf = readRDS("./data/single_cell/sc1conf.rds")
  sc1def  = readRDS("./data/single_cell/sc1def.rds")
  sc1gene = readRDS("./data/single_cell/sc1gene.rds")
  sc1meta = readRDS("./data/single_cell/sc1meta.rds")
  h5_file_path <- "./data/single_cell/sc1gexpr.h5"
  

  
  ### For all tags and Server-side selectize
  observe_helpers()
  optCrt="{ option_create: function(data,escape) {return('<div class=\"create\"><strong>' + '</strong></div>');} }"
  updateSelectizeInput(session, "gene_plot_culstered_selection", choices = names(sc1gene), server = TRUE,
                       selected = sc1def$gene1, options = list(
                         maxOptions = 7, create = TRUE, persist = TRUE, render = I(optCrt)))
  # updateSelectizeInput(session, "sc1a3inp1", choices = names(sc1gene), server = TRUE,
  #                      selected = sc1def$gene1, options = list(
  #                        maxOptions = 7, create = TRUE, persist = TRUE, render = I(optCrt)))
  # updateSelectizeInput(session, "sc1a3inp2", choices = names(sc1gene), server = TRUE,
  #                      selected = sc1def$gene2, options = list(
  #                        maxOptions = 7, create = TRUE, persist = TRUE, render = I(optCrt)))
  # updateSelectizeInput(session, "sc1b2inp1", choices = names(sc1gene), server = TRUE,
  #                      selected = sc1def$gene1, options = list(
  #                        maxOptions = 7, create = TRUE, persist = TRUE, render = I(optCrt)))
  # updateSelectizeInput(session, "sc1b2inp2", choices = names(sc1gene), server = TRUE,
  #                      selected = sc1def$gene2, options = list(
  #                        maxOptions = 7, create = TRUE, persist = TRUE, render = I(optCrt)))
  # updateSelectizeInput(session, "sc1c1inp2", server = TRUE,
  #                      choices = c(sc1conf[is.na(fID)]$UI,names(sc1gene)),
  #                      selected = sc1conf[is.na(fID)]$UI[1], options = list(
  #                        maxOptions = length(sc1conf[is.na(fID)]$UI) + 3,
  #                        create = TRUE, persist = TRUE, render = I(optCrt)))
  # 
  # 
  

  
  ### Plots for tab a1
  output$cell_subset_choices <- renderUI({
    ns <- NS(id)
    sub = strsplit(sc1conf[UI == input$cell_subset]$fID, "\\|")[[1]]
    checkboxGroupInput(ns("sc1a1sub2"), "Select which cells to show", inline = TRUE,
                       choices = sub, selected = sub)
  })
  
  observeEvent(input$cell_subset_none, {
    sub = strsplit(sc1conf[UI == input$cell_subset]$fID, "\\|")[[1]]
    updateCheckboxGroupInput(session, inputId = "sc1a1sub2", label = "Select which cells to show",
                             choices = sub, selected = NULL, inline = TRUE)
  })
  
  observeEvent(input$cell_subset_all, {
    sub = strsplit(sc1conf[UI == input$cell_subset]$fID, "\\|")[[1]]
    updateCheckboxGroupInput(session, inputId = "sc1a1sub2", label = "Select which cells to show",
                             choices = sub, selected = sub, inline = TRUE)
  })
  
  
  output$cell_plot_culstered <- renderPlotly({
    scDRcell_plotly(sc1conf, sc1meta, input$cell_plot_culstered_X_axis, input$cell_plot_culstered_Y_axis, input$cell_plot_culstered_info,
                    input$cell_subset, input$sc1a1sub2,
                    input$marker_size, input$cell_plot_culstered_color, input$cell_plot_culstered_order,input$cell_plot_culstered_label)

  })

 
  
  
  output$cell_plot_culstered_pdf <- downloadHandler(
    filename = function() { paste0("sc1",input$cell_plot_culstered_X_axis,"_",input$cell_plot_culstered_Y_axis,"_",
                                   input$cell_plot_culstered_info,".pdf") },
    content = function(file) { ggsave(
      file, device = "pdf", height = input$sc1a1oup1.h, width = input$sc1a1oup1.w, useDingbats = FALSE,
      plot = scDRcell(sc1conf, sc1meta, input$cell_plot_culstered_X_axis, input$cell_plot_culstered_Y_axis, input$cell_plot_culstered_info,
                      input$cell_subset, input$sc1a1sub2,
                      input$marker_size, input$cell_plot_culstered_color, input$cell_plot_culstered_order,
                      input$sc1a1fsz, input$sc1a1asp, input$sc1a1txt, input$cell_plot_culstered_label) )
    })
  
  
  
  output$cell_plot_culstered_png <- downloadHandler(
    filename = function() { paste0("sc1",input$cell_plot_culstered_X_axis,"_",input$cell_plot_culstered_Y_axis,"_",
                                   input$cell_plot_culstered_info,".png") },
    content = function(file) { ggsave(
      file, device = "png", height = input$sc1a1oup1.h, width = input$sc1a1oup1.w,
      plot = scDRcell(sc1conf, sc1meta, input$cell_plot_culstered_X_axis, input$cell_plot_culstered_Y_axis, input$cell_plot_culstered_info,
                      input$cell_subset, input$sc1a1sub2,
                      input$marker_size, input$cell_plot_culstered_color, input$cell_plot_culstered_order,
                      input$sc1a1fsz, input$sc1a1asp, input$sc1a1txt, input$cell_plot_culstered_label) )
    })
  
  
  

  
  output$gene_plot_culstered <- renderPlotly({
    scDRgene_plotly(sc1conf, sc1meta, input$cell_plot_culstered_X_axis, input$cell_plot_culstered_Y_axis, input$gene_plot_culstered_selection,
                    input$cell_subset, input$sc1a1sub2,
                    h5_file_path, sc1gene,
                    input$marker_size, input$gene_plot_culstered_color, input$gene_plot_culstered_order)
    
  })
  
 
  
  
  output$gene_plot_culstered_pdf <- downloadHandler(
    filename = function() { paste0("sc1",input$cell_plot_culstered_X_axis,"_",input$cell_plot_culstered_Y_axis,"_",
                                   input$gene_plot_culstered_selection,".pdf") },
    content = function(file) { ggsave(
      file, device = "pdf", height = input$sc1a1oup2.h, width = input$sc1a1oup2.w, useDingbats = FALSE,
      plot = scDRgene(sc1conf, sc1meta, input$cell_plot_culstered_X_axis, input$cell_plot_culstered_Y_axis, input$gene_plot_culstered_selection,
                      input$cell_subset, input$sc1a1sub2,
                      h5_file_path, sc1gene,
                      input$marker_size, input$gene_plot_culstered_color, input$gene_plot_culstered_order,
                      input$sc1a1fsz, input$sc1a1asp, input$sc1a1txt) )
    })
  
  
  output$gene_plot_culstered_png <- downloadHandler(
    filename = function() { paste0("sc1",input$cell_plot_culstered_X_axis,"_",input$cell_plot_culstered_Y_axis,"_",
                                   input$gene_plot_culstered_selection,".png") },
    content = function(file) { ggsave(
      file, device = "png", height = input$sc1a1oup2.h, width = input$sc1a1oup2.w,
      plot = scDRgene(sc1conf, sc1meta, input$cell_plot_culstered_X_axis, input$cell_plot_culstered_Y_axis, input$gene_plot_culstered_selection,
                      input$cell_subset, input$sc1a1sub2,
                      h5_file_path, sc1gene,
                      input$marker_size, input$gene_plot_culstered_color, input$gene_plot_culstered_order,
                      input$sc1a1fsz, input$sc1a1asp, input$sc1a1txt) )
    })
  
  
  
  
})
}






