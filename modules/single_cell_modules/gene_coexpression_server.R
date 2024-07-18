gene_coexpression_server <- function(id, sc1conf, sc1meta, sc1gene, sc1def, h5_file_path) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
 
    
    debounced_marker_size <- debounce(reactive({ input$marker_size }), millis = 500)
    
    
    
    
    updateSelectizeInput(session, "gene_plot_coexpression_selection", choices = names(sc1gene), server = TRUE)
    updateSelectizeInput(session, "gene_plot_coexpression_selection_2", choices = names(sc1gene), server = TRUE)
    
    
    
    output$gene_plot_coexpression <- renderPlotly({ 
      coexpression_plotly(sc1conf, sc1meta, input$cell_plot_culstered_X_axis, input$cell_plot_culstered_Y_axis,   
               input$gene_plot_coexpression_selection, input$gene_plot_coexpression_selection_2,input$cell_subset, input$cell_subset_choices_box, 
               h5_file_path, sc1gene, 
               debounced_marker_size(), input$gene_plot_coexpression_color) 
    }) 
    
 
    
    
    # 
    # 
    # output$sc1b2oup1.pdf <- downloadHandler( 
    #   filename = function() { paste0("sc1",input$sc1b2drX,"_",input$sc1b2drY,"_",  
    #                                  input$sc1b2inp1,"_",input$sc1b2inp2,".pdf") }, 
    #   content = function(file) { ggsave( 
    #     file, device = "pdf", height = input$sc1b2oup1.h, width = input$sc1b2oup1.w, useDingbats = FALSE, 
    #     plot = scDRcoex(sc1conf, sc1meta, input$sc1b2drX, input$sc1b2drY,  
    #                     input$sc1b2inp1, input$sc1b2inp2, input$sc1b2sub1, input$sc1b2sub2, 
    #                     "sc1gexpr.h5", sc1gene, 
    #                     input$sc1b2siz, input$sc1b2col1, input$sc1b2ord1, 
    #                     input$sc1b2fsz, input$sc1b2asp, input$sc1b2txt) ) 
    #   }) 
    # output$sc1b2oup1.png <- downloadHandler( 
    #   filename = function() { paste0("sc1",input$sc1b2drX,"_",input$sc1b2drY,"_",  
    #                                  input$sc1b2inp1,"_",input$sc1b2inp2,".png") }, 
    #   content = function(file) { ggsave( 
    #     file, device = "png", height = input$sc1b2oup1.h, width = input$sc1b2oup1.w, 
    #     plot = scDRcoex(sc1conf, sc1meta, input$sc1b2drX, input$sc1b2drY,  
    #                     input$sc1b2inp1, input$sc1b2inp2, input$sc1b2sub1, input$sc1b2sub2, 
    #                     "sc1gexpr.h5", sc1gene, 
    #                     input$sc1b2siz, input$sc1b2col1, input$sc1b2ord1, 
    #                     input$sc1b2fsz, input$sc1b2asp, input$sc1b2txt) ) 
    #   }) 
    
    output$gene_datatable_coexpression <- renderDataTable({ 
      ggData = scDRcoexNum(sc1conf, sc1meta, input$gene_plot_coexpression_selection, input$gene_plot_coexpression_selection_2,
                           input$cell_subset, input$cell_subset_choices_box, h5_file_path, sc1gene) 
      datatable(ggData, rownames = FALSE, extensions = "Buttons", 
                options = list(pageLength = -1, dom = "tB", buttons = c("copy", "csv", "excel"))) %>% 
        formatRound(columns = c("percent"), digits = 2) 
    }) 
    
  })
}
