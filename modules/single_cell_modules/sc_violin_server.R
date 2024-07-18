sc_violin_server <- function(id, sc1conf, sc1meta, sc1gene, sc1def, h5_file_path) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    

    updateSelectInput(session, "violin_plot_X",
                      choices = sc1conf[grp == TRUE]$UI,
                      selected = sc1def$grp1)
    
   
    
    updateSelectizeInput(session, "violin_plot_Y", server = TRUE, 
                         choices = c(sc1conf[is.na(fID)]$UI,names(sc1gene)), 
                         selected = sc1conf[is.na(fID)]$UI[1], options = list( 
                           maxOptions = length(sc1conf[is.na(fID)]$UI) + 3)) 
    
    
    
    output$sc_violin_plot <- renderPlotly({ 
      sc_violin_plotly(sc1conf, sc1meta, input$violin_plot_X, input$violin_plot_Y, 
              input$cell_subset, input$cell_subset_choices_box, 
               h5_file_path, sc1gene, input$sc_box_or_violin, input$sc_violon_dot) 
    }) 
    
  
    # output$sc1c1oup.pdf <- downloadHandler( 
    #   filename = function() { paste0("sc1",input$sc1c1typ,"_",input$sc1c1inp1,"_",  
    #                                  input$sc1c1inp2,".pdf") }, 
    #   content = function(file) { ggsave( 
    #     file, device = "pdf", height = input$sc1c1oup.h, width = input$sc1c1oup.w, useDingbats = FALSE, 
    #     plot = scVioBox(sc1conf, sc1meta, input$sc1c1inp1, input$sc1c1inp2, 
    #                     input$sc1c1sub1, input$sc1c1sub2, 
    #                     "sc1gexpr.h5", sc1gene, input$sc1c1typ, input$sc1c1pts, 
    #                     input$sc1c1siz, input$sc1c1fsz) ) 
    #   }) 
    # output$sc1c1oup.png <- downloadHandler( 
    #   filename = function() { paste0("sc1",input$sc1c1typ,"_",input$sc1c1inp1,"_",  
    #                                  input$sc1c1inp2,".png") }, 
    #   content = function(file) { ggsave( 
    #     file, device = "png", height = input$sc1c1oup.h, width = input$sc1c1oup.w, 
    #     plot = scVioBox(sc1conf, sc1meta, input$sc1c1inp1, input$sc1c1inp2, 
    #                     input$sc1c1sub1, input$sc1c1sub2, 
    #                     "sc1gexpr.h5", sc1gene, input$sc1c1typ, input$sc1c1pts, 
    #                     input$sc1c1siz, input$sc1c1fsz) ) 
    #   }) 
    
  })
}