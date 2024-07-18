proportion_server <- function(id, sc1conf, sc1meta, sc1def) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    
    
    updateSelectizeInput(session, "proportion_plot_X",
                      choices = sc1conf[grp == TRUE]$UI,
                      selected = sc1def$grp1)
    
    
    
    updateSelectizeInput(session, "proportion_group_by",
                         choices = sc1conf[grp == TRUE]$UI,
                         selected = sc1def$grp1)
    
    output$proportion_plot <- renderPlotly({ 
      proportion_plotly(sc1conf, sc1meta, input$proportion_plot_X, input$proportion_group_by,  
             input$cell_subset, input$cell_subset_choices_box, 
             input$proportion_type, input$proportion_flip_axis) 
    }) 
 
    # 
    # output$sc1c2oup.pdf <- downloadHandler( 
    #   filename = function() { paste0("sc1",input$sc1c2typ,"_",input$sc1c2inp1,"_",  
    #                                  input$sc1c2inp2,".pdf") }, 
    #   content = function(file) { ggsave( 
    #     file, device = "pdf", height = input$sc1c2oup.h, width = input$sc1c2oup.w, useDingbats = FALSE, 
    #     plot = scProp(sc1conf, sc1meta, input$sc1c2inp1, input$sc1c2inp2,  
    #                   input$sc1c2sub1, input$sc1c2sub2, 
    #                   input$sc1c2typ, input$sc1c2flp, input$sc1c2fsz) ) 
    #   }) 
    # output$sc1c2oup.png <- downloadHandler( 
    #   filename = function() { paste0("sc1",input$sc1c2typ,"_",input$sc1c2inp1,"_",  
    #                                  input$sc1c2inp2,".png") }, 
    #   content = function(file) { ggsave( 
    #     file, device = "png", height = input$sc1c2oup.h, width = input$sc1c2oup.w, 
    #     plot = scProp(sc1conf, sc1meta, input$sc1c2inp1, input$sc1c2inp2,  
    #                   input$sc1c2sub1, input$sc1c2sub2, 
    #                   input$sc1c2typ, input$sc1c2flp, input$sc1c2fsz) ) 
    #   }) 
    # 
    # 
   
  })
}
