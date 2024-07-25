bubheat_server <- function(id, sc1conf, sc1meta, sc1gene, sc1def, h5_file_path) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    
    updateSelectizeInput(session, "bubheat_group_by",
                         choices = sc1conf[grp == TRUE]$UI,
                         selected = sc1def$grp1)


    lazyLoadGenes <- function() {
      future_promise({
        names(sc1gene)
      })
    }
    
    observe({
      lazyLoadGenes() %...>% (function(genes) {
        updateMultiInput(
          session = session,
          inputId = "bubheat_selected_gene",
          choices = genes,
          selected = genes[1:5]
        )
      }) %...!% (function(e) {
        # Handle error and print error message
        showNotification("Error loading genes: ", type = "error")
      })
    })
    
    
    
    output$bubheat_plot <- renderPlotly({ 
     req(input$bubheat_group_by, input$bubheat_selected_gene, input$bubheat_group_by,  input$cell_subset, input$cell_subset_choices_box)
        bubheat_plotly(sc1conf, sc1meta, input$bubheat_selected_gene, input$bubheat_group_by, input$bubheat_type, 
                       input$cell_subset, input$cell_subset_choices_box, h5_file_path, sc1gene, 
                       input$bubheat_scale, input$bubheat_cluster_rows, input$bubheat_cluster_columns, 
                       input$bubheat_color) 
     
    }) 
  })
}


    # output$sc1d1oup.pdf <- downloadHandler( 
    #   filename = function() { paste0("sc1",input$sc1d1plt,"_",input$sc1d1grp,".pdf") }, 
    #   content = function(file) { ggsave( 
    #     file, device = "pdf", height = input$sc1d1oup.h, width = input$sc1d1oup.w, 
    #     plot = scBubbHeat(sc1conf, sc1meta, input$sc1d1inp, input$sc1d1grp, input$sc1d1plt, 
    #                       input$sc1d1sub1, input$sc1d1sub2, "sc1gexpr.h5", sc1gene, 
    #                       input$sc1d1scl, input$sc1d1row, input$sc1d1col, 
    #                       input$sc1d1cols, input$sc1d1fsz, save = TRUE) ) 
    #   }) 
    # output$sc1d1oup.png <- downloadHandler( 
    #   filename = function() { paste0("sc1",input$sc1d1plt,"_",input$sc1d1grp,".png") }, 
    #   content = function(file) { ggsave( 
    #     file, device = "png", height = input$sc1d1oup.h, width = input$sc1d1oup.w, 
    #     plot = scBubbHeat(sc1conf, sc1meta, input$sc1d1inp, input$sc1d1grp, input$sc1d1plt, 
    #                       input$sc1d1sub1, input$sc1d1sub2, "sc1gexpr.h5", sc1gene, 
    #                       input$sc1d1scl, input$sc1d1row, input$sc1d1col, 
    #                       input$sc1d1cols, input$sc1d1fsz, save = TRUE) ) 
    #   }) 
    # 
    