inputs_server <- function(id, shared_reactives) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    

    
    print(shared_reactives$sc1conf_data)
    
    observe({updateSelectInput(session, "cell_subset",
                               choices = shared_reactives$sc1conf_data()[grp == TRUE]$UI,
                               selected = shared_reactives$sc1def_data()$grp1)
      
      
      updateSelectInput(session, "cell_plot_clustered_X_axis",
                        choices = shared_reactives$sc1conf_data()[dimred == TRUE]$UI,
                        selected =  shared_reactives$sc1def_data()$dimred[1])
      
      updateSelectInput(session, "cell_plot_clustered_Y_axis",
                        choices = shared_reactives$sc1conf_data()[dimred == TRUE]$UI,
                        selected =  shared_reactives$sc1def_data()$dimred[2])
      })
    
    output$cell_subset_choices <- renderUI({
      req(input$cell_subset)
      
      sub <- strsplit(shared_reactives$sc1conf_data()[UI == input$cell_subset]$fID, "\\|")[[1]]
      checkboxGroupInput(ns("cell_subset_choices_box"), "Select which cells to show", inline = TRUE,
                         choices = sub, selected = sub)
    })
    
    observeEvent(input$cell_subset_none, {
      req(input$cell_subset)
      
      sub <- strsplit(shared_reactives$sc1conf_data()[UI == input$cell_subset]$fID, "\\|")[[1]]
      updateCheckboxGroupInput(session, "cell_subset_choices_box", label = "Select which cells to show",
                               choices = sub, selected = NULL, inline = TRUE)
    })
    
    observeEvent(input$cell_subset_all, {
      req(input$cell_subset) 
      
      sub <- strsplit(shared_reactives$sc1conf_data()[UI == input$cell_subset]$fID, "\\|")[[1]]
      updateCheckboxGroupInput(session, "cell_subset_choices_box", label = "Select which cells to show",
                               choices = sub, selected = sub, inline = TRUE)
    })
  })
}
