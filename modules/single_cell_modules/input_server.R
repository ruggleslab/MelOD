inputs_server <- function(id, sc1conf, sc1def) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    print("input")
    updateSelectInput(session, "cell_subset",
                      choices = sc1conf[grp == TRUE]$UI,
                      selected = sc1def$grp1)
    
    output$cell_subset_choices <- renderUI({
      req(input$cell_subset) # Ensure input$cell_subset is not NULL
      
      sub <- strsplit(sc1conf[UI == input$cell_subset]$fID, "\\|")[[1]]
      checkboxGroupInput(ns("cell_subset_choices_box"), "Select which cells to show", inline = TRUE,
                         choices = sub, selected = sub)
    })
    
    observeEvent(input$cell_subset_none, {
      req(input$cell_subset) # Ensure input$cell_subset is not NULL
      
      sub <- strsplit(sc1conf[UI == input$cell_subset]$fID, "\\|")[[1]]
      updateCheckboxGroupInput(session, "cell_subset_choices_box", label = "Select which cells to show",
                               choices = sub, selected = NULL, inline = TRUE)
    })
    
    observeEvent(input$cell_subset_all, {
      req(input$cell_subset) # Ensure input$cell_subset is not NULL
      
      sub <- strsplit(sc1conf[UI == input$cell_subset]$fID, "\\|")[[1]]
      updateCheckboxGroupInput(session, "cell_subset_choices_box", label = "Select which cells to show",
                               choices = sub, selected = sub, inline = TRUE)
    })
  })
}
