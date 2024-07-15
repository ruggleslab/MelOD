inputs_server <- function(id, sc1conf, sc1def) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    updateSelectInput(session, "cell_subset",
                      choices = sc1conf[grp == TRUE]$UI,
                      selected = sc1def$grp1)

    output$cell_subset_choices <- renderUI({
      sub <- strsplit(sc1conf[UI == input$cell_subset]$fID, "\\|")[[1]]
      checkboxGroupInput(ns("cell_subset_choices_box"), "Select which cells to show", inline = TRUE,
                         choices = sub, selected = sub)
    })

    observeEvent(input$cell_subset_none, {
      sub <- strsplit(sc1conf[UI == input$cell_subset]$fID, "\\|")[[1]]
      updateCheckboxGroupInput(session, "cell_subset_choices_box", label = "Select which cells to show",
                               choices = sub, selected = NULL, inline = TRUE)
    })

    observeEvent(input$cell_subset_all, {
      sub <- strsplit(sc1conf[UI == input$cell_subset]$fID, "\\|")[[1]]
      updateCheckboxGroupInput(session, "cell_subset_choices_box", label = "Select which cells to show",
                               choices = sub, selected = sub, inline = TRUE)
    })
    
    
  })
}
