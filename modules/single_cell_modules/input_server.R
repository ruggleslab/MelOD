inputs_server <- function(id, shared_reactives, DEG = NA) {
  #' Inputs Server Module
  #'
  #' This function defines a Shiny module server for managing input selections related to cell subsets and dimensions in a single-cell dataset.
  #' It dynamically updates input controls for selecting cell subsets, X and Y axes for clustering plots, and handles user interactions
  #' such as selecting or deselecting all options within a subset.
  #'
  #' @param id The module ID.
  #' @param shared_reactives A list of reactive objects shared across the application, including single-cell
  #' data (`sc1conf_data`, `sc1def_data`).
  #' @param DEG Optional parameter (default is `NA`) for specifying differentially expressed genes.
  #'
  #' @return This module does not return a value; it is used for its side effects within a Shiny application.
  #'
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    observe({
      updateSelectInput(session, "cell_subset",
                        choices = shared_reactives$sc1conf_data()[grp == TRUE]$UI,
                        selected = shared_reactives$sc1def_data()$grp1)
      
      updateSelectInput(session, "cell_plot_clustered_X_axis",
                        choices = shared_reactives$sc1conf_data()[dimred == TRUE]$UI,
                        selected = shared_reactives$sc1def_data()$dimred[1])
      
      updateSelectInput(session, "cell_plot_clustered_Y_axis",
                        choices = shared_reactives$sc1conf_data()[dimred == TRUE]$UI,
                        selected = shared_reactives$sc1def_data()$dimred[2])
    })
    
    output$cell_subset_choices <- renderUI({
      req(input$cell_subset)
      
      sub <- strsplit(shared_reactives$sc1conf_data()[UI == input$cell_subset]$fID, "\\|")[[1]]
      checkboxGroupInput(ns("cell_subset_choices_box"), "Select which cells to show (orange = showing, grey = hidden)", inline = TRUE,
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
