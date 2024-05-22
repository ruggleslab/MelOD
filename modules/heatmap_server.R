heatmap_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    #' Initialize Reactives
    #' 
    #' @description Initializes the reactive expressions for the module
    selected_dds <- reactive({ global_selected_dds() })
    utilities <- reactive({ shared_server_utilities(selected_dds()) })
    filtered_res <- reactive({ utilities()$filtered_genes })
    dds_processed <- reactive({ utilities()$dds })
    
    #' Plot Data with Reset Functionality
    #' 
    #' @description Generates the heatmap plot data and resets selected genes
    #' @return A list containing the heatmap plot
    plot_data <- eventReactive(c(input$update_plot, input$reset_selection), {
      list(heatmap = create_heatmap(dds_processed(), isolate(input$slider_padj), isolate(input$slider_log2), isolate(input$number), isolate(input$selected_gene)))
    })
  
  
    #' Render Plots
    #' 
    #' @description Renders the correlation plot
    #' @param output Shiny output object
    #' @param plot_data Reactive expression containing the plot data
    render_plots <- function(output, plot_data) {
      output$heatmap_plot <- renderPlotly({ plot_data()$heatmap })
    }

    render_plots(output, plot_data)
    
    #' Event Observers for Heatmap
    #' 
    #' @description Sets up observers for plot interactions
    #' @param input Shiny input object
    event_observers_heatmap <- function(input) {
      observeEvent(input$info_heatmap_plot, {
        shinyalert(title = "Heatmap Plot Information", html = TRUE,
                   text = 'This is a test<br><img src="./images/violin_example.png" alt="ViolinPlot" style="width:80%;">')
      })
    }
  })
}


