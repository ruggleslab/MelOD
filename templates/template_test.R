source("global.R", local = TRUE)


blurbs <- "./www/data_blurbs.json" 
blurbs_info <- fromJSON(blurbs)


blurb_explanation_ui <- function(id) { 
  
  Id_info <- blurbs_info[[paste(id, "info", sep = "_")]]  


      box(title = "Explanation of data used in analysis:", status = "info",
          collapsible = TRUE,
          solidHeader = TRUE, width = 12,
          tags$p(Id_info$data_explanation))

}
blurb_study_ui <- function(id) { 
  Id_info <- blurbs_info[[paste(id, "info", sep = "_")]]  
  
    box(
      title = "Study Overview", status = "info", solidHeader = TRUE, width = 12, collapsible = TRUE,
      tags$h2(Id_info$title),
      tags$h3("Lead Author: ", Id_info$lead_author),
      tags$p(Id_info$abstract),
      tags$p("Read the full paper: ", tags$a(href = Id_info$paper_link, "PubMed", target = "_blank")),
      tags$p("DOI: ", tags$a(href = paste("https://doi.org/", Id_info$doi, sep = ""), Id_info$doi, target = "_blank")),
      tags$p("Data Access: ", tags$a(href = Id_info$data_link, "ENA Dataset", target = "_blank"))
    )
    
  
}


gide_selector_ui <- function(id){
  ns <- NS(id)

  box(background='orange',width = 12,
  radioButtons(
    label = "Select comparison",
    inputId = ns("selection"),
    choices = c("Combotherapy", "Monotherapy"),
    selected = "Combotherapy",
    inline = TRUE),
  uiOutput(ns("debug_selection"))
  
  )
  
}



badal_selector_ui <- function(id){
  ns <- NS(id)
  
  box(background='orange',width = 12,
      radioButtons(
        label = "Select comparison",
        inputId = ns("selection_badal"),
        choices = c("Gene", "Tumor Stage"),
        selected = "Gene",
        inline = TRUE),
      uiOutput(ns("debug_selection"))
      
  )
  
}






input_ui <- function(id) { 
  
  ns <- NS(id)
  
      box(title = "Inputs", status = "warning",
          collapsible = TRUE,
          solidHeader = TRUE, width = 12,
          
          tags$h3("Parameters", style = "margin-top: 0;"),  # Title for the parameters section
          numericInput(ns("slider_padj"), "padj Cutoff", 0.05, min = 0, max = 1, step = 0.01),
          numericInput(ns("slider_log2"), "log2foldchange Cutoff", 2, step = 0.1),
          numericInput(ns("number"), "Number of genes for the heatmap (min. 2 if no genes selected)", 10, min = 0, step = 1),
          selectizeInput(ns("selected_gene"), "Gene(s) selection (up ot 10)",
                         choices = NULL,  # Ensure this is accessible here or move to server
                         selected = NULL,  # Default selection
                         multiple = TRUE,
                         options = list(maxItems = 10)),
          actionButton(ns("update_plot"), "Generate plots", class = "btn-primary"),


      )
}  

    

pca_metadata_ui <- function(id){
  
  ns <- NS(id)
  fluidRow(
    
    box(
      title = HTML(paste("PCA", 
                         actionLink(ns("info_pca_plot"), label = "", icon = icon("info-circle")),downloadButton(ns('pca_data'),  label = "", icon = icon("save-file", lib = "glyphicon")))),
      status = "primary", 
      width = 8,# Color theme
      solidHeader = TRUE,            # Gives the box a solid header
      collapsible = TRUE,    
      plotlyOutput(ns(id = 'pca_plot'))
      
    ),
    tabBox(
      title = "Metadata",
      # The id lets us use input$tabset1 on the server to find the current tab
      id = "tabset1", 
      width = 4,
      tabPanel("Mortality", plotlyOutput(ns("mortality"))),
      tabPanel("Gender", plotlyOutput(ns("gender_data")))
    ))
}

   
    
differential_gene_ui <- function(id){
  
  ns <- NS(id)
  fluidRow(
    
    box(
      title = HTML(paste("Differential gene expression", 
                         actionLink(ns("info_violin_plot"), label = "", icon = icon("info-circle")),downloadButton(ns('violin_data'),  label = "", icon = icon("save-file", lib = "glyphicon")))),
      status = "primary", 
      width = 12,
      solidHeader = TRUE,
      collapsible = TRUE, 
      column(6,
             plotlyOutput(ns("volcano_plot"))
      ),
      column(6,
             plotlyOutput(ns(id = 'violin_plot')))        
      # Placeholder for the plot
    ) )
}
    
heatmap_ui <- function(id){
  
  ns <- NS(id)
  fluidRow(
    box(
      title = HTML(paste("Heatmap", 
                         actionLink(ns("info_heatmap_plot"), label = "", icon = icon("info-circle")),downloadButton(ns('heatmap_data'),  label = "", icon = icon("save-file", lib = "glyphicon")))),
      status = "primary",
      width = 12,# Color theme
      solidHeader = TRUE,      # Gives the box a solid header
      collapsible = TRUE,      # Allows the box to be collapsed
      plotlyOutput(ns("heatmap_plot"))
    ))
  
}
    
    
    
correlation_ui <- function(id){
  
  ns <- NS(id)
  fluidRow(
    box(
      title = "Correlation",  # Title of the box
      status = "primary",
      width = 12,# Color theme
      solidHeader = TRUE,      # Gives the box a solid header
      collapsible = TRUE,      # Allows the box to be collapsed
      plotlyOutput(ns("correlation"))  # Placeholder for the volcano plot
    )
    
  )
}
    
    
deseq2_table_ui <- function(id){
  
  ns <- NS(id)
  fluidRow(
    box(width = 12,title = "DESeq2 Results", status = "info", collapsible = TRUE, DT::dataTableOutput(ns("filtered_results"))),
    
  )
  
}
  
    

##############################################################################################

selection_badal_server <- function(dds, clinical_data, id) {
  moduleServer(id, function(input, output, session) {
    observe({
      req(input$selection_badal) # Ensure that input selection is available before proceeding
    })
  })
}



selection_list_server <- function(dds, clinical_data, id) {
  moduleServer(id, function(input, output, session) {
    observe({
      req(input$selection) # Ensure that input selection is available before proceeding
      selected_dds <- NULL
      selected_clinical_data <- NULL
      
      if ("Combotherapy" %in% input$selection) {
        selected_dds <- dds[[1]]
        selected_clinical_data <- clinical_data[[1]]
      } else if ("Monotherapy" %in% input$selection) {
        selected_dds <- dds[[2]]
        selected_clinical_data <- clinical_data[[2]]
      }
      
      if (!is.null(selected_dds)) {
        global_selected_dds(selected_dds)  # Update the global reactive value for dds
        global_selected_clinical_data(selected_clinical_data)  # Update the global reactive value for clinical data
      }
    })
  })
}



global_selected_dds <- reactiveVal()
global_selected_clinical_data <- reactiveVal()





selection_server <- function(dds, clinical_data, id) {
  moduleServer(id, function(input, output, session) {
    observe({
      if (length(dds) > 1) {
        selection_list_server(dds, clinical_data, id)  # Now also passing clinical_data
      } else {
        global_selected_dds(dds[[1]])  # Default to the first dataset if only one is available
        global_selected_clinical_data(clinical_data[[1]]) 
      }
    })
  })
}








shared_server_utilities <- function(dds) {

  # Apply gene names preprocessing function to dds
  
  dds_processed <- gene_names_dds(dds)
  res <- results(dds_processed)
  filtered_genes <- filter_and_order_by_padj(res)
  
  list(
    dds = dds_processed,  # Now you can access dds outside the function
    filtered_genes = filtered_genes,
    display_genes = function(selected_genes) get_display_genes(filtered_genes, selected_genes)
  )
}





input_server <- function(dds, clinical_data, id) {
  moduleServer(id, function(input, output, session) {
    # Check the number of items in `dds` and adjust accordingly
 
   
    # Use the selected `dds` within a reactive context
    utilities <- reactive({
      dds <- global_selected_dds()
 
      shared_server_utilities(dds)
    })
    
    # Observe for changes in the filtered genes and update the select input
    observe({
      filtered_res <- utilities()$filtered_genes
      updateSelectizeInput(session, "selected_gene", choices = rownames(filtered_res), server = TRUE)
    })
    
    # Create a reactive expression for displaying genes
    display_genes <- reactive({
      utilities()$display_genes(input$selected_gene)
    })
    
    # Example output to show selected genes (if needed)
    output$gene_display <- renderTable({
      display_genes()
    })
  })
}





pca_metadata_server <- function(dds ,clinical_data, id) {
  moduleServer(id, function(input, output, session) {
    

    output$pca_plot <- renderPlotly({
      req(pca_data_reactive())  
      creation_pca(pca_data_reactive())  
    })
    
    # Download handler for PCA data
    output$pca_data <- downloadHandler(
      filename = function() {
        paste("pca", "_", Sys.Date(), '.csv', sep = '')
      },
      content = function(file) {
        req(pca_data_reactive())  # Ensure data is available before attempting to write
        write.csv(pca_data_reactive(), file)
      }
    )
    
    # Observer for displaying PCA plot information using shinyalert
    observeEvent(input$info_pca_plot, {
      shinyalert(title = "PCA Plot Information", html = TRUE,
                 text = 'This is a test<br><img src="./images/violin_example.png" alt="ViolinPlot" style="width:80%;">')
    })
    
    
    pca_data_reactive <- reactive({
      
      dds <- global_selected_dds()
   
      pca_data <- pca_data(dds) 
      pca_data
    })
      
    
   
    if (id == 'kunz'){
      
      
      
      output$mortality <- renderPlotly({
        clinical_data <- as.data.frame(global_selected_clinical_data())
        colnames(clinical_data) <- as.character(unlist(clinical_data[2,]))
        clinical_data <- clinical_data[-c(1, 2), ]
        plot_mortality(clinical_data)
        
      })
      output$gender <- renderPlotly({
        colnames(global_selected_clinical_data()) <- as.character(unlist(global_selected_clinical_data()[2,]))
        clinical_data <- global_selected_clinical_data()[-c(1, 2), ]
        plot_gender(clinical_data)
        
      })
      
      
     
    } 
    else {
      output$mortality <- renderPlotly({
        plot_mortality_curve(global_selected_clinical_data())
      })
      
      }
    
  })}
    



differential_gene_server <- function(dds, clinical_data, id) {
  moduleServer(id, function(input, output, session) {
    #' Initialize Reactives
    #' 
    #' @description Initializes the reactive expressions for the module
    selected_dds <- reactive({ global_selected_dds() })
    utilities <- reactive({ shared_server_utilities(selected_dds()) })
    filtered_res <- reactive({ utilities()$filtered_genes })
    dds_processed <- reactive({ utilities()$dds })
    display_genes <- reactive({ get_display_genes(filtered_res(), input$selected_gene) })
    selected_genes_plotly <- reactiveVal(character(0))
    
    #' Plot Data with Reset Functionality
    #' 
    #' @description Generates the plot data and resets selected genes
    #' @return A list containing the violin and volcano plots
    plot_data <- eventReactive(c(input$update_plot,input$reset_selection ),{
      selected_genes_plotly(character(0))  # Reset selected genes
      generate_plot_data(dds_processed(), display_genes(), input$slider_padj, input$slider_log2)
    })
    
    #' Render Plots
    #' 
    #' @description Renders the violin and volcano plots
    render_plots(output, plot_data)
    
    #' Download Handlers
    #' 
    #' @description Sets up the download handlers for exporting data
    download_handlers(output, display_genes)
    
    #' Event Observers
    #' 
    #' @description Sets up observers for plot interactions and gene selection
    event_observers(input, session, display_genes, filtered_res, selected_genes_plotly)
    
    #' Filtered Results Table
    #' 
    #' @description Renders the filtered results table
    output$filtered_results <- render_filtered_results_table(dds_processed, input, selected_genes_plotly)
  })
}



heatmap_server <- function(dds, clinical_data, id) {
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
      generate_heatmap_plot_data(dds_processed(), isolate(input$slider_padj), isolate(input$slider_log2), isolate(input$number), isolate(input$selected_gene))
    })
    
    #' Render Plots
    #' 
    #' @description Renders the heatmap plot
    render_heatmap_plots(output, plot_data)
    
    #' Event Observers
    #' 
    #' @description Sets up observers for plot interactions
    event_observers_heatmap(input)
  })
}




