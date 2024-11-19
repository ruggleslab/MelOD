
gene_search_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      box(
        title = "Information", status = "info", solidHeader = TRUE, width = 12,
        tags$p("This page is designed specifically for transcriptomic studies. It allows you to search and filter genes based on specific parameters. 
       You can adjust the p-value and log2 fold change cutoffs to filter genes on the volcano plot, 
       select specific genes, and choose studies of interest."),
        tags$p("For more detailed insights on individual studies, please visit the 'Studies' page in the sidebar.")
      )
    ),
    fluidRow(
      column(4, box(
        title = "Inputs", status = "warning",
        collapsible = TRUE,
        solidHeader = TRUE, width = 12,
        tags$h3("Parameters", style = "margin-top: 0;"),
        fluidRow(
          column(4, numericInput(
            ns("slider_padj"), 
            "padj Cutoff", 
            value = 0.05, 
            min = 0, 
            max = 1, 
            step = 0.01
          ) %>%
            helper(
              colour = "#FFA812", 
              icon = "question",
              type = "inline", 
              size = "m", 
              fade = TRUE,
              title = "P-value Adjusted Cutoff",
              content = c(
                "This setting allows you to filter genes on the volcano plot."
              )
            )),
          column(1,),
          column(5, numericInput(
            ns("slider_log2"), 
            "Log2 Fold Change Cutoff", 
            value = 1, 
            min = 0,
            step = 0.1
          ) %>%
            helper(
              colour = "#FFA812", 
              type = "inline", 
              icon = "question",
              size = "m", 
              fade = TRUE,
              title = "Log2 Fold Change Cutoff",
              content = c(
                "This cutoff allows you to filter genes on the volcano plot."
              )
            ))
        ),
        shinyWidgets::multiInput(
          inputId = ns("selected_gene"),
          label = "Gene(s) selection (up to 10):",
          choices = "Loading...",
          options = list(limit = 10, enable_search = TRUE),
          width = "100%"
        ),
        actionButton(ns("reset_selection"), "Reset Selection", class = "btn-primary")
      )),
      column(6, box(
        title = "Studies", status = "info",
        collapsible = TRUE,
        solidHeader = TRUE, width = 12,
        
        checkboxGroupInput(
          ns("selected_studies"),
          "Select Studies:",
          choices = "Loading...",
          selected = NULL
        )
      ))
    ),
    box(
      title = "Search Results", status = "primary", solidHeader = TRUE, width = 12, collapsible = TRUE,
      DT::dataTableOutput(ns("results_table"))
    ),
    fluidRow(
      uiOutput(ns("volcano_plots"))
    )
  )
}


process_volcano_data_gene_search <- function(res, padj_cut, log2_cut, selected_genes) {
  res <- as.data.frame(res)
  res$neg_log10_padj <- -log10(res$padj)
  res$log2FoldChange <- round(res$log2FoldChange, 1)
  res$neg_log10_padj <- round(res$neg_log10_padj, 1)
  res$sig <- ifelse(
    res$padj < padj_cut  & abs(res$log2FoldChange) >= log2_cut,
    ifelse(res$log2FoldChange > 0, "Upregulated", "Downregulated"),
    "Not Significant"
  )
  
  return(res)
}



gene_search_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    blurbs <- fromJSON("./www/info_blurbs.json")
    
    ns <- session$ns
    useShinyjs()
    
    show_modal_progress_line(color = "#FFA812", text = "Initialization")
    Sys.sleep(0.5)
    
    tryCatch({
      update_modal_progress(0.3, text="Downloading gene data...")
      Sys.sleep(0.5)
      summary_gene_study <- drive_download("summary_gene_study.rds", overwrite = TRUE)
      summary_gene_study_rds <- readRDS(summary_gene_study$local_path)
      
      downloaded_files <- list(summary_gene_study$local_path)
      for (file_path in downloaded_files) {
        if (file.exists(file_path)) file.remove(file_path)
      }
      
      update_modal_progress(1, text="Finalizing") 
      Sys.sleep(0.5)
      
      
      gene_names <- reactive({
        summary_gene_study_rds %>%
          pull(gene_name) %>%
          unique()
      })
      
      available_studies <- reactive({
        summary_gene_study_rds %>%
          filter(gene_name %in% input$selected_gene) %>%
          pull(analysis_id) %>%
          unique()
      })
      
      
      # Update gene selection and studies based on inputs
      observe({
        updateMultiInput(
          session = session,
          inputId = "selected_gene",
          choices = gene_names(),
          selected = NULL
        )
      })
      
      observeEvent(input$reset_selection, {
        updateMultiInput(
          session = session,
          inputId = "selected_gene",
          choices = gene_names(),
          selected = NULL
        )
      })
      
      observe({
        updateCheckboxGroupInput(
          session = session,
          inputId = "selected_studies",
          choices = available_studies(),
          selected = NULL
        )
      })
      
      
      
      
      processed_data <- reactiveValues()
      
      observe({
        lapply(input$selected_studies, function(study) {
          study_data <- summary_gene_study_rds %>% filter(analysis_id == study)
          processed_data[[study]] <- process_volcano_data_gene_search(
            res = study_data,
            padj_cut = input$slider_padj,
            log2_cut = input$slider_log2,
            selected_genes = input$selected_gene
          )
        })
      })
      
      
      
      
      output$volcano_plots <- renderUI({
        studies <- input$selected_studies
        if (length(studies) == 0) {
          return(tags$p("No studies selected."))
        }
        
        plot_ui_list <- lapply(studies, function(study) {
          box(
            title = HTML(paste("Volcano Plot -", study, 
                               actionLink(ns("info_volcano_plot"), label = "", icon = icon("info-circle")),
                               downloadButton(ns(paste0('download_volcano_data_', study)), label = "", icon = icon("save-file", lib = "glyphicon")))),
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput(ns(paste0("volcano_plot_", study)))
          )
        })
        do.call(tagList, plot_ui_list)
      })
      
      observe({
        lapply(input$selected_studies, function(study) {
          plot_output_id <- paste0("volcano_plot_", study)
          download_button_id <- paste0("download_volcano_data_", study)
          
          # Render Plotly Plot
          output[[plot_output_id]] <- renderPlotly({
            study_data <- summary_gene_study_rds %>% filter(analysis_id == study)
            processed <- processed_data[[study]]
            
            plot_volcano(result_data = processed, deseq2_data = study_data, gene = input$selected_gene)
          })
          study_data <- summary_gene_study_rds %>% filter(analysis_id == study)
          processed <- processed_data[[study]]
          
          setup_download_handler(plot_output_id, output, download_button_id, reactive({processed}), "volcano")
          
        })
      })
      search_results <- reactive({
        summary_gene_study_rds %>%
          filter(gene_name %in% input$selected_gene)
      })
      
      
      
      output$results_table <- DT::renderDataTable({
        data <- search_results()
        
        # Round and format numeric columns for better display
        data$log2FoldChange <- round(as.numeric(data$log2FoldChange), 2)
        data$padj <- formatC(as.numeric(data$padj), format = "e", digits = 2)
        data$pvalue <- formatC(as.numeric(data$pvalue), format = "e", digits = 2) 
        data$lfcSE <- round(as.numeric(data$lfcSE), 1)
        data$stat <- round(as.numeric(data$stat), 1)
        
        # Render the table
        datatable <- DT::datatable(
          data,
          options = list(pageLength = 10, autoWidth = TRUE),
          rownames = data$study
        )
        
        # Use input$slider_padj and input$slider_log2 for dynamic thresholds
        padj_cutoff <- input$slider_padj
        log2_cutoff <- input$slider_log2
        
        datatable %>%
          DT::formatStyle(
            'padj', # Column to format
            backgroundColor = DT::styleInterval(
              c(padj_cutoff), # Single cutoff for two colors
              c('lightgreen','lightpink') # Colors based on threshold
            ),
            fontWeight = 'bold' # Bold the values
          ) %>%
          DT::formatStyle(
            'log2FoldChange',
            color = DT::styleInterval(
              c(-log2_cutoff, log2_cutoff), # Two cutoffs for three colors
              c('#D81B60', 'black', '#1E88E5') # Red for low, black for middle, blue for high
            ),
            fontWeight = 'bold'
          )
      })
      
      observeEvent(input$info_volcano_plot, {
        shinyalert(
          title = blurbs$info$volcano$title, 
          html = TRUE,
          text = blurbs$info$volcano$text
        )
      })
      
      
      
      
    }, error = function(e) {
      showNotification("An error occurred during the process.", type = "error")
      print(e)    
    })
    remove_modal_progress() 
  })
}