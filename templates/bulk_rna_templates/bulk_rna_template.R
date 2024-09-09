source("global.R", local = TRUE)

blurbs <- "./www/data_blurbs.json" 
blurbs_info <- fromJSON(blurbs)



blurb_study_ui <- function(id) {
#' Blurb Study UI
#' 
#' @description Creates the UI component for displaying the study overview
#' @param id Module ID
#' 
#' @return A Shiny UI element
  
  Id_info <- blurbs_info[[paste(id, "info", sep = "_")]]
  box(
    title = "Study Overview", status = "info", solidHeader = TRUE, width = 12, collapsible = TRUE,
    tags$h2(Id_info$title),
    tags$h3("Lead Author: ", Id_info$lead_author),
    tags$p(Id_info$abstract),
    tags$p("Read the full paper: ", tags$a(href = Id_info$paper_link, "PubMed")),
    tags$p("DOI: ", tags$a(href = paste("https://doi.org/", Id_info$doi, sep = ""), Id_info$doi)),
    tags$p("Data Access: ", tags$a(href = Id_info$data_link, "Dataset"))
  )
}


blurb_data_ui <- function(id) {
  #' Blurb Data UI
  #' 
  #' @description Creates the UI component for explaining the data used in the analysis
  #' @param id Module ID
  #' 
  #' @return A Shiny UI element
  
  Id_info <- blurbs_info[[paste(id, "info", sep = "_")]]
  
  box(
    title = "Data used in analysis", status = "info",
    collapsible = TRUE,
    solidHeader = TRUE, width = 12,
    tags$p(HTML(gsub("\n", "<br>", Id_info$data_explanation)))
  )
}



blurb_comparison_ui <- function(id, file = NA) {
  #' Blurb Comparison UI
  #' 
  #' @description Creates the UI component for explaining the comparison done in the analysis
  #' @param id Module ID
  #' 
  #' @return A Shiny UI element
  
  Id_info <- blurbs_info[[paste(id, "info", sep = "_")]]
  ns <- NS(id)

  box(
    status = "warning",
    background = "orange", width = 12,
    tags$p(Id_info$data_comparison)
  )
}


pca_ui <- function(id) {
#' PCA UI
#' 
#' @description Creates the UI component for displaying PCA plot
#' @param id Module ID
#' 
#' @return A Shiny UI element (box)
  
  ns <- NS(id)
  tabBox(
    title = HTML(paste("PCA", actionLink(ns("info_pca_plot"), label = "", icon = icon("info-circle")), downloadButton(ns('pca_data'), label = "", icon = icon("save-file", lib = "glyphicon")))),
    width = 12,
    tabPanel("PCA",
             withSpinner(plotlyOutput(ns('pca_plot')),type = 6, color = "#FFA812", size = 0.5),
             fluidRow(
               column(6,
                      selectInput(
                        inputId = ns("size_by"),
                        label = "Size by:",
                        choices = c("Constant" = "constant", "Size Factor" = "size_factor"),
                        selected = "constant"
                      )
               ),
               column(6,
                      selectInput(
                        inputId = ns("color_by"),
                        label = "Color by:",
                        choices = c("Condition" = "condition", "Sample" = "sample"),
                        selected = "condition"
                      )
               )
             )
    ),
    tabPanel("Variance",
             withSpinner(plotlyOutput(ns('variance_plot')),type = 6, color = "#FFA812", size = 0.5),
    )
  )
}


metadata_ui <- function(id) {
  #' Metadata UI
  #' 
  #' @description Creates the UI component for displaying metadata plots
  #' @param id Module ID
  #' 
  #' @return A Shiny UI element (tab box)
  
  ns <- NS(id)
  tabBox(
    title = HTML(paste("Metadata", actionLink(ns("info_metadata_plot"), label = "", icon = icon("info-circle")), downloadButton(ns('metadata_data'), label = "", icon = icon("save-file", lib = "glyphicon")))),    
    id = "tabset1",
    width = 12,
    tabPanel("Mortality by condition", withSpinner(uiOutput(ns("mortality_by_condition")),type = 6, color = "#FFA812", size = 0.5)),
    tabPanel("Mortality by gene", withSpinner(uiOutput(ns("mortality_by_gene") ,type = 6, color = "#FFA812", size = 0.5)),
                      selectizeInput(ns("gene_mortality"), "Gene", choices = NULL, selected = NULL, multiple = FALSE, options = list(maxItems = 1))),
  )
}


input_ui <- function(id) {
#' Input UI
#' 
#' @description Creates the UI component for input parameters
#' @param id Module ID
#' 
#' @return A Shiny UI element
  
  ns <- NS(id)
  box(
    title = "Inputs", status = "warning",
    collapsible = TRUE,
    solidHeader = TRUE, width = 12,
    tags$h3("Parameters", style = "margin-top: 0;"),
    fluidRow(column(6, numericInput(
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
          "This setting allows you to filter genes on the volcano plot, heatmap, and the significance annotation on the violin/box plot."
        )
      ))),
    fluidRow(column(6,
                    
                    numericInput(
                      ns("slider_log2"), 
                      "Log2 Fold Change Cutoff", 
                      value = 2, 
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
                          "This cutoff allows you to filter genes on the volcano plot and heatmap."
                        )
                      ))),
   
    multiInput(
                inputId = ns("selected_gene"),
                label = "Gene(s) selection (up to 10):",
                autocomplete = TRUE,
                option= list(limit=10),
                choices = "Loading..."),
             actionButton(ns("reset_selection"), "Reset Selection", class = "btn-primary")
  )
}


volcano_ui <- function(id) {
#' Volcano UI
#' 
#' @description Creates the UI component for displaying volcano plot
#' @param id Module ID
#' 
#' @return A Shiny UI element
  
  ns <- NS(id)
  fluidRow(
    box(
      title = HTML(paste("Volcano plot", actionLink(ns("info_volcano_plot"), label = "", icon = icon("info-circle")), downloadButton(ns('volcano_data'), label = "", icon = icon("save-file", lib = "glyphicon")))),
      status = "primary",
      width = 12,
      solidHeader = TRUE,
      collapsible = TRUE,
      withSpinner(plotlyOutput(ns("volcano_plot")),type = 6, color = "#FFA812", size = 0.5),
      br(),br()
    )
  )
}


violin_ui <- function(id) {
#' Violin UI
#' 
#' @description Creates the UI component for displaying violin plot
#' @param id Module ID
#' 
#' @return A Shiny UI element
  
  ns <- NS(id)
  fluidRow(
    box(
      title = HTML(paste("Violin plot", actionLink(ns("info_violin_plot"), label = "", icon = icon("info-circle")), downloadButton(ns('violin_data'), label = "", icon = icon("save-file", lib = "glyphicon")))),
      status = "primary",
      width = 12,
      solidHeader = TRUE,
      collapsible = TRUE,
      withSpinner(plotlyOutput(ns(id = 'violin_plot')),type = 6, color = "#FFA812", size = 0.5),
      fluidRow(
        column(4,
               radioGroupButtons(
                 inputId = ns("box_or_violin"),
                 label = "Type of graph :", 
                 choices = c(`<i class='violin_logo'></i>` = "violin", `<i class='boxplot_logo'></i>` = "Boxplot"),
                 selected="violin",
                 justified = TRUE)),
        column(4,selectizeInput(ns("violon_color"), "Color:", choices = c("Default"="Red & Blue", "Green & Purple"), selected = "Red & Blue", multiple = FALSE, options = list(maxItems = 1))),
        column(4,selectizeInput(ns("violon_dot"), "Dot:", choices = c("Outliers "= "outliers", "All" = "all"), selected = "outliers", multiple = FALSE, options = list(maxItems = 1)))
      )
    )
  )
}


heatmap_ui <- function(id) {
#' Heatmap UI
#' 
#' @description Creates the UI component for displaying heatmaps
#' @param id Module ID
#' 
#' @return A Shiny UI element
  
  ns <- NS(id)
  box(
      useShinyFeedback(), 
    
      title = HTML(paste("Heatmap", actionLink(ns("info_heatmap_plot"), label = "", icon = icon("info-circle")), downloadButton(ns('heatmap_data'), label = "", icon = icon("save-file", lib = "glyphicon")))),
      status = "primary",
      width = 12,
      solidHeader = TRUE,
      collapsible = TRUE,

      withSpinner(uiOutput(ns("heatmap_plot")),type = 6, color = "#FFA812", size = 0.5),
      br(),
      br(),
      fluidRow(
        column(2,numericInput(ns("number"), "Number of default genes", value =10, min = 0,max=20, step = 1)%>%
                 helper(
                   colour = "#FFA812",
                   icon = "question",
                   type = "inline",
                   size = "m", 
                   fade = TRUE,
                   title = "Number of Default Genes",
                   content = c(
                     "This input controls the number of genes displayed by default on the heatmap. By default, genes are ordered by log2 fold change values, with 10 genes shown.",
                     "If you select specific genes and want only those to be displayed on the heatmap, reduce the number of default genes to 0. However, ensure there are always more than 2 genes on the heatmap!"
                   )
                 )),
        column(1,""),
        column(3, selectizeInput(ns("heatmap_palette"), "Heatmap Color Palette:", choices = c("Default" ="RdBu", "Green & Pink "="PiYG", "Red & Grey" = "RdGy", "Brown & Green "= "BrBG"), selected = "RdBu")),
        column(3, numericInput(ns("font_size"), "Font Size:", value = 8, min = 6, max = 20)),
        column(3, numericInput(ns("z_score_range"), "Z-score Range:",  value = 2, min = 2, max = 10, step = 1))
      ),
  )
}


correlation_ui <- function(id) {
#' Correlation UI
#'
#' @description Creates the UI layout for the correlation plot
#' @param id Module ID
#' 
#' @return A Shiny UI element for the correlation plot
  
  ns <- NS(id)
  box(
    title = HTML(paste("Correlation", actionLink(ns("info_correlation_plot"), label = "", icon = icon("info-circle")), downloadButton(ns('correlation_data'), label = "", icon = icon("save-file", lib = "glyphicon")))),
    width = 12,
    solidHeader = TRUE,
    status = "primary",
    
    withSpinner(uiOutput(ns('correlation_plot')),type = 6, color = "#FFA812", size = 0.5),
    br(),
    br(),
    fluidRow(useShinyjs(),
             column(6,selectizeInput(ns("cor_gene_of_interest"), "Gene of interest", choices = NULL, selected = NULL, multiple = FALSE, options = list(maxItems = 1))),
             column(6,numericInput(ns("correlation_threshold"), "Correlation threshold", value = 0.2, min = 0, max = 1, step = 0.1))
    )
    )
}


deseq2_table_ui <- function(id) {
#' DESeq2 Table UI
#' 
#' @description Creates the UI component for displaying DESeq2 results in a table
#' @param id Module ID
#' 
#' @return A Shiny UI element
  
  ns <- NS(id)
  fluidRow(
    box(
      width = 12,
      solidHeader = TRUE,
      title = "DESeq2 Results", status = "info", collapsible = TRUE,
      DT::dataTableOutput(ns("filtered_results"))
    )
  )
}
