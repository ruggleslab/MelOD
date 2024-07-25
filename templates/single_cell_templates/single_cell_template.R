library(shinyhelper)
library(data.table)
library(Matrix)
library(DT)
library(magrittr)
library(ggplot2)
library(ggrepel)
library(hdf5r)
library(ggdendro)
library(gridExtra)

source("global.R", local = TRUE)


inputs_ui <- function(id) {
  ns <- NS(id)
  fluidRow(
    box(
      title = "Inputs", status = "warning", solidHeader = TRUE,
      collapsible = TRUE, collapsed = FALSE,
      width = 12,
      column(8,
             selectInput(ns("cell_subset"), "Cell information to subset:",
                             choices = NULL),
             br(),
             withSpinner(uiOutput(ns("cell_subset_choices")), type = 6, color = "#FFA812", size = 0.5),
             br(),br(),
             actionButton(ns("cell_subset_all"), "Select all groups", class = "btn btn-primary"),
             actionButton(ns("cell_subset_none"), "Deselect all groups", class = "btn btn-primary")),
      column(4,
             numericInput(ns("marker_size"), "Point size:", value = 5, min = 1, max = 10),
             materialSwitch(
               inputId = ns("split_view"),
               label = "Split View: ",
               value = FALSE,
               status = "warning"
             )),
    )
  )
}


cell_datatable_ui <- function(id) {
  
  ns <- NS(id)
  fluidRow(
    box(
      width = 12,
      solidHeader = TRUE,
      title = "Cell DataTable", status = "info", collapsible = TRUE,
      DT::dataTableOutput(ns("cell_datatable")),
      selectizeInput(ns("inpsplt"), "Split continious data", choices = c("Quartile","Decile"),  selected = NULL),
      actionButton(ns("sc1a1splt"), "Select all groups", class = "btn btn-primary"),
      actionButton(ns("inp2"), "Select all groups", class = "btn btn-primary")
    )
  )
}





cell_info_ui <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    box(
      title = HTML(paste("Cell Information", downloadButton(ns("cell_plot_culstered_pdf"), icon = icon("save-file", lib = "glyphicon")), downloadButton(ns("cell_plot_culstered_png"), icon = icon("save-file", lib = "glyphicon")))),
      status = "primary", solidHeader = TRUE,
      
      withSpinner(plotlyOutput(ns("cell_plot_culstered"), height = '700px'), type = 6, color = "#FFA812", size = 0.5),
      width = 12,
      fluidRow(
        column(
          3, selectInput(ns("cell_plot_culstered_X_axis"), "X-axis:", choices = NULL),
          selectInput(ns("cell_plot_culstered_Y_axis"), "Y-axis:", choices = NULL)
        ),
        column(
          3, selectInput(ns("cell_plot_culstered_info"), "Cell information:", choices = NULL) %>%
            helper(type = "inline", size = "m", fade = TRUE,
                   title = "Cell information to colour cells by",
                   content = c("Select cell information to colour cells",
                               "- Categorical covariates have a fixed colour palette",
                               paste0("- Continuous covariates are coloured in a ",
                                      "Blue-Yellow-Red colour scheme, which can be ",
                                      "changed in the plot controls"))),
          conditionalPanel(
            condition = paste0("input['", ns("split_view"), "'] == true"),            
            selectInput(ns("cell_plot_culstered_info_2"), "Cell information 2:", choices = NULL)
          )
        ),
        column(3, selectizeInput(ns("cell_plot_culstered_color"), "Colour (Continuous data):", choices = c("White-Red","Default"="Blue-Yellow-Red","Yellow-Green-Purple"),  selected = "Blue-Yellow-Red")),
        column(3, checkboxInput(ns("cell_plot_culstered_label"), "Show cell info labels", value = TRUE)
        )
      )
    )
  )
}



gene_expression_ui <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    box(
      title = HTML(paste("Gene Expression", downloadButton(ns("gene_plot_culstered_pdf"), icon = icon("save-file", lib = "glyphicon")), downloadButton(ns("gene_plot_culstered_png"), icon = icon("save-file", lib = "glyphicon")))),
      status = "primary", solidHeader = TRUE,
      width = 12,
      withSpinner(plotlyOutput(ns("gene_plot_culstered"), height = '700px'), type = 6, color = "#FFA812", size = 0.5),
      fluidRow(
        column(6,
          column(6,selectizeInput(ns("gene_plot_culstered_selection"), "Gene name:", choices = NULL, selected = NULL, multiple = FALSE, options = list(maxItems = 1))) %>%
            helper(type = "inline", size = "m", fade = TRUE,
                   title = "Gene expression to colour cells by",
                   content = c("Select gene to colour cells by gene expression",
                               paste0("- Gene expression are coloured in a ",
                                      "White-Red colour scheme which can be ",
                                      "changed in the plot controls"))),
       
        conditionalPanel(
          condition = paste0("input['", ns("split_view"), "'] == true"),            
          column(6,selectizeInput(ns("gene_plot_culstered_selection_2"), "Gene name 2:", choices = NULL, selected = NULL, multiple = FALSE, options = list(maxItems = 1)))
        ) ),
        column(4, selectizeInput(ns("gene_plot_culstered_color"), "Colour (Continuous data):", choices = c("Default"="White-Red","Blue-Yellow-Red","Yellow-Green-Purple"),  selected = "White-Red")),
      )
    )
  )
}



gene_coexpression_ui <- function(id) {
  ns <- NS(id)
  fluidRow(
    box(
      title = HTML(paste("Gene Coexpression", downloadButton(ns("gene_plot_culstered_pdf"), icon = icon("save-file", lib = "glyphicon")), downloadButton(ns("gene_plot_culstered_png"), icon = icon("save-file", lib = "glyphicon")))),
      status = "primary", solidHeader = TRUE,
      width = 12,
      withSpinner(plotlyOutput(ns("gene_plot_coexpression"), height = '700px'), type = 6, color = "#FFA812", size = 0.5),
      fluidRow(
        column(4,selectizeInput(ns("gene_plot_coexpression_selection"), "Gene 1 name:", choices = NULL, selected = NULL, multiple = FALSE, options = list(maxItems = 1))),
        column(4,selectizeInput(ns("gene_plot_coexpression_selection_2"), "Gene 2 name:", choices = NULL, selected = NULL, multiple = FALSE, options = list(maxItems = 1))),
        column(4, radioButtons(ns("gene_plot_coexpression_color"), "Colour:", 
                               choices = c("Red (Gene1); Blue (Gene2)", 
                                           "Orange (Gene1); Blue (Gene2)", 
                                           "Red (Gene1); Green (Gene2)", 
                                           "Green (Gene1); Blue (Gene2)"), 
                               selected = "Red (Gene1); Blue (Gene2)"))
      )
    )
  )
}


coexpression_gene_datatable_ui <- function(id) {
  
  ns <- NS(id)
  fluidRow(
    box(
      width = 12,
      solidHeader = TRUE,
      title = "Coexpression gene DataTable", status = "info", collapsible = TRUE,
      DT::dataTableOutput(ns("gene_datatable_coexpression"))
    )
  )
}


sc_violin_ui <- function(id) {
  ns <- NS(id)
  fluidRow(
    box(
      title = HTML(paste("Violin & Boxplot", downloadButton(ns("gene_plot_culstered_pdf"), icon = icon("save-file", lib = "glyphicon")), downloadButton(ns("gene_plot_culstered_png"), icon = icon("save-file", lib = "glyphicon")))),
      status = "primary", solidHeader = TRUE,
      width = 12,
      withSpinner(plotlyOutput(ns("sc_violin_plot"), height = '700px'), type = 6, color = "#FFA812", size = 0.5),
      fluidRow(
        column(3,selectizeInput(ns("violin_plot_X"), "Cell information (X-axis):", choices = NULL, selected = NULL, multiple = FALSE, options = list(maxItems = 1))),
        column(3,selectizeInput(ns("violin_plot_Y"), "Cell Info / Gene name (Y-axis):", choices = NULL, selected = NULL, multiple = FALSE, options = list(maxItems = 1))),
        column(3,selectizeInput(ns("sc_box_or_violin"), "Type of plot:", choices = c("Default" = "violin", "Boxplot" ="box"), selected = "violin", multiple = FALSE, options = list(maxItems = 1))),
        column(4,selectizeInput(ns("sc_violon_dot"), "Dot:", choices = c("Outliers "= "outliers", "All" = "all"), selected = "outliers", multiple = FALSE, options = list(maxItems = 1)))
)
      )
    )
  
}


proportion_ui <- function(id) {
  ns <- NS(id)
  fluidRow(
    box(
      title = HTML(paste("Proportion plot", downloadButton(ns("gene_plot_culstered_pdf"), icon = icon("save-file", lib = "glyphicon")), downloadButton(ns("gene_plot_culstered_png"), icon = icon("save-file", lib = "glyphicon")))),
      status = "primary", solidHeader = TRUE,
      width = 12,
      withSpinner(plotlyOutput(ns("proportion_plot"), height = '700px'), type = 6, color = "#FFA812", size = 0.5),
      fluidRow(
        column(3,selectizeInput(ns("proportion_plot_X"), "Cell information to plot (X-axis):", choices = NULL, selected = NULL, multiple = FALSE, options = list(maxItems = 1))),
        column(3,selectizeInput(ns("proportion_group_by"), "Cell information to group / colour by:", choices = NULL, selected = NULL, multiple = FALSE, options = list(maxItems = 1))),
        column(3,selectizeInput(ns("proportion_type"), "Plot value", choices = c("Proportion" = "Proportion", "Cell Number" ="cell_number"), selected = "Proportion", multiple = FALSE, options = list(maxItems = 1))),
        column(3,checkboxInput(ns("proportion_flip_axis"), "Flip axis", value = FALSE))
      )
    )
  )
  
}


bubheat_ui <- function(id) {
  ns <- NS(id)
  fluidRow(
    box(
      title = HTML(paste("Heatmap & Bubble plot", downloadButton(ns("gene_plot_culstered_pdf"), icon = icon("save-file", lib = "glyphicon")), downloadButton(ns("gene_plot_culstered_png"), icon = icon("save-file", lib = "glyphicon")))),
      status = "primary", solidHeader = TRUE,
      width = 12,
      withSpinner(plotlyOutput(ns("bubheat_plot"), height = '700px'), type = 6, color = "#FFA812", size = 0.5),
      fluidRow(
        column(3,multiInput(
          inputId = ns("bubheat_selected_gene"),
          label = "Gene(s) selection (up to 10):",
          autocomplete = TRUE,
          option= list(limit=10),
          choices = "Loading...")),
        column(2,selectizeInput(ns("bubheat_type"), "Type of plot:", choices = c("Default" = "Bubbleplot", "Heatmap" ="Heatmap"), selected = "Bubbleplot", multiple = FALSE, options = list(maxItems = 1))),
        column(2,selectizeInput(ns("bubheat_group_by"), "Group by:", choices = NULL, selected = NULL, multiple = FALSE, options = list(maxItems = 1))),
        column(1,checkboxInput(ns("bubheat_scale"), "Scale gene expression", value = TRUE)),
        column(1,checkboxInput(ns("bubheat_cluster_rows"), "Cluster rows (genes)", value = TRUE)),
        column(1,checkboxInput(ns("bubheat_cluster_columns"), "Cluster columns (samples)", value = FALSE)),
        column(2, selectizeInput(ns("bubheat_color"), "Colour:", choices = c("Default"="White-Red","Blue-Yellow-Red","Yellow-Green-Purple"),  selected = "White-Red"))
      )
    )
  )
  
}