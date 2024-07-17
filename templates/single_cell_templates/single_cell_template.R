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



