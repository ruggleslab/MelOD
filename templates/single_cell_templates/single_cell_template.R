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
             column(3, numericInput(ns("marker_size"), "Point size:", value = 5, min = 1, max = 10))
),
      
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
          4, selectInput(ns("cell_plot_culstered_X_axis"), "X-axis:", choices = NULL),
          selectInput(ns("cell_plot_culstered_Y_axis"), "Y-axis:", choices = NULL)
        ),
        column(
          4, selectInput(ns("cell_plot_culstered_info"), "Cell information:", choices = NULL) %>%
            helper(type = "inline", size = "m", fade = TRUE,
                   title = "Cell information to colour cells by",
                   content = c("Select cell information to colour cells",
                               "- Categorical covariates have a fixed colour palette",
                               paste0("- Continuous covariates are coloured in a ",
                                      "Blue-Yellow-Red colour scheme, which can be ",
                                      "changed in the plot controls")))
        ),
        column(
          4, 
          radioButtons(ns("cell_plot_culstered_color"), "Colour (Continuous data):",
                       choices = c("White-Red","Blue-Yellow-Red","Yellow-Green-Purple"),
                       selected = "Blue-Yellow-Red"),
          checkboxInput(ns("cell_plot_culstered_label"), "Show cell info labels", value = TRUE)
          
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
      plotlyOutput(ns("gene_plot_culstered"), height = '700px'),
      fluidRow(
        column(
          6, selectInput(ns("gene_plot_culstered_selection"), "Gene name:", choices = NULL) %>%
            helper(type = "inline", size = "m", fade = TRUE,
                   title = "Gene expression to colour cells by",
                   content = c("Select gene to colour cells by gene expression",
                               paste0("- Gene expression are coloured in a ",
                                      "White-Red colour scheme which can be ",
                                      "changed in the plot controls")))
        ),
        column(
          6, 
          radioButtons(ns("gene_plot_culstered_color"), "Colour:", choices = c("White-Red","Blue-Yellow-Red","Yellow-Green-Purple"),
                       selected = "White-Red")
          
        )
      )
    )
  )
}


