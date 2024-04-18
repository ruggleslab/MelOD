library(shiny)
library(bslib)
library(bsicons)

# Define UI
ui <- fluidPage(
  theme = shinythemes::shinytheme("sandstone"),
  includeCSS("./www/styles.css"),
  navbarPage(
    "ShinySeq",
    collapsible=TRUE,
    tabPanel(
      "Query",
      sidebarLayout(
        sidebarPanel(
          textInput("gene_search", "Search Gene:"),
          p("OR"),
          fileInput("file", "Upload CSV file", multiple = TRUE),
          
          # Help text
          tags$br(),
          helpText("Default max file size is 5MB"),
          helpText("Select read.table parameters below:"),
          
          # Checkbox for header
          checkboxInput(
            inputId = "header",
            label = "Header",
            value = TRUE
          ),
          
          
          # Radio buttons for separator
          radioButtons(
            inputId = "sep",
            label = "Separator",
            choices = c(
              Comma = ",",
              Semicolon = ";",
              Tab = "\t",
              Space = " "
            ),
            selected = ","
          ),
          uiOutput("selectfile")
          
        ),
        
        
        mainPanel(# Table output
          tableOutput("tb"))
      )
    ),
    tabPanel("Bulk RNA",
             plotOutput("bulk_rna_boxplot"),
             plotOutput("bulk_rna_correlation")),
    tabPanel("scRNA-SEQ",
             plotOutput("scrna_boxplot"),
             plotOutput("scrna_correlation")),
    tabPanel("Array",
             plotOutput("array_boxplot"),
             plotOutput("array_correlation")),
    tabPanel(
      "Proteomics",
      plotOutput("proteomics_boxplot"),
      plotOutput("proteomics_correlation")
    ))
)
