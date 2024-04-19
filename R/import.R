import_ui <-function(id){
  
sidebarLayout(
  sidebarPanel(
    textInput(NS(id,"gene_search"), "Search Gene:"),
    
    p("OR"),
    
    fileInput(NS(id,"file"), "Upload CSV file", multiple = TRUE),
    helpText("Default max file size is 5MB"),
    htmlOutput(NS(id,'table_parameters')),
    htmlOutput(NS(id,"selectfile"))
  ),
  
  mainPanel(# Table output
    htmlOutput(NS(id,"table_output"))
))
}

import_server <- function(id) {
  moduleServer(id, function(input, output, session) {


    output$table_parameters <- renderUI({
      req(input$file)
      # Help text
      fluidRow(
        helpText("Select read.table parameters below:"),
        column(8, # Radio buttons for separator
               radioButtons(
                 inputId = NS(id,"sep"),
                 label = "Separator",
                 choices = c(
                   Comma = ",",
                   Semicolon = ";",
                   Tab = "\t",
                   Space = " "),
                 selected = ",")),
        
        column(4, # Checkbox for header
               checkboxInput(
                 inputId = NS(id,"header"),
                 label = "Header",
                 value = TRUE))
      )
    })
    
    
    ## Render selectInput for file selection
    output$selectfile <- renderUI({
      req(input$file)
      list(
        hr(),
        helpText("Select the files for which you need to see data and summary stats"),
        selectInput(NS(id,"Select"), "Select", choices = input$file$name)
      )
    })
    
    
    ## Render input$file data frame
    output$filedf <- renderTable({
      req(input$file)
      input$file
    })
    
    ## Render file path for input$file
    output$filedf2 <- renderTable({
      req(input$file)
      input$file$datapath
    })
    
    ## Render structure of input$file
    output$fileob <- renderPrint({
      req(input$file)
      str(input$file)
    })
    
    
    ## Render selected dataset
    output$table <- renderTable({ 
      if(is.null(input$file)){return()}
      read.table(file=input$file$datapath[input$file$name==input$Select], sep=input$sep, header = input$header)
      
    })
    
    
    ## Render summary stats of selected dataset
    output$summ <- renderPrint({
      if(is.null(input$file)){return()}
      summary(read.table(file=input$file$datapath[input$file$name==input$Select], 
                         sep=input$sep, 
                         header = input$header, 
      ))})
    
    

    ## Render tabsetPanel
    output$table_output <- renderUI({
      req(input$file)
      tabsetPanel(
        tabPanel("Input File Object DF", tableOutput(NS(id,"filedf")), tableOutput(NS(id,"filedf2"))),
        tabPanel("Input File Object Structure", verbatimTextOutput(NS(id,"fileob"))),
        tabPanel("Dataset", tableOutput(NS(id,"table"))),
        tabPanel("Summary Stats", verbatimTextOutput(NS(id,"summ")))
      )
    })
    
  })
}
