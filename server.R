library(shiny)

# Define server logic
shinyServer(function(input, output) {
  
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
  
  ## Render selectInput for file selection
  output$selectfile <- renderUI({
    req(input$file)
    list(
      hr(),
      helpText("Select the files for which you need to see data and summary stats"),
      selectInput("Select", "Select", choices = input$file$name)
    )
  })
  
  ## Render summary stats of selected dataset
  output$summ <- renderPrint({
    if(is.null(input$file)){return()}
    summary(read.table(file=input$file$datapath[input$file$name==input$Select], 
                       sep=input$sep, 
                       header = input$header, 
                       ))})
  
  
  ## Render selected dataset
  output$table <- renderTable({ 
    if(is.null(input$file)){return()}
    read.table(file=input$file$datapath[input$file$name==input$Select], sep=input$sep, header = input$header)
    
  })
  ## Render tabsetPanel
  output$tb <- renderUI({
    req(input$file)
    tabsetPanel(
      tabPanel("Input File Object DF", tableOutput("filedf"), tableOutput("filedf2")),
      tabPanel("Input File Object Structure", verbatimTextOutput("fileob")),
      tabPanel("Dataset", tableOutput("table")),
      tabPanel("Summary Stats", verbatimTextOutput("summ"))
    )
  })
})
