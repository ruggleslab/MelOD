# Define UI
home_ui <-function(id){
NS <- NS(id)
  titlePanel("Dataset Selection")
  sidebarLayout(
    sidebarPanel(
      selectInput(NS("dataset"), "Select Dataset:",
                  choices = c("Dataset 1", "Dataset 2", "Dataset 3")),
      actionButton("load", "Load Dataset")
    ),
    mainPanel(
      textOutput("status")
    )
  )


}

home_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Define a reactive expression to load the dataset
    loaded_dataset <- reactive({
      switch(input$dataset,
             "Dataset 1" = {
               read.csv("path_to_dataset_1.csv")
             },
             "Dataset 2" = {
               read.csv("path_to_dataset_2.csv")
             },
             "Dataset 3" = {
               read.csv("path_to_dataset_3.csv")
             }
      )
    })
    
    # Display status message
    output$status <- renderText({
      paste("Dataset", input$dataset, "loaded.")
    })
    
    # Observe the click event on the load button and update the status
    observeEvent(input$load, {
      loaded_dataset()
    })
  })
}
