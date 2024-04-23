library(shiny)

# Define server logic
shinyServer(function(input, output, session) {
  
############### IMPORT TAB #################
  home_server("home_tab")

############### BULK RNA TAB #################
  bulkrna_server("bulkrna_tab")
  
  singlecell_server("singlecell_tab")
})

