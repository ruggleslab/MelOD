library(shiny)

# Define server logic
shinyServer(function(input, output, session) {
  
############### IMPORT TAB #################
  import_server("import_tab")

############### BULK RNA TAB #################
  bulkrna_server("bulkrna_tab")
})
















