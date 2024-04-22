
# Define UI
ui <- fluidPage(
  theme = shinythemes::shinytheme("sandstone"),
  includeCSS("./www/styles.css"),
  navbarPage("ShinySeq",
    collapsible=TRUE,
    
############## TAB PANEL Import ###############

    tabPanel("Import",
             import_ui("import_tab")),

############## TAB PANEL BULK RNA ###############

    tabPanel("Bulk RNA",
             bulkrna_ui("bulkrna_tab")),
             
    tabPanel("scRNA-SEQ",
             plotOutput("scrna_boxplot"),
             plotOutput("scrna_correlation")),
    tabPanel("Array",
             plotOutput("array_boxplot"),
             plotOutput("array_correlation")),
    tabPanel(
      "Proteomics",
      plotOutput("proteomics_boxplot"),
      plotOutput("proteomics_correlation")),
    # oncoplot tab ----
    tabPanel("Oncoplot",
         theme = bs_theme(version = 5, bootswatch = "pulse"),
         oncoplotUI("oncoTab", cohorts)
),

)
)
