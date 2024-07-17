seurat_ui <- function(id) {

  fluidPage(
    fluidRow(
    column(5,inputs_ui("seurat_test")),
    column(7, cell_datatable_ui("seurat_test")),
    ),
    cell_info_ui("seurat_test"),
    gene_expression_ui("seurat_test"),
    tags$h2("Credit. ShinyCell", tags$style(HTML(".shiny-output-error-validation {color: red; font-weight: bold;}")))
  )
}




seurat_server <- function(input, output, session) {
  sc1conf <- readRDS("./data/single_cell/sc1conf.rds")
  sc1def <- readRDS("./data/single_cell/sc1def.rds")
  sc1gene <- readRDS("./data/single_cell/sc1gene.rds")
  sc1meta <- readRDS("./data/single_cell/sc1meta.rds")
  h5_file_path <- "./data/single_cell/sc1gexpr.h5"
  
  observe_helpers()
  
  inputs_server("seurat_test", sc1conf, sc1def)
  cell_info_server("seurat_test", sc1conf, sc1meta, sc1def)
  gene_expression_server("seurat_test", sc1conf, sc1meta, sc1gene, sc1def, h5_file_path)
  
 
}

