seurat_ui <- function(id) {

  fluidPage(
    fluidRow(
      blurb_study_ui("kunz")),
    fluidRow(
      column(6, blurb_data_ui("kunz")),
      column(6, blurb_comparison_ui("kunz"))),
    fluidRow(
    column(5,inputs_ui("seurat_test")),
    column(7, cell_datatable_ui("seurat_test")),
    ),
    cell_info_ui("seurat_test"),
    gene_expression_ui("seurat_test"),
    column(7,gene_coexpression_ui("seurat_test")),
    column(5,coexpression_gene_datatable_ui("seurat_test")),
    column(6, sc_violin_ui("seurat_test")),
    column(6, proportion_ui("seurat_test")),
    bubheat_ui("seurat_test"),
    
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
  
  
  selection_result <- selection_server_single_cell(sc1conf, sc1def, h5_file_path, sc1gene, sc1meta, "seurat_test")
  
  inputs_server("seurat_test", selection_result)
  cell_info_server("seurat_test",selection_result)
  gene_expression_server("seurat_test", selection_result)
  gene_coexpression_server("seurat_test", selection_result)
  sc_violin_server("seurat_test", selection_result)
  proportion_server("seurat_test", selection_result)
  bubheat_server("seurat_test", selection_result)
  
}

