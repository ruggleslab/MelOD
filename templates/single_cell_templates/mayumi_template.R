mayumi_ui <- function(id) {

  fluidPage(
    fluidRow(blurb_study_ui("mayumi")),
    fluidRow(column(6, blurb_data_ui("mayumi")),
             column(6, fluidRow(blurb_comparison_ui("mayumi")))),
    fluidRow(
    column(5,inputs_ui("mayumi")),
    column(7, cell_datatable_ui("mayumi")),
    ),
    comparison_ui("mayumi"),
    column(8,gene_coexpression_ui("mayumi")),
    column(4,coexpression_gene_datatable_ui("mayumi")),
    column(6, sc_violin_ui("mayumi")),
    column(6, proportion_ui("mayumi")),
    bubheat_ui("mayumi"),
    
    tags$h2("Credit. ShinyCell", tags$style(HTML(".shiny-output-error-validation {color: red; font-weight: bold;}")))
  )
}



mayumi_server <- function() {
  
  
  show_modal_progress_line(color = "#FFA812",text = "Initialization")
  Sys.sleep(0.5)
  
  tryCatch({
    update_modal_progress(0.3, text="Downloading data...")
    Sys.sleep(0.5)


    update_modal_progress(0.4, text="Config files..")

    sc1conf_TBPT <- drive_download("TBPT/sc1conf.rds", overwrite = TRUE)
    sc1def_TBPT <- drive_download("TBPT/sc1def.rds", overwrite = TRUE)
    sc1gene_TBPT <- drive_download("TBPT/sc1gene.rds", overwrite = TRUE)
    sc1meta_TBPT <- drive_download("TBPT/sc1meta.rds", overwrite = TRUE)

    update_modal_progress(0.5, text="Gene expression files..")

    h5_file_path_TBPT <- drive_download("TBPT/sc1gexpr.h5", overwrite = TRUE)


    update_modal_progress(0.6, text="Loading data...")
    Sys.sleep(0.5)



    sc1conf_TBPT <- readRDS(sc1conf_TBPT$local_path)
    sc1def_TBPT <- readRDS(sc1def_TBPT$local_path)
    sc1gene_TBPT <- readRDS(sc1gene_TBPT$local_path)
    sc1meta_TBPT <- readRDS(sc1meta_TBPT$local_path)
    h5_file_path_TBPT <- h5_file_path_TBPT$local_path

  
  # 
  # sc1conf_TBPT <- readRDS("/Users/paul/Documents/pro/shiny-seq/data/single_cell/mayumi/TBPT/sc1conf.rds")
  # sc1def_TBPT <- readRDS("/Users/paul/Documents/pro/shiny-seq/data/single_cell/mayumi/TBPT/sc1def.rds")
  # sc1gene_TBPT <- readRDS("/Users/paul/Documents/pro/shiny-seq/data/single_cell/mayumi/TBPT/sc1gene.rds")
  # sc1meta_TBPT <- readRDS("/Users/paul/Documents/pro/shiny-seq/data/single_cell/mayumi/TBPT/sc1meta.rds")
  # h5_file_path_TBPT <- "/Users/paul/Documents/pro/shiny-seq/data/single_cell/mayumi/TBPT/sc1gexpr.h5"

  sc1conf <- list(sc1conf_TBPT)
  sc1def <- list(sc1def_TBPT)
  sc1gene <- list(sc1gene_TBPT)
  sc1meta <- list(sc1meta_TBPT)
  
  h5_file_path_TBPT <- H5File$new(h5_file_path_TBPT, mode = "r")

  h5_file <- list(h5_file_path_TBPT)
  
  observe_helpers()
  
  selection_result <- selection_server_single_cell(sc1conf, sc1def, h5_file, sc1gene, sc1meta, "mayumi")

  
  inputs_server("mayumi", selection_result)
  comparison_server("mayumi", selection_result)
  gene_coexpression_server("mayumi", selection_result)
  sc_violin_server("mayumi", selection_result)
  proportion_server("mayumi", selection_result)
  bubheat_server("mayumi", selection_result)
  
  }, error = function(e) {
    showNotification("An error occurred during the process.", type = "error")
    print(e)
    
  })
  remove_modal_progress()

}

