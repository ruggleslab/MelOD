KBPT_ui <- function(id) {

  fluidPage(
    fluidRow(blurb_study_ui("KBPT")),
    fluidRow(column(6, blurb_data_ui("KBPT")),
             column(6, fluidRow(blurb_comparison_ui("KBPT")))),
    fluidRow(
    column(5,inputs_ui("KBPT")),
    column(7, cell_datatable_ui("KBPT")),
    ),
    comparison_ui("KBPT"),
    column(7,gene_coexpression_ui("KBPT")),
    column(5,coexpression_gene_datatable_ui("KBPT")),
    column(6, sc_violin_ui("KBPT")),
    column(6, proportion_ui("KBPT")),
    bubheat_ui("KBPT"),
    
    tags$h2("Credit. ShinyCell", tags$style(HTML(".shiny-output-error-validation {color: red; font-weight: bold;}")))
  )
}




KBPT_server <- function() {

  show_modal_progress_line(color = "#FFA812",text = "Initialization")
  Sys.sleep(0.5)

  tryCatch({
    update_modal_progress(0.3, text="Downloading data...")
    Sys.sleep(0.5)

    update_modal_progress(0.4, text="Config files..")

    sc1conf_KBPT <- drive_download("KBPT/sc1conf.rds", overwrite = TRUE)
    sc1def_KBPT <- drive_download("KBPT/sc1def.rds", overwrite = TRUE)
    sc1gene_KBPT <- drive_download("KBPT/sc1gene.rds", overwrite = TRUE)
    sc1meta_KBPT <- drive_download("KBPT/sc1meta.rds", overwrite = TRUE)

    update_modal_progress(0.5, text="Gene expression files..")

    # h5_file_path_KBPT <- drive_download("KBPT/sc1gexpr.h5", overwrite = TRUE)
   
    
    local_folder <- "/Users/paul/Documents/pro/shiny-seq/data/single_cell/mayumi/KBPT"
    h5_file_path_KBPT <- drive_download("KBPT/sc1gexpr.h5", path = file.path(local_folder, "sc1gexpr.h5"), overwrite = TRUE)
    

    update_modal_progress(0.6, text="Loading data...")
    Sys.sleep(0.5)



    sc1conf_KBPT <- readRDS(sc1conf_KBPT$local_path)
    sc1def_KBPT <- readRDS(sc1def_KBPT$local_path)
    sc1gene_KBPT <- readRDS(sc1gene_KBPT$local_path)
    sc1meta_KBPT <- readRDS(sc1meta_KBPT$local_path)
    h5_file_path_KBPT <- h5_file_path_KBPT$local_path


    sc1conf <- list(sc1conf_KBPT)
    sc1def <- list(sc1def_KBPT)
    sc1gene <- list(sc1gene_KBPT)
    sc1meta <- list(sc1meta_KBPT)
    
    
    h5_file_path_KBPT <- H5File$new(h5_file_path_KBPT, mode = "r")

    h5_file <- list(h5_file_path_KBPT)

    observe_helpers()

    selection_result <- selection_server_single_cell(sc1conf, sc1def, h5_file, sc1gene, sc1meta, "KBPT")




  inputs_server("KBPT", selection_result)
  comparison_server("KBPT", selection_result)
  gene_coexpression_server("KBPT", selection_result)
  sc_violin_server("KBPT", selection_result)
  proportion_server("KBPT", selection_result)
  bubheat_server("KBPT", selection_result)

  }, error = function(e) {
    showNotification("An error occurred during the process.", type = "error")
    print(e)

  })
  remove_modal_progress()

}
