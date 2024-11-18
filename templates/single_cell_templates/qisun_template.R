qisun_ui <- function(id) {
#' qisun UI
#'
#' @description Creates the UI layout for the qisun analysis tab
#' @param id Module ID
#' 
#' @return A Shiny UI element for the qisun analysis tab
  
  fluidPage(
    fluidRow(blurb_study_ui("qisun")),
    fluidRow(column(6, blurb_data_ui("qisun")),
             column(6, blurb_method_ui("qisun"),
                    blurb_comparison_ui("qisun"))),
    fluidRow(
    column(5,inputs_ui("qisun")),
    column(7, cell_datatable_ui("qisun")),
    ),
    comparison_ui("qisun"),
    column(8,gene_coexpression_ui("qisun")),
    column(4,coexpression_gene_datatable_ui("qisun")),
    column(6, sc_violin_ui("qisun")),
    column(6, proportion_ui("qisun")),
    bubheat_ui("qisun"),
  )
}



qisun_server <- function() {
#' qisun Server
#'
#' @description Sets up the server logic for the qisun analysis tab  
  
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

    sc1conf_TBPT_file <- readRDS(sc1conf_TBPT$local_path)
    sc1def_TBPT_file <- readRDS(sc1def_TBPT$local_path)
    sc1gene_TBPT_file <- readRDS(sc1gene_TBPT$local_path)
    sc1meta_TBPT_file <- readRDS(sc1meta_TBPT$local_path)
    h5_file_path_TBPT_file <- h5_file_path_TBPT$local_path
    
    h5_file_path_TBPT_file <- H5File$new(h5_file_path_TBPT_file, mode = "r")
    
    
    if (file.exists(sc1conf_TBPT$local_path)) {
      file.remove(sc1conf_TBPT$local_path)
    }
    
    if (file.exists(sc1def_TBPT$local_path)) {
      file.remove(sc1def_TBPT$local_path)
    }
    
    if (file.exists(sc1gene_TBPT$local_path)) {
      file.remove(sc1gene_TBPT$local_path)
    }
    
    if (file.exists(sc1meta_TBPT$local_path)) {
      file.remove(sc1meta_TBPT$local_path)
    }
    
    if (file.exists(h5_file_path_TBPT$local_path)) {
      file.remove(h5_file_path_TBPT$local_path)
    }
  
  sc1conf <- list(sc1conf_TBPT_file)
  sc1def <- list(sc1def_TBPT_file)
  sc1gene <- list(sc1gene_TBPT_file)
  sc1meta <- list(sc1meta_TBPT_file)
  
  h5_file <- list(h5_file_path_TBPT_file)
  
  observe_helpers()
  
  
  
  
  
  selection_result <- selection_server_single_cell(sc1conf, sc1def, h5_file, sc1gene, sc1meta, "qisun")

  
  inputs_server("qisun", selection_result, DEG= "TBPT/harmonized_seurat_DE.rds")
  comparison_server("qisun", selection_result)
  gene_coexpression_server("qisun", selection_result)
  sc_violin_server("qisun", selection_result)
  proportion_server("qisun", selection_result)
  bubheat_server("qisun", selection_result)
  

  
  }, error = function(e) {
    showNotification("An error occurred during the process.", type = "error")
    print(e)
    
  })
  remove_modal_progress()

}

