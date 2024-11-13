pozniak_ui <- function(id) {
  #' pozniak UI
  #'
  #' @description Creates the UI layout for the pozniak analysis tab
  #' @param id Module ID
  #' 
  #' @return A Shiny UI element for the pozniak analysis tab
  
  fluidPage(
    fluidRow(blurb_study_ui("pozniak")),
    fluidRow(column(6, blurb_data_ui("pozniak")),
             column(6, blurb_method_ui("pozniak"),
                    blurb_comparison_ui("pozniak"))),
    fluidRow(
      column(5,inputs_ui("pozniak")),
      column(7, cell_datatable_ui("pozniak")),
    ),
    comparison_ui("pozniak"),
    column(8,gene_coexpression_ui("pozniak")),
    column(4,coexpression_gene_datatable_ui("pozniak")),
    column(6, sc_violin_ui("pozniak")),
    column(6, proportion_ui("pozniak")),
    bubheat_ui("pozniak"),
  )
}



pozniak_server <- function() {
  #' pozniak Server
  #'
  #' @description Sets up the server logic for the pozniak analysis tab  
  
  show_modal_progress_line(color = "#FFA812",text = "Initialization")
  Sys.sleep(0.5)
  
  tryCatch({
    update_modal_progress(0.3, text="Downloading data...")
    Sys.sleep(0.5)
    
    
    update_modal_progress(0.4, text="Config files..")
    
    sc1conf_pozniak <- drive_download("pozniak/sc1conf.rds", overwrite = TRUE)
    sc1def_pozniak <- drive_download("pozniak/sc1def.rds", overwrite = TRUE)
    sc1gene_pozniak <- drive_download("pozniak/sc1gene.rds", overwrite = TRUE)
    sc1meta_pozniak <- drive_download("pozniak/sc1meta.rds", overwrite = TRUE)
    
    update_modal_progress(0.5, text="Gene expression files..")
    
    h5_file_path_pozniak <- drive_download("pozniak/sc1gexpr.h5", overwrite = TRUE)
    
    update_modal_progress(0.6, text="Loading data...")
    Sys.sleep(0.5)
    
    sc1conf_pozniak_file <- readRDS(sc1conf_pozniak$local_path)
    sc1def_pozniak_file <- readRDS(sc1def_pozniak$local_path)
    sc1gene_pozniak_file <- readRDS(sc1gene_pozniak$local_path)
    sc1meta_pozniak_file <- readRDS(sc1meta_pozniak$local_path)
    h5_file_path_pozniak_file <- h5_file_path_pozniak$local_path
    
    h5_file_path_pozniak_file <- H5File$new(h5_file_path_pozniak_file, mode = "r")
    
    
    
    if (file.exists(sc1conf_pozniak$local_path)) {
      file.remove(sc1conf_pozniak$local_path)
    }
    
    if (file.exists(sc1def_pozniak$local_path)) {
      file.remove(sc1def_pozniak$local_path)
    }
    
    if (file.exists(sc1gene_pozniak$local_path)) {
      file.remove(sc1gene_pozniak$local_path)
    }
    
    if (file.exists(sc1meta_pozniak$local_path)) {
      file.remove(sc1meta_pozniak$local_path)
    }
    
    if (file.exists(h5_file_path_pozniak$local_path)) {
      file.remove(h5_file_path_pozniak$local_path)
    }
    
    sc1conf <- list(sc1conf_pozniak_file)
    sc1def <- list(sc1def_pozniak_file)
    sc1gene <- list(sc1gene_pozniak_file)
    sc1meta <- list(sc1meta_pozniak_file)
    
    h5_file <- list(h5_file_path_pozniak_file)
    
    observe_helpers()
    
    
    
    
    
    selection_result <- selection_server_single_cell(sc1conf, sc1def, h5_file, sc1gene, sc1meta, "pozniak")
    
    
    inputs_server("pozniak", selection_result, DEG= "pozniak/harmonized_seurat_DE.rds")
    comparison_server("pozniak", selection_result)
    gene_coexpression_server("pozniak", selection_result)
    sc_violin_server("pozniak", selection_result)
    proportion_server("pozniak", selection_result)
    bubheat_server("pozniak", selection_result)
    
    
    
  }, error = function(e) {
    showNotification("An error occurred during the process.", type = "error")
    print(e)
    
  })
  remove_modal_progress()
  
}

