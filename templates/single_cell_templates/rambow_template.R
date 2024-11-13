rambow_ui <- function(id) {
  #' rambow UI
  #'
  #' @description Creates the UI layout for the rambow analysis tab
  #' @param id Module ID
  #' 
  #' @return A Shiny UI element for the rambow analysis tab
  
  fluidPage(
    fluidRow(blurb_study_ui("rambow")),
    fluidRow(column(6, blurb_data_ui("rambow")),
             column(6, blurb_method_ui("rambow"),
                    blurb_comparison_ui("rambow"))),
    fluidRow(
      column(5,inputs_ui("rambow")),
      column(7, cell_datatable_ui("rambow")),
    ),
    comparison_ui("rambow"),
    column(8,gene_coexpression_ui("rambow")),
    column(4,coexpression_gene_datatable_ui("rambow")),
    column(6, sc_violin_ui("rambow")),
    column(6, proportion_ui("rambow")),
    bubheat_ui("rambow"),
  )
}



rambow_server <- function() {
  #' rambow Server
  #'
  #' @description Sets up the server logic for the rambow analysis tab  
  
  show_modal_progress_line(color = "#FFA812",text = "Initialization")
  Sys.sleep(0.5)
  
  tryCatch({
    update_modal_progress(0.3, text="Downloading data...")
    Sys.sleep(0.5)
    
    
    update_modal_progress(0.4, text="Config files..")
    
    sc1conf_rambow <- drive_download("rambow/sc1conf.rds", overwrite = TRUE)
    sc1def_rambow <- drive_download("rambow/sc1def.rds", overwrite = TRUE)
    sc1gene_rambow <- drive_download("rambow/sc1gene.rds", overwrite = TRUE)
    sc1meta_rambow <- drive_download("rambow/sc1meta.rds", overwrite = TRUE)
    
    update_modal_progress(0.5, text="Gene expression files..")
    
    h5_file_path_rambow <- drive_download("rambow/sc1gexpr.h5", overwrite = TRUE)
    
    update_modal_progress(0.6, text="Loading data...")
    Sys.sleep(0.5)
    
    sc1conf_rambow_file <- readRDS(sc1conf_rambow$local_path)
    sc1def_rambow_file <- readRDS(sc1def_rambow$local_path)
    sc1gene_rambow_file <- readRDS(sc1gene_rambow$local_path)
    sc1meta_rambow_file <- readRDS(sc1meta_rambow$local_path)
    h5_file_path_rambow_file <- h5_file_path_rambow$local_path
    
    h5_file_path_rambow_file <- H5File$new(h5_file_path_rambow_file, mode = "r")
    
    
    
    if (file.exists(sc1conf_rambow$local_path)) {
      file.remove(sc1conf_rambow$local_path)
    }
    
    if (file.exists(sc1def_rambow$local_path)) {
      file.remove(sc1def_rambow$local_path)
    }
    
    if (file.exists(sc1gene_rambow$local_path)) {
      file.remove(sc1gene_rambow$local_path)
    }
    
    if (file.exists(sc1meta_rambow$local_path)) {
      file.remove(sc1meta_rambow$local_path)
    }
    
    if (file.exists(h5_file_path_rambow$local_path)) {
      file.remove(h5_file_path_rambow$local_path)
    }
    
    sc1conf <- list(sc1conf_rambow_file)
    sc1def <- list(sc1def_rambow_file)
    sc1gene <- list(sc1gene_rambow_file)
    sc1meta <- list(sc1meta_rambow_file)
    
    h5_file <- list(h5_file_path_rambow_file)
    
    observe_helpers()
    
    
    
    
    
    selection_result <- selection_server_single_cell(sc1conf, sc1def, h5_file, sc1gene, sc1meta, "rambow")
    
    
    inputs_server("rambow", selection_result, DEG= "rambow/harmonized_seurat_DE.rds")
    comparison_server("rambow", selection_result)
    gene_coexpression_server("rambow", selection_result)
    sc_violin_server("rambow", selection_result)
    proportion_server("rambow", selection_result)
    bubheat_server("rambow", selection_result)
    
    
    
  }, error = function(e) {
    showNotification("An error occurred during the process.", type = "error")
    print(e)
    
  })
  remove_modal_progress()
  
}

