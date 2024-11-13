jerby_ui <- function(id) {
  #' jerby UI
  #'
  #' @description Creates the UI layout for the jerby analysis tab
  #' @param id Module ID
  #' 
  #' @return A Shiny UI element for the jerby analysis tab
  
  fluidPage(
    fluidRow(blurb_study_ui("jerby")),
    fluidRow(column(6, blurb_data_ui("jerby")),
             column(6, blurb_method_ui("jerby"),
                    blurb_comparison_ui("jerby"))),
    fluidRow(
      column(5,inputs_ui("jerby")),
      column(7, cell_datatable_ui("jerby")),
    ),
    comparison_ui("jerby"),
    column(8,gene_coexpression_ui("jerby")),
    column(4,coexpression_gene_datatable_ui("jerby")),
    column(6, sc_violin_ui("jerby")),
    column(6, proportion_ui("jerby")),
    bubheat_ui("jerby"),
  )
}



jerby_server <- function() {
  #' jerby Server
  #'
  #' @description Sets up the server logic for the jerby analysis tab  
  
  show_modal_progress_line(color = "#FFA812",text = "Initialization")
  Sys.sleep(0.5)
  
  tryCatch({
    update_modal_progress(0.3, text="Downloading data...")
    Sys.sleep(0.5)
    
    
    update_modal_progress(0.4, text="Config files..")
    
    sc1conf_jerby <- drive_download("jerby/sc1conf.rds", overwrite = TRUE)
    sc1def_jerby <- drive_download("jerby/sc1def.rds", overwrite = TRUE)
    sc1gene_jerby <- drive_download("jerby/sc1gene.rds", overwrite = TRUE)
    sc1meta_jerby <- drive_download("jerby/sc1meta.rds", overwrite = TRUE)
    
    update_modal_progress(0.5, text="Gene expression files..")
    
    h5_file_path_jerby <- drive_download("jerby/sc1gexpr.h5", overwrite = TRUE)
    
    update_modal_progress(0.6, text="Loading data...")
    Sys.sleep(0.5)
    
    sc1conf_jerby_file <- readRDS(sc1conf_jerby$local_path)
    sc1def_jerby_file <- readRDS(sc1def_jerby$local_path)
    sc1gene_jerby_file <- readRDS(sc1gene_jerby$local_path)
    sc1meta_jerby_file <- readRDS(sc1meta_jerby$local_path)
    h5_file_path_jerby_file <- h5_file_path_jerby$local_path
    
    h5_file_path_jerby_file <- H5File$new(h5_file_path_jerby_file, mode = "r")
    
    
    
    if (file.exists(sc1conf_jerby$local_path)) {
      file.remove(sc1conf_jerby$local_path)
    }
    
    if (file.exists(sc1def_jerby$local_path)) {
      file.remove(sc1def_jerby$local_path)
    }
    
    if (file.exists(sc1gene_jerby$local_path)) {
      file.remove(sc1gene_jerby$local_path)
    }
    
    if (file.exists(sc1meta_jerby$local_path)) {
      file.remove(sc1meta_jerby$local_path)
    }
    
    if (file.exists(h5_file_path_jerby$local_path)) {
      file.remove(h5_file_path_jerby$local_path)
    }
    
    sc1conf <- list(sc1conf_jerby_file)
    sc1def <- list(sc1def_jerby_file)
    sc1gene <- list(sc1gene_jerby_file)
    sc1meta <- list(sc1meta_jerby_file)
    
    h5_file <- list(h5_file_path_jerby_file)
    
    observe_helpers()
    
    
    
    
    
    selection_result <- selection_server_single_cell(sc1conf, sc1def, h5_file, sc1gene, sc1meta, "jerby")
    
    
    inputs_server("jerby", selection_result, DEG= "jerby/harmonized_seurat_DE.rds")
    comparison_server("jerby", selection_result)
    gene_coexpression_server("jerby", selection_result)
    sc_violin_server("jerby", selection_result)
    proportion_server("jerby", selection_result)
    bubheat_server("jerby", selection_result)
    
    
    
  }, error = function(e) {
    showNotification("An error occurred during the process.", type = "error")
    print(e)
    
  })
  remove_modal_progress()
  
}

