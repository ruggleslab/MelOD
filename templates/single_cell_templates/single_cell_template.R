source("global.R", local = TRUE)


inputs_ui <- function(id) {
  ns <- NS(id)
  fluidRow(
    box(
      title = "Inputs", status = "warning", solidHeader = TRUE,
      collapsible = TRUE, collapsed = FALSE,
      width = 12,
      column(7,
             selectInput(ns("cell_subset"), "Cell information to subset:",
                             choices = NULL) %>%
               helper(colour = "#FFA812", type = "inline", size = "m", fade = TRUE,icon = "question",
                      title = "Subseting cells",
                      content = c("You can select the type of cells you want to subset for downstream analysis."
                      )),
             br(),
             withSpinner(uiOutput(ns("cell_subset_choices")), type = 6, color = "#FFA812", size = 0.5),
             br(),br(),
             actionButton(ns("cell_subset_all"), "Select all groups", class = "btn btn-primary"),
             actionButton(ns("cell_subset_none"), "Deselect all groups", class = "btn btn-primary")),
      column(1,""),
      column(
        4, selectInput(ns("cell_plot_clustered_X_axis"), "X-axis:", choices = NULL),
        selectInput(ns("cell_plot_clustered_Y_axis"), "Y-axis:", choices = NULL),
     

        sliderTextInput(
          inputId = ns("marker_size"),
          label = "Point size:", 
          choices = c(
            HTML("<span style='font-size: 10px;'>Small</span>"), 
            HTML("<span style='font-size: 13px;'>Medium</span>"), 
            HTML("<span style='font-size: 16px;'>Big</span>")
          ),
          selected = HTML("<span style='font-size: 13px;'>Medium</span>")
        ) )
      
    )
  )
}


cell_datatable_ui <- function(id) {
  
  ns <- NS(id)
  fluidRow(
    box(
      width = 12,
      solidHeader = TRUE,collapsible = TRUE,
      title = HTML(paste("Cell DataTable", actionLink(ns("info_cell_databale"), label = "", icon = icon("info-circle")))), status = "info", 
      DT::dataTableOutput(ns("cell_datatable")),
      column(2, selectizeInput(ns("inpsplt"), "Split if continious data", choices = c("Quartile","Decile"),  selected = NULL)
)
    )
  )
}

comparison_ui <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    box(
      title = HTML(paste("Cell & Gene info", actionLink(ns("info_comparison_plot"), label = "", icon = icon("info-circle")), downloadButton(ns("gene_plot_clustered_data"), icon = icon("save-file", lib = "glyphicon")))),
      status = "primary", solidHeader = TRUE,collapsible = TRUE,
      width = 12,
      withSpinner(plotlyOutput(ns("gene_plot_clustered"), height = '700px'), type = 6, color = "#FFA812", size = 0.5),
      
      fluidRow(
        column(2, pickerInput(
          inputId = ns("choice_comparison_cell_gene"),
          label = "Choose comparison:",
          choices = c("Cell Vs Cell", "Cell Vs Gene", "Gene Vs Gene"),
          selected = "Cell Vs Gene",
          options = list(style = "btn-primary")
        )),
        
        conditionalPanel(
          condition = paste0("input['", ns("choice_comparison_cell_gene"), "'] == 'Cell Vs Gene' || input['", ns("choice_comparison_cell_gene"), "'] == 'Cell Vs Cell'"),
          column(3, selectInput(ns("cell_plot_clustered_info"), "Cell information:", choices = NULL),
                 selectizeInput(ns("cell_plot_clustered_color"), "Cell Colour (Continuous data):", choices = c("White-Red", "Default" = "Blue-Yellow-Red", "Yellow-Green-Purple"), selected = "Blue-Yellow-Red")%>%
                   helper(colour = "#FFA812",icon = "question", type = "inline", size = "m", fade = TRUE,
                          title = "Cell information to colour cells by",
                          content = c("Select cell information to colour cells",
                                      "- Categorical covariates have a fixed colour palette",
                                      paste0("- Continuous covariates are coloured in a ",
                                             "Blue-Yellow-Red colour scheme, which can be ",
                                             "changed in the plot controls")))),
          
          conditionalPanel(
            condition = paste0("input['", ns("choice_comparison_cell_gene"), "'] == 'Cell Vs Cell'"),
          column(3,
            selectInput(ns("cell_plot_clustered_info_2"), "Cell information 2:", choices = NULL),
            selectizeInput(ns("cell_plot_clustered_color_2"), "Colour 2 (Continuous data):", choices = c("White-Red", "Default" = "Blue-Yellow-Red", "Yellow-Green-Purple"), selected = "Blue-Yellow-Red")
          )),
                 
            column(1, br(), awesomeCheckbox(
            inputId = ns("cell_plot_clustered_label"),
            label = "Show cell info labels",
            value = TRUE
          ))
        ),
        
        conditionalPanel(
          condition = paste0("input['", ns("choice_comparison_cell_gene"), "'] == 'Cell Vs Gene' || input['", ns("choice_comparison_cell_gene"), "'] == 'Gene Vs Gene'"),
          column(3, selectizeInput(ns("gene_plot_clustered_selection"), "Gene name:", choices = NULL, selected = NULL, multiple = FALSE, options = list(maxItems = 1)),
                 selectizeInput(ns("gene_plot_clustered_color"), "Gene Colour (Continuous data):", choices = c("Default" = "White-Red", "Blue-Yellow-Red", "Yellow-Green-Purple"), selected = "White-Red")%>%
                   helper(colour = "#FFA812",type = "inline", size = "m", fade = TRUE, icon = "question",
                          title = "Gene expression to colour cells by",
                          content = c("Select gene to colour cells by gene expression",
                                      paste0("- Gene expression are coloured in a ",
                                             "White-Red colour scheme which can be ",
                                             "changed in the plot controls")
                          )
                   )
                 ),
          conditionalPanel(
                   condition = paste0("input['", ns("choice_comparison_cell_gene"), "'] == 'Gene Vs Gene'"),                   
                   column(3, selectizeInput(ns("gene_plot_clustered_selection_2"), "Gene name 2:", choices = NULL, selected = NULL, multiple = FALSE, options = list(maxItems = 1)))
                 ),
          
        )
      )
    )
  )
}



gene_coexpression_ui <- function(id) {
  ns <- NS(id)
  fluidRow(
    box(
      title = HTML(paste("Gene Coexpression", actionLink(ns("info_coexpression_plot"), label = "", icon = icon("info-circle")), downloadButton(ns("coexpressed_gene_plot_clustered_data"), icon = icon("save-file", lib = "glyphicon")))),
      status = "primary", solidHeader = TRUE,collapsible = TRUE,
      width = 12,
      withSpinner(plotlyOutput(ns("gene_plot_coexpression"), height = '700px'), type = 6, color = "#FFA812", size = 0.5),
      fluidRow(
        column(4,selectizeInput(ns("gene_plot_coexpression_selection"), "Gene 1 name:", choices = NULL, selected = NULL, multiple = FALSE, options = list(maxItems = 1))),
        column(4,selectizeInput(ns("gene_plot_coexpression_selection_2"), "Gene 2 name:", choices = NULL, selected = NULL, multiple = FALSE, options = list(maxItems = 1))),
        column(4, selectizeInput(ns("gene_plot_coexpression_color"), "Colour:", 
                               choices = c("Red (Gene1); Blue (Gene2)", 
                                           "Orange (Gene1); Blue (Gene2)", 
                                           "Red (Gene1); Green (Gene2)", 
                                           "Green (Gene1); Blue (Gene2)"), 
                               selected = "Red (Gene1); Blue (Gene2)"))
      )
    )
  )
}


coexpression_gene_datatable_ui <- function(id) {

  ns <- NS(id)
  fluidRow(
    box(
      width = 12,
      solidHeader = TRUE,collapsible = TRUE,
      title = HTML(paste("Coexpression gene DataTable", actionLink(ns("info_coexpression_databale"), label = "", icon = icon("info-circle")))), status = "info", 
      DT::dataTableOutput(ns("gene_datatable_coexpression"))
    )
  )
}


sc_violin_ui <- function(id) {
  ns <- NS(id)
  fluidRow(
    box(
      title = HTML(paste("Violin & Boxplot", actionLink(ns("info_sc_violin_plot"), label = "", icon = icon("info-circle")), downloadButton(ns("sc_violin_boxplot_data"), icon = icon("save-file", lib = "glyphicon")))),
      status = "primary", solidHeader = TRUE,collapsible = TRUE,
      width = 12,
      withSpinner(plotlyOutput(ns("sc_violin_plot"), height = '700px'), type = 6, color = "#FFA812", size = 0.5),
      fluidRow(
        column(3,selectizeInput(ns("violin_plot_X"), "Cell information (X-axis):", choices = NULL, selected = NULL, multiple = FALSE, options = list(maxItems = 1))),
        
        column(3, pickerInput(
            inputId = ns("choice_comparison_violin"),
            label = "Choose Y axis:",
            choices = c("Cell info", "Gene"),
            selected = "Cell info",
            options = list(style = "btn-primary")
          ),
        
        conditionalPanel(
          condition = paste0("input['", ns("choice_comparison_violin"), "'] == 'Cell info'"),
          selectizeInput(ns("violin_plot_Y_cell"), "Cell Info (Y-axis):", choices = NULL, selected = NULL, multiple = FALSE, options = list(maxItems = 1)),
          
        ),
        
        conditionalPanel(
          condition = paste0("input['", ns("choice_comparison_violin"), "'] == 'Gene'"),
          selectizeInput(ns("violin_plot_Y_gene"), "Gene name (Y-axis):", choices = NULL, selected = NULL, multiple = FALSE, options = list(maxItems = 1)),
          
        )),
        
    
        column(3,
               radioGroupButtons(
                 inputId = ns("sc_box_or_violin"),
                 label = "Type of graph :", 
                 choices = c(`<i class='violin_logo'></i>` = "violin", `<i class='boxplot_logo'></i>` = "Boxplot"),
                 selected="violin",
                 justified = TRUE)),
        column(4,selectizeInput(ns("sc_violon_dot"), "Dot:", choices = c("Outliers "= "outliers", "All" = "all"), selected = "outliers", multiple = FALSE, options = list(maxItems = 1)))
)
      )
    )
  
}


proportion_ui <- function(id) {
  ns <- NS(id)
  fluidRow(
    box(
      title = HTML(paste("Proportion plot", actionLink(ns("info_proportion_plot"), label = "", icon = icon("info-circle")), downloadButton(ns("proportion_data"), icon = icon("save-file", lib = "glyphicon")))),
      status = "primary", solidHeader = TRUE,collapsible = TRUE,
      width = 12,
      withSpinner(plotlyOutput(ns("proportion_plot"), height = '700px'), type = 6, color = "#FFA812", size = 0.5),
      fluidRow(
        column(3,selectizeInput(ns("proportion_plot_X"), "Cell information to plot (X-axis):", choices = NULL, selected = NULL, multiple = FALSE, options = list(maxItems = 1))),
        column(4,selectizeInput(ns("proportion_group_by"), "Cell information to group / colour by:", choices = NULL, selected = NULL, multiple = FALSE, options = list(maxItems = 1))),
        column(3,selectizeInput(ns("proportion_type"), "Plot value", choices = c("Proportion" = "Proportion", "Cell Number" ="cell_number"), selected = "Proportion", multiple = FALSE, options = list(maxItems = 1))),
        column(2, br(),awesomeCheckbox(
          inputId = ns("proportion_flip_axis"),
          label = "Flip axis", 
          value = FALSE,
        ))
      )
    )
  )
}


bubheat_ui <- function(id) {
  ns <- NS(id)
  fluidRow(
    box(
      title = HTML(paste("Heatmap & Bubble plot", actionLink(ns("info_bubheat_plot"), label = "", icon = icon("info-circle")), downloadButton(ns("bubheat_data"), icon = icon("save-file", lib = "glyphicon")))),
      status = "primary", solidHeader = TRUE,collapsible = TRUE,
      width = 12,
      withSpinner(uiOutput(ns("bubheat_plot")), type = 6, color = "#FFA812", size = 0.5),
      fluidRow(
        
        column(3,multiInput(
          inputId = ns("bubheat_selected_gene"),
          label = "Genes selection (up to 50):",
          autocomplete = TRUE,
          option= list(limit=50),
          choices = "Loading...")),
        column(3,
               
          textAreaInput(ns("bubheat_selected_gene_text"), HTML("List of gene names (Max 50 genes, separated 
                                           by , or ; or newline):
                                          "), 
                             height = "200px") %>% 
                 helper(colour = "#FFA812",type = "inline", size = "m", fade = TRUE,icon = "question",
                        title = "List of genes to plot on bubbleplot / heatmap", 
                        content = c("Input genes to plot", 
                                    "- Maximum 50 genes (due to ploting space limitations)", 
                                    "- Genes should be separated by comma, semicolon or newline",
                                    "- Don't forget to click on Send list")),
          actionButton(ns("select_genes_single_cell"), "Send list", class = "btn btn-primary"),
          actionButton(ns("reset_selection_single_cell"), "Reset", class = "btn btn-primary")),
        column(1,""),
        column(1,
               selectizeInput(ns("bubheat_group_by"), "Group by:", choices = NULL, selected = NULL, multiple = FALSE, options = list(maxItems = 1)),
               selectizeInput(ns("bubheat_color"), "Colour:", choices = c("Default"="White-Red","Blue-Yellow-Red","Yellow-Green-Purple"),  selected = "White-Red")),
        column(2,
               radioGroupButtons(
                 inputId = ns("bubheat_type"),
                 label = "Type of graph :", 
                 choices = c(`<i class='bubbleplot_logo'></i>` = "Bubbleplot", `<i class='heatmap_logo'></i>` = "Heatmap"),
                 selected="Bubbleplot",
                 justified = TRUE)),
               
        column(2,
              br(),
              br(),
               awesomeCheckbox(
                 inputId = ns("bubheat_scale"),
                 label = "Scale gene expression", 
                 value = TRUE,
               ),
               awesomeCheckbox(
                 inputId = ns("bubheat_cluster_rows"),
                 label = "Cluster rows (genes)", 
                 value = TRUE,
               ),
               awesomeCheckbox(
                 inputId = ns("bubheat_cluster_columns"),
                 label = "Cluster columns (samples)", 
                 value = FALSE,
               )
        )
    )
  )
  )
}

