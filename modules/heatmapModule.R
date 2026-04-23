# modules/heatmapModule.R

# 0. example dataset

set.seed(123)  

genes   <- paste0("Gene", 1:5)
samples <- paste0("Sample", 1:3)

mat_data <- matrix(round(runif(5 * 3, min = 0, max = 10), 2), nrow = 5, ncol = 3)
rownames(mat_data) <- genes
colnames(mat_data) <- samples

# 1. UI
heatmapUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    tags$head(
      tags$style(HTML("
              .button-space {
                margin-bottom: 20px;
              }
              .matrix-label {
                display: block;
                margin-bottom: 8px;
              }
              .matrix-table-space {
                margin-bottom: 12px;
              }
              .col-sm-4 {
                position: sticky;
                top: 60px;
                height: calc(100vh - 60px);
                overflow-y: auto;
              }
              .col-sm-8 {
                height: calc(100vh - 60px);
                overflow-y: auto;
              }
            "))
    ),
    sidebarLayout(
      sidebarPanel(
        position = "left",
        
        # (1) Matrix input using rhandsontable
        tags$label("Paste your matrix data (tab-separated):", class = "matrix-label"),
        div(
          class = "matrix-table-space",
          rhandsontable::rHandsontableOutput(ns("matrix_table"))
        ),
        
        div(class = "button-space",
            fluidRow(
              column(5, 
                     actionButton(ns("submit"), "Submit Data")),
              column(6, 
                     downloadButton(ns("download_example"), "Example Data"))
            )
        ),
        
        # (2,3) File upload
        fileInput(ns("heatmap_file"), "Upload your TSV file",
                  accept = c("text/tab-separated-values", 
                             "text/plain", ".tsv", ".txt")),
        hr(),
        
        # (4) Heatmap parameter settings
        selectInput(ns("scale_option"), "Scale:", 
                    choices = c("none", "row", "column"), 
                    selected = "none"),
        checkboxInput(ns("cluster_rows"), "Cluster Rows", value = TRUE),
        checkboxInput(ns("cluster_cols"), "Cluster Columns", value = TRUE),
        selectInput(ns("dist_method"), "Distance Method:",
                    choices = c("euclidean", "manhattan", "maximum", 
                                "canberra", "binary", "minkowski"),
                    selected = "euclidean"),
        selectInput(ns("hclust_method"), "Clustering Method:",
                    choices = c("complete", "ward.D", "ward.D2", 
                                "single", "average", "mcquitty", 
                                "median", "centroid"),
                    selected = "complete"),
        
        selectInput(ns("color_palette"), "Color Palette:", 
                    choices = c("RdBu", "Blues", "Greens", "Reds", 
                                "YlOrRd", "YlGnBu", "heat.colors"), 
                    selected = "RdBu"),
#        sliderInput(ns("num_colors"), "Number of Colors:", 
#                    min = 3, max = 100, value = 9),
        
        # **Font size settings (added)**
        sliderInput(ns("font_size"), "Font Size:",
                    min = 5, max = 20, value = 10, step = 1),
        
        # (5) Plot size settings
        sliderInput(ns("plot_width"), "Plot Width:", 
                    min = 400, max = 1200, value = 700, step = 50),
        sliderInput(ns("plot_height"), "Plot Height:", 
                    min = 300, max = 1000, value = 600, step = 50)
      ),
      mainPanel(
        plotOutput(ns("heatmap_plot"), width = "100%", height = "auto")
      )
    )
  )
}

# 2. Server

heatmapServer <- function(id, exampleHeatmapData = mat_data, exampleAnnotation = NULL) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      output$matrix_table <- rhandsontable::renderRHandsontable({
        table_data <- data.frame(Gene = rownames(exampleHeatmapData), exampleHeatmapData, check.names = FALSE)
        rownames(table_data) <- NULL

        rhandsontable::rhandsontable(
          table_data,
          rowHeaders = NULL,
          height = 300,
          useTypes = FALSE,
          readOnly = FALSE
        ) %>%
          rhandsontable::hot_table(minCols = 2, minRows = 1) %>%
          rhandsontable::hot_context_menu(allowRowEdit = TRUE, allowColEdit = TRUE)
      })

      # (A) reactiveVal to store data entered by user in handsontable
      parsed_text_data <- reactiveVal(NULL)
      
      # (B) Parse and store data from table when Submit Data button is clicked
      observeEvent(input$submit, {
        req(input$matrix_table)

        tryCatch({
          df <- rhandsontable::hot_to_r(input$matrix_table)
          validate(need(!is.null(df) && ncol(df) >= 2, "Please provide at least one ID column and one numeric column."))

          gene_ids <- as.character(df[[1]])
          missing_idx <- is.na(gene_ids) | trimws(gene_ids) == ""
          gene_ids[missing_idx] <- paste0("Row", which(missing_idx))

          value_df <- df[, -1, drop = FALSE]
          value_df[] <- lapply(value_df, function(col) suppressWarnings(as.numeric(as.character(col))))

          mat <- as.matrix(value_df)
          rownames(mat) <- gene_ids

          validate(need(any(!is.na(mat)), "Please enter at least one numeric value in the matrix."))
          parsed_text_data(mat)
        }, error = function(e) {
          showNotification(paste("Error reading table data:", e$message), type = "error")
        })
      })
      
      
      # (C) reactive that returns data to be used in heatmap
      heatmap_data <- reactive({
        # 1. Use parsed data from table input if available
        if(!is.null(parsed_text_data())) {
          return(parsed_text_data())
        }
        # 2. If file is uploaded
        else if (!is.null(input$heatmap_file)) {
          df <- read.delim(
            input$heatmap_file$datapath, 
            sep = "\t", 
            header = TRUE, 
            check.names = FALSE
          )
          mat <- as.matrix(df[,-1])
          rownames(mat) <- df[[1]]
          return(mat)
        }
        # 3. Otherwise use example data
        else {
          return(exampleHeatmapData)
        }
      })
      
      # (D) Create Heatmap Plot
      output$heatmap_plot <- renderPlot({
        req(heatmap_data())
        mat <- heatmap_data()
        
        # Create color palette
        pal_name <- input$color_palette
        pal_size <- 100
        
        # Set palette based on whether it's RColorBrewer
        if (pal_name %in% rownames(brewer.pal.info)) {
          # RColorBrewer palette
          colors <- colorRampPalette(brewer.pal(min(pal_size, 9), pal_name))(pal_size)
        } else {
          # base R palettes like heat.colors
          if (pal_name == "heat.colors") {
            colors <- heat.colors(pal_size)
          } else {
            # Other exception handling (or fixed RdBu etc.)
            colors <- colorRampPalette(brewer.pal(9, "RdBu"))(pal_size)
          }
        }
        
        # annotation_col (optional)
        annotation_col <- exampleAnnotation
        if (!is.null(exampleAnnotation) && nrow(exampleAnnotation) == ncol(mat)) {
          rownames(annotation_col) <- colnames(mat)
        } else {
          annotation_col <- NA
        }
        
        # Run pheatmap
        pheatmap(
          mat,
          scale = input$scale_option,               
          cluster_rows = input$cluster_rows,
          cluster_cols = input$cluster_cols,
          color = colors,
          annotation_col = if (is.data.frame(annotation_col)) annotation_col else NULL,
          clustering_distance_rows = input$dist_method,
          clustering_distance_cols = input$dist_method,
          clustering_method = input$hclust_method,
          legend = TRUE,
          border_color = "grey80",
          main = "Heatmap",
          fontsize = input$font_size
        )
      }, width = function() input$plot_width, height = function() input$plot_height)
      
      
      # (E) Example data download
      output$download_example <- downloadHandler(
        filename = function() {
          "example_heatmap_data.tsv"
        },
        content = function(file) {
          mat <- exampleHeatmapData
          df_out <- data.frame(rownames(mat), mat, check.names = FALSE)
          colnames(df_out)[1] <- "Gene"
          write.table(df_out, file, sep = "\t", row.names = FALSE, quote = FALSE)
        }
      )
    }
  )
}
