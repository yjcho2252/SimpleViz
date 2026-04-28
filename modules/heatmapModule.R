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
                margin-bottom: 16px;
              }
              .matrix-label {
                display: block;
                margin-bottom: 8px;
                font-weight: 700;
                font-size: 12px;
                letter-spacing: 0.02em;
                color: #183247;
              }
              .matrix-table-space {
                margin-bottom: 12px;
              }
              .heatmap-panel .form-group,
              .heatmap-panel .shiny-input-container {
                margin-bottom: 10px;
              }
              .heatmap-panel .form-control,
              .heatmap-panel .form-select,
              .heatmap-panel .btn,
              .heatmap-panel .selectize-input {
                border-radius: 10px;
                font-size: 12px;
              }
              .heatmap-panel .control-label,
              .heatmap-panel .form-label,
              .heatmap-panel .shiny-input-container label {
                font-size: 12px;
                font-weight: 600;
                color: #24445d;
                margin-bottom: 4px;
              }
              .heatmap-panel .irs-grid-text,
              .heatmap-panel .selectize-dropdown,
              .heatmap-panel .selectize-dropdown-content,
              .heatmap-panel .selectize-dropdown .option {
                font-size: 12px;
              }
              .heatmap-section {
                background: rgba(255, 255, 255, 0.8);
                border: 1px solid #dce6ec;
                border-radius: 14px;
                padding: 14px 14px 10px 14px;
                margin-bottom: 12px;
                box-shadow: 0 6px 14px rgba(20, 47, 70, 0.05);
              }
              .heatmap-section-title {
                font-family: 'Times New Roman', Georgia, serif;
                font-size: 17px;
                font-weight: 700;
                color: #143149;
                margin: 0 0 10px 0;
                letter-spacing: 0.01em;
              }
              .heatmap-section-note {
                font-size: 11px;
                color: #5b7284;
                margin-bottom: 10px;
                line-height: 1.5;
              }
              .heatmap-panel .btn-default,
              .heatmap-panel .btn-secondary {
                background: #f4f7fa;
                border-color: #cdd9e1;
                color: #1f3b53;
              }
              .heatmap-panel .btn-primary {
                background: #183b56;
                border-color: #183b56;
              }
              .heatmap-panel .btn:hover {
                transform: translateY(-1px);
                transition: all 0.15s ease;
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
        div(
          class = "heatmap-panel",
          div(
            class = "heatmap-section",
            tags$h4("Data Input", class = "heatmap-section-title"),
            div("Paste a tab-separated matrix directly into the table or upload a TSV file.", class = "heatmap-section-note"),
            tags$label("Paste your matrix data (tab-separated):", class = "matrix-label"),
            div(
              class = "matrix-table-space",
              rhandsontable::rHandsontableOutput(ns("matrix_table"))
            ),
            div(class = "button-space",
                fluidRow(
                  column(5, actionButton(ns("submit"), "Submit Data")),
                  column(6, downloadButton(ns("download_example"), "Example Data"))
                )
            ),
            fileInput(ns("heatmap_file"), "Upload your TSV file",
                      accept = c("text/tab-separated-values", 
                                 "text/plain", ".tsv", ".txt"))
          ),
          div(
            class = "heatmap-section",
            tags$h4("Clustering", class = "heatmap-section-title"),
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
                        selected = "complete")
          ),
          div(
            class = "heatmap-section",
            tags$h4("Appearance", class = "heatmap-section-title"),
            selectInput(ns("color_palette"), "Color Palette:", 
                        choices = c("RdBu", "Blues", "Greens", "Reds", 
                                    "YlOrRd", "YlGnBu", "heat.colors"), 
                        selected = "RdBu"),
            numericInput(ns("font_size"), "Font Size:", value = 10, min = 5, max = 20, step = 1),
            numericInput(ns("plot_width"), "Plot Width:", value = 700, min = 400, max = 1200, step = 50),
            numericInput(ns("plot_height"), "Plot Height:", value = 600, min = 300, max = 1000, step = 50)
          )
        )
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
      parsed_text_data <- reactiveVal(NULL)
      table_input_data <- reactiveVal(NULL)

      build_table_input <- function(mat) {
        table_df <- data.frame(Gene = rownames(mat), mat, check.names = FALSE, stringsAsFactors = FALSE)
        table_matrix <- rbind(colnames(table_df), as.matrix(table_df))
        table_output <- as.data.frame(table_matrix, stringsAsFactors = FALSE, check.names = FALSE)
        names(table_output) <- paste0("V", seq_len(ncol(table_output)))
        table_output
      }

      parse_table_input <- function(table_df) {
        parsed_df <- as.data.frame(table_df, stringsAsFactors = FALSE, check.names = FALSE)
        parsed_df[] <- lapply(parsed_df, function(col) trimws(as.character(col)))

        non_empty_rows <- apply(parsed_df, 1, function(row) any(!is.na(row) & row != ""))
        non_empty_cols <- apply(parsed_df, 2, function(col) any(!is.na(col) & col != ""))
        parsed_df <- parsed_df[non_empty_rows, non_empty_cols, drop = FALSE]

        validate(need(nrow(parsed_df) >= 2, "Please paste a header row and at least one data row."))
        validate(need(ncol(parsed_df) >= 2, "Please provide at least one ID column and one numeric column."))

        headers <- as.character(unlist(parsed_df[1, ], use.names = FALSE))
        headers[is.na(headers) | headers == ""] <- paste0("Column", seq_along(headers))[is.na(headers) | headers == ""]
        headers <- make.unique(headers, sep = "_")

        value_df <- parsed_df[-1, , drop = FALSE]
        names(value_df) <- headers
        validate(need(nrow(value_df) > 0, "Please provide at least one data row below the headers."))

        gene_ids <- as.character(value_df[[1]])
        missing_idx <- is.na(gene_ids) | trimws(gene_ids) == ""
        gene_ids[missing_idx] <- paste0("Row", which(missing_idx))

        numeric_df <- value_df[, -1, drop = FALSE]
        numeric_df[] <- lapply(numeric_df, function(col) suppressWarnings(as.numeric(as.character(col))))
        mat <- as.matrix(numeric_df)
        rownames(mat) <- gene_ids
        colnames(mat) <- names(numeric_df)

        validate(need(any(!is.na(mat)), "Please enter at least one numeric value in the matrix."))
        mat
      }

      apply_heatmap_matrix <- function(mat) {
        parsed_text_data(mat)
        table_input_data(build_table_input(mat))
      }

      table_input_data(build_table_input(exampleHeatmapData))
      
      output$matrix_table <- rhandsontable::renderRHandsontable({
        table_data <- table_input_data()
        req(table_data)

        rhandsontable::rhandsontable(
          table_data,
          colHeaders = FALSE,
          rowHeaders = NULL,
          height = 300,
          useTypes = FALSE,
          readOnly = FALSE
        ) %>%
          rhandsontable::hot_table(
            minCols = ncol(table_data),
            minRows = nrow(table_data),
            minSpareRows = 0,
            minSpareCols = 0,
            stretchH = "all"
          ) %>%
          rhandsontable::hot_context_menu(allowRowEdit = TRUE, allowColEdit = TRUE)
      })
      
      # (B) Parse and store data from table when Submit Data button is clicked
      observeEvent(input$submit, {
        req(input$matrix_table)

        tryCatch({
          df <- rhandsontable::hot_to_r(input$matrix_table)
          validate(need(!is.null(df), "Please paste TSV text into the table before submitting."))
          mat <- parse_table_input(df)
          apply_heatmap_matrix(mat)
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
          colnames(mat) <- colnames(df)[-1]
          apply_heatmap_matrix(mat)
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
