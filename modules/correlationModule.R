# modules/correlationModule.R

# 0. example dataset
set.seed(123)
genes   <- paste0("Gene", 1:10)
samples <- paste0("Sample", 1:5)

# example data rounded to 2 decimals
mat_data_example <- matrix(
  round(runif(10 * 5, min = 0, max = 15), 2),
  nrow = 10,
  ncol = 5
)
rownames(mat_data_example) <- genes
colnames(mat_data_example) <- samples

# 1. UI
correlationUI <- function(id) {
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
              .corr-panel .form-group,
              .corr-panel .shiny-input-container {
                margin-bottom: 10px;
              }
              .corr-panel .form-control,
              .corr-panel .form-select,
              .corr-panel .btn,
              .corr-panel .selectize-input {
                border-radius: 10px;
                font-size: 12px;
              }
              .corr-panel .control-label,
              .corr-panel .form-label,
              .corr-panel .shiny-input-container label {
                font-size: 12px;
                font-weight: 600;
                color: #24445d;
                margin-bottom: 4px;
              }
              .corr-panel .irs-grid-text,
              .corr-panel .selectize-dropdown,
              .corr-panel .selectize-dropdown-content,
              .corr-panel .selectize-dropdown .option {
                font-size: 12px;
              }
              .corr-section {
                background: rgba(255, 255, 255, 0.8);
                border: 1px solid #dce6ec;
                border-radius: 14px;
                padding: 14px 14px 10px 14px;
                margin-bottom: 12px;
                box-shadow: 0 6px 14px rgba(20, 47, 70, 0.05);
              }
              .corr-section-title {
                font-family: 'Times New Roman', Georgia, serif;
                font-size: 17px;
                font-weight: 700;
                color: #143149;
                margin: 0 0 10px 0;
                letter-spacing: 0.01em;
              }
              .corr-section-note {
                font-size: 11px;
                color: #5b7284;
                margin-bottom: 10px;
                line-height: 1.5;
              }
              .corr-panel .btn-default,
              .corr-panel .btn-secondary {
                background: #f4f7fa;
                border-color: #cdd9e1;
                color: #1f3b53;
              }
              .corr-panel .btn-primary {
                background: #183b56;
                border-color: #183b56;
              }
              .corr-panel .btn:hover {
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
          class = "corr-panel",
          div(
            class = "corr-section",
            tags$h4("Data Input", class = "corr-section-title"),
            div("Paste a tab-separated matrix into the table or upload a TSV file.", class = "corr-section-note"),
            tags$label("Paste your matrix data (tab-separated):", class = "matrix-label"),
            div(
              class = "matrix-table-space",
              rhandsontable::rHandsontableOutput(ns("matrix_table"))
            ),
            div(
              class = "button-space",
              fluidRow(
                column(5, actionButton(ns("submit"), "Submit Data")),
                column(6, downloadButton(ns("download_example"), "Example Data"))
              )
            ),
            fileInput(
              ns("corr_file"),
              "Upload your TSV file",
              accept = c("text/tab-separated-values", "text/plain", ".tsv", ".txt")
            )
          ),
          div(
            class = "corr-section",
            tags$h4("Correlation Settings", class = "corr-section-title"),
            selectInput(
              ns("corrMethod"),
              "Correlation Method",
              choices = c("pearson", "spearman", "kendall"),
              selected = "pearson"
            ),
            selectInput(
              ns("distMethod"),
              "Distance for Clustering",
              choices = c("1 - correlation", "euclidean", "manhattan"),
              selected = "1 - correlation"
            ),
            selectInput(
              ns("hclustMethod"),
              "Clustering Method",
              choices = c("complete", "ward.D", "ward.D2", "single", "average", "mcquitty", "median", "centroid"),
              selected = "complete"
            )
          ),
          div(
            class = "corr-section",
            tags$h4("Appearance", class = "corr-section-title"),
            selectInput(
              ns("color_palette"),
              "Color Palette:",
              choices = c("RdBu", "Blues", "Greens", "Reds", "YlOrRd", "YlGnBu", "heat.colors"),
              selected = "RdBu"
            ),
            checkboxInput(ns("show_numbers"), "Display Correlation Values in Cells", value = TRUE),
            numericInput(ns("fontsize_number"), "Font Size for Numbers:", value = 10, min = 3, max = 30, step = 1),
            numericInput(ns("font_size"), "Font Size for Labels:", value = 10, min = 5, max = 20, step = 1),
            numericInput(ns("plot_width"), "Plot Width:", value = 700, min = 400, max = 1200, step = 50),
            numericInput(ns("plot_height"), "Plot Height:", value = 600, min = 300, max = 1000, step = 50)
          )
        )
      ),
      mainPanel(
        plotOutput(ns("corr_heatmap"), width = "100%", height = "auto")
      )
    )
  )
}

# 2. Server
correlationServer <- function(id, exampleData = mat_data_example) {
  moduleServer(
    id,
    function(input, output, session) {
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

      apply_correlation_matrix <- function(mat) {
        parsed_text_data(mat)
        table_input_data(build_table_input(mat))
      }

      table_input_data(build_table_input(exampleData))

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

      observeEvent(input$submit, {
        req(input$matrix_table)

        tryCatch({
          df <- rhandsontable::hot_to_r(input$matrix_table)
          validate(need(!is.null(df), "Please paste TSV text into the table before submitting."))
          mat <- parse_table_input(df)
          apply_correlation_matrix(mat)
        }, error = function(e) {
          showNotification(paste("Error reading table data:", e$message), type = "error")
        })
      })

      # (B) final input matrix
      raw_mat <- reactive({
        if (!is.null(parsed_text_data())) {
          return(parsed_text_data())
        } else if (!is.null(input$corr_file)) {
          df <- read.table(input$corr_file$datapath, sep = "\t", header = TRUE, check.names = FALSE)
          mat <- as.matrix(df[, -1])
          rownames(mat) <- df[[1]]
          colnames(mat) <- colnames(df)[-1]
          apply_correlation_matrix(mat)
          return(mat)
        } else {
          return(exampleData)
        }
      })

      # (C) correlation matrix -> pheatmap
      output$corr_heatmap <- renderPlot({
        req(raw_mat())
        mat <- raw_mat()

        corr_mat <- cor(mat, method = input$corrMethod, use = "complete.obs")
        corr_mat <- round(corr_mat, 2)

        dist_rows <- NULL
        dist_cols <- NULL
        if (input$distMethod == "1 - correlation") {
          dist_rows <- as.dist(1 - corr_mat)
          dist_cols <- as.dist(1 - corr_mat)
        } else {
          dist_rows <- dist(corr_mat, method = input$distMethod)
          dist_cols <- dist(corr_mat, method = input$distMethod)
        }

        pal_name <- input$color_palette
        pal_size <- 100
        if (pal_name %in% rownames(RColorBrewer::brewer.pal.info)) {
          colors <- colorRampPalette(RColorBrewer::brewer.pal(min(pal_size, 9), pal_name))(pal_size)
        } else if (pal_name == "heat.colors") {
          colors <- heat.colors(pal_size)
        } else {
          colors <- colorRampPalette(RColorBrewer::brewer.pal(9, "RdBu"))(pal_size)
        }

        pheatmap::pheatmap(
          corr_mat,
          color = colors,
          clustering_distance_rows = dist_rows,
          clustering_distance_cols = dist_cols,
          clustering_method = input$hclustMethod,
          legend = TRUE,
          border_color = "grey80",
          main = paste("Correlation Heatmap (", input$corrMethod, ")", sep = ""),
          fontsize = input$font_size,
          display_numbers = if (input$show_numbers) corr_mat else FALSE,
          number_format = "%.2f",
          fontsize_number = input$fontsize_number
        )
      },
      width = function() input$plot_width,
      height = function() input$plot_height)

      # (D) example download
      output$download_example <- downloadHandler(
        filename = function() {
          "example_corr_data.tsv"
        },
        content = function(file) {
          mat <- exampleData
          df_out <- data.frame(rownames(mat), mat, check.names = FALSE)
          colnames(df_out)[1] <- "Gene"
          write.table(df_out, file, sep = "\t", row.names = FALSE, quote = FALSE)
        }
      )
    }
  )
}
