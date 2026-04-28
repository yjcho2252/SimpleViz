# modules/volcanoModule.R

# 0. example dataset
set.seed(123)
n_genes <- 1000
log2FC <- seq(-5, 5, length.out = n_genes)
pvalues <- 10^-(abs(log2FC)^1.5 + rnorm(n_genes, mean = 0, sd = 0.5))
example_volcano_data <- data.frame(
  gene = paste0("Gene_", 1:n_genes),
  log2FoldChange = log2FC,
  pvalue = pvalues,
  padj = p.adjust(pvalues, method = "BH")
)

# 1. UI
volcanoUI <- function(id) {
  ns <- NS(id)

  tagList(
    tags$head(
      tags$style(HTML("
              .volcano-panel .form-group,
              .volcano-panel .shiny-input-container {
                margin-bottom: 10px;
              }
              .volcano-panel .form-control,
              .volcano-panel .form-select,
              .volcano-panel .btn,
              .volcano-panel .selectize-input {
                border-radius: 10px;
                font-size: 12px;
              }
              .volcano-panel .control-label,
              .volcano-panel .form-label,
              .volcano-panel .shiny-input-container label {
                font-size: 12px;
                font-weight: 600;
                color: #24445d;
                margin-bottom: 4px;
              }
              .volcano-panel .irs-grid-text,
              .volcano-panel .selectize-dropdown,
              .volcano-panel .selectize-dropdown-content,
              .volcano-panel .selectize-dropdown .option {
                font-size: 12px;
              }
              .volcano-section {
                background: rgba(255, 255, 255, 0.8);
                border: 1px solid #dce6ec;
                border-radius: 14px;
                padding: 14px 14px 10px 14px;
                margin-bottom: 12px;
                box-shadow: 0 6px 14px rgba(20, 47, 70, 0.05);
              }
              .volcano-section-title {
                font-family: 'Times New Roman', Georgia, serif;
                font-size: 17px;
                font-weight: 700;
                color: #143149;
                margin: 0 0 10px 0;
                letter-spacing: 0.01em;
              }
              .volcano-section-note {
                font-size: 11px;
                color: #5b7284;
                margin-bottom: 10px;
                line-height: 1.5;
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
              .button-space {
                margin-bottom: 16px;
              }
              .volcano-panel .btn-default,
              .volcano-panel .btn-secondary {
                background: #f4f7fa;
                border-color: #cdd9e1;
                color: #1f3b53;
              }
              .volcano-panel .btn-primary {
                background: #183b56;
                border-color: #183b56;
                color: #ffffff;
              }
              .volcano-panel .btn-primary:hover,
              .volcano-panel .btn-primary:focus,
              .volcano-panel .btn-primary:active {
                background: #214d6f;
                border-color: #214d6f;
                color: #ffffff;
              }
            "))
    ),
    sidebarLayout(
      sidebarPanel(
        div(
          class = "volcano-panel",
          div(
            class = "volcano-section",
            tags$h4("Data Input", class = "volcano-section-title"),
            div("Paste or upload a differential expression table with gene labels, fold change, and p-value columns.", class = "volcano-section-note"),
            tags$label("Paste your volcano data (tab-separated):", class = "matrix-label"),
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
            fileInput(ns("volcano_file"), "Upload your TSV file",
                      accept = c("text/tab-separated-values",
                                 "text/plain", ".tsv", ".txt"))
          ),
          div(
            class = "volcano-section",
            tags$h4("Column Mapping", class = "volcano-section-title"),
            selectInput(ns("x_col"), "Select log2 Fold Change column", ""),
            selectInput(ns("y_col"), "Select p-value column", ""),
            selectInput(ns("label_col"), "Select label column", "")
          ),
          div(
            class = "volcano-section",
            tags$h4("Thresholds And Labels", class = "volcano-section-title"),
            numericInput(ns("pCutoff"), "p-value cutoff", value = 0.05, min = 0, max = 1, step = 0.01),
            numericInput(ns("FCcutoff"), "Fold change cutoff", value = 1, min = 0, step = 0.01),
            textInput(ns("highlight_genes"), "Highlight genes (comma-separated)", "Gene_300,Gene_301")
          ),
          div(
            class = "volcano-section",
            tags$h4("Appearance", class = "volcano-section-title"),
            numericInput(ns("x_min"), "X-axis minimum:", value = -5, min = -1000, max = 1000, step = 0.5),
            numericInput(ns("x_max"), "X-axis maximum:", value = 5, min = -1000, max = 1000, step = 0.5),
            numericInput(ns("y_min"), "Y-axis minimum:", value = 0, min = 0, max = 1000, step = 1),
            numericInput(ns("y_max"), "Y-axis maximum:", value = 15, min = 0, max = 1000, step = 1),
            numericInput(ns("point_size"), "Data point size:", value = 1, min = 0.1, max = 5, step = 0.1),
            numericInput(ns("plot_width"), "Plot width (pixels):", value = 700, min = 400, max = 2000),
            numericInput(ns("plot_height"), "Plot height (pixels):", value = 600, min = 300, max = 2000),
            tags$details(
              tags$summary("Manually edit category colors"),
              colourInput(ns("col_ns"), "Not significant", value = "grey"),
              colourInput(ns("col_log2fc"), "FC only", value = "red"),
              colourInput(ns("col_p"), "P only", value = "blue"),
              colourInput(ns("col_both"), "Both", value = "green")
            )
          )
        )
      ),
      mainPanel(
        plotOutput(ns("volcano_plot"))
      )
    )
  )
}

# 2. Server
volcanoServer <- function(id, exampleData = example_volcano_data) {
  moduleServer(
    id,
    function(input, output, session) {
      parsed_table_data <- reactiveVal(NULL)
      table_input_data <- reactiveVal(NULL)
      default_volcano_colors <- c(
        "Not significant" = "grey",
        "FC only" = "red",
        "P only" = "blue",
        "Both" = "green"
      )
      display_signif_digits <- 4

      build_table_input <- function(df, format_example = FALSE) {
        format_for_display <- function(col, col_name) {
          if (is.numeric(col)) {
            if (!isTRUE(format_example)) {
              formatted <- as.character(col)
            } else {
              formatted <- if (identical(col_name, "log2FoldChange")) {
              formatC(col, format = "f", digits = 2)
            } else if (col_name %in% c("pvalue", "padj")) {
              formatC(col, format = "e", digits = 2)
            } else {
              format(signif(col, display_signif_digits), scientific = FALSE, trim = TRUE)
            }
            }
            formatted[is.na(col)] <- ""
            return(formatted)
          }
          as.character(col)
        }

        df_char <- as.data.frame(
          Map(format_for_display, df, names(df)),
          stringsAsFactors = FALSE,
          check.names = FALSE
        )
        table_matrix <- rbind(colnames(df_char), as.matrix(df_char))
        table_df <- as.data.frame(table_matrix, stringsAsFactors = FALSE, check.names = FALSE)
        names(table_df) <- paste0("V", seq_len(ncol(table_df)))
        table_df
      }

      normalize_volcano_data <- function(df) {
        normalized_df <- as.data.frame(df, stringsAsFactors = FALSE, check.names = FALSE)
        validate(need(ncol(normalized_df) >= 3, "Please provide at least gene, fold change, and p-value columns."))

        numeric_candidates <- setdiff(names(normalized_df), names(normalized_df)[1])
        for (col in numeric_candidates) {
          normalized_df[[col]] <- suppressWarnings(as.numeric(as.character(normalized_df[[col]])))
        }

        normalized_df
      }

      parse_table_input <- function(table_df) {
        parsed_df <- as.data.frame(table_df, stringsAsFactors = FALSE, check.names = FALSE)
        parsed_df[] <- lapply(parsed_df, function(col) trimws(as.character(col)))

        non_empty_rows <- apply(parsed_df, 1, function(row) any(!is.na(row) & row != ""))
        non_empty_cols <- apply(parsed_df, 2, function(col) any(!is.na(col) & col != ""))
        parsed_df <- parsed_df[non_empty_rows, non_empty_cols, drop = FALSE]

        validate(need(nrow(parsed_df) >= 2, "Please paste a header row and at least one data row."))
        validate(need(ncol(parsed_df) >= 3, "Please provide at least three columns."))

        headers <- as.character(unlist(parsed_df[1, ], use.names = FALSE))
        headers[is.na(headers) | headers == ""] <- paste0("Column", seq_along(headers))[is.na(headers) | headers == ""]
        headers <- make.unique(headers, sep = "_")

        value_df <- parsed_df[-1, , drop = FALSE]
        names(value_df) <- headers
        validate(need(nrow(value_df) > 0, "Please provide at least one data row below the headers."))

        normalize_volcano_data(value_df)
      }

      apply_volcano_data <- function(df) {
        normalized_df <- normalize_volcano_data(df)
        parsed_table_data(normalized_df)
        table_input_data(build_table_input(normalized_df))
      }

      table_input_data(build_table_input(exampleData, format_example = TRUE))

      output$matrix_table <- rhandsontable::renderRHandsontable({
        table_data <- table_input_data()
        req(table_data)

        rhandsontable::rhandsontable(
          table_data,
          colHeaders = FALSE,
          rowHeaders = NULL,
          height = 260,
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
          apply_volcano_data(parse_table_input(df))
        }, error = function(e) {
          showNotification(paste("Error reading table data:", e$message), type = "error")
        })
      })

      volcano_data <- reactive({
        if (!is.null(parsed_table_data())) {
          return(parsed_table_data())
        } else if (is.null(input$volcano_file)) {
          return(exampleData)
        } else {
          df <- read.delim(input$volcano_file$datapath, sep = "\t", header = TRUE, stringsAsFactors = FALSE, check.names = FALSE)
          normalized_df <- normalize_volcano_data(df)
          apply_volcano_data(normalized_df)
          normalized_df
        }
      })

      current_volcano_colors <- reactive({
        colors <- default_volcano_colors
        colors["Not significant"] <- input$col_ns
        colors["FC only"] <- input$col_log2fc
        colors["P only"] <- input$col_p
        colors["Both"] <- input$col_both
        unname(colors[c("Not significant", "FC only", "P only", "Both")])
      })

      axis_limits <- reactive({
        data <- volcano_data()
        y_source_col <- if ("padj" %in% names(data)) "padj" else if ("pvalue" %in% names(data)) "pvalue" else names(data)[3]
        x_source_col <- if ("log2FoldChange" %in% names(data)) "log2FoldChange" else names(data)[2]

        zero_present <- any(data[[y_source_col]] == 0, na.rm = TRUE)
        if (zero_present) {
          y_max <- 310
        } else {
          valid_y <- data[[y_source_col]]
          y_max <- -log10(max(valid_y[valid_y > 0], na.rm = TRUE))
        }

        x_min <- min(data[[x_source_col]], na.rm = TRUE)
        x_max <- max(data[[x_source_col]], na.rm = TRUE)
        list(y_max = y_max, x_min = x_min, x_max = x_max)
      })

      observe({
        data <- volcano_data()
        limits <- axis_limits()
        col_names <- names(data)
        numeric_cols <- col_names[sapply(data, is.numeric)]
        label_default <- if ("gene" %in% col_names) "gene" else col_names[1]
        x_default <- if ("log2FoldChange" %in% numeric_cols) "log2FoldChange" else numeric_cols[1]
        y_default <- if ("padj" %in% numeric_cols) "padj" else if ("pvalue" %in% numeric_cols) "pvalue" else numeric_cols[min(2, length(numeric_cols))]

        updateSelectInput(session, "x_col", choices = numeric_cols, selected = x_default)
        updateSelectInput(session, "y_col", choices = numeric_cols, selected = y_default)
        updateSelectInput(session, "label_col", choices = col_names, selected = label_default)

        updateNumericInput(session, "y_min", value = 0, min = 0, max = max(1000, ceiling(limits$y_max) + 50))
        updateNumericInput(session, "y_max", value = max(15, ceiling(limits$y_max)), min = 0, max = max(1000, ceiling(limits$y_max) + 50))
        updateNumericInput(session, "x_min", value = floor(limits$x_min), min = min(-1000, floor(limits$x_min) - 50), max = max(1000, ceiling(limits$x_max) + 50))
        updateNumericInput(session, "x_max", value = ceiling(limits$x_max), min = min(-1000, floor(limits$x_min) - 50), max = max(1000, ceiling(limits$x_max) + 50))
      })

      output$volcano_plot <- renderPlot({
        req(input$x_col, input$y_col, input$label_col)

        highlight_genes <- unlist(strsplit(input$highlight_genes, ","))
        highlight_genes <- trimws(highlight_genes)

        EnhancedVolcano(
          volcano_data(),
          lab = volcano_data()[[input$label_col]],
          x = input$x_col,
          y = input$y_col,
          pCutoff = input$pCutoff,
          FCcutoff = input$FCcutoff,
          pointSize = input$point_size,
          labSize = 4.0,
          title = "Volcano Plot",
          subtitle = "",
          caption = "",
          legendLabels = c("Not significant", "log2FC", "p-value", "p-value and log2FC"),
          xlim = c(input$x_min, input$x_max),
          ylim = c(input$y_min, input$y_max),
          colAlpha = 1,
          col = current_volcano_colors(),
          selectLab = highlight_genes,
          drawConnectors = TRUE,
          boxedLabels = TRUE
        ) +
          labs(colour = "Significance") +
          scale_y_continuous(limits = c(input$y_min, input$y_max), expand = expansion(mult = c(0, 0))) +
          theme(panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                legend.position = "right",
                legend.title = element_text(size = 12, face = "plain"),
                legend.text = element_text(size = 11),
                legend.key.height = unit(0.55, "cm"),
                legend.key.width = unit(0.55, "cm"),
                legend.spacing.y = unit(0.12, "cm"),
                legend.margin = ggplot2::margin(4, 4, 4, 4),
                legend.box.margin = ggplot2::margin(2, 2, 2, 6)) +
          guides(
            colour = guide_legend(
              override.aes = list(size = 4, alpha = 1),
              byrow = TRUE
            )
          )
      }, width = function() input$plot_width, height = function() input$plot_height)

      output$download_example <- downloadHandler(
        filename = function() {
          "example_volcano_data.tsv"
        },
        content = function(file) {
          write.table(exampleData, file, sep = "\t", row.names = FALSE, quote = FALSE)
        }
      )
    }
  )
}
