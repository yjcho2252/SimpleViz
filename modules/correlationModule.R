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

        # (A) matrix input using rhandsontable
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

        # (B) file upload
        fileInput(
          ns("corr_file"),
          "Upload your TSV file",
          accept = c("text/tab-separated-values", "text/plain", ".tsv", ".txt")
        ),
        hr(),

        # (C) correlation and clustering options
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
        ),

        # (D) color palette
        selectInput(
          ns("color_palette"),
          "Color Palette:",
          choices = c("RdBu", "Blues", "Greens", "Reds", "YlOrRd", "YlGnBu", "heat.colors"),
          selected = "RdBu"
        ),

        # (E) correlation value display
        checkboxInput(ns("show_numbers"), "Display Correlation Values in Cells", value = TRUE),

        # (F) text size and plot size
        sliderInput(ns("fontsize_number"), "Font Size for Numbers:", min = 3, max = 30, value = 10, step = 1),
        sliderInput(ns("font_size"), "Font Size for Labels:", min = 5, max = 20, value = 10, step = 1),
        sliderInput(ns("plot_width"), "Plot Width:", min = 400, max = 1200, value = 700, step = 50),
        sliderInput(ns("plot_height"), "Plot Height:", min = 300, max = 1000, value = 600, step = 50)
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
      output$matrix_table <- rhandsontable::renderRHandsontable({
        table_data <- data.frame(Gene = rownames(exampleData), exampleData, check.names = FALSE)
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

      # (A) handsontable input parsing
      parsed_text_data <- reactiveVal(NULL)

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

      # (B) final input matrix
      raw_mat <- reactive({
        if (!is.null(parsed_text_data())) {
          return(parsed_text_data())
        } else if (!is.null(input$corr_file)) {
          df <- read.table(input$corr_file$datapath, sep = "\t", header = TRUE, check.names = FALSE)
          mat <- as.matrix(df[, -1])
          rownames(mat) <- df[[1]]
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
