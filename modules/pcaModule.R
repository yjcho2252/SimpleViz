# modules/pcaModule.R

# 0. example dataset
set.seed(123)
n_samples <- 30  # Reduced from 100
n_features <- 10  # Reduced from 20
n_groups <- 3

# Generate example data
generate_group_data <- function(n, features, mean, sd) {
  matrix(round(rnorm(n * features, mean = mean, sd = sd), 2), nrow = n)
}

group1 <- generate_group_data(10, n_features, mean = 0, sd = 1)    # 10 samples
group2 <- generate_group_data(10, n_features, mean = 2, sd = 1.5)  # 10 samples
group3 <- generate_group_data(10, n_features, mean = -1, sd = 0.5) # 10 samples

data <- rbind(group1, group2, group3)
groups <- rep(paste0("Group", 1:n_groups), each = 10)
colnames(data) <- paste0("Feature", 1:n_features)
sample_names <- paste0("Sample", 1:nrow(data))
example_pca_data <- data.frame(Sample = sample_names, Group = groups, data)

# 1. UI
pcaUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    tags$head(
      tags$style(HTML("
              .pca-panel .form-group,
              .pca-panel .shiny-input-container {
                margin-bottom: 10px;
              }
              .pca-panel .form-control,
              .pca-panel .form-select,
              .pca-panel .btn,
              .pca-panel .selectize-input {
                border-radius: 10px;
                font-size: 12px;
              }
              .pca-panel .control-label,
              .pca-panel .form-label,
              .pca-panel .shiny-input-container label {
                font-size: 12px;
                font-weight: 600;
                color: #24445d;
                margin-bottom: 4px;
              }
              .pca-panel .irs-grid-text,
              .pca-panel .selectize-dropdown,
              .pca-panel .selectize-dropdown-content,
              .pca-panel .selectize-dropdown .option {
                font-size: 12px;
              }
              .pca-section {
                background: rgba(255, 255, 255, 0.8);
                border: 1px solid #dce6ec;
                border-radius: 14px;
                padding: 14px 14px 10px 14px;
                margin-bottom: 12px;
                box-shadow: 0 6px 14px rgba(20, 47, 70, 0.05);
              }
              .pca-section-title {
                font-family: 'Times New Roman', Georgia, serif;
                font-size: 17px;
                font-weight: 700;
                color: #143149;
                margin: 0 0 10px 0;
                letter-spacing: 0.01em;
              }
              .pca-section-note {
                font-size: 11px;
                color: #5b7284;
                margin-bottom: 10px;
                line-height: 1.5;
              }
              .pca-panel .btn-default,
              .pca-panel .btn-secondary {
                background: #f4f7fa;
                border-color: #cdd9e1;
                color: #1f3b53;
              }
              .pca-panel .btn-primary {
                background: #183b56;
                border-color: #183b56;
              }
              .pca-panel .btn:hover {
                transform: translateY(-1px);
                transition: all 0.15s ease;
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
            "))
    ),
    sidebarLayout(
      sidebarPanel(
        div(
          class = "pca-panel",
          div(
            class = "pca-section",
            tags$h4("Data Input", class = "pca-section-title"),
            div("Paste or upload a table with Sample, Group, and numeric feature columns.", class = "pca-section-note"),
            tags$label("Paste your data (tab-separated):", class = "matrix-label"),
            div(
              class = "matrix-table-space",
              rhandsontable::rHandsontableOutput(ns("matrix_table"))
            ),
            div(
              class = "button-space",
              fluidRow(
                column(5, actionButton(ns("submit"), "Submit Data")),
                column(6, downloadButton(ns("downloadPCAData"), "Example Data"))
              )
            ),
            fileInput(ns("pca_file"), "Upload your TSV file",
                      accept = c("text/tab-separated-values", "text/plain", ".tsv", ".txt"))
          ),
          div(
            class = "pca-section",
            tags$h4("Analysis Setup", class = "pca-section-title"),
            radioButtons(ns("analysis_method"), "Analysis method:", choices = c("PCA", "NMDS"), selected = "PCA"),
            selectInput(ns("x_axis"), "X-axis:", choices = paste0("Dim", 1:5), selected = "Dim1"),
            selectInput(ns("y_axis"), "Y-axis:", choices = paste0("Dim", 1:5), selected = "Dim2"),
            sliderInput(ns("x_range"), "X-axis range:", min = -10, max = 10, 
                        value = c(-10, 10), step = 1),
            sliderInput(ns("y_range"), "Y-axis range:", min = -10, max = 10, 
                        value = c(-10, 10), step = 1)
          ),
          div(
            class = "pca-section",
            tags$h4("Appearance", class = "pca-section-title"),
            numericInput(ns("point_size"), "Point Size:", value = 2, min = 1, max = 5, step = 0.5),
            numericInput(ns("axis_font_size"), "Axis Font Size:", value = 12, min = 8, max = 20, step = 1),
            checkboxInput(ns("add_ellipse"), "Add ellipses", value = TRUE),
            selectInput(ns("ellipse_type"), "Ellipse Type:", 
                        choices = c("concentration", "convex"), selected = "concentration"),
            checkboxInput(ns("show_points"), "Show points without text", value = TRUE),
            selectInput(
              ns("palette_name"),
              "Color Palette:",
              choices = c("Set2", "Dark2", "Paired", "Set1", "Pastel1", "Accent"),
              selected = "Set2"
            ),
            numericInput(ns("plot_width"), "Plot Width:", value = 800, min = 400, max = 1200, step = 50),
            numericInput(ns("plot_height"), "Plot Height:", value = 600, min = 300, max = 1000, step = 50)
          ),
          div(
            class = "pca-section",
            tags$h4("Export", class = "pca-section-title"),
            selectInput(ns("export_format"), "Export Format:",
                        choices = c("PNG", "SVG", "PDF"),
                        selected = "PNG"),
            numericInput(ns("dpi"), "DPI (for PNG only):", value = 300, min = 72, max = 600),
            textInput(ns("filename"), "Export Filename:", value = "ordination_plot"),
            downloadButton(ns("save_plot"), "Download Plot")
          )
        )
      ),
      mainPanel(
        plotOutput(ns("pca_plot"), width = "100%", height = "auto"),
        verbatimTextOutput(ns("permanova_result")),
        verbatimTextOutput(ns("pairwise_result"))
      )
    )
  )
}

# 2. Server
pcaServer <- function(id, examplePCAData=example_pca_data) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      current_ord_plot <- reactiveVal(NULL)
      parsed_table_data <- reactiveVal(NULL)
      table_input_data <- reactiveVal(NULL)
      default_palette_name <- "Set2"

      get_palette_values <- function(palette_name, n_groups) {
        if (n_groups <= 0) {
          return(character(0))
        }

        if (palette_name %in% rownames(RColorBrewer::brewer.pal.info)) {
          max_colors <- RColorBrewer::brewer.pal.info[palette_name, "maxcolors"]
          base_colors <- RColorBrewer::brewer.pal(min(max(3, n_groups), max_colors), palette_name)
          return(grDevices::colorRampPalette(base_colors)(n_groups))
        }

        RColorBrewer::brewer.pal(min(max(3, n_groups), 8), default_palette_name)[seq_len(n_groups)]
      }

      build_table_input <- function(df) {
        df_char <- as.data.frame(lapply(df, as.character), stringsAsFactors = FALSE, check.names = FALSE)
        table_matrix <- rbind(colnames(df_char), as.matrix(df_char))
        table_df <- as.data.frame(table_matrix, stringsAsFactors = FALSE, check.names = FALSE)
        names(table_df) <- paste0("V", seq_len(ncol(table_df)))
        table_df
      }

      normalize_pca_data <- function(df) {
        normalized_df <- as.data.frame(df, stringsAsFactors = FALSE, check.names = FALSE)

        if ("Cluster" %in% colnames(normalized_df) && !"Group" %in% colnames(normalized_df)) {
          colnames(normalized_df)[colnames(normalized_df) == "Cluster"] <- "Group"
        }

        validate(need(all(c("Sample", "Group") %in% colnames(normalized_df)), "Data must include 'Sample' and 'Group' columns."))

        normalized_df$Sample <- trimws(as.character(normalized_df$Sample))
        normalized_df$Group <- trimws(as.character(normalized_df$Group))

        sample_missing <- is.na(normalized_df$Sample) | normalized_df$Sample == ""
        normalized_df$Sample[sample_missing] <- paste0("Sample", which(sample_missing))
        group_missing <- is.na(normalized_df$Group) | normalized_df$Group == ""
        normalized_df$Group[group_missing] <- "Group1"

        numeric_cols <- setdiff(colnames(normalized_df), c("Sample", "Group", "Variable"))
        validate(need(length(numeric_cols) > 0, "Please include at least one numeric feature column."))

        for (col in numeric_cols) {
          normalized_df[[col]] <- suppressWarnings(as.numeric(as.character(normalized_df[[col]])))
        }

        keep_numeric_cols <- numeric_cols[colSums(!is.na(normalized_df[numeric_cols])) > 0]
        validate(need(length(keep_numeric_cols) > 0, "Please include at least one numeric feature column with values."))

        normalized_df[, c("Sample", "Group", keep_numeric_cols), drop = FALSE]
      }

      parse_table_input <- function(table_df) {
        parsed_df <- as.data.frame(table_df, stringsAsFactors = FALSE, check.names = FALSE)
        parsed_df[] <- lapply(parsed_df, function(col) trimws(as.character(col)))

        non_empty_rows <- apply(parsed_df, 1, function(row) any(!is.na(row) & row != ""))
        non_empty_cols <- apply(parsed_df, 2, function(col) any(!is.na(col) & col != ""))
        parsed_df <- parsed_df[non_empty_rows, non_empty_cols, drop = FALSE]

        validate(need(nrow(parsed_df) >= 2, "Please paste a header row and at least one data row."))
        validate(need(ncol(parsed_df) >= 3, "Please provide Sample, Group, and at least one feature column."))

        headers <- as.character(unlist(parsed_df[1, ], use.names = FALSE))
        headers[is.na(headers) | headers == ""] <- paste0("Column", seq_along(headers))[is.na(headers) | headers == ""]
        headers <- make.unique(headers, sep = "_")

        value_df <- parsed_df[-1, , drop = FALSE]
        names(value_df) <- headers
        validate(need(nrow(value_df) > 0, "Please provide at least one data row below the headers."))

        normalize_pca_data(value_df)
      }

      apply_pca_data <- function(df) {
        normalized_df <- normalize_pca_data(df)
        parsed_table_data(normalized_df)
        table_input_data(build_table_input(normalized_df))
      }

      table_input_data(build_table_input(examplePCAData))

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
          apply_pca_data(parse_table_input(df))
        }, error = function(e) {
          showNotification(paste("Error reading table data:", e$message), type = "error")
        })
      })
      
      # Upload / Example data selection
      pca_dataset <- reactive({
        if (!is.null(parsed_table_data())) {
          return(parsed_table_data())
        } else if (is.null(input$pca_file)) {
          return(examplePCAData)
        } else {
          df <- read.delim(input$pca_file$datapath, sep = "\t", header = TRUE, check.names = FALSE)
          normalized_df <- normalize_pca_data(df)
          apply_pca_data(normalized_df)
          return(normalized_df)
        }
      })

      selected_palette <- reactive({
        df <- pca_dataset()
        if (!"Group" %in% colnames(df)) return(NULL)

        groups <- unique(df$Group)
        palette_name <- input$palette_name %||% default_palette_name
        setNames(get_palette_values(palette_name, length(groups)), groups)
      })
      
      # Analysis result (PCA or NMDS)
      analysis_result <- reactive({
        df <- pca_dataset()
        numeric_cols <- setdiff(colnames(df), c("Sample", "Group", "Variable"))
        X <- df[, numeric_cols, drop = FALSE]
        X <- as.data.frame(lapply(X, as.numeric))
        #X[X < 0] <- 0  # Set all negative values to 0 for all analyses
        X <- X[, colSums(X) > 0, drop = FALSE]  # Remove OTUs (columns) with total sum 0
        if (input$analysis_method == "PCA") {
          res <- PCA(X, scale.unit = TRUE, graph = FALSE)
          return(list(type = "PCA", result = res, X = X))
        } else {
          res <- tryCatch({
            metaMDS(X, distance = "bray", k = 2, trymax = 300)
          }, error = function(e) NULL)
          if (is.null(res) || is.null(res$points)) {
            return(list(type = "NMDS", result = NULL, X = X))
          } else {
            return(list(type = "NMDS", result = res, X = X))
          }
        }
      })
      
      # Dynamically update axis ranges when file is uploaded/axis is changed
      observe({
        res <- analysis_result()  # Use analysis_result() to get PCA/NMDS results
        if (is.null(res)) return()

        if (res$type == "PCA") {
          coords <- res$result$ind$coord
        } else if (res$type == "NMDS" && !is.null(res$result) && !is.null(res$result$points)) {
          coords <- res$result$points
        } else {
          return()
        }

        x_axis <- which(paste0("Dim", 1:10) == input$x_axis)
        y_axis <- which(paste0("Dim", 1:10) == input$y_axis)
        if (ncol(coords) < max(x_axis, y_axis)) return()

        x_range <- range(coords[, x_axis])
        y_range <- range(coords[, y_axis])

        # Increase padding for wider range
        x_padding <- diff(x_range) * 0.3
        y_padding <- diff(y_range) * 0.4
        x_range_initial <- x_range + c(-x_padding, x_padding)
        y_range_initial <- y_range + c(-y_padding, y_padding)

        # Set appropriate slider ranges with wider limits
        x_min <- floor(x_range_initial[1])
        x_max <- ceiling(x_range_initial[2])
        y_min <- floor(y_range_initial[1])
        y_max <- ceiling(y_range_initial[2])

        slider_x_min <- x_min - 5
        slider_x_max <- x_max + 5
        slider_x_value <- c(max(x_min, slider_x_min), min(x_max, slider_x_max))

        slider_y_min <- y_min - 5
        slider_y_max <- y_max + 5
        slider_y_value <- c(max(y_min, slider_y_min), min(y_max, slider_y_max))

        updateSliderInput(session, "x_range", 
                          min = slider_x_min, max = slider_x_max, 
                          value = slider_x_value)
        updateSliderInput(session, "y_range", 
                          min = slider_y_min, max = slider_y_max, 
                          value = slider_y_value)
      })
      
      # PCA/NMDS Plot
      output$pca_plot <- renderPlot({
        df <- pca_dataset()
        res <- analysis_result()
        if (is.null(res) || !"Group" %in% colnames(df)) return()
        if (res$type == "PCA") {
          x_axis <- which(paste0("Dim", 1:10) == input$x_axis)
          y_axis <- which(paste0("Dim", 1:10) == input$y_axis)
          explained_var <- res$result$eig[, 2]
          x_label <- if (length(explained_var) >= x_axis) {
            paste0(input$x_axis, " (", sprintf("%.1f", explained_var[x_axis]), "%)")
          } else {
            input$x_axis
          }
          y_label <- if (length(explained_var) >= y_axis) {
            paste0(input$y_axis, " (", sprintf("%.1f", explained_var[y_axis]), "%)")
          } else {
            input$y_axis
          }
          p <- fviz_pca_ind(
            res$result,
            title = "Principal Component Analysis",
            repel = TRUE,
            axes = c(x_axis, y_axis),
            geom.ind = if (input$show_points) "point" else c("point", "text"),
            col.ind = df$Group,
            palette = unname(selected_palette()[unique(df$Group)]),
            addEllipses = input$add_ellipse,
            ellipse.level = 0.9,
            legend.title = "Group",
            mean.point = FALSE,
            pointsize = input$point_size
          )
          if (input$add_ellipse && input$ellipse_type == "convex") {
            p$layers[[2]]$aes_params$linetype <- 2
          }
          p <- p +
            labs(
              x = x_label,
              y = y_label
            ) +
            theme(axis.line = element_line(color = "black"),
                  panel.border = element_blank(),
                  panel.background = element_blank(),
                  axis.text = element_text(size = input$axis_font_size),
                  axis.title = element_text(size = input$axis_font_size + 2, face = "plain"),
                  plot.title = element_text(hjust = 0.5, size = input$axis_font_size + 5, face = "plain"),
                  legend.title = element_text(size = input$axis_font_size + 1, face = "plain"),
                  legend.text = element_text(size = input$axis_font_size),
                  legend.position = "right") +
            scale_x_continuous(limits = input$x_range) +
            scale_y_continuous(limits = input$y_range)
          current_ord_plot(p)
          p
        } else {
          # NMDS plot
          if (is.null(res$result) || is.null(res$result$points)) {
            plot.new(); text(0.5, 0.5, "NMDS failed: Check data structure.", cex = 1.5); return()
          }
          nmds_points <- as.data.frame(res$result$points)
          colnames(nmds_points) <- c("NMDS1", "NMDS2")
          nmds_points$Group <- df$Group
          p <- ggplot(nmds_points, aes(NMDS1, NMDS2, color = Group)) +
            geom_point(size = input$point_size) +
            scale_color_manual(values = selected_palette()) +
            labs(
              title = "Non-metric Multidimensional Scaling",
              x = "NMDS1",
              y = "NMDS2",
              color = "Group"
            ) +
            theme_minimal(base_size = input$axis_font_size) +
            theme(
              plot.title = element_text(hjust = 0.5, size = input$axis_font_size + 5, face = "plain"),
              axis.title = element_text(size = input$axis_font_size + 2, face = "plain"),
              axis.text = element_text(size = input$axis_font_size),
              legend.title = element_text(size = input$axis_font_size + 1, face = "plain"),
              legend.text = element_text(size = input$axis_font_size),
              panel.grid.minor = element_blank(),
              legend.position = "right"
            )
          current_ord_plot(p)
          p
        }
      }, width = function() input$plot_width, height = function() input$plot_height)
      
      # PERMANOVA result
      output$permanova_result <- renderPrint({
        res <- analysis_result()
        df <- pca_dataset()
        if (is.null(res) || !"Group" %in% colnames(df)) {
          cat("No valid data for PERMANOVA.\n")
          return()
        }
        if (res$type == "NMDS" && (is.null(res$result) || is.null(res$result$points))) {
          cat("NMDS failed: Check data structure.\n"); return()
        }
        coords <- res$X
        if (nrow(coords) != nrow(df)) {
          cat("Data size mismatch. Cannot run PERMANOVA.\n")
          return()
        }
        permanova_result <- adonis2(coords ~ Group, data = df, permutations = 999)
        cat("PERMANOVA Results:\n")
        print(permanova_result)
        cat("\nOverall P-value:", permanova_result$`Pr(>F)`[1], "\n")
      })
      
      # Pairwise PERMANOVA
      output$pairwise_result <- renderPrint({
        res <- analysis_result()
        df <- pca_dataset()
        if (is.null(res) || !"Group" %in% colnames(df)) {
          cat("No valid data for Pairwise PERMANOVA.\n")
          return()
        }
        if (res$type == "NMDS" && (is.null(res$result) || is.null(res$result$points))) {
          cat("NMDS failed: Check data structure.\n"); return()
        }
        coords <- res$X
        if (nrow(coords) != nrow(df)) {
          cat("Data size mismatch. Cannot run pairwiseAdonis.\n")
          return()
        }
        pairwise_result <- pairwise.adonis(coords, df$Group, p.adjust.m = "bonferroni")
        cat("Pairwise PERMANOVA Results:\n")
        print(pairwise_result)
      })
      
      # Example data download
      output$downloadPCAData <- downloadHandler(
        filename = function() {
          "example_pca_data.tsv"
        },
        content = function(file) {
          # Use the embedded example data directly
          write.table(example_pca_data, file, row.names = FALSE, sep = "\t", quote = FALSE)
        }
      )

      output$save_plot <- downloadHandler(
        filename = function() {
          paste0(input$filename, ".", tolower(input$export_format))
        },
        content = function(file) {
          tryCatch(
            {
              if (is.null(current_ord_plot())) {
                stop("No plot available to save")
              }

              p <- current_ord_plot()
              width_inches <- input$plot_width / 72
              height_inches <- input$plot_height / 72

              if (input$export_format == "PNG") {
                png_scale <- as.numeric(input$dpi) / 96
                png_plot <- p +
                  theme(
                    plot.title = element_text(size = (input$axis_font_size + 5) * png_scale, face = "plain", hjust = 0.5),
                    axis.title = element_text(size = (input$axis_font_size + 2) * png_scale, face = "plain"),
                    axis.text = element_text(size = input$axis_font_size * png_scale),
                    legend.title = element_text(size = (input$axis_font_size + 1) * png_scale, face = "plain"),
                    legend.text = element_text(size = input$axis_font_size * png_scale)
                  )
                ggsave(file,
                       plot = png_plot,
                       device = "png",
                       width = input$plot_width / 96,
                       height = input$plot_height / 96,
                       units = "in",
                       dpi = as.numeric(input$dpi),
                       bg = "white")
              } else if (input$export_format == "SVG") {
                ggsave(file,
                       plot = p,
                       device = "svg",
                       width = width_inches,
                       height = height_inches,
                       bg = "white")
              } else if (input$export_format == "PDF") {
                ggsave(file,
                       plot = p,
                       device = "pdf",
                       width = width_inches,
                       height = height_inches,
                       bg = "white")
              }
            },
            error = function(e) {
              showNotification(paste("Error saving plot:", e$message), type = "error")
            }
          )
        }
      )
      
    }
  )
}
