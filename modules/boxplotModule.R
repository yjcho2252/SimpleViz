# modules/boxplotModule.R

# 0. example dataset
boxplot_default_data <- data.frame(
  dose = factor(c(0.5, 0.5, 0.5, 1, 1, 1, 2, 2, 2, 0.5, 0.5, 0.5, 1, 1, 1, 2, 2, 2)),
  len = c(4.2, 11.5, 7.3, 16.5, 16.5, 15.2, 19.7, 23.3, 23.6, 15.2, 21.5, 17.6, 22.4, 25.8, 19.7, 28.5, 33.9, 30.9),
  supp = factor(c("VC", "VC", "VC", "VC", "VC", "VC", "VC", "VC", "VC", "OJ", "OJ", "OJ", "OJ", "OJ", "OJ", "OJ", "OJ", "OJ"))
)

# 1. UI
boxplotUI <- function(id) {
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
              .boxplot-panel .form-group,
              .boxplot-panel .shiny-input-container {
                margin-bottom: 10px;
              }
              .boxplot-panel .form-control,
              .boxplot-panel .form-select,
              .boxplot-panel .btn,
              .boxplot-panel .selectize-input {
                border-radius: 10px;
                font-size: 12px;
              }
              .boxplot-panel .control-label,
              .boxplot-panel .form-label,
              .boxplot-panel .shiny-input-container label {
                font-size: 12px;
                font-weight: 600;
                color: #24445d;
                margin-bottom: 4px;
              }
              .boxplot-panel .irs-grid-text,
              .boxplot-panel .selectize-dropdown,
              .boxplot-panel .selectize-dropdown-content,
              .boxplot-panel .selectize-dropdown .option {
                font-size: 12px;
              }
              .boxplot-section {
                background: rgba(255, 255, 255, 0.8);
                border: 1px solid #dce6ec;
                border-radius: 14px;
                padding: 14px 14px 10px 14px;
                margin-bottom: 12px;
                box-shadow: 0 6px 14px rgba(20, 47, 70, 0.05);
              }
              .boxplot-section-title {
                font-family: 'Times New Roman', Georgia, serif;
                font-size: 17px;
                font-weight: 700;
                color: #143149;
                margin: 0 0 10px 0;
                letter-spacing: 0.01em;
              }
              .boxplot-section-note {
                font-size: 11px;
                color: #5b7284;
                margin-bottom: 10px;
                line-height: 1.5;
              }
              .boxplot-panel .btn-default,
              .boxplot-panel .btn-secondary {
                background: #f4f7fa;
                border-color: #cdd9e1;
                color: #1f3b53;
              }
              .boxplot-panel .btn-primary {
                background: #183b56;
                border-color: #183b56;
              }
              .boxplot-panel .btn:hover {
                transform: translateY(-1px);
                transition: all 0.15s ease;
              }
              .boxplot-panel .hr-soft {
                height: 1px;
                background: linear-gradient(90deg, rgba(24, 59, 86, 0), rgba(24, 59, 86, 0.22), rgba(24, 59, 86, 0));
                border: 0;
                margin: 8px 0 2px 0;
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
      position = "left",
      sidebarPanel(
        div(
          class = "boxplot-panel",
          div(
            class = "boxplot-section",
            tags$h4("Data Input", class = "boxplot-section-title"),
            div("Paste tab-separated data directly into the table or load a TSV file.", class = "boxplot-section-note"),
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
              ns("upload_tsv"),
              "Upload your TSV file",
              accept = c("text/tab-separated-values", "text/plain", ".tsv", ".txt")
            )
          ),
          div(
            class = "boxplot-section",
            tags$h4("Plot Mapping", class = "boxplot-section-title"),
            selectInput(ns("plot_type"), "Select Plot Type:",
              choices = c("Box Plot", "Violin Plot", "Dot Plot", "Bar Plot"),
              selected = "Box Plot"
            ),
            selectInput(ns("x_var"), "Select X-axis Variable:", choices = names(boxplot_default_data), selected = "dose"),
            selectInput(ns("y_var"), "Select Y-axis Variable:", choices = names(boxplot_default_data), selected = "len"),
            selectInput(ns("facet_var"), "Select Facet Variable (Optional):",
              choices = c("None", names(boxplot_default_data)),
              selected = "None"
            ),
            uiOutput(ns("x_order_input"))
          ),
          div(
            class = "boxplot-section",
            tags$h4("Labels And Stats", class = "boxplot-section-title"),
            numericInput(ns("ymin"), "Y-axis minimum:", value = 0),
            numericInput(ns("ymax"), "Y-axis maximum:", value = 50),
            textInput(ns("xlab"), "X-axis Label:", value = "Dose"),
            textInput(ns("ylab"), "Y-axis Label:", value = "Length"),
            selectInput(ns("stat_method"), "Statistical Method:",
              choices = c(
                "t-test" = "t.test",
                "ANOVA" = "anova",
                "Kruskal-Wallis" = "kruskal.test"
              ),
              selected = "t.test"
            )
          ),
          div(
            class = "boxplot-section",
            tags$h4("Style Controls", class = "boxplot-section-title"),
            selectInput(
              ns("palette_name"),
              "Color Palette:",
              choices = c("Set2", "Dark2", "Paired", "Set1", "Pastel1", "Blues", "Greens", "RdPu", "YlGnBu"),
              selected = "Set2"
            ),
            tags$hr(class = "hr-soft"),
            numericInput(ns("pointSize"), "Data Point Size:", value = 2, min = 0, max = 5, step = 0.1),
            numericInput(ns("barWidth"), "Bar Width:", value = 0.7, min = 0.1, max = 1, step = 0.05),
            numericInput(ns("lineThickness"), "Box/Violin Line Thickness:", value = 1.0, min = 0, max = 2, step = 0.05),
            numericInput(ns("fontSize"), "Font Size:", value = 12, min = 6, max = 24, step = 1),
            numericInput(ns("plotWidth"), "Plot Width (pixels):", value = 400, min = 200, max = 2000, step = 10),
            numericInput(ns("plotHeight"), "Plot Height (pixels):", value = 600, min = 200, max = 1500, step = 10)
          ),
          div(
            class = "boxplot-section",
            tags$h4("Export", class = "boxplot-section-title"),
            selectInput(ns("export_format"), "Export Format:",
              choices = c("PNG", "SVG", "PDF"),
              selected = "PNG"
            ),
            numericInput(ns("dpi"), "DPI (for PNG only):", value = 300, min = 72, max = 600),
            textInput(ns("filename"), "Export Filename:", value = "boxplot"),
            downloadButton(ns("save_plot"), "Download Plot")
          )
        )
      ),
      mainPanel(
        uiOutput(ns("dynamic_output"))
      )
    )
  )
}

# 2. Server
boxplotServer <- function(id, default_data = boxplot_default_data) {
  moduleServer(id, function(input, output, session) {
    current_box_plot <- reactiveVal()
    data <- reactiveVal(default_data)
    color_palette <- reactiveVal(NULL)
    table_input_data <- reactiveVal(NULL)

    default_colors <- c("#F8766D", "#00BA38", "#619CFF", "#F564E3", "#00BFC4", "#B79F00")
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

      rep(default_colors, length.out = n_groups)
    }

    update_group_palette <- function(group_names, palette_name = NULL) {
      selected_palette <- palette_name %||% input$palette_name %||% default_palette_name
      palette_vals <- get_palette_values(selected_palette, length(group_names))
      color_palette(setNames(palette_vals, group_names))
    }

    build_table_input <- function(df) {
      df_char <- as.data.frame(lapply(df, function(col) {
        if (is.factor(col)) as.character(col) else as.character(col)
      }), stringsAsFactors = FALSE, check.names = FALSE)

      base_matrix <- rbind(colnames(df_char), as.matrix(df_char))
      table_df <- as.data.frame(base_matrix, stringsAsFactors = FALSE, check.names = FALSE)
      names(table_df) <- paste0("V", seq_len(ncol(table_df)))
      table_df
    }

    parse_table_input <- function(table_df) {
      parsed_df <- as.data.frame(table_df, stringsAsFactors = FALSE, check.names = FALSE)
      parsed_df[] <- lapply(parsed_df, function(col) trimws(as.character(col)))

      non_empty_rows <- apply(parsed_df, 1, function(row) any(!is.na(row) & row != ""))
      non_empty_cols <- apply(parsed_df, 2, function(col) any(!is.na(col) & col != ""))

      parsed_df <- parsed_df[non_empty_rows, non_empty_cols, drop = FALSE]

      validate(need(nrow(parsed_df) >= 2, "Please paste a header row and at least one data row."))
      validate(need(ncol(parsed_df) >= 2, "Please provide at least two columns."))

      headers <- as.character(unlist(parsed_df[1, ], use.names = FALSE))
      headers[is.na(headers) | headers == ""] <- paste0("Column", seq_along(headers))[is.na(headers) | headers == ""]
      headers <- make.unique(headers, sep = "_")

      value_df <- parsed_df[-1, , drop = FALSE]
      names(value_df) <- headers
      validate(need(nrow(value_df) > 0, "Please provide at least one data row below the headers."))

      value_df
    }

    normalize_boxplot_data <- function(df) {
      normalized_df <- df

      for (col in names(normalized_df)) {
        if (is.character(normalized_df[[col]])) {
          trimmed <- trimws(normalized_df[[col]])
          non_empty <- trimmed != "" & !is.na(trimmed)
          num_col <- suppressWarnings(as.numeric(trimmed))
          if (any(non_empty) && all(!is.na(num_col[non_empty]))) {
            normalized_df[[col]] <- num_col
          }
        }
      }

      normalized_df
    }

    apply_boxplot_data <- function(new_data, update_table = TRUE) {
      normalized_data <- normalize_boxplot_data(new_data)
      data(normalized_data)
      if (isTRUE(update_table)) {
        table_input_data(build_table_input(normalized_data))
      }

      col_names <- names(normalized_data)
      x_col <- col_names[1]
      numeric_cols <- col_names[sapply(normalized_data, is.numeric)]
      numeric_y_candidates <- setdiff(numeric_cols, x_col)
      y_col <- if (length(numeric_y_candidates) > 0) {
        numeric_y_candidates[1]
      } else if (length(numeric_cols) > 0) {
        numeric_cols[1]
      } else {
        col_names[min(2, length(col_names))]
      }

      updateSelectInput(session, "x_var", choices = col_names, selected = x_col)
      updateSelectInput(session, "y_var", choices = col_names, selected = y_col)
      updateSelectInput(session, "facet_var", choices = c("None", col_names), selected = "None")
      updateTextInput(session, "xlab", value = x_col)
      updateTextInput(session, "ylab", value = y_col)

      if (is.numeric(normalized_data[[y_col]])) {
        y_values <- normalized_data[[y_col]]
        y_min_raw <- min(y_values, na.rm = TRUE)
        y_max_raw <- max(y_values, na.rm = TRUE)

        if (is.finite(y_min_raw) && is.finite(y_max_raw)) {
          y_range <- y_max_raw - y_min_raw
          y_pad <- max(1, y_range * 0.1)
          updateNumericInput(session, "ymin", value = floor(y_min_raw - y_pad))
          updateNumericInput(session, "ymax", value = ceiling(y_max_raw + y_pad))
        }
      }

      groups <- levels(as.factor(normalized_data[[x_col]]))
      update_group_palette(groups)
    }

    table_input_data(build_table_input(default_data))

    output$dynamic_output <- renderUI({
      tagList(
        plotOutput(session$ns("plot"), width = "100%", height = paste0(input$plotHeight, "px")),
        verbatimTextOutput(session$ns("ttest_results"))
      )
    })

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

    # Box plot: Initialize color palette
    observe({
      req(data())
      groups <- levels(as.factor(data()[[input$x_var]]))
      if (is.null(color_palette())) {
        update_group_palette(groups, default_palette_name)
      }
    })

    # Box plot: Create download handler for example file
    output$download_example <- downloadHandler(
      filename = function() {
        "boxplot_example_data.txt"
      },
      content = function(file) {
        write.table(boxplot_default_data, file, sep = "\t", row.names = FALSE, quote = FALSE)
      }
    )

    # **Important modification**: Added fill=TRUE and NA removal option
    observeEvent(input$submit, {
      req(input$matrix_table)
      tryCatch(
        {
          matrix_data <- rhandsontable::hot_to_r(input$matrix_table)
          validate(need(!is.null(matrix_data), "Please paste TSV text into the table before submitting."))
          parsed_data <- parse_table_input(matrix_data)
          apply_boxplot_data(parsed_data)
        },
        error = function(e) {
          showNotification(paste("Error reading data:", e$message), type = "error")
        }
      )
    })

    observeEvent(input$upload_tsv, {
      req(input$upload_tsv$datapath)
      tryCatch(
        {
          uploaded_data <- read.delim(
            input$upload_tsv$datapath,
            header = TRUE,
            sep = "\t",
            stringsAsFactors = FALSE,
            check.names = FALSE
          )

          validate(need(nrow(uploaded_data) > 0, "Uploaded file has no data rows."))
          validate(need(ncol(uploaded_data) > 1, "Uploaded file must have at least two columns."))
          apply_boxplot_data(uploaded_data)
          showNotification("TSV file uploaded successfully.", type = "message")
        },
        error = function(e) {
          showNotification(paste("Error uploading TSV:", e$message), type = "error")
        }
      )
    })

    observeEvent(input$x_var, {
      updateTextInput(session, "xlab", value = input$x_var)
    })

    observeEvent(input$y_var, {
      updateTextInput(session, "ylab", value = input$y_var)
    })

    output$x_order_input <- renderUI({
      req(data(), input$x_var)
      x_levels <- levels(as.factor(data()[[input$x_var]]))
      tagList(
        tags$b("X-axis Order:"),
        sortable::rank_list(
          text = "Drag to reorder",
          labels = x_levels,
          input_id = session$ns("x_order")
        )
      )
    })

    observeEvent(input$palette_name, {
      req(data(), input$x_var)
      groups <- levels(as.factor(data()[[input$x_var]]))
      update_group_palette(groups, input$palette_name)
    })

    plot_data <- reactive({
      req(data(), input$x_var, input$y_var, input$plot_type, input$x_order)

      data_plot <- data()
      data_plot[[input$x_var]] <- factor(data_plot[[input$x_var]], levels = input$x_order)
      y_numeric <- suppressWarnings(as.numeric(as.character(data_plot[[input$y_var]])))
      validate(need(any(!is.na(y_numeric)), "Selected Y-axis variable must be numeric. Please choose a numeric column."))
      data_plot[[input$y_var]] <- y_numeric

      groups <- levels(data_plot[[input$x_var]])

      current_palette <- color_palette()
      if (is.null(current_palette) || !all(groups %in% names(current_palette))) {
        update_group_palette(groups)
        current_palette <- color_palette()
      }

      list(
        data = data_plot,
        x_var = input$x_var,
        y_var = input$y_var,
        facet_var = input$facet_var,
        plot_type = input$plot_type,
        groups = groups,
        colors = color_palette() %||% setNames(get_palette_values(input$palette_name %||% default_palette_name, length(groups)), groups)
      )
    })

    build_boxplot <- function(text_scale = 1) {
      plot_info <- plot_data()

      p <- ggplot(plot_info$data, aes_string(x = plot_info$x_var, y = plot_info$y_var, group = plot_info$x_var)) +
        labs(
          title = paste(plot_info$plot_type),
          x = input$xlab, y = input$ylab
        ) +
        theme_pubr(base_size = input$fontSize) +
        theme(
          plot.title = element_text(hjust = 0.5, size = (input$fontSize * 1.3) * text_scale),
          axis.title = element_text(size = (input$fontSize * 1.2) * text_scale),
          axis.text = element_text(size = input$fontSize * text_scale),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(size = 1, color = "black"),
          axis.ticks = element_line(size = 1, color = "black"),
          legend.title = element_text(size = input$fontSize * text_scale),
          legend.text = element_text(size = (input$fontSize * 0.8) * text_scale)
        ) +
        scale_x_discrete(limits = plot_info$groups) +
        scale_fill_manual(values = color_palette(), name = input$xlab) +
        scale_y_continuous(expand = expansion(mult = c(0, 0))) +
        coord_cartesian(ylim = c(input$ymin, input$ymax))

      if (plot_info$plot_type == "Box Plot") {
        p <- p + geom_boxplot(aes(fill = .data[[plot_info$x_var]]),
          width = input$barWidth,
          size = input$lineThickness,
          outlier.shape = NA
        )
      } else if (plot_info$plot_type == "Violin Plot") {
        p <- p + geom_violin(aes(fill = .data[[plot_info$x_var]]),
          width = input$barWidth,
          size = input$lineThickness,
          trim = FALSE
        )
      } else if (plot_info$plot_type == "Dot Plot") {
        p <- p + geom_dotplot(aes(fill = .data[[plot_info$x_var]]),
          binaxis = "y",
          stackdir = "center",
          dotsize = input$pointSize * 0.4,
          binwidth = (input$ymax - input$ymin) / 50
        )
      } else if (plot_info$plot_type == "Bar Plot") {
        p <- p + stat_summary(
          aes(fill = .data[[plot_info$x_var]]),
          fun = mean,
          geom = "bar",
          width = input$barWidth,
          color = "black",
          linewidth = input$lineThickness
        ) +
          stat_summary(
            fun.data = mean_se,
            geom = "errorbar",
            width = input$barWidth * 0.3,
            linewidth = input$lineThickness
          )
      }

      if (plot_info$plot_type %in% c("Box Plot", "Violin Plot", "Bar Plot")) {
        p <- p + geom_jitter(color = "black", width = 0.2, size = input$pointSize, alpha = 0.7)
      }

      p <- p + scale_fill_manual(values = plot_info$colors, name = input$xlab)

      if (length(plot_info$groups) >= 2) {
        formula <- as.formula(paste(plot_info$y_var, "~", plot_info$x_var))
        stat_label_size <- (input$fontSize / ggplot2::.pt) * text_scale

        if (input$stat_method == "t.test") {
          comparisons <- combn(plot_info$groups, 2, simplify = FALSE)

          y_max <- max(plot_info$data[[plot_info$y_var]])
          step <- (input$ymax - y_max) / (length(comparisons) + 1)
          y_positions <- seq(y_max + step, by = step, length.out = length(comparisons))

          p <- p + stat_compare_means(
            comparisons = comparisons,
            label = "p.signif",
            method = "t.test",
            label.y = y_positions,
            size = stat_label_size,
            textsize = stat_label_size
          )
        } else if (input$stat_method %in% c("anova", "kruskal.test")) {
          p <- p + stat_compare_means(
            label.y = max(plot_info$data[[plot_info$y_var]]) * 1.4,
            method = input$stat_method,
            size = stat_label_size,
            textsize = stat_label_size
          )
        }
      }

      if (plot_info$facet_var != "None") {
        p <- p + facet_wrap(as.formula(paste("~", plot_info$facet_var)))
      }

      p
    }

    output$plot <- renderPlot(
      {
        p <- build_boxplot(1)
        current_box_plot(p)
        p
      },
      width = function() input$plotWidth,
      height = function() input$plotHeight
    )

    output$ttest_results <- renderPrint({
      req(data(), input$x_var, input$y_var, input$stat_method)
      data_test <- data()
      data_test[[input$x_var]] <- as.factor(data_test[[input$x_var]])
      data_test[[input$y_var]] <- as.numeric(as.character(data_test[[input$y_var]]))
      groups <- levels(data_test[[input$x_var]])

      if (length(groups) >= 2) {
        formula <- as.formula(paste(input$y_var, "~", input$x_var))

        if (input$stat_method == "t.test") {
          stat_test <- compare_means(formula, data = data_test, method = "t.test")

          cat("t-test Results (p-values):\n")
          for (i in 1:nrow(stat_test)) {
            cat(paste(
              stat_test$group1[i], "vs", stat_test$group2[i], ":",
              format(stat_test$p[i], scientific = TRUE, digits = 4), "\n"
            ))
          }
        } else if (input$stat_method == "anova") {
          anova_result <- aov(formula, data = data_test)
          cat("ANOVA Results:\n")
          print(summary(anova_result))

          tukey_result <- TukeyHSD(anova_result)
          cat("\nTukey's HSD Pairwise Comparisons:\n")
          print(tukey_result[[1]])
        } else if (input$stat_method == "kruskal.test") {
          kruskal_result <- kruskal.test(formula, data = data_test)
          cat("Kruskal-Wallis Test Results:\n")
          print(kruskal_result)

          if (!requireNamespace("dunn.test", quietly = TRUE)) {
            install.packages("dunn.test")
          }
          library(dunn.test)
          dunn_result <- dunn.test(data_test[[input$y_var]], data_test[[input$x_var]], method = "bonferroni")
          cat("\nDunn's Test Pairwise Comparisons:\n")
          print(dunn_result)
        }
      } else {
        cat("Not enough groups to perform statistical tests.")
      }
    })

    output$save_plot <- downloadHandler(
      filename = function() {
        paste0(input$filename, ".", tolower(input$export_format))
      },
      content = function(file) {
        tryCatch(
          {
            if (is.null(current_box_plot())) {
              stop("No plot available to save")
            }

            # Ensure the plot is properly rendered
            p <- current_box_plot()

            # Convert dimensions to inches (1 inch = 72 pixels)
            width_inches <- input$plotWidth / 72
            height_inches <- input$plotHeight / 72

            if (input$export_format == "PNG") {
              png_scale <- as.numeric(input$dpi) / 96
              png_plot <- build_boxplot(png_scale)

              ggsave(file,
                plot = png_plot,
                device = "png",
                width = input$plotWidth / 96,
                height = input$plotHeight / 96,
                units = "in",
                dpi = as.numeric(input$dpi),
                bg = "white"
              )
            } else if (input$export_format == "SVG") {
              ggsave(file,
                plot = p,
                device = "svg",
                width = width_inches,
                height = height_inches,
                bg = "white"
              )
            } else if (input$export_format == "PDF") {
              ggsave(file,
                plot = p,
                device = "pdf",
                width = width_inches,
                height = height_inches,
                bg = "white"
              )
            }
          },
          error = function(e) {
            showNotification(paste("Error saving plot:", e$message), type = "error")
          }
        )
      }
    )
  })
}
