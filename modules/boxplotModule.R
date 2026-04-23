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
      position = "left",
      sidebarPanel(
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
        ),
        hr(),
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
        ),
        uiOutput(ns("x_order_input")),
        uiOutput(ns("color_inputs")),
        actionButton(ns("apply_colors"), "Apply Colors"),
        sliderInput(ns("pointSize"), "Data Point Size:", min = 0, max = 5, value = 2, step = 0.1),
        sliderInput(ns("barWidth"), "Bar Width:", min = 0.1, max = 1, value = 0.7, step = 0.05),
        sliderInput(ns("lineThickness"), "Box/Violin Line Thickness:", min = 0, max = 2, value = 1.0, step = 0.05),
        sliderInput(ns("fontSize"), "Font Size:", min = 6, max = 24, value = 12, step = 1),
        sliderInput(ns("plotWidth"), "Plot Width (pixels):", value = 400, min = 200, max = 2000, step = 10),
        sliderInput(ns("plotHeight"), "Plot Height (pixels):", value = 600, min = 200, max = 1500, step = 10),
        selectInput(ns("export_format"), "Export Format:",
          choices = c("PNG", "SVG", "PDF"),
          selected = "PNG"
        ),
        numericInput(ns("dpi"), "DPI (for PNG only):", value = 300, min = 72, max = 600),
        textInput(ns("filename"), "Export Filename:", value = "boxplot"),
        downloadButton(ns("save_plot"), "Download Plot"),
        hr()
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

    default_colors <- c("#F8766D", "#00BA38", "#619CFF", "#F564E3", "#00BFC4", "#B79F00")

    output$dynamic_output <- renderUI({
      tagList(
        plotOutput(session$ns("plot"), width = "100%", height = paste0(input$plotHeight, "px")),
        verbatimTextOutput(session$ns("ttest_results"))
      )
    })

    output$matrix_table <- rhandsontable::renderRHandsontable({
      table_data <- data()
      table_data[] <- lapply(table_data, function(col) {
        if (is.factor(col)) as.character(col) else col
      })

      rhandsontable::rhandsontable(
        table_data,
        rowHeaders = NULL,
        height = 300,
        useTypes = FALSE,
        readOnly = FALSE
      ) %>%
        rhandsontable::hot_table(minCols = 1) %>%
        rhandsontable::hot_context_menu(allowRowEdit = TRUE, allowColEdit = TRUE)
    })

    # Box plot: Initialize color palette
    observe({
      req(data())
      groups <- levels(as.factor(data()[[input$x_var]]))
      if (is.null(color_palette())) {
        color_palette(setNames(default_colors[1:length(groups)], groups))
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

          # Ensure numeric columns are properly converted
          for (col in names(matrix_data)) {
            if (is.character(matrix_data[[col]])) {
              # Try to convert to numeric if possible
              num_col <- suppressWarnings(as.numeric(matrix_data[[col]]))
              if (!all(is.na(num_col))) {
                matrix_data[[col]] <- num_col
              }
            }
          }

          # Update the data
          data(matrix_data)
          # Update variable selections
          updateSelectInput(session, "x_var", choices = names(matrix_data), selected = names(matrix_data)[1])
          updateSelectInput(session, "y_var", choices = names(matrix_data), selected = names(matrix_data)[2])

          # Update labels
          updateTextInput(session, "xlab", value = names(matrix_data)[1])
          updateTextInput(session, "ylab", value = names(matrix_data)[2])

          # Update y-axis range
          if (is.numeric(matrix_data[[names(matrix_data)[2]]])) {
            y_min <- min(matrix_data[[names(matrix_data)[2]]], na.rm = TRUE) - 10
            y_max <- max(matrix_data[[names(matrix_data)[2]]], na.rm = TRUE) + 10
            updateNumericInput(session, "ymin", value = floor(y_min))
            updateNumericInput(session, "ymax", value = ceiling(y_max))
          }

          # Update color palette
          groups <- levels(as.factor(matrix_data[[names(matrix_data)[1]]]))
          color_palette(setNames(default_colors[1:length(groups)], groups))
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

          # Convert character columns to numeric only when all non-empty values are numeric.
          for (col in names(uploaded_data)) {
            if (is.character(uploaded_data[[col]])) {
              trimmed <- trimws(uploaded_data[[col]])
              non_empty <- trimmed != "" & !is.na(trimmed)
              num_col <- suppressWarnings(as.numeric(trimmed))
              if (any(non_empty) && all(!is.na(num_col[non_empty]))) {
                uploaded_data[[col]] <- num_col
              }
            }
          }

          uploaded_df <- uploaded_data
          data(uploaded_df)
          col_names <- names(uploaded_df)
          x_col <- col_names[1]
          numeric_cols <- col_names[sapply(uploaded_df, is.numeric)]
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
          updateTextInput(session, "xlab", value = x_col)
          updateTextInput(session, "ylab", value = y_col)

          if (is.numeric(uploaded_df[[y_col]])) {
            y_values <- uploaded_df[[y_col]]
            y_min_raw <- min(y_values, na.rm = TRUE)
            y_max_raw <- max(y_values, na.rm = TRUE)
            if (is.finite(y_min_raw) && is.finite(y_max_raw)) {
              y_range <- y_max_raw - y_min_raw
              y_pad <- max(1, y_range * 0.1)
              updateNumericInput(session, "ymin", value = floor(y_min_raw - y_pad))
              updateNumericInput(session, "ymax", value = ceiling(y_max_raw + y_pad))
            }
          }

          groups <- levels(as.factor(uploaded_df[[x_col]]))
          palette_vals <- rep(default_colors, length.out = length(groups))
          color_palette(setNames(palette_vals, groups))
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

    output$color_inputs <- renderUI({
      req(data(), input$x_var)
      groups <- levels(as.factor(data()[[input$x_var]]))
      current_palette <- color_palette() %||% setNames(default_colors[1:length(groups)], groups)

      color_inputs <- lapply(seq_along(groups), function(i) {
        colourInput(
          inputId = session$ns(paste0("color", i)),
          label = paste("Color for", groups[i]),
          value = current_palette[groups[i]]
        )
      })

      do.call(tagList, color_inputs)
    })

    observeEvent(input$apply_colors, {
      req(data(), input$x_var)
      groups <- levels(as.factor(data()[[input$x_var]]))
      new_palette <- sapply(seq_along(groups), function(i) input[[paste0("color", i)]])
      color_palette(setNames(new_palette, groups))
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
        current_palette <- setNames(default_colors[1:length(groups)], groups)
        color_palette(current_palette)
      }

      list(
        data = data_plot,
        x_var = input$x_var,
        y_var = input$y_var,
        facet_var = input$facet_var,
        plot_type = input$plot_type,
        groups = groups,
        colors = color_palette() %||% setNames(default_colors[1:length(groups)], groups)
      )
    })

    output$plot <- renderPlot(
      {
        plot_info <- plot_data()

        p <- ggplot(plot_info$data, aes_string(x = plot_info$x_var, y = plot_info$y_var, group = plot_info$x_var)) +
          labs(
            title = paste(plot_info$plot_type),
            x = input$xlab, y = input$ylab
          ) +
          theme_pubr(base_size = input$fontSize) +
          theme(
            plot.title = element_text(hjust = 0.5, size = input$fontSize * 1.3),
            axis.title = element_text(size = input$fontSize * 1.2),
            axis.text = element_text(size = input$fontSize),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            axis.line = element_line(size = 1, color = "black"),
            axis.ticks = element_line(size = 1, color = "black"),
            legend.title = element_text(size = input$fontSize),
            legend.text = element_text(size = input$fontSize * 0.8)
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

          if (input$stat_method == "t.test") {
            stat_test <- compare_means(formula, data = plot_info$data, method = "t.test")
            comparisons <- combn(plot_info$groups, 2, simplify = FALSE)

            y_max <- max(plot_info$data[[plot_info$y_var]])
            step <- (input$ymax - y_max) / (length(comparisons) + 1)
            y_positions <- seq(y_max + step, by = step, length.out = length(comparisons))

            p <- p + stat_compare_means(
              comparisons = comparisons,
              label = "p.signif",
              method = "t.test",
              label.y = y_positions
            )
          } else if (input$stat_method %in% c("anova", "kruskal.test")) {
            stat_test <- compare_means(formula, data = plot_info$data, method = input$stat_method)
            p <- p + stat_compare_means(
              label.y = max(plot_info$data[[plot_info$y_var]]) * 1.4,
              method = input$stat_method
            )
          }
        }

        # Add facet if facet variable is selected
        if (plot_info$facet_var != "None") {
          p <- p + facet_wrap(as.formula(paste("~", plot_info$facet_var)))
        }

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
              ggsave(file,
                plot = p,
                device = "png",
                width = width_inches,
                height = height_inches,
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
