# modules/deseqModule.R

# 0. example dataset
set.seed(123)

# Example count matrix
ex_counts <- data.frame(
  Sample1 = c(50, 200, 0, 100, 15),
  Sample2 = c(40, 220, 5, 80, 10),
  Sample3 = c(55, 180, 10, 130, 20),
  Sample4 = c(700, 250, 2, 150, 5),
  Sample5 = c(600, 270, 7, 140, 12),
  Sample6 = c(550, 300, 3, 120, 18)
)
rownames(ex_counts) <- c("GeneA", "GeneB", "GeneC", "GeneD", "GeneE")

# Example metadata
ex_meta <- data.frame(
  condition = c("Control", "Control", "Control", "Treatment", "Treatment", "Treatment"),
  row.names = c("Sample1", "Sample2", "Sample3", "Sample4", "Sample5", "Sample6")
)

# 1. UI
deseqUI <- function(id) {
  ns <- NS(id)
  tagList(
    tags$head(
      tags$style(HTML("
              .button-space {
                margin-bottom: 20px;
              }
              .deseq-panel .form-group,
              .deseq-panel .shiny-input-container {
                margin-bottom: 10px;
              }
              .deseq-panel .form-control,
              .deseq-panel .form-select,
              .deseq-panel .btn,
              .deseq-panel .selectize-input {
                border-radius: 10px;
                font-size: 12px;
              }
              .deseq-panel .control-label,
              .deseq-panel .form-label,
              .deseq-panel .shiny-input-container label {
                font-size: 12px;
                font-weight: 600;
                color: #24445d;
                margin-bottom: 4px;
              }
              .deseq-section {
                background: rgba(255, 255, 255, 0.8);
                border: 1px solid #dce6ec;
                border-radius: 14px;
                padding: 14px 14px 10px 14px;
                margin-bottom: 12px;
                box-shadow: 0 6px 14px rgba(20, 47, 70, 0.05);
              }
              .deseq-section-title {
                font-family: 'Times New Roman', Georgia, serif;
                font-size: 17px;
                font-weight: 700;
                color: #143149;
                margin: 0 0 10px 0;
                letter-spacing: 0.01em;
              }
              .deseq-section-note {
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
              .deseq-panel .btn-default,
              .deseq-panel .btn-secondary {
                background: #f4f7fa;
                border-color: #cdd9e1;
                color: #1f3b53;
              }
              .deseq-panel .btn-primary {
                background: #183b56;
                border-color: #183b56;
                color: #ffffff;
              }
              .deseq-panel .btn-primary:hover,
              .deseq-panel .btn-primary:focus,
              .deseq-panel .btn-primary:active {
                background: #214d6f;
                border-color: #214d6f;
                color: #ffffff;
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
        div(
          class = "deseq-panel",
          div(
            class = "deseq-section",
            tags$h4("Data Input", class = "deseq-section-title"),
            div("Upload a count matrix and use the controls below to assign two groups.", class = "deseq-section-note"),
            tags$label("Paste your count data (tab-separated):", class = "matrix-label"),
            div(
              class = "matrix-table-space",
              rhandsontable::rHandsontableOutput(ns("matrix_table"))
            ),
            div(
              class = "button-space",
              fluidRow(
                column(5, actionButton(ns("submit"), "Submit Data")),
                column(6, downloadButton(ns("download_ex_counts"), "Example Data"))
              )
            ),
            fileInput(
              ns("count_file"),
              label = "Upload Count Data (TSV)",
              accept = c(".csv", ".tsv", ".txt")
            ),
            helpText("Example data will be used if no file is uploaded.")
          ),
          div(
            class = "deseq-section",
            tags$h4("Sample Assignment", class = "deseq-section-title"),
            selectInput(
              ns("available_samples"),
              label = "Available Samples",
              choices = character(0),
              selected = NULL,
              multiple = TRUE,
              selectize = FALSE,
              size = 6
            ),
            fluidRow(
              column(6, actionButton(ns("add_group1"), "-> Group1")),
              column(6, actionButton(ns("add_group2"), "-> Group2"))
            ),
            textInput(ns("group1_name"), "Group 1 Name:", value = "Control"),
            selectInput(
              ns("group1_samples"),
              label = "Group 1",
              choices = character(0),
              selected = NULL,
              multiple = TRUE,
              selectize = FALSE,
              size = 6
            ),
            actionButton(ns("remove_group1"), "<- Remove"),
            textInput(ns("group2_name"), "Group 2 Name:", value = "Treatment"),
            selectInput(
              ns("group2_samples"),
              label = "Group 2",
              choices = character(0),
              selected = NULL,
              multiple = TRUE,
              selectize = FALSE,
              size = 6
            ),
            actionButton(ns("remove_group2"), "<- Remove")
          ),
          div(
            class = "deseq-section",
            tags$h4("Run And Export", class = "deseq-section-title"),
            actionButton(ns("run_deseq"), "Run DESeq2", class = "btn-primary"),
            br(), br(),
            downloadButton(ns("download_deseq_res"), "Download DESeq2 Result")
          )
        ),
        width = 4
      ),
      mainPanel(
        h4("DESeq2 Analysis Result"),
        DTOutput(ns("deseq_table"))
      )
    )
  )
}

# 2. Server
deseqServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    parsed_table_counts <- reactiveVal(NULL)
    table_input_data <- reactiveVal(NULL)

    build_table_input <- function(df) {
      table_df <- data.frame(Gene = rownames(df), df, check.names = FALSE, stringsAsFactors = FALSE)
      table_matrix <- rbind(colnames(table_df), as.matrix(table_df))
      table_output <- as.data.frame(table_matrix, stringsAsFactors = FALSE, check.names = FALSE)
      names(table_output) <- paste0("V", seq_len(ncol(table_output)))
      table_output
    }

    normalize_count_data <- function(df) {
      normalized_df <- as.data.frame(df, stringsAsFactors = FALSE, check.names = FALSE)
      normalized_df[] <- lapply(normalized_df, function(col) suppressWarnings(as.numeric(as.character(col))))
      normalized_df
    }

    parse_table_input <- function(table_df) {
      parsed_df <- as.data.frame(table_df, stringsAsFactors = FALSE, check.names = FALSE)
      parsed_df[] <- lapply(parsed_df, function(col) trimws(as.character(col)))

      non_empty_rows <- apply(parsed_df, 1, function(row) any(!is.na(row) & row != ""))
      non_empty_cols <- apply(parsed_df, 2, function(col) any(!is.na(col) & col != ""))
      parsed_df <- parsed_df[non_empty_rows, non_empty_cols, drop = FALSE]

      validate(need(nrow(parsed_df) >= 2, "Please paste a header row and at least one data row."))
      validate(need(ncol(parsed_df) >= 2, "Please provide a gene column and at least one sample column."))

      headers <- as.character(unlist(parsed_df[1, ], use.names = FALSE))
      headers[is.na(headers) | headers == ""] <- paste0("Column", seq_along(headers))[is.na(headers) | headers == ""]
      headers <- make.unique(headers, sep = "_")

      value_df <- parsed_df[-1, , drop = FALSE]
      names(value_df) <- headers
      validate(need(nrow(value_df) > 0, "Please provide at least one data row below the headers."))

      gene_ids <- as.character(value_df[[1]])
      missing_idx <- is.na(gene_ids) | trimws(gene_ids) == ""
      gene_ids[missing_idx] <- paste0("Gene", which(missing_idx))

      count_df <- value_df[, -1, drop = FALSE]
      count_df <- normalize_count_data(count_df)
      validate(need(ncol(count_df) > 0, "Please provide at least one sample column."))

      count_mat <- as.data.frame(count_df, check.names = FALSE)
      rownames(count_mat) <- gene_ids
      count_mat
    }

    apply_count_data <- function(df) {
      normalized_df <- normalize_count_data(df)
      parsed_table_counts(normalized_df)
      table_input_data(build_table_input(normalized_df))
    }

    table_input_data(build_table_input(ex_counts))

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
        apply_count_data(parse_table_input(df))
      }, error = function(e) {
        showNotification(paste("Error reading table data:", e$message), type = "error")
      })
    })
    
    # (A) Get Count Data (Upload or Example)
    countData <- reactive({
      if (!is.null(parsed_table_counts())) {
        parsed_table_counts()
      } else if (!is.null(input$count_file)) {
        ext <- tools::file_ext(input$count_file$name)
        if (ext %in% c("csv")) {
          df <- read.csv(input$count_file$datapath, row.names = 1, header = TRUE, check.names = FALSE)
        } else {
          df <- read.delim(input$count_file$datapath, row.names = 1, header = TRUE, check.names = FALSE)
        }
        normalized_df <- normalize_count_data(df)
        apply_count_data(normalized_df)
        normalized_df
      } else {
        ex_counts
      }
    })
    
    # (B) Group Management with reactiveValues
    rvals <- reactiveValues(
      available = character(0),
      group1 = character(0),
      group2 = character(0)
    )
    
    # (C) Initialize Samples when countData() Changes
    observeEvent(countData(), {
      samples <- colnames(countData())
      n <- length(samples)
      if (n >= 6) {
        rvals$group1    <- samples[1:3]
        rvals$group2    <- samples[(n-2):n]
        if (n > 6) {
          rvals$available <- samples[4:(n-3)]
        } else {
          rvals$available <- character(0)
        }
      } else if (n >= 3) {
        rvals$group1    <- samples[1:3]
        rvals$group2    <- setdiff(samples, rvals$group1)
        rvals$available <- character(0)
      } else {
        rvals$group1    <- character(0)
        rvals$group2    <- character(0)
        rvals$available <- samples
      }
    })
    
    # (D) Add/Remove Button Logic
    observeEvent(input$add_group1, {
      selected <- input$available_samples
      rvals$group1 <- unique(c(rvals$group1, selected))
      rvals$available <- setdiff(rvals$available, selected)
    })
    
    observeEvent(input$add_group2, {
      selected <- input$available_samples
      rvals$group2 <- unique(c(rvals$group2, selected))
      rvals$available <- setdiff(rvals$available, selected)
    })
    
    observeEvent(input$remove_group1, {
      selected <- input$group1_samples
      rvals$available <- unique(c(rvals$available, selected))
      rvals$group1 <- setdiff(rvals$group1, selected)
    })
    
    observeEvent(input$remove_group2, {
      selected <- input$group2_samples
      rvals$available <- unique(c(rvals$available, selected))
      rvals$group2 <- setdiff(rvals$group2, selected)
    })
    
    # (E) Synchronize selectInput with reactiveValues
    observe({
      updateSelectInput(session, "available_samples",
                        choices  = rvals$available,
                        selected = intersect(input$available_samples, rvals$available)
      )
      updateSelectInput(session, "group1_samples",
                        choices  = rvals$group1,
                        selected = intersect(input$group1_samples, rvals$group1)
      )
      updateSelectInput(session, "group2_samples",
                        choices  = rvals$group2,
                        selected = intersect(input$group2_samples, rvals$group2)
      )
    })
    
    # (F) Create metaData
    makeMetaData <- reactive({
      req(countData())
      allSamples <- colnames(countData())
      cond <- rep("Unassigned", length(allSamples))
      
      cond[allSamples %in% rvals$group1] <- input$group1_name
      cond[allSamples %in% rvals$group2] <- input$group2_name
      
      data.frame(condition = cond, row.names = allSamples)
    })
    
    # (G) Run DESeq2
    dds <- eventReactive(input$run_deseq, {
      cData <- countData()
      mData <- makeMetaData()
      
      # Filter out unassigned samples
      valid_samples <- rownames(mData)[mData$condition != "Unassigned"]
      cData <- cData[, valid_samples, drop = FALSE]
      mData <- mData[valid_samples, , drop = FALSE]
      
      # Convert condition to factor with user-specified levels
      mData$condition <- factor(mData$condition, 
                              levels = c(input$group1_name, input$group2_name))
      
      dds_obj <- DESeqDataSetFromMatrix(
        countData = cData,
        colData   = mData,
        design    = ~ condition
      )
      dds_obj <- DESeq(dds_obj)
      dds_obj
    })
    
    # (H) Result Table & Download
    # --- (1) Merge DESeq2 Results with Normalized Counts ---
    deseq_results_with_norm <- reactive({
      req(dds())
      # 1) DEG Results
      res  <- results(dds())
      resdf <- as.data.frame(res)
      resdf$gene <- rownames(resdf)
      
      # 2) Normalized Counts
      norm_counts <- counts(dds(), normalized = TRUE)
      norm_df <- as.data.frame(norm_counts)
      norm_df$gene <- rownames(norm_df)
      
      # 3) Merge
      merged <- merge(resdf, norm_df, by = "gene", all = TRUE)
      merged <- merged[order(merged$padj), ]
      merged
    })
    
    # --- (2) Display Results Table ---
    output$deseq_table <- renderDT({
      req(deseq_results_with_norm())
      datatable(
        deseq_results_with_norm(),
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          dom = 'Bfrtip',
          buttons = c('tsv', 'excel')
        ),
        extensions = 'Buttons'
      )
    })
    
    # --- (3) Download Example Count Data ---
    output$download_ex_counts <- downloadHandler(
      filename = function() {
        "example_count_data.tsv"
      },
      content = function(file) {
        write.table(ex_counts, file, sep = "\t", quote = FALSE)
      }
    )
    
    # --- (4) Download DESeq2 Results ---
    output$download_deseq_res <- downloadHandler(
      filename = function() {
        "deseq2_results.tsv"
      },
      content = function(file) {
        write.table(deseq_results_with_norm(), file, sep = "\t", quote = FALSE, row.names = FALSE)
      }
    )
  })
}
