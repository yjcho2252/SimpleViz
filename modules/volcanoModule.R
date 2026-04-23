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
  # Set Shiny module Namespace
  ns <- NS(id)
  
  tagList(
    sidebarLayout(
      sidebarPanel(
        fileInput(ns("volcano_file"), "Upload your TSV file", 
                  accept = c("text/tab-separated-values", 
                             "text/plain", ".tsv", ".txt")),
        downloadButton(ns("download_example"), "Example Data"),
        hr(),
        
        selectInput(ns("x_col"), "Select log2 Fold Change column", ""),
        selectInput(ns("y_col"), "Select p-value column", ""),
        selectInput(ns("label_col"), "Select label column", ""),
        
        numericInput(ns("pCutoff"), "p-value cutoff", value = 0.05, min = 0, max = 1),
        numericInput(ns("FCcutoff"), "Fold change cutoff", value = 1, min = 0),
        
        textInput(ns("highlight_genes"), "Highlight genes (comma-separated)", "Gene_300,Gene_301"),
        
        sliderInput(ns("x.range"), "X-axis range:",
                    min = -20, max = 20, value = c(-5, 5)),
        sliderInput(ns("y.range"), "Y-axis range:",
                    min = 0, max = 310, value = c(0, 15)),
        sliderInput(ns("point_size"), "Data point size:",
                    min = 0.1, max = 5, value = 2, step = 0.1),
        
        numericInput(ns("plot_width"), "Plot width (pixels):", 
                     value = 800, min = 400, max = 2000),
        numericInput(ns("plot_height"), "Plot height (pixels):", 
                     value = 800, min = 300, max = 2000),
        
        colourInput(ns("col_ns"), "Color for non-significant", value = "grey"),
        colourInput(ns("col_log2fc"), "Color for log2FC significant", value = "red"),
        colourInput(ns("col_p"), "Color for p-value significant", value = "blue"),
        colourInput(ns("col_both"), "Color for both significant", value = "green")
      ),
      mainPanel(
        plotOutput(ns("volcano_plot"))
      )
    )
  )
}


# 2. Server
volcanoServer <- function(id, exampleData=example_volcano_data) {
  moduleServer(
    id,
    function(input, output, session) {
      # Assign module internal Namespace (ns)
      ns <- session$ns
      
      # Reactive: uploaded file or example data
      volcano_data <- reactive({
        if (is.null(input$volcano_file)) {
          return(exampleData)    # Example data injected from outside
        } else {
          read.delim(input$volcano_file$datapath, sep = "\t", header = TRUE, stringsAsFactors = FALSE)
        }
      })
      
      # Calculate dynamic axis ranges
      axis_limits <- reactive({
        data <- volcano_data()
        zero_present <- any(data$padj == 0, na.rm = TRUE)
        
        if (zero_present) {
          y_max <- 310
        } else {
          # If no padj is 0, use maximum of -log10(padj)
          y_max <- -log10(max(data$padj[data$padj > 0], na.rm = TRUE))
        }
        
        x_min <- min(data$log2FoldChange, na.rm = TRUE)
        x_max <- max(data$log2FoldChange, na.rm = TRUE)
        list(y_max = y_max, x_min = x_min, x_max = x_max)
      })
      
      # Set default values when no file is uploaded
      observe({
        limits <- axis_limits()
        
        # Set selectable column names
        updateSelectInput(session, "x_col", choices = "log2FoldChange")
        updateSelectInput(session, "y_col", choices = c("padj", "pvalue"))
        updateSelectInput(session, "label_col", choices = "gene")
        
        # Update slider ranges
        updateSliderInput(session, "y.range", 
                          min = 0, max = 310, 
                          value = c(floor(limits$x_min), max(15,ceiling(limits$y_max))))
        
        updateSliderInput(session, "x.range", 
                          min = -20, max = 20, 
                          value = c(floor(limits$x_min), ceiling(limits$x_max)))
        
        if (is.null(input$volcano_file)) {
          updateSelectInput(session, "x_col", selected = "log2FoldChange")
          updateSelectInput(session, "y_col", selected = "padj")
          updateSelectInput(session, "label_col", selected = "gene")
        }
      })
      
      # Draw Volcano Plot
      output$volcano_plot <- renderPlot({
        req(input$x_col, input$y_col, input$label_col)
        
        # List of genes to highlight
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
          subtitle = "Created with EnhancedVolcano",
          xlim = input$x.range,
          ylim = input$y.range,
          colAlpha = 1,
          col = c(input$col_ns, input$col_log2fc, input$col_p, input$col_both),
          selectLab = highlight_genes,
          drawConnectors = TRUE,
          boxedLabels = TRUE
        ) +
          theme(panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank())
      }, width = function() input$plot_width, height = function() input$plot_height)
      
      # Example data download
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

