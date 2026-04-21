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
      # (A) Left Sidebar
      sidebarPanel(
        h4("Sample and Group Settings"),
        
        # (1) Count Data Upload
        fileInput(
          ns("count_file"),
          label = "Upload Count Data (TSV)",
          accept = c(".csv", ".tsv", ".txt")
        ),
        helpText("Example data will be used if no file is uploaded."),
        br(),
        
        # (1-2) Example Data Download Button
        downloadButton(ns("download_ex_counts"), "Download Example Data"),
        br(), br(),
        
        # (2) Available Samples (Select by Click)
        selectInput(
          ns("available_samples"),
          label = "Available Samples",
          choices = character(0),
          selected = NULL,
          multiple = TRUE,
          selectize = FALSE,  # Turn off selectize to use size option
          size = 6
        ),
        
        # (3) Add Button: Selected Samples → Group1, Group2
        fluidRow(
          column(6, actionButton(ns("add_group1"), "-> G1")),
          column(6, actionButton(ns("add_group2"), "-> G2"))
        ),
        br(),
        
        # (4) Group 1
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
        br(), br(),
        
        # (5) Group 2
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
        actionButton(ns("remove_group2"), "<- Remove"),
        br(), br(),
        
        # (6) DESeq2 Run / Download Button
        actionButton(ns("run_deseq"), "Run DESeq2", class = "btn-primary"),
        br(), br(),
        downloadButton(ns("download_deseq_res"), "Download DESeq2 Result"),
        
        width = 4  # Sidebar width adjustment
      ),
      
      # (B) Right Main Area
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
    
    # (A) Get Count Data (Upload or Example)
    countData <- reactive({
      if (!is.null(input$count_file)) {
        ext <- tools::file_ext(input$count_file$name)
        if (ext %in% c("csv")) {
          read.csv(input$count_file$datapath, row.names = 1, header = TRUE, check.names = FALSE)
        } else {
          read.delim(input$count_file$datapath, row.names = 1, header = TRUE, check.names = FALSE)
        }
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
