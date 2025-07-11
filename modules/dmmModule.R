## Dirichlet-Multinomial Mixture (DMM) clustering module — **v4.4.3**
## ─────────────────────────────────────────────────────────────
## UI call   : `dmmUI("dmm")`
## Server call: `dmmServer("dmm")`
##
## Changelog (v4.4.3)
## • Default max K = 5, default criterion = Laplace (unchanged)
## • **Removed** "Re-run example" button (v4.4.2)
## • **NEW**: The criterion actually used to pick the model is now shown above
##   - Sample Clusters table  ➜ *Criterion used: ...*
##   - Top drivers plot       ➜ *Criterion used: ...*
##
# ────────────────────────────── LIBS ─────────────────────────────
library(DirichletMultinomial)   # dmn(), AIC/BIC/Laplace, mixture()
library(reshape2)
library(dplyr)
library(tidyr)
library(ggplot2)
library(DT)

# ───────────────────────────── UI ──────────────────────────────
dmmUI <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::sidebarLayout(
    shiny::sidebarPanel(
      shiny::fileInput(ns("countFile"),
                       label    = "Upload count table (taxa × samples)",
                       multiple = FALSE,
                       accept   = c(".csv", ".tsv", ".txt", ".rds")),
      
      shiny::downloadButton(ns("exampleDownload"), "Download example data"),
      shiny::br(), shiny::hr(),
      
      shiny::numericInput(ns("maxK"), "Max clusters to test", value = 5,  min = 2, step = 1),
      shiny::numericInput(ns("chosenK"), "Force number of clusters (0 = auto)", value = 0, min = 0, step = 1),
      shiny::selectInput(ns("criterion"), "Selection criterion (auto mode)",
                         choices = c("BIC", "AIC", "Laplace"), selected = "Laplace"),
      shiny::numericInput(ns("scale"), "Pseudo-count factor (0 = skip)", value = 10000, min = 0, step = 1000),
      shiny::actionButton(ns("run"), label = "Run DMM", icon = shiny::icon("play")),
      width = 3
    ),
    
    shiny::mainPanel(
      shiny::tabsetPanel(
        shiny::tabPanel("Criteria",
                        shiny::plotOutput(ns("critPlot"), height = "420px"),
                        shiny::hr(),
                        shiny::HTML(
                          "<b>Information criteria guide:</b><ul>
             <li><b>AIC</b> (Akaike Information Criterion) – balances goodness-of-fit against model complexity; lower is better.</li>
             <li><b>BIC</b> (Bayesian Information Criterion) – penalises parameters more strongly, often selecting simpler models.</li>
             <li><b>Laplace</b> (−log posterior) – negative Laplace approximation; conservative.</li>
             </ul>")
        ),
        shiny::tabPanel("Sample Clusters",
                        shiny::downloadButton(ns("downloadClusters"), "Download TSV"),
                        shiny::htmlOutput(ns("critUsed1")),
                        DT::DTOutput(ns("membershipTable"))
        ),
        shiny::tabPanel("Top drivers",
                        shiny::htmlOutput(ns("critUsed2")),
                        shiny::downloadButton(ns("downloadDrivers"), "Download drivers table"),
                        shiny::plotOutput(ns("driverPlot"), height = "600px")
        )
      ), width = 9)
  )
}

# ─────────────────────────── SERVER ────────────────────────────
dmmServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    DETECTION  <- 0.001   # 0.1 %
    PREVALENCE <- 0.10    # 10 % of samples
    EX_PATH    <- "modules/dmmModule_example.txt"  # Example data path
    CACHE_PATH <- "modules/dmmCache.rds"           # Cache file path
    
    output$exampleDownload <- shiny::downloadHandler(
      filename = function() "dmmModule_example.txt",
      content  = function(file) file.copy(EX_PATH, file)
    )
    
    # ── helper fns ─────────────────
    read_counts <- function(path) {
      ext <- tools::file_ext(path)
      obj <- switch(ext,
                    csv = read.csv(path, row.names = 1, check.names = FALSE),
                    tsv = read.delim(path, row.names = 1, check.names = FALSE),
                    txt = read.delim(path, row.names = 1, check.names = FALSE),
                    rds = readRDS(path),
                    stop("Unsupported file: ", ext)
      )
      if (inherits(obj, "phyloseq")) obj <- phyloseq::otu_table(obj)
      if (inherits(obj, "otu_table")) obj <- as(obj, "matrix")
      if (is.data.frame(obj))           obj <- as.matrix(obj)
      if (!is.matrix(obj)) stop("Cannot coerce to matrix.")
      as.matrix(t(obj))  # samples × taxa
    }
    
    to_relative <- function(mat) { rs <- rowSums(mat); rs[rs == 0] <- 1; sweep(mat, 1, rs, "/") }
    core_filter <- function(rel) { rel[, colMeans(rel > DETECTION) >= PREVALENCE, drop = FALSE] }
    valid_matrix <- function(mat) {
      if (!is.numeric(mat))                "Matrix not numeric." else
        if (anyNA(mat))                      "Matrix contains NA." else
          if (any(mat < 0))                    "Negative values present." else
            if (nrow(mat) < 2 || ncol(mat) < 2) "Need ≥2 samples & ≥2 taxa." else
              if (sum(mat) == 0)                   "All zeros." else NULL }
    
    rv <- reactiveValues(metrics = NULL, member = NULL, drivers = NULL, critTxt = NULL, raw = NULL)
    
    saveCache <- function() saveRDS(list(metrics = rv$metrics, member = rv$member, drivers = rv$drivers, critTxt = rv$critTxt, raw = rv$raw), CACHE_PATH)
    
    runDMM <- function(path, cache = FALSE) {
      raw <- tryCatch(read_counts(path), error = function(e) { shiny::showModal(shiny::modalDialog("Read error", e$message)); NULL })
      if (is.null(raw)) return(NULL)
      if (!is.null(msg <- valid_matrix(raw))) { shiny::showModal(shiny::modalDialog("Invalid", msg)); return(NULL) }
      
      rel  <- to_relative(raw) |> core_filter()
      if (ncol(rel) < 2) { shiny::showModal(shiny::modalDialog("Filtering removed all taxa")); return(NULL) }
      
      scale <- as.numeric(input$scale)
      pseudo <- if (scale > 0) round(rel * scale) else raw
      if (any(rowSums(pseudo) == 0)) { shiny::showModal(shiny::modalDialog("Zero rows after scaling")); return(NULL) }
      
      ks   <- seq_len(input$maxK)
      fits <- vector("list", length(ks))
      shiny::withProgress(message = "Fitting DMM", value = 0, {
        for (i in seq_along(ks)) {
          shiny::incProgress(1/length(ks), detail = paste0("K = ", ks[i]))
          fits[[i]] <- dmn(pseudo, ks[i], verbose = FALSE)
        }
      })
      
      rv$metrics <- bind_rows(lapply(seq_along(fits), function(i) {
        data.frame(K = ks[i],
                   BIC     = BIC(fits[[i]]),
                   AIC     = AIC(fits[[i]]),
                   Laplace = laplace(fits[[i]]))
      })) |> mutate(K = as.factor(K))
      
      if (input$chosenK > 0 && input$chosenK %in% ks) {
        best <- fits[[which(ks == input$chosenK)]]
        rv$critTxt <- paste("Forced K =", input$chosenK)
      } else {
        crit <- input$criterion
        best <- fits[[ which.min(rv$metrics[[crit]]) ]]
        rv$critTxt <- paste("selected by", crit)
      }
      
      rv$member <- data.frame(Sample = rownames(pseudo), Cluster = paste0("Cluster_", max.col(mixture(best))))
      
      drv <- melt(fitted(best)); names(drv) <- c("OTU", "Cluster", "Contribution")
      drv$Cluster <- factor(paste0("Cluster_", drv$Cluster))
      rv$drivers <- drv |> group_by(Cluster) |> slice_max(order_by = abs(Contribution), n = 10, with_ties = FALSE) |> ungroup()
      
      rv$raw <- raw  # Save original data
      
      if (cache) saveCache()
      TRUE
    }
    
    ## Event wiring
    observeEvent(input$run,  { shiny::req(input$countFile); runDMM(input$countFile$datapath) })
    # Auto-load example (cached) once at start-up
    observeEvent(TRUE, {
      if (file.exists(CACHE_PATH)) {
        tmp <- readRDS(CACHE_PATH); rv$metrics <- tmp$metrics; rv$member <- tmp$member; rv$drivers <- tmp$drivers; rv$critTxt <- tmp$critTxt; rv$raw <- tmp$raw
      } else runDMM(EX_PATH, cache = TRUE)
    }, once = TRUE)
    
    ## Outputs
    output$critPlot <- renderPlot({ shiny::req(rv$metrics);
      m <- rv$metrics |> pivot_longer(c(AIC, BIC, Laplace), names_to = "Metric", values_to = "Score")
      ggplot(m, aes(K, Score, group = Metric, colour = Metric)) +
        geom_line() + geom_point(size = 3) +
        theme_minimal() + labs(x = "Number of clusters", y = "Information score") })
    
    output$critUsed1 <- output$critUsed2 <- shiny::renderUI({ shiny::req(rv$critTxt); shiny::tags$em(paste("Criterion used:", rv$critTxt)) })
    
    output$membershipTable <- DT::renderDT({ shiny::req(rv$member); rv$member }, options = list(pageLength = 15, autoWidth = TRUE))
    
    output$downloadClusters <- shiny::downloadHandler(
      filename = function() "sample_clusters.tsv",
      content  = function(file) {
        member <- rv$member
        raw <- rv$raw
        if (!is.null(raw) && !is.null(member)) {
          # Match sample order
          member <- member[match(rownames(raw), member$Sample), ]
          out <- cbind(member, raw)
          write.table(out, file, sep = "\t", quote = FALSE, row.names = FALSE)
        } else {
          write.table(member, file, sep = "\t", quote = FALSE, row.names = FALSE)
        }
      })
    
    output$driverPlot <- renderPlot({ shiny::req(rv$drivers);
      ggplot(rv$drivers, aes(x = reorder_within(OTU, Contribution, Cluster), y = Contribution, fill = Cluster)) +
        geom_col(show.legend = FALSE) +
        geom_text(aes(label = round(Contribution, 2)), hjust = ifelse(rv$drivers$Contribution >= 0, -0.1, 1.1), size = 3) +
        scale_x_reordered() + coord_flip() + facet_wrap(~Cluster, scales = "free_y") +
        theme_minimal() + labs(x = "Taxa", y = "Contribution") })
    
    output$downloadDrivers <- shiny::downloadHandler(
      filename = function() "top_drivers.tsv",
      content  = function(file) {
        drv <- rv$drivers
        if (!is.null(drv)) {
          write.table(drv, file, sep = "\t", quote = FALSE, row.names = FALSE)
        }
      }
    )
  })
}