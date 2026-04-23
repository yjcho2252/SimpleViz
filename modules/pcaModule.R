# modules/pcaModule.R

# 0. example dataset
set.seed(123)
n_samples <- 30  # Reduced from 100
n_features <- 10  # Reduced from 20
n_groups <- 3

# Generate example data
generate_group_data <- function(n, features, mean, sd) {
  matrix(rnorm(n * features, mean = mean, sd = sd), nrow = n)
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
    sidebarLayout(
      sidebarPanel(
        # File upload
        fileInput(ns("pca_file"), "Upload your TSV file",
                  accept = c("text/tab-separated-values", "text/plain", ".tsv", ".txt")),
        
        # Example data download
        downloadButton(ns("downloadPCAData"), "Example Data"),
        hr(),
        
        # Analysis method (PCA/NMDS)
        radioButtons(ns("analysis_method"), "Analysis method:", choices = c("PCA", "NMDS"), selected = "PCA"),
        
        # PCA axis selection
        selectInput(ns("x_axis"), "X-axis:", choices = paste0("Dim", 1:5), selected = "Dim1"),
        selectInput(ns("y_axis"), "Y-axis:", choices = paste0("Dim", 1:5), selected = "Dim2"),
        
        # Axis range settings
        sliderInput(ns("x_range"), "X-axis range:", min = -10, max = 10, 
                    value = c(-10, 10), step = 1),
        sliderInput(ns("y_range"), "Y-axis range:", min = -10, max = 10, 
                    value = c(-10, 10), step = 1),
        
        # Point size & font size
        sliderInput(ns("point_size"), "Point Size:", min = 1, max = 5, value = 2, step = 0.5),
        sliderInput(ns("axis_font_size"), "Axis Font Size:", min = 8, max = 20, value = 12, step = 1),
        
        # Ellipse display option
        checkboxInput(ns("add_ellipse"), "Add ellipses", value = TRUE),
        selectInput(ns("ellipse_type"), "Ellipse Type:", 
                    choices = c("concentration", "convex"), selected = "concentration"),
        
        # Show points only / Show text
        checkboxInput(ns("show_points"), "Show points without text", value = TRUE),
        
        # Plot size
        sliderInput(ns("plot_width"), "Plot Width:", min = 400, max = 1200, value = 800, step = 50),
        sliderInput(ns("plot_height"), "Plot Height:", min = 300, max = 1000, value = 600, step = 50),
        
        # Group colors
        uiOutput(ns("color_pickers")),
        
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
      
      # Upload / Example data selection
      pca_dataset <- reactive({
        # Use example data if no file is uploaded
        if (is.null(input$pca_file)) {
          return(examplePCAData)
        } else {
          df <- read.delim(input$pca_file$datapath, sep = "\t", header = TRUE, check.names = FALSE)
          # If 'Cluster' column exists, rename it to 'Group'
          if ("Cluster" %in% colnames(df)) {
            colnames(df)[colnames(df) == "Cluster"] <- "Group"
          }
          # If both Sample and Group columns exist, return as is (wide format)
          if (all(c("Sample", "Group") %in% colnames(df))) {
            return(df)
          }
          return(df)
        }
      })
      
      # Analysis result (PCA or NMDS)
      analysis_result <- reactive({
        df <- pca_dataset()
        numeric_cols <- setdiff(colnames(df), c("Sample", "Group", "Variable"))
        X <- df[, numeric_cols, drop = FALSE]
        X <- as.data.frame(lapply(X, as.numeric))
        X[X < 0] <- 0  # Set all negative values to 0 for all analyses
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
      
      # Extract group names and create color selection UI for each group
      output$color_pickers <- renderUI({
        df <- pca_dataset()
        if (!"Group" %in% colnames(df)) return(NULL)
        
        groups <- unique(df$Group)
        
        color_inputs <- lapply(seq_along(groups), function(i) {
          colourInput(
            inputId = ns(paste0("color_", i)),
            label = paste("Color for", groups[i]),
            value = brewer.pal(8, "Set2")[((i - 1) %% 8) + 1]
          )
        })
        
        do.call(tagList, color_inputs)
      })
      
      # Create palette for each group
      selected_palette <- reactive({
        df <- pca_dataset()
        if (!"Group" %in% colnames(df)) return(NULL)
        
        groups <- unique(df$Group)
        sapply(seq_along(groups), function(i) {
          input[[paste0("color_", i)]]
        })
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
          p <- fviz_pca_ind(
            res$result,
            title = "PCA Plot",
            repel = TRUE,
            axes = c(x_axis, y_axis),
            geom.ind = if (input$show_points) "point" else c("point", "text"),
            col.ind = df$Group,
            palette = selected_palette(),
            addEllipses = input$add_ellipse,
            ellipse.level = 0.9,
            legend.title = "Groups",
            mean.point = FALSE,
            pointsize = input$point_size
          )
          if (input$add_ellipse && input$ellipse_type == "convex") {
            p$layers[[2]]$aes_params$linetype <- 2
          }
          p <- p +
            theme(axis.line = element_line(color = "black"),
                  panel.border = element_blank(),
                  panel.background = element_blank(),
                  axis.text = element_text(size = input$axis_font_size),
                  axis.title = element_text(size = input$axis_font_size + 2)) +
            scale_x_continuous(limits = input$x_range) +
            scale_y_continuous(limits = input$y_range)
          p
        } else {
          # NMDS plot
          if (is.null(res$result) || is.null(res$result$points)) {
            plot.new(); text(0.5, 0.5, "NMDS failed: Check data structure.", cex = 1.5); return()
          }
          nmds_points <- as.data.frame(res$result$points)
          colnames(nmds_points) <- c("NMDS1", "NMDS2")
          nmds_points$Group <- df$Group
          ggplot(nmds_points, aes(NMDS1, NMDS2, color = Group)) +
            geom_point(size = input$point_size) +
            theme_minimal(base_size = input$axis_font_size) +
            labs(title = "NMDS Plot")
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
      
    }
  )
}
