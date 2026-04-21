# modules/correlationModule.R

# 0. example dataset
set.seed(123)
genes   <- paste0("Gene", 1:10)
samples <- paste0("Sample", 1:5)

# 예시 데이터: 소수점 둘째 자리까지 반올림
mat_data_example <- matrix(
  round(runif(10*5, min = 0, max = 15), 2),
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
        
        # (A) 텍스트 영역 입력
        textAreaInput(ns("matrix_input"), "Paste your matrix data (tab-separated):",
                      rows = 10,
                      placeholder = "Gene\tSample1\tSample2\tSample3\nGene1\t12.3\t7.5\t9.0\n..."),
        
        div(class = "button-space",
            fluidRow(
              column(5, actionButton(ns("submit"), "Submit Data")),
              column(6, downloadButton(ns("download_example"), "Example Data"))
            )
        ),
        
        # (B) 파일 업로드
        fileInput(ns("corr_file"), "Upload your TSV file",
                  accept = c("text/tab-separated-values", 
                             "text/plain", ".tsv", ".txt")),
        hr(),
        
        # (C) 상관계수 및 클러스터 옵션
        selectInput(ns("corrMethod"), "Correlation Method",
                    choices = c("pearson", "spearman", "kendall"),
                    selected = "pearson"),
        selectInput(ns("distMethod"), "Distance for Clustering",
                    choices = c("1 - correlation", "euclidean", "manhattan"),
                    selected = "1 - correlation"),
        selectInput(ns("hclustMethod"), "Clustering Method",
                    choices = c("complete", "ward.D", "ward.D2", 
                                "single", "average", "mcquitty", 
                                "median", "centroid"),
                    selected = "complete"),
        
        # (D) 색상 팔레트
        selectInput(ns("color_palette"), "Color Palette:",
                    choices = c("RdBu", "Blues", "Greens", "Reds", 
                                "YlOrRd", "YlGnBu", "heat.colors"),
                    selected = "RdBu"),
        
        # (E) 상관값 표시 설정
        checkboxInput(ns("show_numbers"), "Display Correlation Values in Cells", value = TRUE),
        
        # (F) 폰트, 플롯 크기
        sliderInput(ns("fontsize_number"), "Font Size for Numbers:",
                    min = 3, max = 30, value = 10, step = 1),
        sliderInput(ns("font_size"), "Font Size for Labels:",
                    min = 5, max = 20, value = 10, step = 1),
        sliderInput(ns("plot_width"), "Plot Width:", 
                    min = 400, max = 1200, value = 700, step = 50),
        sliderInput(ns("plot_height"), "Plot Height:", 
                    min = 300, max = 1000, value = 600, step = 50)
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
      ns <- session$ns
      
      # (A) 텍스트영역 입력 파싱
      parsed_text_data <- reactiveVal(NULL)
      
      observeEvent(input$submit, {
        req(input$matrix_input)
        
        data_lines <- strsplit(input$matrix_input, "\n")[[1]]
        df <- read.table(
          textConnection(data_lines),
          sep = "\t",
          header = TRUE,
          check.names = FALSE
        )
        
        mat <- as.matrix(df[, -1])  # 첫 열은 Gene ID, 나머지는 수치
        rownames(mat) <- df[[1]]
        
        parsed_text_data(mat)
      })
      
      # (B) 최종 사용할 데이터
      raw_mat <- reactive({
        # (1) 텍스트 입력이 우선
        if(!is.null(parsed_text_data())) {
          return(parsed_text_data())
        }
        # (2) 업로드 파일
        else if(!is.null(input$corr_file)) {
          df <- read.table(input$corr_file$datapath,
                           sep = "\t", header = TRUE, check.names = FALSE)
          mat <- as.matrix(df[, -1])
          rownames(mat) <- df[[1]]
          return(mat)
        }
        # (3) 없으면 예시
        else {
          return(exampleData)
        }
      })
      
      # (C) 상관행렬 -> pheatmap 렌더
      output$corr_heatmap <- renderPlot({
        req(raw_mat())
        mat <- raw_mat()
        
        # (1) 상관계수 계산
        corr_mat <- cor(mat, method = input$corrMethod, use = "complete.obs")
        corr_mat <- round(corr_mat, 2)
        # 
        # (2) 거리 계산
        dist_rows <- NULL
        dist_cols <- NULL
        if (input$distMethod == "1 - correlation") {
          dist_rows <- as.dist(1 - corr_mat)
          dist_cols <- as.dist(1 - corr_mat)
        } else {
          dist_rows <- dist(corr_mat, method = input$distMethod)
          dist_cols <- dist(corr_mat, method = input$distMethod)
        }
        
        # (3) 색상 설정
        pal_name <- input$color_palette
        pal_size <- 100
        if (pal_name %in% rownames(RColorBrewer::brewer.pal.info)) {
          colors <- colorRampPalette(RColorBrewer::brewer.pal(min(pal_size, 9), pal_name))(pal_size)
        } else {
          if (pal_name == "heat.colors") {
            colors <- heat.colors(pal_size)
          } else {
            colors <- colorRampPalette(RColorBrewer::brewer.pal(9, "RdBu"))(pal_size)
          }
        }
        
        # (4) pheatmap: 숫자를 소수점 둘째 자리까지 표시
        pheatmap::pheatmap(
          corr_mat,
          color = colors,
          clustering_distance_rows = dist_rows,
          clustering_distance_cols = dist_cols,
          clustering_method = input$hclustMethod,
          legend = TRUE,
          border_color = "grey80",
          main = paste("Correlation Heatmap (", input$corrMethod, ")", sep = ""),
          
          # 텍스트 및 숫자
          fontsize = input$font_size,
          display_numbers = if (input$show_numbers) corr_mat else FALSE,
          number_format = "%.2f",      # ← 소수점 둘째 자리까지만 표시
          fontsize_number = input$fontsize_number
        )
      },
      width = function() input$plot_width,
      height = function() input$plot_height)
      
      # (D) 예시 데이터 다운로드
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
