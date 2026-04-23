library(devtools)
library(shiny)
library(ggplot2)
library(colourpicker)
library(showtext)
library(ggpubr)
library(rstatix)
library(curl)
library(dunn.test)
library(EnhancedVolcano)
library(dplyr)
library(FactoMineR)
library(factoextra)
library(vegan)
library(tidyverse)
library(pairwiseAdonis)
library(RColorBrewer)
library(reshape2)
library(BiocManager)
library(pheatmap)
library(tidyr)
library(DESeq2)
library(DT)
library(sortable)
library(gggenes)
library(rtracklayer)
library(tools)
library(svglite)
library(ANCOMBC)
library(phyloseq)
library(DirichletMultinomial)
library(tidytext)
library(bslib)
library(rhandsontable)

# Load configuration and modules
source("modules/mod_top.R")
source("modules/boxplotModule.R")
source("modules/pcaModule.R")
source("modules/volcanoModule.R")
source("modules/heatmapModule.R")
source("modules/deseqModule.R")
source("modules/citationModule.R")
source("modules/correlationModule.R")
#source("modules/ancombc2Module.R")
#source("modules/snpModule.R")
#source("modules/GenesyntenyModule.R")
# Load some fonts
#font_add_google("Tinos", "Times New Roman")
if (file.exists("/usr/share/fonts/truetype/tinos/Tinos-Regular.ttf")) {
  font_add("Times New Roman", regular = "/usr/share/fonts/truetype/tinos/Tinos-Regular.ttf")
} else if (file.exists("Tinos-Regular.ttf")) {
  font_add("Times New Roman", regular = "Tinos-Regular.ttf")
}
showtext_auto()
addResourcePath("modules", "modules")

# Define UI
ui <- fluidPage(
  page_navbar(
    title = actionLink(
      "go_home",
      label = tagList(icon("chart-simple"), " SimpleViz"),
      style = "font-weight: 700; letter-spacing: 0.3px; font-size: 16px; color: inherit; text-decoration: none; padding: 0;"
    ),
    id = "tab_panel_main",
    theme = bs_theme(
      version = 5,
      bootswatch = "flatly",
      primary = "#2c3e50"
    ),
    header = tags$head(
      tags$link(rel = "icon", href = "data:image/svg+xml,<svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 448 512' fill='%232c3e50'><path d='M160 80c0-26.5 21.5-48 48-48h32c26.5 0 48 21.5 48 48v352c0 26.5-21.5 48-48 48h-32c-26.5 0-48-21.5-48-48V80zM0 272c0-26.5 21.5-48 48-48h32c26.5 0 48 21.5 48 48v160c0 26.5-21.5 48-48 48H48c-26.5 0-48-21.5-48-48V272zM368 96h32c26.5 0 48 21.5 48 48v288c0 26.5-21.5 48-48 48h-32c-26.5 0-48-21.5-48-48V144c0-26.5 21.5-48 48-48z'/></svg>"),
      tags$style(HTML("
        body { font-size: 14px; }
        .navbar { min-height: 40px; }
        .navbar-brand { font-size: 15px; padding-top: 6px; padding-bottom: 6px; line-height: 1.1; }
        .navbar-nav .nav-link { font-size: 13px; padding-top: 6px; padding-bottom: 6px; line-height: 1.1; }
        .navbar-nav .nav-link[data-value='Top'] { display: none !important; }
        .navbar-nav .nav-link[data-value='Home'] { display: none !important; }
        .dropdown-menu { font-size: 13px; }
        .btn, .form-control, .form-select, .form-check-label { font-size: 13px; }
        h4 { font-size: 1.15rem; }
        h5 { font-size: 1.02rem; }
        .sidebar h4 { font-size: 0.98rem; margin-bottom: 0.45rem; }
        .sidebar h5 { font-size: 0.90rem; }
        .sidebar .control-label,
        .sidebar .form-label,
        .sidebar .form-check-label,
        .sidebar .help-block,
        .sidebar details summary,
        .sidebar .shiny-input-container label {
          font-size: 12px;
        }
        .sidebar .form-control,
        .sidebar .form-select,
        .sidebar .btn {
          font-size: 12px;
        }
        .selectize-input,
        .selectize-dropdown,
        .selectize-dropdown-content,
        .selectize-dropdown .option,
        .selectize-dropdown .optgroup-header,
        select.form-select,
        .irs-grid-text {
          font-size: 12px !important;
        }
        .sidebar .shiny-input-container {
          margin-bottom: 5px;
        }
        .sidebar .control-label,
        .sidebar .form-label {
          margin-bottom: 2px;
        }
        .sidebar .form-group {
          margin-bottom: 5px;
        }
        .sidebar hr {
          margin-top: 6px;
          margin-bottom: 6px;
        }
        .sidebar details {
          margin-top: 2px !important;
          margin-bottom: 2px !important;
        }
        .sidebar .help-block {
          margin-top: 1px;
          margin-bottom: 2px;
        }
      "))
    ),
    
    nav_panel(
      title = "Home",
      icon = icon("house"),
      topUI("top")
    ),
    nav_panel(
      title = "Box/Violin/Dot/Bar plot", 
      icon = icon("chart-bar"),
      boxplotUI("boxplot")
    ),
    nav_panel(
      title = "Ordination plot", 
      icon = icon("project-diagram"),
      pcaUI("pca")
    ),
    nav_menu(
      title = "Differential analysis",
      icon = icon("vial-circle-check"),
      nav_panel("Volcano plot", icon = icon("chart-line"), volcanoUI("volcano")),
      nav_panel("DESeq2", icon = icon("vial-circle-check"), deseqUI("DESeq2"))
    ),
    nav_menu(
      title = "Pattern discovery",
      icon = icon("magnifying-glass"),
      nav_panel("Heatmap", icon = icon("th"), heatmapUI("heatmap")),
      nav_panel("Correlation matrix", icon = icon("table"), correlationUI("correlation"))
    ),
    nav_panel(
      title = "Citation",
      icon = icon("book"),
      citationUI("citation")
    )
  ),
  conditionalPanel(
    condition = "input.tab_panel_main === 'Home'",
    tags$footer(
      style = "text-align: center; padding: 20px;",
      tags$div(
        style = "display: flex; justify-content: center; align-items: center;",
        tags$span(
          style = "display: inline-flex; align-items: center; margin-right: 10px;",
          tags$img(
            src = "https://www.kangwon.ac.kr/assets/ko/images/sub/symbol1.webp",
            alt = "Kangwon National University",
            style = "height: 22px; width: auto; display: block;"
          )
        ),
        tags$span(
          style = "font-size: 14px; color: #6c757d;",
          "Microbial Genomics Lab"
        )
      )
    )
  )
)
# Define Server
server <- function(input, output, session) {
  observeEvent(input$go_home, {
    updateTabsetPanel(session, inputId = "tab_panel_main", selected = "Home")
  })
  
  topServer("top")
  boxplotServer("boxplot")
  pcaServer("pca")
  volcanoServer("volcano")
  heatmapServer("heatmap")
  deseqServer("DESeq2")
  correlationServer("correlation")
#  ancombc2Server("ancombc2")
#  dmmServer("dmm")
#  snpServer("snp") 
#  GenesyntenyServer("Genesynteny")
  citationServer("citation")
  
}
options(shiny.maxRequestSize = 100 * 1024^2)

# Run the app
shinyApp(ui, server)
