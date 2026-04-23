# mod_top.R

# Top page module for SimpleViz

# UI
topUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidPage(
      tags$head(
        tags$style(HTML(".top-card { background-color: #f8f9fa; border-radius: 12px; padding: 20px; margin-bottom: 20px; } .well { margin-bottom: 20px; }"))
      ),
      fluidRow(
        column(
          width = 12,
          div(
            class = "top-card",
            h1("Welcome to SimpleViz"),
            p("SimpleViz is a web-based Shiny app for exploring tabular omics/microbiome data andgenerating publication-ready plots and analyses from your data."),
            p("Choose one of the tabs above to start with Box/Violin/Dot/Bar plot, Ordination plot, Differential analysis, Pattern discovery, or see citation information."),
            tags$ul(
              tags$li("Use the panel controls in each module to paste or upload your data."),
              tags$li("Customize plot appearance, color mapping, and export settings."),
              tags$li("Most modules support tab-separated input and include sample data examples.")
            )
          )
        )
      ),
      fluidRow(
        column(
          width = 12,
          wellPanel(
            h3("How to use SimpleViz"),
            p("1. Select the analysis tab for your desired visualization or statistical method."),
            p("2. Paste your data into the input box or use the example data provided."),
            p("3. Adjust plot settings, colors, and labels to fit your needs."),
            p("4. Download the resulting figure in PNG, SVG, or PDF format.")
          )
        ),

      ),
      fluidRow(
        column(
          width = 12,
          wellPanel(
            h3("Modules"),
            p("1. Box/Violin/Dot/Bar plot: Create boxplots, violin plots, or dot plots to visualize distributions of your data across groups."),
            p("2. Ordination plot: Generate PCA or NMDS plots to explore sample relationships based on your data."),
            p("3. Differential analysis: Perform DESeq2 analysis or create volcano plots to identify significant features."),
            p("4. Pattern discovery: Create heatmaps or correlation matrices to identify patterns and relationships")
          )
        ),

      ),
    )
  )
}

# Server

topServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    # No server-side logic required for the top page at this time.
  })
}
