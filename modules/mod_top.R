# mod_top.R

# Top page module for SimpleViz

# UI
topUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidPage(
      tags$head(
        tags$style(HTML("
          .top-wrap {
            background: transparent;
            padding: 24px 0 14px 0;
          }
          .hero-card {
            background: linear-gradient(160deg, #103c5c 0%, #0b2f49 100%);
            border: 1px solid #6f93ad;
            border-radius: 16px;
            padding: 32px 34px;
            color: #edf6fc;
            box-shadow: 0 14px 28px rgba(11, 32, 49, 0.26);
            margin-bottom: 20px;
          }
          .hero-card h1 {
            margin-top: 0;
            margin-bottom: 12px;
            font-weight: 700;
            letter-spacing: 0.3px;
            font-family: 'Times New Roman', Georgia, serif;
            font-size: 35px;
            line-height: 1.1;
          }
          .hero-card p {
            margin-bottom: 12px;
            font-size: 15px;
            line-height: 1.8;
            opacity: 0.97;
            color: #d9e8f3;
          }
          .feature-grid {
            margin-top: 8px;
          }
          .feature-list {
            margin: 0;
            padding-left: 24px;
          }
          .feature-list li {
            font-size: 14px;
            line-height: 1.7;
            color: #d7e6f2;
            margin-bottom: 7px;
          }
          .feature-list li::marker {
            color: #90b3cb;
          }
          .content-card {
            background: linear-gradient(160deg, #103c5c 0%, #0b2f49 100%);
            border: 1px solid #6f93ad;
            border-radius: 14px;
            padding: 24px 26px;
            box-shadow: 0 12px 22px rgba(11, 32, 49, 0.22);
            margin-bottom: 16px;
          }
          .top-wrap .row:last-child .content-card {
            margin-bottom: 6px;
          }
          .content-card h3 {
            margin-top: 0;
            margin-bottom: 14px;
            color: #e6f1f9;
            font-weight: 700;
            font-family: 'Times New Roman', Georgia, serif;
            font-size: 26px;
            letter-spacing: 0.2px;
          }
          .content-card p {
            margin-bottom: 10px;
            line-height: 1.7;
            color: #d7e6f2;
            font-size: 14px;
          }
          .step-badge {
            display: inline-block;
            min-width: 24px;
            height: 24px;
            text-align: center;
            border-radius: 999px;
            background: #d8e6f1;
            color: #103c5c;
            font-weight: 800;
            margin-right: 8px;
            line-height: 24px;
            font-size: 12px;
          }
        "))
      ),
      div(
        class = "top-wrap",
        fluidRow(
          column(
            width = 12,
            div(
              class = "hero-card",
              h1("Welcome to SimpleViz"),
              p("SimpleViz is a web-based Shiny app for exploring tabular omics/microbiome data and generating publication-ready plots and analyses from your data."),
              p("Choose one of the tabs above to start with Box/Violin/Dot/Bar plot, Ordination plot, Differential analysis, Pattern discovery, or see citation information."),
              div(
                class = "feature-grid",
                tags$ul(
                  class = "feature-list",
                  tags$li("Use the panel controls in each module to paste or upload your data."),
                  tags$li("Customize plot appearance, color mapping, and export settings."),
                  tags$li("Most modules support tab-separated input and include sample data examples.")
                )
              )
            )
          )
        ),
        fluidRow(
          column(
            width = 12,
            div(
              class = "content-card",
              h3("How to use SimpleViz"),
              p(HTML("<span class='step-badge'>1</span>Select the analysis tab for your desired visualization or statistical method.")),
              p(HTML("<span class='step-badge'>2</span>Paste your data into the input box or use the example data provided.")),
              p(HTML("<span class='step-badge'>3</span>Adjust plot settings, colors, and labels to fit your needs.")),
              p(HTML("<span class='step-badge'>4</span>Download the resulting figure in PNG, SVG, or PDF format."))
            )
          )
        ),
        fluidRow(
          column(
            width = 12,
            div(
              class = "content-card",
              h3("Modules"),
              p(HTML("<span class='step-badge'>1</span><strong>Box/Violin/Dot/Bar plot</strong>: Create boxplots, violin plots, or dot plots to visualize distributions of your data across groups.")),
              p(HTML("<span class='step-badge'>2</span><strong>Ordination plot</strong>: Generate PCA or NMDS plots to explore sample relationships based on your data.")),
              p(HTML("<span class='step-badge'>3</span><strong>Differential analysis</strong>: Perform DESeq2 analysis or create volcano plots to identify significant features.")),
              p(HTML("<span class='step-badge'>4</span><strong>Pattern discovery</strong>: Create heatmaps or correlation matrices to identify patterns and relationships"))
            )
          )
        )
      )
    )
  )
}

# Server

topServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    # No server-side logic required for the top page at this time.
  })
}
