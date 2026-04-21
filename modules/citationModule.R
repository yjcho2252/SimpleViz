# modules/citationModule.R

#UI
citationUI <- function(id) {
  ns <- NS(id)
  tagList(
    mainPanel(
      h3("How to cite SimpleViz"),
      p("If you use SimpleViz in your research, please cite:"),
      div(style = "background-color: #f8f9fa; padding: 20px; border-radius: 5px;",
          p(style = "font-weight: bold;", "SimpleViz: A User-Friendly, Web-Based Tool for Publication-Ready Data Visualization in Bioinformatics"),
          p("Byeong Seob Oh", tags$sup("1†"), ", Juhee Kim", tags$sup("2†"), ", Minjeong Gwon", tags$sup("2"), ", Jiwon Bang", tags$sup("2"), ", Kwang-Jun Lee", tags$sup("3"), ", Eun-Jin Lee", tags$sup("4"), " and Yong-Joon Cho", tags$sup("1,2*")),
          p(tags$sup("1"), "Multidimensional Genomics Research Center, Kangwon National University, Chuncheon 24341, Republic of Korea"),
          p(tags$sup("2"), "Department of Molecular Bioscience, Kangwon National University, Chuncheon 24341, Republic of Korea"),
          p(tags$sup("3"), "Division of Zoonotic and Vector Borne Diseases Research, Center for Infectious Diseases Research, National Institute of Health, Cheongju 28159, Republic of Korea"),
          p(tags$sup("4"), "Department of Life Sciences, School of Life Sciences and Biotechnology, Korea University, Seoul 02841, South Korea"),
          p(tags$sup("†"), "These authors contributed equally to this work"),
          p(tags$sup("*"), "Corresponding author: Yong-Joon Cho, Ph.D. (Tel: +82-33-250-8544, E-mail: yongjoon@kangwon.ac.kr)")
      ),
      br(),
      p("For more information about SimpleViz, please visit our GitHub repository or contact the corresponding author.")
    )
  )
}

# Server
citationServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    # No server-side logic needed for now
  })
}
