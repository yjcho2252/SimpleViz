# SimpleViz

SimpleViz is an R Shiny-based interactive data visualization tool designed for biological and medical data analysis. It provides a user-friendly interface for various statistical and visualization methods commonly used in research.

## Live Demo

You can access the live version of SimpleViz at: https://mglab.shinyapps.io/simpleviz/

## Features
- Example dataset included for quick testing and demonstration
- Multiple modules for different types of analyses and visualizations

### 1. Box/Violin/Dot plot Module
- Interactive box plots, violin plots, and dot plots
- Statistical analysis (t-test, ANOVA, Kruskal-Wallis)
- Customizable appearance and colors

### 2. Ordination plot
- PCA and NMDS plots for exploring sample relationships

### 3. Differential analysis
- DESeq2 analysis for identifying differentially expressed features
- Volcano plots for visualizing significant features

### 4. Pattern discovery
- Heatmap visualization for identifying patterns in the data
- Correlation matrix for exploring relationships between variables


## Installation

1. Clone the repository:
```bash
git clone https://github.com/yjcho2252/SimpleViz.git
```

2. Install required R packages:
```R
source("install.R")
```

3. Run the application:
```R
shiny::runApp()
```

## Usage

1. Launch the application
2. Select the desired analysis module
3. Upload your data or use the example dataset
4. Customize the analysis parameters
5. Download the results

## Requirements

- R (version 3.6.0 or higher)
- R Shiny
- Required R packages (see install.R)

## License

This project is licensed under the terms specified in the LICENSE file.

## Citation

If you use SimpleViz in your research, please cite:
SimpleViz: A user-friendly, web-based tool for publication-ready data visualization in bioinformatics
https://doi.org/10.1016/j.mocell.2025.100222

## Contact

For questions or suggestions, please contact yongjoon(at)kangwon.ac.kr.

