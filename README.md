# pcplotr

**pcplotr** is an R package designed to facilitate the visualization of principal component analysis (PCA) results. It streamlines the process of generating informative PCA plots, making it easier for users to interpret and present their PCA data.

## Features

- **Visualization**: Generates customizable plots to visualize principal components from existing PCA results.
- **Integration**: Seamlessly integrates with R's data structures (data.frame, matrix, prcomp and princomp output objects).

## Installation

To install the development version of `pcplotr` directly from GitHub, use the following commands:

```r
# Install remotes package if not already installed
install.packages("remotes")

# Install pcplotr from GitHub
remotes::install_github("svalbuenaa/pcplotr")
```

## Usage
library(pcplotr)

### Assume you have performed PCA using prcomp or similar
pca_result <- prcomp(iris[, 1:4], scale. = TRUE)

### Visualize PC scores of data.frame, matrix or PCA results objects with pcplotr::twoPCsScoresPlot()
pcplotr::twoPCsScoresPlot(arg_PC = pca_result,  # Object (data.frame, matrix or PCA result) with PC scores
						  arg_scores = c(1,2),  # PCs to plot (provide them as an atomic vector)
						  arg_color = NULL)		# Variable to color the data points (e.g. "treatment"). Use only if providing a data.frame or matrix



## License
This project is licensed under the MIT License. See the LICENSE.md file for details.