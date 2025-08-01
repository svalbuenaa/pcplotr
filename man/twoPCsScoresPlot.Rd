% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/twopcplot.R
\name{twoPCsScoresPlot}
\alias{twoPCsScoresPlot}
\title{TwoPCPlot
ggplot-generated visualization of scores of principal components (PCs)
More on PCA and PCs in R:
https://www.rdocumentation.org/packages/FactoMineR/versions/2.9/topics/PCA
https://cran.r-project.org/web/packages/LearnPCA/vignettes/Vig_04_Scores_Loadings.pdf}
\usage{
twoPCsScoresPlot(arg_PC, arg_scores, arg_color = NULL)
}
\arguments{
\item{arg_PC}{Object containing PC scores for each point in the dataset}

\item{arg_scores}{Atomic vector containing scores to be plotted
scores should be integers}

\item{arg_color}{Optional: If arg_PC is a matrix/data.frame contains}
}
\description{
Accepts either a PCA output object (prcomp or princomp) or a dataframe
containing the PC scores of a pca, as well as the scores to be plotted.
Plots the required scores
}
