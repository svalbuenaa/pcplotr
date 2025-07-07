#' TwoPCPlot
#' ggplot-generated visualization of scores of principal components (PCs)
#' More on PCA and PCs in R:
#' https://www.rdocumentation.org/packages/FactoMineR/versions/2.9/topics/PCA
#' https://cran.r-project.org/web/packages/LearnPCA/vignettes/Vig_04_Scores_Loadings.pdf
#'
#'
#' Accepts either a pca object or a dataframe containing the PC scores of a pca,
#' as well as the scores to be plotted
#'
#'
#'
#'
#'
#'
#'
#'
#'
twoPCsScoresPlot <- function(arg_scores, arg_PC){
  ggplot(arg_scores, aes_string(x=paste0(arg_PC[1]), y=paste0(arg_PC[2]), color="treatment")) +
    geom_point() +
    geom_path() +
    scale_x_continuous(breaks = seq(from = 0, to = 2, by = 0.2)) +
    labs(title=paste0("Exp ", str_extract(deparse(substitute(arg_scores)), "(\\d+)"),", ", arg_PC[1], " vs ", arg_PC[2]))
}
