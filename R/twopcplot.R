#' TwoPCPlot
#' ggplot-generated visualization of scores of principal components (PCs)
#' More on PCA and PCs in R:
#' https://www.rdocumentation.org/packages/FactoMineR/versions/2.9/topics/PCA
#' https://cran.r-project.org/web/packages/LearnPCA/vignettes/Vig_04_Scores_Loadings.pdf
#'
#'
#' Accepts either a PCA output object (prcomp or princomp) or a dataframe
#' containing the PC scores of a pca, as well as the scores to be plotted.
#' Plots the required scores
#'
#'
#'
#' @param arg_scores Atomic vector containing scores to be plotted
#'    scores should be integers
#' @param arg_PC Object containing PC scores for each point in the dataset
#' @param arg_color Optional: If arg_PC is a matrix/data.frame contains
#' @export
twoPCsScoresPlot <- function(arg_PC,
                             arg_scores,
                             arg_color=NULL){
  ## Controlling sanity of arg_PC
  # Cast arg_PC to data.frame if it is data.frame or matrix
  if (is.data.frame(arg_PC) || is.matrix(arg_PC)){
    arg_PC_ <- as.data.frame(arg_PC)
  }

  # Assign scores of prcomp object as arg_PC_
  else if (class(arg_PC) == "prcomp"){
    arg_PC_ <- as.data.frame(arg_PC$x)
    if (!is.null(arg_color)){
      message("Ignoring arg_color with PCA object as input")
    }
  }

  # Assign scores of princomp object as arg_PC_
  else if (class(arg_PC) == "princomp"){
    arg_PC_ <- as.data.frame(arg_PC$scores)
    colnames(arg_PC_) <- paste0("PC", 1:ncol(arg_PC_)) # Rename columns to have
                                                       # a consistent format
    if (!is.null(arg_color)){
      message("Ignoring arg_color with PCA object as input")
    }
  }

  # Throw an error if arg_PC provided is not any of the above
  else {
    stop("Please, introduce a valid arg_PC (PC scores matrix or,
           data.frame, prcomp or princomp PCA object)")
  }


  # Throw an error if there are not at least 2 PC# columns
  indexes_PCs <- grep("PC[0-9]", colnames(arg_PC_))
  if (length(indexes_PCs) < 2){
    stop("Please, introduce a valid arg_PC (PC scores matrix or,
           data.frame with more than 2 PCs, or a prcomp or princomp PCA object)")
  }

  # Reorder arg_PC_ PCs, keep any other columns present in the df
  arg_PC_ <- arg_PC_[, c(colnames(arg_PC_)[order(colnames(arg_PC_)[indexes_PCs])],
                         setdiff(colnames(arg_PC_),
                                 colnames(arg_PC_[indexes_PCs])))]

  # ---------------------------------------------------------------------------

  ## Controlling sanity of arg_scores
  # Throw error if arg_scores is not an atomic vector
  if (!is.atomic(arg_scores)){
    stop("Please, introduce PC scores as an atomic vector c()")
  }

  # Throw error if arg_scores can't be cast to integer
  tryCatch({
    arg_scores_ <- as.integer(arg_scores)
  },
  warning =  function(w){
    stop("Please, introduce PC scores as numeric values")
  },
  error = function(e){
    stop("Please, introduce PC scores as numeric values")
  })

  # Throw an error if arg_scores does not contain exactly two values
  if (length(arg_scores_) != 2){
    stop("Please, introduce 2 PC scores")
  }

  # Throw an error if arg_scores values are too high
  else if(arg_scores_[1] > ncol(arg_PC_) ||
          arg_scores_[2] > ncol(arg_PC_)){
    stop("Please, introduce PC scores values present in the matrix/data.frame
         or the pca object")
  }

  # Throw an error if arg_scores values are not positive
  else if(arg_scores_[1] < 1 ||
          arg_scores_[2] < 1){
    stop("Please, introduce positive PC scores in arg_scores")
  }

  else if(arg_scores_[1] == arg_scores_[2]){
    stop("Please, introduce two different PC scores in arg_scores")
  }


  # Convert for tidyeval
  arg_scores_list <- lapply(arg_scores_, function(x) sym(paste0("PC", x)))

  # ---------------------------------------------------------------------------

  ## Controlling sanity of arg_color
  # Throw an error if arg_color is not a character
  if (!is.null(arg_color) &&
      !is.character(arg_color)){
    stop("Plase, introduce arg_color as a character string (e.g. 'treatment'")
  }

  # Throw an error if the arg_color is not in the colnames of arg_PC_
  if (!is.null(arg_color) &&
      !arg_color %in% colnames(arg_PC_)){
    stop("Plase, introduce a column existing in arg_PC as arg_color. Note that
         arg_color is a valid parameter only when arg_PC is a data.frame or matrix")
  }

  # Throw an error if the arg_color is not a factor column
  if (!is.null(arg_color) &&
      !is.factor(arg_PC_[[arg_color]])){
    stop("Plase, introduce a column containing factor values as arg_color")
  }

  # ---------------------------------------------------------------------------

  ## Create plot
  # Plot with color if arg_color is not null
  if (!is.null(arg_color)){
    arg_color_ <- sym(arg_color)
    ggplot(arg_PC_,
           aes(
             x = !!arg_scores_list[[1]],
             y = !!arg_scores_list[[2]],
             color = !!arg_color_
           )) +
      geom_point()
  # Otherwise, `plot without color`
  } else{
    ggplot(arg_PC_,
           aes(
             x = !!arg_scores_list[[1]],
             y = !!arg_scores_list[[2]]
           )) +
      geom_point()
  }
}


temp_plot <- twoPCsScoresPlot(temp_pca_prin, c(1,3))
temp_plot

matrix_3 <- as.data.frame(matrix(seq(1,9), ncol = 3))


temp_df <-  as.data.frame(matrix(c(1,2,3,42,2,1,2,3,1,3,1,2,22,23,1), ncol = 3))
temp_pca <- prcomp(temp_df)
temp_scores <- as.data.frame(temp_pca$x)
temp_scores$treatment <- as.factor(c(rep("Ctrl", 3), rep("Tmt", 2)))

temp_pca_prin <- princomp(temp_df)
temp_scores_prin <- as.data.frame(temp_pca_prin$scores)
temp_scores_prin$treatment <- as.factor(c(rep("Ctrl", 3), rep("Tmt", 2)))



