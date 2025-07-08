


# Test sanity of arg_PC
test_that("arg_PC format/value errors", {
  expect_error(twoPCsScoresPlot(arg_PC = list(1,2), arg_scores = c(1,2), arg_color=NULL), info = "class list")           # Error if arg_PC is a list
  expect_error(twoPCsScoresPlot(arg_PC = c(1,2), arg_scores = c(1,2), arg_color=NULL), info = "class atomic vector")     # Error if arg_PC is an atomic vector
  expect_error(twoPCsScoresPlot(arg_PC =matrix_3, arg_scores = c(1,2), arg_color=NULL), info = "class atomic vector")    # Error if arg_PC is an atomic vector
  #expect_error(twoPCsScoresPlot(arg_PC = temp_scores, arg_scores = c(1,2), arg_color=NULL), info = "Temp no error") # Temp no error
})

# ---------------------------------------------------------------------------------------------------------------------

# Test sanity of arg_scores
test_that("arg_scores format/value errors", {
  expect_error(twoPCsScoresPlot(arg_PC = temp_scores, arg_scores = list(1,2), arg_color=NULL), info = "class list")         # Error if arg_scores is a list
  expect_error(twoPCsScoresPlot(arg_PC = temp_scores, arg_scores = matrix_3, arg_color=NULL), info = "class matrix")        # Error if arg_scores is a matrix
  expect_error(twoPCsScoresPlot(arg_PC = temp_scores, arg_scores = 1, arg_color=NULL), info = "single value")               # Error if arg_scores is a single value
  expect_error(twoPCsScoresPlot(arg_PC = temp_scores, arg_scores = c(1,"a"), arg_color=NULL), info = "not numeric")         # Error if values in arg_score are not numeric (but not if numbers introduced as characters)
  expect_error(twoPCsScoresPlot(arg_PC = temp_scores, arg_scores = NULL, arg_color=NULL), info = "null input")              # Error if arg_score is NULL
  expect_error(twoPCsScoresPlot(arg_PC = temp_scores, arg_scores = c(1,2,3), arg_color=NULL), info = "more than 2 values")  # Error if more than 2 values in arg_score
  expect_error(twoPCsScoresPlot(arg_PC = temp_scores, arg_scores = c(1,5), arg_color=NULL), info = "excess values")         # Error if values in arg_score are larger than the numer of arg_PC columns or smaller than 0
  expect_error(twoPCsScoresPlot(arg_PC = temp_scores, arg_scores = c(0,2), arg_color=NULL), info = "not positive values")   # Error if values in arg_score are smaller than 0
  expect_error(twoPCsScoresPlot(arg_PC = temp_scores, arg_scores = c(2,2), arg_color=NULL), info = "equal values")          # Error if values in arg_score are equal
  #expect_error(twoPCsScoresPlot(arg_PC = temp_scores, arg_scores = c(1,2), arg_color=NULL), info = "Temp no error")         # Temp no error
})

# ---------------------------------------------------------------------------------------------------------------------

# Test sanity of arg_color
test_that("arg_color format/value errors", {
  expect_error(twoPCsScoresPlot(arg_PC = temp_scores, arg_scores = c(1,2), arg_color=1), info = "numeric")                        # Error if arg_color is not a character (numeric)
  expect_error(twoPCsScoresPlot(arg_PC = temp_scores, arg_scores = c(1,2), arg_color=list()), info = "not positive values")       # Error if arg_color is not a character (empty list) (but not if empty atomic vector "c()")
  expect_error(twoPCsScoresPlot(arg_PC = temp_scores, arg_scores = c(1,2), arg_color="wrong_col"), info = "not positive values")  # Error if arg_color is not a colname in arg_PC)
  #expect_error(twoPCsScoresPlot(arg_PC = temp_scores, arg_scores = c(1,2), arg_color="treatment"), info = "Temp no error")  # Temp no error
})
