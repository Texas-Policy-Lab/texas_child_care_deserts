#' @title Test and select input
#' @description Tests that the expected columns exist, and then selects those
#' columns
#' @param df data.frame
#' @param input_columns. List. List of variable names to select with expected
#' variable types.
#' @return data.frame
test_input <- function(df, input_columns) {

  assertthat::assert_that(all(c(names(input_columns) %in% colnames(df))))

  assertthat::assert_that(all(
    sapply(names(input_columns), function(col) {
    cls <- input_columns[[col]]
    test_col <- structure(list(v = df[[col]]), 
                          class = cls)
    do.call(test_type, test_col)
    })
  ))

  df %>%
    dplyr::select(dplyr::one_of(names(input_columns)))
}

#' @title Tests the vector or column type
#' @param v. Vector. A vector to test.
test_type <- function(v) UseMethod("test_type")

#' @title Test for character (string) column or vector
#' @inheritParams test_type
test_type.character <- function(v) {

  assertthat::assert_that(class(v) == "character",
                          msg = "Not a character (string) vector")
}

#' @title Test for numeric column or vector
#' @inheritParams test_type
test_type.numeric <- function(v) {

  assertthat::assert_that(class(v) == "numeric",
                          msg = "Not a numeric vector")
}

#' @title Test for date column or vector
#' @inheritParams test_type
test_type.date <- function(v) {

  assertthat::assert_that(class(v) == "Date",
                          msg = "Not a date vector")
}

#' @title Test for boolean (logical) column or vector
#' @inheritParams test_type
test_type.boolean <- function(v) {
  
  assertthat::assert_that(class(v) == "logical",
                          msg = "Not a boolean (logical) vector")
}
