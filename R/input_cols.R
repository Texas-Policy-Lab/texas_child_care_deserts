#' @title Test and select input
#' @description Tests that the expected columns exist, and then selects those
#' columns
#' @param df data.frame
#' @param input_columns. Vector. Vector of variable names to select
#' @return data.frame
test_input <- function(df, input_columns) {

  assertthat::assert_that(all(c(input_columns %in% colnames(df))))

  df %>%
    dplyr::select(dplyr::one_of(input_columns))
}

#' @title Tests the vector or column type
#' @param v. Vector. A vector to test.
test_type <- function(v) UseMethod("test_type")

#' @title Test for character (string) column or vector
#' @inheritParams test_type
test_type.character <- function(v) {

  assertthat::assert_that(typeof(v) == "character" & class(v) == "character",
                          msg = "Not a character (string) vector")
}

#' @title Test for numeric column or vector
#' @inheritParams test_type
test_type.numeric <- function(v) {

  assertthat::assert_that(typeof(v) == "double" & class(v) != "Date",
                          msg = "Not a numeric vector")
}

#' @title Test for date column or vector
#' @inheritParams test_type
test_type.date <- function(v) {

  assertthat::assert_that(typeof(v) == "double" & class(v) == "Date",
                          msg = "Not a date vector")
}

#' @title Test for boolean (logical) column or vector
#' @inheritParams test_type
test_type.boolean <- function(v) {
  
  assertthat::assert_that(typeof(v) == "logical" & class(v) == "logical",
                          msg = "Not a boolean (logical) vector")
}
